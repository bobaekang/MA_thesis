##------------------------------------------------------------------------------
## Bobae's MA Thesis (github.com/bobaekang/MA_thesis)
## Researcher: Bobae Kang (github.com/bobaekang)
## Advisor: Benjamin Soltoff (github.com/bensoltoff)
##------------------------------------------------------------------------------
## Script: thesis04_geodemo.R
## Last updated: 4/4/17
##------------------------------------------------------------------------------
## This script performs the following tasks:
## 1. Download and import data
## 2. Augment Divvy station data with CBD variable
## 3. Augment Divvy station data with community area variable
## 4. Augment Divvy station data with demographic variables 
## 5. Save outputs
##------------------------------------------------------------------------------
## This script requires the outputs from the following scripts: 
## * thesis01_prepare1.R
##------------------------------------------------------------------------------

#import pacakges
library(tidyverse)
library(feather)
library(data.table)
library(stringr)
library(sp)
library(rgdal)
library(ggmap)

# import divvy_station data
divvy_station <- read_feather('data/Divvy_station.feather')

##------------------------------------------------------------------------------
## 1. DOWNLOAD AND IMPORT GEOGRAPHICAL DATA 
##------------------------------------------------------------------------------
# 1.0 Download and import shapefiles for boundaries-----------------------------
# get urls
url_cbd       <- 'https://data.cityofchicago.org/api/geospatial/tksj-nvsw?method=export&format=Shapefile'
url_cca       <- 'https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=Shapefile'
url_2010tract <- 'https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=Shapefile'

url_zips      <- c(url_cbd, url_cca, url_2010tract)
filename_zips <- c('central_business_district', 'community_area', 'census2010_tract')

# download and unzip
for (i in 1:length(url_zips)){
  download.file(url = url_zips[i],
                destfile = str_c('rawdata/', filename_zips[i], '.zip'),
                mode = 'wb')
  unzip(str_c('rawdata/', filename_zips[i], '.zip'), exdir='rawdata')
}

list.files(str_c('rawdata/', filename_zips[1], '.zip'))

# read and transform into spatial file
boundaries_cbd   <- readOGR('rawdata', 'geo_export_bce1b8ab-1cb7-41a0-b443-45ae0a6ff03b') %>%
  spTransform(., CRS('+proj=longlat +datum=WGS84'))
boundaries_cca   <- readOGR('rawdata', 'geo_export_9b418bb7-cfed-475b-a3f0-9b0021a34a2b') %>%
  spTransform(., CRS('+proj=longlat +datum=WGS84'))
boundaries_tract <- readOGR('rawdata', 'geo_export_caef13b6-ffd6-42d6-841f-0731b1d386cb') %>%
  spTransform(., CRS('+proj=longlat +datum=WGS84'))


# 1.1 Import population and employment data by census 2010 block group----------
tract_to_cca   <- fread('data_demo/2010_tract_to_cca.csv')
population2015 <- fread('data_demo/ACS_15_5YR_B01003_with_ann.csv', skip=1)
employment2015 <- fread('data_demo/ACS_15_5YR_S2301_with_ann.csv', skip=1) %>%
  select(c(1:4, 6, 10))


##------------------------------------------------------------------------------
## 2. AUGMENT DIVVY STATION DATA WITH CBD VARIABLE
##------------------------------------------------------------------------------
divvy_coord <- divvy_station %>%
  select(lon, lat) %>% # select coordinates only
  SpatialPoints(.)     # convert to SpatialPoints object
proj4string(divvy_coord) <- CRS("+proj=longlat +datum=WGS84") # get CRS

cbd_vec <- over(divvy_coord, boundaries_cbd)$objectid
cbd_vec[is.na(cbd_vec)] <- 0 # replace NAs with 0s

divvy_station$cbd <- cbd_vec # add cbd column


##------------------------------------------------------------------------------
## 3. AUGMENT DIVVY STATION DATA WITH COMMUNITY AREA VARIABLE
##------------------------------------------------------------------------------
cca_vec <- over(divvy_coord, boundaries_cca) %>% select(community, area_num_1)

divvy_station$cca_name <- cca_vec$community  # add cca_name column
divvy_station$cca_id   <- cca_vec$area_num_1 # # add cca_num column


##------------------------------------------------------------------------------
## 4. AUGMENT DIVVY STATION DATA WITH DEMOGRAPHIC VARIABLES 
##------------------------------------------------------------------------------
# define function to extract tract id from geoid
getTract <- function(x){
  if(substring(x, 6, 6) == 0){
    tract <- substring(x, 7, 11)
  } else {
    tract <- substring(x, 6, 11)
  }
  return(tract)
}

# get tract and population columns only
population2015 <- population2015 %>%
  mutate(tract = as.vector(sapply(as.character(population2015$Id2), getTract)),
         population = `Estimate; Total`) %>%
  select(tract, population)

# get tract and employment columns only
colnames(employment2015) <- c('id', 'id2', 'geography', 'total', 'participation_rates', 'unemployment_rate')
employment2015 <- employment2015 %>%
  mutate(tract = as.vector(sapply(as.character(employment2015$id2), getTract)),
         employed = total*(as.numeric(participation_rates)/100)*
           (1-(as.numeric(unemployment_rate)/100))) %>%
  select(tract, employed) %>%
  na.omit()

# join population and employment data
pop_emp2015 <- left_join(population2015, employment2015, by = 'tract')
pop_emp2015$tract <- as.integer(pop_emp2015$tract)         # for joining
pop_emp2015$employed[is.na(pop_emp2015$employed)] <- 0     # replace NAs with 0s

# join tract_to_cca and pop_emp data
tract_to_cca <- tract_to_cca %>%
  left_join(pop_emp2015, by=c('TRACT' = 'tract')) %>%
  as_tibble()

# cca population and employed people data
cca_pop_emp <- tract_to_cca %>%
  na.omit() %>%                                 # remove NAs 
  group_by(cca_id      = as.factor(CHGOCA)) %>% # group by ids
  summarise(population = sum(population),       # population by cca
            employed   = sum(employed)) %>%     # employed people by cca
  as_tibble()

# cca data
cca <- data.frame(cca_id   = boundaries_cca$area_num_1,     # cca id
                  cca_name = boundaries_cca$community,      # cca name  
                  area     = boundaries_cca$shape_area) %>% # cca area
  arrange(cca_id) %>%                                       # order by cca number
  left_join(cca_pop_emp) %>%                                # add population and employed
  as_tibble()

# augment divvy_station data 
divvy_station <- divvy_station %>%
  left_join(cca, by=c('cca_id', 'cca_name'))
divvy_station$cca_id <- as.factor(divvy_station$cca_id) # cca id as factor var

##------------------------------------------------------------------------------
## 5. SAVE OUTPUTS 
##------------------------------------------------------------------------------
write_feather(divvy_station, 'data/Divvy_station_geodemo.feather')


# Clear the environment---------------------------------------------------------
rm(list=ls()) # clear the environment

