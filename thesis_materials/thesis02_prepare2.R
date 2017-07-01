##------------------------------------------------------------------------------
## Bobae's MA Thesis (github.com/bobaekang/MA_thesis)
## Researcher: Bobae Kang (github.com/bobaekang)
## Advisor: Benjamin Soltoff (github.com/bensoltoff)
##------------------------------------------------------------------------------
## Script: thesis02_prepare2.R
## Last updated: 5/19/17
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
# setwd("~/UChicago/MA Thesis/R scripts") # for local use

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
  path <- str_c('rawdata/', filename_zips[i])
  download.file(url = url_zips[i],
                destfile = str_c(path, '.zip'),
                mode = 'wb')
  unlink(path, recursive = TRUE)
  dir.create(path)
  unzip(str_c(path, '.zip'), exdir=path)
}

# get shapefile path
shapefile_string <- c()
for (i in 1:length(url_zips)){
  path                <-  str_c('rawdata/', filename_zips[i])
  shapefile           <- regexpr(".+shp$", list.files(path), perl=TRUE)
  shapefile_name      <- regmatches(list.files(path), shapefile)
  shapefile_string[i] <- substring(shapefile_name, 1, nchar(shapefile_name)-4)  
}

# read and transform into spatial file
boundaries_cbd   <- readOGR(str_c('rawdata/', filename_zips[1]), shapefile_string[1]) %>%
  spTransform(., CRS('+proj=longlat +datum=WGS84'))
boundaries_cca   <- readOGR(str_c('rawdata/', filename_zips[2]), shapefile_string[2]) %>%
  spTransform(., CRS('+proj=longlat +datum=WGS84'))
boundaries_tract <- readOGR(str_c('rawdata/', filename_zips[3]), shapefile_string[3]) %>%
  spTransform(., CRS('+proj=longlat +datum=WGS84'))


# 1.1 Import population and employment data by census 2010 block group----------
tract_to_cca   <- fread('data_predownload/2010_tract_to_cca.csv')
# population2015 <- fread('data_predownload/ACS_15_5YR_B01003_with_ann.csv', skip=1)
# employment2015 <- fread('data_predownload/ACS_15_5YR_S2301_with_ann.csv', skip=1) %>%
#   select(c(1:4, 6, 10))

population2015 <- fread('data_predownload/ACS_15_5YR_B02001_with_ann.csv', skip=1) %>%
  select(c(1:5, 8:9))
income2015     <- fread('data_predownload/ACS_15_5YR_B19301_with_ann.csv', skip=1)
employment2015 <- fread('data_predownload/ACS_15_5YR_S2301_with_ann.csv', skip=1) %>%
  select(c(1:4, 8))

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
  if(substring(x, 15, 15) == 0){
    tract <- substring(x, 16, 20)
  } else {
    tract <- substring(x, 15, 20)
  }
  return(tract)
}

# get tract-level features
features_pop <- population2015 %>%
  mutate(tract            = as.vector(sapply(as.character(population2015$Id), getTract)),
         population       = `Estimate; Total:`,
         population_black = `Estimate; Total: - Black or African American alone`) %>%
  select(tract, population, population_black)

features_inc  <- income2015 %>%
  mutate(tract = as.vector(sapply(as.character(income2015$Id), getTract)),
         income_per_capita = as.numeric(`Estimate; Per capita income in the past 12 months (in 2015 Inflation-adjusted dollars)`)) %>%
  select(tract, income_per_capita)
features_inc$income_per_capita[is.na(features_inc$income_per_capita)] <- 0 # replace NA with 0

features_emp  <- employment2015 %>%
  mutate(tract = as.vector(sapply(as.character(population2015$Id), getTract)),
         population_employable = `Total; Estimate; Population 16 years and over`,
         population_employed   = `Total; Estimate; Population 16 years and over`*
           as.numeric(`Employment/Population Ratio; Estimate; Population 16 years and over`)/100) %>%
  select(tract, population_employable, population_employed)
features_emp$population_employed[is.na(features_emp$population_employed)] <- 0 # replace NA with 0

# join all features
features <- features_pop %>%
  left_join(features_inc) %>%
  left_join(features_emp)

# aggregaet tract features to cca
tract_to_cca$TRACT <- as.character(tract_to_cca$TRACT) # for the joining

# join tract_to_cca and features for aggregation
tract_to_cca_with_features <- tract_to_cca %>%
left_join(features, by=c('TRACT' = 'tract')) %>%
as.data.table()

# aggregate tract to cca
cca_features <- tract_to_cca_with_features %>%
  na.omit() %>%                                                       # remove NAs 
  group_by(cca_id = as.factor(CHGOCA)) %>%                            # group by ids
  summarise(population_all        = sum(population),                  # population by cca
            population_black      = sum(population_black),            # black population by cca
            income_aggregate      = sum(income_per_capita*population),# aggregated income by cca
            population_employable = sum(population_employable),       # employable population by cca
            population_employed   = sum(population_employed)) %>%     # employed people by cca
  as.data.table() %>%
  mutate(pop_density = population_all/boundaries_cca@data$shape_area*10764000, # get population density (square km)
         perc_black  = population_black/population_all*100,                    # get black population ratio
         perc_employed     = population_employed/population_employable*100,# get employment/population ratio
         income_per_capita = income_aggregate/population_all) %>%              # get per-capita income
  select(cca_id, population_all, pop_density, perc_black, perc_employed, income_per_capita)

# # standardize all columns (s.t. mean = 0, sd = 1)
# cca_normed <- cca$cca_id %>%
#   cbind(scale(cca %>% select(-cca_id)) %>%
#           as.data.table())
# cca_normed <- scale(cca %>% select(-cca_id)) %>%
#   as.data.table() %>%
#   mutate(cca_id = cca$cca_id)

# augment divvy_station data 
divvy_station <- divvy_station %>%
  left_join(cca_features, by=c('cca_id'))
divvy_station$cca_id <- as.factor(divvy_station$cca_id) # cca id as factor var

##------------------------------------------------------------------------------
## 5. SAVE OUTPUTS 
##------------------------------------------------------------------------------
write_feather(cca_features, 'data/cca_features.feather')
write_feather(divvy_station, 'data/Divvy_station_geodemo.feather')

# Clear the environment---------------------------------------------------------
rm(list=ls()) # clear the environment

