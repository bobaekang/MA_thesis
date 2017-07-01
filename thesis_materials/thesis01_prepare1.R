##------------------------------------------------------------------------------
## Bobae's MA Thesis (github.com/bobaekang/MA_thesis)
## Researcher: Bobae Kang (github.com/bobaekang)
## Advisor: Benjamin Soltoff (github.com/bensoltoff)
##------------------------------------------------------------------------------
## Script: thesis01_prepare.R
## Last updated: 4/28/17
##------------------------------------------------------------------------------
## This script performs the following tasks:
## 1. reading and transforming the CTA data to generate a data that contains
## information on both time and location for CTA stops.
## 2. reading and transforming the Divvy data to generate 1) a tidy Divvy trip
## data with spatial and proximity variables and 2) a tidy Divvy station data
## with proximity variables. 
## 3. creating a data that links the Divvy stations to close-by CTA stops.
## 4. Save outputs
##------------------------------------------------------------------------------
## This script requires the outputs from the following scripts: 
## * thesis00_download.R
##------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(data.table)
library(feather)
library(geosphere)
library(stringr)

##------------------------------------------------------------------------------
## 1, READ AND TRANSFORM THE CTA DATA
## The following codes reads two CTA dataset concerning public transit stops and combine them
##------------------------------------------------------------------------------
# load and join the data
cta_stops <- fread("rawdata/stops.txt")
cta_stop_times <- fread("rawdata/stop_times.txt")

cta_stop_times_loc <- left_join(cta_stop_times, cta_stops, by = "stop_id")
# drop less meaningful variables
cta_stop_times_loc <- cta_stop_times_loc %>%
  select(-stop_sequence, -stop_headsign, -shape_dist_traveled, -stop_code, -stop_desc, -wheelchair_boarding)

##------------------------------------------------------------------------------
## 2. READ AND TRANSFORM THE DIVVY DATA
## The following codes read and join two Divvy dataset on
## 1) Divvy trips and 2) locations of Divvy stations.
##------------------------------------------------------------------------------

## 2.0. Import and prepare the data---------------------------------------------
# Create a vector of the names of Divvy trip files
divvy_trip_files <- list.files("rawdata/", pattern = ".*Trips.*\\.csv$", full.names = TRUE)

# Use map function to read all four trip files and bind them
divvy_trip <- divvy_trip_files %>%
  map(read_csv) %>%
  bind_rows()

# Read the station file
divvy_station <- fread("rawdata/Divvy_Stations_2016_Q4.csv") # only the latest/most comprehensive list of stations  
colnames(divvy_station) <- c("id", "name", "lat", "lon", "dpcapacity", "online_date")


## 2.1. Add space-related variables---------------------------------------------
# write a function to calculate Manhattan distance
distManhattan <- function(loc1, loc2){ 
  result <- matrix(, nrow = 0, ncol = dim(loc2)[1])
  for(i in 1:dim(loc1)[1]){
    adj_lon <- cbind(loc2[,1], loc1[i,2])
    adj_lat <- cbind(loc1[i,1], loc2[,2])
    
    lon_dist <- distm(loc1[i,], adj_lon, fun = distHaversine)
    lat_dist <- distm(loc1[i,], adj_lat, fun = distHaversine)
    manhat_dist <- lon_dist + lat_dist
    
    result <- rbind(result, manhat_dist)
  }
  return(result)
}

# get distance between divvy stations 
station_latlon <- divvy_station %>% 
  arrange(by=id) %>%   # sort by id 
  select(lat, lon)     # get a station lat-lon pairs

station_dist <- distManhattan(station_latlon, station_latlon) %>% # get a 581 by 581 distnace matrix
  as.data.table()                                                 # convert the matrix to a data table

station_id <- (divvy_station %>% arrange(by=id))$id %>%           # get a vector of station ids
  as.character()                                                  # make the ids characters 

colnames(station_dist) <- station_id                              # ids as column names
station_dist           <- cbind(station_id, station_dist)         # add id column

tripdist               <- gather(station_dist, 'to_station_id', 'tripdistance', 2:ncol(station_dist))
colnames(tripdist)     <- c('from_station_id', colnames(tripdist)[2:3])
tripdist <- tripdist %>%                                          # convert station ids back to integers
  mutate(from_station_id = as.integer(from_station_id), 
         to_station_id   = as.integer(to_station_id))

# add tripdistance variable to trip data
divvy_trip <- left_join(divvy_trip, tripdist, by=c('from_station_id', 'to_station_id'))

# add onemile variable to trip data (tripdistance greater than 0 and less than a mile, 1, otherwise, 0)
divvy_trip$onemile <- ifelse((divvy_trip$tripdistance > 0 & divvy_trip$tripdistance < 1609.344), 1, 0)

# create matrices of Divvy and CTA coordinates
divvy_m <- cbind(divvy_station$lon, divvy_station$lat)
cta_m   <- cbind(cta_stops$stop_lon, cta_stops$stop_lat)
#distance_m <- distm(Divvy_m, CTA_m, fun = distHaversine) # use this line for Euclidean distance

# calculate Manhattan distance between Divvy stations and CTA stations
distance_m <-distManhattan(divvy_m, cta_m) # a 581 by 11487 matrix

# get proximity variables for 50 meters
distance50              <- distance_m <= 50 # check if the distance is <=50 meters or approximately 0.1 mile
proximity50_1           <- (rowSums(distance50) > 0)*1 # binary; a Divvy station is <=50m from any CTA stop, 1; otherwise, 0 
divvy_station$prox50    <- as.integer(proximity50_1)
proximity50_2           <- rowSums(distance50) # non-binary; number of close CTA stops
divvy_station$prox50_n  <- as.integer(proximity50_2)

# for 100 meters
distance100             <- distance_m <= 100 # check if the distance is <=100 meters or approximately 0.1 mile
proximity100_1          <- (rowSums(distance100) > 0)*1 # binary; a Divvy station is <=50m from any CTA stop, 1; otherwise, 0 
divvy_station$prox100   <- as.integer(proximity100_1)
proximity100_2          <- rowSums(distance100) # non-binary; number of close CTA stops
divvy_station$prox100_n <- as.integer(proximity100_2)

# for 200 meters
distance200             <- distance_m <= 200 # check if the distance is <=100 meters or approximately 0.1 mile
proximity200_1          <- (rowSums(distance200) > 0)*1 # binary; a Divvy station is <=50m from any CTA stop, 1; otherwise, 0 
divvy_station$prox200   <- as.integer(proximity200_1)
proximity200_2          <- rowSums(distance200) # non-binary; number of close CTA stops
divvy_station$prox200_n <- as.integer(proximity200_2)

# finally, for 300 meters
distance300             <- distance_m <= 300 # check if the distance is <=300 meters or approximately 0.1 mile
proximity300_1          <- (rowSums(distance300) > 0)*1 # binary; a Divvy station is <=50m from any CTA stop, 1; otherwise, 0 
divvy_station$prox300   <- as.integer(proximity300_1)
proximity300_2          <- rowSums(distance300) # non-binary; number of close CTA stops
divvy_station$prox300_n <- as.integer(proximity300_2)

# Adding spatial (i.e., latitude and longitude) and proximity variables to the Divvy trip data
from_station <- divvy_station %>%
  select(id, lon, lat, prox50, prox50_n, prox100, prox100_n, prox200, prox200_n, prox300, prox300_n)
colnames(from_station) <- c("from_station_id", "from_lon", "from_lat",
                            "from_prox50", "from_prox50_n", 
                            "from_prox100", "from_prox100_n",
                            "from_prox200", "from_prox200_n",
                            "from_prox300", "from_prox300_n")

to_station <- divvy_station %>%
  select(id, lon, lat, prox50, prox50_n, prox100, prox100_n, prox200, prox200_n, prox300, prox300_n)
colnames(to_station) <- c("to_station_id", "to_lon", "to_lat",
                          "to_prox50", "to_prox50_n",
                          "to_prox100", "to_prox100_n",
                          "to_prox200", "to_prox200_n",
                          "to_prox300", "to_prox300_n")

divvy_data_from <- left_join(divvy_trip, from_station)
divvy_data      <- left_join(divvy_data_from, to_station)



## 2.2 Add time-related variables-----------------------------------------------
# make starttime and stoptime variables POSITXlt data
divvy_data$starttime <- as.POSIXlt(as.character(divvy_data$starttime), format = "%m/%d/%Y %H:%M", tz = "America/Chicago")
divvy_data$stoptime  <- as.POSIXlt(divvy_data$stoptime, format = "%m/%d/%Y %H:%M", tz = "America/Chicago")

# Add separate date and hour columns
divvy_data <- divvy_data %>%
  mutate(startdate = as.Date(starttime),
         starthour = format(starttime, format = "%H:%M"),
         stopdate  = as.Date(stoptime),
         stophour  = format(stoptime, format = "%H:%M"))

# Add a variable for weekdays/weekends 
divvy_data$weekday <- ifelse((divvy_data$starttime$wday != 0 & divvy_data$starttime$wday != 6), 1, 0)

# Add variables for rush hour (6:00 to 10:00 and 16:00 to 20:00 for weekdays)
divvy_data$startrush <- ifelse(((divvy_data$starttime$hour>=6 & divvy_data$starttime$hour<10) |
                                 (divvy_data$starttime$hour>=16 & divvy_data$starttime$hour<20)) &
                                 (divvy_data$weekday == 1), 1, 0)
divvy_data$stoprush  <- ifelse(((divvy_data$stoptime$hour>=6 & divvy_data$stoptime$hour<10) | 
                                 (divvy_data$stoptime$hour>=16 & divvy_data$stoptime$hour<20)) &
                                 (divvy_data$weekday == 1), 1, 0)
# rush morning
divvy_data$startrushAM <- ifelse(((divvy_data$starttime$hour>=6 & divvy_data$starttime$hour<10)) &
                                 (divvy_data$weekday == 1), 1, 0)
divvy_data$stoprushAM  <- ifelse(((divvy_data$stoptime$hour>=6 & divvy_data$stoptime$hour<10)) &
                                 (divvy_data$weekday == 1), 1, 0)
# rush evening
divvy_data$startrushPM <- ifelse(((divvy_data$starttime$hour>=16 & divvy_data$starttime$hour<20)) &
                                 (divvy_data$weekday == 1), 1, 0)
divvy_data$stoprushPM  <- ifelse(((divvy_data$stoptime$hour>=16 & divvy_data$stoptime$hour<20)) &
                                 (divvy_data$weekday == 1), 1, 0)


# Add a variable for tripduration >= 30min
divvy_data$longtrip <- ifelse(divvy_data$tripduration >= 1800, 1, 0)

# convert to POSIXct because feather cannot write POSIXlt
divvy_data$starttime <- as.POSIXct(divvy_data$starttime, format = "%Y-%m-%d %H:%M", tz = "America/Chicago")
divvy_data$stoptime  <- as.POSIXct(divvy_data$stoptime, format = "%Y-%m-%d %H:%M", tz = "America/Chicago")


## 2.3 Add weather-related variables (TMIN, TMAX, PRCP)
# import Chicago weather for 2016
weather <- read_csv("data_predownload/ChicagoWeather.csv") %>%
  select(DATE, PRCP, TMAX, TMIN)

# make the DATE column date object
weather$DATE <- as.Date(as.character(weather$DATE), format='%Y%m%d', tz="America/Chicago") 
weather$PRCP <- ifelse((weather$PRCP != 0), 1, 0) # binarize precipitation, where 1 = yes and 0 = no.

divvy_data <- left_join(divvy_data, weather, by=c('startdate' = 'DATE'))

##------------------------------------------------------------------------------
## 3. CREATE A DATA FRAME LINKING DIVVY STATIONS AND CLOSE CTA STOPS
##------------------------------------------------------------------------------

# 3.1. Get index for each proximity standard -----------------------------------
# for 50-meter proximity standard
index50            <- as.data.table(which(distance50 == TRUE, arr.ind = T)) # matrix of indices where the distance is <= 50
index50            <- index50[order(index50$row, index50$col),]
colnames(index50)  <- c('divvy_ix', 'cta_ix')

# for 100 meters
index100           <- as.data.table(which(distance100 == TRUE, arr.ind = T)) # matrix of indices where the distance is <= 100
index100           <- index100[order(index100$row, index100$col),]
colnames(index100) <- c('divvy_ix', 'cta_ix')

# for 200 meters
index200           <- as.data.table(which(distance200 == TRUE, arr.ind = T)) # matrix of indices where the distance is <= 300
index200           <- index200[order(index200$row, index200$col),]
colnames(index200) <- c('divvy_ix', 'cta_ix')

# finally, for 300 meters
index300           <- as.data.table(which(distance300 == TRUE, arr.ind = T)) # matrix of indices where the distance is <= 300
index300           <- index300[order(index300$row, index300$col),]
colnames(index300) <- c('divvy_ix', 'cta_ix')

divvy_station$divvy_ix <- sequence(nrow(divvy_station))
cta_stops$cta_ix <- sequence(nrow(cta_stops))
cta_ix <- cta_stops %>% select(stop_id, stop_name, cta_ix)

# 3.2. Join divvy_station data with the proximity indices-----------------------
# <50m
divvy_station50 <- divvy_station %>%
  left_join(index50) %>%
  left_join(cta_ix) %>%
  select(id, name, prox50, prox50_n, stop_id, stop_name) # select only the columns necessary

# <100m
divvy_station100 <- divvy_station %>%
  left_join(index100) %>%
  left_join(cta_ix) %>%
  select(id, name, prox100, prox100_n, stop_id, stop_name) # select only the columns necessary

# <200m
divvy_station200 <- divvy_station %>%
  left_join(index200) %>%
  left_join(cta_ix) %>%
  select(id, name, prox200, prox200_n, stop_id, stop_name) # select only the columns necessary

# <300m
divvy_station300 <- divvy_station %>%
  left_join(index300) %>%
  left_join(cta_ix) %>%
  select(id, name, prox300, prox300_n, stop_id, stop_name) # select only the columns necessary


##------------------------------------------------------------------------------
## 4. SAVE OUTPUTS
##------------------------------------------------------------------------------

# Oututs from  Part 1
write_feather(cta_stop_times_loc, "data/CTA_stop_times_location.feather")

# Oututs from Part 2
write_feather(divvy_data, "data/Divvy_data.feather")
write_feather(divvy_station, "data/Divvy_station.feather")

# Oututs from Part 3
write_feather(divvy_station50, "data/stops_within_50m.feather")
write_feather(divvy_station100, "data/stops_within_100m.feather")
write_feather(divvy_station200, "data/stops_within_200m.feather")
write_feather(divvy_station300, "data/stops_within_300m.feather")


# Clear the environment---------------------------------------------------------
rm(list=ls()) # clear the environment
