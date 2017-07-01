##------------------------------------------------------------------------------
## Bobae's MA Thesis (github.com/bobaekang/MA_thesis)            
## Researcher: Bobae Kang (github.com/bobaekang)                 
## Advisor: Benjamin Soltoff (github.com/bensoltoff)             
##------------------------------------------------------------------------------
## Script: thesis05_tables.R                                    
## Last updated: 5/19/17                                         
##------------------------------------------------------------------------------
## This script performs the following tasks:                     
## 1. get statistics to include in tables for sample divvy trips and stations                                         
##------------------------------------------------------------------------------
## This script requires the outputs from the following scripts:  
## * thesis03_sample_generation.R
##------------------------------------------------------------------------------

# setwd("~/UChicago/MA Thesis/R scripts") # for local use

# import pacakges
library(tidyverse)
library(feather)

# import data
divvy_from_train <- read_feather('data/divvy_from_train.feather')      # training set, trips originating
divvy_to_train   <- read_feather('data/divvy_to_train.feather')        # training set, trips terminating
# divvy_from_test  <- read_feather('data/divvy_from_test.feather')       # test set, trips originating
# divvy_to_test    <- read_feather('data/divvy_to_test.feather')         # test set, trips terminating
divvy_station    <- read_feather('data/Divvy_station_geodemo.feather') # divvy station
cca_features     <- read_feather('data/cca_features.feather')          # demographic features

# define a function to report
report <- function(input, full=TRUE){
  if(full == TRUE){
    name <- c('mean', 'sd', 'median', 'max', 'min')
    value <- c(mean(input), sd(input), median(input), max(input), min(input))
  } else{
    name <- c('mean', 'sd')
    value <- c(mean(input), sd(input))
  }
  
  output <- tibble(name = name,
                   value = value)
  return(output)
}

##------------------------------------------------------------------------------
# Trips originating
# annual membership
report(ifelse(divvy_from_train$usertype == 'Subscriber', 1, 0), full=FALSE)

# gender
report(ifelse((divvy_from_train %>% filter(usertype == 'Subscriber'))$gender == 'Male', 1, 0), full=FALSE)
(divvy_from_train %>% filter(usertype == 'Subscriber'))$gender %>% length()

# age
report((2016-(divvy_from_train %>% filter(usertype == 'Subscriber'))$birthyear))
(2016-(divvy_from_train %>% filter(usertype == 'Subscriber'))$birthyear) %>% length()

# weekday
report(divvy_from_train$weekday, full=FALSE)

# wet day
report(divvy_from_train$PRCP, full=FALSE)

# rush hour
report(divvy_from_train$startrush, full=FALSE)
report((divvy_from_train %>% filter(startrush == 1))$startrushAM, full=FALSE)
(divvy_from_train %>% filter(startrush == 1))$startrushAM %>% length()
report((divvy_from_train %>% filter(startrush == 1))$startrushPM, full=FALSE)

# trip duration (min)
report(divvy_from_train$tripduration/60)

# trip distance (km)
report(divvy_from_train$tripdistance/1000)

# proximity
# 50m
report(divvy_from_train$from_prox50, full=FALSE)
divvy_from_train$from_prox50 %>% sum()
# 100m
report(divvy_from_train$from_prox100, full=FALSE)
divvy_from_train$from_prox100 %>% sum()
# 200m
report(divvy_from_train$from_prox200, full=FALSE)
divvy_from_train$from_prox200 %>% sum()
# 300m
report(divvy_from_train$from_prox300, full=FALSE)
divvy_from_train$from_prox300 %>% sum()

# PMM
# 50m
report(divvy_from_train$mm50, full=FALSE)
divvy_from_train$mm50 %>% sum()
# 100m
report(divvy_from_train$mm100, full=FALSE)
divvy_from_train$mm100 %>% sum()
# 200m
report(divvy_from_train$mm200, full=FALSE)
divvy_from_train$mm200 %>% sum()
# 300m
report(divvy_from_train$mm300, full=FALSE)
divvy_from_train$mm300 %>% sum()

##------------------------------------------------------------------------------
# Trips terminating
# annual membership
report(ifelse(divvy_to_train$usertype == 'Subscriber', 1, 0), full=FALSE)

# gender
report(ifelse((divvy_to_train %>% filter(usertype == 'Subscriber'))$gender == 'Male', 1, 0), full=FALSE)
(divvy_to_train %>% filter(usertype == 'Subscriber'))$gender %>% length()

# age
report((2016-(divvy_to_train %>% filter(usertype == 'Subscriber'))$birthyear))
(2016-(divvy_to_train %>% filter(usertype == 'Subscriber'))$birthyear) %>% length()

# weekday
report(divvy_to_train$weekday, full=FALSE)

# wet day
report(divvy_to_train$PRCP, full=FALSE)

# rush hour
report(divvy_to_train$stoprush, full=FALSE)
report((divvy_to_train %>% filter(stoprush == 1))$stoprushAM, full=FALSE)
(divvy_to_train %>% filter(stoprush == 1))$stoprushAM %>% length()
report((divvy_to_train %>% filter(stoprush == 1))$stoprushPM, full=FALSE)

# trip duration (min)
report(divvy_to_train$tripduration/60)

# trip distance (km)
report(divvy_to_train$tripdistance/1000)

# proximity
# 50m
report(divvy_to_train$to_prox50, full=FALSE)
divvy_to_train$to_prox50 %>% sum()
# 100m
report(divvy_to_train$to_prox100, full=FALSE)
divvy_to_train$to_prox100 %>% sum()
# 200m
report(divvy_to_train$to_prox200, full=FALSE)
divvy_to_train$to_prox200 %>% sum()
# 300m
report(divvy_to_train$to_prox300, full=FALSE)
divvy_to_train$to_prox300 %>% sum()

# PMM
# 50m
report(divvy_to_train$mm50, full=FALSE)
divvy_to_train$mm50 %>% sum()
# 100m
report(divvy_to_train$mm100, full=FALSE)
divvy_to_train$mm100 %>% sum()
# 200m
report(divvy_to_train$mm200, full=FALSE)
divvy_to_train$mm200 %>% sum()
# 300m
report(divvy_to_train$mm300, full=FALSE)
divvy_to_train$mm300 %>% sum()


##------------------------------------------------------------------------------
# Stations
# trips originating
# all
report((divvy_from_train %>% group_by(from_station_id) %>% count())$n)
(divvy_from_train %>% group_by(from_station_id) %>% count())$n %>% length()
# PMM
# 50m
report((divvy_from_train %>% filter(mm50 == 1) %>% group_by(from_station_id) %>% count())$n)
(divvy_from_train %>% filter(mm50 == 1) %>% group_by(from_station_id) %>% count())$n %>% sum()
# 100m
report((divvy_from_train %>% filter(mm100 == 1) %>% group_by(from_station_id) %>% count())$n)
(divvy_from_train %>% filter(mm100 == 1) %>% group_by(from_station_id) %>% count())$n %>% sum()
# 200m
report((divvy_from_train %>% filter(mm200 == 1) %>% group_by(from_station_id) %>% count())$n)
(divvy_from_train %>% filter(mm200 == 1) %>% group_by(from_station_id) %>% count())$n %>% sum()
# 300m
report((divvy_from_train %>% filter(mm300 == 1) %>% group_by(from_station_id) %>% count())$n)
(divvy_from_train %>% filter(mm300 == 1) %>% group_by(from_station_id) %>% count())$n %>% sum()

# trips terminating
# all
report((divvy_to_train %>% group_by(to_station_id) %>% count())$n)
(divvy_to_train %>% group_by(to_station_id) %>% count())$n %>% length()
# PMM
# 50m
report((divvy_to_train %>% filter(mm50 == 1) %>% group_by(to_station_id) %>% count())$n)
(divvy_to_train %>% filter(mm50 == 1) %>% group_by(to_station_id) %>% count())$n %>% sum()
# 100m
report((divvy_to_train %>% filter(mm100 == 1) %>% group_by(to_station_id) %>% count())$n)
(divvy_to_train %>% filter(mm100 == 1) %>% group_by(to_station_id) %>% count())$n %>% sum()
# 200m
report((divvy_to_train %>% filter(mm200 == 1) %>% group_by(to_station_id) %>% count())$n)
(divvy_to_train %>% filter(mm200 == 1) %>% group_by(to_station_id) %>% count())$n %>% sum()
# 300m
report((divvy_to_train %>% filter(mm300 == 1) %>% group_by(to_station_id) %>% count())$n)
(divvy_to_train %>% filter(mm300 == 1) %>% group_by(to_station_id) %>% count())$n %>% sum()

# station capacity
divvy_station_train <- subset(divvy_station,
                              (id %in% divvy_from_train$from_station_id |
                                 id %in% divvy_to_train$to_station_id))
report(divvy_station_train$dpcapacity)
length(divvy_station_train$dpcapacity)

# located in CBD
report(divvy_station_train$cbd, full=FALSE)

# prox
# 50m
report(divvy_station_train$prox50, full=FALSE)
# 100m
report(divvy_station_train$prox100, full=FALSE)
# 200m
report(divvy_station_train$prox200, full=FALSE)
# 300m
report(divvy_station_train$prox300, full=FALSE)

# prox n
# 50m
report(divvy_station_train$prox50_n)
# close only
report((divvy_station_train %>% filter(prox50 == 1))$prox50_n)
(divvy_station_train %>% filter(prox50 == 1))$prox50_n %>% length()
# 100m
report(divvy_station_train$prox100_n)
# close only
report((divvy_station_train %>% filter(prox100 == 1))$prox100_n)
(divvy_station_train %>% filter(prox100 == 1))$prox100_n %>% length()
# 200m
report(divvy_station_train$prox200_n)
# close only
report((divvy_station_train %>% filter(prox200 == 1))$prox200_n)
(divvy_station_train %>% filter(prox200 == 1))$prox200_n %>% length()
# 300m
report(divvy_station_train$prox300_n)
# close only
report((divvy_station_train %>% filter(prox300 == 1))$prox300_n)
(divvy_station_train %>% filter(prox300 == 1))$prox300_n %>% length()


##------------------------------------------------------------------------------
# Demographic characteristics for community areas
report(cca_features$population)        # population
report(cca_features$pop_density)       # population density (sq)
report(cca_features$perc_black)        # black percentage
report(cca_features$perc_employed)     # employed percentage
report(cca_features$income_per_capita) # income per capita


# Clear the environment---------------------------------------------------------
rm(list=ls()) # clear the environment
