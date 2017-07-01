##------------------------------------------------------------------------------
## Bobae's MA Thesis (github.com/bobaekang/MA_thesis)            
## Researcher: Bobae Kang (github.com/bobaekang)                 
## Advisor: Benjamin Soltoff (github.com/bensoltoff)             
##------------------------------------------------------------------------------
## Script: thesis04_plots.R                                      
## Last updated: 7/1/17                                         
##------------------------------------------------------------------------------
## This script generates the following plots:                    
## * Number of Divvy trips per day                               
## * Number of Divvy trips per day (by day of week)
## * Number of Divvy trips per day (by time of day)
## * Number of Divvy trips per gender                            
## * Trip duration per day
## * Trip duration per day (by day of week)
## * Trip duration per day (by time of day)
## * Max and min temperature per day
## * Divvy stations and CTA stops
## * Divvy stations, trips origination
## * Divvy stations, trips terminating
##------------------------------------------------------------------------------
## This script requires the outputs from the following scripts:  
## * thesis01_prepare1.R                     
##------------------------------------------------------------------------------

# setwd("~/UChicago/MA Thesis/R scripts") # for local use

# import pacakges
library(tidyverse)
library(ggmap)
library(rgdal)
library(maptools)
library(feather)
library(stringr)

# import data
divvy_data <- read_feather("data/Divvy_data.feather") 
divvy_station <- read_feather("data/Divvy_station.feather")
divvy_from_train <- read_feather('data/divvy_from_train.feather')
divvy_to_train <- read_feather('data/divvy_to_train.feather')

# get base map: chicago
chicago_map <- ggmap(
  get_googlemap(
    center = c(lon = -87.65, lat = 41.855),
    zoom = 10))

# get shapefiles
paths <- c('central_business_district', 'community_area')
shapefile_string <- c()
for (i in 1:length(paths)){
  path                <-  str_c('rawdata/', paths[i])
  shapefile           <- regexpr(".+shp$", list.files(path), perl=TRUE)
  shapefile_name      <- regmatches(list.files(path), shapefile)
  shapefile_string[i] <- substring(shapefile_name, 1, nchar(shapefile_name)-4)  
}

# read and transform into spatial file
boundaries_cbd   <- readOGR(str_c('rawdata/', paths[1]), shapefile_string[1]) %>%
  spTransform(., CRS('+proj=longlat +datum=WGS84')) %>%
  fortify()
boundaries_cca   <- readOGR(str_c('rawdata/', paths[2]), shapefile_string[2]) %>%
  spTransform(., CRS('+proj=longlat +datum=WGS84')) %>%
  fortify()


##------------------------------------------------------------------------------
## * NUMBER OF DIVVY TRIPS PER DAY                               
##------------------------------------------------------------------------------
# generate plot
divvy_data %>%
  group_by(startdate) %>%
  count(usertype) %>% 
  # There are only 40 obervations with 'Dependent' usertype,
  # for all of which birthyaer == 1979 and gender == male 
  filter(usertype != 'Dependent') %>%
  ggplot(aes(x=startdate, y=n, color=usertype)) +
  geom_line(alpha=.5) +
  geom_smooth() + 
  geom_vline(xintercept = wet_days, color='gray', alpha=.3) +
  scale_color_manual("",
                     breaks = c('Customer', 'Subscriber'),
                     values = c('#F8766D', '#00BFC4')) +
  labs(title = 'Daily Divvy trips by usertype and day of week',
       subtitle = '0 = weekends, 1 = weekdays',
       x = 'Date',
       y = 'Trips') +
  theme_classic()

# save plot
ggsave('images/trips_per_day.png')


##------------------------------------------------------------------------------
## * NUMBER OF DIVVY TRIPS PER DAY (BY DAY OF WEEK)              
##------------------------------------------------------------------------------
# generate plot
divvy_data %>%
  group_by(startdate) %>%
  count(usertype) %>% 
  # There are only 40 obervations with 'Dependent' usertype,
  # for all of which birthyaer == 1979 and gender == male 
  filter(usertype != 'Dependent') %>%
  # Add weekday attribute to the data frame
  left_join(select(divvy_data, startdate, weekday) %>%
              group_by(startdate) %>%
              summarise(weekday = mean(weekday))) %>%
  ggplot(aes(x=startdate, y=n, color=usertype)) +
  geom_line(alpha=.5) +
  geom_smooth() + 
  facet_wrap(~weekday, nrow=2) + 
  scale_color_manual("",
                     breaks = c('Customer', 'Subscriber'),
                     values = c('#F8766D', '#00BFC4')) +
  labs(title = 'Daily Divvy trips by usertype and day of week',
       subtitle = '0 = weekends, 1 = weekdays',
       x = 'Date',
       y = 'Trips') +
  theme_classic()

# save plot
ggsave('images/trips_per_day_by_weekday.png')


##------------------------------------------------------------------------------
## * NUMBER OF DIVVY TRIPS PER DAY (BY TIME OF DAY)              
##------------------------------------------------------------------------------
# generate plot
divvy_data %>%
  group_by(startdate, startrush) %>%
  count(usertype) %>%
  # There are only 40 obervations with 'Dependent' usertype,
  # for all of which birthyaer == 1979 and gender == male 
  filter(usertype != 'Dependent') %>%
  ggplot(aes(x=startdate, y=n, color=usertype)) +
  geom_line(alpha=.5) +
  geom_smooth() + 
  facet_wrap(~startrush, nrow=2) +
  scale_color_manual("",
                     breaks = c('Customer', 'Subscriber'),
                     values = c('#F8766D', '#00BFC4')) +
  labs(title = 'Daily Divvy trips by usertype and time of day',
       subtitle = '0 = not rush hour, 1 = rush hour',
       x = 'Date',
       y = 'Trips') +
  theme_classic()

# save plot
ggsave('images/trips_per_day_by_time.png')

##------------------------------------------------------------------------------
## * NUMBER OF DIVVY TRIPS PER GENDER                            
##------------------------------------------------------------------------------
# generate plot
divvy_data %>%
  filter(usertype == 'Subscriber') %>%
  group_by(startdate) %>%
  count(gender) %>% 
  # There are only 40 obervations with 'Dependent' usertype,
  # for all of which birthyaer == 1979 and gender == male 
  ggplot(aes(x=startdate, y=n, color=gender)) +
  geom_line(alpha=.5) +
  geom_smooth() +
  # geom_vline(xintercept = wet_days, color='gray', alpha=.3) +
  scale_color_manual("",
                     breaks = c('Female', 'Male'),
                     values = c('#F8766D', '#00BFC4')) +
  labs(title = 'Daily Divvy trips by gender',
       x = 'Date',
       y = 'Trips') +
  theme_classic()

# save plot
ggsave('images/trips_per_gender.png')


##------------------------------------------------------------------------------
## * TRIP DURATION PER DAY                                       
##------------------------------------------------------------------------------
# generate plot
divvy_data %>%
  # There are only 40 obervations with 'Dependent' usertype,
  # for all of which birthyaer == 1979 and gender == male 
  filter(usertype != 'Dependent') %>% 
  group_by(startdate) %>%
  summarise(mean = mean(tripduration),
            median = median(tripduration),
            std.dev = sd(tripduration)) %>% 
  ggplot(aes(x=startdate)) +
  geom_line(aes(y=mean, color='Mean')) +
  geom_line(aes(y=median, color='Median')) +
  geom_line(aes(y=std.dev, color = 'Std.Dev.'), alpha=.5) +
  scale_color_manual("",
                     breaks = c('Mean', 'Median', 'Std.Dev.'),
                     values = c('#F8766D', '#00BFC4', '#C77CFF')) +
  labs(title = 'Daily Divvy trip durations by day of week',
       subtitle = 'Mean, median, and standard deviation; 0 = weekends, 1 = weekdays',
       x = 'Date',
       y = 'Duration (in seconds)') +
  theme_classic()

# save plot
ggsave('images/duration_per_day.png')


##------------------------------------------------------------------------------
## * TRIP DURATION PER DAY (BY DAY OF WEEK)                      
##------------------------------------------------------------------------------
# generate plot
divvy_data %>%
  # There are only 40 obervations with 'Dependent' usertype,
  # for all of which birthyaer == 1979 and gender == male 
  filter(usertype != 'Dependent') %>% 
  group_by(startdate, startrush) %>%
  summarise(mean = mean(tripduration),
            median = median(tripduration),
            std.dev = sd(tripduration)) %>%
  ggplot(aes(x=startdate)) +
  geom_line(aes(y=mean, color='Mean')) +
  geom_line(aes(y=median, color='Median')) +
  geom_line(aes(y=std.dev, color = 'Std.Dev.'), alpha=.5) +
  facet_wrap(~startrush, nrow=2) + 
  scale_color_manual("",
                     breaks = c('Mean', 'Median', 'Std.Dev.'),
                     values = c('#F8766D', '#00BFC4', '#C77CFF')) +
  labs(title = 'Daily Divvy trip durations by time of day',
       subtitle = 'Mean, median, and standard deviation; 0 = not rush hours, 1 = rush hours',
       x = 'Date',
       y = 'Duration (in seconds)') +
  theme_classic()

# save plot
ggsave('images/duration_per_day_by_time.png')


##------------------------------------------------------------------------------
## * TRIP DURATION PER DAY (BY DAY OF WEEK)                      
##------------------------------------------------------------------------------
# generate plot
divvy_data %>%
  # There are only 40 obervations with 'Dependent' usertype,
  # for all of which birthyaer == 1979 and gender == male 
  filter(usertype != 'Dependent') %>% 
  group_by(startdate) %>%
  summarise(mean = mean(tripduration),
            median = median(tripduration),
            std.dev = sd(tripduration)) %>% 
  # Add weekday attribute to the data frame
  left_join(select(divvy_data, startdate, weekday) %>%
              group_by(startdate) %>%
              summarise(weekday = mean(weekday))) %>%
  ggplot(aes(x=startdate)) +
  geom_line(aes(y=mean, color='Mean')) +
  geom_line(aes(y=median, color='Median')) +
  geom_line(aes(y=std.dev, color = 'Std.Dev.'), alpha=.5) +
  facet_wrap(~weekday, nrow=2) + 
  scale_color_manual("",
                     breaks = c('Mean', 'Median', 'Std.Dev.'),
                     values = c('#F8766D', '#00BFC4', '#C77CFF')) +
  labs(title = 'Daily Divvy trip durations by day of week',
       subtitle = 'Mean, median, and standard deviation; 0 = weekends, 1 = weekdays',
       x = 'Date',
       y = 'Duration (in seconds)') +
  theme_classic()

# save plot
ggsave('images/duration_per_day_by_weekday.png')


##------------------------------------------------------------------------------
## * MAX AND MIN TEMPERATURE PER DAY                             
##------------------------------------------------------------------------------
# get a list of wet days (PRCP == 1)
wet_days <- divvy_data %>% 
  group_by(startdate) %>%
  summarise(PRCP = mean(PRCP)) %>%
  filter(PRCP == 1) %>%
  select(startdate)
wet_days <- wet_days$startdate %>%
  as.numeric()

# generate plot
divvy_data %>%
  group_by(startdate) %>%
  summarise(MAX = mean(TMAX),
            MIN = mean(TMIN),
            PRCP = mean(PRCP)) %>%
  ggplot(aes(x=startdate)) +
  geom_line(aes(y=MAX, color='Max')) +
  geom_line(aes(y=MIN, color='Min')) +
  geom_hline(yintercept = 32, linetype='dashed') +
  geom_vline(xintercept = wet_days, color='gray', alpha=.3) +
  scale_color_manual("",
                     breaks = c('Max', 'Min'),
                     values = c('#F8766D', '#00BFC4')) +
  labs(
    # title = 'Daily temperature',
    # subtitle = 'Dashed line marks the freezing degree; gray lines mark days with precipitation',
    x = 'Date',
    y = expression('Temperature in' ~degree~F)) +
  theme_classic()

# save plot
ggsave('images/temperature_per_day.png')


##------------------------------------------------------------------------------
## * DIVVY STATIONS AND CTA STOPS                             
##------------------------------------------------------------------------------
divvy_station$proximity <- ifelse(divvy_station$prox300 == 0, '>300m',
                                  ifelse((divvy_station$prox300 == 1 & divvy_station$prox200 == 0), "200-300m",
                                         ifelse((divvy_station$prox200 == 1 & divvy_station$prox100 == 0), "100-200m",
                                                ifelse((divvy_station$prox100 == 1 & divvy_station$prox50 == 0), "50-100m", "<50m"))))

chicago_map +
# ggplot() +
  geom_polygon(data=boundaries_cca,
               aes(x=long, y=lat, group=group),
               size=0.25,
               fill='grey',
               color='black',
               alpha=0.3) +
  geom_point(data = divvy_station,
             aes(x = lon, y = lat, color = proximity),
             size = 1,
             alpha = 0.7) +
  geom_polygon(data=boundaries_cbd,
               aes(x=long, y=lat, group=group),
               color='yellow',
               alpha=0) +
  xlim(c(-87.95, -87.5)) +
  ylim(c(41.63, 42.07)) +
  scale_color_discrete(limits=c('<50m','50-100m','100-200m','200-300m','>300m')) +
  labs(color = "Proximity") +
  # ggtitle("Divvy Stations by Proximity to CTA Stops",
  #         subtitle = "With different proximity standards") +
  xlab("Longitude") +
  ylab("Latitude")

# save plot
ggsave('images/fig_stations_by_proximity.png', width = 7.5, height = 5.7)

##------------------------------------------------------------------------------
## * DIVVY STATIONS, TRIPS ORIGINATING                             
##------------------------------------------------------------------------------
chicago_map +
  geom_polygon(data=boundaries_cca,
               aes(x=long, y=lat, group=group),
               size=0.25,
               fill='grey',
               color='black',
               alpha=0.3) +
  geom_point(data = divvy_data %>%
               select(from_station_name, from_lat, from_lon) %>%
               group_by(from_station_name) %>%
               mutate(trip = n()) %>%
               filter(!duplicated(from_station_name)),
             aes(x = from_lon, y = from_lat, size = trip),
             alpha = .4,
             color = "#F8766D") +
  geom_polygon(data=boundaries_cbd,
               aes(x=long, y=lat, group=group),
               fill='white',
               color='yellow',
               alpha=0) +
  xlim(c(-87.95, -87.5)) +
  ylim(c(41.63, 42.07)) +
  # ggtitle("Divvy Stations, Trips Originating") +
  labs(size = "Trips originating") +
  xlab("Longitude") +
  ylab("Latitude")

# save plot
ggsave('images/fig_stations_trips_originating.png', width = 7.5, height = 6)

##------------------------------------------------------------------------------
## * DIVVY STATIONS, TRIPS TERMINATING                             
##------------------------------------------------------------------------------
# counts
chicago_map +
  geom_polygon(data=boundaries_cca,
               aes(x=long, y=lat, group=group),
               size=0.25,
               fill='grey',
               color='black',
               alpha=0.3) +
  geom_point(data = divvy_data  %>%
             select(to_station_name, to_lat, to_lon) %>%
             group_by(to_station_name) %>%
             mutate(trip = n()) %>%
             filter(!duplicated(to_station_name)),
           aes(x = to_lon, y = to_lat, size = trip),
           alpha = .4,
           color = "#00BFC4") +
  geom_polygon(data=boundaries_cbd,
               aes(x=long, y=lat, group=group),
               fill='white',
               color='yellow',
               alpha=0) +
  xlim(c(-87.95, -87.5)) +
  ylim(c(41.63, 42.07)) +
  # ggtitle("Divvy Stations, Trips Terminating") +
  labs(size = "Trips terminating") + 
  xlab("Longitude") +
  ylab("Latitude")

# save plot
ggsave('images/fig_stations_trips_terminating.png', width = 7.5, height = 6)


##------------------------------------------------------------------------------
## * DIVVY TRIPS, DURATION AND DISTANCE                             
##------------------------------------------------------------------------------
divvy_data %>%
  filter(usertype != 'Dependent') %>%
  ggplot(aes(x=tripdistance/1000, y=tripduration/60, color=usertype)) +
  geom_point(alpha=0.2, size=1) +
  geom_smooth() +
  facet_wrap(~usertype) +
  # guides(color=guide_legend(title=NULL)) +
  guides(color=FALSE) +
  labs(x = 'Distance (m)',
       y = 'Duration (min)') +
  theme_classic()

# save plot
ggsave('images/divvy_trip_duration_distance.png')

##------------------------------------------------------------------------------
## * DIVVY TRIPS, AGE DISTRIBUTION                             
##------------------------------------------------------------------------------
divvy_data %>%
  filter(usertype=='Subscriber',
         gender != is.na(gender)) %>%
  ggplot(aes(x=(2016-birthyear), fill=gender)) +
  geom_histogram(binwidth=1) +
  geom_vline(xintercept=80, linetype='dashed') +
  annotate('text', label = 'Age = 80',
           x = 80, y = 10000, color = 'red', size = 3.5) +
  guides(fill=guide_legend(title=NULL)) +
  labs(x = 'Age',
       y = 'Trips') +
  theme_classic()

# save plot
ggsave('images/divvy_trips_age_dist.png')


##------------------------------------------------------------------------------
## * PMM DIVVY TRIPS BY PROXIMITY,  TRIPS ORIGINATING                            
##------------------------------------------------------------------------------
plotTripsByProximity <- function(divvy_data, proximity='50m', stage='from'){
  # filter data by proximity value
  if(proximity == '50m'){
    filtered <- divvy_data %>%
      filter(mm50 == 1)
  } else if(proximity == '100m') {
    filtered <- divvy_data %>%
      filter(mm100 == 1)
  } else if(proximity == '200m') {
    filtered <- divvy_data %>%
      filter(mm200 == 1)
  } else {
    filtered = divvy_data %>%
      filter(mm300 == 1)
  }
  
  # prepare for plotting by stage
  if(stage == 'from'){
    data = filtered %>%
      group_by(from_station_id) %>%
      mutate(trip = n()) %>%
      filter(!duplicated(from_station_id))

    data$lon <- data$from_lon
    data$lat <- data$from_lat

    color <- "#F8766D"
    lab <- "Trips originating"
  } else {
    data = filtered %>%
      group_by(to_station_id) %>%
      mutate(trip = n()) %>%
      filter(!duplicated(to_station_id))

    data$lon <- data$to_lon
    data$lat <- data$to_lat

    color <- "#00BFC4"
    lab <- "Trips terminating"
  }
  
  # plot the map
  chicago_map +
    geom_polygon(data=boundaries_cca,
                 aes(x=long, y=lat, group=group),
                 size=0.25,
                 fill='grey',
                 color='black',
                 alpha=0.3) +
    geom_point(data = data,
               aes(x = lon, y = lat, size = trip),
               alpha = .4,
               color = color) +
    scale_size_continuous(breaks = c(0,50,100,150,200)) +
    geom_polygon(data=boundaries_cbd,
                 aes(x=long, y=lat, group=group),
                 fill='white',
                 color='yellow',
                 alpha=0) +
    xlim(c(-87.95, -87.5)) +
    ylim(c(41.6, 42.05)) +
    ggtitle(proximity) +
    labs(size = lab) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(legend.position = "bottom")
}

from_mm50 <- plotTripsByProximity(divvy_from_train, proximity='50m', stage='from')
ggsave('images/fig_pmm_from50.png')
from_mm100 <- plotTripsByProximity(divvy_from_train, proximity='100m', stage='from')
ggsave('images/fig_pmm_from100.png')
from_mm200 <- plotTripsByProximity(divvy_from_train, proximity='200m', stage='from')
ggsave('images/fig_pmm_from200.png')
from_mm300 <- plotTripsByProximity(divvy_from_train, proximity='300m', stage='from')
ggsave('images/fig_pmm_from300.png')

##------------------------------------------------------------------------------
## * PMM DIVVY TRIPS BY PROXIMITY,  TRIPS TERMINATING                            
# ##------------------------------------------------------------------------------
to_mm50 <- plotTripsByProximity(divvy_to_train, proximity='50m', stage='to')
ggsave('images/fig_pmm_to50.png')
to_mm100 <- plotTripsByProximity(divvy_to_train, proximity='100m', stage='to')
ggsave('images/fig_pmm_to100.png')
to_mm200 <- plotTripsByProximity(divvy_to_train, proximity='200m', stage='to')
ggsave('images/fig_pmm_to200.png')
to_mm300 <- plotTripsByProximity(divvy_to_train, proximity='300m', stage='to')
ggsave('images/fig_pmm_to300.png')


# Clear the environment---------------------------------------------------------
# rm(list=ls()) # clear the environment
