##------------------------------------------------------------------------------
## Bobae's MA Thesis (github.com/bobaekang/MA_thesis)            
## Researcher: Bobae Kang (github.com/bobaekang)                 
## Advisor: Benjamin Soltoff (github.com/bensoltoff)             
##------------------------------------------------------------------------------
## Script: thesis03_plots.R                                      
## Last updated: 3/24/17                                         
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
## * Divvy stations, trips origination
## * Divvy stations, trips terminating
##------------------------------------------------------------------------------
## This script requires the outputs from the following scripts:  
## * thesis01_prepare1.R                                         
##------------------------------------------------------------------------------

# import pacakges
library(tidyverse)
library(ggmap)

# import data
divvy_data <- read_feather("data/Divvy_data.feather") 


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
  labs(title = 'Daily temperature',
       subtitle = 'Dashed line marks the freezing degree; gray lines mark days with precipitation',
       x = 'Date',
       y = expression('Temperature in' ~degree~F)) +
  theme_classic()

# save plot
ggsave('images/temperature_per_day.png')


##------------------------------------------------------------------------------
## * DIVVY STATIONS AND CTA STOPS                             
##------------------------------------------------------------------------------

# chicago map
chicago_map <- ggmap(
  get_googlemap(
    center = c(lon = -87.68, lat = 41.91),
    zoom = 11))

chicago_map +
  geom_point(data = divvy_station,
             aes(x = lon, y = lat),
             alpha = .5,
             size = 2,
             color = "blue") +
  geom_point(data = cta_stops,
             aes(x = stop_lon, y = stop_lat),
             alpha = .1,
             size = 1,
             color = "red") +
  ggtitle("Locations of Divvy Stations and CTA Stops") +
  xlab("Longitude") +
  ylab("Latitude")


##------------------------------------------------------------------------------
## * DIVVY STATIONS AND CTA STOPS                             
##------------------------------------------------------------------------------

divvy_station$proximity <- ifelse(divvy_station$prox300 == 0, '>300m',
                                  ifelse((divvy_station$prox300 == 1 & divvy_station$prox200 == 0), "200-300m",
                                         ifelse((divvy_station$prox200 == 1 & divvy_station$prox100 == 0), "100-200m",
                                                ifelse((divvy_station$prox100 == 1 & divvy_station$prox50 == 0), "50-100m", "<50m"))))

chicago_map +
  geom_point(data = divvy_station,
             aes(x = lon, y = lat, color = proximity),
             size = 2) +
  scale_color_discrete(limits=c('<50m','50-100m','100-200m','200-300m','>300m')) +
  labs(color = "Proximity") +
  ggtitle("Divvy Stations by Proximity to CTA Stops",
          subtitle = "With different proximity standards") +
  xlab("Longitude") +
  ylab("Latitude")

# save plot
ggsave('images/divvy_stations_by_proximity.png')


##------------------------------------------------------------------------------
## * DIVVY STATIONS, TRIPS ORIGINATING                             
##------------------------------------------------------------------------------
chicago_map +
  geom_point(data = divvy_data %>%
               select(from_station_name, from_lat, from_lon) %>%
               group_by(from_station_name) %>%
               mutate(trip = n()) %>%
               filter(!duplicated(from_station_name)),
             aes(x = from_lon, y = from_lat, size = trip),
             alpha = .5,
             color = "#F8766D") +
  ggtitle("Divvy Stations, Trips Originating") +
  labs(size = "Trips originating") +
  xlab("Longitude") +
  ylab("Latitude")

# save plot
ggsave('images/divvy_stations_trips_originating.png')


##------------------------------------------------------------------------------
## * DIVVY STATIONS, TRIPS TERMINATING                             
##------------------------------------------------------------------------------
chicago_map +
  geom_point(data = divvy_data  %>%
               select(to_station_name, to_lat, to_lon) %>%
               group_by(to_station_name) %>%
               mutate(trip = n()) %>%
               filter(!duplicated(to_station_name)),
             aes(x = to_lon, y = to_lat, size = trip),
             alpha = .5,
             color = "#00BFC4") +
    ggtitle("Divvy Stations, Trips Terminating") +
    labs(size = "Trips terminating") + 
    xlab("Longitude") +
    ylab("Latitude")

# save plot
ggsave('images/divvy_stations_trips_terminating.png')

# Clear the environment---------------------------------------------------------
# rm(list=ls()) # clear the environment
