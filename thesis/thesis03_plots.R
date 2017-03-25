##---------------------------------------------------------------##
## Bobae's MA Thesis (github.com/bobaekang/MA_thesis)            ##
## Researcher: Bobae Kang (github.com/bobaekang)                 ##
## Advisor: Benjamin Soltoff (github.com/bensoltoff)             ##
##---------------------------------------------------------------##
## Script: thesis03_plots.R                                      ##
## Last updated: 3/24/17                                         ##
##---------------------------------------------------------------##
## This script generates the following plots:                    ##
## * Number of Divvy trips per day                               ##
## * Number of Divvy trips per gender                            ##
## * Trip duration per day                                       ##
## * Max and min temperature per day                             ##
##---------------------------------------------------------------##
## This script requires the outputs from the following scripts:  ##
## * thesis01_prepare1.R                                         ##
##---------------------------------------------------------------##

# import pacakges
library(tidyverse)

# import data
divvy_data <- read_feather("data/Divvy_data.feather") 


##--------------------------------------------------------------------------------------------------##
## * NUMBER OF DIVVY TRIPS PER DAY                                                                  ##
##--------------------------------------------------------------------------------------------------##

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
  scale_color_manual("",
                     breaks = c('Customer', 'Subscriber'),
                     values = c('#F8766D', '#00BFC4')) +
  labs(title = 'Daily Divvy trips by usertype',
       x = 'Date',
       y = 'Trips') +
  theme_classic()

# save plot
ggsave('images/trips_per_day.png')


##--------------------------------------------------------------------------------------------------##
## * NUMBER OF DIVVY TRIPS PER GENDER                                                               ##
##--------------------------------------------------------------------------------------------------##

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
  scale_color_manual("",
                     breaks = c('Female', 'Male'),
                     values = c('#F8766D', '#00BFC4')) +
  labs(title = 'Daily Divvy trips by gender',
       x = 'Date',
       y = 'Trips') +
  theme_classic()

# save plot
ggsave('images/trips_per_gender.png')


##--------------------------------------------------------------------------------------------------##
## * TRIP DURATION PER DAY                                                                          ##
##--------------------------------------------------------------------------------------------------##

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
  labs(title = 'Daily Divvy trip durations',
       subtitle = 'Mean, median, and standard deviation',
       x = 'Date',
       y = 'Duration (in seconds)') +
  theme_classic()

# save plot
ggsave('images/duration_per_day.png')


##--------------------------------------------------------------------------------------------------##
## * MAX AND MIN TEMPERATURE PER DAY                                                                ##
##--------------------------------------------------------------------------------------------------##

# generate plot
divvy_data %>%
  group_by(startdate) %>%
  summarise(MAX = mean(TMAX),
            MIN = mean(TMIN)) %>%
  ggplot(aes(x=startdate)) +
  geom_line(aes(y=MAX, color='Max')) +
  geom_line(aes(y=MIN, color='Min')) +
  geom_hline(yintercept = 32, linetype='dashed') +
  scale_color_manual("",
                     breaks = c('Max', 'Min'),
                     values = c('#F8766D', '#00BFC4')) +
  labs(title = 'Daily temperature in 2016',
       subtitle = 'Dashed line marks the freezing degree',
       x = 'Date',
       y = expression('Temperature in' ~degree~F)) +
  theme_classic()

# save plot
ggsave('images/temperature_per_day.png')


# Clear the environment------------------------------------------------------------------------------#
rm(list=ls()) # clear the environment
