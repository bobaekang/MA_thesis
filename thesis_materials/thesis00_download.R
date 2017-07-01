##------------------------------------------------------------------------------
## Bobae's MA Thesis (github.com/bobaekang/MA_thesis)
## Researcher: Bobae Kang (github.com/bobaekang)    
## Advisor: Benjamin Soltoff (github.com/bensoltoff)
##------------------------------------------------------------------------------
## Script: thesis00_download.R
## Last updated: 4/3/17
##------------------------------------------------------------------------------
## This script performs the following tasks: 
## 1. Downloading and unzipping raw data sets from Divvy and CTA that are
## necessary for the current project and used in the scripts that follow this.
##------------------------------------------------------------------------------

# Load packages
library(downloader)
library(stringr)

##------------------------------------------------------------------------------
## 1, DOWNLOAD AND UNZIP DATA
## The following download Divvy and CTA data
##------------------------------------------------------------------------------

# 1.0. Download data------------------------------------------------------------
# Divvy data
download(url = "https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2016_Q1Q2.zip",
         destfile = "rawdata/Divvy_Trips_2016_Q1Q2.zip")
download(url = "https://s3.amazonaws.com/divvy-data/tripdata/Divvy_Trips_2016_Q3Q4.zip",
         destfile = "rawdata/Divvy_Trips_2016_Q3Q4.zip")

# CTA data: stops and schedule
download(url = "http://www.transitchicago.com/downloads/sch_data/google_transit.zip",
         destfile = "rawdata/google_transit.zip")


# 1.1 unzip the files-----------------------------------------------------------
datazip <- list.files("rawdata", pattern = "\\.zip$")
for (zipfile in datazip){
  filepath = str_c("rawdata/", zipfile)
  unzip(filepath, exdir = "rawdata")
}

# take out the Divvy data for Q1Q2 from a separate folder
file.rename(from = "rawdata/Divvy_Trips_2016_Q1Q2/README.txt",
            to = "rawdata/Divvy_Trips_2016_Q1Q2/README_2016_Q1Q2.txt")
divvy_Q1Q2_files <- list.files("rawdata/Divvy_Trips_2016_Q1Q2")
for(filename in divvy_Q1Q2_files){
  frompath <- str_c("rawdata/Divvy_Trips_2016_Q1Q2/", filename)
  topath <- str_c ("rawdata/", filename)
  file.rename(from = frompath, to = topath)
}
unlink("rawdata/Divvy_Trips_2016_Q1Q2", recursive = TRUE) # delete the now empty directory


# Clear the environment---------------------------------------------------------
rm(list=ls()) # clear the environment