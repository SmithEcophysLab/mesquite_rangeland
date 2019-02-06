# Clean soil moisture
## script to clean the soil moisture data for the mesquite comparison

## load packages
library(tidyverse)

## read in data
sm_raw = read.csv('raw/soil_moisture.csv')
head(sm_raw)
nrow(sm_raw)

## make a new data frame
sm_colnames = c('day', 'month', 'year', 'doy', 'date', 'plot', 'location', 'vwc')
sm_date = c(rep(as.Date('2018-07-27'), times = 80), 
            rep(as.Date('2018-08-03'), times = 80), 
            rep(as.Date('2018-08-09'), times = 80), 
            rep(as.Date('2018-08-16'), times = 80), 
            rep(as.Date('2018-08-24'), times = 80), 
            rep(as.Date('2018-08-30'), times = 80), 
            rep(as.Date('2018-09-07'), times = 80), 
            rep(as.Date('2018-09-16'), times = 80))
