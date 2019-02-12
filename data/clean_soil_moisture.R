# Clean soil moisture
## script to clean the soil moisture data for the mesquite comparison

## load packages
library(tidyverse)

## read in data
sm_raw = read.csv('raw/soil_moisture.csv')
head(sm_raw)
nrow(sm_raw)

## make a new data frame
sm_date = c(rep(as.Date('2018-07-27'), times = 80), 
            rep(as.Date('2018-08-03'), times = 80), 
            rep(as.Date('2018-08-09'), times = 80), 
            rep(as.Date('2018-08-16'), times = 80), 
            rep(as.Date('2018-08-24'), times = 80), 
            rep(as.Date('2018-08-30'), times = 80), 
            rep(as.Date('2018-09-07'), times = 80), 
            rep(as.Date('2018-09-16'), times = 80))
sm_plot = c(rep(seq(1, 40, 1), times = 16))
sm_location = rep(c(rep('under', times = 40), rep('away', times = 40)), times = 8)
sm_vwc_rows = c(2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24)
sm_vwc = c(as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[1]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[2]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[3]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[4]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[5]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[6]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[7]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[8]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[9]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[10]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[11]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[12]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[13]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[14]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[15]])),
           as.numeric(as.character(sm_raw[2:41, sm_vwc_rows[16]])))

# combine it all together
sm_clean = expand.grid(date = sm_date, plot = sm_plot)
sm_clean$location = sm_location
sm_clean$vwc = sm_vwc
head(sm_clean)
