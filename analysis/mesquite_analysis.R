# Scipt for analyzing the mequite data
## The script proceeds as stated questions followed by the code to analyze the data
## in response to that stated question

## load libraries
library(tidyverse)
library(vioplot)
library(lme4)
library(car)
library(emmeans)

## read in data
sm = read.csv("../data/sm_clean.csv") # soil moisture
bm = read.csv("../data/bm_clean.csv") # biomass
lai = read.csv("../data/lai_clean.csv") # lai

##########################################################################################
### Q1: What does the size distribution (in LAI) of the trees look like?
##########################################################################################
#### create a histogram
hist(lai$lai,breaks = 20) 

##########################################################################################
### A1: LAI is fairly normal (center just below 1), but quite a big spread
##########################################################################################

##########################################################################################
### Q2: How does mesquite presence influence soil moisture?
##########################################################################################
#### run paired t-tests for all data and each date
sm_dates = levels(sm$date)
t.test(subset(sm, location == 'under')$vwc, subset(sm, location == 'away')$vwc,
       paired = T)
t.test(subset(sm, location == 'under' & date == sm_dates[1])$vwc, 
       subset(sm, location == 'away' & date == sm_dates[1])$vwc,
       paired = T)
t.test(subset(sm, location == 'under' & date == sm_dates[2])$vwc, 
       subset(sm, location == 'away' & date == sm_dates[2])$vwc,
       paired = T)
t.test(subset(sm, location == 'under' & date == sm_dates[3])$vwc, 
       subset(sm, location == 'away' & date == sm_dates[3])$vwc,
       paired = T)
t.test(subset(sm, location == 'under' & date == sm_dates[4])$vwc, 
       subset(sm, location == 'away' & date == sm_dates[4])$vwc,
       paired = T)
t.test(subset(sm, location == 'under' & date == sm_dates[5])$vwc, 
       subset(sm, location == 'away' & date == sm_dates[5])$vwc,
       paired = T)
t.test(subset(sm, location == 'under' & date == sm_dates[6])$vwc, 
       subset(sm, location == 'away' & date == sm_dates[6])$vwc,
       paired = T)
t.test(subset(sm, location == 'under' & date == sm_dates[7])$vwc, 
       subset(sm, location == 'away' & date == sm_dates[7])$vwc,
       paired = T)
t.test(subset(sm, location == 'under' & date == sm_dates[8])$vwc, 
       subset(sm, location == 'away' & date == sm_dates[8])$vwc,
       paired = T)

#### plot the data
sm_vioplot = vioplot(subset(sm, location == 'under')$vwc, 
                     subset(sm, location == 'away')$vwc,
                     ylim = c(0, 40), yaxt = 'n',
                     names = c('Under', 'Away'),
                     col = c('grey', 'yellow'))
axis(2, seq(0, 40, 10), las = 1)

sm_vioplot_date = plot(NULL, ylim = c(0, 40), xlim = c(1, 16), yaxt = 'n', xaxt = 'n',
                       ylab = '', xlab = '')
sm_vioplot_date = vioplot(subset(sm, location == 'under' & date == sm_dates[1])$vwc, 
                     subset(sm, location == 'away' & date == sm_dates[1])$vwc,
                     subset(sm, location == 'under' & date == sm_dates[2])$vwc, 
                     subset(sm, location == 'away' & date == sm_dates[2])$vwc,
                     subset(sm, location == 'under' & date == sm_dates[3])$vwc, 
                     subset(sm, location == 'away' & date == sm_dates[3])$vwc,
                     subset(sm, location == 'under' & date == sm_dates[4])$vwc, 
                     subset(sm, location == 'away' & date == sm_dates[4])$vwc,
                     subset(sm, location == 'under' & date == sm_dates[5])$vwc, 
                     subset(sm, location == 'away' & date == sm_dates[5])$vwc,
                     subset(sm, location == 'under' & date == sm_dates[6])$vwc, 
                     subset(sm, location == 'away' & date == sm_dates[6])$vwc,
                     subset(sm, location == 'under' & date == sm_dates[7])$vwc, 
                     subset(sm, location == 'away' & date == sm_dates[7])$vwc,
                     subset(sm, location == 'under' & date == sm_dates[8])$vwc, 
                     subset(sm, location == 'away' & date == sm_dates[8])$vwc,
                     add = T,
                     col = rep(c('grey', 'yellow'), times = 8),
                     ylab = '', xlab = '')
axis(2, seq(0, 40, 10), las = 1)
axis(1, seq(1, 8, 1), at = seq(1.5, 15.5, 2))
mtext(side = 1, 'Measurement Period', line = 3)
mtext(side = 2, 'VWC (%)', line = 3)
legend('topright', c('Under', 'Away'), pch = 16, col = c('grey', 'yellow'))

##########################################################################################
### A2: It's wetter outside of the mesquite area
##########################################################################################

##########################################################################################
### Q3: Is biomass different under the mesquite?
##########################################################################################
#### total biomass
##### get total biomass for each plot
bm_total_group_by = group_by(bm, date, plot, location)
bm_total = summarise(bm_total_group_by, weight_sum = sum(weight))

##### run the analysis
t.test(subset(bm_total, location == 'under')$weight_sum, 
       subset(bm_total, location == 'away')$weight_sum,
       paired = T)

##########################################################################################
### A3: Lower under the mesquite
##########################################################################################
