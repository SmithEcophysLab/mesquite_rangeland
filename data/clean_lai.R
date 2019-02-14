# Clean mesquite LAI data
## script to clean the LAI data for the mesquite comparison

## load packages
library(tidyverse)
library(reshape2)

## read in data
lai_raw = read.csv('clean_lai/lai_output.csv')
head(lai_raw)
nrow(lai_raw)

## will need to create a data frame with date, plot, and LAI

## date
lai_date = rep(as.Date('2018-09-22'), times = nrow(lai_raw)) # for all 40 plots

## add remarks to LAI data (to indicate plot #)
lai_remarks = read.csv('clean_lai/lai_remarks.csv')

lai_raw$remark = 'NA'
for (i in 1:length(lai_raw$B_Obs)){
  
  lai_raw$remark[lai_raw$B_Obs == lai_raw$B_Obs[i]] <- as.character(lai_remarks$Remark[lai_remarks$B_Obs == lai_raw$B_Obs[i]])
  
}

lai_plot = as.numeric(sapply(strsplit(lai_raw$remark, 'P'), '[', 2))

## combine it all together
lai_clean = data.frame(date = lai_date, plot = lai_plot, lai = lai_raw$LAI)

## check and write out
head(lai_clean)
tail(lai_clean)

# lai_clean = arrange(lai_clean, date, plot)
# write.csv(lai_clean, 'lai_clean.csv')