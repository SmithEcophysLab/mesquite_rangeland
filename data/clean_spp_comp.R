# Clean biomass
## Script to clean species composition data

## load packages
library(tidyverse)
library(reshape2)

## read in data
sc_raw = read.csv('raw/spp_comp_v2.csv')
head(sc_raw)
nrow(sc_raw)

### will need to create new dataframe with date, plot, location, code, and cover

## date
sc_date = rep(as.Date('2018-09-22'), times = 313)

## plot
sc_plot = sc_raw$plot

## location
sc_location = as.character(sc_raw$location)
sc_location[sc_location == 'A'] <- 'away'
sc_location[sc_location == 'U'] <- 'under'

## code
sc_code = as.character(sc_raw$code)
levels(as.factor(sc_code))

### convert PoPi to PoMu
sc_code[sc_code == "PoPi"] <- "PoMu"

## cover
sc_cover = sc_raw$cover

## check sizes and combine
length(sc_date)
length(sc_plot)
length(sc_location)
length(sc_code)
length(sc_cover)

sc_clean = data.frame(date = sc_date, plot = sc_plot, location = sc_location)
sc_clean$code = sc_code
sc_clean$cover = sc_cover
head(sc_clean)
tail(sc_clean)

## add additional species information using the code
sp = read.csv('species.csv')

sc_clean$binomial = 'NA'
sc_clean$lifeform = 'NA'
sc_clean$family = 'NA'

for (i in 1:length(sc_clean$binomial)){
  
  sc_clean$binomial[i] <- as.character(subset(sp, Code == as.character(sc_clean$code[i]))$Latin.binomial)
  sc_clean$lifeform[i] <- as.character(subset(sp, Code == as.character(sc_clean$code[i]))$Life.form)
  sc_clean$family[i] <- as.character(subset(sp, Code == as.character(sc_clean$code[i]))$Family)
  
}

sc_clean$genus = vapply(strsplit(sc_clean$binomial, " ", fixed = TRUE), "[", "", 1)
sc_clean$species = vapply(strsplit(sc_clean$binomial, " ", fixed = TRUE), "[", "", 2)

## check and write out
head(sc_clean)
tail(sc_clean)


# sc_clean = arrange(sc_clean, date, plot, location, code)
# write.csv(sc_clean, 'sc_clean.csv')