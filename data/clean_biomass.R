# Clean biomass
## Script to clean biomass data for the mesquite comparison

## load packages
library(tidyverse)
library(reshape2)

## read in data
bm_raw = read.csv('raw/biomass.csv')
head(bm_raw)
nrow(bm_raw)

### will need to create new dataframe with date, plot, location, species, and weight

## date
bm_date = rep(as.Date('2018-09-22'), times = 218)
  
## plot
bm_plot = bm_raw$plot

## location
bm_location = as.character(bm_raw$X.A.U.)
bm_location[bm_location == 'A'] <- 'away'
bm_location[bm_location == 'a'] <- 'away'
bm_location[bm_location == 'a '] <- 'away'
bm_location[bm_location == 'u'] <- 'under'
bm_location[bm_location == 'U'] <- 'under'

## species
bm_species_fac = bm_raw$Species
bm_species_fac_levels = levels(bm_species_fac)
bm_species = as.character(bm_raw$Species)
bm_species[bm_species == bm_species_fac_levels[1] | 
             bm_species == bm_species_fac_levels[2]] <- 'AmPS'
bm_species[bm_species == bm_species_fac_levels[3] | 
             bm_species == bm_species_fac_levels[4] |
             bm_species == bm_species_fac_levels[5] | 
             bm_species == bm_species_fac_levels[6] |
             bm_species == bm_species_fac_levels[7] | 
             bm_species == bm_species_fac_levels[8]] <- 'BoGr'
bm_species[bm_species == bm_species_fac_levels[9] | 
             bm_species == bm_species_fac_levels[10] |
             bm_species == bm_species_fac_levels[11] | 
             bm_species == bm_species_fac_levels[12] |
             bm_species == bm_species_fac_levels[13] | 
             bm_species == bm_species_fac_levels[14] |
             bm_species == bm_species_fac_levels[15] | 
             bm_species == bm_species_fac_levels[16] |
             bm_species == bm_species_fac_levels[17]] <- 'ChLa'
bm_species[bm_species == bm_species_fac_levels[18] | 
             bm_species == bm_species_fac_levels[19] |
             bm_species == bm_species_fac_levels[20] | 
             bm_species == bm_species_fac_levels[21]] <- 'LITT'
bm_species[bm_species == bm_species_fac_levels[22] | 
             bm_species == bm_species_fac_levels[23] |
             bm_species == bm_species_fac_levels[24] | 
             bm_species == bm_species_fac_levels[25] |
             bm_species == bm_species_fac_levels[26] | 
             bm_species == bm_species_fac_levels[27] |
             bm_species == bm_species_fac_levels[28] | 
             bm_species == bm_species_fac_levels[29]] <- 'GuSa'
bm_species[bm_species == bm_species_fac_levels[30]] <- 'HeAn'
bm_species[bm_species == bm_species_fac_levels[31] |
             bm_species == bm_species_fac_levels[32]] <- 'LaCo'
bm_species[bm_species == bm_species_fac_levels[33] | 
             bm_species == bm_species_fac_levels[34] |
             bm_species == bm_species_fac_levels[35] | 
             bm_species == bm_species_fac_levels[36]] <- 'PoMu'
bm_species[bm_species == bm_species_fac_levels[37]] <- 'PrGl'
bm_species[bm_species == bm_species_fac_levels[38] |
             bm_species == bm_species_fac_levels[39]] <- 'RaCo'
bm_species[bm_species == bm_species_fac_levels[40]] <- 'SaTr'
bm_species[bm_species == bm_species_fac_levels[41] | 
             bm_species == bm_species_fac_levels[42] |
             bm_species == bm_species_fac_levels[43] | 
             bm_species == bm_species_fac_levels[44] |
             bm_species == bm_species_fac_levels[45] | 
             bm_species == bm_species_fac_levels[46]] <- 'SoEl'
bm_species[bm_species == bm_species_fac_levels[47] | 
             bm_species == bm_species_fac_levels[48] |
             bm_species == bm_species_fac_levels[49] | 
             bm_species == bm_species_fac_levels[50]] <- 'ZiGr'           


## weight
bm_weight = bm_raw$Weight.g.

## check sizes and combine
length(bm_date)
length(bm_plot)
length(bm_location)
length(bm_species)
length(bm_weight)

bm_clean = data.frame(date = bm_date, plot = bm_plot, location = bm_location)
bm_clean$code = bm_species
bm_clean$weight = bm_weight
head(bm_clean)
tail(bm_clean)

## add additional species information using the code
sp = read.csv('species.csv')

bm_clean$binomial = 'NA'
bm_clean$lifeform = 'NA'
bm_clean$family = 'NA'

for (i in 1:length(bm_clean$binomial)){
  
  bm_clean$binomial[i] <- as.character(subset(sp, Code == as.character(bm_clean$code[i]))$Latin.binomial)
  bm_clean$lifeform[i] <- as.character(subset(sp, Code == as.character(bm_clean$code[i]))$Life.form)
  bm_clean$family[i] <- as.character(subset(sp, Code == as.character(bm_clean$code[i]))$Family)
  
}

bm_clean$genus = vapply(strsplit(bm_clean$binomial, " ", fixed = TRUE), "[", "", 1)
bm_clean$species = vapply(strsplit(bm_clean$binomial, " ", fixed = TRUE), "[", "", 2)

## check and write out
head(bm_clean)
tail(bm_clean)


# bm_clean = arrange(bm_clean, date, plot, location, code)
# write.csv(bm_clean, 'bm_clean.csv')
