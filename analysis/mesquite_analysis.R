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
### Q3a: Is biomass different under the mesquite?
##########################################################################################
#### total biomass
##### make a dataframe with no litter
bm_nolitt = subset(bm, code != 'LITT')

##### create dateframe with correct columns for total biomass
bm_total_pre = data.frame(plot = rep(seq(1, 40), 2), 
                      location = c(rep('under', 40), rep('away', 40)),
                      date = rep('2018-09-22', times = 80))
nrow(bm_total_pre)

##### get total biomass for each plot (excluding dead stuff)
bm_total_group_by = group_by(bm_nolitt, date, plot, location)
bm_total_sums = summarise(bm_total_group_by, weight_sum = sum(weight))

##### combine sums into data fram with zeros
bm_total = left_join(bm_total_pre, bm_total_sums)
head(bm_total)
bm_total$weight_sum[is.na(bm_total$weight_sum)] = 0
head(bm_total)
tail(bm_total)

##### run the analysis
t.test(subset(bm_total, location == 'under')$weight_sum, 
       subset(bm_total, location == 'away')$weight_sum,
       paired = T)

##########################################################################################
### A3a: Slightly lower under the mesquite, but not significantly different
##########################################################################################

##########################################################################################
### Q3b: Does biomass result vary by lifeform?
##########################################################################################
##### create dateframe with correct columns for biomass by lifeform type
bm_lifeform_pre = data.frame(plot = rep(seq(1, 40), 8), 
                          location = c(rep('under', 160), rep('away', 160)),
                          lifeform = rep(c(rep(levels(bm$lifeform)[1], 40),
                                           rep(levels(bm$lifeform)[2], 40),
                                           rep(levels(bm$lifeform)[3], 40),
                                           rep(levels(bm$lifeform)[4], 40)), 2),
                          date = rep('2018-09-22', times = 320))
nrow(bm_lifeform_pre)

##### get lifeform biomass for each plot
bm_lifeform_group_by = group_by(bm, date, plot, location, lifeform)
bm_lifeform_sums = summarise(bm_lifeform_group_by, weight_sum = sum(weight))

##### combine sums into data fram with zeros
bm_lifeform = left_join(bm_lifeform_pre, bm_lifeform_sums)
head(bm_lifeform)
bm_lifeform$weight_sum[is.na(bm_lifeform$weight_sum)] = 0
head(bm_lifeform)
tail(bm_lifeform)

##### separate out the different lifeforms
bm_lifeform_forb = subset(bm_lifeform, lifeform == 'Forb')
bm_lifeform_grass = subset(bm_lifeform, lifeform == 'Grass')
bm_lifeform_litter = subset(bm_lifeform, lifeform == 'LITT')
bm_lifeform_tree = subset(bm_lifeform, lifeform == 'Shrub/tree')

##### run the analysis
t.test(subset(bm_lifeform_forb, location == 'under')$weight_sum, 
       subset(bm_lifeform_forb, location == 'away')$weight_sum,
       paired = T) # slightly more fobs under
t.test(subset(bm_lifeform_grass, location == 'under')$weight_sum, 
       subset(bm_lifeform_grass, location == 'away')$weight_sum,
       paired = T) # more grass away!
t.test(subset(bm_lifeform_litter, location == 'under')$weight_sum, 
       subset(bm_lifeform_litter, location == 'away')$weight_sum,
       paired = T) # more litter away!
t.test(subset(bm_lifeform_tree, location == 'under')$weight_sum, 
       subset(bm_lifeform_tree, location == 'away')$weight_sum,
       paired = T) # nothing with trees (only one tree plot)

##########################################################################################
### A3b: less grass and more forbs under the mesquite
##########################################################################################


##########################################################################################
### Q3c: are there biomass differences by family (fabaceae in particular)?
##########################################################################################
#### are there many legumes?
levels(bm_nolitt$family)
subset(bm_nolitt, family == 'Fabaceae') # nope...

##########################################################################################
### A3c: not enough fabaceae for an interesting comparison
##########################################################################################


##########################################################################################
### Q4c: is species richness different under the mesquite?
##########################################################################################
#### 
bm_richness_pre = data.frame(plot = rep(seq(1, 40), 2), 
                          location = c(rep('under', 40), rep('away', 40)),
                          date = rep('2018-09-22', times = 80))
nrow(bm_richness_pre)

#### calculate species richness per plot type (for non-litter)
bm_richness_group_by = group_by(bm_nolitt, date, plot, location)
bm_richness_sums = summarise(bm_richness_group_by, richness = n())

##### combine richness into data fram with zeros
bm_richness = left_join(bm_richness_pre, bm_richness_sums)
head(bm_richness)
bm_richness$richness[is.na(bm_richness$richness)] = 0
head(bm_richness)
tail(bm_richness)

##### run the analysis
t.test(subset(bm_richness, location == 'under')$richness, 
       subset(bm_richness, location == 'away')$richness,
       paired = T)

##########################################################################################
### A4c: species richness is generally the same
##########################################################################################

##########################################################################################
### Q5a: does tree size matter for soil moisture effect?
##########################################################################################
#### calculate average soil moisture effect across dates and create new data frame
sm_under = subset(sm, location == "under")
sm_away = subset(sm, location == "away")
sm_effect = ((sm_away$vwc - sm_under$vwc) / sm_away$vwc) * 100
sm_effect_df = data.frame(sm_under[, c(2,3)], sm_effect = sm_effect)

sm_effect_df_group_by_plot = group_by(sm_effect_df, plot)
sm_effect_df_mean = summarise(sm_effect_df_group_by_plot, 
                              sm_effect_mean = mean(sm_effect))

#### get average lai for each tree
lai_group_by_plot = group_by(lai, plot)
lai_mean = summarise(lai_group_by_plot, lai_mean = mean(lai))
head(lai_mean)
nrow(lai_mean)

sm_effect_df_mean_lai = left_join(sm_effect_df_mean, lai_mean)
head(sm_effect_df_mean_lai)
nrow(sm_effect_df_mean_lai)

#### run regression
plot(sm_effect_df_mean_lai$sm_effect_mean ~ 
       sm_effect_df_mean_lai$lai_mean) # not much there
sm_effect_df_mean_lai_lm = lm(sm_effect_df_mean_lai$sm_effect_mean ~ 
                                sm_effect_df_mean_lai$lai_mean) # not much there
anova(sm_effect_df_mean_lai_lm) # nada

##########################################################################################
### A5a: tree size does not matter for soil moisture effect
##########################################################################################


##########################################################################################
### Q5b: does tree size matter for biomass effect?
##########################################################################################
#### calculate average total biomass effect across dates and create new data frame
bm_total_under = subset(bm_total, location == "under")
bm_total_away = subset(bm_total, location == "away")
bm_total_effect = ((bm_total_away$weight_sum - bm_total_under$weight_sum) / 
                     bm_total_away$weight_sum) * 100
bm_total_effect_df = data.frame(plot = bm_total_under[, 1], 
                                bm_total_effect = bm_total_effect)

bm_total_effect_df_lai = left_join(bm_total_effect_df, lai_mean)
head(bm_total_effect_df_lai)
nrow(bm_total_effect_df_lai)

#### run regression
bm_total_effect_df_lai_sub = subset(bm_total_effect_df_lai, bm_total_effect > -5000)
plot(bm_total_effect_df_lai_sub$bm_total_effect ~ 
       bm_total_effect_df_lai_sub$lai_mean, ylim = c(-100, 100)) # not much there
bm_total_effect_df_mean_lai_lm = lm(bm_total_effect_df_lai_sub$bm_total_effect ~ 
                                      bm_total_effect_df_lai_sub$lai_mean) # not much there
anova(bm_total_effect_df_mean_lai_lm) # nada

##########################################################################################
### Q5b: tree size doesn't seem to matter for biomass effect
##########################################################################################





