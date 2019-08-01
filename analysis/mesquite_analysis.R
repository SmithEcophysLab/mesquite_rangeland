# Scipt for analyzing the mequite data
## The script proceeds as stated questions followed by the code to analyze the data
## in response to that stated question
### TTABS figures can be found by searching for TTABS

## load libraries
library(tidyverse)
library(vioplot)
library(lme4)
library(car)
library(emmeans)

## ggplot theme
th = theme(legend.position="none", 
           axis.title.y=element_text(size=rel(1.5), colour = 'black'),
           axis.title.x=element_text(size=rel(1.5), colour = 'black'),
           axis.text.x=element_text(size=rel(1.5), colour = 'black'),
           axis.text.y=element_text(size=rel(1.5), colour = 'black'),
           panel.background = element_rect(fill = 'white', colour = 'black'),
           panel.grid.major = element_line(colour = "grey"),
           #axis.line = element_line(colour = 'black'),
           strip.text = element_text(size=rel(1)))

## read in data
sm = read.csv("../data/sm_clean.csv") # soil moisture
bm = read.csv("../data/bm_clean.csv") # biomass
lai = read.csv("../data/lai_clean.csv") # lai
sc = read.csv("../data/sc_clean.csv") # cover (species composition)

##########################################################################################
### Q1: What does the size distribution (in LAI) of the trees look like?
##########################################################################################
#### mean plot lai
lai_group_by_plot = group_by(lai, plot)
lai_mean = summarise(lai_group_by_plot, lai_mean = mean(lai))

#### create a histogram and get summary statistics
hist(lai_mean$lai_mean, breaks = 5)
mean(lai_mean$lai_mean)
sd(lai_mean$lai_mean) / sqrt(40)

#### TTABS ####
par(oma = c(0,0,0,0), mar = c(5, 6, 1, 1))
hist(lai_mean$lai_mean, breaks = seq(0, 2.5, 0.5), 
     ylim = c(0, 20), xlab = '', ylab ='', col = 'darkgreen',
     yaxt = 'n', xaxt = 'n', main = '')
axis(1, seq(0, 2.5, 0.5), cex.axis = 1.5)
axis(2, seq(0, 20, 4), cex.axis = 1.5, las = 1)
mtext(side = 1, expression('Mesquite Leaf Area Index (m'^'2' * ' m'^'-2' *')'), line = 4, cex = 2)
mtext(side = 2, expression('Frequency'), line = 3.5, cex = 2)
#### TTABS ####

lai_histogram = ggplot(data = lai_mean, aes(x = lai_mean)) +
  theme(legend.position="none", 
        axis.title.y=element_text(size=rel(3), colour = 'black'),
        axis.title.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        #axis.line = element_line(colour = 'black'),
        strip.text = element_text(size=rel(1))) +
  geom_histogram(fill = c('grey'), colour = 'black', binwidth = 0.25) +
  ylab('Count') +
  xlab(expression('Mesquite Leaf Area Index (m'^'2' * ' m'^'-2' *')'))

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
                     col = rep(c('darkgreen', 'yellow'), times = 8),
                     ylab = '', xlab = '')
axis(2, seq(0, 40, 10), las = 1)
axis(1, seq(1, 8, 1), at = seq(1.5, 15.5, 2))
mtext(side = 1, 'Measurement Period', line = 3)
mtext(side = 2, 'VWC (%)', line = 3)
legend('topright', c('Under', 'Away'), pch = 16, col = c('grey', 'yellow'))

sm_boxplot = ggplot(data = sm, aes(x = location, y = vwc)) +
  theme(legend.position="none", 
        axis.title.y=element_text(size=rel(4), colour = 'black'),
        axis.title.x=element_text(size=rel(4), colour = 'black'),
        axis.text.x=element_text(size=rel(4), colour = 'black'),
        axis.text.y=element_text(size=rel(3), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        #axis.line = element_line(colour = 'black'),
        strip.text = element_text(size=rel(1))) +
  geom_boxplot(fill = c('yellow', 'grey'), lwd = 4) +
  ylab('Soil water content (%)') +
  xlab('') +
  scale_x_discrete(labels = c('away' = 'Outside', 'under' = 'Under'))

sm$spot = paste(sm$plot, sm$location, sep = '_')
sm$plotfac = as.factor(sm$plot)
hist(sm$vwc)
sm_lmer = lmer(vwc ~ location * date + (1|plotfac) + (1|spot), data = sm)
summary(sm_lmer)
plot(resid(sm_lmer) ~ fitted(sm_lmer))
Anova(sm_lmer)

##########################################################################################
### A2: It's wetter outside of the mesquite area, but biggest difference in wet times
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

#### TTABS ####
par(oma = c(0, 2, 1, 1))
bm_boxplot = plot(weight_sum * 4 ~ location, data = bm_total,
                  col = c('orange', 'blue'),
                  outcol = c('orange', 'blue'),
                  outpch = 16,
                  yaxt = 'n', xaxt = 'n',
                  ylab ='', xlab = '',
                  ylim = c(0, 150))
axis(1, at = c(1, 2), c('Outside', 'Under'), cex.axis = 2)
axis(2, seq(0, 150, 50), cex.axis = 1.5, las = 1)
mtext(side = 2, expression('Understory Biomass (g m'^'-2' *')'), cex = 2.4, line = 4)
text(2.2, 145, 'p = 0.054', cex = 1.8)

bm_boxplot = ggplot(data = bm_total, aes(x = location, y = weight_sum * 4)) +
  theme(legend.position="none", 
        axis.title.y=element_text(size=rel(4), colour = 'black'),
        axis.title.x=element_text(size=rel(4), colour = 'black'),
        axis.text.x=element_text(size=rel(4), colour = 'black'),
        axis.text.y=element_text(size=rel(3), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        #axis.line = element_line(colour = 'black'),
        strip.text = element_text(size=rel(1))) +
  geom_boxplot(fill = c('yellow', 'grey'), lwd = 4) +
  ylab(expression('Understory AGB (g m'^'-2' *')')) +
  xlab('') +
  scale_x_discrete(labels = c('away' = 'Outside', 'under' = 'Under'))

bm_total$spot = paste(bm_total$plot, bm_total$location, sep = '_')
bm_total$plotfac = as.factor(bm_total$plot)
hist(log(bm_total$weight_sum + 0.01))
hist(bm_total$weight_sum)
bm_total_lmer = lmer(log(weight_sum + 0.01) ~ location + (1|plotfac), data = bm_total)
summary(bm_total_lmer)
plot(resid(bm_total_lmer) ~ fitted(bm_total_lmer))
Anova(bm_total_lmer)
emmeans(bm_total_lmer, ~location)

# bm_dot = ggplot(bm_total, aes(x=location, y=weight_sum, fill = location)) +
#                 geom_boxplot(fill = 'white') +
#                 geom_dotplot(binaxis = 'y', stackdir = 'center') +
#                 theme_minimal() +
#                 labs(x = 'Location', y = 'Total Biomass (g)')
# bm_dot + scale_fill_manual(values = c('blue', 'orange')) + 
#          theme(legend.position="none") +
#          scale_y_continuous(limit = c(0, 40)) +
#          scale_x_discrete(labels = c('away' = 'Outside', 'under' = 'Under'))
#          theme(axis.text.y = element_text(size = 20, color = 'black')) +
#          theme(axis.text.x = element_text(size = 20, color = 'black'))
#### TTABS ####


##########################################################################################
### A3a: Lower under the mesquite
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

#### grass ratio
grass_ratio = bm_lifeform_grass$weight_sum / 
  rowSums(cbind(bm_lifeform_forb$weight_sum, bm_lifeform_grass$weight_sum, 
      bm_lifeform_tree$weight_sum))

grass_ratio_df = data.frame(bm_lifeform_grass[, c(1,2)], grass_ratio = grass_ratio)

t.test(subset(grass_ratio_df, location == 'under')$grass_ratio,
       subset(grass_ratio_df, location == 'away')$grass_ratio,
       paired = T)

#### forb ratio
forb_ratio = bm_lifeform_forb$weight_sum / 
  rowSums(cbind(bm_lifeform_forb$weight_sum, bm_lifeform_grass$weight_sum, 
                bm_lifeform_tree$weight_sum))

forb_ratio_df = data.frame(bm_lifeform_forb[, c(1,2)], forb_ratio = forb_ratio)

t.test(subset(forb_ratio_df, location == 'under')$forb_ratio,
       subset(forb_ratio_df, location == 'away')$forb_ratio,
       paired = T)


#### plot out grass ratio results
grass_ratio_vioplot = plot(NULL, ylim = c(0, 1.2), xlim = c(0, 3), yaxt = 'n', 
                           xaxt = 'n', ylab = '', xlab = '')
grass_ratio_vioplot = vioplot(subset(grass_ratio_df, location == 'under')$grass_ratio,
                              subset(grass_ratio_df, location == 'away')$grass_ratio,
                              add = T, col = c('grey', 'yellow'))
axis(2, seq(0, 1, 0.2), las = 1)
axis(1, c('under', 'away'), at = c(1, 2))
mtext(side = 2, 'Grass ratio', line = 3)
text(1, 1.1, paste(round(mean(subset(grass_ratio_df, location == 'under')$grass_ratio, 
                              na.rm =T), 2),
                   '±',
                   round(sd(subset(grass_ratio_df, location == 'under')$grass_ratio, 
                            na.rm =T) / sqrt(40), 2),
                   sep = ' '))
text(2, 1.1, paste(round(mean(subset(grass_ratio_df, location == 'away')$grass_ratio, 
                              na.rm =T), 2),
                   '±',
                   round(sd(subset(grass_ratio_df, location == 'away')$grass_ratio, 
                            na.rm =T) / sqrt(40), 2),
                   '*',
                   sep = ' '))
#legend('topright', c('Under', 'Away'), pch = 16, col = c('grey', 'yellow'))

#### plot out the lifeform results
bm_lifeform_vioplot = plot(NULL, ylim = c(0, 40), xlim = c(0, 5), yaxt = 'n', 
                           xaxt = 'n', ylab = '', xlab = '')
bm_lifeform_vioplot = vioplot(subset(bm_lifeform_grass, location == 'under')$weight_sum,
                              subset(bm_lifeform_grass, location == 'away')$weight_sum,
                              subset(bm_lifeform_forb, location == 'under')$weight_sum,
                              subset(bm_lifeform_forb, location == 'away')$weight_sum,
                              add = T,
                              col = rep(c('grey', 'yellow'), times = 2),
                              ylab = '', xlab = '')
axis(2, seq(0, 40, 10), las = 1)
axis(1, c('grass', 'forb'), at = c(1.5, 3.5))
mtext(side = 2, 'Biomass (g)', line = 3)
legend('topright', c('Under', 'Away'), pch = 16, col = c('grey', 'yellow'))

#### TTABS ####
grass_ratio_boxplot = boxplot(grass_ratio * 100 ~ location, data = grass_ratio_df,
                  col = c('orange', 'blue'),
                  outcol = c('orange', 'blue'),
                  outpch = 16,
                  yaxt = 'n', xaxt = 'n',
                  ylab ='', xlab = '',
                  ylim = c(0, 115))
axis(1, at = c(1, 2), c('Outside', 'Under'), cex.axis = 2)
axis(2, seq(0, 100, 25), cex.axis = 1.5, las = 1)
mtext(side = 2, 'Grass Biomass (%)', cex = 2.4, line = 4)
text(2.2, 109, 'p < 0.05', cex = 1.8)
#### TTABS ####

#### TTABS ####
forb_ratio_boxplot = boxplot(forb_ratio * 100 ~ location, data = forb_ratio_df,
                              col = c('orange', 'blue'),
                              outcol = c('orange', 'blue'),
                              outpch = 16,
                              yaxt = 'n', xaxt = 'n',
                              ylab ='', xlab = '',
                              ylim = c(0, 115))
axis(1, at = c(1, 2), c('Outside', 'Under'), cex.axis = 2)
axis(2, seq(0, 100, 25), cex.axis = 1.5, las = 1)
mtext(side = 2, 'Forb Biomass (%)', cex = 2.4, line = 4)
text(2.2, 109, 'p < 0.05', cex = 1.8)
#### TTABS ####

#### TTABS ####
grass_forb_boxplot = boxplot(subset(bm_lifeform_grass, location == 'away')$weight_sum * 4,
                            subset(bm_lifeform_grass, location == 'under')$weight_sum * 4,
                            subset(bm_lifeform_forb, location == 'away')$weight_sum * 4,
                            subset(bm_lifeform_forb, location == 'under')$weight_sum * 4,
                           col = c('orange', 'blue'),
                           outcol = c('orange', 'blue'),
                           outpch = 16,
                           yaxt = 'n', xaxt = 'n',
                           ylab ='', xlab = '',
                           ylim = c(0, 80),
                           outline = F)
axis(1, at = c(1.5, 3.5), c('Grass', 'Forb'), cex.axis = 2)
axis(2, seq(0, 80, 20), cex.axis = 1.5, las = 1)
mtext(side = 2, expression('Understory Biomass (g m'^'-2' *')'), cex = 2.4, line = 4)
legend('topright', c('Outside', 'Under'), pch = 15, col = c('orange', 'blue'), cex = 2)
#### TTABS ####

forb_percent_boxplot = ggplot(data = forb_ratio_df, aes(x = location, y = forb_ratio * 100)) +
  theme(legend.position="none", 
        axis.title.y=element_text(size=rel(4), colour = 'black'),
        axis.title.x=element_text(size=rel(4), colour = 'black'),
        axis.text.x=element_text(size=rel(4), colour = 'black'),
        axis.text.y=element_text(size=rel(3), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        #axis.line = element_line(colour = 'black'),
        strip.text = element_text(size=rel(1))) +
  geom_boxplot(fill = c('yellow', 'grey'), lwd = 4) +
  ylab(expression('C'[3] * ' Forb Biomass (%)')) +
  xlab('') +
  scale_x_discrete(labels = c('away' = 'Outside', 'under' = 'Under'))

grass_percent_boxplot = ggplot(data = forb_ratio_df, aes(x = location, y =  100 - (forb_ratio * 100))) +
  theme(legend.position="none", 
        axis.title.y=element_text(size=rel(4), colour = 'black'),
        axis.title.x=element_text(size=rel(4), colour = 'black'),
        axis.text.x=element_text(size=rel(4), colour = 'black'),
        axis.text.y=element_text(size=rel(3), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        #axis.line = element_line(colour = 'black'),
        strip.text = element_text(size=rel(1))) +
  geom_boxplot(fill = c('yellow', 'grey'), lwd = 4) +
  ylab(expression('C'[4] * ' Grass Biomass (%)')) +
  xlab('') +
  scale_x_discrete(labels = c('away' = 'Outside', 'under' = 'Under'))

forb_ratio_df$spot = paste(forb_ratio_df$plot, forb_ratio_df$location, sep = '_')
forb_ratio_df$plotfac = as.factor(forb_ratio_df$plot)
hist(log(forb_ratio_df$forb_ratio + 0.01))
hist(forb_ratio_df$forb_ratio)
forb_ratio_lmer = lmer(log(forb_ratio + 0.01) ~ location + (1|plotfac), data = forb_ratio_df)
summary(forb_ratio_lmer)
plot(resid(forb_ratio_lmer) ~ fitted(forb_ratio_lmer))
Anova(forb_ratio_lmer)
emmeans(forb_ratio_lmer, ~location)

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
### A5b: tree size doesn't seem to matter for biomass effect
##########################################################################################


##########################################################################################
### Q6a: does mesquite influence species richness?
##########################################################################################
#### remove litter and bareground from cover data
sc_live = subset(sc, code != 'BARE' & code != 'LITT')

#### calculate richness
sc_richness_pre = data.frame(plot = rep(seq(1, 40), 2), 
                             location = c(rep('under', 40), rep('away', 40)),
                             date = rep('2018-09-22', times = 80))
nrow(sc_richness_pre)

#### calculate species richness per plot type (for non-litter)
sc_richness_group_by = group_by(sc_live, date, plot, location)
sc_richness_sums = summarise(sc_richness_group_by, richness = n())

##### combine richness into data fram with zeros
sc_richness = left_join(sc_richness_pre, sc_richness_sums)
head(sc_richness)
sc_richness$richness[is.na(sc_richness$richness)] = 0
head(sc_richness)
tail(sc_richness)

##### run the analysis
t.test(subset(sc_richness, location == 'under')$richness, 
       subset(sc_richness, location == 'away')$richness,
       paired = T)

##########################################################################################
### A6a: richness is the same under the mesquite
##########################################################################################

##########################################################################################
### Q6b: does mesquite influence species diversity?
##########################################################################################
#### diversity is Shannon's diversity (D) where
#### H = -sum(percent cover * log(percent cover))
#### probably need to recalculate the cover as cover relative to living cover of the plot

#### get total living biomass of the each plot
sc_live_group_by_plot = group_by(sc_live, date, plot, location)
sc_live_sums = summarise(sc_live_group_by_plot, cover_sum = sum(cover))

#### join cover sums to sc_live
sc_live_revised = left_join(sc_live, sc_live_sums)

#### get relative cover
sc_live_revised$cover_revised = sc_live_revised$cover / sc_live_revised$cover_sum

#### do diversity calcs
sc_live_revised$logcover = log(sc_live_revised$cover_revised) # percent cover squared

#### calculate diversity
sc_diversity_pre = data.frame(plot = rep(seq(1, 40), 2), 
                             location = c(rep('under', 40), rep('away', 40)),
                             date = rep('2018-09-22', times = 80))
nrow(sc_diversity_pre)

#### get sum of cover2 by plot
sc_diversity_group_by = group_by(sc_live_revised, date, plot, location)
sc_diversity_sums = summarise(sc_diversity_group_by, logcover_sum = sum(logcover))

#### calculate diversity
sc_diversity_sums$H = -sc_diversity_sums$logcover_sum

##### combine diversity into data fram with zeros
sc_diversity = left_join(sc_diversity_pre, sc_diversity_sums)
head(sc_diversity)
sc_diversity$H[is.na(sc_diversity$H)] = 0
head(sc_diversity)
tail(sc_diversity)

##### run the analysis
t.test(subset(sc_diversity, location == 'under')$H, 
       subset(sc_diversity, location == 'away')$H,
       paired = T)
hist(sc_diversity$H) # no difference
plot(H ~ location, sc_diversity)

#### TTABS ####
bm_boxplot = plot(H ~ location, data = sc_diversity,
                  col = c('orange', 'blue'),
                  outcol = c('orange', 'blue'),
                  outpch = 16,
                  yaxt = 'n', xaxt = 'n',
                  ylab ='', xlab = '',
                  ylim = c(0, 12))
axis(1, at = c(1, 2), c('Outside', 'Under'), cex.axis = 2)
axis(2, seq(0, 12, 4), cex.axis = 1.5, las = 1)
mtext(side = 2, 'Shannon Diversity (H)', cex = 2.4, line = 4)
text(2.2, 11, 'p = 0.82', cex = 1.8)
#### TTABS ####

diversity_boxplot = ggplot(data = sc_diversity, aes(x = location, y = H)) +
  theme(legend.position="none", 
        axis.title.y=element_text(size=rel(4), colour = 'black'),
        axis.title.x=element_text(size=rel(4), colour = 'black'),
        axis.text.x=element_text(size=rel(4), colour = 'black'),
        axis.text.y=element_text(size=rel(3), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        #axis.line = element_line(colour = 'black'),
        strip.text = element_text(size=rel(1))) +
  geom_boxplot(fill = c('yellow', 'grey'), lwd = 4) +
  ylab('Shannon Diversity (H)') +
  xlab('') +
  scale_x_discrete(labels = c('away' = 'Outside', 'under' = 'Under'))

sc_diversity$spot = paste(sc_diversity$plot, sc_diversity$location, sep = '_')
sc_diversity$plotfac = as.factor(sc_diversity$plot)
hist(log(sc_diversity$H + 0.01))
hist(sc_diversity$H)
H_lmer = lmer(log(H + 0.01) ~ location + (1|plotfac), data = sc_diversity)
summary(H_lmer)
plot(resid(H_lmer) ~ fitted(H_lmer))
Anova(H_lmer)
emmeans(H_lmer, ~location)


##########################################################################################
### A6b: diversity is the same under the mesquite
##########################################################################################


##########################################################################################
### Q6c: is evenness the same under the mesquite?
##########################################################################################
#### here we're defining evenness as diversity divided by richness

#### put diversity and richness data together
sc_evenness = left_join(sc_richness, sc_diversity)
head(sc_evenness)

#### calculate evenness
sc_evenness$evenness = sc_evenness$H / sc_evenness$richness

#### run analysis
t.test(subset(sc_evenness, location == 'under')$evenness, 
       subset(sc_evenness, location == 'away')$evenness,
       paired = T)
hist(sc_evenness$evenness) # no difference
plot(evenness ~ location, sc_evenness)

##########################################################################################
### A6c: evenness is the same under the mesquite
##########################################################################################
