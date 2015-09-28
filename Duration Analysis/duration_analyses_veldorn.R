###---------------------------------------------------------------------
### Re-analysis of Vellend et al. and Dornelas et al. 
### richness change results using the Vellend et al.
### log ratio approach. Previous work can be found in
### https://github.com/jebyrnes/velResponse
###
### Reference: Gonzalez et al. In Review. stimating local biodiversity 
### change:  a critique of recent papers claiming no net loss of local 
### diversity.
###
###
### Authors: Jarrett Byrnes, Forest Isbell, Andy Gonzalez
###
### Changelog
###
### Sept 27, 2015 - Made Dornelas et al. dataset name consistent
### Sept 22, 2015 - Cleaned comments, removed extraneous material
### Aug 15, 2015 - Removed analysis that combined both studies into one model
### June 14, 2015 - Added >200 yr study back in after conversation with Forest & Mark V.
### June 8, 2015 -  Added datasets with LR but no S values after conversation with Mark V.
###---------------------------------------------------------------------


#---------------
# Load libraries
# and a few extra functions
#---------------
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
cent <- function(x) x-mean(x, na.rm=T)

#---------------
# Read in the Dornelas et al. data 
#---------------
dorn <- read.csv("1248484_s1.csv")
dornOrig <- dorn

#add columns for the first and last richness and year
dorn$S1 <- c()
dorn$S2 <- c()
dorn$Y1 <- c()
dorn$Y2 <- c()
for(i in dorn$ID) {
	dorn$S1[dorn$ID==i] <- dorn$S[dorn$ID==i & dorn$Year==min(dorn$Year[dorn$ID==i])]
	dorn$S2[dorn$ID==i] <- dorn$S[dorn$ID==i & dorn$Year==max(dorn$Year[dorn$ID==i])]
	dorn$Y1[dorn$ID==i] <- dorn$Year[dorn$ID==i & dorn$Year==min(dorn$Year[dorn$ID==i])]
	dorn$Y2[dorn$ID==i] <- dorn$Year[dorn$ID==i & dorn$Year==max(dorn$Year[dorn$ID==i])]
}

#add columns comparable to the Vellend data
dorn$duration <- dorn$Y2-dorn$Y1
dorn$effect <- (log(dorn$S2) - log(dorn$S1)) / (dorn$duration/10)
dorn$effect_no_duration <- (log(dorn$S2) - log(dorn$S1))
dorn$prop_change <- (dorn$S2-dorn$S1)/dorn$S1
dorn$diff <- dorn$S2-dorn$S1

#remove all extra rows and columns
dorn <- dorn[!duplicated(dorn[,c(1,16)]),-c(2:12)]


#######
# Load Vellend et al. Data
#######

vel <- read.csv("sd01.csv", header=T)
vel <- subset(vel, vel$SR_analysis==1)

# calculate the effect size using equastions in text and 
# some other metrics provided by Vellend directly
vel <- within(vel,{
  effect <- (log(SR_Year2_CT) - log(SR_Year1_CT))/(Duration/10)
  effect_no_duration <- (log(SR_Year2_CT) - log(SR_Year1_CT))
  prop_change <- (SR_Year2_CT-SR_Year1_CT)/SR_Year1_CT
})      			

#Some studies only report LRs
#Make sure they are included
haveLR <- which(is.na(vel$SR_Year1_CT) & !is.na(vel$log_SR_ratio))
vel$effect_no_duration[haveLR] <- vel$log_SR_ratio[haveLR]
   
# narrow down the data frame for future use with LME, etc.
# which do not tolerate NA values
vel <- subset(vel, !is.na(vel$effect))

#----------------
# Model both datasets looking at the effect
# of duration on LR to estimate a rate of change
#----------------

#fit a simple linear model to the Dornelas data
fit.Du <- lm(effect_no_duration~duration,data=dorn)
summary(fit.Du)

dorn$fit <- fitted(fit.Du)


fit.Du.noLongest <- lm(effect_no_duration~duration,
                       data=dorn %>% filter(dorn$duration<90))
summary(fit.Du.noLongest)

#mixed model of slopes, to see if this can be picked up in original data
dorn <- dornOrig %>% group_by(ID) %>%
  mutate(duration = max(Year) - min(Year), 
         meanS = mean(S),
         sStd = S/mean(S)) %>%
  ungroup

mod.dorn.lmer <- lmer(sStd ~ Year + (1 |ID) , weights=duration, data=dorn)
summary(mod.dorn.lmer)

mod.dorn.lmer.nooutlier <- lmer(sStd ~ Year + (1 |ID), 
                                weights=duration, 
                                data=dorn %>% filter(duration<90))
summary(mod.dorn.lmer.nooutlier)

#Linear analysis of the Vellend data using a mixed model
#with a random effect of study 
fit.Vel <- lmer(effect_no_duration ~ Duration + (1+Duration|Study), data=vel, REML=T)
summary(fit.Vel)

#Make sure Vellend results isn't driven by that one 
#data point with a duration of >200 years
fit.Vel.nooutlier <- lmer(effect_no_duration ~ Duration + (1+Duration|Study), 
                data=vel %>% filter(Duration<200), REML=T)
summary(fit.Vel.nooutlier)

#----------------
#Plot Them side-by-side
#----------------
velPlot <- qplot(Duration,effect_no_duration, data=vel_ag, size=I(2)) +
  geom_abline(intercept=fixef(fit.Vel)[1], slope=fixef(fit.Vel)[2], 
              lwd=1.5, color="red") +
  xlab("\nStudy Duration (years)") +
  ylab("Effect Size: ln(S2/S1)\n") +
  theme_bw(base_size=17) +
  annotate(geom="text", x=0.25, y=1.35, label="a)") +
  xlim(c(0,275))+
  ggtitle("Vellend et al.")

dorPlot <-  qplot(duration,effect_no_duration, data=dorn, size=I(2)) +
  geom_abline(intercept=coef(fit.Du)[1], slope=coef(fit.Du)[2], 
              lwd=1.5, color="red") +
  xlab("\nStudy Duration (years)") +
  ylab("Effect Size: ln(S2/S1)\n") +
  theme_bw(base_size=17) +
  annotate(geom="text", x=0.25, y=2.25, label="b)")+
  xlim(c(0,100)) +
  ggtitle("Dornelas et al.")

library(gridExtra)
jpeg(file="velDornLRPlots.jpg", width=800, height=400, type="quartz")
grid.arrange(velPlot, dorPlot, ncol=2)
dev.off()