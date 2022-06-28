#####################################################################
library(survey)
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)
library(networkD3)
library(tidyverse)
library(dplyr)
library(scales)
library(lubridate)

# setwd("C:\\Users\\karis\\Documents\\SBP\\AreaEstimation")
setwd("~/Desktop/SIG/PC382_SBP/uncertainty_estimation/SBP_uncertainty")

# preprocess data ####
## read in CEO data ####
#dataCEOYR <- read.csv('ceo-Estonia_SBP_carbonmonitoring_upto2021_v2-plot-data-2022-04-07 (1).csv')
dataCEOYR <- read.csv('ceo-Estonia_SBP_carbonmonitoring_upto2021_v2-plot-data-2022-05-20.csv')
head(dataCEOYR)
colnames(dataCEOYR)
dataCEOYR$pl_oldid

## read in and prep GEE data ####
#dataCEO <- read.csv('ceo-Estonia_SBP_carbonmonitoring_upto2021_v2-sample-data-2022-03-14_modified_30m_and_100m.csv')
dataGEE <- read.csv('ceo-Estonia-SBP-standage-80.csv')
head(dataGEE,2)
colnames(dataGEE)
#Land.Cover.in.2021.
colnames(dataGEE)[34]<-'Loss90_98'  # "Forest.loss.1990.1998." T/F
# [35] "Forest.loss.1999.2021."                      "Land.Cover.in.2021."
# [37] "NonForest.to.Forest.conversion..1990.1998.." "NonForest.to.Forest.conversion..1999.2021.."
colnames(dataGEE)[35]<-'Loss99_ 21'  # T/F
colnames(dataGEE)[36]<-'LC2021'  # "Forest"     "Non-Forest"
colnames(dataGEE)[37]<-'Gain90_98'
colnames(dataGEE)[38]<-'Gain99_21'
dataGEE<-dataGEE[,c(48, 49, 42:43, 34:39, 2:33)]
colnames(dataGEE)
head(dataGEE)

## Prepare CEO data for merging with GEE data ####
colnames(dataCEOYR[,c(7, 16, 17,18, 24,29)])
colnames(dataCEOYR)[16]
colnames(dataCEOYR)[18]<-'LC_forest_2021CEO'  # 0 or 100
colnames(dataCEOYR)[19]<-'LC_nonforest_2021'  # 100 - LC_forest_2021CEO
colnames(dataCEOYR)[20]<-'Forest_Loss_99_21'  # 0 or 100
colnames(dataCEOYR)[25]<-'Forest_Gain_99_21'
colnames(dataCEOYR)[22]<-'Forest_Loss_90_98'
colnames(dataCEOYR)[27]<-'Forest_Gain_90_98'
colnames(dataCEOYR)[24]<-'YrLossCEO'  # yyyy 9999 or NA
colnames(dataCEOYR)[29]<-'YrGainCEO'  # yyyy 9999 or NA
colnames(dataCEOYR)[30] <- 'notes'

dataCEOYR$YrLossCEO[is.na(dataCEOYR$YrLossCEO)] <- 9999
dataCEOYR$YrGainCEO[is.na(dataCEOYR$YrGainCEO)] <- 9999

#dataall<- merge(dataGEE, dataCEOYR[,c(7,16, 17, 18, 24,29)],
#                by.x = c('pl_sampleid', 'email'), by.y = c('pl_sampleid', 'email'))
#dataall[,43]==dataall[,3]

## merge GEE & CEO data, remove extra strata ####
dataall<- merge(dataGEE, dataCEOYR[,c(7, 17, 18, 24,29)],
                by.x = c('pl_sampleid', 'email'), by.y = c('pl_sampleid', 'email'))
colnames(dataall)  # "LC_forest_2021CEO""YrLossCEO""YrGainCEO" from dataCEOYR

## read in and prep strata pixel-count data ####
dataStrata <- read.csv('stratav0_4326_100m.csv')
head(dataStrata)
dataStrata[,c(2, 4, 5)]
dataStrata[5]<-'strataName'  # readable column now filled with 'strataName'
# readable column used to non forest, forest less than 20years old, forest greater than 20 years old

## Merge strata pixel-count data with CEO-GEE-combo data ####
#CEO has pl_strata
dataSBP<- merge(dataall, dataStrata[c(2, 4, 5)], by.x= 'pl_strata', by.y = 'map_value', all.x = T)
head(dataSBP)
colnames(dataSBP)  # count, readable from dataStrata
rm(dataCEOYR, dataall, dataGEE, dataStrata)

##################################
#############################

### convert CEO info to stand age ####
# goal: fill this df w/ stand ages (a row for each plot)
CEOStandAge <- data.frame(matrix(ncol = length(2021:1990),
                                 nrow = length(dataSBP$LC_forest_2021CEO)))
colnames(CEOStandAge) <- 2021:1990

MaxAge<-80

#### preprocess CEO info in dataSBP ####
# add loss & gain event T/F indicators
dataSBP$HasLoss <- dataSBP$YrLossCEO != 9999  # has forest loss 90-21 or not
dataSBP$HasGain <- dataSBP$YrGainCEO != 9999  # has forest gain 90-21 or not
# add info about loss & gain order [WARNING: comparison with 9999 makes no sense
# only use the info when both loss and gain events are recorded]
dataSBP$LossThenGain <- dataSBP$YrGainCEO > dataSBP$YrLossCEO
dataSBP$GainThenLoss <- dataSBP$YrGainCEO < dataSBP$YrLossCEO

#### filling stand age into CEOStandAge ####
# returns a vector of stand ages from 2021 to 1990
# MaxAge is the assumed stand age before a year of loss when the year of gain
# is unknown
calc_stand_age_21_90 <- function(CEOinfo, MaxAge) {
  is_forest_2021 <- CEOinfo$LC_forest_2021CEO
  yr_loss <- CEOinfo$YrLossCEO
  yr_gain <- CEOinfo$YrGainCEO
  has_loss <- CEOinfo$HasLoss
  has_gain <- CEOinfo$HasGain
  loss_then_gain <- CEOinfo$LossThenGain
  gain_then_loss <- CEOinfo$GainThenLoss
  # if is non-forest in 2021
  if (!is_forest_2021) {
    if (!has_loss & !has_gain) {
      return(rep(0, 32))

    } else if (has_loss & !has_gain) {
      stand_age_21_seed <- c(0:MaxAge, rep(0, 2021-yr_loss+1)) %>% rev()
      return(stand_age_21_seed[1:32])

    } else if (!has_loss & has_gain) {
      print('not possible 1')
      # assume is_forest_2021
      stand_age_since_gain <- 1:(2021-yr_gain+1)
      stand_age_bc_gain <- rep(0, 1000000)
      stand_age_bc_21 <- c(stand_age_bc_gain,
                           stand_age_since_gain)
      return(rev(stand_age_bc_21)[1:32])

    } else if (has_loss & has_gain) {
      if (loss_then_gain) {
        print('not possible 2')
        # assume is_forest_2021
        stand_age_since_gain <- 1:(2021-yr_gain+1)
        stand_age_from_loss_to_gain <- rep(0, yr_gain-yr_loss)
        stand_age_seed_loss <- 0:MaxAge
        stand_age_seed_21 <- c(stand_age_seed_loss,
                               stand_age_from_loss_to_gain,
                               stand_age_since_gain)
        return(rev(stand_age_seed_21)[1:32])

      } else if (gain_then_loss) {
        stand_age_since_loss <- rep(0, 2021-yr_loss+1)
        stand_age_from_gain_to_loss <- 1:(yr_loss-yr_gain)
        stand_age_before_gain <- rep(0, 100000)
        stand_age_bc_21 <- c(stand_age_before_gain,
                             stand_age_from_gain_to_loss,
                             stand_age_since_loss)
        return(rev(stand_age_bc_21)[1:32])
      } else {print("shouldn't be here!")}

    } else {print("shouldn't be here!")}

    # if is forest in 2021
  } else if (is_forest_2021) {
    if (!has_loss & !has_gain) {
      return(MaxAge:(MaxAge-32+1))  # 80 to 49

    } else if (has_loss & !has_gain) {
      print('not possible 3')
      # assume !is_forest_2021
      stand_age_21_seed <- c(0:MaxAge, rep(0, 2021-yr_loss+1)) %>% rev()
      return(stand_age_21_seed[1:32])

    } else if (!has_loss & has_gain) {
      stand_age_since_gain <- 1:(2021-yr_gain+1)
      stand_age_bc_gain <- rep(0, 1000000)
      stand_age_bc_21 <- c(stand_age_bc_gain,
                           stand_age_since_gain)
      return(rev(stand_age_bc_21)[1:32])

    } else if (has_loss & has_gain) {
      if (gain_then_loss) {
        print('not possible 4')
        # assume !is_forest_2021
        stand_age_since_loss <- rep(0, 2021-yr_loss+1)
        stand_age_from_gain_to_loss <- 1:(yr_loss-yr_gain)
        stand_age_before_gain <- rep(0, 100000)
        stand_age_bc_21 <- c(stand_age_before_gain,
                             stand_age_from_gain_to_loss,
                             stand_age_since_loss)
        return(rev(stand_age_bc_21)[1:32])

      } else if (loss_then_gain) {
        stand_age_since_gain <- 1:(2021-yr_gain+1)
        stand_age_from_loss_to_gain <- rep(0, yr_gain-yr_loss)
        stand_age_seed_loss <- 0:MaxAge
        stand_age_seed_21 <- c(stand_age_seed_loss,
                               stand_age_from_loss_to_gain,
                               stand_age_since_gain)
        return(rev(stand_age_seed_21)[1:32])

      } else {print("shouldn't be here!")}

    } else {print("shouldn't be here!")}
  } else {print("shouldn't be here!")}

}

for (r in 1:nrow(dataSBP)) {
  # print(r)
  CEOinfo <- dataSBP[r, c('LC_forest_2021CEO','YrLossCEO','YrGainCEO',
                          'HasLoss','HasGain','LossThenGain','GainThenLoss')]
  CEOStandAge[r, ] <- calc_stand_age_21_90(CEOinfo, MaxAge)
}
view(CEOStandAge)

## estimate carbon ####
### planted conifer, excluding pine in Europe ######
b0 <- 156.968
b1 <- 0.064457
b2 <- 3.946418
###  CI upper:
b0up <- 83.6653
b1up <- 0.048341
b2up <- 1.286441
###  CI lower:
b0low <- 62.1984
b1low <- 0.086366
b2low <- 4.068786

##CEO carbon calcs
CEOcarbonCON <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
for (i in 1:32){
  CEOcarbonCON[,i] <- b0 * (1-exp(-b1 * CEOStandAge[i]) )^b2
  colnames(CEOcarbonCON)[i] <- paste0('carbon', 2022-i, 'CON')
}

##CEO carbon calcs
CEOcarbonCONup <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
for (i in 1:32){
  CEOcarbonCONup[,i] <- b0up * (1-exp(-b1up * CEOStandAge[i]) )^b2up
  colnames(CEOcarbonCONup)[i] <- paste0('carbon', 2022-i, 'CONup')
}

##CEO carbon calcs
CEOcarbonCONlow <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
for (i in 1:32){
  CEOcarbonCONlow[,i] <- b0low * (1-exp(-b1low * CEOStandAge[i]) )^b2low
  colnames(CEOcarbonCONlow)[i] <- paste0('carbon', 2022-i, 'CONlow')
}


#################################
### nat regen forest europe: ####
b0 <- 72.20785
b1 <- 0.066939
b2 <- 2.231247
### CI upper
b0up <- 83.6653
b1up <- 0.048341
b2up <- 1.286441

### CI lower:
b0low <- 62.1984
b1low <- 0.086366
b2low <- 4.068786

##CEO carbon calcs
CEOcarbonNReg <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
for (i in 1:32){
  CEOcarbonNReg[,i] <- b0 * (1-exp(-b1 * CEOStandAge[i]) )^b2
  colnames(CEOcarbonNReg)[i] <- paste0('carbon', 2022-i, 'NReg')
}

##CEO carbon calcs
CEOcarbonNRegup <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
for (i in 1:32){
  CEOcarbonNRegup[,i] <- b0up * (1-exp(-b1up * CEOStandAge[i]) )^b2up
  colnames(CEOcarbonNRegup)[i] <- paste0('carbon', 2022-i, 'NRegup')
}

##CEO carbon calcs
CEOcarbonNReglow <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
for (i in 1:32){
  CEOcarbonNReglow[,i] <- b0low * (1-exp(-b1low * CEOStandAge[i]) )^b2low
  colnames(CEOcarbonNReglow)[i] <- paste0('carbon', 2022-i, 'NReglow')
}

CarbonCon <- cbind(dataSBP, CEOcarbonCON, CEOcarbonCONlow, CEOcarbonCONup)
CarbonNatReg <- cbind(dataSBP, CEOcarbonNReg, CEOcarbonNReglow, CEOcarbonNRegup)

rm(dataSBP,CEOStandAge, CEOcarbonNReg, CEOcarbonNReglow, CEOcarbonNRegup, CEOcarbonCON, CEOcarbonCONlow, CEOcarbonCONup)

##################################
# Area weighted estimates ####
colnames(CarbonCon)
CarbonCon$pl_strata
CarbonCon$count
strat_design <- svydesign(id = ~1, strata = ~pl_strata, fpc = ~count, data = CarbonCon)
strat_design

Year<-seq(from= 2021, to = 1990)

C_ConEst<- rbind(as.matrix(as.data.frame(svytotal(~carbon2021CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2020CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2019CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2018CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2017CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2016CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2015CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2014CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2013CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2012CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2011CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2010CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2009CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2008CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2007CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2006CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2005CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2004CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2003CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2002CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2001CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon2000CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon1999CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon1998CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon1997CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon1996CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon1995CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon1994CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon1993CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon1992CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon1991CON, strat_design))),
as.matrix(as.data.frame(svytotal(~carbon1990CON, strat_design))))

C_ConEst <- C_ConEst * 0.09  # 30*30/10000 pixel to ha

C_ConEst <- cbind(Year, C_ConEst)
colnames(C_ConEst)[3]<-'SE'

C_ConEst
# write.csv(C_ConEst, file = 'results\\coniferCarbonEst.csv')
# write.csv(C_ConEst, file = 'results/coniferCarbonEst_40.csv')

# scratch ####
## Richard Chapman func / growth formula ####
### planted conifer, excluding pine in Europe ####
b0 <- 156.968
b1 <- 0.064457
b2 <- 3.946418
###  CI upper:
b0up <- 83.6653
b1up <- 0.048341
b2up <- 1.286441
###  CI lower:
b0low <- 62.1984
b1low <- 0.086366
b2low <- 4.068786
estimate_carbon <- function(stand_age) {
  return(b0 * (1-exp(-b1 * stand_age) )^b2)
}
estimate_carbon_low <- function(stand_age) {
  return(b0low * (1-exp(-b1low * stand_age) )^b2low)
}
estimate_carbon_up <- function(stand_age) {
  return(b0up * (1-exp(-b1up * stand_age) )^b2up)
}
stand_age_vec <- 1:150
Cest_vec <- estimate_carbon(stand_age_vec)

plot(stand_age_vec, Cest_vec, type='l', col='black',
     ylim=c(0,300), main='planted conifer, excluding pine in Europe')
lines(stand_age_vec, estimate_carbon_low(stand_age_vec), col='blue')
lines(stand_age_vec, estimate_carbon_up(stand_age_vec), col='purple')
legend('topright', legend=c('estimate','lower CI','upper CI'),
       col=c('black', 'blue', 'purple'), lty=1)

### nat regen forest europe: ####
b0 <- 72.20785
b1 <- 0.066939
b2 <- 2.231247
### CI upper
b0up <- 83.6653
b1up <- 0.048341
b2up <- 1.286441
### CI lower:
b0low <- 62.1984
b1low <- 0.086366
b2low <- 4.068786
estimate_carbon <- function(stand_age) {
  return(b0 * (1-exp(-b1 * stand_age) )^b2)
}
estimate_carbon_low <- function(stand_age) {
  return(b0low * (1-exp(-b1low * stand_age) )^b2low)
}
estimate_carbon_up <- function(stand_age) {
  return(b0up * (1-exp(-b1up * stand_age) )^b2up)
}
stand_age_vec <- 1:150
Cest_vec <- estimate_carbon(stand_age_vec)

plot(stand_age_vec, Cest_vec, type='l', col='black',
     ylim=c(0,300), main='nat regen in Europe')
lines(stand_age_vec, estimate_carbon_low(stand_age_vec), col='blue')
lines(stand_age_vec, estimate_carbon_up(stand_age_vec), col='purple')
legend('topright', legend=c('estimate','lower CI','upper CI'),
       col=c('black', 'blue', 'purple'), lty=1)

## Converting CEO info to stand age ####
CEOinfo <- dataSBP[, c('LC_forest_2021CEO', 'YrLossCEO', 'YrGainCEO')]
# forest in 2021? year of most recent loss? year of most recent gain?

### check out CEOinfo: what are all the possible forest gain loss event ####
### series to deal with ####
# add event T/F indicators
CEOinfo$HasLoss <- CEOinfo$YrLossCEO != 9999  # has forest loss 90-21 or not
CEOinfo$HasGain <- CEOinfo$YrGainCEO != 9999  # has forest gain 90-21 or not

CEOinfo_s <- CEOinfo[, c('LC_forest_2021CEO', 'HasLoss', 'HasGain')]  # simple
CEOinfo_s %>% group_by_all() %>% count()  # unique rows and their counts
# all possible combo except for 0,F,T (gained, no loss, but non-forest in 2021)
# shouldn't have 100,T,F combo (loss, no gain, but still forest in 2021), but
# 1 plot has the combo, TO-THINK about what to do with it

# when has both gain and loss, check if the order of the 2 years makes sense:
CEOinfo_both <- CEOinfo[CEOinfo$HasGain & CEOinfo$HasLoss, ]  # no 9999!
sum(CEOinfo_both$YrGainCEO == CEOinfo_both$YrLossCEO) == 0  # no plot has
# gain year the same as loss year

# add info about loss & gain order
CEOinfo_both$LossThenGain <- CEOinfo_both$YrGainCEO > CEOinfo_both$YrLossCEO
CEOinfo_both$GainThenLoss <- CEOinfo_both$YrGainCEO < CEOinfo_both$YrLossCEO
# unique combo
CEOinfo_both[, c('LC_forest_2021CEO', 'LossThenGain', 'GainThenLoss')] %>%
  group_by_all() %>% count()
# shouldn't have 0,T,F (loss then gain, but non-forest in 2021) - but 4 plots
# have the combo, TO-THINK about what to do with them

### convert CEO info to stand age ####
# goal: fill this df w/ stand ages (a row for each plot)
CEOStandAge <- data.frame(matrix(ncol = length(2021:1990),
                                 nrow = length(dataSBP$LC_forest_2021CEO)))
colnames(CEOStandAge) <- 2021:1990

MaxAge<-80

#### preprocess CEO info in dataSBP ####
# add loss & gain event T/F indicators
dataSBP$HasLoss <- dataSBP$YrLossCEO != 9999  # has forest loss 90-21 or not
dataSBP$HasGain <- dataSBP$YrGainCEO != 9999  # has forest gain 90-21 or not
# add info about loss & gain order [WARNING: comparison with 9999 makes no sense
# only use the info when both loss and gain events are recorded]
dataSBP$LossThenGain <- dataSBP$YrGainCEO > dataSBP$YrLossCEO
dataSBP$GainThenLoss <- dataSBP$YrGainCEO < dataSBP$YrLossCEO

#### filling stand age ####
# # tests of `calc_stand_age_21_90`
# rbind(2021:1990, VECTOR_GOES_HERE)

# returns a vector of stand ages from 2021 to 1990
# MaxAge is the assumed stand age before a year of loss when the year of gain
# is unknown
calc_stand_age_21_90 <- function(CEOinfo, MaxAge) {
  is_forest_2021 <- CEOinfo$LC_forest_2021CEO
  yr_loss <- CEOinfo$YrLossCEO
  yr_gain <- CEOinfo$YrGainCEO
  has_loss <- CEOinfo$HasLoss
  has_gain <- CEOinfo$HasGain
  loss_then_gain <- CEOinfo$LossThenGain
  gain_then_loss <- CEOinfo$GainThenLoss
  # if is non-forest in 2021
  if (!is_forest_2021) {
    if (!has_loss & !has_gain) {
      return(rep(0, 32))

    } else if (has_loss & !has_gain) {
      stand_age_21_seed <- c(0:MaxAge, rep(0, 2021-yr_loss+1)) %>% rev()
      return(stand_age_21_seed[1:32])

    } else if (!has_loss & has_gain) {
      print('not possible 1')
      # assume is_forest_2021
      stand_age_since_gain <- 1:(2021-yr_gain+1)
      stand_age_bc_gain <- rep(0, 1000000)
      stand_age_bc_21 <- c(stand_age_bc_gain,
                           stand_age_since_gain)
      return(rev(stand_age_bc_21)[1:32])

    } else if (has_loss & has_gain) {
      if (loss_then_gain) {
        print('not possible 2')
        # assume is_forest_2021
        stand_age_since_gain <- 1:(2021-yr_gain+1)
        stand_age_from_loss_to_gain <- rep(0, yr_gain-yr_loss)
        stand_age_seed_loss <- 0:MaxAge
        stand_age_seed_21 <- c(stand_age_seed_loss,
                               stand_age_from_loss_to_gain,
                               stand_age_since_gain)
        return(rev(stand_age_seed_21)[1:32])

      } else if (gain_then_loss) {
        stand_age_since_loss <- rep(0, 2021-yr_loss+1)
        stand_age_from_gain_to_loss <- 1:(yr_loss-yr_gain)
        stand_age_before_gain <- rep(0, 100000)
        stand_age_bc_21 <- c(stand_age_before_gain,
                             stand_age_from_gain_to_loss,
                             stand_age_since_loss)
        return(rev(stand_age_bc_21)[1:32])
      } else {print("shouldn't be here!")}

    } else {print("shouldn't be here!")}

  # if is forest in 2021
  } else if (is_forest_2021) {
    if (!has_loss & !has_gain) {
      return(MaxAge:(MaxAge-32+1))  # 80 to 49

    } else if (has_loss & !has_gain) {
      print('not possible 3')
      # assume !is_forest_2021
      stand_age_21_seed <- c(0:MaxAge, rep(0, 2021-yr_loss+1)) %>% rev()
      return(stand_age_21_seed[1:32])

    } else if (!has_loss & has_gain) {
      stand_age_since_gain <- 1:(2021-yr_gain+1)
      stand_age_bc_gain <- rep(0, 1000000)
      stand_age_bc_21 <- c(stand_age_bc_gain,
                           stand_age_since_gain)
      return(rev(stand_age_bc_21)[1:32])

    } else if (has_loss & has_gain) {
      if (gain_then_loss) {
        print('not possible 4')
        # assume !is_forest_2021
        stand_age_since_loss <- rep(0, 2021-yr_loss+1)
        stand_age_from_gain_to_loss <- 1:(yr_loss-yr_gain)
        stand_age_before_gain <- rep(0, 100000)
        stand_age_bc_21 <- c(stand_age_before_gain,
                             stand_age_from_gain_to_loss,
                             stand_age_since_loss)
        return(rev(stand_age_bc_21)[1:32])

      } else if (loss_then_gain) {
        stand_age_since_gain <- 1:(2021-yr_gain+1)
        stand_age_from_loss_to_gain <- rep(0, yr_gain-yr_loss)
        stand_age_seed_loss <- 0:MaxAge
        stand_age_seed_21 <- c(stand_age_seed_loss,
                               stand_age_from_loss_to_gain,
                               stand_age_since_gain)
        return(rev(stand_age_seed_21)[1:32])

      } else {print("shouldn't be here!")}

    } else {print("shouldn't be here!")}
  } else {print("shouldn't be here!")}

}

for (r in 1:nrow(dataSBP)) {
  # print(r)
  CEOinfo <- dataSBP[r, c('LC_forest_2021CEO','YrLossCEO','YrGainCEO',
                          'HasLoss','HasGain','LossThenGain','GainThenLoss')]
  CEOStandAge[r, ] <- calc_stand_age_21_90(CEOinfo, MaxAge)
}
view(CEOStandAge)



##################################
## OLD CODE
####################################################################
####################################################################


Change<-Change* 30 * 30 / 10000
Change
Change<-round(Change, digits = 0)
Change
##################################
## bar charts
dataTest$Arealab <- comma_format()(dataTest$Area)
dataTest$SElab <- paste0('+/- ', comma_format()(dataTest$SE))

# create dummy data
#data <- data.frame(
#  name=letters[1:5],
# value=sample(seq(4,15),5),
#  sd=c(1,0.2,3,2,4)
#)

data <- data.frame(dataTest)
#vLABELH <- c(145813,152997,90409,160376,286795,355990,346318)

# Calculate y position, placing it in the middle
data <- data %>%
  group_by(Strata) %>%
  #mutate(label_y = cumsum(Area) - 0.5 * Area)
  mutate(label_y = cumsum(Area) -cumsum(Area))
# Calculate error position, placing it above
data <- data %>%
  group_by(Strata) %>%
  mutate(label_error = SE+Area)

# Most basic error bar
ggplot(data, aes(fill= phase, x = Strata, y = Area )) +
  geom_bar(position="dodge", stat="identity", alpha=1) +
  geom_errorbar(aes(ymin = Area - SE, ymax = Area + SE),
                width = 0.2, colour = "black",
                position = position_dodge(.9)) +
  geom_text(aes(label = Arealab, y = label_error), position = position_dodge(.9),
            vjust = -2.5, colour = "black", size=3)+
  geom_text(aes(label = SElab, y = label_error), position = position_dodge(.9),
            vjust = -1, colour="black", size=3) +
  xlab('LU activities') + # for the x axis label
  ylab('Area, ha') + scale_y_continuous(labels = comma)


