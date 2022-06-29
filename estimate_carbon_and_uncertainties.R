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
dataCEOYR <- read.csv('ceo-Estonia_SBP_carbonmonitoring_upto2021_v2-plot-data-2022-05-20.csv')
head(dataCEOYR)
colnames(dataCEOYR)

## read in and prep GEE data ####
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

######################

## convert CEO info to stand age ####
# goal: fill this df w/ stand ages (a row for each plot)
CEOStandAge <- data.frame(matrix(ncol = length(2021:1990),
                                 nrow = length(dataSBP$LC_forest_2021CEO)))
colnames(CEOStandAge) <- 2021:1990

#### preprocess CEO info in dataSBP ####
# add loss & gain event T/F indicators
dataSBP$HasLoss <- dataSBP$YrLossCEO != 9999  # has forest loss 90-21 or not
dataSBP$HasGain <- dataSBP$YrGainCEO != 9999  # has forest gain 90-21 or not
# add info about loss & gain order [WARNING: comparison with 9999 makes no sense
# only use the info when both loss and gain events are recorded]
dataSBP$LossThenGain <- dataSBP$YrGainCEO > dataSBP$YrLossCEO
dataSBP$GainThenLoss <- dataSBP$YrGainCEO < dataSBP$YrLossCEO

#### filling stand age into CEOStandAge ####
StartAge <- 80

# returns a vector of stand ages from 2021 to 1990
# #StartAge is the assumed stand age in 1990 if that cannot be determined
# through other logic
# #special treatment if loss then gain: assume gain happened 1 year after loss,
# because gain is generally detected late
# #special treatment if the earliest known event is a forest gain,
# assume the stand age at the year of gain is 4 to account for the fact that
# forest gain is generally detected late
calc_stand_age_21_90 <- function(CEOinfo, StartAge) {
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
      stand_age_90_loss <- StartAge:(StartAge+(yr_loss-1990)-1)
      stand_age_since_loss <- rep(0, 2021-yr_loss+1)
      stand_age_90_21 <- c(stand_age_90_loss, stand_age_since_loss)
      return(rev(stand_age_90_21))

    } else if (!has_loss & has_gain) {
      print('not possible 1')
      # assume is_forest_2021
      # assume stand age at year of gain is 4 already
      stand_age_since_gain <- (1+3):(2021-yr_gain+1+3)
      stand_age_bc_gain <- c(rep(0, 1000000), 1:3)
      stand_age_bc_21 <- c(stand_age_bc_gain,
                           stand_age_since_gain)
      return(rev(stand_age_bc_21)[1:32])

    } else if (has_loss & has_gain) {
      if (loss_then_gain) {
        print('not possible 2')
        # assume is_forest_2021
        # assume yr_gain to be 1 year after yr_loss
        yr_gain <- yr_loss + 1
        stand_age_since_gain <- 1:(2021-yr_gain+1)
        stand_age_from_loss_to_gain <- rep(0, yr_gain-yr_loss)
        stand_age_90_loss <- StartAge:(StartAge+(yr_loss-1990)-1)
        stand_age_90_21 <- c(stand_age_90_loss,
                             stand_age_from_loss_to_gain,
                             stand_age_since_gain)
        return(rev(stand_age_90_21))


      } else if (gain_then_loss) {
        stand_age_since_loss <- rep(0, 2021-yr_loss+1)
        # assume stand age at year of gain is 4 already
        stand_age_from_gain_to_loss <- (1+3):(yr_loss-yr_gain+3)
        stand_age_before_gain <- c(rep(0, 100000), 1:3)
        stand_age_bc_21 <- c(stand_age_before_gain,
                             stand_age_from_gain_to_loss,
                             stand_age_since_loss)
        return(rev(stand_age_bc_21)[1:32])
      } else {print("shouldn't be here!")}

    } else {print("shouldn't be here!")}

    # if is forest in 2021
  } else if (is_forest_2021) {
    if (!has_loss & !has_gain) {
      # return(MaxAge:(MaxAge-32+1))  # 80 to 49
      return((StartAge+32-1):StartAge)  # 111 to 80(StartAge)

    } else if (has_loss & !has_gain) {
      print('not possible 3')
      # assume !is_forest_2021
      stand_age_90_loss <- StartAge:(StartAge+(yr_loss-1990)-1)
      stand_age_since_loss <- rep(0, 2021-yr_loss+1)
      stand_age_90_21 <- c(stand_age_90_loss, stand_age_since_loss)
      return(rev(stand_age_90_21))

    } else if (!has_loss & has_gain) {
      # assume stand age at year of gain is 4 already
      stand_age_since_gain <- (1+3):(2021-yr_gain+1+3)
      stand_age_bc_gain <- c(rep(0, 1000000), 1:3)
      stand_age_bc_21 <- c(stand_age_bc_gain,
                           stand_age_since_gain)
      return(rev(stand_age_bc_21)[1:32])

    } else if (has_loss & has_gain) {
      if (gain_then_loss) {
        print('not possible 4')
        # assume !is_forest_2021
        stand_age_since_loss <- rep(0, 2021-yr_loss+1)
        # assume stand age at year of gain is 4 already
        stand_age_from_gain_to_loss <- (1+3):(yr_loss-yr_gain+3)
        stand_age_before_gain <- c(rep(0, 100000), 1:3)
        stand_age_bc_21 <- c(stand_age_before_gain,
                             stand_age_from_gain_to_loss,
                             stand_age_since_loss)
        return(rev(stand_age_bc_21)[1:32])

      } else if (loss_then_gain) {
        # assume yr_gain to be 1 year after yr_loss
        yr_gain <- yr_loss + 1
        stand_age_since_gain <- 1:(2021-yr_gain+1)
        stand_age_from_loss_to_gain <- rep(0, yr_gain-yr_loss)
        stand_age_90_loss <- StartAge:(StartAge+(yr_loss-1990)-1)
        stand_age_90_21 <- c(stand_age_90_loss,
                             stand_age_from_loss_to_gain,
                             stand_age_since_gain)
        return(rev(stand_age_90_21))

      } else {print("shouldn't be here!")}

    } else {print("shouldn't be here!")}
  } else {print("shouldn't be here!")}

}

for (r in 1:nrow(dataSBP)) {
  # print(r)
  CEOinfo <- dataSBP[r, c('LC_forest_2021CEO','YrLossCEO','YrGainCEO',
                          'HasLoss','HasGain','LossThenGain','GainThenLoss')]
  CEOStandAge[r, ] <- calc_stand_age_21_90(CEOinfo, StartAge)
}
view(CEOStandAge)
# write.csv(CEOStandAge, file='results/stand_age_start80_755ac7c.csv')

# #### compare with stand age and carbon by year from the map ####
# Carbon assumes a forest start age of 80 and is for natural regeneration
age_carbon_df <- read.csv('ceo-samples-standage-carbon.csv')
age_df <- age_carbon_df[, grepl('standAge', colnames(age_carbon_df))]
age_df <- cbind(age_carbon_df$sampleid, age_df)
carbon_df <- age_carbon_df[, grepl('carbon', colnames(age_carbon_df))]
carbon_df <- cbind(age_carbon_df$sampleid, carbon_df)
# view(age_df[order(age_df[,1]),
#             order(colnames(age_df), decreasing = T)])
# view(CEOStandAge[order(dataSBP$pl_sampleid), ])
carbon_df_ord <- carbon_df[order(carbon_df[,1]),
                           order(colnames(carbon_df), decreasing = T)]
# view(carbon_df_ord)  # compare with CEOcarbonNReg below

##### scatter plot carbon for each sample pt: carbon based on map (age_carbon_df) ####
# vs carbon based on CEO (CEOcarbonNReg from below made with start age 80)
# for 2021 only
plot(CEOcarbonNReg[order(dataSBP$pl_sampleid), 'carbon2021NReg'],
     carbon_df_ord$X2021_carbon*0.09,
     pch=19, xlab='based on CEO', ylab='based on map',
     main='Estimate carbon in sample plots (tonne) in 2021')
lines(0:7, 0:7)

## estimate carbon per pixel ####
### planted conifer, excluding pine in Europe ######
b0 <- 156.968
b1 <- 0.064457
b2 <- 3.946418
###  CI upper:
b0up <- 177.1787
b1up <- 0.047689
b2up <- 2.00096
###  CI lower:
b0low <- 138.3733
b1low <- 0.080944
b2low <- 8.37114

##CEO carbon calcs
CEOcarbonCON <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
for (i in 1:32){
  CEOcarbonCON[,i] <- b0 * (1-exp(-b1 * CEOStandAge[i]) )^b2 * 0.09  # tons ( 1pix * 0.09ha/pix * xxx tonC/ha )
  colnames(CEOcarbonCON)[i] <- paste0('carbon', 2022-i, 'CON')
}

##CEO carbon calcs
CEOcarbonCONup <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
for (i in 1:32){
  CEOcarbonCONup[,i] <- b0up * (1-exp(-b1up * CEOStandAge[i]) )^b2up * 0.09
  colnames(CEOcarbonCONup)[i] <- paste0('carbon', 2022-i, 'CONup')
}

##CEO carbon calcs
CEOcarbonCONlow <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
for (i in 1:32){
  CEOcarbonCONlow[,i] <- b0low * (1-exp(-b1low * CEOStandAge[i]) )^b2low * 0.09
  colnames(CEOcarbonCONlow)[i] <- paste0('carbon', 2022-i, 'CONlow')
}


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
  CEOcarbonNReg[,i] <- b0 * (1-exp(-b1 * CEOStandAge[i]) )^b2 * 0.09
  colnames(CEOcarbonNReg)[i] <- paste0('carbon', 2022-i, 'NReg')
}

##CEO carbon calcs
CEOcarbonNRegup <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
for (i in 1:32){
  CEOcarbonNRegup[,i] <- b0up * (1-exp(-b1up * CEOStandAge[i]) )^b2up * 0.09
  colnames(CEOcarbonNRegup)[i] <- paste0('carbon', 2022-i, 'NRegup')
}

##CEO carbon calcs
CEOcarbonNReglow <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
for (i in 1:32){
  CEOcarbonNReglow[,i] <- b0low * (1-exp(-b1low * CEOStandAge[i]) )^b2low * 0.09
  colnames(CEOcarbonNReglow)[i] <- paste0('carbon', 2022-i, 'NReglow')
}

CarbonCon <- cbind(dataSBP, CEOcarbonCON, CEOcarbonCONlow, CEOcarbonCONup)
CarbonNatReg <- cbind(dataSBP, CEOcarbonNReg, CEOcarbonNReglow, CEOcarbonNRegup)

# view(round(CEOcarbonNReg[order(dataSBP$pl_sampleid), ],2))

rm(dataSBP,CEOStandAge, CEOcarbonNReg, CEOcarbonNReglow, CEOcarbonNRegup, CEOcarbonCON, CEOcarbonCONlow, CEOcarbonCONup)




# area weighted estimates of total carbon ####
### select data of interest - per-pixel carbon estimate and ci ####
C_est_ci_df <- CarbonCon
forest_type <- 'CON'
forest_type_full <- 'conifer'

# C_est_ci_df <- CarbonNatReg
# forest_type <- 'NReg'
# forest_type_full <- 'natreg'

### survey design ####
strat_design <- svydesign(id = ~1, strata = ~pl_strata, fpc = ~count,
                          data = C_est_ci_df)
strat_design

### total carbon & confidence intervals based on ####
### 1) per-pixel carbon estimate, 2) upper CI of per-pixel C est
### and 3) lower CI of per-pixel C est.
C_est_ci_ci_df <- data.frame()

for (yyyy in 1990:2021) {
  # total C and CI based on per-pixel C estimate
  C_est_formula <- as.formula(paste0('~carbon',as.character(yyyy),forest_type))
  svy_tot <- svytotal(C_est_formula, strat_design)  # total C
  ci <- confint(svy_tot)  # CI of total C
  C_est_ci_yyyy <- cbind(coef(svy_tot), ci)
  # total C and CI based on upperCI of per-pixel C estimate
  C_est_formula_up <- as.formula(paste0('~carbon',as.character(yyyy),
                                        forest_type,'up'))
  svy_tot_up <- svytotal(C_est_formula_up, strat_design)  # total C
  ci_up <- confint(svy_tot_up)  # CI of total C
  C_est_ci_yyyy_up <- cbind(coef(svy_tot_up), ci_up)
  # total C and CI based on lowerCI of per-pixel C estimate
  C_est_formula_low <- as.formula(paste0('~carbon',as.character(yyyy),
                                         forest_type,'low'))
  svy_tot_low <- svytotal(C_est_formula_low, strat_design)  # total C
  ci_low <- confint(svy_tot_low)  # CI of total C
  C_est_ci_yyyy_low <- cbind(coef(svy_tot_low), ci_low)
  # save all results
  C_est_ci_ci_df <- rbind(C_est_ci_ci_df,
                          cbind(C_est_ci_yyyy, C_est_ci_yyyy_up, C_est_ci_yyyy_low))
}
C_est_ci_ci_df
C_est_ci_ci_df <- cbind(1990:2021, C_est_ci_ci_df)
colnames(C_est_ci_ci_df) <- c('year',
                           'totalC_estimate_from_growthFunc_ton',
                           'upp95CI_totalC_estimate_from_growthFunc_ton',
                           'low95CI_totalC_estimate_from_growthFunc_ton',
                           'totalC_estimate_from_uppGrowthFunc_ton',
                           'upp95CI_totalC_estimate_from_uppGrowthFunc_ton',
                           'low95CI_totalC_estimate_from_uppGrowthFunc_ton',
                           'totalC_estimate_from_lowGrowthFunc_ton',
                           'upp95CI_totalC_estimate_from_lowGrowthFunc_ton',
                           'low95CI_totalC_estimate_from_lowGrowthFunc_ton')
view(C_est_ci_ci_df)

write.csv(C_est_ci_ci_df, file = paste0('results/',
                                        'C_estimate_95ci_loMiUpGrowthFunc_',
                                        'start',
                                        as.character(StartAge),
                                        '_',
                                        forest_type_full,
                                        '.csv'))