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

setwd("C:\\Users\\karis\\Documents\\SBP\\AreaEstimation")

#dataCEOYR <- read.csv('ceo-Estonia_SBP_carbonmonitoring_upto2021_v2-plot-data-2022-04-07 (1).csv')
dataCEOYR <- read.csv('ceo-Estonia_SBP_carbonmonitoring_upto2021_v2-plot-data-2022-05-20.csv')
head(dataCEOYR)
colnames(dataCEOYR)
dataCEOYR$pl_oldid

#dataCEO <- read.csv('ceo-Estonia_SBP_carbonmonitoring_upto2021_v2-sample-data-2022-03-14_modified_30m_and_100m.csv')
dataGEE <- read.csv('ceo-Estonia-SBP-standage-80.csv')
head(dataGEE)
colnames(dataGEE)
#Land.Cover.in.2021.
colnames(dataGEE)[34]<-'Loss90_98'
colnames(dataGEE)[35]<-'Loss99_21'
colnames(dataGEE)[36]<-'LC2021'
colnames(dataGEE)[37]<-'Gain90_98'
colnames(dataGEE)[38]<-'Gain99_21'
dataGEE<-dataGEE[,c(48, 49, 42:43, 34:39, 2:33)]
colnames(dataGEE)
head(dataGEE)

### Prepare data for merging
colnames(dataCEOYR[,c(7, 16, 17,18, 24,29)])
colnames(dataCEOYR)[16]
colnames(dataCEOYR)[18]<-'LC_forest_2021CEO'
colnames(dataCEOYR)[19]<-'LC_nonforest_2021'
colnames(dataCEOYR)[20]<-'Forest_Loss_99_21'
colnames(dataCEOYR)[25]<-'Forest_Gain_99_21'
colnames(dataCEOYR)[22]<-'Forest_Loss_90_98'
colnames(dataCEOYR)[27]<-'Forest_Gain_90_98'
colnames(dataCEOYR)[24]<-'YrLossCEO'
colnames(dataCEOYR)[29]<-'YrGainCEO'
colnames(dataCEOYR)[30] <- 'notes'

dataCEOYR$YrLossCEO[is.na(dataCEOYR$YrLossCEO)] <- 9999

#dataall<- merge(dataGEE, dataCEOYR[,c(7,16, 17, 18, 24,29)], 
#                by.x = c('pl_sampleid', 'email'), by.y = c('pl_sampleid', 'email'))
#dataall[,43]==dataall[,3]

## remove extra strata
dataall<- merge(dataGEE, dataCEOYR[,c(7, 17, 18, 24,29)], 
                by.x = c('pl_sampleid', 'email'), by.y = c('pl_sampleid', 'email'))
colnames(dataall)


dataStrata <- read.csv('stratav0_4326_100m.csv')
head(dataStrata)
dataStrata[,c(2, 4, 5)]
dataStrata[5]<-'strataName'

## Merge data
#CEO has pl_strata
dataSBP<- merge(dataall, dataStrata[c(2, 4, 5)], by.x= 'pl_strata', by.y = 'map_value', all.x = T)
head(dataSBP)
colnames(dataSBP)
rm(dataCEOYR, dataall, dataGEE, dataStrata)

##################################
#############################

## 'YrLossCEO', 'YrGainCEO'
##CEO stand age calcs
CEOStandAge <- data.frame(matrix(ncol = 30, nrow = length(dataSBP$LC_forest_2021CEO)))
CEOStandAge[, 1] <- rep(0, length(dataSBP$LC_forest_2021CEO))

CEOStandAge[dataSBP$LC_forest_2021CEO ==100, 1] <- MaxAge
CEOStandAge[dataSBP$YrLossCEO<9999, 1] = 2021 - dataSBP$YrLossCEO[dataSBP$YrLossCEO<9999]
colnames(CEOStandAge)[1] <- 'carbon2021'
##CEO stand age calcs
for (i in 2:32){
  prev <- i - 1
  CEOStandAge[,i] <- CEOStandAge[,prev] - 1
  CEOStandAge[CEOStandAge[,prev]==0,i] <-0
  colnames(CEOStandAge)[i] <- paste0('carbon', 2022-i)
}


MaxAge<-80

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


#############################
### nat regen forest europe:
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
## Area weighted estimates
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

C_ConEst <- C_ConEst * 0.09

C_ConEst <- cbind(Year, C_ConEst)
colnames(C_ConEst)[3]<-'SE'

write.csv(C_ConEst, file = 'results\\coniferCarbonEst.csv')

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


