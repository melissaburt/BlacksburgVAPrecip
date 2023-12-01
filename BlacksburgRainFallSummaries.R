#Blacksburg Precipitation Summary 
#Dataset comes from publicly availble weather data
#Code written by: Melissa Burt
#Last Update: March 2023

#Objective: MAB is using the following code to determine frequency of rain events in the past. 

#Load the following packages:
library(tidyr)
library(dplyr)
library(lubridate)
library(simmer)
library(xts)
library(tsbox)
library(forecast)


#Read in data
PrecipDF<-BlacksburgPrecipTemp_1950to1990 #import from files first
str(PrecipDF)

#Change station to factor 
PrecipDF$STATION<-as.factor(PrecipDF$STATION)
PrecipDF$Season<-as.factor(PrecipDF$Season)

#Make R recognize date column (currently reads in as chr; need to tell R that the century 
#is 19, will automatically read in as 20)
PrecipDF$DATE<-as.Date(PrecipDF$DATE,format="%m/%d/%y") %>% format("19%y%m%d") %>% as.Date("%Y%m%d")

str(PrecipDF)
#Summmarize Precip information
#Replace NAs with 0s (note, these should be considered missing data)
PrecipDF <- PrecipDF %>% 
  mutate(PRCP = ifelse(is.na(PRCP), 0, PRCP),
         TMAX = ifelse(is.na(TMAX), 0, TMAX),
         TMIN = ifelse(is.na(TMIN), 0, TMIN))

##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
#Summarize precip data 
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################

#Sum of rain amount across months
PrecipDF_MonthSummaries <- PrecipDF %>%
  group_by(month = lubridate::floor_date(DATE, "month)")) %>%
  summarize(totalprecip = sum(PRCP))
summary(PrecipDF_MonthSummaries)
plot(PrecipDF_MonthSummaries$month, PrecipDF_MonthSummaries$totalprecip)

PrecipDF_MonthSummaries <- as.data.frame(PrecipDF_MonthSummaries)
str(PrecipDF_MonthSummaries)

PrecipDF_YearSummaries <- PrecipDF %>%
  group_by(year = lubridate::floor_date(DATE, "year)")) %>%
  summarize(totalprecip = sum(PRCP))

#Summarized across whole year
summary(PrecipDF_YearSummaries)
sd(PrecipDF_YearSummaries$totalprecip) #150.6565
var(PrecipDF_YearSummaries$totalprecip) #22697.37
median(PrecipDF_YearSummaries$totalprecip) #995.6


#Sumarized by across season

str(PrecipDF_MonthSummaries)

#Filter into season datasets (haven't gotten this to work yet)

#Spring (March, April, May)
  #select only spring months in dataset
  str(PrecipDF_MonthSummaries)
  
SpringWeather<-PrecipDF %>%
  filter(Season == "Spring")

SpringWeather$PRCP_YorN<-NA
SpringWeather$PRCP_YorN<-ifelse(SpringWeather$PRCP == 0, 0, 1)

summary(SpringWeather)

PrecipDF_SpringSummaries_TotalPRCP <- SpringWeather %>%
  group_by(year = lubridate::floor_date(DATE, "year)")) %>%
  summarize(totalprecip = sum(PRCP))

PrecipDF_SpringSummaries_Freq <- SpringWeather %>%
  group_by(year = lubridate::floor_date(DATE, "year)")) %>%
  summarize(PRCP_FREQ = sum(PRCP_YorN))

#write.csv(PrecipDF_SpringSummaries, "PrecipDF_SpringSummaries.csv")
#write.csv(SpringWeather, "SpringWeather.csv")

#Summarized across whole year
summary(PrecipDF_SpringSummaries_TotalPRCP)
sd(PrecipDF_SpringSummaries_TotalPRCP$totalprecip) #75.51005
var(PrecipDF_SpringSummaries_TotalPRCP$totalprecip) #5701.768
median(PrecipDF_SpringSummaries_TotalPRCP$totalprecip) #270.3
mean(PrecipDF_SpringSummaries_TotalPRCP$totalprecip) #284.7024

hist(PrecipDF_SpringSummaries_TotalPRCP$totalprecip)

#Cut data to be within 1 stnd deviation of the mean
str(PrecipDF_SpringSummaries_TotalPRCP)
PrecipDF_SpringSummaries_SubsetWithinMean <- PrecipDF_SpringSummaries_TotalPRCP[PrecipDF_SpringSummaries_TotalPRCP$totalprecip>209.1924 & PrecipDF_SpringSummaries_TotalPRCP$totalprecip < 360.2124,]


str(PrecipDF_SpringSummaries_Freq)
summary(PrecipDF_SpringSummaries_Freq)
sd(PrecipDF_SpringSummaries_Freq$PRCP_FREQ) #5.953068
var(PrecipDF_SpringSummaries_Freq$PRCP_FREQ) #35.43902
median(PrecipDF_SpringSummaries_Freq$PRCP_FREQ) #36
mean(PrecipDF_SpringSummaries_Freq$PRCP_FREQ) #35.2439

PrecipFREQDF_SpringSummaries_SubsetWithinMean <- PrecipDF_SpringSummaries_Freq[PrecipDF_SpringSummaries_Freq$PRCP_FREQ>29.29083 & PrecipDF_SpringSummaries_Freq$PRCP_FREQ < 41.19697,]

#To select the spring to use for my watering schedule, I first subsetted based on whether
#the years fit within both the standard deviation of the mean magnitude and mean frequency 
#of precip. I then randomly selected among the years that fit both of these requirements.

###########################
#Summer(June, July, August)
###########################

SummerWeather<-PrecipDF %>%
  filter(Season == "Summer")

SummerWeather$PRCP_YorN<-NA
SummerWeather$PRCP_YorN<-ifelse(SummerWeather$PRCP == 0, 0, 1)

summary(SummerWeather)

PrecipDF_SummerSummaries_TotalPRCP <- SummerWeather %>%
  group_by(year = lubridate::floor_date(DATE, "year)")) %>%
  summarize(totalprecip = sum(PRCP))

PrecipDF_SummerSummaries_Freq <- SummerWeather %>%
  group_by(year = lubridate::floor_date(DATE, "year)")) %>%
  summarize(PRCP_FREQ = sum(PRCP_YorN))

#Summarized across whole year
summary(PrecipDF_SummerSummaries_TotalPRCP)
sd(PrecipDF_SummerSummaries_TotalPRCP$totalprecip) #60.03117
var(PrecipDF_SummerSummaries_TotalPRCP$totalprecip) #3603.741
median(PrecipDF_SummerSummaries_TotalPRCP$totalprecip) #261.7
mean(PrecipDF_SummerSummaries_TotalPRCP$totalprecip) #273.4341

hist(PrecipDF_SummerSummaries_TotalPRCP$totalprecip)

#Cut data to be within 1 stnd deviation of the mean
str(PrecipDF_SummerSummaries_TotalPRCP)
PrecipDF_SummerSummaries_SubsetWithinMean <- PrecipDF_SummerSummaries_TotalPRCP[PrecipDF_SummerSummaries_TotalPRCP$totalprecip>213.40293 & PrecipDF_SummerSummaries_TotalPRCP$totalprecip < 333.465,]
#write.csv(PrecipDF_SummerSummaries_SubsetWithinMean, "PrecipDF_SummerSummaries_SubsetWithinMean.csv")

str(PrecipDF_SummerSummaries_Freq)
summary(PrecipDF_SummerSummaries_Freq)
sd(PrecipDF_SummerSummaries_Freq$PRCP_FREQ) #5.198851
var(PrecipDF_SummerSummaries_Freq$PRCP_FREQ) #27.02805
median(PrecipDF_SummerSummaries_Freq$PRCP_FREQ) #33
mean(PrecipDF_SummerSummaries_Freq$PRCP_FREQ) #33.85366

PrecipFREQDF_SummerSummaries_SubsetWithinMean <- PrecipDF_SummerSummaries_Freq[PrecipDF_SummerSummaries_Freq$PRCP_FREQ>28.654809 & PrecipDF_SummerSummaries_Freq$PRCP_FREQ < 39.052511,]

#write.csv(PrecipFREQDF_SummerSummaries_SubsetWithinMean, "PrecipFREQDF_SummerSummaries_SubsetWithinMean.csv")
####################################
#Winter (September, October, November)
####################################
FallWeather<-PrecipDF %>%
  filter(Season == "Fall")

FallWeather$PRCP_YorN<-NA
FallWeather$PRCP_YorN<-ifelse(FallWeather$PRCP == 0, 0, 1)

summary(FallWeather)

PrecipDF_FallSummaries_TotalPRCP <- FallWeather %>%
  group_by(year = lubridate::floor_date(DATE, "year)")) %>%
  summarize(totalprecip = sum(PRCP))

PrecipDF_FallSummaries_Freq <- FallWeather %>%
  group_by(year = lubridate::floor_date(DATE, "year)")) %>%
  summarize(PRCP_FREQ = sum(PRCP_YorN))

#Summarized across whole year
summary(PrecipDF_FallSummaries_TotalPRCP)
sd(PrecipDF_FallSummaries_TotalPRCP$totalprecip) #87.08572
var(PrecipDF_FallSummaries_TotalPRCP$totalprecip) #7583.922
median(PrecipDF_FallSummaries_TotalPRCP$totalprecip) #218.4
mean(PrecipDF_FallSummaries_TotalPRCP$totalprecip) #238.3878

hist(PrecipDF_FallSummaries_TotalPRCP$totalprecip)

#Cut data to be within 1 stnd deviation of the mean
str(PrecipDF_FallSummaries_TotalPRCP)
PrecipDF_FallSummaries_SubsetWithinMean <- PrecipDF_FallSummaries_TotalPRCP[PrecipDF_FallSummaries_TotalPRCP$totalprecip>151.30208 & PrecipDF_FallSummaries_TotalPRCP$totalprecip < 325.47352,]
write.csv()
#write.csv(PrecipDF_FallSummaries_SubsetWithinMean, "PrecipDF_FallSummaries_SubsetWithinMean.csv")


str(PrecipDF_FallSummaries_Freq)
summary(PrecipDF_FallSummaries_Freq)
sd(PrecipDF_FallSummaries_Freq$PRCP_FREQ) #6.563201
var(PrecipDF_FallSummaries_Freq$PRCP_FREQ) #43.07561
median(PrecipDF_FallSummaries_Freq$PRCP_FREQ) #27
mean(PrecipDF_FallSummaries_Freq$PRCP_FREQ) #26.78049

PrecipFREQDF_FallSummaries_SubsetWithinMean <- PrecipDF_FallSummaries_Freq[PrecipDF_FallSummaries_Freq$PRCP_FREQ>20.217289 & PrecipDF_FallSummaries_Freq$PRCP_FREQ < 33.343691,]
#write.csv(PrecipFREQDF_FallSummaries_SubsetWithinMean, "PrecipFREQDF_FallSummaries_SubsetWithinMean.csv")


######################################
#Winter  (December, January, February)
######################################
WinterWeather<-PrecipDF %>%
  filter(Season == "Winter")

WinterWeather$PRCP_YorN<-NA
WinterWeather$PRCP_YorN<-ifelse(WinterWeather$PRCP == 0, 0, 1)

summary(WinterWeather)

PrecipDF_WinterSummaries_TotalPRCP <- WinterWeather %>%
  group_by(year = lubridate::floor_date(DATE, "year)")) %>%
  summarize(totalprecip = sum(PRCP))

PrecipDF_WinterSummaries_Freq <- WinterWeather %>%
  group_by(year = lubridate::floor_date(DATE, "year)")) %>%
  summarize(PRCP_FREQ = sum(PRCP_YorN))

#Summarized across whole year
summary(PrecipDF_WinterSummaries_TotalPRCP)
sd(PrecipDF_WinterSummaries_TotalPRCP$totalprecip) #66.90492
var(PrecipDF_WinterSummaries_TotalPRCP$totalprecip) #4476.268
median(PrecipDF_WinterSummaries_TotalPRCP$totalprecip) #216.5
mean(PrecipDF_WinterSummaries_TotalPRCP$totalprecip) #217.9659

hist(PrecipDF_WinterSummaries_TotalPRCP$totalprecip)

#Cut data to be within 1 stnd deviation of the mean
str(PrecipDF_WinterSummaries_TotalPRCP)
PrecipDF_WinterSummaries_SubsetWithinMean <- PrecipDF_WinterSummaries_TotalPRCP[PrecipDF_WinterSummaries_TotalPRCP$totalprecip>151.06098 & PrecipDF_WinterSummaries_TotalPRCP$totalprecip < 284.87082,]


str(PrecipDF_WinterSummaries_Freq)
summary(PrecipDF_WinterSummaries_Freq)
sd(PrecipDF_WinterSummaries_Freq$PRCP_FREQ) #6.517519
var(PrecipDF_WinterSummaries_Freq$PRCP_FREQ) #42.47805
median(PrecipDF_WinterSummaries_Freq$PRCP_FREQ) #29
mean(PrecipDF_WinterSummaries_Freq$PRCP_FREQ) #29.85366

PrecipFREQDF_WinterSummaries_SubsetWithinMean <- PrecipDF_WinterSummaries_Freq[PrecipDF_WinterSummaries_Freq$PRCP_FREQ>23.336141 & PrecipDF_WinterSummaries_Freq$PRCP_FREQ < 36.371179,]





  


