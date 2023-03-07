#Blacksburg Precipitation Summary 
#Dataset comes from publicly availble weather data
#Code written by: Melissa Burt
#Last Update: March 2023

#Objective: MAB is using the following code to determine frequency of rain events in the past. 

#Load the following packages:
library(tidyr)
library(dplyr)

#Read in data
PrecipDF<-RainPerDay_1970to2022
str(PrecipDF)

#Change station to factor 
PrecipDF$STATION<-as.factor(PrecipDF$STATION)

#Calculate the average rain event for each Station
PrecipAveragePerDay <- PrecipDF %>%
  #group_by(STATION) %>%
  summarize(MeanPrecip = mean(PRCP))
