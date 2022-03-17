##########################################################################################################
## Organize climate lag data into autocorrelation metric
## Input trait start and end points
## Author Daniel Anstett
## 
##
## Last Modified Feb 17, 2022
##########################################################################################################

# Clear environment
rm(list = ls())

#Import libraries
library(tidyverse)
##########################################################################################################
#Import files


lat_long <- read.csv("Data/site_lat_lag.csv", header=T)  #Basic Lat/Long plus headers
env_lag0 <- read.csv("Data/env_lag0.csv", header=T) #site/year climate data
y9 <- read.csv("Data/y9.csv", header=T) #trait data

#Remove sites not in the phenotype available range
env_lag0 <- env_lag0 %>% filter(ID_Year!="S02_2015" & ID_Year!="S02_2016" &
                                  ID_Year!="S08_2015" & ID_Year!="S08_2016" &
                                  ID_Year!="S17_2010" & ID_Year!="S36_2010")

#Calculate Spatial autocorrelation single example
#https://www.statology.org/autocorrelation-in-r/

