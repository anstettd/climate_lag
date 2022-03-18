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
library(stats)
##########################################################################################################
#Import files


z1 <- read.csv("Data/site_lat_lag.csv", header=T)  #Basic Lat/Long plus headers
env_lag0 <- read.csv("Data/env_lag0.csv", header=T) #site/year climate data
y9 <- read.csv("Data/y9.csv", header=T) #trait data

#Remove sites not in the phenotype available range
env_lag0 <- env_lag0 %>% filter(ID_Year!="S02_2015" & ID_Year!="S02_2016" &
                                  ID_Year!="S08_2010" & ID_Year!="S08_2015" & ID_Year!="S08_2016" &
                                  ID_Year!="S17_2010" & ID_Year!="S36_2010")

#Input autocorr for each climate variable across sites
for(i in 1:12){
  for(j in 1:4){
    env_lag0_1 <- env_lag0 %>% filter(Site==i)
    auto_corr_1 <- acf(env_lag0_1[8+j], lag=1,pl=FALSE) #calculate lag1 autocorr
    z1[i,9+j] <- auto_corr_1$acf[2]
  }
  }

#Calculate Spatial autocorrelation single example
#https://www.statology.org/autocorrelation-in-r/
#env_lag0_1 <- env_lag0 %>% filter(Site=="1") #subset data for S02 SPEI
#auto_corr_1 <- acf(env_lag0_1$SPEI_lag0, lag=1,pl=FALSE) #calculate lag1 autocorr
#auto_corr_1$acf[2]


#Get endpoints for traits
start_order <- c(2010,2010,2010,
                 2011,2010,2010,
                 2010,2011,2010,
                 2011,2010,2010)
end_order <- c(2014,2016,2016,
               2014,2016,2016,
               2016,2016,2016,
               2016,2016,2016)
for(i in 1:12){
  start_W <- y9 %>% filter(Site==10 & Year==start_order[10] & Drought=="W") #subset data for S02 SPEI
  end_W <- y9 %>% filter(Site==10 & Year==end_order[10] & Drought=="W") #subset data for S02 SPEI
  start_SLA_W <-mean(start_W$SLA,na.rm = TRUE)
  end_SLA_W <-mean(end_W$SLA,na.rm = TRUE)
  z1[10,14] <- end_SLA_W - start_SLA_W 
}

#change_SLA_W

#Get endpoints for traits single example
#start1_W <- y9 %>% filter(Site==2 & Year=="2010" & Drought=="W") #subset data for S02 SPEI
#start_SLA_W_ave <- mean(start1_W$SLA,na.rm = TRUE)
#start_FT_W_ave <- mean(start1_W$Experiment_Date,na.rm = TRUE)











