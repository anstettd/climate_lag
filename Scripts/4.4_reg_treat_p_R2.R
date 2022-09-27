##########################################################################################################
## Get the deltaAIC table for the global comparison within each region treatment combinations
## Author HB & DA
## 
##
## Last Modified September 26, 2022
##########################################################################################################
##########################################################################################################
# Clear environment
rm(list = ls())

library(tidyverse)
library(lme4)
library(lmtest)
library(car)
library(visreg)
library(cowplot)
library(Hmisc)
library(MuMIn)

##########################################################################################################
#Function

#Import files

y9 <- read.csv("Data/y9.csv", header=T) #Import trait & lag data

#filter by region
north_wet <- filter(y9,Region=="North" & Drought=="W")
north_dry <- filter(y9,Region=="North" & Drought=="D")
centre_wet <- filter(y9, Region=="Center" & Drought=="W")
centre_dry <- filter(y9, Region=="Center" & Drought=="D")
south_wet <- filter(y9, Region=="South" & Drought=="W")
south_dry <- filter(y9, Region=="South" & Drought=="D")

y_data <- north_wet



lmer_info <- function(y_data){

  
  return()
}
df_trait <- data.frame()
trait0 <- lmer(SLA ~  MAPA_lag0 + (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
trait0a <- lmer(SLA ~  (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
lr_trait0 <- lrtest(trait0,trait0a)
df_trait[1,1] <- lr_trait0$LogLik[1] - lr_trait0$LogLik[2]
df_trait[1,2] <- lr_trait0$`Pr(>Chisq)`[2]
df_trait[1,3] <- lr_trait0$`Pr(>Chisq)`[2]







#MAPA lag 0
sla0 <- lmer(SLA ~  MAPA_lag0 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
sla1 <- lmer(SLA ~  MAPA_lag1 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
sla2 <- lmer(SLA ~  MAPA_lag2 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
sla01 <- lmer(SLA ~  MAPA_lag01 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
sla012 <- lmer(SLA ~  MAPA_lag012 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)

fl0 <- lmer(Experiment_Date ~  MAPA_lag0 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
fl1 <- lmer(Experiment_Date ~  MAPA_lag1 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
fl2 <- lmer(Experiment_Date ~  MAPA_lag2 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
fl01 <- lmer(Experiment_Date ~  MAPA_lag01 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
fl012 <- lmer(Experiment_Date ~  MAPA_lag012 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)

#MAPA no main effect
sla0a <- lmer(SLA ~  (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
sla1a <- lmer(SLA ~  (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
sla2a <- lmer(SLA ~  (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
sla01a <- lmer(SLA ~  (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
sla012a <- lmer(SLA ~  (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)

fl0a <- lmer(Experiment_Date ~  (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
fl1a <- lmer(Experiment_Date ~  (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
fl2a <- lmer(Experiment_Date ~  (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
fl01a <- lmer(Experiment_Date ~   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
fl012a <- lmer(Experiment_Date ~  (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)

#LR test






