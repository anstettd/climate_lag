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

lmer_info <- function(y_data, y_trait, env){
  df_trait <- data.frame()
  trait0 <- lmer(y_trait ~  env + Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  trait0a <- lmer(y_trait ~  Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  lr_trait0 <- lrtest(trait0,trait0a)
  df_trait[1,1] <- lr_trait0$LogLik[1] - lr_trait0$LogLik[2]
  df_trait[1,2] <- lr_trait0$`Pr(>Chisq)`[2]
  df_trait[1,3] <- r.squaredGLMM(trait0)[1]
  df_trait[1,4] <- r.squaredGLMM(trait0)[2]
  
  return(df_trait)
}



##########################################################################################################
#Import files

y9 <- read.csv("Data/y9.csv", header=T) #Import trait & lag data

#filter by region
north_wet <- filter(y9,Region=="North" & Drought=="W")
north_dry <- filter(y9,Region=="North" & Drought=="D")
centre_wet <- filter(y9, Region=="Center" & Drought=="W")
centre_dry <- filter(y9, Region=="Center" & Drought=="D")
south_wet <- filter(y9, Region=="South" & Drought=="W")
south_dry <- filter(y9, Region=="South" & Drought=="D")


##########################################################################################################
#Test Case
#y_data <- north_wet

#df_trait <- data.frame()
#trait0 <- lmer(SLA ~  MAPA_lag0 + (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
#trait0a <- lmer(SLA ~  (1|Block) + (1|Year) + (1|Site_Lat/Family), data=y_data)
#lr_trait0 <- lrtest(trait0,trait0a)
#df_trait[1,1] <- lr_trait0$LogLik[1] - lr_trait0$LogLik[2]
#df_trait[1,2] <- lr_trait0$`Pr(>Chisq)`[2]
#df_trait[1,3] <- r.squaredGLMM(trait0)[2]

##########################################################################################################
lmer_table <- data.frame()

#North Wet
lmer_table[1,1:4] <-lmer_info(north_wet, north_wet$SLA, north_wet$MAPA_lag0)
lmer_table[2,1:4] <-lmer_info(north_wet, north_wet$Experiment_Date, north_wet$MAPA_lag0)
lmer_table[3,1:4] <-lmer_info(north_wet, north_wet$Experiment_Date, north_wet$MAPA_lag1)
lmer_table[4,1:4] <-lmer_info(north_wet, north_wet$Experiment_Date, north_wet$MAPA_lag2)

#North Dry
lmer_table[5,1:4] <-lmer_info(north_dry, north_dry$SLA, north_dry$MAPA_lag2)
lmer_table[6,1:4] <-lmer_info(north_dry, north_dry$Experiment_Date, north_dry$MAPA_lag0)
lmer_table[7,1:4] <-lmer_info(north_dry, north_dry$Experiment_Date, north_dry$MAPA_lag1)
lmer_table[8,1:4] <-lmer_info(north_dry, north_dry$Experiment_Date, north_dry$MAPA_lag2)
lmer_table[9,1:4] <-lmer_info(north_dry, north_dry$Experiment_Date, north_dry$MAPA_lag01)
lmer_table[10,1:4] <-lmer_info(north_dry, north_dry$Experiment_Date, north_dry$MAPA_lag012)

#Centre Wet
lmer_table[11,1:4] <-lmer_info(centre_wet, centre_wet$SLA, centre_wet$MAPA_lag0)
lmer_table[12,1:4] <-lmer_info(centre_wet, centre_wet$SLA, centre_wet$MAPA_lag1)
lmer_table[13,1:4] <-lmer_info(centre_wet, centre_wet$SLA, centre_wet$MAPA_lag2)
lmer_table[14,1:4] <-lmer_info(centre_wet, centre_wet$SLA, centre_wet$MAPA_lag012)
lmer_table[15,1:4] <-lmer_info(centre_wet, centre_wet$Experiment_Date, centre_wet$MAPA_lag0)
lmer_table[16,1:4] <-lmer_info(centre_wet, centre_wet$Experiment_Date, centre_wet$MAPA_lag1)
lmer_table[17,1:4] <-lmer_info(centre_wet, centre_wet$Experiment_Date, centre_wet$MAPA_lag2)
lmer_table[18,1:4] <-lmer_info(centre_wet, centre_wet$Experiment_Date, centre_wet$MAPA_lag01)
lmer_table[19,1:4] <-lmer_info(centre_wet, centre_wet$Experiment_Date, centre_wet$MAPA_lag012)

#Centre Dry
lmer_table[20,1:4] <-lmer_info(centre_dry, centre_dry$SLA, centre_dry$MAPA_lag0)
lmer_table[21,1:4] <-lmer_info(centre_dry, centre_dry$SLA, centre_dry$MAPA_lag1)
lmer_table[22,1:4] <-lmer_info(centre_dry, centre_dry$SLA, centre_dry$MAPA_lag2)
lmer_table[23,1:4] <-lmer_info(centre_dry, centre_dry$Experiment_Date, centre_dry$MAPA_lag0)
lmer_table[24,1:4] <-lmer_info(centre_dry, centre_dry$Experiment_Date, centre_dry$MAPA_lag2)

#South Wet
lmer_table[25,1:4] <-lmer_info(south_wet, south_wet$SLA, south_wet$MAPA_lag1)
lmer_table[26,1:4] <-lmer_info(south_wet, south_wet$SLA, south_wet$MAPA_lag2)
lmer_table[27,1:4] <-lmer_info(south_wet, south_wet$SLA, south_wet$MAPA_lag2)
lmer_table[28,1:4] <-lmer_info(south_wet, south_wet$SLA, south_wet$MAPA_lag012)

#South Dry
lmer_table[29,1:4] <-lmer_info(south_dry, south_dry$SLA, south_dry$MAPA_lag0)
lmer_table[30,1:4] <-lmer_info(south_dry, south_dry$SLA, south_dry$MAPA_lag1)
lmer_table[31,1:4] <-lmer_info(south_dry, south_dry$SLA, south_dry$MAPA_lag2)
lmer_table[32,1:4] <-lmer_info(south_dry, south_dry$SLA, south_dry$MAPA_lag01)
lmer_table[33,1:4] <-lmer_info(south_dry, south_dry$SLA, south_dry$MAPA_lag012)

lmer_table[34,1:4] <-lmer_info(south_dry, south_dry$SLA, south_dry$MATA_lag0)

lmer_table[35,1:4] <-lmer_info(south_dry, south_dry$Experiment_Date, south_dry$MAPA_lag0)
lmer_table[36,1:4] <-lmer_info(south_dry, south_dry$Experiment_Date, south_dry$MAPA_lag1)
lmer_table[37,1:4] <-lmer_info(south_dry, south_dry$Experiment_Date, south_dry$MAPA_lag2)
lmer_table[38,1:4] <-lmer_info(south_dry, south_dry$Experiment_Date, south_dry$MAPA_lag01)
lmer_table[39,1:4] <-lmer_info(south_dry, south_dry$Experiment_Date, south_dry$MAPA_lag012)

#Export
write.table(lmer_table, file = "Data/lmer_table.csv", sep = ",", row.names = T)













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






