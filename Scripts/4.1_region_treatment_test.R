##########################################################################################################
## Get AIC for one region drought combination
## 
## Author HB & DA
## 
##
## Last Modified September 26, 2022
##########################################################################################################
##########################################################################################################
# Clear environment
rm(list = ls())

# Get this package retrieving function
## This function will automatically load packages that you already have
## and will install packages you don't yet have then load them
ipak <- function(pkg){
  # Function written by Dr. Evan Fricke
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}

# Define the packages that the script needs
myPackages <- c("tidyverse","lme4","lmtest","car","visreg","cowplot","Hmisc")

# Load the packages
ipak(myPackages)

##########################################################################################################
#Import files

y9 <- read.csv("Data/y9.csv", header=T) #Import trait & lag data

#filter by region
north_wet <- filter(y9, Region=="South" & Drought=="D")

##########################################################################################################
##Lag Models

#SPEI models
n_sla_lag0 <- lmer(SLA ~  lag0 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_sla_lag1 <- lmer(SLA ~  lag1 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_sla_lag2 <- lmer(SLA ~  lag2 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_sla_lag01 <- lmer(SLA ~  lag01 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_sla_lag012 <- lmer(SLA ~  lag012 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)

n_fl_lag0 <- lmer(Experiment_Date ~  lag0 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_fl_lag1 <- lmer(Experiment_Date ~  lag1 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_fl_lag2 <- lmer(Experiment_Date ~  lag2 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_fl_lag01 <- lmer(Experiment_Date ~  lag01 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_fl_lag012 <- lmer(Experiment_Date ~  lag012 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)

#MATA models
n_MATA_sla_lag0 <- lmer(SLA ~  MATA_lag0 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MATA_sla_lag1 <- lmer(SLA ~  MATA_lag1 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MATA_sla_lag2 <- lmer(SLA ~  MATA_lag2 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MATA_sla_lag01 <- lmer(SLA ~  MATA_lag01 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MATA_sla_lag012 <- lmer(SLA ~  MATA_lag012 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)

n_MATA_fl_lag0 <- lmer(Experiment_Date ~  MATA_lag0 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MATA_fl_lag1 <- lmer(Experiment_Date ~  MATA_lag1 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MATA_fl_lag2 <- lmer(Experiment_Date ~  MATA_lag2 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MATA_fl_lag01 <- lmer(Experiment_Date ~  MATA_lag01 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MATA_fl_lag012 <- lmer(Experiment_Date ~  MATA_lag012 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)

#MAPA models
n_MAPA_sla_lag0 <- lmer(SLA ~  MAPA_lag0 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MAPA_sla_lag1 <- lmer(SLA ~  MAPA_lag1 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MAPA_sla_lag2 <- lmer(SLA ~  MAPA_lag2 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MAPA_sla_lag01 <- lmer(SLA ~  MAPA_lag01 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MAPA_sla_lag012 <- lmer(SLA ~  MAPA_lag012 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)

n_MAPA_fl_lag0 <- lmer(Experiment_Date ~  MAPA_lag0 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MAPA_fl_lag1 <- lmer(Experiment_Date ~  MAPA_lag1 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MAPA_fl_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MAPA_fl_lag01 <- lmer(Experiment_Date ~  MAPA_lag01 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_MAPA_fl_lag012 <- lmer(Experiment_Date ~  MAPA_lag012 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)

#CMDA models
n_CMDA_sla_lag0 <- lmer(SLA ~  CMDA_lag0 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_CMDA_sla_lag1 <- lmer(SLA ~  CMDA_lag1 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_CMDA_sla_lag2 <- lmer(SLA ~  CMDA_lag2 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_CMDA_sla_lag01 <- lmer(SLA ~  CMDA_lag01 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_CMDA_sla_lag012 <- lmer(SLA ~  CMDA_lag012 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)

n_CMDA_fl_lag0 <- lmer(Experiment_Date ~  CMDA_lag0 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_CMDA_fl_lag1 <- lmer(Experiment_Date ~  CMDA_lag1 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_CMDA_fl_lag2 <- lmer(Experiment_Date ~  CMDA_lag2 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_CMDA_fl_lag01 <- lmer(Experiment_Date ~  CMDA_lag01 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
n_CMDA_fl_lag012 <- lmer(Experiment_Date ~  CMDA_lag012 +   (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)


#################### AIC ###################################
n_lag_AIC <- data.frame()

#SPEI AIC
n_lag_AIC[1,1] <- AIC(n_sla_lag0)
n_lag_AIC[2,1] <- AIC(n_sla_lag1)
n_lag_AIC[3,1] <- AIC(n_sla_lag2)
n_lag_AIC[4,1] <- AIC(n_sla_lag01)
n_lag_AIC[5,1] <- AIC(n_sla_lag012)

n_lag_AIC[1,2] <- AIC(n_fl_lag0)
n_lag_AIC[2,2] <- AIC(n_fl_lag1)
n_lag_AIC[3,2] <- AIC(n_fl_lag2)
n_lag_AIC[4,2] <- AIC(n_fl_lag01)
n_lag_AIC[5,2] <- AIC(n_fl_lag012)

#CMDA AIC
n_lag_AIC[1,3] <- AIC(n_CMDA_sla_lag0)
n_lag_AIC[2,3] <- AIC(n_CMDA_sla_lag1)
n_lag_AIC[3,3] <- AIC(n_CMDA_sla_lag2)
n_lag_AIC[4,3] <- AIC(n_CMDA_sla_lag01)
n_lag_AIC[5,3] <- AIC(n_CMDA_sla_lag012)

n_lag_AIC[1,4] <- AIC(n_CMDA_fl_lag0)
n_lag_AIC[2,4] <- AIC(n_CMDA_fl_lag1)
n_lag_AIC[3,4] <- AIC(n_CMDA_fl_lag2)
n_lag_AIC[4,4] <- AIC(n_CMDA_fl_lag01)
n_lag_AIC[5,4] <- AIC(n_CMDA_fl_lag012)

#MAPA AIC
n_lag_AIC[1,5] <- AIC(n_MAPA_sla_lag0)
n_lag_AIC[2,5] <- AIC(n_MAPA_sla_lag1)
n_lag_AIC[3,5] <- AIC(n_MAPA_sla_lag2)
n_lag_AIC[4,5] <- AIC(n_MAPA_sla_lag01)
n_lag_AIC[5,5] <- AIC(n_MAPA_sla_lag012)

n_lag_AIC[1,6] <- AIC(n_MAPA_fl_lag0)
n_lag_AIC[2,6] <- AIC(n_MAPA_fl_lag1)
n_lag_AIC[3,6] <- AIC(n_MAPA_fl_lag2)
n_lag_AIC[4,6] <- AIC(n_MAPA_fl_lag01)
n_lag_AIC[5,6] <- AIC(n_MAPA_fl_lag012)

#MATA AIC
n_lag_AIC[1,7] <- AIC(n_MATA_sla_lag0)
n_lag_AIC[2,7] <- AIC(n_MATA_sla_lag1)
n_lag_AIC[3,7] <- AIC(n_MATA_sla_lag2)
n_lag_AIC[4,7] <- AIC(n_MATA_sla_lag01)
n_lag_AIC[5,7] <- AIC(n_MATA_sla_lag012)

n_lag_AIC[1,8] <- AIC(n_MATA_fl_lag0)
n_lag_AIC[2,8] <- AIC(n_MATA_fl_lag1)
n_lag_AIC[3,8] <- AIC(n_MATA_fl_lag2)
n_lag_AIC[4,8] <- AIC(n_MATA_fl_lag01)
n_lag_AIC[5,8] <- AIC(n_MATA_fl_lag012)

colnames(n_lag_AIC) <- c("SLA_SPEI","Date of Flowering_SPEI","SLA_CMDA","Date of Flowering_CMDA",
                         "SLA_MAPA","Date of Flowering_MAPA","SLA_MATA","Date of Flowering_MATA") 
### First SLA and DF pairs are SPEI, 2nd= MATA, 3rd = MAPA, 4th= CMDA
rownames(n_lag_AIC) <- c("lag 0", "lag 1", "lag 2", "lag 0,1","lag 0,1,2")
#write.table(n_lag_AIC, file = "Data/north_wet_AIC.csv", sep = ",", row.names = T)

#Make Global Trait specific AIC Table
lag_AIC_SLA <- as.data.frame(cbind(n_lag_AIC[,1],n_lag_AIC[,3],n_lag_AIC[,5],n_lag_AIC[,7]))
lag_AIC_DF <- as.data.frame(cbind(n_lag_AIC[,2],n_lag_AIC[,4],n_lag_AIC[,6],n_lag_AIC[,8]))
names(lag_AIC_SLA) <- c("SPEI","CMDA","MAPA","MATA")
names(lag_AIC_DF) <- c("SPEI","CMDA","MAPA","MATA")
delta_AIC_SLA <- lag_AIC_SLA-min(lag_AIC_SLA)
delta_AIC_DF <- lag_AIC_DF-min(lag_AIC_DF)

global_delta_aic <- cbind(delta_AIC_SLA,delta_AIC_DF)

#write.table(delta_AIC_SLA, file = "Data/Global_delta_AIC_SLA_North_wet.csv", sep = ",", row.names = T)
#write.table(delta_AIC_DF, file = "Data/Global_delta_AIC_DF_North_dry.csv", sep = ",", row.names = T)


#Make delta AIC table
n_delta_AIC <- data.frame()
n_delta_AIC[1:5,1] <- n_lag_AIC[,1]-min(n_lag_AIC[,1])
n_delta_AIC[1:5,2] <- n_lag_AIC[,3]-min(n_lag_AIC[,3])
n_delta_AIC[1:5,3] <- n_lag_AIC[,5]-min(n_lag_AIC[,5])
n_delta_AIC[1:5,4] <- n_lag_AIC[,7]-min(n_lag_AIC[,7])
n_delta_AIC[1:5,5] <- n_lag_AIC[,2]-min(n_lag_AIC[,2])
n_delta_AIC[1:5,6] <- n_lag_AIC[,4]-min(n_lag_AIC[,4])
n_delta_AIC[1:5,7] <- n_lag_AIC[,6]-min(n_lag_AIC[,6])
n_delta_AIC[1:5,8] <- n_lag_AIC[,8]-min(n_lag_AIC[,8])

colnames(n_delta_AIC) <- c("SPEI","CMDA","MAPA","MATA","SPEI","CMDA","MAPA","MATA")



#write.table(n_delta_AIC, file = "Data/north_wet_deltaAIC.csv", sep = ",", row.names = T)







