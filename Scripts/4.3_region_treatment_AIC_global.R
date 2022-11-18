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
#Function

get_AIC <- function(y_data){
  
  #SPEI models
  n_sla_lag0 <- lmer(SLA ~  lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag1 <- lmer(SLA ~  lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag2 <- lmer(SLA ~  lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag01 <- lmer(SLA ~  lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag012 <- lmer(SLA ~  lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_fl_lag0 <- lmer(Experiment_Date ~  lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag1 <- lmer(Experiment_Date ~  lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag2 <- lmer(Experiment_Date ~  lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag01 <- lmer(Experiment_Date ~  lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag012 <- lmer(Experiment_Date ~  lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  #MATA models
  n_MATA_sla_lag0 <- lmer(SLA ~  MATA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag1 <- lmer(SLA ~  MATA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag2 <- lmer(SLA ~  MATA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag01 <- lmer(SLA ~  MATA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag012 <- lmer(SLA ~  MATA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_MATA_fl_lag0 <- lmer(Experiment_Date ~  MATA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag1 <- lmer(Experiment_Date ~  MATA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag2 <- lmer(Experiment_Date ~  MATA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag01 <- lmer(Experiment_Date ~  MATA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag012 <- lmer(Experiment_Date ~  MATA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  #MAPA models
  n_MAPA_sla_lag0 <- lmer(SLA ~  MAPA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag1 <- lmer(SLA ~  MAPA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag2 <- lmer(SLA ~  MAPA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag01 <- lmer(SLA ~  MAPA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag012 <- lmer(SLA ~  MAPA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_MAPA_fl_lag0 <- lmer(Experiment_Date ~  MAPA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag1 <- lmer(Experiment_Date ~  MAPA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag01 <- lmer(Experiment_Date ~  MAPA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag012 <- lmer(Experiment_Date ~  MAPA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  #CMDA models
  n_CMDA_sla_lag0 <- lmer(SLA ~  CMDA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag1 <- lmer(SLA ~  CMDA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag2 <- lmer(SLA ~  CMDA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag01 <- lmer(SLA ~  CMDA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag012 <- lmer(SLA ~  CMDA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_CMDA_fl_lag0 <- lmer(Experiment_Date ~  CMDA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag1 <- lmer(Experiment_Date ~  CMDA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag2 <- lmer(Experiment_Date ~  CMDA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag01 <- lmer(Experiment_Date ~  CMDA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag012 <- lmer(Experiment_Date ~  CMDA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  
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

  
  #Make Global Trait specific AIC Table
  lag_AIC_SLA <- as.data.frame(cbind(n_lag_AIC[,1],n_lag_AIC[,3],n_lag_AIC[,5],n_lag_AIC[,7]))
  lag_AIC_DF <- as.data.frame(cbind(n_lag_AIC[,2],n_lag_AIC[,4],n_lag_AIC[,6],n_lag_AIC[,8]))
  names(lag_AIC_SLA) <- c("SPEI","CMDA","MAPA","MATA")
  names(lag_AIC_DF) <- c("SPEI","CMDA","MAPA","MATA")
  delta_AIC_SLA <- lag_AIC_SLA-min(lag_AIC_SLA)
  delta_AIC_DF <- lag_AIC_DF-min(lag_AIC_DF)
  
  global_delta_aic <- cbind(delta_AIC_SLA,delta_AIC_DF)

  return(global_delta_aic)
}

##########################################################################################################

get_AIC_north_d <- function(y_data){
  
  #SPEI models
  n_sla_lag0 <- lmer(SLA ~  lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag1 <- lmer(SLA ~  lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag2 <- lmer(SLA ~  lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag01 <- lmer(SLA ~  lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag012 <- lmer(SLA ~  lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_fl_lag0 <- lmer(Experiment_Date ~  lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag1 <- lmer(Experiment_Date ~  lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag2 <- lmer(Experiment_Date ~  lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag01 <- lmer(Experiment_Date ~  lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag012 <- lmer(Experiment_Date ~  lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  #MATA models
  n_MATA_sla_lag0 <- lmer(SLA ~  MATA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag1 <- lmer(SLA ~  MATA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag2 <- lmer(SLA ~  MATA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag01 <- lmer(SLA ~  MATA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag012 <- lmer(SLA ~  MATA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_MATA_fl_lag0 <- lmer(Experiment_Date ~  MATA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag1 <- lmer(Experiment_Date ~  MATA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag2 <- lmer(Experiment_Date ~  MATA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag01 <- lmer(Experiment_Date ~  MATA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag012 <- lmer(Experiment_Date ~  MATA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  #MAPA models
  n_MAPA_sla_lag0 <- lmer(SLA ~  MAPA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag1 <- lmer(SLA ~  MAPA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag2 <- lmer(SLA ~  MAPA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag01 <- lmer(SLA ~  MAPA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag012 <- lmer(SLA ~  MAPA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_MAPA_fl_lag0 <- lmer(Experiment_Date ~  MAPA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag1 <- lmer(Experiment_Date ~  MAPA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag01 <- lmer(Experiment_Date ~  MAPA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag012 <- lmer(Experiment_Date ~  MAPA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  #CMDA models
  n_CMDA_sla_lag0 <- lmer(SLA ~  CMDA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag1 <- lmer(SLA ~  CMDA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag2 <- lmer(SLA ~  CMDA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag01 <- lmer(SLA ~  CMDA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag012 <- lmer(SLA ~  CMDA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_CMDA_fl_lag0 <- lmer(Experiment_Date ~  CMDA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag1 <- lmer(Experiment_Date ~  CMDA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family),
                         control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y_data)
  n_CMDA_fl_lag2 <- lmer(Experiment_Date ~  CMDA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag01 <- lmer(Experiment_Date ~  CMDA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag012 <- lmer(Experiment_Date ~  CMDA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  
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
  
  
  #Make Global Trait specific AIC Table
  lag_AIC_SLA <- as.data.frame(cbind(n_lag_AIC[,1],n_lag_AIC[,3],n_lag_AIC[,5],n_lag_AIC[,7]))
  lag_AIC_DF <- as.data.frame(cbind(n_lag_AIC[,2],n_lag_AIC[,4],n_lag_AIC[,6],n_lag_AIC[,8]))
  names(lag_AIC_SLA) <- c("SPEI","CMDA","MAPA","MATA")
  names(lag_AIC_DF) <- c("SPEI","CMDA","MAPA","MATA")
  delta_AIC_SLA <- lag_AIC_SLA-min(lag_AIC_SLA)
  delta_AIC_DF <- lag_AIC_DF-min(lag_AIC_DF)
  
  global_delta_aic <- cbind(delta_AIC_SLA,delta_AIC_DF)
  
  return(global_delta_aic)
}

##########################################################################################################
get_AIC_centre_w <- function(y_data){
  
  #SPEI models
  n_sla_lag0 <- lmer(SLA ~  lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag1 <- lmer(SLA ~  lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag2 <- lmer(SLA ~  lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag01 <- lmer(SLA ~  lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag012 <- lmer(SLA ~  lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_fl_lag0 <- lmer(Experiment_Date ~  lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag1 <- lmer(Experiment_Date ~  lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag2 <- lmer(Experiment_Date ~  lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag01 <- lmer(Experiment_Date ~  lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag012 <- lmer(Experiment_Date ~  lag012 +   Block + (1|Year) + (1|Site_Lat/Family), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y_data)
  
  #MATA models
  n_MATA_sla_lag0 <- lmer(SLA ~  MATA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag1 <- lmer(SLA ~  MATA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag2 <- lmer(SLA ~  MATA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag01 <- lmer(SLA ~  MATA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag012 <- lmer(SLA ~  MATA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_MATA_fl_lag0 <- lmer(Experiment_Date ~  MATA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag1 <- lmer(Experiment_Date ~  MATA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag2 <- lmer(Experiment_Date ~  MATA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag01 <- lmer(Experiment_Date ~  MATA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag012 <- lmer(Experiment_Date ~  MATA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  #MAPA models
  n_MAPA_sla_lag0 <- lmer(SLA ~  MAPA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag1 <- lmer(SLA ~  MAPA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag2 <- lmer(SLA ~  MAPA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag01 <- lmer(SLA ~  MAPA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag012 <- lmer(SLA ~  MAPA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_MAPA_fl_lag0 <- lmer(Experiment_Date ~  MAPA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag1 <- lmer(Experiment_Date ~  MAPA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag01 <- lmer(Experiment_Date ~  MAPA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family),
                          control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y_data)
  n_MAPA_fl_lag012 <- lmer(Experiment_Date ~  MAPA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  #CMDA models
  n_CMDA_sla_lag0 <- lmer(SLA ~  CMDA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag1 <- lmer(SLA ~  CMDA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag2 <- lmer(SLA ~  CMDA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag01 <- lmer(SLA ~  CMDA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag012 <- lmer(SLA ~  CMDA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_CMDA_fl_lag0 <- lmer(Experiment_Date ~  CMDA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag1 <- lmer(Experiment_Date ~  CMDA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag2 <- lmer(Experiment_Date ~  CMDA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag01 <- lmer(Experiment_Date ~  CMDA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag012 <- lmer(Experiment_Date ~  CMDA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  
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
  
  
  #Make Global Trait specific AIC Table
  lag_AIC_SLA <- as.data.frame(cbind(n_lag_AIC[,1],n_lag_AIC[,3],n_lag_AIC[,5],n_lag_AIC[,7]))
  lag_AIC_DF <- as.data.frame(cbind(n_lag_AIC[,2],n_lag_AIC[,4],n_lag_AIC[,6],n_lag_AIC[,8]))
  names(lag_AIC_SLA) <- c("SPEI","CMDA","MAPA","MATA")
  names(lag_AIC_DF) <- c("SPEI","CMDA","MAPA","MATA")
  delta_AIC_SLA <- lag_AIC_SLA-min(lag_AIC_SLA)
  delta_AIC_DF <- lag_AIC_DF-min(lag_AIC_DF)
  
  global_delta_aic <- cbind(delta_AIC_SLA,delta_AIC_DF)
  
  return(global_delta_aic)
}
##########################################################################################################
get_AIC_centre_d <- function(y_data){
  
  #SPEI models
  n_sla_lag0 <- lmer(SLA ~  lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag1 <- lmer(SLA ~  lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag2 <- lmer(SLA ~  lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag01 <- lmer(SLA ~  lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_sla_lag012 <- lmer(SLA ~  lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_fl_lag0 <- lmer(Experiment_Date ~  lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag1 <- lmer(Experiment_Date ~  lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag2 <- lmer(Experiment_Date ~  lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag01 <- lmer(Experiment_Date ~  lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_fl_lag012 <- lmer(Experiment_Date ~  lag012 +   Block + (1|Year) + (1|Site_Lat/Family), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y_data)
  
  #MATA models
  n_MATA_sla_lag0 <- lmer(SLA ~  MATA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag1 <- lmer(SLA ~  MATA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag2 <- lmer(SLA ~  MATA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag01 <- lmer(SLA ~  MATA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_sla_lag012 <- lmer(SLA ~  MATA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_MATA_fl_lag0 <- lmer(Experiment_Date ~  MATA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag1 <- lmer(Experiment_Date ~  MATA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag2 <- lmer(Experiment_Date ~  MATA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag01 <- lmer(Experiment_Date ~  MATA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MATA_fl_lag012 <- lmer(Experiment_Date ~  MATA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  #MAPA models
  n_MAPA_sla_lag0 <- lmer(SLA ~  MAPA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag1 <- lmer(SLA ~  MAPA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag2 <- lmer(SLA ~  MAPA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag01 <- lmer(SLA ~  MAPA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_sla_lag012 <- lmer(SLA ~  MAPA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_MAPA_fl_lag0 <- lmer(Experiment_Date ~  MAPA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag1 <- lmer(Experiment_Date ~  MAPA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag01 <- lmer(Experiment_Date ~  MAPA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_MAPA_fl_lag012 <- lmer(Experiment_Date ~  MAPA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  #CMDA models
  n_CMDA_sla_lag0 <- lmer(SLA ~  CMDA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag1 <- lmer(SLA ~  CMDA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag2 <- lmer(SLA ~  CMDA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag01 <- lmer(SLA ~  CMDA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_sla_lag012 <- lmer(SLA ~  CMDA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  n_CMDA_fl_lag0 <- lmer(Experiment_Date ~  CMDA_lag0 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag1 <- lmer(Experiment_Date ~  CMDA_lag1 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag2 <- lmer(Experiment_Date ~  CMDA_lag2 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  n_CMDA_fl_lag01 <- lmer(Experiment_Date ~  CMDA_lag01 +   Block + (1|Year) + (1|Site_Lat/Family), 
                          control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y_data)
  n_CMDA_fl_lag012 <- lmer(Experiment_Date ~  CMDA_lag012 +   Block + (1|Year) + (1|Site_Lat/Family), data=y_data)
  
  
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
  
  
  #Make Global Trait specific AIC Table
  lag_AIC_SLA <- as.data.frame(cbind(n_lag_AIC[,1],n_lag_AIC[,3],n_lag_AIC[,5],n_lag_AIC[,7]))
  lag_AIC_DF <- as.data.frame(cbind(n_lag_AIC[,2],n_lag_AIC[,4],n_lag_AIC[,6],n_lag_AIC[,8]))
  names(lag_AIC_SLA) <- c("SPEI","CMDA","MAPA","MATA")
  names(lag_AIC_DF) <- c("SPEI","CMDA","MAPA","MATA")
  delta_AIC_SLA <- lag_AIC_SLA-min(lag_AIC_SLA)
  delta_AIC_DF <- lag_AIC_DF-min(lag_AIC_DF)
  
  global_delta_aic <- cbind(delta_AIC_SLA,delta_AIC_DF)
  
  return(global_delta_aic)
}


##########################################################################################################






##########################################################################################################
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
#Run get AIC function

AIC_north_wet <- get_AIC(north_wet)
AIC_north_dry <- get_AIC_north_d(north_dry)

AIC_centre_wet <- get_AIC_centre_w(centre_wet)
AIC_centre_dry <- get_AIC_centre_d(centre_dry)

AIC_south_wet <- get_AIC(south_wet)
AIC_south_dry <- get_AIC(south_dry)

AIC_table_N <- cbind(AIC_north_wet,AIC_north_dry)
AIC_table_C <- cbind(AIC_centre_wet,AIC_centre_dry)
AIC_table_S <- cbind(AIC_south_wet,AIC_south_dry)

AIC_table <- rbind(AIC_table_N ,AIC_table_C, AIC_table_S)

#Export
write.table(AIC_table, file = "Data/AIC_table.csv", sep = ",", row.names = T)

  
  
  
  


  
  
  
  
  
  
  
  
  
  