##########################################################################################################
## Test for MAT, MAP & CMD associations with SLA & Date of FLowering
## Test importance of lags and agrigate measures
## Author Daniel Anstett
## 
##
## Last Modified Feb 17, 2022
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

##########################################################################################################
#################### lag models ###################################
#SPEI models
sla_lag0 <- lmer(SLA ~ Region*Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y9)
sla_lag1 <- lmer(SLA ~ Region*Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
sla_lag2 <- lmer(SLA ~ Region*Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
sla_lag01 <- lmer(SLA ~ Region*Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
sla_lag012 <- lmer(SLA ~ Region*Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)

fl_lag0 <- lmer(Experiment_Date ~ Region*Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
fl_lag1 <- lmer(Experiment_Date ~ Region*Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
fl_lag2 <- lmer(Experiment_Date ~ Region*Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
fl_lag01 <- lmer(Experiment_Date ~ Region*Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
fl_lag012 <- lmer(Experiment_Date ~ Region*Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)

#MATA models
MATA_sla_lag0 <- lmer(SLA ~ Region*Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MATA_sla_lag1 <- lmer(SLA ~ Region*Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MATA_sla_lag2 <- lmer(SLA ~ Region*Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MATA_sla_lag01 <- lmer(SLA ~ Region*Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MATA_sla_lag012 <- lmer(SLA ~ Region*Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)

MATA_fl_lag0 <- lmer(Experiment_Date ~ Region*Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MATA_fl_lag1 <- lmer(Experiment_Date ~ Region*Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MATA_fl_lag2 <- lmer(Experiment_Date ~ Region*Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MATA_fl_lag01 <- lmer(Experiment_Date ~ Region*Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MATA_fl_lag012 <- lmer(Experiment_Date ~ Region*Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)

#MAPA models
MAPA_sla_lag0 <- lmer(SLA ~ Region*Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MAPA_sla_lag1 <- lmer(SLA ~ Region*Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MAPA_sla_lag2 <- lmer(SLA ~ Region*Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MAPA_sla_lag01 <- lmer(SLA ~ Region*Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MAPA_sla_lag012 <- lmer(SLA ~ Region*Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)

MAPA_fl_lag0 <- lmer(Experiment_Date ~ Region*Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MAPA_fl_lag1 <- lmer(Experiment_Date ~ Region*Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MAPA_fl_lag2 <- lmer(Experiment_Date ~ Region*Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
MAPA_fl_lag01 <- lmer(Experiment_Date ~ Region*Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y9)
MAPA_fl_lag012 <- lmer(Experiment_Date ~ Region*Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)

#CMDA models
CMDA_sla_lag0 <- lmer(SLA ~ Region*Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
CMDA_sla_lag1 <- lmer(SLA ~ Region*Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
CMDA_sla_lag2 <- lmer(SLA ~ Region*Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
CMDA_sla_lag01 <- lmer(SLA ~ Region*Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
CMDA_sla_lag012 <- lmer(SLA ~ Region*Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)

CMDA_fl_lag0 <- lmer(Experiment_Date ~ Region*Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
CMDA_fl_lag1 <- lmer(Experiment_Date ~ Region*Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
CMDA_fl_lag2 <- lmer(Experiment_Date ~ Region*Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
CMDA_fl_lag01 <- lmer(Experiment_Date ~ Region*Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
CMDA_fl_lag012 <- lmer(Experiment_Date ~ Region*Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)


#################### AIC ###################################
lag_AIC <- data.frame()

#SPEI AIC
lag_AIC[1,1] <- AIC(sla_lag0)
lag_AIC[2,1] <- AIC(sla_lag1)
lag_AIC[3,1] <- AIC(sla_lag2)
lag_AIC[4,1] <- AIC(sla_lag01)
lag_AIC[5,1] <- AIC(sla_lag012)

lag_AIC[1,2] <- AIC(fl_lag0)
lag_AIC[2,2] <- AIC(fl_lag1)
lag_AIC[3,2] <- AIC(fl_lag2)
lag_AIC[4,2] <- AIC(fl_lag01)
lag_AIC[5,2] <- AIC(fl_lag012)

#MATA AIC
lag_AIC[1,3] <- AIC(MATA_sla_lag0)
lag_AIC[2,3] <- AIC(MATA_sla_lag1)
lag_AIC[3,3] <- AIC(MATA_sla_lag2)
lag_AIC[4,3] <- AIC(MATA_sla_lag01)
lag_AIC[5,3] <- AIC(MATA_sla_lag012)

lag_AIC[1,4] <- AIC(MATA_fl_lag0)
lag_AIC[2,4] <- AIC(MATA_fl_lag1)
lag_AIC[3,4] <- AIC(MATA_fl_lag2)
lag_AIC[4,4] <- AIC(MATA_fl_lag01)
lag_AIC[5,4] <- AIC(MATA_fl_lag012)

#MAPA AIC
lag_AIC[1,5] <- AIC(MAPA_sla_lag0)
lag_AIC[2,5] <- AIC(MAPA_sla_lag1)
lag_AIC[3,5] <- AIC(MAPA_sla_lag2)
lag_AIC[4,5] <- AIC(MAPA_sla_lag01)
lag_AIC[5,5] <- AIC(MAPA_sla_lag012)

lag_AIC[1,6] <- AIC(MAPA_fl_lag0)
lag_AIC[2,6] <- AIC(MAPA_fl_lag1)
lag_AIC[3,6] <- AIC(MAPA_fl_lag2)
lag_AIC[4,6] <- AIC(MAPA_fl_lag01)
lag_AIC[5,6] <- AIC(MAPA_fl_lag012)

#CMDA AIC
lag_AIC[1,7] <- AIC(CMDA_sla_lag0)
lag_AIC[2,7] <- AIC(CMDA_sla_lag1)
lag_AIC[3,7] <- AIC(CMDA_sla_lag2)
lag_AIC[4,7] <- AIC(CMDA_sla_lag01)
lag_AIC[5,7] <- AIC(CMDA_sla_lag012)

lag_AIC[1,8] <- AIC(CMDA_fl_lag0)
lag_AIC[2,8] <- AIC(CMDA_fl_lag1)
lag_AIC[3,8] <- AIC(CMDA_fl_lag2)
lag_AIC[4,8] <- AIC(CMDA_fl_lag01)
lag_AIC[5,8] <- AIC(CMDA_fl_lag012)

colnames(lag_AIC) <- c("SLA","Date of Flowering","SLA","Date of Flowering",
                       "SLA","Date of Flowering","SLA","Date of Flowering")
rownames(lag_AIC) <- c("lag 0", "lag 1", "lag 2", "lag 0,1","lag 0,1,2")
write.table(lag_AIC, file = "Data/climate_AIC.csv", sep = ",", row.names = T)
#Make delta AIC table
delta_AIC <- lag_AIC
delta_AIC[,1] <- lag_AIC[,1]-min(lag_AIC[,1])
delta_AIC[,2] <- lag_AIC[,2]-min(lag_AIC[,2])
delta_AIC[,3] <- lag_AIC[,3]-min(lag_AIC[,3])
delta_AIC[,4] <- lag_AIC[,4]-min(lag_AIC[,4])
delta_AIC[,5] <- lag_AIC[,5]-min(lag_AIC[,5])
delta_AIC[,6] <- lag_AIC[,6]-min(lag_AIC[,6])
delta_AIC[,7] <- lag_AIC[,7]-min(lag_AIC[,7])
delta_AIC[,8] <- lag_AIC[,8]-min(lag_AIC[,8])
write.table(delta_AIC, file = "Data/delta_AIC.csv", sep = ",", row.names = T)

########################################################################################################## 
#Test models that where delta AIC < 2

##SPEI

############ SLA ############
#SLA lag 0
sla_lag0a <- lmer(SLA ~ Region*Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(sla_lag0,sla_lag0a) #3-way interaction significant

#SLA lag 0,1,2
sla_lag012a <- lmer(SLA ~ Region*Drought+lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(sla_lag012,sla_lag012a) #3-way interaction significant


############ Flowering Time ############

#fl lag 1
fl_lag1a <- lmer(Experiment_Date ~ Region*Drought+lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(fl_lag1,fl_lag1a) #3-way interaction significant

#fl lag 01
fl_lag01a <- lmer(Experiment_Date ~ Region*Drought+lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(fl_lag01,fl_lag01a) #3-way interaction marginally significant

#fl lag 0,1,2
fl_lag012a <- lmer(Experiment_Date ~ Region*Drought+lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(fl_lag012,fl_lag012a) #3-way interaction significant




