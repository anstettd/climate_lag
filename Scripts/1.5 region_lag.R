##########################################################################################################
## Test for within Region MAT, MAP & CMD associations with SLA & Date of FLowering
## Test importance of lags and agrigate measures
## Author HB
## 
##
## Last Modified Feb 23, 2022
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
north <- filter(y9,Region=="North")
centre <- filter(y9, Region=="Center")
centre <- filter(y9, Region=="centre")

##########################################################################################################
#################### lag models ###################################

#### North ######

#SPEI models
sla_lag0 <- lmer(SLA ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=north)
sla_lag1 <- lmer(SLA ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
sla_lag2 <- lmer(SLA ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
sla_lag01 <- lmer(SLA ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
sla_lag012 <- lmer(SLA ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

fl_lag0 <- lmer(Experiment_Date ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
fl_lag1 <- lmer(Experiment_Date ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
fl_lag2 <- lmer(Experiment_Date ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
fl_lag01 <- lmer(Experiment_Date ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
fl_lag012 <- lmer(Experiment_Date ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)

#MATA models
MATA_sla_lag0 <- lmer(SLA ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MATA_sla_lag1 <- lmer(SLA ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MATA_sla_lag2 <- lmer(SLA ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MATA_sla_lag01 <- lmer(SLA ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MATA_sla_lag012 <- lmer(SLA ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

MATA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MATA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MATA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MATA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MATA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

#MAPA models
MAPA_sla_lag0 <- lmer(SLA ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MAPA_sla_lag1 <- lmer(SLA ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MAPA_sla_lag2 <- lmer(SLA ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MAPA_sla_lag01 <- lmer(SLA ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MAPA_sla_lag012 <- lmer(SLA ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

MAPA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MAPA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MAPA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
MAPA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=north)
MAPA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

#CMDA models
CMDA_sla_lag0 <- lmer(SLA ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
CMDA_sla_lag1 <- lmer(SLA ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
CMDA_sla_lag2 <- lmer(SLA ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
CMDA_sla_lag01 <- lmer(SLA ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
CMDA_sla_lag012 <- lmer(SLA ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

CMDA_fl_lag0 <- lmer(Experiment_Date ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
CMDA_fl_lag1 <- lmer(Experiment_Date ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
CMDA_fl_lag2 <- lmer(Experiment_Date ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
CMDA_fl_lag01 <- lmer(Experiment_Date ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
CMDA_fl_lag012 <- lmer(Experiment_Date ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)


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
 ### First SLA and DF pairs are SPEI, 2nd= MATA, 3rd = MAPA, 4th= CMDA
rownames(lag_AIC) <- c("lag 0", "lag 1", "lag 2", "lag 0,1","lag 0,1,2")
write.table(lag_AIC, file = "Data/north_AIC.csv", sep = ",", row.names = T)
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
write.table(delta_AIC, file = "Data/north_deltaAIC.csv", sep = ",", row.names = T)

########################################################################################################## 
#Test models that where delta AIC < 2

############ SPEI north ############
#SLA lag 0
sla_lag0a <- lmer(SLA ~  Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(sla_lag0,sla_lag0a) #2-way interaction strong evidence

#fl lag 1
fl_lag1a <- lmer(Experiment_Date ~  Drought+lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(fl_lag1,fl_lag1a) #2-way interaction strong evidence


############ MATA north ############
#SLA lag1
MATA_sla_lag1a <- lmer(SLA ~  Drought+MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(MATA_sla_lag1,MATA_sla_lag1a) #2-way interaction strong evidence

#SLA lag 2
MATA_sla_lag2a <- lmer(SLA ~  Drought+MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(MATA_sla_lag2,MATA_sla_lag2a) #2-way interaction strong evidence

#fl lag 1
MATA_fl_lag1a <- lmer(Experiment_Date ~  Drought+MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(MATA_fl_lag1,MATA_fl_lag1a) #2-way interaction moderate evidence

#fl lag 2
MATA_fl_lag2a <- lmer(Experiment_Date ~  Drought+MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(MATA_fl_lag2,MATA_fl_lag2a) #no difference, keep simpler model

# main effects for fl lag 2
MATA_fl_no_lag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(MATA_fl_lag2a,MATA_fl_no_lag) #no difference

MATA_fl_lag2_no_drought <- lmer(Experiment_Date ~  MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(MATA_fl_no_lag,MATA_fl_lag2_no_drought) #Drought only, lag 2 doesn't explain anything

############ MAPA north ############

#SLA lag2
MAPA_sla_lag2a <- lmer(SLA ~  Drought+MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(MAPA_sla_lag2,MAPA_sla_lag2a) #2-way interaction strong evidence

#SLA lag0,1,2
MAPA_sla_lag012a <- lmer(SLA ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(MAPA_sla_lag012,MAPA_sla_lag012a) #2-way interaction very strong evidence

#fl lag1
MAPA_fl_lag1a <- lmer(Experiment_Date ~  Drought+MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(MAPA_fl_lag1,MAPA_fl_lag1a) #2-way interaction very strong evidence

#fl lag0,1
MAPA_fl_lag01a <- lmer(Experiment_Date ~  Drought+MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(MAPA_fl_lag01,MAPA_fl_lag01a) #2-way interaction very strong evidence

#fl lag0,1,2
MAPA_fl_lag012a <- lmer(Experiment_Date ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(MAPA_fl_lag012,MAPA_fl_lag012a) #2-way interaction very strong evidence


############ CMDA ############
#SLA lag2
CMDA_sla_lag2a <- lmer(SLA ~  Drought+CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(CMDA_sla_lag2,CMDA_sla_lag2a) #no evidence of difference, select simpler model
CMDA_sla_no_lag <- lmer(SLA ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
CMDA_sla_no_drought <- lmer(SLA ~  CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(CMDA_sla_no_lag,CMDA_sla_no_drought) #very strong evidenc for only drought 

#fl lag1
CMDA_fl_lag1a <- lmer(Experiment_Date ~  Drought+CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=north)
lrtest(CMDA_fl_lag1,CMDA_fl_lag1a) #no evidence for either model, choose simpler model
CMDA_fl_lag1_no_lag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
CMDA_fl_lag1_no_drought <- lmer(Experiment_Date ~  CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_fl_lag1_no_lag,CMDA_fl_lag1_no_drought) #strong evidence in favor of CMDA_lag1 model as main effect




#### centre ######

#SPEI models
sla_lag0 <- lmer(SLA ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
                 control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=centre)
sla_lag1 <- lmer(SLA ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
sla_lag2 <- lmer(SLA ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
sla_lag01 <- lmer(SLA ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
sla_lag012 <- lmer(SLA ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

fl_lag0 <- lmer(Experiment_Date ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
fl_lag1 <- lmer(Experiment_Date ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
fl_lag2 <- lmer(Experiment_Date ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
fl_lag01 <- lmer(Experiment_Date ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
fl_lag012 <- lmer(Experiment_Date ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

#MATA models
MATA_sla_lag0 <- lmer(SLA ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_sla_lag1 <- lmer(SLA ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_sla_lag2 <- lmer(SLA ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_sla_lag01 <- lmer(SLA ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_sla_lag012 <- lmer(SLA ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

MATA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

#MAPA models
MAPA_sla_lag0 <- lmer(SLA ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MAPA_sla_lag1 <- lmer(SLA ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MAPA_sla_lag2 <- lmer(SLA ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MAPA_sla_lag01 <- lmer(SLA ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MAPA_sla_lag012 <- lmer(SLA ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

MAPA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MAPA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MAPA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MAPA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
                      control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=centre)
MAPA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

#CMDA models
CMDA_sla_lag0 <- lmer(SLA ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
CMDA_sla_lag1 <- lmer(SLA ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
CMDA_sla_lag2 <- lmer(SLA ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
CMDA_sla_lag01 <- lmer(SLA ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
CMDA_sla_lag012 <- lmer(SLA ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

CMDA_fl_lag0 <- lmer(Experiment_Date ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
CMDA_fl_lag1 <- lmer(Experiment_Date ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
CMDA_fl_lag2 <- lmer(Experiment_Date ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
CMDA_fl_lag01 <- lmer(Experiment_Date ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
CMDA_fl_lag012 <- lmer(Experiment_Date ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)


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
### First SLA and DF pairs are SPEI, 2nd= MATA, 3rd = MAPA, 4th= CMDA
rownames(lag_AIC) <- c("lag 0", "lag 1", "lag 2", "lag 0,1","lag 0,1,2")
write.table(lag_AIC, file = "Data/centre_AIC.csv", sep = ",", row.names = T)
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
write.table(delta_AIC, file = "Data/centre_deltaAIC.csv", sep = ",", row.names = T)

########################################################################################################## 
#Test models that where delta AIC < 2

############ SPEI centre############
#SLA lag 0
sla_lag0a <- lmer(SLA ~  Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(sla_lag0,sla_lag0a) #2-way interaction evidence

#SLA lag 1
sla_lag1a <- lmer(SLA ~  Drought+lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(sla_lag1,sla_lag1a) #2-way interaction strong evidence

#SLA lag 2
sla_lag2a <- lmer(SLA ~  Drought+lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(sla_lag2,sla_lag2a) #2-way interaction strong evidence

#fl lag 0
fl_lag0a <- lmer(Experiment_Date ~  Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(fl_lag0,fl_lag0a) #no evidence, keep simpler 
fl_lag0_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
fl_lag0_nodrought <- lmer(Experiment_Date ~  lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(fl_lag0_nolag,fl_lag0_nodrought) # strong evidence for no lag

#fl lag 1
fl_lag1a <- lmer(Experiment_Date ~  Drought+lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(fl_lag1,fl_lag1a) #no evidence, keep simpler 
fl_lag1_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
fl_lag1_nodrought <- lmer(Experiment_Date ~  lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(fl_lag1_nolag,fl_lag1_nodrought) # strong evidence for no lag

#fl lag 2
fl_lag2a <- lmer(Experiment_Date ~  Drought+lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(fl_lag2,fl_lag2a) #no evidence, keep simpler 
fl_lag2_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
fl_lag2_nodrought <- lmer(Experiment_Date ~  lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(fl_lag2_nolag,fl_lag2_nodrought) # strong evidence for no lag


############ MATA centre ############
#SLA lag 0
MATA_sla_lag0a <- lmer(SLA ~  Drought+MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_sla_lag0,MATA_sla_lag0a) #2-way interaction evidence

#SLA lag1
MATA_sla_lag1a <- lmer(SLA ~  Drought+MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_sla_lag1,MATA_sla_lag1a) #2-way interaction strong evidence

#SLA lag2
MATA_sla_lag2a <- lmer(SLA ~  Drought+MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_sla_lag2,MATA_sla_lag2a) #2-way interaction strong evidence

#SLA lag01
MATA_sla_lag01a <- lmer(SLA ~  Drought+MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_sla_lag01,MATA_sla_lag01a) #2-way interaction strong evidence

#SLA lag012
MATA_sla_lag012a <- lmer(SLA ~  Drought+MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_sla_lag012,MATA_sla_lag012a) #2-way interaction strong evidence

#fl lag 0
MATA_fl_lag0a <- lmer(Experiment_Date ~  Drought+MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_fl_lag0,MATA_fl_lag0a) #no evidence, keep simpler
MATA_fl_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_fl_nodrought <- lmer(Experiment_Date ~  MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_fl_nolag,MATA_fl_nodrought) #no lag supported 

#fl lag 1
MATA_fl_lag1a <- lmer(Experiment_Date ~  Drought+MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_fl_lag1,MATA_fl_lag1a) #no evidence, keep simpler
MATA_fl_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_fl_nodrought <- lmer(Experiment_Date ~  MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_fl_nolag,MATA_fl_nodrought) #no lag supported

#fl lag 2
MATA_fl_lag2a <- lmer(Experiment_Date ~  Drought+MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_fl_lag2,MATA_fl_lag2a) #no difference, keep simpler model

# main effects for fl lag 2
MATA_fl_no_lag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_fl_lag2_no_drought <- lmer(Experiment_Date ~  MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_fl_no_lag,MATA_fl_lag2_no_drought) #no lag supported 

#fl lag 0,1
MATA_fl_lag01a <- lmer(Experiment_Date ~  Drought+MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_fl_lag01,MATA_fl_lag01a) #no difference, keep simpler model
# main effects for fl lag 0.1
MATA_fl_no_lag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_fl_lag01_no_drought <- lmer(Experiment_Date ~  MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_fl_no_lag,MATA_fl_lag01_no_drought) #no lag supported 

#fl lag 0,1,2
MATA_fl_lag012a <- lmer(Experiment_Date ~  Drought+MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_fl_lag012,MATA_fl_lag012a) #no difference, keep simpler model

# main effects for fl lag 2
MATA_fl_no_lag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
MATA_fl_lag012_no_drought <- lmer(Experiment_Date ~  MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MATA_fl_no_lag,MATA_fl_lag012_no_drought) #no lag supported 


############ MAPA centre ############
#SLA lag1
MAPA_sla_lag1a <- lmer(SLA ~  Drought+MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MAPA_sla_lag1,MAPA_sla_lag1a) #2-way interaction very strong evidence

#SLA lag2
MAPA_sla_lag2a <- lmer(SLA ~  Drought+MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MAPA_sla_lag2,MAPA_sla_lag2a) #2-way interaction strong evidence

#SLA lag0,1,2
MAPA_sla_lag012a <- lmer(SLA ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MAPA_sla_lag012,MAPA_sla_lag012a) #2-way interaction very strong evidence

#fl lag0
MAPA_fl_lag0a <- lmer(Experiment_Date ~  Drought+MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MAPA_fl_lag0,MAPA_fl_lag0a) #2-way interaction moderate evidence

#fl lag2
MAPA_fl_lag2a <- lmer(Experiment_Date ~  Drought+MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MAPA_fl_lag2,MAPA_fl_lag2a) #2-way interaction moderate evidence

#fl lag0,1
MAPA_fl_lag01a <- lmer(Experiment_Date ~  Drought+MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MAPA_fl_lag01,MAPA_fl_lag01a) #2-way interaction moderate evidence

#fl lag0,1,2
MAPA_fl_lag012a <- lmer(Experiment_Date ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(MAPA_fl_lag012,MAPA_fl_lag012a) #2-way interaction moderateevidence


############ CMDA ############
#SLA lag0
CMDA_sla_lag0a <- lmer(SLA ~  Drought+CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(CMDA_sla_lag0,CMDA_sla_lag0a) # 2-way moderate evidence 

#SLA lag1
CMDA_sla_lag1a <- lmer(SLA ~  Drought+CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(CMDA_sla_lag1,CMDA_sla_lag1a) #2-way moderate evidence

#SLA lag2
CMDA_sla_lag2a <- lmer(SLA ~  Drought+CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(CMDA_sla_lag2,CMDA_sla_lag2a) #no evidence of difference, select simpler model
CMDA_sla_no_lag <- lmer(SLA ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
CMDA_sla_no_drought <- lmer(SLA ~  CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(CMDA_sla_no_lag,CMDA_sla_no_drought) # no lag

#fl lag0
CMDA_fl_lag0a <- lmer(Experiment_Date ~  Drought+CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(CMDA_fl_lag0,CMDA_fl_lag0a) #strong evidence simpler
CMDA_fl_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
CMDA_fl_lag0a_nodrought <- lmer(Experiment_Date ~  CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(CMDA_fl_nolag,CMDA_fl_lag0a_nodrought) #no lag

#fl lag1
CMDA_fl_lag1a <- lmer(Experiment_Date ~  Drought+CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(CMDA_fl_lag1,CMDA_fl_lag1a) #strong evidence simpler
CMDA_fl_lag1a_nodrought <- lmer(Experiment_Date ~  CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(CMDA_fl_nolag,CMDA_fl_lag1a_nodrought) #no lag

#fl lag2
CMDA_fl_lag2a <- lmer(Experiment_Date ~  Drought+CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(CMDA_fl_lag2,CMDA_fl_lag2a) #strong evidence simpler
CMDA_fl_lag2a_nodrought <- lmer(Experiment_Date ~  CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(CMDA_fl_nolag,CMDA_fl_lag2a_nodrought) #no lag



#### South ######

#SPEI models
sla_lag0 <- lmer(SLA ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat),data=south)
sla_lag1 <- lmer(SLA ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
sla_lag2 <- lmer(SLA ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
sla_lag01 <- lmer(SLA ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
sla_lag012 <- lmer(SLA ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

fl_lag0 <- lmer(Experiment_Date ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
fl_lag1 <- lmer(Experiment_Date ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
fl_lag2 <- lmer(Experiment_Date ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
fl_lag01 <- lmer(Experiment_Date ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
fl_lag012 <- lmer(Experiment_Date ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

#MATA models
MATA_sla_lag0 <- lmer(SLA ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MATA_sla_lag1 <- lmer(SLA ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MATA_sla_lag2 <- lmer(SLA ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MATA_sla_lag01 <- lmer(SLA ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MATA_sla_lag012 <- lmer(SLA ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

MATA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MATA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MATA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MATA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MATA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

#MAPA models
MAPA_sla_lag0 <- lmer(SLA ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MAPA_sla_lag1 <- lmer(SLA ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MAPA_sla_lag2 <- lmer(SLA ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MAPA_sla_lag01 <- lmer(SLA ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MAPA_sla_lag012 <- lmer(SLA ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

MAPA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MAPA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MAPA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MAPA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
MAPA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

#CMDA models
CMDA_sla_lag0 <- lmer(SLA ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
CMDA_sla_lag1 <- lmer(SLA ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
CMDA_sla_lag2 <- lmer(SLA ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
CMDA_sla_lag01 <- lmer(SLA ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
CMDA_sla_lag012 <- lmer(SLA ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

CMDA_fl_lag0 <- lmer(Experiment_Date ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
CMDA_fl_lag1 <- lmer(Experiment_Date ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
CMDA_fl_lag2 <- lmer(Experiment_Date ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
CMDA_fl_lag01 <- lmer(Experiment_Date ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
CMDA_fl_lag012 <- lmer(Experiment_Date ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)


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
### First SLA and DF pairs are SPEI, 2nd= MATA, 3rd = MAPA, 4th= CMDA
rownames(lag_AIC) <- c("lag 0", "lag 1", "lag 2", "lag 0,1","lag 0,1,2")
write.table(lag_AIC, file = "Data/south_AIC.csv", sep = ",", row.names = T)
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
write.table(delta_AIC, file = "Data/south_deltaAIC.csv", sep = ",", row.names = T)

########################################################################################################## 
#Test models that where delta AIC < 2

############ SPEI centre############
#SLA lag 0
sla_lag0a <- lmer(SLA ~  Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(sla_lag0,sla_lag0a) #2-way interaction evidence

#SLA lag 0,1,2
sla_lag012a <- lmer(SLA ~  Drought+lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(sla_lag012,sla_lag012a) #2-way interaction strong evidence

#fl lag 0
fl_lag0a <- lmer(Experiment_Date ~  Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(fl_lag0,fl_lag0a) #no evidence, keep simpler 
fl_lag0_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
fl_lag0_nodrought <- lmer(Experiment_Date ~  lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(fl_lag0_nolag,fl_lag0_nodrought) # strong evidence for no lag

#fl lag 1
fl_lag1a <- lmer(Experiment_Date ~  Drought+lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(fl_lag1,fl_lag1a) #no evidence, keep simpler 
fl_lag1_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
fl_lag1_nodrought <- lmer(Experiment_Date ~  lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(fl_lag1_nolag,fl_lag1_nodrought) # strong evidence for no lag

#fl lag 2
fl_lag2a <- lmer(Experiment_Date ~  Drought+lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(fl_lag2,fl_lag2a) #no evidence, keep simpler 
fl_lag2_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
fl_lag2_nodrought <- lmer(Experiment_Date ~  lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(fl_lag2_nolag,fl_lag2_nodrought) # strong evidence for no lag


############ MATA centre ############
#SLA lag 0
MATA_sla_lag0a <- lmer(SLA ~  Drought+MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(MATA_sla_lag0,MATA_sla_lag0a) #2-way interaction very stong evidence

#fl lag 2
MATA_fl_lag2a <- lmer(Experiment_Date ~  Drought+MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(MATA_fl_lag2,MATA_fl_lag2a) #2-way evidence

############ MAPA centre ############

#SLA lag0,1,2
MAPA_sla_lag012a <- lmer(SLA ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(MAPA_sla_lag012,MAPA_sla_lag012a) #2-way interaction very strong evidence

#fl lag2
MAPA_fl_lag2a <- lmer(Experiment_Date ~  Drought+MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(MAPA_fl_lag2,MAPA_fl_lag2a) #2-way interaction significant evidence

#fl lag0,1,2
MAPA_fl_lag012a <- lmer(Experiment_Date ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(MAPA_fl_lag012,MAPA_fl_lag012a) #2-way interaction significant evidence


############ CMDA ############
#SLA lag0
CMDA_sla_lag0a <- lmer(SLA ~  Drought+CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_sla_lag0,CMDA_sla_lag0a) #no evidence, simpler
CMDA_sla_nolag <- lmer(SLA ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
CMDA_sla_lag0_nodrought <- lmer(SLA ~  CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_sla_nolag,CMDA_sla_lag0_nodrought) #no lag

#SLA lag1
CMDA_sla_lag1a <- lmer(SLA ~  Drought+CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_sla_lag1,CMDA_sla_lag1a) #simpler moderate evidence
CMDA_sla_lag1_nodrought <- lmer(SLA ~  CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_sla_nolag,CMDA_sla_lag1_nodrought) #no lag

#fl lag0
CMDA_fl_lag0a <- lmer(Experiment_Date ~  Drought+CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_fl_lag0,CMDA_fl_lag0a) #strong evidence simpler
CMDA_fl_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
CMDA_fl_lag0a_nodrought <- lmer(Experiment_Date ~  CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_fl_nolag,CMDA_fl_lag0a_nodrought) #no lag

#fl lag1
CMDA_fl_lag1a <- lmer(Experiment_Date ~  Drought+CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_fl_lag1,CMDA_fl_lag1a) #strong evidence simpler
CMDA_fl_lag1a_nodrought <- lmer(Experiment_Date ~  CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_fl_nolag,CMDA_fl_lag1a_nodrought) #no lag

#fl lag01
CMDA_fl_lag01a <- lmer(Experiment_Date ~  Drought+CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_fl_lag01,CMDA_fl_lag01a) #strong evidence simpler
CMDA_fl_lag01_nodrought <- lmer(Experiment_Date ~  CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_fl_nolag,CMDA_fl_lag01_nodrought) #no lag

#fl lag012
CMDA_fl_lag012a <- lmer(Experiment_Date ~  Drought+CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_fl_lag012,CMDA_fl_lag012a) #strong evidence simpler
CMDA_fl_lag012_nodrought <- lmer(Experiment_Date ~  CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(CMDA_fl_nolag,CMDA_fl_lag01_nodrought) #no lag

