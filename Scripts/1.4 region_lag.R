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
south <- filter(y9, Region=="South")

##########################################################################################################
#################### lag models ###################################

##########################################################################################################
#### North ######
##########################################################################################################

#SPEI models
n_sla_lag0 <- lmer(SLA ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_sla_lag1 <- lmer(SLA ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_sla_lag2 <- lmer(SLA ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_sla_lag01 <- lmer(SLA ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_sla_lag012 <- lmer(SLA ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

n_fl_lag0 <- lmer(Experiment_Date ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_fl_lag1 <- lmer(Experiment_Date ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_fl_lag2 <- lmer(Experiment_Date ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_fl_lag01 <- lmer(Experiment_Date ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_fl_lag012 <- lmer(Experiment_Date ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

#MATA models
n_MATA_sla_lag0 <- lmer(SLA ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MATA_sla_lag1 <- lmer(SLA ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MATA_sla_lag2 <- lmer(SLA ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=north)
n_MATA_sla_lag01 <- lmer(SLA ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MATA_sla_lag012 <- lmer(SLA ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

n_MATA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MATA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MATA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MATA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MATA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

#MAPA models
n_MAPA_sla_lag0 <- lmer(SLA ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MAPA_sla_lag1 <- lmer(SLA ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MAPA_sla_lag2 <- lmer(SLA ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MAPA_sla_lag01 <- lmer(SLA ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MAPA_sla_lag012 <- lmer(SLA ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

n_MAPA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MAPA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MAPA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MAPA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_MAPA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

#CMDA models
n_CMDA_sla_lag0 <- lmer(SLA ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_CMDA_sla_lag1 <- lmer(SLA ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_CMDA_sla_lag2 <- lmer(SLA ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_CMDA_sla_lag01 <- lmer(SLA ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_CMDA_sla_lag012 <- lmer(SLA ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)

n_CMDA_fl_lag0 <- lmer(Experiment_Date ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_CMDA_fl_lag1 <- lmer(Experiment_Date ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_CMDA_fl_lag2 <- lmer(Experiment_Date ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_CMDA_fl_lag01 <- lmer(Experiment_Date ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
n_CMDA_fl_lag012 <- lmer(Experiment_Date ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)


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

#MATA AIC
n_lag_AIC[1,3] <- AIC(n_MATA_sla_lag0)
n_lag_AIC[2,3] <- AIC(n_MATA_sla_lag1)
n_lag_AIC[3,3] <- AIC(n_MATA_sla_lag2)
n_lag_AIC[4,3] <- AIC(n_MATA_sla_lag01)
n_lag_AIC[5,3] <- AIC(n_MATA_sla_lag012)

n_lag_AIC[1,4] <- AIC(n_MATA_fl_lag0)
n_lag_AIC[2,4] <- AIC(n_MATA_fl_lag1)
n_lag_AIC[3,4] <- AIC(n_MATA_fl_lag2)
n_lag_AIC[4,4] <- AIC(n_MATA_fl_lag01)
n_lag_AIC[5,4] <- AIC(n_MATA_fl_lag012)

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

#CMDA AIC
n_lag_AIC[1,7] <- AIC(n_CMDA_sla_lag0)
n_lag_AIC[2,7] <- AIC(n_CMDA_sla_lag1)
n_lag_AIC[3,7] <- AIC(n_CMDA_sla_lag2)
n_lag_AIC[4,7] <- AIC(n_CMDA_sla_lag01)
n_lag_AIC[5,7] <- AIC(n_CMDA_sla_lag012)

n_lag_AIC[1,8] <- AIC(n_CMDA_fl_lag0)
n_lag_AIC[2,8] <- AIC(n_CMDA_fl_lag1)
n_lag_AIC[3,8] <- AIC(n_CMDA_fl_lag2)
n_lag_AIC[4,8] <- AIC(n_CMDA_fl_lag01)
n_lag_AIC[5,8] <- AIC(n_CMDA_fl_lag012)

colnames(n_lag_AIC) <- c("SLA","Date of Flowering","SLA","Date of Flowering",
                       "SLA","Date of Flowering","SLA","Date of Flowering") 
 ### First SLA and DF pairs are SPEI, 2nd= MATA, 3rd = MAPA, 4th= CMDA
rownames(n_lag_AIC) <- c("lag 0", "lag 1", "lag 2", "lag 0,1","lag 0,1,2")
write.table(n_lag_AIC, file = "Data/north_AIC.csv", sep = ",", row.names = T)

#Make Global Trait specific AIC Table
lag_AIC_SLA <- as.data.frame(cbind(n_lag_AIC[,1],n_lag_AIC[,3],n_lag_AIC[,5],n_lag_AIC[,7]))
lag_AIC_DF <- as.data.frame(cbind(n_lag_AIC[,2],n_lag_AIC[,4],n_lag_AIC[,6],n_lag_AIC[,8]))
names(lag_AIC_SLA) <- c("SPEI","CMDA","MAPA","MATA")
names(lag_AIC_DF) <- c("SPEI","CMDA","MAPA","MATA")
delta_AIC_SLA <- lag_AIC_SLA-min(lag_AIC_SLA)
delta_AIC_DF <- lag_AIC_DF-min(lag_AIC_DF)
write.table(delta_AIC_SLA, file = "Data/Global_delta_AIC_SLA_North.csv", sep = ",", row.names = T)
write.table(delta_AIC_DF, file = "Data/Global_delta_AIC_DF_North.csv", sep = ",", row.names = T)


#Make delta AIC table
n_delta_AIC <- n_lag_AIC
n_delta_AIC[,1] <- n_lag_AIC[,1]-min(n_lag_AIC[,1])
n_delta_AIC[,2] <- n_lag_AIC[,2]-min(n_lag_AIC[,2])
n_delta_AIC[,3] <- n_lag_AIC[,3]-min(n_lag_AIC[,3])
n_delta_AIC[,4] <- n_lag_AIC[,4]-min(n_lag_AIC[,4])
n_delta_AIC[,5] <- n_lag_AIC[,5]-min(n_lag_AIC[,5])
n_delta_AIC[,6] <- n_lag_AIC[,6]-min(n_lag_AIC[,6])
n_delta_AIC[,7] <- n_lag_AIC[,7]-min(n_lag_AIC[,7])
n_delta_AIC[,8] <- n_lag_AIC[,8]-min(n_lag_AIC[,8])
write.table(n_delta_AIC, file = "Data/north_deltaAIC.csv", sep = ",", row.names = T)

########################################################################################################## 
#Test models that where delta AIC < 2

############ SPEI north ############
#SLA lag 0
n_sla_lag0a <- lmer(SLA ~  Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_sla_lag0,n_sla_lag0a) #2-way interaction strong evidence

#fl lag 1
n_fl_lag1a <- lmer(Experiment_Date ~  Drought+lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_fl_lag1,n_fl_lag1a) #2-way interaction strong evidence


############ MATA north ############
#SLA lag1
n_MATA_sla_lag1a <- lmer(SLA ~  Drought+MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MATA_sla_lag1, n_MATA_sla_lag1a) #2-way interaction strong evidence

#SLA lag 2
n_MATA_sla_lag2a <- lmer(SLA ~  Drought+MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MATA_sla_lag2, n_MATA_sla_lag2a) #2-way interaction strong evidence


#fl lag 1
n_MATA_fl_lag1a <- lmer(Experiment_Date ~  Drought+MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MATA_fl_lag1, n_MATA_fl_lag1a) #2-way interaction moderate evidence

#fl lag 2
n_MATA_fl_lag2a <- lmer(Experiment_Date ~  Drought+MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MATA_fl_lag2, n_MATA_fl_lag2a) #no difference, keep simpler model

# main effects for fl lag 2
n_MATA_fl_no_lag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MATA_fl_lag2a, n_MATA_fl_no_lag) #no difference

n_MATA_fl_lag2_no_drought <- lmer(Experiment_Date ~  MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MATA_fl_no_lag, n_MATA_fl_lag2_no_drought) #Drought only, lag 2 doesn't explain anything

n_MATA_fl_lag2_nothing <- lmer(Experiment_Date ~ (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MATA_fl_no_lag, n_MATA_fl_lag2_nothing) #Keep drought, very strong evidence

############ MAPA north ############

### MAPA SLA Lag 0 
n_MAPA_sla_lag0a <- lmer(SLA ~  Drought+MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MAPA_sla_lag0, n_MAPA_sla_lag0a) #2-way interaction strong evidence

#SLA lag2
n_MAPA_sla_lag2a <- lmer(SLA ~  Drought+MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MAPA_sla_lag2, n_MAPA_sla_lag2a) #2-way interaction strong evidence

#SLA lag0,1,2
n_MAPA_sla_lag012a <- lmer(SLA ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MAPA_sla_lag012, n_MAPA_sla_lag012a) #2-way interaction very strong evidence


#fl lag1
n_MAPA_fl_lag1a <- lmer(Experiment_Date ~  Drought+MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MAPA_fl_lag1, n_MAPA_fl_lag1a) #2-way interaction very strong evidence

#fl lag0,1
n_MAPA_fl_lag01a <- lmer(Experiment_Date ~  Drought+MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MAPA_fl_lag01, n_MAPA_fl_lag01a) #2-way interaction very strong evidence

#fl lag0,1,2
n_MAPA_fl_lag012a <- lmer(Experiment_Date ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_MAPA_fl_lag012, n_MAPA_fl_lag012a) #2-way interaction very strong evidence


############ CMDA ############
#SLA lag2
n_CMDA_sla_lag2a <- lmer(SLA ~  Drought+CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_CMDA_sla_lag2,n_CMDA_sla_lag2a) #weak evidence for simpler model being better
n_CMDA_sla_no_lag <- lmer(SLA ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_CMDA_sla_lag2a,n_CMDA_sla_no_lag) #No difference select simpler model
n_CMDA_sla_no_drought <- lmer(SLA ~  CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_CMDA_sla_no_lag,n_CMDA_sla_no_drought) #very strong evidence for only drought 
n_CMDA_sla_nothing <- lmer(SLA ~ (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_CMDA_sla_no_lag,n_CMDA_sla_nothing) #very strong evidence for only drought 

#fl lag1
n_CMDA_fl_lag1a <- lmer(Experiment_Date ~  Drought+CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat),data=north)
lrtest(n_CMDA_fl_lag1,n_CMDA_fl_lag1a) #no evidence for either model, choose simpler model
n_CMDA_fl_lag1_no_lag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_CMDA_fl_lag1a,n_CMDA_fl_lag1_no_lag) # Select drought only
n_CMDA_fl_lag1_no_drought <- lmer(Experiment_Date ~  CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_CMDA_fl_lag1_no_lag,n_CMDA_fl_lag1_no_drought) #strong evidence in favor of CMDA_lag1 model as main effect
n_CMDA_fl_lag1_nothing <- lmer(Experiment_Date ~ (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=north)
lrtest(n_CMDA_fl_lag1_no_lag,n_CMDA_fl_lag1_nothing) ##very strong evidence for only drought 

##########################################################################################################
#### Centre ######
##########################################################################################################

#SPEI models
c_sla_lag0 <- lmer(SLA ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_sla_lag1 <- lmer(SLA ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_sla_lag2 <- lmer(SLA ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_sla_lag01 <- lmer(SLA ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_sla_lag012 <- lmer(SLA ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

c_fl_lag0 <- lmer(Experiment_Date ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_fl_lag1 <- lmer(Experiment_Date ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_fl_lag2 <- lmer(Experiment_Date ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_fl_lag01 <- lmer(Experiment_Date ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_fl_lag012 <- lmer(Experiment_Date ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

#MATA models
c_MATA_sla_lag0 <- lmer(SLA ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MATA_sla_lag1 <- lmer(SLA ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MATA_sla_lag2 <- lmer(SLA ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MATA_sla_lag01 <- lmer(SLA ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MATA_sla_lag012 <- lmer(SLA ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

c_MATA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MATA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MATA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MATA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MATA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

#MAPA models
c_MAPA_sla_lag0 <- lmer(SLA ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
                        control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=centre)
c_MAPA_sla_lag1 <- lmer(SLA ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MAPA_sla_lag2 <- lmer(SLA ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MAPA_sla_lag01 <- lmer(SLA ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MAPA_sla_lag012 <- lmer(SLA ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

c_MAPA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MAPA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MAPA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MAPA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_MAPA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

#CMDA models
c_CMDA_sla_lag0 <- lmer(SLA ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_CMDA_sla_lag1 <- lmer(SLA ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_CMDA_sla_lag2 <- lmer(SLA ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_CMDA_sla_lag01 <- lmer(SLA ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_CMDA_sla_lag012 <- lmer(SLA ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)

c_CMDA_fl_lag0 <- lmer(Experiment_Date ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_CMDA_fl_lag1 <- lmer(Experiment_Date ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_CMDA_fl_lag2 <- lmer(Experiment_Date ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_CMDA_fl_lag01 <- lmer(Experiment_Date ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
c_CMDA_fl_lag012 <- lmer(Experiment_Date ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)


#################### AIC ###################################
c_lag_AIC <- data.frame()

#SPEI AIC
c_lag_AIC[1,1] <- AIC(c_sla_lag0)
c_lag_AIC[2,1] <- AIC(c_sla_lag1)
c_lag_AIC[3,1] <- AIC(c_sla_lag2)
c_lag_AIC[4,1] <- AIC(c_sla_lag01)
c_lag_AIC[5,1] <- AIC(c_sla_lag012)

c_lag_AIC[1,2] <- AIC(c_fl_lag0)
c_lag_AIC[2,2] <- AIC(c_fl_lag1)
c_lag_AIC[3,2] <- AIC(c_fl_lag2)
c_lag_AIC[4,2] <- AIC(c_fl_lag01)
c_lag_AIC[5,2] <- AIC(c_fl_lag012)

#MATA AIC
c_lag_AIC[1,3] <- AIC(c_MATA_sla_lag0)
c_lag_AIC[2,3] <- AIC(c_MATA_sla_lag1)
c_lag_AIC[3,3] <- AIC(c_MATA_sla_lag2)
c_lag_AIC[4,3] <- AIC(c_MATA_sla_lag01)
c_lag_AIC[5,3] <- AIC(c_MATA_sla_lag012)

c_lag_AIC[1,4] <- AIC(c_MATA_fl_lag0)
c_lag_AIC[2,4] <- AIC(c_MATA_fl_lag1)
c_lag_AIC[3,4] <- AIC(c_MATA_fl_lag2)
c_lag_AIC[4,4] <- AIC(c_MATA_fl_lag01)
c_lag_AIC[5,4] <- AIC(c_MATA_fl_lag012)

#MAPA AIC
c_lag_AIC[1,5] <- AIC(c_MAPA_sla_lag0)
c_lag_AIC[2,5] <- AIC(c_MAPA_sla_lag1)
c_lag_AIC[3,5] <- AIC(c_MAPA_sla_lag2)
c_lag_AIC[4,5] <- AIC(c_MAPA_sla_lag01)
c_lag_AIC[5,5] <- AIC(c_MAPA_sla_lag012)

c_lag_AIC[1,6] <- AIC(c_MAPA_fl_lag0)
c_lag_AIC[2,6] <- AIC(c_MAPA_fl_lag1)
c_lag_AIC[3,6] <- AIC(c_MAPA_fl_lag2)
c_lag_AIC[4,6] <- AIC(c_MAPA_fl_lag01)
c_lag_AIC[5,6] <- AIC(c_MAPA_fl_lag012)

#CMDA AIC
c_lag_AIC[1,7] <- AIC(c_CMDA_sla_lag0)
c_lag_AIC[2,7] <- AIC(c_CMDA_sla_lag1)
c_lag_AIC[3,7] <- AIC(c_CMDA_sla_lag2)
c_lag_AIC[4,7] <- AIC(c_CMDA_sla_lag01)
c_lag_AIC[5,7] <- AIC(c_CMDA_sla_lag012)

c_lag_AIC[1,8] <- AIC(c_CMDA_fl_lag0)
c_lag_AIC[2,8] <- AIC(c_CMDA_fl_lag1)
c_lag_AIC[3,8] <- AIC(c_CMDA_fl_lag2)
c_lag_AIC[4,8] <- AIC(c_CMDA_fl_lag01)
c_lag_AIC[5,8] <- AIC(c_CMDA_fl_lag012)

colnames(c_lag_AIC) <- c("SLA","Date of Flowering","SLA","Date of Flowering",
                       "SLA","Date of Flowering","SLA","Date of Flowering") 
### First SLA and DF pairs are SPEI, 2nd= MATA, 3rd = MAPA, 4th= CMDA
rownames(c_lag_AIC) <- c("lag 0", "lag 1", "lag 2", "lag 0,1","lag 0,1,2")
write.table(c_lag_AIC, file = "Data/centre_AIC.csv", sep = ",", row.names = T)

#Make Global Trait specific AIC Table
lag_AIC_SLA <- as.data.frame(cbind(c_lag_AIC[,1],c_lag_AIC[,3],c_lag_AIC[,5],c_lag_AIC[,7]))
lag_AIC_DF <- as.data.frame(cbind(c_lag_AIC[,2],c_lag_AIC[,4],c_lag_AIC[,6],c_lag_AIC[,8]))
names(lag_AIC_SLA) <- c("SPEI","CMDA","MAPA","MATA")
names(lag_AIC_DF) <- c("SPEI","CMDA","MAPA","MATA")
delta_AIC_SLA <- lag_AIC_SLA-min(lag_AIC_SLA)
delta_AIC_DF <- lag_AIC_DF-min(lag_AIC_DF)
write.table(delta_AIC_SLA, file = "Data/Global_delta_AIC_SLA_Centre.csv", sep = ",", row.names = T)
write.table(delta_AIC_DF, file = "Data/Global_delta_AIC_DF_Centre.csv", sep = ",", row.names = T)

#Make delta AIC table
c_delta_AIC <- c_lag_AIC
c_delta_AIC[,1] <- c_lag_AIC[,1]-min(c_lag_AIC[,1])
c_delta_AIC[,2] <- c_lag_AIC[,2]-min(c_lag_AIC[,2])
c_delta_AIC[,3] <- c_lag_AIC[,3]-min(c_lag_AIC[,3])
c_delta_AIC[,4] <- c_lag_AIC[,4]-min(c_lag_AIC[,4])
c_delta_AIC[,5] <- c_lag_AIC[,5]-min(c_lag_AIC[,5])
c_delta_AIC[,6] <- c_lag_AIC[,6]-min(c_lag_AIC[,6])
c_delta_AIC[,7] <- c_lag_AIC[,7]-min(c_lag_AIC[,7])
c_delta_AIC[,8] <- c_lag_AIC[,8]-min(c_lag_AIC[,8])
write.table(c_delta_AIC, file = "Data/centre_deltaAIC.csv", sep = ",", row.names = T)

########################################################################################################## 
#Test models that where delta AIC < 2

############ SPEI centre############
#SLA lag 0
c_sla_lag0a <- lmer(SLA ~  Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_sla_lag0,c_sla_lag0a) #2-way interaction evidence, 0.02257

#SLA lag 1
c_sla_lag1a <- lmer(SLA ~  Drought+lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_sla_lag1,c_sla_lag1a) #2-way interaction strong evidence, 0.002031

#SLA lag 2
c_sla_lag2a <- lmer(SLA ~  Drought+lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_sla_lag2,c_sla_lag2a) #2-way interaction strong evidence, 0.002539


#fl lag 0
c_fl_lag0a <- lmer(Experiment_Date ~  Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag0,c_fl_lag0a) #no evidence, keep simpler 
c_fl_lag0_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag0a,c_fl_lag0_nolag) #no evidence, keep simpler 
c_fl_lag0_nodrought <- lmer(Experiment_Date ~  lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag0_nolag,c_fl_lag0_nodrought) # strong evidence for no lag
c_fl_lag0_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag0_nolag,c_fl_lag0_nodrought) # strong evidence for no lag, 2.2e-16

#fl lag 1
c_fl_lag1a <- lmer(Experiment_Date ~  Drought+lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag1,c_fl_lag1a) #no evidence, keep simpler 
c_fl_lag1_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag1a,c_fl_lag1_nolag) #no evidence, keep simpler 
c_fl_lag1_nodrought <- lmer(Experiment_Date ~  lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag1_nolag,c_fl_lag1_nodrought) # strong evidence for no lag
c_fl_lag1_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag1_nolag,c_fl_lag1_nothing) # strong evidence for no lag

#fl lag 2
c_fl_lag2a <- lmer(Experiment_Date ~  Drought+lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag2,c_fl_lag2a) #no evidence, keep simpler 
c_fl_lag2_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag2a,c_fl_lag2_nolag) #no evidence, keep simpler 
c_fl_lag2_nodrought <- lmer(Experiment_Date ~  lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag2_nolag,c_fl_lag2_nodrought) # strong evidence for no lag
c_fl_lag2_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_fl_lag2_nolag,c_fl_lag2_nothing) # strong evidence for no lag,2.2e-16

############ MATA centre ############
#SLA lag 0
c_MATA_sla_lag0a <- lmer(SLA ~  Drought+MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_sla_lag0,c_MATA_sla_lag0a) #2-way interaction evidence, 0.01691

#SLA lag1
c_MATA_sla_lag1a <- lmer(SLA ~  Drought+MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_sla_lag1,c_MATA_sla_lag1a) #2-way interaction strong evidence, 0.004278

#SLA lag2
c_MATA_sla_lag2a <- lmer(SLA ~  Drought+MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_sla_lag2,c_MATA_sla_lag2a) #2-way interaction strong evidence, 0.008889

#SLA lag01
c_MATA_sla_lag01a <- lmer(SLA ~  Drought+MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_sla_lag01,c_MATA_sla_lag01a) #2-way interaction strong evidence, 0.008292

#SLA lag012
c_MATA_sla_lag012a <- lmer(SLA ~  Drought+MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_sla_lag012,c_MATA_sla_lag012a) #2-way interaction strong evidence, 0.00774


#fl lag 0
c_MATA_fl_lag0a <- lmer(Experiment_Date ~  Drought+MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_lag0,c_MATA_fl_lag0a) #no evidence, keep simpler
c_MATA_fl_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_lag0a,c_MATA_fl_nolag) #no evidence, keep simpler
c_MATA_fl_nodrought <- lmer(Experiment_Date ~  MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_nolag,c_MATA_fl_nodrought) #no lag supported
c_MATA_fl_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_nolag,c_MATA_fl_nothing) # strong evidence for drought only, 2.2e-16

#fl lag 1
c_MATA_fl_lag1a <- lmer(Experiment_Date ~  Drought+MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_lag1,c_MATA_fl_lag1a) #no evidence, keep simpler
c_MATA_fl_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_lag1a,c_MATA_fl_nolag) #no evidence, keep simpler
c_MATA_fl_nodrought <- lmer(Experiment_Date ~  MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_nolag,c_MATA_fl_nodrought) #no lag supported
c_MATA_fl_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_nolag,c_MATA_fl_nothing) #strong evidence for drought only, 2.2e-16

#fl lag 2
c_MATA_fl_lag2a <- lmer(Experiment_Date ~  Drought+MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_lag2,c_MATA_fl_lag2a) #no difference, keep simpler model
c_MATA_fl_no_lag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_lag2a,c_MATA_fl_no_lag) #no difference, keep simpler model
c_MATA_fl_lag2_no_drought <- lmer(Experiment_Date ~  MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_no_lag,c_MATA_fl_lag2_no_drought) #no lag supported 
c_MATA_fl_lag2_nothing <- lmer(Experiment_Date ~ (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_no_lag,c_MATA_fl_lag2_nothing) #strong evidence for drought only, 2.2e-16

#fl lag 0,1
c_MATA_fl_lag01a <- lmer(Experiment_Date ~  Drought+MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_lag01,c_MATA_fl_lag01a) #no difference, keep simpler model
c_MATA_fl_no_lag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_lag01a,c_MATA_fl_no_lag) #no difference, keep simpler model
c_MATA_fl_lag01_no_drought <- lmer(Experiment_Date ~  MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_no_lag,c_MATA_fl_lag01_no_drought) #no lag supported 
c_MATA_fl_lag01_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_no_lag,c_MATA_fl_lag01_nothing) #strong evidence for drought only, 2.2e-16

#fl lag 0,1,2
c_MATA_fl_lag012a <- lmer(Experiment_Date ~  Drought+MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_lag012,c_MATA_fl_lag012a) #no difference, keep simpler model
c_MATA_fl_no_lag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_lag012a,c_MATA_fl_no_lag) #no difference, keep simpler model
c_MATA_fl_lag012_no_drought <- lmer(Experiment_Date ~  MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_no_lag,c_MATA_fl_lag012_no_drought) #no lag supported 
c_MATA_fl_lag012_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MATA_fl_no_lag,c_MATA_fl_lag012_nothing) #strong evidence for drought only, 2.2e-16


############ MAPA centre ############
#SLA lag1
c_MAPA_sla_lag1a <- lmer(SLA ~  Drought+MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MAPA_sla_lag1,c_MAPA_sla_lag1a) #2-way interaction very strong evidence, 0.0005356

#SLA lag2
c_MAPA_sla_lag2a <- lmer(SLA ~  Drought+MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MAPA_sla_lag2,c_MAPA_sla_lag2a) #2-way interaction strong evidence, 0.001017

#SLA lag0,1,2
c_MAPA_sla_lag012a <- lmer(SLA ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MAPA_sla_lag012,c_MAPA_sla_lag012a) #2-way interaction very strong evidence, 0.0005995


#fl lag0
c_MAPA_fl_lag0a <- lmer(Experiment_Date ~  Drought+MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MAPA_fl_lag0,c_MAPA_fl_lag0a) #2-way interaction moderate evidence, 0.02002

#fl lag2
c_MAPA_fl_lag2a <- lmer(Experiment_Date ~  Drought+MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MAPA_fl_lag2,c_MAPA_fl_lag2a) #2-way interaction moderate evidence, 0.03125

#fl lag0,1
c_MAPA_fl_lag01a <- lmer(Experiment_Date ~  Drought+MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MAPA_fl_lag01,c_MAPA_fl_lag01a) #2-way interaction moderate evidence, 0.01872

#fl lag0,1,2
c_MAPA_fl_lag012a <- lmer(Experiment_Date ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_MAPA_fl_lag012,c_MAPA_fl_lag012a) #2-way interaction moderateevidence, 0.01965


############ CMDA ############
#SLA lag0
c_CMDA_sla_lag0a <- lmer(SLA ~  Drought+CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_lag0,c_CMDA_sla_lag0a) # 2-way moderate evidence for simpler model
c_CMDA_sla_no_lag <- lmer(SLA ~  Drought+ (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_lag0a,c_CMDA_sla_no_lag) # no evidence, select simpler model
c_CMDA_sla_no_drought <- lmer(SLA ~  CMDA_lag0+ (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_no_lag,c_CMDA_sla_no_drought) # no lag supported 
c_CMDA_sla_nothing <- lmer(SLA ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_no_lag,c_CMDA_sla_nothing) #strong evidence for drought only

#SLA lag1
c_CMDA_sla_lag1a <- lmer(SLA ~  Drought+CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_lag1,c_CMDA_sla_lag1a) #weak evidence for simpler model
c_CMDA_sla_no_lag <- lmer(SLA ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_lag1a,c_CMDA_sla_no_lag ) #weak evidence for simpler model
c_CMDA_sla_no_drought <- lmer(SLA ~  CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_no_lag,c_CMDA_sla_no_drought) # no lag supported 
c_CMDA_sla_nothing <- lmer(SLA ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_no_lag,c_CMDA_sla_nothing) #strong evidence for drought only

#SLA lag2
c_CMDA_sla_lag2a <- lmer(SLA ~  Drought+CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_lag2,c_CMDA_sla_lag2a) #no evidence of difference, select simpler model
c_CMDA_sla_no_lag <- lmer(SLA ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_lag2a,c_CMDA_sla_no_lag) #moderate evidence for no lag model
c_CMDA_sla_no_drought <- lmer(SLA ~  CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_no_lag,c_CMDA_sla_no_drought) # very strong evidence for no lag
c_CMDA_sla_nothing <- lmer(SLA ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_sla_no_lag,c_CMDA_sla_nothing) # strong evidence for drought only model


#fl lag0
c_CMDA_fl_lag0a <- lmer(Experiment_Date ~  Drought+CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_lag0,c_CMDA_fl_lag0a) #strong evidence simpler
c_CMDA_fl_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_lag0a,c_CMDA_fl_nolag) #strong evidence simpler
c_CMDA_fl_lag0a_nodrought <- lmer(Experiment_Date ~  CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_nolag,c_CMDA_fl_lag0a_nodrought) # very strong evidence for no lag
c_CMDA_fl_lag0a_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_nolag,c_CMDA_fl_lag0a_nothing) # strong evidence for drought only model

#fl lag1
c_CMDA_fl_lag1a <- lmer(Experiment_Date ~  Drought+CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_lag1,c_CMDA_fl_lag1a) #strong evidence simpler
c_CMDA_fl_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_lag1a,c_CMDA_fl_nolag ) #strong evidence simpler
c_CMDA_fl_lag1a_nodrought <- lmer(Experiment_Date ~  CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_nolag,c_CMDA_fl_lag1a_nodrought) #no lag
c_CMDA_fl_lag1a_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_nolag,c_CMDA_fl_lag1a_nothing) # strong evidence for drought only model

#fl lag2
c_CMDA_fl_lag2a <- lmer(Experiment_Date ~  Drought+CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_lag2,c_CMDA_fl_lag2a) #strong evidence simpler
c_CMDA_fl_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_lag2a,c_CMDA_fl_nolag) #moderate evidence simpler
c_CMDA_fl_lag2a_nodrought <- lmer(Experiment_Date ~  CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_nolag,c_CMDA_fl_lag2a_nodrought) #no lag
c_CMDA_fl_lag2a_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=centre)
lrtest(c_CMDA_fl_nolag,c_CMDA_fl_lag2a_nothing)  # strong evidence for drought only model


##########################################################################################################
#### South ######
##########################################################################################################

#SPEI models
s_sla_lag0 <- lmer(SLA ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat),data=south)
s_sla_lag1 <- lmer(SLA ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_sla_lag2 <- lmer(SLA ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_sla_lag01 <- lmer(SLA ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=south)
s_sla_lag012 <- lmer(SLA ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

s_fl_lag0 <- lmer(Experiment_Date ~  Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_fl_lag1 <- lmer(Experiment_Date ~  Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_fl_lag2 <- lmer(Experiment_Date ~  Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
                  control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=south)
s_fl_lag01 <- lmer(Experiment_Date ~  Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_fl_lag012 <- lmer(Experiment_Date ~  Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

#MATA models
s_MATA_sla_lag0 <- lmer(SLA ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MATA_sla_lag1 <- lmer(SLA ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MATA_sla_lag2 <- lmer(SLA ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MATA_sla_lag01 <- lmer(SLA ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MATA_sla_lag012 <- lmer(SLA ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

s_MATA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MATA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MATA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MATA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MATA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MATA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

#MAPA models
s_MAPA_sla_lag0 <- lmer(SLA ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MAPA_sla_lag1 <- lmer(SLA ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MAPA_sla_lag2 <- lmer(SLA ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MAPA_sla_lag01 <- lmer(SLA ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MAPA_sla_lag012 <- lmer(SLA ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

s_MAPA_fl_lag0 <- lmer(Experiment_Date ~  Drought*MAPA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MAPA_fl_lag1 <- lmer(Experiment_Date ~  Drought*MAPA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MAPA_fl_lag2 <- lmer(Experiment_Date ~  Drought*MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MAPA_fl_lag01 <- lmer(Experiment_Date ~  Drought*MAPA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_MAPA_fl_lag012 <- lmer(Experiment_Date ~  Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

#CMDA models
s_CMDA_sla_lag0 <- lmer(SLA ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_CMDA_sla_lag1 <- lmer(SLA ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_CMDA_sla_lag2 <- lmer(SLA ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_CMDA_sla_lag01 <- lmer(SLA ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_CMDA_sla_lag012 <- lmer(SLA ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)

s_CMDA_fl_lag0 <- lmer(Experiment_Date ~  Drought*CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_CMDA_fl_lag1 <- lmer(Experiment_Date ~  Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_CMDA_fl_lag2 <- lmer(Experiment_Date ~  Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_CMDA_fl_lag01 <- lmer(Experiment_Date ~  Drought*CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
s_CMDA_fl_lag012 <- lmer(Experiment_Date ~  Drought*CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)


#################### AIC ###################################
s_lag_AIC <- data.frame()

#SPEI AIC
s_lag_AIC[1,1] <- AIC(s_sla_lag0)
s_lag_AIC[2,1] <- AIC(s_sla_lag1)
s_lag_AIC[3,1] <- AIC(s_sla_lag2)
s_lag_AIC[4,1] <- AIC(s_sla_lag01)
s_lag_AIC[5,1] <- AIC(s_sla_lag012)

s_lag_AIC[1,2] <- AIC(s_fl_lag0)
s_lag_AIC[2,2] <- AIC(s_fl_lag1)
s_lag_AIC[3,2] <- AIC(s_fl_lag2)
s_lag_AIC[4,2] <- AIC(s_fl_lag01)
s_lag_AIC[5,2] <- AIC(s_fl_lag012)

#MATA AIC
s_lag_AIC[1,3] <- AIC(s_MATA_sla_lag0)
s_lag_AIC[2,3] <- AIC(s_MATA_sla_lag1)
s_lag_AIC[3,3] <- AIC(s_MATA_sla_lag2)
s_lag_AIC[4,3] <- AIC(s_MATA_sla_lag01)
s_lag_AIC[5,3] <- AIC(s_MATA_sla_lag012)

s_lag_AIC[1,4] <- AIC(s_MATA_fl_lag0)
s_lag_AIC[2,4] <- AIC(s_MATA_fl_lag1)
s_lag_AIC[3,4] <- AIC(s_MATA_fl_lag2)
s_lag_AIC[4,4] <- AIC(s_MATA_fl_lag01)
s_lag_AIC[5,4] <- AIC(s_MATA_fl_lag012)

#MAPA AIC
s_lag_AIC[1,5] <- AIC(s_MAPA_sla_lag0)
s_lag_AIC[2,5] <- AIC(s_MAPA_sla_lag1)
s_lag_AIC[3,5] <- AIC(s_MAPA_sla_lag2)
s_lag_AIC[4,5] <- AIC(s_MAPA_sla_lag01)
s_lag_AIC[5,5] <- AIC(s_MAPA_sla_lag012)

s_lag_AIC[1,6] <- AIC(s_MAPA_fl_lag0)
s_lag_AIC[2,6] <- AIC(s_MAPA_fl_lag1)
s_lag_AIC[3,6] <- AIC(s_MAPA_fl_lag2)
s_lag_AIC[4,6] <- AIC(s_MAPA_fl_lag01)
s_lag_AIC[5,6] <- AIC(s_MAPA_fl_lag012)

#CMDA AIC
s_lag_AIC[1,7] <- AIC(s_CMDA_sla_lag0)
s_lag_AIC[2,7] <- AIC(s_CMDA_sla_lag1)
s_lag_AIC[3,7] <- AIC(s_CMDA_sla_lag2)
s_lag_AIC[4,7] <- AIC(s_CMDA_sla_lag01)
s_lag_AIC[5,7] <- AIC(s_CMDA_sla_lag012)

s_lag_AIC[1,8] <- AIC(s_CMDA_fl_lag0)
s_lag_AIC[2,8] <- AIC(s_CMDA_fl_lag1)
s_lag_AIC[3,8] <- AIC(s_CMDA_fl_lag2)
s_lag_AIC[4,8] <- AIC(s_CMDA_fl_lag01)
s_lag_AIC[5,8] <- AIC(s_CMDA_fl_lag012)

colnames(s_lag_AIC) <- c("SLA","Date of Flowering","SLA","Date of Flowering",
                       "SLA","Date of Flowering","SLA","Date of Flowering") 
### First SLA and DF pairs are SPEI, 2nd= MATA, 3rd = MAPA, 4th= CMDA
rownames(s_lag_AIC) <- c("lag 0", "lag 1", "lag 2", "lag 0,1","lag 0,1,2")
write.table(s_lag_AIC, file = "Data/south_AIC.csv", sep = ",", row.names = T)

#Global AIC South
lag_AIC_SLA <- as.data.frame(cbind(s_lag_AIC[,1],s_lag_AIC[,3],s_lag_AIC[,5],s_lag_AIC[,7]))
lag_AIC_DF <- as.data.frame(cbind(s_lag_AIC[,2],s_lag_AIC[,4],s_lag_AIC[,6],s_lag_AIC[,8]))
names(lag_AIC_SLA) <- c("SPEI","CMDA","MAPA","MATA")
names(lag_AIC_DF) <- c("SPEI","CMDA","MAPA","MATA")
delta_AIC_SLA <- lag_AIC_SLA-min(lag_AIC_SLA)
delta_AIC_DF <- lag_AIC_DF-min(lag_AIC_DF)
write.table(delta_AIC_SLA, file = "Data/Global_delta_AIC_SLA_South.csv", sep = ",", row.names = T)
write.table(delta_AIC_DF, file = "Data/Global_delta_AIC_DF_South.csv", sep = ",", row.names = T)


#Make delta AIC table
s_delta_AIC <- s_lag_AIC
s_delta_AIC[,1] <- s_lag_AIC[,1]-min(s_lag_AIC[,1])
s_delta_AIC[,2] <- s_lag_AIC[,2]-min(s_lag_AIC[,2])
s_delta_AIC[,3] <- s_lag_AIC[,3]-min(s_lag_AIC[,3])
s_delta_AIC[,4] <- s_lag_AIC[,4]-min(s_lag_AIC[,4])
s_delta_AIC[,5] <- s_lag_AIC[,5]-min(s_lag_AIC[,5])
s_delta_AIC[,6] <- s_lag_AIC[,6]-min(s_lag_AIC[,6])
s_delta_AIC[,7] <- s_lag_AIC[,7]-min(s_lag_AIC[,7])
s_delta_AIC[,8] <- s_lag_AIC[,8]-min(s_lag_AIC[,8])
write.table(s_delta_AIC, file = "Data/south_deltaAIC.csv", sep = ",", row.names = T)

########################################################################################################## 
#Test models that where delta AIC < 2

############ SPEI south############
#SLA lag 0
s_sla_lag0a <- lmer(SLA ~  Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_sla_lag0,s_sla_lag0a) #2-way interaction evidence, 0.001207

#SLA lag 0,1,2
s_sla_lag012a <- lmer(SLA ~  Drought+lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_sla_lag012,s_sla_lag012a) #2-way interaction strong evidence, 0.0003426


#fl lag 0
s_fl_lag0a <- lmer(Experiment_Date ~  Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag0,s_fl_lag0a) #no evidence, keep simpler
s_fl_lag0_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag0a,s_fl_lag0_nolag) #no evidence, keep simpler
s_fl_lag0_nodrought <- lmer(Experiment_Date ~  lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag0_nolag,s_fl_lag0_nodrought) # strong evidence for no lag
s_fl_lag0_nodrought <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag0_nolag,s_fl_lag0_nodrought) # strong evidence for no lag

#fl lag 1
s_fl_lag1a <- lmer(Experiment_Date ~  Drought+lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag1,s_fl_lag1a) #no evidence, keep simpler 
s_fl_lag1_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag1a,s_fl_lag1_nolag) #no evidence, keep simpler
s_fl_lag1_no_drought <- lmer(Experiment_Date ~  lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag1_nolag,s_fl_lag1_no_drought) # strong evidence for no lag
s_fl_lag1_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag1_nolag,s_fl_lag1_nothing) # strong evidence for no lag, 0.005598

#fl lag 2
s_fl_lag2a <- lmer(Experiment_Date ~  Drought+lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag2,s_fl_lag2a) #no evidence, keep simpler 
s_fl_lag2_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag2a,s_fl_lag2_nolag) #no evidence, keep simpler 
s_fl_lag2_nodrought <- lmer(Experiment_Date ~  lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag2_nolag,s_fl_lag2_nodrought) # strong evidence for no lag
s_fl_lag2_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_fl_lag2_nolag,s_fl_lag2_nothing) # strong evidence for no lag, 0.005598


############ MATA south ############
#SLA lag 0
s_MATA_sla_lag0a <- lmer(SLA ~  Drought+MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_MATA_sla_lag0,s_MATA_sla_lag0a) #2-way interaction very stong evidence, 5.729e-05


#fl lag 2
s_MATA_fl_lag2a <- lmer(Experiment_Date ~  Drought+MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_MATA_fl_lag2,s_MATA_fl_lag2a) #2-way moderate evidence, 0.02988

############ MAPA south ############

#SLA lag0,1,2
s_MAPA_sla_lag012a <- lmer(SLA ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_MAPA_sla_lag012,s_MAPA_sla_lag012a) #2-way interaction very strong evidence, 2.117e-05


#fl lag2
s_MAPA_fl_lag2a <- lmer(Experiment_Date ~  Drought+MAPA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_MAPA_fl_lag2,s_MAPA_fl_lag2a) #2-way interaction moderate significant evidence, 0.01824

#fl lag0,1,2
s_MAPA_fl_lag012a <- lmer(Experiment_Date ~  Drought+MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_MAPA_fl_lag012,s_MAPA_fl_lag012a) #2-way interaction strong significant evidence, 0.00876


############ CMDA ############
#SLA lag0
s_CMDA_sla_lag0a <- lmer(SLA ~  Drought+CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_sla_lag0,s_CMDA_sla_lag0a) #no evidence, simpler
s_CMDA_sla_nolag <- lmer(SLA ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_sla_lag0a,s_CMDA_sla_nolag) #no evidence, simpler
s_CMDA_sla_lag0_nodrought <- lmer(SLA ~  CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_sla_nolag,s_CMDA_sla_lag0_nodrought) #no lag
s_CMDA_sla_lag0_nothing <- lmer(SLA ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_sla_nolag,s_CMDA_sla_lag0_nothing) #no lag, 6.877e-07

#SLA lag1
s_CMDA_sla_lag1a <- lmer(SLA ~  Drought+CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_sla_lag1,s_CMDA_sla_lag1a) #simpler moderate evidence
s_CMDA_sla_nolag <- lmer(SLA ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_sla_lag1a,s_CMDA_sla_nolag) # no difference, select simpler model
s_CMDA_sla_lag1_nodrought <- lmer(SLA ~  CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_sla_nolag,s_CMDA_sla_lag1_nodrought) #no lag
s_CMDA_sla_lag1_nothing <- lmer(SLA ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_sla_nolag,s_CMDA_sla_lag1_nothing) #no lag, 6.877e-07


#fl lag0
s_CMDA_fl_lag0a <- lmer(Experiment_Date ~  Drought+CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_lag0,s_CMDA_fl_lag0a) #strong evidence simpler
s_CMDA_fl_nolag <- lmer(Experiment_Date ~  Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_lag0a,s_CMDA_fl_nolag) #strong evidence simpler
s_CMDA_fl_lag0a_nodrought <- lmer(Experiment_Date ~  CMDA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_nolag,s_CMDA_fl_lag0a_nodrought) #no lag
s_CMDA_fl_lag0a_nothing <- lmer(Experiment_Date ~ (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_nolag,s_CMDA_fl_lag0a_nothing) #no lag, 0.005598

#fl lag1
s_CMDA_fl_lag1a <- lmer(Experiment_Date ~  Drought+CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_lag1,s_CMDA_fl_lag1a) #strong evidence simpler
s_CMDA_fl_lag1a_nodrought <- lmer(Experiment_Date ~  CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_nolag,s_CMDA_fl_lag1a_nodrought) #no lag
s_CMDA_fl_lag1a_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_nolag,s_CMDA_fl_lag1a_nothing) #no lag, 0.005598
#see earlier model 


#fl lag01
s_CMDA_fl_lag01a <- lmer(Experiment_Date ~  Drought+CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_lag01,s_CMDA_fl_lag01a) #strong evidence simpler
s_CMDA_fl_lag01_nodrought <- lmer(Experiment_Date ~  CMDA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_nolag,s_CMDA_fl_lag01_nodrought) #no lag
s_CMDA_fl_lag01a_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_nolag,s_CMDA_fl_lag01a_nothing) #no lag, 0.005598
#see earlier model 

#fl lag012
s_CMDA_fl_lag012a <- lmer(Experiment_Date ~  Drought+CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_lag012,s_CMDA_fl_lag012a) #strong evidence simpler
s_CMDA_fl_lag012_nodrought <- lmer(Experiment_Date ~  CMDA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_nolag,s_CMDA_fl_lag01_nodrought) #no lag
s_CMDA_fl_lag01a_nothing <- lmer(Experiment_Date ~  (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=south)
lrtest(s_CMDA_fl_nolag,s_CMDA_fl_lag01a_nothing) #no lag, 0.005598
#see earlier model 

