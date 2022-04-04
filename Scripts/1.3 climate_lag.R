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
#write.table(lag_AIC, file = "Data/climate_AIC.csv", sep = ",", row.names = T)

#Make Global Trait specific AIC Table
lag_AIC_SLA <- as.data.frame(cbind(lag_AIC[,1],lag_AIC[,3],lag_AIC[,5],lag_AIC[,7]))
lag_AIC_DF <- as.data.frame(cbind(lag_AIC[,2],lag_AIC[,4],lag_AIC[,6],lag_AIC[,8]))
names(lag_AIC_SLA) <- c("SPEI","CMDA","MAPA","MATA")
names(lag_AIC_DF) <- c("SPEI","CMDA","MAPA","MATA")
delta_AIC_SLA <- lag_AIC_SLA-min(lag_AIC_SLA)
delta_AIC_DF <- lag_AIC_DF-min(lag_AIC_DF)
write.table(delta_AIC_SLA, file = "Data/Global_delta_AIC_SLA.csv", sep = ",", row.names = T)
write.table(delta_AIC_DF, file = "Data/Global_delta_AIC_DF.csv", sep = ",", row.names = T)

#Make local delta AIC table
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

############ SPEI ############
#SLA lag 0
sla_lag0a <- lmer(SLA ~ Region*Drought + Region*lag0 + Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(sla_lag0,sla_lag0a) #3-way interaction very strong evidence, P = 0.0005959

#fl lag 1
fl_lag1a <- lmer(Experiment_Date ~ Region*Drought + Region*lag1 + Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(fl_lag1,fl_lag1a) #3-way interaction moderate evidence, P = 0.01551


############ MATA ############
#SLA lag 0
MATA_sla_lag0a <- lmer(SLA ~ Region*Drought + Region*MATA_lag0 + Drought*MATA_lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(MATA_sla_lag0,MATA_sla_lag0a) #3-way interaction very strong evidence, P = 0.0001335

#SLA lag0,1
MATA_sla_lag01a <- lmer(SLA ~ Region*Drought + Region*MATA_lag01 + Drought*MATA_lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(MATA_sla_lag01,MATA_sla_lag01a) #3-way interaction very strong evidence, P = 5.553e-05

#SLA lag0,1,2
MATA_sla_lag012a <- lmer(SLA ~ Region*Drought + Region*MATA_lag012 + Drought*MATA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(MATA_sla_lag012,MATA_sla_lag012a) #3-way interaction very strong evidence, P = 7.027e-05

#fl lag 2
MATA_fl_lag2a <- lmer(Experiment_Date ~ Region*Drought + Region*MATA_lag2 + Drought*MATA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(MATA_fl_lag2,MATA_fl_lag2a) #3-way interaction moderate evidence, P = 0.01619


############ MAPA ############
#SLA lag0,1,2
MAPA_sla_lag012a <- lmer(SLA ~ Region*Drought + Region*MAPA_lag012 + Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(MAPA_sla_lag012,MAPA_sla_lag012a) #3-way interaction very strong evidence, P = 3.19e-07

#fl lag0,1,2
MAPA_fl_lag012a <- lmer(Experiment_Date ~ Region*Drought + Region*MAPA_lag012 + Drought*MAPA_lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(MAPA_fl_lag012,MAPA_fl_lag012a) #3-way interaction very strong evidence, P = 2.896e-05


############ CMDA ############
#SLA lag2
CMDA_sla_lag2a <- lmer(SLA ~ Region*Drought + Region*CMDA_lag2 + Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_sla_lag2,CMDA_sla_lag2a) #weak evidence for simpler model

CMDA_sla_lag2b <- lmer(SLA ~ Region*CMDA_lag2 + Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_sla_lag2a,CMDA_sla_lag2b) #strong evidence to keep model with 3 two-ways

CMDA_sla_lag2c <- lmer(SLA ~ Region*Drought + Drought*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_sla_lag2a,CMDA_sla_lag2c) #no evidence for diff, keep simpler

CMDA_sla_lag2d <- lmer(SLA ~ Region*Drought + Region*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_sla_lag2a,CMDA_sla_lag2d) #no evidence for diff, keep simpler
lrtest(CMDA_sla_lag2c,CMDA_sla_lag2d) #weak evidence for d. Region*Drought + Region*CMDA_lag

CMDA_sla_lag2e <- lmer(SLA ~ Region*Drought + CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_sla_lag2d,CMDA_sla_lag2e) #no evidence for diff, keep simpler

CMDA_sla_lag2f <- lmer(SLA ~ Drought + Region*CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_sla_lag2e,CMDA_sla_lag2f) #very strong evidence for e.

CMDA_sla_lag2g <- lmer(SLA ~ Region + Drought + CMDA_lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_sla_lag2e,CMDA_sla_lag2g) #strong evidence for e. Region*Drought + CMDA_lag2, P = 0.00165

#fl lag1
CMDA_fl_lag1a <- lmer(Experiment_Date ~ Region*Drought + Region*CMDA_lag1 + Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), 
data=y9)
lrtest(CMDA_fl_lag1,CMDA_fl_lag1a) #moderate evidence in favor of simpler model

CMDA_fl_lag1b <- lmer(Experiment_Date ~ Region*CMDA_lag1 + Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_fl_lag1a,CMDA_fl_lag1b) #strong evidence in favor of a.

CMDA_fl_lag1c <- lmer(Experiment_Date ~ Region*Drought + Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_fl_lag1a,CMDA_fl_lag1c) #strong evidence in favor of c.

CMDA_fl_lag1d <- lmer(Experiment_Date ~ Region*Drought + Region*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_fl_lag1a,CMDA_fl_lag1d) #strong evidence in favor of d.
lrtest(CMDA_fl_lag1c,CMDA_fl_lag1d) #strong evidence in favor of c.

CMDA_fl_lag1e <- lmer(Experiment_Date ~ Region*Drought + CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_fl_lag1c,CMDA_fl_lag1e) #strong evidence in favor of e.

CMDA_fl_lag1f <- lmer(Experiment_Date ~ Region+ Drought*CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_fl_lag1c,CMDA_fl_lag1f) #strong evidence in favor of c.

CMDA_fl_lag1g <- lmer(Experiment_Date ~ Region + Drought + CMDA_lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site_Lat), data=y9)
lrtest(CMDA_fl_lag1e,CMDA_fl_lag1g) #strong evidence in favor of e: Region*Drought + CMDA_lag1, p = 2.071e-06




########################################################################################################## 
#################### Graphs ###################################

############ SPEI ############
# SPEI SLA lag0
vis_sla_D<-visreg(sla_lag0, xvar="lag0", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(sla_lag0, xvar="lag0", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_sla_D<-vis_sla_D$res ; Res_sla_W<-vis_sla_W$res # Extract residuals
Res_sla_all<-rbind(Res_sla_D, Res_sla_W) #Row bind wet and dry residuals into one data frame
Res_sla_all$Region<-as.factor(Res_sla_all$Region)
Res_sla_all$Region<-factor(Res_sla_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_sla_all$Drought <- as.factor(Res_sla_all$Drought)
Res_sla_all$Drought <- factor(Res_sla_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
SLA_plot<-ggplot(Res_sla_all, aes(lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("SPEI (lag 0)") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
SLA_plot <-SLA_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
SLA_plot <-SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
SLA_plot
ggsave("Figure_pannels/1A.SPEI_lag0_SLA.pdf", width = 8, height = 6, units = "in")


# SPEI Date of Flowering Lag1
vis_ft_D<-visreg(fl_lag1, xvar="lag1", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_ft_W<-visreg(fl_lag1, xvar="lag1", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_ft_D<-vis_ft_D$res ; Res_ft_W<-vis_ft_W$res # Extract residuals
Res_ft_all<-rbind(Res_ft_D, Res_ft_W) #Row bind wet and dry residuals into one data frame
Res_ft_all$Region<-as.factor(Res_ft_all$Region)
Res_ft_all$Region<-factor(Res_ft_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_ft_all$Drought <- as.factor(Res_ft_all$Drought)
Res_ft_all$Drought <- factor(Res_ft_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
ft_plot<-ggplot(Res_ft_all, aes(lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("SPEI (lag 1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
ft_plot <-ft_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
ft_plot <-ft_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
ft_plot
ggsave("Figure_pannels/1B.SPEI_lag1_ft.pdf", width = 8, height = 6, units = "in")



############ MATA ############
# MATA SLA lag0
vis_sla_D<-visreg(MATA_sla_lag0, xvar="MATA_lag0", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(MATA_sla_lag0, xvar="MATA_lag0", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_sla_D<-vis_sla_D$res ; Res_sla_W<-vis_sla_W$res # Extract residuals
Res_sla_all<-rbind(Res_sla_D, Res_sla_W) #Row bind wet and dry residuals into one data frame
Res_sla_all$Region<-as.factor(Res_sla_all$Region)
Res_sla_all$Region<-factor(Res_sla_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_sla_all$Drought <- as.factor(Res_sla_all$Drought)
Res_sla_all$Drought <- factor(Res_sla_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
SLA_plot<-ggplot(Res_sla_all, aes(MATA_lag0, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("MATA (lag 0)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
SLA_plot <-SLA_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
SLA_plot <-SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
SLA_plot
ggsave("Figure_pannels/2A.MATA_lag0_SLA.pdf", width = 8, height = 6, units = "in")


# MATA SLA lag01
vis_sla_D<-visreg(MATA_sla_lag01, xvar="MATA_lag01", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(MATA_sla_lag01, xvar="MATA_lag01", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_sla_D<-vis_sla_D$res ; Res_sla_W<-vis_sla_W$res # Extract residuals
Res_sla_all<-rbind(Res_sla_D, Res_sla_W) #Row bind wet and dry residuals into one data frame
Res_sla_all$Region<-as.factor(Res_sla_all$Region)
Res_sla_all$Region<-factor(Res_sla_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_sla_all$Drought <- as.factor(Res_sla_all$Drought)
Res_sla_all$Drought <- factor(Res_sla_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
SLA_plot<-ggplot(Res_sla_all, aes(MATA_lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("MATA 2-Year Average") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
SLA_plot <-SLA_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
SLA_plot <-SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
SLA_plot
ggsave("Figure_pannels/2B.ATA_lag01_SLA.pdf", width = 8, height = 6, units = "in")


# MATA SLA lag012
vis_sla_D<-visreg(MATA_sla_lag012, xvar="MATA_lag012", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(MATA_sla_lag012, xvar="MATA_lag012", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_sla_D<-vis_sla_D$res ; Res_sla_W<-vis_sla_W$res # Extract residuals
Res_sla_all<-rbind(Res_sla_D, Res_sla_W) #Row bind wet and dry residuals into one data frame
Res_sla_all$Region<-as.factor(Res_sla_all$Region)
Res_sla_all$Region<-factor(Res_sla_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_sla_all$Drought <- as.factor(Res_sla_all$Drought)
Res_sla_all$Drought <- factor(Res_sla_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
mata_SLA_plot<-ggplot(Res_sla_all, aes(MATA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("MATA 3-Year Average") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
mata_SLA_plot <-mata_SLA_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
mata_SLA_plot <-mata_SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
mata_SLA_plot
ggsave("Figure_pannels/2C.MATA_lag012_SLA.pdf", width = 8, height = 6, units = "in")


# MATA Date of Flowering Lag2
vis_ft_D<-visreg(MATA_fl_lag2, xvar="MATA_lag2", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_ft_W<-visreg(MATA_fl_lag2, xvar="MATA_lag2", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_ft_D<-vis_ft_D$res ; Res_ft_W<-vis_ft_W$res # Extract residuals
Res_ft_all<-rbind(Res_ft_D, Res_ft_W) #Row bind wet and dry residuals into one data frame
Res_ft_all$Region<-as.factor(Res_ft_all$Region)
Res_ft_all$Region<-factor(Res_ft_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_ft_all$Drought <- as.factor(Res_ft_all$Drought)
Res_ft_all$Drought <- factor(Res_ft_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
mata_ft_plot<-ggplot(Res_ft_all, aes(MATA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("MATA (lag 2)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
mata_ft_plot <-mata_ft_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
mata_ft_plot <-mata_ft_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
mata_ft_plot
ggsave("Figure_pannels/2D.MATA_lag2_ft.pdf", width = 8, height = 6, units = "in")

plot_grid(mapa_SLA_plot, mata_SLA_plot, mapa_ft_plot, mata_ft_plot, ncol=2)
#save 8 x 12

############ MAPA ############
# MAPA SLA lag012
vis_sla_D<-visreg(MAPA_sla_lag012, xvar="MAPA_lag012", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(MAPA_sla_lag012, xvar="MAPA_lag012", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_sla_D<-vis_sla_D$res ; Res_sla_W<-vis_sla_W$res # Extract residuals
Res_sla_all<-rbind(Res_sla_D, Res_sla_W) #Row bind wet and dry residuals into one data frame
Res_sla_all$Region<-as.factor(Res_sla_all$Region)
Res_sla_all$Region<-factor(Res_sla_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_sla_all$Drought <- as.factor(Res_sla_all$Drought)
Res_sla_all$Drought <- factor(Res_sla_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
mapa_SLA_plot<-ggplot(Res_sla_all, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("log(MAPA 3-Year Average)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
mapa_SLA_plot <-mapa_SLA_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
mapa_SLA_plot <-,apa_SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
mapa_SLA_plot
ggsave("Figure_pannels/3A.MAPA_lag012_SLA.pdf", width = 8, height = 6, units = "in")


# MAPA Date of Flowering Lag012
vis_ft_D<-visreg(MAPA_fl_lag012, xvar="MAPA_lag012", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_ft_W<-visreg(MAPA_fl_lag012, xvar="MAPA_lag012", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_ft_D<-vis_ft_D$res ; Res_ft_W<-vis_ft_W$res # Extract residuals
Res_ft_all<-rbind(Res_ft_D, Res_ft_W) #Row bind wet and dry residuals into one data frame
Res_ft_all$Region<-as.factor(Res_ft_all$Region)
Res_ft_all$Region<-factor(Res_ft_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_ft_all$Drought <- as.factor(Res_ft_all$Drought)
Res_ft_all$Drought <- factor(Res_ft_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
mapa_ft_plot<-ggplot(Res_ft_all, aes(MAPA_lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("log(MAPA 3-Year Average)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
mapa_ft_plot <-mapa_ft_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
mapa_ft_plot <-mapa_ft_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
mapa_ft_plot
ggsave("Figure_pannels/3B.MAPA_lag012_ft.pdf", width = 8, height = 6, units = "in")



############ CMDA ## Region*Drought*CMDA_lag2
# CMDA SLA lag2
vis_sla_D<-visreg(CMDA_sla_lag2, xvar="CMDA_lag2", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(CMDA_sla_lag2, xvar="CMDA_lag2", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_sla_D<-vis_sla_D$res ; Res_sla_W<-vis_sla_W$res # Extract residuals
Res_sla_all<-rbind(Res_sla_D, Res_sla_W) #Row bind wet and dry residuals into one data frame
Res_sla_all$Region<-as.factor(Res_sla_all$Region)
Res_sla_all$Region<-factor(Res_sla_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_sla_all$Drought <- as.factor(Res_sla_all$Drought)
Res_sla_all$Drought <- factor(Res_sla_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
SLA_plot<-ggplot(Res_sla_all, aes(CMDA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("CMDA (lag 2)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
SLA_plot <-SLA_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
SLA_plot <-SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
SLA_plot
ggsave("Figure_pannels/4A.CMDA_lag2_SLA.pdf", width = 8, height = 6, units = "in")


# CMDA Date of Flowering Lag1
vis_ft_D<-visreg(CMDA_fl_lag1, xvar="CMDA_lag1", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_ft_W<-visreg(CMDA_fl_lag1, xvar="CMDA_lag1", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_ft_D<-vis_ft_D$res ; Res_ft_W<-vis_ft_W$res # Extract residuals
Res_ft_all<-rbind(Res_ft_D, Res_ft_W) #Row bind wet and dry residuals into one data frame
Res_ft_all$Region<-as.factor(Res_ft_all$Region)
Res_ft_all$Region<-factor(Res_ft_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_ft_all$Drought <- as.factor(Res_ft_all$Drought)
Res_ft_all$Drought <- factor(Res_ft_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
ft_plot<-ggplot(Res_ft_all, aes(CMDA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("CMDA (lag 1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
ft_plot <-ft_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
ft_plot <-ft_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
ft_plot
ggsave("Figure_pannels/4B.CMDA_lag1_ft.pdf", width = 8, height = 6, units = "in")



############ CMDA  ## Region*Drought+CMDA_lag2
# CMDA SLA lag2
vis_sla_D<-visreg(CMDA_sla_lag2e, xvar="CMDA_lag2", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(CMDA_sla_lag2e, xvar="CMDA_lag2", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_sla_D<-vis_sla_D$res ; Res_sla_W<-vis_sla_W$res # Extract residuals
Res_sla_all<-rbind(Res_sla_D, Res_sla_W) #Row bind wet and dry residuals into one data frame
Res_sla_all$Region<-as.factor(Res_sla_all$Region)
Res_sla_all$Region<-factor(Res_sla_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_sla_all$Drought <- as.factor(Res_sla_all$Drought)
Res_sla_all$Drought <- factor(Res_sla_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
cmda_SLA_plot<-ggplot(Res_sla_all, aes(CMDA_lag2, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("CMDA (lag 2)") +
  #scale_x_continuous(limits=c(-0.1,0.4))+
  scale_y_continuous(name="SLA", limits=c(100,400))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
cmda_SLA_plot <-cmda_SLA_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
cmda_SLA_plot <-cmda_SLA_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
cmda_SLA_plot
ggsave("Figure_pannels/5A.CMDA_lag2_SLA.pdf", width = 8, height = 6, units = "in")


# CMDA Date of Flowering Lag1
vis_ft_D<-visreg(CMDA_fl_lag1e, xvar="CMDA_lag1", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_ft_W<-visreg(CMDA_fl_lag1e, xvar="CMDA_lag1", by="Region", cond=list(Drought="W")) #set up visreg for Wet
Res_ft_D<-vis_ft_D$res ; Res_ft_W<-vis_ft_W$res # Extract residuals
Res_ft_all<-rbind(Res_ft_D, Res_ft_W) #Row bind wet and dry residuals into one data frame
Res_ft_all$Region<-as.factor(Res_ft_all$Region)
Res_ft_all$Region<-factor(Res_ft_all$Region,levels=c("North","Center","South"))
#Reorder Treatments
Res_ft_all$Drought <- as.factor(Res_ft_all$Drought)
Res_ft_all$Drought <- factor(Res_ft_all$Drought, levels=c("W", "D"))
#Set up site lables equating names to codes
Site_Labs<-c("North"="A (North)", "Center"="B (Centre)", "South"="C (South)")
#Use ggplot to generate plot with all required formating
cmda_ft_plot<-ggplot(Res_ft_all, aes(CMDA_lag1, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("CMDA (lag 1)") +
  scale_y_continuous(name="Date of Flowering", limits=c(80,120))+
  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
cmda_ft_plot <-cmda_ft_plot + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
cmda_ft_plot <-cmda_ft_plot + facet_wrap(.~Region,labeller = labeller(Region=Site_Labs)) +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
cmda_ft_plot
ggsave("Figure_pannels/5B.CMDA_lag1_ft.pdf", width = 8, height = 6, units = "in")

plot_grid(SLA_plot, cmda_SLA_plot, ft_plot, cmda_ft_plot, ncol = 2) #SPEI
# saved 12 x 8



