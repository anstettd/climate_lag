##########################################################################################################
## Use climwin to analyse climate data
## Find window that influences SPRI
## Author Daniel Anstett
## 
##
## Last Modified November 8, 2021
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
#Look at correlation across lags and multi-year averages
#corr_lag <- y9 %>% select(lag0:lag0123)
#rcorr(as.matrix(corr_lag)) #lag0 lag1 & lag2 not highly correated. Averages highly correated with lag0, lag1

#Trait ~ Region*Treatment*env(lag0) + Random Effects
#Trait ~ Region*Treatment*env(lag1) + Random Effects
#Trait ~ Region*Treatment*env(lag2) + Random Effects
#Trait ~ Region*Treatment*env(average (lag01) + Random Effects
#Trait ~ Region*Treatment*env(average (lag012) + Random Effects

#Random Effects = + (1|Family) + (1|Block) + (1|Year) + (1|Site)

#lag models
sla_lag0 <- lmer(SLA ~ Region*Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
sla_lag1 <- lmer(SLA ~ Region*Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
sla_lag2 <- lmer(SLA ~ Region*Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
sla_lag01 <- lmer(SLA ~ Region*Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
sla_lag012 <- lmer(SLA ~ Region*Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)

fl_lag0 <- lmer(Experiment_Date ~ Region*Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
fl_lag1 <- lmer(Experiment_Date ~ Region*Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
fl_lag2 <- lmer(Experiment_Date ~ Region*Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
fl_lag01 <- lmer(Experiment_Date ~ Region*Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
fl_lag012 <- lmer(Experiment_Date ~ Region*Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)

wc_lag0 <- lmer(Water_Content ~ Region*Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
wc_lag1 <- lmer(Water_Content ~ Region*Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
wc_lag2 <- lmer(Water_Content ~ Region*Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
wc_lag01 <- lmer(Water_Content ~ Region*Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
wc_lag012 <- lmer(Water_Content ~ Region*Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)

a_lag0 <- lmer(Assimilation ~ Region*Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
a_lag1 <- lmer(Assimilation ~ Region*Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
a_lag2 <- lmer(Assimilation ~ Region*Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
a_lag01 <- lmer(Assimilation ~ Region*Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
a_lag012 <- lmer(Assimilation ~ Region*Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)

gz_lag0 <- lmer(Stomatal_Conductance ~ Region*Drought*lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat),
                control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)),data=y9)
gz_lag1 <- lmer(Stomatal_Conductance ~ Region*Drought*lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
gz_lag2 <- lmer(Stomatal_Conductance ~ Region*Drought*lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
gz_lag01 <- lmer(Stomatal_Conductance ~ Region*Drought*lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
gz_lag012 <- lmer(Stomatal_Conductance ~ Region*Drought*lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)

#################### AIC ###################################
lag_AIC <- data.frame()
#AIC
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
  
  lag_AIC[1,3] <- AIC(wc_lag0)
  lag_AIC[2,3] <- AIC(wc_lag1)
  lag_AIC[3,3] <- AIC(wc_lag2)
  lag_AIC[4,3] <- AIC(wc_lag01)
  lag_AIC[5,3] <- AIC(wc_lag012)
  
  lag_AIC[1,4] <- AIC(a_lag0)
  lag_AIC[2,4] <- AIC(a_lag1)
  lag_AIC[3,4] <- AIC(a_lag2)
  lag_AIC[4,4] <- AIC(a_lag01)
  lag_AIC[5,4] <- AIC(a_lag012)
  
  lag_AIC[1,5] <- AIC(gz_lag0)
  lag_AIC[2,5] <- AIC(gz_lag1)
  lag_AIC[3,5] <- AIC(gz_lag2)
  lag_AIC[4,5] <- AIC(gz_lag01)
  lag_AIC[5,5] <- AIC(gz_lag012)
  
colnames(lag_AIC) <- c("SLA","Date of Flowering","Water Content","Assimilation","Stomatal Conductance")
rownames(lag_AIC) <- c("lag 0", "lag 1", "lag 2", "lag 0,1","lag 0,1,2")
write.table(lag_AIC, file = "Data/region_AIC.csv", sep = ",", row.names = T)
#Make delta AIC table
delta_AIC <- lag_AIC
delta_AIC[,1] <- lag_AIC[,1]-min(lag_AIC[,1])
delta_AIC[,2] <- lag_AIC[,2]-min(lag_AIC[,2])
delta_AIC[,3] <- lag_AIC[,3]-min(lag_AIC[,3])
delta_AIC[,4] <- lag_AIC[,4]-min(lag_AIC[,4])
delta_AIC[,5] <- lag_AIC[,5]-min(lag_AIC[,5])
write.table(delta_AIC, file = "Data/delta_region_AIC.csv", sep = ",", row.names = T)

########################################################################################################## 

##########################################################################################################  
#Test models that where delta AIC < 2

############ SLA ############
#SLA lag 0
sla_lag0a <- lmer(SLA ~ Region*Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(sla_lag0,sla_lag0a) #3-way interaction significant
  
#SLA lag 0,1,2
sla_lag012a <- lmer(SLA ~ Region*Drought+lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(sla_lag012,sla_lag012a) #3-way interaction significant
  

############ Flowering Time ############

#fl lag 1
fl_lag1a <- lmer(Experiment_Date ~ Region*Drought+lag1 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag1,fl_lag1a) #3-way interaction significant

#fl lag 01
fl_lag01a <- lmer(Experiment_Date ~ Region*Drought+lag01 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag01,fl_lag01a) #3-way interaction marginally significant

#fl lag 0,1,2
fl_lag012a <- lmer(Experiment_Date ~ Region*Drought+lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(fl_lag012,fl_lag012a) #3-way interaction significant


############ Water Content ############
#wc lag 2
wc_lag2a <- lmer(Water_Content ~ Region*Drought+lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(wc_lag2,wc_lag2a) # Simpler model significant
wc_lag2b <- lmer(Water_Content ~ Region+Drought+lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(wc_lag2a,wc_lag2b) # Simpler model significant
wc_lag2c <- lmer(Water_Content ~ Region+Drought+(1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(wc_lag2b,wc_lag2c) # Simpler model significant
wc_lag2d <- lmer(Water_Content ~ Drought+(1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(wc_lag2c,wc_lag2d) # No difference, select Drought only model
wc_lag2e <- lmer(Water_Content ~ (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(wc_lag2d,wc_lag2e) # Select drought only model



############ Assimilation ############


#Assimilation lag 0
a_lag0a <- lmer(Assimilation ~ Region*Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(a_lag0,a_lag0a) #no difference, keep simpler model
a_lag0b <- lmer(Assimilation ~ Region+Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(a_lag0a,a_lag0b) #no difference, keep simpler model
a_lag0c <- lmer(Assimilation ~ Region+Drought+(1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(a_lag0b,a_lag0c) #no difference, keep simpler model
a_lag0d <- lmer(Assimilation ~ Drought+(1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(a_lag0c,a_lag0d) #no difference, keep simpler model
a_lag0e <- lmer(Assimilation ~ (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(a_lag0d,a_lag0e) #no difference select model with no fixed effects

#Assimilation lag 2
a_lag2a <- lmer(Assimilation ~ Region*Drought+lag2 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(a_lag2,a_lag2a) #no difference, keep simpler model
a_lag2b <- lmer(Assimilation ~ Region*Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(a_lag2a,a_lag2b) #no difference, keep simpler model. See lag0 test

#Assimilation lag 012
a_lag012a <- lmer(Assimilation ~ Region*Drought+lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(a_lag012,a_lag012a) #no difference, keep simpler model
a_lag012b <- lmer(Assimilation ~ Region+Drought+lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(a_lag012a,a_lag012b) #no difference, keep simpler model. See lag0 test


############ Stomatal Conductance ############
#gz lag 0
gz_lag0a <- lmer(Stomatal_Conductance ~ Region*Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(gz_lag0,gz_lag0a) # Simpler model significantly more likely
gz_lag0b <- lmer(Stomatal_Conductance ~ Region+Drought+lag0 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(gz_lag0a,gz_lag0b) # Simpler model significantly more likely
gz_lag0c <- lmer(Stomatal_Conductance ~ Region+Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(gz_lag0b,gz_lag0c) # Simpler model significantly more likely
gz_lag0d <- lmer(Stomatal_Conductance ~ Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(gz_lag0c,gz_lag0d) # Simpler model significantly more likely
gz_lag0e <- lmer(Stomatal_Conductance ~ (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(gz_lag0d,gz_lag0e) # Marginal evidence for drought effect


#gz lag 012
gz_lag012a <- lmer(Stomatal_Conductance ~ Region*Drought+lag012 + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(gz_lag012,gz_lag012a) #interaction significantly less likely
gz_lag012b <- lmer(Stomatal_Conductance ~ Region+Drought + (1|Family) + (1|Block) + (1|Year) + (1|Site.Lat), data=y9)
lrtest(gz_lag012a,gz_lag012b) # Model better without SPEI, see lag0 for further tests

########################################################################################################## 
#################### Graphs ###################################

############ SLA Graphs ############
# SLA lag0
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
ggsave("Figures/SPEI_lag0_SLA.pdf", width = 8, height = 6, units = "in")

# SLA 3-year average (lag012)
vis_sla_D<-visreg(sla_lag012, xvar="lag012", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_sla_W<-visreg(sla_lag012, xvar="lag012", by="Region", cond=list(Drought="W")) #set up visreg for Wet
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
SLA_plot<-ggplot(Res_sla_all, aes(lag012, y=visregRes, fill=Drought, colour=Drought))+
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
ggsave("Figures/SPEI_lag0-12_SLA.pdf", width = 8, height = 6, units = "in")


############ Date of Flowering Graphs ############
#Ft Lag1
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
ggsave("Figures/SPEI_lag1_ft.pdf", width = 8, height = 6, units = "in")

#FT lag01
vis_ft_D<-visreg(fl_lag01, xvar="lag01", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_ft_W<-visreg(fl_lag01, xvar="lag01", by="Region", cond=list(Drought="W")) #set up visreg for Wet
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
ft_plot<-ggplot(Res_ft_all, aes(lag01, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("Two-Year SPEI") +
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
ggsave("Figures/SPEI_lag01_ft.pdf", width = 8, height = 6, units = "in")


#FT lag012
vis_ft_D<-visreg(fl_lag012, xvar="lag012", by="Region", cond=list(Drought="D")) #set up visreg for Drought
vis_ft_W<-visreg(fl_lag012, xvar="lag012", by="Region", cond=list(Drought="W")) #set up visreg for Wet
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
ft_plot<-ggplot(Res_ft_all, aes(lag012, y=visregRes, fill=Drought, colour=Drought))+
  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  facet_wrap(.~Region)+
  xlab("Three-Year SPEI") +
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
ggsave("Figures/SPEI_lag012_ft.pdf", width = 8, height = 6, units = "in")

                                            










