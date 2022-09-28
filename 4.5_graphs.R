##########################################################################################################
## Make graphs for single main effect models
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
library(visreg)
library(cowplot)

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
#Set up modes

#North Wet
nw_sla_lag0 <- lmer(SLA ~  lag0 + (1|Block) + (1|Year) + (1|Site_Lat/Family), data=north_wet)
#nw_sla_lag0 <- lmer(SLA ~  lag0 + (1|Block), data=north_wet)

#Do Visreg then ggplot
vis_nw_sla_lag0<-visreg(nw_sla_lag0, xvar="lag0")
res_nw_sla_lag0<-vis_flower_D$res
res_nw_sla_lag0_input <- res_nw_sla_lag0 %>% select(SLA,visregRes)


c_vis_sla<-visreg(res_nw_sla_lag0_input, x="lag0",y="SLA")+
  geom_point(aes(lag0,SLA))+
  theme_classic()



#With Visreg
c_vis_sla<-visreg(nw_sla_lag0, x="lag0",y="SLA")+
geom_point(aes(lag0,SLA))+
  theme_classic()
  







c_vis_sla<-visreg(nw_sla_lag0$visregRes , xvar="lag0") #set up visreg for Drought
c_Res_sla<-c_vis_sla$res # Extract residuals

C1<-ggplot(c_Res_sla, aes(lag0, y=visregRes))+
  geom_point()+
#  geom_jitter(aes(colour=Drought), size=0.2)+
  geom_smooth(method="lm")+
  xlab("SPEI Lag 0") +
  scale_y_continuous(name="SLA", limits=c(100,400))+
#  scale_color_manual(values= c("D"="#FF7700", "W"="#006600"))+
#  scale_fill_manual(values= c("D"="#FF7700", "W"="#006600"))+
  theme_classic()
C1





C1 <-C1 + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
C1 <-C1 +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
C1
ggsave("Figures/C.SPEI_lag0_SLA.pdf", width = 6, height = 6, units = "in")









