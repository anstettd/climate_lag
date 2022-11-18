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
#Fuctions

#Define Theme

theme_paper3 <- function(){ 
  
  theme_classic() %+replace%    #replace elements we want to change
    theme( 
      axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
      axis.text.y = element_text(size=15,face="bold"),
      axis.title.x = element_text(color="black", size=20, vjust = 0, face="bold"),
      axis.title.y = element_text(color="black", size=20, vjust = 1.6,face="bold",hjust=0.5 ,angle=90)
      )
}

visreg(nw_sla_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="black")+
  scale_y_continuous(name="SLA")+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3()   



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
#Set up models

#North Wet
nw_sla_lag0 <- lmer(SLA ~  MAPA_lag0 + Block + (1|Year) + (1|Site_Lat/Family), data=north_wet)
nw_fd_lag0 <- lmer(Experiment_Date ~  MAPA_lag0 + Block + (1|Year) + (1|Site_Lat/Family), data=north_wet)
nw_fd_lag1 <- lmer(Experiment_Date ~  MAPA_lag1 + Block + (1|Year) + (1|Site_Lat/Family), data=north_wet)
nw_fd_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=north_wet)

#North Dry
nd_sla_lag0 <- lmer(SLA ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)
nd_fd_lag0 <- lmer(Experiment_Date ~  MAPA_lag0 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)
nd_fd_lag1 <- lmer(Experiment_Date ~  MAPA_lag1 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)
nd_fd_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)
nd_fd_lag01 <- lmer(Experiment_Date ~  MAPA_lag01 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)
nd_fd_lag012 <- lmer(Experiment_Date ~  MAPA_lag012 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)


#Centre Wet
cw_sla_lag0 <- lmer(SLA ~  MAPA_lag0 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_wet)
cw_sla_lag1 <- lmer(SLA ~  MAPA_lag1 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_wet)
cw_sla_lag2 <- lmer(SLA ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_wet)
cw_sla_lag012 <- lmer(SLA ~  MAPA_lag012 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_wet)

cw_fd_lag0 <- lmer(Experiment_Date ~  MAPA_lag0 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_wet)
cw_fd_lag1 <- lmer(Experiment_Date ~  MAPA_lag1 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_wet)
cw_fd_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_wet)
cw_fd_lag01 <- lmer(Experiment_Date ~  MAPA_lag01 + Block + (1|Year) + (1|Site_Lat/Family),
                    control=lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=100000)), data=centre_wet)
cw_fd_lag012 <- lmer(Experiment_Date ~  MAPA_lag012 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_wet)

#Centre Dry
cd_sla_lag0 <- lmer(SLA ~  MAPA_lag0 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_dry)
cd_sla_lag1 <- lmer(SLA ~  MAPA_lag1 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_dry)
cd_sla_lag2 <- lmer(SLA ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_dry)
cd_fd_lag0 <- lmer(Experiment_Date ~  MAPA_lag0 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_dry)
cd_fd_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=centre_dry)

#South Wet
cw_sla_lag1 <- lmer(SLA ~  MAPA_lag1 + Block + (1|Year) + (1|Site_Lat/Family), data=south_wet)
cw_sla_lag2 <- lmer(SLA ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=south_wet)
cw_sla_lag2 <- lmer(SLA ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=south_wet)
cw_sla_lag012 <- lmer(SLA ~  MAPA_lag012 + Block + (1|Year) + (1|Site_Lat/Family), data=south_wet)

#South Dry
sd_sla_lag0 <- lmer(SLA ~  MAPA_lag0 + Block + (1|Year) + (1|Site_Lat/Family), data=south_dry)
sd_sla_lag1 <- lmer(SLA ~  MAPA_lag1 + Block + (1|Year) + (1|Site_Lat/Family), data=south_dry)
sd_sla_lag2 <- lmer(SLA ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=south_dry)
sd_sla_lag01 <- lmer(SLA ~  MAPA_lag01 + Block + (1|Year) + (1|Site_Lat/Family), data=south_dry)

sd_sla_lag0_MATA <- lmer(SLA ~  MATA_lag0 + Block + (1|Year) + (1|Site_Lat/Family), data=south_dry)

sd_fd_lag0 <- lmer(Experiment_Date ~  MAPA_lag0 + Block + (1|Year) + (1|Site_Lat/Family), data=south_dry)
sd_fd_lag1 <- lmer(Experiment_Date ~  MAPA_lag1 + Block + (1|Year) + (1|Site_Lat/Family), data=south_dry)
sd_fd_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=south_dry)
sd_fd_lag01 <- lmer(Experiment_Date ~  MAPA_lag01 + Block + (1|Year) + (1|Site_Lat/Family), data=south_dry)
sd_fd_lag012 <- lmer(Experiment_Date ~  MAPA_lag012 + Block + (1|Year) + (1|Site_Lat/Family), data=south_dry)

#################################################################################################################

#Set up graphs

#North Wet
graph_1 <- visreg(nw_sla_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="black")+
  scale_y_continuous(name="SLA")+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3() 
graph_2 <- visreg(nw_fd_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="black")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3() 
graph_3 <- visreg(nw_fd_lag1,"MAPA_lag1",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="black")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 1")+  theme_paper3() 
graph_4 <- visreg(nw_fd_lag2,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="black")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 


#North Dry
nd_sla_lag0 <- lmer(SLA ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)
nd_fd_lag0 <- lmer(Experiment_Date ~  MAPA_lag0 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)
nd_fd_lag1 <- lmer(Experiment_Date ~  MAPA_lag1 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)
nd_fd_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)
nd_fd_lag01 <- lmer(Experiment_Date ~  MAPA_lag01 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)
nd_fd_lag012 <- lmer(Experiment_Date ~  MAPA_lag012 + Block + (1|Year) + (1|Site_Lat/Family), data=north_dry)
  



ggsave("Revised_figures/01_nw_sla_lag0.pdf",graph_1,width=8, height = 6, units = "in")
ggsave("Revised_figures/02_nw_fd_lag0.pdf",graph_2,width=8, height = 6, units = "in")
ggsave("Revised_figures/03_nw_fd_lag1.pdf",graph_3,width=8, height = 6, units = "in")
ggsave("Revised_figures/04_nw_fd_lag2.pdf",graph_4,width=8, height = 6, units = "in")

















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









