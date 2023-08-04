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
      axis.title.x = element_text(color="black", size=18, vjust = 0, face="bold"),
      axis.title.y = element_text(color="black", size=15, vjust = 1.6,face="bold",hjust=0.5 ,angle=90)
      )
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
sw_sla_lag1 <- lmer(SLA ~  MAPA_lag1 + Block + (1|Year) + (1|Site_Lat/Family), data=south_wet)
sw_sla_lag2 <- lmer(SLA ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=south_wet)
sw_fd_lag2 <- lmer(Experiment_Date ~  MAPA_lag2 + Block + (1|Year) + (1|Site_Lat/Family), data=south_wet)
sw_fd_lag012 <- lmer(Experiment_Date ~  MAPA_lag012 + Block + (1|Year) + (1|Site_Lat/Family), data=south_wet)

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
graph_1 <- visreg(nw_sla_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3() 

graph_2 <- visreg(nw_fd_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3() 
graph_3 <- visreg(nw_fd_lag1,"MAPA_lag1",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 1")+  theme_paper3() 
graph_4 <- visreg(nw_fd_lag2,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 


#"#FF7700"
#North Dry
graph_5 <- visreg(nd_sla_lag0,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 

graph_6 <- visreg(nd_fd_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3()
graph_7 <- visreg(nd_fd_lag1,"MAPA_lag1",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 1")+  theme_paper3() 
graph_8 <- visreg(nd_fd_lag2,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 
graph_9 <- visreg(nd_fd_lag01,"MAPA_lag01",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA 2-Year Cumul.")+  theme_paper3() 
graph_10 <- visreg(nd_fd_lag012,"MAPA_lag012",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA 3-Year Cumul.")+  theme_paper3() 


#Try 3
#North
#Fig 2
plot_grid(graph_1,graph_5,graph_2,graph_4,graph_6,graph_8, labels = "AUTO",ncol = 2,label_x = 0.25) #export at 6 X 8 

#Fig S5
plot_grid(graph_3,graph_7,graph_9,graph_10, labels = "AUTO",ncol = 2,label_x = 0.23) #export at 6 X 6 




#Centre Wet
graph_11 <- visreg(cw_sla_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3()
graph_12 <- visreg(cw_sla_lag1,"MAPA_lag1",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 1")+  theme_paper3() 
graph_13 <- visreg(cw_sla_lag2,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 
graph_14 <- visreg(cw_sla_lag012,"MAPA_lag012",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA 3-Year Cumul.")+  theme_paper3() 

graph_15 <- visreg(cw_fd_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3()
graph_16 <- visreg(cw_fd_lag1,"MAPA_lag1",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 1")+  theme_paper3() 
graph_17 <- visreg(cw_fd_lag2,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 
graph_18 <- visreg(cw_fd_lag01,"MAPA_lag01",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA 2-Year Cumul.")+  theme_paper3() 
graph_19 <- visreg(cw_fd_lag012,"MAPA_lag012",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA 3-Year Cumul.")+  theme_paper3() 

#Centre Dry
graph_20 <- visreg(cd_sla_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3()
graph_21 <- visreg(cd_sla_lag1,"MAPA_lag1",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 1")+  theme_paper3() 
graph_22 <- visreg(cd_sla_lag2,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 

graph_23 <- visreg(cd_fd_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3()
graph_24 <- visreg(cd_fd_lag2,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 


#Try 3
#Centre
#Fig 3
plot_grid(graph_11,graph_13,graph_20,graph_22,graph_15,graph_16,graph_23,graph_24,
          labels = "AUTO",ncol = 2,label_x = 0.25) #export at 6 X 10 

#Fig S6
plot_grid(graph_12,graph_14,graph_21,graph_17,graph_18,graph_19, labels = "AUTO",ncol = 2,label_x = 0.25) #export at 6 X 8 



#South Wet
graph_25 <- visreg(sw_sla_lag1,"MAPA_lag1",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 1")+  theme_paper3() 
graph_26 <- visreg(sw_sla_lag2,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 

graph_27 <- visreg(sw_fd_lag2,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 
graph_28 <- visreg(sw_fd_lag012,"MAPA_lag012",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="skyblue3")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA 3-Year Cumul.")+  theme_paper3()


#South Dry
graph_29 <- visreg(sd_sla_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3()
graph_30 <- visreg(sd_sla_lag1,"MAPA_lag1",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 1")+  theme_paper3() 
graph_31 <- visreg(sd_sla_lag2,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 
graph_32 <- visreg(sd_sla_lag01,"MAPA_lag01",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MAPA 2-Year Cumul.")+  theme_paper3() 

graph_33 <- visreg(sd_sla_lag0_MATA,"MATA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name = expression(bold("SLA (" ~ cm^{2} ~ "/mg)")))+ scale_x_continuous(name="MATA lag 0")+  theme_paper3()

graph_34 <- visreg(sd_fd_lag0,"MAPA_lag0",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 0")+  theme_paper3()
graph_35 <- visreg(sd_fd_lag1,"MAPA_lag1",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 1")+  theme_paper3() 
graph_36 <- visreg(sd_fd_lag2,"MAPA_lag2",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA lag 2")+  theme_paper3() 
graph_37 <- visreg(sd_fd_lag01,"MAPA_lag01",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA 2-Year Cumul.")+  theme_paper3() 
graph_38<- visreg(sd_fd_lag012,"MAPA_lag012",gg=T)+ geom_point(size=0.8)+ geom_smooth(method="lm",color="#FF7700")+
  scale_y_continuous(name="Date of Flowering")+ scale_x_continuous(name="MAPA 3-Year Cumul.")+  theme_paper3() 

#Try 3
#South

#Fig 4 -- SLA
plot_grid(graph_26,graph_33,graph_29,graph_31,graph_27,graph_34,graph_35,graph_36,
          labels = "AUTO",ncol = 2,label_x = 0.25) #export at 6 X 10 

#Fig S7
plot_grid(graph_25,graph_30,graph_32,graph_28,graph_37,graph_38, labels = "AUTO",ncol = 2,label_x = 0.25) #export at 6 X 8 








##########################################################################################################
#Old Graphics
#Try 2
#Fig 1 North - Wet
plot_grid(graph_1,graph_2,graph_4,graph_5,graph_6,graph_8,
labels = "AUTO",ncol = 2,label_x = 0.23) #export at 6 X 8 

#Fig 2 

#Centre - Wet
plot_grid(graph_11,graph_12,graph_13,graph_14,graph_15,graph_16,graph_17,graph_18,graph_19,
          labels = "AUTO",ncol = 3,label_x = 0.01) #export at 8 X 8 

#North - Dry
plot_grid(graph_20,graph_21,graph_22,graph_23,graph_24,
          labels = "AUTO",ncol = 2,label_x = 0.20) #export at 8 X 8 

#South - Wet
plot_grid(graph_25,graph_26,graph_27,graph_28,
          labels = "AUTO",ncol = 2,label_x = 0.01) #export at 6 X 6 

#South - Dry
plot_grid(graph_29,graph_30,graph_31,graph_32,graph_33,graph_34,graph_35,graph_36,graph_37,graph_38,
          labels = "AUTO",ncol = 4,label_x = 0.01) #export at 8 X 8 


#Supplemental
#Fig S1
plot_grid(graph_3,graph_7,graph_9,graph_10,
          labels = "AUTO",ncol = 2,label_x = 0.23) #export at 5 X 6 



#Try 1
##SLA
#North
plot_grid(graph_1,graph_5,labels = "AUTO",ncol = 2) #export at 4 X 8 

#Centre wet
plot_grid(graph_11,graph_12,graph_13,graph_14,labels = "AUTO",ncol = 2) #export at  7 x 8

#Centre dry
plot_grid(graph_20,graph_21,graph_22,labels = "AUTO",ncol = 2) #export at 7 x 8

#South wet
plot_grid(graph_25,graph_26,labels = "AUTO",ncol = 2) #export at 4 X 8

#South dry
plot_grid(graph_29,graph_30,graph_31,graph_32,labels = "AUTO",ncol = 2) #export at  7 x 8

#MATA South Dry
ggsave("Revised_figures/1.6_SLA_MATA_south_dry.pdf",graph_33,width=4, height = 4, units = "in")

##Date of Flowering

#North Wet
plot_grid(graph_2,graph_3,graph_4,labels = "AUTO",ncol = 2) #export at 7 X 8 

#North Dry
plot_grid(graph_6,graph_7,graph_8,graph_9,graph_10,labels = "AUTO",ncol = 3) #export at 9 X 8 

#Centre wet
plot_grid(graph_15,graph_16,graph_17,graph_18,graph_19,labels = "AUTO",ncol = 3) #export at 9 x 8 

#Centre dry
plot_grid(graph_23,graph_24,labels = "AUTO",ncol = 2) #export at 4 X 8

#South wet
plot_grid(graph_27,graph_28,labels = "AUTO",ncol = 2) #export at 4 X 8

#South dry
plot_grid(graph_34,graph_35,graph_36,graph_37,graph_38,labels = "AUTO",ncol = 3) #export at 9 x 8 





#Singleton save
#ggsave("Revised_figures/single/01_nw_sla_lag0.pdf",graph_1,width=8, height = 6, units = "in")
#ggsave("Revised_figures/single/02_nw_fd_lag0.pdf",graph_2,width=8, height = 6, units = "in")
#ggsave("Revised_figures/single/03_nw_fd_lag1.pdf",graph_3,width=8, height = 6, units = "in")
#ggsave("Revised_figures/single/04_nw_fd_lag2.pdf",graph_4,width=8, height = 6, units = "in")



