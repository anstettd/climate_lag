##########################################################################################################
## Graph autocorrelation metric per traits
##
## Author Daniel Anstett
## 
##
## Last Modified Feb 17, 2022
##########################################################################################################

# Clear environment
rm(list = ls())

#Import libraries
library(tidyverse)
library(cowplot)

##########################################################################################################
#Import files

z1 <- read.csv("Data/site_lat_lag.csv", header=T)  #Basic Lat/Long plus headers

#SLA
#CMDA
SLA1<-ggplot(z1, aes(x=CMDA_auto, y=change_SLA_W, group=Region)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
    axis.text.y = element_text(size=14,face="bold"),
    axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
    axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

SLA2<-ggplot(z1, aes(x=CMDA_auto, y=change_SLA_D, group=Region)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

#MAPA
SLA3<-ggplot(z1, aes(x=MAPA_auto, y=change_SLA_W, group=Region)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

SLA4<-ggplot(z1, aes(x=MAPA_auto, y=change_SLA_D, group=Region)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

#MATA
SLA5<-ggplot(z1, aes(x=MATA_auto, y=change_SLA_W, group=Region)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

SLA6<-ggplot(z1, aes(x=MATA_auto, y=change_SLA_D, group=Region)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

plot_grid(SLA1,SLA2,SLA3,SLA4,SLA5,SLA6,ncol = 2) #Random









