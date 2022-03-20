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

z1 <- read.csv("Data/z1.csv", header=T)  #Basic Lat/Long plus headers

#SLA
#CMDA
SLA1<-ggplot(z1, aes(x=CMDA_auto, y=abs_change_SLA_W)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
    axis.text.y = element_text(size=14,face="bold"),
    axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
    axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

SLA2<-ggplot(z1, aes(x=CMDA_auto, y=abs_change_SLA_D)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

#MAPA
SLA3<-ggplot(z1, aes(x=MAPA_auto, y=abs_change_SLA_W)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

SLA4<-ggplot(z1, aes(x=MAPA_auto, y=abs_change_SLA_D)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

#MATA
SLA5<-ggplot(z1, aes(x=MATA_auto, y=abs_change_SLA_W)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

SLA6<-ggplot(z1, aes(x=MATA_auto, y=abs_change_SLA_D)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

plot_grid(SLA1,SLA2,SLA3,SLA4,SLA5,SLA6,ncol = 2) #Random


#DF
#CMDA
DF1<-ggplot(z1, aes(x=CMDA_auto, y=abs_change_DF_W)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
#  scale_y_continuous(name="change_DF_W", limits=c(0,12))+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

DF2<-ggplot(z1, aes(x=CMDA_auto, y=abs_change_DF_D)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
# scale_y_continuous(name="change_DF_D", limits=c(0,12))+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

#MAPA
DF3<-ggplot(z1, aes(x=MAPA_auto, y=abs_change_DF_W)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
#  scale_y_continuous(name="change_DF_W", limits=c(0,12))+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

DF4<-ggplot(z1, aes(x=MAPA_auto, y=abs_change_DF_D)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
#  scale_y_continuous(name="change_DF_D", limits=c(0,12))+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

#MATA
DF5<-ggplot(z1, aes(x=MATA_auto, y=abs_change_DF_W)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
#  scale_y_continuous(name="change_DF_W", limits=c(0,12))+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

DF6<-ggplot(z1, aes(x=MATA_auto, y=abs_change_DF_D)) +
  geom_point(aes(fill=Region),size=3, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  geom_smooth(method='lm', formula= y~x, color = "black")+
 # scale_y_continuous(name="change_DF_D", limits=c(0,12))+
  theme_classic() + theme(axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                          axis.text.y = element_text(size=14,face="bold"),
                          axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
                          axis.title.y = element_text(color="black", size=15,vjust = 2, face="bold",hjust=0.5))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size=15))

plot_grid(DF1,DF2,DF3,DF4,DF5,DF6,ncol = 2) #Random






