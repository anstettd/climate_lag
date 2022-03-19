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

##########################################################################################################
#Import files

z1 <- read.csv("Data/site_lat_lag.csv", header=T)  #Basic Lat/Long plus headers

#Make Graphs
ggplot(z1, aes(x=SPEI_auto, y=change_SLA_W, group=Region)) +
  geom_point(aes(color=Region)) +
  theme_classic()+ theme(
    axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
    axis.text.y = element_text(size=15,face="bold"),
    axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
    axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))

ggplot(z1, aes(x=SPEI_auto, y=change_SLA_D, group=Region)) +
  geom_point(aes(color=Region)) +
  theme_classic()+ theme(
    axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
    axis.text.y = element_text(size=15,face="bold"),
    axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
    axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))




