######

#Import Library
library(tidyverse)

#Import data
dat <- read.csv('Data/env_lag0.csv')

#Clean up dataset and set regions and lables
dat<-dat %>% mutate(Region = ifelse(Lat >= 40, "North", 
                                            ifelse((Lat >35) & (Lat <40), "Centre","South"))) %>%
  mutate(Site.Lat=paste(Lat,Site,sep="_")) #Set up Site.Lat
dat$Region<-as.factor(dat$Region) #Make Region appear in logical order
dat$Region<-factor(dat$Region,levels=c("North","Centre","South"))
Site.label<-c("32.89928_1"="1","34.07808_12"="2","34.28425_2"="3","36.20081_3"="4", 
              "36.69096_4"="5","37.539_5"="6","39.39442_6"="7", "39.74298_7"="8", 
              "41.66546_8"="9","41.80979_9"="10","42.27411_10"="11", "43.37876_11"="12")

dat$Site <- as.character(dat$Site)
#Use ggplot to generate plot with all required formating
MAPA_Lat<-ggplot(dat, aes(x=Site.Lat, y=MAPA_lag0, shape=factor(Year), col=factor(Region)))+ 
  geom_point(aes(fill=Region), size =3)+
  scale_shape_manual(values =c(48:54))+
  scale_y_continuous(name="MAPA")+
  xlab("Site")+
  geom_hline(yintercept = 0, color="black", size=0.8)+
  coord_flip()+
  scale_color_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333")) +
  theme_classic()
MAPA_Lat<-MAPA_Lat + theme(legend.position = "none",
                           axis.title.x=element_text(size=16,vjust = 0, face="bold",hjust=0.5),
                           #axis.ticks.x = element_blank(),
                           axis.text.x = element_text(size=14, face="bold", angle=0,hjust=0.5),
                           axis.text.y = element_text(size=14,face="bold"),
                           axis.title.y = element_text(size=16,vjust = 0, face="bold",hjust=0.5)) +
  scale_x_discrete(labels=Site.label)
MAPA_Lat
