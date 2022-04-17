##################################################################################
## Daniel Anstett
## Generate climate focused Fig 1
## Using Climate Moisture Deficit (CMD) and Cliamte Moisture Deficit Anomaly (CMDA)
##
## Last Modified January 22, 2020
###################################################################################


###################################################################################
#Import libraries
library(tidyverse)
library(cowplot)
library(ggmap)

###################################################################################
###################################################################################
# Fig 1A, make map with sites

#Import California & Oregon Map
cali_or <- subset(map_data("state"), region %in% c("california", "oregon"))
#Import Site Data
site.lat.long <- read.csv("Data/sites.csv", header=T)
#site.lat.long <- site.lat.long %>% filter(Site!=12)

#Map Making
base_map <- ggplot(cali_or) + 
  geom_polygon(aes(x=long,y=lat,group = group), colour="black", fill="white") + coord_fixed(1.3) +
  geom_point(data=site.lat.long, aes(x=Longitude, y=Latitude, fill=Region),size=4, shape=21,color="black")+
  scale_fill_manual(values= c("North"="#3399FF", "Centre"="#FFCC00", "South"="#FF3333"))+
  theme_nothing()
base_map

###### MAPA across sites ######
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



######
weather <- read.csv("Data/weather.csv")
weather <- filter(weather, ID %in%  c("S02", "S10", "S36"))



wna1 <- read_csv("Climate/timeseries_lat_2010-2016.csv")
impact_all <- wna1 %>% select(ID_Year1,Year,Elevation) #Get relevant meta_data

#Import datasets and add year_actual variable
weather_2007 <- read.csv("Climate/timeseries_monthly_2007.csv", header=T)
weather_2008 <- read.csv("Climate/timeseries_monthly_2008.csv", header=T)
weather_2009 <- read.csv("Climate/timeseries_monthly_2009.csv", header=T)
weather_2010 <- read.csv("Climate/timeseries_monthly_2010.csv", header=T)
weather_2011 <- read.csv("Climate/timeseries_monthly_2011.csv", header=T)
weather_2012 <- read.csv("Climate/timeseries_monthly_2012.csv", header=T)
weather_2013 <- read.csv("Climate/timeseries_monthly_2013.csv", header=T)
weather_2014 <- read.csv("Climate/timeseries_monthly_2014.csv", header=T)
weather_2015 <- read.csv("Climate/timeseries_monthly_2015.csv", header=T)
weather_2016 <- read.csv("Climate/timeseries_monthly_2016.csv", header=T)

weather_2007$MAPavg <- rowMeans(weather_2007[ , c(42,53)], na.rm=TRUE)
weather_2008$MAPavg <- rowMeans(weather_2008[ , c(42,53)], na.rm=TRUE)
weather_2009$MAPavg <- rowMeans(weather_2009[ , c(42,53)], na.rm=TRUE)
weather_2010$MAPavg <- rowMeans(weather_2010[ , c(42,53)], na.rm=TRUE)
weather_2011$MAPavg <- rowMeans(weather_2011[ , c(42,53)], na.rm=TRUE)
weather_2012$MAPavg <- rowMeans(weather_2012[ , c(42,53)], na.rm=TRUE)
weather_2013$MAPavg <- rowMeans(weather_2013[ , c(42,53)], na.rm=TRUE)
weather_2014$MAPavg <- rowMeans(weather_2014[ , c(42,53)], na.rm=TRUE)
weather_2015$MAPavg <- rowMeans(weather_2015[ , c(42,53)], na.rm=TRUE)
weather_2016$MAPavg <- rowMeans(weather_2016[ , c(42,53)], na.rm=TRUE)

weather_2007 <- subset(weather_2007, select=c(ID, ID2, Latitude, MAPavg))
weather_2008 <- subset(weather_2008, select=c(ID, ID2, Latitude, MAPavg))
weather_2009 <- subset(weather_2009, select=c(ID, ID2, Latitude, MAPavg))
weather_2010 <- subset(weather_2010, select=c(ID, ID2, Latitude, MAPavg))
weather_2011 <- subset(weather_2011, select=c(ID, ID2, Latitude, MAPavg))
weather_2012 <- subset(weather_2012, select=c(ID, ID2, Latitude, MAPavg))
weather_2013 <- subset(weather_2013, select=c(ID, ID2, Latitude, MAPavg))
weather_2014 <- subset(weather_2014, select=c(ID, ID2, Latitude, MAPavg))
weather_2015 <- subset(weather_2015, select=c(ID, ID2, Latitude, MAPavg))
weather_2016 <- subset(weather_2016, select=c(ID, ID2, Latitude, MAPavg))

weather_2007$Year <- 2007
weather_2008$Year <- 2008
weather_2009$Year <- 2009
weather_2010$Year <- 2010
weather_2011$Year <- 2011
weather_2012$Year <- 2012
weather_2013$Year <- 2013
weather_2014$Year <- 2014
weather_2015$Year <- 2015
weather_2016$Year <- 2016

weather_all <- rbind(weather_2007,weather_2008,weather_2009,weather_2010,weather_2011,weather_2012,weather_2013,weather_2014,weather_2015,weather_2016)
weather_all <- filter(weather_all, ID %in%  c("S02", "S10", "S36"))
#write.csv(weather_all, "Data/weather_3_fig1.csv")
weather_all<-read.csv("Data/weather_3_fig1.csv")

library(ggplot2)

graph <- ggplot(weather_all, aes(Year, y=MAPavg, fill=ID, colour=ID))+
  geom_line(aes(colour=ID))+
  xlab("Year") +
  scale_y_continuous(name="MAP")+
  scale_color_manual(values= c("S02"="#FF3333", "S10"="#FFCC00", "S36"="#3399FF"))+
  scale_fill_manual(values= c("S02"="#FF3333", "S10"="#FFCC00", "S36"="#3399FF"))+
  theme_classic()
graph <-graph + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
graph <-graph +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
graph

###################################################################################
#Place all four figures into one output using Cowplot. Export at  6 X 10 inches
plot_grid(base_map, MAP_years, MAPA_Lat,ncol = 4,labels = "AUTO", 
          rel_widths = c(1.07, 1, 1, 1.5), rel_heights = c(1, 0.1, 0.1, 0.1))

###################################################################################
