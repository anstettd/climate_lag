##################################################################################
## Haley Branch & Daniel Anstett
## Generate climate focused Fig 1
## Using MAPA
##
## Last Modified Dec 6, 2023
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
dat <- filter(dat, ID %in%  c("S02", "S10", "S36"))

Site.label<-c("32.89928_1"="1","36.20081_3"="2","42.27411_10"="3")

dat$Site <- as.character(dat$Site)
#Use ggplot to generate plot with all required formating
MAPA_Lat<-ggplot(dat, aes(x=Site.Lat, y=MAPA_lag0, shape=factor(Year), col=factor(Region)))+ 
  geom_point(aes(fill=Region), size =9)+
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



###############################################################################################################
#Make MAP timeseries
weather <- read.csv("Data/weather.csv")
weather <- filter(weather, ID %in%  c("S02", "S10", "S36")) %>% 
  select(ID,ID2,Latitude,Year,MAP.weath,MAP.weath.1,MAP.weath.2)

#Get 2008 and 2009 water years
weather09_10 <- weather %>% filter(Year==2010 | Year==2011) %>% select(-MAP.weath,-MAP.weath.1)
weather09_10$Year[1:3] <- 2008 ; weather09_10$Year[4:6] <- 2009
names(weather09_10)[names(weather09_10)=="MAP.weath.2"] <- "MAP.weath"

#Merge into single data frame
weather <- weather %>% select(-MAP.weath.1,-MAP.weath.2)
weather_all <- rbind(weather,weather09_10)
names(weather_all)[names(weather_all)=="MAP.weath"] <- "MAP"



#Trend line
graph <- ggplot(weather_all, aes(Year, y=MAP, fill=ID, colour=ID))+
  geom_line(aes(colour=ID))+
  xlab("Year") +
  scale_y_continuous(name="MAP")+
  scale_color_manual(values= c("S02"="#FF3333", "S10"="#FFCC00", "S36"="#3399FF"))+
  scale_fill_manual(values= c("S02"="#FF3333", "S10"="#FFCC00", "S36"="#3399FF"))+
  ylim(0,1800)+
  xlim(2007,2016)+
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
ggsave("Conceptual_fig/Fig1b1.pdf", width = 6, height = 4, units = "in")



#######################
#Correct
#30-Year Mean
weather <- read.csv("Data/weather.csv")
climate <- read.csv("Data/climate.csv")
climate_yearly <- read.csv("Data/m_year.csv")

#Filter for MAP and correct sites
climate <- filter(climate, ID %in%  c("S02", "S10", "S36")) %>% select(-MAT,-CMD)
climate_yearly <- filter(climate_yearly, ID %in%  c("S02", "S10", "S36")) %>% select(-MAT,-CMD)

#Select each site
yearly_south <- climate_yearly %>% filter(ID=="S02")
yearly_centre <- climate_yearly %>% filter(ID=="S10")
yearly_north <- climate_yearly %>% filter(ID=="S36")

#Calc Standard Error for each Site
error_input <- data.frame()
error_input[1,1] <- qt(0.975, df=30-1)*sd(yearly_south$MAP)/sqrt(30)
error_input[2,1] <- qt(0.975, df=30-1)*sd(yearly_centre$MAP)/sqrt(30)
error_input[3,1] <- qt(0.975, df=30-1)*sd(yearly_north$MAP)/sqrt(30)
error_input[1,2] <- "South"
error_input[2,2] <- "Centre"
error_input[3,2] <- "North"
colnames(error_input) <- c("STDER","Region")

#Merge dataframes, calc upper and lower error
mean_ci_30y <- cbind(climate,error_input) %>% mutate(Upper_CI=MAP+STDER) %>% mutate(Lower_CI=MAP-STDER)
mean_ci_30y$Region<-as.factor(mean_ci_30y$Region) #Make Region appear in logical order
mean_ci_30y$Region<-factor(mean_ci_30y$Region,levels=c("North","Centre","South"))

ggplot(mean_ci_30y, aes(Region, y=MAP)) +
  geom_errorbar(width=.1, aes(ymin=Lower_CI, ymax=Upper_CI, colour=Region)) +
  geom_point(aes(fill=Region, colour=Region), shape=21, size=3) +
  scale_color_manual(values= c("South"="#FF3333", "Centre"="#FFCC00", "North"="#3399FF"))+
  scale_fill_manual(values= c("South"="#FF3333", "Centre"="#FFCC00", "North"="#3399FF"))+
  ylim(0,1800)+
  theme_classic()
ggsave("Conceptual_fig/Fig1b2.pdf", width = 6, height = 4, units = "in")
























