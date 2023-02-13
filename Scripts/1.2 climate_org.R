##########################################################################################################
## Organize climate lag data into trait spreadsheet
## Standardised Precipitation-Evapotranspiration Index
## Author Daniel Anstett
## 
##
## Last Modified Feb 3, 2022
##########################################################################################################

# Clear environment
rm(list = ls())

#Import libraries
library(tidyverse)
##########################################################################################################
#Import files
spei_pop <- read.csv("Data/spei_pop.csv", header=T)

y3 <- read.csv("Data/y3.csv", header=T) #Import trait data
y3$Site_Amy <- y3$Site #Duplicate site ID variable

#Select Relevant Data
y3<-y3 %>% select(ID_Year,Latitude,Longitude,Site_Family1:Year,Experiment_Date,SLA)

#Define regions
y3<-y3 %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                  ifelse((Latitude >35) & (Latitude <40), "Center","South")))

#Import Climate Data
wna <- read_csv("Data/climate.csv") %>%  #Oct to Setp data
  select(Site=ID, MAT.clim=MAT,MAP.clim=MAP,CMD.clim=CMD) 
#%>% wna$Site <- as.factor(wna$Site)

#Import Weather Data
wna2 <- read_csv("Data/weather.csv") #Import
wna2 <- wna2 %>% #Selects MAT, MAP, CMD,
  select(ID_Year1:CMD.weath.2) %>% 
  mutate(log.MAP.weath = log10(MAP.weath), log.MAP.weath.1 = log10(MAP.weath.1),log.MAP.weath.2 = log10(MAP.weath.2)) %>% 
  separate(ID_Year1, into = c("Site", "Year"), sep = "_") #makes site/year variable
wna2$Site <- as.factor(wna2$Site) ; wna2$Year <- as.numeric(wna2$Year) #define variables


##########################################################################################################

#Change sites codes to match Anstett et al. 2021 Evolution Letters
y3$Site <- gsub('S02',"1",y3$Site)
y3$Site <- gsub('S11',"12",y3$Site)
y3$Site <- gsub('S07',"2",y3$Site)
y3$Site <- gsub('S10',"3",y3$Site)
y3$Site <- gsub('S08',"4",y3$Site)
y3$Site <- gsub('S32',"5",y3$Site)

y3$Site <- gsub('S29',"6",y3$Site)
y3$Site <- gsub('S18',"7",y3$Site)
y3$Site <- gsub('S17',"8",y3$Site)
y3$Site <- gsub('S16',"9",y3$Site)
y3$Site <- gsub('S36',"10",y3$Site)
y3$Site <- gsub('S15',"11",y3$Site)

##########################################################################################################
#Setup SPRI Lags
#Make lag columns
spei_pop_2010 <- spei_pop %>% select(SPEI_2010, SPEI_2009, SPEI_2008, SPEI_2007)
colnames(spei_pop_2010) <- c("SPEI_2010_lag0","SPEI_2010_lag1", "SPEI_2010_lag2","SPEI_2010_lag3")

spei_pop_2011 <- spei_pop %>% select(SPEI_2011, SPEI_2010, SPEI_2009, SPEI_2008)
colnames(spei_pop_2011) <- c("SPEI_2011_lag0","SPEI_2011_lag1", "SPEI_2011_lag2","SPEI_2011_lag3")

spei_pop_2012 <- spei_pop %>% select(SPEI_2012, SPEI_2011, SPEI_2010, SPEI_2009)
colnames(spei_pop_2012) <- c("SPEI_2012_lag0","SPEI_2012_lag1", "SPEI_2012_lag2","SPEI_2012_lag3")

spei_pop_2013 <- spei_pop %>% select(SPEI_2013, SPEI_2012, SPEI_2011, SPEI_2010)
colnames(spei_pop_2013) <- c("SPEI_2013_lag0","SPEI_2013_lag1", "SPEI_2013_lag2","SPEI_2013_lag3")

spei_pop_2014 <- spei_pop %>% select(SPEI_2014, SPEI_2013, SPEI_2012, SPEI_2011)
colnames(spei_pop_2014) <- c("SPEI_2014_lag0","SPEI_2014_lag1", "SPEI_2014_lag2","SPEI_2014_lag3")

spei_pop_2015 <- spei_pop %>% select(SPEI_2015, SPEI_2014, SPEI_2013, SPEI_2012)
colnames(spei_pop_2015) <- c("SPEI_2015_lag0","SPEI_2015_lag1", "SPEI_2015_lag2","SPEI_2015_lag3")

spei_pop_2016 <- spei_pop %>% select(SPEI_2016, SPEI_2015, SPEI_2014, SPEI_2013)
colnames(spei_pop_2016) <- c("SPEI_2016_lag0","SPEI_2016_lag1", "SPEI_2016_lag2","SPEI_2016_lag3")

#Merge with rest of data
spei_pop_basic <- spei_pop %>% select(Site:Long_spei)
spei_lag_columns <- cbind(spei_pop_basic,spei_pop_2010,spei_pop_2011,spei_pop_2012,spei_pop_2013,
                          spei_pop_2014,spei_pop_2015,spei_pop_2016)

#Re-organize columns to get lag data as separate columns 
spei_gather <- spei_lag_columns %>% gather(Year, SPEI,SPEI_2010_lag0:SPEI_2016_lag3)
spei_gather$Year <- gsub("SPEI_","",spei_gather$Year)
spei_lag <- spei_gather %>% separate(Year,c("Year","Lag"),sep="_")
spei_lag <- spei_lag %>% spread(Lag,SPEI) 

#Calculate average across multiple years
spei_lag$lag01 <- spei_lag$lag0 + spei_lag$lag1
spei_lag$lag012 <- spei_lag$lag0 + spei_lag$lag1 + spei_lag$lag2
spei_lag$lag0123 <- spei_lag$lag0 + spei_lag$lag1 + spei_lag$lag2 + spei_lag$lag3

#Make a Site/Year Variable with for climate and for y3
spei_lag$ID2 <- spei_lag$ID
spei_lag$Year2 <- spei_lag$Year
spei_lag <- spei_lag %>% unite(col="ID_Year", c("ID2","Year2"),sep="_")

#Input Lag data into phenotypic dataframe
y9 <- left_join(y3,spei_lag,by="ID_Year")
names(y9)[names(y9) == 'Year.x'] <- 'Year'

# join climate and weather, calculate anomaly
wna_all <- left_join(wna2, wna, by="Site") %>% 
  mutate(CMDA_lag0 = CMD.weath - CMD.clim,
         CMDA_lag1 = CMD.weath.1 - CMD.clim,
         CMDA_lag2 = CMD.weath.2 - CMD.clim, 
         CMDA_lag01 = CMDA_lag0 + CMDA_lag1,
         CMDA_lag012 = CMDA_lag0 + CMDA_lag1 + CMDA_lag2,
         
         MATA_lag0 = MAT.weath - MAT.clim,
         MATA_lag1 = MAT.weath.1 - MAT.clim,
         MATA_lag2 = MAT.weath.2 - MAT.clim,
         MATA_lag01 = MATA_lag0 + MATA_lag1,
         MATA_lag012 = MATA_lag0 + MATA_lag1 + MATA_lag2,
         
         MAPA_lag0 = log.MAP.weath - log10(MAP.clim),       
         MAPA_lag1 = log.MAP.weath.1 - log10(MAP.clim),
         MAPA_lag2 = log.MAP.weath.2 - log10(MAP.clim),
         MAPA_lag01 = MAPA_lag0 + MAPA_lag1,
         MAPA_lag012 = MAPA_lag0 + MAPA_lag1 + MAPA_lag2
  )

#Make a Site/Year Variable with for climate
wna_all$ID2 <- wna_all$ID
wna_all$Year2 <- wna_all$Year
wna_all <- wna_all %>% unite(col="ID_Year", c("ID2","Year2"),sep="_")
         
#Join
y9 <- left_join(y9,wna_all,by="ID_Year")

#Clean Up names
y9 <- y9 %>% select(-Year.y,-Year.y.y,-Latitude.y,-Longitude.y,-Elevation.y,-ID.y,-Site.y)
names(y9)[names(y9) == 'Year.x'] <- 'Year'
names(y9)[names(y9) == 'Latitude.x'] <- 'Latitude'
names(y9)[names(y9) == 'Longitude.x'] <- 'Longitude'
names(y9)[names(y9) == 'Elevation.x'] <- 'Elevation'
names(y9)[names(y9) == 'ID.x'] <- 'ID'
names(y9)[names(y9) == 'Site.x'] <- 'Site'

write.csv(y9,"Data/y9.csv")


#Make a site/year table for SPEI, CMDA, MAPA, MATA
env_lag0_all <- left_join(spei_lag,wna_all,by="ID_Year")
env_lag0 <- env_lag0_all %>% select(Site.x:Long,Year.x,lag0,CMDA_lag0,MAPA_lag0,MATA_lag0)
names(env_lag0)[names(env_lag0) == 'Site.x'] <- 'Site'
names(env_lag0)[names(env_lag0) == 'ID.x'] <- 'ID'
names(env_lag0)[names(env_lag0) == 'Year.x'] <- 'Year'
names(env_lag0)[names(env_lag0) == 'lag0'] <- 'SPEI_lag0'

write.csv(env_lag0,"Data/env_lag0.csv")











