##########################################################################################################
## Organize climate lag data into autocorrelation metric
## For multiple start and end points: 2010-2013,2011-2014,2012-2015,2013-2016
## Author Daniel Anstett
## 
##
## Last Modified March 26, 2022
##########################################################################################################

# Clear environment
rm(list = ls())

#Import libraries
library(tidyverse)
library(stats)
##########################################################################################################
#Import files

z1 <- read.csv("Data/site_lat_lag.csv", header=T)  #Basic Lat/Long plus headers
env_lag0 <- read.csv("Data/env_lag0.csv", header=T) #site/year climate data
y9 <- read.csv("Data/y9.csv", header=T) #trait data
y9 %>% 
  
  ##########################################################################################################
#Filter datasets
env_1012 <- env_lag0 %>% filter(Year %in% (2010:2012))
env_1113 <- env_lag0 %>% filter(Year %in% (2011:2013))
env_1214 <- env_lag0 %>% filter(Year %in% (2012:2014))
env_1315 <- env_lag0 %>% filter(Year %in% (2013:2015))
env_1416 <- env_lag0 %>% filter(Year %in% (2014:2016))

y9_1012 <- y9 %>% filter(Year %in% (2010:2012))
y9_1113 <- y9 %>% filter(Year %in% (2011:2013))
y9_1214 <- y9 %>% filter(Year %in% (2012:2014))
y9_1315 <- y9 %>% filter(Year %in% (2013:2015))
y9_1416 <- y9 %>% filter(Year %in% (2014:2016))

z_1012 <- z1 
z_1113 <- z1 
z_1214 <- z1 
z_1315 <- z1
z_1416 <- z1
#Figure out which years are missing
y9.1 <- y9 %>% unite(Site_Year,Year,Site,sep="_")
unique(y9.1$Site_Year)


#Remove sites not in the phenotype available range
env_1012 <- env_1012 %>% filter(Site!=2 & Site!=4 & Site!=5 & Site!=8 & Site!=10 & Site!=11)
env_1114 <- env_1114 %>% filter(Site!=5 & Site!=6 & Site!=7 & Site!=9 & Site!=11 & Site!=12)
env_1215 <- env_1215 %>% filter(Site!=1 & Site!=2 & Site!=4 & Site!=6 & Site!=7 & Site!=8 & Site!=12)
env_1316 <- env_1316 %>% filter(Site!=1 & Site!=2 & Site!=4 & Site!=5 & Site!=8 & Site!=10 & Site!=11)

z_1013 <- z_1013 %>% filter(Site!=2 & Site!=4 & Site!=5 & Site!=8 & Site!=10 & Site!=11)
z_1114 <- z_1114 %>% filter(Site!=5 & Site!=6 & Site!=7 & Site!=9 & Site!=11 & Site!=12)
z_1215 <- z_1215 %>% filter(Site!=1 & Site!=2 & Site!=4 & Site!=6 & Site!=7 & Site!=8 & Site!=12)
z_1316 <- z_1316 %>% filter(Site!=1 & Site!=2 & Site!=4 & Site!=5 & Site!=8 & Site!=10 & Site!=11)


#Input autocorr and mean for each climate variable across sites
sites_1013 <- unique(env_1013$Site)
sites_1114 <- unique(env_1114$Site)
sites_1215 <- unique(env_1215$Site)
sites_1316 <- unique(env_1316$Site)



##########################################################################################################
#Calculate autocorrelation and mean

#2010-2013 & 2011-2014

for(i in 1:6){
  for(j in 1:4){
    env_1 <- env_1013 %>% filter(Site==sites_1013[i])
    env_2 <- env_1114 %>% filter(Site==sites_1114[i])
    auto_corr_1 <- acf(env_1[8+j], lag=1,pl=FALSE) #calculate lag1 autocorr
    auto_corr_2 <- acf(env_2[8+j], lag=1,pl=FALSE) #calculate lag1 autocorr
    mean_1 <- mean(env_1[,8+j])
    mean_2 <- mean(env_2[,8+j])
    z_1013[i,9+j] <- auto_corr_1$acf[2]
    z_1114[i,9+j] <- auto_corr_2$acf[2]
    z_1013[i,13+j] <- mean_1
    z_1114[i,13+j] <- mean_2
  }
}


##2012-2015 & 2013-2016
for(i in 1:5){
  for(j in 1:4){
    env_3 <- env_1215 %>% filter(Site==sites_1215[i])
    env_4 <- env_1316 %>% filter(Site==sites_1316[i])
    auto_corr_3 <- acf(env_1[8+j], lag=1,pl=FALSE) #calculate lag1 autocorr
    auto_corr_4 <- acf(env_2[8+j], lag=1,pl=FALSE) #calculate lag1 autocorr
    mean_3 <- mean(env_1[,8+j])
    mean_4 <- mean(env_2[,8+j])
    z_1215[i,9+j] <- auto_corr_3$acf[2]
    z_1316[i,9+j] <- auto_corr_4$acf[2]
    z_1215[i,13+j] <- mean_3
    z_1316[i,13+j] <- mean_4
  }
}

##########################################################################################################
#Get endpoints for traits

#2010-2013 & 2011-2014
for(i in 1:6){
  start_W_1 <- y9_1013 %>% filter(Site==sites_1013[i] & Year==2010 & Drought=="W") 
  end_W_1 <- y9_1013 %>% filter(Site==sites_1013[i] & Year==2013 & Drought=="W") 
  start_D_1 <- y9_1013 %>% filter(Site==sites_1013[i] & Year==2010 & Drought=="D") 
  end_D_1 <- y9_1013 %>% filter(Site==sites_1013[i] & Year==2013 & Drought=="D") 
  
  start_W_2 <- y9_1114 %>% filter(Site==sites_1114[i] & Year==2011 & Drought=="W") 
  end_W_2 <- y9_1114 %>% filter(Site==sites_1114[i] & Year==2014 & Drought=="W") 
  start_D_2 <- y9_1114 %>% filter(Site==sites_1114[i] & Year==2011 & Drought=="D") 
  end_D_2 <- y9_1114 %>% filter(Site==sites_1114[i] & Year==2014 & Drought=="D") 
  
  #SLA
  start_SLA_W_1 <-mean(start_W_1$SLA,na.rm = TRUE)
  end_SLA_W_1 <-mean(end_W_1$SLA,na.rm = TRUE)
  start_SLA_D_1 <-mean(start_D_1$SLA,na.rm = TRUE)
  end_SLA_D_1 <-mean(end_D_1$SLA,na.rm = TRUE)
  
  start_SLA_W_2 <-mean(start_W_2$SLA,na.rm = TRUE)
  end_SLA_W_2 <-mean(end_W_2$SLA,na.rm = TRUE)
  start_SLA_D_2 <-mean(start_D_2$SLA,na.rm = TRUE)
  end_SLA_D_2 <-mean(end_D_2$SLA,na.rm = TRUE)
  
  #Date of Flowering
  start_DF_W_1 <-mean(start_W_1$Experiment_Date,na.rm = TRUE)
  end_DF_W_1 <-mean(end_W_1$Experiment_Date,na.rm = TRUE)
  start_DF_D_1 <-mean(start_D_1$Experiment_Date,na.rm = TRUE)
  end_DF_D_1 <-mean(end_D_1$Experiment_Date,na.rm = TRUE)
  
  start_DF_W_2 <-mean(start_W_2$Experiment_Date,na.rm = TRUE)
  end_DF_W_2 <-mean(end_W_2$Experiment_Date,na.rm = TRUE)
  start_DF_D_2 <-mean(start_D_2$Experiment_Date,na.rm = TRUE)
  end_DF_D_2 <-mean(end_D_2$Experiment_Date,na.rm = TRUE)
  
  #Change in trait
  z_1013[i,18] <- end_SLA_W_1 - start_SLA_W_1
  z_1013[i,19] <- end_SLA_D_1 - start_SLA_D_1
  z_1013[i,20] <- end_DF_W_1 - start_DF_W_1
  z_1013[i,21] <- end_DF_D_1 - start_DF_D_1
  
  z_1114[i,18] <- end_SLA_W_2 - start_SLA_W_2
  z_1114[i,19] <- end_SLA_D_2 - start_SLA_D_2
  z_1114[i,20] <- end_DF_W_2 - start_DF_W_2
  z_1114[i,21] <- end_DF_D_2 - start_DF_D_2
}

z_1013 <- z_1013 %>% mutate(abs_change_SLA_W = abs(change_SLA_W),
                            abs_change_SLA_D = abs(change_SLA_D),
                            abs_change_DF_W = abs(change_DF_W),
                            abs_change_DF_D = abs(change_DF_D))
z_1114 <- z_1114 %>% mutate(abs_change_SLA_W = abs(change_SLA_W),
                            abs_change_SLA_D = abs(change_SLA_D),
                            abs_change_DF_W = abs(change_DF_W),
                            abs_change_DF_D = abs(change_DF_D))

##2012-2015 & 2013-2016
for(i in 1:5){
  start_W_3 <- y9_1215 %>% filter(Site==sites_1215[i] & Year==2012 & Drought=="W") 
  end_W_3 <- y9_1215 %>% filter(Site==sites_1215[i] & Year==2015 & Drought=="W") 
  start_D_3 <- y9_1215 %>% filter(Site==sites_1215[i] & Year==2012 & Drought=="D") 
  end_D_3 <- y9_1215 %>% filter(Site==sites_1215[i] & Year==2015 & Drought=="D") 
  
  start_W_4 <- y9_1316 %>% filter(Site==sites_1316[i] & Year==2013 & Drought=="W") 
  end_W_4 <- y9_1316 %>% filter(Site==sites_1316[i] & Year==2016 & Drought=="W") 
  start_D_4 <- y9_1316 %>% filter(Site==sites_1316[i] & Year==2013 & Drought=="D") 
  end_D_4 <- y9_1316 %>% filter(Site==sites_1316[i] & Year==2016 & Drought=="D") 
  
  #SLA
  start_SLA_W_3 <-mean(start_W_3$SLA,na.rm = TRUE)
  end_SLA_W_3 <-mean(end_W_3$SLA,na.rm = TRUE)
  start_SLA_D_3 <-mean(start_D_3$SLA,na.rm = TRUE)
  end_SLA_D_3 <-mean(end_D_3$SLA,na.rm = TRUE)
  
  start_SLA_W_4 <-mean(start_W_4$SLA,na.rm = TRUE)
  end_SLA_W_4 <-mean(end_W_4$SLA,na.rm = TRUE)
  start_SLA_D_4 <-mean(start_D_4$SLA,na.rm = TRUE)
  end_SLA_D_4 <-mean(end_D_4$SLA,na.rm = TRUE)
  
  #Date of Flowering
  start_DF_W_3 <-mean(start_W_3$Experiment_Date,na.rm = TRUE)
  end_DF_W_3 <-mean(end_W_3$Experiment_Date,na.rm = TRUE)
  start_DF_D_3 <-mean(start_D_3$Experiment_Date,na.rm = TRUE)
  end_DF_D_3 <-mean(end_D_3$Experiment_Date,na.rm = TRUE)
  
  start_DF_W_4 <-mean(start_W_4$Experiment_Date,na.rm = TRUE)
  end_DF_W_4 <-mean(end_W_4$Experiment_Date,na.rm = TRUE)
  start_DF_D_4 <-mean(start_D_4$Experiment_Date,na.rm = TRUE)
  end_DF_D_4 <-mean(end_D_4$Experiment_Date,na.rm = TRUE)
  
  #Change in trait
  z_1215[i,18] <- end_SLA_W_3 - start_SLA_W_3
  z_1215[i,19] <- end_SLA_D_3 - start_SLA_D_3
  z_1215[i,20] <- end_DF_W_3 - start_DF_W_3
  z_1215[i,21] <- end_DF_D_3 - start_DF_D_3
  
  z_1316[i,18] <- end_SLA_W_4 - start_SLA_W_4
  z_1316[i,19] <- end_SLA_D_4 - start_SLA_D_4
  z_1316[i,20] <- end_DF_W_4 - start_DF_W_4
  z_1316[i,21] <- end_DF_D_4 - start_DF_D_4
}

z_1215 <- z_1215 %>% mutate(abs_change_SLA_W = abs(change_SLA_W),
                            abs_change_SLA_D = abs(change_SLA_D),
                            abs_change_DF_W = abs(change_DF_W),
                            abs_change_DF_D = abs(change_DF_D))
z_1316 <- z_1316 %>% mutate(abs_change_SLA_W = abs(change_SLA_W),
                            abs_change_SLA_D = abs(change_SLA_D),
                            abs_change_DF_W = abs(change_DF_W),
                            abs_change_DF_D = abs(change_DF_D))

#Make Identifier
z_1013$time_range <- "2010_2013"
z_1114$time_range <- "2011_2014" 
z_1215$time_range <- "2012_2015"
z_1316$time_range <- "2013_2016"

z_multiple_time <- rbind(z_1013,z_1114,z_1215,z_1316)

#Export data
write_csv(z_multiple_time,"Data/z_multiple_time.csv")










