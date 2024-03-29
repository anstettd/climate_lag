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
env_1012 <- env_1012 %>% filter(Site!=2 & Site!=4 & Site!=6 & Site!=8 & Site!=10 & Site!=12)
env_1113 <- env_1113 %>% filter(Site!=2 & Site!=5 & Site!=6 & Site!=7 & Site!=8 & Site!=9 & Site!=10, Site!=11)
env_1214 <- env_1214 %>% filter(Site!=2 & Site!=6 & Site!=8 & Site!=12)
env_1315 <- env_1315 %>% filter(Site!=1 & Site!=2 & Site!=4 & Site!=5 & Site!=7 & Site!=8 & Site!=10 & Site!=11)
env_1416 <- env_1416 %>% filter(Site!=1 & Site!=4 & Site!=12)

z_1012 <- z_1012 %>% filter(Site!=2 & Site!=4 & Site!=6 & Site!=8 & Site!=10 & Site!=12)
z_1113 <- z_1113 %>% filter(Site!=2 & Site!=5 & Site!=6 & Site!=7 & Site!=8 & Site!=9 & Site!=10, Site!=11)
z_1214 <- z_1214 %>% filter(Site!=2 & Site!=6 & Site!=8 & Site!=12)
z_1315 <- z_1315 %>% filter(Site!=1 & Site!=2 & Site!=4 & Site!=5 & Site!=7 & Site!=8 & Site!=10 & Site!=11)
z_1416 <- z_1416 %>% filter(Site!=1 & Site!=4 & Site!=12)


#Input autocorr and mean for each climate variable across sites
sites_1012 <- unique(env_1012$Site)
sites_1113 <- unique(env_1113$Site)
sites_1214 <- unique(env_1214$Site)
sites_1315 <- unique(env_1315$Site)
sites_1416 <- unique(env_1416$Site)


##########################################################################################################
#Calculate autocorrelation and mean

# 2010-2012

for(i in 1:6){
  for(j in 1:4){
    env_1 <- env_1012 %>% filter(Site==sites_1012[i])
    auto_corr_1 <- acf(env_1[8+j], lag=1,pl=FALSE) #calculate lag1 autocorr
    mean_1 <- mean(env_1[,8+j])
    z_1012[i,9+j] <- auto_corr_1$acf[2]
    z_1012[i,13+j] <- mean_1
  }
}

##2011-2013 & 2013-2015
for(i in 1:4){
  for(j in 1:4){
    env_2 <- env_1113 %>% filter(Site==sites_1113[i])
    env_3 <- env_1315 %>% filter(Site==sites_1315[i])
    auto_corr_2 <- acf(env_2[8+j], lag=1,pl=FALSE) #calculate lag1 autocorr
    auto_corr_3 <- acf(env_3[8+j], lag=1,pl=FALSE) #calculate lag1 autocorr
    mean_2 <- mean(env_2[,8+j])
    mean_3 <- mean(env_3[,8+j])
    z_1113[i,9+j] <- auto_corr_2$acf[2]
    z_1315[i,9+j] <- auto_corr_3$acf[2]
    z_1113[i,13+j] <- mean_2
    z_1315[i,13+j] <- mean_3
  }
}


## 2012-2014
for(i in 1:8){
  for(j in 1:4){
    env_4 <- env_1214 %>% filter(Site==sites_1214[i])
    auto_corr_4 <- acf(env_4[8+j], lag=1,pl=FALSE) #calculate lag1 autocorr
    mean_4 <- mean(env_4[,8+j])
    z_1214[i,9+j] <- auto_corr_4$acf[2]
    z_1214[i,13+j] <- mean_4
  }
}


## 2014-2016
for(i in 1:9){
  for(j in 1:4){
    env_5 <- env_1416 %>% filter(Site==sites_1416[i])
    auto_corr_5 <- acf(env_5[8+j], lag=1,pl=FALSE) #calculate lag1 autocorr
    mean_5 <- mean(env_5[,8+j])
    z_1416[i,9+j] <- auto_corr_5$acf[2]
    z_1416[i,13+j] <- mean_5
  }
}








##########################################################################################################
#Get endpoints for traits

#2010-2012
for(i in 1:6){
  start_W_1 <- y9_1012 %>% filter(Site==sites_1012[i] & Year==2010 & Drought=="W") 
  end_W_1 <- y9_1012 %>% filter(Site==sites_1012[i] & Year==2012 & Drought=="W") 
  start_D_1 <- y9_1012 %>% filter(Site==sites_1012[i] & Year==2010 & Drought=="D") 
  end_D_1 <- y9_1012 %>% filter(Site==sites_1012[i] & Year==2012 & Drought=="D") 
  
  #SLA
  start_SLA_W_1 <-mean(start_W_1$SLA,na.rm = TRUE)
  end_SLA_W_1 <-mean(end_W_1$SLA,na.rm = TRUE)
  start_SLA_D_1 <-mean(start_D_1$SLA,na.rm = TRUE)
  end_SLA_D_1 <-mean(end_D_1$SLA,na.rm = TRUE)
  
  #Date of Flowering
  start_DF_W_1 <-mean(start_W_1$Experiment_Date,na.rm = TRUE)
  end_DF_W_1 <-mean(end_W_1$Experiment_Date,na.rm = TRUE)
  start_DF_D_1 <-mean(start_D_1$Experiment_Date,na.rm = TRUE)
  end_DF_D_1 <-mean(end_D_1$Experiment_Date,na.rm = TRUE)
 
  #Change in trait
  z_1012[i,18] <- end_SLA_W_1 - start_SLA_W_1
  z_1012[i,19] <- end_SLA_D_1 - start_SLA_D_1
  z_1012[i,20] <- end_DF_W_1 - start_DF_W_1
  z_1012[i,21] <- end_DF_D_1 - start_DF_D_1
  
}

z_1012 <- z_1012 %>% mutate(abs_change_SLA_W = abs(change_SLA_W),
                            abs_change_SLA_D = abs(change_SLA_D),
                            abs_change_DF_W = abs(change_DF_W),
                            abs_change_DF_D = abs(change_DF_D))

## 2011-2013 & 2013-2015
for(i in 1:4){
  start_W_3 <- y9_1113 %>% filter(Site==sites_1113[i] & Year==2011 & Drought=="W") 
  end_W_3 <- y9_1113 %>% filter(Site==sites_1113[i] & Year==2013 & Drought=="W") 
  start_D_3 <- y9_1113 %>% filter(Site==sites_1113[i] & Year==2011 & Drought=="D") 
  end_D_3 <- y9_1113 %>% filter(Site==sites_1113[i] & Year==2013 & Drought=="D") 
  
  start_W_4 <- y9_1315 %>% filter(Site==sites_1315[i] & Year==2013 & Drought=="W") 
  end_W_4 <- y9_1315 %>% filter(Site==sites_1315[i] & Year==2015 & Drought=="W") 
  start_D_4 <- y9_1315 %>% filter(Site==sites_1315[i] & Year==2013 & Drought=="D") 
  end_D_4 <- y9_1315 %>% filter(Site==sites_1315[i] & Year==2015 & Drought=="D") 
  
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
  z_1113[i,18] <- end_SLA_W_3 - start_SLA_W_3
  z_1113[i,19] <- end_SLA_D_3 - start_SLA_D_3
  z_1113[i,20] <- end_DF_W_3 - start_DF_W_3
  z_1113[i,21] <- end_DF_D_3 - start_DF_D_3
  
  z_1315[i,18] <- end_SLA_W_4 - start_SLA_W_4
  z_1315[i,19] <- end_SLA_D_4 - start_SLA_D_4
  z_1315[i,20] <- end_DF_W_4 - start_DF_W_4
  z_1315[i,21] <- end_DF_D_4 - start_DF_D_4
}

z_1113 <- z_1113 %>% mutate(abs_change_SLA_W = abs(change_SLA_W),
                            abs_change_SLA_D = abs(change_SLA_D),
                            abs_change_DF_W = abs(change_DF_W),
                            abs_change_DF_D = abs(change_DF_D))
z_1315 <- z_1315 %>% mutate(abs_change_SLA_W = abs(change_SLA_W),
                            abs_change_SLA_D = abs(change_SLA_D),
                            abs_change_DF_W = abs(change_DF_W),
                            abs_change_DF_D = abs(change_DF_D))

# 2012_2014
for(i in 1:8){
  start_W_1 <- y9_1214 %>% filter(Site==sites_1214[i] & Year==2012 & Drought=="W") 
  end_W_1 <- y9_1214 %>% filter(Site==sites_1214[i] & Year==2014 & Drought=="W") 
  start_D_1 <- y9_1214 %>% filter(Site==sites_1214[i] & Year==2012 & Drought=="D") 
  end_D_1 <- y9_1214 %>% filter(Site==sites_1214[i] & Year==2014 & Drought=="D") 
  
  #SLA
  start_SLA_W_1 <-mean(start_W_1$SLA,na.rm = TRUE)
  end_SLA_W_1 <-mean(end_W_1$SLA,na.rm = TRUE)
  start_SLA_D_1 <-mean(start_D_1$SLA,na.rm = TRUE)
  end_SLA_D_1 <-mean(end_D_1$SLA,na.rm = TRUE)
  
  #Date of Flowering
  start_DF_W_1 <-mean(start_W_1$Experiment_Date,na.rm = TRUE)
  end_DF_W_1 <-mean(end_W_1$Experiment_Date,na.rm = TRUE)
  start_DF_D_1 <-mean(start_D_1$Experiment_Date,na.rm = TRUE)
  end_DF_D_1 <-mean(end_D_1$Experiment_Date,na.rm = TRUE)
  
  #Change in trait
  z_1214[i,18] <- end_SLA_W_1 - start_SLA_W_1
  z_1214[i,19] <- end_SLA_D_1 - start_SLA_D_1
  z_1214[i,20] <- end_DF_W_1 - start_DF_W_1
  z_1214[i,21] <- end_DF_D_1 - start_DF_D_1
  
}

z_1214 <- z_1214 %>% mutate(abs_change_SLA_W = abs(change_SLA_W),
                            abs_change_SLA_D = abs(change_SLA_D),
                            abs_change_DF_W = abs(change_DF_W),
                            abs_change_DF_D = abs(change_DF_D))

# 2014-2016
for(i in 1:9){
  start_W_1 <- y9_1416 %>% filter(Site==sites_1416[i] & Year==2014 & Drought=="W") 
  end_W_1 <- y9_1416 %>% filter(Site==sites_1416[i] & Year==2016 & Drought=="W") 
  start_D_1 <- y9_1416 %>% filter(Site==sites_1416[i] & Year==2014 & Drought=="D") 
  end_D_1 <- y9_1416 %>% filter(Site==sites_1416[i] & Year==2016 & Drought=="D") 
  
  #SLA
  start_SLA_W_1 <-mean(start_W_1$SLA,na.rm = TRUE)
  end_SLA_W_1 <-mean(end_W_1$SLA,na.rm = TRUE)
  start_SLA_D_1 <-mean(start_D_1$SLA,na.rm = TRUE)
  end_SLA_D_1 <-mean(end_D_1$SLA,na.rm = TRUE)
  
  #Date of Flowering
  start_DF_W_1 <-mean(start_W_1$Experiment_Date,na.rm = TRUE)
  end_DF_W_1 <-mean(end_W_1$Experiment_Date,na.rm = TRUE)
  start_DF_D_1 <-mean(start_D_1$Experiment_Date,na.rm = TRUE)
  end_DF_D_1 <-mean(end_D_1$Experiment_Date,na.rm = TRUE)
  
  #Change in trait
  z_1416[i,18] <- end_SLA_W_1 - start_SLA_W_1
  z_1416[i,19] <- end_SLA_D_1 - start_SLA_D_1
  z_1416[i,20] <- end_DF_W_1 - start_DF_W_1
  z_1416[i,21] <- end_DF_D_1 - start_DF_D_1
  
}

z_1416 <- z_1416 %>% mutate(abs_change_SLA_W = abs(change_SLA_W),
                            abs_change_SLA_D = abs(change_SLA_D),
                            abs_change_DF_W = abs(change_DF_W),
                            abs_change_DF_D = abs(change_DF_D))


#Make Identifier
z_1012$time_range <- "2010_2012"
z_1113$time_range <- "2011_2013" 
z_1214$time_range <- "2012_2014"
z_1315$time_range <- "2013_2015"
z_1416$time_range <- "2014_2016"

z_multiple_time <- rbind(z_1012,z_1113,z_1214,z_1315,z_1416)

#Export data
#write_csv(z_multiple_time,"Data/z_multiple_time_5.csv")


# Mean * Autocorr
lm1 <- lm(abs_change_SLA_W~CMDA_auto*CMDA_mean,data=z_multiple_time)
lm2 <- lm(abs_change_SLA_D~CMDA_auto*CMDA_mean,data=z_multiple_time)
lm3 <- lm(abs_change_SLA_W~MAPA_auto*MAPA_mean,data=z_multiple_time)
lm4 <- lm(abs_change_SLA_D~MAPA_auto*MAPA_mean,data=z_multiple_time)
lm5 <- lm(abs_change_SLA_W~MATA_auto*MATA_mean,data=z_multiple_time)
lm6 <- lm(abs_change_SLA_D~MATA_auto*MATA_mean,data=z_multiple_time)

lm7 <- lm(abs_change_DF_W~CMDA_auto*CMDA_mean,data=z_multiple_time)
lm8 <- lm(abs_change_DF_D~CMDA_auto*CMDA_mean,data=z_multiple_time)
lm9 <- lm(abs_change_DF_W~MAPA_auto*MAPA_mean,data=z_multiple_time)
lm10 <- lm(abs_change_DF_D~MAPA_auto*MAPA_mean,data=z_multiple_time)
lm11 <- lm(abs_change_DF_W~MATA_auto*MATA_mean,data=z_multiple_time)
lm12 <- lm(abs_change_DF_D~MATA_auto*MATA_mean,data=z_multiple_time)

sm1 <- summary(lm1)
sm2 <- summary(lm2)
sm3 <- summary(lm3)
sm4 <- summary(lm4)
sm5 <- summary(lm5)
sm6 <- summary(lm6)

sm7 <- summary(lm7)
sm8 <- summary(lm8)
sm9 <- summary(lm9)
sm10 <- summary(lm10)
sm11 <- summary(lm11)
sm12 <- summary(lm12)

#No evidenced for interaction
sm1 
sm2 
sm3 
sm4 
sm5 
sm6 
sm7 
sm8 
sm9 
sm10 
sm11
sm12


# Main effects only
lm_main1 <- lm(abs_change_SLA_W~CMDA_auto+CMDA_mean,data=z_multiple_time)
lm_main2 <- lm(abs_change_SLA_D~CMDA_auto+CMDA_mean,data=z_multiple_time)
lm_main3 <- lm(abs_change_SLA_W~MAPA_auto+MAPA_mean,data=z_multiple_time)
lm_main4 <- lm(abs_change_SLA_D~MAPA_auto+MAPA_mean,data=z_multiple_time)
lm_main5 <- lm(abs_change_SLA_W~MATA_auto+MATA_mean,data=z_multiple_time)
lm_main6 <- lm(abs_change_SLA_D~MATA_auto+MATA_mean,data=z_multiple_time)

lm_main7 <- lm(abs_change_DF_W~CMDA_auto+CMDA_mean,data=z_multiple_time)
lm_main8 <- lm(abs_change_DF_D~CMDA_auto+CMDA_mean,data=z_multiple_time)
lm_main9 <- lm(abs_change_DF_W~MAPA_auto+MAPA_mean,data=z_multiple_time)
lm_main10 <- lm(abs_change_DF_D~MAPA_auto+MAPA_mean,data=z_multiple_time)
lm_main11 <- lm(abs_change_DF_W~MATA_auto+MATA_mean,data=z_multiple_time)
lm_main12 <- lm(abs_change_DF_D~MATA_auto+MATA_mean,data=z_multiple_time)

sm_main1 <- summary(lm_main1)
sm_main2 <- summary(lm_main2)
sm_main3 <- summary(lm_main3)
sm_main4 <- summary(lm_main4)
sm_main5 <- summary(lm_main5)
sm_main6 <- summary(lm_main6)

sm_main7 <- summary(lm_main7)
sm_main8 <- summary(lm_main8)
sm_main9 <- summary(lm_main9)
sm_main10 <- summary(lm_main10)
sm_main11 <- summary(lm_main11)
sm_main12 <- summary(lm_main12)

#No evidence for interaction
sm_main1 
sm_main2 
sm_main3 
sm_main4 
sm_main5 
sm_main6 
sm_main7 
sm_main8 
sm_main9 
sm_main10 
sm_main11
sm_main12  #Significant effect of MATA_autocorrelation


DF12<-ggplot(z_multiple_time, aes(x=MATA_auto, y=abs_change_DF_D)) +
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
DF12














