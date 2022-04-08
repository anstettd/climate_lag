##########################################################################################################
## Get p-values & R2 for basic lm graphs
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
#library(MASS)

#Import files
z_multiple_time <- read.csv("Data/z_multiple_time_5.csv", header=T)  #Basic Lat/Long plus headers

##########################################################################################################
#Import files

auto_table <- data.frame()

  for(i in 1:3){
    lm1 <- lm(abs_change_SLA_W~z_multiple_time[,10+i]+z_multiple_time[,14+i],data=z_multiple_time)
    lm2 <- lm(abs_change_SLA_D~z_multiple_time[,10+i]+z_multiple_time[,14+i],data=z_multiple_time)
    lm3 <- lm(abs_change_DF_W~z_multiple_time[,10+i]+z_multiple_time[,14+i],data=z_multiple_time)
    lm4 <- lm(abs_change_DF_D~z_multiple_time[,10+i]+z_multiple_time[,14+i],data=z_multiple_time)
    
    sm1 <- summary(lm1)
    sm2 <- summary(lm2)
    sm3 <- summary(lm3)
    sm4 <- summary(lm4)
    
    #Input autocorr
    auto_table[i,1] <- sm1$coefficients[2,1] #slope
    auto_table[i,2] <- sm1$coefficients[2,4] #p-value
    auto_table[i,3] <- sm1$r.squared #R2
    
    auto_table[i,4] <- sm2$coefficients[2,1] #slope
    auto_table[i,5] <- sm2$coefficients[2,4] #p-value
    auto_table[i,6] <- sm2$r.squared #R2
    
    auto_table[i,7] <- sm3$coefficients[2,1] #slope
    auto_table[i,8] <- sm3$coefficients[2,4] #p-value
    auto_table[i,9] <- sm3$r.squared #R2
    
    auto_table[i,10] <- sm4$coefficients[2,1] #slope
    auto_table[i,11] <- sm4$coefficients[2,4] #p-value
    auto_table[i,12] <- sm4$r.squared #R2
    
    #Input Climate Mean
    auto_table[3+i,1] <- sm1$coefficients[3,1] #slope
    auto_table[3+i,2] <- sm1$coefficients[3,4] #p-value
    auto_table[3+i,3] <- sm1$r.squared #R2
    
    auto_table[3+i,4] <- sm2$coefficients[3,1] #slope
    auto_table[3+i,5] <- sm2$coefficients[3,4] #p-value
    auto_table[3+i,6] <- sm2$r.squared #R2
    
    auto_table[3+i,7] <- sm3$coefficients[3,1] #slope
    auto_table[3+i,8] <- sm3$coefficients[3,4] #p-value
    auto_table[3+i,9] <- sm3$r.squared #R2
    
    auto_table[3+i,10] <- sm4$coefficients[3,1] #slope
    auto_table[3+i,11] <- sm4$coefficients[3,4] #p-value
    auto_table[3+i,12] <- sm4$r.squared #R2
    
  }
#rownames(auto_table) <- c("CMDA","MAPA","MATA","CMDA","MAPA","MATA")
colnames(auto_table) <- c("slope","p-value","R2","slope","p-value","R2",
                          "slope","p-value","R2","slope","p-value","R2")
#3-row titles: SLA Wet, SLA Dry, DF Wet, DF Dry

write_csv(auto_table,"Tables/autocorr_stats_5.csv")
