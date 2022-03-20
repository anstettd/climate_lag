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
z1 <- read.csv("Data/z1.csv", header=T)  #Basic Lat/Long plus headers

##########################################################################################################
#Import files

auto_table <- data.frame()

  for(i in 1:3){
    lm1 <- lm(abs_change_SLA_W~z1[,10+i],data=z1)
    lm2 <- lm(abs_change_SLA_D~z1[,10+i],data=z1)
    lm3 <- lm(abs_change_DF_W~z1[,10+i],data=z1)
    lm4 <- lm(abs_change_DF_D~z1[,10+i],data=z1)
    
    sm1 <- summary(lm1)
    sm2 <- summary(lm2)
    sm3 <- summary(lm3)
    sm4 <- summary(lm4)
    
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
  }
rownames(auto_table) <- c("CMDA","MAPA","MATA")
colnames(auto_table) <- c("slope","p-value","R2","slope","p-value","R2",
                          "slope","p-value","R2","slope","p-value","R2")
#3-row titles: SLA Wet, SLA Dry, DF Wet, DF Dry

write_csv(auto_table,"Tables/autocorr_stats.csv")
