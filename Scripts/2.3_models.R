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
#Interative models
lm1 <- lm(abs_change_SLA_W~SPEI_auto*SPEI_mean,data=z1)
lm2 <- lm(abs_change_SLA_D~CMDA_auto*CMDA_mean,data=z1)
lm3 <- lm(abs_change_DF_W~MAPA_auto*MAPA_mean,data=z1)
lm4 <- lm(abs_change_DF_D~MATA_auto*MATA_mean,data=z1)

sm1 <- summary(lm1)
sm2 <- summary(lm2)
sm3 <- summary(lm3)
sm4 <- summary(lm4)

#No evidenced for interaction
sm1 
sm2 
sm3 
sm4 

#Additive models
lm5 <- lm(abs_change_SLA_W~SPEI_auto+SPEI_mean,data=z1)
lm6 <- lm(abs_change_SLA_D~CMDA_auto+CMDA_mean,data=z1)
lm7 <- lm(abs_change_DF_W~MAPA_auto+MAPA_mean,data=z1)
lm8 <- lm(abs_change_DF_D~MATA_auto+MATA_mean,data=z1)

sm5 <- summary(lm5)
sm6 <- summary(lm6)
sm7 <- summary(lm7)
sm8 <- summary(lm8)

#No evidenced for interaction
sm5 
sm6 
sm7 
sm8 


#Mean only
lm9 <- lm(abs_change_SLA_W~SPEI_mean,data=z1)
lm10 <- lm(abs_change_SLA_D~CMDA_mean,data=z1)
lm11 <- lm(abs_change_DF_W~MAPA_mean,data=z1)
lm12 <- lm(abs_change_DF_D~MATA_mean,data=z1)

sm9 <- summary(lm5)
sm10 <- summary(lm6)
sm11 <- summary(lm7)
sm12 <- summary(lm8)

#No evidenced for interaction
sm9 
sm10 
sm11 
sm12



