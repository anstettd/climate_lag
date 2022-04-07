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
# Mean * Autocorr
lm1 <- lm(abs_change_SLA_W~CMDA_auto*CMDA_mean,data=z1)
lm2 <- lm(abs_change_SLA_D~CMDA_auto*CMDA_mean,data=z1)
lm3 <- lm(abs_change_SLA_W~MAPA_auto*MAPA_mean,data=z1)
lm4 <- lm(abs_change_SLA_D~MAPA_auto*MAPA_mean,data=z1)
lm5 <- lm(abs_change_SLA_W~MATA_auto*MATA_mean,data=z1)
lm6 <- lm(abs_change_SLA_D~MATA_auto*MATA_mean,data=z1)

lm7 <- lm(abs_change_DF_W~CMDA_auto*CMDA_mean,data=z1)
lm8 <- lm(abs_change_DF_D~CMDA_auto*CMDA_mean,data=z1)
lm9 <- lm(abs_change_DF_W~MAPA_auto*MAPA_mean,data=z1)
lm10 <- lm(abs_change_DF_D~MAPA_auto*MAPA_mean,data=z1)
lm11 <- lm(abs_change_DF_W~MATA_auto*MATA_mean,data=z1)
lm12 <- lm(abs_change_DF_D~MATA_auto*MATA_mean,data=z1)

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
lm_main1 <- lm(abs_change_SLA_W~CMDA_auto+CMDA_mean,data=z1)
lm_main2 <- lm(abs_change_SLA_D~CMDA_auto+CMDA_mean,data=z1)
lm_main3 <- lm(abs_change_SLA_W~MAPA_auto+MAPA_mean,data=z1)
lm_main4 <- lm(abs_change_SLA_D~MAPA_auto+MAPA_mean,data=z1)
lm_main5 <- lm(abs_change_SLA_W~MATA_auto+MATA_mean,data=z1)
lm_main6 <- lm(abs_change_SLA_D~MATA_auto+MATA_mean,data=z1)

lm_main7 <- lm(abs_change_DF_W~CMDA_auto+CMDA_mean,data=z1)
lm_main8 <- lm(abs_change_DF_D~CMDA_auto+CMDA_mean,data=z1)
lm_main9 <- lm(abs_change_DF_W~MAPA_auto+MAPA_mean,data=z1)
lm_main10 <- lm(abs_change_DF_D~MAPA_auto+MAPA_mean,data=z1)
lm_main11 <- lm(abs_change_DF_W~MATA_auto+MATA_mean,data=z1)
lm_main12 <- lm(abs_change_DF_D~MATA_auto+MATA_mean,data=z1)

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
sm_main4 #marginally significant main effect of MAPA_auto
sm_main5 
sm_main6 
sm_main7 
sm_main8 
sm_main9 
sm_main10 
sm_main11
sm_main12