##########################################################################################################
## Correlate climate
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
library(Hmisc)
##########################################################################################################
#Import files

env_lag0 <- read.csv("Data/env_lag0.csv", header=T) #site/year climate data
env_corr <- as.matrix(env_lag0[,9:12])

rcorr(env_corr)



