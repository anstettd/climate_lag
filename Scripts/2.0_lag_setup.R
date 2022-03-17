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