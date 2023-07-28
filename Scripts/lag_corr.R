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
library(Hmisc)

#Import data
wna_all <- read_csv("Data/wna_all.csv")

#Define regions
wna_all<-wna_all %>% mutate(Region = ifelse(Latitude >= 40, "North", 
                                  ifelse((Latitude >35) & (Latitude <40), "Center","South")))


MAPA_all <- wna_all %>% select(MAPA_lag0:MAPA_lag012)
MATA_all <- wna_all %>% select(MATA_lag0:MATA_lag012)
CMDA_all <- wna_all %>% select(CMDA_lag0:CMDA_lag012)
SPEI_all <- wna_all %>% select(SPEI_lag0:SPEI_lag012)

rcorr(as.matrix(MATA_all))
rcorr(as.matrix(MAPA_all))
rcorr(as.matrix(CMDA_all))
rcorr(as.matrix(SPEI_all))

#Regions
wna_north <- wna_all %>% filter(Region=="North")
wna_center <- wna_all %>% filter(Region=="Center")
wna_south <- wna_all %>% filter(Region=="South")


#North 
MAPA_north <- wna_north %>% select(MAPA_lag0:MAPA_lag012)
MATA_north <- wna_north %>% select(MATA_lag0:MATA_lag012)
CMDA_north <- wna_north %>% select(CMDA_lag0:CMDA_lag012)
SPEI_north <- wna_north %>% select(SPEI_lag0:SPEI_lag012)

#center 
MAPA_center <- wna_center %>% select(MAPA_lag0:MAPA_lag012)
MATA_center <- wna_center %>% select(MATA_lag0:MATA_lag012)
CMDA_center <- wna_center %>% select(CMDA_lag0:CMDA_lag012)
SPEI_center <- wna_center %>% select(SPEI_lag0:SPEI_lag012)

#south 
MAPA_south <- wna_south %>% select(MAPA_lag0:MAPA_lag012)
MATA_south <- wna_south %>% select(MATA_lag0:MATA_lag012)
CMDA_south <- wna_south %>% select(CMDA_lag0:CMDA_lag012)
SPEI_south <- wna_south %>% select(SPEI_lag0:SPEI_lag012)

#MATA
rcorr(as.matrix(MATA_north))
rcorr(as.matrix(MATA_center))
rcorr(as.matrix(MATA_south))

#MAPA
rcorr(as.matrix(MAPA_north))
rcorr(as.matrix(MAPA_center))
rcorr(as.matrix(MAPA_south))

#CMDA
rcorr(as.matrix(CMDA_north))
rcorr(as.matrix(CMDA_center))
rcorr(as.matrix(CMDA_south))

#SPEI
rcorr(as.matrix(SPEI_north))
rcorr(as.matrix(SPEI_center))
rcorr(as.matrix(SPEI_south))


