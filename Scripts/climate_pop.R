#################
# Get weather data into Oct 1 to Sept 31 format
#################
# Also assessment of normality
library(tidyverse)



#Import data

weather <- read_csv("Data/weather.csv") %>% select(-Elevation)
climate <- read_csv("Data/climate.csv")
weather <- weather[,3:16]
climate <- climate[,2:8]
weather_input <- weather %>% select(ID,ID2,Latitude,Longitude,Year,MAT.weath,MAP.weath,CMD.weath)


weather10 <- weather %>% filter(Year==2010)
weatherlag1 <- weather10[,9:11]
weatherlag2 <- weather10[,12:14]


#Make Data frame for 2009 and 2008
weather2009 <- climate[,1:4]
weather2008 <- climate[,1:4]

weather2009$Year <- "2009"
weather2008$Year <- "2008"
  
weather9 <- cbind(weather2009,weatherlag1)
weather8 <- cbind(weather2008,weatherlag2)

colnames(weather9) <- c("ID","ID2","Latitude","Longitude","Year","MAT.weath","MAP.weath","CMD.weath")
colnames(weather8) <- c("ID","ID2","Latitude","Longitude","Year","MAT.weath","MAP.weath","CMD.weath")

weather_pop <-rbind(weather8,weather9,weather_input)

write.csv(weather_pop,'Data/weather_pop.csv') #Export file



