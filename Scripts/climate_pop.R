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


#log MAPA
weather_pop$MAP.weath <- log(weather_pop$MAP.weath)
climate$MAP <- log(climate$MAP)

w_temp_2008 <- weather_pop %>% filter(Year==2008) %>% select("MAT.weath","MAP.weath","CMD.weath")
w_temp_2009 <- weather_pop %>% filter(Year==2009) %>% select("MAT.weath","MAP.weath","CMD.weath")
w_temp_2010 <- weather_pop %>% filter(Year==2010) %>% select("MAT.weath","MAP.weath","CMD.weath")
w_temp_2011 <- weather_pop %>% filter(Year==2011) %>% select("MAT.weath","MAP.weath","CMD.weath")
w_temp_2012 <- weather_pop %>% filter(Year==2012) %>% select("MAT.weath","MAP.weath","CMD.weath")
w_temp_2013 <- weather_pop %>% filter(Year==2013) %>% select("MAT.weath","MAP.weath","CMD.weath")
w_temp_2014 <- weather_pop %>% filter(Year==2014) %>% select("MAT.weath","MAP.weath","CMD.weath")
w_temp_2015 <- weather_pop %>% filter(Year==2015) %>% select("MAT.weath","MAP.weath","CMD.weath")
w_temp_2016 <- weather_pop %>% filter(Year==2016) %>% select("MAT.weath","MAP.weath","CMD.weath")


anom2008 <- w_temp_2008 - climate[,5:7]
anom2009 <- w_temp_2009 - climate[,5:7]
anom2010 <- w_temp_2010 - climate[,5:7]
anom2011 <- w_temp_2011 - climate[,5:7]
anom2012 <- w_temp_2012 - climate[,5:7]
anom2013 <- w_temp_2013 - climate[,5:7]
anom2014 <- w_temp_2014 - climate[,5:7]
anom2015 <- w_temp_2015 - climate[,5:7]
anom2016 <- w_temp_2016 - climate[,5:7]

anom <- rbind(anom2008,anom2009,anom2010,anom2011,anom2012,anom2013,anom2014,anom2015,anom2016)
colnames(anom) <- c("MATA","MAPA","CMDA")

final_df <- cbind(weather_pop,anom) %>% select(-MAT.weath,-MAP.weath,-CMD.weath)

write.csv(final_df,'Data/pop_clim_final.csv') #Export file

