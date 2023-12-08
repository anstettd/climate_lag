##########################################################################################################
## Test for MAT, MAP & CMD associations with SLA & Date of FLowering
## Make data frame for conceptual figure 
## Author Haley Branch
## 
##
## Last Modified April 17, 2022
##########################################################################################################
##########################################################################################################
# Clear environment
rm(list = ls())

#Import libraries
library(tidyverse)


#Import datasets and add year_actual variable
weather_1979 <- read.csv("Climate/timeseries_monthly_1979.csv", header=T)
weather_1980 <- read.csv("Climate/timeseries_monthly_1980.csv", header=T)
weather_1981 <- read.csv("Climate/timeseries_monthly_1981.csv", header=T)
weather_1982 <- read.csv("Climate/timeseries_monthly_1982.csv", header=T)
weather_1983 <- read.csv("Climate/timeseries_monthly_1983.csv", header=T)
weather_1984 <- read.csv("Climate/timeseries_monthly_1984.csv", header=T)
weather_1985 <- read.csv("Climate/timeseries_monthly_1985.csv", header=T)
weather_1986 <- read.csv("Climate/timeseries_monthly_1986.csv", header=T)
weather_1987 <- read.csv("Climate/timeseries_monthly_1987.csv", header=T)
weather_1988 <- read.csv("Climate/timeseries_monthly_1988.csv", header=T)
weather_1989 <- read.csv("Climate/timeseries_monthly_1989.csv", header=T)
weather_1990 <- read.csv("Climate/timeseries_monthly_1990.csv", header=T)
weather_1991 <- read.csv("Climate/timeseries_monthly_1991.csv", header=T)
weather_1992 <- read.csv("Climate/timeseries_monthly_1992.csv", header=T)
weather_1993 <- read.csv("Climate/timeseries_monthly_1993.csv", header=T)
weather_1994 <- read.csv("Climate/timeseries_monthly_1994.csv", header=T)
weather_1995 <- read.csv("Climate/timeseries_monthly_1995.csv", header=T)
weather_1996 <- read.csv("Climate/timeseries_monthly_1996.csv", header=T)
weather_1997 <- read.csv("Climate/timeseries_monthly_1997.csv", header=T)
weather_1998 <- read.csv("Climate/timeseries_monthly_1998.csv", header=T)
weather_1999 <- read.csv("Climate/timeseries_monthly_1999.csv", header=T)
weather_2000 <- read.csv("Climate/timeseries_monthly_2000.csv", header=T)
weather_2001 <- read.csv("Climate/timeseries_monthly_2001.csv", header=T)
weather_2002 <- read.csv("Climate/timeseries_monthly_2002.csv", header=T)
weather_2003 <- read.csv("Climate/timeseries_monthly_2003.csv", header=T)
weather_2004 <- read.csv("Climate/timeseries_monthly_2004.csv", header=T)
weather_2005 <- read.csv("Climate/timeseries_monthly_2005.csv", header=T)
weather_2006 <- read.csv("Climate/timeseries_monthly_2006.csv", header=T)
weather_2007 <- read.csv("Climate/timeseries_monthly_2007.csv", header=T)
weather_2008 <- read.csv("Climate/timeseries_monthly_2008.csv", header=T)
weather_2009 <- read.csv("Climate/timeseries_monthly_2009.csv", header=T)



#weather_1979$MAPavg <- rowSums(weather_1979[ , c(42,53)], na.rm=TRUE)
#weather_1979$stder <- std.error(weather_1979[ , c(42,53)])

weather_1980$MAPavg <- rowSums(weather_1980[ , c(42,53)], na.rm=TRUE)
weather_1981$MAPavg <- rowSums(weather_1981[ , c(42,53)], na.rm=TRUE)
weather_1982$MAPavg <- rowSums(weather_1982[ , c(42,53)], na.rm=TRUE)
weather_1983$MAPavg <- rowSums(weather_1983[ , c(42,53)], na.rm=TRUE)
weather_1984$MAPavg <- rowSums(weather_1984[ , c(42,53)], na.rm=TRUE)
weather_1985$MAPavg <- rowSums(weather_1985[ , c(42,53)], na.rm=TRUE)
weather_1986$MAPavg <- rowSums(weather_1986[ , c(42,53)], na.rm=TRUE)
weather_1987$MAPavg <- rowSums(weather_1987[ , c(42,53)], na.rm=TRUE)
weather_1988$MAPavg <- rowSums(weather_1988[ , c(42,53)], na.rm=TRUE)
weather_1989$MAPavg <- rowSums(weather_1989[ , c(42,53)], na.rm=TRUE)
weather_1990$MAPavg <- rowSums(weather_1990[ , c(42,53)], na.rm=TRUE)
weather_1991$MAPavg <- rowSums(weather_1991[ , c(42,53)], na.rm=TRUE)
weather_1992$MAPavg <- rowSums(weather_1992[ , c(42,53)], na.rm=TRUE)
weather_1993$MAPavg <- rowSums(weather_1993[ , c(42,53)], na.rm=TRUE)
weather_1994$MAPavg <- rowSums(weather_1994[ , c(42,53)], na.rm=TRUE)
weather_1995$MAPavg <- rowSums(weather_1995[ , c(42,53)], na.rm=TRUE)
weather_1996$MAPavg <- rowSums(weather_1996[ , c(42,53)], na.rm=TRUE)
weather_1997$MAPavg <- rowSums(weather_1997[ , c(42,53)], na.rm=TRUE)
weather_1998$MAPavg <- rowSums(weather_1998[ , c(42,53)], na.rm=TRUE)
weather_1999$MAPavg <- rowSums(weather_1999[ , c(42,53)], na.rm=TRUE)
weather_2000$MAPavg <- rowSums(weather_2000[ , c(42,53)], na.rm=TRUE)
weather_2001$MAPavg <- rowSums(weather_2001[ , c(42,53)], na.rm=TRUE)
weather_2002$MAPavg <- rowSums(weather_2002[ , c(42,53)], na.rm=TRUE)
weather_2003$MAPavg <- rowSums(weather_2003[ , c(42,53)], na.rm=TRUE)
weather_2004$MAPavg <- rowSums(weather_2004[ , c(42,53)], na.rm=TRUE)
weather_2005$MAPavg <- rowSums(weather_2005[ , c(42,53)], na.rm=TRUE)
weather_2006$MAPavg <- rowSums(weather_2006[ , c(42,53)], na.rm=TRUE)
weather_2007$MAPavg <- rowSums(weather_2007[ , c(42,53)], na.rm=TRUE)
weather_2008$MAPavg <- rowSums(weather_2008[ , c(42,53)], na.rm=TRUE)
weather_2009$MAPavg <- rowSums(weather_2009[ , c(42,53)], na.rm=TRUE)

#w1 <- subset(weather_1979, select=c(ID, ID2, Latitude, MAPavg))
w2 <- subset(weather_1980, select=c(ID, ID2, Latitude, MAPavg))
w3 <- subset(weather_1981, select=c(ID, ID2, Latitude, MAPavg))
w4 <- subset(weather_1982, select=c(ID, ID2, Latitude, MAPavg))
w5 <- subset(weather_1983, select=c(ID, ID2, Latitude, MAPavg))
w6 <- subset(weather_1984, select=c(ID, ID2, Latitude, MAPavg))
w7 <- subset(weather_1985, select=c(ID, ID2, Latitude, MAPavg))
w8 <- subset(weather_1986, select=c(ID, ID2, Latitude, MAPavg))
w9 <- subset(weather_1987, select=c(ID, ID2, Latitude, MAPavg))
w10 <- subset(weather_1988, select=c(ID, ID2, Latitude, MAPavg))
w11 <- subset(weather_1989, select=c(ID, ID2, Latitude, MAPavg))
w13 <- subset(weather_1990, select=c(ID, ID2, Latitude, MAPavg))
w14 <- subset(weather_1991, select=c(ID, ID2, Latitude, MAPavg))
w15 <- subset(weather_1992, select=c(ID, ID2, Latitude, MAPavg))
w16 <- subset(weather_1993, select=c(ID, ID2, Latitude, MAPavg))
w17 <- subset(weather_1994, select=c(ID, ID2, Latitude, MAPavg))
w18 <- subset(weather_1995, select=c(ID, ID2, Latitude, MAPavg))
w19 <- subset(weather_1996, select=c(ID, ID2, Latitude, MAPavg))
w20 <- subset(weather_1997, select=c(ID, ID2, Latitude, MAPavg))
w21 <- subset(weather_1998, select=c(ID, ID2, Latitude, MAPavg))
w22 <- subset(weather_1999, select=c(ID, ID2, Latitude, MAPavg))
w23 <- subset(weather_2000, select=c(ID, ID2, Latitude, MAPavg))
w24 <- subset(weather_2001, select=c(ID, ID2, Latitude, MAPavg))
w25 <- subset(weather_2002, select=c(ID, ID2, Latitude, MAPavg))
w26 <- subset(weather_2003, select=c(ID, ID2, Latitude, MAPavg))
w27 <- subset(weather_2004, select=c(ID, ID2, Latitude, MAPavg))
w28 <- subset(weather_2005, select=c(ID, ID2, Latitude, MAPavg))
w29 <- subset(weather_2006, select=c(ID, ID2, Latitude, MAPavg))
w30 <- subset(weather_2007, select=c(ID, ID2, Latitude, MAPavg))
w31 <- subset(weather_2008, select=c(ID, ID2, Latitude, MAPavg))
w32 <- subset(weather_2009, select=c(ID, ID2, Latitude, MAPavg))

all<-rbind(w2,w3,w4,w5,w7,w7,w8,w9,w10,w11,w13,w14,w15,w16,w17,w18,w19,w20,w21,
      w23,w24,w25,w26,w27,w28,w29,w30,w31,w32)

all <- filter(all, ID %in%  c("S02", "S10", "S36"))

# Make dataframes for each population
south <- all %>% filter(ID=="S02")
centre <- all %>% filter(ID=="S10")
north <- all %>% filter(ID=="S36")

#Make DF
mean_ci_30y <- data.frame()

#Mean
mean_ci_30y[1,1] <- mean(north$MAPavg)
mean_ci_30y[2,1] <- mean(centre$MAPavg)
mean_ci_30y[3,1] <- mean(south$MAPavg)

#Error
north_error <- qt(0.975, df=30-1)*sd(north$MAPavg)/sqrt(30)
centre_error <- qt(0.975, df=30-1)*sd(centre$MAPavg)/sqrt(30)
south_error <- qt(0.975, df=30-1)*sd(south$MAPavg)/sqrt(30)

#Upper CI
mean_ci_30y[1,2] <- mean(north$MAPavg) + north_error
mean_ci_30y[2,2] <- mean(centre$MAPavg) + centre_error
mean_ci_30y[3,2] <- mean(south$MAPavg) + south_error

#Lower CI
mean_ci_30y[1,3] <- mean(north$MAPavg) - north_error
mean_ci_30y[2,3] <- mean(centre$MAPavg) - centre_error
mean_ci_30y[3,3] <- mean(south$MAPavg) - south_error

#Dummy variables
mean_ci_30y[1,4] <- "a"
mean_ci_30y[2,4] <- "b"
mean_ci_30y[3,4] <- "c"

mean_ci_30y[,4] <- as.factor(mean_ci_30y[,4])

colnames(mean_ci_30y) <- c("Mean","Upper_CI","Lower_CI","dummy") 
rownames(mean_ci_30y) <- c("North","Centre","South") 

write.csv(mean_ci_30y,"Data/mean_ci_30y.csv")












#mean_all <- all %>% group_by(ID) %>% mean(MAPavg)
#Stats <- all %>% group_by(ID) %>% summarize(by="ID",Mean = mean(MAPavg)), SD = sd(MAPavg),
#                                                                          CI_L = Mean - (SD * 1.96)/sqrt(30),
#                                                                          CI_U = Mean + (SD * 1.96)/sqrt(30))







###############################################################################################################
#Wrong calculation
wna1 <- read_csv("Climate/timeseries_lat_2010-2016.csv")
impact_all <- wna1 %>% select(ID_Year1,Year,Elevation) #Get relevant meta_data

#Import datasets and add year_actual variable
weather_2007 <- read.csv("Climate/timeseries_monthly_2007.csv", header=T)
weather_2008 <- read.csv("Climate/timeseries_monthly_2008.csv", header=T)
weather_2009 <- read.csv("Climate/timeseries_monthly_2009.csv", header=T)
weather_2010 <- read.csv("Climate/timeseries_monthly_2010.csv", header=T)
weather_2011 <- read.csv("Climate/timeseries_monthly_2011.csv", header=T)
weather_2012 <- read.csv("Climate/timeseries_monthly_2012.csv", header=T)
weather_2013 <- read.csv("Climate/timeseries_monthly_2013.csv", header=T)
weather_2014 <- read.csv("Climate/timeseries_monthly_2014.csv", header=T)
weather_2015 <- read.csv("Climate/timeseries_monthly_2015.csv", header=T)
weather_2016 <- read.csv("Climate/timeseries_monthly_2016.csv", header=T)

weather_2007$MAPavg <- rowMeans(weather_2007[ , c(42,53)], na.rm=TRUE)
weather_2008$MAPavg <- rowMeans(weather_2008[ , c(42,53)], na.rm=TRUE)
weather_2009$MAPavg <- rowMeans(weather_2009[ , c(42,53)], na.rm=TRUE)
weather_2010$MAPavg <- rowMeans(weather_2010[ , c(42,53)], na.rm=TRUE)
weather_2011$MAPavg <- rowMeans(weather_2011[ , c(42,53)], na.rm=TRUE)
weather_2012$MAPavg <- rowMeans(weather_2012[ , c(42,53)], na.rm=TRUE)
weather_2013$MAPavg <- rowMeans(weather_2013[ , c(42,53)], na.rm=TRUE)
weather_2014$MAPavg <- rowMeans(weather_2014[ , c(42,53)], na.rm=TRUE)
weather_2015$MAPavg <- rowMeans(weather_2015[ , c(42,53)], na.rm=TRUE)
weather_2016$MAPavg <- rowMeans(weather_2016[ , c(42,53)], na.rm=TRUE)

weather_2007 <- subset(weather_2007, select=c(ID, ID2, Latitude, MAPavg))
weather_2008 <- subset(weather_2008, select=c(ID, ID2, Latitude, MAPavg))
weather_2009 <- subset(weather_2009, select=c(ID, ID2, Latitude, MAPavg))
weather_2010 <- subset(weather_2010, select=c(ID, ID2, Latitude, MAPavg))
weather_2011 <- subset(weather_2011, select=c(ID, ID2, Latitude, MAPavg))
weather_2012 <- subset(weather_2012, select=c(ID, ID2, Latitude, MAPavg))
weather_2013 <- subset(weather_2013, select=c(ID, ID2, Latitude, MAPavg))
weather_2014 <- subset(weather_2014, select=c(ID, ID2, Latitude, MAPavg))
weather_2015 <- subset(weather_2015, select=c(ID, ID2, Latitude, MAPavg))
weather_2016 <- subset(weather_2016, select=c(ID, ID2, Latitude, MAPavg))

weather_2007$Year <- 2007
weather_2008$Year <- 2008
weather_2009$Year <- 2009
weather_2010$Year <- 2010
weather_2011$Year <- 2011
weather_2012$Year <- 2012
weather_2013$Year <- 2013
weather_2014$Year <- 2014
weather_2015$Year <- 2015
weather_2016$Year <- 2016

weather_all <- rbind(weather_2007,weather_2008,weather_2009,weather_2010,weather_2011,weather_2012,weather_2013,weather_2014,weather_2015,weather_2016)
weather_all <- filter(weather_all, ID %in%  c("S02", "S10", "S36"))
#write.csv(weather_all, "Data/weather_3_fig1.csv")
#weather_all<-read.csv("Data/weather_3_fig1.csv")


#Trend line
graph <- ggplot(weather_all, aes(Year, y=MAPavg, fill=ID, colour=ID))+
  geom_line(aes(colour=ID))+
  xlab("Year") +
  scale_y_continuous(name="MAP")+
  scale_color_manual(values= c("S02"="#FF3333", "S10"="#FFCC00", "S36"="#3399FF"))+
  scale_fill_manual(values= c("S02"="#FF3333", "S10"="#FFCC00", "S36"="#3399FF"))+
  ylim(0,410)+
  xlim(2004,2016)+
  theme_classic()
graph <-graph + theme(
  axis.text.x = element_text(size=12, face="bold", angle=0,hjust=0.5),
  axis.text.y = element_text(size=15,face="bold"),
  axis.title.x = element_text(color="black", size=20, vjust = 0.5, face="bold"),
  axis.title.y = element_text(color="black", size=20,vjust = 2, face="bold",hjust=0.5))
graph <-graph +
  theme(legend.title = element_blank(),legend.text = element_text(size=12,face="bold"),
        strip.background = element_blank(), strip.text.x=element_text(size=14,face="bold",hjust=0.05,vjust=-1.2))
graph





