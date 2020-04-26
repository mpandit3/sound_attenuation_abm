#Compile weather data

library(suncalc)
library(lubridate)
library(ggplot2)
library(okmesonet)
library(plyr)
library(dplyr)
library(data.table)


### Weather patterns, using data from ERIC Station year 2011, 2015, 
##and 2019 on dates 6/22-6/26
years = c(2011,2015,2019)
wdsum_total = NULL

for(i in 1:length(years)){ #beginning of weather data loop
  if(i == 1){
    beginTime = "2011-06-22 00:00:00"
    endTime = "2011-06-26 23:55"
  } else if(i == 2){
    beginTime = "2015-06-22 00:00:00"
    endTime = "2015-06-26 23:55"
  } else if(i == 3){
    beginTime = "2019-06-22 00:00:00"
    endTime = "2019-06-26 23:55"
  }
  stid <- "ERIC"
  stations <- read.csv("/cloud/project/geoinfo.csv", stringsAsFactors = F)
  lat = stations$nlat[which(stations$stid == stid)]
  lon = stations$elon[which(stations$stid == stid)]
  
  #Obtain data from OK Mesonet website
  updatestn() #get latest information on mesonet stations
  okstations = updatestn() #save latest information into okstations
  
  w = NULL
  w <- okmts(begintime=beginTime,
             endtime=endTime, 
             variables = c("TAIR", "RELH","PRES"),
             station="eric",  
             localtime=F) #Need to download the data into UTC
  w$DT = as.POSIXct(w$TIME, tz = "UTC")
  w$DTL = w$DT #Saves datetime into a new vector for local datetime
  w$DATE = as.POSIXct(substr(w$TIME,0,10))
  w$YMD = w$DATE
  attributes(w$DTL)$tzone = "America/Chicago" #changes datetime to Central Time Zone
  w$YMDL <- as_date(w$DTL) #gives local (oklahoma) ymd date
  
  #Splitting w dataframe by date to calculate sunrise time
  
  w1 = w[1:288,]
  w2 = w[289:576,]
  w3 = w[577:864,]
  w4 = w[865:1152,]
  w5 = w[1153:1440,]
  
  dates<-list(w1,w2,w3,w4,w5)
  
  wd = NULL
  for(d in 1:length(dates)){
    weather = as.data.frame(dates[[d]])
    sr1 <- getSunlightTimes(date = as_date(weather$YMD[1]), lat = lat, lon = lon, tz = "UTC", keep = c("sunrise"))
    sr2 <- getSunlightTimes(date = as_date(weather$YMD[1]-86400), lat = lat, lon = lon, tz = "UTC", keep = c("sunrise"))
    sr1 <- sr1$sunrise
    sr2 <- sr2$sunrise
    weather$MAS <- ifelse(weather$DT<sr1, difftime(weather$DT, sr2, units = "mins"), difftime(weather$DT, sr1, units = "mins"))
    weather$MAS <- round(weather$MAS, 0)
    if(as.character(d) == as.character(dates[1])) {
      wd <- weather
    } else {
      wd <- rbind(wd,weather)
    }
    #print(paste0("Date Completed:", sep = " ", dates[d]))
  }
  
  labels <- seq(0,1435,5)
  wd$bins <- cut(wd$MAS,seq(0,1440,5), labels = labels, right = FALSE)               #make 5 minute bins
  wd$bins <- as.numeric(as.character(wd$bins))
  wd$DEW <- wd$TAIR-((100-wd$RELH)/5)
  #wd$futureTAIR = wd$TAIR+3
  
  wdsum <- data.frame(bin1 = wd$bins, 
                      bin2 = wd$bins+5,
                      date = wd$YMD, 
                      dateLocal = wd$YMDL,
                      DEW = wd$DEW,
                      TAIR = wd$TAIR,
                      #fTAIR = wd$futureTAIR, 
                      RELH = wd$RELH, 
                      PRES = wd$PRES)
  
  if(i == 1){
    wdsum$model = "hot"
  } else if(i == 2){
    wdsum$model = "normal"
  } else if(i == 3){
    wdsum$model = "cold"
  }
  #wdsum_total = rbind(wdsum2011,wdsum2015,wdsum2019)
  print(i) #prints year that is done getting data for

  if(as.character(i) == as.character(dates[1])) {
    wdsum_total <- wdsum
  } else {
    wdsum_total <- rbind(wdsum_total,wdsum)
  }
  #print(paste0("Year Completed:", sep = " ", years[i]))
  

} #end of weather data loop

#Use station ID to access folder and get a list of data files
setwd("/cloud/project/data/ERIC_weather_normal/")
fname = paste0(stid, "_weather_normal")
file_list <- as.list(list.files(fname))

wdsum = wdsum_total
save(wdsum, file = paste0(fname, ".Rdata"))


# # rain <- wd[wd$RAIN>0 & wd$TIME != 0,]
# # raindays <- unique(rain$YMD)
# # wd <- wd[!wd$YMD %in% raindays,]
# 
# #wd <- wd[wd$MAS<6*60,] #subset to sunrise data only

#Graph some data.
#Load wdsum data so you do not need to run code again:
load("/cloud/project/data/ERIC_weather_normal/ERIC_weather_normal.Rdata")
Song_volume = 85
Song_detection = 30
Song_freq = 7000
source("/cloud/project/src/Atmospheric_sound_attenuation.R")
wdsum$CallRad <- mapply(aud_range,Song_volume,Song_detection,Song_freq,wdsum$TAIR,wdsum$RELH,wdsum$PRES)
wdtemp <- wdsum[which(wdsum$bin1<=600),]
wdtemp$dateLocal <- as.factor(wdtemp$dateLocal )

ggplot( data = wdtemp, aes(x=bin1, y=CallRad, group=dateLocal, color = dateLocal)) +
  geom_line()+
  theme_classic()

ggplot( data = wdtemp, aes(x=bin1, y=TAIR, group=dateLocal, color = dateLocal)) +
  geom_line()+
  theme_classic()

ggplot( data = wdtemp, aes(x=bin1, y=DEW, group=dateLocal, color = dateLocal)) +
  geom_line() +
  theme_classic()+
  labs(x= "Min from Sunrise",
       y = "Dewpoint \nTemperature")

ggplot( data = wdtemp, aes(x=bin1, y=RELH, group=dateLocal, color = dateLocal)) +
  geom_line() +
  theme_classic()

ggplot( data = wdtemp, aes(x=DEW, y=CallRad, group=dateLocal, color = dateLocal)) +
  geom_line()

ggplot( data = wdtemp, aes(x=DEW, y=CallRad)) +
  geom_point()

ggplot( data = wdtemp, aes(x=TAIR, y=DEW, group=dateLocal, color = dateLocal)) +
  geom_line() +
  theme_classic()+
  labs(x= "Air Temperature (C)",
       y = "Dewpoint \nTemperature")

ggplot( data = wdtemp, aes(x=TAIR, y=RELH, group=dateLocal, color = dateLocal)) +
  geom_line() +
  theme_classic()+
  labs(x= "Air Temperature (C)",
       y = "Relative \nHumidity (%)")
