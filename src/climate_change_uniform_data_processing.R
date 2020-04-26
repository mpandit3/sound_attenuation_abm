#Compile weather data

library(suncalc)
library(lubridate)
library(ggplot2)
library(okmesonet)
library(dplyr)
library(data.table)


### Weather patterns, using data from ERIC Station year 2011, 2015, 
##and 2019 on dates 6/22-6/26
years = c(2011,2015,2019)
wdsum_total = NULL

for(i in 1:length(years)){
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
                      #TAIR = wd$TAIR,
                      fTAIR = wd$futureTAIR, 
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
  
  
}

#Use station ID to access folder and get a list of data files
#setwd("/cloud/project/data/ERIC_weather_normal/")
setwd("/cloud/project/data/ERIC_weather_uniform_cc/")

#fname = paste0(stid, "_weather_normal")
fname = paste0(stid, "_weather_uniform_cc")

#file_list <- as.list(list.files(fname))

wdsum = wdsum_total
save(wdsum, file = paste0(fname, ".Rdata"))

# write.csv(w[1:288,], file = "/cloud/project/data/ERIC_weather_cc_uniform/20190622.csv")
# write.csv(w[289:576,], file = "/cloud/project/data/ERIC_weather_cc_uniform/20190623.csv")
# write.csv(w[577:864,], file = "/cloud/project/data/ERIC_weather_cc_uniform/20190624.csv")
# write.csv(w[865:1152,], file = "/cloud/project/data/ERIC_weather_cc_uniform/20190625.csv")
# write.csv(w[1153:1440,], file = "/cloud/project/data/ERIC_weather_cc_uniform/20190626.csv")
# 
# 
# #Use station ID to access folder and get a list of data files
# fname = paste0(stid, "_weather_2019")
# file_list <- as.list(list.files(fname))
# 
# #Use station ID to access folder and get a list of data files
# # fname = paste0(stid, "_weather_2018")
# # file_list <- as.list(list.files(fname))
# 
# for(i in file_list){
#   #w <- read.csv(paste0(fname,"/",i), skip=2, header=T, row.names = NULL)
#   w <- read.csv(paste0(fname,"/",i), skip=0, header=T, row.names = NULL)
#   w$filename = i  #add filename to data frame
#   w$year = as.numeric(2080)
#   #w$DATETIME = as.POSIXct(w$TIME) #Renaming time column
#   #w$TIME = strftime(w$DATETIME, format="%H:%M:%S")
#   # w$YMD <- as_date(w$TIME)
#   # w$YMD = as.POSIXct(w$YMD)
#   #w$DT = as.POSIXct((w$DT), tz = "UTC")
#   w$year = as.numeric(substr(i,start = 1, stop = 4)) #add year to data frame
#   w$month = as.numeric(substr(i,start = 5, stop = 6))  #add month to data frame
#   w$day = as.numeric(substr(i,start = 7, stop = 8))  #add day of month to data frame
#   w$DT = as.POSIXct(w$TIME, tz = "UTC")
#   #w$DT = as.POSIXct(paste0(w$year,"-",w$month,"-",w$day," 00:00:00"), tz = "UTC") + w$TIME*60
#   w$YMD = as.POSIXct(paste(w$year,w$month,w$day, sep ="-"), tz = "UTC")
#   #w$YMD = as.POSIXct.
#   w$DTL = w$DT #Saves datetime into a new vector for local datetime
#   attributes(w$DTL)$tzone = "America/Chicago" #changes datetime to Central Time Zone
#   w$YMDL <- as_date(w$DTL)
#   sr1 <- getSunlightTimes(date = as_date(w$YMD[1]), lat = lat, lon = lon, tz = "UTC", keep = c("sunrise"))
#   sr2 <- getSunlightTimes(date = as_date(w$YMD[1]-86400), lat = lat, lon = lon, tz = "UTC", keep = c("sunrise"))
#   sr1 <- sr1$sunrise
#   sr2 <- sr2$sunrise
#   w$MAS <- ifelse(w$DT<sr1, difftime(w$DT, sr2, units = "mins"), difftime(w$DT, sr1, units = "mins"))
#   w$MAS <- round(w$MAS, 0)
#   if(as.character(i) == as.character(file_list[1])) {
#      wd <- w
#   } else {
#     wd <- rbind(wd,w)
#   }
#   print(i)
#   
#   
# }
# 
# # rain <- wd[wd$RAIN>0 & wd$TIME != 0,]
# # raindays <- unique(rain$YMD)
# # wd <- wd[!wd$YMD %in% raindays,]
# 
# #wd <- wd[wd$MAS<6*60,] #subset to sunrise data only
# 
# #Assign sunrise dates
# 
# labels <- seq(0,1435,5)
# wd$bins <- cut(wd$MAS,seq(0,1440,5), labels = labels, right = FALSE)               #make 5 minute bins
# wd$bins <- as.numeric(as.character(wd$bins))
# wd$DEW <- wd$TAIR-((100-wd$RELH)/5)
# wd$futureTAIR = wd$TAIR+3
# 
# attach(wd)
# 
# wdsum <- data.frame(bin1 = wd$bins, 
#                     bin2 = wd$bins+5,
#                     date = wd$YMD, 
#                     dateLocal = wd$YMDL,
#                     DEW = wd$DEW,
#                     TAIR = wd$TAIR, 
#                     RELH = wd$RELH, 
#                     PRES = wd$PRES)          
# 
# setwd("/cloud/project/ERIC_weather_cc_uniform")
# save(wdsum, file = paste0(fname, ".Rdata"))

#Graph some data.
Song_volume = 85
Song_detection = 30
Song_freq = 7000
source("/cloud/project/Atmospheric_sound_attenuation.R")
wdsum$CallRad <- mapply(aud_range,Song_volume,Song_detection,Song_freq,wdsum$TAIR,wdsum$RELH,wdsum$PRES)
wdtemp <- wdsum[which(wdsum$bin1<=600),]
wdtemp$dateLocal <- as.factor(wdtemp$dateLocal )

ggplot( data = wdtemp, aes(x=bin1, y=CallRad, group=dateLocal, color = dateLocal)) +
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
