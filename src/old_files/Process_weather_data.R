#Compile weather data

library(suncalc)
library(geonames)
library(lubridate)

#Provide location latitude and longitude
lat = 36.4336 #woodward
lon = -99.3904 #woodward

file_list <- as.list(list.files("Weather_data"))


for(i in file_list){
  w <- read.table(paste0("Weather_data/",i), skip=2, header=T, row.names = NULL)
  w$filename = i  #add filename to data frame
  w$year = as.numeric(substr(i,start = 1, stop = 4)) #add year to data frame
  w$month = as.numeric(substr(i,start = 5, stop = 6))  #add month to data frame
  w$day = as.numeric(substr(i,start = 7, stop = 8))  #add day of month to data frame
  w$YMD = as.POSIXct(paste(w$year,w$month,w$day, sep ="-"), tz = "UTC")
  w$DT = as.POSIXct(paste0(w$year,"-",w$month,"-",w$day," 00:00:00"), tz = "UTC") + w$TIME*60
  sr1 <- getSunlightTimes(date = as_date(w$YMD[1]), lat = lat, lon = lon, tz = "UTC", keep = c("sunrise"))
  sr2 <- getSunlightTimes(date = as_date(w$YMD[1]-86400), lat = lat, lon = lon, tz = "UTC", keep = c("sunrise"))
  sr1 <- sr1$sunrise
  sr2 <- sr2$sunrise
  w$MAS <- ifelse(w$DT<sr1, difftime(w$DT, sr2, units = "mins"), difftime(w$DT, sr1, units = "mins"))
  w$MAS <- round(w$MAS, 0)
  if(as.character(i) == as.character(file_list[1])) {
    wd <- w
  } else {
    wd <- rbind(wd,w) 
  }
  print(i)
}

rain <- wd[wd$RAIN>0 & wd$TIME != 0,]
raindays <- unique(rain$YMD)
wd <- wd[!wd$YMD %in% raindays,]

#wd <- wd[wd$MAS<6*60,] #subset to sunrise data only
plot(wd$MAS,wd$TAIR)
plot(wd$MAS,wd$RELH)

wd$bins <- cut(wd$MAS,seq(0,1440,5))               #make 5 minute bins
levels(wd$bins) <- seq(0,by = 5,length.out = 288)  #rename the bins with lower range value
wdsum <- aggregate(list(wd$TAIR, wd$RELH, wd$PRES), by= list(wd$bins), median)  #get medians for each five minute period
colnames(wdsum) <- c("bin1", "TAIR", "RELH", "PRES")
wdsum$bin1 <- as.numeric(as.character(wdsum$bin1))
wdsum$bin2 <- wdsum$bin1 + 4.9   #define upper bin range
wdsum <- wdsum[,c(1,5,2,3,4)]           #reorder columns

plot(wdsum$bin1,wdsum$TAIR)
plot(wdsum$bin1,wdsum$RELH)
plot(wdsum$bin1,wdsum$PRES)

save(wdsum, file = "woodward_data.Rdata")

