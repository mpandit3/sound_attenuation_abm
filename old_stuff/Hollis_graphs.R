#Hollis graphs

load("Hollis_data.Rdata")

#Plot temperature for first 600 minutes of each day
dm3 <- as.POSIXct("2018-05-03", tz = "UTC")
m3 <- wdsum[which(wdsum$dateLocal == dm3 & wdsum$bin1<601),]

dm4 <- as.POSIXct("2018-05-04", tz = "UTC")
m4 <- wdsum[which(wdsum$dateLocal == dm4 & wdsum$bin1<601),]

dm5 <- as.POSIXct("2018-05-05", tz = "UTC")
m5 <- wdsum[which(wdsum$dateLocal == dm5 & wdsum$bin1<601),]

dm6 <- as.POSIXct("2018-05-06", tz = "UTC")
m6 <- wdsum[which(wdsum$dateLocal == dm6 & wdsum$bin1<601),]

dm7 <- as.POSIXct("2018-05-07", tz = "UTC")
m7 <- wdsum[which(wdsum$dateLocal == dm7 & wdsum$bin1<601),]


plot(m6$bin1,m6$TAIR, col = "red", ylim = c(7,37), type = "l")
lines(m5$bin1,m5$TAIR, col = "blue")
lines(m3$bin1,m3$TAIR, col = "green")
lines(m4$bin1,m4$TAIR, col = "orange")
lines(m7$bin1,m7$TAIR, col = "black")

#use 5 and 6



