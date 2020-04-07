#Agent based model of territory defense interactions among birds
#The arena is a hexagonal grid representing an array of territories
#Birds sing, move, and rest within their grid cells according to 
#pre-assigned probabilities
#The "goal" of each bird is to sing within the hearing range of each 
#of its neighbors.
#The model ends when all of the birds with interior territories have
#met this goal.

### Based on OK Mesonet data, will use 2011 for hot weather days, 
## 2015 for normal, 
# and 2019 for cool days 
# Dates: 06/22, 06/23, 06/24/, 06/25, 06/26,

library(rgeos)
library(sp)
library(spdep)
library(FNN)
library(plotrix)
library(ggplot2)

Arena = 10000        #rough size of entire square arena in meters
HexSize = 1000       #diameter of individual hexagons (territories)
Song_volume = 85     #song sound pressure in db
Song_detection = 30  #minimum sound pressure for detection
Song_freq = 7000     #relevant frequency in Hz
SingProb = 0.33      #probability of singing on a given turn
MoveProb = 0.33      #probability of moving on a given turn
RestProb = 1-(SingProb+MoveProb)  #probability of resting (not singing or moving)

#Date1 <- as.POSIXct("2018-05-04", tz = "UTC")
#Date2 <- as.POSIXct("2018-05-08", tz = "UTC")
Date1 <- as.POSIXct("2015-06-22", tz = "UTC")
Date2 <- as.POSIXct("2015-06-23", tz = "UTC")
Date3 <- as.POSIXct("2015-06-24", tz = "UTC")
Date4 <- as.POSIXct("2015-06-25", tz = "UTC")
Date5 <- as.POSIXct("2015-06-26", tz = "UTC")
#Date6 <- as.POSIXct("2080-05-08", tz = "UTC")

wdata = "ERIC_weather_2015.Rdata"

source("Atmospheric_sound_attenuation.R")
#example song radius at 25 degrees C, 50% hhumidty, 1 Atm pressure:
att_coef(f=Song_freq, T_cel=25, h_rel=50, Pa=101.325)
aud_range(dbOri = Song_volume, 
          dbMin = Song_detection, 
          f = Song_freq, 
          T_cel = 25, 
          h_rel = 50, 
          Pa = 90)

#how many iterations (model runs)
iter = 40
#duration of each run
runTime = 500 #500 min = 8.333 hours decent run time

# for(k in 1:iter) { 
#   
#   #Change date and parameters at midpoint
#   if(k > iter/2) {
#     Date <- Date2
#   } else {
#     Date <- Date1
#   }

for(k in 1:iter) { 
  
  #Change date and parameters at midpoint
 
  if(k > 12) {
    Date <- Date5
  } else if (k > 9){
    Date <- Date4
  } else if (k>6){
    Date <- Date3
  } else if (k>3){
    Date <- Date2
  } else {
    Date <- Date1
  }
  
  load(wdata)
  wdsum <- wdsum[which(wdsum$dateLocal == Date),]
  wdsum$CallRad <- mapply(aud_range,
                          Song_volume,
                          Song_detection,
                          Song_freq,
                          wdsum$TAIR,
                          wdsum$RELH,
                          wdsum$PRES)
  
  
  r1 = matrix(data = c(0,Arena,Arena,0,0,0,Arena,Arena), nrow = 4, ncol = 2)
  Ps1 = Polygons(list(Polygon(r1)), ID = "a")
  SPs = SpatialPolygons(list(Ps1))
  
  set.seed(12) #set random seed so you always get the same arrangement of hexagons
  # Fill arena with hexigons
  HexPts <-spsample(SPs, type="hexagonal", cellsize=1000)
  HexPols <- HexPoints2SpatialPolygons(HexPts)
  #plot(HexPols)
  
  set.seed(second(Sys.time())) #set seed by system time to go back to real randomness
  
  # ID index of grid
  pid <- sapply(slot(HexPols, "polygons"), function(x) slot(x, "ID"))
  
  # Make hexigons a spatial data frame, include 
  Hx = SpatialPolygonsDataFrame(HexPols, data.frame(N = c(1:length(HexPols)), row.names = pid))
  
  # Add to df a list of each territory's neighbors and the number of neighbors
  Hx$Prox = poly2nb(HexPols)
  Hx$nProx <- unlist(lapply(Hx$Prox, FUN = length)) 
  
  #initialize a column that designates current activity
  Hx@data$Action <- 0  #1 = sing, 2 = move, 0 = rest
  
  #make six columns for tracking neighbors.
  Hx@data[c("N1","N2","N3","N4","N5","N6")] <- 0
  
  #one more column to indicate if all interactions are complete
  Hx$done <- ifelse(Hx$nProx < 6, 1, 0) #edge cells with fewer than 6 neighbors -> set to 1
  
  #make three columns for activity tracking
  Hx@data[c("SingCnt","MoveCnt","RestCnt")] <- 0
  
  #Generate a random location in each grid cell
  Locs <- lapply(Hx@polygons, FUN = function(x) spsample(x, n = 1, "random"))
  Locs = SpatialPoints(Locs)
  Locs <- as.numeric(Locs@coords)
  Locs <- t(matrix(Locs, nrow = 2, ncol = length(Locs)/2))
  
  #Add locations to the data frame
  Hx$Loc = Locs[,1:2]
  
  #Initialize counter
  timeCnt = 0

  #while(prod(Hx$done)==0) {
  while(timeCnt < runTime) { 
    
    timeCnt = timeCnt+1 #augment timer
    #Extract relevant call radius for this iteration
    CallRad <- wdsum$CallRad[wdsum$bin1<=timeCnt & wdsum$bin2>timeCnt]
     
    Hx@data$Action <- 0 #reinitialize action column
    
    #Give each bird a random number to determine activity
    Rd = runif(n=nrow(Hx@data))
    
    #Birds with a random valule below SingProb will sing
    Hx@data$Action = ifelse(Rd <= SingProb, 1, 0)  #1 = sing, 2 = move, 0 = rest
    sing = which(Hx@data$Action==1)                #which birds choose to sing
    
    fn <- get.knn(Hx$Loc, k=6)  #Find the 6 nearest neighbors                          
    fn <- data.frame(orig=floor(seq(1,nrow(Hx)+.9, by=1/6)),  #coerse the output to a dataframe
                     neib=as.vector(t(fn$nn.index)),
                     dist=as.vector(t(fn$nn.dist)))
    fn <- fn[fn$dist <= CallRad,] #remove distances greater than call radius
    fn1 <- fn[fn$orig %in% sing,] #consider only the birds that sing this turn
    
    if(nrow(fn1)>0){ #nothing to do if there are no interactions
      #Birds will respond to a neighbor that sings. (only first order responses)
      Hx$Action[fn1$neib] <- 1  #make otherwise non-singing birds respond to singing neighbors
      Rd[fn1$neib] <- 0   #set the random numbers to 0 so these birds will not move on this turn
      sing = which(Hx@data$Action==1)  #which birds sing and respond
      Hx$SingCnt[sing] = Hx$SingCnt[sing] + 1
      fn <- fn[fn$orig %in% sing,] #consider only the birds that sing and respond this turn
      
      for (i in 1:nrow(fn)) { #loop through each interaction
        brd = fn$orig[i]         #number of original bird
        neb = fn$neib[i]         #number of neighbor bird
        nebs <- Hx$Prox[brd]         #list of all neighbor birds
        column <- paste0("N",which(neb == unlist(nebs)))  #which neighbor is this one
        column <- which(colnames(Hx@data) == column)   #determine the correct neighbor column
        Hx@data[brd,column] = Hx@data[brd,column] + 1  #add 1 to the column for the specific neighbor interaction
      }
    }
    
    #Store some activity data here
    Hx$SingCnt[sing] <- Hx$SingCnt[sing] + 1        
    doneTest <- Hx$N1*Hx$N2*Hx$N3*Hx$N4*Hx$N5*Hx$N6  #multiply to see if there is a zero in the neighbors column
    Hx$done <- ifelse(Hx$done==0 & doneTest>0, timeCnt, Hx$done)       #note the time count when a bird is done.
    
    #Now deal with the movers
    Hx@data$Action[Rd >= 1-MoveProb] = 2  #1 = sing, 2 = move, 0 = rest
    Hx@data$Action[Rd >= 1-MoveProb] = 2  #1 = sing, 2 = move, 0 = rest
    Mvrs <- which(Hx$Action == 2)
    
    #Generate some new locations for the movers
    LocX = Hx$Loc #store the old locations for plotting
    Locs <- lapply(Hx@polygons[Mvrs], FUN = function(x) spsample(x, n = 1, "random"))
    Locs = SpatialPoints(Locs)
    Locs <- as.numeric(Locs@coords)
    Locs <- t(matrix(Locs, nrow = 2, ncol = length(Locs)/2))
    
    #Add locations to the data frame
    Hx$Loc[Mvrs,] = Locs[,1:2]
    Hx$MoveCnt[Mvrs] <- Hx$MoveCnt[Mvrs] + 1
    
    #Resting birds just add to the restCnt column
    rest <- which(Hx$Action == 0)
    Hx$RestCnt[rest] <- Hx$RestCnt[rest] +1
    
    cat(paste0("date ", Date, ". run ", k, ". Iteration ", timeCnt, ". Call radius ", CallRad, ". Birds finished ", length(which(Hx$done>1)), "\n"))
    
    ################################################
    ########plot outcome############################
    ################################################
  
    # startLocs <- LocX
    # endLocs <- Hx$Loc[Mvrs,]
    # Arrows <- cbind(LocX[Mvrs,], Hx$Loc[Mvrs,])
    # 
    # par(mar=c(0,0,0,0))
    # Hcol = ifelse(Hx$done==0, "#FFFFCC30", ifelse(Hx$done==1, "#CCCCCC90", "#99FF6680"))
    # plot(HexPols, col=Hcol)
    # points(LocX, pch = 20, cex = 0.5, col = "red") #use LocX - unmodified by movement on this turn
    # points(Hx$Loc[which(Hx$Action==2),],  pch = 20, cex = 0.5, col = "dodgerblue") #plot movement destinations
    # arrows(x0=LocX[Mvrs,1], y0=LocX[Mvrs,2], x1=Hx$Loc[Mvrs,1], y1=Hx$Loc[Mvrs,2], length = 0.05)
    # for(i in sing){
    #   cCol = ifelse(i %in% fn$orig, "#FF006645", "#CCFF6630")
    #   draw.circle(x=Hx$Loc[i,1], y=Hx$Loc[i,2], radius=CallRad, col = cCol)
    # }
    # text(x=5000, y = 200, labels = paste0("Time ", timeCnt, "   Call radius = ", CallRad))
    # Sys.sleep(0.25) #pause a bit to see the plot
  }
  
  Result <- as.data.frame(Hx)         #extract data from spatial object
  Result <- Result[Result$nProx==6,]  #keep only the data from interior territories
  Result$run = k
  Result$date = Date
  if(k == 1) {
    Results = Result
  } else {
    Results = rbind(Result, Results)
  }
}


cells <- length(unique(Results$N))
Res1 <- Results[which(Results$date == Date1),]
Res2 <- Results[which(Results$date == Date2),]
Res3 <- Results[which(Results$date == Date3),]
Res4 <- Results[which(Results$date == Date4),]
Res5 <- Results[which(Results$date == Date5),]
#Res6 <- Results[which(Results$date == Date6),]

#Compiling results of birds that contacted all neighbors
completion1<-data.frame(time = 1:runTime, completed = 0)
for (i in 1:runTime){
  completion1$completed[i] <- length(which(Res1$done<=completion1$time[i] & Res1$done!=0))}
completion1$percent <- completion1$completed/(iter/40)/cells
completion1$date = Date1

completion2<-data.frame(time = 1:runTime, completed = 0)
for (i in 1:runTime){
  completion2$completed[i] <- length(which(Res2$done<=completion2$time[i] & Res2$done!=0))}
completion2$percent <- completion2$completed/(iter/40)/cells
completion2$date = Date2

completion3<-data.frame(time = 1:runTime, completed = 0)
for (i in 1:runTime){
  completion3$completed[i] <- length(which(Res3$done<=completion3$time[i] & Res3$done!=0))}
completion3$percent <- completion3$completed/(iter/40)/cells
completion3$date = Date3

completion4<-data.frame(time = 1:runTime, completed = 0)
for (i in 1:runTime){
  completion4$completed[i] <- length(which(Res4$done<=completion4$time[i] & Res4$done!=0))}
completion4$percent <- completion4$completed/(iter/40)/cells
completion4$date = Date4

completion5<-data.frame(time = 1:runTime, completed = 0)
for (i in 1:runTime){
  completion5$completed[i] <- length(which(Res5$done<=completion4$time[i] & Res5$done!=0))}
completion5$percent <- completion5$completed/(iter/40)/cells
completion5$date = Date5

completion_normal_total = rbind(completion1,completion2,completion3,completion4,completion5
                                )
