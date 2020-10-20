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

library(suncalc)
library(lubridate)
library(rgeos)
library(sp)
library(spdep)
library(FNN)
library(plotrix)
library(ggplot2)
library(dplyr)

##Which model are you running?

m=0 #0 for normal data, 
    #1 for uniform climate change, 3C increase in temp only
    #2 for asymmetric increases in nighttime temperatures, 
    #3 for increases in temperature and decreases in relh

if(m == 0){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/normal")
  
  wdata = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data/ERIC_weather_normal/ERIC_weather_normal.Rdata"
  
} else if(m == 1){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/uniform_cc")
  wdata = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data/ERIC_weather_cc_uniform/ERIC_weather_cc_uniform.Rdata" #Uniform Climate Change Data
  
} else if (m == 2){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/nighttime_cc")
  
  wdata = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data/ERIC_weather_cc_nighttime/ERIC_weather_cc_nighttime.Rdata"
  
} else if (m==3){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/cc_aridity")
  
  wdata = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data/ERIC_weather_tair_relh_increase/ERIC_weather_tair_relh_increase.Rdata"
}


# load(wdata)
# set.seed(744)

Arena = 10000        #rough size of entire square arena in meters
HexSize = 1000       #diameter of individual hexagons (territories)
Song_volume = 85     #song sound pressure in db
Song_detection = 30  #minimum sound pressure for detection
Song_freq = 7000     #relevant frequency in Hz
SingProb = 0.33      #probability of singing on a given turn
MoveProb = 0.33      #probability of moving on a given turn
RestProb = 1-(SingProb+MoveProb)  #probability of resting (not singing or moving)
value = integer(0)

source("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/src/Atmospheric_sound_attenuation.R")
#example song radius at 25 degrees C, 50% hhumidty, 1 Atm pressure:
att_coef(f=Song_freq, T_cel=25, h_rel=50, Pa=101.325)
aud_range(dbOri = Song_volume, 
          dbMin = Song_detection, 
          f = Song_freq, 
          T_cel = 25, 
          h_rel = 50, 
          Pa = 90)

#how many iterations (model runs)
iter =1 #runs for 4 iterations for each 5 min time step
#duration of each run
runTime = 360 #360 min = 6 hours decent run time

# for(k in 1:iter) { 
#   
#   #Change date and parameters at midpoint
#   if(k > iter/2) {
#     Date <- Date2
#   } else {
#     Date <- Date1
#   }
years = as.list(c(2011, 2015, 2019))# 2011 is hot year, 2015 normal, 2019 cold year, all relative
#dates = as.list(c("06-22", "06-23", "06-24", "06-25", "06-26"))
load(wdata)

# dates = wdsum %>%
#   group_by(date)
# dates = group_keys(dates)
dates = as.data.frame(table(wdsum$dateLocal))
dates = as.vector(dates$Var1)


for(d in 1:length(dates)){ #dates loop
    Date = dates[d]

    print(Date)
  wdsum = NULL
for(k in 1:iter) { #beginning of iteration loop

  load(wdata)
  wdsum <- wdsum[which(wdsum$dateLocal == as_date(Date)),]
  wdsum$CallRad <- mapply(aud_range,
                          Song_volume,
                          Song_detection,
                          Song_freq,
                          # wdsum$FTAIR,
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
  # set.seed(1000)
  
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
  Locs <- as.numeric(Locs@coords) #was originally as.numeric
  Locs <- t(matrix(Locs, nrow = 2, ncol = length(Locs)/2))
  
  #Add locations to the data frame
  Hx$Loc = Locs[,1:2]
  
  #Initialize counter
  timeCnt = 0

  #while(prod(Hx$done)==0) {
  while(timeCnt < runTime) {
    for (i in 1:length(wdsum$bin1)) {
    if(wdsum$TEWL[i] <2.49){ #15% of Body Mass of painted bunting
      SingProb = 0.33      #probability of singing on a given turn
      MoveProb = 0.33      #probability of moving on a given turn
      RestProb = 1-(SingProb+MoveProb)  #probability of resting (not singing or moving)
    } else if (wdsum$TEWL[i]>2.49){
      SingProb = 0.01 #model would not work when singing probability was 0 so keeping it at a low number, need to keep it above 0
      MoveProb = 0.01
      RestProb = 1-(SingProb+MoveProb)
    }
  }
    timeCnt = timeCnt+1 #augment timer
    #Extract relevant call radius for this iteration
    CallRad <- wdsum$CallRad[wdsum$bin1<=timeCnt & wdsum$bin2>timeCnt]
     
    Hx@data$Action <- 0 #reinitialize action column
    
    #Give each bird a random number to determine activity
    Rd = runif(n=nrow(Hx@data))
    
    #Birds with a random value below SingProb will sing
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

###Mvrs being empty is what gives the non-numeric matrix error, need to figure out a way around this
    if(length(value) != length(Mvrs)){
      #Generate some new locations for the movers
      LocX = Hx$Loc #store the old locations for plotting
      Locs <- lapply(Hx@polygons[Mvrs], FUN = function(x) spsample(x, n = 1, "random"))
      Locs = SpatialPoints(Locs)
      Locs <- as.numeric(Locs@coords) #changed from "as.numeric" to as.integer
      Locs <- t(matrix(Locs, nrow = 2, ncol = length(Locs)/2))
      
    } else {
      Locs = LocX
    }
      #Add locations to the data frame
      Hx$Loc[Mvrs,] = Locs[,1:2]
      Hx$MoveCnt[Mvrs] <- Hx$MoveCnt[Mvrs] + 1
      
    #Resting birds just add to the restCnt column
    rest <- which(Hx$Action == 0)
    Hx$RestCnt[rest] <- Hx$RestCnt[rest] +1
    
    # cat(paste0("date ", Date, ". Iteration ", k, ". TimeStamp ", timeCnt, ". Call radius ", CallRad, ". Birds finished ", length(which(Hx$done>1)), "\n"))
    # 
    # ################################################
    # ########plot outcome############################
    # ################################################
    # 
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
    
  } #end of timecount loop
  
  Result <- as.data.frame(Hx)         #extract data from spatial object
  Result <- Result[Result$nProx==6,]  #keep only the data from interior territories
  Result$TotalInt = (Result$N1+Result$N2+Result$N3+Result$N4+Result$N5+Result$N6)
  Result$run = k
  Result$date = Date
  if(d == 1) {
    Results = Result
  } else {
    Results = rbind(Result, Results)
  }#end of Results ifelse loop
  
  } #end of iteration loop
  
  #Compiling results of birds that contacted all neighbors
  completion<-data.frame(time = 1:runTime, completed = 0)
  for (c in 1:runTime){
    completion$completed[c] <- length(which(Results$date == Date & Results$done<=completion$time[c] & Results$done!=0))
    completion$SingCnt[c] <- sum(Results$SingCnt[c])
    completion$MoveCnt[c] <- sum(Results$MoveCnt[c])
    completion$RestCnt[c] <- sum(Results$RestCnt[c])
    completion$TotalInt[c] <- sum(Results$TotalInt[c])
    completion$date = Date
  } #end of completion loop
  
  if(d == 1) {
    completions = completion
  } else {
    completions = rbind(completion, completions)
  }#end of completions ifelse loop
} #end of dates for loop

completions$percent = completions$completed/(72*iter)

# # Saving Results ----------------------------------------------------------
# 
# # #Loc is an array, need to separate and add it back to dataframe
# # locations = Results$Loc #Save locations to a separate array with two vectors
# # formatted.results = subset(Results, select =-c(Loc)) #Remove Loc vector
# # formatted.results$LocX = locations[1] #Adding X coordinate of location
# # formatted.results$LocY = locations[2] #Adding Y coordinate of location
# # formatted.results = as.data.frame(formatted.results)
# # 
# # #Prox is a list, need to separate and add it back to dataframe
# # proximity = formatted.results$Prox #Isolating proximity list
# # formatted.results2 = subset(formatted.results, select = -c(Prox)) #removing it from Results dataframe
# # proximity <- data.frame(matrix(unlist(proximity), 
# #                                nrow=2880, 
# #                                byrow=T),
# #                         stringsAsFactors=FALSE) #converting list into dataframe
# # colnames(proximity) = c("neighbor1", #renaming columns in proximity dataframe
# #                         "neighbor2",
# #                         "neighbor3",
# #                         "neighbor4",
# #                         "neighbor5",
# #                         "neighbor6")
# # formatted.results3 = cbind(formatted.results2, proximity)
# # attach(formatted.results3)
# 
# #write.csv(formatted.results3, "/cloud/project/data_clean/2019_cold/2019_normal_abm_results.csv")
# 
if(m == 0){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/normal")
  Results_normal = Results
  save(Results_normal, file = paste0("results_normal", ".Rdata"))
  completion_normal = completions
  save(completion_normal, file = paste0("completion_normal", ".Rdata"))
} else if(m == 1){
  setwd("~/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_052420/data_clean/uniform_cc")
  Results_uniform_cc = Results
  save(Results_uniform_cc, file = paste0("results_uniform_cc", ".Rdata"))
  completion_uniform_cc = completions
  save(completion_uniform_cc, file = paste0("completion_uniform_cc", ".Rdata"))
  
} else if (m == 2){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/nighttime_cc")
  Results_nighttime_cc = Results
  save(Results_nighttime_cc, file = paste0("results_nighttime_cc", ".Rdata"))
  completion_nighttime_cc = completions
  save(completion_nighttime_cc, file = paste0("completion_nighttime_cc", ".Rdata"))
} else if (m==3){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/cc_aridity")
  Results_aridity_cc = Results
  save(Results_aridity_cc, file = paste0("results_aridity_cc", ".Rdata"))
  completion_aridity_cc = completions
  save(completion_aridity_cc, file = paste0("completion_aridity_cc", ".Rdata"))
}
