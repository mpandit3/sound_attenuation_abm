#Atmospheric Sound attenuation model.

#alpha attenuation

#frequency (Hz)
f = 10000

#Temperature K
Temp = 293

#reference Temp K
Temp0 <- 293.15

#molar concentration of watr vapor (%)
h = .7

#Oxygen relaxation frequency
FrO <- 24 + 4.04e4 * h*((0.02+h)/(0.391+h))

FrO <- 88687.24

#Nitrogen relaxation frequency
FrN <- ((Temp/Temp0)^(-.5))*(9 + 280*h*exp(-4.17*((Temp/Temp0)^(-1/3)*-1)))

#alpha <- 869*(f^2)*(((1.84e-11)*((Temp/Temp0)^(0.5))) + ((Temp/Temp0)^(-5/2)) * ( 0.01275*(exp(-2239.1/Temp)/((FrO+f^2)/FrO)) + 0.1068*(exp(-3352/Temp)/((FrN+f^2)/FrN)) ) ) 

alf <- function(fr, h, FrO, FrN, Temp, Temp0) {
  alpha <- 869*f^2*(1.84e-11 * (Temp/Temp0)^.5)  +  (Temp/Temp0)^(-5/2)*(exp(-2239.1/Temp)*(FrO/(FrO^2 + f^2))) + (Temp/Temp0)^(-5/2)*(exp(-3352/Temp)*(FrN/(FrN^2 + f^2)))
    #869*(f^2)*(((1.84e-11)*((Temp/Temp0)^(0.5))) + ((Temp/Temp0)^(-5/2)) * ( 0.01275*(exp(-2239.1/Temp)/((FrO^2+f^2)/FrO)) + 0.1068*(exp(-3352/Temp)/((FrN^2+f^2)/FrN)) ) ) 
  return(alpha)
}

classical <- function(f, h, FrO, FrN, Temp, Temp0) {
  cla <- 869*f^2*(1.84e-11 * (Temp/Temp0)^.05)
  return(cla)
}

oxy <- function(f, h, FrO, FrN, Temp, Temp0) {
  ox <-  (Temp/Temp0)^(-5/2)*(exp(-2239.1/Temp)*(FrO/(FrO^2 + f^2)))  
  return(ox)
}

nit <- function(f, h, FrO, FrN, Temp, Temp0) {
  ni <- (Temp/Temp0)^(-5/2)*(exp(-3352/Temp)*(FrN/(FrN^2 + f^2)))
    return(ni)
}


df = data.frame(freq=10^(seq(1.5,6,.1)))
df$alpha <- alf(df$freq, h, FrO, FrN, Temp, Temp0)
df$classical <- classical(df$freq, h, FrO, FrN, Temp, Temp0)
df$oxygen <- oxy(df$freq, h, FrO, FrN, Temp, Temp0)
df$nitrogen <- nit(df$freq, h, FrO, FrN, Temp, Temp0)

plot(df$freq, df$alpha, log="xy", ylim = c(10^-2.2, 10^4))
points(df$freq, df$classical, col = "green" )
points(df$freq, df$oxygen, col = "blue")
points(df$freq, df$nitrogen,  col = "red")


#Agent based model of territory defense interactions among birds
#The arena is a hexagonal grid representing an array of territories
#Birds sing, move, and rest within their grid cells according to 
#pre-assigned probabilities
#The "goal" of each bird is to sing within the hearing range of each 
#of its neighbors.
#The model ends when all of the birds with interior territories have
#met this goal.

library(rgeos)
library(sp)
library(spdep)
#library(fractal)
library(FNN)
library(plotrix)

Arena = 10000   #rough size of entire square arena in meters
HexSize = 1000  #diameter of individual hexagons (territories)
CallRad = 500   #effective call radius
SingProb = 0.25  #probability of singing on a given turn
MoveProb = 0.25  #probability of moving on a given turn
RestProb = 1-(SingProb+MoveProb)

r1 = matrix(data = c(0,Arena,Arena,0,0,0,Arena,Arena), nrow = 4, ncol = 2)
Ps1 = Polygons(list(Polygon(r1)), ID = "a")
SPs = SpatialPolygons(list(Ps1))

# Fill arena with hexigons
set.seed(12)
HexPts <-spsample(SPs, type="hexagonal", cellsize=1000)
HexPols <- HexPoints2SpatialPolygons(HexPts)
#plot(HexPols)

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
set.seed(20)
Locs <- lapply(Hx@polygons, FUN = function(x) spsample(x, n = 1, "random"))
Locs = SpatialPoints(Locs)
Locs <- as.numeric(Locs@coords)
Locs <- t(matrix(Locs, nrow = 2, ncol = length(Locs)/2))

#Add locations to the data frame
Hx$Loc = Locs[,1:2]

#Initialize timer
timeCnt = 0

while(prod(Hx$done)==0) {
  #while(timeCnt < 70) { 
  
  timeCnt = timeCnt+1 #augment timer
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
  
  cat(paste0("Iteration number ", timeCnt, ". Birds finished ", length(which(Hx$done>1)), "\n"))
  
  ################################################
  ########plot outcome############################
  ################################################

  par(mar=c(0,0,0,0))
  Hcol = ifelse(Hx$done==0, "#FFFFCC30", ifelse(Hx$done==1, "#CCCCCC90", "#99FF6680"))
  plot(HexPols, col=Hcol)
  points(LocX, pch = 20, cex = 0.5, col = "red") #use LocX - unmodified by movement on this turn
  points(Hx$Loc[which(Hx$Action==2),],  pch = 20, cex = 0.5, col = "dodgerblue") #plot movement destinations
  for(i in sing){
    cCol = ifelse(i %in% fn$orig, "#FF006645", "#CCFF6630")
    draw.circle(x=Hx$Loc[i,1], y=Hx$Loc[i,2], radius=CallRad, col = cCol)
  }
  Sys.sleep(0.25) #pause a bit to see the plot
}

#HOW TO READ THE GRAPH
#Grid cells are territories and dots are bird locations
#Red dots are locations at the beginning of a time step
#Blue dots are locations at the end of a time step
#Circles show call radii for singing birds
#Pink circles indicate an interaction (the call reached another bird)
#If a bird "hears" a call it is forced to respond on that time step (it sings too)
#When a grid cell turns green it means the owner has interacted with all six of its neighbors
#To simplify things, the gray grid cells on the edges are disregarded (they have <6 neighbors)

Result <- as.data.frame(Hx)         #extract data from spatial object
Result <- Result[Result$nProx==6,]  #keep only the data from interior territories
par(mar=c(4,4,2,2))
hist(Result$done)

