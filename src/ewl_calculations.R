#Calculation developed from EWL.pro, created by Denis Mutiibwa, PhD

# Conditions for calculating EWL
# Temperature greater or equal to Tb
# Total amount of water lost less than 15% of Mb (body mass)  (4.9g, CAWR). 

load("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data/ERIC_weather_normal/ERIC_weather_normal.Rdata")
wdsum2 = wdsum %>% 
  filter(!is.na(bin1))
         
wd$tewl = 0 #total evaporative water loss increases until it reaches 4.9
wd$ewl = NULL
for(j in 1:length(wd$bins)){
  if(wd$TAIR[j]>=40){ 
    wd$ewl[j] = ((wd$TAIR[j]*0.1703)-6.566)/12 #ewl equation divided by 12 to account for the 5 min bins
    # wd$tewl = wd$tewl[j]+wd$ewl[j]
  } else if(wd$TAIR[j]<40) {
    wd$ewl = 0
  }
  if(j == 1){
    wd$tewl[j] = 0 + wd$ewl[j]
  } else {
    wd$tewl[j] = wd$tewl[j-1]+wd$ewl[j]
  }
  if(wd$bins[j] == 0){
    wd$tewl[j] = 0
  } else{
    
  }
}


# if (temparray[i,j,t] GE 40.0) AND (TEWL[i,j] LT 4.9) then  begin
# ewlhours[i,j] = 1.0 
# EWL[i,j] = (temparray[i,j,t]*0.1703)-6.566 ; EWL equation
# TEWL[i,j]= TEWL[i,j]+EWL[i,j]
# count = count + 1.0
# thours[i,j] = thours[i,j] + ewlhours[i,j]
# tewlhours[i,j] = thours[i,j]
# endif
# 
# endfor