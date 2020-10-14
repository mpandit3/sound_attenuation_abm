night_temp = function(temp, temp_increase){

  #decrease of 0.44 C/decade for max temps calculated from: 
  #https://www.nws.noaa.gov/ost/climate/STIP/FY11CTBSeminars/lzhou_052511_f4.jpg

  cc_final = NULL
  for(i in 1:length(wd$bins)){
    if(between(wd$bins[i],105,895)){
      cc_temp = wd$TAIR[i]-2.64
    } else if(between(wd$bins[i],0,105)){
      cc_temp = wd$TAIR[i]+12.11
    } else if(between(wd$bins[i],900,1435)){
      cc_temp = wd$TAIR[i]+12.11
    }
    cc_final = rbind(cc_final,cc_temp)
    #print(paste0("Actual TAIR ",wd2$TAIR, " CC Increase ", cc_final))
  }
  
  out = cbind(wd$bins,wd$TAIR,cc_final)
  
}