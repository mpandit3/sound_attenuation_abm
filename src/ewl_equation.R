#######################################################
############ Evaporative Waterloss Equation ############
########################################################
#Based on Albright et al. 2017's equation
#Modified to include temperatures below 40C

# install.packages("digitize")
library(digitize)
library(lme4)
library(ggplot2)
library(drc)
library(nlme)
library(aomisc)
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120")


# Lesser Goldfinch --------------------------------------------------------

lego_cal <- ReadAndCal('data/lego_albright.png') #needs to go from xleft, xright, ybottom, y top
lego_ewl <- DigitData(col = 'red') #highlight all the individual
lego_data <- Calibrate(lego_ewl, lego_cal, 30, 50, 0, 12)
save(lego_data,file = "data/lego_ewl_data.Rdata")

load("data/lego_ewl_data.Rdata")
m_lego = lm(y~x, data = lego_data)
summary(m_lego) #linear equation: y = 0.3847x-11.4512
lego_plot = ggplot(lego_data, aes(x,y)) +
  geom_point()+
  geom_smooth(method = "lm");lego_plot

# House Finch -------------------------------------------------------------
hofi_cal <- ReadAndCal('data/hofi_albright.png') #needs to go from xleft, xright, ybottom, y top
hofi_ewl <- DigitData(col = 'red') #highlight all the individual
hofi_data <- Calibrate(hofi_ewl, hofi_cal, 30, 50, 0, 12)
save(hofi_data,file = "data/hofi_ewl_data.Rdata")

load("data/hofi_ewl_data.Rdata")
m_hofi = lm(y~x, data = hofi_data)
summary(m_hofi) #linear equation: y = 0.46179x-15.85529
#exponential equation: 0.0045e^0.1511x
hofi_plot = ggplot(hofi_data, aes(x,y)) +
  geom_point()+
  geom_smooth(method = "gam");hofi_plot


model <- drm(y ~ x, fct = DRC.expoGrowth(),
             data = hofi_data)
summary(model)
# Cactus Wren -------------------------------------------------------------
cawr_cal <- ReadAndCal('data/cawr_albright.png') #needs to go from xleft, xright, ybottom, y top
cawr_ewl <- DigitData(col = 'red') #highlight all the individual
cawr_data <- Calibrate(cawr_ewl, cawr_cal, 30, 50, 0, 12)
save(cawr_data,file = "data/cawr_ewl_data.Rdata")

load("data/cawr_ewl_data.Rdata")
m_cawr = lm(y~x, data = cawr_data)
summary(m_cawr) #linear equation: y = 0.30025x-9.28818
cawr_plot = ggplot(cawr_data, aes(x,y)) +
  geom_point()+
  geom_smooth(method = "lm");cawr_plot

# Albert's Towhee -------------------------------------------------------------
abto_cal <- ReadAndCal('data/abto_albright.png') #needs to go from xleft, xright, ybottom, y top
abto_ewl <- DigitData(col = 'red') #highlight all the individual
abto_data <- Calibrate(abto_ewl, abto_cal, 30, 50, 0, 12)
save(abto_data,file = "data/abto_ewl_data.Rdata")

load("data/abto_ewl_data.Rdata")
m_abto = lm(y~x, data = abto_data)
summary(m_abto) #linear equation: y = 0.25440x-7.54874
abto_plot = ggplot(abto_data, aes(x,y)) +
  geom_point()+
  geom_smooth(method = "lm");abto_plot

# Curved-Billed Thrasher -------------------------------------------------------------
cbth_cal <- ReadAndCal('data/cbth_albright.png') #needs to go from xleft, xright, ybottom, y top
cbth_ewl <- DigitData(col = 'red') #highlight all the individual
cbth_data <- Calibrate(cbth_ewl, cbth_cal, 30, 50, 0, 12)
save(cbth_data,file = "data/cbth_ewl_data.Rdata")

load("data/cbth_ewl_data.Rdata")
m_cbth = lm(y~x, data = cbth_data)
summary(m_cbth) #linear equation: y = 0.24952x-7.94863
cbth_plot = ggplot(cbth_data, aes(x,y)) +
  geom_point()+
  geom_smooth(method = "lm");cbth_plot

library(drc)
library(nlme)
library(aomisc)