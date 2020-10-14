################Stats Analysis###################

library(nlme)
library(lme4)
library(dplyr)
library(lmerTest)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(nortest)


# Load Models Separately --------------------------------------------------

m = 2 #m is the model number: m = 0 for the normal data, m = 1 for the uniform climate change data, m = 2 for the night time temperature increase

if(m == 0){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/normal")
    
    mdata1 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/normal/results_normal.Rdata"
    mdata2 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/normal/completion_normal.Rdata"
    
    load(mdata1)
    load(mdata2)
    
    data1 = Results_normal #loading Results data - summarized by individual
    data2 = completion_normal #loading Completion data - summarized by time-step
} else if(m==1){
    setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/uniform_cc")
    
    mdata1 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/uniform_cc/results_uniform_cc.Rdata"
    mdata2 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/uniform_cc/completion_uniform_cc.Rdata"
    
    load(mdata1)
    load(mdata2)
    
    data1 = Results_uniform_cc #loading Results data - summarized by individual
    data2 = completion_uniform_cc #loading Completion data - summarized by time-step
} else if(m==2){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/nighttime_cc")
  
  mdata1 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/nighttime_cc/results_nighttime_cc.Rdata"
  mdata2 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/nighttime_cc/completion_nighttime_cc.Rdata"
  
  load(mdata1)
  load(mdata2)
  
  data1 = Results_nighttime_cc #loading Results data - summarized by individual
  data2 = completion_nighttime_cc #loading Completion data - summarized by time-step
  
} else if(m==3){
  setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/cc_aridity")
  
  mdata1 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/cc_aridity/results_aridity_cc.Rdata"
  mdata2 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/cc_aridity/completion_aridity_cc.Rdata"
  
  load(mdata1)
  load(mdata2)
  
  data1 = Results_aridity_cc #loading Results data - summarized by individual
  data2 = completion_aridity_cc #loading Completion data - summarized by time-step
}

# Load Models Together ----------------------------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/normal")

mdata1 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/normal/results_normal.Rdata"
mdata2 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/normal/completion_normal.Rdata"

load(mdata1)
load(mdata2)

data1 = Results_normal #loading Results data - summarized by individual
data2 = completion_normal #loading Completion data - summarized by time-step

###Load Uniform Climate Change Model Results
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/uniform_cc")

mdata3 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/uniform_cc/results_uniform_cc.Rdata"
mdata4 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/uniform_cc/completion_uniform_cc.Rdata"

load(mdata3)
load(mdata4)

data3 = Results_uniform_cc #loading Results data - summarized by individual
data4 = completion_uniform_cc #loading Completion data - summarized by time-step

mdata5 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/cc_aridity/results_aridity_cc.Rdata"
mdata6 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/cc_aridity/completion_aridity_cc.Rdata"

load(mdata5)
load(mdata6)

data5 = Results_aridity_cc #loading Results data - summarized by individual
data6 = completion_aridity_cc #loading Completion data - summarized by time-step

mdata7 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/nighttime_cc/results_nighttime_cc.Rdata"

mdata8 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/nighttime_cc/completion_nighttime_cc.Rdata"

load(mdata7)
load(mdata8)

data7 = Results_nighttime_cc
data8 = completion_nighttime_cc

# Add Dates to Data Frames ------------------------------------------------

#Add year (year) to the dataframe
data1$year = as.factor((substr(data1$date, 1,4))) #creates year vector from date
data1$month = as.factor(substr(data1$date, 6,7)) #creates month-day vector from date
data1$day = as.factor(substr(data1$date,9,10))
data1$monthDay = as.factor(substr(data1$date,6,10))

data2$year = as.factor(substr(data2$date, 1,4)) #creates year vector from date
data2$month = as.factor(substr(data2$date, 6,7)) #creates month-day vector from date
data2$day = as.factor(substr(data2$date,9,10))
data2$monthDay = as.factor(substr(data2$date,6,10))

data3$year = as.factor((substr(data3$date, 1,4))) #creates year vector from date
data3$month = as.factor(substr(data3$date, 6,7)) #creates month-day vector from date
data3$day = as.factor(substr(data3$date,9,10))
data3$monthDay = as.factor(substr(data3$date,6,10))

data4$year = as.factor(substr(data4$date, 1,4)) #creates year vector from date
data4$month = as.factor(substr(data4$date, 6,7)) #creates month-day vector from date
data4$day = as.factor(substr(data4$date,9,10))
data4$monthDay = as.factor(substr(data4$date,6,10))

data5$year = as.factor((substr(data5$date, 1,4))) #creates year vector from date
data5$month = as.factor(substr(data5$date, 6,7)) #creates month-day vector from date
data5$day = as.factor(substr(data5$date,9,10))
data5$monthDay = as.factor(substr(data5$date,6,10))

data6$year = as.factor(substr(data6$date, 1,4)) #creates year vector from date
data6$month = as.factor(substr(data6$date, 6,7)) #creates month-day vector from date
data6$day = as.factor(substr(data6$date,9,10))
data6$monthDay = as.factor(substr(data6$date,6,10))

data7$year = as.factor(substr(data7$date, 1,4)) #creates year vector from date
data7$month = as.factor(substr(data7$date, 6,7)) #creates month-day vector from date
data7$day = as.factor(substr(data7$date,9,10))
data7$monthDay = as.factor(substr(data7$date,6,10))

data8$year = as.factor(substr(data8$date, 1,4)) #creates year vector from date
data8$month = as.factor(substr(data8$date, 6,7)) #creates month-day vector from date
data8$day = as.factor(substr(data8$date,9,10))
data8$monthDay = as.factor(substr(data8$date,6,10))

# Calculate Means for each Normal Weather Conditions -------------------------------------

data1means = data1 %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(year) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   SingCntMean = mean(SingCnt), 
                   SingCntSE = (sd(SingCnt)/sqrt(n)),
                   MoveCntMean = mean(MoveCnt),
                   MoveCntSE = (sd(MoveCnt)/sqrt(n)),
                   RestCntMean = mean(RestCnt),
                   RestCntSE = (sd(RestCnt)/sqrt(n)),
                   TotalIntMean = mean(TotalInt),
                   TotalIntSE = (sd(TotalInt)/sqrt(n))
)


data2means = data2 %>% 
  filter(!is.na(SingCnt)) %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(year) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   SingCntMean = mean(SingCnt), 
                   SingCntSE = (sd(SingCnt)/sqrt(n)),
                   MoveCntMean = mean(MoveCnt),
                   MoveCntSE = (sd(MoveCnt)/sqrt(n)),
                   RestCntMean = mean(RestCnt),
                   RestCntSE = (sd(RestCnt)/sqrt(n)),
                   TotalIntMean = mean(TotalInt),
                   TotalIntSE = (sd(TotalInt)/sqrt(n)),
                   CompletedMean = mean(completed),
                   CompletedSE = (sd(completed)/sqrt(n)),
                   PercentMean = mean(percent),
                   PercentsE = (sd(percent)/sqrt(n))
  )
  
#Barplot to show means and SE of percent of individuals who completed for each year
#need to calcualte means for each individual year
ggplot(data2means, aes(x=year, y=PercentMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=PercentMean-PercentsE, 
                    ymax=PercentMean+PercentsE), 
                width=.2,
                position=position_dodge(.9)) + 
  theme_classic(base_size = 16)

#Scatterplot demonstrating mean Percent completed across the 360 minute timeframe

data2means = data2 %>% 
  filter(!is.na(SingCnt)) %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(time,year) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   SingCntMean = mean(SingCnt), 
                   SingCntSE = (sd(SingCnt)/sqrt(n)),
                   MoveCntMean = mean(MoveCnt),
                   MoveCntSE = (sd(MoveCnt)/sqrt(n)),
                   RestCntMean = mean(RestCnt),
                   RestCntSE = (sd(RestCnt)/sqrt(n)),
                   TotalIntMean = mean(TotalInt),
                   TotalIntSE = (sd(TotalInt)/sqrt(n)),
                   CompletedMean = mean(completed),
                   CompletedSE = (sd(completed)/sqrt(n)),
                   PercentMean = mean(percent),
                   PercentsE = (sd(percent)/sqrt(n))
  )
data2means$model = "normal"

#Colorblind friendly palette
cbPalette <- c("#E69F00", #orange
               "#999999", #gray
               "#56B4E9", #blue
               "#F0E442", #yellow
               "#009E73", #teal
               "#0072B2", #dark blue
               "#D55E00", #dark orange
               "#CC79A7", #purple
               "#000000",
               "#004949",
               "#009292",
               "#ff6db6",
               "$ffb6db",
               "#490092",
               "#006ddb",
               "#b66dff",
               "#6db6ff",
               "#b6dbff",
               "#920000",
               "#924900",
               "#db6d00",
               "#24ff24",
               "#ffff6d") #purple

ggplot(data = data2means, 
       aes(x=time, 
           y=PercentMean, 
           group=year, 
           color = year)) +
  #geom_point(size = 2)+
  geom_line(size = 2)+
  scale_y_continuous(name = "Percent Contacted All\n Neighbors", limits=c(0.0, 1.0))+
  scale_colour_manual(values = c("#E69F00",
                                 "#999999",
                                 "#56B4E9"))+
  theme_classic(base_size = 15)+
  theme(legend.position = "top")

  # theme(text = element_text(size=30),
  #       panel.background = element_rect(fill = "white")
        
# Calculate Means for Uniform Climate Change Conditions -------------------

levels(data3$year)[1] = "2081"
levels(data3$year)[2] = "2085"
levels(data3$year)[3] = "2089"

data3$year = as.factor(data3$year)
data3means = data3 %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(year) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   SingCntMean = mean(SingCnt), 
                   SingCntSE = (sd(SingCnt)/sqrt(n)),
                   MoveCntMean = mean(MoveCnt),
                   MoveCntSE = (sd(MoveCnt)/sqrt(n)),
                   RestCntMean = mean(RestCnt),
                   RestCntSE = (sd(RestCnt)/sqrt(n)),
                   TotalIntMean = mean(TotalInt),
                   TotalIntSE = (sd(TotalInt)/sqrt(n))
  )



# ggplot(data1means, aes(x=monthDay, 
#                        y=SingCntMean, 
#                        color=year)) + 
#   geom_errorbar(aes(ymin=SingCntMean-SingCntSE, 
#                     ymax=SingCntMean+SingCntSE), 
#                 width=.2,
#                 position=position_dodge(.9)) + 
#   geom_line(position=position_dodge(.9)) +
#   geom_point(position=position_dodge(.9), size=3)+
#   theme_classic()

data4$percent = data4$completed/(72*4)

levels(data4$year)[1] = "2081"
levels(data4$year)[2] = "2085"
levels(data4$year)[3] = "2089"

data4means = data4 %>% 
  filter(!is.na(SingCnt)) %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(year) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   SingCntMean = mean(SingCnt), 
                   SingCntSE = (sd(SingCnt)/sqrt(n)),
                   MoveCntMean = mean(MoveCnt),
                   MoveCntSE = (sd(MoveCnt)/sqrt(n)),
                   RestCntMean = mean(RestCnt),
                   RestCntSE = (sd(RestCnt)/sqrt(n)),
                   TotalIntMean = mean(TotalInt),
                   TotalIntSE = (sd(TotalInt)/sqrt(n)),
                   CompletedMean = mean(completed),
                   CompletedSE = (sd(completed)/sqrt(n)),
                   PercentMean = mean(percent),
                   PercentsE = (sd(percent)/sqrt(n))
  )

# Calculate means for Arid Climate Change Conditions ----------------------

levels(data5$year)[1] = "2081"
levels(data5$year)[2] = "2085"
levels(data5$year)[3] = "2089"

data5$year = as.factor(data5$year)
data5means = data5 %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(year) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   SingCntMean = mean(SingCnt), 
                   SingCntSE = (sd(SingCnt)/sqrt(n)),
                   MoveCntMean = mean(MoveCnt),
                   MoveCntSE = (sd(MoveCnt)/sqrt(n)),
                   RestCntMean = mean(RestCnt),
                   RestCntSE = (sd(RestCnt)/sqrt(n)),
                   TotalIntMean = mean(TotalInt),
                   TotalIntSE = (sd(TotalInt)/sqrt(n))
  )

data6$percent = data6$completed/(72*iter)

levels(data6$year)[1] = "2081"
levels(data6$year)[2] = "2085"
levels(data6$year)[3] = "2089"

data6means = data6 %>% 
  filter(!is.na(SingCnt)) %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(time, year) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   SingCntMean = mean(SingCnt), 
                   SingCntSE = (sd(SingCnt)/sqrt(n)),
                   MoveCntMean = mean(MoveCnt),
                   MoveCntSE = (sd(MoveCnt)/sqrt(n)),
                   RestCntMean = mean(RestCnt),
                   RestCntSE = (sd(RestCnt)/sqrt(n)),
                   TotalIntMean = mean(TotalInt),
                   TotalIntSE = (sd(TotalInt)/sqrt(n)),
                   CompletedMean = mean(completed),
                   CompletedSE = (sd(completed)/sqrt(n)),
                   PercentMean = mean(percent),
                   PercentsE = (sd(percent)/sqrt(n))
  )
#Barplot to show means and SE of percent of individuals who completed for each year
#need to calcualte means for each individual year
ggplot(data2means, aes(x=year, y=PercentMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=PercentMean-PercentsE, 
                    ymax=PercentMean+PercentsE), 
                width=.2,
                position=position_dodge(.9)) + 
  theme_classic(base_size = 16)

#Scatterplot demonstrating mean Percent completed across the 360 minute timeframe

data6means = data6 %>% 
  filter(!is.na(SingCnt)) %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(time,year) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   SingCntMean = mean(SingCnt), 
                   SingCntSE = (sd(SingCnt)/sqrt(n)),
                   MoveCntMean = mean(MoveCnt),
                   MoveCntSE = (sd(MoveCnt)/sqrt(n)),
                   RestCntMean = mean(RestCnt),
                   RestCntSE = (sd(RestCnt)/sqrt(n)),
                   TotalIntMean = mean(TotalInt),
                   TotalIntSE = (sd(TotalInt)/sqrt(n)),
                   CompletedMean = mean(completed),
                   CompletedSE = (sd(completed)/sqrt(n)),
                   PercentMean = mean(percent),
                   PercentsE = (sd(percent)/sqrt(n))
  )
data6means$model = "arid_cc"

ggplot(data = data6means, 
       aes(x=time, 
           y=PercentMean, 
           group=year, 
           color = year)) +
#  geom_point(size = 2)+
  geom_line(size = 5)+
  scale_y_continuous(name = "Percent Contacted All\n Neighbors", limits=c(0.0, 1.0))+
  scale_colour_manual(values = cbPalette)+
  theme_classic(base_size = 15)+
  theme(legend.position = "top",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
        )

data8means = data8 %>% 
  filter(!is.na(SingCnt)) %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(time,year) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   SingCntMean = mean(SingCnt), 
                   SingCntSE = (sd(SingCnt)/sqrt(n)),
                   MoveCntMean = mean(MoveCnt),
                   MoveCntSE = (sd(MoveCnt)/sqrt(n)),
                   RestCntMean = mean(RestCnt),
                   RestCntSE = (sd(RestCnt)/sqrt(n)),
                   TotalIntMean = mean(TotalInt),
                   TotalIntSE = (sd(TotalInt)/sqrt(n)),
                   CompletedMean = mean(completed),
                   CompletedSE = (sd(completed)/sqrt(n)),
                   PercentMean = mean(percent),
                   PercentsE = (sd(percent)/sqrt(n))
  )
data8means$model = "asymmetric"

# Plotting Normal and Climate Change Percent Completed Together -----------

data_completed = as.data.frame(rbind(data2means,data6means,data8means))
# data_completed = as.data.frame(rbind(data2means,data6means))

data_completed_hot = data_completed %>%
  group_by(year) %>%
  filter(year == "2011" | year == "2081")
data_completed_hot$treatment = "Hot"

data_completed_medium = data_completed %>%
  group_by(year) %>%
  filter(year == "2015" | year == "2085")

data_completed_medium$treatment = "Medium"

data_completed_cold = data_completed %>%
  group_by(year) %>%
  filter(year == "2019" | year == "2089")

data_completed_cold$treatment = "Cold"

df_completed = as.data.frame(rbind(data_completed_hot,
                                   data_completed_medium,
                                   data_completed_cold))

# df_present = df_completed %>%
#   group_by(year)%>%
#   filter(year == "2011"|year == "2015"|year == "2019")
# df_present$model = "Current"
# 
# df_future = df_completed %>%
#   group_by(year)%>%
#   filter(year == "2081"|year == "2085"|year == "2089")
# df_future$model = "Predicted"

df_completed2 = df_completed %>%
  group_by(time,year)


ggplot(data = df_completed2, 
       aes(x=time, 
           y=PercentMean,
           color = treatment)) +
  #  geom_point(size = 2)+
  geom_line(size = 1,
            aes(linetype=model)) +
  # geom_errorbar(aes(ymin = PercentMean-PercentsE,
  #                   ymax = PercentMean+PercentsE))+
  # geom_ribbon(aes(ymin = PercentMean-PercentsE,
  #                 ymax = PercentMean+PercentsE,
  #                 color = treatment))+
  # geom_smooth(aes(time, PercentMean),
  #             stat = "smooth",
  #             method = "loess",
  #             formula = y~x,
  #             se = TRUE,
  #             size = 1,
  #             linetype = "dotted")+
  scale_linetype_manual(values=c("dotted",
                                 "dashed",
                                 "solid"))+
  scale_y_continuous(name = "Percent Contacted All\n Neighbors", 
                     limits=c(0.0, 1.0))+
  scale_x_continuous(name =  "Time (min)")+
  scale_colour_manual(values = c("#56B4E9", #blue,
                                 "#E69F00", #yellow
                                 "#999999" #gray
                                 ))+
  theme_classic(base_size = 10)
# +
#   theme(legend.position = "top")
  
###Plotting Percent completed across individual days
# ggplot(data2means, aes(x=monthDay, y=CompletedMean, fill=year)) + 
#   geom_bar(stat="identity", 
#            position=position_dodge()) +
#   geom_errorbar(aes(ymin=CompletedMean-CompletedSE, 
#                     ymax=CompletedMean+CompletedSE), 
#                 width=.2,
#                 position=position_dodge(.9)) + 
#   theme_classic(base_size = 16)

# Models ------------------------------------------------------------------
###Check spread of data

hist(data1$SingCnt)
hist(data1$MoveCnt)
hist(data1$RestCnt)
hist(data1$TotalInt)
hist(data2$completed)
hist(log(data2$completed))

data2$percent = data2$completed/(72*4)
hist(data2$percent)
hist(log(data2$percent))
shapiro.test(data2$percent)

ad.test(data2$percent)
hist(log(data1$SingCnt))
hist(log(data1$MoveCnt))
hist(log(data1$RestCnt))
hist(log(data1$total_int))

M1 = glmer(SingCnt ~ year + (1|monthDay/N), data = data1, family = poisson)
summary(M1)
anova(M1)

M1 = lmer(log(SingCnt) ~ year + (1|monthDay/N), data = data1)
#Checking singularity
tt <- getME(M1,"theta")
ll <- getME(M1,"lower")
min(tt[ll==0])
summary(M1)
anova(M1)

M2 = glmer(MoveCnt ~ year + (1|monthDay/N), data = data1, family = poisson)
summary(M2)
anova(M2)

M3 = glmer(RestCnt ~ year + (1|monthDay/N), data = data1, family = poisson)
summary(M3)
anova(M3)

M4 = glmer(total_int ~ year + (1|monthDay/N), data = data1, family = poisson)
summary(M4)
anova(M4)

m5 = lmer(data2$percent ~ year + (1|monthDay), data = data2)
summary(m5)
anova(m5)
confint(m5)
cV = ranef(m5, condVar = TRUE) 
as.data.frame(cV)
# Violin Plots for Posters ------------------------------------------------------------------

#Violin Plot
violin_sing = ggplot(data = data1, aes(x=year, y=SingCnt, group=year)) +
  geom_violin(aes(fill = year),
                  draw_quantiles = c(0.25, 0.5, 0.75))+
  scale_y_continuous(name = "Total Songs\n Produced") +
  theme_classic(base_size = 5)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = cbPalette)

violin_move = ggplot(data1, aes(x=year, y=MoveCnt, group=year)) + 
  geom_violin(aes(fill = year),
              draw_quantiles = c(0.25, 0.5, 0.75))+
  # geom_point(stat="identity", position=position_dodge()) +
  # geom_errorbar(aes(ymin=MoveCntMean-MoveCntSE, ymax=MoveCntMean+MoveCntSE), width=.2,
  #               position=position_dodge(.9)) + 
  scale_y_continuous(name = "Total Moves")+
  theme_classic(base_size =5)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_blank(),
        legend.position = "none"
  )+
  scale_fill_manual(values = cbPalette)
  

violin_rest = ggplot(data1, aes(x=year, y=RestCnt, group=year)) + 
  geom_violin(aes(fill = year),
              draw_quantiles = c(0.25, 0.5, 0.75))+
  # geom_point(stat="identity", position=position_dodge()) +
  # geom_errorbar(aes(ymin=RestCntMean-RestCntSE, ymax=RestCntMean+RestCntSE), width=.2,
  #               position=position_dodge(.9)) + 
  scale_y_continuous(name = "Total Rests \nTaken")+
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size = 5)+
  theme(legend.position = "none"
  )

grid.arrange(violin_sing,violin_move,violin_rest, ncol = 1)

# #Panneled graphs for poster: Bar Graphs - Normal Data ---------------------------------------------

bar_sing = ggplot(data1means, aes(x=year, y=SingCntMean, fill=year)) + 
  geom_bar(stat="identity", 
             position=position_dodge(), 
             size = 10) +
  geom_errorbar(aes(ymin=SingCntMean-SingCntSE, ymax=SingCntMean+SingCntSE), 
                width=.5,
                position=position_dodge(0)) +
  scale_y_continuous(name = "Total Songs") +
  coord_cartesian(ylim = c(200,400)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size =48)+
  theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line.x = element_blank(),
        legend.position = "top"
                )

bar_move = ggplot(data1means, aes(x=year, y=MoveCntMean, fill=year)) + 
  geom_bar(stat="identity", 
           position=position_dodge(),
           size = 10) +
  geom_errorbar(aes(ymin=MoveCntMean-MoveCntSE, ymax=MoveCntMean+MoveCntSE), 
                width=.5,
                position=position_dodge(0)) + 
  scale_y_continuous(name = "Total Moves") +
  coord_cartesian(ylim = c(75,110)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size = 48)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none"
        )

bar_rest = ggplot(data1means, aes(x=year, y=RestCntMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=RestCntMean-RestCntSE, ymax=RestCntMean+RestCntSE), 
                width=.5,
                position=position_dodge(.9)) + 
  scale_y_continuous(name = "Total Rests") +
  coord_cartesian(ylim = c(75,110)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size = 48)+
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none"
        )

grid.arrange(bar_sing,bar_move,bar_rest, ncol = 1)

# Bar Plots - Uniform Climate Change --------------------------------------

bar_sing = ggplot(data3means, aes(x=year, y=SingCntMean, fill=year)) + 
  geom_bar(stat="identity", 
           position=position_dodge(), 
           size = 10) +
  geom_errorbar(aes(ymin=SingCntMean-SingCntSE, ymax=SingCntMean+SingCntSE), 
                width=.5,
                position=position_dodge(0)) +
  scale_y_continuous(name = "Total Songs") +
  coord_cartesian(ylim = c(200,400)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size =48)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "top"
  );bar_sing

bar_move = ggplot(data3means, aes(x=year, y=MoveCntMean, fill=year)) + 
  geom_bar(stat="identity", 
           position=position_dodge(),
           size = 10) +
  geom_errorbar(aes(ymin=MoveCntMean-MoveCntSE, ymax=MoveCntMean+MoveCntSE), 
                width=.5,
                position=position_dodge(0)) + 
  scale_y_continuous(name = "Total Moves") +
  coord_cartesian(ylim = c(75,110)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size = 48)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none"
  ); bar_move

bar_rest = ggplot(data3means, aes(x=year, y=RestCntMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=RestCntMean-RestCntSE, ymax=RestCntMean+RestCntSE), 
                width=.5,
                position=position_dodge(.9)) + 
  scale_y_continuous(name = "Total Rests") +
  coord_cartesian(ylim = c(75,110)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size = 48)+
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none"
  );bar_rest

grid.arrange(bar_sing,bar_move,bar_rest, ncol = 1)

###Graph for Sing Count
#Violin Plot
ggplot(data = data1, aes(x=year, y=SingCnt, group=year, color = year)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.line = element_blank())

#Bar Graph
ggplot(data1means, aes(x=year, y=SingCntMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=SingCntMean-SingCntSE, ymax=SingCntMean+SingCntSE), width=.2,
                position=position_dodge(0)) + 
  theme_classic(base_size = 48)

#Line Graph
ggplot(data1means, aes(x=year, y=SingCntMean, color=year)) + 
  geom_point() +
  geom_errorbar(aes(ymin=SingCntMean-SingCntSE, 
                    ymax=SingCntMean+SingCntSE), 
                width=.2) + 
  theme_classic()

###Graph for Move Count
#Violing Plot
ggplot(data = data1, aes(x=year, y=MoveCnt, group=year, color = year)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_classic()


#Bar Graph
ggplot(data1means, aes(x=year, y=MoveCntMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=MoveCntMean-MoveCntSE, ymax=MoveCntMean+MoveCntSE), width=.2,
                position=position_dodge(.9)) + 
  theme_classic(base_size = 48)

###Graph for Rest Count
#Violin Plot across years
ggplot(data = data1, aes(x=year, y=RestCnt, group=year, color = year)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_classic()

#Bar Graph
ggplot(data1means, aes(x=year, y=RestCntMean, fill=year)) + 
geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=RestCntMean-RestCntSE, ymax=RestCntMean+RestCntSE), width=.2,
                position=position_dodge(.9)) + 
  theme_classic(base_size = 48)

###Graph for Number of total Interactions
#Scatterplot
ggplot(data = data1means, aes(x=year, y=TotalIntMean, group=year, color = year)) +
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=TotalIntMean-TotalIntSE, ymax=TotalIntMean+TotalIntSE), width=0.5,
                position=position_dodge(0)) + 
  theme_classic()

#Violin Plot
ggplot(data = data1, aes(x=year, y=TotalInt, group=year, color = year)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_classic()

#Bar Graph
ggplot(data1means, aes(x=year, y=TotalIntMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=TotalIntMean-TotalIntSE, ymax=TotalIntMean+TotalIntSE), width=.2,
                position=position_dodge(.9)) + 
  theme_classic(base_size = 48)


#Graph for Number of Individuals who successfully contacted all neighbors
ggplot(data = data2, aes(x=time, y=completed, group=year, color = year)) +
  geom_point()+
  theme_classic()

#Graph for total number of interactions overr time frame
ggplot(data = data2means, aes(x=time, y=TotalIntMean, group=year, color = year)) +
  geom_point(size = 5)+
  theme_classic(base_size = 48)



# Box and Whisker Plots - Normal Data -------------------------------------

box_sing = ggplot(data1, aes(x=year, y=SingCnt, fill=year)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Total Songs") +
  #coord_cartesian(ylim = c(200,400)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size =48)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "top"
  );box_sing

box_move = ggplot(data1, aes(x=year, y=MoveCnt, fill=year)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Total Moves") +
  # coord_cartesian(ylim = c(90,110)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size = 48)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none"
  );bar_move

box_rest = ggplot(data1, aes(x=year, y=RestCnt, fill = year)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Total Rests") +
  #coord_cartesian(ylim = c(90,110)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size = 48)+
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none"
  );bar_rest

grid.arrange(bar_sing,bar_move,bar_rest, ncol = 1)


# Uniform Climate Change Boxplots -----------------------------------------

box_sing = ggplot(data_counts, aes(x=year, y=SingCnt, fill=year)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Total Songs") +
  #coord_cartesian(ylim = c(200,400)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size =48)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "top"
  )

box_move = ggplot(data_counts, aes(x=year, y=MoveCnt, fill=year)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Total Moves") +
  # coord_cartesian(ylim = c(90,110)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size = 48)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none"
  )

box_rest = ggplot(data_counts, aes(x=year, y=RestCnt, fill = year)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Total Rests") +
  #coord_cartesian(ylim = c(90,110)) +
  scale_fill_manual(values = cbPalette)+
  theme_classic(base_size = 48)+
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none"
  )

grid.arrange(box_sing,box_move,box_rest, ncol = 1)

data_counts = as.data.frame(rbind(data1,data3))


# Graphing Temperature Trends Across Models -------------------------------

###Loading temperature data from models

#Normal/Current Weather Conditions
load("~/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_052420/data/ERIC_weather_normal/ERIC_weather_normal.Rdata")

wnormal = wdsum
wnormal = wnormal %>%
  filter(!is.na(bin1))
wnormal$model = "normal"
load("~/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_052420/data/ERIC_weather_tair_relh_increase/ERIC_weather_tair_relh_increase.Rdata")

wuniform = wdsum
wuniform = wuniform %>%
  filter(!is.na(bin1))
wuniform$model = "uniform"

load("~/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_052420/data/ERIC_weather_cc_nighttime/ERIC_weather_cc_nighttime.Rdata")

wasymmetric = wdsum
wasymmetric$model = "asymmetric"

normal_means = wnormal %>% 
  filter(!is.na(TAIR)) %>%
  group_by(bin1,year,model) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   TAIRMean = mean(TAIR),
                   TAIRSe = (sd(TAIR)/sqrt(n)),
                   RELHMean = mean(RELH),
                   RELHSe = (sd(RELH)/sqrt(n)),
                   PRESMean = mean(PRES),
                   PRESSe = (sd(PRES)/sqrt(n))
  )

uniform_means = wuniform %>% 
  filter(!is.na(TAIR)) %>%
  group_by(bin1,year,model) %>%
  dplyr::summarize(n = n(), 
                   TAIRMean = mean(TAIR),
                   TAIRSe = (sd(TAIR)/sqrt(n)),
                   RELHMean = mean(RELH),
                   RELHSe = (sd(RELH)/sqrt(n)),
                   PRESMean = mean(PRES),
                   PRESSe = (sd(PRES)/sqrt(n))
  )

asymmetric_means = wasymmetric %>% 
  filter(!is.na(TAIR)) %>%
  group_by(bin1,year,model) %>%
  dplyr::summarize(n = n(), 
                   TAIRMean = mean(FTAIR),
                   TAIRSe = (sd(TAIR)/sqrt(n)),
                   RELHMean = mean(RELH),
                   RELHSe = (sd(RELH)/sqrt(n)),
                   PRESMean = mean(PRES),
                   PRESSe = (sd(PRES)/sqrt(n))
  )


model_conditions = as.data.frame(rbind(normal_means,
                         uniform_means,
                         asymmetric_means))
Song_volume = 85
Song_detection = 30
Song_freq = 7000
source("~/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_052420/src/Atmospheric_sound_attenuation.R")
model_conditions$CallRad <- mapply(aud_range,
                                   Song_volume,
                                   Song_detection,
                                   Song_freq,
                                   model_conditions$TAIRMean,
                                   model_conditions$RELHMean,
                                   model_conditions$PRESMean)

ggplot(data = model_conditions, 
       aes(x=bin1, 
           y=TAIRMean,
           color = year)) +
  geom_line(size = 1,
            aes(linetype=model)) +

  scale_linetype_manual(values=c("dashed",
                                  "solid",
                                  "dotted"))+

  scale_colour_manual(values = c("#E69F00", #yellow
                                 "#999999", #gray
                                 "#56B4E9" #blue,
  ))+
  theme_classic(base_size = 20)
# geom_errorbar(aes(ymin = TAIRMean-TAIRSe, 
#                   ymax = TAIRMean+TAIRSe))+
# scale_y_continuous(name = "Percent Contacted All\n Neighbors", 
# limits=c(0.0, 1.0))+
# scale_x_continuous(name =  "Time (min)")+

ggplot(data = model_conditions, 
       aes(x=bin1, 
           y=RELHMean,
           color = year)) +
  geom_point(size = 1, aes(shape = model)) +
  geom_line(size = 1,
            aes(linetype=model)) +
  
  scale_linetype_manual(values=c("dashed",
                                 "solid",
                                 "dotted"))+
  
  scale_colour_manual(values = c("#E69F00", #yellow
                                 "#999999", #gray
                                 "#56B4E9" #blue,
  ))+
  theme_classic(base_size = 20)
# geom_errorbar(aes(ymin = TAIRMean-TAIRSe, 
#                   ymax = TAIRMean+TAIRSe))+
# scale_y_continuous(name = "Percent Contacted All\n Neighbors", 
# limits=c(0.0, 1.0))+
# scale_x_continuous(name =  "Time (min)")+

ggplot(data = model_conditions, 
       aes(x=TAIRMean, 
           y=CallRad,
           color = year)) +
  # geom_point(size = 1, aes(shape = model)) +
  geom_line(size = 1,
            aes(linetype=model)) +
  
  scale_linetype_manual(values=c("dashed",
                                 "solid",
                                 "dotted"))+
  
  scale_colour_manual(values = c("#E69F00", #yellow
                                 "#999999", #gray
                                 "#56B4E9" #blue,
  ))+
  theme_classic(base_size = 20)
# geom_errorbar(aes(ymin = TAIRMean-TAIRSe, 
#                   ymax = TAIRMean+TAIRSe))+
# scale_y_continuous(name = "Percent Contacted All\n Neighbors", 
# limits=c(0.0, 1.0))+
# scale_x_continuous(name =  "Time (min)")+