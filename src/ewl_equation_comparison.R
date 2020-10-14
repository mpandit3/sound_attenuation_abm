###########################################
#####Comparing EWL to no EWL Models#######
###########################################

#Aridity Climate Change Conditions
#EWL Equation included in model
m_ewl_results = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/cc_aridity/results_aridity_cc.Rdata"
m_ewl_comps = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/cc_aridity/completion_aridity_cc.Rdata"

# m_ewl_results = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/nighttime_cc/results_nighttime_cc.Rdata"
# m_ewl_comps = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_090120/data_clean/nighttime_cc/completion_nighttime_cc.Rdata"

load(m_ewl_results)
load(m_ewl_comps)

m_ewl_results = Results_aridity_cc
m_ewl_comps = completion_aridity_cc

# m_ewl_results = Results_nighttime_cc #loading Results data - summarized by individual
# m_ewl_comps = completion_nighttime_cc #loading Completion data - summarized by time-step

#Add year (year) to the dataframe
m_ewl_results$year = as.factor((substr(m_ewl_results$date, 1,4))) #creates year vector from date
m_ewl_results$month = as.factor(substr(m_ewl_results$date, 6,7)) #creates month-day vector from date
m_ewl_results$day = as.factor(substr(m_ewl_results$date,9,10))
m_ewl_results$monthDay = as.factor(substr(m_ewl_results$date,6,10))

m_ewl_comps$year = as.factor(substr(m_ewl_comps$date, 1,4)) #creates year vector from date
m_ewl_comps$month = as.factor(substr(m_ewl_comps$date, 6,7)) #creates month-day vector from date
m_ewl_comps$day = as.factor(substr(m_ewl_comps$date,9,10))
m_ewl_comps$monthDay = as.factor(substr(m_ewl_comps$date,6,10))

m_ewl_comps_means = m_ewl_comps %>% 
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
m_ewl_comps_means$model = "ewl"



#No EWL equation in model
m_no_ewl_results = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_052420/data_clean/cc_aridity/results_aridity_cc.Rdata"
m_no_ewl_comps = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_052420/data_clean/cc_aridity/completion_aridity_cc.Rdata"

# m_no_ewl_results = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_052420/data_clean/nighttime_cc/results_nighttime_cc.Rdata"
# m_no_ewl_comps = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/abm_month_timeframe_052420/data_clean/nighttime_cc/completion_nighttime_cc.Rdata"

load(m_no_ewl_results)
load(m_no_ewl_comps)

m_no_ewl_results = Results_aridity_cc #loading Results data - summarized by individual
m_no_ewl_comps = completion_aridity_cc #loading Completion data - summarized by time-step

# m_no_ewl_results = Results_aridity_cc #loading Results data - summarized by individual
# m_no_ewl_comps = completion_aridity_cc #loading Completion data - summarized by time-step

#Add year (year) to the dataframe
m_no_ewl_results$year = as.factor((substr(m_no_ewl_results$date, 1,4))) #creates year vector from date
m_no_ewl_results$month = as.factor(substr(m_no_ewl_results$date, 6,7)) #creates month-day vector from date
m_no_ewl_results$day = as.factor(substr(m_no_ewl_results$date,9,10))
m_no_ewl_results$monthDay = as.factor(substr(m_no_ewl_results$date,6,10))

m_no_ewl_comps$year = as.factor(substr(m_no_ewl_comps$date, 1,4)) #creates year vector from date
m_no_ewl_comps$month = as.factor(substr(m_no_ewl_comps$date, 6,7)) #creates month-day vector from date
m_no_ewl_comps$day = as.factor(substr(m_no_ewl_comps$date,9,10))
m_no_ewl_comps$monthDay = as.factor(substr(m_no_ewl_comps$date,6,10))
m_no_ewl_comps$percent = m_no_ewl_comps$completed/(72*4)

m_no_ewl_comps_means = m_no_ewl_comps %>% 
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
m_no_ewl_comps_means$model = "no ewl"

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

#Group dataframes together
ewl_comparison = as.data.frame(rbind(m_ewl_comps_means,
                                     m_no_ewl_comps_means))

ewl_comparison_hot = ewl_comparison %>%
  group_by(year) %>%
  filter(year == "2011" | year == "2081")
ewl_comparison_hot$treatment = "Hot"

ewl_comparison_medium = ewl_comparison %>%
  group_by(year) %>%
  filter(year == "2015" | year == "2085")

ewl_comparison_medium$treatment = "Medium"

ewl_comparison_cold = ewl_comparison %>%
  group_by(year) %>%
  filter(year == "2019" | year == "2089")
ewl_comparison_cold$treatment = "Cold"

ewl_comparison2 = as.data.frame(rbind(ewl_comparison_hot,
                                   ewl_comparison_medium,
                                   ewl_comparison_cold))

ewl_comparison3 = ewl_comparison2 %>% 
  group_by(time,year)

ewl_comparison3$treatment = factor(ewl_comparison3$treatment,
                                   levels = c("Hot",
                                              "Medium",
                                              "Cold"))
ggplot(data = ewl_comparison3, 
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
                              "solid"))+
scale_y_continuous(name = "Percent Contacted All\n Neighbors", 
                   limits=c(0.0, 1.0))+
  scale_x_continuous(name =  "Time (min)")+
  scale_colour_manual(values = cbPalette)+
  theme_classic(base_size = 10)

  