################Stats Analysis###################

library(nlme)
library(lme4)
library(plyr)
library(dplyr)
library(lmerTest)
library(ggplot2)

#setwd("/cloud/project/data_clean/normal")
setwd("C:/Users/Jeremy/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/data_clean/normal")

mdata1 = "C:/Users/Jeremy/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/data_clean/normal/results_normal.Rdata"
mdata2 = "C:/Users/Jeremy/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Aridity Agent Based Model/data_clean/normal/completion_normal.Rdata"

load(mdata1)
load(mdata2)

data1 = Results_normal #loading Results data
data2 = completion_normal #loading Completion data

#Add treatment (year) to the dataframe
data1$treatment = substr(data1$date, 1,4) #creates year vector from date
data1$monthDay = substr(data1$date, 6,10) #creates month-day vector from date

data2$treatment = substr(data2$date, 1,4) #creates year vector from date
data2$monthDay = substr(data2$date, 6,10) #creates month-day vector from date

data1 %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(monthDay) %>%
  summarize(mean = mean(SingCnt), n = n())
colnames(data1_means) = c("treatment", "date", "monthDay","ind", "meanSingCnt") #Rename columns


# Calculating Means for Completion Data -----------------------------------

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  #require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

data3 <- data_summary(data2, 
                      varname="SingCnt", 
                      groupnames=c("treatment", "monthDay"))
# Convert dose to a factor variable
data3$monthDay=as.factor(data3$monthDay)
head(data3)

data4 <- data_summary(data2, 
                      varname="MoveCnt", 
                      groupnames=c("treatment", "monthDay"))
# Convert dose to a factor variable
data4$monthDay=as.factor(data4$monthDay)
head(data4)

data5 <- data_summary(data2, 
                      varname="RestCnt", 
                      groupnames=c("treatment", "monthDay"))
# Convert dose to a factor variable
data5$monthDay=as.factor(data5$monthDay)
head(data5)

data6 <- data_summary(data2, 
                      varname="TotalInt", 
                      groupnames=c("treatment", "monthDay"))
# Convert dose to a factor variable
data6$monthDay=as.factor(data6$monthDay)
head(data6)

data2_means = as.data.frame(cbind(data3$treatment, data3$monthDay, 
                                  data3$SingCnt, data3$sd, 
                                  data4$MoveCnt, data4$sd, 
                                  data5$RestCnt, data5$sd, 
                                  data6$TotalInt, data6$sd)
)
colnames(data2_means) = c("treatment","monthDay",
                          "SingCntMean","SingCntSD",
                          "MoveCntMean","MoveCntSD",
                          "RestCntMean", "RestCntSD",
                          "TotalInt", "TotalIntSD")

# Graphs ------------------------------------------------------------------

#Plot Means
ggplot(data = data1_means, aes(x=date, y=meanSingCnt, fill = treatment)) +
  geom_bar(stat = "identity")+
  theme_classic()

hist(log(data2$percent))
hist(data2$completed)
hist(log(data2$completed))

M1 = lmer(SingCnt ~ treatment + (1|date/N), data = data1)
summary(M1)
coefs <- data.frame(coef(summary(M1)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
anova(M1)

M2 = lmer(MoveCnt ~ treatment + (1|date/N), data = data1)
summary(M2)
anova(M2)

M3 = lmer(RestCnt ~ treatment + (1|date/N), data = data1)
summary(M3)
anova(M3)

M3.5 = lmer(total_int ~ treatment + (1|date/N), data = data1)
summary(M3.5)
anova(M3.5)

M4 = lm(scale(completed) ~ treatment, data = data2)
summary(M4)
coefs <- data.frame(coef(summary(M4)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

M5 = lm(scale(percent) ~ treatment +(1|monthDay), data = data2)
summary(M5)
anova(M5)

M6 = lm(scale(TotalInt) ~ treatment, data = data2)
summary(M6)
anova(M6)

###Graph for Sing Count
#Violin Plot
ggplot(data = data1, aes(x=treatment, y=SingCnt, group=treatment, color = treatment)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_classic()

#Bar Graph
ggplot(data3, aes(x=monthDay, y=SingCnt, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=SingCnt-sd, ymax=SingCnt+sd), width=.2,
                position=position_dodge(.9)) + 
  theme_classic()

#Line Graph
ggplot(data3, aes(x=monthDay, y=SingCnt, color=treatment)) + 
  geom_point() +
  geom_errorbar(aes(ymin=SingCnt-sd, ymax=SingCnt+sd), width=.2) + 
  theme_classic()

###Graph for Move Count
#Violing Plot
ggplot(data = data1, aes(x=treatment, y=MoveCnt, group=treatment, color = treatment)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_classic()

#Bar Graph
ggplot(data4, aes(x=monthDay, y=MoveCnt, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=MoveCnt-sd, ymax=MoveCnt+sd), width=.2,
                position=position_dodge(.9)) + 
  theme_classic()

###Graph for Rest Count
#Violin Plot across years
ggplot(data = data1, aes(x=treatment, y=RestCnt, group=treatment, color = treatment)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_classic()

#Bar Graph
ggplot(data5, aes(x=monthDay, y=RestCnt, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=RestCnt-sd, ymax=RestCnt+sd), width=.2,
                position=position_dodge(.9)) + 
  theme_classic()

###Graph for Number of total Interactions
#Scatterplot
ggplot(data = data2, aes(x=time, y=TotalInt, group=treatment, color = treatment)) +
  geom_point()+
  theme_classic()

#Violin Plot
ggplot(data = data1, aes(x=treatment, y=total_int, group=treatment, color = treatment)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_classic()

#Bar Graph
ggplot(data6, aes(x=monthDay, y=TotalInt, fill=treatment)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=TotalInt-sd, ymax=TotalInt+sd), width=.2,
                position=position_dodge(.9)) + 
  theme_classic()


#Graph for Number of Individuals who successfully contacted all neighbors
ggplot(data = data2, aes(x=time, y=completed, group=treatment, color = treatment)) +
  geom_point()+
  theme_classic()

#Graph for Percent of Individuals who successfully contacted all neighbors
ggplot(data = data2, aes(x=time, y=percent, group=treatment, color = treatment)) +
  geom_point()+
  theme_classic()




