################Stats Analysis###################

library(nlme)
library(lme4)
library(dplyr)
library(lmerTest)
library(ggplot2)
library(nortest)

setwd("/cloud/project/data_clean/normal")

mdata1 = "/cloud/project/data_clean/normal/results_normal.Rdata"
mdata2 = "/cloud/project/data_clean/normal/completion_normal.Rdata"

load(mdata1)
load(mdata2)

data1 = Results_normal #loading Results data - summarized by individual
data2 = completion_normal #loading Completion data - summarized by time-step

#Add year (year) to the dataframe
data1$year = as.factor((substr(data1$date, 1,4))) #creates year vector from date
data1$monthDay = as.factor(substr(data1$date, 6,10)) #creates month-day vector from date

data2$year = as.factor(substr(data2$date, 1,4)) #creates year vector from date
data2$monthDay = as.factor(substr(data2$date, 6,10)) #creates month-day vector from date
data2$actualPercent = data2$completed/576 #Calculating percent from the number of birds across the 8 iteractions for each monthDay

data1means = data1 %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(monthDay, year) %>%
  dplyr::summarize(n = n(), #need to use dplyr:: because other libraries have the summarize function
                   SingCntMean = mean(SingCnt), 
                   SingCntSE = (sd(SingCnt)/sqrt(n)),
                   MoveCntMean = mean(MoveCnt),
                   MoveCntSE = (sd(MoveCnt)/sqrt(n)),
                   RestCntMean = mean(RestCnt),
                   RestCntSE = (sd(RestCnt)/sqrt(n)),
                   TotalIntMean = mean(total_int),
                   TotalIntSE = (sd(total_int)/sqrt(n))
                   )

# have 101 individuals in the dataframe
# colnames(data1_means) = c("year", 
#                           "date", 
#                           "monthDay",
#                           "ind", 
#                           "meanSingCnt", 
#                           "nSingCnt",
#                           "meanMoveCnt",
#                           "nMoveCnt") #Rename columns

ggplot(data1means, aes(x=monthDay, y=SingCntMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=SingCntMean-SingCntSE, ymax=SingCntMean+SingCntSE), width=.2,
                position=position_dodge(.9)) + 
  theme_classic()

data2means = data2 %>% #Finds Means of data *cannot use dataframe name in dplyr!
  group_by(year, monthDay) %>%
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
  
ggplot(data2means, aes(x=monthDay, y=PercentMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=PercentMean-PercentsE, ymax=PercentMean+PercentsE), width=.2,
                position=position_dodge(.9)) + 
  theme_classic()

ggplot(data2means, aes(x=monthDay, y=CompletedMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=CompletedMean-CompletedSE, ymax=CompletedMean+CompletedSE), width=.2,
                position=position_dodge(.9)) + 
  theme_classic()


# Models ------------------------------------------------------------------
###Check spread of data

hist(data1$SingCnt)
hist(data1$MoveCnt)
hist(data1$RestCnt)
hist(data1$total_int)

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
summary(M1.5)
anova(M1.5)

M2 = glmer(MoveCnt ~ year + (1|monthDay/N), data = data1, family = poisson)
summary(M2)
anova(M2)

M3 = glmer(RestCnt ~ year + (1|monthDay/N), data = data1, family = poisson)
summary(M3)
anova(M3)

M4 = glmer(total_int ~ year + (1|monthDay/N), data = data1, family = poisson)
summary(M4)
anova(M4)

M5 = glm(completed ~ year, data = data2, family = poisson)
summary(M5)
anova(M5)

M6 = glm(completed ~ percent, data = data2, family = poisson)
summary(M6)
anova(M6)

# Graphs ------------------------------------------------------------------
###Graph for Sing Count
#Violin Plot
ggplot(data = data1, aes(x=year, y=SingCntMean, group=year, color = year)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_classic()

#Bar Graph
ggplot(data1means, aes(x=monthDay, y=SingCntMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=SingCntMean-SingCntSE, ymax=SingCntMean+SingCntSE), width=.2,
                position=position_dodge(.9)) + 
  theme_classic()

#Line Graph
ggplot(data1means, aes(x=monthDay, y=SingCntMean, color=year)) + 
  geom_point() +
  geom_errorbar(aes(ymin=SingCntMean-SingCntSE, 
                    ymax=SingCntMean+SingCntSE), 
                width=.2) + 
  theme_classic()

###Graph for Move Count
#Violing Plot
ggplot(data = data1means, aes(x=year, y=MoveCntMean, group=year, color = year)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_classic()

#Bar Graph
ggplot(data1means, aes(x=monthDay, y=MoveCntMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=MoveCntMean-MoveCntSE, ymax=MoveCntMean+MoveCntSE), width=.2,
                position=position_dodge(.9)) + 
  theme_classic()

###Graph for Rest Count
#Violin Plot across years
ggplot(data = data1means, aes(x=year, y=RestCntMean, group=year, color = year)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_classic()

#Bar Graph
ggplot(data1means, aes(x=monthDay, y=RestCntMean, fill=year)) + 
geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=RestCntMean-RestCntSE, ymax=RestCntMean+RestCntSE), width=.2,
                position=position_dodge(.9)) + 
  theme_classic()

###Graph for Number of total Interactions
#Scatterplot
ggplot(data = data1means, aes(x=time, y=TotalIntMean, group=year, color = year)) +
  geom_point()+
  theme_classic()

#Violin Plot
ggplot(data = data1means, aes(x=year, y=TotalIntMean, group=year, color = year)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  theme_classic()

#Bar Graph
ggplot(data1means, aes(x=monthDay, y=TotalIntMean, fill=year)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=TotalIntMean-TotalIntSE, ymax=TotalIntMean+TotalIntSE), width=.2,
                position=position_dodge(.9)) + 
  theme_classic()


#Graph for Number of Individuals who successfully contacted all neighbors
ggplot(data = data2, aes(x=time, y=completed, group=year, color = year)) +
  geom_point()+
  theme_classic()

#Graph for Percent of Individuals who successfully contacted all neighbors
ggplot(data = data2, aes(x=time, y=percent, group=year, color = year)) +
  geom_point()+
  theme_classic()


#Graph for completed of Individuals/808 (number of individuals who have 6 neighbors) 
#who successfully contacted all neighbors
ggplot(data = data2, aes(x=time, y=actualPercent, group=year, color = year)) +
  geom_point()+
  theme_classic()

#Graph for completed of Individuals/101 (number of individuals who have 6 neighbors) 
#who successfully contacted all neighbors
ggplot(data = data2, aes(x=time, y=TotalInt, group=year, color = year)) +
  geom_point()+
  geom_smooth(method = "lm") +
  theme_classic()
