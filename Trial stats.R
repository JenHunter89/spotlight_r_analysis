install.packages("tidyverse")
install.packages("haven")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("readr")
install.packages("plyr")
install.packages("lubridate")
install.packages("openxlsx")
install.packages("data.table")
install.packages("tidyselect")

library(tidyverse)
library(haven)
library(Hmisc)
library(dplyr)
library(readr)
library(plyr)
library(lubridate)
library(openxlsx)
library(data.table)
library(tidyselect)

setwd("/Users/jenniferhunter/Documents/SH PhD Project")

statsdata <- read_csv("Data/trajectories.csv")

## ====
## This looks at stats for complete data, used in write up 
## ====

## ++++++++
## Naming:
## var1, var2 -> var1 = ward measurement, var2 = icu measurement
## var1wt, var2wt = weighting for that vairable
## full_score1/2, partABG_score1/2, part_score1/2
## var1norm, var2norm
## scalescorefull1/2, scalescorepartABG1/2, scalescorepart1/2
## ++++++++

## ======
## Number of patients with full, partial + ABG, and partial scores
## =====
countfull <- length(which(!is.na(statsdata$full_scoretraj)))
countpart_ABG <- length(which(!is.na(statsdata$partABG_scoretraj)))
countpart <- length(which(!is.na(statsdata$part_scoretraj)))

## ======
## Used for median and IQR for ward APS, admission APS, trajectory and time2icu for fullscore only
## ======

fullscoredata <- statsdata %>% filter(!is.na(full_scoretraj))
describe(fullscoredata$full_score1)
describe(fullscoredata$full_score2)
describe(fullscoredata$full_scoretraj)
describe(fullscoredata$time2icu)
describe(fullscoredata$age)
describe(fullscoredata$dead28)

## ======
## Box plot trajectories divided by completeness of data
## ======

dfplot2 <- statsdata %>% select(full_scoretraj, partABG_scoretraj, part_scoretraj, sepsis)

dfmelt2 <- melt(dfplot2, id=c("sepsis"))
dfmelt2$sepsisfactor <- ifelse(dfmelt2$sepsis < 3 | is.na(dfmelt2$sepsis), 0, 1)
describe(dfmelt2$sepsis)
dfmelt2$sepsisfactor <- as.factor(dfmelt2$sepsisfactor)


ggplot(data = dfmelt2) +
  geom_boxplot(aes(x = variable, y = value, color = sepsis))

## ======
## T-test for difference between means of score based on sepsis where sepsis is 
## ======

statsdata$sepsisfactor <- ifelse(statsdata$sepsis < 3 | is.na(statsdata$sepsis), 0, 1)

clipdata <- statsdata %>% filter(clipfull == 1)

t.test(statsdata$full_scoretraj[statsdata$sepsisfactor == 1], statsdata$full_scoretraj[statsdata$sepsisfactor == 0], paired = FALSE)
t.test(statsdata$scale_fulltraj[statsdata$sepsisfactor == 1], statsdata$scale_fulltraj[statsdata$sepsisfactor == 0], paired = FALSE)

t.test(clipdata$full_scoretraj[clipdata$sepsisfactor == 1], clipdata$full_scoretraj[clipdata$sepsisfactor == 0], paired = FALSE)
t.test(clipdata$scale_fulltraj[clipdata$sepsisfactor == 1], clipdata$scale_fulltraj[clipdata$sepsisfactor == 0], paired = FALSE)

## statistically significant difference in mortality in sepsis and non sepsis in ICU and at 28 days
t.test(statsdata$dead28[statsdata$sepsisfactor == 1], statsdata$dead28[statsdata$sepsisfactor == 0], paired = FALSE)
t.test(statsdata$dead_icu[statsdata$sepsisfactor == 1], statsdata$dead_icu[statsdata$sepsisfactor == 0], paired = FALSE)


## ======
## Plot trajectory against initial severity and ITU severity
## ======

clipdata <- filter(statsdata, clipfull == 1)

ggplot(data = clipdata) +
  geom_point(aes(x = clipdata$scale_fulltraj, y = clipdata$scalescorefull2))

ggplot(data = statsdata) +
  geom_point(aes(x = statsdata$partABG_scoretraj, y = statsdata$partABG_score2))

ggplot(data = statsdata) +
  geom_point(aes(x = statsdata$part_scoretraj, y = statsdata$part_score2))



statsdata$sepsis <- as.factor(statsdata$sepsis)
graphdata <- filter(statsdata, !is.na(full_score1) & !is.na(full_score2) & !is.na(sepsis) & icu_admit<quantile(icu_admit, 0.9))

describe(statsdata$full_scoretraj)

ggplot(data = graphdata) +
  geom_point(aes(x = 1, y = full_score1)) +
  geom_point(aes(x = 6, y = (full_scoretraj*6))) + 
  geom_segment(aes(x = 1, y = full_score1, xend = 6, yend = (full_scoretraj*6), colour = sepsis)) +
  facet_wrap(~sepsis, nrow = 2)

ggplot(data = graphdata) +
  geom_point(aes(x = 1, y = full_score1)) +
  geom_point(aes(x = 6, y = (full_scoretraj*6))) + 
  geom_segment(aes(x = 1, y = full_score1, xend = 6, yend = (full_scoretraj*6), colour = sepsis)) +
  facet_wrap(~sepsis, nrow = 2)

ggplot(data = graphdata) +
  geom_point(aes(x = (v_timestamp - v_timestamp + 1), y = full_score1)) +
  geom_point(aes(x = (icu_admit - v_timestamp + 1), y = full_score2)) + 
  geom_segment(aes(x = v_timestamp - v_timestamp + 1, y = full_score1, xend = icu_admit - v_timestamp + 1, yend = full_score2, colour = sepsis)) + 
  facet_wrap(~sepsis, nrow = 2)

## ======
## Testing for normal distribution
## Note: full_scoretraj negatively skewed, full_score1 and full_score2 are positivley skewed
## Have tried log and sqrt transformation for 
## Have tried exponential transformation for 
## ======


shapiro.test(statsdata$full_scoretraj)
ggplot(data = statsdata) + 
  geom_histogram(aes(x = statsdata$full_scoretraj), binwidth = 2)

shapiro.test(statsdata$full_score1)
ggplot(data = statsdata) + 
  geom_histogram(aes(x = statsdata$full_score1), binwidth = 2)

shapiro.test(statsdata$full_score2)
ggplot(data = statsdata) + 
  geom_histogram(aes(x = statsdata$full_score2), binwidth = 2)

shapiro.test(statsdata$scale_fulltraj)
ggplot(data = statsdata) + 
  geom_histogram(aes(x = statsdata$scale_fulltraj), binwidth = 5)

## =====
## ANOVA
## Note: this is not valid as not normally distributed
## =====

aov.fullscoretraj <- aov(statsdata$full_scoretraj ~ statsdata$sepsis, data = statsdata) 
summary(aov.fullscoretraj)

aov.scale_fulltraj <- aov(statsdata$scale_fulltraj ~ statsdata$sepsis, data = statsdata)
summary(aov.scale_fulltraj)

man.allscores <- manova(cbind(statsdata$full_scoretraj, statsdata$partABG_scoretraj, statsdata$part_scoretraj) ~ statsdata$sepsis, data = statsdata) 
summary(man.allscores)

## =====
## Kruskal Test 
## Need to see if can test for confounding factors with multivariate - may need to perform other tests to look for significant difference in confounding factors of age and time2icu
## =====

## Test for ICNARC APS trajectory by sepsis binary factor and sepsis likelihood groups

krusktesttraj <- kruskal.test(full_scoretraj ~ sepsisfactor, data = statsdata)
krusktesttraj

krusktesttraj2 <- kruskal.test(full_scoretraj ~ sepsis, data = statsdata)
krusktesttraj2

medianINCARCsepsis <- quantile(statsdata$full_scoretraj[statsdata$sepsisfactor == 1], 0.5, na.rm = TRUE)
medianINCARCsepsis

medianICNARCnosepsis <- quantile(statsdata$full_scoretraj[statsdata$sepsisfactor == 0], 0.5, na.rm = TRUE)
medianICNARCnosepsis

describe(statsdata$full_scoretraj)

## Test for ICNARC APS fullscore ward assessment and sepsis groups

krusktestfs1 <- kruskal.test(full_score1 ~ sepsisfactor, data = statsdata)
krusktestfs1

krusktesttraj2 <- kruskal.test(full_score1 ~ sepsis, data = statsdata)
krusktesttraj2

medianfs1sepsis <- quantile(statsdata$full_score1[statsdata$sepsisfactor == 1], 0.5, na.rm = TRUE)
medianfs1sepsis

medianfs1nosepsis <- quantile(statsdata$full_score1[statsdata$sepsisfactor == 0], 0.5, na.rm = TRUE)
medianfs1nosepsis

## Test for scale trajectory, by sepsis binary factor and sepsis likelihood groups
krusktestscale <- kruskal.test(scale_fulltraj ~ sepsisfactor, data = statsdata)
krusktestscale

krusktestscale2 <- kruskal.test(scale_fulltraj ~ sepsis, data = statsdata)
krusktestscale2

medianscalesepsis <- quantile(statsdata$scale_fulltraj[statsdata$sepsisfactor == 1], 0.5, na.rm = TRUE)
medianscalesepsis

medianscalenosepsis<- quantile(statsdata$scale_fulltraj[statsdata$sepsisfactor == 0], 0.5, na.rm = TRUE)
medianscalenosepsis

length(which(statsdata$sepsis >= 3))
length(which(statsdata$sepsis == 1 | statsdata$sepsis == 2))

## ======
## Aim: logistic regression to determine whether sepsis state, trajectory, and ward and ITU ICNARC scores are independent predictors of mortality 
## However, see section below; only linearity 
## ======
## !!! Note: you have not tested for linearity or normal distribution , need to do this, Katz 43

## Univariate logistic regression 
lapply(c("sepsisfactor","full_scoretraj","full_score1","full_score2", "age"),

function(var) {
  
  formula    <- as.formula(paste("dead28 ~", var))
  res.logist <- glm(formula, data = clipdata, family = binomial)
  
  summary(res.logist)
})

## Multiple logistic regression 

multvar1 <- glm(dead28 ~ sepsis + full_scoretraj + full_score1 + age + lactrajnorm, data = clipdata, family = binomial(link = "logit"))
summary(multvar1)         


## ======
## Checking for linearity full_scoretraj
## ======

describe(statsdata$full_scoretraj)
## -52 to 30; brackets -60-50, -50-40, -40-30 etc to 20-30
statsdata$fs6050 <- ifelse(statsdata$full_scoretraj > -60 & statsdata$full_scoretraj <= -50, 1, 0)
statsdata$fs5040 <- ifelse(statsdata$full_scoretraj > -50 & statsdata$full_scoretraj <= -40, 1, 0)
statsdata$fs4030 <- ifelse(statsdata$full_scoretraj > -40 & statsdata$full_scoretraj <= -30, 1, 0)
statsdata$fs3020 <- ifelse(statsdata$full_scoretraj > -30 & statsdata$full_scoretraj <= -20, 1, 0)
statsdata$fs2010 <- ifelse(statsdata$full_scoretraj > -20 & statsdata$full_scoretraj <= -10, 1, 0)
statsdata$fs1000 <- ifelse(statsdata$full_scoretraj > -10 & statsdata$full_scoretraj <= 0, 1, 0)
statsdata$fs0010 <- ifelse(statsdata$full_scoretraj > 0 & statsdata$full_scoretraj <= 10, 1, 0)
statsdata$fs1020 <- ifelse(statsdata$full_scoretraj > 10 & statsdata$full_scoretraj <= 20, 1, 0)
statsdata$fs2030 <- ifelse(statsdata$full_scoretraj > 20 & statsdata$full_scoretraj <= 30, 1, 0)

## Univariate logistic regression 

resultstab <- data.table(C1 = as.numeric(1:9) * 10 - 35, C2 = NA)
resultstab[,2] <- c(lapply(c("fs6050","fs5040","fs4030","fs3020", "fs2010","fs1000", "fs0010", "fs1020", "fs2030"),
       
       function(var) {
         
         formula    <- as.formula(paste("dead28 ~", var))
         res.logist <- glm(formula, data = statsdata, family = binomial)
         
         summary(res.logist)
         var <- summary(res.logist)$coefficients[2,1]
       }))
resultstab$C1 <- as.numeric(resultstab$C1)
resultstab$C2 <- as.numeric(resultstab$C2)

ggplot(data = resultstab) +
  geom_point(aes(x = C1, y = C2))

## ======
## Checking for linearity full_score2
## ======

describe(statsdata$full_score2)
## 0 to 60; brackets 0-10, 10-20, 20-30 etc to 50-60
statsdata$f2score00.10 <- ifelse(statsdata$full_score2 > 0 & statsdata$full_score2 <= 10, 1, 0)
statsdata$f2score10.20 <- ifelse(statsdata$full_score2 > 10 & statsdata$full_score2 <= 20, 1, 0)
statsdata$f2score20.30 <- ifelse(statsdata$full_score2 > 20 & statsdata$full_score2 <= 30, 1, 0)
statsdata$f2score30.40 <- ifelse(statsdata$full_score2 > 30 & statsdata$full_score2 <= 40, 1, 0)
statsdata$f2score40.50 <- ifelse(statsdata$full_score2 > 40 & statsdata$full_score2 <= 50, 1, 0)
statsdata$f2score50.60 <- ifelse(statsdata$full_score2 > 50 & statsdata$full_score2 <= 60, 1, 0)

## Univariate logistic regression 

resultstab2 <- data.table(C1 = as.numeric(1:6) * 10 - 5, C2 = NA)

resultstab2[,2] <- lapply(c("f2score00.10","f2score10.20","f2score20.30","f2score30.40", "f2score40.50","f2score50.60"),
                           
                           function(var) {
                             
                             formula    <- as.formula(paste("dead28 ~", var))
                             logregfscore <- glm(formula, data = statsdata, family = binomial)
                             
                             summary(logregfscore)
                             var <- summary(logregfscore)$coefficients[2,1]
                           })
resultstab2$C1 <- as.numeric(resultstab2$C1)
resultstab2$C2 <- as.numeric(resultstab2$C2)

ggplot(data = resultstab2) +
  geom_point(aes(x = C1, y = C2))


## ======
## Checking for linearity full_score1
## ======

describe(statsdata$full_score1)
## 0 to 50; brackets 0-10, 10-20, 20-30 etc to 40-50
statsdata$f1score00.10 <- ifelse(statsdata$full_score1 > 0 & statsdata$full_score1 <= 10, 1, 0)
statsdata$f1score10.20 <- ifelse(statsdata$full_score1 > 10 & statsdata$full_score1 <= 20, 1, 0)
statsdata$f1score20.30 <- ifelse(statsdata$full_score1 > 20 & statsdata$full_score1 <= 30, 1, 0)
statsdata$f1score30.40 <- ifelse(statsdata$full_score1 > 30 & statsdata$full_score1 <= 40, 1, 0)
statsdata$f1score40.50 <- ifelse(statsdata$full_score1 > 40 & statsdata$full_score1 <= 50, 1, 0)

## Univariate logistic regression 

resultstab3 <- data.table(C1 = as.numeric(1:5) * 10 - 5, C2 = NA)

resultstab3[,2] <- lapply(c("f1score00.10","f1score10.20","f1score20.30","f1score30.40", "f1score40.50"),
                          
                          function(var) {
                            
                            formula    <- as.formula(paste("dead28 ~", var))
                            logregfscore <- glm(formula, data = statsdata, family = binomial)
                            
                            summary(logregfscore)
                            var <- summary(logregfscore)$coefficients[2,1]
                          })
resultstab3$C1 <- as.numeric(resultstab3$C1)
resultstab3$C2 <- as.numeric(resultstab3$C2)

ggplot(data = resultstab3) +
  geom_point(aes(x = C1, y = C2))
