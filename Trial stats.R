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

## ======
## Box plot trajectories divided by sepsis category
## ======

dfplot2 <- statsdata %>% select(full_scoretraj, partABG_scoretraj, part_scoretraj, sepsis)

dfmelt2 <- melt(dfplot2, id=c("sepsis"))
dfmelt2$sepsisfactor <- ifelse(dfmelt2$sepsis < 3 | is.na(dfmelt2$sepsis), 0, 1)
describe(dfmelt2$sepsis)
dfmelt2$sepsisfactor <- as.factor(dfmelt2$sepsisfactor)

ggplot(data = dfmelt2) +
  geom_boxplot(aes(x = variable, y = value, color = sepsisfactor))

## ======
## T-test for difference between means of score based on sepsis
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

ggplot(data = clipdata[clipdata$scalef]) +
  geom_point(aes(x = clipdata$scale_fulltraj, y = clipdata$scalescorefull2))

ggplot(data = statsdata) +
  geom_point(aes(x = statsdata$partABG_scoretraj, y = statsdata$partABG_score2))

ggplot(data = statsdata) +
  geom_point(aes(x = statsdata$part_scoretraj, y = statsdata$part_score2))

## ======
## Aim: logistic regression to determine whether sepsis state, trajectory, and ward and ITU ICNARC scores are independent predictors of mortality 
## ======
## !!! Note: you have not tested for linearity, need to do this, Katz 43

## Univariate logistic regression 
lapply(c("sepsisfactor","full_scoretraj","full_score1","full_score2", "age"),

function(var) {
  
  formula    <- as.formula(paste("dead28 ~", var))
  res.logist <- glm(formula, data = clipdata, family = binomial)
  
  summary(res.logist)
})

## multiple logistic regression 

multvar1 <- glm(dead28 ~ sepsisfactor + scale_fulltraj + scalescorefull2 + scalescorefull1 + age, data = clipdata, family = binomial(link = "logit"))
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
statsdata$fscore00.10 <- ifelse(statsdata$full_scoretraj > 0 & statsdata$full_scoretraj <= 10, 1, 0)
statsdata$fscore10.20 <- ifelse(statsdata$full_scoretraj > 10 & statsdata$full_scoretraj <= 20, 1, 0)
statsdata$fscore20.30 <- ifelse(statsdata$full_scoretraj > 20 & statsdata$full_scoretraj <= 30, 1, 0)
statsdata$fscore30.40 <- ifelse(statsdata$full_scoretraj > 30 & statsdata$full_scoretraj <= 40, 1, 0)
statsdata$fscore40.50 <- ifelse(statsdata$full_scoretraj > 40 & statsdata$full_scoretraj <= 50, 1, 0)
statsdata$fscore50.60 <- ifelse(statsdata$full_scoretraj > 50 & statsdata$full_scoretraj <= 60, 1, 0)


## Univariate logistic regression 

resultstab2 <- data.table(C1 = as.numeric(1:6) * 10 - 5, C2 = NA)
resultstab2[,2] <- lapply(c("fscore00.10","fscore10.20","fscore20.30","fscore30.40", "fscore40.50","fscore50.60"),
                           
                           function(var) {
                             
                             formula    <- as.formula(paste("dead28 ~", var))
                             res.logist <- glm(formula, data = statsdata, family = binomial)
                             
                             summary(res.logist)
                             var <- summary(res.logist)$coefficients[2,1]
                           })
resultstab2$C1 <- as.numeric(resultstab2$C1)
resultstab2$C2 <- as.numeric(resultstab2$C2)

ggplot(data = resultstab2) +
  geom_point(aes(x = C1, y = C2))
