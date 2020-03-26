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

trajdata <- read_csv("Data/varcalcs_data.csv")

## To do:
##!! Incorporate lactate


## ++++++++
## Trajectories of interest:
## 1. Trajectories for severity scores, full and 2x partial
## 2. Trajectories for absolute variable values, clipped and non-clipped
## ++++++++

## ++++++++
## To use:
## 1. Raw variables
## 2. Variable severity score
## 3. Summed severity scores
## 4. Scaled 0 to 1 scores, 0 = normal, 1 = max badness either direction 
## 5. Summed scaled scores
## 6. 'clip' 0 or 1; 0 indicates absolute variable value for 1st measurement in top or bottom 10%
## ++++++++

## ++++++++
## Naming:
## var1, var2
## var1wt, var2wt
## vardata$full_score1/2, vardata$partABG_score1/2, vardata$part_score1/2
## clipfull, clippartABG, clippart, clipvar
## var1norm, var2norm
## scalescorefull1/2, scalescorepartABG1/2, scalescorepart1/2

## !!NOTE: have replaced 0 times with 0.5 to avoid 0 denominator
trajdata$time2icu[trajdata$time2icu == 0] <- 0.5

## =====
## Trajectories for individual raw variables, variable severity scores, and summed severity scores
## =====
names <- c('hr', 'bps', 'temp', 'rr','pf','ph','urea', 'cr', 'na', 'urin', 'wcc', 'gcs')

## Individual variable scores
trajdata[,paste(names,"trajabsolut",sep="")] <- trajdata[,paste(names,"2",sep="")] - trajdata[,paste(names,"1",sep="")] / trajdata$time2icu
trajdata[,paste(names,"trajwt",sep="")] <- trajdata[,paste(names,"2wt",sep="")] - trajdata[,paste(names,"1wt",sep="")] / trajdata$time2icu

## Summed severity scores
## Calculated as score2 - score1, more positive score = worse, therefore positive trajectory = getting worse, negative trajectory = getting better
trajdata$full_scoretraj <- ifelse(!is.na(trajdata$full_score1) & !is.na(trajdata$full_score2), ((trajdata$full_score2 - trajdata$full_score1)/trajdata$time2icu), NA)
trajdata$partABG_scoretraj <- ifelse(!is.na(trajdata$partABG_score1) & !is.na(trajdata$partABG_score2), ((trajdata$partABG_score2 - trajdata$partABG_score1)/trajdata$time2icu), NA)
trajdata$part_scoretraj <- ifelse(!is.na(trajdata$part_score1) & !is.na(trajdata$part_score2), ((trajdata$part_score2 - trajdata$part_score1)/trajdata$time2icu), NA)

trajdata$full_scoretraj

## ======
## Trajectories for scaled variables and summed scales -> not using these
## ======

nameslac <- c(names, 'lac')

## Individual variable scaled trajectory
trajdata[,paste(nameslac,"trajnorm",sep="")] <- trajdata[,paste(names,"2norm",sep="")] - trajdata[,paste(names,"1norm",sep="")] / (trajdata$time2icu/24)
describe(trajdata$lactrajnorm)

## summed scales trajectories
trajdata$scale_fulltraj <- ifelse(!is.na(trajdata$scalescorefull1) & !is.na(trajdata$scalescorefull2), ((trajdata$scalescorefull2 - trajdata$scalescorefull1)/(trajdata$time2icu/24)), NA)
trajdata$scale_partABGtraj <- ifelse(!is.na(trajdata$scalescorepartABG1) & !is.na(trajdata$scalescorepartABG2), ((trajdata$scalescorepartABG2 - trajdata$scalescorepartABG1)/(trajdata$time2icu/24)), NA)
trajdata$scale_parttraj <- ifelse(!is.na(trajdata$scalescorepart1) & !is.na(trajdata$scalescorepart2), ((trajdata$scalescorepart2 - trajdata$scalescorepart1)/(trajdata$time2icu/24)), NA)

## ++++++++
## Things to plot
## 1. Full score trajectory of both types for sepsis and non sepsis
## 2. Individual trajectories
## ++++++++

write.csv(trajdata, "/Users/jenniferhunter/Documents/SH PhD Project/Data/trajectories.csv")

