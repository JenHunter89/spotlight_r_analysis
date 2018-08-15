install.packages("tidyverse")
install.packages("haven")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("readr")
install.packages("plyr")
install.packages("lubridate")
install.packages("openxlsx")
install.packages("data.table")

library(tidyverse)
library(haven)
library(Hmisc)
library(dplyr)
library(readr)
library(plyr)
library(lubridate)
library(openxlsx)
library(data.table)

setwd("/Users/jenniferhunter/Documents/SH PhD Project")

rawdata <- read_dta("raw_data/data-spot_traj/working_raw_mris.dta", encoding='latin1')

strobe <- data.table(value=c('AssesElg', 'ExcTot', 'ExcAge', 'ExcArrest', 'ExcReadm', 'ExcSite', 'ExcLink', 'Recruit', 'MissWard', 'MissFU', 'Irrec', 'Analysis'))
addcols <- c("Patients", "Months")
strobe[, addcols] <- NA
strobe$Patients <- as.numeric(strobe$Patients)
strobe$Months <- as.numeric(strobe$Months)

## ======
## This bit checks there are values in icode and study month for all
## ======

rawdata$included_sites <- NULL
rawdata$included_sites <- ifelse (is.na(rawdata$icode), 0, 1)
table(rawdata$included_sites)

rawdata$included_months <- NULL
rawdata$included_months <- ifelse (is.na(rawdata$studymonth), 0, 1)
table(rawdata$included_months)

sum(rawdata$included_sites == 1)
sum(rawdata$included_months == 1)

nrow(rawdata %>% select(icode, studymonth) %>% distinct())

## ======
## Define clean data (209-252)
## Include 0 if month missing, eligible date not true, place not eligible and protocol not eligible
## ======

rawdata$include = 1
sum(rawdata$include == 1)
rawdata$include[rawdata$cmpd_month_miss == 1 | is.na(rawdata$cmpd_month_miss)] <- 0
sum(rawdata$include == 0)
rawdata$include[rawdata$elgdate != 1 | is.na(rawdata$elgdate)] <- 0
sum(rawdata$include == 0)
rawdata$include[rawdata$elgward == 0 & rawdata$elgoward == 0 & rawdata$elgtrans == 0] <- 0
sum(rawdata$include == 0)
rawdata$include[rawdata$elgprotocol == 0] <- 0
sum(rawdata$include == 0)
rawdata$include[rawdata$elgemx != 1] <- 0
sum(rawdata$include == 0)
sum(rawdata$include == 1)
## 14465

rawdata$included_sites <- NULL
rawdata$included_sites <- ifelse (!is.na(rawdata$icode) & rawdata$include == 1, 1, 0)
rawdata$included_months <- NULL
rawdata$included_months <- ifelse (!is.na(rawdata$studymonth) & rawdata$include == 1, 1, 0)
sum(rawdata$included_sites == 1)
sum(rawdata$included_months == 1)

table(rawdata$match_is_ok, rawdata$include == 1)


## ======
## Include only if meets initial quality criteria = include 1 (257-268)
## ======
rawdata$include[rawdata$site_quality_q1 < 80 | is.na(rawdata$site_quality_q1)] <- 0

strobe[1,2] <- sum(rawdata$include == 1)
## 10150

rawdata$included_sites <- NULL
rawdata$included_sites <- ifelse (!is.na(rawdata$icode) & rawdata$include == 1, 1, 0)
rawdata$included_months <- NULL
rawdata$included_months <- ifelse (!is.na(rawdata$studymonth) & rawdata$include == 1, 1, 0)
sum(rawdata$included_sites == 1)
sum(rawdata$included_months == 1)

strobe[1,3] <- nrow(rawdata %>% select(icode, studymonth) %>% filter(rawdata$include == 1) %>% distinct())


table(rawdata$match_is_ok, rawdata$include == 1)


## ======
## Exclude by design = exclude1 (273-307)
## ======

rawdata$exclude1 = 0
strobe[3,2] <- sum(rawdata$include == 1 & rawdata$elgage == 0)
strobe[4,2] <- sum(rawdata$include == 1 & rawdata$elgcpr == 0)
strobe[5,2] <- sum(rawdata$include == 1 & rawdata$withinsh == 1)
sum(rawdata$include == 1 & rawdata$elgreport_heads == 0)
sum(rawdata$include == 1 & rawdata$elgreport_tails == 0)
strobe[6,2] <- sum(rawdata$include == 1 & rawdata$elgreport_heads == 0) + sum(rawdata$include == 1 & rawdata$elgreport_tails == 0)

rawdata$exclude1[rawdata$include == 1 & rawdata$elgage == 0] <- 1
rawdata$exclude1[rawdata$include == 1 & rawdata$elgcpr == 0] <- 1
rawdata$exclude1[rawdata$include == 1 & rawdata$withinsh == 1] <- 1
rawdata$exclude1[rawdata$include == 1 & rawdata$elgreport_heads == 0] <- 1
rawdata$exclude1[rawdata$include == 1 & rawdata$elgreport_tails == 0] <- 1

strobe[2,2] <- sum(rawdata$exclude1)

table(rawdata$exclude1, rawdata$include == 1)

rawdata$included_sites <- NULL
rawdata$included_sites <- ifelse (!is.na(rawdata$icode) & rawdata$include == 1 & rawdata$exclude1 == 0, 1, 0)
rawdata$included_months <- NULL
rawdata$included_months <- ifelse (!is.na(rawdata$studymonth) & rawdata$include == 1 & rawdata$exclude1 == 0, 1, 0)
sum(rawdata$included_sites == 1)
sum(rawdata$included_months == 1)

table(rawdata$match_is_ok, rawdata$include == 1 & rawdata$exclude1 == 0)

## ======
## Numbers agree up to here: Exclude because of poor quality = exclude 2 (309 - 329)
## ======

rawdata$exclude2 = NULL 
rawdata$exclude2 = 0
describe(rawdata$site_quality_by_month)
sum(rawdata$exclude2 == 1)

##rawdata$exclude2 <- ifelse(is.na(rawdata$site_quality_by_month) | rawdata$site_quality_by_month <= 80, 1, 0)
##rawdata$exclude2 <- ifelse((!is.na(rawdata$site_quality_by_month)) | rawdata$site_quality_by_month < 80 & rawdata$include == 1 & rawdata$exclude1 == 0, 0, 1)
rawdata$exclude2[is.na(rawdata$site_quality_by_month)] <- 1
rawdata$exclude2[rawdata$site_quality_by_month < 80] <- 1

strobe[7,2] <- sum(rawdata$exclude2 == 1 & rawdata$include == 1 & rawdata$exclude1 == 0)
strobe[7,3] <- nrow(rawdata %>% filter(rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 1) %>% select(icode, studymonth) %>% distinct())

table(rawdata$exclude2, rawdata$include == 1 & rawdata$exclude1 == 0)

rawdata$included_sites <- NULL
rawdata$included_sites <- ifelse (!is.na(rawdata$icode) & rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0, 1, 0)
rawdata$included_months <- NULL
rawdata$included_months <- ifelse (!is.na(rawdata$studymonth) & rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0, 1, 0)
sum(rawdata$included_sites == 1)
sum(rawdata$included_months == 1)

strobe[8,2] <- length(which(rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0))

table(rawdata$match_is_ok, rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0)

strobe[8,3] <- nrow(rawdata %>% filter(rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0) %>% select(icode, studymonth) %>% distinct())

++++++++++++++++++++++++++++++

## Numbers are now different; match is OK table
##    FALSE TRUE
## 0  4589  904 (Steves 905)
## 1  3071 6358 (Steves 6359)
## ======
## Exclude lost to follow up = exclude 3(336-362)
## ======

length(which(rawdata$date_event == rawdata$date_trace))
length(which(rawdata$date_event - rawdata$date_trace > 7 & !is.na(rawdata$date_event) & !is.na(rawdata$date_trace)))

### Check - think this is essentially saying that all but 7 of the dates where there is a difference will be got rid of by getting rid of NAs

table(rawdata$dead, rawdata$dead_mris)

### Check - think this implies that there are only 9 patients where there isn't agreement, so can just use one measure? But why not get rid of ones where no agreement?

rawdata$dead = NULL
rawdata$date_trace = NULL

names(rawdata)[names(rawdata) == "dead_mris"] <- "dead"
names(rawdata)[names(rawdata) == "date_event"] <- "date_trace"
 
rawdata$exclude3 = 0
rawdata$exclude3[is.na(rawdata$date_trace)] <- 1
strobe[10,2] <- length(which(rawdata$exclude3 == 1 & rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0))

table(rawdata$exclude3, rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0)

rawdata$included_sites <- NULL
rawdata$included_sites <- ifelse (!is.na(rawdata$icode) & rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0, 1, 0)
rawdata$included_months <- NULL
rawdata$included_months <- ifelse (!is.na(rawdata$studymonth) & rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0, 1, 0)
sum(rawdata$included_sites == 1)
sum(rawdata$included_months == 1)

table(rawdata$match_is_ok, rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0)
nrow(rawdata %>% filter(rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0) %>% select(icode, studymonth) %>% distinct())

## Numbers here still differ by 1
##   FALSE TRUE
## 0  4810  683 (684)
## 1  3199 6230 (6231)
## ======
## Exclude where data cannot be reconciled = exclude 4 (368 - 422)
## ======

rawdata$exclude4 = 0

rawdata$last_trace = NULL
rawdata$last_trace = as_datetime(rawdata$date_trace) + dhours(23) + dminutes(58)

rawdata$icu_discharge <- as_datetime(rawdata$icu_discharge)
rawdata$date_trace <- as_datetime(rawdata$date_trace)

rawdata$last_trace <- with(rawdata, ifelse((rawdata$dead_icu == 1 & !is.na(icu_discharge)), rawdata$icu_discharge, rawdata$last_trace))
rawdata$last_trace <- as_datetime(rawdata$last_trace)

table <- select(rawdata, dead, dead_icu, dod, date_trace, idvisit) %>% 
  filter(rawdata$dod != rawdata$date_trace | rawdata$icu_discharge > rawdata$date_trace & 
           rawdata$dead_icu != rawdata$dead & !is.na(rawdata$dead) & 
           !is.na(rawdata$dod) & 
           !is.na(rawdata$icu_discharge)) %>% 
  group_by(idvisit)

## check counts meeting criteria

length(which((as_date(rawdata$icu_discharge) != rawdata$date_trace) & rawdata$dead_icu == 1 & !is.na(rawdata$dead)))
length(which(floor_date(rawdata$icu_admit) > floor_date(rawdata$icu_discharge) & !is.na(rawdata$icu_admit) & !is.na(rawdata$icu_discharge)))
length(which(floor_date(rawdata$v_timestamp) > floor_date(rawdata$icu_admit) & !is.na(rawdata$v_timestamp) & !is.na(rawdata$icu_admit)))
length(which(floor_date(rawdata$v_timestamp) > floor_date(rawdata$icu_discharge) & !is.na(rawdata$v_timestamp) & !is.na(rawdata$icu_discharge)))
length(which(floor_date(rawdata$icu_admit) > floor_date(rawdata$last_trace) & !is.na(rawdata$icu_admit) & !is.na(rawdata$last_trace)))
length(which(rawdata$icu_discharge > rawdata$last_trace & !is.na(rawdata$icu_discharge) & !is.na(rawdata$last_trace)))
length(which(rawdata$v_timestamp > rawdata$last_trace & !is.na(rawdata$v_timestamp) & !is.na(rawdata$last_trace)))

rawdata$exclude4[as_date(rawdata$icu_discharge) != rawdata$date_trace & rawdata$dead_icu == 1 & !is.na(rawdata$dead)] <- 1
rawdata$exclude4[floor_date(rawdata$icu_admit) > floor_date(rawdata$icu_discharge) & !is.na(rawdata$icu_admit) & !is.na(rawdata$icu_discharge)] <- 1
rawdata$exclude4[floor_date(rawdata$v_timestamp) > floor_date(rawdata$icu_admit) & !is.na(rawdata$v_timestamp) & !is.na(rawdata$icu_admit)] <- 1
rawdata$exclude4[floor_date(rawdata$v_timestamp) > floor_date(rawdata$icu_discharge) & !is.na(rawdata$v_timestamp) & !is.na(rawdata$icu_discharge)] <- 1
rawdata$exclude4[floor_date(rawdata$icu_admit) > floor_date(rawdata$last_trace) & !is.na(rawdata$icu_admit) & !is.na(rawdata$last_trace)] <- 1
rawdata$exclude4[rawdata$icu_discharge > rawdata$last_trace & !is.na(rawdata$icu_discharge) & !is.na(rawdata$last_trace)] <- 1
rawdata$exclude4[rawdata$v_timestamp > rawdata$last_trace & !is.na(rawdata$v_timestamp) & !is.na(rawdata$last_trace)] <- 1

strobe[11,2] <-length(which(rawdata$exclude4 == 1 & rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0))

table(rawdata$exclude4, rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0)

rawdata$included_sites <- NULL
rawdata$included_sites <- ifelse (!is.na(rawdata$icode) & rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0 & rawdata$exclude4 == 0, 1, 0)
rawdata$included_months <- NULL
rawdata$included_months <- ifelse (!is.na(rawdata$studymonth) & rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0 & rawdata$exclude4 == 0, 1, 0)
sum(rawdata$included_sites == 1)
sum(rawdata$included_months == 1)

table(rawdata$match_is_ok, rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0 & rawdata$exclude4 == 0)
strobe[9,2] <- length(which(rawdata$match_is_ok == 0 & rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0 & rawdata$exclude4 == 0))

##   FALSE TRUE
## 0  4828  665 (Steve's 664)
## 1  3353 6076 (Steve's 6099)

## ======
## Exclude unmatched = exclude 5 (430-444)
## ======

rawdata$exclude5 = 0

rawdata$exclude5[rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$match_is_ok == 0] <- 1

table(rawdata$exclude5, rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0 & rawdata$exclude4 == 0)

## use describe to check numbers add up with missing data

rawdata$included_sites <- NULL
rawdata$included_sites <- ifelse (!is.na(rawdata$icode) & rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0 & rawdata$exclude4 == 0 & rawdata$exclude5 == 0, 1, 0)
rawdata$included_months <- NULL
rawdata$included_months <- ifelse (!is.na(rawdata$studymonth) & rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0 & rawdata$exclude4 == 0 & rawdata$exclude5 == 0, 1, 0)
sum(rawdata$included_sites == 1)
sum(rawdata$included_months == 1)

table(rawdata$match_is_ok, rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0 & rawdata$exclude4 == 0 & rawdata$exclude5 == 0)

strobe[12,2] <- length(which(rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0 & rawdata$exclude4 == 0 & rawdata$exclude5 == 0))
strobe[12,3] <- nrow(rawdata %>% filter(rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0 & rawdata$exclude4 == 0 & rawdata$exclude5 == 0) %>% select(icode, studymonth) %>% distinct())

## Final numbers
##    FALSE TRUE
## 0  5494    0
## 1  3353 6076 (Steve's 6099)

write.csv(rawdata, "/Users/jenniferhunter/Documents/SH PhD Project/Data/exclusions_data.csv")

analysisdata <- subset(rawdata, rawdata$include == 1 & rawdata$exclude1 == 0 & rawdata$exclude2 == 0 & rawdata$exclude3 == 0 & rawdata$exclude4 == 0 & rawdata$exclude5 == 0)

write.csv(analysisdata, "/Users/jenniferhunter/Documents/SH PhD Project/Data/analysis_data.csv")
