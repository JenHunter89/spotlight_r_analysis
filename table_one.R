install.packages("tidyverse")
install.packages("haven")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("readr")
install.packages("plyr")
install.packages("lubridate")
install.packages("openxlsx")

library(tidyverse)
library(haven)
library(Hmisc)
library(dplyr)
library(readr)
library(plyr)
library(lubridate)
library(openxlsx)

setwd("/Users/jenniferhunter/Documents/SH PhD Project")

table_one <- read_csv("data/analysis_data.csv")



## Age
table_one$age <- (ymd(20120901) - as_date(table_one$dob))/365
table_one$age <- as.numeric(table_one$age)
describe(table_one$age)
sd(table_one$age)

## Sex
describe(table_one$sex)
table(table_one$sex)

## Ethnicity - think 1, 2, 3 are white ethnicity based on first 3 NHS ethnicity codes
describe(table_one$ethnicity)
table(table_one$ethnicity)
length(which(table_one$ethnicity == 1|table_one$ethnicity == 2|table_one$ethnicity == 3))
length(which(!(table_one$ethnicity == 1|table_one$ethnicity == 2|table_one$ethnicity == 3)))
length(which(table_one$ethnicity == 1|table_one$ethnicity == 2|table_one$ethnicity == 3))/5429*100
length(which(!(table_one$ethnicity == 1|table_one$ethnicity == 2|table_one$ethnicity == 3)))/5429*100
length(which(is.na(table_one$ethnicity)))
(length(which(is.na(table_one$ethnicity)))/5429)*100

## Sepsis diagnosis, very unlikely = 4, very likely = 1
describe(table_one$sepsis)
length(which(is.na(table_one$sepsis)))
length(which(is.na(table_one$sepsis)))/5429*100

## Delay referral
describe(table_one$v_delay)
length(which(is.na(table_one$v_delay)))/5429*100

## Delay admission
describe(table_one$icu_delay)
table(table_one$icu_delay)
length(which(is.na(table_one$icu_delay)))/5429*100
length(which(table_one$icu_delay == 1))/5429*100
length(which(table_one$icu_delay == 0))/5429*100

## ICU LOS - needs work
describe(table_one$dod)
table_one$icu_los <- ifelse(is.na(table_one$dod),
                            as_date(table_one$icu_discharge) - as_date(table_one$icu_admit), 
                            as_date(table_one$dod) - as_date(table_one$icu_admit))
describe(table_one$icu_los)
quantile(table_one$icu_los, c(0.25, 0.5, 0.75), na.rm = TRUE)

##table_one$icu_los <- NA
##table_one$icu_los[is.na(table_one$dod) & table_one$withinsh == 0] <- as_date(table_one$icu_discharge) - as_date(table_one$icu_admit)

## Critical Care Mortality
describe(table_one$dod)
length(which(is.na(table_one$dod)))/5429*100

## Hospital Survival
table(table_one$ahsurv)
describe(table_one$ahsurv)
length(which(is.na(table_one$ahsurv)))/5429*100
length(which(table_one$ahsurv == 1))/5429*100
length(which(table_one$ahsurv == 0))/5429*100

## 28 day mortality
table_one$mort28d <- 0
table_one$mort28d[(as_date(table_one$dod) - as_date(table_one$icu_admit)) < 28] <- 1
table_one$mort28d[table_one$ahsurv == 1 & (table_one$yhlos < 28)] <- 1
describe(table_one$mort28d)
table(table_one$mort28d)
length(which(table_one$mort28d == 1))/5429*100
