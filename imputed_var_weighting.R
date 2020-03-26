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

## ======
## Imputation weighting; same as variable weighting, but with imputed data
## Also calculates trajectories
## ======

setwd("/Users/jenniferhunter/Documents/SH PhD Project")

compdata <- read_csv("Data/imputdata.csv")

## ===== 
## Pick out few impossible values
## =====

compdata$hr1[compdata$hr1 == 0] <- NA
compdata$rr1[compdata$rr1 == 0] <- NA
compdata$rr1[compdata$rr1 >= 80] <- NA
compdata$lac1[compdata$lac1 == 0.0] <- NA

## ======
## Create Variables
## ======

compdata$hr1wt <- NA
compdata$hr1wt[compdata$hr1 <= 39] <- 14
compdata$hr1wt[compdata$hr1 > 39 & compdata$hr1 <= 109] <- 0
compdata$hr1wt[compdata$hr1 >109 &compdata$hr1 <= 119] <- 1
compdata$hr1wt[compdata$hr1 >119 &compdata$hr1 <= 139] <- 2
compdata$hr1wt[compdata$hr1 >139] <- 3
describe(compdata$hr1)
describe(compdata$hr1wt)

compdata$hr2wt <- NA
compdata$hr2wt[compdata$hr2 <= 39] <- 14
compdata$hr2wt[compdata$hr2 > 39 & compdata$hr2 <= 109] <- 0
compdata$hr2wt[compdata$hr2 >109 &compdata$hr2 <= 119] <- 1
compdata$hr2wt[compdata$hr2 >119 &compdata$hr2 <= 139] <- 2
compdata$hr2wt[compdata$hr2 >139] <- 3
describe(compdata$hr2)
describe(compdata$hr2wt)

compdata$bps1wt <- NA
compdata$bps1wt[compdata$bps1 <= 49] <- 15
compdata$bps1wt[compdata$bps1 >49 & compdata$bps1 <= 59] <- 9
compdata$bps1wt[compdata$bps1 >59 & compdata$bps1 <= 69] <- 6
compdata$bps1wt[compdata$bps1 >69 & compdata$bps1 <= 79] <- 4
compdata$bps1wt[compdata$bps1 >79 & compdata$bps1 <= 99] <- 2
compdata$bps1wt[compdata$bps1 >99 & compdata$bps1 <= 179] <- 0
compdata$bps1wt[compdata$bps1 >179 & compdata$bps1 <= 219] <- 7
compdata$bps1wt[compdata$bps1 >219] <- 16 
describe(compdata$bps1)
describe(compdata$bps1wt)

compdata$bps2wt <- NA
compdata$bps2wt[compdata$bps2 <= 49] <- 15
compdata$bps2wt[compdata$bps2 >49 & compdata$bps2 <= 59] <- 9
compdata$bps2wt[compdata$bps2 >59 & compdata$bps2 <= 69] <- 6
compdata$bps2wt[compdata$bps2 >69 & compdata$bps2 <= 79] <- 4
compdata$bps2wt[compdata$bps2 >79 & compdata$bps2 <= 99] <- 2
compdata$bps2wt[compdata$bps2 >99 & compdata$bps2 <= 179] <- 0
compdata$bps2wt[compdata$bps2 >179 & compdata$bps2 <= 219] <- 7
compdata$bps2wt[compdata$bps2 >219] <- 16 
describe(compdata$bps2)
describe(compdata$bps2wt)

compdata$temp1wt <- NA
compdata$temp1wt[compdata$temp1 <= 25] <- 0
compdata$temp1wt[compdata$temp1 >25 & compdata$temp1 <= 33.9] <- 12
compdata$temp1wt[compdata$temp1 >33.9 & compdata$temp1 <= 35.9] <- 7
compdata$temp1wt[compdata$temp1 >35.9 & compdata$temp1 <= 38.4] <- 1
compdata$temp1wt[compdata$temp1 >38.4 & compdata$temp1 <= 41] <- 0
compdata$temp1wt[compdata$temp1 >41] <- 1
compdata$temp1wt[is.na(compdata$temp1)] <- 0
describe(compdata$temp1)
describe(compdata$temp1wt)

compdata$temp2wt <- NA
compdata$temp2wt[compdata$temp2 <= 25] <- 0
compdata$temp2wt[compdata$temp2 >25 & compdata$temp2 <= 33.9] <- 12
compdata$temp2wt[compdata$temp2 >33.9 & compdata$temp2 <= 35.9] <- 7
compdata$temp2wt[compdata$temp2 >35.9 & compdata$temp2 <= 38.4] <- 1
compdata$temp2wt[compdata$temp2 >38.4 & compdata$temp2 <= 41] <- 0
compdata$temp2wt[compdata$temp2 >41] <- 1
compdata$temp2wt[is.na(compdata$temp2)] <- 0
describe(compdata$temp2)
describe(compdata$temp2wt)

compdata$rr1wt <- NA
compdata$rr1wt[compdata$rr1 <= 5] <- 1
compdata$rr1wt[compdata$rr1 >5 & compdata$rr1 <= 12] <- 0
compdata$rr1wt[compdata$rr1 >12 & compdata$rr1 <= 14] <- 1
compdata$rr1wt[compdata$rr1 >14 & compdata$rr1 <= 25] <- 2
compdata$rr1wt[compdata$rr1 >25] <- 5
describe(compdata$rr1)
describe(compdata$rr1wt)

compdata$rr2wt <- NA
compdata$rr2wt[compdata$rr2 <= 5] <- 1
compdata$rr2wt[compdata$rr2 >5 & compdata$rr2 <= 12] <- 0
compdata$rr2wt[compdata$rr2 >12 & compdata$rr2 <= 14] <- 1
compdata$rr2wt[compdata$rr2 >14 & compdata$rr2 <= 25] <- 2
compdata$rr2wt[compdata$rr2 >25] <- 5
describe(compdata$rr2)
describe(compdata$rr2wt)

compdata$pf1wt <- NA
compdata$rxfio2[is.na(compdata$rxfio2)] <- 0
compdata$pf1wt[compdata$pf1 <= 13 & (compdata$rxfio2 != 1 | compdata$rxfio2 != 2 | compdata$rxfio2 != 3)] <- 6
compdata$pf1wt[compdata$pf1 <= 27 & compdata$pf1 >13 & (compdata$rxfio2 != 1 | compdata$rxfio2 != 2 | compdata$rxfio2 != 3)] <- 3
compdata$pf1wt[compdata$pf1 >27 & (compdata$rxfio2 != 1 | compdata$rxfio2 != 2 | compdata$rxfio2 != 3)] <- 0
compdata$pf1wt[compdata$pf1 <= 13 & (compdata$rxfio2 == 1 | compdata$rxfio2 == 2 | compdata$rxfio2 == 3)] <- 8
compdata$pf1wt[compdata$pf1 <= 27 & compdata$pf1 >13 & (compdata$rxfio2 == 1 | compdata$rxfio2 == 2 | compdata$rxfio2 == 3)] <- 5
compdata$pf1wt[compdata$pf1 >27 & (compdata$rxfio2 == 1 | compdata$rxfio2 == 2 | compdata$rxfio2 == 3)] <- 3
describe(compdata$pf1)
describe(compdata$pf1wt)

compdata$pf2wt <- NA
compdata$intilpo[is.na(compdata$intilpo)] <- 0
compdata$pf2wt[compdata$pf2 <= 13 & compdata$intilpo == 0] <- 6
compdata$pf2wt[compdata$pf2 <= 27 & compdata$pf2 > 13 & compdata$intilpo == 0] <- 3
compdata$pf2wt[compdata$pf2 >27 & compdata$intilpo == 0] <- 0
compdata$pf2wt[compdata$pf2 <= 13 & compdata$intilpo == 1] <- 8
compdata$pf2wt[compdata$pf2 <= 27 & compdata$pf2 > 13 & compdata$intilpo == 1] <- 5
compdata$pf2wt[compdata$pf2 >27 & compdata$intilpo == 1] <- 3
describe(compdata$pf2)
describe(compdata$pf2wt)

compdata$ph1wt <- NA
compdata$ph1wt[compdata$ph1 <= 7.14] <- 4
compdata$ph1wt[compdata$ph1 >7.14 & compdata$ph1 <= 7.24] <- 2
compdata$ph1wt[compdata$ph1 >7.24 & compdata$ph1 <= 7.32] <- 0
compdata$ph1wt[compdata$ph1 >7.32 & compdata$ph1 <= 7.49] <- 1
compdata$ph1wt[compdata$ph1 >7.49] <- 4
describe(compdata$ph1)
describe(compdata$ph1wt)

compdata$ph2wt <- NA
compdata$ph2wt[compdata$ph2 <= 7.14] <- 4
compdata$ph2wt[compdata$ph2 >7.14 & compdata$ph2 <= 7.24] <- 2
compdata$ph2wt[compdata$ph2 >7.24 & compdata$ph2 <= 7.32] <- 0
compdata$ph2wt[compdata$ph2 >7.32 & compdata$ph2 <= 7.49] <- 1
compdata$ph2wt[compdata$ph2 >7.49] <- 4
describe(compdata$ph2)
describe(compdata$ph2wt)

compdata$urea1wt <- NA
compdata$urea1wt[compdata$urea1 <= 6.1] <- 0
compdata$urea1wt[compdata$urea1 > 6.1 & compdata$urea1 <= 7.1] <- 1
compdata$urea1wt[compdata$urea1 > 7.1 & compdata$urea1 <= 14.3] <- 3
compdata$urea1wt[compdata$urea1 > 14.3] <- 5
describe(compdata$urea1)
describe(compdata$urea1wt)

compdata$urea2wt <- NA
compdata$urea2wt[compdata$urea2 <= 6.1] <- 0
compdata$urea2wt[compdata$urea2 > 6.1 & compdata$urea2 <= 7.1] <- 1
compdata$urea2wt[compdata$urea2 > 7.1 & compdata$urea2 <= 14.3] <- 3
compdata$urea2wt[compdata$urea2 > 14.3] <- 5
describe(compdata$urea2)
describe(compdata$urea2wt)

compdata$cr1wt <- NA
compdata$cr1wt[compdata$cr1 <= (0.5 * 88.4)] <- 0
compdata$cr1wt[compdata$cr1 > (0.5 * 88.4) & compdata$cr1 <= (1.5*88.4)] <- 2
compdata$cr1wt[compdata$cr1 > (1.5 * 88.4)] <- 4
describe(compdata$cr1)
describe(compdata$cr1wt)

compdata$cr2wt <- NA
compdata$cr2wt[compdata$cr2 <= (0.5 * 88.4)] <- 0
compdata$cr2wt[compdata$cr2 > (0.5 * 88.4) & compdata$cr2 <= (1.5*88.4)] <- 2
compdata$cr2wt[compdata$cr2 > (1.5 * 88.4)] <- 4
describe(compdata$cr2)
describe(compdata$cr2wt)

compdata$na1wt <- NA
compdata$na1wt[compdata$na1 <= 129] <- 4
compdata$na1wt[compdata$na1 >129 & compdata$na1 <= 149] <- 0
compdata$na1wt[compdata$na1 >149 & compdata$na1 <= 154] <- 4
compdata$na1wt[compdata$na1 >154 & compdata$na1 <= 160] <- 7
compdata$na1wt[compdata$na1 >160] <- 8
describe(compdata$na1)
describe(compdata$na1wt)

compdata$na2wt <- NA
compdata$na2wt[compdata$na2 <= 129] <- 4
compdata$na2wt[compdata$na2 >129 & compdata$na2 <= 149] <- 0
compdata$na2wt[compdata$na2 >149 & compdata$na2 <= 154] <- 4
compdata$na2wt[compdata$na2 >154 & compdata$na2 <= 160] <- 7
compdata$na2wt[compdata$na2 >160] <- 8
describe(compdata$na2)
describe(compdata$na2wt)

compdata$urin1wt <- NA
compdata$urin1wt[compdata$urin1 <= (399/24)] <- 7
compdata$urin1wt[compdata$urin1 > (399/24) & compdata$urin1 <= (599/24)] <- 6
compdata$urin1wt[compdata$urin1 > (599/24) & compdata$urin1 <= (899/24)] <- 5
compdata$urin1wt[compdata$urin1 > (899/24) & compdata$urin1 <= (1499/24)] <- 3
compdata$urin1wt[compdata$urin1 > (1499/24) & compdata$urin1 <= (1999/24)] <- 1
compdata$urin1wt[compdata$urin1 > (1999/24)] <- 0
describe(compdata$urin1)
describe(compdata$urin1wt)

compdata$urin2wt <- NA
compdata$urin2wt[compdata$urin2 <= (399/24)] <- 7
compdata$urin2wt[compdata$urin2 > (399/24) & compdata$urin2 <= (599/24)] <- 6
compdata$urin2wt[compdata$urin2 > (599/24) & compdata$urin2 <= (899/24)] <- 5
compdata$urin2wt[compdata$urin2 > (899/24) & compdata$urin2 <= (1499/24)] <- 3
compdata$urin2wt[compdata$urin2 > (1499/24) & compdata$urin2 <= (1999/24)] <- 1
compdata$urin2wt[compdata$urin2 > (1999/24)] <- 0
describe(compdata$urin2)
describe(compdata$urin2wt)

compdata$wcc1wt <- NA
compdata$wcc1wt[compdata$wcc1 <= 0.9] <- 6
compdata$wcc1wt[compdata$wcc1 > 0.9 & compdata$wcc1 <= 2.9] <- 3
compdata$wcc1wt[compdata$wcc1 > 2.9 & compdata$wcc1 <= 14.9] <- 0
compdata$wcc1wt[compdata$wcc1 > 14.9 & compdata$wcc1 <= 39.9] <- 2
compdata$wcc1wt[compdata$wcc1 > 39.9] <- 4
describe(compdata$wcc1)
describe(compdata$wcc1wt)

compdata$wcc2wt <- NA
compdata$wcc2wt[compdata$wcc2 <= 0.9] <- 6
compdata$wcc2wt[compdata$wcc2 > 0.9 & compdata$wcc2 <= 2.9] <- 3
compdata$wcc2wt[compdata$wcc2 > 2.9 & compdata$wcc2 <= 14.9] <- 0
compdata$wcc2wt[compdata$wcc2 > 14.9 & compdata$wcc2 <= 39.9] <- 2
compdata$wcc2wt[compdata$wcc2 > 39.9] <- 4
describe(compdata$wcc2)
describe(compdata$wcc2wt)

compdata$gcs1wt <- NA
compdata$gcs1wt[compdata$gcs1 == 3] <- 11
compdata$gcs1wt[compdata$gcs1 == 4] <- 9
compdata$gcs1wt[compdata$gcs1 == 5] <- 6
compdata$gcs1wt[compdata$gcs1 == 6] <- 4
compdata$gcs1wt[compdata$gcs1 >6 & compdata$gcs1 <= 13] <- 2
compdata$gcs1wt[compdata$gcs1 >13 & compdata$gcs1 <= 14] <- 1
compdata$gcs1wt[compdata$gcs1 == 15] <- 0
describe(compdata$gcs1)
describe(compdata$gcs1wt)


compdata$gcs2wt <- NA
compdata$gcs2wt[compdata$gcs2 == 3] <- 11
compdata$gcs2wt[compdata$gcs2 == 4] <- 9
compdata$gcs2wt[compdata$gcs2 == 5] <- 6
compdata$gcs2wt[compdata$gcs2 == 6] <- 4
compdata$gcs2wt[compdata$gcs2 >6 & compdata$gcs2 <= 13] <- 2
compdata$gcs2wt[compdata$gcs2 >13 & compdata$gcs2 <= 14] <- 1
compdata$gcs2wt[compdata$gcs2 == 15] <- 0
describe(compdata$gcs2)
describe(compdata$gcs2wt)

## ======
## Confirm none missing
## ======

compdata$none_missing1 <- ifelse(!is.na(compdata$hr1wt) & !is.na(compdata$bps1wt) & !is.na(compdata$temp1wt) & !is.na(compdata$rr1wt) & !is.na(compdata$pf1wt) & !is.na(compdata$ph1wt) & !is.na(compdata$urea1wt) & !is.na(compdata$cr1wt) & !is.na(compdata$na1wt) & !is.na(compdata$urin1wt) & !is.na(compdata$wcc1wt) & !is.na(compdata$gcs1wt), 1, 0)
describe(compdata$none_missing1)
compdata$none_missing2 <- ifelse(!is.na(compdata$hr2wt) & !is.na(compdata$bps2wt) & !is.na(compdata$temp2wt) & !is.na(compdata$rr2wt) & !is.na(compdata$pf2wt) & !is.na(compdata$ph2wt) & !is.na(compdata$urea2wt) & !is.na(compdata$cr2wt) & !is.na(compdata$na2wt) & !is.na(compdata$urin2wt) & !is.na(compdata$wcc2wt) & !is.na(compdata$gcs2wt), 1, 0)
describe(compdata$none_missing2)

## ======
## Calculate summed scores
## ======

length(which(compdata$none_missing1 ==1 & compdata$none_missing2 == 1))
compdata$full_score1 <- ifelse(compdata$none_missing1 != 1, NA, compdata$hr1wt + compdata$bps1wt + compdata$temp1wt + compdata$rr1wt + compdata$pf1wt + compdata$ph1wt + compdata$urea1wt + compdata$cr1wt + compdata$na1wt + compdata$urin1wt + compdata$wcc1wt + compdata$gcs1wt)
compdata$full_score2 <- ifelse(compdata$none_missing2 != 1, NA, compdata$hr2wt + compdata$bps2wt + compdata$temp2wt + compdata$rr2wt + compdata$pf2wt + compdata$ph2wt + compdata$urea2wt + compdata$cr2wt + compdata$na2wt + compdata$urin2wt + compdata$wcc2wt + compdata$gcs2wt)
describe(compdata$full_score1)
describe(compdata$full_score2)

## ======
## Calculate trajectories for individual variables
## ======

names <- c('hr', 'bps', 'temp', 'rr','pf','ph','urea', 'cr', 'na', 'urin', 'wcc', 'gcs')

## !!NOTE: have replaced 0 times with 0.5 to avoid 0 denominator
compdata$time2icu[compdata$time2icu == 0] <- 0.5

## Individual variable trajectories
compdata[,paste(names,"trajabsolut",sep="")] <- compdata[,paste(names,"2",sep="")] - compdata[,paste(names,"1",sep="")] / compdata$time2icu
compdata[,paste(names,"trajwt",sep="")] <- compdata[,paste(names,"2wt",sep="")] - compdata[,paste(names,"1wt",sep="")] / compdata$time2icu

## ======
## Imputed trajectory scores
## ======
compdata$imputtraj <- ifelse(!is.na(compdata$full_score1) & !is.na (compdata$full_score2), (compdata$full_score2 - compdata$full_score1)/compdata$time2icu, NA)

describe(compdata$time2icu)
describe(compdata$imputtraj)

## ======
## Write imputed data trajectories
## ======

write.csv(compdata, "Data/imputedtraj.csv")
