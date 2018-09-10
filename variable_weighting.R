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

vardata <- read_csv("Data/variables_data.csv")

## ++++++++
## This creates:
## 1. Weighted variable severity scores
## 3. Checks how many data sets full, missing GCS+urine, missing GCS+urine+pH+pf
## 4. Sums full, part with ABG and partial severity scores
## 5. Identify top and bottom 10% to clip in trajectory calculations, as these can't go up or down
## 6. Scales all variables from 0 to 1, where 0 is normal, and 1 is badness in either direction
## ++++++++


## ===== 
## Pick out few impossible values
## =====

vardata$hr1[vardata$hr1 == 0] <- NA
vardata$rr1[vardata$rr1 == 0] <- NA
vardata$rr1[vardata$rr1 >= 80] <- NA

## ======
## Create Variables
## ======

vardata$hr1wt <- NA
vardata$hr1wt[vardata$hr1 <= 39] <- 14
vardata$hr1wt[vardata$hr1 > 39 & vardata$hr1 <= 109] <- 0
vardata$hr1wt[vardata$hr1 >109 &vardata$hr1 <= 119] <- 1
vardata$hr1wt[vardata$hr1 >119 &vardata$hr1 <= 139] <- 2
vardata$hr1wt[vardata$hr1 >139] <- 3
describe(vardata$hr1)
describe(vardata$hr1wt)

vardata$hr2wt <- NA
vardata$hr2wt[vardata$hr2 <= 39] <- 14
vardata$hr2wt[vardata$hr2 > 39 & vardata$hr2 <= 109] <- 0
vardata$hr2wt[vardata$hr2 >109 &vardata$hr2 <= 119] <- 1
vardata$hr2wt[vardata$hr2 >119 &vardata$hr2 <= 139] <- 2
vardata$hr2wt[vardata$hr2 >139] <- 3
describe(vardata$hr2)
describe(vardata$hr2wt)

vardata$bps1wt <- NA
vardata$bps1wt[vardata$bps1 <= 49] <- 15
vardata$bps1wt[vardata$bps1 >49 & vardata$bps1 <= 59] <- 9
vardata$bps1wt[vardata$bps1 >59 & vardata$bps1 <= 69] <- 6
vardata$bps1wt[vardata$bps1 >69 & vardata$bps1 <= 79] <- 4
vardata$bps1wt[vardata$bps1 >79 & vardata$bps1 <= 99] <- 2
vardata$bps1wt[vardata$bps1 >99 & vardata$bps1 <= 179] <- 0
vardata$bps1wt[vardata$bps1 >179 & vardata$bps1 <= 219] <- 7
vardata$bps1wt[vardata$bps1 >219] <- 16 
describe(vardata$bps1)
describe(vardata$bps1wt)

vardata$bps2wt <- NA
vardata$bps2wt[vardata$bps2 <= 49] <- 15
vardata$bps2wt[vardata$bps2 >49 & vardata$bps2 <= 59] <- 9
vardata$bps2wt[vardata$bps2 >59 & vardata$bps2 <= 69] <- 6
vardata$bps2wt[vardata$bps2 >69 & vardata$bps2 <= 79] <- 4
vardata$bps2wt[vardata$bps2 >79 & vardata$bps2 <= 99] <- 2
vardata$bps2wt[vardata$bps2 >99 & vardata$bps2 <= 179] <- 0
vardata$bps2wt[vardata$bps2 >179 & vardata$bps2 <= 219] <- 7
vardata$bps2wt[vardata$bps2 >219] <- 16 
describe(vardata$bps2)
describe(vardata$bps2wt)

vardata$temp1wt <- NA
vardata$temp1wt[vardata$temp1 <= 25] <- 0
vardata$temp1wt[vardata$temp1 >25 & vardata$temp1 <= 33.9] <- 12
vardata$temp1wt[vardata$temp1 >33.9 & vardata$temp1 <= 35.9] <- 7
vardata$temp1wt[vardata$temp1 >35.9 & vardata$temp1 <= 38.4] <- 1
vardata$temp1wt[vardata$temp1 >38.4 & vardata$temp1 <= 41] <- 0
vardata$temp1wt[vardata$temp1 >41] <- 1
vardata$temp1wt[is.na(vardata$temp1)] <- 0
describe(vardata$temp1)
describe(vardata$temp1wt)

vardata$temp2wt <- NA
vardata$temp2wt[vardata$temp2 <= 25] <- 0
vardata$temp2wt[vardata$temp2 >25 & vardata$temp2 <= 33.9] <- 12
vardata$temp2wt[vardata$temp2 >33.9 & vardata$temp2 <= 35.9] <- 7
vardata$temp2wt[vardata$temp2 >35.9 & vardata$temp2 <= 38.4] <- 1
vardata$temp2wt[vardata$temp2 >38.4 & vardata$temp2 <= 41] <- 0
vardata$temp2wt[vardata$temp2 >41] <- 1
vardata$temp2wt[is.na(vardata$temp2)] <- 0
describe(vardata$temp2)
describe(vardata$temp2wt)

vardata$rr1wt <- NA
vardata$rr1wt[vardata$rr1 <= 5] <- 1
vardata$rr1wt[vardata$rr1 >5 & vardata$rr1 <= 12] <- 0
vardata$rr1wt[vardata$rr1 >12 & vardata$rr1 <= 14] <- 1
vardata$rr1wt[vardata$rr1 >14 & vardata$rr1 <= 25] <- 2
vardata$rr1wt[vardata$rr1 >25] <- 5
describe(vardata$rr1)
describe(vardata$rr1wt)

vardata$rr2wt <- NA
vardata$rr2wt[vardata$rr2 <= 5] <- 1
vardata$rr2wt[vardata$rr2 >5 & vardata$rr2 <= 12] <- 0
vardata$rr2wt[vardata$rr2 >12 & vardata$rr2 <= 14] <- 1
vardata$rr2wt[vardata$rr2 >14 & vardata$rr2 <= 25] <- 2
vardata$rr2wt[vardata$rr2 >25] <- 5
describe(vardata$rr2)
describe(vardata$rr2wt)

vardata$pf1wt <- NA
vardata$rxfio2[is.na(vardata$rxfio2)] <- 0
vardata$pf1wt[vardata$pf1 <= 13 & (vardata$rxfio2 != 1 | vardata$rxfio2 != 2 | vardata$rxfio2 != 3)] <- 6
vardata$pf1wt[vardata$pf1 <= 27 & vardata$pf1 >13 & (vardata$rxfio2 != 1 | vardata$rxfio2 != 2 | vardata$rxfio2 != 3)] <- 3
vardata$pf1wt[vardata$pf1 >27 & (vardata$rxfio2 != 1 | vardata$rxfio2 != 2 | vardata$rxfio2 != 3)] <- 0
vardata$pf1wt[vardata$pf1 <= 13 & (vardata$rxfio2 == 1 | vardata$rxfio2 == 2 | vardata$rxfio2 == 3)] <- 8
vardata$pf1wt[vardata$pf1 <= 27 & vardata$pf1 >13 & (vardata$rxfio2 == 1 | vardata$rxfio2 == 2 | vardata$rxfio2 == 3)] <- 5
vardata$pf1wt[vardata$pf1 >27 & (vardata$rxfio2 == 1 | vardata$rxfio2 == 2 | vardata$rxfio2 == 3)] <- 3
describe(vardata$pf1)
describe(vardata$pf1wt)

vardata$pf2wt <- NA
vardata$intilpo[is.na(vardata$intilpo)] <- 0
vardata$pf2wt[vardata$pf2 <= 13 & vardata$intilpo == 0] <- 6
vardata$pf2wt[vardata$pf2 <= 27 & vardata$pf2 > 13 & vardata$intilpo == 0] <- 3
vardata$pf2wt[vardata$pf2 >27 & vardata$intilpo == 0] <- 0
vardata$pf2wt[vardata$pf2 <= 13 & vardata$intilpo == 1] <- 8
vardata$pf2wt[vardata$pf2 <= 27 & vardata$pf2 > 13 & vardata$intilpo == 1] <- 5
vardata$pf2wt[vardata$pf2 >27 & vardata$intilpo == 1] <- 3
describe(vardata$pf2)
describe(vardata$pf2wt)

vardata$ph1wt <- NA
vardata$ph1wt[vardata$ph1 <= 7.14] <- 4
vardata$ph1wt[vardata$ph1 >7.14 & vardata$ph1 <= 7.24] <- 2
vardata$ph1wt[vardata$ph1 >7.24 & vardata$ph1 <= 7.32] <- 0
vardata$ph1wt[vardata$ph1 >7.32 & vardata$ph1 <= 7.49] <- 1
vardata$ph1wt[vardata$ph1 >7.49] <- 4
describe(vardata$ph1)
describe(vardata$ph1wt)

vardata$ph2wt <- NA
vardata$ph2wt[vardata$ph2 <= 7.14] <- 4
vardata$ph2wt[vardata$ph2 >7.14 & vardata$ph2 <= 7.24] <- 2
vardata$ph2wt[vardata$ph2 >7.24 & vardata$ph2 <= 7.32] <- 0
vardata$ph2wt[vardata$ph2 >7.32 & vardata$ph2 <= 7.49] <- 1
vardata$ph2wt[vardata$ph2 >7.49] <- 4
describe(vardata$ph2)
describe(vardata$ph2wt)

vardata$urea1wt <- NA
vardata$urea1wt[vardata$urea1 <= 6.1] <- 0
vardata$urea1wt[vardata$urea1 > 6.1 & vardata$urea1 <= 7.1] <- 1
vardata$urea1wt[vardata$urea1 > 7.1 & vardata$urea1 <= 14.3] <- 3
vardata$urea1wt[vardata$urea1 > 14.3] <- 5
describe(vardata$urea1)
describe(vardata$urea1wt)

vardata$urea2wt <- NA
vardata$urea2wt[vardata$urea2 <= 6.1] <- 0
vardata$urea2wt[vardata$urea2 > 6.1 & vardata$urea2 <= 7.1] <- 1
vardata$urea2wt[vardata$urea2 > 7.1 & vardata$urea2 <= 14.3] <- 3
vardata$urea2wt[vardata$urea2 > 14.3] <- 5
describe(vardata$urea2)
describe(vardata$urea2wt)

vardata$cr1wt <- NA
vardata$cr1wt[vardata$cr1 <= (0.5 * 88.4)] <- 0
vardata$cr1wt[vardata$cr1 > (0.5 * 88.4) & vardata$cr1 <= (1.5*88.4)] <- 2
vardata$cr1wt[vardata$cr1 > (1.5 * 88.4)] <- 4
describe(vardata$cr1)
describe(vardata$cr1wt)

vardata$cr2wt <- NA
vardata$cr2wt[vardata$cr2 <= (0.5 * 88.4)] <- 0
vardata$cr2wt[vardata$cr2 > (0.5 * 88.4) & vardata$cr2 <= (1.5*88.4)] <- 2
vardata$cr2wt[vardata$cr2 > (1.5 * 88.4)] <- 4
describe(vardata$cr2)
describe(vardata$cr2wt)

vardata$na1wt <- NA
vardata$na1wt[vardata$na1 <= 129] <- 4
vardata$na1wt[vardata$na1 >129 & vardata$na1 <= 149] <- 0
vardata$na1wt[vardata$na1 >149 & vardata$na1 <= 154] <- 4
vardata$na1wt[vardata$na1 >154 & vardata$na1 <= 160] <- 7
vardata$na1wt[vardata$na1 >160] <- 8
describe(vardata$na1)
describe(vardata$na1wt)

vardata$na2wt <- NA
vardata$na2wt[vardata$na2 <= 129] <- 4
vardata$na2wt[vardata$na2 >129 & vardata$na2 <= 149] <- 0
vardata$na2wt[vardata$na2 >149 & vardata$na2 <= 154] <- 4
vardata$na2wt[vardata$na2 >154 & vardata$na2 <= 160] <- 7
vardata$na2wt[vardata$na2 >160] <- 8
describe(vardata$na2)
describe(vardata$na2wt)

vardata$urin1wt <- NA
vardata$urin1wt[vardata$urin1 <= (399/24)] <- 7
vardata$urin1wt[vardata$urin1 > (399/24) & vardata$urin1 <= (599/24)] <- 6
vardata$urin1wt[vardata$urin1 > (599/24) & vardata$urin1 <= (899/24)] <- 5
vardata$urin1wt[vardata$urin1 > (899/24) & vardata$urin1 <= (1499/24)] <- 3
vardata$urin1wt[vardata$urin1 > (1499/24) & vardata$urin1 <= (1999/24)] <- 1
vardata$urin1wt[vardata$urin1 > (1999/24)] <- 0
describe(vardata$urin1)
describe(vardata$urin1wt)

vardata$urin2wt <- NA
vardata$urin2wt[vardata$urin2 <= (399/24)] <- 7
vardata$urin2wt[vardata$urin2 > (399/24) & vardata$urin2 <= (599/24)] <- 6
vardata$urin2wt[vardata$urin2 > (599/24) & vardata$urin2 <= (899/24)] <- 5
vardata$urin2wt[vardata$urin2 > (899/24) & vardata$urin2 <= (1499/24)] <- 3
vardata$urin2wt[vardata$urin2 > (1499/24) & vardata$urin2 <= (1999/24)] <- 1
vardata$urin2wt[vardata$urin2 > (1999/24)] <- 0
describe(vardata$urin2)
describe(vardata$urin2wt)

vardata$wcc1wt <- NA
vardata$wcc1wt[vardata$wcc1 <= 0.9] <- 6
vardata$wcc1wt[vardata$wcc1 > 0.9 & vardata$wcc1 <= 2.9] <- 3
vardata$wcc1wt[vardata$wcc1 > 2.9 & vardata$wcc1 <= 14.9] <- 0
vardata$wcc1wt[vardata$wcc1 > 14.9 & vardata$wcc1 <= 39.9] <- 2
vardata$wcc1wt[vardata$wcc1 > 39.9] <- 4
describe(vardata$wcc1)
describe(vardata$wcc1wt)

vardata$wcc2wt <- NA
vardata$wcc2wt[vardata$wcc2 <= 0.9] <- 6
vardata$wcc2wt[vardata$wcc2 > 0.9 & vardata$wcc2 <= 2.9] <- 3
vardata$wcc2wt[vardata$wcc2 > 2.9 & vardata$wcc2 <= 14.9] <- 0
vardata$wcc2wt[vardata$wcc2 > 14.9 & vardata$wcc2 <= 39.9] <- 2
vardata$wcc2wt[vardata$wcc2 > 39.9] <- 4
describe(vardata$wcc2)
describe(vardata$wcc2wt)

vardata$gcs1wt <- NA
vardata$gcs1wt[vardata$gcs1 == 3] <- 11
vardata$gcs1wt[vardata$gcs1 == 4] <- 9
vardata$gcs1wt[vardata$gcs1 == 5] <- 6
vardata$gcs1wt[vardata$gcs1 == 6] <- 4
vardata$gcs1wt[vardata$gcs1 >6 & vardata$gcs1 <= 13] <- 2
vardata$gcs1wt[vardata$gcs1 >13 & vardata$gcs1 <= 14] <- 1
vardata$gcs1wt[vardata$gcs1 == 15] <- 0
describe(vardata$gcs1)
describe(vardata$gcs1wt)


vardata$gcs2wt <- NA
vardata$gcs2wt[vardata$gcs2 == 3] <- 11
vardata$gcs2wt[vardata$gcs2 == 4] <- 9
vardata$gcs2wt[vardata$gcs2 == 5] <- 6
vardata$gcs2wt[vardata$gcs2 == 6] <- 4
vardata$gcs2wt[vardata$gcs2 >6 & vardata$gcs2 <= 13] <- 2
vardata$gcs2wt[vardata$gcs2 >13 & vardata$gcs2 <= 14] <- 1
vardata$gcs2wt[vardata$gcs2 == 15] <- 0
describe(vardata$gcs2)
describe(vardata$gcs2wt)

## ======
## Compute 'ICNARC' scores
## ======

## Identify data with none missing
vardata$none_missing1 <- ifelse(!is.na(vardata$hr1wt) & !is.na(vardata$bps1wt) & !is.na(vardata$temp1wt) & !is.na(vardata$rr1wt) & !is.na(vardata$pf1wt) & !is.na(vardata$ph1wt) & !is.na(vardata$urea1wt) & !is.na(vardata$cr1wt) & !is.na(vardata$na1wt) & !is.na(vardata$urin1wt) & !is.na(vardata$wcc1wt) & !is.na(vardata$gcs1wt), 1, 0)
describe(vardata$none_missing1)
vardata$none_missing2 <- ifelse(!is.na(vardata$hr2wt) & !is.na(vardata$bps2wt) & !is.na(vardata$temp2wt) & !is.na(vardata$rr2wt) & !is.na(vardata$pf2wt) & !is.na(vardata$ph2wt) & !is.na(vardata$urea2wt) & !is.na(vardata$cr2wt) & !is.na(vardata$na2wt) & !is.na(vardata$urin2wt) & !is.na(vardata$wcc2wt) & !is.na(vardata$gcs2wt), 1, 0)
describe(vardata$none_missing2)

## Identify data missing GCS, urine
vardata$partial_ABG1 <- ifelse(!is.na(vardata$hr1wt) & !is.na(vardata$bps1wt) & !is.na(vardata$temp1wt) & !is.na(vardata$rr1wt) & !is.na(vardata$pf1wt) & !is.na(vardata$ph1wt) & !is.na(vardata$urea1wt) & !is.na(vardata$cr1wt) & !is.na(vardata$na1wt) & is.na(vardata$urin1wt) & !is.na(vardata$wcc1wt) & is.na(vardata$gcs1wt), 1, 0)
describe(vardata$partial_ABG1)
vardata$partial_ABG2 <- ifelse(!is.na(vardata$hr2wt) & !is.na(vardata$bps2wt) & !is.na(vardata$temp2wt) & !is.na(vardata$rr2wt) & !is.na(vardata$pf2wt) & !is.na(vardata$ph2wt) & !is.na(vardata$urea2wt) & !is.na(vardata$cr2wt) & !is.na(vardata$na2wt) & is.na(vardata$urin2wt) & !is.na(vardata$wcc2wt) & is.na(vardata$gcs2wt), 1, 0)
describe(vardata$partial_ABG2)

## Identify data missing GCS, urine, pH, PF
vardata$partial1 <- ifelse(!is.na(vardata$hr1wt) & !is.na(vardata$bps1wt) & !is.na(vardata$temp1wt) & !is.na(vardata$rr1wt) & is.na(vardata$pf1wt) & is.na(vardata$ph1wt) & !is.na(vardata$urea1wt) & !is.na(vardata$cr1wt) & !is.na(vardata$na1wt) & is.na(vardata$urin1wt) & !is.na(vardata$wcc1wt) & is.na(vardata$gcs1wt), 1, 0)
describe(vardata$partial1)
vardata$partial2 <- ifelse(!is.na(vardata$hr2wt) & !is.na(vardata$bps2wt) & !is.na(vardata$temp2wt) & !is.na(vardata$rr2wt) & is.na(vardata$pf2wt) & is.na(vardata$ph2wt) & !is.na(vardata$urea2wt) & !is.na(vardata$cr2wt) & !is.na(vardata$na2wt) & is.na(vardata$urin2wt) & !is.na(vardata$wcc2wt) & is.na(vardata$gcs2wt), 1, 0)
describe(vardata$partial2)

## Sum full score
length(which(vardata$none_missing1 ==1 & vardata$none_missing2 == 1))
vardata$full_score1 <- ifelse(vardata$none_missing1 != 1, NA, vardata$hr1wt + vardata$bps1wt + vardata$temp1wt + vardata$rr1wt + vardata$pf1wt + vardata$ph1wt + vardata$urea1wt + vardata$cr1wt + vardata$na1wt + vardata$urin1wt + vardata$wcc1wt + vardata$gcs1wt)
vardata$full_score2 <- ifelse(vardata$none_missing2 != 1, NA, vardata$hr2wt + vardata$bps2wt + vardata$temp2wt + vardata$rr2wt + vardata$pf2wt + vardata$ph2wt + vardata$urea2wt + vardata$cr2wt + vardata$na2wt + vardata$urin2wt + vardata$wcc2wt + vardata$gcs2wt)
describe(vardata$full_score1)
describe(vardata$full_score2)

## Sum with ABG, missing GCS, urine
vardata$partABG_score1 <- ifelse(vardata$none_missing1 != 1 & vardata$partial_ABG1 != 1, NA, vardata$hr1wt + vardata$bps1wt + vardata$temp1wt + vardata$rr1wt + vardata$pf1wt + vardata$ph1wt + vardata$urea1wt + vardata$cr1wt + vardata$na1wt + vardata$wcc1wt)
describe(vardata$partABG_score1)
vardata$partABG_score2 <- ifelse(vardata$none_missing2 != 1 & vardata$partial_ABG2 != 1, NA, vardata$hr2wt + vardata$bps2wt + vardata$temp2wt + vardata$rr2wt + vardata$pf2wt + vardata$ph2wt + vardata$urea2wt + vardata$cr2wt + vardata$na2wt + vardata$wcc2wt)
describe(vardata$partABG_score2)

## Sum partial score, missing GCS, urine, pH, PF
vardata$part_score1 <- ifelse(vardata$none_missing1 != 1 & vardata$partial_ABG1 != 1 & vardata$partial1 != 1, NA, vardata$hr1wt + vardata$bps1wt + vardata$temp1wt + vardata$rr1wt + vardata$urea1wt + vardata$cr1wt + vardata$na1wt + vardata$wcc1wt)
describe(vardata$part_score1)
vardata$part_score2 <- ifelse(vardata$none_missing2 != 1 & vardata$partial_ABG2 != 1 & vardata$partial2 != 1, NA, vardata$hr2wt + vardata$bps2wt + vardata$temp2wt + vardata$rr2wt + vardata$urea2wt + vardata$cr2wt + vardata$na2wt + vardata$wcc2wt)
describe(vardata$part_score2)


## ======
## Identify clipped scores and variables - note only clipping first variable, as this is what limits trajectory; NOTE this is due to effect on trajectory calculations NOT to get rid of outliers
## ======

#### Change this to 0 or 1, whether value should be used if clipped values are to be used in calculations 
vardata$clipfull <- ifelse(!is.na(vardata$full_score1) & vardata$full_score1 < quantile(vardata$full_score1, 0.90, na.rm=TRUE) & vardata$full_score1 > quantile(vardata$full_score1, 0.10, na.rm=TRUE), 1, 0)
vardata$clippartABG <- ifelse(!is.na(vardata$partABG_score1) & vardata$partABG_score1 < quantile(vardata$partABG_score1, 0.90, na.rm=TRUE) & vardata$partABG_score1 > quantile(vardata$partABG_score1, 0.10, na.rm=TRUE), 1, 0)
vardata$clippart <- ifelse(!is.na(vardata$part_score1) & vardata$part_score1 < quantile(vardata$part_score1, 0.90, na.rm=TRUE) & vardata$part_score1 > quantile(vardata$part_score1, 0.10, na.rm=TRUE), 1, 0)

vardata$cliphr <- ifelse(!is.na(vardata$hr1) & vardata$hr1 < quantile(vardata$hr1, 0.90, na.rm=TRUE) & vardata$hr1 > quantile(vardata$hr1, 0.10, na.rm=TRUE), 1, 0)
vardata$clipbps <- ifelse(!is.na(vardata$bps1) & vardata$bps1 < quantile(vardata$bps1, 0.90, na.rm=TRUE) & vardata$bps1 > quantile(vardata$bps1, 0.10, na.rm=TRUE), 1, 0)
vardata$cliptemp <- ifelse(!is.na(vardata$temp1) & vardata$temp1 < quantile(vardata$temp1, 0.90, na.rm=TRUE) & vardata$temp1 > quantile(vardata$temp1, 0.10, na.rm=TRUE), 1, 0)
vardata$cliprr <- ifelse(!is.na(vardata$rr1) & vardata$rr1 < quantile(vardata$rr1, 0.90, na.rm=TRUE) & vardata$rr1 > quantile(vardata$rr1, 0.10, na.rm=TRUE), 1, 0)
vardata$clippf <- ifelse(!is.na(vardata$pf1) & vardata$pf1 < quantile(vardata$pf1, 0.90, na.rm=TRUE) & vardata$pf1 > quantile(vardata$pf1, 0.10, na.rm=TRUE), 1, 0)
vardata$clipph <- ifelse(!is.na(vardata$ph1) & vardata$ph1 < quantile(vardata$ph1, 0.90, na.rm=TRUE) & vardata$ph1 > quantile(vardata$ph1, 0.10, na.rm=TRUE), 1, 0)
vardata$clipurea <- ifelse(!is.na(vardata$urea1) & vardata$urea1 < quantile(vardata$urea1, 0.90, na.rm=TRUE) & vardata$urea1 > quantile(vardata$urea1, 0.10, na.rm=TRUE), 1, 0)
vardata$clipcr <- ifelse(!is.na(vardata$cr1) & vardata$cr1 < quantile(vardata$cr1, 0.90, na.rm=TRUE) & vardata$cr1 > quantile(vardata$cr1, 0.10, na.rm=TRUE), 1, 0)
vardata$clipna <- ifelse(!is.na(vardata$na1) & vardata$na1 < quantile(vardata$na1, 0.90, na.rm=TRUE) & vardata$na1 > quantile(vardata$na1, 0.10, na.rm=TRUE), 1, 0)
vardata$clipurin <- ifelse(!is.na(vardata$urin1) & vardata$urin1 < quantile(vardata$urin1, 0.90, na.rm=TRUE) & vardata$urin1 > quantile(vardata$urin1, 0.10, na.rm=TRUE), 1, 0)
vardata$clipwcc <- ifelse(!is.na(vardata$wcc1) & vardata$wcc1 < quantile(vardata$wcc1, 0.90, na.rm=TRUE) & vardata$wcc1 > quantile(vardata$wcc1, 0.10, na.rm=TRUE), 1, 0)
vardata$clipgcs <- ifelse(!is.na(vardata$gcs1) & vardata$gcs1 < quantile(vardata$gcs1, 0.90, na.rm=TRUE) & vardata$gcs1 > quantile(vardata$gcs1, 0.10, na.rm=TRUE), 1, 0)


## ======
## Scale score 1 and score 2 from 0 to 1
## ======

## HR
hr1min <- min(vardata$hr1, na.rm = TRUE)
hr1max <- max(vardata$hr1, na.rm = TRUE)
vardata$hr1norm <- ifelse(!is.na(vardata$hr1), 
                          ifelse(vardata$hr1 < 60, (60 - vardata$hr1)/(60 - hr1min), 
                                 ifelse(vardata$hr1 < 90, 0, (vardata$hr1 - 90)/(hr1max - 90))), NA)

hr2min <- min(vardata$hr2, na.rm = TRUE)
hr2max <- max(vardata$hr2, na.rm = TRUE)
vardata$hr2norm <- ifelse(!is.na(vardata$hr2), 
                          ifelse(vardata$hr2 < 60, (60 - vardata$hr2)/(60 - hr2min), 
                                 ifelse(vardata$hr2 < 90, 0, (vardata$hr2 - 90)/(hr2max - 90))), NA)
describe(vardata$hr1norm)
describe(vardata$hr2norm)

## BPS
bps1min <- min(vardata$bps1, na.rm = TRUE)
bps1max <- max(vardata$bps1, na.rm = TRUE)
vardata$bps1norm <- ifelse(!is.na(vardata$bps1), 
                           ifelse(vardata$bps1 < 90, (90 - vardata$bps1)/(90 - bps1min), 
                                  ifelse(vardata$bps1 < 140, 0, (vardata$bps1 - 140)/(bps1max - 140))), NA)

bps2min <- min(vardata$bps2, na.rm = TRUE)
bps2max <- max(vardata$bps2, na.rm = TRUE)
vardata$bps2norm <- ifelse(!is.na(vardata$bps2), 
                           ifelse(vardata$bps2 < 90, (90 - vardata$bps2)/(90 - bps2min), 
                                  ifelse(vardata$bps2 < 140, 0, (vardata$bps2 - 140)/(bps2max - 140))), NA)
describe(vardata$bps1norm)
describe(vardata$bps2norm)

## Temp
temp1min <- min(vardata$temp1, na.rm = TRUE)
temp1max <- max(vardata$temp1, na.rm = TRUE)
vardata$temp1norm <- ifelse(!is.na(vardata$temp1), 
                            ifelse(vardata$temp1 < 36, (36 - vardata$temp1)/(36 - temp1min), 
                                   ifelse(vardata$temp1 < 38, 0, (vardata$temp1 - 38)/(temp1max - 38))), NA)

temp2min <- min(vardata$temp2, na.rm = TRUE)
temp2max <- max(vardata$temp2, na.rm = TRUE)
vardata$temp2norm <- ifelse(!is.na(vardata$temp2), 
                            ifelse(vardata$temp2 < 36, (36 - vardata$temp2)/(36 - temp2min), 
                                   ifelse(vardata$temp2 < 38, 0, (vardata$temp2 - 38)/(temp2max - 38))), NA)
describe(vardata$temp1norm)
describe(vardata$temp2norm)

## RR
rr1min <- min(vardata$rr1, na.rm = TRUE)
rr1max <- max(vardata$rr1, na.rm = TRUE)
vardata$rr1norm <- ifelse(!is.na(vardata$rr1), 
                          ifelse(vardata$rr1 < 8, (8 - vardata$rr1)/(8 - rr1min), 
                                 ifelse(vardata$rr1 < 18, 0, (vardata$rr1 - 18)/(rr1max - 18))), NA)

rr2min <- min(vardata$rr2, na.rm = TRUE)
rr2max <- max(vardata$rr2, na.rm = TRUE)
vardata$rr2norm <- ifelse(!is.na(vardata$rr2), 
                          ifelse(vardata$rr2 < 8, (8 - vardata$rr2)/(8 - rr2min), 
                                 ifelse(vardata$rr2 < 18, 0, (vardata$rr2 - 18)/(rr2max - 18))), NA)
describe(vardata$rr1norm)
describe(vardata$rr2norm)

## pf
pf1min <- min(vardata$pf1, na.rm = TRUE)
pf2min <- min(vardata$pf2, na.rm = TRUE)
vardata$pf1norm <- ifelse(vardata$pf1 >= 60, 0, (60 - vardata$pf1)/(60 - pf1min))
vardata$pf2norm <- ifelse(vardata$pf2 >= 60, 0, (60 - vardata$pf2)/(60 - pf2min))

describe(vardata$pf1norm)
describe(vardata$pf2norm)

## pH
ph1min <- min(vardata$ph1, na.rm = TRUE)
ph1max <- max(vardata$ph1, na.rm = TRUE)
vardata$ph1norm <- ifelse(!is.na(vardata$ph1), 
                          ifelse(vardata$ph1 < 7.35, (7.35 - vardata$ph1)/(7.35 - ph1min), 
                                 ifelse(vardata$ph1 < 7.45, 0, (vardata$ph1 - 7.45)/(ph1max - 7.45))), NA)

ph2min <- min(vardata$ph2, na.rm = TRUE)
ph2max <- max(vardata$ph2, na.rm = TRUE)
vardata$ph2norm <- ifelse(!is.na(vardata$ph2), 
                          ifelse(vardata$ph2 < 7.35, (7.35 - vardata$ph2)/(7.35 - ph2min), 
                                 ifelse(vardata$ph2 < 7.45, 0, (vardata$ph2 - 7.45)/(ph2max - 7.45))), NA)
describe(vardata$ph1norm)
describe(vardata$ph2norm)

## urea
urea1max <- max(vardata$urea1, na.rm = TRUE)
vardata$urea1norm <- ifelse(vardata$urea1 <= 6.1, 0, (vardata$urea1 - 6.1)/(urea1max - 6.1))

urea2max <- max(vardata$urea2, na.rm = TRUE)
vardata$urea2norm <- ifelse(vardata$urea2 <= 6.1, 0, (vardata$urea2 - 6.1)/(urea2max - 6.1))

describe(vardata$urea1norm)
describe(vardata$urea2norm)

## creat
cr1max <- max(vardata$cr1, na.rm = TRUE)
vardata$cr1norm <- ifelse(vardata$cr1 <= 50, 0, (vardata$cr1 - 50)/(cr1max - 50))

cr2max <- max(vardata$cr2, na.rm = TRUE)
vardata$cr2norm <- ifelse(vardata$cr2 <= 50, 0, (vardata$cr2 - 50)/(cr2max - 50))

describe(vardata$cr1norm)
describe(vardata$cr2norm)

## Na
na1min <- min(vardata$na1, na.rm = TRUE)
na1max <- max(vardata$na1, na.rm = TRUE)
vardata$na1norm <- ifelse(!is.na(vardata$na1), 
                          ifelse(vardata$na1 < 135, (135 - vardata$na1)/(135 - na1min), 
                                 ifelse(vardata$na1 < 145, 0, (vardata$na1 - 145)/(na1max - 145))), NA)

na2min <- min(vardata$na2, na.rm = TRUE)
na2max <- max(vardata$na2, na.rm = TRUE)
vardata$na2norm <- ifelse(!is.na(vardata$na2), 
                          ifelse(vardata$na2 < 135, (135 - vardata$na2)/(135 - na2min), 
                                 ifelse(vardata$na2 < 145, 0, (vardata$na2 - 145)/(na2max - 145))), NA)
describe(vardata$na1norm)
describe(vardata$na2norm)

## urine
urin1min <- min(vardata$urin1, na.rm = TRUE)
vardata$urin1norm <- ifelse(vardata$urin1 >= 30, 0, (30 - vardata$urin1)/(30 - urin1min))

urin2min <- min(vardata$urin2, na.rm = TRUE)
vardata$urin2norm <- ifelse(vardata$urin2 >= 30, 0, (30 - vardata$urin2)/(30 - urin2min))

describe(vardata$urin1norm)
describe(vardata$urin2norm)

## wcc
wcc1min <- min(vardata$wcc1, na.rm = TRUE)
wcc1max <- max(vardata$wcc1, na.rm = TRUE)
vardata$wcc1norm <- ifelse(!is.na(vardata$wcc1), 
                           ifelse(vardata$wcc1 < 4, (4 - vardata$wcc1)/(4 - wcc1min), 
                                  ifelse(vardata$wcc1 < 12, 0, (vardata$wcc1 - 12)/(wcc1max - 12))), NA)

wcc2min <- min(vardata$wcc2, na.rm = TRUE)
wcc2max <- max(vardata$wcc2, na.rm = TRUE)
vardata$wcc2norm <- ifelse(!is.na(vardata$wcc2), 
                           ifelse(vardata$wcc2 < 4, (4 - vardata$wcc2)/(4 - wcc2min), 
                                  ifelse(vardata$wcc2 < 12, 0, (vardata$wcc2 - 12)/(wcc2max - 12))), NA)
describe(vardata$wcc1norm)
describe(vardata$wcc2norm)

## gcs
vardata$gcs1norm <- ifelse(vardata$gcs1 == 15, 0, (15 - vardata$gcs1)/(15 - 3))
vardata$gcs2norm <- ifelse(vardata$gcs2 == 15, 0, (15 - vardata$gcs2)/(15 - 3))


## !!!!! Add in lactate

## ======
## Sum scaled scores
## ======
length(which(vardata$none_missing1 ==1 & vardata$none_missing2 == 1))
vardata$scalescorefull1 <- ifelse(vardata$none_missing1 != 1, NA, vardata$hr1norm + vardata$bps1norm + vardata$temp1norm + vardata$rr1norm + vardata$pf1norm + vardata$ph1norm + vardata$urea1norm + vardata$cr1norm + vardata$na1norm + vardata$urin1norm + vardata$wcc1norm + vardata$gcs1norm)
vardata$scalescorefull2 <- ifelse(vardata$none_missing2 != 1, NA, vardata$hr2norm + vardata$bps2norm + vardata$temp2norm + vardata$rr2norm + vardata$pf2norm + vardata$ph2norm + vardata$urea2norm + vardata$cr2norm + vardata$na2norm + vardata$urin2norm + vardata$wcc2norm + vardata$gcs2norm)
describe(vardata$scalescorefull1)
describe(vardata$scalescorefull2)

## Sum with ABG, missing GCS, urine
vardata$scalescorepartABG1 <- ifelse(vardata$none_missing1 != 1 & vardata$partial_ABG1 != 1, NA, vardata$hr1norm + vardata$bps1norm + vardata$temp1norm + vardata$rr1norm + vardata$pf1norm + vardata$ph1norm + vardata$urea1norm + vardata$cr1norm + vardata$na1norm + vardata$wcc1norm)
describe(vardata$scalescorepartABG1)
vardata$scalescorepartABG2 <- ifelse(vardata$none_missing2 != 1 & vardata$partial_ABG2 != 1, NA, vardata$hr2norm + vardata$bps2norm + vardata$temp2norm + vardata$rr2norm + vardata$pf2norm + vardata$ph2norm + vardata$urea2norm + vardata$cr2norm + vardata$na2norm + vardata$wcc2norm)
describe(vardata$scalescorepartABG2)

## Sum partial score, missing GCS, urine, pH, PF
vardata$scalescorepart1 <- ifelse(vardata$none_missing1 != 1 & vardata$partial_ABG1 != 1 & vardata$partial1 != 1, NA, vardata$hr1norm + vardata$bps1norm + vardata$temp1norm + vardata$rr1norm + vardata$urea1norm + vardata$cr1norm + vardata$na1norm + vardata$wcc1norm)
describe(vardata$scalescorepart1)
vardata$scalescorepart2 <- ifelse(vardata$none_missing2 != 1 & vardata$partial_ABG2 != 1 & vardata$partial2 != 1, NA, vardata$hr2norm + vardata$bps2norm + vardata$temp2norm + vardata$rr2norm + vardata$urea2norm + vardata$cr2norm + vardata$na2norm + vardata$wcc2norm)
describe(vardata$scalescorepart2)

describe(vardata$time2icu)

## ======
## Save data
## ======


write.csv(vardata, "/Users/jenniferhunter/Documents/SH PhD Project/Data/varcalcs_data.csv")






