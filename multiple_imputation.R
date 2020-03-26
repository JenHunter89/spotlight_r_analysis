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
install.packages("mice")


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
library(mice)

setwd("/Users/jenniferhunter/Documents/SH PhD Project")

imputdata <- read_csv("Data/variables_data.csv")

imputdatavars <- select(imputdata, hr1, hr2, bps1, bps2, temp1, temp2, rr1, rr2, pf1, pf2, ph1, ph2, urea1, urea2, cr1, cr2, na1, na2, urin1, urin2, wcc1, wcc2, gcs1, gcs2, lac1, lac2)
imputdatavarsminuslac <- select(imputdata, hr1, hr2, bps1, bps2, temp1, temp2, rr1, rr2, pf1, pf2, ph1, ph2, urea1, urea2, cr1, cr2, na1, na2, urin1, urin2, wcc1, wcc2, gcs1, gcs2)

sum(is.na(imputdatavarsminuslac))
##16495
sum(!is.na(imputdatavarsminuslac))
##113801
## Check: 5429 x 24 = 130,296
## Sum above is the same
16495/130296

md.pattern(imputdatavars)

imputed_data <- mice(imputdatavars, m=5, maxit = 50, method = 'pmm', seed = 500)

fulldata <- complete(imputed_data, 1)

md.pattern(fulldata)

imputdata$hr1 <- fulldata$hr1
imputdata$hr2 <- fulldata$hr2
imputdata$bps1 <- fulldata$bps1
imputdata$bps2 <- fulldata$bps2
imputdata$temp1 <- fulldata$temp1
imputdata$temp2 <- fulldata$temp2
imputdata$rr1 <- fulldata$rr1
imputdata$rr2 <- fulldata$rr2
imputdata$pf1 <- fulldata$pf1
imputdata$pf2 <- fulldata$pf2
imputdata$ph1 <- fulldata$ph1
imputdata$ph2 <- fulldata$ph2
imputdata$urea1 <- fulldata$urea1
imputdata$urea2 <- fulldata$urea2
imputdata$cr1 <- fulldata$cr1
imputdata$cr2 <- fulldata$cr2
imputdata$na1 <- fulldata$na1
imputdata$na2 <- fulldata$na2
imputdata$urin1 <- fulldata$urin1
imputdata$urin2 <- fulldata$urin2
imputdata$wcc1 <- fulldata$wcc1
imputdata$wcc2 <- fulldata$wcc2
imputdata$gcs1 <- fulldata$gcs1
imputdata$gcs2 <- fulldata$gcs2
imputdata$lac1 <- fulldata$lac1
imputdata$lac2 <- fulldata$lac2

write.csv(imputdata, "Data/imputdata.csv")
