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

variables_data <- read_dta("Data/stata_variables.dta", encoding='latin1')
screening_data <- read_csv("Data/exclusions_data.csv")

names(screening_data)

merge_data <- merge(variables_data, screening_data[ , c("idpatient", "include", "exclude1", "exclude2", "exclude3", "exclude4", "exclude5", "last_trace", "included_sites", "included_months")], by = intersect(variables_data$idpatient, screening_data$idpatient),
      by.x = "idpatient", by.y = "idpatient", all.x = TRUE, all.y = FALSE)

names(merge_data)

colnames(merge_data)[322] <- "include"
colnames(merge_data)[323] <- "exclude1"
colnames(merge_data)[324] <- "exclude2"
colnames(merge_data)[325] <- "exclude3"
colnames(merge_data)[326] <- "exclude4"
colnames(merge_data)[327] <- "exclude5"
colnames(merge_data)[328] <- "last_trace"
colnames(merge_data)[329] <- "included_sites"
colnames(merge_data)[330] <- "included_months"

merge_data$include.x <- NULL
merge_data$exclude1.x <- NULL
merge_data$exclude2.x <- NULL
merge_data$exclude3.x <- NULL
merge_data$exclude4.x <- NULL
merge_data$exclude5.x <- NULL
merge_data$last_trace.x <- NULL
merge_data$included_sites.x <- NULL
merge_data$included_months.x <- NULL

merge_data_screen <- subset(merge_data, merge_data$include == 1 & merge_data$exclude1 == 0 & merge_data$exclude2 == 0 & merge_data$exclude3 == 0 & merge_data$exclude4 == 0 & merge_data$exclude5 == 0)

names(merge_data_screen)

write.csv(merge_data_screen, "/Users/jenniferhunter/Documents/SH PhD Project/Data/variables_data.csv")

##tomergefilter <- tomerge %>% filter(tomerge$idpatient %in% analysisdata$idpatient)

##m_mergedata <- merge(analysisdata, variables_data, by = intersect(analysisdata$idpatient, tomerge$idpatient), 
                   ##by.x = "idpatient", by.y = "idpatient", all.x = TRUE, all.y = FALSE)

##m_finaldata <- m_mergedata[!duplicated(as.list(m_mergedata))]
