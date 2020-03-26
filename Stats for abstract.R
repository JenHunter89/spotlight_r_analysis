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



## Testing only data where fullscore is complete
fullscoredata <- statsdata %>% filter(!is.na(statsdata$full_scoretraj))

## ++++++++
## Naming:
## var1, var2
## var1wt, var2wt
## vardata$full_score1/2, vardata$partABG_score1/2, vardata$part_score1/2
## clipfull, clippartABG, clippart, clipvar
## var1norm, var2norm
## scalescorefull1/2, scalescorepartABG1/2, scalescorepart1/2
## ++++++++

fullscoredata$sepsisfactor <- ifelse(fullscoredata$sepsis < 3 | is.na(fullscoredata$sepsis), 0, 1)
describe(fullscoredata$sepsisfactor)

clipdata <- statsdata %>% filter(clipfull == 1)

## ======
## Testing for normal distribution
## Note: full_scoretraj negatively skewed, full_score1 and full_score2 are positivley skewed
## Have tried log and sqrt transformation for full_score1 and full_score2 - still not acheiving normality
## Have tried exponential transformation for full_scoretraj - still not normal 
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
## Kruskal Test - think this is valid. 
## Need to see if can test for confounding factors with multivariate - may need to perform other tests to look for significant difference in confounding factors of age and time2icu
## =====

## Test for ICNARC APS trajectory by sepsis binary factor and sepsis likelihood groups

krusktesttraj <- kruskal.test(full_scoretraj ~ sepsisfactor, data = fullscoredata)
krusktesttraj

krusktesttraj2 <- kruskal.test(full_scoretraj ~ sepsis, data = fullscoredata)
krusktesttraj2

medianINCARCsepsis <- quantile(fullscoredata$full_scoretraj[fullscoredata$sepsisfactor == 1], 0.5, na.rm = TRUE)
ICNARCsepsis75 <- quantile(fullscoredata$full_scoretraj[fullscoredata$sepsisfactor == 1], 0.75, na.rm = TRUE)
ICNARCsepsis25 <- quantile(fullscoredata$full_scoretraj[fullscoredata$sepsisfactor == 1], 0.25, na.rm = TRUE)
medianINCARCsepsis
ICNARCsepsis75
ICNARCsepsis25

medianICNARCnosepsis <- quantile(fullscoredata$full_scoretraj[fullscoredata$sepsisfactor == 0], 0.5, na.rm = TRUE)
ICNARCnosepsis75 <- quantile(fullscoredata$full_scoretraj[fullscoredata$sepsisfactor == 0], 0.75, na.rm = TRUE)
ICNARCnosepsis25 <- quantile(fullscoredata$full_scoretraj[fullscoredata$sepsisfactor == 0], 0.25, na.rm = TRUE)
medianICNARCnosepsis
ICNARCnosepsis75
ICNARCnosepsis25

describe(fullscoredata$full_scoretraj)

## Test for ICNARC APS fullscore ward assessment and sepsis groups

krusktestfs1 <- kruskal.test(full_score1 ~ sepsisfactor, data = fullscoredata)
krusktestfs1

krusktesttraj2 <- kruskal.test(full_score1 ~ sepsis, data = fullscoredata)
krusktesttraj2

medianfs1sepsis <- quantile(fullscoredata$full_score1[fullscoredata$sepsisfactor == 1], 0.5, na.rm = TRUE)
medianfs1sepsis

medianfs1nosepsis <- quantile(fullscoredata$full_score1[fullscoredata$sepsisfactor == 0], 0.5, na.rm = TRUE)
medianfs1nosepsis

## ===== 
## Test for age
## =====

krusktestage <- kruskal.test(age ~ sepsisfactor, data = fullscoredata)
krusktestage

krusktestage2 <- kruskal.test(age ~ sepsis, data = fullscoredata)
krusktestage2


## =====
## Count of patients in each sepsis category, and number of patients with complete full score
## =====

length(which(!is.na(fullscoredata$full_scoretraj)))
length(which(fullscoredata$sepsis >= 3))
length(which(fullscoredata$sepsis == 1 | fullscoredata$sepsis == 2))
length(which(is.na(fullscoredata$sepsis)))

length(which(statsdata$sepsis >= 3))/5429
length(which(fullscoredata$sepsis == 1 | fullscoredata$sepsis == 2))/5429


## ======
## Table 1
## ======

##Sepsis severity likely
describe(fullscoredata$sepsisfactor)

##Age
describe(fullscoredata$age[fullscoredata$sepsisfactor == 1])
describe(fullscoredata$age[fullscoredata$sepsisfactor == 0])

##Ward severity score
describe(fullscoredata$full_score1[fullscoredata$sepsisfactor == 1])
describe(fullscoredata$full_score1[fullscoredata$sepsisfactor == 0])

##Trajectory
describe(fullscoredata$full_scoretraj[fullscoredata$sepsisfactor == 1])
describe(fullscoredata$full_scoretraj[fullscoredata$sepsisfactor == 0])

##Time2Icu
describe(fullscoredata$time2icu[fullscoredata$sepsisfactor == 1])
describe(fullscoredata$time2icu[fullscoredata$sepsisfactor == 0])

##28dmort
describe(fullscoredata$dead28[fullscoredata$sepsisfactor == 1])
describe(fullscoredata$dead28[fullscoredata$sepsisfactor == 0])

##====
## Look at diagnosis with fullscore data
##====

## table of diagnoses
diagnosistable <- count(fullscoredata, 'raicu1')
diagnosistable$diag <- "other"
## NB 2.1.4.27.5 = pneumonia other, 2.1.4.27.1 = pneumonia (bacterial)
diagnosistable$diag[diagnosistable$raicu1 == '2.1.4.27.5'] <- "Pneumonia"
diagnosistable$diag[diagnosistable$raicu1 == '2.1.4.27.1'] <- "Pneumonia"
diagnosistable$diag[diagnosistable$raicu1 == '2.2.12.35.2'] <- "Septic shock"
##diagnosistable$diag[diagnosistable$raicu1 == '2.3.9.28.1'] <- "Acute pancreatitis"
diagnosistable$diag[diagnosistable$raicu1 == '2.7.1.13.4'] <- "Acute renal failure (infection related)"
##diagnosistable$diag[diagnosistable$raicu1 == '2.7.1.13.1'] <- "Acute renal failure (haemodynamic related)"
##diagnosistable$diag[diagnosistable$raicu1 == '2.7.1.13.2'] <- "Acute renal failure (toxin/drug related)"
diagnosistable$diag[diagnosistable$raicu1 == '2.9.1.27.4'] <- "Septicaemia"
##diagnosistable$diag[diagnosistable$raicu1 == '2.4.2.33.1'] <- "Status epilepticus"
##diagnosistable$diag[diagnosistable$raicu1 == '2.1.4.31.9'] <- "Cardiogenic pulmonary oedema"
diagnosistable$diag[diagnosistable$raicu1 == '2.7.1.27.1'] <- "Urosepsis"
diagnosistable$diag[diagnosistable$raicu1 == '2.7.2.27.1'] <- "Urosepsis"

# create code in main table based on above diagnoses
fullscoredata$admdiag <- 0
fullscoredata$admdiag[fullscoredata$raicu1 == '2.1.4.27.5'] <- 1
fullscoredata$admdiag[fullscoredata$raicu1 == '2.1.4.27.1'] <- 1
fullscoredata$admdiag[fullscoredata$raicu1 == '2.2.12.35.2'] <- 2
##fullscoredata$admdiag[fullscoredata$raicu1 == '2.3.9.28.1'] <- 4
fullscoredata$admdiag[fullscoredata$raicu1 == '2.7.1.13.4'] <- 3
##fullscoredata$admdiag[fullscoredata$raicu1 == '2.7.1.13.1'] <- 6
##fullscoredata$admdiag[fullscoredata$raicu1 == '2.7.1.13.2'] <- 7
fullscoredata$admdiag[fullscoredata$raicu1 == '2.9.1.27.4'] <- 4
##fullscoredata$admdiag[fullscoredata$raicu1 == '2.4.2.33.1'] <- 9
##fullscoredata$admdiag[fullscoredata$raicu1 == '2.1.4.31.9'] <- 10
fullscoredata$admdiag[fullscoredata$raicu1 == '2.7.1.27.1'] <- 5
fullscoredata$admdiag[fullscoredata$raicu1 == '2.7.2.27.1'] <- 5

fullscoredata$diag <- "Other"
fullscoredata$diag[fullscoredata$raicu1 == '2.1.4.27.5'] <- "Pneumonia"
fullscoredatae$diag[fullscoredata$raicu1 == '2.1.4.27.1'] <- "Pneumonia"
fullscoredata$diag[fullscoredata$raicu1 == '2.2.12.35.2'] <- "Septic shock"
##fullscoredata$diag[fullscoredata$raicu1 == '2.3.9.28.1'] <- "Acute pancreatitis"
fullscoredata$diag[fullscoredata$raicu1 == '2.7.1.13.4'] <- "Acute renal failure (infection related)"
##fullscoredata$diag[fullscoredata$raicu1 == '2.7.1.13.1'] <- "Acute renal failure (haemodynamic related)"
##fullscoredata$diag[fullscoredata$raicu1 == '2.7.1.13.2'] <- "Acute renal failure (toxin/drug related)"
fullscoredata$diag[fullscoredata$raicu1 == '2.9.1.27.4'] <- "Septicaemia"
##fullscoredata$diag[fullscoredata$raicu1 == '2.4.2.33.1'] <- "Status epilepticus"
##fullscoredata$diag[fullscoredata$raicu1 == '2.1.4.31.9'] <- "Cardiogenic pulmonary oedema"
fullscoredata$diag[fullscoredata$raicu1 == '2.7.1.27.1'] <- "Urosepsis"
fullscoredata$diag[fullscoredata$raicu1 == '2.7.2.27.1'] <- "Urosepsis"

describe(fullscoredata$diag)

fullscoredata$admdiag <- as.factor(fullscoredata$admdiag)

fullscoredata$sepsisbinary <- ifelse(fullscoredata$admdiag == 0, 0, 1)

krusktestsepsisdiag <- kruskal.test(full_scoretraj ~ admdiag, data = fullscoredata)
krusktestsepsisdiag

krusktestsepsisbinary <- kruskal.test(full_scoretraj ~ sepsisbinary, data = fullscoredata)
krusktestsepsisbinary

##====
## Multvar analysis 
##====

fullscoredata.narm <- filter(fullscoredata, !is.na(fullscoredata$admdiag) & !is.na(fullscoredata$) & !is.na(fullscoredata$full_score1) & !is.na(fullscoredata$age) & !is.na(fullscoredata$lac1) & !is.na(fullscoredata$trajbinary))

multvar1 <- glm(dead28 ~ sepsisbinary + full_scoretraj + full_score1 + age + lac1, data = fullscoredata.narm, family = binomial(link = "logit"))
summary(multvar1)


multvar2 <- glm(dead28 ~ sepsisbinary + full_scoretraj + full_score1 + age + lac1 + sepsisbinary*full_scoretraj, data = fullscoredata.narm, family = binomial(link = "logit"))
summary(multvar2)    

multvar3 <- glm(dead28 ~ sepsis + full_scoretraj + full_score1 + full_score2 + age + lac1, data = fullscoredata.narm, family = binomial(link = "logit"))
summary(multvar3)
multvar3

multvar4 <- glm(dead28 ~ full_scoretraj + full_score1 + age + lac1 + admdiag, data = fullscoredata.narm, family = binomial(link = "logit"))
summary(multvar4)
contrasts(fullscoredata.narm$admdiag)

medtraj <- quantile(fullscoredata$full_scoretraj, 0.5, na.rm = TRUE)

ggplot(data = filter(fullscoredata, !is.na(sepsis) & time2icu < quantile(time2icu, 0.9, na.rm = TRUE))) +
  geom_point(aes(x = 0, y = full_score1), size = 0.1) +
  geom_point(aes(x = time2icu, y = full_score2), size = 0.1) + 
  geom_segment(aes(x = 0, y = full_score1, xend = time2icu, yend = full_score2), colour = "light blue") +
  geom_segment(aes(x = 0, y = quantile(full_score1, 0.5, na.rm = TRUE), xend = 20, yend = quantile(full_score1, 0.5, na.rm = TRUE) + (medtraj * 20)), size = 0.1) +
  theme(text = element_text(size=20))
