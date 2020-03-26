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
install.packages("RColorBrewer")
install.packages("ggrepel")
install.packages("magrittr")
install.packages("qwraps2")
install.packages("sqldf")
install.packages("scales")
install.packages("huxtable")
install.packages("FSA")
install.packages("ggpubr", dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("ISLR")
install.packages("pROC")


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
library(RColorBrewer)
library(ggrepel)
library(magrittr)
library(qwraps2)
library(sqldf)
library(scales)
library(huxtable)
library(dunn.test)
library(ggpubr)
library(ISLR)
library(pROC)

setwd("/Users/jenniferhunter/Documents/SH PhD Project")

imputstats <- read_csv("Data/imputedtraj.csv")


## ====
## Create a lactate trajectory variable
## No weighting, so use absolute variable
## ====

imputstats$lactraj <- (imputstats$lac2 - imputstats$lac1)/imputstats$time2icu
describe(imputstats$lactraj)

## ====
## Descriptive histograms for imputed trajectories, ward score and admissions scores, showing non normal distrubcutions
## Upper and lower 2% of trajectories removed for ease of interpretation 
## ====

ggplot(data = imputstats %>% filter(imputstats$imputtraj > quantile(imputstats$imputtraj, 0.02, na.rm = TRUE) & imputstats$imputtraj < quantile(imputstats$imputtraj, 0.98, na.rm = TRUE)),
       aes(x = imputtraj)) + 
       geom_histogram(binwidth = 1, colour = "dodgerblue3", fill = "dodgerblue3") +
       theme(text = element_text(size=20))

ggplot(data = imputstats,
       aes(x = full_score1)) + 
       geom_histogram(binwidth = 1, colour = "dodgerblue3", fill = "dodgerblue3") +
       theme(text = element_text(size=20))

ggplot(data = imputstats,
       aes(x = full_score2)) + 
  geom_histogram(binwidth = 1, colour = "dodgerblue3", fill = "dodgerblue3") +
  theme(text = element_text(size=20))

## =====
## Basic description of imputed data set - median and IQR for admission score, ward score, trajectory and time2icu
## =====

describe(imputstats$full_score1)
describe(imputstats$full_score2)
describe(imputstats$imputtraj)
describe(imputstats$time2icu)
describe(imputstats$age)
describe(imputstats$dead28)


## =====
## Looking at link between time2icu and trajectory, to look for evidence for lead time bias
## ====

## Spearman rank correlation coefficient test for association between trajectory and time 2 icu
## Looks at theory of lead time bias - does a longer time 2 icu associate with a better trajectory?
## Answer - positive association - longer time2icu = more positive trajectory, i.e. worse

assoc_traj_time2icu <- cor.test(imputstats$imputtraj, imputstats$time2icu, method = "spearman")
assoc_traj_time2icu

describe(imputstats$time2icu)

## Create categorical variable for time2icu: early = ≤1 hour, average = 1-9, late = ≥9
imputstats$timecat <- cut(imputstats$time2icu, breaks = c(0,1,9,Inf), labels = c("Short", "Average", "Long"))
describe(imputstats$timecat)

chisq.test(imputstats$dead28, imputstats$timecat,
           correct = FALSE)

kruskal.test(imputtraj ~ timecat, data = imputstats)
kruskal.test(full_score2 ~ timecat, data = imputstats)
kruskal.test(full_score1 ~ timecat, data = imputstats)

ggplot(data = imputstats,
       aes(x = timecat, y = imputtraj)) +
  geom_boxplot()

ggplot(data = imputstats,
       aes(x = timecat, y = full_score1)) +
  geom_boxplot()

ggplot(data = imputstats,
       aes(x = timecat, y = full_score2)) +
  geom_boxplot()

## Create binary variable delayed vs non-delayed based on crit care 2014 Hung et al delayed admission paper
## (They find mortality effect emerges at 4 hours)

imputstats$delayed <- ifelse(imputstats$time2icu <= 4, 0, 1)
describe(imputstats$delayed)

kruskal.test(imputtraj ~ delayed, data = imputstats)
kruskal.test(full_score2 ~ delayed, data = imputstats)
kruskal.test(full_score1 ~ delayed, data = imputstats)

describe(imputstats$full_score1[imputstats$delayed == 0])
describe(imputstats$full_score1[imputstats$delayed == 1])
describe(imputstats$full_score2[imputstats$delayed == 0])
describe(imputstats$full_score2[imputstats$delayed == 1])
describe(imputstats$imputtraj[imputstats$delayed == 0])
describe(imputstats$imputtraj[imputstats$delayed == 1])
describe(imputstats$dead28[imputstats$delayed == 0])
describe(imputstats$dead28[imputstats$delayed == 1])

chisq.test(imputstats$dead28, imputstats$delayed,
           correct = FALSE)

imputstats$delayed <- as.factor(imputstats$delayed)

ggplot(data = imputstats,
       aes(x = delayed, y = imputtraj)) +
  geom_boxplot()

ggplot(data = imputstats,
       aes(x = delayed, y = full_score1)) +
  geom_boxplot()

ggplot(data = imputstats,
       aes(x = delayed, y = full_score2)) +
  geom_boxplot()


## Spearman rank correlation coefficient test for association between trajectory and time 2 icu
## Looks at theory of lead time bias - does a longer time 2 icu associate with worse admission severity score?
## Answer: no significant association 

imputstats$full_score2 <- as.numeric(imputstats$full_score2)

assoc_critcarescore_time2icu <- cor.test(imputstats$full_score2, imputstats$time2icu, method = "spearman")
assoc_critcarescore_time2icu

ggplot(data = imputstats %>% filter(time2icu < 150), 
       aes(x=time2icu, y=full_score2)) +
  geom_point()



## ======
## Kruskall wallace test - difference in trajectory by sepsis category 1-4
## ======

## Create a binary sepsis factor - either likely or unlikely 
imputstats$sepsisfactor <- ifelse(imputstats$sepsis < 3 | is.na(imputstats$sepsis), 0, 1)


## Significant: sepsis with 4 categories; significant for trajectory, ward severity score, icu severity score, dead28
krusktestsepsistraj <- kruskal.test(imputtraj ~ sepsis, data = imputstats)
krusktestsepsistraj
pairwise.wilcox.test(imputstats$imputtraj, imputstats$sepsis)
dunn.test(imputstats$imputtraj, imputstats$sepsis,
          method = "bh")

krusktestsepsiswardscore <- kruskal.test(full_score1 ~ sepsis, data = imputstats)
krusktestsepsiswardscore
dunn.test(imputstats$full_score1, imputstats$sepsis,
          method = "bh")

krusktestsepsisicuscore <- kruskal.test(full_score2 ~ sepsis, data = imputstats)
krusktestsepsisicuscore
dunn.test(imputstats$full_score2, imputstats$sepsis,
          method = "bh")

krusktestsepsistime2icu <- kruskal.test(time2icu ~ sepsis, data = imputstats)
krusktestsepsistime2icu
dunn.test(imputstats$time2icu, imputstats$sepsis,
          method = "bh")

krusktestsepsisdead28 <- kruskal.test(dead28 ~ sepsis, data = imputstats)
chisq.test(imputstats$dead28, imputstats$sepsis,
           correct = FALSE)

## ===
## Trajectory, ward score, time2icu and dead28 quantiles/% for each sepsis group
## NB could just do describe, testing whether can store values to output as table 

## Group 1
trajsepsis1 <- quantile(imputstats$imputtraj[imputstats$sepsis == 1], c(0.25, 0.5,0.75), na.rm = TRUE)
wardscoresepsis1 <- quantile(imputstats$full_score1[imputstats$sepsis == 1], c(0.25, 0.5,0.75), na.rm = TRUE)
time2icusepsis1 <- quantile(imputstats$time2icu[imputstats$sepsis == 1], c(0.25, 0.5,0.75), na.rm = TRUE)
dead28sepsis1 <- mean(imputstats$dead28[imputstats$sepsis == 1], na.rm = TRUE)
trajsepsis1
wardscoresepsis1
time2icusepsis1
dead28sepsis1
length(which(imputstats$sepsis == 1))

## Group 2
trajsepsis2 <- quantile(imputstats$imputtraj[imputstats$sepsis == 2], c(0.25, 0.5,0.75), na.rm = TRUE)
wardscoresepsis2 <- quantile(imputstats$full_score1[imputstats$sepsis == 2], c(0.25, 0.5,0.75), na.rm = TRUE)
time2icusepsis2 <- quantile(imputstats$time2icu[imputstats$sepsis == 2], c(0.25, 0.5,0.75), na.rm = TRUE)
dead28sepsis2 <- mean(imputstats$dead28[imputstats$sepsis == 2], na.rm = TRUE)
trajsepsis2
wardscoresepsis2
time2icusepsis2
dead28sepsis2
length(which(imputstats$sepsis == 2))

## Group 3
trajsepsis3 <- quantile(imputstats$imputtraj[imputstats$sepsis == 3], c(0.25, 0.5,0.75), na.rm = TRUE)
wardscoresepsis3 <- quantile(imputstats$full_score1[imputstats$sepsis == 3], c(0.25, 0.5,0.75), na.rm = TRUE)
time2icusepsis3 <- quantile(imputstats$time2icu[imputstats$sepsis == 3], c(0.25, 0.5,0.75), na.rm = TRUE)
dead28sepsis3 <- mean(imputstats$dead28[imputstats$sepsis == 3], na.rm = TRUE)
trajsepsis3
wardscoresepsis3
time2icusepsis3
dead28sepsis3
length(which(imputstats$sepsis == 3))

## Group 4
trajsepsis4 <- quantile(imputstats$imputtraj[imputstats$sepsis == 4], c(0.25, 0.5,0.75), na.rm = TRUE)
wardscoresepsis4 <- quantile(imputstats$full_score1[imputstats$sepsis == 4], c(0.25, 0.5,0.75), na.rm = TRUE)
time2icusepsis4 <- quantile(imputstats$time2icu[imputstats$sepsis == 4], c(0.25, 0.5,0.75), na.rm = TRUE)
dead28sepsis4 <- mean(imputstats$dead28[imputstats$sepsis == 4], na.rm = TRUE)
trajsepsis4
wardscoresepsis4
time2icusepsis4
dead28sepsis4
length(which(imputstats$sepsis == 4))


## ===
## Boxplots for differences in sepsis groups, trajectory and ward severity score by Likert Scale
## ====

## Boxplot trajectory with sepsis group  
ggplot(data = filter(imputstats, !is.na(sepsis))) +
  geom_boxplot(aes(x = sepsis, y = imputtraj, fill = sepsis), outlier.shape = NA) +
  scale_y_continuous(limits = quantile(imputstats$imputtraj, c(0.1, 0.95), na.rm = TRUE)) +
  coord_cartesian(ylim = c(-12.5,7.5)) +
  scale_fill_brewer(name = "", labels = c("Sepsis very unlikely", "Sepsis unlikely", "Sepsis likely", "Sepsis very likely"), palette = "Blues") +
  scale_x_discrete(labels = c("Sepsis very unlikely", "Sepsis unlikely", "Sepsis likely", "Sepsis very likely")) +
  theme(text = element_text(size=24)) +
  labs(x = "", y = "")

## Boxplot ward score with sepsis group 
ggplot(data = filter(imputstats, !is.na(sepsis))) +
  geom_boxplot(aes(x = sepsis, y = full_score1, fill = sepsis), outlier.shape = NA) +
##  scale_y_continuous(limits = c(0, quantile(imputstats$full_score1, c(0.99), na.rm = TRUE))) +
  coord_cartesian(ylim = c(0,45)) +  
  scale_fill_brewer(name = "", labels = c("Sepsis very unlikely", "Sepsis unlikely", "Sepsis likely", "Sepsis very likely"), palette = "Blues") +
  scale_x_discrete(labels = c("Sepsis very unlikely", "Sepsis unlikely", "Sepsis likely", "Sepsis very likely")) +
  theme(text = element_text(size=24)) +
  labs(x = "", y = "")

## Not significant: binary sepsis by likert scale
krusktestsepsisfactor <- kruskal.test(imputtraj ~ sepsisfactor, data = imputstats)
krusktestsepsisfactor

## diagnosis with traj
krusktestdx <- kruskal.test(imputtraj ~ raicu1, data = imputstats)
krusktestdx
imputstats$raicu1 <- as.factor(imputstats$raicu1)
describe(imputstats$raicu1)
summary(imputstats$raicu1)

## ====
## Look at data by ICNARC CMP diagnosis
## ====

## table of diagnoses for looking at frequency, label top 10 diagnoses by frequency 
diagnosistable <- count(imputstats, 'raicu1')
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

# create code in main table based on above diagnoses, sepsis only 
imputstats$admdiag <- 0
imputstats$admdiag[imputstats$raicu1 == '2.1.4.27.5'] <- 1
imputstats$admdiag[imputstats$raicu1 == '2.1.4.27.1'] <- 1
imputstats$admdiag[imputstats$raicu1 == '2.2.12.35.2'] <- 2
imputstats$admdiag[imputstats$raicu1 == '2.7.1.13.4'] <- 3
imputstats$admdiag[imputstats$raicu1 == '2.9.1.27.4'] <- 4
imputstats$admdiag[imputstats$raicu1 == '2.7.1.27.1'] <- 5
imputstats$admdiag[imputstats$raicu1 == '2.7.2.27.1'] <- 5

describe(imputstats$admdiag)

##Non sepsis diagnoses
##imputstats$admdiag[imputstats$raicu1 == '2.3.9.28.1'] <- 6
##imputstats$admdiag[imputstats$raicu1 == '2.7.1.13.1'] <- 7
##imputstats$admdiag[imputstats$raicu1 == '2.7.1.13.2'] <- 8
##imputstats$admdiag[imputstats$raicu1 == '2.4.2.33.1'] <- 9
##imputstats$admdiag[imputstats$raicu1 == '2.1.4.31.9'] <- 10

## name label for sepsis diagnoses
imputstats$diag <- "Other"
imputstats$diag[imputstats$raicu1 == '2.1.4.27.5'] <- "Pneumonia"
imputstats$diag[imputstats$raicu1 == '2.1.4.27.1'] <- "Pneumonia"
imputstats$diag[imputstats$raicu1 == '2.2.12.35.2'] <- "Septic shock"
imputstats$diag[imputstats$raicu1 == '2.7.1.13.4'] <- "Acute renal failure (infection related)"
imputstats$diag[imputstats$raicu1 == '2.9.1.27.4'] <- "Septicaemia"
imputstats$diag[imputstats$raicu1 == '2.7.1.27.1'] <- "Urosepsis"
imputstats$diag[imputstats$raicu1 == '2.7.2.27.1'] <- "Urosepsis"

##is there significant difference between different sepsis diagnoses. Answer  = no
notother <- filter(imputstats, admdiag != 0)
krusktestadmdiagnotother <- kruskal.test(imputtraj ~ admdiag, data = notother)
krusktestadmdiagnotother

## is there a significant difference between all admission diagnoses? Yes, but this includes all, too many 
krusktestadmdiag <- kruskal.test(imputtraj ~ admdiag, data = imputstats)
krusktestadmdiag


## Comparison of trajectories for sepsis and non-sepsis patients, could just use describe function - should this go somewhere else? 
mediantrajsepsis <- quantile(imputstats$imputtraj[imputstats$admdiag != 0], 0.5, na.rm = TRUE)
mediantrajnonsepsis <- quantile(imputstats$imputtraj[imputstats$admdiag == 0], 0.5, na.rm = TRUE)
q25trajsepsis <- quantile(imputstats$imputtraj[imputstats$admdiag != 0], 0.25, na.rm = TRUE)
q25trajnonsepsis <- quantile(imputstats$imputtraj[imputstats$admdiag == 0], 0.25, na.rm = TRUE)
q75trajsepsis <- quantile(imputstats$imputtraj[imputstats$admdiag != 0], 0.75, na.rm = TRUE)
q75trajnonsepsis <- quantile(imputstats$imputtraj[imputstats$admdiag == 0], 0.75, na.rm = TRUE)


## calculate medians for full_score to use in graph 
medfsdiag0 = quantile(imputstats$full_score1[imputstats$admdiag == 0], 0.5, na.rm = TRUE)
medfsdiag1 = quantile(imputstats$full_score1[imputstats$admdiag == 1], 0.5, na.rm = TRUE)
medfsdiag2 = quantile(imputstats$full_score1[imputstats$admdiag == 2], 0.5, na.rm = TRUE)
medfsdiag3 = quantile(imputstats$full_score1[imputstats$admdiag == 3], 0.5, na.rm = TRUE)
medfsdiag4 = quantile(imputstats$full_score1[imputstats$admdiag == 4], 0.5, na.rm = TRUE)
medfsdiag5 = quantile(imputstats$full_score1[imputstats$admdiag == 5], 0.5, na.rm = TRUE)
medfsdiagall = quantile(imputstats$full_score1, 0.5, na.rm = TRUE)
medfsdiagsepsis = quantile(imputstats$full_score1[imputstats$admdiag != 0], 0.5, na.rm = TRUE)


## calculate medians for trajectory to use in graph
medtrajdiag0 = quantile(imputstats$imputtraj[imputstats$admdiag == 0], 0.5, na.rm = TRUE)
medtrajdiag1 = quantile(imputstats$imputtraj[imputstats$admdiag == 1], 0.5, na.rm = TRUE)
medtrajdiag2 = quantile(imputstats$imputtraj[imputstats$admdiag == 2], 0.5, na.rm = TRUE)
medtrajdiag3 = quantile(imputstats$imputtraj[imputstats$admdiag == 3], 0.5, na.rm = TRUE)
medtrajdiag4 = quantile(imputstats$imputtraj[imputstats$admdiag == 4], 0.5, na.rm = TRUE)
medtrajdiag5 = quantile(imputstats$imputtraj[imputstats$admdiag == 5], 0.5, na.rm = TRUE)
medtrajdiagall = quantile(imputstats$imputtraj, 0.5, na.rm = TRUE)
medtrajdiagsepsis = quantile(imputstats$imputtraj[imputstats$admdiag != 0], 0.5, na.rm = TRUE)

diag_names1 <- c('1' = "Pneumonia (other)",
                '2' = "Septic Shock",
                '3' = "AKI (infection related)",
                '4' = "Septicaemia",
                '5' = "Urosepsis")

## graph plotting sepsis diagnosis trajectories with all sepsis and all patients, facet wrap by sepsis diagnosis 
ggplot(data = filter(imputstats, time2icu < quantile(time2icu, 0.9, na.rm = TRUE))) + 
  facet_wrap(~admdiag, nrow = 1, labeller = as_labeller(diag_names1)) +
  geom_segment(data = filter(imputstats, admdiag == 1),
               aes(x = 0, y = medfsdiag1, xend = 20, yend = (medfsdiag1 + medtrajdiag1 * 20)), colour = "blue", size = 0.01) +
  geom_segment(data = filter(imputstats, admdiag == 2),
               aes(x = 0, y = medfsdiag2, xend = 20, yend = (medfsdiag2 + medtrajdiag2 * 20)), colour = "blue", size = 0.01) +
  geom_segment(data = filter(imputstats, admdiag == 3),
               aes(x = 0, y = medfsdiag3, xend = 20, yend = (medfsdiag3 + medtrajdiag3 * 20)), colour = "blue", size = 0.01) +
  geom_segment(data = filter(imputstats, admdiag == 4),
               aes(x = 0, y = medfsdiag4, xend = 20, yend = (medfsdiag4 + medtrajdiag4 * 20)), colour = "blue", size = 0.01) +
  geom_segment(data = filter(imputstats, admdiag == 5),
               aes(x = 0, y = medfsdiag5, xend = 20, yend = (medfsdiag5 + medtrajdiag5 * 20)), colour = "blue", size = 0.01) +
  geom_segment(data = filter(imputstats, admdiag == 0),
               aes(x = 0, y = medfsdiag0, xend = 20, yend = (medfsdiag0 + medtrajdiag0 * 20)), colour = "blue", size = 0.01) +
  geom_segment(data = imputstats,
               aes(x = 0, y = medfsdiagall, xend = 20, yend = (medfsdiagall + medtrajdiagall * 20)), colour = "aquamarine4", size = 0.01) +
  geom_segment(data = imputstats,
               aes(x = 0, y = medfsdiagsepsis, xend = 20, yend = (medfsdiagsepsis + medtrajdiagsepsis * 20)), colour = "red", size = 0.01) +
  labs(x = "Hours since ward assessment", y = "ICNARC APS severity score")

## graph plotting sepsis vs non sepsis 
ggplot(data = filter(imputstats, time2icu < quantile(time2icu, 0.9, na.rm = TRUE))) + 
  geom_segment(data = imputstats,
               aes(x = 0, y = medfsdiag0, xend = 20, yend = (medfsdiag0 + medtrajdiag0 * 20)), colour = "blue", size = 0.01) +
  geom_segment(data = imputstats,
               aes(x = 0, y = medfsdiagall, xend = 20, yend = (medfsdiagall + medtrajdiagall * 20)), colour = "black", size = 0.01) +
  geom_segment(data = imputstats,
               aes(x = 0, y = medfsdiagsepsis, xend = 20, yend = (medfsdiagsepsis + medtrajdiagsepsis * 20)), colour = "red", size = 0.01) +
  labs(x = "Hours since ward assessment", y = "ICNARC APS severity score") +
  theme(text = element_text(size=20))

## graph plotting different sepsis categories on one graph 
ggplot(data = filter(imputstats, time2icu < quantile(time2icu, 0.9, na.rm = TRUE) & admdiag != 0)) + 
  geom_segment(data = filter(imputstats, admdiag == 1),
               aes(x = 0, y = medfsdiag1, xend = 20, yend = (medfsdiag1 + medtrajdiag1 * 20)), colour = "blue", size = 0.01) +
  geom_segment(data = filter(imputstats, admdiag == 2),
               aes(x = 0, y = medfsdiag2, xend = 20, yend = (medfsdiag2 + medtrajdiag2 * 20)), colour = "forestgreen", size = 0.01) +
  geom_segment(data = filter(imputstats, admdiag == 3),
               aes(x = 0, y = medfsdiag3, xend = 20, yend = (medfsdiag3 + medtrajdiag3 * 20)), colour = "red", size = 0.01) +
  geom_segment(data = filter(imputstats, admdiag == 4),
               aes(x = 0, y = medfsdiag4, xend = 20, yend = (medfsdiag4 + medtrajdiag4 * 20)), colour = "purple", size = 0.01) +
  geom_segment(data = filter(imputstats, admdiag == 5),
               aes(x = 0, y = medfsdiag5, xend = 20, yend = (medfsdiag5 + medtrajdiag5 * 20)), colour = "orange", size = 0.01) +
  geom_segment(data = imputstats,
               aes(x = 0, y = medfsdiagall, xend = 20, yend = (medfsdiagall + medtrajdiagall * 20)), colour = "black", size = 0.01) +
  labs(x = "Hours since ward assessment", y = "ICNARC APS severity score") +
  theme(text = element_text(size=20))

### ===
## Analysis based on sepsis diagnosis binary based on icnarc coded diagnosis
### ===

describe(imputstats$admdiag)

imputstats$icnarcsepsisbinary <- ifelse(imputstats$admdiag <= 5 & imputstats$admdiag != 0, 1, 0)
describe(imputstats$icnarcsepsisbinary)

wilcoxtestsepsisbinarytraj <- wilcox.test(imputtraj ~ icnarcsepsisbinary, data = imputstats)
wilcoxtestsepsisbinarytraj
describe(imputstats$imputtraj[imputstats$icnarcsepsisbinary == 1])
describe(imputstats$imputtraj[imputstats$icnarcsepsisbinary == 0])

wilcoxtestsepsisbinaryfs1 <- wilcox.test(full_score1 ~ icnarcsepsisbinary, data = imputstats)
wilcoxtestsepsisbinaryfs1
describe(imputstats$full_score1[imputstats$icnarcsepsisbinary == 1])
describe(imputstats$full_score1[imputstats$icnarcsepsisbinary == 0])

wilcoxtestsepsisbinaryfs2 <- wilcox.test(full_score2 ~ icnarcsepsisbinary, data = imputstats)
wilcoxtestsepsisbinaryfs2
describe(imputstats$full_score2[imputstats$icnarcsepsisbinary == 1])
describe(imputstats$full_score2[imputstats$icnarcsepsisbinary == 0])

wilcoxtestsepsisbinarytime2icu <- wilcox.test(time2icu ~ icnarcsepsisbinary, data = imputstats)
wilcoxtestsepsisbinarytime2icu
describe(imputstats$time2icu[imputstats$icnarcsepsisbinary == 1])
describe(imputstats$time2icu[imputstats$icnarcsepsisbinary == 0])


chisq.test(imputstats$dead28, imputstats$icnarcsepsisbinary,
           correct = FALSE)
describe(imputstats$dead28[imputstats$icnarcsepsisbinary == 1])
describe(imputstats$dead28[imputstats$icnarcsepsisbinary == 0])

imputstats.sepsisonly <- imputstats %>% filter(admdiag != 0) 
krusktestsepsisonly <- kruskal.test(imputtraj ~ admdiag, data = imputstats.sepsisonly)
krusktestsepsisonly
pairwise.wilcox.test(imputstats.sepsisonly$imputtraj, imputstats.sepsisonly$admdiag)

DT <- dunn.test(imputstats$imputtraj, imputstats$admdiag,
               method = "bh")

describe(imputstats$otherdiag)

dunn.test(imputstats$imputtraj, imputstats$otherdiag,
          method = "bh")

pairwise.wilcox.test(imputstats$imputtraj, imputstats$admdiag)

##====
## Looking at other non sepsis diagnoses - different variable, otherdiag not admdiag
##====
##Non sepsis diagnoses
imputstats$otherdiag <- NA
imputstats$otherdiag[imputstats$raicu1 == '2.3.9.28.1'] <- 6
imputstats$otherdiag[imputstats$raicu1 == '2.7.1.13.1'] <- 7
imputstats$otherdiag[imputstats$raicu1 == '2.7.1.13.2'] <- 7
imputstats$otherdiag[imputstats$raicu1 == '2.4.2.33.1'] <- 8
imputstats$otherdiag[imputstats$raicu1 == '2.1.4.31.9'] <- 9


## Non sepsis diagnoses - changed to $otherdiag to keep separate from sepsis
imputstats$diag[imputstats$raicu1 == '2.3.9.28.1'] <- "Acute pancreatitis"
imputstats$diag[imputstats$raicu1 == '2.7.1.13.1'] <- "Acute renal failure (haemodynamic related)"
imputstats$diag[imputstats$raicu1 == '2.7.1.13.2'] <- "Acute renal failure (toxin/drug related)"
imputstats$diag[imputstats$raicu1 == '2.4.2.33.1'] <- "Status epilepticus"
imputstats$diag[imputstats$raicu1 == '2.1.4.31.9'] <- "Cardiogenic pulmonary oedema"

medfsdiag6 = quantile(imputstats$full_score1[imputstats$otherdiag == 6], 0.5, na.rm = TRUE)
medfsdiag7 = quantile(imputstats$full_score1[imputstats$otherdiag == 7], 0.5, na.rm = TRUE)
medfsdiag8 = quantile(imputstats$full_score1[imputstats$otherdiag == 8], 0.5, na.rm = TRUE)
medfsdiag9 = quantile(imputstats$full_score1[imputstats$otherdiag == 9], 0.5, na.rm = TRUE)

medtrajdiag6 = quantile(imputstats$imputtraj[imputstats$otherdiag == 6], 0.5, na.rm = TRUE)
medtrajdiag7 = quantile(imputstats$imputtraj[imputstats$otherdiag == 7], 0.5, na.rm = TRUE)
medtrajdiag8 = quantile(imputstats$imputtraj[imputstats$otherdiag == 8], 0.5, na.rm = TRUE)
medtrajdiag9 = quantile(imputstats$imputtraj[imputstats$otherdiag == 9], 0.5, na.rm = TRUE)


diag_names2 <- c('0' = "Other diagnosis",
                '1' = "Pneumonia (other)",
                '2' = "Septic Shock",
                '3' = "AKI (infection related)",
                '4' = "Septicaemia",
                '5' = "Urosepsis",
                '6' = "Acute pancreatitis",
                '7' = "Acute renal failure (haemodynamic related)",
                '8' = "Acute renal failure (toxin/drug related)",
                '9' = "Status epilepticus",
                '10' = "Cardiogenic pulmonary oedema")


describe(imputstats$otherdiag)

kruskal.test(imputtraj ~ otherdiag, data = imputstats)

imputtraj.noepilepsy <- imputstats %>% filter(otherdiag!= 9)
kruskal.test(imputtraj ~ otherdiag, data = imputtraj.noepilepsy)

dunn.test(imputstats$imputtraj, imputstats$otherdiag,
          method = "bh")


ggplot(data = filter(imputstats, time2icu < quantile(time2icu, 0.9, na.rm = TRUE))) + 
  geom_segment(data = filter(imputstats),
               aes(x = 0, y = medfsdiag6, xend = 20, yend = (medfsdiag6 + medtrajdiag6 * 20)), colour = "red", size = 0.01) +
  geom_segment(data = filter(imputstats),
               aes(x = 0, y = medfsdiag7, xend = 20, yend = (medfsdiag7 + medtrajdiag7 * 20)), colour = "blue", size = 0.01) +
  geom_segment(data = filter(imputstats),
               aes(x = 0, y = medfsdiag8, xend = 20, yend = (medfsdiag8 + medtrajdiag8 * 20)), colour = "forestgreen", size = 0.01) +
  geom_segment(data = filter(imputstats),
               aes(x = 0, y = medfsdiag9, xend = 20, yend = (medfsdiag9 + medtrajdiag9 * 20)), colour = "purple", size = 0.01) +
  geom_segment(data = imputstats,
               aes(x = 0, y = medfsdiagall, xend = 20, yend = (medfsdiagall + medtrajdiagall * 20)), colour = "black", size = 0.01) +
  labs(x = "Hours since ward assessment", y = "ICNARC APS severity score") +
  theme(text = element_text(size=20))


ggplot(data = filter(imputstats, time2icu < quantile(time2icu, 0.9, na.rm = TRUE))) + 
  geom_segment(data = filter(imputstats, admdiag == 0),
               aes(x = 0, y = medfsdiag0, xend = 10, yend = (medfsdiag0 + medtrajdiag0 * 10)), colour = "blue", size = 0.01) +
  geom_segment(data = imputstats,
               aes(x = 0, y = medfsdiagall, xend = 10, yend = (medfsdiagall + medtrajdiagall * 10)), colour = "aquamarine4", size = 0.01) +
  geom_segment(data = imputstats,
               aes(x = 0, y = medfsdiagsepsis, xend = 10, yend = (medfsdiagsepsis + medtrajdiagnotsepsis * 10)), colour = "red", size = 0.01) +
  labs(x = "Hours since ward assessment", y = "ICNARC APS everity score") +
  scale_x_discrete(limits = c (0, 5, 10)) +
  scale_y_discrete(limits = c(15, 17.5, 20, 22.5, 25)) + 
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12))

## 
## Descriptive statistics for sepsis ICNARC codes - does this duplicate code above? Check 
## 

describe(imputstats$admdiag)

describe(imputstats$full_score1[imputstats$admdiag == 1])
describe(imputstats$full_score1[imputstats$admdiag == 2])
describe(imputstats$full_score1[imputstats$admdiag == 3])
describe(imputstats$full_score1[imputstats$admdiag == 4])
describe(imputstats$full_score1[imputstats$admdiag == 5])
describe(imputstats$full_score1[imputstats$admdiag != 1 & imputstats$admdiag != 2 & imputstats$admdiag != 3 & imputstats$admdiag != 4 & imputstats$admdiag != 5])

describe(imputstats$imputtraj[imputstats$admdiag == 1])
describe(imputstats$imputtraj[imputstats$admdiag == 2])
describe(imputstats$imputtraj[imputstats$admdiag == 3])
describe(imputstats$imputtraj[imputstats$admdiag == 4])
describe(imputstats$imputtraj[imputstats$admdiag == 5])
describe(imputstats$imputtraj[imputstats$admdiag != 1 & imputstats$admdiag != 2 & imputstats$admdiag != 3 & imputstats$admdiag != 4 & imputstats$admdiag != 5])


describe(imputstats$dead28[imputstats$admdiag == 1])
describe(imputstats$dead28[imputstats$admdiag == 2])
describe(imputstats$dead28[imputstats$admdiag == 3])
describe(imputstats$dead28[imputstats$admdiag == 4])
describe(imputstats$dead28[imputstats$admdiag == 5])
describe(imputstats$dead28[imputstats$admdiag != 1 & imputstats$admdiag != 2 & imputstats$admdiag != 3 & imputstats$admdiag != 4 & imputstats$admdiag != 5])


describe(imputstats$time2icu[imputstats$admdiag == 1])
describe(imputstats$time2icu[imputstats$admdiag == 2])
describe(imputstats$time2icu[imputstats$admdiag == 3])
describe(imputstats$time2icu[imputstats$admdiag == 4])
describe(imputstats$time2icu[imputstats$admdiag == 5])
describe(imputstats$time2icu[imputstats$admdiag != 1 & imputstats$admdiag != 2 & imputstats$admdiag != 3 & imputstats$admdiag != 4 & imputstats$admdiag != 5])



##
## How much correlation between Likert scale and ICNARC sepsis code?
##

describe(imputstats$admdiag)
describe(imputstats$sepsis)

with(imputstats, table(admdiag, sepsis))

length(which(imputstats$admdiag != 0 & imputstats$sepsis == 3))
length(which(imputstats$admdiag != 0 & imputstats$sepsis == 4))

##
## Look at significance of differences between sepsis ICNARC diagnoses groups - this duplicates stuff above, go through and check 

describe(imputstats$admdiag)

krusktestICNARCsepsistraj <- kruskal.test(imputtraj ~ admdiag, data = imputstats)
krusktestICNARCsepsistraj
pairwise.wilcox.test(imputstats$imputtraj, imputstats$admdiag)

krusktestICNARCsepsiswardscore <- kruskal.test(full_score1 ~ admdiag, data = imputstats)
krusktestICNARCsepsiswardscore
pairwise.wilcox.test(imputstats$full_score1, imputstats$admdiag)

krusktestsepsistime2icu <- kruskal.test(time2icu ~ sepsis, data = imputstats)
krusktestsepsistime2icu
pairwise.wilcox.test(imputstats$time2icu, imputstats$sepsis)

krusktestsepsisdead28 <- kruskal.test(dead28 ~ sepsis, data = imputstats)
krusktestsepsisdead28
pairwise.wilcox.test(imputstats$dead28, imputstats$sepsis)

  
##
## Table of quartiles - not using
##

diagnosistabletop12 <- diagnosistable %>% filter(freq >80)

diagnosistabletop12$diag[diagnosistabletop12$raicu1 == '2.3.9.28.1'] <- "Acute pancreatitis"
diagnosistabletop12$diag[diagnosistabletop12$raicu1 == '2.7.1.13.1'] <- "Acute renal failure (haemodynamic related)"
diagnosistabletop12$diag[diagnosistabletop12$raicu1 == '2.7.1.13.2'] <- "Acute renal failure (toxin/drug related)"
diagnosistabletop12$diag[diagnosistabletop12$raicu1 == '2.4.2.33.1'] <- "Status epilepticus"
diagnosistabletop12$diag[diagnosistabletop12$raicu1 == '2.1.4.31.9'] <- "Cardiogenic pulmonary oedema"

fun1 <- function(x) {quantile(x, 0.25, na.rm = TRUE)}
fun2 <- function(x) {quantile(x, 0.5, na.rm = TRUE)}
fun3 <- function(x) {quantile(x, 0.75, na.rm = TRUE)}

a <- aggregate(imputstats$imputtraj, list(imputstats$raicu1), fun1)
b <- aggregate(imputstats$imputtraj, list(imputstats$raicu1), fun2)
c <- aggregate(imputstats$imputtraj, list(imputstats$raicu1), fun3)
names(a)[1] <- "raicu1"
names(b)[1] <- "raicu1"
names(c)[1] <- "raicu1"
diagnosistabletop10 <- merge(x = diagnosistabletop10, y = a, by = "raicu1", all.x = TRUE)
colnames(diagnosistabletop10)[colnames(diagnosistabletop10)=="x"] <- "q1"
diagnosistabletop10 <- merge(x = diagnosistabletop10, y = b, by = "raicu1", all.x = TRUE)
colnames(diagnosistabletop10)[colnames(diagnosistabletop10)=="x"] <- "q2"
diagnosistabletop10 <- merge(x = diagnosistabletop10, y = c, by = "raicu1", all.x = TRUE)
colnames(diagnosistabletop10)[colnames(diagnosistabletop10)=="x"] <- "q3"

## =====
## Splitting data into test and train
## Note on sepsis labels: sepsis = Likert scale, icnarcsepsisbinary = ICNARC sepsis diagnosis, raicu1 = reason for ICU admission
## =====

## Put in vitals trajectory and model-based trajectory variables at this point (based on analysis below)
## traj 1: individually predictive: bps, rr, ph
## traj 2: predictive if all included in model: bps, rr, creat, UOP 
## traj 3: include all 'vitals': hr, bps, temp, rr, pf, gcs, urin

imputstats$traj1 <- imputstats$bpstrajwt + imputstats$rrtrajwt + imputstats$pftrajwt + imputstats$urintrajwt
imputstats$traj2 <- imputstats$bpstrajwt + imputstats$rrtrajwt + imputstats$crtrajwt + imputstats$urintrajwt
imputstats$traj3 <- imputstats$hrtrajwt + imputstats$bpstrajwt + imputstats$temptrajwt + imputstats$rrtrajwt + imputstats$pftrajwt + imputstats$gcstrajwt + imputstats$urintrajwt

## Remove NAs as must be removed for model to work 

length(which(!is.na(imputstats$sepsis) & !is.na(imputstats$imputtraj) & !is.na(imputstats$full_score1) &!is.na(imputstats$full_score2) & !is.na(imputstats$age) & !is.na(imputstats$lac2) & !is.na(imputstats$icnarcsepsisbinary) & !is.na(imputstats$raicu1)))
imputstats.narm <- filter(imputstats, !is.na(imputstats$sepsis) & !is.na(imputstats$imputtraj) & !is.na(imputstats$full_score1) & !is.na(imputstats$full_score2) & !is.na(imputstats$age) & !is.na(imputstats$lac2) & !is.na(imputstats$icnarcsepsisbinary) & !is.na(imputstats$raicu1))

## Split to test and train
smp_siz <- floor(0.8*nrow(imputstats))
set.seed(104)
train_set <- sample(seq_len(nrow(imputstats.narm)), size = smp_siz)
traindata <- imputstats.narm[train_set,]
testdata <- imputstats.narm[-train_set,]




## ======
## Models wtih different combinations of variables for predicting outcome dead at 28 days
## ======

## Likert sepsis category, trajectory, ward admission score, age, lactate
multvar1 <- glm(dead28 ~ sepsis + imputtraj + full_score1 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvar1)

## Likert sepsis category, trajectory, ward admission score, icu admission score, age, lactate
multvar2 <- glm(dead28 ~ sepsis + imputtraj + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvar2)

## ICNARC sepsis category, trajectory, ward admission score, age, lactate
multvar3 <- glm(dead28 ~ icnarcsepsisbinary + imputtraj + full_score1 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvar3)

## ICNARC sepsis category, trajectory, ward admission score, icu admission score, age, lactate
multvar4 <- glm(dead28 ~ icnarcsepsisbinary + imputtraj + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvar4)

## ICNARC sepsis category, trajectory, ward admission score, icu admission score, age, lactate + interaction term between sepsis and imputed trajectory
multvar5 <- glm(dead28 ~ icnarcsepsisbinary + imputtraj + full_score1 + full_score2 + age + lac2 + icnarcsepsisbinary*imputtraj, data = traindata, family = binomial(link = "logit"))
summary(multvar5)    

## ====
## Look at lacatate absolute values vs lactate weight
## ====

multvar6 <- glm(dead28 ~ icnarcsepsisbinary + imputtraj + full_score1 + full_score2 + age + lac1 + lac2 + lactraj, data = traindata, family = binomial(link = "logit"))
summary(multvar6)

## =====
## Model with no measure of trajectory 
## ====

multvar7 <- glm(dead28 ~ icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvar7)

predmultvar7 <- predict(multvar7, testdata, type="response")
testdata$pred7 = predmultvar7
g7 <- roc(dead28 ~ pred7, data = testdata)
plot(g7)
auc(g7)

## ====
## OR and CI from models above
## ====

## Multvar1 OR and 95% CI
exp(cbind(OR = coef(multvar1), confint(multvar1)))

## based on multvar1
## logit(dead28) = -4.858 + 0.073*sepsis + 0.081*imputtraj + 0.081*wardscore + 0.028*age + 0.098*lactate)
## calculate ORs: e^coefficient
orsepsis = exp(0.073)
upper95orsepsis = exp(0.073 + 0.035)
lower95orsepsis = exp(0.073 - 0.035)
orimputtraj = exp(0.081)
upper95orimputtraj = exp(0.082 + 0.006)
lower95orimputtraj = exp(0.082 - 0.006)
orwardscore = exp(0.081)
upper95orwardscore = exp(0.081 + 0.005)
lower95orwardscore = exp(0.081 - 0.005)
orage = exp(0.028)
upper95orage = exp(0.028 + 0.002)
lower95orage = exp(0.028 - 0.002)
orlactate = exp(0.098)
upper95orlactate = exp(0.098 + 0.012)
lower95orlactate = exp(0.098 - 0.012)

orimputtraj
upper95orimputtraj
lower95orimputtraj 

## based on multvar4    
upper95orimputtraj4 = exp(0.082 + 0.006)
lower95orimputtraj4 = exp(0.082 - 0.006)
orwardscore4 = exp(0.082)
upper95orimputtraj4
lower95orimputtraj4
orwardscore4


## ====
##Try prediction?
## ====

predmultvar4 <- predict(multvar4, testdata, type="response")
testdata$pred = predmultvar4
g <- roc(dead28 ~ pred, data = testdata)
plot(g)
auc(g)


## =====
## multvar models looking at whether trajectories of individual variables are signficantly predictive of outcome
##'hr', 'bps', 'temp', 'rr','pf','ph','urea', 'cr', 'na', 'urin', 'wcc', 'gcs'
## individual name = hrtrajabsolut
## first model includes fs1, second model (.fs2) does not include fs1
## =====

## 1. hr
multvarhr <- glm(dead28 ~ hrtrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarhr)
## not significant
multvarhr.fs2 <- glm(dead28 ~ hrtrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarhr.fs2)
## not significant

## 2. bps
multvarbps <- glm(dead28 ~ bpstrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarbps)
## significant
multvarbps.fs2 <- glm(dead28 ~ bpstrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarbps.fs2)
## significant

## 3. temp
multvartemp <- glm(dead28 ~ temptrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvartemp)
## not significant
multvartemp.fs2 <- glm(dead28 ~ temptrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvartemp.fs2)
## not significant

## 4. rr
multvarrr <- glm(dead28 ~ rrtrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarrr)
## significant
multvarrr.fs2 <- glm(dead28 ~ rrtrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarrr.fs2)
## significant

## 5. pf
multvarpf <- glm(dead28 ~ pftrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarpf)
## significant
multvarpf.fs2 <- glm(dead28 ~ pftrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarpf.fs2)
## significant

## 6. ph
multvarph <- glm(dead28 ~ phtrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarph)
## not significant
multvarph.fs2 <- glm(dead28 ~ phtrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarph.fs2)
## not significant

## 7. urea
multvarurea <- glm(dead28 ~ ureatrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarurea)
## not significant
multvarurea.fs2 <- glm(dead28 ~ ureatrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarurea.fs2)
## not significant

## 8. creat
multvarcr <- glm(dead28 ~ crtrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarcr)
## not significant
multvarcr.fs2 <- glm(dead28 ~ crtrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarcr.fs2)
## not significant

## 9. Na
multvarna <- glm(dead28 ~ natrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarna)
## not significant
multvarna.fs2 <- glm(dead28 ~ natrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarna.fs2)
## not significant

## 10. urine output
multvarurin <- glm(dead28 ~ urintrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarurin)
## significant
multvarurin.fs2 <- glm(dead28 ~ urintrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarurin.fs2)
## significant

## 11. WCC
multvarwcc <- glm(dead28 ~ wcctrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarwcc)
## not significant
multvarwcc.fs2 <- glm(dead28 ~ wcctrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarwcc.fs2)
## not significant

## 12. GCS
multvargcs <- glm(dead28 ~ gcstrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvargcs)
## not significant
multvargcs.fs2 <- glm(dead28 ~ gcstrajwt + icnarcsepsisbinary + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvargcs.fs2)
## not significant

## Try model with all weights individually, does this give the same?

multvarindivid <- glm(dead28 ~ hrtrajwt + bpstrajwt + temptrajwt + rrtrajwt + pftrajwt + phtrajwt + ureatrajwt + crtrajwt + natrajwt + urintrajwt + wcctrajwt + gcstrajwt + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvarindivid)
## significant: bps, rr, creat, UOP
## Same variables are significant if fs1 not included in model 

## =====
## looking at whether a vitals based trajectory is predictive of outcome
## traj 1: individually predictive: bps, rr, pf, UOP
## traj 2: predictive if all included in model: bps, rr, creat, UOP 
## traj 3: include all 'vitals': hr, bps, temp, rr, pf, gcs, urin
## =====

## traj1 model: trajectory based on inidividually significant variables
multvartraj1 <- glm(dead28 ~ traj1 + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvartraj1)

predmultvartraj1 <- predict(multvartraj1, testdata, type="response")
testdata$predtraj1 = predmultvartraj1
gtraj1 <- roc(dead28 ~ predtraj1, data = testdata)
plot(gtraj1)
auc(gtraj1)

exp(cbind(OR = coef(multvartraj1), confint(multvartraj1)))

## traj2 model: trajectory based on variables significant when all included individually 
multvartraj2 <- glm(dead28 ~ traj2 + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvartraj2)

predmultvartraj2 <- predict(multvartraj2, testdata, type="response")
testdata$predtraj2 = predmultvartraj2
gtraj2 <- roc(dead28 ~ predtraj2, data = testdata)
plot(gtraj2)
auc(gtraj2)

exp(cbind(OR = coef(multvartraj2), confint(multvartraj2)))

## traj3 model: trajectory based on bedside observations
multvartraj3 <- glm(dead28 ~ traj3 + icnarcsepsisbinary + full_score1 + full_score2 + age + lac2, data = traindata, family = binomial(link = "logit"))
summary(multvartraj3)

predmultvartraj3 <- predict(multvartraj3, testdata, type="response")
testdata$predtraj3 = predmultvartraj3
gtraj3 <- roc(dead28 ~ predtraj3, data = testdata)
plot(gtraj3)
auc(gtraj3)

exp(cbind(OR = coef(multvartraj3), confint(multvartraj3)))

## traj4 model: include individual trajectories of significant variables
multvartraj4 <- glm(dead28 ~ icnarcsepsisbinary + full_score1 + full_score2 + age + lac2 + bpstrajwt + rrtrajwt + crtrajwt + urintrajwt, data = traindata, family = binomial(link = "logit"))
summary(multvartraj4)

predmultvartraj4 <- predict(multvartraj4, testdata, type="response")
testdata$predtraj4 = predmultvartraj4
gtraj4 <- roc(dead28 ~ predtraj4, data = testdata)
plot(gtraj4)
auc(gtraj4)

exp(cbind(OR = coef(multvartraj4), confint(multvartraj4)))


## =======
## To plot logits; will plot glm for trajectory only, for each sepsis group
## =======

model_trajsepsis1 <- glm(dead28 ~ imputtraj, data = filter(imputstats.narm, sepsis == 1), family = binomial(link = "logit"))
summary(model_trajsepsis1)

model_trajsepsis2 <- glm(dead28 ~ imputtraj, data = filter(imputstats.narm, sepsis == 2), family = binomial(link = "logit"))
summary(model_trajsepsis2)

model_trajsepsis3 <- glm(dead28 ~ imputtraj, data = filter(imputstats.narm, sepsis == 3), family = binomial(link = "logit"))
summary(model_trajsepsis3)

model_trajsepsis4 <- glm(dead28 ~ imputtraj, data = filter(imputstats.narm, sepsis == 4), family = binomial(link = "logit"))
summary(model_trajsepsis4)

describe(imputstats$imputtraj)
ximputtraj <- seq(-100, 50, 0.1)
ydead28sepsis1 <- predict(model_trajsepsis1, list(imputtraj = ximputtraj), type = "response")
ydead28sepsis2 <- predict(model_trajsepsis2, list(imputtraj = ximputtraj), type = "response")
ydead28sepsis3 <- predict(model_trajsepsis3, list(imputtraj = ximputtraj), type = "response")
ydead28sepsis4 <- predict(model_trajsepsis4, list(imputtraj = ximputtraj), type = "response")

modellines <- data.frame(ximputtraj, ydead28sepsis1, ydead28sepsis2, ydead28sepsis3, ydead28sepsis4)

ggplot(data = imputstats) +
  geom_point(aes(x = imputtraj, y = dead28)) +
  geom_line(data = modellines,
            aes(x = ximputtraj, y = ydead28sepsis1), col = "red") +
  geom_line(data = modellines,
            aes(x = ximputtraj, y = ydead28sepsis2), col = "blue") +
  geom_line(data = modellines,
            aes(x = ximputtraj, y = ydead28sepsis3), col = "dark green") +
  geom_line(data = modellines,
            aes(x = ximputtraj, y = ydead28sepsis4), col = "purple") +
  labs(x = "", y = "") +
  theme(text = element_text(size=22))

## ======
## To plot data for binary sepsis based on diagnosis code
## ======

model_trajsepsisy <- glm(dead28 ~ imputtraj, data = filter(imputstats.narm, sepsisbinary == 1), family = binomial(link = "logit"))
summary(model_trajsepsisy)

model_trajsepsisn <- glm(dead28 ~ imputtraj, data = filter(imputstats.narm, sepsisbinary == 0), family = binomial(link = "logit"))
summary(model_trajsepsisn)

describe(imputstats$imputtraj)
ximputtraj <- seq(-100, 50, 0.1)
ydead28sepsisy <- predict(model_trajsepsisy, list(imputtraj = ximputtraj), type = "response")
ydead28sepsisn <- predict(model_trajsepsisn, list(imputtraj = ximputtraj), type = "response")


modellines <- data.frame(ximputtraj, ydead28sepsisy, ydead28sepsisn)

ggplot(data = imputstats) +
  geom_point(aes(x = imputtraj, y = dead28)) +
  geom_line(data = modellines,
            aes(x = ximputtraj, y = ydead28sepsisy), col = "red") +
  geom_line(data = modellines,
            aes(x = ximputtraj, y = ydead28sepsisn), col = "blue") +
  labs(x = "", y = "") +
  theme(text = element_text(size=22))

## =======
## Plot of trajectories based on Likert sepsis likelihood
## Need to graph time and severity score
## =======
describe(imputstats$icu_admit)
describe(imputstats$v_timestamp)
imputstats$sepsis <- as.factor(imputstats$sepsis)

quantile(imputstats$time2icu, 0.5, na.rm = TRUE)
medtraj = quantile(imputstats$imputtraj, 0.5, na.rm = TRUE)
medtrajsepsis1 = quantile(imputstats$imputtraj[imputstats$sepsis == 1], 0.5, na.rm = TRUE)
medtrajsepsis2 = quantile(imputstats$imputtraj[imputstats$sepsis == 2], 0.5, na.rm = TRUE)
medtrajsepsis3 = quantile(imputstats$imputtraj[imputstats$sepsis == 3], 0.5, na.rm = TRUE)
medtrajsepsis4 = quantile(imputstats$imputtraj[imputstats$sepsis == 4], 0.5, na.rm = TRUE)

sepsis_names <- c('1' = "Sepsis Very Unlikely",
                  '2' = "Sepsis Unlikely",
                  '3' = "Sepsis Likely",
                  '4' = "Sepsis Very Likely ")

## actual trajectories; actual scores at time points 0 for ward assessment, time2icu for 
## Note, this has top 10% of time 2 icu clipped out
ggplot(data = filter(imputstats, !is.na(sepsis) & time2icu < quantile(time2icu, 0.9, na.rm = TRUE))) +
  geom_point(aes(x = 0, y = full_score1), size = 0.1) +
  geom_point(aes(x = time2icu, y = full_score2), size = 0.1) + 
  geom_segment(aes(x = 0, y = full_score1, xend = time2icu, yend = full_score2), colour = "light blue") +
  facet_wrap(~sepsis, nrow = 1, labeller = as_labeller(sepsis_names)) +
  geom_segment(aes(x = 0, y = quantile(full_score1, 0.5, na.rm = TRUE), xend = 20, yend = quantile(full_score1, 0.5, na.rm = TRUE) + (medtraj * 20)), size = 0.1) +
  geom_segment(data = filter(imputstats, sepsis == 1),
               aes(x = 0, y = quantile(full_score1[sepsis == 1], 0.5, na.rm = TRUE), xend = 20, yend = quantile(full_score1[sepsis == 1], 0.5, na.rm = TRUE) + (medtrajsepsis1 * 20)), colour = "blue", size = 0.1) +
  geom_segment(data = filter(imputstats, sepsis == 2),
               aes(x = 0, y = quantile(full_score1[sepsis == 2], 0.5, na.rm = TRUE), xend = 20, yend = quantile(full_score1[sepsis == 2], 0.5, na.rm = TRUE) + (medtrajsepsis2 * 20)), colour = "blue", size = 0.1) + 
  geom_segment(data = filter(imputstats, sepsis == 3),
               aes(x = 0, y = quantile(full_score1[sepsis == 3], 0.5, na.rm = TRUE), xend = 20, yend = quantile(full_score1[sepsis == 3], 0.5, na.rm = TRUE) + (medtrajsepsis3 * 20)), colour = "blue", size = 0.1) + 
  geom_segment(data = filter(imputstats, sepsis == 4),
               aes(x = 0, y = quantile(full_score1[sepsis == 4], 0.5, na.rm = TRUE), xend = 20, yend = quantile(full_score1[sepsis == 4], 0.5, na.rm = TRUE) + (medtrajsepsis4 * 20)), colour = "blue", size = 0.1) + 
  labs(x = "", y = "") +
  theme(text = element_text(size=18))

## ====
## Graph trajectories with mean only
## ====
ggplot(data = filter(imputstats, !is.na(sepsis) & time2icu < quantile(time2icu, 0.9, na.rm = TRUE))) +
  geom_point(aes(x = 0, y = full_score1), size = 0.1) +
  geom_point(aes(x = time2icu, y = full_score2), size = 0.1) + 
  geom_segment(aes(x = 0, y = full_score1, xend = time2icu, yend = full_score2), colour = "light blue") +
  geom_segment(aes(x = 0, y = quantile(full_score1, 0.5, na.rm = TRUE), xend = 20, yend = quantile(full_score1, 0.5, na.rm = TRUE) + (medtraj * 20)), size = 0.1)

## sepsis very unlikely = dark green
## sepsis unlikely = blue
## sepsis likely = purple
## sepsis very likely = blue green

describe(imputstats$time2icu)

## extrapolated trajectory over 3 hours - not as good a graph, don't use  
ggplot(data = filter(imputstats, !is.na(sepsis))) +
  geom_point(aes(x = 0, y = full_score1)) +
  geom_point(aes(x = 3, y = (full_score1 + (imputtraj*3)))) + 
  geom_segment(aes(x = 0, y = full_score1, xend = 3, yend = (full_score1 + (imputtraj*3)), colour = sepsis)) +
  facet_wrap(~sepsis, nrow = 2) +
  geom_segment(aes(x = 0, y = quantile(full_score1, 0.5, na.rm = TRUE), xend =3, yend = quantile(full_score1, 0.5, na.rm = TRUE) + (medtraj * 3))) +
  geom_segment(data = filter(imputstats, sepsis == 1),
               aes(x = 0, y = quantile(full_score1[sepsis == 1], 0.5, na.rm = TRUE), xend = 3, yend = quantile(full_score1[sepsis == 1], 0.5, na.rm = TRUE) + (medtrajsepsis1 * 3)), colour = "blue") +
  geom_segment(data = filter(imputstats, sepsis == 2),
               aes(x = 0, y = quantile(full_score1[sepsis == 2], 0.5, na.rm = TRUE), xend = 3, yend = quantile(full_score1[sepsis == 2], 0.5, na.rm = TRUE) + (medtrajsepsis2 * 3)), colour = "blue") + 
  geom_segment(data = filter(imputstats, sepsis == 3),
               aes(x = 0, y = quantile(full_score1[sepsis == 3], 0.5, na.rm = TRUE), xend = 3, yend = quantile(full_score1[sepsis == 3], 0.5, na.rm = TRUE) + (medtrajsepsis3 * 3)), colour = "blue") + 
  geom_segment(data = filter(imputstats, sepsis == 4),
               aes(x = 0, y = quantile(full_score1[sepsis == 4], 0.5, na.rm = TRUE), xend = 3, yend = quantile(full_score1[sepsis == 4], 0.5, na.rm = TRUE) + (medtrajsepsis4 * 3)), colour = "blue")




