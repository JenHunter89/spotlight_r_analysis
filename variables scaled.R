## Trial: scale outside normality 

##'hr', 'bps', 'temp', 'rr','pf','ph','urea', 'cr', 'na', 'urin', 'wcc', 'gcs'


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


## Leave PF values til last - not all changed ###
##pfmin <- min(vardata$pf1, na.rm = TRUE)
##pfmax <- max(vardata$pf1, na.rm = TRUE)
##vardata$pfnorm <- ifelse(!is.na(vardata$hr1), 
##                         ifelse(vardata$hr1 < 60, (60 - vardata$hr1)/(60 - hrmin), 
##                                ifelse(vardata$hr1 < 90, 0, (vardata$hr1 - 90)/(hrmax - 90))), NA)

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