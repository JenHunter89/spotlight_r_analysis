install.packages("tidyverse")
install.packages("haven")
install.packages("Hmisc")
install.packages("dplyr")
install.packages("plyr")
install.packages("grid")
install.packages("Gmisc")

library(tidyverse)
library(haven)
library(Hmisc)
library(dplyr)
library(plyr)
library(grid)
library(Gmisc)

## create boxes

grid.newpage()

a <- "Assessed for eligibility"
b <- "Excluded - not eligible"
c <- "Excluded - linkage poor"
d <- "Patients recruited"
e <- "reasons for exclusion"
f <- "available for analysis"

## box 1: assessed for elig
total <- boxGrob(a, x=0.3, y=0.875, box_gp = gpar(fill = "white"), width = 0.4, height = 0.1)
## box 2: excluded - not eligible
exc_elig <- boxGrob(b, x=0.6, y=0.75, box_gp = gpar(fill = "white"), width = 0.4, height = 0.1)
## box 3: excluded - linkage poor
exc_link <- boxGrob(c, x=0.6, y=0.625, box_gp = gpar(fill = "white"), width = 0.4, height = 0.1)
## box 4: patients recruited
recruited <- boxGrob(d, x=0.3, y=0.5, box_gp = gpar(fill = "white"), width = 0.4, height = 0.1)
## box 5: other exclusions
exc_other <- boxGrob(e, x=0.6, y =0.375, box_gp = gpar(fill = "white"), width = 0.4, height = 0.1)
## box 6: analysis
analysis <- boxGrob(f, x=0.3, y=0.25, box_gp = gpar(fill = "white"), width = 0.4, height = 0.1)


connectGrob(total, exc_elig, "L")
connectGrob(total, exc_link, "L")
connectGrob(total, recruited, "v")
connectGrob(recruited, exc_other, "L")
connectGrob(recruited, analysis, "v")

plot(total)
plot(exc_elig)
plot(exc_link)
plot(recruited)
plot(exc_other)
plot(analysis)
