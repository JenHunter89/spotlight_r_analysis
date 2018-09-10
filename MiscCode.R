## To delete a column 
  ## rawdata <- select(rawdata, -rawdata$included_sites)
  ## rawdata$included_sites <- NULL
  ## rawdata <- subset( rawdata, select = -included_sites)
  ## Data <- subset( Data, select = -c(d, b ) )


## To replace a value in a column based on a condition
  ## rawdata$col1[condition e.g. rawdata$col2 == 3] <- "TRUE"

## Tab function in stata displays a frequency summary table
## count(dataset, 'column') gives same thing in r

## Relative paths. ../ go up directory
## html widgets


##path <- file.path("raw_data","wrmc.csv")
##rawdata <- read.csv(path, stringsAsFactors = FALSE)

describe(vardata$sepsis_severity)
vardata$sepsis <- as.factor(vardata$sepsis)
vardata$sepsis_severity <- as.factor(vardata$sepsis_severity)
vardata$dx_pneum <- as.factor(vardata$dx_pneum)

ggplot(data = vardata) + 
  geom_boxplot(mapping = aes(x = vardata$dx_pneum, y = vardata$partABG_scoretraj))