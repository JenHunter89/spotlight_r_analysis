## Summary 

## 1. screen_data
##    Loads working_raw_mris.dta
##    Creates exclusions_data.csv (all data with coding for inc and exc)
##    Creates analysis_data.csv (only inclusions)
##    Applies all inclusions and exclusions, creates data set with all of these

## 2. table_one
##    Loads analysis_data.csv
##    Calculates figures for table 1 for all patients included

## 3. merge_data_labels
##    Loads stata_variables.dta (stata file from Steve with variables calculated) 
##    and loads exclusions_data.csv (from 1, all data with coding for inc and exc)
##    Creates variables_data.csv - used for analysis, supersedes analysis_data.csv

## 4. variable_weighting
##    Loads variables_data.csv (from 3)
##    calculates weighted variables, using ICNARC data and proportion of max/min
##    Creates varcalcs_data.csv - has severity scores

## 5. Trajectories
##    Loads varcalcs_data.csv (from 4)
##    Creates trajectories.csv 

## 6. Trial stats
##    Loads trajectories.csv

## 7. multiple_imputation
##    Loads