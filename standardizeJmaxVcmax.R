## Read in standardizeLimitations function
source("nitrogen_pH/Functions/standardizeLimitations.R")

## Load datasheet to put standardized values in
nitrogen_pH_datasheet <- read.csv("Dropbox/GitHub/nitrogen_pH/data_sheets/n_pH_datasheet.csv",
                                  stringsAsFactors = FALSE,
                                  na.strings = c("NA", ""),
                                  strip.white = TRUE)

tGrow <- 16.41 # Average temperature from May 27, 2019 to June 25, 2019

## mapply returns a list of all of the outputs
## Note: 
## Odd numbers are standardized vcmax values
## Even numbers are standardized jmax values
standardizedLimits <- mapply(standardizeLimitations, 
                             Vcmax_est = nitrogen_pH_datasheet$vcmax_raw,
                             Jmax_est = nitrogen_pH_datasheet$jmax_raw,
                             tLeaf = nitrogen_pH_datasheet$mean_Tleaf,
                             tGrow = tGrow)

## Subset all odd numbered columns (standardized Vcmax values)
standardVcmax <- standardizedLimits[c(TRUE, FALSE),]

## Subset all even-numbered cols (standardized Jmax values)
standardJmax <- standardizedLimits[c(FALSE, TRUE),]
