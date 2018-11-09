# This script adds missingness to the cleaned ANES data.

rm(list=ls())
setwd("C:/Users/Steve/Documents/GitHub/MIML/ANES")
#setwd("C:/Users/sum410/Documents/GitHub/MIML/ANES")

library(mice)
library(MASS)
library(stats)

seed <- 24519

### ANES Data -> Add missingness MCAR

load('cleanedANES.RData')

# For "natural" missingngess, convert NA's to -999 (to distinguish from added missingness)
#anes[is.na(anes)] <- -999

# Listwise deletion for ``natural" missingness
dim(anes)
anes <- anes[complete.cases(anes),]
dim(anes)

# Remove values -> MCAR
set.seed(seed)
anes.mcar <- ampute(anes, prop = 0.3, mech = 'MCAR')
anes.mcar

# Validate values were deleted MCAR -> Each row represents # of values removed 
# based on the "pattern", columns represent # of cells removed from each 
# variable, and maroon cells indicated removed values
md.pattern(anes.mcar$amp)
mypatterns <- anes.mcar$patterns
mypatterns

# Visualize missingness of data
anes.mcar$amp

# Validate proportion of missing data
anes.mcar$prop

# Determine number of incomplete observations
sum(!complete.cases(anes.mcar$amp)) #600

amp.mcar <- anes.mcar$amp

### ANES Data -> Add missingness MAR
set.seed(seed)
anes.mar <- ampute(anes, prop = 0.3, mech = 'MAR')
anes.mar

# Examine patterns of missingess
md.pattern(anes.mar$amp)
mypatterns <- anes.mar$patterns
mypatterns

# Visualize missingness of both "natural" and removed data
anes.mar$amp

# Compare proportion of missing data
anes.mar$prop

# Determine number of incomplete observations across natural versus added df's
sum(!complete.cases(anes.mar$amp)) #620

amp.mar <- anes.mar$amp


### ANES Data -> Add missingness MNAR
set.seed(seed)
anes.mnar <- ampute(anes, prop = 0.3, mech = 'MNAR')
anes.mnar

# Examine patterns of missingess
md.pattern(anes.mnar$amp)
mypatterns <- anes.mnar$patterns
mypatterns

# Visualize missingness of both "natural" and removed data
anes.mnar$amp

# Compare proportion of missing data
anes.mnar$prop

# Determine number of incomplete observations across natural versus added df's
sum(!complete.cases(anes.mnar$amp)) #562

amp.mnar <- anes.mnar$amp

# Save six df's
save(amp.mcar, amp.mar, amp.mnar, file = 'anesMissing.RData')
