# This script adds missingness to the cleaned ANES data.

rm(list=ls())
setwd("C:/Users/Steve/Documents/GitHub/MIML/ANES")

library(mice)
library(MASS)

seed <- 24519

### ANES Data -> Add missingness MCAR

load('cleanedANES.RData')

# For "natural" missingngess, convert NA's to -999 (to distinguish from added missingness)
anes[is.na(anes)] <- -999

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

# Visualize missingness of both "natural" and removed data
full.na.mcar <- anes.mcar$amp
full.na.mcar[full.na.mcar == -999] <- NA
md.pattern(full.na.mcar) # This looks wonky b/c there is no clear "pattern"

# Compare proportion of missing data
anes.mcar$prop

# Determine number of incomplete observations across natural versus added df's
sum(!complete.cases(anes.mcar$amp)) #1114
sum(!complete.cases(full.na)) #2450

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
full.na.mar <- anes.mar$amp
full.na.mar[full.na.mar == -999] <- NA
md.pattern(full.na.mar) # This looks wonky b/c there is no clear "pattern"

# Compare proportion of missing data
anes.mar$prop

# Determine number of incomplete observations across natural versus added df's
sum(!complete.cases(anes.mar$amp)) #1161
sum(!complete.cases(full.na.mar)) #2612

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
full.na.mnar <- anes.mnar$amp
full.na.mnar[full.na.mnar == -999] <- NA
md.pattern(full.na.mnar) # This looks wonky b/c there is no clear "pattern"

# Compare proportion of missing data
anes.mnar$prop

# Determine number of incomplete observations across natural versus added df's
sum(!complete.cases(anes.mnar$amp)) #1147
sum(!complete.cases(full.na.mnar)) #2511

amp.mnar <- anes.mnar$amp

# Save six df's
save(full.na.mcar, amp.mcar, full.na.mar, amp.mar, amp.mnar, full.na.mnar, file = 'anesMissing.RData')
