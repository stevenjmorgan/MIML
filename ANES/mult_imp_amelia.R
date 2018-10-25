# This code applies multiplie imputation w/ AMELIA II to ANES cleaned data.

rm(list=ls())
setwd("~/GitHub/MIML/ANES")

library(Amelia)

load('cleanedANES.RData')

# Multiple imputation assuming multivariate normal distribution
a.out <- amelia(anes)
a.out

hist(a.out$imputations[[3]]$DJT.FT, col="grey", border="white")
