### This code simulates a dataset.

rm(list=ls())
setwd("~/GitHub/MIML")

library(evd)

### Uncorrelated data
# Create df of uncorrelated data
set.seed(24519)
N <- 10000
x1 <- rnorm(N, 0, 1)
x2 <- runif(N, 0, 1)
x3 <- rgumbel(N, 0, 1)
y <- rpois(N, 3)

# Correlations are all about 0
cor(x1, x2)
cor(x1, x3)
cor(x1, y)
cor(x2, x3)
cor(x2, y)
cor(x3, y)

## Add ``missing-ness'' to data
# DV Missing Completely at Random (MCAR)
prob.m <- 0.05
uncorr.mcar <- runif(N, min=0, max=1)
y.mcar <- ifelse(uncorr.mcar < prob.m, NA, y)
uncorr.missing <- cbind(x1, x2, x3, y, y.mcar)

# As expected, still not correlation between IV's and DV w/ MCAR
cor(x1, y.mcar, use = 'complete.obs')
cor(x2, y.mcar, use = 'complete.obs')
cor(x3, y.mcar, use = 'complete.obs')

## Missing at Random (MAR)
#y.mar = cbind(x1, x2, x3, y)
mod <- x1 - 0.08*x2 + x3 - 0.05 # Response model
rp <- exp(mod) / (exp(mod) + 1) # Suppress values between 0 and 1 via inverse-logit
y.mar <- rbinom(N, 1, rp)
uncorr.mar <- as.data.frame(cbind(x1, x2, x3, y))
uncorr.mar$y.mar <- ifelse(y.mar == 1, y, NA)

cor(uncorr.mar$x1, uncorr.mar$y.mar, use = 'complete.obs')
cor(uncorr.mar$x2, uncorr.mar$y.mar, use = 'complete.obs')
cor(uncorr.mar$x3, uncorr.mar$y.mar, use = 'complete.obs')


## Create df of correlated dependent variable
set.seed(1553)
y <- rnorm(N)
x1 <- y + rnorm(N, 0, 1)
x2 <- y + runif(N, 0, 1)
x3 <- y + rgumbel(N, 0, 1)

# Correlations range from moderately to strongly positive
cor(x1, x2)
cor(x1, x3)
cor(x1, y)
cor(x2, x3)
cor(x2, y)
cor(x3, y)

correlated <- cbind(x1, x2, x3, y)
