### This code simulates a dataset.

rm(list=ls())
setwd("~/GitHub/MIML")

library(evd)

### Uncorrelated data
# Create df of uncorrelated data
set.seed(24519)
N <- 1000
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
uncorr.missing <- (cbind(x1, x2, x3, y, y.mcar))

# As expected, still not correlation between IV's and DV w/ MCAR
cor(x1, y.mcar, use = 'complete.obs')
cor(x2, y.mcar, use = 'complete.obs')
cor(x3, y.mcar, use = 'complete.obs')

## Missing at Random (MAR)
y.mar = matrix(y, ncol=nj, nrow=ni, byrow=TRUE)
for(i in 1:ni){
  for(j in 4:nj){
    dif1 = y.mar[i,j-2]-y.mar[i,j-3]
    dif2 = y.mar[i,j-1]-y.mar[i,j-2]
    if(dif1>0 & dif2>0){  # if weight goes up twice, drops out
      y.mar[i,j:nj] = NA;  break
    }
  }
}
y.mar = as.vector(t(y.mar))
View(cbind(id, week, cond, base, y, y.mar))



## Create df of correlated dependent variable
set.seed(1553)
N <- 1000
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



correlated <- cbind(c(x1, x2, x3, y))


