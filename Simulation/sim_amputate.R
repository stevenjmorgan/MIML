rm(list=ls())

library(mice)
library(MASS)

#y <- rep(1, 10)
#x <- matrix(1, nrow = 10, ncol = 5)
#ry <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
#data <- norm.draw(x, ry, y)

set.seed(24519)
test1 <- MASS::mvrnorm(n=1000, mu = rep(0, 10), Sigma = diag(10))

test1.miss <- ampute(test1, 0.2, mech = 'MCAR')


# http://www.gerkovink.com/Amputation_with_Ampute/Vignette/ampute.html
set.seed(2016)
testdata <- MASS::mvrnorm(n = 10000, mu = c(10, 5, 0), Sigma = matrix(data = c(1.0, 0.2, 0.2, 0.2, 1.0, 0.2, 0.2, 0.2, 1.0), nrow = 3, byrow = T))
testdata <- as.data.frame(testdata)
summary(testdata)

result <- ampute(testdata)
result

md.pattern(result$amp)
