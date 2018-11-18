rm(list=ls())
setwd("C:/Users/Steve/Dropbox/PSU2018-2019/Fall2018/SODA502/MLMI")
#setwd("C:/Users/sum410/Dropbox/PSU2018-2019/Fall2018/SODA502/MLMI")


library(MASS)
library(ggplot2)
library(e1071)
library(Amelia)

rmse <- function(error){
  sqrt(mean(error^2))
}

load('mcar1_singledf.RData')

# Create a matrix that notes NA values for each cell
miss.mat <- matrix(NA, nrow = nrow(mcar1), ncol = ncol(mcar1))
for (i in 1:nrow(mcar1)){
  for (j in 1:ncol(mcar1)){
    if (is.na(mcar1[i,j])){
      miss.mat[i,j] <- 0
    }
    else{
      miss.mat[i,j] <- 1
    }
  }
}

colnames(miss.mat) <- c("Y", "X1", "X2", "X3" ,"X4")
colnames(mcar1) <- c("Y", "X1", "X2", "X3" ,"X4")
mcar1 <- as.data.frame(mcar1)

# Run MI
mcar1.out <- amelia(mcar1, 1)
mcar1.out <- mcar1.out[[1]]$imp1

# Evaluate
miss.error <- numeric()
for (i in 1:nrow(miss.mat)){
  for (j in 1:ncol(miss.mat)){
    if (miss.mat[i,j] == 0){
      miss.error <-  append(miss.error, data[i,j] - mcar1.out[i,j])
    }
  }
}

rmse(miss.error) # The RMSE for MI (amelia) for a MCAR1 dataset is about 1.118


### MCAR-2
rm(list=ls())
load('mcar2_singledf.RData')

# Create a matrix that notes NA values for each cell
miss.mat <- matrix(NA, nrow = nrow(mcar2), ncol = ncol(mcar2))
for (i in 1:nrow(mcar2)){
  for (j in 1:ncol(mcar2)){
    if (is.na(mcar2[i,j])){
      miss.mat[i,j] <- 0
    }
    else{
      miss.mat[i,j] <- 1
    }
  }
}

rmse <- function(error){
  sqrt(mean(error^2))
}

# Run MI
mcar2.out <- amelia(mcar2, 1)
mcar2.out <- mcar2.out[[1]]$imp1

# Evaluate
miss.error <- numeric()
for (i in 1:nrow(miss.mat)){
  for (j in 1:ncol(miss.mat)){
    if (miss.mat[i,j] == 0){
      miss.error <-  append(miss.error, data[i,j] - mcar2.out[i,j])
    }
  }
}

rmse(miss.error) # The RMSE for MI (amelia) for a MCAR2 dataset is about 1.100

### MAR-1
rm(list=ls())
load('mar1_singledf.RData')

# Create a matrix that notes NA values for each cell
miss.mat <- matrix(NA, nrow = nrow(mar1), ncol = ncol(mar1))
for (i in 1:nrow(mar1)){
  for (j in 1:ncol(mar1)){
    if (is.na(mar1[i,j])){
      miss.mat[i,j] <- 0
    }
    else{
      miss.mat[i,j] <- 1
    }
  }
}

rmse <- function(error){
  sqrt(mean(error^2))
}

# Run MI
mar1.out <- amelia(mar1, 1)
mar1.out <- mar1.out[[1]]$imp1

# Evaluate
miss.error <- numeric()
for (i in 1:nrow(miss.mat)){
  for (j in 1:ncol(miss.mat)){
    if (miss.mat[i,j] == 0){
      miss.error <-  append(miss.error, data[i,j] - mar1.out[i,j])
    }
  }
}

rmse(miss.error) # The RMSE for MI (amelia) for a MCAR2 dataset is about 1.104


### MAR-2
rm(list=ls())
load('mar2_singledf.RData')

# Create a matrix that notes NA values for each cell
miss.mat <- matrix(NA, nrow = nrow(mar2), ncol = ncol(mar2))
for (i in 1:nrow(mar2)){
  for (j in 1:ncol(mar2)){
    if (is.na(mar2[i,j])){
      miss.mat[i,j] <- 0
    }
    else{
      miss.mat[i,j] <- 1
    }
  }
}

rmse <- function(error){
  sqrt(mean(error^2))
}

# Run MI
mar2.out <- amelia(mar2, 1)
mar2.out <- mar2.out[[1]]$imp1

# Evaluate
miss.error <- numeric()
for (i in 1:nrow(miss.mat)){
  for (j in 1:ncol(miss.mat)){
    if (miss.mat[i,j] == 0){
      miss.error <-  append(miss.error, data[i,j] - mar2.out[i,j])
    }
  }
}

rmse(miss.error) # The RMSE for MI (amelia) for a MCAR2 dataset is about 1.241
