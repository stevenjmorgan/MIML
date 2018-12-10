rm(list=ls())
setwd("C:/Users/steve/Dropbox/PSU2018-2019/Fall2018/SODA502/MLMI")
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


# We can also evaluate by variable
miss.Y <- numeric()
data <- as.data.frame(data)
miss.mat <- as.data.frame(miss.mat)
for (i in 1:nrow(miss.mat)){
  if (miss.mat$Y[i] == 0){
    miss.Y <-  append(miss.error, data$Y[i] - mcar1.out$Y[i])
  }
}
rmse(miss.Y) # RMSE for Amelia for MNAR dataset for Y variable is about 1.024876

miss.X1 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X1[i] == 0){
    miss.X1 <-  append(miss.error, data$X1[i] - mcar1.out$X1[i])
  }
}
rmse(miss.X1) # RMSE for Amelia for MNAR dataset for X1 variable is about 1.012796

miss.X2 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X2[i] == 0){
    miss.X2 <-  append(miss.error, data$X2[i] - mcar1.out$X2[i])
  }
}
rmse(miss.X2) # RMSE for Amelia for MNAR dataset for X2 variable is about 1.005986

# We didn't impute for X3 so we don't calculate RMSE
miss.X4 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X4[i] == 0){
    miss.X4 <-  append(miss.error, data$X4[i] - mcar1.out$X4[i])
  }
}
rmse(miss.X4) # RMSE for Amelia for MNAR dataset for X1 variable is about 1.016983

########################



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

# We can also evaluate by variable
miss.Y <- numeric()
data <- as.data.frame(data)
miss.mat <- as.data.frame(miss.mat)
colnames(miss.mat) <- c("Y", "X1", "X2", "X3" ,"X4")
colnames(mcar2) <- c("Y", "X1", "X2", "X3" ,"X4")
for (i in 1:nrow(miss.mat)){
  if (miss.mat$Y[i] == 0){
    miss.Y <-  append(miss.error, data$Y[i] - mcar2.out$Y[i])
  }
}
rmse(miss.Y) # RMSE for Amelia for MCAR dataset for Y variable is about 1.092

miss.X1 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X1[i] == 0){
    miss.X1 <-  append(miss.error, data$X1[i] - mcar2.out$X1[i])
  }
}
rmse(miss.X1) # RMSE for Amelia for MNAR dataset for X1 variable is about 1.092

miss.X2 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X2[i] == 0){
    miss.X2 <-  append(miss.error, data$X2[i] - mcar2.out$X2[i])
  }
}
rmse(miss.X2) # RMSE for Amelia for MNAR dataset for X2 variable is about 1.092

# We didn't impute for X3 so we don't calculate RMSE
miss.X4 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X4[i] == 0){
    miss.X4 <-  append(miss.error, data$X4[i] - mcar2.out$X4[i])
  }
}
rmse(miss.X4) # RMSE for Amelia for MNAR dataset for X1 variable is about 1.092




#########################
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

rmse(miss.error) # The RMSE for MI (amelia) for a MAR1 dataset is about 1.104

colnames(miss.mat) <- c("Y", "X1", "X2", "X3" ,"X4")
colnames(mar1) <- c("Y", "X1", "X2", "X3" ,"X4")
colnames(mar1.out) <- c("Y", "X1", "X2", "X3" ,"X4")


# We can also evaluate by variable
miss.Y <- numeric()
data <- as.data.frame(data)
miss.mat <- as.data.frame(miss.mat)
for (i in 1:nrow(miss.mat)){
  if (miss.mat$Y[i] == 0){
    miss.Y <-  append(miss.error, data$Y[i] - mar1.out$Y[i])
  }
}
rmse(miss.Y) # RMSE for Amelia for MNAR dataset for Y variable is about 1.122

miss.X1 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X1[i] == 0){
    miss.X1 <-  append(miss.error, data$X1[i] - mar1.out$X1[i])
  }
}
rmse(miss.X1) # RMSE for Amelia for MNAR dataset for X1 variable is about 1.121

miss.X2 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X2[i] == 0){
    miss.X2 <-  append(miss.error, data$X2[i] - mar1.out$X2[i])
  }
}
rmse(miss.X2) # RMSE for Amelia for MNAR dataset for X2 variable is about 1.123

# We didn't impute for X3 so we don't calculate RMSE
miss.X4 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X4[i] == 0){
    miss.X4 <-  append(miss.error, data$X4[i] - mar1.out$X4[i])
  }
}
rmse(miss.X4) # RMSE for Amelia for MNAR dataset for X1 variable is about 1.122

##################
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

colnames(miss.mat) <- c("Y", "X1", "X2", "X3" ,"X4")
colnames(mar2) <- c("Y", "X1", "X2", "X3" ,"X4")
colnames(mar2.out) <- c("Y", "X1", "X2", "X3" ,"X4")


# We can also evaluate by variable
miss.Y <- numeric()
data <- as.data.frame(data)
miss.mat <- as.data.frame(miss.mat)
for (i in 1:nrow(miss.mat)){
  if (miss.mat$Y[i] == 0){
    miss.Y <-  append(miss.error, data$Y[i] - mar2.out$Y[i])
  }
}
rmse(miss.Y) # RMSE for Amelia for MNAR dataset for Y variable is about 1.221

miss.X1 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X1[i] == 0){
    miss.X1 <-  append(miss.error, data$X1[i] - mar2.out$X1[i])
  }
}
rmse(miss.X1) # RMSE for Amelia for MNAR dataset for X1 variable is about 1.219

miss.X2 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X2[i] == 0){
    miss.X2 <-  append(miss.error, data$X2[i] - mar2.out$X2[i])
  }
}
rmse(miss.X2) # RMSE for Amelia for MNAR dataset for X2 variable is about 1.223

# We didn't impute for X3 so we don't calculate RMSE
miss.X4 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X4[i] == 0){
    miss.X4 <-  append(miss.error, data$X4[i] - mar2.out$X4[i])
  }
}
rmse(miss.X4) # RMSE for Amelia for MNAR dataset for X1 variable is about 1.220
