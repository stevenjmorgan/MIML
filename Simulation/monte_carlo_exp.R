# This code generates and runs Monte Carlo experiments on missing data

rm(list=ls())
#setwd("C:/Users/Steve/Dropbox/PSU2018-2019/Fall2018/SODA502/MLMI")
setwd("C:/Users/sum410/Dropbox/PSU2018-2019/Fall2018/SODA502/MLMI")


library(MASS)
library(ggplot2)

seed <- 24519

# Create covariance matrix
S <- matrix(c(1.0, -0.12, -0.1, 0.5, 0.1,
             -0.12, 1.0, 0.1, -0.6, 0.1,
             -0.1, 0.1, 1.0, -0.5, 0.1,
             0.5, -0.6, -0.5, 1.0, 0.1,
             0.1, 0.1, 0.1, 0.1, 1.0), ncol = 5)
colnames(S) <- c("Y", "X1", "X2", "X3" ,"X4")

# Create a single dataset from multivariate normal dist. w/ 5 variables
set.seed(seed)
data <- mvrnorm(500, rep(0,5), S)
colnames(data) <- c("Y", "X1", "X2", "X3" ,"X4")

# Verify Gaussian assumptions (mu = 0, var = 1)
mean(data[,1])
mean(data[,2])
var(data[,1])
var(data[,2])

# Generate M matrix from uniform distribution to track missingness
m.matrix <- as.data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)))
set.seed(seed)
for(i in 1:nrow(data)){
              for(j in 1:ncol(data)){
                m.matrix[i,j] <- runif(1)
  }
}

# Create first MCAR dataset -> 86.4% full observations
mcar1 <- as.data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)))
for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)){
    
    if(j != 4){
      
      if(m.matrix[i,j] < 0.97){
        mcar1[i,j] <- data[i,j]
          
      }
      else{
        mcar1[i,j] <- NA
      }
    }
      
    else{
      mcar1[i,j] <- data[i,j]
    }
  }
}

summary(complete.cases(mcar1)) #86.4%


# Create second MCAR dataset -> 50.8% full observations
mcar2 <- as.data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)))
for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)){
    
    if(j != 4){
      
      if(m.matrix[i,j] < 0.841){
        mcar2[i,j] <- data[i,j]
        
      }
      else{
        mcar2[i,j] <- NA
      }
    }
    
    else{
      mcar2[i,j] <- data[i,j]
    }
  }
}

summary(complete.cases(mcar2)) #50.8%


### Create lists of simulated data

# Simulate 100 datasets of 500 obs. from multivariate normal dist., init. lists
simulated_multi <- vector("list", 100)
mcar1.list <- vector("list", 100)
mcar2.list <- vector("list", 100)
mar1.list <- vector("list", 100)
mar2.list <- vector("list", 100)
set.seed(seed)
for(i in 1:100){
  simulated_multi[[i]] <- as.data.frame(mvrnorm(500, rep(0,5), S))
  colnames(simulated_multi[[i]]) <- c("Y", "X1", "X2", "X3" ,"X4")
  mcar1.list[[i]] <- as.data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)))
  mcar2.list[[i]] <- as.data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)))
  mar1.list[[i]] <- as.data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)))
  mar2.list[[i]] <- as.data.frame(matrix(NA, nrow = nrow(data), ncol = ncol(data)))
}


# Generate 100 MCAR1 datasets
for(elem in 1:100){
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)){
      if(j != 4){
        
        if(m.matrix[i,j] < 0.97){
          mcar1.list[[elem]][i,j] <- simulated_multi[[elem]][i,j]
        }
        
        else{
          mcar1.list[[elem]][i,j] <- NA
        }
      }
          
      else{
        mcar1.list[[elem]][i,j] <- simulated_multi[[elem]][i,j]
      }
    }
  } 
}

# Generate 100 MCAR2 datasets
for(elem in 1:100){
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)){
      if(j != 4){
        
        if(m.matrix[i,j] < 0.841){
          mcar2.list[[elem]][i,j] <- simulated_multi[[elem]][i,j]
        }
        
        else{
          mcar2.list[[elem]][i,j] <- NA
        }
      }
      
      else{
        mcar2.list[[elem]][i,j] <- simulated_multi[[elem]][i,j]
      }
    }
  } 
}

# Generate 100 MAR1 datasets -> 69.4% of rows fully observed
for(elem in 1:100){
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)){
      if(j==4){  #X3 is completely observed
        mar1.list[[elem]][i,j] <- simulated_multi[[elem]][i,j]
      }
      if(j==1 | j==5){  # Y and X4 are MNAR
        if(m.matrix[i,j] < 0.97){
          mar1.list[[elem]][i,j] <- simulated_multi[[elem]][i,j]
        }
        else{
          mar1.list[[elem]][i,j] <- NA
        }
      }
      if(j==2 | j==3){
        if(simulated_multi[[elem]][i,4] < 0.8 & m.matrix[i,j] < 0.98){
          mar1.list[[elem]][i,j] <- simulated_multi[[elem]][i,j]
        }
        else{
          mar1.list[[elem]][i,j] <- NA
        }
      }
    }
  }
}

summary(complete.cases(mar1.list[[1]]))


# Generate 100 MAR2 datasets -> 50.6% of rows fully observed
for(elem in 1:100){
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)){
      if(j==4){  #X3 is completely observed
        mar2.list[[elem]][i,j] <- simulated_multi[[elem]][i,j]
      }
      if(j==1 | j==5){  # Y and X4 are MNAR
        if(m.matrix[i,j] < 0.97){
          mar2.list[[elem]][i,j] <- simulated_multi[[elem]][i,j]
        }
        else{
          mar2.list[[elem]][i,j] <- NA
        }
      }
      if(j==2 | j==3){
        if(simulated_multi[[elem]][i,4] < 0.1 & m.matrix[i,j] < 0.98){
          mar2.list[[elem]][i,j] <- simulated_multi[[elem]][i,j]
        }
        else{
          mar2.list[[elem]][i,j] <- NA
        }
      }
    }
  }
}

summary(complete.cases(mar2.list[[1]]))


save(mcar1.list, mcar2.list, mar1.list, mar2.list, simulated_multi, file = 'mcar_sets.RData')
