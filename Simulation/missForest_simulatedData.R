## MIML_missForest
## Lulu Peng
## Created: 11/21/18
## Last Updated: 11/21/18

# This script applies multiple imputation w/ missForest to simulated data

rm(list=ls())
setwd("~/GitHub/MIML/Simulation")

library(missForest)

# function to calculate RMSE
rmse <- function(miss.df, imp.df, true.df){
    # Create a matrix that notes NA values for each cell
    miss.mat <- matrix(NA, nrow = nrow(true.df), ncol = ncol(true.df))
    for (i in 1:nrow(miss.df)){
        for (j in 1:ncol(miss.df)){
            if (is.na(miss.df[i,j])){
                miss.mat[i,j] <- 0
            }
            else{
                miss.mat[i,j] <- 1
            }
        }
    }
    
    # calculate imputation errors
    miss.error <- numeric()
    for (i in 1:nrow(miss.mat)){
        for (j in 1:ncol(miss.mat)){
            if (miss.mat[i,j] == 0){
                miss.error <-  append(miss.error, true.df[i,j] - imp.df[i,j])
            }
        }
    }
    rmse <- sqrt(mean(miss.error^2))
    return(rmse)
}

# load datasets
load('mcar1_singledf.RData')
load('mcar2_singledf.RData')
load('mar1_singledf.RData')
load('mar2_singledf.RData')

lst <- list(mcar1, mcar2, mar1, mar2)
lst <- lapply(lst, function(x) {colnames(x) <- c("Y", "X1", "X2", "X3" ,"X4"); as.data.frame(x)})

imp <- list()
rmse.mf <- c() # empty vector to store RMSE
for(i in 1:length(lst)) {
    set.seed(11212018)
    # the function itself gives out-of-bag NRMSE and true NRMSE
    imp[[i]] <- missForest(lst[[i]], xtrue = data) 
    rmse.mf[i] <- rmse(lst[[i]], imp[[i]]$ximp, data)
}

rmse.df <- t(as.data.frame(rmse.mf))
colnames(rmse.df) <- c("mcar1", "mcar2", "mar1", "mar2")
# 0.9046120 0.8866511 0.9653924 1.0937433