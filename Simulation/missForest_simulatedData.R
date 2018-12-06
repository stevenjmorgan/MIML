## MIML_missForest
## Lulu Peng
## Created: 11/21/18
## Last Updated: 12/05/18

# This script applies multiple imputation w/ missForest to simulated data

rm(list=ls())
setwd("~/GitHub/MIML/Simulation")

library(missForest)

# function to calculate miss errors
miss.error <- function(miss.df, imp.df, true.df){
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
    return(miss.error)
}

# load datasets
load('mcar1_singledf.RData')
load('mcar2_singledf.RData')
load('mar1_singledf.RData')
load('mar2_singledf.RData')

lst <- list(mcar1, mcar2, mar1, mar2)
varnames <- c("Y", "X1", "X2", "X3", "X4")
lst <- lapply(lst, function(x) {colnames(x) <- varnames; as.data.frame(x)})

imp <- list()
miss.error.mf <- list()
for(i in 1:length(lst)) {
    set.seed(11212018)
    # the function itself gives out-of-bag NRMSE and true NRMSE
    imp[[i]] <- missForest(lst[[i]], xtrue = data) 
    miss.error.mf[[i]] <- miss.error(lst[[i]], imp[[i]]$ximp, data)
}

# rmse by variable
rmse.var <- matrix(NA, 4, 5)
colnames(rmse.var) <- varnames
rownames(rmse.var) <- c("MCAR1", "MCAR2", "MAR1", "MAR2")
for(i in 1:length(miss.error.mf)) {
    miss <- miss.error.mf[[i]]
    rmse.var[i,] <-
        sapply(varnames, function(x) {
        temp <- miss[which(names(miss)==x)]; 
        return(sqrt(mean(temp^2)))
        })
}
rmse.var <- round(rmse.var, 3)
xtable(rmse.var, digits = 3)

rmse.mf <- sapply(miss.error.mf, function(x) sqrt(mean(x^2)))

rmse.df <- as.data.frame(rmse.mf)
rownames(rmse.df) <- c("MCAR1", "MCAR2", "MAR1", "MAR2")
# 0.9046120 0.8866511 0.9653924 1.0937433