rm(list=ls())
setwd("C:/Users/Steve/Dropbox/PSU2018-2019/Fall2018/SODA502/MLMI")
#setwd("C:/Users/sum410/Dropbox/PSU2018-2019/Fall2018/SODA502/MLMI")


library(MASS)
library(ggplot2)
library(e1071)

rmse <- function(error){
  sqrt(mean(error^2))
}


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
colnames(miss.mat) <- c("Y", "X1", "X2", "X3" ,"X4")
colnames(mcar2) <- c("Y", "X1", "X2", "X3" ,"X4")
mcar2 <- as.data.frame(mcar2)

# Subset data by complete cases
mcar2.comp <- mcar2[complete.cases(mcar2),]
colnames(mcar2.comp) <- c("Y", "X1", "X2", "X3" ,"X4")
mcar2.incomp <- mcar2[!complete.cases(mcar2),]
colnames(mcar2.incomp) <- c("Y", "X1", "X2", "X3" ,"X4")

# Run SVR models for different combo's of X on complete cases to predict Y
model.full <- svm(Y ~ X1 + X2 + X3 + X4, mcar2.comp) # Full model -> use X1:X4
predictedY <- predict(model.full, mcar2.comp)
error <- mcar2.comp$Y - predictedY
rmse(error) # 0.750

model.x13 <- svm(Y ~ X1 + X2 + X3, mcar2.comp) # Model Y without x4
predictedY <- predict(model.x13, mcar2.comp)
error <- mcar2.comp$Y - predictedY
rmse(error) #0.764

model.x12 <- svm(Y ~ X1 + X2, mcar2.comp) # Model Y without x3 or x4
predictedY <- predict(model.x12, mcar2.comp)
error <- mcar2.comp$Y - predictedY
rmse(error) # 0.971

model.x1 <- svm(Y ~ X1 + X3, mcar2.comp) # Model Y without x2 or x4
predictedY <- predict(model.x1, mcar2.comp)
error <- mcar2.comp$Y - predictedY
rmse(error) # 0.55

model.x24 <- svm(Y ~ X2 + X3 + X4, mcar2.comp) # Model Y without x2 or x4
predictedY <- predict(model.x24, mcar2.comp)
error <- mcar2.comp$Y - predictedY
rmse(error) # 0.830


# Run model on incomplete data -> subset incomplete data for missing values of Y
incomp.Y.mcar2 <- mcar2.incomp[is.na(mcar2.incomp$Y),]
incomp.Y.mcar2$Y <- 0
pred.incomp.y <- predict(model.full, incomp.Y.mcar2)


# Impute Y values with SVR models depending on non-missing X values
imputedY <- as.data.frame(matrix(NA, nrow = nrow(mcar2), ncol = ncol(mcar2)))
colnames(imputedY) <- c("Y", "X1", "X2", "X3" ,"X4")
for (i in 1:nrow(mcar2)){
  
  # For each row in mcar2, if Y is not missing, just store real value
  if (!is.na(mcar2$Y[i])){
    imputedY$Y[i] <- mcar2$Y[i]
  }
  
  # If Y is NA, predict value of Y and store (this is the imputation)
  if (is.na(mcar2$Y[i])){ 
    
    # We want to use as much info. as possible to predict missing values in Y
    # So if there is no missingness in X1, X2, X3, and X4, use all values to predict
    if (!is.na(mcar2$X1[i]) & !is.na(mcar2$X2[i]) & !is.na(mcar2$X3[i]) & !is.na(mcar2$X4[i])){
      mcar2$Y[i] <- 0 #Set to 0 temporarily to run model-> replace w/ pred.
      imputedY$Y[i] <- as.numeric(predict(model.full, mcar2[i,]))
    }
    
    # If Y is missing and X4 is also missing (can't run full SVR model)
    if(!is.na(mcar2$X1[i]) & !is.na(mcar2$X2[i]) & !is.na(mcar2$X3[i]) & is.na(mcar2$X4[i])){
      mcar2$Y[i] <- 0 #Set to 0 temporarily to run model-> replace w/ pred.
      mcar2$X4[i] <- 0 #Set to zero so SVR will run -> not used in actual calc.
      imputedY$Y[i] <- as.numeric(predict(model.x13, mcar2[i,]))
    }
    
    # If Y is missing and X3 and X4 are also missing
    if(!is.na(mcar2$X1[i]) & !is.na(mcar2$X2[i])){
      mcar2$Y[i] <- 0 #Set to 0 temporarily to run model-> replace w/ pred.
      imputedY$Y[i] <- as.numeric(predict(model.x12, mcar2[i,]))
    }
    
    # If Y is missing and X2 and X4 are also missing
    if(!is.na(mcar2$X1[i]) & !is.na(mcar2$X3[i])){
      mcar2$Y[i] <- 0 #Set to 0 temporarily to run model-> replace w/ pred.
      mcar2$X2[i] <- 0 #Set to zero so SVR will run -> not used in actual calc.
      mcar2$X4[i] <- 0
      imputedY$Y[i] <- as.numeric(predict(model.x1, mcar2[i,]))
    }
    
    # If Y is missing and X1 is also missing
    if(!is.na(mcar2$X2[i]) & !is.na(mcar2$X3[i]) & !is.na(mcar2$X4[i])){
      mcar2$Y[i] <- 0 #Set to 0 temporarily to run model-> replace w/ pred.
      mcar2$X1[i] <- 0 #Set to zero so SVR will run -> not used in actual calc.
      imputedY$Y[i] <- as.numeric(predict(model.x24, mcar2[i,]))
    }
  }
}

for (i in 1:nrow(mcar2)){
  for (j in 1:ncol(mcar2)) {
    if(is.na(mcar2[i,j])){
      imputedY[i,j] <- 0
    }
  }
}

# Impute values for X1
# Since we changed some of the NA values in X to zero, let's reload the data
load('mcar2_singledf.RData')
colnames(mcar2) <- c("Y", "X1", "X2", "X3" ,"X4")

# The imputed values of Y should be used for predictions of X, so include those imputations
summary(is.na(mcar2$Y))
for (i in 1:nrow(mcar2)){
  if (is.na(mcar2$Y[i])){
    mcar2$Y[i] <- imputedY$Y[i]
  }
}
summary(is.na(mcar2$Y))

## Build models to predict X1
# Subset data by complete cases
mcar2.comp <- mcar2[complete.cases(mcar2),]

model.full.X1 <- svm(X1 ~ Y + X2 + X3 + X4, mcar2.comp) # Full model -> use Y, X2, X3, X4
predictedY <- predict(model.full.X1, mcar2.comp)
error <- mcar2.comp$X1 - predictedY
rmse(error) # 0.638

model.X1.noX4 <- svm(X1 ~ Y + X2 + X3, mcar2.comp) # X4 is missing
predictedY <- predict(model.X1.noX4, mcar2.comp)
error <- mcar2.comp$X1 - predictedY
rmse(error) # 0.673

model.X1.noX3 <- svm(X1 ~ Y + X2 + X4, mcar2.comp) # X3 is missing
predictedY <- predict(model.X1.noX3, mcar2.comp)
error <- mcar2.comp$X1 - predictedY
rmse(error) # 0.961

model.X1.noX2 <- svm(X1 ~ Y + X3 + X4, mcar2.comp) # X2 is missing
predictedY <- predict(model.X1.noX2, mcar2.comp)
error <- mcar2.comp$X1 - predictedY
rmse(error) # 0.764

model.X1.noX3X4 <- svm(X1 ~ Y + X2, mcar2.comp) # X3 and X4 are missing
predictedY <- predict(model.X1.noX3X4, mcar2.comp)
error <- mcar2.comp$X1 - predictedY
rmse(error) # 0.995

model.X1.onlyY <- svm(X1 ~ Y, mcar2.comp) # X2, X3, and X4 are missing
predictedY <- predict(model.X1.onlyY, mcar2.comp)
error <- mcar2.comp$X1 - predictedY
rmse(error) # 1.02 -> something funny is going on here too

# Predict X1 values for rows with X1 missing
for (i in 1:nrow(mcar2)){
  
  # For each row in mcar2, if X1 is not missing, just store real value
  if (!is.na(mcar2$X1[i])){
    imputedY$X1[i] <- mcar2$X1[i]
  }
  
  # If X1 is NA, predict value of X1 and store (this is the imputation)
  if (is.na(mcar2$X1[i])){ 
    
    # Run on most info. possible: only X1 is missing
    if (!is.na(mcar2$Y[i]) & !is.na(mcar2$X2[i]) & !is.na(mcar2$X3[i]) & !is.na(mcar2$X4[i])){
      mcar2$X1[i] <- 0 #Set to 0 temporarily to run model-> replace w/ pred.
      imputedY$X1[i] <- as.numeric(predict(model.full.X1, mcar2[i,]))
    }
    
    # In this dataset, it turns out there were no instances where X1 was missing
    # as well as either X2, X3, or X4, so I don't implement the other SVR's
    
  }
}


### Impute for X2 with same process
load('mcar2_singledf.RData')
colnames(mcar2) <- c("Y", "X1", "X2", "X3" ,"X4")
summary(is.na(mcar2$Y))
for (i in 1:nrow(mcar2)){
  if (is.na(mcar2$Y[i])){
    mcar2$Y[i] <- imputedY$Y[i]
  }
}
summary(is.na(mcar2$Y))

summary(is.na(mcar2$X1))
for (i in 1:nrow(mcar2)){
  if (is.na(mcar2$X1[i])){
    mcar2$X1[i] <- imputedY$X1[i]
  }
}
summary(is.na(mcar2$X1))

## Build models to predict X1
# Subset data by complete cases
mcar2.comp <- mcar2[complete.cases(mcar2),]

model.full.X2 <- svm(X2 ~ Y + X1 + X3 + X4, mcar2.comp) # Full model -> use Y, X1, X3, X4
predictedY <- predict(model.full.X2, mcar2.comp)
error <- mcar2.comp$X2 - predictedY
rmse(error) # 0.610

model.X2.noX3 <- svm(X2 ~ Y + X1 + X4, mcar2.comp) # X2 and X3 missing
predictedY <- predict(model.X2.noX3, mcar2.comp)
error <- mcar2.comp$X2 - predictedY
rmse(error) # 0.898

model.X2.noX4 <- svm(X2 ~ Y + X1 + X3, mcar2.comp) # X2 and X4 missing
predictedY <- predict(model.X2.noX4, mcar2.comp)
error <- mcar2.comp$X2 - predictedY
rmse(error) # 0.655

model.X1Y <- svm(X2 ~ Y + X1, mcar2.comp) # Just use Y and X1
predictedY <- predict(model.X1Y, mcar2.comp)
error <- mcar2.comp$X2 - predictedY
rmse(error) # 0.932

# Predict X2 values for rows with X2 missing
for (i in 1:nrow(mcar2)){
  
  # For each row in mcar2, if X2 is not missing, just store real value
  if (!is.na(mcar2$X2[i])){
    imputedY$X2[i] <- mcar2$X2[i]
  }
  
  # If X2 is NA, predict value of X2 and store (this is the imputation)
  if (is.na(mcar2$X2[i])){ 
    
    # Run on most info. possible: only X1 is missing
    if (!is.na(mcar2$Y[i]) & !is.na(mcar2$X1[i]) & !is.na(mcar2$X3[i]) & !is.na(mcar2$X4[i])){
      mcar2$X2[i] <- 0 #Set to 0 temporarily to run model-> will be replaced w/ pred.
      imputedY$X2[i] <- as.numeric(predict(model.full.X2, mcar2[i,]))
    }
    
    else{
      mcar2$X2[i] <- 0 #Set to 0 temporarily to run model-> will be replaced w/ pred.
      mcar2$X4[i] <- 0 #Set to 0 temporarily to run model-> will be replaced w/ pred.
      imputedY$X2[i] <- as.numeric(predict(model.X1Y, mcar2[i,]))
    }
  }
}


### Impute X3
### Impute for X3 with same process
load('mcar2_singledf.RData')
colnames(mcar2) <- c("Y", "X1", "X2", "X3" ,"X4")
summary(is.na(mcar2$Y))
for (i in 1:nrow(mcar2)){
  if (is.na(mcar2$Y[i])){
    mcar2$Y[i] <- imputedY$Y[i]
  }
}
summary(is.na(mcar2$Y))

summary(is.na(mcar2$X1))
for (i in 1:nrow(mcar2)){
  if (is.na(mcar2$X1[i])){
    mcar2$X1[i] <- imputedY$X1[i]
  }
}
summary(is.na(mcar2$X1))

summary(is.na(mcar2$X2))
for (i in 1:nrow(mcar2)){
  if (is.na(mcar2$X2[i])){
    mcar2$X2[i] <- imputedY$X2[i]
  }
}
summary(is.na(mcar2$X2))

# This is actually unnecessary since there are no NA's in X3, but writing out it if there were NA's
## Build models to predict X3
# Subset data by complete cases
mcar2.comp <- mcar2[complete.cases(mcar2),]

model.full.X3 <- svm(X3 ~ Y + X1 + X2 + X4, mcar2.comp) # Full model -> use Y, X1, X2, X4
predictedY <- predict(model.full.X3, mcar2.comp)
error <- mcar2.comp$X3 - predictedY
rmse(error) # 0.428

model.X3.noX4 <- svm(X3 ~ Y + X1 + X2, mcar2.comp) #  use Y, X1, and X2
predictedY <- predict(model.X3.noX4, mcar2.comp)
error <- mcar2.comp$X3 - predictedY
rmse(error) # 0.461

# We are using imputed values from previous runs, so no need to build out more permutations of right-hand side of model
# Predict X3 values for rows with X3 missing
for (i in 1:nrow(mcar2)){
  
  # For each row in mcar2, if X3 is not missing, just store real value
  if (!is.na(mcar2$X3[i])){
    imputedY$X3[i] <- mcar2$X3[i]
  }
  
  # If X3 is NA, predict value of X3 and store (this is the imputation)
  if (is.na(mcar2$X3[i])){ 
    
    # Run on most info. possible: only X1 is missing
    if (!is.na(mcar2$Y[i]) & !is.na(mcar2$X1[i]) & !is.na(mcar2$X2[i]) & !is.na(mcar2$X4[i])){
      mcar2$X3[i] <- 0 #Set to 0 temporarily to run model-> will be replaced w/ pred.
      imputedY$X3[i] <- as.numeric(predict(model.full.X3, mcar2[i,]))
    }
    
    else{
      mcar2$X3[i] <- 0 #Set to 0 temporarily to run model-> will be replaced w/ pred.
      mcar2$X4[i] <- 0 #Set to 0 temporarily to run model-> will be replaced w/ pred.
      imputedY$X3[i] <- as.numeric(predict(model.X3.noX4, mcar2[i,]))
    }
    
  }
}

### Impute X4
load('mcar2_singledf.RData')
colnames(mcar2) <- c("Y", "X1", "X2", "X3" ,"X4")
summary(is.na(mcar2$Y))
for (i in 1:nrow(mcar2)){
  if (is.na(mcar2$Y[i])){
    mcar2$Y[i] <- imputedY$Y[i]
  }
}
summary(is.na(mcar2$Y))

summary(is.na(mcar2$X1))
for (i in 1:nrow(mcar2)){
  if (is.na(mcar2$X1[i])){
    mcar2$X1[i] <- imputedY$X1[i]
  }
}
summary(is.na(mcar2$X1))

summary(is.na(mcar2$X2))
for (i in 1:nrow(mcar2)){
  if (is.na(mcar2$X2[i])){
    mcar2$X2[i] <- imputedY$X2[i]
  }
}
summary(is.na(mcar2$X2))

# Unnecessary since we don't actually impute any values of X3
summary(is.na(mcar2$X3))
for (i in 1:nrow(mcar2)){
  if (is.na(mcar2$X3[i])){
    mcar2$X3[i] <- imputedY$X3[i]
  }
}
summary(is.na(mcar2$X3))

## Build models to predict X4
# Subset data by complete cases
mcar2.comp <- mcar2[complete.cases(mcar2),]

model.full.X4 <- svm(X4 ~ Y + X1 + X2 + X3, mcar2.comp) # Full model -> use Y, X1, X2, X3
predictedY <- predict(model.full.X4, mcar2.comp)
error <- mcar2.comp$X4 - predictedY
rmse(error) # 0.833

# There are no missing values in the columns beyond X4 now b/c of imputation
for (i in 1:nrow(mcar2)){
  
  # For each row in mcar2, if X4 is not missing, just store real value
  if (!is.na(mcar2$X4[i])){
    imputedY$X4[i] <- mcar2$X4[i]
  }
  
  # If X3 is NA, predict value of X3 and store (this is the imputation)
  if (is.na(mcar2$X4[i])){ 
    
    mcar2$X4[i] <- 0 #Set to 0 temporarily to run model-> will be replaced w/ pred.
    imputedY$X4[i] <- as.numeric(predict(model.full.X4, mcar2[i,]))
  }
}

save(imputedY, miss.mat, file = 'imputeSVR_MCAR2.RData')

### So now we have an imputed dataset, the dataset w/ missingness, and the real 
### values behind those missing values -> Evaluate w/ RMSE on NA data
rm(list=ls())
load('imputeSVR_MCAR2.RData')
load('mcar2_singledf.RData')

rmse <- function(error){
  sqrt(mean(error^2)) # Error is a vector of differences
}

miss.error <- numeric()
for (i in 1:nrow(miss.mat)){
  for (j in 1:ncol(miss.mat)){
    if (miss.mat[i,j] == 0){
      miss.error <-  append(miss.error, data[i,j] - imputedY[i,j])
    }
  }
}

rmse(miss.error) # The RMSE for SVR for a MNAR dataset is about 0.934

# We can also evaluate by variable
miss.Y <- numeric()
data <- as.data.frame(data)
miss.mat <- as.data.frame(miss.mat)
for (i in 1:nrow(miss.mat)){
  if (miss.mat$Y[i] == 0){
    miss.Y <-  append(miss.error, data$Y[i] - imputedY$Y[i])
  }
}
rmse(miss.Y) # RMSE for SVR for MNAR dataset for Y variable is about 0.934

miss.X1 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X1[i] == 0){
    miss.X1 <-  append(miss.error, data$X1[i] - imputedY$Y[i])
  }
}
rmse(miss.X1) # RMSE for SVR for MNAR dataset for X1 variable is about 0.934

miss.X2 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X2[i] == 0){
    miss.X2 <-  append(miss.error, data$X2[i] - imputedY$Y[i])
  }
}
rmse(miss.X2) # RMSE for SVR for MNAR dataset for X2 variable is about 0.934

# We didn't impute for X3 so we don't calculate RMSE
miss.X4 <- numeric()
for (i in 1:nrow(miss.mat)){
  if (miss.mat$X4[i] == 0){
    miss.X4 <-  append(miss.error, data$X4[i] - imputedY$Y[i])
  }
}
rmse(miss.X4) # RMSE for SVR for MNAR dataset for X1 variable is about 0.950
