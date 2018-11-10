###
### Claire Kelling
### 
### Multiple Imputation Project
###
### Implementation of the Protocol through SVM
### Created: 10/10/18
### Last Updated: 11/9/18
###

# In this code, I will implement the protocol for the SODA 502 project for 
# Multiple Imputation- Machine Learning (MIML).

#clear workspace
rm(list = ls())

library(Amelia)
library(caret)

###
### Direct Imputation
###

# First, I load full ANES data.
load("C:/Users/ckell/Desktop/Google Drive/01_Penn State/2018-2019/Fall 2018/soda_502/project/MIML/ANES/cleanedANES.Rdata")

# Next, I load the "amputed" ANES data (with missingness added - MCAR, MAR, MNAR).
#load("C:/Users/ckell/Desktop/Google Drive/01_Penn State/2018-2019/Fall 2018/soda_502/project/MIML/ANES/anesMCAR.RData")
load("C:/Users/ckell/Desktop/Google Drive/01_Penn State/2018-2019/Fall 2018/soda_502/project/MIML/ANES/anesMissing.RData")

# Load SVM function
source("C:/Users/ckell/Desktop/Google Drive/01_Penn State/2018-2019/Fall 2018/soda_502/project/MIML/src_mult_imp/00_svm_fnc.R")

# Exploratory analysis:
#par(mfrow=c(2,2))
#missmap(anes, main = "Missing values vs observed, Full Data")
#missmap(amp.mar, main = "Missing values vs observed, MAR")
#missmap(amp.mcar, main = "Missing values vs observed, MCAR")
#missmap(amp.mnar, main = "Missing values vs observed, MNAR")

# Listwise deletion for ``natural" missingness in anes
dim(anes)
anes <- anes[complete.cases(anes),]
dim(anes) #deletes 1889 rows


# Then I impute values via train/test split or cross validation (preferably CV) 
# for the three "amputed" datasets. 
# The imputation procedure results in more than one dataset (i.e.  if using 10-fold CV, 
# return the 10 datasets, each missing a "fold")

# First, I need to set up cross-validation, I will use the caret package method
# You can't have any missing values here, so I use the full dataset to create the folds
n_folds <- 10
flds <- createFolds(anes$vote.dem, k = n_folds, list = TRUE, returnTrain = FALSE)
#names(flds)[1] <- "train"

#how to access the folds:
fold1 <- amp.mar[flds[[1]],]

#SVM through the function found in David Roger's thesis
#SVMI<- function(data,categ.vars,modlist,max.iter=100,min.tol=1e-4)
#    categ.vars is a vector indicating the column numbers of the categorical variables
amp.mar <- amp.mar[,-7] #bible variable is causing some problems in the SVM
colnames(amp.mar)
categ.vars <- c(1,2,3,4,7,8,9,10,11,12,13) #everything except "HRC.FT", "DJT.FT"

# convert these columns to categorical
amp.mar[categ.vars] <- lapply(amp.mar[categ.vars], factor)
sapply(amp.mar, class)

#    modlist should be a list containing the prefitted SVM models for each of the categ.vars
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Use this when we want to just use training sets for SVM
# ind_train <- unlist(flds[-10])
# training <- amp.mar[ind_train,]

# For now, I will use the full dataset for training
training <- amp.mar
#svm_Linear <- train(DJT.FT ~., data = training, method = "svmLinear",
#                    trControl=trctrl,
#                    preProcess = c("center", "scale"),
#                    tuneLength = 10)

#to store the full list of models
modlist <- list()

#for(j in 1:n_folds){
  #when we want to do cross-validation
  #ind_train <- unlist(flds[-10])
  #training <- amp.mar[ind_train,]
  for(i in 1:length(categ.vars)){
    print(i)
    #i <- 5
    
    #take out missingness in training data (response and categorical variables)
    comp_train <- training[complete.cases(training),]
    
    #train svm on one categorical variable
    #i <- 2
    form <- paste(colnames(comp_train)[categ.vars[i]], "~ .")
    form <- as.formula(form)
    
    svm_Linear <- train(form, data = comp_train, method = "svmLinear",
                        trControl=trctrl,
                        preProcess = c("center", "scale"),
                        tuneLength = 10)
    
    #add svm model to the modlist 
    modlist[[i]] <- svm_Linear
  }
#}

#run the imputation
out <- SVMI(amp.mar,categ.vars,modlist,max.iter=100,min.tol=1e-4)


# I compare the imputed datasets against the original dataset (with no added missingness) 
# to determine how well the algorithm recovers missing values. (This line might not be necessary)


###
### Regression
###

# I will use the imputed data created above for my algorithm. 

# With the imputed dataset, I run two regressions:  logit and OLS.

# Logit:
logit_form <- as.formula(.~.)
logit_full <- glm(logit_form, data = anes, family=binomial(link='logit'))
logit_mar <- glm(logit_form, data = amp.mar, family=binomial(link='logit'))
logit_mnar <- glm(logit_form, data = amp.mnar, family=binomial(link='logit'))
logit_mcar <- glm(logit_form, data = amp.mcar, family=binomial(link='logit'))

# OLS:
ols_form <- as.formula(.~.)
ols_full <- lm(ols_form, data = anes)
ols_mar <- lm(ols_form, data = amp.mar)
ols_mnar <- lm(ols_form, data = amp.mnar)
ols_mcar <- lm(ols_form, data = amp.mcar)

#First, I report the results: 
###
### Logit: 
### 
#     Coefficients
logit_bind_coeff <- cbind(as.numeric(logit_full$coefficients),
                    as.numeric(logit_mar$coefficients),
                    as.numeric(logit_mnar$coefficients),
                    as.numeric(logit_mcar$coefficients))
colnames(logit_bind_coeff) <- c("full", "mar", "mnar", "mcar")

#     P-value? Not sure if this is necessary? for coefficients or model?

#     Model fit
logit_mod_f <- as.data.frame(c(logit_full$aic,
                               logit_mar$aic
                               logit_mnar$aic
                               logit_mcar$aic))
colnames(logit_mod_f) <- c("full", "mar", "mnar", "mcar")
###
### OLS: 
### 
#     Coefficients
ols_bind_coeff <- cbind(as.numeric(ols_full$coefficients),
                        as.numeric(ols_mar$coefficients),
                        as.numeric(ols_mnar$coefficients),
                        as.numeric(ols_mcar$coefficients))
colnames(ols_bind_coeff) <- c("full", "mar", "mnar", "mcar")

#     P-value? Not sure if this is necessary? for coefficients or model?

#     Model fit
ols_mod_f <- as.data.frame(c(ols_full$aic,
                               ols_mar$aic
                               ols_mnar$aic
                               ols_mcar$aic))
colnames(ols_mod_f) <- c("full", "mar", "mnar", "mcar")


###
### Prediction
###
# Once again, I use the imputed data that is created above.
# I compare the imputed simulated missing data to its original value.


# I report the MAPE and MSE for the data. 
act <- anes$vote.dem
pred_mar <- amp.mar$vote.dem
pred_mnar <- amp.mnar$vote.dem
pred_mcar <- amp.mcar$vote.dem

#mse
mse_mar <- mean((act-pred_mar)^2)
mse_mnar <- mean((act-pred_mnar)^2)
mse_mcar <- mean((act-pred_mcar)^2)

#mape
mape_mar <- mean(abs((act-pred_mar)/act))
mape_mnar <- mean(abs((act-pred_mnar)/act))
mape_mcar <- mean(abs((act-pred_mcar)/act))