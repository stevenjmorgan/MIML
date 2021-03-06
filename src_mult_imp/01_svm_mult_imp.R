###
### Claire Kelling
### 
### Multiple Imputation Project
###
### Implementation of the Protocol through SVM
### Created: 10/10/18
### Last Updated: 11/29/18
###

# In this code, I will implement the protocol for the SODA 502 project for 
# Multiple Imputation- Machine Learning (MIML).

#clear workspace
rm(list = ls())

library(Amelia)
library(caret)
library(stargazer)

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

### Checking number of NA's per row
sum_nacount <- apply(amp.mar, 1, function(x) sum(is.na(x)))
length(which(sum_nacount>1))
sum_nacount2 <- apply(amp.mnar, 1, function(x) sum(is.na(x)))
length(which(sum_nacount2>1))
sum_nacount3 <- apply(amp.mcar, 1, function(x) sum(is.na(x)))
length(which(sum_nacount3>1))

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
anes <- as.data.frame(anes)

# specify variable type
cols1 <- c("vote.dem", "dem", "gop", "female", "black", "hisp", "white") # categorical
cols2 <- c("ideo", "bible", "inform", "edu", "income") # ordinal
# Use lapply() to coerce and replace the chosen columns to factors:
anes[cols1] <- lapply(anes[cols1], factor)
anes[cols2] <- lapply(anes[cols2], ordered)
amp.mar[cols1] <- lapply(amp.mar[cols1], factor)
amp.mar[cols2] <- lapply(amp.mar[cols2], ordered)
amp.mnar[cols1] <- lapply(amp.mnar[cols1], factor)
amp.mnar[cols2] <- lapply(amp.mnar[cols2], factor)
amp.mcar[cols1] <- lapply(amp.mcar[cols1], factor)
amp.mcar[cols2] <- lapply(amp.mcar[cols2], factor)


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
# amp.mar <- amp.mar[,-7] #bible variable is causing some problems in the SVM
# amp.mnar <- amp.mnar[,-7] 
# amp.mcar <- amp.mcar[,-7] 
# colnames(amp.mar)
categ.vars <- c(1,2,3,4,7,8,9,10,11,12,13) #everything except "HRC.FT", "DJT.FT"
# 
# # convert these columns to categorical
# amp.mar[categ.vars] <- lapply(amp.mar[categ.vars], factor)
# amp.mnar[categ.vars] <- lapply(amp.mnar[categ.vars], factor)
# amp.mcar[categ.vars] <- lapply(amp.mcar[categ.vars], factor)
# sapply(amp.mar, class)

#    modlist should be a list containing the prefitted SVM models for each of the categ.vars
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Use this when we want to just use training sets for SVM
# ind_train <- unlist(flds[-10])
# training <- amp.mar[ind_train,]

# For now, I will use the full dataset for training
training.mar <- amp.mar
training.mnar <- amp.mnar
training.mcar <- amp.mcar

#to store the full list of all models (continuous and categorical)
modlist.all.mar <- list()
modlist.all.mnar <- list()
modlist.all.mcar <- list()

#for(j in 1:n_folds){
#when we want to do cross-validation
#ind_train <- unlist(flds[-10])
#training <- amp.mar[ind_train,]
for(i in 1:ncol(amp.mar)){
  print(i)
  #i <- 5
  
  #take out missingness in training data (response and categorical variables)
  comp_train <- training.mar[complete.cases(training.mar),]
  
  #train svm on one categorical variable
  #i <- 2
  form <- paste(colnames(comp_train)[i], "~ .")
  form <- as.formula(form)
  
  svm_Linear <- train(form, data = comp_train, method = "svmLinear",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)
  
  #add svm model to the modlist 
  modlist.all.mar[[i]] <- svm_Linear
}
#}

for(i in 1:ncol(amp.mnar)){
  print(i)
  #i <- 5
  
  #take out missingness in training data (response and categorical variables)
  comp_train <- training.mnar[complete.cases(training.mnar),]
  
  #train svm on one categorical variable
  #i <- 2
  form <- paste(colnames(comp_train)[i], "~ .")
  form <- as.formula(form)
  
  svm_Linear <- train(form, data = comp_train, method = "svmLinear",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)
  
  #add svm model to the modlist 
  modlist.all.mnar[[i]] <- svm_Linear
}


for(i in 1:ncol(amp.mcar)){
  print(i)
  #i <- 5
  
  #take out missingness in training data (response and categorical variables)
  comp_train <- training.mcar[complete.cases(training.mcar),]
  
  #train svm on one categorical variable
  #i <- 2
  form <- paste(colnames(comp_train)[i], "~ .")
  form <- as.formula(form)
  
  svm_Linear <- train(form, data = comp_train, method = "svmLinear",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)
  
  #add svm model to the modlist 
  modlist.all.mcar[[i]] <- svm_Linear
}

#run the imputation
out.mar <- SVMI_all(amp.mar,categ.vars,modlist.all.mar,max.iter=100,min.tol=1e-4)
out.mnar <- SVMI_all(amp.mnar,categ.vars,modlist.all.mnar,max.iter=100,min.tol=1e-4)
out.mcar <- SVMI_all(amp.mcar,categ.vars,modlist.all.mcar,max.iter=100,min.tol=1e-4)

#check to make sure there are no missing values
length(which(is.na(out.mar)))
length(which(is.na(out.mcar)))
length(which(is.na(out.mnar)))

#number of missing values to start
length(which(is.na(amp.mar)))
length(which(is.na(amp.mcar)))
length(which(is.na(amp.mnar)))

# I compare the imputed datasets against the original dataset (with no added missingness) 
# to determine how well the algorithm recovers missing values. (This line might not be necessary)


###
### Regression
###

# I will use the imputed data created above for my algorithm. 

# With the imputed dataset, I run two regressions:  logit and OLS.

# Logit:
# Model vote choice w/ feeling thermometers ##
logit_form <- as.formula(vote.dem ~ dem + gop + ideo + HRC.FT + DJT.FT + bible + inform + edu + income + female + black + hisp)
#logit.FT_svm <- glm(logit_form, data = ANES.svm$ximp, family=binomial(link='logit'))
# logit_full <- glm(logit_form, data = anes.complete, family=binomial(link='logit'))
# logit_mar <- glm(logit_form, data = out.mar$ximp, family=binomial(link='logit'))
# logit_mnar <- glm(logit_form, data = out.mnar$ximp, family=binomial(link='logit'))
# logit_mcar <- glm(logit_form, data = out.mcar$ximp, family=binomial(link='logit'))

## Model vote choice w/o feeling thermometers (reduce collinearity w/ ideology) ##
# vote.dem.noFT <- glm(vote.dem ~ dem + gop + ideo + bible + inform + edu + 
#                          income + female + black + hisp, data = ANES.svm$ximp, family = 'binomial')
# summary(vote.dem.noFT)
logit_form <- as.formula(vote.dem ~ dem + gop + ideo + bible + inform + edu + 
                           income + female + black + hisp)
#logit.noFT_svm <- glm(logit_form, data = ANES.svm$ximp, family=binomial(link='logit'))
logit_full <- glm(logit_form, data = anes, family=binomial(link='logit'))
logit_mar <- glm(logit_form, data = out.mar, family=binomial(link='logit'))
logit_mnar <- glm(logit_form, data = out.mnar, family=binomial(link='logit'))
logit_mcar <- glm(logit_form, data = out.mcar, family=binomial(link='logit'))

# OLS:
# Model feeling thermometer for Trump (0-100 treated as continuous)
ols_form <- as.formula(DJT.FT ~ dem + gop + ideo + bible + inform + edu + edu + income + female + black + hisp)
#ols_svm <- lm(ols_form, data = ANES.svm$ximp)
ols_full <- lm(ols_form, data = anes)
ols_mar <- lm(ols_form, data = out.mar)
ols_mnar <- lm(ols_form, data = out.mnar)
ols_mcar <- lm(ols_form, data = out.mcar)

ols_list <- list(ols_full, ols_mar, ols_mnar, ols_mcar)
logit_list <- list(logit_full, logit_mar, logit_mnar, logit_mcar)

save(logit_list, ols_list, file = "C:/Users/ckell/Desktop/Google Drive/01_Penn State/2018-2019/Fall 2018/soda_502/project/MIML/ANES/model output/ANES_svm.RData")

# # Logit:
# logit_form <- as.formula(.~.)
# logit_full <- glm(logit_form, data = anes, family=binomial(link='logit'))
# logit_mar <- glm(logit_form, data = out.mar, family=binomial(link='logit'))
# logit_mnar <- glm(logit_form, data = out.mnar, family=binomial(link='logit'))
# logit_mcar <- glm(logit_form, data = out.mcar, family=binomial(link='logit'))
# 
# # OLS:
# ols_form <- as.formula(.~.)
# ols_full <- lm(ols_form, data = anes)
# ols_mar <- lm(ols_form, data = out.mar)
# ols_mnar <- lm(ols_form, data = out.mnar)
# ols_mcar <- lm(ols_form, data = out.mcar)
# 
# #First, I report the results: 
# ###
# ### Logit: 
# ### 
# #     Coefficients
# logit_bind_coeff <- cbind(as.numeric(logit_full$coefficients),
#                     as.numeric(logit_mar$coefficients),
#                     as.numeric(logit_mnar$coefficients),
#                     as.numeric(logit_mcar$coefficients))
# colnames(logit_bind_coeff) <- c("full", "mar", "mnar", "mcar")
# 
# #     P-value? Not sure if this is necessary? for coefficients or model?
# 
# #     Model fit
# logit_mod_f <- as.data.frame(c(logit_full$aic,
#                                logit_mar$aic,
#                                logit_mnar$aic,
#                                logit_mcar$aic))
# colnames(logit_mod_f) <- c("full", "mar", "mnar", "mcar")
# ###
# ### OLS: 
# ### 
# #     Coefficients
# ols_bind_coeff <- cbind(as.numeric(ols_full$coefficients),
#                         as.numeric(ols_mar$coefficients),
#                         as.numeric(ols_mnar$coefficients),
#                         as.numeric(ols_mcar$coefficients))
# colnames(ols_bind_coeff) <- c("full", "mar", "mnar", "mcar")
# 
# #     P-value? Not sure if this is necessary? for coefficients or model?
# 
# #     Model fit
# ols_mod_f <- as.data.frame(c(ols_full$aic,
#                                ols_mar$aic,
#                                ols_mnar$aic,
#                                ols_mcar$aic))
# colnames(ols_mod_f) <- c("full", "mar", "mnar", "mcar")
# 
# 
# ###
# ### Prediction
# ###
# # Once again, I use the imputed data that is created above.
# # I compare the imputed simulated missing data to its original value.
# 
# 
# # I report the MAPE and MSE for the data. 
# act <- anes$vote.dem
# pred_mar <- out.mar$vote.dem
# pred_mnar <- out.mnar$vote.dem
# pred_mcar <- out.mcar$vote.dem
# 
# #mse
# mse_mar <- mean((act-pred_mar)^2)
# mse_mnar <- mean((act-pred_mnar)^2)
# mse_mcar <- mean((act-pred_mcar)^2)
# 
# #mape
# mape_mar <- mean(abs((act-pred_mar)/act))
# mape_mnar <- mean(abs((act-pred_mnar)/act))
# mape_mcar <- mean(abs((act-pred_mcar)/act))



###
### Code for just categorical
###

#svm_Linear <- train(DJT.FT ~., data = training, method = "svmLinear",
#                    trControl=trctrl,
#                    preProcess = c("center", "scale"),
#                    tuneLength = 10)

# #to store the full list of models for categorical
# modlist <- list()
# 
# #for(j in 1:n_folds){
#   #when we want to do cross-validation
#   #ind_train <- unlist(flds[-10])
#   #training <- amp.mar[ind_train,]
#   for(i in 1:length(categ.vars)){
#     print(i)
#     #i <- 5
#     
#     #take out missingness in training data (response and categorical variables)
#     comp_train <- training[complete.cases(training),]
#     
#     #train svm on one categorical variable
#     #i <- 2
#     form <- paste(colnames(comp_train)[categ.vars[i]], "~ .")
#     form <- as.formula(form)
#     
#     svm_Linear <- train(form, data = comp_train, method = "svmLinear",
#                         trControl=trctrl,
#                         preProcess = c("center", "scale"),
#                         tuneLength = 10)
#     
#     #add svm model to the modlist 
#     modlist[[i]] <- svm_Linear
#   }
# #}


# Useful Links:
# BYU Code: https://scholarsarchive.byu.edu/cgi/viewcontent.cgi?article=4214&context=etd
# SVM's using careg: http://dataaspirant.com/2017/01/19/support-vector-machine-classifier-implementation-r-caret-package/