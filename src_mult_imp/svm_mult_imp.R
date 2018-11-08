###
### Claire Kelling
### 
### Multiple Imputation Project
###
### Implementation of the Protocol through SVM
### Created: 10/10/18
### Last Updated: 11/7/18
###

# In this code, I will implement the protocol for the SODA 502 project for 
# Multiple Imputation- Machine Learning (MIML).

#clear workspace
rm(list = ls())

library(Amelia)

###
### Direct Imputation
###

# First, I load full ANES data.
load("C:/Users/ckell/Desktop/Google Drive/01_Penn State/2018-2019/Fall 2018/soda_502/project/MIML/ANES/cleanedANES.Rdata")

# Next, I load the "amputed" ANES data (with missingness added - MCAR, MAR, MNAR).
#load("C:/Users/ckell/Desktop/Google Drive/01_Penn State/2018-2019/Fall 2018/soda_502/project/MIML/ANES/anesMCAR.RData")
load("C:/Users/ckell/Desktop/Google Drive/01_Penn State/2018-2019/Fall 2018/soda_502/project/MIML/ANES/anesMissing.RData")

# Exploratory analysis:
par(mfrow=c(2,2))
missmap(anes, main = "Missing values vs observed, Full Data")
missmap(full.na.mar, main = "Missing values vs observed, MAR")
missmap(full.na.mcar, main = "Missing values vs observed, MCAR")
missmap(full.na.mnar, main = "Missing values vs observed, MNAR")


# Then I impute values via train/test split or cross validation (preferably CV) 
# for the three "amputed" datasets. 
# The imputation procedure results in more than one dataset (i.e.  if using 10-fold CV, 
# return the 10 datasets, each missing a "fold")

# PUT SVM CODE HERE

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
logit_mar <- glm(logit_form, data = full.na.mar, family=binomial(link='logit'))
logit_mnar <- glm(logit_form, data = full.na.mnar, family=binomial(link='logit'))
logit_mcar <- glm(logit_form, data = full.na.mcar, family=binomial(link='logit'))

# OLS:
ols_form <- as.formula(.~.)
ols_full <- lm(ols_form, data = anes)
ols_mar <- lm(ols_form, data = full.na.mar)
ols_mnar <- lm(ols_form, data = full.na.mnar)
ols_mcar <- lm(ols_form, data = full.na.mcar)

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
pred_mar <- full.na.mar$vote.dem
pred_mnar <- full.na.mnar$vote.dem
pred_mcar <- full.na.mcar$vote.dem

#mse
mse_mar <- mean((act-pred_mar)^2)
mse_mnar <- mean((act-pred_mnar)^2)
mse_mcar <- mean((act-pred_mcar)^2)

#mape
mape_mar <- mean(abs((act-pred_mar)/act))
mape_mnar <- mean(abs((act-pred_mnar)/act))
mape_mcar <- mean(abs((act-pred_mcar)/act))