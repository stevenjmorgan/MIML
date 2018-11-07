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


###
### Direct Imputation
###

# First, I load full ANES data.


# Next, I load the "amputed" ANES data (with missingness added - MCAR, MAR, MNAR).


# Then I impute values via train/test split or cross validation (preferably CV) 
# for the three "amputed" datasets. 
# The imputation procedure results in more than one dataset (i.e.  if using 10-fold CV, 
# return the 10 datasets, each missing a "fold")

# I compare the imputed datasets against the original dataset (with no added missingness) 
# to determine how well the algorithm recovers missing values. 





###
### Regression
###

# I will use the imputed data created above for my algorithm. 

# With the imputed dataset, I run two regressions:  logit and OLS.

# Logit:

#OLS:

#First, I report the results: 
#     Coefficients

#     P-value

#     Model fit


###
### Prediction
###
# Once again, I use the imputed data that is created above.

# I compare the imputed simulated missing data to its original value.

# I report the MAPE and MSE for the data. 
MAPE, MSE