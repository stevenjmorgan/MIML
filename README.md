# MIML

Project evaluating the effects of machine learning multiple imputation methods compared to traditional multiple imputation methods for dealing with missing observations in survey data.

October 18
1) Upload all of ANES 2016 data (Steve)
2) Upload subset of ANES 2016 with only variables of interest(DV and IVs) (Steve)
3) Randomly delete 10% of subset data (steve)
4) Upload subset of data with missing values (Steve)
5) Explore Missing Imputation with traditional techniques and machine learning:

    MICE (Xiaoran)
    KNN (So Young)
    Amelia (Steve)
    SVR (Shipi)
    missForest(Lulu)
    k-means ?
    SVM (Claire)

6) Write literature side by side on each of the above topics  
    
November 13

Protocol

1) ANES:
* impute for the cleanedANES.RData, which only has the natural missingness
* run the last two regression models according to the script of ANES_clean.R
* save the model output in the "ANES/model output" folder so that we can pull all the models together and use stargazer to generate a whole table (naming the saved model "imputation name_model name.RData", e.g., "mice_Trump.FT.RData")

2) Simulated data:
* create the complete dataset using the first 24 lines of code in monte_carlo_exp.R (dataframe name is 'data')
* the simulated missing data is mcar1_singledf.RData
* impute for the missing dataset and compare the imputed dataset to the complete dataset: use RMSE, i.e., Root Mean Square Error
