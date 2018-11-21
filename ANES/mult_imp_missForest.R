## MIML_missForest
## Lulu Peng
## Created: 11/15/18
## Last Updated: 11/15/18

# This script applies multiplie imputation w/ missForest to ANES cleaned data

rm(list=ls())
setwd("~/GitHub/MIML/ANES")
if (!require("missForest")) install.packages("missForest")
if (!require("stargazer")) install.packages("stargazer")

load("cleanedANES.RData")
load("anesMissing.RData")
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

summary(anes)
summary(amp.mar)
summary(amp.mnar)
summary(amp.mcar)

# Listwise deletion for "natural" missingness in anes
dim(anes)
anes.complete <- anes[complete.cases(anes),]
dim(anes.complete) #deletes 1889 rows

#### Imputation with missForest ####
# impute data w/ natural missingness
ANES.mF <- missForest(anes, maxiter = 30) # gives OOBerror (NRMSE for continuous variables & proportion of falsely clarified entries for categorical variables) 

# impute simulated data; compare against the complete dataset
out.mar <- missForest(amp.mar, xtrue = anes.complete)
out.mnar <- missForest(amp.mnar, xtrue = anes.complete)
out.mcar <- missForest(amp.mcar, xtrue = anes.complete)

ANES.mF$ximp[cols2] <- lapply(ANES.mF$ximp[cols2], as.numeric)
out.mar$ximp[cols2] <- lapply(out.mar$ximp[cols2], as.numeric)
out.mnar$ximp[cols2] <- lapply(out.mnar$ximp[cols2], as.numeric)
out.mcar$ximp[cols2] <- lapply(out.mcar$ximp[cols2], as.numeric)

#### Regression ####

# Logit:
## Model vote choice w/ feeling thermometers ##
# logit_form <- as.formula(vote.dem ~ dem + gop + ideo + HRC.FT + DJT.FT + bible + inform + edu + income + female + black + hisp)
# logit_raw <- glm(logit_form, data = ANES.mF$ximp, family=binomial(link='logit'))
# logit_full <- glm(logit_form, data = anes.complete, family=binomial(link='logit'))
# logit_mar <- glm(logit_form, data = out.mar$ximp, family=binomial(link='logit'))
# logit_mnar <- glm(logit_form, data = out.mnar$ximp, family=binomial(link='logit'))
# logit_mcar <- glm(logit_form, data = out.mcar$ximp, family=binomial(link='logit'))

## Model vote choice w/o feeling thermometers (reduce collinearity w/ ideology) ##
# vote.dem.noFT <- glm(vote.dem ~ dem + gop + ideo + bible + inform + edu + 
#                          income + female + black + hisp, data = ANES.mF$ximp, family = 'binomial')
# summary(vote.dem.noFT)
logit_form <- as.formula(vote.dem ~ dem + gop + ideo + bible + inform + edu + 
                             income + female + black + hisp)
logit_raw <- glm(logit_form, data = ANES.mF$ximp, family=binomial(link='logit'))
logit_full <- glm(logit_form, data = anes.complete, family=binomial(link='logit'))
logit_mar <- glm(logit_form, data = out.mar$ximp, family=binomial(link='logit'))
logit_mnar <- glm(logit_form, data = out.mnar$ximp, family=binomial(link='logit'))
logit_mcar <- glm(logit_form, data = out.mcar$ximp, family=binomial(link='logit'))

# OLS:
# Model feeling thermometer for Trump (0-100 treated as continuous)
ols_form <- as.formula(DJT.FT ~ dem + gop + ideo + bible + inform + edu + edu + income + female + black + hisp)
ols_raw <- lm(ols_form, data = ANES.mF$ximp)
ols_full <- lm(ols_form, data = anes.complete)
ols_mar <- lm(ols_form, data = out.mar$ximp)
ols_mnar <- lm(ols_form, data = out.mnar$ximp)
ols_mcar <- lm(ols_form, data = out.mcar$ximp)

## Logit: 
stargazer(logit_raw, logit_full, logit_mar, logit_mnar, logit_mcar, title="Results", align=TRUE)

# Coefficients
logit_bind_coeff <- cbind(as.numeric(logit_raw$coefficients),
                          as.numeric(logit_full$coefficients),
                          as.numeric(logit_mar$coefficients),
                          as.numeric(logit_mnar$coefficients),
                          as.numeric(logit_mcar$coefficients))
colnames(logit_bind_coeff) <- c("raw", "full", "mar", "mnar", "mcar")

# P-value? 
# coef(summary(logit_raw))[,4]

# Model fit
logit_mod_f <- as.data.frame(t(c(logit_raw$aic,
                                 logit_full$aic,
                                 logit_mar$aic,
                                 logit_mnar$aic,
                                 logit_mcar$aic)))
colnames(logit_mod_f) <- c("raw", "full", "mar", "mnar", "mcar")

## OLS: 
stargazer(ols_raw, ols_full, ols_mar, ols_mnar, ols_mcar, align = TRUE)

# Coefficients

ols_bind_coeff <- cbind(as.numeric(ols_raw$coefficients),
                        as.numeric(ols_full$coefficients),
                        as.numeric(ols_mar$coefficients),
                        as.numeric(ols_mnar$coefficients),
                        as.numeric(ols_mcar$coefficients))
colnames(ols_bind_coeff) <- c("raw", "full", "mar", "mnar", "mcar")

# P-value? 
# coef(summary(ols_raw))[,4]
