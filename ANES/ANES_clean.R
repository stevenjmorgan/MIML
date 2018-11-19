# This code cleans 14 variables of interest from the 2016 ANES Pre-Election 
# Study and models vote choice and feeling thermometer values.

rm(list=ls())
setwd("C:/Users/Steve/Documents/Github/MIML/ANES")
#setwd("C:/Users/sum410/Documents/Github/MIML/ANES")

library(data.table)
library(car)
library(dplyr)
library(stargazer)

anes <- fread('C:/Users/Steve/Dropbox/PSU2018-2019/Fall2018/SODA502/MLMI/anes_timeseries_2016_rawdata.txt')

# Variables of interest: PID, HRC FT, DJT FT, Bible word of God, 
# appear informed, education, income, female, black, vote choice

# Convert to categorical 
anes$PID <- car::recode(anes$V161155, "1 = 'Democrat'; 2 = 'Republican';
                   3 = 'Independent'; 5 = 'Other'; 0 = 'No Party'; else = NA")

# Split variables into binary
anes$dem <- car::recode(anes$V161155, "1 = 1; 2 = 0; 3 = 0; 5 = 0; 0 = 0; else = NA")
anes$gop <- car::recode(anes$V161155, "2 = 1; 1 = 0; 3 = 0; 5 = 0; 0 = 0; else = NA")

# Remove NA's from ideology item
anes$ideo <- ifelse(anes$V161126 < 0 | anes$V161126 > 7, NA, anes$V161126)

# Convert candidate FT values to NA when appropriate (negative values)
anes$HRC.FT <- ifelse(anes$V162078 < 0 | anes$V162078 > 100, NA, anes$V162078)
anes$DJT.FT <- ifelse(anes$V162079 < 0 | anes$V162079 > 100, NA, anes$V162079)

# Convert bible word of God values to NA when appropriate (ordinal)
anes$bible <- ifelse(anes$V161243 < 0, NA, anes$V161243)

# Convert level of information values to NA when appropriate (ordinal)
anes$inform <- ifelse(anes$V161003 < 0, NA, anes$V161003)

# Bin education responses: 1 = Did not graduate high school, 2 = graduated high
# school, 3 = some college, 4 = graduated college, 5 = post-college education
anes$edu <- car::recode(anes$V161270, "2:8 = 1; 9 = 2; 10:12 = 3; 13 = 4; 14:16 = 5;
                   else = NA")

# Bin income
anes$income <- car::recode(anes$V161361x, "1:10 = 1; 11:14 = 2; 15:20 = 3;
                      21:24 = 4; 25:hi = 4; else = NA")

# Dichotomize sex
anes$female <- car::recode(anes$V161342, "1 = 0; 2 = 1; else = NA")

# Dichotomize African-American
anes$black <- car::recode(anes$V161310x, "2 = 1; -9 = NA; else = 0")

# Dichotomize Hispanic
anes$hisp <- car::recode(anes$V161310x, "5 = 1; -9 = NA; else = 0")

# Dichotomize White
anes$white <- car::recode(anes$V161310x, "1 = 1; -9 = NA; else = 0")

# Intend to vote for president (two-party vote only)
anes$vote.dem <- car::recode(anes$V161031, "1 = 1; 2 = 0; -1 = NA; -8 = NA; -9 = NA;
                        else = 9")

# Remove voters for non-traditional party presidential candidates
anes <- anes[which(anes$vote.dem != 9 | is.na(anes$vote.dem)),]

# Subset ANES to only include variables of interest
anes <- select(anes, vote.dem, dem, gop, ideo, HRC.FT, DJT.FT, bible, 
                inform, edu, income, female, black, hisp, white)

# Model vote choice w/ feeling thermometers
vote.dem.logit <- glm(vote.dem ~ dem + gop + ideo + HRC.FT + DJT.FT + bible + 
                      inform + edu + income + female + black + hisp, 
                      data = anes, family = 'binomial')
summary(vote.dem.logit)
stargazer(vote.dem.logit, single.row = FALSE, 
          covariate.labels = c('Democrat', 'Republican', 'Ideology', 'Clinton FT',
                               'Trump FT', 'Bible Inerrant', 'Pol. Informed', 'Education',
                               'Income', 'Female', 'Black', 'Hispanic'))

# Model vote choice w/o feeling thermometers (reduce collinearity w/ ideology)
vote.dem.noFT <- glm(vote.dem ~ dem + gop + ideo + bible + inform + edu + 
                      income + female + black + hisp, data = anes, 
                      family = 'binomial')
summary(vote.dem.noFT)
stargazer(vote.dem.noFT, single.row = FALSE, 
          covariate.labels = c('Democrat', 'Republican', 'Ideology', 
                               'Bible Inerrant', 'Pol. Informed', 'Education',
                               'Income', 'Female', 'Black', 'Hispanic'))


# Model feeling thermometer for Trump (0-100 treated as continuous)
trump.FT <- lm(DJT.FT ~ dem + gop + ideo + bible + inform + edu + edu + income +
              female + black + hisp, data = anes)
summary(trump.FT)
stargazer(trump.FT, single.row = FALSE, 
          covariate.labels = c('Democrat', 'Republican', 'Ideology', 'Bible Inerrant', 'Pol. Informed', 'Education',
                               'Income', 'Female', 'Black', 'Hispanic'))

save(anes, file = 'cleanedANES.RData')
