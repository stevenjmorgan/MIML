# This code cleans 8 variables of interest from the 2016 ANES Pre-Election Study


rm(list=ls())
setwd("C:/Users/Steve/Desktop/Git_Clones/MIML/ANES")
#setwd("C:/Users/sum410/Desktop/Git_Clones/MIML/ANES")

library(data.table)
library(car)

anes <- fread('C:/Users/Steve/Dropbox/PSU2018-2019/Fall2018/SODA502/MLMI/anes_timeseries_2016_rawdata.txt')

summary(anes$V162034a)
unique(anes$V162034a)

# PID, HRC FT, DJT FT, Bible word of God, Appear informed, education,
# income, female, black

# Convert to categorical 
anes$PID <- recode(anes$V161155, "1 = 'Democrat'; 2 = 'Republican';
                   3 = 'Independent'; 5 = 'Other'; 0 = 'No Party'; else = NA")
# Convert candidate FT values to NA when appropriate (negative values)
anes$HRC.FT <- ifelse(anes$V162078 < 0, NA, anes$V162078)
anes$DJT.FT <- ifelse(anes$V162079 < 0, NA, anes$V162079)

# Convert bible word of God values to NA when appropriate (ordinal)
anes$bible <- ifelse(anes$V161243 < 0, NA, anes$V161243)

# Convert level of information values to NA when appropriate (ordinal)
anes$obs.inform <- ifelse(anes$V168016 < 0, NA, anes$V168016)

# Bin education responses: 1 = Did not graduate high school, 2 = graduated high
# school, 3 = some college, 4 = graduated college, 5 = post-college education
anes$edu <- recode(anes$V161270, "2:8 = 1; 9 = 2; 10:12 = 3; 13 = 4; 14:16 = 5;
                   else = NA")
