rm(list=ls())
setwd("C:/Users/Steve/Dropbox/PSU2018-2019/Fall2018/SODA502/MLMI")
#setwd("C:/Users/sum410/Dropbox/PSU2018-2019/Fall2018/SODA502/MLMI")


library(MASS)
library(ggplot2)
library(e1071)
library(Amelia)
library(stargazer)

load('cleanedANES.RData')

# Run MI
anes.out <- amelia(anes, 1)
anes.out <- anes.out[[1]]$imp1

anes.out$vote.dem <- ifelse(anes.out$vote.dem < 0.5, 0, 1)

# Model vote choice w/o feeling thermometers (reduce collinearity w/ ideology)
vote.dem.noFT1 <- glm(vote.dem ~ dem + gop + ideo + bible + inform + edu + 
                       income + female + black + hisp, data = anes.out, 
                     family = 'binomial')
summary(vote.dem.noFT1)
stargazer(vote.dem.noFT1, single.row = FALSE, 
          covariate.labels = c('Democrat', 'Republican', 'Ideology', 
                               'Bible Inerrant', 'Pol. Informed', 'Education',
                               'Income', 'Female', 'Black', 'Hispanic'))


# Model feeling thermometer for Trump (0-100 treated as continuous)
trump.FT <- lm(DJT.FT ~ dem + gop + ideo + bible + inform + edu + edu + income +
                 female + black + hisp, data = anes.out)
summary(trump.FT)
stargazer(trump.FT, single.row = FALSE, 
          covariate.labels = c('Democrat', 'Republican', 'Ideology', 'Bible Inerrant', 'Pol. Informed', 'Education',
                               'Income', 'Female', 'Black', 'Hispanic'))

#save(anes, file = 'cleanedANES.RData')