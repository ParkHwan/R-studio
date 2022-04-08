rm(list = ls())

purifier <- read.csv("data/purifier.csv")
purifier

library(psych)
library(corrplot)
library(corrgram)
library(dplyr)

purifier <- purifier %>%
  mutate(new_purifier = purifier-old_purifier)

purifier <- purifier %>%
  mutate(engineer = as_time/160)

purifier

purifierEngineer.lm <- lm (engineer ~ new_purifier + old_purifier, data = purifier)
summary(purifierEngineer.lm)
predict.lm(purifierEngineer.lm, newdata = data.frame(new_purifier=230000, old_purifier= 70000))

par(mfrow=c(2,2)) # 2 x 2 pictures on one plot
plot(purifierEngineer.lm)
#약 234명

purifierTime.lm <- lm (as_time ~ new_purifier + old_purifier, data = purifier)
summary(purifierTime.lm)
predict.lm(purifierTime.lm, newdata = data.frame(new_purifier=230000, old_purifier= 70000))
#약 37404시간
par(mfrow=c(2,2)) # 2 x 2 pictures on one plot
plot(purifierTime.lm)
