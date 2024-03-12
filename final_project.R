library(tidyverse)
library(dplyr)

##### Initial Data Manipulation #####
data = read.csv("~/Documents/bradygiacopelli/csvs/2022SECdata.csv")

cleandata = data %>%
  filter(!TaggedPitchType %in% c("Undefined", "Other", "Knuckleball"))

cleandata = cleandata %>%
  mutate(Fastball = ifelse(TaggedPitchType %in% c("Sinker", "Fastball", "Cutter"), 1, 0),
         Offspeed = ifelse(!TaggedPitchType %in% c("Sinker", "Fastball", "Cutter"), 1, 0))

cleandata = cleandata %>%
  mutate(Whiffable = ifelse(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall"), 1, 0))

cleandata = cleandata %>%
  filter(Whiffable == 1)

cleandata = cleandata %>% 
  mutate(Whiff = ifelse(PitchCall == "StrikeSwinging", 1, 0))

whiff = sum(cleandata$Whiff)/nrow(cleandata)


##### POWER ANALYSIS #####

library(pwr)

fbWhiff = ifelse(cleandata$Fastball == 1 & cleandata$Whiff == 1, 1, 0)
osWhiff = ifelse(cleandata$Offspeed == 1 & cleandata$Whiff == 1, 1, 0)

fbWhiffRate = sum(fbWhiff) / sum(cleandata$Fastball)
osWhiffRate = sum(osWhiff) / sum(cleandata$Offspeed)

# Calculate effect size using the proportions
effectSize = ES.h(p1 = fbWhiffRate, p2 = osWhiffRate)

# Perform power analysis to find required sample size for given power and significance level
powerAnalysisResult = pwr.2p.test(h = effectSize, sig.level = 0.05, power = 0.80, alternative = "two.sided")

print(powerAnalysisResult)

ofb = fbWhiffRate/(1-fbWhiffRate)
oos = osWhiffRate/(1-osWhiffRate)

OR = ofb/oos

##### Generalized Linear Model #####

#PURPOSE The purpose of the assignment is to apply generalized linear modeling using your own final project data set and practice scientific report writing. 
# This Model uses Whiff rate as the response. Pitch type, batter handedness, and pitcher handedness being the predictors
# All 4 of these variables are all binary. 

cleandata$Whiff
cleandata$PitchSide
cleandata$BatSide
cleandata$Fastball

# Fitting a GLM with logistic regression
model_glm = glm(Whiff ~ PitchSide + BatSide + Fastball, data = cleandata, family = binomial)

# Viewing the summary of the model
summary(model_glm)

# Now we want to create 4 different models, one for each of the 4 possible combinations of batter and pitcher handedness.
# PitchSide (1 = Right, 0 = Left) BatSide (1 = Right, 0 = Left) Fastball (1 = Fastball, 0 = Offspeed)

# Right handed batter, right handed pitcher
cleandata_rr = cleandata %>%
  filter(PitchSide == 1 & BatSide == 1)

model_rr = glm(Whiff ~ Fastball, data = cleandata_rr, family = binomial)

summary(model_rr)
# Right handed batter, left handed pitcher

cleandata_rl = cleandata %>%
  filter(PitchSide == 0 & BatSide == 1)

model_rl = glm(Whiff ~ Fastball, data = cleandata_rl, family = binomial)

summary(model_rl)

# Left handed batter, right handed pitcher

cleandata_lr = cleandata %>%
  filter(PitchSide == 1 & BatSide == 0)

model_lr = glm(Whiff ~ Fastball, data = cleandata_lr, family = binomial)

summary(model_lr)

# Left handed batter, left handed pitcher

cleandata_ll = cleandata %>%
  filter(PitchSide == 0 & BatSide == 0)

model_ll = glm(Whiff ~ Fastball, data = cleandata_ll, family = binomial)

summary(model_ll)


