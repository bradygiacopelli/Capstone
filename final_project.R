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

sum(cleandata$Whiff)/nrow(cleandata)


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
