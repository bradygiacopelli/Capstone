library(tidyverse)
library(dplyr)
library(pwr)
library(doParallel)
library(caret)
library(randomForest)
library(ranger)
library(data.table)
library(ggplot2)

set.seed(69420) 

# Read and initially clean the data
data <- data.table::fread("~/Documents/bradygiacopelli/csvs/2022SECdata.csv") %>%
  filter(!TaggedPitchType %in% c("Undefined", "Other", "Knuckleball")) %>%
  mutate(
    Fastball = as.integer(TaggedPitchType %in% c("Sinker", "Fastball", "Cutter")),
    Offspeed = as.integer(!TaggedPitchType %in% c("Sinker", "Fastball", "Cutter")),
    Whiffable = as.integer(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall")),
    Whiff = as.integer(PitchCall == "StrikeSwinging"),
    PitchBatSide = as.integer(PitcherThrows == BatterSide)
  ) %>%
  filter(Whiffable == 1)


# Calculate overall whiff rate
whiff_rate <- mean(data$Whiff)

##### POWER #####

fbWhiffRate <- mean(data$Whiff[data$Fastball == 1])
osWhiffRate <- mean(data$Whiff[data$Offspeed == 1])

effectSize <- ES.h(p1 = fbWhiffRate, p2 = osWhiffRate)
powerAnalysisResult <- pwr.2p.test(h = effectSize, sig.level = 0.05, power = 0.80, alternative = "two.sided")
print(powerAnalysisResult)

# Odds Ratio Calculation
ofb <- fbWhiffRate / (1 - fbWhiffRate)
oos <- osWhiffRate / (1 - osWhiffRate)
OR <- ofb / oos

##### GLM #####

model_glm <- glm(Whiff ~ PitchBatSide + Fastball, data = data, family = binomial)
summary(model_glm)

saveRDS(model_glm, "~/Documents/bradygiacopelli/repositories/Capstone/models/GLM_model.rds")

##### RANDOM FOREST GENERATION ######

# Selecting relevant features and the target variable
data_rf <- data %>%
  select(PitchBatSide, TaggedPitchType, RelSpeed, SpinRate, InducedVertBreak, HorzBreak, Whiff)


# Convert categorical variables to factors
data_rf$PitchBatSide <- as.factor(data_rf$PitchBatSide)
data_rf$TaggedPitchType <- as.factor(data_rf$TaggedPitchType)
data_rf$HorzBreak = abs(data_rf$HorzBreak)
# Handling missing values (simple example by omitting them)
data_rf <- na.omit(data_rf)


training_indices <- sample(1:nrow(data_rf), 0.5 * nrow(data_rf))
train_data <- data_rf[training_indices, ]
test_data <- data_rf[-training_indices, ]

train_data$Whiff <- factor(train_data$Whiff, levels = c("0", "1"), labels = c("NonWhiff", "Whiff"))

test_data$Whiff <- factor(test_data$Whiff, levels = c("0", "1"), labels = c("NonWhiff", "Whiff"))

train_control <- trainControl(method="cv", number=3, savePredictions = "final", classProbs = TRUE)

# Defining a tuning grid that includes mtry, splitrule, and min.node.size for the ranger package
tune_grid <- expand.grid(
  mtry = c(2,3),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

rf_model <- train(
  Whiff ~ ., 
  data = train_data,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = 'impurity'
)

saveRDS(rf_model, "~/Documents/bradygiacopelli/repositories/Capstone/RF_model.rds")
