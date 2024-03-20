# Load necessary libraries
library(tidyverse)
library(dplyr)
library(pwr)
library(doParallel)
library(caret)
library(randomForest)
library(ranger)

set.seed(69420) 

# Read and initially clean the data
data <- read.csv("~/Documents/bradygiacopelli/csvs/2022SECdata.csv") %>%
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

# Power Analysis
fbWhiffRate <- mean(data$Whiff[data$Fastball == 1])
osWhiffRate <- mean(data$Whiff[data$Offspeed == 1])

effectSize <- ES.h(p1 = fbWhiffRate, p2 = osWhiffRate)
powerAnalysisResult <- pwr.2p.test(h = effectSize, sig.level = 0.05, power = 0.80, alternative = "two.sided")
print(powerAnalysisResult)

# Odds Ratio Calculation
ofb <- fbWhiffRate / (1 - fbWhiffRate)
oos <- osWhiffRate / (1 - osWhiffRate)
OR <- ofb / oos

# Generalized Linear Model (GLM) for Whiff Prediction
model_glm <- glm(Whiff ~ PitchBatSide + Fastball, data = data, family = binomial)
summary(model_glm)

# GLMs for each combination of batter and pitcher handedness
combinations <- expand.grid(PitchSide = c(0, 1), BatSide = c(0, 1))
models <- lapply(1:nrow(combinations), function(i) {
  subset <- combinations[i, ]
  data_sub <- data %>%
    filter(PitchSide == subset$PitchSide & BatSide == subset$BatSide)
  
  model <- glm(Whiff ~ Fastball, data = data_sub, family = binomial)
  summary(model)
  return(list(subset = subset, model_summary = summary(model)))
})

##### RANDOM FOREST GENERATION ######

# Selecting relevant features and the target variable
data_rf <- data %>%
  select(PitchBatSide, TaggedPitchType, RelSpeed, SpinRate, SpinAxis, InducedVertBreak, HorzBreak, Whiff)


# Convert categorical variables to factors
data_rf$PitchBatSide <- as.factor(data_rf$PitchBatSide)
data_rf$TaggedPitchType <- as.factor(data_rf$TaggedPitchType)
data_rf$SpinAxis <- as.numeric(data_rf$SpinAxis)

# Handling missing values (simple example by omitting them)
data_rf <- na.omit(data_rf)


training_indices <- sample(1:nrow(data_rf), 0.05 * nrow(data_rf))
train_data <- data_rf[training_indices, ]
test_data <- data_rf[-training_indices, ]

train_data$Whiff <- factor(train_data$Whiff, levels = c("0", "1"), labels = c("NonWhiff", "Whiff"))

# Make sure to apply the same transformation to your test dataset if you're going to use it later
test_data$Whiff <- factor(test_data$Whiff, levels = c("0", "1"), labels = c("NonWhiff", "Whiff"))


train_control <- trainControl(method="cv", number=3, savePredictions = "final", classProbs = TRUE)

# Defining a tuning grid that includes mtry, splitrule, and min.node.size for the ranger package
tune_grid <- expand.grid(
  mtry = round(sqrt(ncol(train_data)-1)),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 3, 5)
)

simple_rf_model <- train(
  Whiff ~ ., 
  data = train_data,
  method = "ranger",
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = 'impurity'
)

print(simple_rf_model$bestTune)

# Directly print the accuracy of the best model
best_accuracy <- max(simple_rf_model$results$Accuracy)
print(paste("Best Accuracy:", best_accuracy))

# Creating a new data frame for the adjusted prediction
new_pitch <- data.frame(
  PitchBatSide = as.factor(1), # Example where pitcher and batter handedness are the same
  TaggedPitchType = "Fastball",
  RelSpeed = 90,
  SpinRate = 2000,
  SpinAxis = 200,
  InducedVertBreak = 17,
  HorzBreak = 6
)

# Making the prediction with the adjusted model
predictions <- predict(simple_rf_model, new_pitch, type="prob")

# Extract probabilities of the class of interest, assuming it's the second column
prob_A_wins <- predictions[,2]

print(prob_A_wins)


