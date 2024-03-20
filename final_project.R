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
    PitchSide = as.integer(PitcherThrows == "Right"),
    BatSide = as.integer(BatterSide == "Right")
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
model_glm <- glm(Whiff ~ PitchSide + BatSide + Fastball, data = data, family = binomial)
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
  select(PitchSide, BatSide, TaggedPitchType, RelSpeed, SpinRate, SpinAxis, InducedVertBreak, HorzBreak, Whiff)

# Convert categorical variables to factors
data_rf$PitchSide <- as.factor(data_rf$PitchSide)
data_rf$BatSide <- as.factor(data_rf$BatSide)
data_rf$TaggedPitchType <- as.factor(data_rf$TaggedPitchType)
data_rf$Whiff <- factor(data_rf$Whiff, levels = c("0", "1"), labels = c("Non-Whiff", "Whiff"))


# Handling missing values (simple example by omitting them)
data_rf <- na.omit(data_rf)

training_indices <- sample(1:nrow(data_rf), 0.05 * nrow(data_rf))
train_data <- data_rf[training_indices, ]
test_data <- data_rf[-training_indices, ]

simple_rf_model <- ranger(
  formula = Whiff ~ ., 
  data = train_data,
  mtry = round(sqrt(ncol(train_data)-1)),
  min.node.size = 5,
  num.trees = 500,
  importance = 'impurity',
  verbose = TRUE
)

# You can print the model summary to see the results
print(simple_rf_model)

# Checking the importance of variables
importance(simple_rf_model)

# Predicting on the test dataset
predictions <- predict(simple_rf_model, data = test_data)

# Converting probabilities to binary outcomes based on a 0.5 threshold
# This step is necessary if your model is predicting probabilities for each class
predicted_numeric <- as.numeric(as.character(predictions$predictions)) - 1

preds = predictions$predictions

# Actual values, ensuring they are numeric for direct comparison
# If your actual values are factors, you might need to adjust this
actual_numeric <- test_data$Whiff

# Calculate accuracy
accuracy <- mean(preds == actual_numeric)
print(paste("Accuracy:", accuracy))


