library(tidyverse)
library(caret)


# Load Data
data = read.csv("~/Documents/STAT_4970W/2022SECdata.csv")

# Clean Data
cleandata = data %>%
  filter(!TaggedPitchType %in% c("Undefined", "Other", "Knuckleball"))

cleandata = cleandata %>%
  mutate(Fastball = ifelse(TaggedPitchType %in% c("Sinker", "Fastball", "Cutter"), 1, 0),
         Offspeed = ifelse(!TaggedPitchType %in% c("Sinker", "Fastball", "Cutter"), 1, 0),
         BatSide = ifelse(BatterSide %in% c("Right"), 1,0),
         PitchSide = ifelse(PitcherThrows %in% c("Right"), 1,0))

cleandata = cleandata %>%
  mutate(Whiffable = ifelse(PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall"), 1, 0))

cleandata = cleandata %>%
  filter(Whiffable == 1)

cleandata = cleandata %>% 
  mutate(Whiff = ifelse(PitchCall == "StrikeSwinging", 1, 0))

whiff = sum(cleandata$Whiff)/nrow(cleandata)

# Build Logistic Classification Model
# Split data into training and testing sets
set.seed(123) # for reproducibility

selected_columns <- c("RelSpeed","VertRelAngle","HorzRelAngle","SpinRate","SpinAxis","RelHeight","RelSide","Extension","VertBreak","InducedVertBreak","HorzBreak","ZoneSpeed","VertApprAngle","HorzApprAngle","ZoneTime","pfxx","pfxz","vx0","vy0","vz0","ax0","ay0","az0","Fastball")

class_data <- subset(cleandata, select = selected_columns)
class_data <- na.omit(class_data)


trainIndex <- createDataPartition(class_data$Fastball, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- class_data[trainIndex, ]
data_test <- class_data[-trainIndex, ]

# Train a logistic regression model
model <- train(Fastball ~ ., data = data_train, method = "glm", trControl = trainControl(method = "cv"))

# Make predictions on the test set
predictions <- predict(model, newdata = data_test)

# Evaluate the model
confusionMatrix(predictions, data_test$label)
