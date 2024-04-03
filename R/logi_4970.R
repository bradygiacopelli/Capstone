library(stats)
library(broom)

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

data_log_reg <- data %>%
  select(PitchBatSide, Fastball, RelSpeed, SpinRate, InducedVertBreak, HorzBreak, Whiff)

data_log_reg$HorzBreak = abs(data_log_reg$HorzBreak)

logistic_model <- glm(Whiff ~ PitchBatSide + Fastball + RelSpeed + SpinRate + InducedVertBreak + HorzBreak, data = data_log_reg, family = binomial)


tidy_logistic_model <- tidy(logistic_model)
print(tidy_logistic_model)

new_data <- data.frame(
  PitchBatSide = c(1, 0, 1, 0), # 1 for same side, 0 for different
  Fastball = c(1, 0, 0, 1), # 1 for fastball, 0 for offspeed
  RelSpeed = c(95, 88, 92, 90), # Relative speed in mph
  SpinRate = c(2200, 1800, 2300, 2100), # Spin rate in rpm
  InducedVertBreak = c(16, 12, 14, 10), # Induced vertical break
  HorzBreak = c(6, 8, 5, 7) # Horizontal break
)

# Predict the probability of a whiff for each pitch
predictions <- predict(logistic_model, newdata = new_data, type = "response")

# Adding predictions to the new_data dataframe for easier interpretation
new_data$WhiffProbability = predictions

# Print the updated dataframe with predictions
print(new_data)

