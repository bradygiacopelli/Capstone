library(dplyr)

data = read.csv("2022SECdata.csv")

# Remove any Knuckleballs, or pitches that did not properly get tagged.
cleaned = data %>%
  filter(!TaggedPitchType %in% c("", "Other", "Undefined", "Knuckleball"))

# Calculate max velocity for each pitcher
pitcher_max_velo <- cleaned %>%
  group_by(PitcherId) %>%
  summarise(MaxVelocity = max(RelSpeed, na.rm = TRUE))

# Join max velocity back to the original dataset and calculate the percentage
cleaned <- cleaned %>%
  left_join(pitcher_max_velo, by = "PitcherId") %>%
  mutate(VeloPctOfMax = RelSpeed / MaxVelocity * 100)

cleaned <- cleaned %>%
  mutate(
    # Invert HorzBreak for left-handed pitchers
    HorzBreakAdj = ifelse(PitcherThrows == "Left", HorzBreak * -1, HorzBreak),
    # Adjust SpinAxis for left-handed pitchers
    SpinAxisAdj = ifelse(
      PitcherThrows == "Left", 
      360 - as.numeric(SpinAxis),  # Reflect across 180 degrees for lefties
      as.numeric(SpinAxis)
    )
  )

# Assuming you want to include these variables in the clustering
clustering_data <- cleaned %>%
  select(VeloPctOfMax, SpinAxisAdj, HorzBreakAdj, SpinRate, InducedVertBreak) %>%
  na.omit()  # Remove rows with NA to avoid issues during clustering

# Scale the data
clustering_data_scaled <- scale(clustering_data)

set.seed(69420)  # For reproducibility

wss <- sapply(1:10, function(k){
  kmeans(clustering_data_scaled, centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Total Within Clusters Sum of Squares")
