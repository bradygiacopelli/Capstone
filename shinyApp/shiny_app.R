library(shiny)
library(caret)
library(ranger)

# Load your model
model <- readRDS("~/Documents/bradygiacopelli/repositories/Capstone/models/RF_model.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Predict Whiff Rate for Baseball Pitches"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pitchBatSide", "Pitch/Bat Side", choices = c("", "Same" = "Same", "Opposite" = "Opposite"), selected = ""),
      selectInput("taggedPitchType", "Tagged Pitch Type", 
                  choices = c("", 
                              "Fastball" = "Fastball",
                              "Curveball" = "Curveball", 
                              "Slider" = "Slider", 
                              "ChangeUp" = "ChangeUp", 
                              "Cutter" = "Cutter", 
                              "Sinker" = "Sinker", 
                              "Splitter" = "Splitter"), 
                  selected = ""),
      numericInput("relSpeed", "Release Speed (mph)", value = NULL),
      numericInput("spinRate", "Spin Rate (rpm)", value = NULL),
      numericInput("inducedVertBreak", "Induced Vertical Break (inches)", value = NULL),
      numericInput("horzBreak", "Horizontal Break (inches)", value = NULL),
      actionButton("predict", "Predict Whiff Rate")
    ),
    mainPanel(
      textOutput("whiffRate")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$predict, {
    # Check if any input is NULL or empty
    if (input$pitchBatSide == "" | input$taggedPitchType == "" | is.null(input$relSpeed) | is.null(input$spinRate) | is.null(input$inducedVertBreak) | is.null(input$horzBreak)) {
      missing_values <- c()
      if (input$pitchBatSide == "") missing_values <- c(missing_values, "Pitch/Bat Side")
      if (input$taggedPitchType == "") missing_values <- c(missing_values, "Tagged Pitch Type")
      if (is.na(input$relSpeed)) missing_values <- c(missing_values, "Release Speed")
      if (is.na(input$spinRate)) missing_values <- c(missing_values, "Spin Rate")
      if (is.na(input$inducedVertBreak)) missing_values <- c(missing_values, "Induced Vertical Break")
      if (is.na(input$horzBreak)) missing_values <- c(missing_values, "Horizontal Break")
      
      output$whiffRate <- renderText({
        paste("Value needed for:", paste(missing_values, collapse = ", "))
      })
    } else {
      # Convert pitchBatSide input to model's expected format
      side <- ifelse(input$pitchBatSide == "Same", 1, 0)
      
      # Prepare new_pitch data frame for prediction
      new_pitch <- data.frame(
        PitchBatSide = factor(side, levels = c("1", "0")),
        TaggedPitchType = factor(input$taggedPitchType, levels = c("ChangeUp", "Curveball", "Cutter", "Fastball", "Sinker", "Slider", "Splitter")),
        RelSpeed = input$relSpeed,
        SpinRate = input$spinRate,
        InducedVertBreak = input$inducedVertBreak,
        HorzBreak = abs(input$horzBreak) # Ensure horizontal break is positive
      )
      
      # Perform prediction
      predictions <- predict(model, new_pitch, type = "prob")
      prob_whiff <- paste0(round(predictions[, "Whiff"], 4) * 100, "%")
      output$whiffRate <- renderText({
        paste("Predicted Whiff Rate:", prob_whiff)
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
