library(shiny)
library(shinythemes)
library(ggplot2)
library(caret)
library(ranger)

# Load your model
# Ensure the path to the model file is correct
# model <- readRDS("~/Documents/bradygiacopelli/repositories/Capstone/models/RF_model.rds")

# Define UI
ui <- fluidPage(
  theme = shinytheme("darkly"),
  tags$head(
    tags$style(HTML("
      .title h2 {
        color: #fff !important;
      }
      .centered-plot {
        margin-left: auto;
        margin-right: auto;
        width: 95%;
        text-align: center;
      }
    "))
  ),
  titlePanel("Predict Whiff Rate for Baseball Pitches"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pitchBatSide", "Pitch/Bat Side", choices = c("Same", "Opposite"), selected = "Same"),
      selectInput("taggedPitchType", "Tagged Pitch Type", 
                  choices = c("Fastball", "Curveball", "Slider", "ChangeUp", 
                              "Cutter", "Sinker", "Splitter"), selected = "Fastball"),
      numericInput("relSpeed", "Release Speed (mph)", step = 5, value = 85),
      numericInput("spinRate", "Spin Rate (rpm)", step = 100, value = 2000),
      numericInput("inducedVertBreak", "Induced Vertical Break (inches)", step = 1, value = 0),
      numericInput("horzBreak", "Horizontal Break (inches)", step = 1, value = 0),
      actionButton("predict", "Predict Whiff Rate", class = "btn-primary"),
    ),
    mainPanel(
      div(plotOutput("strikezone_plot", click = "plot_click", width = "100%", height = "550px"), class = "centered-plot"),
      div(verbatimTextOutput("coordText"), class = "centered-plot"),
      div(textOutput("whiffRate"), class = "centered-plot")
    )
  )
)

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
  
  values <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot_click, {
    values$x <- input$plot_click$x
    values$y <- input$plot_click$y
    
    # Use these x and y coordinates as needed
    output$coordText <- renderText({
      paste("Clicked coordinates - x:", round(values$x, 2), "y:", round(values$y, 2))
    })
  })
  
  # The plot output now uses the stored click coordinates to draw a red circle
  output$strikezone_plot <- renderPlot({
    # Create a blank theme for the plot
    theme_blank <- theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.background = element_rect(fill = "white")
    )
    
    # Start the plot with ggplot
    p <- ggplot() +
      geom_rect(aes(xmin = 0.4, xmax = 0.6, ymin = 0.2, ymax = 0.8), fill = NA, color = "white") +
      theme_blank +
      xlim(-1, 1) + 
      ylim(0, 3)
    
    # If x and y coordinates are set, add a red point to the plot
    if (!is.null(values$x) && !is.null(values$y)) {
      p <- p + geom_point(aes(x = values$x, y = values$y), color = "red", size = 15)
    }
    
    # Return the plot
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)

