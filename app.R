# Load necessary libraries
library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Scientific/Graphing Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter an Equation"),
      textInput("equation", "Equation:", value = "x^2"),
      numericInput("xmin", "X Min:", -10),
      numericInput("xmax", "X Max:", 10),
      numericInput("npoints", "Number of Points:", 100),
      actionButton("calculate", "Calculate"),
      br(),
      h4("Graph Options"),
      checkboxInput("showGrid", "Show Grid", value = TRUE),
      checkboxInput("showPoints", "Show Points", value = TRUE),
      selectInput("lineType", "Line Type", choices = c("solid", "dashed", "dotted"), selected = "solid"),
      sliderInput("lineSize", "Line Size", min = 1, max = 5, value = 2),
      selectInput("lineColor", "Line Color", choices = c("red", "blue", "green", "purple")),
      selectInput("pointShape", "Point Shape", choices = c("circle", "square", "triangle", "diamond"), selected = "circle"),
      numericInput("pointSize", "Point Size", min = 1, max = 5, value = 2),
      selectInput("pointColor", "Point Color", choices = c("red", "blue", "green", "purple"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Calculator", tableOutput("calculation")),
        tabPanel("Graph", plotOutput("graph"))
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Reactive function to perform calculations
  calculate <- reactive({
    x <- seq(input$xmin, input$xmax, length.out = input$npoints)
    y <- eval(parse(text = input$equation))
    
    # Additional functions
    if (!is.null(input$additionalFunctions)) {
      for (func in input$additionalFunctions) {
        y <- y + eval(parse(text = func))
      }
    }
    
    data.frame(x = x, y = y)
  })
  
  # Output calculation results
  output$calculation <- renderTable({
    calculate()
  })
  
  # Reactive function to plot the graph
  plotGraph <- reactive({
    ggplot(calculate(), aes(x, y)) +
      geom_line(linetype = input$lineType, size = input$lineSize, color = input$lineColor) +
      geom_point(shape = input$pointShape, size = input$pointSize, color = input$pointColor, alpha = 0.5 * input$showPoints) +
      theme_minimal() +
      theme(panel.grid = element_blank()) +
      coord_cartesian(clip = "off") +
      labs(title = "Graph of the Equation",
           x = "X",
           y = "Y")
  })
  
  # Output the graph
  output$graph <- renderPlot({
    print(plotGraph())
  })
  
  # Reset button action
  observeEvent(input$reset, {
    updateTextInput(session, "equation", value = "x^2")
    updateNumericInput(session, "xmin", value = -10)
    updateNumericInput(session, "xmax", value = 10)
    updateNumericInput(session, "npoints", value = 100)
    updateCheckboxInput(session, "showGrid", value = TRUE)
    updateCheckboxInput(session, "showPoints", value = TRUE)
    updateSelectInput(session, "lineType", selected = "solid")
    updateSliderInput(session, "lineSize", value = 2)
    updateSelectInput(session, "lineColor", selected = "red")
    updateSelectInput(session, "pointShape", selected = "circle")
    updateNumericInput(session, "pointSize", value = 2)
    updateSelectInput(session, "pointColor", selected = "red")
    updateSelectInput(session, "functionType", selected = NULL)
    updateTextAreaInput(session, "additionalFunctions", value = "")
  })
  
  # Add function button action
  observeEvent(input$addFunction, {
    selectedFunction <- switch(input$functionType,
                               "sin(x)" = "sin(x)",
                               "cos(x)" = "cos(x)",
                               "tan(x)" = "tan(x)",
                               "sqrt(x)" = "sqrt(x)",
                               "log(x)" = "log(x)",
                               "exp(x)" = "exp(x)",
                               "")
    
    if (nchar(selectedFunction) > 0) {
      currentFunctions <- isolate(input$additionalFunctions)
      newFunctions <- paste(currentFunctions, selectedFunction, sep = "\n")
      updateTextAreaInput(session, "additionalFunctions", value = newFunctions)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
