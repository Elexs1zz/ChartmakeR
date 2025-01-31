library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("RChart - Graph Maker"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("chartType", "Select Chart Type:", 
                  choices = c("Line Chart", "Bar Chart", "Pie Chart")),
      
      numericInput("numPoints", "Number of Data Points:", value = 5, min = 1, max = 20),
      
      uiOutput("dataInput"),
      
      actionButton("submit", "Show Chart")
    ),
    
    mainPanel(
      plotOutput("chart")
    )
  )
)

server <- function(input, output, session) {
  
  output$dataInput <- renderUI({
    num_points <- input$numPoints
    lapply(1:num_points, function(i) {
      numericInput(paste("value", i, sep = ""), paste("Data Point", i), value = 10, min = 1, max = 100)
    })
  })
  
  observeEvent(input$submit, {
    data <- sapply(1:input$numPoints, function(i) input[[paste("value", i, sep = "")]])
    
    output$chart <- renderPlot({
      chart_type <- input$chartType
      if (chart_type == "Line Chart") {
        df <- data.frame(x = 1:length(data), y = data)
        ggplot(df, aes(x = x, y = y)) +
          geom_line() +
          geom_point() +
          labs(title = "Line Chart", x = "X", y = "Y")
      } else if (chart_type == "Bar Chart") {
        df <- data.frame(x = factor(1:length(data)), y = data)
        ggplot(df, aes(x = x, y = y)) +
          geom_bar(stat = "identity") +
          labs(title = "Bar Chart", x = "X", y = "Y")
      } else if (chart_type == "Pie Chart") {
        df <- data.frame(x = factor(1:length(data)), y = data)
        ggplot(df, aes(x = "", y = y, fill = x)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar(theta = "y") +
          labs(title = "Pie Chart", x = NULL, y = NULL)
      }
    })
  })
}

shinyApp(ui = ui, server = server)
