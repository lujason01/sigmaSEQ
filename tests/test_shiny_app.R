library(shiny)
library(testthat)

# Mini test app
ui <- fluidPage(
  titlePanel("Test App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data"),
      actionButton("analyze", "Analyze")
    ),
    mainPanel(
      plotlyOutput("plot"),
      verbatimTextOutput("aiInsight")
    )
  )
)

server <- function(input, output, session) {
  # Test reactive chain
  data <- reactive({
    req(input$file)
    # Simulate data loading
    create_test_data()$counts
  })
  
  # Test plot generation
  output$plot <- renderPlotly({
    req(data())
    plot_ly(data = as.data.frame(colSums(data())),
            type = "bar")
  })
  
  # Test AI integration
  output$aiInsight <- renderText({
    req(input$analyze, data())
    "AI Analysis Complete"
  })
}

# Run the test app
if (interactive()) {
  shinyApp(ui, server)
} 