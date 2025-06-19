library(shiny)
library(bs4Dash)
library(here)
source(here("R/helpers/api_utils.R"))

ui <- dashboardPage(
  header = dashboardHeader(title = "API Test"),
  sidebar = dashboardSidebar(),
  body = dashboardBody(
    fluidRow(
      box(
        title = "API Test",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        
        # API Status
        div(
          class = "text-center",
          h4("API Status"),
          textOutput("apiStatus"),
          tags$br(),
          actionButton("checkApi", "Check API Connection"),
          tags$br(),
          tags$br(),
          verbatimTextOutput("apiDetails")
        ),
        
        # Test Interpretation
        div(
          style = "margin-top: 20px;",
          textAreaInput(
            "testPrompt",
            "Test Prompt",
            value = "Please analyze this RNA-seq data: 5 samples show high expression."
          ),
          actionButton("testInterpretation", "Test Interpretation"),
          tags$br(),
          tags$br(),
          verbatimTextOutput("interpretationResult")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Check API status
  output$apiStatus <- renderText({
    if (has_valid_api_key()) {
      "✓ API Connected"
    } else {
      "× API Not Connected"
    }
  })
  
  # Show API details
  output$apiDetails <- renderText({
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (api_key == "") {
      "No API key found in environment variables"
    } else {
      paste("API Key found:", substr(api_key, 1, 10), "...")
    }
  })
  
  # Handle API check button
  observeEvent(input$checkApi, {
    withProgress(message = 'Testing API connection...', {
      tryCatch({
        if (initialize_openai()) {
          showNotification("API connection successful!", type = "success")
        } else {
          showNotification("API connection failed!", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })
  
  # Handle test interpretation
  observeEvent(input$testInterpretation, {
    withProgress(message = 'Testing API...', {
      tryCatch({
        result <- generate_ai_interpretation(
          list(
            type = "test",
            data = list(description = input$testPrompt)
          ),
          c("technical", "biological")
        )
        
        if (is.null(result)) {
          showNotification("Failed to generate interpretation", type = "error")
          return()
        }
        
        output$interpretationResult <- renderText({
          result$text
        })
        
        showNotification("Interpretation generated successfully!", type = "success")
      }, error = function(e) {
        output$interpretationResult <- renderText({
          paste("Error:", e$message)
        })
        showNotification("Error generating interpretation", type = "error")
      })
    })
  })
}

# Run the test app
shinyApp(ui, server)