library(shiny)
library(openai)

# Load API key
readRenviron(".env")
Sys.setenv(OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY"))

ui <- fluidPage(
  titlePanel("OpenAI API Test"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("prompt", "Enter a prompt:", "What are Differentially Expressed Genes?"),
      actionButton("submit", "Get Response")
    ),
    
    mainPanel(
      verbatimTextOutput("response")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$submit, {
    withProgress(message = 'Getting response...', {
      tryCatch({
        response <- openai$ChatCompletion$create(
          model = "gpt-3.5-turbo",
          messages = list(
            list(
              "role" = "user",
              "content" = input$prompt
            )
          )
        )
        
        output$response <- renderText({
          response$choices[[1]]$message$content
        })
      }, error = function(e) {
        output$response <- renderText({
          paste("Error:", e$message)
        })
      })
    })
  })
}

# Run the test app
shinyApp(ui = ui, server = server) 