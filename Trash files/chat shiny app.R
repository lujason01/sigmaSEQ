# Required packages
library(shiny)
library(bslib)
library(ggplot2)
library(httr)
library(jsonlite)
library(base64enc)



# Replace with your actual OpenAI API key
openai_api_key <- "your_openai_api_key"

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  card(
    card_header("Graph Insight Assistant"),
    
    # Image
    img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e1/PCA_2D.svg/2560px-PCA_2D.svg.png",
        alt = "PCA plot", style = "width: 100%; height: auto;"),
    
    # Prompt input
    textInput("user_prompt", "Ask me something about this graph:", ""),
    
    # Submit button
    actionButton("ask_btn", "Explain Image", class = "btn-primary"),
    
    br(), br(),
    
    # ChatGPT response
    verbatimTextOutput("chat_response")
  )
)

# Server
server <- function(input, output, session) {
  observeEvent(input$ask_btn, {
    req(input$user_prompt)
    
    # Construct the API request (you must use your actual API key)
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", openai_api_key)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-4",
        messages = list(
          list(role = "system", content = "You are a helpful assistant for explaining plots in R."),
          list(role = "user", content = input$user_prompt)
        ),
        temperature = 0.7
      )
    )
    
    # Extract the result
    res_text <- content(response)$choices[[1]]$message$content
    
    output$chat_response <- renderText({
      res_text
    })
  })
}

shinyApp(ui, server)
