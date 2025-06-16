test_openai_connection <- function() {
  # Load required packages
  if (!require("openai", quietly = TRUE)) {
    install.packages("openai", repos = "https://cran.rstudio.com/")
    library(openai)
  }
  
  # Load environment variables
  readRenviron(".env")
  Sys.setenv(OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY"))
  
  cat("Testing OpenAI API connection...\n")
  
  tryCatch({
    # Try a simple completion request
    response <- create_chat_completion(
      model = "gpt-4",
      messages = list(
        list(
          role = "user",
          content = "Hello, this is a test message. Please respond with: 'API connection successful!'"
        )
      )
    )
    
    if (!is.null(response)) {
      cat("\n✓ API connection successful!\n")
      cat("Response received:", response$choices[[1]]$message$content, "\n")
      return(TRUE)
    }
  }, error = function(e) {
    cat("\n× API connection failed!\n")
    cat("Error message:", e$message, "\n")
    return(FALSE)
  })
}

# Run the test
test_openai_connection()