#' Helper functions for API interactions

# Load required packages
load_required_packages <- function() {
  required_packages <- c("openai", "jsonlite")
  
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      log_message(paste("Installing package:", pkg))
      install.packages(pkg, repos = "https://cran.rstudio.com/")
      library(pkg, character.only = TRUE)
    }
  }
}

# Set up logging
setup_logging <- function() {
  log_dir <- "logs"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  log_file <- file.path(log_dir, "api_connection.log")
  if (!file.exists(log_file)) {
    file.create(log_file)
  }
  return(log_file)
}

# Log function with timestamp
log_message <- function(message, level = "INFO") {
  # Ensure level is one of the allowed types
  level <- toupper(level)
  if (!level %in% c("INFO", "WARNING", "ERROR")) {
    level <- "INFO"  # Default to INFO if invalid level
  }
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste(timestamp, level, message, sep = " - ")
  
  # Write to log file
  write(log_entry, file = log_file, append = TRUE)
  
  # Also print to console with appropriate type
  switch(level,
    "ERROR" = message(paste("[ERROR]", message), domain = NA),
    "WARNING" = warning(paste("[WARNING]", message), call. = FALSE),
    message(log_entry)  # Default INFO case
  )
}

# Initialize logging
log_file <- setup_logging()

# Load required packages
load_required_packages()

# Retry function for API calls
retry_api_call <- function(api_call, max_retries = 3, delay = 5) {
  for (attempt in 1:max_retries) {
    tryCatch({
      log_message(paste("Attempt", attempt, "to connect to API"))
      result <- api_call()
      log_message("API call successful")
      return(result)
    }, error = function(e) {
      log_message(paste("Attempt", attempt, "failed:", e$message), "ERROR")
      
      if (attempt < max_retries) {
        log_message(paste("Retrying in", delay, "seconds..."))
        Sys.sleep(delay)
      } else {
        log_message("All retry attempts failed", "ERROR")
        stop(paste("Failed after", max_retries, "attempts:", e$message))
      }
    })
  }
}

# Initialize OpenAI configuration
initialize_openai <- function() {
  log_message("Initializing OpenAI configuration")
  
  # Ensure package is loaded
  if (!require("openai", character.only = TRUE)) {
    log_message("openai package not found, attempting to install", "ERROR")
    install.packages("openai", repos = "https://cran.rstudio.com/")
    library(openai)
  }
  
  # Ensure environment variables are loaded
  if (file.exists(".env")) {
    readRenviron(".env")
    log_message("Loaded environment variables from .env file")
  }
  
  # Set API key from environment
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    # Try reloading the environment variable
    api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
    if (api_key == "") {
      log_message("No API key found in environment variables", "ERROR")
      stop("OPENAI_API_KEY not found in environment variables. Please check your .env file.")
    }
  }
  
  # Configure openai package
  Sys.setenv(OPENAI_API_KEY = api_key)
  log_message("API key configured")
  
  # Test the connection with retry - using minimal prompt
  tryCatch({
    retry_api_call(function() {
      response <- openai::create_chat_completion(
        model = "gpt-4",
        messages = list(
          list(
            role = "user",
            content = "."
          )
        ),
        max_tokens = 1
      )
      log_message("API connection test successful")
      return(TRUE)
    })
  }, error = function(e) {
    log_message(paste("API connection test failed:", e$message), "ERROR")
    return(FALSE)
  })
}

# Validate API key
has_valid_api_key <- function() {
  log_message("Validating API key")
  
  # Ensure package is loaded
  if (!require("openai", character.only = TRUE)) {
    log_message("openai package not found", "ERROR")
    return(FALSE)
  }
  
  # Ensure environment variables are loaded
  if (file.exists(".env")) {
    readRenviron(".env")
    log_message("Loaded environment variables from .env file")
  }
  
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    # Try reloading the environment variable
    api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
    if (api_key == "") {
      log_message("No API key found in environment variables", "ERROR")
    return(FALSE)
    }
  }
  
  # Test API connection with retry - using minimal prompt
  tryCatch({
    retry_api_call(function() {
      response <- openai::create_chat_completion(
        model = "gpt-4",
      messages = list(
        list(
            role = "user",
            content = "."
        )
      ),
        max_tokens = 1
    )
      log_message("API key validation successful")
    return(TRUE)
    })
  }, error = function(e) {
    log_message(paste("API key validation failed:", e$message), "ERROR")
    return(FALSE)
  })
}

# Generate AI interpretation
generate_ai_interpretation <- function(context, focus) {
  log_message("Generating AI interpretation")
  
  #' API Response Structure Notes:
  #' Through testing, we discovered several important aspects of the OpenAI API response:
  #' 1. Top-level response is a list with fields: 
  #'    - id, object, created, model, choices, usage, service_tier, system_fingerprint
  #' 2. The choices array can contain different types of responses:
  #'    - Sometimes a list with message/content structure
  #'    - Sometimes direct values (e.g., integer 0)
  #'    - May vary based on API version or call parameters
  #' 3. Response handling strategy:
  #'    - Check for list structure first
  #'    - Handle direct values by converting to character
  #'    - Validate content before processing
  #'    - Use flexible parsing to handle different formats
  
  # Ensure package is loaded
  if (!require("openai", character.only = TRUE)) {
    log_message("openai package not found", "ERROR")
    stop("openai package not found")
  }
  
  # Ensure API is initialized
  if (!initialize_openai()) {
    log_message("Failed to initialize OpenAI API", "ERROR")
    stop("Failed to initialize OpenAI API. Please check your API key and billing status")
  }
  
  # Prepare the prompt based on context and focus
  prompt <- create_analysis_prompt(context, focus)
  log_message("Prompt prepared")
  
  # Make API call with retry
  tryCatch({
    result <- retry_api_call(function() {
      #' API Call Configuration:
      #' - Using GPT-4 model for best analysis quality
      #' - System message sets context for RNA-seq analysis
      #' - Temperature 0.7 balances creativity and accuracy
      #' - Max tokens 1000 allows for detailed responses
      response <- openai::create_chat_completion(
        model = "gpt-4",
    messages = list(
      list(
            role = "system",
            content = "You are an expert bioinformatician specializing in RNA-seq analysis."
      ),
      list(
            role = "user",
            content = prompt
      )
    ),
    temperature = 0.7,
    max_tokens = 1000
  )
  
      # Process response - with detailed logging and validation
      log_message(paste("Response received. Structure:", paste(names(response), collapse=", ")))
      
      #' Response Processing Strategy:
      #' 1. Validate basic response structure
      #' 2. Extract and validate choices array
      #' 3. Handle different choice formats flexibly
      #' 4. Convert content to usable format
      #' 5. Generate interpretation with plot suggestions
      if (!is.null(response)) {
        if (is.list(response)) {
          log_message("Response is a list")
          
          if ("choices" %in% names(response)) {
            log_message("Found 'choices' in response")
            
            if (length(response$choices) > 0) {
              log_message("Choices array is not empty")
              
              choice <- response$choices[[1]]
              log_message(paste("First choice class:", class(choice)[1]))
              log_message(paste("First choice type:", typeof(choice)))
              log_message(paste("First choice raw content:", paste(capture.output(str(choice)), collapse="\n")))
              
              # Handle different types of choice objects
              if (is.list(choice)) {
                log_message("Choice is a list, checking structure...")
                if ("message" %in% names(choice)) {
                  content <- choice$message$content
                  log_message("Using message.content structure")
                } else if ("text" %in% names(choice)) {
                  content <- choice$text
                  log_message("Using text structure")
                } else if ("delta" %in% names(choice) && "content" %in% names(choice$delta)) {
                  content <- choice$delta$content
                  log_message("Using delta.content structure")
                } else {
                  log_message(paste("Unknown list structure. Available fields:", 
                                  paste(names(choice), collapse=", ")), "ERROR")
                  stop("Unexpected choice structure in API response")
                }
              } else if (is.character(choice)) {
                log_message("Choice is a character string, using directly")
                content <- choice
              } else {
                # Try to convert to character if possible
                tryCatch({
                  content <- as.character(choice)
                  log_message("Converted choice to character string")
                }, error = function(e) {
                  log_message(paste("Cannot convert choice to usable format:", e$message), "ERROR")
                  stop("Cannot process API response format")
                })
              }
              
              if (!is.null(content)) {
                interpretation <- list(
                  text = content,
                  plots = extract_plot_suggestions(content)
                )
                
                log_message("Response processed successfully")
                return(interpretation)
              } else {
                log_message("Content is NULL", "ERROR")
              }
            } else {
              log_message("Choices array is empty", "ERROR")
            }
          } else {
            log_message("No 'choices' field in response", "ERROR")
          }
        } else {
          log_message("Response is not a list", "ERROR")
        }
      } else {
        log_message("Response is NULL", "ERROR")
      }
      
      # If we get here, something was wrong with the response structure
      log_message("Unexpected API response structure", "ERROR")
      stop("Unexpected API response structure")
    })
    
    log_message("Interpretation generated successfully")
    return(result)
  }, error = function(e) {
    log_message(paste("Error generating interpretation:", e$message), "ERROR")
    return(NULL)
  })
}

# Helper function to create analysis prompt
create_analysis_prompt <- function(context, focus) {
  log_message("Creating analysis prompt")
  
  # Base prompt
  base_prompt <- switch(context$type,
    "eda" = "Analyze this RNA-seq exploratory data analysis:",
    "dea" = "Interpret these differential expression results:",
    "pathway" = "Explain these pathway analysis findings:"
  )
  
  # Add context details
  prompt <- paste(base_prompt, "\n\nData summary:", jsonlite::toJSON(context$data, auto_unbox = TRUE))
  
  # Add focus areas
  if ("technical" %in% focus) {
    prompt <- paste(prompt, "\n\nPlease include technical details about the analysis.")
  }
  if ("biological" %in% focus) {
    prompt <- paste(prompt, "\n\nPlease include biological interpretations.")
  }
  if ("recommendations" %in% focus) {
    prompt <- paste(prompt, "\n\nPlease suggest next steps or additional analyses.")
  }
  
  log_message("Prompt created successfully")
  prompt
}

# Helper function to extract plot suggestions from AI response
extract_plot_suggestions <- function(response_text) {
  log_message("Extracting plot suggestions")
  
  tryCatch({
  # Parse response for plot suggestions
  # This is a placeholder - implement based on your needs
    suggestions <- list(
    type = "default",
    data = NULL,
    suggestions = NULL
  )
    
    log_message("Plot suggestions extracted successfully")
    return(suggestions)
  }, error = function(e) {
    log_message(paste("Error extracting plot suggestions:", e$message), "ERROR")
    return(list(
      type = "error",
      data = NULL,
      suggestions = NULL
    ))
  })
}

# Update OpenAI API configuration
setup_openai <- function() {
  log_message("Setting up OpenAI configuration")
  
  # Ensure package is loaded
  if (!require("openai", character.only = TRUE)) {
    log_message("openai package not found", "ERROR")
    stop("openai package not found")
  }
  
  openai_config <- get_config("openai")
  Sys.setenv(
    OPENAI_API_KEY = openai_config$api_key,
    OPENAI_MODEL = openai_config$model,
    OPENAI_TIMEOUT = openai_config$timeout
  )
  
  log_message("OpenAI configuration updated")
} 