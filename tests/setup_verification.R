verify_setup <- function() {
  # List of required packages
  required_packages <- c(
    "shiny", "bs4Dash", "DT", "plotly", "ggplot2",
    "reactable", "httr", "jsonlite", "base64enc",
    "openai", "visNetwork", "DESeq2", "edgeR",
    "limma", "markdown", "rmarkdown"
  )
  
  # Check directory structure
  check_directory_structure <- function() {
    required_dirs <- c(
      "R",
      "R/modules",
      "R/helpers",
      "www",
      "www/css",
      "www/images",
      "data",
      "data/example_data"
    )
    
    missing_dirs <- required_dirs[!dir.exists(required_dirs)]
    
    if (length(missing_dirs) > 0) {
      # Create missing directories
      sapply(missing_dirs, dir.create, recursive = TRUE)
      cat("Created missing directories:", paste(missing_dirs, collapse = ", "), "\n")
    } else {
      cat("✓ Directory structure is correct\n")
    }
  }
  
  # Check required files
  check_required_files <- function() {
    required_files <- c(
      "app.R",
      "global.R",
      "config.R",
      ".env",
      "R/modules/eda_ui.R",
      "R/modules/dea_ui.R",
      "R/modules/pathway_ui.R",
      "R/modules/ai_interpretation_ui.R",
      "R/modules/eda_server.R",
      "R/modules/dea_server.R",
      "R/modules/pathway_server.R",
      "R/modules/ai_interpretation_server.R",
      "R/helpers/dea_methods.R",
      "www/css/custom.css"
    )
    
    missing_files <- required_files[!file.exists(required_files)]
    
    if (length(missing_files) > 0) {
      cat("Missing files:", paste(missing_files, collapse = ", "), "\n")
      return(FALSE)
    } else {
      cat("✓ All required files present\n")
      return(TRUE)
    }
  }
  
  # Check package installation
  check_packages <- function() {
    missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
    
    if (length(missing_packages) > 0) {
      cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
      install.packages(missing_packages)
    } else {
      cat("✓ All required packages installed\n")
    }
  }
  
  # Check OpenAI API key
  check_api_key <- function() {
    if (!file.exists(".env")) {
      cat("× .env file missing. Creating template...\n")
      writeLines("OPENAI_API_KEY=your_api_key_here", ".env")
      return(FALSE)
    }
    
    env_contents <- readLines(".env")
    if (!any(grepl("^OPENAI_API_KEY=.+", env_contents))) {
      cat("× OpenAI API key not properly configured in .env file\n")
      return(FALSE)
    }
    
    cat("✓ .env file present\n")
    return(TRUE)
  }
  
  # Run all checks
  cat("Verifying SigmaSEQ setup...\n\n")
  check_directory_structure()
  files_ok <- check_required_files()
  check_packages()
  api_key_ok <- check_api_key()
  
  # Return overall status
  list(
    files_ok = files_ok,
    api_key_ok = api_key_ok
  )
} 