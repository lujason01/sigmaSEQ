#' Configuration Utilities
#' @description Functions to manage application configuration
#' @author Your Name
#' @version 1.0.0

# Create a new environment for configuration
config_env <- new.env()

#' Load configuration
#' @description Load and validate configuration from config.yml
#' @return List containing configuration
load_config <- function() {
  if (!require("yaml")) {
    install.packages("yaml")
    library(yaml)
  }
  
  config_file <- "config.yml"
  
  # Check if config file exists
  if (!file.exists(config_file)) {
    stop("Configuration file config.yml not found")
  }
  
  # Load configuration
  config <- yaml::read_yaml(config_file)
  
  # Validate required fields
  required_fields <- c(
    "app_name",
    "app_version",
    "app_description",
    "theme",
    "max_upload_size",
    "default_parameters",
    "openai",
    "paths"
  )
  
  missing_fields <- required_fields[!required_fields %in% names(config$default)]
  if (length(missing_fields) > 0) {
    stop(sprintf("Missing required configuration fields: %s",
                paste(missing_fields, collapse = ", ")))
  }
  
  # Convert paths to absolute paths using here
  if (!require("here")) {
    install.packages("here")
    library(here)
  }
  
  config$default$paths <- lapply(config$default$paths, function(path) {
    here::here(path)
  })
  
  return(config$default)
}

#' Get configuration value
#' @param key Configuration key to retrieve
#' @param default Default value if key not found
#' @return Configuration value
get_config <- function(key, default = NULL) {
  if (!exists("config", envir = config_env)) {
    config_env$config <- load_config()
  }
  
  value <- config_env$config[[key]]
  if (is.null(value)) {
    return(default)
  }
  return(value)
}

#' Initialize configuration
#' @description Load and validate configuration, creating config.yml if needed
#' @return Loaded configuration
initialize_config <- function() {
  if (!file.exists("config.yml")) {
    # Create default configuration
    default_config <- list(
      default = list(
        app_name = "SigmaSEQ",
        app_version = "1.0.0",
        app_description = "RNA-Seq Analysis Platform",
        theme = list(
          primary = "#0073b7",
          secondary = "#6c757d",
          success = "#28a745",
          info = "#17a2b8",
          warning = "#ffc107",
          danger = "#dc3545"
        ),
        max_upload_size = 5000,
        allowed_file_types = c(".csv", ".txt", ".tsv"),
        default_parameters = list(
          min_count = 10,
          min_samples = 3,
          pvalue_cutoff = 0.05,
          fdr_cutoff = 0.1,
          fold_change_cutoff = 1.0
        ),
        openai = list(
          model = "gpt-4",
          temperature = 0.7,
          max_tokens = 1000,
          timeout = 30
        ),
        paths = list(
          data = "data",
          results = "results",
          logs = "logs",
          www = "www"
        )
      )
    )
    
    yaml::write_yaml(default_config, "config.yml")
  }
  
  # Load and validate configuration
  config_env$config <- load_config()
  return(config_env$config)
} 