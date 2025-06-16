#' Security Utilities for SigmaSEQ
#' @description Functions for input sanitization and security checks

#' Sanitize file input
#' @param file_path Path to the file
#' @return Sanitized file path or error
sanitize_file_path <- function(file_path) {
  # Remove any directory traversal attempts
  sanitized <- gsub("\\.\\./|\\.\\.\\\\", "", file_path)
  
  # Ensure file is within allowed directories
  allowed_dirs <- c("data", "temp", "results")
  file_dir <- dirname(sanitized)
  
  if (!file_dir %in% allowed_dirs) {
    stop("File access denied: Invalid directory")
  }
  
  return(sanitized)
}

#' Sanitize user input
#' @param input User input string
#' @return Sanitized string
sanitize_input <- function(input) {
  # Remove potentially dangerous characters
  sanitized <- gsub("[<>\"'&]", "", input)
  return(sanitized)
}

#' Validate file type
#' @param file_path Path to the file
#' @param allowed_types Vector of allowed file extensions
#' @return Boolean indicating if file type is allowed
validate_file_type <- function(file_path, allowed_types) {
  ext <- tolower(tools::file_ext(file_path))
  return(ext %in% allowed_types)
}

#' Validate file size
#' @param file_path Path to the file
#' @param max_size Maximum allowed size in bytes
#' @return Boolean indicating if file size is allowed
validate_file_size <- function(file_path, max_size) {
  file_size <- file.size(file_path)
  return(file_size <= max_size)
}

#' Check for malicious content in uploaded files
#' @param file_path Path to the file
#' @return Boolean indicating if file is safe
check_file_safety <- function(file_path) {
  # Read first few lines
  lines <- readLines(file_path, n = 10)
  
  # Check for common malicious patterns
  malicious_patterns <- c(
    "system\\(", "eval\\(", "exec\\(", "shell\\(",
    "rm -rf", "del /f", "format", "chmod 777"
  )
  
  for (pattern in malicious_patterns) {
    if (any(grepl(pattern, lines, ignore.case = TRUE))) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#' Log security events
#' @param event_type Type of security event
#' @param details Additional details about the event
#' @param severity Severity level (INFO, WARNING, ERROR)
log_security_event <- function(event_type, details, severity = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf(
    "[%s] [%s] %s: %s",
    timestamp,
    severity,
    event_type,
    details
  )
  
  # Write to security log
  log_file <- file.path("logs", "security.log")
  write(log_entry, log_file, append = TRUE)
}

#' Validate API key format
#' @param api_key API key to validate
#' @return Boolean indicating if API key format is valid
validate_api_key <- function(api_key) {
  # Basic format validation for OpenAI API key
  return(grepl("^sk-[A-Za-z0-9]{32,}$", api_key))
}

#' Check session security
#' @param session Shiny session object
#' @return Boolean indicating if session is secure
check_session_security <- function(session) {
  # Check if session has been inactive for too long
  last_activity <- session$userData$last_activity
  if (is.null(last_activity)) {
    session$userData$last_activity <- Sys.time()
    return(TRUE)
  }
  
  inactive_time <- as.numeric(difftime(Sys.time(), last_activity, units = "secs"))
  return(inactive_time <= 1800)  # 30 minutes timeout
} 