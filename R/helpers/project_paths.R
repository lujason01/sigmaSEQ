#' Project Paths Management
#' @description Functions to manage and standardize project paths
#' @author Your Name
#' @version 1.0.0

#' Get standardized project paths
#' @return List of standardized project paths
get_project_paths <- function() {
  # Use here package for path management
  if (!require("here")) {
    install.packages("here")
    library(here)
  }
  
  # Define standard paths
  paths <- list(
    root = here::here(),
    data = here::here("data"),
    results = here::here("results"),
    logs = here::here("logs"),
    www = here::here("www"),
    modules = here::here("R", "modules"),
    helpers = here::here("R", "helpers"),
    tests = here::here("tests")
  )
  
  # Create directories if they don't exist
  for (path in paths) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
  }
  
  return(paths)
}

#' Validate file path exists
#' @param path Character string of file path
#' @return Logical indicating if path exists
validate_path <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Path does not exist: %s", path))
  }
  return(TRUE)
}

#' Get data file path
#' @param filename Name of file in data directory
#' @return Full path to data file
get_data_path <- function(filename) {
  file.path(get_project_paths()$data, filename)
}

#' Get results file path
#' @param filename Name of file in results directory
#' @return Full path to results file
get_results_path <- function(filename) {
  file.path(get_project_paths()$results, filename)
}

#' Get module file path
#' @param module_name Name of module file
#' @return Full path to module file
get_module_path <- function(module_name) {
  file.path(get_project_paths()$modules, module_name)
}

#' Initialize project structure
#' @description Creates necessary directories and validates structure
#' @return Invisible NULL
initialize_project_structure <- function() {
  paths <- get_project_paths()
  
  # Create README files in each directory
  readme_content <- list(
    data = "# Data Directory\nStore raw and processed data files here.",
    results = "# Results Directory\nAnalysis outputs and generated files.",
    logs = "# Logs Directory\nApplication logs and error reports.",
    www = "# WWW Directory\nStatic files for the web interface.",
    modules = "# Modules Directory\nShiny module files (UI and server components).",
    helpers = "# Helpers Directory\nUtility functions and helper scripts.",
    tests = "# Tests Directory\nUnit tests and test data."
  )
  
  # Create directories and README files
  for (name in names(readme_content)) {
    dir_path <- paths[[name]]
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    readme_path <- file.path(dir_path, "README.md")
    if (!file.exists(readme_path)) {
      writeLines(readme_content[[name]], readme_path)
    }
  }
  
  # Create .gitkeep files for empty directories
  for (path in paths) {
    if (length(list.files(path)) == 0) {
      file.create(file.path(path, ".gitkeep"))
    }
  }
  
  invisible(NULL)
}

# Initialize project structure when sourced
initialize_project_structure()

# This works everywhere:
plots_dir <- here("results", "plots") 

# Your code just uses pre-configured paths:
save_plot <- function(plot, name) {
  ggsave(
    filename = file.path(plots_dir, name),
    plot = plot
  )
}