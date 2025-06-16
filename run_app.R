#!/usr/bin/env Rscript

#' Initialize and run the SigmaSEQ application
#' @description Handles automated loading of dependencies, configuration, and app startup
#' @author Jason Lubega
#' @version 1.0.0

# Initial check for logger package
if (!require("logger", quietly = TRUE)) {
  message("Installing required package: logger")
  install.packages("logger", repos = "https://cran.rstudio.com/")
  library(logger)
}

# Error handling for the initialization process
handle_startup_error <- function(e, stage) {
  message(sprintf("Error during %s: %s", stage, e$message))
  if (interactive()) {
    readline(prompt = "Press [Enter] to exit")
  }
  quit(status = 1)
}

# Function to check and create necessary directories
setup_directories <- function() {
  dirs <- c(
    "R/modules",
    "R/helpers",
    "data",
    "results",
    "logs",
    "www",
    "tests"
  )
  
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      message(sprintf("Created directory: %s", dir))
    }
  }
}

# Function to load and check required packages
load_dependencies <- function() {
  required_packages <- c(
    "shiny",
    "bs4Dash",
    "DT",
    "plotly",
    "ggplot2",
    "reactable",
    "httr",
    "jsonlite",
    "base64enc",
    "openai",
    "here",
    "logger",
    "yaml",
    "config",
    "testthat"
  )
  
  message("Checking and installing required packages...")
  for (package in required_packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      message(sprintf("Installing package: %s", package))
      install.packages(package, repos = "https://cran.rstudio.com/")
      library(package, character.only = TRUE)
    }
  }
  
  # Bioconductor packages
  if (!require("BiocManager", quietly = TRUE)) {
    message("Installing BiocManager...")
    install.packages("BiocManager", repos = "https://cran.rstudio.com/")
    library(BiocManager)
  }
  
  bioc_packages <- c("DESeq2", "edgeR", "limma", "clusterProfiler")
  message("Checking and installing Bioconductor packages...")
  BiocManager::install(bioc_packages, update = FALSE, ask = FALSE)
}

# Function to source files in order
source_files <- function() {
  # Define source order with dependencies
  source_order <- list(
    project_setup = list(
      path = "R/helpers",
      files = c("project_paths.R", "config_utils.R")  # Load these first
    ),
    helpers = list(
      path = "R/helpers",
      pattern = "\\.[rR]$",
      exclude = c("project_paths.R", "config_utils.R")  # Exclude already sourced files
    ),
    modules_ui = list(
      path = "R/modules",
      pattern = "_ui\\.[rR]$"
    ),
    modules_server = list(
      path = "R/modules",
      pattern = "_server\\.[rR]$"
    )
  )
  
  # First source setup files
  for (file in source_order$project_setup$files) {
    file_path <- file.path(source_order$project_setup$path, file)
    if (file.exists(file_path)) {
      message(sprintf("Sourcing setup file: %s", file))
      source(file_path, local = FALSE)
    } else {
      stop(sprintf("Required setup file not found: %s", file_path))
    }
  }
  
  # Initialize configuration
  message("Initializing configuration...")
  config <- initialize_config()
  
  # Then source remaining files in order
  for (category in names(source_order)[-1]) {  # Skip project_setup
    if (dir.exists(source_order[[category]]$path)) {
      files <- list.files(
        path = source_order[[category]]$path,
        pattern = source_order[[category]]$pattern,
        full.names = TRUE
      )
      
      # Exclude specific files if defined
      if (!is.null(source_order[[category]]$exclude)) {
        files <- files[!basename(files) %in% source_order[[category]]$exclude]
      }
      
      for (file in files) {
        message(sprintf("Sourcing %s: %s", category, basename(file)))
        source(file, local = FALSE)
      }
    } else {
      message(sprintf("Warning: Directory %s does not exist", 
                     source_order[[category]]$path))
    }
  }
}

# Function to verify configuration
verify_config <- function() {
  if (!file.exists("config.yml")) {
    message("config.yml not found, creating default configuration...")
    # Create default config if it doesn't exist
    default_config <- list(
      default = list(
        app_name = "SigmaSEQ",
        app_version = "1.0.0",
        app_description = "RNA-Seq Analysis Platform",
        theme = list(
          primary = "#0073b7",
          secondary = "#6c757d"
        ),
        max_upload_size = 5000,
        openai_api_key = Sys.getenv("OPENAI_API_KEY", ""),
        allowed_file_types = c("fastq", "fastq.gz", "fq", "fq.gz")
      )
    )
    yaml::write_yaml(default_config, "config.yml")
  }
  
  config <- yaml::read_yaml("config.yml")
  required_configs <- c("app_name", "app_version", "max_upload_size", "allowed_file_types")
  missing_configs <- required_configs[!required_configs %in% names(config$default)]
  
  if (length(missing_configs) > 0) {
    message(sprintf("Warning: Missing configurations: %s", 
                   paste(missing_configs, collapse = ", ")))
  }
}

# Function to initialize logging
setup_logging <- function() {
  log_dir <- "logs"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir)
  }
  
  log_file <- file.path(log_dir, 
                        format(Sys.time(), "sigmaSEQ_%Y%m%d_%H%M%S.log"))
  log_appender(appender_file(log_file))
  log_threshold(INFO)
  log_info("SigmaSEQ initialization started")
}

# Add this function just before initialize_app
verify_sourcing <- function() {
  required_functions <- c(
    # Project paths functions
    "get_project_paths",
    "validate_path",
    "get_data_path",
    "get_results_path",
    
    # UI functions
    "create_eda_ui",
    "create_dea_ui",
    "create_pathway_ui",
    "create_ai_ui",
    
    # Server functions
    "eda_server",
    "dea_server",
    "pathway_server",
    "ai_interpretation_server",
    
    # Configuration functions
    "get_config",
    "initialize_config",
    
    # Helper functions
    "setup_logging",
    "handle_startup_error"
  )
  
  missing_functions <- required_functions[!sapply(required_functions, exists)]
  
  if (length(missing_functions) > 0) {
    stop(sprintf("Missing required functions: %s", 
                paste(missing_functions, collapse = ", ")))
  }
  
  return(TRUE)
}

# Main initialization function
initialize_app <- function() {
  tryCatch({
    message("Setting up directories...")
    setup_directories()
    
    message("Loading dependencies...")
    load_dependencies()
    
    message("Setting up logging...")
    setup_logging()
    
    message("Verifying configuration...")
    verify_config()
    
    message("Sourcing application files...")
    source_files()
    
    message("Verifying required functions...")
    verify_sourcing()
    
    log_info("Initialization complete")
    initialized <<- TRUE
    return(TRUE)
  }, error = function(e) {
    message(sprintf("Initialization failed: %s", e$message))
    handle_startup_error(e, "initialization")
    return(FALSE)
  })
}

# Main execution
if (!interactive()) {
  options(shiny.port = 3838)
  options(shiny.host = "127.0.0.1")
  options(shiny.launch.browser = TRUE)
}

# Initialize and run the app
if (initialize_app()) {
  message("Starting SigmaSEQ application...")
  # Load required packages
  source("global.R")

  # Initialize analysis parameters
  analysis_params <- list(
    normalization_method = "DESeq2",
    transformation_method = "vst",
    min_count = 10,
    fc_threshold = 1.5,
    pval_threshold = 0.05
  )

  # Define UI
  ui <- bs4Dash::dashboardPage(
    # Header
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "sigmaSEQ",
        color = "primary",
        href = "https://github.com/lujason01/sigmaSEQ",
        image = "https://raw.githubusercontent.com/lujason01/r-practice/refs/heads/main/sigmaseq-high-resolution-logo.png"
      ),
      rightUi = bs4Dash::dropdownMenu(
        type = "notifications",
        badgeStatus = "danger",
        icon = icon("bell"),
        bs4Dash::notificationItem(
          text = "Welcome to sigmaSEQ!",
          status = "success"
        )
      )
    ),
    
    # Sidebar
    sidebar = create_sidebar_ui("sidebar"),
    
    # Body
    body = create_body_ui("body"),
    
    # Controlbar
    controlbar = bs4Dash::dashboardControlbar(
      skin = "light",
      pinned = TRUE,
      collapsed = FALSE,
      overlay = FALSE,
      id = "controlbar",
      
      # Controlbar Menu
      bs4Dash::controlbarMenu(
        id = "controlbarMenu",
        
        # Settings Tab
        bs4Dash::controlbarItem(
          "Settings",
          icon = icon("cogs"),
          sliderInput(
            "plot_height",
            "Plot Height",
            min = 400,
            max = 1000,
            value = 600,
            step = 50
          ),
          sliderInput(
            "plot_width",
            "Plot Width",
            min = 400,
            max = 1000,
            value = 800,
            step = 50
          )
        ),
        
        # Help Tab
        bs4Dash::controlbarItem(
          "Help",
          icon = icon("question-circle"),
          p("For help, please refer to the documentation or contact support.")
        )
      )
    ),
    
    # Footer
    footer = bs4Dash::dashboardFooter(
      left = a(
        href = "https://github.com/lujason01/sigmaSEQ",
        target = "_blank",
        "sigmaSEQ on GitHub"
      ),
      right = "© 2025 Jason Lubega's Lab"
    ),
    
    # Title
    title = "sigmaSEQ - RNA-Seq Analysis Platform",
    
    # Skin
    skin = "light"
  )

  # Define server logic
  server <- function(input, output, session) {
    # Get configuration values
    upload_config <- list(
      max_size = get_config("max_upload_size"),
      allowed_types = get_config("allowed_file_types")
    )
    
    # Initialize states
    data_state <- reactiveVal(list(
      count_matrix = NULL,
      metadata = NULL,
      feature_metadata = NULL,
      parameters = analysis_params
    ))
    
    # Initialize modules
    upload_data <- create_upload_server("upload", data_state)
    geo_data <- create_geo_data_server("geo_data", data_state)
    eda_results <- create_eda_server("eda", data_state)
    dea_results <- create_dea_server("dea", data_state)
    pathway_results <- create_pathway_server("pathway", dea_results)
    ai_interpretation <- create_ai_interpretation_server("ai_interpretation", dea_results, pathway_results)
    dashboard_data <- create_dashboard_server("dashboard", data_state, dea_results, pathway_results)
    
    # Update data state when upload completes
    observe({
      req(upload_data())
      data_state(upload_data())
    })
    
    # Update data state when GEO data is loaded
    observe({
      geo_result <- geo_data()
      if (!is.null(geo_result)) {
        data_state(list(
          count_matrix = geo_result$counts,
          metadata = geo_result$sample_metadata,
          feature_metadata = geo_result$feature_metadata,
          parameters = analysis_params
        ))
      }
    })
    
    # Update progress bar
    observe({
      progress <- 0
      if (!is.null(data_state()$count_matrix)) progress <- progress + 20
      if (!is.null(eda_results())) progress <- progress + 20
      if (!is.null(dea_results())) progress <- progress + 20
      if (!is.null(pathway_results())) progress <- progress + 20
      if (!is.null(ai_interpretation())) progress <- progress + 20
      
      updateProgressBar(
        session = session,
        id = "analysisProgress",
        value = progress,
        label = sprintf("%d%% Complete", progress)
      )
    })
    
    # File validation observer
    observe({
      req(input$upload)
      
      # Validate file
      validate(
        need(
          file.size(input$upload$datapath) <= upload_config$max_size,
          sprintf("File size must be less than %d MB", upload_config$max_size/1024^2)
        ),
        need(
          tools::file_ext(input$upload$name) %in% 
            gsub("^\\.", "", upload_config$allowed_types),
          sprintf(
            "File type must be one of: %s",
            paste(upload_config$allowed_types, collapse = ", ")
          )
        )
      )
    })
    
    # Handle session ending
    session$onSessionEnded(function() {
      # Clean up temporary files
      temp_files <- list.files(
        tempdir(),
        pattern = "sigmaSEQ",
        full.names = TRUE
      )
      unlink(temp_files)
      
      # Clear cache if it exists
      if (exists("cache")) {
        cache$purge()
      }
      
      # Log session end
      log_info(sprintf(
        "Session ended for %s v%s, cleanup complete",
        get_config("app_name"),
        get_config("app_version")
      ))
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)
} 