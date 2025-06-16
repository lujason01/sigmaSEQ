# Clear the environment first
#rm(list = ls())

# Setup proper project root with 'here'
if (!file.exists(".here")) {
  # Remove incorrect .here.txt if it exists
  if (file.exists(".here.txt")) {
    unlink(".here.txt")
  }
  # Create proper .here file
  file.create(".here")
}

# Load required packages
required_packages <- c(
  # UI and Visualization
  "shiny", "bs4Dash", "DT", "plotly", "ggplot2", "reactable",
  # API and Data Handling
  "httr", "jsonlite", "base64enc", "openai",
  # Additional utilities
  "dplyr", "tidyr", "reshape2", "pheatmap", "RColorBrewer",
  "scales", "viridis", "gridExtra", "here", "shinydashboard", "shinyBS",
  "fresh",  # For theming
  "waiter"  # For loading screens
)

# Load all required packages
for (pkg in required_packages) {
  library(pkg, character.only = TRUE)
}

# Load Bioconductor packages
bioc_packages <- c(
  "DESeq2", "edgeR", "limma", "GO.db", 
  "org.Hs.eg.db", "KEGGREST", "reactome.db", "pathview",
  "clusterProfiler", "enrichplot"  # Added for pathway analysis
)

# Install Bioconductor packages if not already installed
if (!require("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install(bioc_packages, update = FALSE, ask = FALSE)

# Load Bioconductor packages
for (pkg in bioc_packages) {
  library(pkg, character.only = TRUE)
}

# Initialize 'here'
library(here)
message("Project root directory: ", here())

# Load environment variables
if (file.exists(".env")) {
  readRenviron(".env")
} else {
  stop(".env file not found. Please create one with your OpenAI API key")
}

# Set options for better error messages
options(
  shiny.fullstacktrace = TRUE,
  shiny.trace = TRUE,
  shiny.maxRequestSize = 50 * 1024^2
)

# Configure parallel processing if available
if (require("parallel")) {
  options(mc.cores = parallel::detectCores() - 1)
}

# Set up logging
log_info <- function(msg) {
  cat(sprintf("[%s] INFO: %s\n", format(Sys.time()), msg))
}

log_error <- function(msg) {
  cat(sprintf("[%s] ERROR: %s\n", format(Sys.time()), msg))
}

# Create necessary directories
for (dir in c("data", "results", "logs", "R/helpers", "R/modules")) {
  dir_path <- here(dir)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created directory: ", dir_path)
  }
}

log_info("Global configuration loaded successfully")

# Load helper functions
source(here("R/helpers/api_utils.R"))

# Initialize OpenAI
tryCatch({
  initialize_openai()
  message("OpenAI API initialized successfully")
}, error = function(e) {
  stop("Failed to initialize OpenAI API: ", e$message)
})

# Load module files
source(here("R/modules/upload_ui.R"))
source(here("R/modules/upload_server.R"))
source(here("R/modules/eda_ui.R"))
source(here("R/modules/eda_server.R"))
source(here("R/modules/dea_ui.R"))
source(here("R/modules/dea_server.R"))
source(here("R/modules/pathway_ui.R"))
source(here("R/modules/pathway_server.R"))
source(here("R/modules/ai_interpretation_ui.R"))
source(here("R/modules/ai_interpretation_server.R"))
source(here("R/modules/geo_data_ui.R"))
source(here("R/modules/geo_data_server.R"))
source(here("R/modules/dashboard_ui.R"))
source(here("R/modules/dashboard_server.R"))
source(here("R/modules/sidebar_ui.R"))
source(here("R/modules/body_ui.R"))
source(here("R/helpers/config_utils.R"))
source(here("R/helpers/project_paths.R")) 