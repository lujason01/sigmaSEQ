# setup.R
install_required_packages <- function() {
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
  
  # Install BiocManager if needed
  if (!require("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  
  # Install Bioconductor packages
  bioc_packages <- c(
    "DESeq2", "edgeR", "limma", "GO.db", 
    "org.Hs.eg.db", "KEGG.db", "reactome.db", "pathview"
  )
  
  # Install missing CRAN packages
  new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  if (length(new_packages)) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages)
  }
  
  # Install missing Bioconductor packages
  for (pkg in bioc_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      BiocManager::install(pkg, update = FALSE, ask = FALSE)
    }
  }
}

create_required_directories <- function() {
  dirs <- c(
    "R/modules",
    "R/helpers",
    "www/css",
    "www/js",
    "www/images",
    "data",
    "results",
    "logs"
  )
  
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      message("Created directory: ", dir)
    }
  }
}

create_required_files <- function() {
  files <- c(
    "www/css/custom.css",
    "www/js/custom.js",
    ".env"
  )
  
  for (file in files) {
    if (!file.exists(file)) {
      file.create(file)
      message("Created file: ", file)
    }
  }
  
  # Add basic CSS if file is empty
  if (file.size("www/css/custom.css") == 0) {
    writeLines(
      c(
        "/* Custom CSS for SigmaSEQ */",
        ".bs4-card {",
        "  margin-bottom: 1rem;",
        "}",
        ".plot-container {",
        "  height: 400px;",
        "}"
      ),
      "www/css/custom.css"
    )
  }
  
  # Add basic JS if file is empty
  if (file.size("www/js/custom.js") == 0) {
    writeLines(
      c(
        "// Custom JavaScript for SigmaSEQ",
        "$(document).ready(function() {",
        "  // Add any custom JavaScript here",
        "});"
      ),
      "www/js/custom.js"
    )
  }
}

# Main setup function
setup_environment <- function() {
  tryCatch({
    message("Starting setup...")
    
    # Install packages
    install_required_packages()
    
    # Create directories
    create_required_directories()
    
    # Create files
    create_required_files()
    
    message("Setup completed successfully!")
  }, error = function(e) {
    message("Error during setup: ", e$message)
    stop("Setup failed. Please check the error message above.")
  })
}

# Run the setup
setup_environment()
