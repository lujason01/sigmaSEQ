#' Create EDA UI
#' @param id Module ID
#' @return UI elements for exploratory data analysis
create_eda_ui <- function(id) {
  ns <- NS(id)  # Create namespace function
  
  # Get configuration
  default_params <- get_config("default_parameters")
  
  tagList(
    # Add dependencies
    tags$head(
      tags$script(src = "js/eda_handlers.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/eda.css")
    ),
    
    fluidPage(
      # Analysis Settings Panel
    fluidRow(
      column(
          width = 4,
          bs4Dash::box(
            title = "Analysis Settings",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
            maximizable = TRUE,
            
            # Normalization Method
            selectInput(
              ns("normalization"),
              "Normalization Method",
              choices = c(
                "DESeq2" = "deseq2",
                "EdgeR" = "edger",
                "Limma" = "limma",
                "None" = "none"
              ),
              selected = "deseq2"
            ),
            
            # Transformation Method
            selectInput(
              ns("transformation"),
              "Transformation",
              choices = c(
                "Log2" = "log2",
                "VST" = "vst",
                "RLOG" = "rlog",
                "None" = "none"
              ),
              selected = "log2"
          ),
          
          # Filtering Options
            numericInput(
              ns("min_count"),
              "Minimum Count",
              value = 10,
              min = 0
            ),
            
            numericInput(
              ns("min_samples"),
              "Minimum Samples",
              value = 3,
              min = 1
            ),
            
            # Run Analysis Button
          actionButton(
              ns("run_analysis"),
              "Run Analysis",
              icon = icon("play"),
              class = "btn-primary"
            )
          )
        ),
        
        # Results Panel
        column(
          width = 8,
          bs4Dash::box(
            title = "Results",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            
            # Quality Control Plots
            h4("Quality Control"),
            fluidRow(
              column(width = 10, plotlyOutput(ns("qc_plot"), height = "400px")),
              column(width = 2, downloadButton(ns("download_qc_plot"), "Download PNG", class = "btn-primary"))
            ),
            
            # Sample Clustering
            h4("Sample Clustering"),
            fluidRow(
              column(width = 10, plotlyOutput(ns("clustering_plot"), height = "400px")),
              column(width = 2, downloadButton(ns("download_clustering_plot"), "Download PNG", class = "btn-primary"))
            ),
            
            # PCA Plot
            h4("Principal Component Analysis"),
            fluidRow(
              column(width = 10, plotlyOutput(ns("pca_plot"), height = "400px")),
              column(width = 2, downloadButton(ns("download_pca_plot"), "Download PNG", class = "btn-primary"))
            ),
            
            # Heatmap
            h4("Expression Heatmap"),
            fluidRow(
              column(width = 10, plotlyOutput(ns("heatmap"), height = "400px")),
              column(width = 2, downloadButton(ns("download_heatmap"), "Download PNG", class = "btn-primary"))
            )
          )
        )
      ),
      
      # Summary Statistics
      fluidRow(
        column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("total_genes"), width = 12)
        ),
        column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("filtered_genes"), width = 12)
        ),
            column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("total_samples"), width = 12)
            ),
            column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("data_quality"), width = 12)
        )
      )
    )
  )
} 
