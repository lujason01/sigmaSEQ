#' Create DEA UI
#' @param id Module ID
#' @return UI elements for differential expression analysis
create_dea_ui <- function(id) {
  ns <- NS(id)  # Create namespace function
  
  # Get configuration
  default_params <- get_config("default_parameters")
  
  tagList(
    # Add dependencies
    tags$head(
      tags$script(src = "js/dea_handlers.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/dea.css")
    ),
    
    fluidPage(
      # DEA Settings Panel
      fluidRow(
        column(
          width = 3,
          bs4Dash::box(
            title = "DEA Settings",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            
            # Analysis Method
            selectInput(
              ns("method"),
              "Analysis Method",
              choices = c(
                "DESeq2" = "deseq2",
                "EdgeR" = "edger",
                "Limma" = "limma"
              ),
              selected = "deseq2"
            ),
            
            # Contrast Selection
            selectInput(
              ns("contrast"),
              "Contrast",
              choices = NULL
            ),
            
            # Fold Change Threshold
            numericInput(
              ns("fc_threshold"),
              "Fold Change Threshold",
              value = 1.5,
              min = 0,
              step = 0.1
            ),
            
            # P-value Threshold
            numericInput(
              ns("pvalue_threshold"),
              "P-value Threshold",
              value = 0.05,
              min = 0,
              max = 1,
              step = 0.01
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
          width = 9,
          bs4Dash::box(
            title = "Results",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            
            # Volcano Plot
            h4("Volcano Plot"),
            fluidRow(
              column(width = 10, plotlyOutput(ns("volcano_plot"), height = "400px")),
              column(width = 2, downloadButton(ns("download_volcano_plot"), "Download PNG", class = "btn-primary"))
            ),
            
            # MA Plot
            h4("MA Plot"),
            fluidRow(
              column(width = 10, plotlyOutput(ns("ma_plot"), height = "400px")),
              column(width = 2, downloadButton(ns("download_ma_plot"), "Download PNG", class = "btn-primary"))
            ),
            
            # Expression Heatmap
            h4("Expression Heatmap"),
            fluidRow(
              column(width = 10, plotlyOutput(ns("expression_heatmap"), height = "400px")),
              column(width = 2, downloadButton(ns("download_heatmap"), "Download PNG", class = "btn-primary"))
            ),
            
            # Results Table
            h4("Differential Expression Results"),
            DTOutput(ns("results_table"))
          )
        )
      ),
      
      # Summary Statistics
      fluidRow(
        column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("up_genes"), width = 12)
        ),
        column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("down_genes"), width = 12)
        ),
        column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("total_de_genes"), width = 12)
        ),
        column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("analysis_status"), width = 12)
        )
      )
    )
  )
} 