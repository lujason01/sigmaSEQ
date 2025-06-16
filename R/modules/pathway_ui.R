#' Create Pathway Analysis UI
#' @param id Module ID
#' @return UI elements for pathway analysis
create_pathway_ui <- function(id) {
  ns <- NS(id)  # Create namespace function
  
  # Get configuration
  default_params <- get_config("default_parameters")
  
  tagList(
    # Add dependencies
    tags$head(
      tags$script(src = "js/pathway_handlers.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/pathway.css")
    ),
    
    fluidPage(
      # Control Panel
      fluidRow(
        column(
          width = 3,
          bs4Dash::box(
            title = "Pathway Analysis Settings",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            
            # Analysis Method Selection
            selectInput(
              ns("pathway_method"),
              "Analysis Method",
              choices = c(
                "GSEA" = "gsea",
                "ORA" = "ora",
                "SPIA" = "spia"
              ),
              selected = "gsea"
            ),
            
            # Gene Set Selection
            selectInput(
              ns("gene_set"),
              "Gene Set Database",
              choices = c(
                "KEGG" = "kegg",
                "GO" = "go",
                "Reactome" = "reactome"
              ),
              selected = "kegg"
            ),
            
            # Threshold Settings
            numericInput(
              ns("pvalue_cutoff"),
              "P-value Cutoff",
              value = default_params$pvalue_cutoff,
              min = 0,
              max = 1,
              step = 0.01
            ),
            
            numericInput(
              ns("qvalue_cutoff"),
              "Q-value Cutoff",
              value = default_params$qvalue_cutoff,
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
          # Results Summary
          fluidRow(
            column(
              width = 4,
              bs4Dash::valueBoxOutput(ns("total_pathways"), width = 12)
            ),
            column(
              width = 4,
              bs4Dash::valueBoxOutput(ns("significant_pathways"), width = 12)
            ),
            column(
              width = 4,
              bs4Dash::valueBoxOutput(ns("top_pathway"), width = 12)
            )
          ),
          
          # Interactive Plots
          fluidRow(
            column(
              width = 6,
              bs4Dash::box(
                title = "Enrichment Plot",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                fluidRow(
                  column(width = 10, plotlyOutput(ns("enrichment_plot"), height = "400px")),
                  column(width = 2, downloadButton(ns("download_enrichment_plot"), "Download PNG", class = "btn-primary"))
                  )
              )
            ),
            column(
              width = 6,
              bs4Dash::box(
                title = "Pathway Network",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                maximizable = TRUE,
                fluidRow(
                  column(width = 10, plotlyOutput(ns("network_plot"), height = "400px")),
                  column(width = 2, downloadButton(ns("download_network_plot"), "Download PNG", class = "btn-primary"))
                )
              )
            )
          ),
          
          # Results Table
          bs4Dash::box(
            title = "Pathway Analysis Results",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(ns("results_table"))
          ),
          
          # Gene-Pathway Table
          bs4Dash::box(
            title = "Gene-Pathway Associations",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(ns("gene_pathway_table"))
          )
        )
      )
    )
  )
} 