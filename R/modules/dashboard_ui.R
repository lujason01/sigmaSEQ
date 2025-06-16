#' Create Dashboard UI
#' @param id Module ID
#' @return UI elements for dashboard
create_dashboard_ui <- function(id) {
  ns <- NS(id)  # Create namespace function
  
  # Get configuration
  default_params <- get_config("default_parameters")
  
  tagList(
    # Add dependencies
    tags$head(
      tags$script(src = "js/dashboard_handlers.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/dashboard.css")
    ),
    
    fluidPage(
      # Summary Statistics
      fluidRow(
        column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("total_samples"), width = 12)
        ),
        column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("total_genes"), width = 12)
        ),
        column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("de_genes"), width = 12)
        ),
        column(
          width = 3,
          bs4Dash::valueBoxOutput(ns("pathways"), width = 12)
        )
      ),
      
      # Data Overview
      fluidRow(
        column(
          width = 12,
          bs4Dash::box(
            title = "Data Overview",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            plotlyOutput(ns("data_overview"), height = "400px")
          )
        )
      ),
      
      # Analysis Progress
      fluidRow(
        column(
          width = 12,
          bs4Dash::box(
            title = "Analysis Progress",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            div(
              class = "progress-title",
              "Overall Progress"
            ),
            bs4Dash::progressBar(
              value = 0,
              status = "primary",
              striped = TRUE
            ),
            div(
              class = "progress-title",
              "Data Quality"
            ),
            bs4Dash::progressBar(
              value = 0,
              status = "success",
              striped = TRUE
            ),
            div(
              class = "progress-title",
              "Analysis Quality"
            ),
            bs4Dash::progressBar(
              value = 0,
              status = "warning",
              striped = TRUE
            )
          )
        )
      ),
      
      # Recent Results
      fluidRow(
        column(
          width = 12,
          bs4Dash::box(
            title = "Recent Results",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            DTOutput(ns("recent_results"))
          )
        )
      ),
      
      # Analysis Log
      fluidRow(
        column(
          width = 12,
          bs4Dash::box(
            title = "Analysis Log",
            width = 12,
            status = "secondary",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            verbatimTextOutput(ns("analysis_log"))
          )
        )
      )
    )
  )
} 