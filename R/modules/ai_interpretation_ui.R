#' Create AI Interpretation UI
#' @param id Module ID
#' @return UI elements for AI interpretation
create_ai_interpretation_ui <- function(id) {
  ns <- NS(id)  # Create namespace function
  
  # Get configuration
  default_params <- get_config("default_parameters")
  
  tagList(
    # Add dependencies
    tags$head(
      tags$script(src = "js/ai_handlers.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/ai.css")
    ),
    
    fluidPage(
      # Control Panel
      fluidRow(
        column(
          width = 3,
          bs4Dash::box(
            title = "AI Analysis Controls",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            
            # Analysis Type Selection
            selectInput(
              ns("analysis_type"),
              "Select Analysis to Interpret",
              choices = c(
                "Exploratory Analysis" = "eda",
                "Differential Expression" = "dea",
                "Pathway Analysis" = "pathway"
              ),
              selected = "dea"
            ),
            
            # Interpretation Focus
            checkboxGroupInput(
              ns("interpretation_focus"),
              "Interpretation Focus",
              choices = c(
                "Technical Summary" = "technical",
                "Biological Insights" = "biological",
                "Recommendations" = "recommendations"
              ),
              selected = c("biological", "recommendations")
            ),
            
            # Custom Prompt Input
            textAreaInput(
              ns("custom_prompt"),
              "Custom Analysis Question (Optional)",
              rows = 3,
              placeholder = "Ask a specific question about your results..."
            ),
            
            # Generate Button
            actionButton(
              ns("generate_interpretation"),
              "Generate Interpretation",
              icon = icon("robot"),
              class = "btn-warning"
            )
          )
        ),
        
        # Results Panel
        column(
          width = 9,
          # AI Response
          bs4Dash::box(
            title = "AI Analysis Insights",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            
            # Loading Indicator
            conditionalPanel(
              condition = sprintf("input['%s'] != 0 && !output['%s']",
                               ns("generate_interpretation"),
                               ns("interpretation_ready")),
              div(
                class = "text-center",
                tags$img(src = "images/loading.gif"),
                h4("Generating interpretation...")
              )
            ),
            
            # Results Display
            htmlOutput(ns("ai_interpretation")),
            
            # Export Options
            footer = div(
              class = "text-right",
              downloadButton(
                ns("download_report"),
                "Export Report",
                class = "btn-sm"
              )
            )
          ),
          
          # Interactive Visualization
          bs4Dash::box(
            title = "AI-Enhanced Visualizations",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            maximizable = TRUE,
            
            plotlyOutput(ns("ai_enhanced_plot")),
            
            footer = div(
              class = "text-muted",
              "AI-generated visualizations based on your analysis results"
            )
          )
        )
      )
    )
  )
}

#' Alias for create_ai_interpretation_ui
#' @export
create_ai_ui <- create_ai_interpretation_ui 