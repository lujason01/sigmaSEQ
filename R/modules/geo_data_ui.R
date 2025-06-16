# R/modules/geo_data_ui.R

#' Create GEO Data UI
#' @param id Module ID
#' @return UI elements for GEO data import
create_geo_data_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/geo.css"),
      tags$script(src = "js/geo_handlers.js")
    ),
    
    bs4Dash::box(
      title = "GEO Data Import",
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      
      div(class = "geo-container", id = ns("container"),
        div(class = "geo-input-group",
          tags$label(class = "geo-label", "GEO Dataset ID"),
          div(
            class = "geo-input",
            textInput(ns("geo_id"), NULL, placeholder = "e.g. GSE12345")
          ),
          actionButton(ns("get_geo"), "Fetch GEO Data", class = "geo-button geo-button-primary")
        ),
        
        div(id = ns("loading_container"), class = "geo-loading-container",
          div(class = "geo-spinner"),
          div(class = "geo-progress-container",
            div(id = ns("progress_bar"), class = "geo-progress-bar"),
            div(id = ns("progress_text"), class = "geo-progress-text")
          )
        ),
        
        div(id = ns("status_container"), class = "geo-status"),
        
        div(class = "geo-input-group",
          tags$label(class = "geo-label", "Select Platform"),
          div(
            class = "geo-select",
            selectInput(ns("platform"), NULL, choices = NULL, selected = NULL)
          )
        ),
        
        div(class = "geo-input-group",
          tags$label(class = "geo-label", "Select Samples"),
          div(
            class = "geo-select",
            selectInput(ns("samples"), NULL, choices = NULL, selected = NULL, multiple = TRUE)
          )
        ),
        
        div(class = "geo-input-group",
          actionButton(ns("process_geo"), "Process Selected Data", 
                      class = "geo-button geo-button-primary",
                      style = "display: none;")
        )
      )
    ),
    
    # AI Data Explorer Box
    bs4Dash::box(
      title = "AI Data Explorer",
      width = 12,
      status = "info",
      solidHeader = TRUE,
      
      div(class = "ai-explorer-container",
        # Query input
        div(class = "query-input-group",
          textAreaInput(
            ns("query_input"),
            "Ask about your data",
            placeholder = "e.g., What are the most differentially expressed genes? What pathways are enriched?",
            rows = 3,
            resize = "vertical"
          ),
          actionButton(
            ns("submit_query"),
            "Ask AI",
            icon = icon("robot"),
            class = "btn-info"
          )
        ),
        
        # Chat history
        div(
          id = ns("chat_history"),
          class = "chat-history",
          style = "max-height: 400px; overflow-y: auto;"
        ),
        
        # Suggested queries
        div(class = "suggested-queries",
          tags$label("Suggested Questions:"),
          tags$ul(
            tags$li(
              actionLink(
                ns("suggest_1"),
                "What are the key findings in this dataset?"
              )
            ),
            tags$li(
              actionLink(
                ns("suggest_2"),
                "Show me the top pathways and their significance"
              )
            ),
            tags$li(
              actionLink(
                ns("suggest_3"),
                "Generate a summary of the experimental design"
              )
            ),
            tags$li(
              actionLink(
                ns("suggest_4"),
                "What are the quality control metrics?"
              )
            )
          )
        )
      )
    ),
    
    # Data preview boxes
    fluidRow(
      bs4Dash::box(
        title = "Expression Data Preview",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        DTOutput(ns("expression_preview"))
      ),
      bs4Dash::box(
        title = "Sample Metadata Preview",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        DTOutput(ns("metadata_preview"))
      )
    ),
    
    # Summary boxes
    fluidRow(
      bs4Dash::valueBox(
        value = textOutput(ns("geo_title")),
        subtitle = "Dataset Title",
        icon = icon("file-alt"),
        color = "primary",
        width = 4
      ),
      bs4Dash::valueBox(
        value = textOutput(ns("total_samples")),
        subtitle = "Total Samples",
        icon = icon("vial"),
        color = "success",
        width = 4
      ),
      bs4Dash::valueBox(
        value = textOutput(ns("platform_info")),
        subtitle = "Platform",
        icon = icon("microchip"),
        color = "info",
        width = 4
      )
    )
  )
}
