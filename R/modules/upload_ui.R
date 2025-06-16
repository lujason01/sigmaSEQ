#' Create Upload UI
#' @param id Module ID
#' @return UI elements for data upload
create_upload_ui <- function(id) {
  ns <- NS(id)  # Create namespace function
  
  # Get configuration
  default_params <- get_config("default_parameters")
  
  tagList(
    # Add dependencies
    tags$head(
      tags$script(src = "js/upload_handlers.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/upload.css")
    ),
    
    fluidPage(
      # Description Box
      bs4Dash::box(
        title = "About Data Upload",
        width = 12,
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        p("Upload your RNA-seq count matrix and sample metadata files. Supported formats: CSV, TSV, TXT."),
        p(strong("Requirements:")),
        tags$ul(
          tags$li("Count matrix: First column should contain gene IDs, subsequent columns are samples"),
          tags$li("Metadata: First column should contain sample IDs matching the count matrix"),
          tags$li("Both files should have headers")
        )
      ),
      
      # Upload Panel
      fluidRow(
        column(
          width = 6,
          bs4Dash::box(
            title = "Upload Files",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            # Count Matrix Upload
            fileInput(
              ns("countFile"),
              "Count Matrix File",
              accept = c(".csv", ".tsv", ".txt"),
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            ),
            
            # Metadata Upload
            fileInput(
              ns("metadataFile"),
              "Metadata File",
              accept = c(".csv", ".tsv", ".txt"),
              buttonLabel = "Browse...",
              placeholder = "No file selected"
            ),
            
            # Upload Button and Status
            div(
              style = "margin-top: 20px;",
              # Upload Button
              actionButton(
                ns("upload_button"),
                "Process Files",
                icon = icon("upload"),
                class = "btn-primary",
                width = "100%"
              ),
              
              # Upload Status
              div(
                style = "margin-top: 10px;",
                uiOutput(ns("upload_status"))
              ),
              
              # Progress Bar
              div(
                style = "margin-top: 10px;",
                bs4Dash::progressBar(
                  value = 0,
                  status = "primary",
                  striped = TRUE,
                  animated = TRUE
                )
              )
            )
          )
        ),
        
        column(
          width = 6,
          # Data Summary Boxes
          fluidRow(
            column(
              width = 4,
              bs4Dash::valueBoxOutput(ns("total_samples"), width = 12)
            ),
            column(
              width = 4,
              bs4Dash::valueBoxOutput(ns("total_genes"), width = 12)
            ),
            column(
              width = 4,
              bs4Dash::valueBoxOutput(ns("data_status"), width = 12)
            )
          ),
          
          # Data Preview Box
          bs4Dash::box(
            title = "Data Preview",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            
            # Tabset for different previews
            bs4Dash::tabBox(
              width = 12,
              title = "Preview",
              side = "right",
              
              # Count Matrix Preview Tab
              tabPanel(
                "Count Matrix",
                DTOutput(ns("count_preview"))
              ),
              
              # Metadata Preview Tab
              tabPanel(
                "Metadata",
                DTOutput(ns("metadata_preview"))
              )
            )
          )
        )
      )
    )
  )
}
