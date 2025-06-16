#' UI Components
#' @description Helper functions for creating UI components
#' @author Jason Lubega
#' @version 1.0.0

# Create sidebar
create_sidebar <- function() {
  bs4DashSidebar(
    skin = "light",
    status = "primary",
    title = "SigmaSEQ",
    brandColor = "primary",
    url = "https://github.com/lujason01/sigmaSEQ",
    src = "www/images/logo.png",
    elevation = 3,
    opacity = 0.8,
    bs4SidebarMenu(
      id = "sidebarMenu",
      bs4SidebarMenuItem(
        "Home",
        tabName = "home",
        icon = icon("home")
      ),
      bs4SidebarMenuItem(
        "Upload Data",
        tabName = "upload",
        icon = icon("upload")
      ),
      bs4SidebarMenuItem(
        "Exploratory Analysis",
        tabName = "eda",
        icon = icon("chart-line")
      ),
      bs4SidebarMenuItem(
        "Differential Expression",
        tabName = "dea",
        icon = icon("dna")
      ),
      bs4SidebarMenuItem(
        "Pathway Analysis",
        tabName = "pathway",
        icon = icon("project-diagram")
      ),
      bs4SidebarMenuItem(
        "AI Interpretation",
        tabName = "ai",
        icon = icon("robot")
      )
    )
  )
}

# Create body
create_body <- function() {
  bs4DashBody(
    bs4TabItems(
      # Home tab
      bs4TabItem(
        tabName = "home",
        fluidRow(
          bs4Card(
            width = 12,
            title = "Welcome to SigmaSEQ",
            status = "primary",
            solidHeader = TRUE,
            includeMarkdown("www/markdown/welcome.md")
          )
        )
      ),
      
      # Upload tab
      bs4TabItem(
        tabName = "upload",
        create_upload_ui("upload")
      ),
      
      # EDA tab
      bs4TabItem(
        tabName = "eda",
        create_eda_ui("eda")
      ),
      
      # DEA tab
      bs4TabItem(
        tabName = "dea",
        create_dea_ui("dea")
      ),
      
      # Pathway tab
      bs4TabItem(
        tabName = "pathway",
        create_pathway_ui("pathway")
      ),
      
      # AI tab
      bs4TabItem(
        tabName = "ai",
        create_ai_interpretation_ui("ai")
      )
    ),
    
    # Progress bar
    bs4ProgressBar(
      id = "analysisProgress",
      value = 0,
      status = "primary",
      striped = TRUE,
      animated = TRUE
    )
  )
}

# Create value box
create_value_box <- function(value, subtitle, icon, color = "primary") {
  bs4ValueBox(
    value = value,
    subtitle = subtitle,
    icon = icon(icon),
    color = color
  )
}

# Create download button
create_download_button <- function(id, label = "Download") {
  tags$button(
    id = id,
    class = "download-button",
    icon("download"),
    tags$span(class = "btn-text", label)
  )
}

# Create action button
create_action_button <- function(id, label, icon = NULL, status = "primary") {
  bs4ActionButton(
    id,
    label,
    status = status,
    icon = icon(icon)
  )
}

# Create card
create_card <- function(..., title = NULL, status = "primary", width = 6) {
  bs4Card(
    ...,
    title = title,
    status = status,
    solidHeader = TRUE,
    width = width,
    collapsible = TRUE,
    maximizable = TRUE
  )
} 