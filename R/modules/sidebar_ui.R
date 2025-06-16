#' Create Sidebar UI
#' 
#' @param id The module ID
#' @return A bs4Dash sidebar UI component
#' @export
create_sidebar_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::dashboardSidebar(
    skin = "light",
    status = "primary",
    elevation = 3,
    collapsed = FALSE,
    minified = FALSE,
    expandOnHover = TRUE,
    fixed = TRUE,
    id = "sidebar",
    
    # Sidebar Menu
    bs4Dash::sidebarMenu(
      id = "sidebarMenu",
      
      # Dashboard Menu Item
      bs4Dash::menuItem(
        text = "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      # Data Upload Menu Item
      bs4Dash::menuItem(
        text = "Data Upload",
        tabName = "upload",
        icon = icon("upload")
      ),
      
      # GEO Data Menu Item
      bs4Dash::menuItem(
        text = "GEO Data",
        tabName = "geo_data",
        icon = icon("database")
      ),
      
      # EDA Menu Item
      bs4Dash::menuItem(
        text = "Exploratory Analysis",
        tabName = "eda",
        icon = icon("chart-line")
      ),
      
      # DEA Menu Item
      bs4Dash::menuItem(
        text = "Differential Expression",
        tabName = "dea",
        icon = icon("dna")
      ),
      
      # Pathway Analysis Menu Item
      bs4Dash::menuItem(
        text = "Pathway Analysis",
        tabName = "pathway",
        icon = icon("project-diagram")
      ),
      
      # AI Interpretation Menu Item
      bs4Dash::menuItem(
        text = "AI Interpretation",
        tabName = "ai_interpretation",
        icon = icon("robot")
      )
    ),
    
    # Sidebar Footer (using div instead of sidebarFooter)
    div(
      class = "sidebar-footer",
      style = "position: absolute; bottom: 0; width: 100%; padding: 10px; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
      p(
        class = "text-center",
        "sigmaSEQ v1.0.0",
        br(),
        "© 2025 - Jason Lubega's Lab"
      )
    )
  )
} 