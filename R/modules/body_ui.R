#' Create Body UI
#' 
#' @param id The module ID
#' @return A bs4Dash body UI component
#' @export
create_body_ui <- function(id) {
  ns <- NS(id)
  
  bs4Dash::dashboardBody(
    # Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
    ),
    
    # Tab Items
    bs4Dash::tabItems(
      # Dashboard Tab
      bs4Dash::tabItem(
        tabName = "dashboard",
        create_dashboard_ui("dashboard")
      ),
      
      # Upload Tab
      bs4Dash::tabItem(
        tabName = "upload",
        create_upload_ui("upload")
      ),
      
      # GEO Data Tab
      bs4Dash::tabItem(
        tabName = "geo_data",
        create_geo_data_ui("geo_data")
      ),
      
      # EDA Tab
      bs4Dash::tabItem(
        tabName = "eda",
        create_eda_ui("eda")
      ),
      
      # DEA Tab
      bs4Dash::tabItem(
        tabName = "dea",
        create_dea_ui("dea")
      ),
      
      # Pathway Analysis Tab
      bs4Dash::tabItem(
        tabName = "pathway",
        create_pathway_ui("pathway")
      ),
      
      # AI Interpretation Tab
      bs4Dash::tabItem(
        tabName = "ai_interpretation",
        create_ai_interpretation_ui("ai_interpretation")
      )
    )
  )
} 