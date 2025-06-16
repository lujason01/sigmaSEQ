# Dashboard Code for R -------------

# Dashboard tab ----

tabItem(
  tabName = "dashboard",
  
  ## Info boxes ----
  fluidRow(
    
    column(
      width = 4,
      infoBox(
        width = 12,
        title = "Column 1",
        value = counts_rawdata,
        icon = icon("list"),
        color = "primary"
      )
    ),
    
    column(
      width = 4,
      infoBox(
        width = 12,
        title = "EXploratory Data Analysis",
        value = counts_rawdata,
        icon = icon("dove"),
        color = "primary"
      )
    ),
    
    column(
      width = 4,
      infoBox(
        width = 12,
        title = "Biological Pathway Analysis",
        value = counts_rawdata,
        icon = icon("location-dot"),
        color = "primary"
      )
    )
    
  ),
  
  ## Sortable boxes ---- (movable or draggable)
  fluidRow(
    sortable(
      width = 6,
      
      box(
        title = "Significantly Expressed Genes", 
        width = 12, 
        status = "olive",
        collapsible = FALSE, 
        ribbon(
          text = "NEW",
          color = "olive"
        )
      ),
      
      box(
        title = "DEA - Differential Expression Analysis",
        width = 12, 
        closable = TRUE, 
        status = "olive"
      )
      
      
    ),
    
    sortable(
      width = 6,
      
      box(
        title = "Box 3: ghg",
        width = 12,  
        status = "olive",
        collapsible = FALSE,
        maximizable = TRUE,
        
        
        
      ),
      
      box(
        title = "Box 4: ",
        width = 12, 
        status = "olive",
        collapsible = FALSE, 
        label = boxLabel(
          text = "Label", 
          status = "primary", 
          tooltip = "I'm a label!"),
        
        sidebar = boxSidebar(
          id = "",
          numericInput(
            inputId = "counts_rawdata",
            label = "Show Top N",
            value = 6,
            min = 1,
            max = 50,
            width = "97%"
          )
        )
        
      )
      
    )
  ),
  
  ## Tab box ----
  tabBox(
    title = "Data",
    width = 12,
    type = "tabs",
    status = "olive",
    solidHeader = TRUE
    
    
  )
)

# In app.R, we only load what we need
source("R/modules/eda_ui.R")  # Load EDA UI when needed
source("R/modules/dea_ui.R")  # Load DEA UI when needed

# Create test directory if it doesn't exist
dir.create("tests", showWarnings = FALSE)

# Save test files
writeLines(test_api_connection_code, "tests/test_api_connection.R")
writeLines(test_module_integration_code, "tests/test_module_integration.R")
writeLines(test_shiny_app_code, "tests/test_shiny_app.R")

# Run tests
library(testthat)

# Run API test
source("tests/test_api_connection.R")

# Run module integration test
source("tests/test_module_integration.R")

# Run Shiny app test
source("tests/test_shiny_app.R")

