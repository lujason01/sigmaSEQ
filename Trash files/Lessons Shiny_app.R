

# AI Shiny app lessons with boxes and transparent container

# To organize your bs4Dash interface with transparent boxes containing other boxes, you can use the bs4Card function with a nested structure


library(shiny)
library(bs4Dash)
library(DT)

# Sample data for the table
sample_data <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "David", "Eva"),
  Age = c(28, 34, 22, 45, 31),
  Position = c("Analyst", "Manager", "Developer", "CEO", "Designer"),
  Experience = c(3, 8, 1, 15, 5)
)

ui <- dashboardPage(
  dashboardHeader(title = "bs4Dash Nested Boxes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wildlife Data", tabName = "data", icon = icon("table")),
      menuItem("Nature Gallery", tabName = "gallery", icon = icon("images"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Species Information Table",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  DT::dataTableOutput("sample_table"),
                  footer = "Data last updated: April 2025"
                )
              )
      ),
      
      tabItem(tabName = "gallery",
              # Transparent container box
              bs4Card(
                title = "Nature Photography Section",
                width = 12,
                status = NULL,  # No background color
                solidHeader = FALSE,
                headerBorder = FALSE,
                background = NULL,
                
                # First row of nested boxes
                fluidRow(
                  box(
                    title = "Mountain Landscape",
                    width = 6,
                    status = "info",
                    solidHeader = TRUE,
                    div(style = "text-align: center;",
                        img(src = "https://via.placeholder.com/350x200?text=Mountain+Landscape", 
                            alt = "Mountain Landscape", width = "100%")
                    ),
                    footer = "Placeholder image for mountains"
                  ),
                  box(
                    title = "Ocean View",
                    width = 6,
                    status = "success",
                    solidHeader = TRUE,
                    div(style = "text-align: center;",
                        img(src = "https://via.placeholder.com/350x200?text=Ocean+View", 
                            alt = "Ocean View", width = "100%")
                    ),
                    footer = "Placeholder image for ocean"
                  )
                )
              ),
              
              # Another transparent container box for a different section
              bs4Card(
                title = "Wildlife Photography Section",
                width = 12,
                status = NULL,
                solidHeader = FALSE,
                headerBorder = FALSE,
                background = NULL,
                
                # Second row of nested boxes
                fluidRow(
                  box(
                    title = "Bird Species",
                    width = 6,
                    status = "warning",
                    solidHeader = TRUE,
                    div(style = "text-align: center;",
                        img(src = "https://via.placeholder.com/350x200?text=Birds", 
                            alt = "Birds", width = "100%")
                    ),
                    footer = "Placeholder image for birds"
                  ),
                  box(
                    title = "Forest Animals",
                    width = 6,
                    status = "danger",
                    solidHeader = TRUE,
                    div(style = "text-align: center;",
                        img(src = "https://via.placeholder.com/350x200?text=Forest+Animals", 
                            alt = "Forest Animals", width = "100%")
                    ),
                    footer = "Placeholder image for forest animals"
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  output$sample_table <- DT::renderDataTable({
    DT::datatable(sample_data, options = list(pageLength = 5))
  })
}

shinyApp(ui = ui, server = server)