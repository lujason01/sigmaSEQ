# Load required packages
if (!require("shiny")) install.packages("shiny")
if (!require("bs4Dash")) install.packages("bs4Dash")
if (!require("config")) install.packages("config")

# Source the main app file
source("app.R")

# Run the app
shiny::runApp() 