library(testthat)

test_verify_components <- function() {
  # 1. Check file structure
  test_that("Required files exist", {
    required_files <- c(
      "app.R",
      "global.R",
      "config.R",
      ".env",
      "R/ui_components.R",
      "R/modules/dashboard_ui.R",
      "R/modules/dashboard_server.R",
      "R/modules/eda_ui.R",
      "R/modules/eda_server.R",
      "R/modules/dea_ui.R",
      "R/modules/dea_server.R",
      "R/modules/pathway_ui.R",
      "R/modules/pathway_server.R",
      "R/modules/ai_interpretation_ui.R",
      "R/modules/ai_interpretation_server.R"
    )
    
    for (file in required_files) {
      expect_true(file.exists(file), 
                 info = sprintf("Missing required file: %s", file))
    }
  })

  # 2. Test package dependencies
  test_that("Required packages are available", {
    required_packages <- c(
      "shiny",
      "bs4Dash",
      "DT",
      "plotly",
      "ggplot2",
      "reactable",
      "httr",
      "jsonlite",
      "base64enc",
      "openai"
    )
    
    for (pkg in required_packages) {
      expect_true(require(pkg, character.only = TRUE),
                 info = sprintf("Package not available: %s", pkg))
    }
  })

  # 3. Test OpenAI API configuration
  test_that("OpenAI API is configured", {
    expect_true(file.exists(".env"), "Missing .env file")
    env_contents <- readLines(".env")
    expect_true(any(grepl("^OPENAI_API_KEY=", env_contents)), 
               "Missing OPENAI_API_KEY in .env")
  })

  # 4. Test module loading
  test_that("Modules can be loaded", {
    expect_error({
      source("R/modules/eda_ui.R")
      source("R/modules/eda_server.R")
      source("R/modules/dea_ui.R")
      source("R/modules/dea_server.R")
    }, NA)
  })

  # 5. Test app initialization
  test_that("App can initialize", {
    expect_error({
      source("app.R")
    }, NA)
  })
}

# Run verification
cat("Starting component verification...\n")
test_results <- test_verify_components()
cat("Verification complete.\n") 