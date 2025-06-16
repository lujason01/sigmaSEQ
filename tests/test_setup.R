 library(testthat)

test_that("Directory structure is correct", {
  required_dirs <- c(
    "R/modules",
    "R/helpers",
    "data",
    "results",
    "logs",
    "www",
    "tests"
  )
  
  for (dir in required_dirs) {
    expect_true(dir.exists(dir))
  }
})

test_that("Configuration is valid", {
  config <- get_config()
  expect_type(config, "list")
  expect_true("app_name" %in% names(config))
  expect_true("app_version" %in% names(config))
})

test_that("Required packages are available", {
  required_packages <- c(
    "shiny",
    "bs4Dash",
    "DT",
    "plotly",
    "ggplot2"
  )
  
  for (package in required_packages) {
    expect_true(require(package, character.only = TRUE))
  }
})

test_that("Module files exist", {
  module_files <- c(
    "R/modules/eda_ui.R",
    "R/modules/eda_server.R",
    "R/modules/dea_ui.R",
    "R/modules/dea_server.R",
    "R/modules/pathway_ui.R",
    "R/modules/pathway_server.R",
    "R/modules/ai_interpretation_ui.R",
    "R/modules/ai_interpretation_server.R"
  )
  
  for (file in module_files) {
    expect_true(file.exists(file))
  }
})

test_that("Configuration is properly loaded", {
  config <- get_config()
  expect_type(config, "list")
  expect_true(!is.null(config$app_name))
  expect_true(!is.null(config$app_version))
  expect_true(!is.null(config$theme))
  expect_true(!is.null(config$default_parameters))
})