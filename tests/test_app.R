test_application <- function() {
  library(shiny)
  library(testthat)
  
  # Load test data
  test_data <- generate_test_data()
  
  # Test EDA module
  test_that("EDA module works", {
    # Create a test environment
    testServer(eda_server, {
      # Simulate data upload
      session$setInputs(
        normMethod = "tmm",
        minCounts = 10,
        minSamples = 3
      )
      
      # Check if normalized data is created
      expect_true(!is.null(normalized_data()))
      
      # Check if PCA is calculated
      expect_true(!is.null(pca_results()))
    })
  })
  
  # Test DEA module
  test_that("DEA module works", {
    testServer(dea_server, {
      # Simulate DEA analysis
      session$setInputs(
        condition1 = "Control",
        condition2 = "Treatment",
        deaMethod = "deseq2",
        pvalThreshold = 0.05,
        fcThreshold = 1
      )
      
      # Check if results are generated
      expect_true(!is.null(dea_state()$results))
    })
  })
  
  # Test Pathway module
  test_that("Pathway module works", {
    testServer(pathway_server, {
      # Simulate pathway analysis
      session$setInputs(
        pathwayType = "go",
        goCategories = "BP",
        pvalueCutoff = 0.05,
        qvalueCutoff = 0.1
      )
      
      # Check if results are generated
      expect_true(!is.null(pathway_state()$results))
    })
  })
} 