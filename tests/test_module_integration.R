library(testthat)
library(shiny)

# Load all required modules
source("R/modules/eda_ui.R")
source("R/modules/eda_server.R")
source("R/modules/dea_server.R")

# Create test data
create_test_data <- function() {
  counts <- matrix(
    rpois(1000, lambda = 10),
    nrow = 100,
    ncol = 10,
    dimnames = list(
      paste0("gene", 1:100),
      paste0("sample", 1:10)
    )
  )
  
  metadata <- data.frame(
    sample = colnames(counts),
    condition = rep(c("control", "treatment"), each = 5)
  )
  
  list(counts = counts, metadata = metadata)
}

test_that("Module integration works", {
  test_data <- create_test_data()
  
  testServer(eda_server, args = list(
    data_state = reactiveVal(list(
      count_matrix = test_data$counts,
      metadata = test_data$metadata
    ))
  ), {
    # Test EDA functionality
    session$setInputs(normMethod = "tmm")
    expect_false(is.null(normalized_data()))
    
    # Test PCA calculation
    expect_false(is.null(pca_results()))
  })
}) 