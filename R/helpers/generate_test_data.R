#' Test Data Generation
#' @description Functions to generate test data for the application
#' @author Your Name
#' @version 1.0.0

#' Generate random count matrix
#' @param n_genes Number of genes
#' @param n_samples Number of samples
#' @return Matrix of simulated count data
generate_count_matrix <- function(n_genes = 1000, n_samples = 6) {
  # Set seed for reproducibility
  set.seed(42)
  
  # Generate gene names
  gene_names <- paste0("Gene_", seq_len(n_genes))
  
  # Generate sample names
  sample_names <- paste0("Sample_", seq_len(n_samples))
  
  # Generate random counts
  counts <- matrix(
    rnbinom(n_genes * n_samples, size = 3, mu = 100),
    nrow = n_genes,
    ncol = n_samples,
    dimnames = list(gene_names, sample_names)
  )
  
  return(counts)
}

#' Generate sample metadata
#' @param n_samples Number of samples
#' @return Data frame of sample metadata
generate_metadata <- function(n_samples = 6) {
  # Generate condition labels
  conditions <- rep(c("Control", "Treatment"), each = n_samples/2)
  
  # Generate batch information
  batches <- rep(c("Batch1", "Batch2"), times = n_samples/2)
  
  # Create metadata data frame
  metadata <- data.frame(
    sample = paste0("Sample_", seq_len(n_samples)),
    condition = conditions,
    batch = batches,
    stringsAsFactors = FALSE
  )
  
  return(metadata)
}

#' Generate test dataset
#' @param save_files Logical indicating whether to save files
#' @return List containing count matrix and metadata
generate_test_dataset <- function(save_files = FALSE) {
  # Generate data
  counts <- generate_count_matrix()
  metadata <- generate_metadata()
  
  # Save files if requested
  if (save_files) {
    data_dir <- get_project_paths()$data
    
    # Save count matrix
    write.csv(
      counts,
      file = file.path(data_dir, "test_counts.csv")
    )
    
    # Save metadata
    write.csv(
      metadata,
      file = file.path(data_dir, "test_metadata.csv"),
      row.names = FALSE
    )
  }
  
  return(list(
    counts = counts,
    metadata = metadata
  ))
}

#' Load test dataset
#' @return List containing count matrix and metadata
load_test_dataset <- function() {
  data_dir <- get_project_paths()$data
  
  # Check if test files exist
  counts_file <- file.path(data_dir, "test_counts.csv")
  metadata_file <- file.path(data_dir, "test_metadata.csv")
  
  if (!file.exists(counts_file) || !file.exists(metadata_file)) {
    # Generate and save test data if files don't exist
    return(generate_test_dataset(save_files = TRUE))
  }
  
  # Load existing files
  counts <- as.matrix(read.csv(counts_file, row.names = 1))
  metadata <- read.csv(metadata_file)
  
  return(list(
    counts = counts,
    metadata = metadata
  ))
} 