# DESeq2 Analysis
run_deseq2_analysis <- function(counts, design) {
  library(DESeq2)
  
  # Create DESeq2 object
  dds <- DESeqDataSetFromMatrix(
    countData = round(counts),
    colData = design,
    design = ~ condition
  )
  
  # Run analysis
  dds <- DESeq(dds)
  
  # Get results
  res <- results(dds, contrast = c("condition", levels(design$condition)[2], levels(design$condition)[1]))
  
  # Convert to data frame
  as.data.frame(res) %>%
    rownames_to_column("gene") %>%
    arrange(padj)
}

# edgeR Analysis
run_edger_analysis <- function(counts, design) {
  library(edgeR)
  
  # Create DGEList object
  dge <- DGEList(counts = counts)
  
  # Calculate normalization factors
  dge <- calcNormFactors(dge)
  
  # Create design matrix
  design_matrix <- model.matrix(~ condition, data = design)
  
  # Estimate dispersion
  dge <- estimateDisp(dge, design_matrix)
  
  # Fit model
  fit <- glmQLFit(dge, design_matrix)
  
  # Test for differential expression
  qlf <- glmQLFTest(fit, coef = 2)
  
  # Get results
  topTags(qlf, n = Inf) %>%
    as.data.frame() %>%
    rownames_to_column("gene")
}

# limma-voom Analysis
run_limma_analysis <- function(counts, design) {
  library(limma)
  
  # Create design matrix
  design_matrix <- model.matrix(~ condition, data = design)
  
  # Apply voom transformation
  v <- voom(counts, design_matrix, plot = FALSE)
  
  # Fit linear model
  fit <- lmFit(v, design_matrix)
  
  # Apply empirical Bayes smoothing
  fit <- eBayes(fit)
  
  # Get results
  topTable(fit, coef = 2, number = Inf) %>%
    rownames_to_column("gene")
} 