#' EDA Server Logic
#' @param id Module ID
#' @param data_state Reactive value containing data state
#' @return Reactive value containing EDA state
#' @export
create_eda_server <- function(id, data_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define namespace
    
    # Reactive values for EDA state
    eda_state <- reactiveVal(list(
      normalized_data = NULL,
    transformed_data = NULL,
      pca_results = NULL,
    quality_metrics = NULL,
    is_valid = FALSE,
    error_message = NULL
    ))
    
  # Input validation
    observe({
    req(data_state())
    validate(
      need(!is.null(data_state()$count_matrix), "Please upload data first"),
      need(input$normalizationMethod, "Please select a normalization method"),
      need(input$transformationMethod, "Please select a transformation method"),
      need(is.numeric(input$minCount) && input$minCount >= 0, "Minimum count must be a non-negative number")
      )
    })
    
  # Helper function to normalize data
  normalize_data <- function(count_data, method) {
      withProgress(message = 'Normalizing data...', {
        tryCatch({
        switch(method,
          "deseq2" = {
            dds <- DESeq2::DESeqDataSetFromMatrix(
              countData = count_data,
              colData = data_state()$metadata,
              design = ~1
            )
            dds <- DESeq2::estimateSizeFactors(dds)
            DESeq2::counts(dds, normalized = TRUE)
          },
          "edger" = {
            dge <- edgeR::DGEList(counts = count_data)
            dge <- edgeR::calcNormFactors(dge)
            edgeR::cpm(dge, normalized.lib.sizes = TRUE)
          },
          "tmm" = {
            dge <- edgeR::DGEList(counts = count_data)
            dge <- edgeR::calcNormFactors(dge, method = "TMM")
            edgeR::cpm(dge, normalized.lib.sizes = TRUE)
          },
          "rle" = {
            dge <- edgeR::DGEList(counts = count_data)
            dge <- edgeR::calcNormFactors(dge, method = "RLE")
            edgeR::cpm(dge, normalized.lib.sizes = TRUE)
          }
        )
        }, error = function(e) {
          showNotification(
          paste("Error normalizing data:", e$message),
            type = "error"
          )
          NULL
        })
      })
  }
    
  # Helper function to transform data
  transform_data <- function(normalized_data, method) {
    withProgress(message = 'Transforming data...', {
      tryCatch({
      switch(method,
          "log2" = log2(normalized_data + 1),
          "vst" = DESeq2::varianceStabilizingTransformation(normalized_data),
          "rlog" = DESeq2::rlog(normalized_data)
        )
      }, error = function(e) {
        showNotification(
          paste("Error transforming data:", e$message),
          type = "error"
        )
        NULL
      })
    })
    }
    
  # Helper function to perform PCA
  perform_pca <- function(transformed_data) {
    withProgress(message = 'Performing PCA...', {
      tryCatch({
        pca <- prcomp(t(transformed_data), scale. = TRUE)
        list(
          scores = pca$x,
          loadings = pca$rotation,
          variance = pca$sdev^2 / sum(pca$sdev^2)
        )
      }, error = function(e) {
        showNotification(
          paste("Error performing PCA:", e$message),
          type = "error"
        )
        NULL
      })
    })
  }

  # Helper function to calculate quality metrics
  calculate_quality_metrics <- function(count_data, normalized_data) {
    withProgress(message = 'Calculating quality metrics...', {
      tryCatch({
        list(
          total_genes = nrow(count_data),
          filtered_genes = nrow(normalized_data),
          quality_score = mean(colSums(normalized_data > 0) / nrow(normalized_data))
        )
      }, error = function(e) {
        showNotification(
          paste("Error calculating quality metrics:", e$message),
          type = "error"
        )
        NULL
      })
    })
  }

  # Handle analysis execution
  observeEvent(input$runAnalysis, {
    req(data_state()$count_data, input$normalizationMethod, input$transformationMethod)
    
    withProgress(message = 'Running EDA analysis...', {
      # Normalize data
      normalized_data <- normalize_data(data_state()$count_data, input$normalizationMethod)
      
      if (!is.null(normalized_data)) {
        # Transform data
        transformed_data <- transform_data(normalized_data, input$transformationMethod)
        
        if (!is.null(transformed_data)) {
          # Perform PCA
          pca_results <- perform_pca(transformed_data)
          
          # Calculate quality metrics
          quality_metrics <- calculate_quality_metrics(
            data_state()$count_data,
            normalized_data
          )
          
          # Update EDA state
          eda_state(list(
            normalized_data = normalized_data,
            transformed_data = transformed_data,
            pca_results = pca_results,
            quality_metrics = quality_metrics,
            is_valid = TRUE,
            error_message = NULL
          ))
          
          showNotification(
            "EDA analysis completed successfully",
            type = "success"
      )
        }
      }
    })
  })

  # Output: Data Quality Summary
  output$totalGenes <- renderbs4ValueBox({
    req(eda_state()$quality_metrics)
    bs4ValueBox(
      value = eda_state()$quality_metrics$total_genes,
      subtitle = "Total Genes",
      icon = icon("dna"),
      color = "primary"
      )
    })

  output$filteredGenes <- renderbs4ValueBox({
    req(eda_state()$quality_metrics)
    bs4ValueBox(
      value = eda_state()$quality_metrics$filtered_genes,
      subtitle = "Filtered Genes",
        icon = icon("filter"),
      color = "success"
      )
    })

  output$qualityScore <- renderbs4ValueBox({
    req(eda_state()$quality_metrics)
    bs4ValueBox(
      value = round(eda_state()$quality_metrics$quality_score * 100, 1),
      subtitle = "Quality Score (%)",
      icon = icon("chart-line"),
      color = "info"
    )
    })

  # Output: Sample Clustering
  output$sampleClustering <- renderPlotly({
    req(eda_state()$transformed_data)
      
    withProgress(message = 'Creating sample clustering plot...', {
      tryCatch({
        # Calculate correlation matrix
        cor_matrix <- cor(eda_state()$transformed_data)
      
      # Create heatmap
        p <- plot_ly(
        z = cor_matrix,
        type = "heatmap",
          colors = "RdBu"
      ) %>%
        layout(
            title = "Sample Correlation Heatmap",
            xaxis = list(title = "Sample"),
            yaxis = list(title = "Sample")
          )
        
        p
      }, error = function(e) {
        showNotification(
          paste("Error creating sample clustering plot:", e$message),
          type = "error"
        )
        NULL
      })
    })
  })

  # Output: Expression Distribution
  output$expressionDistribution <- renderPlotly({
    req(eda_state()$transformed_data)
    
    withProgress(message = 'Creating expression distribution plot...', {
      tryCatch({
        # Calculate mean expression per gene
        mean_expr <- rowMeans(eda_state()$transformed_data)
        
        # Create density plot
        p <- plot_ly(
          x = mean_expr,
          type = "histogram",
          nbinsx = 50
        ) %>%
          layout(
            title = "Expression Distribution",
            xaxis = list(title = "Mean Expression"),
            yaxis = list(title = "Count")
          )
        
        p
      }, error = function(e) {
        showNotification(
          paste("Error creating expression distribution plot:", e$message),
          type = "error"
        )
        NULL
      })
    })
    })

    # Output: PCA Plot
    output$pcaPlot <- renderPlotly({
    req(eda_state()$pca_results)
    
    withProgress(message = 'Creating PCA plot...', {
      tryCatch({
        # Create scatter plot
        p <- plot_ly(
          x = eda_state()$pca_results$scores[, 1],
          y = eda_state()$pca_results$scores[, 2],
          type = "scatter",
          mode = "markers",
          text = rownames(eda_state()$pca_results$scores)
        ) %>%
          layout(
            title = "Principal Component Analysis",
            xaxis = list(
              title = paste0("PC1 (", round(eda_state()$pca_results$variance[1] * 100, 1), "%)")
            ),
            yaxis = list(
              title = paste0("PC2 (", round(eda_state()$pca_results$variance[2] * 100, 1), "%)")
        )
          )
        
        p
      }, error = function(e) {
        showNotification(
          paste("Error creating PCA plot:", e$message),
          type = "error"
        )
        NULL
      })
    })
    })

  # Output: Results Table
  output$resultsTable <- renderDT({
    req(eda_state()$transformed_data)
    
    withProgress(message = 'Rendering results table...', {
      tryCatch({
        # Create summary statistics
        summary_stats <- data.frame(
          Gene = rownames(eda_state()$transformed_data),
          Mean = rowMeans(eda_state()$transformed_data),
          SD = apply(eda_state()$transformed_data, 1, sd),
          Min = apply(eda_state()$transformed_data, 1, min),
          Max = apply(eda_state()$transformed_data, 1, max)
        )
        
        datatable(
          summary_stats,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            autoWidth = TRUE
          )
        )
      }, error = function(e) {
        showNotification(
          paste("Error rendering results table:", e$message),
          type = "error"
        )
        NULL
      })
    })
  })
  
  # Download handlers for plots
  output$download_qc_plot <- downloadHandler(
    filename = function() {
      paste("qc_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
      req(eda_state()$quality_metrics)
      p <- ggplot(eda_state()$quality_metrics$qc_data, aes(x = Sample, y = Value)) +
        geom_boxplot() +
        theme_minimal()
      ggsave(file, p, width = 10, height = 6, dpi = 300)
    }
  )

  output$download_clustering_plot <- downloadHandler(
    filename = function() {
      paste("clustering_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
      req(eda_state()$transformed_data)
      p <- plot_ly(
        z = cor(eda_state()$transformed_data),
        type = "heatmap",
        colors = "RdBu"
      ) %>%
        layout(
          title = "Sample Correlation Heatmap",
          xaxis = list(title = "Sample"),
          yaxis = list(title = "Sample")
        )
      export(p, file = file)
    }
  )

  output$download_pca_plot <- downloadHandler(
    filename = function() {
      paste("pca_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
      req(eda_state()$pca_results)
      p <- plot_ly(
        x = eda_state()$pca_results$scores[, 1],
        y = eda_state()$pca_results$scores[, 2],
        type = "scatter",
        mode = "markers",
        text = rownames(eda_state()$pca_results$scores)
      ) %>%
        layout(
          title = "Principal Component Analysis",
          xaxis = list(
            title = paste0("PC1 (", round(eda_state()$pca_results$variance[1] * 100, 1), "%)")
          ),
          yaxis = list(
            title = paste0("PC2 (", round(eda_state()$pca_results$variance[2] * 100, 1), "%)")
          )
        )
      export(p, file = file)
    }
  )

  output$download_heatmap <- downloadHandler(
    filename = function() {
      paste("heatmap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
      req(eda_state()$transformed_data)
      p <- plot_ly(
        z = eda_state()$transformed_data,
        type = "heatmap",
        colors = colorRamp(c("#4575B4", "#FFFFBF", "#D73027"))
      ) %>%
        layout(
          title = "Expression Heatmap",
          xaxis = list(title = "Sample"),
          yaxis = list(title = "Gene")
        )
      export(p, file = file)
    }
  )
  
  # Return the EDA state
  reactive({
    eda_state()
  })
  })
}

#' Alias for create_eda_server
#' @export
eda_server <- create_eda_server 