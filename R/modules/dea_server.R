#' DEA Server Logic
#' @param id Module ID
#' @param data_state Reactive value containing input data state
#' @param eda_state Reactive value containing EDA state
#' @return Reactive value containing DEA state
#' @export
create_dea_server <- function(id, data_state, eda_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define namespace
    
  # Load required packages
    required_packages <- c("DESeq2", "edgeR", "limma", "plotly", "DT")
    for (pkg in required_packages) {
      if (!require(pkg, character.only = TRUE)) {
        showNotification(
          sprintf("Required package %s is not available", pkg),
          type = "error"
        )
        return(NULL)
      }
    }
  
  # Reactive values for DEA state
  dea_state <- reactiveVal(list(
    results = NULL,
    significant_genes = NULL,
    volcano_data = NULL
  ))

  # Input validation
  observe({
      req(input$pvalThreshold, input$fcThreshold)
      
    validate(
        need(is.numeric(input$pvalThreshold) && input$pvalThreshold >= 0 && input$pvalThreshold <= 1,
           "P-value threshold must be between 0 and 1"),
        need(is.numeric(input$fcThreshold) && input$fcThreshold >= 0,
           "Fold change threshold must be non-negative")
    )
  })

  # Update condition choices based on metadata
  observe({
    req(data_state$metadata)
    
    conditions <- unique(data_state$metadata$condition)
    updateSelectizeInput(session, "condition1", choices = conditions)
    updateSelectizeInput(session, "condition2", choices = conditions)
  })

  # Helper functions for DEA methods
  run_deseq2_analysis <- function(counts, design) {
    withProgress(message = 'Running DESeq2 analysis...', {
        tryCatch({
      dds <- DESeqDataSetFromMatrix(
        countData = round(counts),
        colData = design,
        design = ~condition
      )
      dds <- DESeq(dds)
      results(dds, tidy = TRUE)
        }, error = function(e) {
          showNotification(
            paste("Error in DESeq2 analysis:", e$message),
            type = "error"
          )
          NULL
        })
    })
  }

  run_edger_analysis <- function(counts, design) {
    withProgress(message = 'Running edgeR analysis...', {
        tryCatch({
      y <- DGEList(counts = counts, group = design$condition)
      y <- calcNormFactors(y)
      design_matrix <- model.matrix(~condition, data = design)
      y <- estimateDisp(y, design_matrix)
      fit <- glmQLFit(y, design_matrix)
      qlf <- glmQLFTest(fit)
      topTags(qlf, n = nrow(counts)) %>% as.data.frame()
        }, error = function(e) {
          showNotification(
            paste("Error in edgeR analysis:", e$message),
            type = "error"
          )
          NULL
        })
    })
  }

  run_limma_analysis <- function(counts, design) {
    withProgress(message = 'Running limma-voom analysis...', {
        tryCatch({
      design_matrix <- model.matrix(~condition, data = design)
      v <- voom(counts, design_matrix)
      fit <- lmFit(v, design_matrix)
      fit <- eBayes(fit)
      topTable(fit, coef = 2, number = nrow(counts)) %>% as.data.frame()
        }, error = function(e) {
          showNotification(
            paste("Error in limma-voom analysis:", e$message),
            type = "error"
          )
          NULL
        })
    })
  }

  # Perform DEA
  observeEvent(input$runDEA, {
    req(eda_state()$normalized_data)
    req(input$condition1, input$condition2)
    req(input$deaMethod)
    
    # Validate conditions are different
    validate(
      need(input$condition1 != input$condition2,
           "Please select different conditions for comparison")
    )
    
    withProgress(message = 'Performing differential expression analysis...', {
      counts <- eda_state()$normalized_data
      
      design <- data_state$metadata %>%
        filter(condition %in% c(input$condition1, input$condition2)) %>%
        mutate(condition = factor(condition, levels = c(input$condition1, input$condition2)))
      
      results <- tryCatch({
        switch(input$deaMethod,
          "deseq2" = run_deseq2_analysis(counts, design),
          "edger" = run_edger_analysis(counts, design),
          "limma" = run_limma_analysis(counts, design)
        )
      }, error = function(e) {
        showNotification(
          paste("Error in DEA:", e$message),
          type = "error",
          duration = NULL
        )
        NULL
      })
      
      if (!is.null(results)) {
        # Standardize column names
        results <- results %>%
          rename_with(
            ~case_when(
              . %in% c("log2FoldChange", "logFC") ~ "log2FoldChange",
              . %in% c("padj", "FDR", "adj.P.Val") ~ "padj",
              TRUE ~ .
            )
          )
        
        # Update DEA state
        dea_state(list(
          results = results,
          significant_genes = results %>%
            filter(padj < input$pvalThreshold,
                   abs(log2FoldChange) > input$fcThreshold),
          volcano_data = results
        ))
        
        showNotification(
          "Differential expression analysis completed successfully",
          type = "success"
        )
      }
    })
  })

  # Output: Summary Statistics
    output$totalDE <- renderbs4ValueBox({
    req(dea_state()$significant_genes)
      bs4ValueBox(
        value = nrow(dea_state()$significant_genes),
        subtitle = "Total DE Genes",
      icon = icon("dna"),
      color = "primary"
    )
  })

    output$upregulated <- renderbs4ValueBox({
    req(dea_state()$significant_genes)
      bs4ValueBox(
        value = sum(dea_state()$significant_genes$log2FoldChange > 0),
        subtitle = "Up-regulated",
      icon = icon("arrow-up"),
      color = "success"
    )
  })

    output$downregulated <- renderbs4ValueBox({
    req(dea_state()$significant_genes)
      bs4ValueBox(
        value = sum(dea_state()$significant_genes$log2FoldChange < 0),
        subtitle = "Down-regulated",
      icon = icon("arrow-down"),
      color = "danger"
    )
  })

  # Output: Volcano Plot
  output$volcanoPlot <- renderPlotly({
    req(dea_state()$volcano_data)
    
      withProgress(message = 'Creating volcano plot...', {
        tryCatch({
    results <- dea_state()$volcano_data
    
    # Add significance categories
    results$Category <- case_when(
      results$padj < input$pvalThreshold & results$log2FoldChange > input$fcThreshold ~ "Up-regulated",
      results$padj < input$pvalThreshold & results$log2FoldChange < -input$fcThreshold ~ "Down-regulated",
      TRUE ~ "Not significant"
    )
    
    # Create plot
    p <- ggplot(results, aes(x = log2FoldChange, y = -log10(padj),
                            color = Category, text = rownames(results))) +
      geom_point(size = input$volcanoPointSize, alpha = 0.6) +
      scale_color_manual(values = c(
        "Up-regulated" = "#FF4B4B",
        "Down-regulated" = "#4B4BFF",
        "Not significant" = "#CCCCCC"
      )) +
      theme_minimal() +
      labs(x = "Log2 Fold Change", y = "-Log10 Adjusted P-value")
    
    ggplotly(p, tooltip = c("text", "x", "y"))
        }, error = function(e) {
          showNotification(
            paste("Error creating volcano plot:", e$message),
            type = "error"
          )
          NULL
        })
      })
  })

  # Output: MA Plot
  output$maPlot <- renderPlotly({
    req(dea_state()$results)
    
      withProgress(message = 'Creating MA plot...', {
        tryCatch({
    results <- dea_state()$results
    
    # Calculate mean expression
    results$baseMean <- rowMeans(eda_state()$normalized_data)
    
    p <- ggplot(results, aes(x = log2(baseMean), y = log2FoldChange,
                            text = rownames(results))) +
      geom_point(aes(color = padj < input$pvalThreshold), alpha = 0.6) +
      scale_color_manual(values = c("TRUE" = "#FF4B4B", "FALSE" = "#CCCCCC")) +
      theme_minimal() +
      labs(x = "Log2 Mean Expression", y = "Log2 Fold Change")
    
    ggplotly(p, tooltip = c("text", "x", "y"))
        }, error = function(e) {
          showNotification(
            paste("Error creating MA plot:", e$message),
            type = "error"
          )
          NULL
        })
      })
  })

  # Output: Expression Heatmap
  output$expressionHeatmap <- renderPlotly({
    req(dea_state()$significant_genes, eda_state()$normalized_data)
    
      withProgress(message = 'Creating expression heatmap...', {
        tryCatch({
    # Get top genes
    top_genes <- dea_state()$significant_genes %>%
      arrange(padj) %>%
      head(input$topGenes) %>%
      rownames()
    
    # Get expression data for top genes
    expr_data <- eda_state()$normalized_data[top_genes, ]
    
    # Scale rows
    expr_data_scaled <- t(scale(t(expr_data)))
    
    # Create heatmap
    plot_ly(
      x = colnames(expr_data),
      y = rownames(expr_data),
      z = expr_data_scaled,
      type = "heatmap",
      colors = colorRamp(c("#4575B4", "#FFFFBF", "#D73027")),
      hoverongaps = FALSE
    ) %>%
      layout(
        title = "Expression Heatmap of Top DE Genes",
        xaxis = list(tickangle = 45),
        yaxis = list(tickangle = 0)
      )
        }, error = function(e) {
          showNotification(
            paste("Error creating expression heatmap:", e$message),
            type = "error"
          )
          NULL
        })
      })
  })

  # Output: Results Table
  output$deaResults <- renderDT({
    req(dea_state()$results)
    
      withProgress(message = 'Rendering results table...', {
        tryCatch({
    datatable(
      dea_state()$results,
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
    output$download_volcano_plot <- downloadHandler(
      filename = function() {
        paste("volcano_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
      },
      content = function(file) {
        req(dea_state()$volcano_data)
        results <- dea_state()$volcano_data
        results$Category <- case_when(
          results$padj < input$pvalThreshold & results$log2FoldChange > input$fcThreshold ~ "Up-regulated",
          results$padj < input$pvalThreshold & results$log2FoldChange < -input$fcThreshold ~ "Down-regulated",
          TRUE ~ "Not significant"
        )
        
        p <- ggplot(results, aes(x = log2FoldChange, y = -log10(padj),
                                color = Category, text = rownames(results))) +
          geom_point(size = input$volcanoPointSize, alpha = 0.6) +
          scale_color_manual(values = c(
            "Up-regulated" = "#FF4B4B",
            "Down-regulated" = "#4B4BFF",
            "Not significant" = "#CCCCCC"
          )) +
          theme_minimal() +
          labs(x = "Log2 Fold Change", y = "-Log10 Adjusted P-value")
        
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )

    output$download_ma_plot <- downloadHandler(
      filename = function() {
        paste("ma_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
      },
      content = function(file) {
        req(dea_state()$results)
        results <- dea_state()$results
        results$baseMean <- rowMeans(eda_state()$normalized_data)
        
        p <- ggplot(results, aes(x = log2(baseMean), y = log2FoldChange,
                                text = rownames(results))) +
          geom_point(aes(color = padj < input$pvalThreshold), alpha = 0.6) +
          scale_color_manual(values = c("TRUE" = "#FF4B4B", "FALSE" = "#CCCCCC")) +
          theme_minimal() +
          labs(x = "Log2 Mean Expression", y = "Log2 Fold Change")
        
        ggsave(file, p, width = 10, height = 6, dpi = 300)
      }
    )

    output$download_heatmap <- downloadHandler(
    filename = function() {
        paste("expression_heatmap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
        req(dea_state()$significant_genes, eda_state()$normalized_data)
        top_genes <- dea_state()$significant_genes %>%
          arrange(padj) %>%
          head(input$topGenes) %>%
          rownames()
        
        expr_data <- eda_state()$normalized_data[top_genes, ]
        expr_data_scaled <- t(scale(t(expr_data)))
        
        p <- plot_ly(
          x = colnames(expr_data),
          y = rownames(expr_data),
          z = expr_data_scaled,
          type = "heatmap",
          colors = colorRamp(c("#4575B4", "#FFFFBF", "#D73027")),
          hoverongaps = FALSE
        ) %>%
          layout(
            title = "Expression Heatmap of Top DE Genes",
            xaxis = list(tickangle = 45),
            yaxis = list(tickangle = 0)
          )
        
        export(p, file = file)
      }
    )
    
    # Return the DEA state
    reactive({
      dea_state()
    })
  })
}

#' Alias for create_dea_server
#' @export
dea_server <- create_dea_server 