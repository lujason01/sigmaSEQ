#' Pathway Analysis Server Logic
#' @param id Module ID
#' @param dea_state Reactive value containing DEA state
#' @return Reactive value containing pathway analysis state
#' @export
create_pathway_server <- function(id, dea_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define namespace
    
  # Load required packages
    required_packages <- c("clusterProfiler", "ReactomePA", "enrichplot", "plotly", "DT")
    for (pkg in required_packages) {
      if (!require(pkg, character.only = TRUE)) {
        if (pkg == "ReactomePA") {
          if (!require("BiocManager", quietly = TRUE)) {
            install.packages("BiocManager")
          }
          BiocManager::install("ReactomePA")
        } else {
          install.packages(pkg)
        }
        library(pkg, character.only = TRUE)
      }
    }
  
  # Reactive values for pathway state
  pathway_state <- reactiveVal(list(
    results = NULL,
      significant_pathways = NULL,
      enrichment_data = NULL,
      network_data = NULL
  ))

  # Input validation
  observe({
    req(input$pvalueCutoff, input$qvalueCutoff)
    
    validate(
      need(is.numeric(input$pvalueCutoff) && input$pvalueCutoff >= 0 && input$pvalueCutoff <= 1,
           "P-value cutoff must be between 0 and 1"),
      need(is.numeric(input$qvalueCutoff) && input$qvalueCutoff >= 0 && input$qvalueCutoff <= 1,
           "Q-value cutoff must be between 0 and 1")
    )
  })

    # Helper functions for pathway analysis methods
    run_gsea_analysis <- function(gene_list, gene_set) {
      withProgress(message = 'Running GSEA analysis...', {
        tryCatch({
          switch(gene_set,
            "kegg" = gseKEGG(
              geneList = gene_list,
              organism = "hsa",
              pvalueCutoff = input$pvalueCutoff,
              pAdjustMethod = "BH"
            ),
            "go" = gseGO(
              geneList = gene_list,
                OrgDb = org.Hs.eg.db,
              ont = "ALL",
              pvalueCutoff = input$pvalueCutoff,
              pAdjustMethod = "BH"
            ),
            "reactome" = gsePathway(
              geneList = gene_list,
              organism = "human",
              pvalueCutoff = input$pvalueCutoff,
              pAdjustMethod = "BH"
            )
          )
        }, error = function(e) {
          showNotification(
            paste("Error in GSEA analysis:", e$message),
            type = "error"
          )
          NULL
    })
      })
    }

    run_ora_analysis <- function(gene_list, gene_set) {
      withProgress(message = 'Running ORA analysis...', {
        tryCatch({
          switch(gene_set,
            "kegg" = enrichKEGG(
              gene = gene_list,
                organism = "hsa",
              pvalueCutoff = input$pvalueCutoff,
              pAdjustMethod = "BH"
            ),
            "go" = enrichGO(
              gene = gene_list,
              OrgDb = org.Hs.eg.db,
              ont = "ALL",
              pvalueCutoff = input$pvalueCutoff,
              pAdjustMethod = "BH"
            ),
            "reactome" = enrichPathway(
              gene = gene_list,
              organism = "human",
              pvalueCutoff = input$pvalueCutoff,
              pAdjustMethod = "BH"
            )
          )
        }, error = function(e) {
          showNotification(
            paste("Error in ORA analysis:", e$message),
            type = "error"
          )
          NULL
        })
      })
  }

    run_spia_analysis <- function(gene_list) {
      withProgress(message = 'Running SPIA analysis...', {
        tryCatch({
          spia(
            de = gene_list,
            all = rownames(dea_state()$results),
            organism = "hsa",
            data.dir = NULL,
            pathids = NULL,
            nB = 2000,
            plots = FALSE,
            verbose = TRUE
          )
        }, error = function(e) {
          showNotification(
            paste("Error in SPIA analysis:", e$message),
            type = "error"
          )
          NULL
        })
      })
  }

  # Perform pathway analysis
  observeEvent(input$runPathway, {
      req(dea_state()$results)
      req(input$pathwayMethod)
      req(input$geneSet)
    
    withProgress(message = 'Performing pathway analysis...', {
        # Prepare gene list
        gene_list <- dea_state()$results %>%
          arrange(padj) %>%
          pull(log2FoldChange)
        names(gene_list) <- rownames(dea_state()$results)
      
        # Run analysis based on selected method
      results <- tryCatch({
          switch(input$pathwayMethod,
            "gsea" = run_gsea_analysis(gene_list, input$geneSet),
            "ora" = run_ora_analysis(names(gene_list), input$geneSet),
            "spia" = run_spia_analysis(gene_list)
        )
      }, error = function(e) {
        showNotification(
          paste("Error in pathway analysis:", e$message),
          type = "error",
          duration = NULL
        )
        NULL
      })
      
      if (!is.null(results)) {
          # Update pathway state
        pathway_state(list(
          results = results,
            significant_pathways = results %>%
              filter(p.adjust < input$qvalueCutoff),
            enrichment_data = results,
            network_data = results
        ))
        
        showNotification(
          "Pathway analysis completed successfully",
          type = "success"
        )
      }
    })
  })

  # Output: Summary Statistics
    output$totalPathways <- renderbs4ValueBox({
    req(pathway_state()$results)
      bs4ValueBox(
        value = nrow(pathway_state()$results),
        subtitle = "Total Pathways",
      icon = icon("project-diagram"),
      color = "primary"
    )
  })

    output$significantPathways <- renderbs4ValueBox({
      req(pathway_state()$significant_pathways)
      bs4ValueBox(
        value = nrow(pathway_state()$significant_pathways),
        subtitle = "Significant Pathways",
        icon = icon("star"),
      color = "success"
    )
  })

    output$topPathway <- renderbs4ValueBox({
      req(pathway_state()$significant_pathways)
      top_pathway <- pathway_state()$significant_pathways %>%
        arrange(p.adjust) %>%
        head(1)
      bs4ValueBox(
        value = top_pathway$Description,
        subtitle = "Top Pathway",
        icon = icon("award"),
        color = "info"
    )
  })

  # Output: Enrichment Plot
  output$enrichmentPlot <- renderPlotly({
      req(pathway_state()$enrichment_data)
      
      withProgress(message = 'Creating enrichment plot...', {
        tryCatch({
          p <- dotplot(
            pathway_state()$enrichment_data,
            showCategory = 20,
            size = "Count",
            color = "p.adjust"
          )
          ggplotly(p)
        }, error = function(e) {
          showNotification(
            paste("Error creating enrichment plot:", e$message),
            type = "error"
          )
          NULL
        })
      })
  })

    # Output: Network Plot
    output$networkPlot <- renderPlotly({
      req(pathway_state()$network_data)
      
      withProgress(message = 'Creating network plot...', {
        tryCatch({
          p <- cnetplot(
            pathway_state()$network_data,
            showCategory = 5,
            foldChange = pathway_state()$network_data$foldChange
          )
          ggplotly(p)
        }, error = function(e) {
          showNotification(
            paste("Error creating network plot:", e$message),
            type = "error"
          )
          NULL
        })
      })
  })

  # Output: Results Table
  output$pathwayResults <- renderDT({
    req(pathway_state()$results)
    
      withProgress(message = 'Rendering results table...', {
        tryCatch({
    datatable(
      pathway_state()$results,
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

    # Output: Gene-Pathway Table
    output$genePathwayTable <- renderDT({
      req(pathway_state()$results)
      
      withProgress(message = 'Rendering gene-pathway table...', {
        tryCatch({
          gene_pathway_data <- pathway_state()$results %>%
            select(ID, Description, geneID) %>%
            separate_rows(geneID, sep = "/") %>%
            rename(Gene = geneID)
          
          datatable(
            gene_pathway_data,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              autoWidth = TRUE
            )
          )
        }, error = function(e) {
          showNotification(
            paste("Error rendering gene-pathway table:", e$message),
            type = "error"
          )
          NULL
        })
      })
  })

    # Download handlers for plots
    output$download_enrichment_plot <- downloadHandler(
    filename = function() {
        paste("enrichment_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
        req(pathway_state()$enrichment_data)
        p <- dotplot(
          pathway_state()$enrichment_data,
          showCategory = 20,
          size = "Count",
          color = "p.adjust"
        )
        ggsave(file, p, width = 10, height = 6, dpi = 300)
    }
  )

    output$download_network_plot <- downloadHandler(
    filename = function() {
        paste("network_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png", sep = "")
    },
    content = function(file) {
        req(pathway_state()$network_data)
        p <- cnetplot(
          pathway_state()$network_data,
          showCategory = 5,
          foldChange = pathway_state()$network_data$foldChange
        )
        ggsave(file, p, width = 10, height = 6, dpi = 300)
    }
  )

    # Return the pathway state
    reactive({
      pathway_state()
    })
  })
}

#' Alias for create_pathway_server
#' @export
pathway_server <- create_pathway_server 