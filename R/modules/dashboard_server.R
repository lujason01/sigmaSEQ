#' Dashboard Server Logic
#' @param id Module ID
#' @param data_state Reactive value containing data state
#' @param dea_results Reactive value containing DEA results
#' @param pathway_results Reactive value containing pathway results
#' @return Reactive value containing dashboard state
#' @export
create_dashboard_server <- function(id, data_state, dea_results, pathway_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define namespace
    
    # Reactive values for dashboard state
    dashboard_state <- reactiveVal(list(
      summary_stats = NULL,
      data_overview = NULL,
      recent_results = NULL,
      analysis_log = NULL
    ))

    # Update summary statistics
    observe({
      req(data_state(), dea_results(), pathway_results())
      
      # Calculate summary statistics
      summary_stats <- list(
        total_samples = ncol(data_state()$count_matrix),
        total_genes = nrow(data_state()$count_matrix),
        de_genes = if (!is.null(dea_results()$significant_genes)) {
          nrow(dea_results()$significant_genes)
        } else 0,
        pathways = if (!is.null(pathway_results()$significant_pathways)) {
          nrow(pathway_results()$significant_pathways)
        } else 0
      )
      
      # Update dashboard state
      dashboard_state(list(
        summary_stats = summary_stats,
        data_overview = data_state()$count_matrix,
        recent_results = dea_results()$results,
        analysis_log = c(
          paste("Data loaded:", Sys.time()),
          paste("Total samples:", summary_stats$total_samples),
          paste("Total genes:", summary_stats$total_genes),
          paste("DE genes:", summary_stats$de_genes),
          paste("Pathways:", summary_stats$pathways)
        )
      ))
    })

    # Output: Summary Statistics
    output$totalSamples <- renderbs4ValueBox({
      req(dashboard_state()$summary_stats)
      bs4ValueBox(
        value = dashboard_state()$summary_stats$total_samples,
        subtitle = "Total Samples",
        icon = icon("vial"),
      color = "primary"
    )
  })
  
    output$totalGenes <- renderbs4ValueBox({
      req(dashboard_state()$summary_stats)
      bs4ValueBox(
        value = dashboard_state()$summary_stats$total_genes,
        subtitle = "Total Genes",
        icon = icon("dna"),
      color = "success"
    )
  })
  
    output$deGenes <- renderbs4ValueBox({
      req(dashboard_state()$summary_stats)
      bs4ValueBox(
        value = dashboard_state()$summary_stats$de_genes,
        subtitle = "DE Genes",
        icon = icon("chart-line"),
      color = "warning"
    )
  })
  
    output$pathways <- renderbs4ValueBox({
      req(dashboard_state()$summary_stats)
      bs4ValueBox(
        value = dashboard_state()$summary_stats$pathways,
        subtitle = "Pathways",
        icon = icon("project-diagram"),
        color = "info"
      )
    })

    # Output: Data Overview Plot
    output$dataOverviewPlot <- renderPlotly({
      req(dashboard_state()$data_overview)
      
      withProgress(message = 'Creating data overview plot...', {
        tryCatch({
          # Calculate mean expression per sample
          mean_expr <- colMeans(dashboard_state()$data_overview)
          
          # Create plot
          p <- plot_ly(
            x = names(mean_expr),
            y = mean_expr,
            type = "bar",
            name = "Mean Expression"
          ) %>%
            layout(
              title = "Mean Expression per Sample",
              xaxis = list(title = "Sample"),
              yaxis = list(title = "Mean Expression")
            )
          
          p
        }, error = function(e) {
          showNotification(
            paste("Error creating data overview plot:", e$message),
            type = "error"
          )
          NULL
        })
      })
    })

    # Update progress bars
    observe({
      req(dashboard_state()$summary_stats)
      
      # Calculate progress values
      total_samples <- dashboard_state()$summary_stats$total_samples
      total_genes <- dashboard_state()$summary_stats$total_genes
      de_genes <- dashboard_state()$summary_stats$de_genes
      pathways <- dashboard_state()$summary_stats$pathways
      
      # Update analysis progress
      analysis_progress <- if (pathways > 0) 100 else if (de_genes > 0) 75 else if (total_genes > 0) 50 else 25
      updateProgressBar(
        session,
        "analysisProgress",
        value = analysis_progress
      )
      
      # Update data quality
      data_quality <- if (total_samples >= 10 && total_genes >= 1000) 100 else 50
      updateProgressBar(
        session,
        "dataQuality",
        value = data_quality
      )
      
      # Update analysis quality
      analysis_quality <- if (de_genes >= 100 && pathways >= 10) 100 else 50
      updateProgressBar(
        session,
        "analysisQuality",
        value = analysis_quality
      )
    })

    # Output: Recent Results
    output$recentResults <- renderDT({
      req(dashboard_state()$recent_results)
      
      withProgress(message = 'Rendering recent results...', {
        tryCatch({
          datatable(
            head(dashboard_state()$recent_results, 10),
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              autoWidth = TRUE
            )
          )
        }, error = function(e) {
          showNotification(
            paste("Error rendering recent results:", e$message),
            type = "error"
          )
          NULL
        })
      })
    })

    # Return the dashboard state
    return(dashboard_state)
  })
} 