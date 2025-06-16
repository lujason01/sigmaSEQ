#' AI Interpretation Server Logic
#' @param id Module ID
#' @param dea_state Reactive value containing DEA state
#' @param pathway_state Reactive value containing pathway state
#' @param validate_api_key Function to validate API key
#' @return Reactive value containing AI interpretation state
#' @export
create_ai_interpretation_server <- function(id, dea_state, pathway_state, validate_api_key = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define namespace
    
    # Load required packages
    require(openai)
    require(markdown)
    require(rmarkdown)
    require(plotly)
    
    # Reactive values for AI state
    ai_state <- reactiveVal(list(
      current_interpretation = NULL,
      plot_suggestions = NULL,
      last_error = NULL
    ))

    # Helper functions
    prepare_eda_context <- function(eda_data) {
      list(
        sample_info = summarize_samples(eda_data$normalized_data),
        pca_results = eda_data$pca_results,
        correlation_matrix = eda_data$sample_correlations
      )
    }

    prepare_dea_context <- function(dea_data) {
      list(
        total_genes = nrow(dea_data$results),
        sig_genes = nrow(dea_data$significant_genes),
        top_genes = head(dea_data$results, 10),
        volcano_data = summarize_volcano(dea_data$volcano_data)
      )
    }

    prepare_pathway_context <- function(pathway_data) {
      list(
        total_pathways = nrow(pathway_data$results@result),
        sig_pathways = sum(pathway_data$results@result$qvalue < 0.1),
        top_pathways = head(pathway_data$results@result, 10)
      )
    }

    generate_ai_interpretation <- function(context, focus) {
      tryCatch({
        # Prepare prompt based on context and focus
        prompt <- create_analysis_prompt(context, focus)
        
        # Call OpenAI API
        openai_config <- get_config("openai")
        response <- openai::create_completion(
          model = openai_config$model,
          prompt = prompt,
          max_tokens = openai_config$max_tokens,
          temperature = openai_config$temperature
        )
        
        # Process response
        list(
          text = process_ai_response(response$choices$text),
          plots = extract_plot_suggestions(response$choices$text)
        )
      }, error = function(e) {
        ai_state(list(
          current_interpretation = NULL,
          plot_suggestions = NULL,
          last_error = e$message
        ))
        showNotification(
          paste("Error generating interpretation:", e$message),
          type = "error",
          duration = NULL
        )
        NULL
      })
    }

    generate_enhanced_plot <- function(suggestions, analysis_type) {
      tryCatch({
        switch(analysis_type,
          "eda" = create_enhanced_eda_plot(suggestions, eda_state()),
          "dea" = create_enhanced_dea_plot(suggestions, dea_state()),
          "pathway" = create_enhanced_pathway_plot(suggestions, pathway_state())
        )
      }, error = function(e) {
        showNotification(
          paste("Error generating plot:", e$message),
          type = "error"
        )
        NULL
      })
    }

    # Validate API key
    validate_openai_key <- function(api_key) {
      if (!is.null(validate_api_key)) {
        return(validate_api_key(api_key))
      }
      # Default validation if no function provided
      return(grepl("^sk-[A-Za-z0-9]{32,}$", api_key))
    }
    
    # Handle API key input
    observeEvent(input$api_key, {
      if (!validate_openai_key(input$api_key)) {
        showNotification("Invalid API key format", type = "error")
        updateTextInput(session, "api_key", value = "")
      }
    })

    # Handle interpretation request
    observeEvent(input$interpret_button, {
      req(input$api_key, dea_state(), pathway_state())
      
      # Validate API key
      if (!validate_openai_key(input$api_key)) {
        showNotification("Invalid API key", type = "error")
        return()
      }
      
      # Show loading notification
      showNotification("Generating interpretation...", type = "message")
      
      tryCatch({
        # Set API key
        Sys.setenv(OPENAI_API_KEY = input$api_key)
        
        # Prepare data for interpretation
        interpretation_data <- list(
          dea_results = dea_state(),
          pathway_results = pathway_state(),
          focus_areas = input$focus_areas,
          interpretation_type = input$interpretation_type
        )
        
        # Generate interpretation
        interpretation <- generate_ai_interpretation(
          interpretation_data,
          input$focus_areas
        )
        
        if (!is.null(interpretation)) {
          ai_state(list(
            current_interpretation = interpretation$text,
            plot_suggestions = interpretation$plots,
            last_error = NULL
          ))
          
          showNotification(
            "Interpretation generated successfully",
            type = "message"
          )
        }
      }, error = function(e) {
        ai_state(list(
          current_interpretation = NULL,
          plot_suggestions = NULL,
          last_error = e$message
        ))
        showNotification(
          paste("Error generating interpretation:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })

    # Outputs
    output$interpretationReady <- reactive({
      !is.null(ai_state()$current_interpretation)
    })

    output$aiInterpretation <- renderUI({
      req(ai_state()$current_interpretation)
      HTML(markdownToHTML(
        text = ai_state()$current_interpretation,
        fragment.only = TRUE
      ))
    })

    output$aiEnhancedPlot <- renderPlotly({
      req(ai_state()$plot_suggestions)
      generate_enhanced_plot(
        ai_state()$plot_suggestions,
        input$interpretation_type
      )
    })

    # Download handler
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste0("ai_analysis_report_",
               input$interpretation_type, "_",
               format(Sys.time(), "%Y%m%d_%H%M%S"),
               ".html")
      },
      content = function(file) {
        # Create temp report file
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(system.file("reports", "ai_report_template.Rmd",
                             package = "sigmaSEQ"),
                  tempReport, overwrite = TRUE)
        
        # Render report
        rmarkdown::render(
          input = tempReport,
          output_file = file,
          params = list(
            interpretation = ai_state()$current_interpretation,
            plots = ai_state()$plot_suggestions,
            analysis_type = input$interpretation_type,
            timestamp = Sys.time()
          ),
          envir = new.env(parent = globalenv())
        )
      }
    )

    return(ai_state)
  })
}

#' Alias for create_ai_interpretation_server
#' @export
ai_interpretation_server <- create_ai_interpretation_server 