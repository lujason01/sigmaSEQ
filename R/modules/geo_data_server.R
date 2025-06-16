#' GEO Data Module Server
#' 
#' @param id The module ID
#' @param data_state Reactive value containing the current data state
#' @param sanitize_input Function to sanitize user input
#' @return A reactive value containing the GEO data state
#' @export
create_geo_data_server <- function(id, data_state, sanitize_input = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define namespace
    
    # Validate data_state
    if (!is.reactive(data_state)) {
      stop("data_state must be a reactive value")
    }
    
    # Ensure data_state has required structure
    if (is.null(data_state()$parameters)) {
      data_state(list(
        count_matrix = NULL,
        metadata = NULL,
        feature_metadata = NULL,
        parameters = list(
          normalization_method = "DESeq2",
          transformation_method = "vst",
          min_count = 10,
          fc_threshold = 1.5,
          pval_threshold = 0.05
        )
      ))
    }
    
    # Set up working directory and storage
    app_dir <- getwd()
    data_dir <- file.path(app_dir, "data")
    geo_dir <- file.path(data_dir, "geo")
    
    # Debug logging for directories
    print(paste("App directory:", app_dir))
    print(paste("Data directory:", data_dir))
    print(paste("GEO directory:", geo_dir))
    
    # Create directories if they don't exist
    if (!dir.exists(data_dir)) {
      print("Creating data directory...")
      dir.create(data_dir, showWarnings = TRUE, recursive = TRUE)
    }
    
    if (!dir.exists(geo_dir)) {
      print("Creating GEO directory...")
      dir.create(geo_dir, showWarnings = TRUE, recursive = TRUE)
    }
    
    # Verify directories exist
    print(paste("Data directory exists:", dir.exists(data_dir)))
    print(paste("GEO directory exists:", dir.exists(geo_dir)))
    
    # Load required packages with better error handling
    required_packages <- c("GEOquery", "limma", "Biobase")
    
    # Check which packages are already installed
    installed_packages <- installed.packages()[, "Package"]
    packages_to_install <- required_packages[!required_packages %in% installed_packages]
    
    if (length(packages_to_install) > 0) {
      showNotification(
        paste("Installing required packages:", paste(packages_to_install, collapse = ", ")),
        type = "message"
      )
      
      # Install BiocManager if needed
      if (!require("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager", repos = "https://cran.rstudio.com/")
      }
      
      # Install missing packages
      BiocManager::install(packages_to_install, update = FALSE, ask = FALSE)
    }
    
    # Load all required packages
    for (pkg in required_packages) {
      tryCatch({
        if (!require(pkg, character.only = TRUE)) {
          stop(paste("Failed to load package:", pkg))
        }
      }, error = function(e) {
        showNotification(
          paste("Error loading package", pkg, ":", e$message),
          type = "error"
        )
        stop(paste("Package dependency error:", e$message))
      })
    }
    
    # Check internet connectivity
    check_internet <- function() {
      tryCatch({
        test_url <- "https://www.ncbi.nlm.nih.gov/geo/"
        response <- httr::GET(test_url, timeout(5))
        return(httr::status_code(response) == 200)
      }, error = function(e) {
        return(FALSE)
      })
    }
    
    # Set GEOquery options with timeout
    options(
      GEOquery.destdir = geo_dir,
      timeout = 300,  # 5 minutes timeout
      download.file.method = "libcurl"
    )
    
    # Reactive value for GEO data state
    geo_state <- reactiveVal(list(
      gse = NULL,
      platforms = NULL,
      samples = NULL,
      counts = NULL,
      metadata = NULL,
      is_valid = FALSE,
      error_message = NULL
    ))
    
    # Set up logging
    setup_logging <- function() {
      log_file <- file.path(geo_dir, "geo_download.log")
      if (!file.exists(log_file)) {
        file.create(log_file)
      }
      return(log_file)
    }
    
    log_file <- setup_logging()
    
    # Log function with timestamp
    log_message <- function(message, level = "INFO") {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- paste(timestamp, level, message, sep = " - ")
      write(log_entry, file = log_file, append = TRUE)
      print(log_entry)  # Also print to console for immediate feedback
    }
    
    # Helper function to check GEO accession validity and fetch metadata
    fetch_geo_metadata <- function(accession) {
      tryCatch({
        # Construct eSearch URL
        base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"
        search_url <- sprintf(
          "%s/esearch.fcgi?db=gds&term=%s[ACCN]+AND+gse[ETYP]&retmax=1&usehistory=y",
          base_url,
          accession
        )
        
        # Perform eSearch
        search_result <- httr::GET(search_url)
        if (httr::status_code(search_result) != 200) {
          stop("Failed to query GEO database")
        }
        
        # Parse XML response
        xml_data <- xml2::read_xml(httr::content(search_result, "text"))
        count <- as.numeric(xml2::xml_text(xml2::xml_find_first(xml_data, "//Count")))
        
        if (count == 0) {
          stop("GEO accession not found")
        }
        
        # Get WebEnv and QueryKey
        webenv <- xml2::xml_text(xml2::xml_find_first(xml_data, "//WebEnv"))
        querykey <- xml2::xml_text(xml2::xml_find_first(xml_data, "//QueryKey"))
        
        # Fetch summary using eSummary
        summary_url <- sprintf(
          "%s/esummary.fcgi?db=gds&query_key=%s&WebEnv=%s&version=2.0",
          base_url,
          querykey,
          webenv
        )
        
        summary_result <- httr::GET(summary_url)
        if (httr::status_code(summary_result) != 200) {
          stop("Failed to fetch GEO metadata")
        }
        
        return(list(
          webenv = webenv,
          querykey = querykey,
          summary = httr::content(summary_result, "text")
        ))
        
      }, error = function(e) {
        stop(paste("Error fetching GEO metadata:", e$message))
      })
    }
    
    # Helper function to construct GEO FTP URLs
    construct_geo_urls <- function(accession) {
      # Extract numeric part and calculate directory
      acc_num <- as.numeric(gsub("GSE", "", accession))
      nnn <- sprintf("%dnnn", floor(acc_num/1000))
      
      # Base FTP URL
      ftp_base <- "ftp://ftp.ncbi.nlm.nih.gov/geo/series"
      
      # Construct specific file URLs
      urls <- list(
        matrix = sprintf(
          "%s/GSE%s/%s/matrix/%s_series_matrix.txt.gz",
          ftp_base, nnn, accession, accession
        ),
        miniml = sprintf(
          "%s/GSE%s/%s/miniml/%s_family.xml.tgz",
          ftp_base, nnn, accession, accession
        ),
        soft = sprintf(
          "%s/GSE%s/%s/soft/%s_family.soft.gz",
          ftp_base, nnn, accession, accession
        )
      )
      
      return(urls)
    }
    
    # Helper function to download GEO files with retry logic
    download_geo_files <- function(accession, geo_dir, max_retries = 3) {
      tryCatch({
        # Get URLs for different file formats
        urls <- construct_geo_urls(accession)
        
        # Create directory for this accession
        acc_dir <- file.path(geo_dir, accession)
        if (!dir.exists(acc_dir)) {
          dir.create(acc_dir, recursive = TRUE)
        }
        
        # Function to download with retry
        download_with_retry <- function(url, destfile) {
          for (attempt in 1:max_retries) {
            tryCatch({
              download.file(url, destfile, mode = "wb", quiet = TRUE)
              return(TRUE)
            }, error = function(e) {
              if (attempt == max_retries) {
                stop(sprintf("Failed to download after %d attempts: %s", max_retries, e$message))
              }
              Sys.sleep(2 ^ attempt) # Exponential backoff
            })
          }
        }
        
        # Download files
        files <- list()
        
        # Try matrix file first (most compact)
        matrix_file <- file.path(acc_dir, paste0(accession, "_series_matrix.txt.gz"))
        if (download_with_retry(urls$matrix, matrix_file)) {
          files$matrix <- matrix_file
        }
        
        # If matrix file fails, try MINiML
        if (is.null(files$matrix)) {
          miniml_file <- file.path(acc_dir, paste0(accession, "_family.xml.tgz"))
          if (download_with_retry(urls$miniml, miniml_file)) {
            files$miniml <- miniml_file
          }
        }
        
        # If both fail, try SOFT format
        if (length(files) == 0) {
          soft_file <- file.path(acc_dir, paste0(accession, "_family.soft.gz"))
          if (download_with_retry(urls$soft, soft_file)) {
            files$soft <- soft_file
          }
        }
        
        if (length(files) == 0) {
          stop("Failed to download data in any format")
        }
        
        return(files)
        
      }, error = function(e) {
        stop(paste("Error downloading GEO files:", e$message))
      })
    }
    
    # Sanitize GEO accession number
    sanitize_geo_accession <- function(accession) {
      if (!is.null(sanitize_input)) {
        return(sanitize_input(accession))
      }
      # Default sanitization if no function provided
      return(gsub("[^A-Za-z0-9]", "", accession))
    }
    
    # Handle GEO accession input
    observeEvent(input$geo_accession, {
      # Sanitize input
      sanitized_accession <- sanitize_geo_accession(input$geo_accession)
      
      # Update input if sanitization changed the value
      if (sanitized_accession != input$geo_accession) {
        updateTextInput(session, "geo_accession", value = sanitized_accession)
      }
      
      # Validate GEO accession format
      if (!grepl("^GSE\\d+$", sanitized_accession)) {
        showNotification("Invalid GEO accession number format", type = "error")
        return()
      }
    })
    
    # Handle GEO data download
    observeEvent(input$download_geo, {
      req(input$geo_accession)
      
      # Sanitize and validate accession number
      accession <- sanitize_geo_accession(input$geo_accession)
      
      if (!grepl("^GSE\\d+$", accession)) {
        showNotification("Invalid GEO accession number format", type = "error")
        return()
      }
      
      # Show loading notification
      showNotification("Downloading GEO data...", type = "message")
      
      tryCatch({
        # Download GEO data
        geo_data <- GEOquery::getGEO(accession)
        
        if (is.null(geo_data)) {
          showNotification("Failed to download GEO data", type = "error")
          return()
        }
        
        # Process GEO data
        processed_data <- process_geo_data(geo_data)
        
        # Update state
        geo_state(processed_data)
        data_state(list(
          count_matrix = processed_data$counts,
          metadata = processed_data$metadata,
          feature_metadata = processed_data$feature_metadata,
          parameters = data_state()$parameters
        ))
        
        showNotification("GEO data downloaded successfully", type = "message")
      }, error = function(e) {
        showNotification(paste("Error downloading GEO data:", e$message), type = "error")
      })
    })
    
    # Update platform choices
    observeEvent(input$get_geo, {
      req(input$geo_id)
      
      # Show loading state
      session$sendCustomMessage("toggleLoading", list(
        id = ns("loading_container"),
        show = TRUE
      ))
      
      # Update progress
      session$sendCustomMessage("updateProgress", list(
        id = ns("progress_bar"),
        textId = ns("progress_text"),
        value = 0,
        text = "Validating GEO accession..."
      ))
      
      withProgress(message = 'Processing GEO data...', value = 0, {
        tryCatch({
          # Validate accession format
          if (!grepl("^GSE\\d+$", input$geo_id)) {
            stop("Invalid GEO accession number format. Please use GSEXXXXX format.")
          }
          
          # Check internet connection
          incProgress(0.1, detail = "Checking internet connection...")
          if (!check_internet()) {
            stop("No internet connection detected")
          }
          
          # Download data files
          incProgress(0.3, detail = "Downloading GEO files...")
          files <- download_geo_files(input$geo_id, geo_dir)
          
          # Process downloaded files
          incProgress(0.2, detail = "Processing data files...")
          
          # Load data based on available format
          if (!is.null(files$matrix)) {
            gse <- getGEO(filename = files$matrix)
          } else if (!is.null(files$miniml)) {
            gse <- getGEO(filename = files$miniml)
          } else {
            gse <- getGEO(filename = files$soft)
          }
          
          # Update progress
          session$sendCustomMessage("updateProgress", list(
            id = ns("progress_bar"),
            textId = ns("progress_text"),
            value = 70,
            text = "Processing platforms..."
          ))
          
          # Update platform choices
          platforms <- names(gse)
          session$sendCustomMessage("updatePlatformChoices", list(
            id = ns("platform"),
            choices = platforms
          ))
          
          # Update sample choices for the first platform
          if (length(platforms) > 0) {
            incProgress(0.1, detail = "Processing samples...")
            samples <- colnames(exprs(gse[[1]]))
            session$sendCustomMessage("updateSampleChoices", list(
              id = ns("samples"),
              choices = samples
            ))
          }
          
          # Update progress
          session$sendCustomMessage("updateProgress", list(
            id = ns("progress_bar"),
            textId = ns("progress_text"),
            value = 100,
            text = "Complete!"
          ))
          
          # Show success message
          session$sendCustomMessage("showStatus", list(
            id = ns("status_container"),
            type = "success",
            text = "GEO data downloaded successfully"
          ))
          
          # Show process button
          session$sendCustomMessage("toggleProcessButton", list(
            id = ns("process_geo"),
            show = TRUE
          ))
          
          # Update GEO state
          geo_state(list(
            gse = gse,
            platforms = platforms,
            samples = NULL,
            counts = NULL,
            metadata = NULL,
            is_valid = TRUE,
            error_message = NULL
          ))
          
        }, error = function(e) {
          # Show error message
          session$sendCustomMessage("showStatus", list(
            id = ns("status_container"),
            type = "error",
            text = paste("Error processing GEO data:", e$message)
          ))
        }, finally = {
          # Hide loading state
          session$sendCustomMessage("toggleLoading", list(
            id = ns("loading_container"),
            show = FALSE
          ))
        })
      })
    })
    
    # Update sample choices when platform changes
    observeEvent(input$platform, {
      req(input$platform, geo_state()$gse)
      
      gse <- geo_state()$gse
      if (!is.null(gse) && input$platform %in% names(gse)) {
        samples <- colnames(exprs(gse[[input$platform]]))
        session$sendCustomMessage("updateSampleChoices", list(
          id = ns("samples"),
          choices = samples
        ))
        
        # Show info message
        session$sendCustomMessage("showStatus", list(
          id = ns("status_container"),
          type = "info",
          text = paste("Selected platform:", input$platform)
        ))
      }
    })
    
    # Handle sample selection
    observeEvent(input$samples, {
      req(geo_data()$is_valid, input$platform, input$samples)
      
      tryCatch({
        # Get GSE data
        data <- geo_data()
        gse <- data$gse[[input$platform]]
        
        # Extract expression data
        counts <- exprs(gse)[, input$samples]
        metadata <- pData(gse)[input$samples, ]
        
        # Update GEO state
        current_state <- geo_state()
        current_state$counts <- counts
        current_state$metadata <- metadata
        geo_state(current_state)
        
        # Update data state
        data_state(list(
          count_matrix = counts,
          metadata = metadata,
          feature_metadata = NULL,
          parameters = data_state()$parameters
        ))
        
        # Show success message
        session$sendCustomMessage("showStatus", list(
          id = ns("status_container"),
          type = "success",
          text = paste(length(input$samples), "samples selected and processed successfully")
        ))
        
      }, error = function(e) {
        # Show error message
        session$sendCustomMessage("showStatus", list(
          id = ns("status_container"),
          type = "error",
          text = paste("Error processing samples:", e$message)
        ))
      })
    })
    
    # Output: Expression Data Preview
    output$expression_preview <- renderDT({
      req(geo_state()$counts)
      
      tryCatch({
        datatable(
          head(geo_state()$counts, 10),
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            autoWidth = TRUE
          )
        )
      }, error = function(e) {
        showNotification(
          paste("Error rendering expression data preview:", e$message),
          type = "error"
        )
        NULL
      })
    })
    
    # Output: Metadata Preview
    output$metadata_preview <- renderDT({
      req(geo_state()$metadata)
      
      tryCatch({
        datatable(
          geo_state()$metadata,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            autoWidth = TRUE
          )
        )
      }, error = function(e) {
        showNotification(
          paste("Error rendering metadata preview:", e$message),
          type = "error"
        )
        NULL
      })
    })
    
    # Output: GEO Dataset Summary
    output$geo_title <- renderbs4ValueBox({
      req(geo_state()$metadata)
      title <- unique(geo_state()$metadata$title)[1]
      bs4ValueBox(
        value = title,
        subtitle = "Dataset Title",
        icon = icon("file-alt"),
        color = "primary"
      )
    })
    
    output$total_samples <- renderbs4ValueBox({
      req(geo_state()$counts)
      bs4ValueBox(
        value = ncol(geo_state()$counts),
        subtitle = "Total Samples",
        icon = icon("vial"),
        color = "success"
      )
    })
    
    output$platform_info <- renderbs4ValueBox({
      req(geo_state()$gse)
      platform <- geo_state()$gse[[1]]@annotation
      bs4ValueBox(
        value = platform,
        subtitle = "Platform",
        icon = icon("microchip"),
        color = "info"
      )
    })
    
    # Output: GEO Status
    output$geo_status <- renderUI({
      data <- geo_data()
      
      if (is.null(data$gse)) {
        return(tags$div(
          class = "alert alert-info",
          "Please enter a GEO accession number"
        ))
      }
      
      if (!is.null(data$error_message)) {
        return(tags$div(
          class = "alert alert-danger",
          data$error_message
        ))
      }
      
      if (data$is_valid) {
        return(tags$div(
          class = "alert alert-success",
          "GEO data downloaded successfully"
        ))
      }
      
      return(NULL)
    })
    
    # Initialize querychat for AI data exploration
    ai_explorer <- querychat::QueryChat$new(
      model = "gpt-4",  # or another appropriate model
      system_prompt = "You are an expert bioinformatician analyzing RNA-seq data. Help users understand their GEO dataset by answering questions about the data, experimental design, and analysis results."
    )
    
    # Helper function to format chat messages
    format_chat_message <- function(role, content) {
      div(
        class = paste("chat-message", role),
        if (role == "assistant") tags$i(class = "fas fa-robot") else tags$i(class = "fas fa-user"),
        p(content)
      )
    }
    
    # Handle AI query submissions
    observeEvent(input$submit_query, {
      req(input$query_input, geo_state()$is_valid)
      
      # Get the current query
      query <- input$query_input
      
      # Add user message to chat history
      insertUI(
        selector = paste0("#", ns("chat_history")),
        where = "beforeEnd",
        ui = format_chat_message("user", query)
      )
      
      # Prepare context for the AI
      context <- list(
        dataset = list(
          accession = input$geo_id,
          title = geo_state()$metadata$title,
          summary = geo_state()$metadata$summary,
          platform = geo_state()$metadata$platform,
          num_samples = length(geo_state()$metadata$samples)
        ),
        analysis = list(
          expression_data = if (!is.null(geo_state()$counts)) dim(geo_state()$counts) else NULL,
          selected_platform = input$platform,
          selected_samples = input$samples
        )
      )
      
      # Get AI response
      tryCatch({
        response <- ai_explorer$chat(
          messages = list(
            list(
              role = "system",
              content = sprintf(
                "You are analyzing GEO dataset %s. Here's the context: %s",
                input$geo_id,
                jsonlite::toJSON(context, auto_unbox = TRUE)
              )
            ),
            list(
              role = "user",
              content = query
            )
          )
        )
        
        # Add AI response to chat history
        insertUI(
          selector = paste0("#", ns("chat_history")),
          where = "beforeEnd",
          ui = format_chat_message("assistant", response$choices[[1]]$message$content)
        )
        
      }, error = function(e) {
        # Show error in chat
        insertUI(
          selector = paste0("#", ns("chat_history")),
          where = "beforeEnd",
          ui = format_chat_message(
            "assistant",
            paste("I apologize, but I encountered an error:", e$message)
          )
        )
      })
      
      # Clear input
      updateTextAreaInput(session, "query_input", value = "")
    })
    
    # Handle suggested query clicks
    observe({
      suggested_queries <- c(
        input$suggest_1,
        input$suggest_2,
        input$suggest_3,
        input$suggest_4
      )
      
      for (i in seq_along(suggested_queries)) {
        if (!is.null(suggested_queries[i]) && suggested_queries[i] > 0) {
          # Get the corresponding question text
          question <- switch(i,
            "What are the key findings in this dataset?",
            "Show me the top pathways and their significance",
            "Generate a summary of the experimental design",
            "What are the quality control metrics?"
          )
          
          # Update the input
          updateTextAreaInput(session, "query_input", value = question)
          
          # Trigger the query submission
          click("submit_query")
        }
      }
    })
    
    # Return the GEO state
    return(geo_state)
  })
}
