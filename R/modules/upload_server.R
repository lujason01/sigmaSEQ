#' Upload Server Logic
#' @param id Module ID
#' @param data_state Reactive value containing data state
#' @param validate_file_type Function to validate file type
#' @param validate_file_size Function to validate file size
#' @param check_file_safety Function to check file safety
#' @return Reactive value containing uploaded data state
create_upload_server <- function(id, data_state, 
                               validate_file_type = NULL,
                               validate_file_size = NULL,
                               check_file_safety = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define namespace
    
    # Debug logger
    log_debug <- function(msg, data = NULL) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      cat(sprintf("[DEBUG] %s - %s\n", timestamp, msg))
      if (!is.null(data)) {
        cat("Data details:\n")
        print(str(data))
      }
    }
    
    log_debug("Upload server initialized", list(id = id))
    
    # Initialize upload state
    upload_state <- reactiveVal(list(
      count_data = NULL,
      metadata = NULL,
      is_valid = FALSE,
      error_message = NULL
    ))
    
    # Reactive expression for file validation
    files_valid <- reactive({
      log_debug("Checking file validation")
      
      if (is.null(input$countFile) || is.null(input$metadataFile)) {
        log_debug("Files not yet uploaded", 
                 list(countFile = !is.null(input$countFile),
                      metadataFile = !is.null(input$metadataFile)))
        return(FALSE)
      }
      
      # Apply security validations if provided
      if (!is.null(validate_file_type)) {
        if (!validate_file_type(input$countFile$name, c("csv", "txt", "tsv")) ||
            !validate_file_type(input$metadataFile$name, c("csv", "txt", "tsv"))) {
          log_debug("File type validation failed")
          return(FALSE)
        }
      }
      
      if (!is.null(validate_file_size)) {
        if (!validate_file_size(input$countFile$datapath, 50 * 1024^2) ||
            !validate_file_size(input$metadataFile$datapath, 50 * 1024^2)) {
          log_debug("File size validation failed")
          return(FALSE)
        }
      }
      
      if (!is.null(check_file_safety)) {
        if (!check_file_safety(input$countFile$datapath) ||
            !check_file_safety(input$metadataFile$datapath)) {
          log_debug("File safety check failed")
          return(FALSE)
        }
      }
      
      log_debug("Files valid")
      return(TRUE)
    })
    
    # Input validation observer
    observe({
      if (!files_valid()) {
        showNotification(
          "Please upload valid CSV, TSV, or TXT files",
          type = "error"
        )
        return()
      }
    })
    
    # Helper function to read data files with security checks
    read_data_file <- function(file_path, format) {
      log_debug(sprintf("Reading %s file", format), 
                list(path = file_path, format = format))
      
      # Apply security checks
      if (!is.null(check_file_safety) && !check_file_safety(file_path)) {
        log_debug("File safety check failed during read")
        showNotification("File contains potentially unsafe content", type = "error")
        return(NULL)
      }
      
      tryCatch({
        data <- switch(format,
          "csv" = read.csv(file_path, row.names = 1, check.names = FALSE),
          "txt" = read.delim(file_path, row.names = 1, check.names = FALSE),
          "tsv" = read.delim(file_path, row.names = 1, check.names = FALSE)
        )
        log_debug("File read successfully", 
                  list(dimensions = dim(data)))
        return(data)
      }, error = function(e) {
        log_debug(sprintf("Error reading file: %s", e$message))
        showNotification(
          paste("Error reading file:", e$message),
          type = "error"
        )
        return(NULL)
      })
    }
    
    # Helper function to validate data
    validate_data <- function(count_data, metadata) {
      tryCatch({
        # Check if count data is numeric
        if (!all(sapply(count_data, is.numeric))) {
          return(list(
            is_valid = FALSE,
            error_message = "Count data must be numeric"
          ))
        }
        
        # Check if sample names match
        if (!all(colnames(count_data) %in% rownames(metadata))) {
          return(list(
            is_valid = FALSE,
            error_message = "Sample names in count data and metadata do not match"
          ))
        }
        
        return(list(
          is_valid = TRUE,
          error_message = NULL
        ))
      }, error = function(e) {
        return(list(
          is_valid = FALSE,
          error_message = paste("Validation error:", e$message)
        ))
      })
    }
    
    # Reactive expression for data processing
    processed_data <- reactive({
      log_debug("Starting data processing")
      
      if (!files_valid()) {
        log_debug("Files not valid, stopping processing")
        return(NULL)
      }
      
      # Create a list to store results
      result <- list(
        count_data = NULL,
        metadata = NULL,
        is_valid = FALSE,
        error_message = NULL
      )
      
      tryCatch({
        # Read count data
        log_debug("Reading count matrix")
        count_data <- read_data_file(
          input$countFile$datapath,
          tools::file_ext(input$countFile$name)
        )
        
        if (is.null(count_data)) {
          result$error_message <- "Error reading count matrix"
          log_debug("Count matrix read error")
          return(result)
        }
        
        # Read metadata
        log_debug("Reading metadata")
        metadata <- read_data_file(
          input$metadataFile$datapath,
          tools::file_ext(input$metadataFile$name)
        )
        
        if (is.null(metadata)) {
          result$error_message <- "Error reading metadata"
          log_debug("Metadata read error")
          return(result)
        }
        
        # Validate data
        log_debug("Validating data")
        validation <- validate_data(count_data, metadata)
        
        if (!validation$is_valid) {
          result$error_message <- validation$error_message
          log_debug("Data validation failed", list(error = validation$error_message))
          return(result)
        }
        
        # Data is valid
        log_debug("Data processing successful", 
                  list(count_dims = dim(count_data),
                       meta_dims = dim(metadata)))
        
        result$count_data <- count_data
        result$metadata <- metadata
        result$is_valid <- TRUE
        
        return(result)
        
      }, error = function(e) {
        log_debug(sprintf("Processing error: %s", e$message))
        result$error_message <- paste("Processing error:", e$message)
        return(result)
      })
    })
    
    # Handle upload button click
    observeEvent(input$upload_button, {
      log_debug("Upload button clicked")
      
      if (is.null(processed_data())) {
        log_debug("No processed data available")
        return()
      }
      
      # Create progress bar
      withProgress(message = "Processing files...", value = 0, {
        log_debug("Started progress tracking")
        
        tryCatch({
          # Get processed data
          data <- processed_data()
          
          if (!data$is_valid) {
            log_debug("Invalid data", list(error = data$error_message))
            showNotification(data$error_message, type = "error")
            return()
          }
          
          # Update progress
          log_debug("Updating data state")
          incProgress(0.5, detail = "Updating data state...")
          
          # Update data state
          new_state <- list(
            count_matrix = data$count_data,
            metadata = data$metadata,
            feature_metadata = NULL,
            parameters = data_state()$parameters
          )
          log_debug("New state prepared", list(state = names(new_state)))
          
          data_state(new_state)
          log_debug("Data state updated")
          
          # Update upload state
          upload_state(list(
            count_data = data$count_data,
            metadata = data$metadata,
            is_valid = TRUE,
            error_message = NULL
          ))
          log_debug("Upload state updated")
          
          # Complete progress
          incProgress(0.5, detail = "Upload complete!")
          
          # Show success message
          showNotification("Files uploaded successfully!", type = "message")
          log_debug("Upload process completed successfully")
          
        }, error = function(e) {
          log_debug(sprintf("Error in upload process: %s", e$message))
          showNotification(
            paste("Error processing files:", e$message),
            type = "error"
          )
        })
      })
    })
    
    # Output: Upload Status
    output$upload_status <- renderUI({
      log_debug("Rendering upload status")
      data <- processed_data()
      
      if (is.null(data$count_data) && is.null(data$metadata)) {
        log_debug("No files uploaded yet")
        return(tags$div(
          class = "alert alert-info",
          "Please upload both count matrix and metadata files"
        ))
      }
      
      if (!is.null(data$error_message)) {
        log_debug("Showing error message", list(error = data$error_message))
        return(tags$div(
          class = "alert alert-danger",
          data$error_message
        ))
      }
      
      if (data$is_valid) {
        log_debug("Data is valid")
        return(tags$div(
          class = "alert alert-success",
          "Data ready for upload"
        ))
      }
      
      return(NULL)
    })
    
    # Output: Count Matrix Preview
    output$count_preview <- renderDT({
      log_debug("Rendering count preview")
      data <- processed_data()
      req(data$count_data)
      
      log_debug("Count preview data ready", 
                list(dims = dim(data$count_data)))
      
      datatable(
        head(data$count_data, 10),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 't'
        ),
        rownames = TRUE
      )
    })
    
    # Output: Metadata Preview
    output$metadata_preview <- renderDT({
      log_debug("Rendering metadata preview")
      data <- processed_data()
      req(data$metadata)
      
      log_debug("Metadata preview data ready", 
                list(dims = dim(data$metadata)))
      
      datatable(
        head(data$metadata, 10),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 't'
        ),
        rownames = TRUE
      )
    })
    
    # Output: Total Samples
    output$total_samples <- renderbs4ValueBox({
      data <- processed_data()
      req(data$count_data)
      
      bs4ValueBox(
        value = ncol(data$count_data),
        subtitle = "Total Samples",
        icon = icon("vial"),
        color = "primary"
      )
    })
    
    # Output: Total Genes
    output$total_genes <- renderbs4ValueBox({
      data <- processed_data()
      req(data$count_data)
      
      bs4ValueBox(
        value = nrow(data$count_data),
        subtitle = "Total Genes",
        icon = icon("dna"),
        color = "info"
      )
    })
    
    # Output: Data Status
    output$data_status <- renderbs4ValueBox({
      data <- processed_data()
      
      bs4ValueBox(
        value = if (data$is_valid) "Valid" else "Invalid",
        subtitle = "Data Status",
        icon = icon(if (data$is_valid) "check-circle" else "exclamation-circle"),
        color = if (data$is_valid) "success" else "danger"
      )
    })
    
    # Return reactive upload state
    return(upload_state)
  })
}

#' Upload Server Module
#' @param id Module ID
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @return Reactive value containing uploaded data state
#' @export
create_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Define namespace
    
    # Reactive values for data state
    data_state <- reactiveVal(list(
      count_matrix = NULL,
      metadata = NULL,
      is_valid = FALSE,
      error_message = NULL
    ))
    
    # Helper function to read data files
    read_data_file <- function(file_path, file_type) {
      tryCatch({
        if (file_type == "count_matrix") {
          data <- read.csv(file_path, row.names = 1, check.names = FALSE)
          # Validate count matrix
          if (!all(apply(data, 2, is.numeric))) {
            stop("Count matrix must contain only numeric values")
          }
          if (any(data < 0)) {
            stop("Count matrix cannot contain negative values")
          }
          return(data)
        } else if (file_type == "metadata") {
          data <- read.csv(file_path, row.names = 1, check.names = FALSE)
          return(data)
        }
      }, error = function(e) {
        showNotification(
          sprintf("Error reading %s: %s", file_type, e$message),
          type = "error",
          duration = NULL
        )
        return(NULL)
      })
    }
    
    # Handle count matrix upload
    observeEvent(input$count_file, {
      req(input$count_file)
      
      # Validate file type
      ext <- tools::file_ext(input$count_file$name)
      if (!ext %in% c("csv", "txt", "tsv")) {
        showNotification(
          "Please upload a CSV, TSV, or TXT file",
          type = "error"
        )
        return()
      }
      
      # Read count matrix
      count_data <- read_data_file(input$count_file$datapath, "count_matrix")
      if (!is.null(count_data)) {
        current_state <- data_state()
        current_state$count_matrix <- count_data
        data_state(current_state)
        
        # Update count matrix preview
        output$count_preview <- renderDT({
          datatable(
            head(count_data, 10),
            options = list(scrollX = TRUE, dom = 't'),
            rownames = TRUE
          )
        })
        
        showNotification(
          "Count matrix uploaded successfully",
          type = "success"
        )
      }
    })
    
    # Handle metadata upload
    observeEvent(input$metadata_file, {
      req(input$metadata_file)
      
      # Validate file type
      ext <- tools::file_ext(input$metadata_file$name)
      if (!ext %in% c("csv", "txt", "tsv")) {
        showNotification(
          "Please upload a CSV, TSV, or TXT file",
          type = "error"
        )
        return()
      }
      
      # Read metadata
      metadata <- read_data_file(input$metadata_file$datapath, "metadata")
      if (!is.null(metadata)) {
        current_state <- data_state()
        current_state$metadata <- metadata
        data_state(current_state)
        
        # Update metadata preview
        output$metadata_preview <- renderDT({
          datatable(
            head(metadata, 10),
            options = list(scrollX = TRUE, dom = 't'),
            rownames = TRUE
          )
        })
        
        showNotification(
          "Metadata uploaded successfully",
          type = "success"
        )
      }
    })
    
    # Validate data
    observe({
      current_state <- data_state()
      
      # Check if both files are uploaded
      if (is.null(current_state$count_matrix) || is.null(current_state$metadata)) {
        current_state$is_valid <- FALSE
        current_state$error_message <- "Please upload both count matrix and metadata files"
        data_state(current_state)
        return()
      }
      
      # Check sample names match
      count_samples <- colnames(current_state$count_matrix)
      metadata_samples <- rownames(current_state$metadata)
      
      if (!all(count_samples %in% metadata_samples)) {
        current_state$is_valid <- FALSE
        current_state$error_message <- "Sample names in count matrix and metadata do not match"
        data_state(current_state)
        return()
      }
      
      # Data is valid
      current_state$is_valid <- TRUE
      current_state$error_message <- NULL
      data_state(current_state)
      
      showNotification(
        "Data validation successful",
        type = "success"
      )
    })
    
    # Create summary boxes
    output$total_samples <- renderbs4ValueBox({
      current_state <- data_state()
      count_data <- current_state$count_matrix
      
      bs4ValueBox(
        value = if (!is.null(count_data)) ncol(count_data) else 0,
        subtitle = "Total Samples",
        icon = icon("vials"),
        color = "primary"
      )
    })
    
    output$total_genes <- renderbs4ValueBox({
      current_state <- data_state()
      count_data <- current_state$count_matrix
      
      bs4ValueBox(
        value = if (!is.null(count_data)) nrow(count_data) else 0,
        subtitle = "Total Genes",
        icon = icon("dna"),
        color = "info"
      )
    })
    
    output$data_status <- renderbs4ValueBox({
      current_state <- data_state()
      
      bs4ValueBox(
        value = if (current_state$is_valid) "Valid" else "Invalid",
        subtitle = "Data Status",
        icon = icon("check-circle"),
        color = if (current_state$is_valid) "success" else "danger"
      )
    })
    
    # Return reactive data state
    return(data_state)
  })
} 