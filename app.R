#' SigmaSEQ Main Application
#' @description Main Shiny application file for SigmaSEQ
#' @author Jason Lubega
#' @version 1.0.0

# Source global configurations (packages are already loaded in global.R)
source("global.R")

# Security configurations
session_timeout <- 30 * 60  # 30 minutes in seconds
rate_limit <- list(
  window = 60,  # 1 minute window
  max_requests = 100  # maximum requests per window
)

# Rate limiting implementation
rate_limiter <- function() {
  current_time <- as.numeric(Sys.time())
  if (!exists("request_times")) {
    request_times <<- numeric(0)
  }
  
  # Remove requests older than the window
  request_times <<- request_times[request_times > (current_time - rate_limit$window)]
  
  # Check if we're over the limit
  if (length(request_times) >= rate_limit$max_requests) {
    return(FALSE)
  }
  
  # Add current request
  request_times <<- c(request_times, current_time)
  return(TRUE)
}

# Get theme configuration
theme_config <- get_config("theme")

# Initialize analysis parameters
analysis_params <- list(
  normalization_method = "DESeq2",
  transformation_method = "vst",
  min_count = 10,
  fc_threshold = 1.5,
  pval_threshold = 0.05
)

# UI Definition
ui <- bs4Dash::dashboardPage(
  # Add CSS dependencies
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/download_buttons.css")
  ),
  
  # Header
  header = bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand(
      title = "sigmaSEQ",
      color = "primary",
      href = "https://github.com/lujason01/sigmaSEQ",
      image = "https://raw.githubusercontent.com/lujason01/r-practice/refs/heads/main/sigmaseq-high-resolution-logo.png"
    ),
    rightUi = bs4Dash::dropdownMenu(
      type = "notifications",
      badgeStatus = "danger",
      icon = icon("bell"),
      bs4Dash::notificationItem(
        text = "Welcome to sigmaSEQ!",
        status = "success"
      )
    )
  ),
  
  # Sidebar
  sidebar = create_sidebar_ui("sidebar"),
  
  # Body
  body = create_body_ui("body"),
  
  # Controlbar
  controlbar = bs4Dash::dashboardControlbar(
    skin = "light",
    pinned = TRUE,
    collapsed = FALSE,
    overlay = FALSE,
    id = "controlbar",
    
    # Controlbar Menu
    bs4Dash::controlbarMenu(
      id = "controlbarMenu",
      
      # Settings Tab
      bs4Dash::controlbarItem(
        "Settings",
        icon = icon("cogs"),
        sliderInput(
          "plot_height",
          "Plot Height",
          min = 400,
          max = 1000,
          value = 600,
          step = 50
        ),
        sliderInput(
          "plot_width",
          "Plot Width",
          min = 400,
          max = 1000,
          value = 800,
          step = 50
        )
      ),
      
      # Help Tab
      bs4Dash::controlbarItem(
        "Help",
        icon = icon("question-circle"),
        p("For help, please refer to the documentation or contact support.")
      )
    )
  ),
  
  # Footer
  footer = bs4Dash::dashboardFooter(
    left = a(
      href = "https://github.com/lujason01/sigmaSEQ",
      target = "_blank",
      "sigmaSEQ on GitHub"
    ),
    right = "© 2025 Jason Lubega's Lab"
  ),
  
  # Title
  title = "sigmaSEQ - RNA-Seq Analysis Platform",
  
  # Skin
  skin = "light"
)

# Server logic
server <- function(input, output, session) {
  # Source security utilities
  source("R/helpers/security_utils.R")
  
  # Initialize session security
  session$userData$last_activity <- Sys.time()
  
  # Get configuration values
  upload_config <- list(
    max_size = get_config("max_upload_size"),
    allowed_types = get_config("allowed_file_types")
  )
  
  # Initialize states
  data_state <- reactiveVal(list(
    count_matrix = NULL,
    metadata = NULL,
    feature_metadata = NULL,
    parameters = analysis_params
  ))
  
  # Session activity observer
  observe({
    # Update last activity time
    session$userData$last_activity <- Sys.time()
    
    # Check session security
    if (!check_session_security(session)) {
      showModal(modalDialog(
        title = "Session Timeout",
        "Your session has expired due to inactivity. Please refresh the page to continue.",
        footer = NULL
      ))
    }
  })
  
  # Rate limiting observer
  observe({
    if (!rate_limiter()) {
      showModal(modalDialog(
        title = "Rate Limit Exceeded",
        "Too many requests. Please wait a moment before trying again.",
        footer = NULL
      ))
    }
  })
  
  # Initialize modules with security checks
  upload_data <- create_upload_server("upload", data_state, 
                                    validate_file_type = validate_file_type,
                                    validate_file_size = validate_file_size,
                                    check_file_safety = check_file_safety)
  
  geo_data <- create_geo_data_server("geo_data", data_state,
                                    sanitize_input = sanitize_input)
  
  eda_results <- create_eda_server("eda", data_state)
  dea_results <- create_dea_server("dea", data_state)
  pathway_results <- create_pathway_server("pathway", dea_results)
  
  # AI interpretation with API key validation
  ai_interpretation <- create_ai_interpretation_server(
    "ai_interpretation", 
    dea_results, 
    pathway_results,
    validate_api_key = validate_api_key
  )
  
  dashboard_data <- create_dashboard_server("dashboard", data_state, dea_results, pathway_results)
  
  # Update data state when upload completes
  observe({
    req(upload_data())
    tryCatch({
      data_state(upload_data())
      log_security_event("DATA_UPLOAD", "Data uploaded successfully", "INFO")
    }, error = function(e) {
      log_security_event("DATA_UPLOAD_ERROR", e$message, "ERROR")
      showNotification("Error processing uploaded data", type = "error")
    })
  })
  
  # Update data state when GEO data is loaded
  observe({
    geo_result <- geo_data()
    if (!is.null(geo_result)) {
      tryCatch({
        data_state(list(
          count_matrix = geo_result$counts,
          metadata = geo_result$metadata,
          feature_metadata = geo_result$feature_metadata,
          parameters = analysis_params
        ))
        log_security_event("GEO_DATA_LOAD", "GEO data loaded successfully", "INFO")
      }, error = function(e) {
        log_security_event("GEO_DATA_ERROR", e$message, "ERROR")
        showNotification("Error loading GEO data", type = "error")
      })
    }
  })
  
  # File validation observer with enhanced security
  observe({
    req(input$countFile, input$metadataFile)
    
    tryCatch({
      # Validate count file
      validate(
        need(
          validate_file_size(input$countFile$datapath, upload_config$max_size),
          sprintf("Count file size must be less than %d MB", upload_config$max_size/1024^2)
        ),
        need(
          validate_file_type(input$countFile$name, upload_config$allowed_types),
          sprintf(
            "Count file type must be one of: %s",
            paste(upload_config$allowed_types, collapse = ", ")
          )
        ),
        need(
          check_file_safety(input$countFile$datapath),
          "Count file contains potentially unsafe content"
        )
      )
      
      # Validate metadata file
      validate(
        need(
          validate_file_size(input$metadataFile$datapath, upload_config$max_size),
          sprintf("Metadata file size must be less than %d MB", upload_config$max_size/1024^2)
        ),
        need(
          validate_file_type(input$metadataFile$name, upload_config$allowed_types),
          sprintf(
            "Metadata file type must be one of: %s",
            paste(upload_config$allowed_types, collapse = ", ")
          )
        ),
        need(
          check_file_safety(input$metadataFile$datapath),
          "Metadata file contains potentially unsafe content"
        )
      )
      
      log_security_event("FILE_VALIDATION", "Files validated successfully", "INFO")
    }, error = function(e) {
      log_security_event("FILE_VALIDATION_ERROR", e$message, "ERROR")
      showNotification(e$message, type = "error")
    })
  })

  # Handle session ending with cleanup
  session$onSessionEnded(function() {
    tryCatch({
      # Clean up temporary files
      temp_files <- list.files(
        tempdir(),
        pattern = "sigmaSEQ",
        full.names = TRUE
      )
      unlink(temp_files)

      # Clear cache if it exists
      if (exists("cache")) {
        cache$purge()
      }

      # Log session end
      log_security_event("SESSION_END", "Session ended normally", "INFO")
      log_info(sprintf(
        "Session ended for %s v%s, cleanup complete",
        get_config("app_name"),
        get_config("app_version")
      ))
    }, error = function(e) {
      log_security_event("SESSION_CLEANUP_ERROR", e$message, "ERROR")
    })
  })
}

# Set application options
options(
  shiny.port = 3838,
  shiny.host = "127.0.0.1",
  shiny.launch.browser = TRUE,
  shiny.maxRequestSize = get_config("max_upload_size"),
  shiny.trace = TRUE  # Enables Shiny tracing for debugging
)

# Run the application
shinyApp(ui = ui, server = server)


# User Interface 

ui <- dashboardPage(
  help = NULL,
  fullscreen = TRUE,
  
  
  #layout
  
  title = "SigmaSEQ: RNASeq Analysis Tool", 
  header = dashboardHeader(
    title = dashboardBrand(
      title = "SigmaSEQ",
      image = "https://raw.githubusercontent.com/lujason01/r-practice/refs/heads/main/sigmaseq-high-resolution-logo.png"
    ),
    rightUi = dropdownMenu(
      badgeStatus = "info", 
      type = "notifications",
      notificationItem(
        text = "Success",
        status = "success",
        icon = icon("circle-check")
      ), 
      notificationItem(
        text = "Warning",
        status = "warning",
        icon = icon("circle-exclamation")
      ),
      notificationItem(
        text = "Error",
        status = "danger",
        icon = icon("circle-xmark")
      )
    )
  ),
  
  #side bar ----- TABS -----
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("line-chart")
      ), 
      menuItem(
        "New Analysis",
        tabName = "newanalysis",
        icon = icon("add")
      )
    )
  ), 
  
  controlbar = dashboardControlbar(),
  
  #footer
  footer = dashboardFooter(
    
    tags$div(class = "main-footer", "sigmaSEQ_v1 © 2025 - All Rights Reserved")
  ),
  
  # BODY
  # Here we will try to organise the pages of the app
  # Both home and the dashboard page
  body = dashboardBody(
    
    #set font style and Background Images for dashboard and newanalysis tabs 
    tags$head(
      
      tags$link(href = "https://fonts.googleapis.com/css2?family=Winky+Sans:ital,wght@0,300..900;1,300..900&display=swap", rel = "stylesheet"),
      
      tags$style(HTML("
      
        body, .content-wrapper, .main-header, .main-sidebar {
          font-family: Winky Sans, sans-serif;
        }
        
        .content-wrapper {
          padding-bottom: 50px; /* Prevents content from overlapping the footer */
        }
        .main-footer {
          position: fixed;
          bottom: 0;
          left: 0;
          width: 100%;
          background-color: #222d32; /* Dark footer background */
          color: white;
          text-align: center;
          padding: 10px;
          font-size: 14px;
          z-index: 1000; /* Ensures it stays above other elements */
        }
      
      
         .dashboard-tab {
          background-image: url('https://github.com/lujason01/r-practice/blob/e7f5b72cdc6a6a4b1af70e4a41bd54835d8fc823/sigmaseq-high-resolution-logo-grayscale-transparent.png?raw=true');
          background-attachment: fixed;
          background-size: contain;
          background-position: center center;
          background-repeat: no-repeat;
          height: 100vh;
         }
        
        
        .newanalysis-tab {
          background-image: url('https://raw.githubusercontent.com/lujason01/r-practice/refs/heads/main/Sigmaseq%20washed-out%20background.png');
          background-attachment: fixed;
          background-size: contain;
          background-position: center center;
          background-repeat: no-repeat;
          height: 100vh;
        }
        
        
      "))
    ),
    
    
    #TABS content-----------------------------------
    
    tabItems(
      
      # Home tab ----
      tabItem(
        tabName = "home",
        class = "home-tab",
        
        jumbotron(
          title = "Hi, welcome to SigmaSEQ!",
          lead = "This is SigmaSEQ, a platform that aims to automate RNA Seq Analysis.", "\n This demo dataset will explore transcriptomic changes in MDS patients vs Young and Elderly Healthy controls.",
          status = "info",
          href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE183325",
          btnName = "Link to Dataset", 
          "Data available from the GEO Database, Accession: GSE183325"
        ),
        
        br(),
        
        fluidRow(
          
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "Jason Lubega",
              subtitle = "Developer",
              image = "https://media.licdn.com/dms/image/v2/D4D03AQFj45mW2Tlbuw/profile-displayphoto-shrink_800_800/B4DZUDsx7TGkAc-/0/1739523827018?e=1749081600&v=beta&t=4jGvrMFGkK-jER-QUq9kVoh0y1lPvbbtl73S6Ji5jWo",
              type = 1
            ),
            status = "success",
            " Aserejé, ja, dejé \n
Dejebe tu dejebere seibiunouva
Majavi an de bugui an de buididipi"
          ),
          
          box(
            title = "Inspiration",
            width = 6,
            collapsible = FALSE,
            blockQuote("My first RNA Seq analysis took me a whole 2 weeks. It'd be better not to spend the same amount of time on an RNA seq Analysis project again. \n
                       Therefore, I have built the platform to automate this whole process and make this niche accessible to fellow scientists who would like to make sense of their experiments.", color = "indigo")
          )
        )
      ),
      
      # Dashboard tab ----
      
      tabItem(
        tabName = "dashboard", 
        class = "dashboard-tab",
        
        ## Info boxes ----
        fluidRow(
          
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Total Counts",
              icon = icon("list"),
              color = "primary"
            )
          ),
          
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Significant Genes",
              value = n_sig_Genes,
              icon = icon("dove"),
              color = "primary"
            )
          ),
          
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Enriched Pathways",
              icon = icon("location-dot"),
              color = "primary"
            )
          )
        ),
        
        
        # EXPLORATORY DATA ANALYSIS SECTION
        
        bs4Card(
          title = "Exploratory Data Analysis",
          width = 12,
          status = NULL,  # No background color
          solidHeader = FALSE,
          headerBorder = FALSE,
          background = NULL,
          
          
          ## Sortable boxes ---- (movable or draggable)
          fluidRow(
            sortable(
              width = 6,
              
              box(
                title = "Box 1: Raw vs Normalised Reads", 
                width = 12, 
                status = "olive",
                collapsible = TRUE, 
                maximizable = TRUE
                
              ),
              
              box(
                title = "Box 2: PCA Plot",
                width = 12, 
                closable = TRUE, 
                status = "olive",
                collapsible = TRUE, 
                maximizable = TRUE
                
              )
            ) 
          )
        ),
        
        
        # DIFERENTIAL GENE EXPRESSION SECTION
        
        bs4Card(
          title = "Differential Expression Analysis",
          width = 12,
          status = NULL,  # No background color
          solidHeader = FALSE,
          headerBorder = FALSE,
          background = NULL,
          
          
          ## Sortable boxes ---- (movable or draggable)
          fluidRow(
            sortable(
              width = 6,
              
              #heatmap -------------------------
              
              box(
                title = "Significant Differentially Expressed Genes",
                width = 12,  
                status = "olive",
                collapsible = TRUE,
                maximizable = TRUE,
                plotOutput("DEGenes_heatmap")
                
                
                
              ),
              
              box(
                title = "Volcano Plot",
                width = 12, 
                status = "olive",
                collapsible = TRUE,
                maximizable = TRUE,
                label = boxLabel(
                  text = "Label", 
                  status = "primary", 
                  tooltip = "I'm a label!")
                
              )
            )
            
          )
        ),
        
        bs4Card(
          title = "Pathway Analysis",
          width = 12,
          status = NULL,  # No background color
          solidHeader = FALSE,
          headerBorder = FALSE,
          background = NULL,
          
          
          ## Sortable boxes ---- (movable or draggable)
          fluidRow(
            sortable(
              width = 6,
              
              box(
                title = "Over Representation Analysis: ORA GO", 
                width = 12, 
                status = "danger",
                collapsible = TRUE, 
                maximizable = TRUE
                
              ),
              
              box(
                title = "GSEA: Encriched Pathways",
                width = 12, 
                closable = TRUE, 
                status = "olive",
                collapsible = TRUE, 
                maximizable = TRUE
                
              )
            ) 
          )
        )
        
      ),
      
      
      # New Analysis tab ----
      
      tabItem(
        tabName = "newanalysis", 
        class = "newanalysis-tab", # Applies the class for background image
        
        
        ## Tab box ----
        tabBox(
          title = "Overview",
          width = 12,
          type = "tabs",
          status = "olive",
          solidHeader = TRUE
        ), 
        
        
        ##--- More new analysis tab content
        fluidRow(
          
          column(
            width = 12,
            box(
              width = 12,
              title = "Start Here",
              color = "success"
            )
          )
          
          
        )
      )
    )
    
  )
)



# Define server 
server <- function(input, output) {
  
  # Heatmap Box 3 --------------------------------------------------
  output$DEGenes_heatmap  <- renderPlot({
    plot(DEGenes_heatmap)
  })
  
  
  # Data table (sig_Genes_ordFch_annotated) --------------------------------------------------
  
  output$sig_Genes_ordFch_annotated <- renderReactable({
    reactable(sig_Genes_ordFch_annotated, pagination = FALSE, compact = TRUE)
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
>>>>>>> a354f121ff26387cb29cf3edeffd1858e20d8183
