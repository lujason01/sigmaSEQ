
setwd("C:/Users/Lenovo/MIB Assignments/NOTES/Bioinformatica/Bioinformatics Practice R/lujasonRNASeqApp/ShinyTestapp.R")

library(shiny)
library(bs4Dash)
library(DT)
library(plotly)
library(ggplot2)
#install.packages("reactable")
library(reactable)

install.packages(c("httr", "jsonlite", "base64enc"))
library(httr)
library(jsonlite) #capturing images for chat's explanation 
library(base64enc) #enables retrieval via OpenAi vision

library(devtools)
#devtools::install_github("Broccolito/gptr")
library(gptr) #gptr package queries chatGPT


## ChatGPT setup -----------------------------------------------------------

#install.packages("openai") #openai package
library(openai)

#Sys.setenv(OPENAI_API_KEY = "YOUR_API_KEY") # set API key

#set model
# Set the model to GPT-4o
#model <- "gpt-4o"

# Define your prompt (TEXT GENERATION, Modify to screen grabbing api prompt)
#prompt <- "Write a short poem about a rainy day."

# Call the API
#response <- chatgpt(
#  prompt = prompt,
#  model = model,
#  temperature = 0.7, # Adjust as needed
#  max_tokens = 100 # Adjust as needed
#)

# Print the response
#print(response$choices[[1]]$message$content)


###**CHECKLIST
###* Choose theme
###* Chose layout
###* Set sidebar or side menu (start, Exploratory Data, Differential Expression, Pathway Analysis
###* Set Nav Bar to clear session / Restart with a new dataset.


###* SUMMARY FOR EACH SECTION (Key Plots)
###* EDA (description table, Counts distribution plot (Combine raw and normalised), PCA plot)
###* DEA (Top 50 heatmap (20 bins default), Volcano plot, )
###* PA (ORA GO, GSEA Plots, GSEA tables)


###* MISCELENEOUS  
###* Set conditions to be analysed command to identify a group.
###* 
###*


# **************** LET's GO ****************** # 


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
