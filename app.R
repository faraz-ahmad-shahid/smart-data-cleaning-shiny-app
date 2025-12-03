# Install required packages first (run once):
# install.packages(c("shiny", "readr", "readxl", "jsonlite", "dplyr", "DT"))

library(shiny)
library(readr)
library(readxl)
library(jsonlite)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("Smart Data Cleaning & KPI App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Your Data File",
                accept = c(".csv", ".xlsx", ".xls", ".json", ".txt")),
      
      hr(),
      
      conditionalPanel(
        condition = "output.dataUploaded",
        h4("Export Cleaned Data"),
        downloadButton("downloadCSV", "Download CSV"),
        downloadButton("downloadExcel", "Download Excel"),
        downloadButton("downloadJSON", "Download JSON")
      ),
      
      hr(),
      
      h4("App Info"),
      p("This app automatically:"),
      tags$ul(
        tags$li("Removes duplicates"),
        tags$li("Fills missing values"),
        tags$li("Fixes data types"),
        tags$li("Standardizes formats"),
        tags$li("Generates KPIs")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 h3("Data Summary"),
                 uiOutput("statsBoxes"),
                 hr(),
                 h3("Data Preview"),
                 DTOutput("dataPreview")
        ),
        
        tabPanel("Issues Found",
                 h3("Cleaning Report"),
                 verbatimTextOutput("issuesReport"),
                 DTOutput("issuesTable")
        ),
        
        tabPanel("KPIs",
                 h3("Suggested Key Performance Indicators"),
                 uiOutput("kpiBoxes"),
                 hr(),
                 plotOutput("kpiCharts")
        ),
        
        tabPanel("Full Data",
                 h3("Complete Cleaned Dataset"),
                 DTOutput("fullData")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store data
  rv <- reactiveValues(
    original = NULL,
    cleaned = NULL,
    issues = NULL,
    kpis = NULL
  )
  
  # Read uploaded file
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      ext <- tools::file_ext(input$file$name)
      
      # Read based on file type
      data <- switch(ext,
                     csv = read_csv(input$file$datapath, show_col_types = FALSE),
                     txt = read_delim(input$file$datapath, show_col_types = FALSE),
                     xlsx = read_excel(input$file$datapath),
                     xls = read_excel(input$file$datapath),
                     json = fromJSON(input$file$datapath, flatten = TRUE),
                     stop("Unsupported file type")
      )
      
      rv$original <- data
      
      # Clean data
      cleaned_result <- cleanData(data)
      rv$cleaned <- cleaned_result$data
      rv$issues <- cleaned_result$issues
      
      # Generate KPIs
      rv$kpis <- generateKPIs(rv$cleaned)
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Data cleaning function
  cleanData <- function(data) {
    issues <- data.frame(
      Type = character(),
      Column = character(),
      Row = integer(),
      Action = character(),
      stringsAsFactors = FALSE
    )
    
    cleaned <- data
    row_count <- nrow(cleaned)
    
    # Remove duplicates
    dup_rows <- duplicated(cleaned)
    if (any(dup_rows)) {
      dup_indices <- which(dup_rows)
      issues <- rbind(issues, data.frame(
        Type = "Duplicate",
        Column = "All",
        Row = dup_indices,
        Action = "Removed"
      ))
      cleaned <- cleaned[!dup_rows, ]
    }
    
    # Handle missing values and type conversion
    for (col in names(cleaned)) {
      na_count <- sum(is.na(cleaned[[col]]))
      
      if (na_count > 0) {
        # Try to determine if numeric
        non_na_vals <- cleaned[[col]][!is.na(cleaned[[col]])]
        
        if (length(non_na_vals) > 0) {
          # Check if numeric
          if (is.numeric(non_na_vals) || all(grepl("^[0-9.]+$", non_na_vals))) {
            cleaned[[col]][is.na(cleaned[[col]])] <- 0
            issues <- rbind(issues, data.frame(
              Type = "Missing Value",
              Column = col,
              Row = which(is.na(data[[col]])),
              Action = "Filled with 0"
            ))
          } else {
            cleaned[[col]][is.na(cleaned[[col]])] <- "N/A"
            issues <- rbind(issues, data.frame(
              Type = "Missing Value",
              Column = col,
              Row = which(is.na(data[[col]])),
              Action = "Filled with N/A"
            ))
          }
        }
      }
      
      # Auto-convert numeric columns
      if (is.character(cleaned[[col]])) {
        if (all(grepl("^[0-9.]+$", cleaned[[col]][cleaned[[col]] != "N/A"]))) {
          cleaned[[col]] <- as.numeric(cleaned[[col]])
        }
      }
    }
    
    list(data = cleaned, issues = issues)
  }
  
  # Generate KPIs
  generateKPIs <- function(data) {
    kpis <- list()
    
    for (col in names(data)) {
      if (is.numeric(data[[col]])) {
        vals <- data[[col]][!is.na(data[[col]])]
        
        kpis[[paste0(col, "_total")]] <- list(
          name = paste(col, "- Total"),
          value = sum(vals, na.rm = TRUE)
        )
        
        kpis[[paste0(col, "_avg")]] <- list(
          name = paste(col, "- Average"),
          value = mean(vals, na.rm = TRUE)
        )
        
        kpis[[paste0(col, "_max")]] <- list(
          name = paste(col, "- Maximum"),
          value = max(vals, na.rm = TRUE)
        )
        
        kpis[[paste0(col, "_min")]] <- list(
          name = paste(col, "- Minimum"),
          value = min(vals, na.rm = TRUE)
        )
      } else {
        unique_count <- length(unique(data[[col]]))
        kpis[[paste0(col, "_unique")]] <- list(
          name = paste(col, "- Unique Values"),
          value = unique_count
        )
      }
    }
    
    kpis
  }
  
  # Check if data uploaded
  output$dataUploaded <- reactive({
    !is.null(rv$cleaned)
  })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  
  # Stats boxes
  output$statsBoxes <- renderUI({
    req(rv$cleaned)
    
    total_rows <- nrow(rv$cleaned)
    total_cols <- ncol(rv$cleaned)
    issues_count <- nrow(rv$issues)
    
    fluidRow(
      column(4, 
             div(style = "background: #e3f2fd; padding: 20px; border-radius: 8px; text-align: center;",
                 h4("Total Rows"),
                 h2(total_rows)
             )
      ),
      column(4,
             div(style = "background: #e8f5e9; padding: 20px; border-radius: 8px; text-align: center;",
                 h4("Total Columns"),
                 h2(total_cols)
             )
      ),
      column(4,
             div(style = "background: #fff3e0; padding: 20px; border-radius: 8px; text-align: center;",
                 h4("Issues Fixed"),
                 h2(issues_count)
             )
      )
    )
  })
  
  # Data preview
  output$dataPreview <- renderDT({
    req(rv$cleaned)
    datatable(head(rv$cleaned, 10), options = list(scrollX = TRUE))
  })
  
  # Issues report
  output$issuesReport <- renderText({
    req(rv$issues)
    if (nrow(rv$issues) == 0) {
      "No issues found! Your data was already clean."
    } else {
      paste("Found and fixed", nrow(rv$issues), "issues in your data.")
    }
  })
  
  output$issuesTable <- renderDT({
    req(rv$issues)
    if (nrow(rv$issues) > 0) {
      datatable(rv$issues, options = list(pageLength = 25, scrollX = TRUE))
    }
  })
  
  # KPI boxes
  output$kpiBoxes <- renderUI({
    req(rv$kpis)
    
    kpi_items <- lapply(names(rv$kpis)[1:min(12, length(rv$kpis))], function(k) {
      kpi <- rv$kpis[[k]]
      column(4,
             div(style = "background: #f3e5f5; padding: 15px; border-radius: 8px; margin-bottom: 10px;",
                 h5(kpi$name),
                 h3(round(kpi$value, 2))
             )
      )
    })
    
    do.call(fluidRow, kpi_items)
  })
  
  # Full data table
  output$fullData <- renderDT({
    req(rv$cleaned)
    datatable(rv$cleaned, options = list(pageLength = 50, scrollX = TRUE))
  })
  
  # Download handlers
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste0("cleaned_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(rv$cleaned, file)
    }
  )
  
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste0("cleaned_data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(rv$cleaned, file)
    }
  )
  
  output$downloadJSON <- downloadHandler(
    filename = function() {
      paste0("cleaned_data_", Sys.Date(), ".json")
    },
    content = function(file) {
      write(toJSON(rv$cleaned, pretty = TRUE), file)
    }
  )
}

shinyApp(ui = ui, server = server)
