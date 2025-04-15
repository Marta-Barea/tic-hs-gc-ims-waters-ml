# Load required libraries
library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(viridis)
library(caret)       # for preProcess and predict
library(stringr)
library(egg)         # for theme_test (or use another theme if you prefer)
library(writexl)     # for writing Excel files in downloadHandler
library(kernlab)

# Load optimized SVM model (GA model)
model <- readRDS("svm_model_ga.rds")  # Ensure the file is in the correct path

# Load sample data for download (test file)
sample_test_data <- read_excel("test.xlsx")

# Define server logic
server <- function(input, output, session) {
  
  # Reactive variable indicating that data has been submitted
  data_submitted <- reactiveVal(FALSE)
  
  # Mark data as submitted when the "Submit" button is pressed
  observeEvent(input$submitbutton, {
    data_submitted(TRUE)
  })
  
  # Read the uploaded data (supports CSV and Excel)
  uploaded_data <- reactive({
    req(input$file1)
    file_path <- input$file1$datapath
    if (str_ends(tolower(file_path), "csv") || str_ends(tolower(file_path), "txt")) {
      read.csv(file_path, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
    } else if (str_detect(tolower(file_path), "(xlsx|xls)$")) {
      read_excel(file_path, col_names = input$header)
    } else {
      stop("Unsupported format. Please upload a CSV or Excel file.")
    }
  })
  
  # Display the uploaded data table (showing only the header)
  output$dataTable <- renderTable({
    req(data_submitted())
    head(uploaded_data())
  })
  
  # Generate the spectra plot using the new methodology
  output$spectraPlot <- renderPlot({
    req(data_submitted())
    df <- uploaded_data()
    
    # Check if the "Group" column exists; if not, display a message on the plot
    if (!"Group" %in% colnames(df)) {
      plot.new()
      text(0.5, 0.5, "The 'Group' column was not found in the data.", cex = 1.5)
      return()
    }
    
    # Calculate the mean by group
    mean_data <- df %>%
      group_by(Group) %>%
      summarise_all(mean)
    
    # Convert to long format for plotting and ensure the x-axis variable is numeric
    df_long <- melt(mean_data, id.vars = "Group")
    df_long$variable <- as.numeric(as.character(df_long$variable))
    
    ggplot(df_long, aes(x = variable, y = value, color = Group, group = Group)) +
      geom_line() +
      labs(
        x = "Drift Time [RIP Relative]",
        y = "Total Intensity (V)"
      ) +
      theme_test() +
      theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8, hjust = 1, angle = 90),
        axis.title = element_text(size = 8)
      ) +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 100),
        labels = scales::number_format(accuracy = 0.01),
        expand = c(0, 0)
      ) +
      scale_color_viridis(discrete = TRUE, option = "viridis") +
      scale_fill_viridis(discrete = TRUE)
  })
  
  # Generate predictions using the required variables and proper scaling
  output$predictions <- renderTable({
    req(data_submitted())
    df <- uploaded_data()
    
    # Preprocess: convert column names to the expected format.
    # For each column (except "Group"), if it doesn't start with "X" and begins with a digit,
    # add "X" to the beginning.
    df_transformed <- df
    old_names <- colnames(df_transformed)
    new_names <- sapply(old_names, function(x) {
      if (x == "Group") {
        return(x)
      } else if (!startsWith(x, "X") && grepl("^[0-9]", x)) {
        return(paste0("X", x))
      } else {
        return(x)
      }
    })
    colnames(df_transformed) <- new_names
    
    # Assume the model contains the predictor names in 'coefnames'
    expected_vars <- model$coefnames
    if (is.null(expected_vars)) {
      stop("The model does not contain the expected predictor information.")
    }
    
    # Verify that all required variables are present
    missing_vars <- setdiff(expected_vars, colnames(df_transformed))
    if (length(missing_vars) > 0) {
      stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
    }
    
    # Select only the required variables
    data_to_predict <- df_transformed[, expected_vars, drop = FALSE]
    
    # Apply preprocessing (centering and scaling) using caret
    preProc <- preProcess(data_to_predict, method = c("center", "scale"))
    df_scaled <- predict(preProc, data_to_predict)
    
    # Make predictions using the SVM model
    predictions <- predict(model, newdata = df_scaled)
    
    # Create a data frame with the results.
    # Use the "Sample" column if available; otherwise, use row numbers.
    if ("Sample" %in% colnames(df_transformed)) {
      sample_ids <- df_transformed$Sample
    } else {
      sample_ids <- seq_len(nrow(df_transformed))
    }
    
    result_df <- data.frame(Sample = sample_ids, Prediction = predictions)
    result_df
  })
  
  # Download sample data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("sample_test_data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(sample_test_data, path = file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

# Define the user interface
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .card { background-color: #f8f9fa; border-radius: 10px; padding: 20px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); }
      .card-title { font-weight: bold; font-size: 1.5em; color: #343a40; margin-bottom: 10px; }
      .card-text { font-size: 1.1em; color: #6c757d; }
      .hero { background-color: #f4f7fc; text-align: center; padding: 50px 20px; margin-bottom: 20px; border-radius: 10px; }
      .hero h1 { font-size: 2.5em; font-weight: bold; color: #007bff; }
      .hero p { font-size: 1.3em; color: #6c757d; }
      .feature { text-align: center; padding: 20px; }
      .feature-icon { font-size: 3em; color: #007bff; margin-bottom: 10px; }
      .feature-text { font-size: 1.2em; color: #343a40; }
    "))
  ),
  
  navbarPage("AI4SEA App",
             tabPanel("Home",
                      div(class = "hero",
                          h1("Welcome to AI4SEA App"),
                          p("Advanced sample analysis using AI and HS-GC-IMS")
                      ),
                      fluidRow(
                        column(12, div(class = "card",
                                       h3(class = "card-title", "What is AI4SEA App?"),
                                       p(class = "card-text", "AI4SEA App is an advanced application for sample analysis using HS-GC-IMS. It employs preprocessing techniques such as variable scaling to enhance predictions with an SVM model optimized via GA.")
                        ))
                      ),
                      fluidRow(
                        column(4, div(class = "feature",
                                      div(class = "feature-icon", icon("upload")),
                                      div(class = "feature-text", "Upload your data (.csv or .xlsx)")
                        )),
                        column(4, div(class = "feature",
                                      div(class = "feature-icon", icon("chart-line")),
                                      div(class = "feature-text", "View your spectra")
                        )),
                        column(4, div(class = "feature",
                                      div(class = "feature-icon", icon("brain")),
                                      div(class = "feature-text", "Get predictions with AI")
                        ))
                      )
             ),
             
             tabPanel("Predictions",
                      titlePanel("Upload Your Data"),
                      sidebarLayout(
                        sidebarPanel(
                          downloadButton("downloadData", label = "Download Sample Data"),
                          helpText("Download a sample dataset to test the application."),
                          tags$hr(),
                          fileInput("file1", "Select a file:",
                                    multiple = FALSE,
                                    accept = c(".csv", ".txt", ".xlsx", ".xls")
                          ),
                          helpText("Supported file formats: .csv, .txt, and Excel (.xlsx, .xls)."),
                          tags$hr(),
                          actionButton("submitbutton", "Submit"),
                          helpText("Click 'Submit' after uploading your data."),
                          tags$hr(),
                          checkboxInput("header", "Header included?", TRUE),
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","
                          ),
                          radioButtons("sheet", "Sheet (Excel only)",
                                       choices = c("Sheet 1" = 1,
                                                   "Sheet 2" = 2,
                                                   "Sheet 3" = 3),
                                       selected = 1
                          ),
                          radioButtons("quote", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"'
                          )
                        ),
                        mainPanel(
                          conditionalPanel(
                            condition = "output.dataTable != null && input.submitbutton > 0",
                            tags$h3("Uploaded Data", style = "font-weight: bold; font-size: 1.5em; margin-top: 20px;")
                          ),
                          tableOutput("dataTable"),
                          conditionalPanel(
                            condition = "output.dataTable != null && input.submitbutton > 0",
                            tags$h3("Spectrum", style = "font-weight: bold; font-size: 1.5em; margin-top: 20px;"),
                            plotOutput("spectraPlot")
                          ),
                          conditionalPanel(
                            condition = "output.predictions != null && input.submitbutton > 0",
                            tags$h3("Predictions", style = "font-weight: bold; font-size: 1.5em; margin-top: 20px;")
                          ),
                          tableOutput("predictions")
                        )
                      )
             ),
             
             tabPanel("Collaborators",
                      fluidRow(
                        column(6, div(class = "card text-center",
                                      tags$img(src = "uca.png", alt = "University of Cádiz Logo", width = "80%"),
                                      h4("University of Cádiz"),
                                      p("Collaborating on advanced analytical methods and spectroscopic data analysis."),
                                      tags$a(href = "https://www.uca.es", target = "_blank", class = "btn btn-primary", "Visit Website")
                        )),
                        column(6, div(class = "card text-center",
                                      tags$img(src = "agr291.png", alt = "AGR-291 Logo", width = "80%"),
                                      h4("AGR-291 Research Group"),
                                      p("Experts in hydrocarbon characterization and spectroscopic techniques."),
                                      tags$a(href = "https://agr291.uca.es", target = "_blank", class = "btn btn-primary", "Visit Website")
                        ))
                      )
             )
  )
)

# Run the application
shinyApp(ui = ui, server = server)


