
# ============================================================
# ESM Forecasting App — Version 1
# ============================================================
# Upload individual-level ESM data, generate forecasts,
# and display forecasts.
#
# Data requirements:
# - One participant per CSV file
# - One row per scheduled observation
# - Rows ordered chronologically
# - Target and predictor variables must be numeric
# ============================================================


# 1. Load required packages ----------------------------------

library(shiny)
library(forecast)
library(ggplot2)
library(zoo)
# Load data-preparation functions
source("R/data_preparation.R")
source("R/forecasting.R")
source("R/plotting.R")


# 2. Define the user interface -------------------------------

ui <- fluidPage(
  
  titlePanel("ESM Forecasting App"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("1. Upload data"),
      
      fileInput(
        inputId = "data_file",
        label = "Choose a CSV file",
        accept = ".csv"
      ),
      
      numericInput(
        inputId = "beeps_per_day",
        label = "Observations per day",
        value = 3,
        min = 1,
        step = 1
      ),
      
      hr(),
      
      h4("2. Select variables"),
      
      selectInput(
        inputId = "target",
        label = "Variable to forecast",
        choices = NULL
      ),
      
      # Target response-scale limits
      numericInput(
        "scale_min",
        "Minimum possible target score",
        value = 0
      ),
      
      numericInput(
        "scale_max",
        "Maximum possible target score",
        value = 100
      ),
      
      selectInput(
        "predictors",
        "External predictors (for ARIMAX)",
        choices = NULL,
        multiple = TRUE
      ),
      
      hr(),
      
      h4("3. Generate forecast"),
      
      selectInput(
        "model",
        "Forecasting model",
        choices = c(
          "Naive" = "Naive",
          "ARIMA" = "ARIMA",
          "ARIMAX" = "ARIMAX"
        ),
        selected = "Naive"
      ),
      
      actionButton(
        "generate_forecast",
        "Generate forecast"
      ),
      
      hr(),
      
    ),
    
    mainPanel(
      
      h4("Data preview"),
      
      tableOutput("data_preview"),
      
      hr(),
      
      h4("Data information"),
      
      verbatimTextOutput("data_info"),
      
      hr(),
      
      h4("Data validation"),
      uiOutput("validation_status"),
      
      hr(),
      
      h4("Forecast results"),
      uiOutput("forecast_status"),
      checkboxInput(
        "zoom_scale",
        "Zoom to target scale range",
        value = FALSE
      ),
      
      plotOutput("forecast_plot", height = "450px"),
      
      hr(),
      
    )
    
  )
)


# 3. Define the server logic ---------------------------------

server <- function(input, output, session) {
  
  # 3.1 Read the uploaded CSV file
  
  uploaded_data <- reactive({
    
    req(input$data_file)
    
    read.csv(
      file = input$data_file$datapath,
      stringsAsFactors = FALSE
    )
    
  })
  
  
  # 3.2 Update the variable selection menus
  
  observeEvent(uploaded_data(), {
    
    data <- uploaded_data()
    
    # Only numeric columns can be used for forecasting.
    numeric_columns <- names(data)[
      vapply(data, is.numeric, logical(1))
    ]
    
    updateSelectInput(
      session = session,
      inputId = "target",
      choices = numeric_columns,
      selected = if (length(numeric_columns) > 0) {
        numeric_columns[1]
      } else {
        character(0)
      }
    )
    
    updateSelectInput(
      session = session,
      inputId = "predictors",
      choices = numeric_columns,
      selected = character(0)
    )
    
  })
  
  
  # 3.3 Show the first six rows of the uploaded dataset
  
  output$data_preview <- renderTable({
    
    head(uploaded_data())
    
  })
  
  
  # 3.4 Display basic information about the data
  
  output$data_info <- renderPrint({
    
    data <- uploaded_data()
    
    cat("Number of observations:", nrow(data), "\n")
    cat("Number of variables:", ncol(data), "\n")
    
    cat(
      "Numeric variables:",
      sum(vapply(data, is.numeric, logical(1))),
      "\n"
    )
    
    cat(
      "Total missing values:",
      sum(is.na(data)),
      "\n"
    )
    
  })
  
  # 3.5 Prevent the target from being selected as a predictor
  
  observeEvent(input$target, {
    
    req(uploaded_data(), input$target)
    
    numeric_columns <- names(uploaded_data())[
      vapply(uploaded_data(), is.numeric, logical(1))
    ]
    
    available_predictors <- setdiff(
      numeric_columns,
      input$target
    )
    
    updateSelectInput(
      session = session,
      inputId = "predictors",
      choices = available_predictors,
      selected = intersect(
        input$predictors,
        available_predictors
      )
    )
    
  })
  
  
  # 3.6 Validate the selected variables
  
  validation_message <- reactive({
    
    req(uploaded_data(), input$target)
    
    validate_esm_data(
      data = uploaded_data(),
      target = input$target,
      predictors = input$predictors
    )
    
  })
  
  
  # 3.7 Display validation results
  
  output$validation_status <- renderUI({
    
    message <- validation_message()
    
    if (!is.null(message)) {
      
      div(
        style = "color: #B22222;",
        strong("Data validation failed"),
        p(message)
      )
      
    } else {
      
      div(
        style = "color: #228B22;",
        strong("Selected variables are valid.")
      )
      
    }
    
  })
  
  
  # 3.8 Track which variables have been approved for LOCF -----
  
  approved_locf_variables <- reactiveVal(character(0))
  pending_locf_variables <- reactiveVal(character(0))
  
  # A new dataset requires new LOCF confirmation.
  observeEvent(input$data_file, {
    
    approved_locf_variables(character(0))
    pending_locf_variables(character(0))
    
    removeModal()
    
  })
  
  
  # 3.9 Ask for LOCF confirmation when necessary ---------------
  
  observeEvent(
    list(uploaded_data(), input$target, input$predictors),
    {
      
      req(uploaded_data(), input$target)
      
      # Do not proceed if validation has failed.
      if (!is.null(validation_message())) {
        return()
      }
      
      selected_variables <- unique(c(
        input$target,
        input$predictors
      ))
      
      selected_data <- uploaded_data()[selected_variables]
      
      # Identify the selected variables containing missing values.
      variables_with_na <- names(selected_data)[
        vapply(selected_data, anyNA, logical(1))
      ]
      
      # Only ask about variables not previously approved.
      new_variables_with_na <- setdiff(
        variables_with_na,
        approved_locf_variables()
      )
      
      if (length(new_variables_with_na) == 0) {
        return()
      }
      
      pending_locf_variables(new_variables_with_na)
      
      showModal(
        modalDialog(
          
          title = "Missing values detected",
          
          p(
            "The following selected variable(s) contain",
            "missing observations:"
          ),
          
          tags$ul(
            lapply(
              new_variables_with_na,
              tags$li
            )
          ),
          
          p(
            "For modeling purposes, the app will use",
            "Last Observation Carried Forward (LOCF).",
            "Each missing value will be replaced with",
            "the most recent available observation."
          ),
          
          p(
            "Missing values at the beginning of the",
            "series cannot be filled using LOCF."
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton("accept_locf", "Accept LOCF")
          ),
          
          easyClose = FALSE
        )
      )
      
    }
  )
  
  
  # 3.10 Record the user's LOCF confirmation -------------------
  
  observeEvent(input$accept_locf, {
    
    approved_locf_variables(
      union(
        approved_locf_variables(),
        pending_locf_variables()
      )
    )
    
    pending_locf_variables(character(0))
    
    removeModal()
    
  })
  
  
  # 3.11 Prepare data after validation and LOCF approval --------
  
  prepared_esm <- reactive({
    
    req(is.null(validation_message()))
    
    selected_variables <- unique(c(
      input$target,
      input$predictors
    ))
    
    selected_data <- uploaded_data()[selected_variables]
    
    variables_with_na <- names(selected_data)[
      vapply(selected_data, anyNA, logical(1))
    ]
    
    # All selected variables with NAs must be approved.
    req(
      all(variables_with_na %in% approved_locf_variables())
    )
    
    prepare_esm_data(
      data = uploaded_data(),
      target = input$target,
      predictors = input$predictors
    )
    
  })
  
  
  # ------------------------------------------------------------
  # Generate forecasts when the user clicks the button
  # ------------------------------------------------------------
  
  forecast_result <- reactiveVal(NULL)
  
  
  # Clear old forecasts when relevant inputs change.
  observeEvent(
    list(
      input$data_file,
      input$target,
      input$predictors,
      input$model,
      input$beeps_per_day
    ),
    {
      forecast_result(NULL)
    },
    ignoreInit = TRUE
  )
  
  
  observeEvent(input$generate_forecast, {
    
    # Clear the previous result before starting.
    forecast_result(NULL)
    
    # Check that data and variables are available.
    req(uploaded_data(), input$target, input$model)
    
    # Display validation errors without crashing the app.
    validation_error <- validation_message()
    
    if (!is.null(validation_error)) {
      showNotification(
        validation_error,
        type = "error"
      )
      return()
    }
    
    # ARIMAX needs at least one predictor.
    if (input$model == "ARIMAX" &&
        length(input$predictors) == 0) {
      
      showNotification(
        "Please select at least one external predictor for ARIMAX.",
        type = "error"
      )
      return()
    }
    
    # Ensure missing-data handling has been approved.
    selected_variables <- if (input$model == "ARIMAX") {
      unique(c(input$target, input$predictors))
    } else {
      input$target
    }
    
    variables_with_na <- names(
      uploaded_data()[selected_variables]
    )[
      vapply(
        uploaded_data()[selected_variables],
        anyNA,
        logical(1)
      )
    ]
    
    if (!all(variables_with_na %in% approved_locf_variables())) {
      showNotification(
        "Please accept the missing-value handling before forecasting.",
        type = "warning"
      )
      return()
    }
    
    # Prepare only the variables needed by the chosen model.
    model_predictors <- if (input$model == "ARIMAX") {
      input$predictors
    } else {
      character(0)
    }
    
    # Run the preparation and forecasting functions.
    result <- tryCatch({
      
      prepared <- prepare_esm_data(
        data = uploaded_data(),
        target = input$target,
        predictors = model_predictors
      )
      
      generate_forecast(
        data = prepared$data,
        target = input$target,
        predictors = model_predictors,
        model = input$model,
        beeps_per_day = input$beeps_per_day
      )
      
    }, error = function(e) {
      
      showNotification(
        conditionMessage(e),
        type = "error",
        duration = NULL
      )
      
      NULL
    })
    
    # Store the result only if forecasting succeeded.
    if (!is.null(result)) {
      forecast_result(result)
    }
    
  })
  
  # ------------------------------------------------------------
  # Display forecast status and preview
  # ------------------------------------------------------------
  
  
  output$forecast_status <- renderUI({
    result <- forecast_result()
    if (is.null(result)) return(p("Select a model and click 'Generate forecast'."))
    p("Forecast generated: ", nrow(result), " future observations predicted.")
  })

  output$forecast_plot <- renderPlot({
    
    req(forecast_result())
    
    # Validate the user-specified response-scale limits.
    validate(
      need(
        is.finite(input$scale_min) &&
          is.finite(input$scale_max) &&
          input$scale_min < input$scale_max,
        "Please enter valid scale limits (minimum below maximum)."
      )
    )
    
    # Apply scale limits only when zoom is enabled.
    plot_min <- NULL
    plot_max <- NULL
    
    if (isTRUE(input$zoom_scale)) {
      plot_min <- input$scale_min
      plot_max <- input$scale_max
    }
    
    plot_esm_forecast(
      original_target = uploaded_data()[[input$target]],
      forecast_results = forecast_result(),
      target_name = input$target,
      beeps_per_day = input$beeps_per_day,
      scale_min = plot_min,
      scale_max = plot_max
    )
    
  })
  
  
  
  
}


# 4. Launch the application ----------------------------------

shinyApp(ui = ui, server = server)