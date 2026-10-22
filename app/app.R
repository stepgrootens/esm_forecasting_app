
# ============================================================
# ESM Forecasting App — Version 4
# ============================================================
# Upload individual-level ESM data, generate forecasts,
# with optional model evaluation and data exploration.
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
source("R/cross_validation.R")
source("R/data_exploration.R")


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
      
      h4("4. Evaluate forecasting accuracy"),
      
      numericInput(
        "initial_training_days",
        "Initial training window (days)",
        value = 7,
        min = 1,
        step = 1
      ),
      
      actionButton(
        "calculate_accuracy",
        "Calculate accuracy"
      )
      
    ),
    
    mainPanel(
      
      h4("Data preview"),
      
      tableOutput("data_preview"),
      
      hr(),
      
      h3("Explore your data"),
      
      p(
        "Explore the distributions and missingness of your selected target",
        "and predictors before generating forecasts.",
        "All summaries use the original observed data, before imputation."
      ),
      
      h4("Descriptive statistics"),
      tableOutput("descriptive_stats"),
      
      h4("Missing observations"),
      tableOutput("missingness_stats"),
      
      h4("Distribution of the target"),
      plotOutput("target_distribution", height = "300px"),
      
      uiOutput("correlation_section"),
      
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
      
      h4("Cross-validation results"),
      
      uiOutput("cv_status"),
      
      tableOutput("cv_results")
      
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
  
  
  # ----------------------------------------------------------
  # DATA EXPLORATION
  # ----------------------------------------------------------
  
  # Variables selected for exploration
  exploration_variables <- reactive({
    
    req(uploaded_data(), input$target)
    
    unique(c(input$target, input$predictors))
  })
  
  # Descriptive statistics
  output$descriptive_stats <- renderTable({
    
    data <- uploaded_data()
    variables <- exploration_variables()
    
    req(length(variables) > 0)
    
    validate(
      need(
        all(variables %in% names(data)),
        "Please select valid variables."
      ),
      need(
        all(vapply(data[variables], is.numeric, logical(1))),
        "Selected variables must be numeric."
      )
    )
    
    calculate_descriptive_stats(data, variables)
    
  }, digits = 2, rownames = FALSE)
  
  # Missingness summary
  output$missingness_stats <- renderTable({
    
    data <- uploaded_data()
    variables <- exploration_variables()
    
    req(length(variables) > 0)
    
    validate(
      need(
        all(variables %in% names(data)),
        "Please select valid variables."
      )
    )
    
    calculate_missingness(data, variables)
    
  }, digits = 2, rownames = FALSE)
  
  # Target distribution
  output$target_distribution <- renderPlot({
    
    data <- uploaded_data()
    
    req(input$target)
    
    validate(
      need(
        input$target %in% names(data),
        "Please select a valid target."
      ),
      need(
        is.numeric(data[[input$target]]),
        "The target must be numeric."
      ),
      need(
        any(!is.na(data[[input$target]])),
        "The target has no observed values."
      )
    )
    
    plot_target_distribution(
      data = data,
      target = input$target
    )
  })
  
  
  # ----------------------------------------------------------
  # TARGET–PREDICTOR CORRELATIONS
  # ----------------------------------------------------------
  
  output$correlation_section <- renderUI({
    
    req(uploaded_data())
    
    # Only display when external predictors are selected
    if (length(input$predictors) == 0) {
      return(NULL)
    }
    
    tagList(
      h4("Correlations with selected external predictors"),
      
      tableOutput("target_correlations"),
      
      p(
        strong("How to interpret these correlations: "),
        "Pearson's r describes the strength and direction of the",
        "linear relationship between the target and each selected",
        "predictor at the same observation.",
        "Values closer to -1 or +1 indicate stronger linear",
        "relationships, while values near 0 indicate weaker",
        "linear relationships."
      ),
      
      p(
        strong("Important: "),
        "A strong correlation does not necessarily mean that a",
        "variable will improve ARIMAX forecasting accuracy.",
        "Correlations describe associations between observed values,",
        "whereas forecasting requires predicting future values.",
        "In this app, future predictor values are assumed to equal",
        "their historical means, which further limits what these",
        "correlations tell us about forecasting performance."
      ),
      
      p(
        em(
          "Correlations are calculated using original observations",
          "before imputation. Only observations with available",
          "values for both variables are included."
        )
      )
    )
  })
  
  
  output$target_correlations <- renderTable({
    
    data <- uploaded_data()
    
    req(input$target)
    req(length(input$predictors) > 0)
    
    variables <- c(input$target, input$predictors)
    
    validate(
      need(
        all(variables %in% names(data)),
        "Please select valid variables."
      ),
      need(
        all(vapply(data[variables], is.numeric, logical(1))),
        "The target and predictors must be numeric."
      ),
      need(
        !input$target %in% input$predictors,
        "The target cannot also be a predictor."
      )
    )
    
    results <- calculate_target_correlations(
      data = data,
      target = input$target,
      predictors = input$predictors
    )
    
    names(results) <- c(
      "Target",
      "External predictor",
      "Pearson's r",
      "Complete pairs"
    )
    
    results
    
  }, digits = 3, rownames = FALSE)
  
  
  
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
  
  # Store cross-validation results separately from forecasts
  cv_result <- reactiveVal(NULL)
  
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
  
  
  # Clear outdated accuracy results when relevant inputs change
  observeEvent(
    list(
      input$data_file,
      input$target,
      input$predictors,
      input$model,
      input$beeps_per_day,
      input$initial_training_days
    ),
    {
      cv_result(NULL)
    },
    ignoreInit = TRUE
  )
  
  
  # Run cross-validation when requested
  observeEvent(input$calculate_accuracy, {
    
    cv_result(NULL)
    
    req(uploaded_data(), input$target, input$model)
    
    validation_error <- validation_message()
    
    if (!is.null(validation_error)) {
      showNotification(validation_error, type = "error")
      return()
    }
    
    if (input$model == "ARIMAX" &&
        length(input$predictors) == 0) {
      
      showNotification(
        "Please select at least one external predictor for ARIMAX.",
        type = "error"
      )
      
      return()
    }
    
    # Only ARIMAX uses external predictors
    model_predictors <- if (input$model == "ARIMAX") {
      input$predictors
    } else {
      character(0)
    }
    
    selected_variables <- unique(
      c(input$target, model_predictors)
    )
    
    # Check whether LOCF approval is required
    variables_with_na <- names(
      uploaded_data()[selected_variables]
    )[
      vapply(
        uploaded_data()[selected_variables],
        anyNA,
        logical(1)
      )
    ]
    
    if (!all(
      variables_with_na %in% approved_locf_variables()
    )) {
      
      showNotification(
        "Please accept missing-value handling before calculating accuracy.",
        type = "warning"
      )
      
      return()
    }
    
    # Run the CV function and handle errors
    result <- tryCatch({
      
      prepared <- prepare_esm_data(
        uploaded_data(),
        input$target,
        model_predictors
      )
      
      cross_validate_esm(
        data = prepared$data,
        original_target = prepared$original_target,
        target = input$target,
        predictors = model_predictors,
        model = input$model,
        beeps_per_day = input$beeps_per_day,
        initial_training_days = input$initial_training_days
      )
      
    }, error = function(e) {
      
      showNotification(
        conditionMessage(e),
        type = "error",
        duration = NULL
      )
      
      NULL
    })
    
    if (!is.null(result)) {
      cv_result(result)
    }
    
  })
  
  
  # Display accuracy status and explanation
  output$cv_status <- renderUI({
    
    result <- cv_result()
    
    if (is.null(result)) {
      return(
        p("Select an initial training window and click 'Calculate accuracy'.")
      )
    }
    
    tagList(
      
      p(
        strong("Cross-validation completed. "),
        paste(
          result$total_model_fits,
          "model fits attempted;",
          result$fit_failures,
          "fitting/forecasting failures."
        )
      ),
      
      p(
        "Accuracy was evaluated using expanding-window",
        "cross-validation. At each step, the model was",
        "trained on the available historical observations",
        "and evaluated against actual observations at",
        "1-day and 7-day forecast horizons."
      ),
      
      p(
        strong("How to interpret MAE: "),
        "Mean Absolute Error (MAE) is the average absolute",
        "difference between predicted and observed values.",
        "Lower MAE indicates better forecasting accuracy.",
        "MAE is expressed in the same units as the target variable."
      ),
      
      p(
        strong("Important: "),
        "The two horizons may use different numbers of",
        "evaluation points. Therefore, their MAE values",
        "are not necessarily directly comparable."
      )
    )
  })
  
  
  # Display accuracy table
  output$cv_results <- renderTable({
    
    req(cv_result())
    
    results <- cv_result()$results
    
    data.frame(
      Horizon = results$horizon,
      MAE = ifelse(
        is.na(results$MAE),
        "Not available",
        sprintf("%.2f", results$MAE)
      ),
      `Eligible CV origins` = results$eligible_origins,
      `Valid comparisons` = results$valid_comparisons,
      check.names = FALSE
    )
    
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  
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
    
    if (is.null(result)) {
      return(
        p("Select a model and click 'Generate forecast'.")
      )
    }
    
    # Short explanation of the selected model
    model_description <- switch(
      input$model,
      
      "Naive" = paste0(
        "The Naive model uses the last available value of ",
        input$target,
        " as the prediction for all future observations."
      ),
      
      "ARIMA" = paste0(
        "ARIMA uses historical values of ", input$target,
        ", including patterns in its past changes and ",
        "forecast errors, to predict future values. ",
        "It does not use external predictors."
      ),
      
      "ARIMAX" = paste0(
        "ARIMAX uses historical values of ", input$target,
        " together with the selected external predictors ",
        "to predict future values."
      )
    )
    
    tagList(
      
      p(
        strong("Forecast generated successfully. "),
        paste(
          nrow(result),
          "future observations predicted."
        )
      ),
      
      p(
        strong("How this model works: "),
        model_description
      ),
      
      if (input$model %in% c("ARIMA", "ARIMAX")) {
        p(
          paste(
            "Model fitted using",
            nrow(uploaded_data()),
            "observations. Caution: Forecasts based on",
            "limited historical data may be unreliable."
          )
        )
      },
      
      if (input$model == "ARIMAX") {
        p(
          strong("Note about ARIMAX forecasts: "),
          paste0(
            "To predict future values of ", input$target,
            ", ARIMAX also needs future values of the selected ",
            "predictors. Because these values are not yet known, ",
            "this app assumes that each predictor will remain at ",
            "its historical mean (its average value across the ",
            "available observations) throughout the forecast period. ",
            "Importantly, the 95% prediction intervals assume these ",
            "future predictor values are fixed at their historical means. ",
            "They do not account for uncertainty about the actual ",
            "future values of the predictors."
          )
        )
      }
    )
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