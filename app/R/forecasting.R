
# ============================================================
# Forecasting Functions
# ============================================================
# Fits a Naive, ARIMA, or ARIMAX model to one individual's
# ESM time series.
#
# Returns point forecasts and 95% prediction intervals
# for every scheduled observation over the next seven days.
#
# For ARIMAX, future predictor values are assumed to equal
# their historical means.
# ============================================================


# 1. Generate forecasts --------------------------------------

generate_forecast <- function(data, target, predictors = NULL,
                              model, beeps_per_day) {
  
  # 1.1 Check the inputs -------------------------------------
  
  if (!model %in% c("Naive", "ARIMA", "ARIMAX")) {
    stop("Model must be 'Naive', 'ARIMA', or 'ARIMAX'.")
  }
  
  if (!is.numeric(beeps_per_day) ||
      length(beeps_per_day) != 1 ||
      is.na(beeps_per_day) ||
      !is.finite(beeps_per_day) ||
      beeps_per_day < 1 ||
      beeps_per_day != floor(beeps_per_day)) {
    stop("Observations per day must be a positive whole number.")
  }
  
  if (!target %in% names(data)) {
    stop("The selected target variable was not found.")
  }
  
  if (model == "ARIMAX" && length(predictors) == 0) {
    stop("ARIMAX requires at least one external predictor.")
  }
  
  if (model == "ARIMAX" &&
      !all(predictors %in% names(data))) {
    stop("One or more selected predictors were not found.")
  }
  
  # 1.2 Prepare the target and forecast horizon ---------------
  
  y <- data[[target]]
  
  if (!is.numeric(y) || anyNA(y) ||
      any(!is.finite(y))) {
    stop("The target must contain only finite numeric values.")
  }
  
  if (length(y) < 2) {
    stop("At least two observations are required.")
  }
  
  n_observations <- length(y)
  forecast_horizon <- 7L * as.integer(beeps_per_day)
  
  # 1.3 Fit the selected model and generate forecasts ---------
  
  # tryCatch converts fitting errors into a readable message.
  predictions <- tryCatch({
    
    if (model == "Naive") {
      
      forecast::naive(
        y,
        h = forecast_horizon,
        level = 95
      )
      
    } else if (model == "ARIMA") {
      
      fitted_model <- forecast::auto.arima(y)
      
      forecast::forecast(
        fitted_model,
        h = forecast_horizon,
        level = 95
      )
      
    } else {
      
      # ARIMAX uses the selected external predictors.
      X <- as.matrix(data[predictors])
      
      if (!all(vapply(data[predictors], is.numeric, logical(1))) ||
          anyNA(X) || any(!is.finite(X))) {
        stop("ARIMAX predictors must contain only finite numeric values.")
      }
      
      if (target %in% predictors) {
        stop("The target cannot also be an ARIMAX predictor.")
      }
      
      flat_predictors <- apply(X, 2, function(x) {
        length(unique(x)) < 2
      })
      
      if (any(flat_predictors)) {
        stop(
          paste(
            "These predictors have no variation:",
            paste(colnames(X)[flat_predictors], collapse = ", ")
          )
        )
      }
      
      fitted_model <- forecast::auto.arima(
        y,
        xreg = X
      )
      
      # Future predictor values are not observed.
      # Use each predictor's historical mean for all 7 days.
      predictor_means <- colMeans(X)
      
      future_X <- matrix(
        rep(predictor_means, times = forecast_horizon),
        nrow = forecast_horizon,
        byrow = TRUE
      )
      
      colnames(future_X) <- colnames(X)
      
      forecast::forecast(
        fitted_model,
        xreg = future_X,
        h = forecast_horizon,
        level = 95
      )
      
    }
    
  }, error = function(e) {
    
    stop(
      paste("Forecasting failed:", conditionMessage(e)),
      call. = FALSE
    )
    
  })
  
  # 1.4 Organize the results ---------------------------------
  
  forecast_results <- data.frame(
    time = n_observations + seq_len(forecast_horizon),
    forecast = as.numeric(predictions$mean),
    lower_95 = as.numeric(predictions$lower[, 1]),
    upper_95 = as.numeric(predictions$upper[, 1])
  )
  
  return(forecast_results)
}
