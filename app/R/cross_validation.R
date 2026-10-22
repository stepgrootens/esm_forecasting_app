
# ============================================================
# Expanding-Window Cross-Validation
# ============================================================
# Evaluates 1-day and 7-day forecast accuracy for one
# participant using Naive, ARIMA, or ARIMAX.
#
# - Training window expands by one observation per iteration.
# - Models are fitted once per CV origin.
# - Only the exact 1-day and 7-day forecasts are evaluated.
# - Original (non-imputed) target values are used for MAE.
# - ARIMAX future predictors use training-window means.
# - No prediction intervals are calculated.
# ============================================================


cross_validate_esm <- function(data,
                               original_target,
                               target,
                               predictors = NULL,
                               model,
                               beeps_per_day,
                               initial_training_days) {
  
  # 1. Validate inputs ---------------------------------------
  
  if (!model %in% c("Naive", "ARIMA", "ARIMAX")) {
    stop("Model must be 'Naive', 'ARIMA', or 'ARIMAX'.")
  }
  
  if (!target %in% names(data)) {
    stop("Target variable not found in the prepared data.")
  }
  
  if (length(original_target) != nrow(data)) {
    stop("Original target and prepared data must have equal lengths.")
  }
  
  valid_positive_integer <- function(x) {
    is.numeric(x) &&
      length(x) == 1 &&
      !is.na(x) &&
      is.finite(x) &&
      x >= 1 &&
      x == floor(x)
  }
  
  if (!valid_positive_integer(beeps_per_day)) {
    stop("Observations per day must be a positive whole number.")
  }
  
  if (!valid_positive_integer(initial_training_days)) {
    stop("Initial training days must be a positive whole number.")
  }
  
  y <- data[[target]]
  
  if (!is.numeric(y) || anyNA(y) || any(!is.finite(y))) {
    stop("Prepared target must contain only finite numeric values.")
  }
  
  if (!is.numeric(original_target)) {
    stop("Original target must be numeric.")
  }
  
  if (model == "ARIMAX") {
    
    if (length(predictors) == 0) {
      stop("ARIMAX requires at least one predictor.")
    }
    
    if (target %in% predictors) {
      stop("Target cannot also be an ARIMAX predictor.")
    }
    
    if (!all(predictors %in% names(data))) {
      stop("One or more ARIMAX predictors were not found.")
    }
    
    if (!all(vapply(data[predictors], is.numeric, logical(1)))) {
      stop("ARIMAX predictors must be numeric.")
    }
    
    X <- as.matrix(data[predictors])
    
    if (anyNA(X) || any(!is.finite(X))) {
      stop("Prepared ARIMAX predictors must contain finite values.")
    }
    
  }
  
  # 2. Define CV origins and horizons ------------------------
  
  n <- length(y)
  
  initial_n <- as.integer(
    beeps_per_day * initial_training_days
  )
  
  if (initial_n < 2) {
    stop("Initial training window must contain at least two observations.")
  }
  
  if (initial_n >= n) {
    stop(
      "Initial training window must be shorter than the dataset."
    )
  }
  
  horizons <- c(
    "1 day" = as.integer(beeps_per_day),
    "7 days" = as.integer(7 * beeps_per_day)
  )
  
  # At least one future observation must exist.
  max_origin <- n - min(horizons)
  
  # Each origin represents the final training observation.
  origins <- seq.int(initial_n, max_origin)
  
  # 3. Set up error storage ----------------------------------
  
  errors <- list(
    "1 day" = numeric(0),
    "7 days" = numeric(0)
  )
  
  eligible_origins <- c(
    "1 day" = max(0L, n - initial_n - horizons["1 day"] + 1L),
    "7 days" = max(0L, n - initial_n - horizons["7 days"] + 1L)
  )
  
  fit_failures <- 0L
  
  # 4. Expanding-window CV loop ------------------------------
  
  for (origin in origins) {
    
    train_y <- y[seq_len(origin)]
    
    # Forecast only as far as the data allow.
    available_horizons <- horizons[
      origin + horizons <= n
    ]
    
    if (length(available_horizons) == 0) {
      next
    }
    
    h_max <- max(available_horizons)
    
    predictions <- tryCatch({
      
      if (model == "Naive") {
        
        fitted_forecast <- forecast::naive(
          train_y,
          h = h_max
        )
        
        as.numeric(fitted_forecast$mean)
        
      } else if (model == "ARIMA") {
        
        fit <- forecast::auto.arima(train_y)
        
        fitted_forecast <- forecast::forecast(
          fit,
          h = h_max
        )
        
        as.numeric(fitted_forecast$mean)
        
      } else {
        
        train_X <- X[seq_len(origin), , drop = FALSE]
        
        # Means calculated using training data only.
        predictor_means <- colMeans(train_X)
        
        future_X <- matrix(
          rep(predictor_means, times = h_max),
          nrow = h_max,
          byrow = TRUE
        )
        
        colnames(future_X) <- colnames(train_X)
        
        fit <- forecast::auto.arima(
          train_y,
          xreg = train_X
        )
        
        fitted_forecast <- forecast::forecast(
          fit,
          xreg = future_X,
          h = h_max
        )
        
        as.numeric(fitted_forecast$mean)
        
      }
      
    }, error = function(e) {
      NULL
    })
    
    if (is.null(predictions)) {
      fit_failures <- fit_failures + 1L
      next
    }
    
    # 5. Evaluate exact forecast horizons --------------------
    
    for (horizon_name in names(available_horizons)) {
      
      h <- unname(available_horizons[horizon_name])
      
      actual <- original_target[origin + h]
      predicted <- predictions[h]
      
      # Do not evaluate against LOCF-imputed target values.
      if (is.na(actual)) {
        next
      }
      
      if (!is.finite(predicted)) {
        next
      }
      
      errors[[horizon_name]] <- c(
        errors[[horizon_name]],
        abs(actual - predicted)
      )
    }
  }
  
  # 6. Summarize results -------------------------------------
  
  results <- data.frame(
    horizon = names(horizons),
    horizon_beeps = as.integer(horizons),
    eligible_origins = as.integer(eligible_origins),
    valid_comparisons = vapply(
      errors,
      length,
      integer(1)
    ),
    MAE = vapply(
      errors,
      function(x) {
        if (length(x) == 0) {
          return(NA_real_)
        }
        mean(x)
      },
      numeric(1)
    ),
    row.names = NULL
  )
  
  # 7. Return results and diagnostics ------------------------
  
  list(
    results = results,
    fit_failures = fit_failures,
    total_model_fits = length(origins)
  )
}
