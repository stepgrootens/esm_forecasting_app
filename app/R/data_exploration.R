
# Data exploration functions
# Adapted from the group's statistical analysis script.

calculate_descriptive_stats <- function(data, variables) {
  
  if (!all(variables %in% names(data))) {
    stop("One or more selected variables were not found.")
  }
  
  if (!all(vapply(data[variables], is.numeric, logical(1)))) {
    stop("All selected variables must be numeric.")
  }
  
  results <- lapply(variables, function(variable) {
    
    x <- data[[variable]]
    observed <- x[!is.na(x)]
    
    data.frame(
      variable = variable,
      n = length(observed),
      mean = if (length(observed) > 0) mean(observed) else NA_real_,
      sd = if (length(observed) > 1) sd(observed) else NA_real_,
      median = if (length(observed) > 0) median(observed) else NA_real_,
      min = if (length(observed) > 0) min(observed) else NA_real_,
      max = if (length(observed) > 0) max(observed) else NA_real_
    )
  })
  
  do.call(rbind, results)
}


calculate_missingness <- function(data, variables) {
  
  if (!all(variables %in% names(data))) {
    stop("One or more selected variables were not found.")
  }
  
  results <- lapply(variables, function(variable) {
    
    x <- data[[variable]]
    
    data.frame(
      variable = variable,
      n_observations = length(x),
      n_missing = sum(is.na(x)),
      percent_missing = mean(is.na(x)) * 100
    )
  })
  
  do.call(rbind, results)
}


plot_target_distribution <- function(data, target) {
  
  if (!target %in% names(data)) {
    stop("The selected target variable was not found.")
  }
  
  if (!is.numeric(data[[target]])) {
    stop("The target variable must be numeric.")
  }
  
  observed <- data[[target]][!is.na(data[[target]])]
  
  if (length(observed) == 0) {
    stop("The selected target has no observed values.")
  }
  
  plot_data <- data.frame(value = observed)
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(
      bins = 25,
      fill = "steelblue",
      color = "white"
    ) +
    ggplot2::labs(
      title = paste("Distribution of", target),
      x = target,
      y = "Number of observations"
    ) +
    ggplot2::theme_minimal()
}


calculate_target_correlations <- function(data, target, predictors) {
  
  if (!target %in% names(data)) {
    stop("The selected target variable was not found.")
  }
  
  if (length(predictors) == 0) {
    return(data.frame())
  }
  
  if (!all(predictors %in% names(data))) {
    stop("One or more selected predictors were not found.")
  }
  
  if (target %in% predictors) {
    stop("The target cannot also be a predictor.")
  }
  
  variables <- c(target, predictors)
  
  if (!all(vapply(data[variables], is.numeric, logical(1)))) {
    stop("The target and predictors must be numeric.")
  }
  
  results <- lapply(predictors, function(predictor) {
    
    x <- data[[predictor]]
    y <- data[[target]]
    
    # Use only observations where both variables are available
    complete <- complete.cases(x, y)
    
    x_complete <- x[complete]
    y_complete <- y[complete]
    
    n_pairs <- length(x_complete)
    
    # Correlation requires at least two complete pairs
    # and variation in both variables.
    correlation <- NA_real_
    
    if (
      n_pairs >= 2 &&
      sd(x_complete) > 0 &&
      sd(y_complete) > 0
    ) {
      correlation <- cor(
        x_complete,
        y_complete,
        method = "pearson"
      )
    }
    
    data.frame(
      target = target,
      predictor = predictor,
      correlation = correlation,
      complete_pairs = n_pairs
    )
  })
  
  do.call(rbind, results)
}


