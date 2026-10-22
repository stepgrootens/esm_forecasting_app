
# ============================================================
# Data Preparation Functions
# ============================================================
# These functions validate selected ESM variables and handle
# missing observations before forecasting.
# ============================================================


# 1. Check selected variables --------------------------------

validate_esm_data <- function(data, target, predictors) {
  
  selected_variables <- unique(c(target, predictors))
  
  # All selected variables must exist and be numeric.
  if (!all(selected_variables %in% names(data))) {
    return("One or more selected variables were not found.")
  }
  
  if (!all(vapply(data[selected_variables], is.numeric, logical(1)))) {
    return("The selected variables must be numeric.")
  }
  
  # The target cannot also be an external predictor.
  if (target %in% predictors) {
    return("The target variable cannot also be an ARIMAX predictor.")
  }
  
  # A completely missing variable cannot be imputed with LOCF.
  all_missing <- vapply(
    data[selected_variables],
    function(x) all(is.na(x)),
    logical(1)
  )
  
  if (any(all_missing)) {
    return(paste(
      "These variables contain only missing values:",
      paste(names(all_missing)[all_missing], collapse = ", ")
    ))
  }
  
  # Flat predictors provide no useful variation for ARIMAX.
  if (length(predictors) > 0) {
    
    flat_predictors <- vapply(
      data[predictors],
      function(x) length(unique(x[!is.na(x)])) < 2,
      logical(1)
    )
    
    if (any(flat_predictors)) {
      return(paste(
        "These predictors have no observed variation:",
        paste(names(flat_predictors)[flat_predictors], collapse = ", "),
        "\nPlease select predictors that vary over time."
      ))
    }
  }
  
  # NULL means that all validation checks passed.
  return(NULL)
}


# 2. Prepare selected variables -------------------------------

prepare_esm_data <- function(data, target, predictors) {
  
  selected_variables <- unique(c(target, predictors))
  
  # Keep original values for evaluating forecast accuracy.
  original_target <- data[[target]]
  
  # Apply Last Observation Carried Forward to selected columns.
  prepared_data <- data[selected_variables]
  
  prepared_data[] <- lapply(
    prepared_data,
    function(x) zoo::na.locf(x, na.rm = FALSE)
  )
  
  # LOCF cannot replace missing values at the beginning.
  if (anyNA(prepared_data)) {
    stop(
      paste(
        "Some missing values remain after LOCF.",
        "Check for missing values at the beginning of the series."
      )
    )
  }
  
  return(list(
    data = prepared_data,
    original_target = original_target
  ))
}
