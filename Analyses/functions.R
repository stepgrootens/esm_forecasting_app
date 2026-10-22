# functions.R
# Reusable functions for the Hoebeke (2022) experience-sampling analysis.
# Designed to be sourced by both main_analysis.R and a Shiny app.

# Required packages:
# install.packages(c("openesm", "dplyr", "tidyr", "purrr", "ggplot2",
#                    "tibble", "lme4", "lmerTest", "broom.mixed",
#                    "performance"))

load_hoebeke_data <- function(dataset_id = "0013_hoebeke") {
  if (!requireNamespace("openesm", quietly = TRUE)) {
    stop("Package 'openesm' is required. Install it with install.packages('openesm').")
  }

  dataset <- openesm::get_dataset(dataset_id)
  if (is.null(dataset$data)) {
    stop("The dataset object does not contain a $data element.")
  }

  data <- as.data.frame(dataset$data)
  if (!"id" %in% names(data)) {
    stop("The dataset has no 'id' column. Check the dataset's variable names.")
  }
  data$id <- factor(data$id)

  list(
    data = data,
    metadata = list(
      dataset_id = dataset$dataset_id,
      dataset_version = dataset$dataset_version,
      metadata_version = dataset$metadata_version
    )
  )
}

check_variables <- function(data,
  expected_variables = c("perseverance", "negativity", "self_criticism",
                         "brooding", "replaying", "att_control", "day", "beep")) {
  stopifnot(is.data.frame(data))
  tibble::tibble(
    variable = expected_variables,
    available = expected_variables %in% names(data)
  )
}

get_rating_variables <- function(data,
  candidates = c("perseverance", "negativity", "self_criticism",
                "brooding", "replaying", "att_control")) {
  available <- intersect(candidates, names(data))
  available[vapply(data[available], is.numeric, logical(1))]
}

get_missingness <- function(data) {
  tibble::tibble(
    variable = names(data),
    n_missing = vapply(data, function(x) sum(is.na(x)), numeric(1)),
    percent_missing = vapply(data, function(x) mean(is.na(x)) * 100, numeric(1))
  ) |>
    dplyr::arrange(dplyr::desc(percent_missing))
}

get_participant_counts <- function(data) {
  if (!"id" %in% names(data)) stop("Required column 'id' is missing.")
  dplyr::count(data, id, name = "n_observations") |>
    dplyr::arrange(n_observations)
}

get_range_checks <- function(data, variables = get_rating_variables(data)) {
  purrr::map_dfr(variables, function(v) {
    x <- data[[v]]
    tibble::tibble(
      variable = v,
      minimum = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
      maximum = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE),
      n_unique = dplyr::n_distinct(x, na.rm = TRUE),
      n_below_0 = sum(x < 0, na.rm = TRUE),
      n_above_100 = sum(x > 100, na.rm = TRUE)
    )
  })
}

get_descriptive_stats <- function(data, variables = get_rating_variables(data)) {
  purrr::map_dfr(variables, function(v) {
    x <- data[[v]]
    tibble::tibble(
      variable = v,
      n = sum(!is.na(x)),
      mean = if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE),
      sd = if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE),
      median = if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE),
      min = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
      max = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    )
  })
}

get_participant_means <- function(data, variables = get_rating_variables(data)) {
  data |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(variables),
                    ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

get_within_between_variation <- function(data, variables = get_rating_variables(data)) {
  purrr::map_dfr(variables, function(v) {
    temp <- data |>
      dplyr::select(id, dplyr::all_of(v)) |>
      dplyr::filter(!is.na(.data[[v]]))
    person_means <- temp |>
      dplyr::group_by(id) |>
      dplyr::summarise(person_mean = mean(.data[[v]]), .groups = "drop")
    tibble::tibble(
      variable = v,
      within_person_sd = stats::sd(
        temp[[v]] - person_means$person_mean[match(temp$id, person_means$id)],
        na.rm = TRUE
      ),
      between_person_sd = stats::sd(person_means$person_mean, na.rm = TRUE)
    )
  })
}

prepare_observation_order <- function(data) {
  if ("counter" %in% names(data) && is.numeric(data$counter)) {
    data <- dplyr::arrange(data, id, counter)
  } else if (all(c("day", "beep") %in% names(data))) {
    data <- dplyr::arrange(data, id, day, beep)
  }
  data |>
    dplyr::group_by(id) |>
    dplyr::mutate(observation_order = dplyr::row_number()) |>
    dplyr::ungroup()
}

plot_missingness <- function(data) {
  missingness <- get_missingness(data)
  ggplot2::ggplot(missingness,
    ggplot2::aes(x = reorder(variable, percent_missing), y = percent_missing)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Missing data by variable", x = NULL,
                  y = "Missing observations (%)") +
    ggplot2::theme_minimal()
}

plot_distribution <- function(data, variable, bins = 25) {
  if (!variable %in% names(data)) stop("Unknown variable: ", variable)
  if (!is.numeric(data[[variable]])) stop("Variable must be numeric: ", variable)
  ggplot2::ggplot(data, ggplot2::aes(x = .data[[variable]])) +
    ggplot2::geom_histogram(bins = bins, na.rm = TRUE) +
    ggplot2::labs(title = paste("Distribution of", variable),
                  x = variable, y = "Frequency") +
    ggplot2::theme_minimal()
}

plot_trajectory <- function(data, variable) {
  if (!variable %in% names(data)) stop("Unknown variable: ", variable)
  if (!is.numeric(data[[variable]])) stop("Variable must be numeric: ", variable)
  plot_data <- prepare_observation_order(data)
  ggplot2::ggplot(plot_data, ggplot2::aes(
    x = observation_order, y = .data[[variable]], group = id
  )) +
    ggplot2::geom_line(alpha = 0.35, na.rm = TRUE) +
    ggplot2::geom_point(alpha = 0.35, na.rm = TRUE) +
    ggplot2::labs(title = paste("Individual trajectories:", variable),
                  x = "Observation order", y = variable) +
    ggplot2::theme_minimal()
}

get_correlations <- function(data,
  variables = intersect(c("perseverance", "negativity", "self_criticism",
                         "brooding", "replaying", "att_control"),
                       get_rating_variables(data))) {
  if (length(variables) < 2) {
    return(list(overall = NULL, between_person = NULL,
                message = "At least two numeric rating variables are needed."))
  }
  participant_means <- get_participant_means(data, variables)
  list(
    overall = stats::cor(data[, variables, drop = FALSE],
                         use = "pairwise.complete.obs", method = "pearson"),
    between_person = stats::cor(participant_means[, variables, drop = FALSE],
                                use = "pairwise.complete.obs", method = "pearson")
  )
}

fit_multilevel_model <- function(data,
  outcome = "negativity",
  predictors = c("perseverance", "self_criticism", "brooding", "replaying"),
  include_day = TRUE,
  random_slope = FALSE) {
  if (!requireNamespace("lme4", quietly = TRUE) ||
      !requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Install 'lme4' and 'lmerTest' to fit multilevel models.")
  }
  if (!"id" %in% names(data)) stop("Required column 'id' is missing.")
  numeric_vars <- get_rating_variables(data)
  predictors <- intersect(predictors, numeric_vars)
  if (!outcome %in% numeric_vars) stop("Numeric outcome '", outcome, "' is unavailable.")
  if (!length(predictors)) stop("No numeric rumination predictors are available.")

  model_data <- data |>
    dplyr::select(id, dplyr::any_of(c("day", "counter", "beep")),
                  dplyr::all_of(c(outcome, predictors))) |>
    dplyr::filter(!is.na(.data[[outcome]]), !is.na(id))

  for (v in predictors) {
    mean_name <- paste0(v, "_person_mean")
    within_name <- paste0(v, "_within")
    model_data <- model_data |>
      dplyr::group_by(id) |>
      dplyr::mutate(
        !!mean_name := if (all(is.na(.data[[v]]))) NA_real_ else mean(.data[[v]], na.rm = TRUE),
        !!within_name := .data[[v]] - .data[[mean_name]]
      ) |>
      dplyr::ungroup()
    model_data[[mean_name]][!is.finite(model_data[[mean_name]])] <- NA_real_
  }

  if (include_day && "day" %in% names(model_data)) {
    day_numeric <- suppressWarnings(as.numeric(as.character(model_data$day)))
    if (sum(!is.na(day_numeric)) > 0) model_data$day_num <- day_numeric
  }

  within_terms <- paste0(predictors, "_within")
  between_terms <- paste0(predictors, "_person_mean")
  fixed_terms <- c(within_terms, between_terms)
  if ("day_num" %in% names(model_data)) fixed_terms <- c(fixed_terms, "day_num")

  needed <- c(outcome, fixed_terms, "id")
  model_data_complete <- model_data |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(needed), ~ !is.na(.x)))

  if (nrow(model_data_complete) == 0 ||
      dplyr::n_distinct(model_data_complete$id) < 2) {
    stop("Insufficient complete observations or participants for the multilevel model.")
  }

  fixed_formula <- paste(fixed_terms, collapse = " + ")
  formula_text <- paste0(outcome, " ~ ", fixed_formula, " + (1 | id)")
  model <- lmerTest::lmer(stats::as.formula(formula_text),
                          data = model_data_complete, REML = TRUE)

  fixed_effects <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
  result <- list(
    model = model,
    summary = summary(model),
    fixed_effects = fixed_effects,
    model_data = model_data_complete,
    formula = stats::as.formula(formula_text),
    icc = if (requireNamespace("performance", quietly = TRUE))
      performance::icc(model) else NULL,
    diagnostics = NULL,
    random_slope_model = NULL
  )

  if (requireNamespace("performance", quietly = TRUE)) {
    result$diagnostics <- tryCatch(performance::check_model(model),
                                   error = function(e) NULL)
  }

  if (random_slope && length(within_terms) > 0) {
    slope_formula <- stats::as.formula(paste0(
      outcome, " ~ ", fixed_formula, " + (1 + ", within_terms[1], " | id)"
    ))
    result$random_slope_model <- tryCatch(
      lmerTest::lmer(slope_formula, data = model_data_complete, REML = FALSE),
      error = function(e) NULL
    )
  }
  result
}

fit_lagged_model <- function(data, predictor = "perseverance",
                             outcome = "negativity") {
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Install 'lmerTest' to fit the lagged model.")
  }
  required <- c("id", "counter", predictor, outcome)
  if (!all(required %in% names(data)) || !is.numeric(data$counter)) {
    stop("Lagged analysis requires id, numeric counter, and the selected variables.")
  }
  lag_data <- data |>
    dplyr::arrange(id, counter) |>
    dplyr::group_by(id) |>
    dplyr::mutate(
      outcome_next = dplyr::lead(.data[[outcome]]),
      predictor_previous = .data[[predictor]],
      outcome_previous = .data[[outcome]],
      next_counter = dplyr::lead(counter)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(outcome_next), !is.na(predictor_previous),
                  !is.na(outcome_previous), !is.na(next_counter),
                  next_counter > counter)
  if (nrow(lag_data) == 0 || dplyr::n_distinct(lag_data$id) < 2) {
    stop("Insufficient data for the lagged model.")
  }
  formula <- stats::as.formula(
    "outcome_next ~ predictor_previous + outcome_previous + (1 | id)"
  )
  # Use fixed column names in the model formula, then rename from user selections.
  model <- lmerTest::lmer(formula, data = lag_data, REML = TRUE)
  list(model = model,
       fixed_effects = broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE),
       data = lag_data,
       note = "This predicts the next observed assessment, not necessarily a fixed time lag.")
}

plot_coefficient_estimates <- function(fixed_effects) {
  if (is.null(fixed_effects) || !nrow(fixed_effects)) {
    stop("No fixed-effect estimates were supplied.")
  }
  fixed_effects |>
    dplyr::filter(term != "(Intercept)") |>
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = reorder(term, estimate))) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = conf.low, xmax = conf.high),
                           width = 0.2, orientation = "y") +
    ggplot2::labs(title = "Associations with negativity",
                  subtitle = "Multilevel regression estimates and 95% CIs",
                  x = "Estimated regression coefficient", y = "Predictor") +
    ggplot2::theme_minimal()
}
