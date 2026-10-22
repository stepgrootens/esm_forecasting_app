# main_analysis.R
# Batch runner for the Hoebeke (2022) analysis.
# Shiny can source functions.R and call these same functions reactively.

required_packages <- c("dplyr", "purrr", "tibble", "ggplot2",
                       "lme4", "lmerTest", "broom.mixed")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install required packages first: ",
       paste(missing_packages, collapse = ", "))
}

source("functions.R")

run_main_analysis <- function(output_dir = "hoebeke_results",
                              dataset_id = "0013_hoebeke",
                              run_lagged = TRUE) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  loaded <- load_hoebeke_data(dataset_id)
  data_raw <- loaded$data
  data <- prepare_observation_order(data_raw)
  rating_variables <- get_rating_variables(data)

  # Save dataset metadata and a raw-data copy for reproducibility.
  write.csv(data_raw, file.path(output_dir, "raw_data.csv"), row.names = FALSE)
  writeLines(capture.output(str(loaded$metadata)),
             file.path(output_dir, "dataset_metadata.txt"))
  writeLines(names(data_raw), file.path(output_dir, "variable_names.txt"))

  # Data checks and descriptive results.
  variable_check <- check_variables(data)
  missingness <- get_missingness(data)
  participant_counts <- get_participant_counts(data)
  range_checks <- get_range_checks(data, rating_variables)
  descriptives <- get_descriptive_stats(data, rating_variables)
  participant_means <- get_participant_means(data, rating_variables)
  variation <- get_within_between_variation(data, rating_variables)

  write.csv(variable_check, file.path(output_dir, "variable_availability.csv"), row.names = FALSE)
  write.csv(missingness, file.path(output_dir, "missingness.csv"), row.names = FALSE)
  write.csv(participant_counts, file.path(output_dir, "observations_per_participant.csv"), row.names = FALSE)
  write.csv(range_checks, file.path(output_dir, "rating_range_checks.csv"), row.names = FALSE)
  write.csv(descriptives, file.path(output_dir, "descriptive_statistics.csv"), row.names = FALSE)
  write.csv(participant_means, file.path(output_dir, "participant_means.csv"), row.names = FALSE)
  write.csv(variation, file.path(output_dir, "within_between_variation.csv"), row.names = FALSE)
  write.csv(data.frame(duplicated_row = which(duplicated(data_raw))),
            file.path(output_dir, "duplicate_row_indices.csv"), row.names = FALSE)

  ggsave(file.path(output_dir, "missingness.png"),
         plot_missingness(data), width = 8, height = 6)

  for (v in rating_variables) {
    ggsave(file.path(output_dir, paste0("histogram_", v, ".png")),
           plot_distribution(data, v), width = 7, height = 5)
  }
  for (v in intersect(c("negativity", "perseverance"), rating_variables)) {
    ggsave(file.path(output_dir, paste0("trajectories_", v, ".png")),
           plot_trajectory(data, v), width = 9, height = 6)
  }

  correlations <- get_correlations(data, rating_variables)
  if (!is.null(correlations$overall)) {
    write.csv(correlations$overall,
              file.path(output_dir, "correlation_matrix.csv"))
    write.csv(correlations$between_person,
              file.path(output_dir, "between_person_correlations.csv"))
  }

  model_result <- NULL
  if ("negativity" %in% rating_variables &&
      length(intersect(c("perseverance", "self_criticism", "brooding", "replaying"),
                       rating_variables)) > 0) {
    model_result <- fit_multilevel_model(data)
    write.csv(model_result$fixed_effects,
              file.path(output_dir, "multilevel_fixed_effects.csv"), row.names = FALSE)
    capture.output(model_result$summary,
                   file = file.path(output_dir, "multilevel_model_summary.txt"))
    saveRDS(model_result$model, file.path(output_dir, "multilevel_model.rds"))
    if (!is.null(model_result$icc)) {
      capture.output(model_result$icc, file = file.path(output_dir, "icc_results.txt"))
    }
    if (!is.null(model_result$diagnostics)) {
      ggsave(file.path(output_dir, "model_diagnostics.png"),
             plot = model_result$diagnostics, width = 10, height = 8)
    }
    ggsave(file.path(output_dir, "coefficient_plot.png"),
           plot_coefficient_estimates(model_result$fixed_effects), width = 9, height = 6)

    residual_data <- data.frame(
      fitted = fitted(model_result$model),
      residual = resid(model_result$model)
    )
    residual_plot <- ggplot2::ggplot(residual_data,
      ggplot2::aes(x = fitted, y = residual)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::labs(title = "Residuals versus fitted values",
                    x = "Fitted values", y = "Conditional residuals") +
      ggplot2::theme_minimal()
    ggsave(file.path(output_dir, "residuals_vs_fitted.png"),
           residual_plot, width = 7, height = 5)

    qq <- stats::qqnorm(resid(model_result$model), plot.it = FALSE)
    qq_data <- data.frame(sample_quantile = qq$x,
                          theoretical_quantile = qq$y)
    qq_plot <- ggplot2::ggplot(qq_data,
      ggplot2::aes(x = theoretical_quantile, y = sample_quantile)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      ggplot2::labs(title = "Residual Q-Q plot",
                    x = "Theoretical quantiles", y = "Sample quantiles") +
      ggplot2::theme_minimal()
    ggsave(file.path(output_dir, "residual_qq.png"), qq_plot, width = 7, height = 5)

    if (!is.null(model_result$random_slope_model)) {
      saveRDS(model_result$random_slope_model,
              file.path(output_dir, "random_slope_model.rds"))
      capture.output(summary(model_result$random_slope_model),
                     file = file.path(output_dir, "random_slope_sensitivity.txt"))
    }
  }

  lag_result <- NULL
  if (run_lagged && all(c("counter", "perseverance", "negativity") %in% names(data)) &&
      is.numeric(data$counter)) {
    lag_result <- tryCatch(
      fit_lagged_model(data),
      error = function(e) {
        message("Lagged model skipped: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(lag_result)) {
      write.csv(lag_result$fixed_effects,
                file.path(output_dir, "lagged_model_effects.csv"), row.names = FALSE)
      saveRDS(lag_result$model, file.path(output_dir, "lagged_model.rds"))
      writeLines(lag_result$note, file.path(output_dir, "lagged_model_note.txt"))
    }
  }

  capture.output(sessionInfo(), file = file.path(output_dir, "session_info.txt"))

  invisible(list(
    data = data,
    metadata = loaded$metadata,
    variable_check = variable_check,
    missingness = missingness,
    descriptives = descriptives,
    correlations = correlations,
    multilevel = model_result,
    lagged = lag_result,
    output_dir = output_dir
  ))
}

# Run this file directly for the batch analysis.
# When sourcing it from a Shiny app, set run_batch <- FALSE first or
# comment out the call below, then call run_main_analysis() as needed.
if (identical(Sys.getenv("RUN_HOEBEKE_ANALYSIS"), "true")) {
  results <- run_main_analysis()
}
