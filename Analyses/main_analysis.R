############################################################
# HOEBEKE (2022) EXPERIENCE-SAMPLING DATA: FULL ANALYSIS
# Dataset: 0013_hoebeke
# Main question:
# Are rumination-related thoughts associated with negativity?
############################################################

# ==========================================================
# 0. SETUP
# ==========================================================

# Install packages once if necessary:
# install.packages(c(
#   "openesm", "tidyverse", "lme4", "lmerTest",
#   "broom.mixed", "performance", "psych", "see"
# ))

library(openesm)
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(performance)
library(psych)

set.seed(123)

# Create an output folder
output_dir <- "hoebeke_results"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Save warnings and outputs in the R console as usual.
# Do not suppress warnings automatically: they can be informative.


# ==========================================================
# 1. LOAD THE DATA
# ==========================================================

dataset <- openesm::get_dataset("0013_hoebeke")

# openESM returns a dataset object; its data are in $data.
data_raw <- as.data.frame(dataset$data)

# Record dataset metadata when available
print(dataset$dataset_id)
print(dataset$dataset_version)
print(dataset$metadata_version)

# Save a copy of the raw data
write.csv(
  data_raw,
  file.path(output_dir, "raw_data.csv"),
  row.names = FALSE
)

# Basic inspection
cat("\nNumber of rows:", nrow(data_raw), "\n")
cat("Number of columns:", ncol(data_raw), "\n")

print(names(data_raw))
print(head(data_raw))
str(data_raw)
summary(data_raw)

writeLines(
  names(data_raw),
  file.path(output_dir, "variable_names.txt")
)


# ==========================================================
# 2. CHECK REQUIRED VARIABLES
# ==========================================================

# Participant ID is required for repeated-measures analyses.
if (!"id" %in% names(data_raw)) {
  stop("The dataset has no 'id' column. Check names(data_raw).")
}

# Convert participant IDs to factors.
data <- data_raw %>%
  mutate(id = factor(id))

# Check whether the expected variables are available.
expected_variables <- c(
  "perseverance",
  "negativity",
  "self_criticism",
  "brooding",
  "replaying",
  "att_control",
  "day",
  "beep"
)

variable_check <- tibble(
  variable = expected_variables,
  available = expected_variables %in% names(data)
)

print(variable_check)

write.csv(
  variable_check,
  file.path(output_dir, "variable_availability.csv"),
  row.names = FALSE
)


# ==========================================================
# 3. DATA QUALITY AND MISSINGNESS
# ==========================================================

# Check duplicated rows
duplicate_rows <- duplicated(data_raw)

cat("\nDuplicated rows:", sum(duplicate_rows), "\n")

# Do not remove duplicates automatically:
# inspect them first to determine whether they are genuine duplicates.

# Number of observations per participant
participant_counts <- data %>%
  count(id, name = "n_observations") %>%
  arrange(n_observations)

print(participant_counts)

write.csv(
  participant_counts,
  file.path(output_dir, "observations_per_participant.csv"),
  row.names = FALSE
)

# Missing values per variable
missingness <- tibble(
  variable = names(data),
  n_missing = vapply(data, function(x) sum(is.na(x)), numeric(1)),
  percent_missing = vapply(
    data,
    function(x) mean(is.na(x)) * 100,
    numeric(1)
  )
) %>%
  arrange(desc(percent_missing))

print(missingness)

write.csv(
  missingness,
  file.path(output_dir, "missingness.csv"),
  row.names = FALSE
)

# Plot missingness percentages
missing_plot <- ggplot(
  missingness,
  aes(x = reorder(variable, percent_missing),
      y = percent_missing)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Missing data by variable",
    x = NULL,
    y = "Missing observations (%)"
  ) +
  theme_minimal()

ggsave(
  file.path(output_dir, "missingness.png"),
  missing_plot,
  width = 8,
  height = 6
)


# ==========================================================
# 4. IDENTIFY MEASUREMENT VARIABLES
# ==========================================================

# These variables are documented as momentary ratings.
# Only use variables that actually exist and are numeric.
candidate_variables <- c(
  "perseverance",
  "negativity",
  "self_criticism",
  "brooding",
  "replaying",
  "att_control"
)

available_candidates <- intersect(
  candidate_variables,
  names(data)
)

rating_variables <- available_candidates[
  vapply(
    data[available_candidates],
    is.numeric,
    logical(1)
  )
]

cat("\nNumeric rating variables available:\n")
print(rating_variables)

# Inspect distributions and possible out-of-range values.
# The documented rating scales generally range from 0 to 100.
# Check before assuming this applies to every variable.

range_checks <- map_dfr(rating_variables, function(v) {
  x <- data[[v]]
  
  tibble(
    variable = v,
    minimum = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
    maximum = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE),
    n_unique = n_distinct(x, na.rm = TRUE),
    n_below_0 = sum(x < 0, na.rm = TRUE),
    n_above_100 = sum(x > 100, na.rm = TRUE)
  )
})

print(range_checks)

write.csv(
  range_checks,
  file.path(output_dir, "rating_range_checks.csv"),
  row.names = FALSE
)


# ==========================================================
# 5. DESCRIPTIVE STATISTICS
# ==========================================================

# Overall means, standard deviations, and ranges.
descriptive_stats <- map_dfr(rating_variables, function(v) {
  x <- data[[v]]
  
  tibble(
    variable = v,
    n = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
})

print(descriptive_stats)

write.csv(
  descriptive_stats,
  file.path(output_dir, "descriptive_statistics.csv"),
  row.names = FALSE
)

# Participant-level means
participant_means <- data %>%
  group_by(id) %>%
  summarise(
    across(
      all_of(rating_variables),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

write.csv(
  participant_means,
  file.path(output_dir, "participant_means.csv"),
  row.names = FALSE
)

# Within-person and between-person variation
variation_stats <- map_dfr(rating_variables, function(v) {
  temp <- data %>%
    select(id, all_of(v)) %>%
    filter(!is.na(.data[[v]]))
  
  person_means <- temp %>%
    group_by(id) %>%
    summarise(
      person_mean = mean(.data[[v]]),
      .groups = "drop"
    )
  
  tibble(
    variable = v,
    within_person_sd = sd(
      temp[[v]] -
        person_means$person_mean[
          match(temp$id, person_means$id)
        ],
      na.rm = TRUE
    ),
    between_person_sd = sd(
      person_means$person_mean,
      na.rm = TRUE
    )
  )
})

print(variation_stats)

write.csv(
  variation_stats,
  file.path(output_dir, "within_between_variation.csv"),
  row.names = FALSE
)


# ==========================================================
# 6. VISUALISE THE DATA
# ==========================================================

# 6a. Overall distributions
for (v in rating_variables) {
  
  p <- ggplot(data, aes(x = .data[[v]])) +
    geom_histogram(bins = 25, na.rm = TRUE) +
    labs(
      title = paste("Distribution of", v),
      x = v,
      y = "Frequency"
    ) +
    theme_minimal()
  
  ggsave(
    file.path(output_dir, paste0("histogram_", v, ".png")),
    p,
    width = 7,
    height = 5
  )
}

# 6b. Individual participant trajectories
# This plot uses the observation order within each participant.
# It is not necessarily chronological unless the data are sorted.

if ("counter" %in% names(data) &&
    is.numeric(data$counter)) {
  
  data <- data %>%
    arrange(id, counter)
  
} else if ("day" %in% names(data) &&
           "beep" %in% names(data)) {
  
  data <- data %>%
    arrange(id, day, beep)
}

data <- data %>%
  group_by(id) %>%
  mutate(observation_order = row_number()) %>%
  ungroup()

for (v in intersect(
  c("negativity", "perseverance"),
  rating_variables
)) {
  
  p <- ggplot(
    data,
    aes(
      x = observation_order,
      y = .data[[v]],
      group = id
    )
  ) +
    geom_line(alpha = 0.35, na.rm = TRUE) +
    geom_point(alpha = 0.35, na.rm = TRUE) +
    labs(
      title = paste("Individual trajectories:", v),
      x = "Observation order",
      y = v
    ) +
    theme_minimal()
  
  ggsave(
    file.path(output_dir, paste0("trajectories_", v, ".png")),
    p,
    width = 9,
    height = 6
  )
}


# ==========================================================
# 7. CORRELATIONS
# ==========================================================

# Overall correlations are descriptive and mix
# within-person and between-person associations.

correlation_variables <- intersect(
  c(
    "perseverance",
    "negativity",
    "self_criticism",
    "brooding",
    "replaying",
    "att_control"
  ),
  rating_variables
)

if (length(correlation_variables) >= 2) {
  
  cor_data <- data %>%
    select(all_of(correlation_variables))
  
  correlation_matrix <- cor(
    cor_data,
    use = "pairwise.complete.obs",
    method = "pearson"
  )
  
  print(correlation_matrix)
  
  write.csv(
    correlation_matrix,
    file.path(output_dir, "correlation_matrix.csv")
  )
  
  # Correlations of participant-level averages
  person_level_correlations <- cor(
    participant_means %>%
      select(all_of(correlation_variables)),
    use = "pairwise.complete.obs"
  )
  
  write.csv(
    person_level_correlations,
    file.path(output_dir, "between_person_correlations.csv")
  )
}


# ==========================================================
# 8. MULTILEVEL MODEL: NEGATIVITY
# ==========================================================

# Main illustrative question:
# Are rumination-related thoughts associated with negativity?

# We distinguish:
# Within-person association:
# Is negativity higher than usual when a participant reports
# higher-than-usual rumination-related thoughts?
#
# Between-person association:
# Do participants with higher average rumination-related scores
# also report higher average negativity?

# Check the outcome and predictor availability.
predictors <- intersect(
  c("perseverance", "self_criticism", "brooding", "replaying"),
  rating_variables
)

if (!"negativity" %in% rating_variables) {
  
  message(
    "No numeric negativity variable is available. ",
    "Skipping the primary multilevel model."
  )
  
} else if (length(predictors) == 0) {
  
  message(
    "No numeric rumination predictors are available. ",
    "Skipping the primary multilevel model."
  )
  
} else {
  
  # Keep observations with an available outcome.
  # Predictor-specific missingness will be handled below.
  model_data <- data %>%
    select(id, any_of(c("day", "beep", "counter")),
           negativity, all_of(predictors)) %>%
    filter(!is.na(negativity), !is.na(id))
  
  # Person-mean centering:
  # Each predictor gets a within-person deviation and a
  # between-person average.
  for (v in predictors) {
    
    mean_name <- paste0(v, "_person_mean")
    within_name <- paste0(v, "_within")
    
    model_data <- model_data %>%
      group_by(id) %>%
      mutate(
        !!mean_name := mean(.data[[v]], na.rm = TRUE),
        !!within_name := .data[[v]] - .data[[mean_name]]
      ) %>%
      ungroup()
    
    # A group with no valid values has a NaN mean.
    model_data[[mean_name]][
      !is.finite(model_data[[mean_name]])
    ] <- NA_real_
  }
  
  # Include day as a covariate if it can be represented
  # meaningfully as a numeric study-day variable.
  if ("day" %in% names(model_data)) {
    
    day_numeric <- suppressWarnings(
      as.numeric(as.character(model_data$day))
    )
    
    if (sum(!is.na(day_numeric)) > 0) {
      model_data$day_num <- day_numeric
    }
  }
  
  # Construct a formula with all available predictors.
  within_terms <- paste0(predictors, "_within")
  between_terms <- paste0(predictors, "_person_mean")
  
  fixed_terms <- c(within_terms, between_terms)
  
  if ("day_num" %in% names(model_data)) {
    fixed_terms <- c(fixed_terms, "day_num")
  }
  
  model_formula <- as.formula(
    paste(
      "negativity ~",
      paste(fixed_terms, collapse = " + "),
      "+ (1 | id)"
    )
  )
  
  # Remove rows missing any variables used in the model.
  model_data_complete <- model_data %>%
    filter(
      if_all(
        all_of(c("negativity", fixed_terms, "id")),
        ~ !is.na(.x)
      )
    )
  
  cat("\nModel formula:\n")
  print(model_formula)
  
  cat("\nObservations used:", nrow(model_data_complete), "\n")
  cat(
    "Participants used:",
    n_distinct(model_data_complete$id),
    "\n"
  )
  
  if (
    nrow(model_data_complete) > 0 &&
    n_distinct(model_data_complete$id) >= 2
  ) {
    
    model_main <- lmerTest::lmer(
      model_formula,
      data = model_data_complete,
      REML = TRUE
    )
    
    print(summary(model_main))
    print(confint(model_main, method = "Wald"))
    
    # Export fixed effects
    fixed_effects <- broom.mixed::tidy(
      model_main,
      effects = "fixed",
      conf.int = TRUE
    )
    
    write.csv(
      fixed_effects,
      file.path(output_dir, "multilevel_fixed_effects.csv"),
      row.names = FALSE
    )
    
    # Export model summary
    capture.output(
      summary(model_main),
      file = file.path(output_dir, "multilevel_model_summary.txt")
    )
    
    # Save the model object for later use.
    saveRDS(
      model_main,
      file.path(output_dir, "multilevel_model.rds")
    )
    
    # Intraclass correlation coefficient (ICC)
    icc_result <- performance::icc(model_main)
    print(icc_result)
    
    capture.output(
      icc_result,
      file = file.path(output_dir, "icc_results.txt")
    )
    
    # Basic model diagnostics
    diagnostic_results <- performance::check_model(model_main)
    print(diagnostic_results)
    
    ggsave(
      file.path(output_dir, "model_diagnostics.png"),
      plot = diagnostic_results,
      width = 10,
      height = 8
    )
    
    # Residuals versus fitted values
    residual_data <- tibble(
      fitted = fitted(model_main),
      residual = resid(model_main)
    )
    
    p_residuals <- ggplot(
      residual_data,
      aes(x = fitted, y = residual)
    ) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        title = "Residuals versus fitted values",
        x = "Fitted values",
        y = "Conditional residuals"
      ) +
      theme_minimal()
    
    ggsave(
      file.path(output_dir, "residuals_vs_fitted.png"),
      p_residuals,
      width = 7,
      height = 5
    )
    
    # Normal Q-Q plot of conditional residuals
    qq_data <- tibble(
      sample_quantile = qqnorm(
        resid(model_main),
        plot.it = FALSE
      )$x,
      theoretical_quantile = qqnorm(
        resid(model_main),
        plot.it = FALSE
      )$y
    )
    
    p_qq <- ggplot(
      qq_data,
      aes(x = theoretical_quantile, y = sample_quantile)
    ) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(
        title = "Residual Q-Q plot",
        x = "Theoretical quantiles",
        y = "Sample quantiles"
      ) +
      theme_minimal()
    
    ggsave(
      file.path(output_dir, "residual_qq.png"),
      p_qq,
      width = 7,
      height = 5
    )
    
    # Sensitivity analysis:
    # Random intercept only is the primary model.
    # A random slope can be considered if there are enough
    # repeated observations and the model converges.
    if (length(within_terms) >= 1) {
      
      random_slope_formula <- as.formula(
        paste(
          "negativity ~",
          paste(fixed_terms, collapse = " + "),
          "+ (1 +",
          within_terms[1],
          "| id)"
        )
      )
      
      model_random_slope <- try(
        lmerTest::lmer(
          random_slope_formula,
          data = model_data_complete,
          REML = FALSE
        ),
        silent = TRUE
      )
      
      if (!inherits(model_random_slope, "try-error")) {
        
        capture.output(
          summary(model_random_slope),
          file = file.path(
            output_dir,
            "random_slope_sensitivity.txt"
          )
        )
        
        saveRDS(
          model_random_slope,
          file.path(
            output_dir,
            "random_slope_model.rds"
          )
        )
        
      } else {
        message(
          "Random-slope sensitivity model did not fit. ",
          "Check convergence and model complexity."
        )
      }
    }
    
  } else {
    
    message(
      "Insufficient complete observations or participants ",
      "for the primary multilevel model."
    )
  }
}


# ==========================================================
# 9. OPTIONAL: TEMPORAL ASSOCIATION
# ==========================================================

# This section examines whether rumination at one assessment
# predicts negativity at the next assessment.
#
# Important:
# Only run this when observations are correctly ordered and
# consecutive observations represent the intended time lag.
# Missing assessments may mean adjacent rows are not adjacent
# scheduled beeps.

if (
  all(c("negativity", "perseverance", "counter", "id") %in%
      names(data)) &&
  is.numeric(data$counter)
) {
  
  lag_data <- data %>%
    arrange(id, counter) %>%
    group_by(id) %>%
    mutate(
      negativity_next = lead(negativity),
      perseverance_previous = perseverance,
      next_counter = lead(counter)
    ) %>%
    ungroup() %>%
    filter(
      !is.na(negativity_next),
      !is.na(perseverance_previous),
      !is.na(next_counter),
      next_counter > counter
    )
  
  # This is a next-observed-assessment analysis, not necessarily
  # a fixed-time-lag analysis.
  if (nrow(lag_data) > 0 &&
      n_distinct(lag_data$id) >= 2) {
    
    lag_model <- lmerTest::lmer(
      negativity_next ~ perseverance_previous +
        negativity + (1 | id),
      data = lag_data,
      REML = TRUE
    )
    
    print(summary(lag_model))
    
    write.csv(
      broom.mixed::tidy(
        lag_model,
        effects = "fixed",
        conf.int = TRUE
      ),
      file.path(output_dir, "lagged_model_effects.csv"),
      row.names = FALSE
    )
    
    saveRDS(
      lag_model,
      file.path(output_dir, "lagged_model.rds")
    )
  }
}

# ------------------------------------------------------------------------------
#PLOT
#-------------------------------------------------------------------------------
# Plot the fixed-effect estimates and 95% confidence intervals
library(ggplot2)
library(dplyr)

coefficient_plot <- fixed_effects %>%
  filter(effect == "fixed", term != "(Intercept)") %>%
  ggplot(aes(
    x = estimate,
    y = reorder(term, estimate)
  )) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.2,
    orientation = "y"
  ) +
  labs(
    title = "Associations with negativity",
    subtitle = "Multilevel regression estimates and 95% CIs",
    x = "Estimated regression coefficient",
    y = "Predictor"
  ) +
  theme_minimal()

ggsave(
  "hoebeke_results/coefficient_plot.png",
  coefficient_plot,
  width = 9,
  height = 6
)

print(coefficient_plot)


# ==========================================================
# 10. SAVE REPRODUCIBILITY INFORMATION
# ==========================================================

capture.output(
  sessionInfo(),
  file = file.path(output_dir, "session_info.txt")
)

cat(
  "\nAnalysis script completed.\n",
  "Check the folder: ", output_dir, "\n",
  sep = ""
)
