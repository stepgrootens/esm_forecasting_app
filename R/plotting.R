
# ============================================================
# Forecast Visualization
# ============================================================
# Plots observed ESM data and seven days of forecasts.
#
# Observed data: solid line, no points
# Forecasts: dashed line, daily markers
# Uncertainty: shaded 95% prediction interval
# Time axis: days relative to the forecast boundary
# ============================================================

plot_esm_forecast <- function(original_target,
                              forecast_results,
                              target_name,
                              beeps_per_day,
                              scale_min = NULL,
                              scale_max = NULL) {
  
  n <- length(original_target)
  b <- as.integer(beeps_per_day)
  
  # 1. Historical observations -------------------------------
  
  observed_data <- data.frame(
    time = (seq_len(n) - n) / b,
    value = original_target
  )
  
  # 2. Forecasts ---------------------------------------------
  
  forecast_data <- data.frame(
    time = (seq_len(nrow(forecast_results))) / b,
    forecast = forecast_results$forecast,
    lower_95 = forecast_results$lower_95,
    upper_95 = forecast_results$upper_95
  )
  
  # Mark the final forecast of each day.
  daily_markers <- forecast_data[
    seq(b, nrow(forecast_data), by = b),
    ,
    drop = FALSE
  ]
  
  # 3. Connect history and forecast --------------------------
  
  # The forecasting model uses the final prepared target
  # value, which may be LOCF-imputed.
  #
  # Use the last actually observed value to connect the
  # visible lines, without drawing an imputed historical point.
  
  observed_indices <- which(!is.na(original_target))
  
  if (length(observed_indices) == 0) {
    stop("No observed target values are available to plot.")
  }
  
  last_observed_index <- tail(observed_indices, 1)
  
  connection_data <- data.frame(
    time = c(
      (last_observed_index - n) / b,
      forecast_data$time
    ),
    value = c(
      original_target[last_observed_index],
      forecast_data$forecast
    )
  )
  
  # 4. Build the plot ----------------------------------------
  
  plot <- ggplot2::ggplot() +
    
    # Historical scores
    ggplot2::geom_line(
      data = observed_data,
      ggplot2::aes(x = time, y = value),
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    
    # 95% prediction interval
    ggplot2::geom_ribbon(
      data = forecast_data,
      ggplot2::aes(
        x = time,
        ymin = lower_95,
        ymax = upper_95
      ),
      fill = "#4C78A8",
      alpha = 0.20
    ) +
    
    # Dashed forecast line
    ggplot2::geom_line(
      data = connection_data,
      ggplot2::aes(x = time, y = value),
      linetype = "dashed",
      colour = "#2166AC",
      linewidth = 0.9
    ) +
    
    # Seven daily forecast markers
    ggplot2::geom_point(
      data = daily_markers,
      ggplot2::aes(x = time, y = forecast),
      colour = "#2166AC",
      size = 2.5
    ) +
    
    # Forecast boundary
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dotted",
      colour = "grey40"
    ) +
    
    ggplot2::scale_x_continuous(
      breaks = seq(
        -ceiling(n / b),
        7,
        by = 1
      )
    ) +
    
    ggplot2::labs(
      title = paste("Forecast of", target_name),
      subtitle = "Historical observations and 7-day forecast",
      x = "Days relative to forecast start",
      y = target_name,
      caption = paste(
        "Shaded area: 95% model-based prediction interval.",
        "Circles mark the end of each forecast day."
      )
    ) +
    
    ggplot2::theme_minimal(base_size = 12) +
    
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  # 5. Optional zoom to the response scale --------------------
  
  if (!is.null(scale_min) && !is.null(scale_max)) {
    
    if (!is.finite(scale_min) ||
        !is.finite(scale_max) ||
        scale_min >= scale_max) {
      stop("Scale limits must be finite, with minimum below maximum.")
    }
    
    plot <- plot +
      ggplot2::coord_cartesian(
        ylim = c(scale_min, scale_max),
        expand = FALSE
      )
  }
  
  return(plot)
}
