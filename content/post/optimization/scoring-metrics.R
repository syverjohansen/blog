# scoring-metrics.R
# Scoring metrics for parameter optimization
# Implements Brier score, log loss, and calibration error

suppressPackageStartupMessages({
  library(dplyr)
})

# =============================================================================
# BRIER SCORE
# =============================================================================

#' Calculate Brier Score for a single threshold
#'
#' @param predictions Data frame with athlete_id and prob_top_{threshold} columns
#' @param actuals Data frame with athlete_id and Place columns
#' @param threshold Position threshold (1, 3, 5, 10, or 30)
#' @return Brier score (lower is better, range 0-1)
calculate_brier_score <- function(predictions, actuals, threshold) {
  prob_col <- paste0("prob_top_", threshold)

  if (!prob_col %in% names(predictions)) {
    warning(paste("Column", prob_col, "not found in predictions"))
    return(NA)
  }

  merged <- predictions %>%
    inner_join(actuals, by = "athlete_id") %>%
    mutate(
      outcome = as.numeric(Place <= threshold),
      squared_error = (.data[[prob_col]] - outcome)^2
    )

  if (nrow(merged) == 0) {
    return(NA)
  }

  brier_score <- mean(merged$squared_error, na.rm = TRUE)
  return(brier_score)
}

#' Calculate Brier Score across multiple races
#'
#' @param backtest_results List of backtest results (each with predictions and actuals)
#' @param threshold Position threshold
#' @return Aggregate Brier score
calculate_brier_score_aggregate <- function(backtest_results, threshold) {
  all_squared_errors <- c()

  for (result in backtest_results) {
    prob_col <- paste0("prob_top_", threshold)

    if (!prob_col %in% names(result$predictions)) next

    merged <- result$predictions %>%
      inner_join(result$actuals, by = "athlete_id") %>%
      mutate(
        outcome = as.numeric(Place <= threshold),
        squared_error = (.data[[prob_col]] - outcome)^2
      )

    all_squared_errors <- c(all_squared_errors, merged$squared_error)
  }

  if (length(all_squared_errors) == 0) return(NA)

  return(mean(all_squared_errors, na.rm = TRUE))
}

# =============================================================================
# LOG LOSS (Cross-Entropy)
# =============================================================================

#' Calculate Log Loss for a single threshold
#'
#' @param predictions Data frame with athlete_id and prob_top_{threshold} columns
#' @param actuals Data frame with athlete_id and Place columns
#' @param threshold Position threshold
#' @param eps Small value to prevent log(0)
#' @return Log loss (lower is better)
calculate_log_loss <- function(predictions, actuals, threshold, eps = 1e-7) {
  prob_col <- paste0("prob_top_", threshold)

  if (!prob_col %in% names(predictions)) {
    warning(paste("Column", prob_col, "not found in predictions"))
    return(NA)
  }

  merged <- predictions %>%
    inner_join(actuals, by = "athlete_id") %>%
    mutate(
      outcome = as.numeric(Place <= threshold),
      # Clip probabilities to avoid log(0)
      prob_clipped = pmax(eps, pmin(1 - eps, .data[[prob_col]])),
      log_loss_i = -(outcome * log(prob_clipped) + (1 - outcome) * log(1 - prob_clipped))
    )

  if (nrow(merged) == 0) {
    return(NA)
  }

  mean_log_loss <- mean(merged$log_loss_i, na.rm = TRUE)
  return(mean_log_loss)
}

#' Calculate Log Loss across multiple races
#'
#' @param backtest_results List of backtest results
#' @param threshold Position threshold
#' @param eps Small value to prevent log(0)
#' @return Aggregate log loss
calculate_log_loss_aggregate <- function(backtest_results, threshold, eps = 1e-7) {
  all_log_losses <- c()

  for (result in backtest_results) {
    prob_col <- paste0("prob_top_", threshold)

    if (!prob_col %in% names(result$predictions)) next

    merged <- result$predictions %>%
      inner_join(result$actuals, by = "athlete_id") %>%
      mutate(
        outcome = as.numeric(Place <= threshold),
        prob_clipped = pmax(eps, pmin(1 - eps, .data[[prob_col]])),
        log_loss_i = -(outcome * log(prob_clipped) + (1 - outcome) * log(1 - prob_clipped))
      )

    all_log_losses <- c(all_log_losses, merged$log_loss_i)
  }

  if (length(all_log_losses) == 0) return(NA)

  return(mean(all_log_losses, na.rm = TRUE))
}

# =============================================================================
# CALIBRATION ERROR (Expected Calibration Error - ECE)
# =============================================================================

#' Calculate Expected Calibration Error for a single threshold
#'
#' Measures how well predicted probabilities match observed frequencies.
#' A well-calibrated model has ECE close to 0.
#'
#' @param predictions Data frame with athlete_id and prob_top_{threshold} columns
#' @param actuals Data frame with athlete_id and Place columns
#' @param threshold Position threshold
#' @param n_bins Number of probability bins (default 10)
#' @return ECE score (lower is better, range 0-1)
calculate_calibration_ece <- function(predictions, actuals, threshold, n_bins = 10) {
  prob_col <- paste0("prob_top_", threshold)

  if (!prob_col %in% names(predictions)) {
    warning(paste("Column", prob_col, "not found in predictions"))
    return(NA)
  }

  merged <- predictions %>%
    inner_join(actuals, by = "athlete_id") %>%
    mutate(
      outcome = as.numeric(Place <= threshold),
      prob = .data[[prob_col]],
      bin = cut(prob, breaks = seq(0, 1, length.out = n_bins + 1),
                include.lowest = TRUE, labels = FALSE)
    )

  if (nrow(merged) == 0) {
    return(NA)
  }

  # Calculate per-bin statistics
  bin_stats <- merged %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      mean_prob = mean(prob, na.rm = TRUE),
      mean_outcome = mean(outcome, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(mean_prob), !is.na(mean_outcome)) %>%
    mutate(
      calibration_error = abs(mean_prob - mean_outcome)
    )

  if (nrow(bin_stats) == 0) {
    return(NA)
  }

  # Weighted average by bin size
  total_n <- sum(bin_stats$n)
  ece <- sum(bin_stats$n * bin_stats$calibration_error) / total_n

  return(ece)
}

#' Calculate ECE across multiple races
#'
#' @param backtest_results List of backtest results
#' @param threshold Position threshold
#' @param n_bins Number of probability bins
#' @return Aggregate ECE
calculate_calibration_ece_aggregate <- function(backtest_results, threshold, n_bins = 10) {
  # Combine all predictions and actuals
  all_preds <- bind_rows(lapply(backtest_results, function(r) r$predictions))
  all_actuals <- bind_rows(lapply(backtest_results, function(r) r$actuals))

  # Remove duplicate athlete_ids (take first occurrence for actuals)
  all_actuals <- all_actuals %>%
    group_by(athlete_id) %>%
    slice(1) %>%
    ungroup()

  return(calculate_calibration_ece(all_preds, all_actuals, threshold, n_bins))
}

# =============================================================================
# COMPOSITE SCORING
# =============================================================================

#' Calculate all metrics for a single race
#'
#' @param predictions Data frame with predictions
#' @param actuals Data frame with actual results
#' @param thresholds Vector of position thresholds (default: c(1, 3, 5, 10, 30))
#' @return List with all metrics
calculate_race_metrics <- function(predictions, actuals,
                                    thresholds = c(1, 3, 5, 10, 30)) {
  metrics <- list()

  for (t in thresholds) {
    t_str <- as.character(t)
    metrics[[paste0("brier_", t_str)]] <- calculate_brier_score(predictions, actuals, t)
    metrics[[paste0("logloss_", t_str)]] <- calculate_log_loss(predictions, actuals, t)
    metrics[[paste0("ece_", t_str)]] <- calculate_calibration_ece(predictions, actuals, t)
  }

  return(metrics)
}

#' Calculate all metrics across multiple races with weighted composite
#'
#' @param backtest_results List of backtest results
#' @param thresholds Vector of position thresholds
#' @param threshold_weights Weights for each threshold (should sum to 1)
#' @param metric_weights Weights for Brier, LogLoss, ECE (should sum to 1)
#' @return List with all metrics including composite score
calculate_all_metrics <- function(backtest_results,
                                   thresholds = c(1, 3, 5, 10, 30),
                                   threshold_weights = c(0.30, 0.25, 0.20, 0.15, 0.10),
                                   metric_weights = c(brier = 0.5, logloss = 0.3, ece = 0.2)) {

  if (length(thresholds) != length(threshold_weights)) {
    stop("thresholds and threshold_weights must have same length")
  }

  # Calculate metrics for each threshold
  brier_scores <- numeric(length(thresholds))
  logloss_scores <- numeric(length(thresholds))
  ece_scores <- numeric(length(thresholds))

  for (i in seq_along(thresholds)) {
    t <- thresholds[i]
    brier_scores[i] <- calculate_brier_score_aggregate(backtest_results, t)
    logloss_scores[i] <- calculate_log_loss_aggregate(backtest_results, t)
    ece_scores[i] <- calculate_calibration_ece_aggregate(backtest_results, t)
  }

  # Replace NAs with worst-case values for scoring
  brier_scores[is.na(brier_scores)] <- 1.0

logloss_scores[is.na(logloss_scores)] <- 10.0
  ece_scores[is.na(ece_scores)] <- 1.0

  # Weighted averages across thresholds
  weighted_brier <- sum(brier_scores * threshold_weights)
  weighted_logloss <- sum(logloss_scores * threshold_weights)
  weighted_ece <- sum(ece_scores * threshold_weights)

  # Normalize log loss for composite (divide by log(2) to scale similar to Brier)
  normalized_logloss <- weighted_logloss / log(2)

  # Composite score
  composite_score <- metric_weights["brier"] * weighted_brier +
                     metric_weights["logloss"] * normalized_logloss +
                     metric_weights["ece"] * weighted_ece

  return(list(
    # Per-threshold metrics
    brier_by_threshold = setNames(brier_scores, paste0("top_", thresholds)),
    logloss_by_threshold = setNames(logloss_scores, paste0("top_", thresholds)),
    ece_by_threshold = setNames(ece_scores, paste0("top_", thresholds)),

    # Weighted averages
    brier_score = weighted_brier,
    log_loss = weighted_logloss,
    calibration_ece = weighted_ece,

    # Composite
    composite_score = composite_score,

    # Metadata
    n_races = length(backtest_results),
    n_predictions = sum(sapply(backtest_results, function(r) nrow(r$predictions)))
  ))
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Format metrics for display
#'
#' @param metrics List from calculate_all_metrics
#' @return Formatted string
format_metrics <- function(metrics) {
  sprintf(
    "Composite: %.4f | Brier: %.4f | LogLoss: %.4f | ECE: %.4f | Races: %d | Predictions: %d",
    metrics$composite_score,
    metrics$brier_score,
    metrics$log_loss,
    metrics$calibration_ece,
    metrics$n_races,
    metrics$n_predictions
  )
}

#' Compare two parameter sets
#'
#' @param metrics1 Metrics from first parameter set
#' @param metrics2 Metrics from second parameter set
#' @return Data frame comparing metrics
compare_metrics <- function(metrics1, metrics2,
                            name1 = "Params1", name2 = "Params2") {
  data.frame(
    Metric = c("Composite", "Brier", "LogLoss", "ECE"),
    !!name1 := c(metrics1$composite_score, metrics1$brier_score,
                 metrics1$log_loss, metrics1$calibration_ece),
    !!name2 := c(metrics2$composite_score, metrics2$brier_score,
                 metrics2$log_loss, metrics2$calibration_ece),
    Diff = c(
      metrics2$composite_score - metrics1$composite_score,
      metrics2$brier_score - metrics1$brier_score,
      metrics2$log_loss - metrics1$log_loss,
      metrics2$calibration_ece - metrics1$calibration_ece
    ),
    Better = c(
      ifelse(metrics2$composite_score < metrics1$composite_score, name2, name1),
      ifelse(metrics2$brier_score < metrics1$brier_score, name2, name1),
      ifelse(metrics2$log_loss < metrics1$log_loss, name2, name1),
      ifelse(metrics2$calibration_ece < metrics1$calibration_ece, name2, name1)
    )
  )
}

# =============================================================================
# CALIBRATION PLOT DATA
# =============================================================================

#' Generate calibration plot data
#'
#' @param backtest_results List of backtest results
#' @param threshold Position threshold
#' @param n_bins Number of probability bins
#' @return Data frame with bin midpoints and observed frequencies
get_calibration_data <- function(backtest_results, threshold, n_bins = 10) {
  prob_col <- paste0("prob_top_", threshold)

  # Combine all results
  all_data <- bind_rows(lapply(backtest_results, function(r) {
    r$predictions %>%
      inner_join(r$actuals, by = "athlete_id") %>%
      mutate(
        outcome = as.numeric(Place <= threshold),
        prob = .data[[prob_col]]
      ) %>%
      select(athlete_id, prob, outcome)
  }))

  if (nrow(all_data) == 0) {
    return(data.frame())
  }

  # Create bins
  breaks <- seq(0, 1, length.out = n_bins + 1)
  all_data$bin <- cut(all_data$prob, breaks = breaks, include.lowest = TRUE, labels = FALSE)

  # Calculate bin statistics
  calibration_data <- all_data %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      mean_predicted = mean(prob, na.rm = TRUE),
      mean_observed = mean(outcome, na.rm = TRUE),
      se_observed = sqrt(mean_observed * (1 - mean_observed) / n),
      .groups = "drop"
    ) %>%
    mutate(
      bin_midpoint = (breaks[bin] + breaks[bin + 1]) / 2
    )

  return(calibration_data)
}

cat("scoring-metrics.R loaded successfully\n")
