# ============================================================================
# RACE PICKS SIMULATION - COMMON FUNCTIONS
# ============================================================================
#
# Shared functions for race picks simulation across all winter sports.
# This library provides the core Monte Carlo simulation infrastructure.
#
# Usage:
#   source("~/blog/daehl-e/content/post/shared/race-picks-common.R")
#
# Sports supported: Alpine, Biathlon, Cross-Country, Nordic Combined, Ski Jumping
#
# Each sport provides:
#   - SPORT_CONFIG: Configuration list with parameters
#   - get_explanatory_vars(race_type): Sport-specific feature selection
#   - get_race_filter(race_type): Sport-specific data filtering
#   - calculate_weighted_prev_points(): Sport-specific race type matching

library(dplyr)
library(tidyr)
library(openxlsx)
library(mgcv)
library(leaps)
library(logger)
library(purrr)
library(lubridate)

# Source logging utilities
source("~/blog/daehl-e/content/post/shared/logging-utils.R")

# ============================================================================
# ENVIRONMENT LOADING
# ============================================================================

#' Load environment variables from .env file
#'
#' @param env_path Path to .env file
#' @return TRUE if loaded successfully, FALSE otherwise
load_env <- function(env_path = "~/ski/elo/.env") {
  env_file <- path.expand(env_path)
  if (file.exists(env_file)) {
    lines <- readLines(env_file, warn = FALSE)
    for (line in lines) {
      line <- trimws(line)
      if (nchar(line) > 0 && !startsWith(line, "#") && grepl("=", line)) {
        parts <- strsplit(line, "=", fixed = TRUE)[[1]]
        key <- trimws(parts[1])
        value <- trimws(paste(parts[-1], collapse = "="))
        value <- gsub("^[\"']|[\"']$", "", value)  # Remove quotes
        do.call(Sys.setenv, setNames(list(value), key))
      }
    }
    return(TRUE)
  }
  return(FALSE)
}

# ============================================================================
# DEFAULT CONFIGURATION
# ============================================================================

#' Create default configuration for a sport
#'
#' @param sport Sport name
#' @return Configuration list
create_default_config <- function(sport) {
  list(
    sport = sport,

    # Simulation parameters
    n_history_required = 10,       # Target historical races per athlete
    n_gam_samples = 0,             # GAM samples (0 = use history + GAM fill only)
    gam_fill_weight_factor = 0.25, # Weight multiplier for GAM-filled slots
    n_simulations = 1000,          # Monte Carlo simulations per race
    decay_lambda = 0.002,          # Exponential decay (0.002 = 50% after 1 year)

    # Variance control
    sd_scale_factor = 0.77,        # Multiply SDs (lower = more deterministic)
    sd_min = 4,                    # Minimum SD
    sd_max = 16,                   # Maximum SD

    # Team/relay variance (if applicable)
    team_sd_scale_factor = 0.8,
    team_sd_min = 3,
    team_sd_max = 12,

    # Position thresholds
    position_thresholds = c(1, 3, 5, 10, 30),

    # Points system (World Cup default)
    points_table = c(100, 95, 90, 85, 80, 75, 72, 69, 66, 63, 60, 58, 56, 54, 52,
                     50, 48, 46, 44, 42, 40, 38, 36, 34, 32, 30, 28, 26, 24, 22,
                     20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5,
                     4, 3, 2, 1),

    # Event-specific overrides (empty by default)
    event_params = list()
  )
}

#' Get configuration parameter, checking event-specific overrides
#'
#' @param config Configuration list
#' @param param Parameter name
#' @param event_type Optional event type for override lookup
#' @return Parameter value
get_config_param <- function(config, param, event_type = NULL) {
  # Check event-specific override first
  if (!is.null(event_type) && !is.null(config$event_params[[event_type]])) {
    if (!is.null(config$event_params[[event_type]][[param]])) {
      return(config$event_params[[event_type]][[param]])
    }
  }
  # Return base config value
  return(config[[param]])
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Convert finish place to points
#'
#' @param place Finish position (1-based)
#' @param points_list Vector of points by position
#' @return Points earned
get_points <- function(place, points_list) {
  if (is.na(place) || place < 1 || place > length(points_list)) {
    return(0)
  }
  return(points_list[place])
}

#' Replace NA values with first quartile
#'
#' @param x Numeric vector
#' @return Vector with NAs replaced
replace_na_with_quartile <- function(x) {
  if (all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

#' Filter features to keep only those with positive coefficients
#'
#' Iteratively removes features with negative coefficients until all are positive.
#' This ensures that higher ELO/performance metrics predict better results.
#'
#' @param data Training data
#' @param response_var Response variable name
#' @param candidate_vars Candidate predictor variables
#' @param family Model family ("gaussian" or "binomial")
#' @return Vector of variables with positive coefficients
filter_positive_coefficients <- function(data, response_var, candidate_vars, family = "gaussian") {
  if (length(candidate_vars) == 0) return(character(0))

  current_vars <- candidate_vars
  max_iterations <- length(candidate_vars)

  for (iter in 1:max_iterations) {
    if (length(current_vars) == 0) break

    formula_str <- paste(response_var, "~", paste(current_vars, collapse = " + "))

    model <- tryCatch({
      if (family == "binomial") {
        glm(as.formula(formula_str), data = data, family = binomial())
      } else {
        lm(as.formula(formula_str), data = data)
      }
    }, error = function(e) NULL)

    if (is.null(model)) {
      log_warn("Model fitting failed in positive coefficient filter, returning remaining vars")
      break
    }

    coefs <- coef(model)
    coefs <- coefs[names(coefs) != "(Intercept)"]

    negative_vars <- names(coefs[coefs < 0])

    if (length(negative_vars) == 0) {
      break
    }

    log_info(paste("    Removing negative coefficient features:", paste(negative_vars, collapse = ", ")))
    current_vars <- setdiff(current_vars, negative_vars)
  }

  if (length(current_vars) > 0) {
    log_info(paste("    Final positive-coefficient features:", paste(current_vars, collapse = ", ")))
  } else {
    log_warn("    No features with positive coefficients remaining")
  }

  return(current_vars)
}

# ============================================================================
# DATA PREPROCESSING
# ============================================================================

#' Calculate PELO percentage columns (normalized within each race)
#'
#' @param chrono_data Chronological data with Pelo columns
#' @param pelo_columns Vector of Pelo column names to normalize
#' @return Data with _pct columns added
calculate_percentage_columns <- function(chrono_data, pelo_columns) {
  for (col in pelo_columns) {
    if (col %in% names(chrono_data)) {
      pct_col <- paste0(col, "_pct")
      chrono_data <- chrono_data %>%
        group_by(Season, Race) %>%
        mutate(!!pct_col := {
          max_val <- max(.data[[col]], na.rm = TRUE)
          if (is.na(max_val) || max_val == 0) {
            rep(0.5, n())
          } else {
            .data[[col]] / max_val
          }
        }) %>%
        ungroup()
    }
  }
  return(chrono_data)
}

#' Generic weighted prev_points calculation
#'
#' This is a template - each sport should implement their own version
#' that filters by appropriate race type matching criteria.
#'
#' @param chrono_data Chronological data
#' @param config Sport configuration
#' @param race_type_matcher Function(prev_row, current_row) -> TRUE/FALSE
#' @return Data with prev_points_weighted column
calculate_weighted_prev_points_generic <- function(chrono_data, config, race_type_matcher) {
  decay_lambda <- get_config_param(config, "decay_lambda")

  log_info("Calculating weighted prev_points with exponential decay...")
  phase_start("Weighted Prev Points")

  result <- chrono_data %>%
    arrange(ID, Date) %>%
    group_by(ID) %>%
    mutate(
      prev_points_weighted = sapply(row_number(), function(i) {
        if (i == 1) return(0)

        prev_data <- cur_data()[1:(i-1), ]
        current_row <- cur_data()[i, ]
        if (nrow(prev_data) == 0) return(0)

        # Apply sport-specific race type matching
        matching <- sapply(1:nrow(prev_data), function(j) {
          race_type_matcher(prev_data[j, ], current_row)
        })

        if (!any(matching)) return(0)

        matching_points <- prev_data$points[matching]
        matching_dates <- prev_data$Date[matching]

        current_date <- current_row$Date
        days_ago <- as.numeric(difftime(current_date, matching_dates, units = "days"))
        weights <- exp(-decay_lambda * days_ago)

        weighted.mean(matching_points, weights, na.rm = TRUE)
      })
    ) %>%
    ungroup()

  phase_end("Weighted Prev Points", sprintf("%d athletes processed", length(unique(result$ID))))
  return(result)
}

#' Preprocess chronological data
#'
#' @param chrono_data Raw chronological data
#' @param pelo_columns Pelo columns to normalize
#' @param config Sport configuration
#' @return Preprocessed data
preprocess_chrono <- function(chrono_data, pelo_columns, config) {
  phase_start("Preprocessing")

  # Log input data quality
  log_data_quality(chrono_data, "Input chrono data", c("ID", "Date", "Place", "points"))

  # Calculate percentage columns
  chrono_data <- calculate_percentage_columns(chrono_data, pelo_columns)

  # Fill NAs in percentage columns
  pct_cols <- paste0(pelo_columns, "_pct")
  for (col in pct_cols) {
    if (col %in% names(chrono_data)) {
      chrono_data[[col]] <- replace_na_with_quartile(chrono_data[[col]])
    }
  }

  log_data_quality(chrono_data, "Preprocessed chrono data")
  phase_end("Preprocessing", sprintf("%d rows, %d athletes", nrow(chrono_data), length(unique(chrono_data$ID))))

  return(chrono_data)
}

# ============================================================================
# GAM MODEL TRAINING
# ============================================================================

#' Train GAM model for points prediction
#'
#' @param chrono_data Training data
#' @param race_filter Expression to filter data for race type
#' @param explanatory_vars Candidate explanatory variables
#' @param race_type_name Name for logging
#' @param gender Gender for logging
#' @return List with model, residual_sd, features, or NULL on failure
train_points_gam <- function(chrono_data, race_filter, explanatory_vars,
                              race_type_name, gender) {

  log_info(paste("Training", gender, race_type_name, "POINTS GAM"))

  # Filter data
  filtered_data <- chrono_data %>% filter(eval(race_filter))
  log_info(paste("    Filtered to", nrow(filtered_data), "records"))

  if (nrow(filtered_data) < 50) {
    log_warn(paste("    Insufficient data for", race_type_name, "- need 50, have", nrow(filtered_data)))
    return(NULL)
  }

  # Ensure explanatory vars exist
  available_vars <- explanatory_vars[explanatory_vars %in% names(filtered_data)]
  if (length(available_vars) < 2) {
    log_warn(paste("    Insufficient explanatory variables available"))
    return(NULL)
  }

  # Fill NAs in explanatory vars
  for (var in available_vars) {
    filtered_data[[var]] <- replace_na_with_quartile(filtered_data[[var]])
  }

  tryCatch({
    # Feature selection via BIC
    formula <- as.formula(paste("points ~", paste(available_vars, collapse = " + ")))
    feature_selection <- regsubsets(formula, data = filtered_data, nbest = 1, method = "exhaustive")
    feature_summary <- summary(feature_selection)
    best_bic_vars <- names(coef(feature_selection, which.min(feature_summary$bic)))[-1]

    log_info(paste("    BIC selected:", paste(best_bic_vars, collapse = ", ")))

    # Filter to positive coefficients
    positive_vars <- filter_positive_coefficients(filtered_data, "points", best_bic_vars, family = "gaussian")

    if (length(positive_vars) == 0) {
      log_warn("    No positive coefficient features - using prev_points_weighted only")
      positive_vars <- "prev_points_weighted"
    }

    # Train GAM
    smooth_terms <- paste("s(", positive_vars, ")", collapse = " + ")
    gam_formula <- as.formula(paste("points ~", smooth_terms))
    points_model <- gam(gam_formula, data = filtered_data, method = "REML")

    # Get residual SD
    residual_sd <- sqrt(points_model$sig2)
    if (is.null(residual_sd) || is.na(residual_sd)) {
      residual_sd <- sqrt(points_model$deviance / points_model$df.residual)
    }
    residual_sd <- max(residual_sd, 5)

    # Log training details
    log_gam_training(points_model, positive_vars, residual_sd, race_type_name, gender)

    return(list(
      model = points_model,
      residual_sd = residual_sd,
      features = positive_vars,
      race_type = race_type_name
    ))

  }, error = function(e) {
    log_error(paste("Error training GAM:", e$message))
    return(NULL)
  })
}

# ============================================================================
# DISTRIBUTION BUILDING
# ============================================================================

#' Build athlete performance distribution
#'
#' Combines historical results (decay-weighted) with GAM predictions
#' for athletes with insufficient history.
#'
#' @param athlete_id Athlete ID
#' @param chrono_data Historical data
#' @param race_filter Expression to filter by race type
#' @param gam_prediction GAM predicted points for this athlete
#' @param gam_residual_sd GAM residual SD
#' @param config Sport configuration
#' @param reference_date Reference date for decay calculation
#' @param event_type Optional event type for config override
#' @return List with mean, sd, n_actual_races, athlete_id
build_athlete_distribution <- function(athlete_id, chrono_data, race_filter,
                                        gam_prediction, gam_residual_sd,
                                        config, reference_date = NULL,
                                        event_type = NULL) {

  n_history <- get_config_param(config, "n_history_required", event_type)
  gam_fill_weight <- get_config_param(config, "gam_fill_weight_factor", event_type)
  decay_lambda <- get_config_param(config, "decay_lambda", event_type)
  points_table <- get_config_param(config, "points_table", event_type)

  # Get athlete's historical results
  athlete_history <- chrono_data %>%
    filter(ID == athlete_id, eval(race_filter)) %>%
    arrange(desc(Date)) %>%
    head(n_history)

  n_actual_races <- nrow(athlete_history)

  all_points <- c()
  all_weights <- c()

  # Part 1: Historical races with exponential decay
  if (n_actual_races > 0) {
    history_points <- sapply(athlete_history$Place, function(p) get_points(p, points_table))

    if (is.null(reference_date)) {
      reference_date <- max(chrono_data$Date, na.rm = TRUE)
    }

    days_ago <- as.numeric(reference_date - athlete_history$Date)
    history_weights <- exp(-decay_lambda * days_ago)

    all_points <- c(all_points, history_points)
    all_weights <- c(all_weights, history_weights)
  }

  # Part 2: GAM fill for missing history slots
  n_missing <- n_history - n_actual_races

  if (n_missing > 0) {
    # Generate synthetic points from GAM distribution
    gam_fill_points <- rnorm(n_missing, mean = gam_prediction, sd = gam_residual_sd)
    gam_fill_points <- pmax(0, pmin(100, gam_fill_points))

    # Weight at fraction of median historical weight
    if (n_actual_races > 0) {
      median_weight <- median(all_weights) * gam_fill_weight
    } else {
      # No history - assume "1 year ago" weight
      median_weight <- exp(-decay_lambda * 365) * gam_fill_weight
    }
    gam_fill_weights <- rep(median_weight, n_missing)

    all_points <- c(all_points, gam_fill_points)
    all_weights <- c(all_weights, gam_fill_weights)
  }

  # Calculate distribution parameters
  if (length(all_points) == 0 || all(is.na(all_points))) {
    # Fallback for completely unknown athletes
    return(list(
      athlete_id = athlete_id,
      mean = gam_prediction,
      sd = gam_residual_sd,
      n_actual_races = 0
    ))
  }

  weighted_mean <- weighted.mean(all_points, all_weights, na.rm = TRUE)
  weighted_var <- sum(all_weights * (all_points - weighted_mean)^2) / sum(all_weights)
  weighted_sd <- sqrt(weighted_var)
  weighted_sd <- max(weighted_sd, 5)  # Floor to prevent degenerate distributions

  return(list(
    athlete_id = athlete_id,
    mean = weighted_mean,
    sd = weighted_sd,
    n_actual_races = n_actual_races
  ))
}

# ============================================================================
# MONTE CARLO SIMULATION
# ============================================================================

#' Simulate race positions using Monte Carlo
#'
#' @param athlete_distributions List of distribution objects
#' @param config Sport configuration
#' @param event_type Optional event type for config override
#' @param max_points Maximum points (for clamping)
#' @return Dataframe with athlete results and probabilities
simulate_race_positions <- function(athlete_distributions, config,
                                     event_type = NULL, max_points = 100) {

  n_simulations <- get_config_param(config, "n_simulations", event_type)
  sd_scale <- get_config_param(config, "sd_scale_factor", event_type)
  sd_min <- get_config_param(config, "sd_min", event_type)
  sd_max <- get_config_param(config, "sd_max", event_type)
  position_thresholds <- get_config_param(config, "position_thresholds", event_type)

  # Filter out invalid distributions
  valid_distributions <- athlete_distributions[sapply(athlete_distributions, function(x) {
    !is.null(x) && !is.null(x$mean) && !is.na(x$mean) &&
      !is.null(x$sd) && !is.na(x$sd) && x$sd > 0
  })]

  if (length(valid_distributions) == 0) {
    log_warn("No valid distributions for simulation")
    return(data.frame())
  }

  n_athletes <- length(valid_distributions)
  athlete_ids <- sapply(valid_distributions, function(x) x$athlete_id)

  # Extract means and sds
  means <- sapply(valid_distributions, function(x) x$mean)
  sds <- sapply(valid_distributions, function(x) x$sd)

  # Apply SD scaling and bounds
  scaled_sds <- pmax(sd_min, pmin(sd_max, sds * sd_scale))

  # Generate all simulations (n_athletes x n_simulations matrix)
  set.seed(NULL)  # Ensure randomness
  all_sims <- matrix(rnorm(n_athletes * n_simulations),
                     nrow = n_athletes, ncol = n_simulations)
  all_sims <- all_sims * scaled_sds + means
  all_sims <- pmax(0, pmin(max_points, all_sims))

  # Rank each simulation (higher points = rank 1)
  ranks_matrix <- apply(all_sims, 2, function(x) rank(-x, ties.method = "random"))

  # Count position achievements
  position_counts <- matrix(0, nrow = n_athletes, ncol = length(position_thresholds))
  for (t_idx in seq_along(position_thresholds)) {
    threshold <- position_thresholds[t_idx]
    position_counts[, t_idx] <- rowSums(ranks_matrix <= threshold)
  }

  # Convert to probabilities
  position_probs <- position_counts / n_simulations

  # Build results dataframe
  results <- data.frame(
    athlete_id = athlete_ids,
    mean_points = means,
    sd_points = sds,
    n_actual_races = sapply(valid_distributions, function(x) x$n_actual_races),
    stringsAsFactors = FALSE
  )

  # Add probability columns
  for (t_idx in seq_along(position_thresholds)) {
    threshold <- position_thresholds[t_idx]
    col_name <- paste0("prob_top_", threshold)
    results[[col_name]] <- position_probs[, t_idx]
  }

  # Sort by expected performance
  results <- results %>% arrange(desc(mean_points))

  return(results)
}

# ============================================================================
# OUTPUT FORMATTING
# ============================================================================

#' Format results for Excel output
#'
#' @param results Simulation results
#' @param startlist Startlist data
#' @param config Sport configuration
#' @return Formatted dataframe
format_results_for_output <- function(results, startlist, config) {
  position_thresholds <- config$position_thresholds

  # Merge with startlist to get skier info
  if ("Skier" %in% names(startlist) && "ID" %in% names(startlist)) {
    output <- results %>%
      left_join(startlist %>% select(ID, Skier, Nation) %>% distinct(),
                by = c("athlete_id" = "ID"))
  } else {
    output <- results
    output$Skier <- output$athlete_id
    output$Nation <- NA
  }

  # Rename probability columns
  prob_col_map <- list(
    "prob_top_1" = "Win",
    "prob_top_3" = "Podium",
    "prob_top_5" = "Top-5",
    "prob_top_6" = "Final",
    "prob_top_10" = "Top-10",
    "prob_top_12" = "Semifinal",
    "prob_top_30" = "Top-30"
  )

  for (old_name in names(prob_col_map)) {
    if (old_name %in% names(output)) {
      new_name <- prob_col_map[[old_name]]
      names(output)[names(output) == old_name] <- new_name
      # Convert to percentage
      output[[new_name]] <- round(output[[new_name]] * 100, 1)
    }
  }

  # Select and order columns
  output_cols <- c("Skier", "Nation")
  prob_cols <- c("Win", "Podium", "Top-5", "Final", "Top-10", "Semifinal", "Top-30")
  output_cols <- c(output_cols, prob_cols[prob_cols %in% names(output)])

  output <- output %>%
    select(any_of(output_cols)) %>%
    arrange(desc(Win))

  return(output)
}

#' Save results to Excel workbook
#'
#' @param results_list Named list of result dataframes
#' @param output_path Path to save Excel file
#' @return TRUE on success
save_to_excel <- function(results_list, output_path) {
  wb <- createWorkbook()

  for (sheet_name in names(results_list)) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, results_list[[sheet_name]])
  }

  # Create directory if needed
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  saveWorkbook(wb, output_path, overwrite = TRUE)
  log_info(paste("Saved:", output_path))

  return(TRUE)
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Get races file path based on TEST_MODE
#'
#' @param sport Sport directory name
#' @param test_mode Whether in test mode
#' @return Path to races file
get_races_file <- function(sport, test_mode) {
  sport_dir <- switch(sport,
                      "cross-country" = "ski",
                      "nordic-combined" = "nordic-combined",
                      sport)

  base_path <- file.path("~/ski/elo/python", sport_dir, "polars/excel365")
  filename <- if (test_mode) "test_races.csv" else "races.csv"

  return(path.expand(file.path(base_path, filename)))
}

#' Get chrono file path
#'
#' @param sport Sport directory name
#' @param gender "men" or "ladies"
#' @return Path to chrono file
get_chrono_file <- function(sport, gender) {
  sport_dir <- switch(sport,
                      "cross-country" = "ski",
                      "nordic-combined" = "nordic-combined",
                      sport)

  base_path <- file.path("~/ski/elo/python", sport_dir, "polars/excel365")
  filename <- paste0(gender, "_chrono_elevation.csv")

  return(path.expand(file.path(base_path, filename)))
}

#' Get startlist file path
#'
#' @param sport Sport directory name
#' @param gender "men" or "ladies"
#' @return Path to startlist file
get_startlist_file <- function(sport, gender) {
  sport_dir <- switch(sport,
                      "cross-country" = "ski",
                      "nordic-combined" = "nordic-combined",
                      sport)

  base_path <- file.path("~/ski/elo/python", sport_dir, "polars/excel365")
  filename <- paste0("startlist_races_", gender, ".csv")

  return(path.expand(file.path(base_path, filename)))
}

# ============================================================================
# MODULE EXPORTS (for documentation)
# ============================================================================
#
# Functions exported by this module:
#
# Configuration:
#   load_env()
#   create_default_config(sport)
#   get_config_param(config, param, event_type)
#
# Helpers:
#   get_points(place, points_list)
#   replace_na_with_quartile(x)
#   filter_positive_coefficients(data, response_var, candidate_vars, family)
#
# Preprocessing:
#   calculate_percentage_columns(chrono_data, pelo_columns)
#   calculate_weighted_prev_points_generic(chrono_data, config, race_type_matcher)
#   preprocess_chrono(chrono_data, pelo_columns, config)
#
# Modeling:
#   train_points_gam(chrono_data, race_filter, explanatory_vars, race_type_name, gender)
#
# Simulation:
#   build_athlete_distribution(athlete_id, chrono_data, race_filter, ...)
#   simulate_race_positions(athlete_distributions, config, event_type, max_points)
#
# Output:
#   format_results_for_output(results, startlist, config)
#   save_to_excel(results_list, output_path)
#
# Utility:
#   get_races_file(sport, test_mode)
#   get_chrono_file(sport, gender)
#   get_startlist_file(sport, gender)
