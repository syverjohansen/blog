# backtest-engine.R
# Backtesting engine for parameter optimization
# Runs simulations against historical races and evaluates predictions

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(mgcv)
})

# Source scoring metrics
source("~/blog/daehl-e/content/post/optimization/scoring-metrics.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

# Default number of simulations for backtesting (lower for speed)
BACKTEST_N_SIMULATIONS <- 500

# Position thresholds to evaluate
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)

# =============================================================================
# CORE SIMULATION FUNCTIONS (Simplified for backtesting)
# =============================================================================

#' Calculate exponential decay weights for historical races
#'
#' @param days_ago Vector of days since each race
#' @param decay_lambda Decay rate (0.002 = ~50% weight after 1 year)
#' @return Vector of weights
calculate_decay_weights <- function(days_ago, decay_lambda) {
  exp(-decay_lambda * days_ago)
}

#' Build athlete distribution from historical data
#'
#' @param athlete_id Athlete ID
#' @param chrono_data Historical race data
#' @param race_date Date of race being predicted
#' @param params List of parameters (decay_lambda, n_history_required, etc.)
#' @return List with mean and sd for athlete's point distribution
build_athlete_distribution_backtest <- function(athlete_id, chrono_data, race_date, params) {

  # Get athlete's historical races BEFORE this race
  athlete_history <- chrono_data %>%
    filter(ID == athlete_id, Date < race_date) %>%
    arrange(desc(Date)) %>%
    head(params$n_history_required)

  n_races <- nrow(athlete_history)

  if (n_races == 0) {
    # No history - use population median with high variance
    pop_median <- median(chrono_data$points, na.rm = TRUE)
    return(list(
      mean = ifelse(is.na(pop_median), 10, pop_median),
      sd = params$sd_max,
      n_races = 0
    ))
  }

  # Calculate decay weights
  days_ago <- as.numeric(race_date - athlete_history$Date)
  weights <- calculate_decay_weights(days_ago, params$decay_lambda)
  weights <- weights / sum(weights)  # Normalize

  # Weighted mean and SD
  weighted_mean <- sum(athlete_history$points * weights)
  weighted_var <- sum(weights * (athlete_history$points - weighted_mean)^2)
  weighted_sd <- sqrt(weighted_var)

  # Apply scaling and bounds
  scaled_sd <- weighted_sd * params$sd_scale_factor
  bounded_sd <- pmax(params$sd_min, pmin(params$sd_max, scaled_sd))

  # If insufficient history, increase uncertainty
  if (n_races < params$n_history_required) {
    history_factor <- n_races / params$n_history_required
    bounded_sd <- bounded_sd + (params$sd_max - bounded_sd) * (1 - history_factor) * params$gam_fill_weight_factor
  }

  return(list(
    mean = weighted_mean,
    sd = bounded_sd,
    n_races = n_races
  ))
}

#' Run Monte Carlo simulation for a race
#'
#' @param athlete_distributions Named list of distributions (name = athlete_id)
#' @param n_simulations Number of simulations
#' @param position_thresholds Vector of position thresholds to calculate
#' @return Data frame with athlete_id and prob_top_X columns
simulate_race_backtest <- function(athlete_distributions, n_simulations = BACKTEST_N_SIMULATIONS,
                                    position_thresholds = POSITION_THRESHOLDS) {

  n_athletes <- length(athlete_distributions)
  athlete_ids <- names(athlete_distributions)

  if (n_athletes < 2) {
    return(data.frame())
  }

  # Extract means and SDs
  means <- sapply(athlete_distributions, function(d) d$mean)
  sds <- sapply(athlete_distributions, function(d) d$sd)

  # Replace any NA/Inf values
  means[is.na(means) | !is.finite(means)] <- 10
  sds[is.na(sds) | !is.finite(sds)] <- 16

  # Run simulations
  position_counts <- matrix(0, nrow = n_athletes, ncol = length(position_thresholds))

  for (sim in 1:n_simulations) {
    # Generate random points for each athlete
    sim_points <- rnorm(n_athletes, mean = means, sd = sds)

    # Rank by points (higher is better)
    ranks <- rank(-sim_points, ties.method = "random")

    # Count position achievements
    for (i in seq_along(position_thresholds)) {
      threshold <- position_thresholds[i]
      position_counts[, i] <- position_counts[, i] + as.numeric(ranks <= threshold)
    }
  }

  # Convert counts to probabilities
  position_probs <- position_counts / n_simulations

  # Build result dataframe
  result <- data.frame(
    athlete_id = as.numeric(athlete_ids),
    mean_points = means,
    sd_points = sds,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(position_thresholds)) {
    col_name <- paste0("prob_top_", position_thresholds[i])
    result[[col_name]] <- position_probs[, i]
  }

  return(result)
}

# =============================================================================
# BACKTEST FUNCTIONS
# =============================================================================

#' Get actual race results from chrono data
#'
#' @param race_info Data frame row with race metadata (Date, Sex, Distance, etc.)
#' @param chrono_data Full chrono data
#' @return Data frame with athlete_id and Place columns
get_race_results <- function(race_info, chrono_data) {
  # Filter to this exact race using all shared grouping columns.
  results <- chrono_data %>%
    filter(Date == race_info$Date, Sex == race_info$Sex)

  race_cols <- intersect(names(race_info), names(results))
  skip_cols <- c("n_competitors", "Date", "Sex")

  for (col in setdiff(race_cols, skip_cols)) {
    value <- race_info[[col]]
    if (length(value) == 0 || is.na(value)) next
    results <- results %>% filter(.data[[col]] == value)
  }

  # Return athlete_id and Place
  results %>%
    select(athlete_id = ID, Place) %>%
    filter(!is.na(Place), Place > 0)
}

#' Get startlist for a historical race (athletes who competed)
#'
#' @param race_info Race metadata
#' @param chrono_data Full chrono data
#' @return Vector of athlete IDs
get_race_startlist <- function(race_info, chrono_data) {
  results <- get_race_results(race_info, chrono_data)
  return(results$athlete_id)
}

#' Backtest a single race with given parameters
#'
#' @param race_info Data frame row with race metadata
#' @param chrono_data Full chrono data
#' @param params List of simulation parameters
#' @param n_simulations Number of Monte Carlo simulations
#' @return List with predictions, actuals, and race_info
backtest_race <- function(race_info, chrono_data, params,
                          n_simulations = BACKTEST_N_SIMULATIONS) {

  race_date <- as.Date(race_info$Date)

  # Get athletes who competed in this race
  athlete_ids <- get_race_startlist(race_info, chrono_data)

  if (length(athlete_ids) < 5) {
    # Skip races with very few competitors
    return(NULL)
  }

  # Build distributions for each athlete
  athlete_distributions <- list()
  for (id in athlete_ids) {
    dist <- build_athlete_distribution_backtest(
      athlete_id = id,
      chrono_data = chrono_data,
      race_date = race_date,
      params = params
    )
    athlete_distributions[[as.character(id)]] <- dist
  }

  # Run simulation
  predictions <- simulate_race_backtest(
    athlete_distributions = athlete_distributions,
    n_simulations = n_simulations
  )

  if (nrow(predictions) == 0) {
    return(NULL)
  }

  # Get actual results
  actuals <- get_race_results(race_info, chrono_data)

  return(list(
    predictions = predictions,
    actuals = actuals,
    race_info = race_info,
    n_athletes = length(athlete_ids)
  ))
}

#' Backtest multiple races with given parameters
#'
#' @param races Data frame of races to backtest
#' @param chrono_data Full chrono data
#' @param params List of simulation parameters
#' @param n_simulations Number of Monte Carlo simulations
#' @param verbose Print progress
#' @return List of backtest results
backtest_races <- function(races, chrono_data, params,
                            n_simulations = BACKTEST_N_SIMULATIONS,
                            verbose = FALSE) {

  results <- list()
  n_races <- nrow(races)

  for (i in 1:n_races) {
    if (verbose && i %% 10 == 0) {
      cat(sprintf("Backtesting race %d/%d\n", i, n_races))
    }

    race_result <- tryCatch({
      backtest_race(races[i, ], chrono_data, params, n_simulations)
    }, error = function(e) {
      if (verbose) cat(sprintf("Error on race %d: %s\n", i, e$message))
      NULL
    })

    if (!is.null(race_result)) {
      results[[length(results) + 1]] <- race_result
    }
  }

  return(results)
}

#' Evaluate a parameter set on calibration races
#'
#' @param params List of simulation parameters
#' @param races Data frame of calibration races
#' @param chrono_data Full chrono data
#' @param n_simulations Number of Monte Carlo simulations
#' @return List with metrics
evaluate_params <- function(params, races, chrono_data,
                            n_simulations = BACKTEST_N_SIMULATIONS) {

  # Run backtests
  backtest_results <- backtest_races(
    races = races,
    chrono_data = chrono_data,
    params = params,
    n_simulations = n_simulations,
    verbose = FALSE
  )

  if (length(backtest_results) == 0) {
    return(list(
      composite_score = Inf,
      brier_score = NA,
      log_loss = NA,
      calibration_ece = NA,
      n_races = 0,
      n_predictions = 0
    ))
  }

  # Calculate metrics
  metrics <- calculate_all_metrics(backtest_results)

  return(metrics)
}

# =============================================================================
# DATA LOADING HELPERS
# =============================================================================

#' Load chrono data for a sport
#'
#' @param sport Sport name ("cross-country", "alpine", "biathlon", "nordic-combined", "skijump")
#' @param gender "men" or "ladies"
#' @return Chrono data frame with standardized columns
load_chrono_data <- function(sport, gender) {
  # Map sport to directory
  sport_dirs <- list(
    "cross-country" = "ski",
    "alpine" = "alpine",
    "biathlon" = "biathlon",
    "nordic-combined" = "nordic-combined",
    "skijump" = "skijump"
  )

  sport_dir <- sport_dirs[[sport]]
  if (is.null(sport_dir)) {
    stop(paste("Unknown sport:", sport))
  }

  # Build file path
  file_name <- paste0(gender, "_chrono_elevation.csv")
  file_path <- file.path("~/ski/elo/python", sport_dir, "polars/excel365", file_name)

  if (!file.exists(file_path)) {
    # Try without _elevation suffix
    file_name <- paste0(gender, "_chrono.csv")
    file_path <- file.path("~/ski/elo/python", sport_dir, "polars/excel365", file_name)
  }

  if (!file.exists(file_path)) {
    stop(paste("Chrono file not found:", file_path))
  }

  chrono <- read.csv(file_path, stringsAsFactors = FALSE)

  # Standardize Date column
  if ("Date" %in% names(chrono)) {
    chrono$Date <- as.Date(chrono$Date)
  }

  # Ensure points column exists
  if (!"points" %in% names(chrono)) {
    # Create points from Place using standard WC points system
    wc_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26,
                   24, 22, 20, 18, 16, 15, 14, 13, 12, 11,
                   10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    chrono$points <- sapply(chrono$Place, function(p) {
      if (is.na(p) || p < 1 || p > 30) return(0)
      wc_points[p]
    })
  }

  return(chrono)
}

#' Get calibration races for a sport
#'
#' @param sport Sport name
#' @param chrono_data Chrono data (to filter to valid races)
#' @param season_range Vector of seasons to include
#' @param race_type Optional race type filter
#' @param min_field_size Minimum number of competitors
#' @return Data frame of races suitable for calibration
get_calibration_races <- function(sport, chrono_data,
                                   season_range = 2018:2025,
                                   race_type = NULL,
                                   min_field_size = 20) {
  # Filter out offseason/reset rows before grouping
  filtered_data <- chrono_data %>%
    filter(Season %in% season_range)

  # Normalize schema differences across sports for grouping/filtering.
  if (!"Distance" %in% names(filtered_data) && "HillSize" %in% names(filtered_data)) {
    filtered_data$Distance <- as.character(filtered_data$HillSize)
  } else if (!"Distance" %in% names(filtered_data)) {
    filtered_data$Distance <- NA_character_
  }
  if (!"Technique" %in% names(filtered_data)) {
    filtered_data$Technique <- NA_character_
  }
  if (!"Hill" %in% names(filtered_data) && "HillSize" %in% names(filtered_data)) {
    filtered_data$Hill <- dplyr::case_when(
      filtered_data$HillSize >= 185 ~ "Flying",
      filtered_data$HillSize >= 120 ~ "Large",
      filtered_data$HillSize >= 85 ~ "Normal",
      TRUE ~ "Other"
    )
  }
  if (!"MS" %in% names(filtered_data) && "MassStart" %in% names(filtered_data)) {
    filtered_data$MS <- filtered_data$MassStart
  }

  # Exclude offseason rows: Event = "Offseason", Distance = "0", Place = 0
  if ("Event" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>% filter(Event != "Offseason")
  }
  if ("Place" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>% filter(Place > 0)
  }
  # Exclude reset rows only if Distance exists for this sport.
  if ("Distance" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>% filter(!is.na(Distance), Distance != "0")
  }

  # Sport-specific grouping columns.
  group_cols <- switch(
    sport,
    "cross-country" = c("Date", "Sex", "Distance", "Technique", "City", "MS"),
    "alpine" = c("Date", "Sex", "Distance", "City"),
    "biathlon" = c("Date", "Sex", "RaceType", "Distance", "City", "MassStart"),
    "nordic-combined" = c("Date", "Sex", "RaceType", "Distance", "City", "MassStart"),
    "skijump" = c("Date", "Sex", "Hill", "HillSize", "RaceType", "City", "TeamEvent"),
    c("Date", "Sex", "Distance", "Technique", "City")
  )
  group_cols <- intersect(group_cols, names(filtered_data))

  races <- filtered_data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n_competitors = n(),
      .groups = "drop"
    ) %>%
    filter(n_competitors >= min_field_size)

  # Apply race type filter if specified
  if (!is.null(race_type)) {
    # Get the filter function from RACE_TYPES defined in param-grid.R
    race_types <- get_sport_race_types(sport)

    if (!is.null(race_types) && race_type %in% names(race_types)) {
      filter_func <- race_types[[race_type]]$filter
      if (!is.null(filter_func)) {
        # Apply the filter function row by row
        keep_rows <- vapply(seq_len(nrow(races)), function(i) {
          tryCatch({
            result <- filter_func(races[i, ])
            if (is.data.frame(result)) {
              result <- unlist(result, use.names = FALSE)
            } else if (is.list(result)) {
              result <- unlist(result, use.names = FALSE)
            }
            result <- as.logical(result)
            result <- result[!is.na(result)]
            if (length(result) == 0) FALSE else result[1]
          }, error = function(e) FALSE)
        }, logical(1))
        races <- races[keep_rows, ]
      }
    } else {
      # Fallback: try to parse race_type string (e.g., "Sprint_C" -> Distance="Sprint", Technique="C")
      parts <- strsplit(race_type, "_")[[1]]
      if (length(parts) >= 2) {
        distance_part <- parts[1]
        technique_part <- parts[2]

        if (distance_part == "Sprint") {
          races <- races %>% filter(Distance == "Sprint")
        } else if (distance_part == "Distance") {
          races <- races %>% filter(!Distance %in% c("Sprint", "Rel", "Ts"))
        }

        if (technique_part == "C") {
          races <- races %>% filter(Technique == "C")
        } else if (technique_part == "F") {
          races <- races %>% filter(Technique == "F")
        }

        # Check for mass start vs individual (third part)
        if (length(parts) >= 3) {
          if (parts[3] == "Ms") {
            races <- races %>% filter(grepl("Ms|Mass", Distance, ignore.case = TRUE) |
                                       grepl("Ms|Mass", Technique, ignore.case = TRUE))
          } else if (parts[3] == "Ind") {
            races <- races %>% filter(!grepl("Ms|Mass", Distance, ignore.case = TRUE) &
                                       !grepl("Ms|Mass", Technique, ignore.case = TRUE))
          }
        }
      }
    }
  }

  return(races)
}

# =============================================================================
# CONVENIENCE WRAPPERS
# =============================================================================

#' Quick evaluation of default parameters on a sport
#'
#' @param sport Sport name
#' @param gender "men" or "ladies"
#' @param n_races Number of races to sample (NULL = all)
#' @return Metrics list
quick_evaluate_defaults <- function(sport, gender = "men", n_races = 50) {
  # Load data
  chrono <- load_chrono_data(sport, gender)

  # Get calibration races
  races <- get_calibration_races(sport, chrono)

  # Sample if too many
  if (!is.null(n_races) && nrow(races) > n_races) {
    races <- races[sample(nrow(races), n_races), ]
  }

  # Default parameters
  default_params <- list(
    decay_lambda = 0.002,
    sd_scale_factor = 0.77,
    sd_min = 4,
    sd_max = 16,
    n_history_required = 10,
    gam_fill_weight_factor = 0.25
  )

  # Evaluate
  cat(sprintf("Evaluating %d races for %s %s...\n", nrow(races), gender, sport))
  metrics <- evaluate_params(default_params, races, chrono, n_simulations = 200)

  cat(sprintf("Results: %s\n", format_metrics(metrics)))

  return(metrics)
}

cat("backtest-engine.R loaded successfully\n")
