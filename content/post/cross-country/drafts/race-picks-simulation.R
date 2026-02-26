# Cross-Country Race Day Predictions: Unified Simulation-Based Approach
#
# This is a unified script that handles ALL race types for daily World Cup predictions:
# - Individual races (distance, sprint)
# - Relay (4-leg)
# - Team Sprint (2-leg)
# - Mixed Relay (4-leg)
#
# Uses Monte Carlo simulation (same methodology as champs-predictions-simulation.R):
# 1. Build a points distribution for each athlete (history + GAM samples)
# 2. Simulate N races by sampling from distributions and ranking
# 3. Count position frequencies to get probabilities naturally
#
# Benefits:
# - No normalization needed (exactly 1 winner, 3 podium per simulation)
# - Large fields don't unfairly penalize top athletes
# - Field size handled naturally
# - Unified codebase for all race types

library(dplyr)
library(tidyr)
library(openxlsx)
library(mgcv)
library(leaps)
library(logger)
library(purrr)
library(lubridate)

# ============================================================================
# CONFIGURATION
# ============================================================================

# ===== TEST MODE =====
# Set to TRUE to use test_races.csv for EDA/sandbox testing
TEST_MODE <- FALSE

# Simulation parameters
N_HISTORY_REQUIRED <- 10      # Target number of historical races per athlete
N_GAM_SAMPLES <- 0            # Number of GAM samples (equal total weight to history)
GAM_FILL_WEIGHT_FACTOR <- 0.25 # Weight multiplier for GAM-filled history slots
N_SIMULATIONS <- 1000         # Number of Monte Carlo simulations per race
DECAY_LAMBDA <- 0.002         # Exponential decay rate (0.002 = 50% weight after 1 year)

# Variance control parameters (calibrated from champs-predictions-simulation.R)
SD_SCALE_FACTOR <- 0.77       # Multiply all SDs by this (lower = more deterministic)
SD_MIN <- 4                   # Minimum SD (prevents degenerate distributions)
SD_MAX <- 16                  # Maximum SD (prevents too much randomness)

# Relay variance control (for hybrid approach)
RELAY_SCORE_SD_MIN <- 0.5     # Minimum score SD for relay simulation
RELAY_SCORE_SD_MAX <- 1.15    # Maximum score SD for relay simulation

# Team Sprint variance control (for hybrid approach)
TS_SCORE_SD_MIN <- 0.45       # Minimum score SD for team sprint simulation
TS_SCORE_SD_MAX <- 0.8        # Maximum score SD for team sprint simulation

# Calibration settings
RUN_CALIBRATION <- FALSE              # Calibrate individual race parameters
RUN_RELAY_CALIBRATION <- FALSE        # Calibrate relay parameters (4 legs)
RUN_TEAM_SPRINT_CALIBRATION <- FALSE  # Calibrate team sprint parameters (2 legs)
CALIBRATION_START_SEASON <- 2018      # Calibrate on races from this season onward

# Position thresholds to track
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)

# Relay data season cutoff
RELAY_SEASON_CUTOFF <- 2010

# Points systems
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,
               40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,
               10,9,8,7,6,5,4,3,2,1)
stage_points <- c(50,47,44,41,38,35,32,30,28,26,24,22,20,18,16,15,14,13,12,11,
                  10,9,8,7,6,5,4,3,2,1)
relay_points <- c(200, 160, 120, 100, 90, 80, 72, 64, 58, 52, 48, 44, 40, 36,
                  32, 30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10, 8, 6, 4, 2)

# ============================================================================
# LOGGING SETUP
# ============================================================================

log_dir <- "~/ski/elo/python/ski/polars/excel365/race-picks-simulation"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "race_picks_simulation.log")))
log_info("Starting Cross-Country race day predictions (UNIFIED SIMULATION approach)")
log_info(paste("Config: N_HISTORY =", N_HISTORY_REQUIRED,
               ", N_GAM_SAMPLES =", N_GAM_SAMPLES,
               ", N_SIMULATIONS =", N_SIMULATIONS,
               ", TEST_MODE =", TEST_MODE))

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

get_points <- function(place, points_list = wc_points) {
  if (is.na(place) || place < 1 || place > length(points_list)) {
    return(0)
  } else {
    return(points_list[place])
  }
}

replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Filter features to keep only those with positive coefficients
# Iteratively removes features with negative coefficients until all are positive
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

    log_info(paste("  Removing features with negative coefficients:", paste(negative_vars, collapse = ", ")))
    current_vars <- setdiff(current_vars, negative_vars)
  }

  if (length(current_vars) > 0) {
    log_info(paste("  Final positive-coefficient features:", paste(current_vars, collapse = ", ")))
  } else {
    log_warn("  No features with positive coefficients remaining")
  }

  return(current_vars)
}

# Function to get leg-specific explanatory variables
get_relay_explanatory_vars <- function(leg_number, n_legs = 4, technique = NULL) {
  base_vars <- c("prev_points_weighted", "Pelo_pct")

  if (n_legs == 4) {
    # 4-leg relay: legs 1-2 classic, legs 3-4 freestyle
    if (leg_number == 1) {
      return(c(base_vars, "Distance_Pelo_pct", "Distance_C_Pelo_pct",
               "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Classic_Pelo_pct"))
    } else if (leg_number == 2) {
      return(c(base_vars, "Distance_Pelo_pct", "Distance_C_Pelo_pct", "Classic_Pelo_pct"))
    } else if (leg_number == 3) {
      return(c(base_vars, "Distance_Pelo_pct", "Distance_F_Pelo_pct", "Freestyle_Pelo_pct"))
    } else if (leg_number == 4) {
      return(c(base_vars, "Distance_Pelo_pct", "Distance_F_Pelo_pct",
               "Sprint_Pelo_pct", "Sprint_F_Pelo_pct", "Freestyle_Pelo_pct"))
    }
  } else if (n_legs == 2) {
    # 2-leg team sprint: technique-specific features
    if (!is.null(technique) && technique == "C") {
      return(c(base_vars, "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Classic_Pelo_pct",
               "Distance_Pelo_pct", "Distance_C_Pelo_pct"))
    } else if (!is.null(technique) && technique == "F") {
      return(c(base_vars, "Sprint_Pelo_pct", "Sprint_F_Pelo_pct", "Freestyle_Pelo_pct",
               "Distance_Pelo_pct", "Distance_F_Pelo_pct"))
    } else {
      return(c(base_vars, "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Sprint_F_Pelo_pct",
               "Classic_Pelo_pct", "Freestyle_Pelo_pct", "Distance_Pelo_pct",
               "Distance_C_Pelo_pct", "Distance_F_Pelo_pct"))
    }
  }

  # Fallback: all variables
  return(c(base_vars, "Distance_Pelo_pct", "Distance_C_Pelo_pct", "Distance_F_Pelo_pct",
           "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Sprint_F_Pelo_pct",
           "Classic_Pelo_pct", "Freestyle_Pelo_pct"))
}

# ============================================================================
# RACE TYPE DEFINITIONS AND EXPLANATORY VARIABLES
# ============================================================================

# Race type definitions for individual races
race_types <- list(
  "Sprint_C" = list(filter = "Distance == 'Sprint' & Technique == 'C'", name = "Sprint Classic"),
  "Sprint_F" = list(filter = "Distance == 'Sprint' & Technique == 'F'", name = "Sprint Freestyle"),
  "Distance_C_Ind" = list(filter = "Distance != 'Sprint' & Technique == 'C' & MS == 0", name = "Distance Classic Individual"),
  "Distance_C_Ms" = list(filter = "Distance != 'Sprint' & Technique == 'C' & MS == 1", name = "Distance Classic Mass Start"),
  "Distance_F_Ind" = list(filter = "Distance != 'Sprint' & Technique == 'F' & MS == 0", name = "Distance Freestyle Individual"),
  "Distance_F_Ms" = list(filter = "Distance != 'Sprint' & Technique == 'F' & MS == 1", name = "Distance Freestyle Mass Start"),
  "Distance_Ind" = list(filter = "Distance != 'Sprint' & MS == 0", name = "Distance Individual"),
  "Distance_Ms" = list(filter = "Technique == 'P'", name = "Skiathlon")
)

# Get technique-dependent explanatory variables for individual races
get_explanatory_vars <- function(race_type_key) {
  base_vars <- c("prev_points_weighted", "Pelo_pct")

  if (grepl("Sprint_C", race_type_key)) {
    return(c(base_vars, "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Classic_Pelo_pct"))
  } else if (grepl("Sprint_F", race_type_key)) {
    return(c(base_vars, "Sprint_Pelo_pct", "Sprint_F_Pelo_pct", "Freestyle_Pelo_pct"))
  } else if (grepl("Distance.*_C", race_type_key)) {
    return(c(base_vars, "Distance_Pelo_pct", "Distance_C_Pelo_pct", "Classic_Pelo_pct"))
  } else if (grepl("Distance.*_F", race_type_key)) {
    return(c(base_vars, "Distance_Pelo_pct", "Distance_F_Pelo_pct", "Freestyle_Pelo_pct"))
  } else {
    return(c(base_vars, "Distance_Pelo_pct", "Distance_C_Pelo_pct", "Distance_F_Pelo_pct",
             "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Sprint_F_Pelo_pct",
             "Classic_Pelo_pct", "Freestyle_Pelo_pct"))
  }
}

# Determine race type key from race characteristics
determine_race_type_key <- function(distance, technique, ms = 0) {
  if (distance == "Sprint" && technique == "C") return("Sprint_C")
  if (distance == "Sprint" && technique == "F") return("Sprint_F")
  if (distance != "Sprint" && technique == "C" && ms == 1) return("Distance_C_Ms")
  if (distance != "Sprint" && technique == "C" && ms == 0) return("Distance_C_Ind")
  if (distance != "Sprint" && technique == "F" && ms == 1) return("Distance_F_Ms")
  if (distance != "Sprint" && technique == "F" && ms == 0) return("Distance_F_Ind")
  if (distance != "Sprint" && technique == "P") return("Distance_Ms")
  if (distance != "Sprint" && ms == 1) return("Distance_Ms")
  if (distance != "Sprint" && ms == 0) return("Distance_Ind")
  return(NA)
}

# ============================================================================
# DATA PREPROCESSING FUNCTIONS
# ============================================================================

# Calculate exponential decay weighted prev_points
calculate_weighted_prev_points <- function(chrono_data, decay_lambda = DECAY_LAMBDA) {
  log_info("Calculating weighted prev_points with exponential decay...")

  chrono_data %>%
    arrange(ID, Date) %>%
    group_by(ID) %>%
    mutate(
      prev_points_weighted = sapply(row_number(), function(i) {
        if (i == 1) return(0)

        prev_data <- cur_data()[1:(i-1), ]
        if (nrow(prev_data) == 0) return(0)

        prev_points_values <- prev_data$points
        prev_dates <- prev_data$Date
        prev_distances <- prev_data$Distance
        prev_techniques <- prev_data$Technique

        current_date <- cur_data()$Date[i]
        current_distance <- cur_data()$Distance[i]
        current_technique <- cur_data()$Technique[i]

        # Match by race type
        if (current_distance == "Sprint" && current_technique == "C") {
          matching <- prev_distances == "Sprint" & prev_techniques == "C"
        } else if (current_distance == "Sprint" && current_technique == "F") {
          matching <- prev_distances == "Sprint" & prev_techniques == "F"
        } else if (current_distance != "Sprint" && current_technique == "C") {
          matching <- prev_distances != "Sprint" & prev_techniques == "C"
        } else if (current_distance != "Sprint" && current_technique == "F") {
          matching <- prev_distances != "Sprint" & prev_techniques == "F"
        } else if (current_distance != "Sprint") {
          matching <- prev_distances != "Sprint"
        } else {
          return(0)
        }

        matching_points <- prev_points_values[matching]
        matching_dates <- prev_dates[matching]
        if (length(matching_points) == 0) return(0)

        # Calculate exponential decay weights
        days_ago <- as.numeric(difftime(current_date, matching_dates, units = "days"))
        weights <- exp(-decay_lambda * days_ago)

        weighted.mean(matching_points, weights, na.rm = TRUE)
      })
    ) %>%
    ungroup()
}

# Calculate PELO percentage columns (normalized within each race)
calculate_percentage_columns <- function(chrono_data) {
  chrono_data %>%
    group_by(Season, Race) %>%
    mutate(
      Pelo_pct = Pelo / max(Pelo, na.rm = TRUE),
      Distance_Pelo_pct = Distance_Pelo / max(Distance_Pelo, na.rm = TRUE),
      Distance_C_Pelo_pct = Distance_C_Pelo / max(Distance_C_Pelo, na.rm = TRUE),
      Distance_F_Pelo_pct = Distance_F_Pelo / max(Distance_F_Pelo, na.rm = TRUE),
      Sprint_Pelo_pct = Sprint_Pelo / max(Sprint_Pelo, na.rm = TRUE),
      Sprint_C_Pelo_pct = Sprint_C_Pelo / max(Sprint_C_Pelo, na.rm = TRUE),
      Sprint_F_Pelo_pct = Sprint_F_Pelo / max(Sprint_F_Pelo, na.rm = TRUE),
      Classic_Pelo_pct = Classic_Pelo / max(Classic_Pelo, na.rm = TRUE),
      Freestyle_Pelo_pct = Freestyle_Pelo / max(Freestyle_Pelo, na.rm = TRUE)
    ) %>%
    ungroup()
}

# ============================================================================
# GAM MODEL TRAINING
# ============================================================================

# Train GAM for POINTS prediction
train_points_gam <- function(chrono_data, race_type_key, gender) {
  race_info <- race_types[[race_type_key]]
  if (is.null(race_info)) {
    log_warn(paste("Unknown race type:", race_type_key))
    return(NULL)
  }

  log_info(paste("Training", gender, race_info$name, "POINTS GAM"))

  filtered_data <- chrono_data %>%
    filter(eval(parse(text = race_info$filter)))

  log_info(paste("Filtered to", nrow(filtered_data), "records"))

  if (nrow(filtered_data) < 50) {
    log_warn(paste("Insufficient data for", race_info$name))
    return(NULL)
  }

  explanatory_vars <- get_explanatory_vars(race_type_key)

  tryCatch({
    # Feature selection via BIC
    formula <- as.formula(paste("points ~", paste(explanatory_vars, collapse = " + ")))
    feature_selection <- regsubsets(formula, data = filtered_data, nbest = 1, method = "exhaustive")
    feature_summary <- summary(feature_selection)
    best_bic_vars <- names(coef(feature_selection, which.min(feature_summary$bic)))[-1]

    log_info(paste("BIC selected features:", paste(best_bic_vars, collapse = ", ")))

    # Filter to keep only features with positive coefficients
    positive_vars <- filter_positive_coefficients(filtered_data, "points", best_bic_vars, family = "gaussian")

    if (length(positive_vars) == 0) {
      log_warn("No positive coefficient features - using prev_points_weighted only")
      positive_vars <- "prev_points_weighted"
    }

    # GAM for points
    smooth_terms <- paste("s(", positive_vars, ")", collapse = " + ")
    gam_formula <- as.formula(paste("points ~", smooth_terms))

    points_model <- gam(gam_formula, data = filtered_data, method = "REML")

    # Get residual SD for distribution building
    residual_sd <- sqrt(points_model$sig2)
    if (is.null(residual_sd) || is.na(residual_sd)) {
      residual_sd <- sqrt(points_model$deviance / points_model$df.residual)
    }
    residual_sd <- max(residual_sd, 5)

    log_info(paste("GAM trained. Residual SD:", round(residual_sd, 2)))
    log_info(paste("Final features:", paste(positive_vars, collapse = ", ")))

    return(list(
      model = points_model,
      residual_sd = residual_sd,
      features = positive_vars,
      race_type = race_type_key
    ))

  }, error = function(e) {
    log_error(paste("Error training GAM:", e$message))
    return(NULL)
  })
}

# ============================================================================
# SIMULATION FUNCTIONS
# ============================================================================

# Build athlete distribution combining history + GAM samples
build_athlete_distribution <- function(athlete_id, race_type_key, chrono_data,
                                        gam_prediction, gam_residual_sd,
                                        n_history = N_HISTORY_REQUIRED,
                                        n_gam_samples = N_GAM_SAMPLES,
                                        gam_fill_weight_factor = GAM_FILL_WEIGHT_FACTOR,
                                        decay_lambda = DECAY_LAMBDA,
                                        reference_date = NULL) {

  # Define race filter
  race_filter <- switch(race_type_key,
    "Sprint_C" = quote(Distance == "Sprint" & Technique == "C"),
    "Sprint_F" = quote(Distance == "Sprint" & Technique == "F"),
    "Distance_C_Ind" = quote(Distance != "Sprint" & Technique == "C" & MS == 0),
    "Distance_C_Ms" = quote(Distance != "Sprint" & Technique == "C" & MS == 1),
    "Distance_F_Ind" = quote(Distance != "Sprint" & Technique == "F" & MS == 0),
    "Distance_F_Ms" = quote(Distance != "Sprint" & Technique == "F" & MS == 1),
    "Distance_Ind" = quote(Distance != "Sprint" & MS == 0),
    "Distance_Ms" = quote(Distance != "Sprint" & MS == 1),
    quote(TRUE)
  )

  # Get athlete's historical results (most recent first)
  athlete_history <- chrono_data %>%
    filter(ID == athlete_id, City != "Summer", City != "Tour De Ski", eval(race_filter)) %>%
    arrange(desc(Date)) %>%
    head(n_history)

  n_actual_races <- nrow(athlete_history)

  all_points <- c()
  all_weights <- c()

  # Part 1: Historical races with exponential decay weighting
  if (n_actual_races > 0) {
    history_points <- sapply(athlete_history$Place, function(p) get_points(p, wc_points))

    if (is.null(reference_date)) {
      reference_date <- max(chrono_data$Date, na.rm = TRUE)
    }

    days_ago <- as.numeric(reference_date - athlete_history$Date)
    history_weights <- exp(-decay_lambda * days_ago)

    all_points <- c(all_points, history_points)
    all_weights <- c(all_weights, history_weights)
  }

  # Part 2: GAM fill for missing history
  n_missing_history <- n_history - n_actual_races

  if (n_missing_history > 0) {
    gam_fill_points <- rnorm(n_missing_history, mean = gam_prediction, sd = gam_residual_sd)
    gam_fill_points <- pmax(0, pmin(100, gam_fill_points))

    if (n_actual_races > 0) {
      median_weight <- median(all_weights) * gam_fill_weight_factor
    } else {
      median_weight <- exp(-decay_lambda * 365) * gam_fill_weight_factor
    }
    gam_fill_weights <- rep(median_weight, n_missing_history)

    all_points <- c(all_points, gam_fill_points)
    all_weights <- c(all_weights, gam_fill_weights)
  }

  # Part 3: GAM samples spanning variance
  if (n_gam_samples > 0) {
    total_history_weight <- sum(all_weights)
    gam_sample_weight <- total_history_weight / n_gam_samples

    gam_samples <- rnorm(n_gam_samples, mean = gam_prediction, sd = gam_residual_sd)
    gam_samples <- pmax(0, pmin(100, gam_samples))

    gam_sample_weights <- rep(gam_sample_weight, n_gam_samples)

    all_points <- c(all_points, gam_samples)
    all_weights <- c(all_weights, gam_sample_weights)
  }

  # Calculate distribution parameters
  weighted_mean <- weighted.mean(all_points, all_weights, na.rm = TRUE)
  weighted_var <- sum(all_weights * (all_points - weighted_mean)^2) / sum(all_weights)
  weighted_sd <- sqrt(weighted_var)
  weighted_sd <- max(weighted_sd, 5)

  if (is.na(weighted_mean) || is.na(weighted_sd)) {
    log_warn(paste("Invalid distribution for athlete:", athlete_id))
  }

  return(list(
    athlete_id = athlete_id,
    mean = weighted_mean,
    sd = weighted_sd,
    n_actual_races = n_actual_races,
    n_gam_fill = n_missing_history
  ))
}

# Simulate race positions using Monte Carlo
simulate_race_positions <- function(athlete_distributions, n_simulations = N_SIMULATIONS,
                                     position_thresholds = POSITION_THRESHOLDS,
                                     sd_scale_factor = SD_SCALE_FACTOR,
                                     sd_min = SD_MIN, sd_max = SD_MAX) {

  n_athletes <- length(athlete_distributions)
  athlete_ids <- sapply(athlete_distributions, function(x) x$athlete_id)

  # Initialize position counts
  position_counts <- matrix(0, nrow = n_athletes, ncol = length(position_thresholds),
                            dimnames = list(athlete_ids, paste0("top_", position_thresholds)))

  # Run simulations
  for (sim in 1:n_simulations) {
    # Sample points from each athlete's distribution
    simulated_points <- sapply(athlete_distributions, function(dist) {
      if (is.null(dist$mean) || is.na(dist$mean) || is.null(dist$sd) || is.na(dist$sd)) {
        return(0)
      }
      scaled_sd <- dist$sd * sd_scale_factor
      bounded_sd <- pmax(sd_min, pmin(sd_max, scaled_sd))
      rnorm(1, mean = dist$mean, sd = bounded_sd)
    })
    simulated_points <- pmax(0, pmin(100, simulated_points))

    # Rank (higher points = better = lower rank)
    ranks <- rank(-simulated_points, ties.method = "random")

    # Count positions
    for (t_idx in seq_along(position_thresholds)) {
      threshold <- position_thresholds[t_idx]
      achieved <- ranks <= threshold
      position_counts[achieved, t_idx] <- position_counts[achieved, t_idx] + 1
    }
  }

  # Convert to probabilities
  position_probs <- position_counts / n_simulations

  # Build results dataframe
  results <- data.frame(
    athlete_id = athlete_ids,
    mean_points = sapply(athlete_distributions, function(x) x$mean),
    sd_points = sapply(athlete_distributions, function(x) x$sd),
    n_actual_races = sapply(athlete_distributions, function(x) x$n_actual_races),
    stringsAsFactors = FALSE
  )

  # Add probability columns
  for (t_idx in seq_along(position_thresholds)) {
    col_name <- paste0("prob_top_", position_thresholds[t_idx])
    results[[col_name]] <- position_probs[, t_idx]
  }

  results <- results %>% arrange(desc(mean_points))
  return(results)
}

# ============================================================================
# RELAY/TEAM SPRINT HELPER FUNCTIONS
# ============================================================================

# Get race-specific prev_points_weighted using exponential decay
get_race_prev_points <- function(chrono_data, athlete_id, race_type, reference_date = NULL) {
  if (race_type == "Sprint_C" || race_type == "Sprint_Classic") {
    races <- chrono_data %>% filter(ID == athlete_id, Distance == "Sprint", Technique == "C")
  } else if (race_type == "Sprint_F" || race_type == "Sprint_Freestyle") {
    races <- chrono_data %>% filter(ID == athlete_id, Distance == "Sprint", Technique == "F")
  } else if (race_type == "Sprint") {
    races <- chrono_data %>% filter(ID == athlete_id, Distance == "Sprint")
  } else if (grepl("Distance_C", race_type)) {
    races <- chrono_data %>% filter(ID == athlete_id, Distance != "Sprint", Technique == "C")
  } else if (grepl("Distance_F", race_type)) {
    races <- chrono_data %>% filter(ID == athlete_id, Distance != "Sprint", Technique == "F")
  } else {
    races <- chrono_data %>% filter(ID == athlete_id, Distance != "Sprint")
  }

  if (nrow(races) == 0) return(0)

  if (is.null(reference_date)) {
    reference_date <- max(chrono_data$Date, na.rm = TRUE)
  }

  races <- races %>%
    arrange(desc(Date)) %>%
    mutate(
      days_ago = as.numeric(difftime(reference_date, Date, units = "days")),
      weight = exp(-DECAY_LAMBDA * days_ago)
    )

  weighted.mean(races$points, races$weight, na.rm = TRUE)
}

# Calculate leg importance based on model deviance explained
calculate_leg_importance_from_models <- function(relay_chrono, individual_chrono, n_legs = 4,
                                                   event_type = "Relay", technique = NULL) {
  if (is.null(relay_chrono) || nrow(relay_chrono) == 0 || !"Leg" %in% names(relay_chrono)) {
    log_warn(paste("No leg data available for", event_type, "- using equal weights"))
    return(rep(1/n_legs, n_legs))
  }

  # Calculate prev_points_weighted from individual chrono
  if (!is.null(individual_chrono) && nrow(individual_chrono) > 0 && "ID" %in% names(relay_chrono)) {
    log_info(paste("Calculating prev_points_weighted for", event_type))

    calc_exp_decay_prev_points <- function(athlete_id, reference_date, chrono_subset) {
      athlete_races <- chrono_subset %>%
        filter(ID == athlete_id, Date < reference_date) %>%
        arrange(desc(Date))

      if (nrow(athlete_races) == 0) return(0)

      days_ago <- as.numeric(difftime(reference_date, athlete_races$Date, units = "days"))
      weights <- exp(-DECAY_LAMBDA * days_ago)

      weighted.mean(athlete_races$points, weights, na.rm = TRUE)
    }

    classic_chrono <- individual_chrono %>% filter(Distance != "Sprint", Technique == "C")
    freestyle_chrono <- individual_chrono %>% filter(Distance != "Sprint", Technique == "F")
    sprint_chrono <- individual_chrono %>% filter(Distance == "Sprint")

    relay_chrono$prev_points_weighted <- sapply(1:nrow(relay_chrono), function(i) {
      row <- relay_chrono[i, ]
      if (n_legs == 2) {
        calc_exp_decay_prev_points(row$ID, row$Date, sprint_chrono)
      } else if (row$Leg %in% c(1, 2)) {
        calc_exp_decay_prev_points(row$ID, row$Date, classic_chrono)
      } else {
        calc_exp_decay_prev_points(row$ID, row$Date, freestyle_chrono)
      }
    })
  } else {
    relay_chrono$prev_points_weighted <- 25
  }

  # Calculate PELO_pct columns if not present
  pelo_cols <- c("Pelo", "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                 "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo", "Classic_Pelo", "Freestyle_Pelo")

  for (col in pelo_cols) {
    pct_col <- paste0(col, "_pct")
    if (col %in% names(relay_chrono) && !pct_col %in% names(relay_chrono)) {
      relay_chrono <- relay_chrono %>%
        group_by(Season, Race) %>%
        mutate(!!pct_col := {
          max_val <- max(.data[[col]], na.rm = TRUE)
          if (is.na(max_val) || max_val == 0) rep(0.5, n()) else .data[[col]] / max_val
        }) %>%
        ungroup()
    }
  }

  leg_data <- relay_chrono %>%
    filter(!is.na(Leg), Leg >= 1, Leg <= n_legs, !is.na(Place), Place > 0)

  if (nrow(leg_data) < 50) {
    log_warn(paste("Insufficient leg data for", event_type, "- using equal weights"))
    return(rep(1/n_legs, n_legs))
  }

  log_info(paste("Training leg-specific models for", event_type, "with", nrow(leg_data), "records"))

  leg_deviances <- numeric(n_legs)

  for (leg in 1:n_legs) {
    leg_subset <- leg_data %>% filter(Leg == leg)

    if (nrow(leg_subset) < 20) {
      leg_deviances[leg] <- 0
      next
    }

    leg_subset <- leg_subset %>%
      mutate(podium = as.numeric(Place <= 3))

    explanatory_vars <- get_relay_explanatory_vars(leg, n_legs, technique)
    available_vars <- explanatory_vars[explanatory_vars %in% names(leg_subset)]

    if (length(available_vars) < 2) {
      leg_deviances[leg] <- 0
      next
    }

    for (var in available_vars) {
      leg_subset[[var]] <- replace_na_with_quartile(leg_subset[[var]])
    }

    positive_vars <- filter_positive_coefficients(leg_subset, "podium", available_vars, family = "binomial")

    if (length(positive_vars) == 0) {
      positive_vars <- "prev_points_weighted"
    }

    leg_model <- NULL
    tryCatch({
      smooth_terms <- paste("s(", positive_vars, ", k=3)", collapse = " + ")
      gam_formula <- as.formula(paste("podium ~", smooth_terms))
      leg_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
    }, error = function(e) NULL)

    if (is.null(leg_model)) {
      tryCatch({
        linear_terms <- paste(positive_vars, collapse = " + ")
        gam_formula <- as.formula(paste("podium ~", linear_terms))
        leg_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
      }, error = function(e) NULL)
    }

    if (!is.null(leg_model)) {
      leg_deviances[leg] <- summary(leg_model)$dev.expl
      log_info(paste("  Leg", leg, ": deviance explained =", round(leg_deviances[leg] * 100, 1), "%"))
    }
  }

  total_deviance <- sum(leg_deviances, na.rm = TRUE)

  if (total_deviance > 0) {
    importance <- leg_deviances / total_deviance
  } else {
    importance <- rep(1/n_legs, n_legs)
  }

  importance <- importance / sum(importance)

  log_info(paste(event_type, "leg importance weights:"))
  for (i in 1:n_legs) {
    log_info(paste("  Leg", i, ":", round(importance[i] * 100, 1), "%"))
  }

  return(importance)
}

# Train leg-specific binomial GAMs for relay simulation
train_relay_leg_models_for_simulation <- function(relay_chrono, individual_chrono, n_legs = 4,
                                                    gender = "men", technique = NULL) {
  if (is.null(relay_chrono) || nrow(relay_chrono) == 0 || !"Leg" %in% names(relay_chrono)) {
    log_warn(paste("No relay data for", gender, "leg-specific models"))
    return(NULL)
  }

  log_info(paste("Training", gender, "leg-specific models for simulation"))

  # Calculate prev_points_weighted
  if (!is.null(individual_chrono) && nrow(individual_chrono) > 0 && "ID" %in% names(relay_chrono)) {
    calc_exp_decay_prev_points <- function(athlete_id, reference_date, chrono_subset) {
      athlete_races <- chrono_subset %>%
        filter(ID == athlete_id, Date < reference_date) %>%
        arrange(desc(Date))

      if (nrow(athlete_races) == 0) return(0)
      days_ago <- as.numeric(difftime(reference_date, athlete_races$Date, units = "days"))
      weights <- exp(-DECAY_LAMBDA * days_ago)
      weighted.mean(athlete_races$points, weights, na.rm = TRUE)
    }

    if (n_legs == 4) {
      classic_chrono <- individual_chrono %>% filter(Distance != "Sprint", Technique == "C")
      freestyle_chrono <- individual_chrono %>% filter(Distance != "Sprint", Technique == "F")

      relay_chrono$prev_points_weighted <- sapply(1:nrow(relay_chrono), function(i) {
        row <- relay_chrono[i, ]
        if (row$Leg %in% c(1, 2)) {
          calc_exp_decay_prev_points(row$ID, row$Date, classic_chrono)
        } else {
          calc_exp_decay_prev_points(row$ID, row$Date, freestyle_chrono)
        }
      })
    } else if (n_legs == 2 && !is.null(technique)) {
      sprint_chrono <- individual_chrono %>% filter(Distance == "Sprint", Technique == technique)

      relay_chrono$prev_points_weighted <- sapply(1:nrow(relay_chrono), function(i) {
        row <- relay_chrono[i, ]
        calc_exp_decay_prev_points(row$ID, row$Date, sprint_chrono)
      })
    }
  }

  # Calculate PELO_pct columns
  pelo_cols <- c("Pelo", "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                 "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo", "Classic_Pelo", "Freestyle_Pelo")

  for (col in pelo_cols) {
    pct_col <- paste0(col, "_pct")
    if (col %in% names(relay_chrono) && !pct_col %in% names(relay_chrono)) {
      relay_chrono <- relay_chrono %>%
        group_by(Season, Race) %>%
        mutate(!!pct_col := {
          max_val <- max(.data[[col]], na.rm = TRUE)
          if (is.na(max_val) || max_val == 0) rep(0.5, n()) else .data[[col]] / max_val
        }) %>%
        ungroup()
    }
  }

  relay_data <- relay_chrono %>%
    filter(!is.na(Leg), Leg >= 1, Leg <= n_legs, !is.na(Place), Place > 0)

  if (nrow(relay_data) < 50) {
    log_warn(paste("Insufficient data for", gender, "leg-specific models"))
    return(NULL)
  }

  leg_models <- list()

  for (leg in 1:n_legs) {
    leg_subset <- relay_data %>% filter(Leg == leg)

    if (nrow(leg_subset) < 30) {
      next
    }

    leg_subset <- leg_subset %>%
      mutate(is_podium = as.numeric(Place <= 3), is_win = as.numeric(Place == 1))

    explanatory_vars <- get_relay_explanatory_vars(leg, n_legs, technique)
    available_vars <- explanatory_vars[explanatory_vars %in% names(leg_subset)]

    if (length(available_vars) < 2) {
      next
    }

    for (var in available_vars) {
      leg_subset[[var]] <- replace_na_with_quartile(leg_subset[[var]])
    }

    positive_vars <- filter_positive_coefficients(leg_subset, "is_podium", available_vars, family = "binomial")

    if (length(positive_vars) == 0) {
      positive_vars <- "prev_points_weighted"
    }

    podium_model <- NULL
    win_model <- NULL

    tryCatch({
      smooth_terms <- paste("s(", positive_vars, ", k=3)", collapse = " + ")
      gam_formula <- as.formula(paste("is_podium ~", smooth_terms))
      podium_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
    }, error = function(e) NULL)

    if (is.null(podium_model)) {
      tryCatch({
        linear_terms <- paste(positive_vars, collapse = " + ")
        gam_formula <- as.formula(paste("is_podium ~", linear_terms))
        podium_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
      }, error = function(e) NULL)
    }

    tryCatch({
      smooth_terms <- paste("s(", positive_vars, ", k=3)", collapse = " + ")
      gam_formula <- as.formula(paste("is_win ~", smooth_terms))
      win_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
    }, error = function(e) NULL)

    if (is.null(win_model)) {
      tryCatch({
        linear_terms <- paste(positive_vars, collapse = " + ")
        gam_formula <- as.formula(paste("is_win ~", linear_terms))
        win_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
      }, error = function(e) NULL)
    }

    if (!is.null(podium_model)) {
      dev_explained <- summary(podium_model)$dev.expl
      log_info(paste("  Leg", leg, ": n=", nrow(leg_subset), ", features=", length(positive_vars),
                     ", deviance=", round(dev_explained * 100, 1), "%"))

      leg_models[[paste0("leg_", leg)]] <- list(
        podium_model = podium_model,
        win_model = win_model,
        model = podium_model,
        features = positive_vars,
        deviance_explained = dev_explained,
        n_training = nrow(leg_subset)
      )
    }
  }

  if (length(leg_models) == 0) {
    log_warn(paste("No successful leg models for", gender))
    return(NULL)
  }

  log_info(paste("Trained", length(leg_models), "leg-specific models for", gender))

  return(list(models = leg_models, n_legs = n_legs, gender = gender))
}

# Calculate team points for a given team assignment
calculate_team_points <- function(team_members, leg_models, chrono_data, n_members,
                                   leg_importance = NULL, technique = NULL, opt_type = "podium") {
  if (is.null(team_members) || nrow(team_members) < n_members) {
    return(list(total_points = 0, weighted_points = 0, member_points = rep(0, n_members)))
  }

  if (is.null(leg_importance) || length(leg_importance) != n_members) {
    leg_importance <- rep(1/n_members, n_members)
  }

  member_probs <- numeric(n_members)

  for (i in 1:n_members) {
    member <- team_members[i, , drop = FALSE]
    member_id <- member$ID
    leg_key <- paste0("leg_", i)

    if (n_members == 4) {
      if (i <= 2) {
        prev_pts <- get_race_prev_points(chrono_data, member_id, "Distance_C")
      } else {
        prev_pts <- get_race_prev_points(chrono_data, member_id, "Distance_F")
      }
    } else {
      if (!is.null(technique) && technique == "C") {
        prev_pts <- get_race_prev_points(chrono_data, member_id, "Sprint_C")
      } else if (!is.null(technique) && technique == "F") {
        prev_pts <- get_race_prev_points(chrono_data, member_id, "Sprint_F")
      } else {
        prev_pts <- get_race_prev_points(chrono_data, member_id, "Sprint")
      }
    }
    if (is.na(prev_pts) || prev_pts == 0) prev_pts <- 25
    member$prev_points_weighted <- prev_pts

    if (!is.null(leg_models) && leg_key %in% names(leg_models$models)) {
      if (opt_type == "win" && !is.null(leg_models$models[[leg_key]]$win_model)) {
        leg_model <- leg_models$models[[leg_key]]$win_model
      } else {
        leg_model <- leg_models$models[[leg_key]]$podium_model
        if (is.null(leg_model)) {
          leg_model <- leg_models$models[[leg_key]]$model
        }
      }
      pred <- tryCatch({
        p <- predict(leg_model, newdata = member, type = "response")
        pmax(0.01, pmin(0.99, p))
      }, error = function(e) {
        pmax(0.01, pmin(0.8, prev_pts / 100))
      })
    } else {
      pred <- pmax(0.01, pmin(0.8, prev_pts / 100))
    }

    member_probs[i] <- as.numeric(pred)
  }

  weighted_score <- sum(member_probs * leg_importance, na.rm = TRUE)
  total_score <- sum(member_probs, na.rm = TRUE)

  return(list(total_points = total_score, weighted_points = weighted_score, member_points = member_probs))
}

# Select best team for a nation by optimizing predicted score
select_relay_team <- function(nation_athletes, n_members = 4, leg_models = NULL, chrono_data = NULL,
                               optimize = TRUE, leg_importance = NULL, technique = NULL) {
  n_athletes <- nrow(nation_athletes)

  if (n_athletes < n_members) {
    return(NULL)
  }

  if (is.null(leg_importance) || length(leg_importance) != n_members) {
    leg_importance <- rep(1/n_members, n_members)
  }

  if (!optimize || is.null(leg_models) || length(leg_models$models) == 0) {
    if (n_members == 4) {
      classic_athletes <- nation_athletes %>%
        arrange(desc(Classic_Elo)) %>%
        head(2)
      freestyle_athletes <- nation_athletes %>%
        filter(!ID %in% classic_athletes$ID) %>%
        arrange(desc(Freestyle_Elo)) %>%
        head(2)
      fallback_team <- bind_rows(classic_athletes, freestyle_athletes)
    } else {
      if (!is.null(technique) && technique == "C") {
        fallback_team <- nation_athletes %>%
          arrange(desc(Classic_Elo)) %>%
          head(2)
      } else {
        fallback_team <- nation_athletes %>%
          arrange(desc(Freestyle_Elo)) %>%
          head(2)
      }
    }
    return(list(
      podium_team = fallback_team,
      win_team = fallback_team,
      podium_member_points = rep(0.5, n_members),
      win_member_points = rep(0.5, n_members),
      podium_weighted_points = 0.5,
      win_weighted_points = 0.5
    ))
  }

  best_podium_weighted <- 0
  best_podium_team <- NULL
  best_podium_member_points <- NULL

  best_win_weighted <- 0
  best_win_team <- NULL
  best_win_member_points <- NULL

  if (n_members == 4) {
    if (n_athletes > 8) {
      nation_athletes <- nation_athletes %>%
        mutate(combined_elo = Classic_Elo + Freestyle_Elo) %>%
        arrange(desc(combined_elo)) %>%
        head(8)
      n_athletes <- 8
    }

    for (leg1 in 1:n_athletes) {
      for (leg2 in setdiff(1:n_athletes, leg1)) {
        for (leg3 in setdiff(1:n_athletes, c(leg1, leg2))) {
          for (leg4 in setdiff(1:n_athletes, c(leg1, leg2, leg3))) {
            team <- nation_athletes[c(leg1, leg2, leg3, leg4), ]

            podium_result <- calculate_team_points(team, leg_models, chrono_data,
                                                    n_members, leg_importance, technique, opt_type = "podium")
            if (podium_result$weighted_points > best_podium_weighted) {
              best_podium_weighted <- podium_result$weighted_points
              best_podium_team <- team
              best_podium_member_points <- podium_result$member_points
            }

            win_result <- calculate_team_points(team, leg_models, chrono_data,
                                                 n_members, leg_importance, technique, opt_type = "win")
            if (win_result$weighted_points > best_win_weighted) {
              best_win_weighted <- win_result$weighted_points
              best_win_team <- team
              best_win_member_points <- win_result$member_points
            }
          }
        }
      }
    }
  } else {
    for (leg1 in 1:n_athletes) {
      for (leg2 in setdiff(1:n_athletes, leg1)) {
        team <- nation_athletes[c(leg1, leg2), ]

        podium_result <- calculate_team_points(team, leg_models, chrono_data,
                                                n_members, leg_importance, technique, opt_type = "podium")
        if (podium_result$weighted_points > best_podium_weighted) {
          best_podium_weighted <- podium_result$weighted_points
          best_podium_team <- team
          best_podium_member_points <- podium_result$member_points
        }

        win_result <- calculate_team_points(team, leg_models, chrono_data,
                                             n_members, leg_importance, technique, opt_type = "win")
        if (win_result$weighted_points > best_win_weighted) {
          best_win_weighted <- win_result$weighted_points
          best_win_team <- team
          best_win_member_points <- win_result$member_points
        }
      }
    }
  }

  return(list(
    podium_team = best_podium_team,
    win_team = best_win_team,
    podium_member_points = best_podium_member_points,
    win_member_points = best_win_member_points,
    podium_weighted_points = best_podium_weighted,
    win_weighted_points = best_win_weighted,
    leg_importance = leg_importance
  ))
}

# Build team distribution using leg-specific models (hybrid approach)
build_team_distribution_hybrid <- function(team_members, leg_models, leg_importance,
                                            chrono_data = NULL, n_members = 4, technique = NULL) {

  if (is.null(team_members) || nrow(team_members) < n_members) {
    return(NULL)
  }

  team_members_prepared <- team_members

  if (!is.null(chrono_data) && "ID" %in% names(team_members_prepared)) {
    for (i in 1:nrow(team_members_prepared)) {
      member_id <- team_members_prepared$ID[i]
      if (n_members == 4) {
        if (i <= 2) {
          team_members_prepared$prev_points_weighted[i] <- get_race_prev_points(chrono_data, member_id, "Distance_C")
        } else {
          team_members_prepared$prev_points_weighted[i] <- get_race_prev_points(chrono_data, member_id, "Distance_F")
        }
      } else {
        if (!is.null(technique) && technique == "C") {
          team_members_prepared$prev_points_weighted[i] <- get_race_prev_points(chrono_data, member_id, "Sprint_C")
        } else if (!is.null(technique) && technique == "F") {
          team_members_prepared$prev_points_weighted[i] <- get_race_prev_points(chrono_data, member_id, "Sprint_F")
        } else {
          team_members_prepared$prev_points_weighted[i] <- get_race_prev_points(chrono_data, member_id, "Sprint")
        }
      }
    }
    valid_pts <- team_members_prepared$prev_points_weighted[team_members_prepared$prev_points_weighted > 0]
    q1 <- if (length(valid_pts) > 0) quantile(valid_pts, 0.25, na.rm = TRUE) else 25
    team_members_prepared$prev_points_weighted[is.na(team_members_prepared$prev_points_weighted) |
                                                team_members_prepared$prev_points_weighted == 0] <- q1
  }

  if (is.null(leg_importance) || length(leg_importance) != n_members) {
    leg_importance <- rep(1/n_members, n_members)
  }

  leg_probs <- numeric(n_members)
  leg_sds <- numeric(n_members)

  for (i in 1:n_members) {
    member <- team_members_prepared[i, , drop = FALSE]
    leg_key <- paste0("leg_", i)

    if (!is.null(leg_models) && leg_key %in% names(leg_models$models)) {
      leg_model <- leg_models$models[[leg_key]]$model

      pred_prob <- tryCatch({
        p <- predict(leg_model, newdata = member, type = "response")
        pmax(0.01, pmin(0.99, p))
      }, error = function(e) {
        if ("prev_points_weighted" %in% names(member)) {
          pmax(0.01, pmin(0.8, member$prev_points_weighted / 100))
        } else {
          0.25
        }
      })

      leg_probs[i] <- pred_prob
      leg_sds[i] <- sqrt(pred_prob * (1 - pred_prob))
    } else {
      if ("prev_points_weighted" %in% names(member) && !is.na(member$prev_points_weighted)) {
        leg_probs[i] <- pmax(0.01, pmin(0.8, member$prev_points_weighted / 100))
      } else {
        leg_probs[i] <- 0.25
      }
      leg_sds[i] <- 0.2
    }
  }

  team_prob <- sum(leg_probs * leg_importance)
  team_score <- log(team_prob / (1 - team_prob))

  team_sd <- sqrt(sum((leg_importance * leg_sds)^2))
  score_sd <- team_sd / (team_prob * (1 - team_prob))

  if (n_members == 4) {
    score_sd <- pmax(RELAY_SCORE_SD_MIN, pmin(RELAY_SCORE_SD_MAX, score_sd))
  } else {
    score_sd <- pmax(TS_SCORE_SD_MIN, pmin(TS_SCORE_SD_MAX, score_sd))
  }

  return(list(
    mean = team_score,
    sd = score_sd,
    team_prob = team_prob,
    leg_probs = leg_probs,
    leg_importance = leg_importance,
    member_ids = team_members$ID,
    member_names = team_members$Skier,
    member_nations = team_members$Nation,
    member_predictions = leg_probs,
    member_sds = leg_sds,
    team_members = team_members
  ))
}

# Simulate team race positions
simulate_team_race <- function(team_distributions, n_simulations = N_SIMULATIONS) {
  n_teams <- length(team_distributions)
  team_names <- names(team_distributions)

  thresholds <- c(1, 3, 5, 10)
  position_counts <- matrix(0, nrow = n_teams, ncol = length(thresholds),
                            dimnames = list(team_names, paste0("top_", thresholds)))

  for (sim in 1:n_simulations) {
    simulated_points <- sapply(team_distributions, function(dist) {
      if (is.null(dist)) return(-Inf)
      rnorm(1, mean = dist$mean, sd = dist$sd)
    })

    ranks <- rank(-simulated_points, ties.method = "random")

    for (t_idx in seq_along(thresholds)) {
      threshold <- thresholds[t_idx]
      achieved <- ranks <= threshold
      position_counts[achieved, t_idx] <- position_counts[achieved, t_idx] + 1
    }
  }

  position_probs <- position_counts / n_simulations

  results <- data.frame(
    Nation = team_names,
    mean_score = sapply(team_distributions, function(d) if(is.null(d)) 0 else d$mean),
    team_prob = sapply(team_distributions, function(d) if(is.null(d)) 0 else d$team_prob),
    prob_top_1 = position_probs[, 1],
    prob_top_3 = position_probs[, 2],
    prob_top_5 = position_probs[, 3],
    prob_top_10 = position_probs[, 4],
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(prob_top_1))

  return(results)
}

# ============================================================================
# PART 1: LOAD RACE SCHEDULE AND DETERMINE TODAY'S RACES
# ============================================================================

log_info("=== PART 1: LOADING RACE SCHEDULE ===")

# Read race schedule
races_file <- if(TEST_MODE) {
  "~/ski/elo/python/ski/polars/excel365/test_races.csv"
} else {
  "~/ski/elo/python/ski/polars/excel365/races.csv"
}
log_info(paste("Reading races from:", races_file))

races <- read.csv(races_file, stringsAsFactors = FALSE) %>%
  mutate(Date = mdy(Date))

# Find races happening TODAY
current_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")
log_info(paste("Current date (UTC):", current_date))

# Filter races for today only
today_races <- races %>%
  filter(Date == current_date)

if(nrow(today_races) == 0) {
  log_info("No races scheduled for today. Exiting.")
  cat("No races scheduled for today:", format(current_date, "%Y-%m-%d"), "\n")
  quit(save = "no", status = 0)
}

log_info(paste("Found", nrow(today_races), "races scheduled for today"))
print(today_races)

# Categorize today's races by type
individual_races <- today_races %>%
  filter(!Distance %in% c("Rel", "Ts"))

relay_races <- today_races %>%
  filter(Distance == "Rel", Sex != "Mixed")

team_sprint_races <- today_races %>%
  filter(Distance == "Ts")

mixed_relay_races <- today_races %>%
  filter(Distance == "Rel", Sex == "Mixed")

log_info(paste("Race breakdown:"))
log_info(paste("  - Individual races:", nrow(individual_races)))
log_info(paste("  - Relay races:", nrow(relay_races)))
log_info(paste("  - Team sprint races:", nrow(team_sprint_races)))
log_info(paste("  - Mixed relay races:", nrow(mixed_relay_races)))

# Determine which race types to process
PROCESS_INDIVIDUAL <- nrow(individual_races) > 0
PROCESS_RELAY <- nrow(relay_races) > 0
PROCESS_TEAM_SPRINT <- nrow(team_sprint_races) > 0
PROCESS_MIXED_RELAY <- nrow(mixed_relay_races) > 0

# Check if any races to process
if(!PROCESS_INDIVIDUAL && !PROCESS_RELAY && !PROCESS_TEAM_SPRINT && !PROCESS_MIXED_RELAY) {
  log_info("No recognized race types for today. Exiting.")
  quit(save = "no", status = 0)
}

# Determine if today is a stage race (affects points system)
is_stage_today <- any(!is.na(today_races$Stage) & today_races$Stage == 1)
points_system <- if(is_stage_today) stage_points else wc_points
log_info(paste("Points system:", if(is_stage_today) "STAGE" else "WORLD CUP"))

# ============================================================================
# PART 2: LOAD CHRONOLOGICAL DATA
# ============================================================================

log_info("=== PART 2: LOADING CHRONOLOGICAL DATA ===")

# Load individual chrono data
men_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

ladies_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv",
                          stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

log_info(paste("Loaded", nrow(men_chrono), "men's chronological records"))
log_info(paste("Loaded", nrow(ladies_chrono), "ladies' chronological records"))

# Add points column
men_chrono <- men_chrono %>%
  mutate(points = sapply(Place, function(p) get_points(p, wc_points))) %>%
  filter(City != "Tour de Ski", Place != 0)

ladies_chrono <- ladies_chrono %>%
  mutate(points = sapply(Place, function(p) get_points(p, wc_points))) %>%
  filter(City != "Tour de Ski", Place != 0)

# Preprocess individual chrono data for simulation
if(PROCESS_INDIVIDUAL) {
  log_info("Preprocessing chrono data for individual race simulation...")

  # Calculate weighted prev_points
  men_chrono <- calculate_weighted_prev_points(men_chrono)
  ladies_chrono <- calculate_weighted_prev_points(ladies_chrono)

  # Filter to last 10 seasons
  current_season <- max(men_chrono$Season, na.rm = TRUE)
  season_cutoff <- current_season - 10
  men_chrono <- men_chrono %>% filter(Season >= season_cutoff)
  ladies_chrono <- ladies_chrono %>% filter(Season >= season_cutoff)
  log_info(paste("Filtered to seasons", season_cutoff, "to", current_season))

  # Calculate PELO percentage columns
  men_chrono <- calculate_percentage_columns(men_chrono)
  ladies_chrono <- calculate_percentage_columns(ladies_chrono)

  # Quartile imputation for PELO columns
  pelo_cols <- c("Pelo", "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                 "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo", "Classic_Pelo", "Freestyle_Pelo")
  for (col in pelo_cols) {
    if (col %in% names(men_chrono)) {
      men_chrono[[col]] <- replace_na_with_quartile(men_chrono[[col]])
    }
    if (col %in% names(ladies_chrono)) {
      ladies_chrono[[col]] <- replace_na_with_quartile(ladies_chrono[[col]])
    }
  }

  log_info("Individual chrono preprocessing complete")
}

# Load relay chrono data (for relay, team sprint, mixed relay)
if(PROCESS_RELAY || PROCESS_TEAM_SPRINT || PROCESS_MIXED_RELAY) {
  log_info("Loading relay chronological data...")

  men_relay_chrono <- tryCatch({
    read.csv("~/ski/elo/python/ski/polars/relay/excel365/men_chrono.csv",
             stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
  }, error = function(e) {
    log_warn(paste("Could not load men's relay chrono:", e$message))
    data.frame()
  })

  ladies_relay_chrono <- tryCatch({
    read.csv("~/ski/elo/python/ski/polars/relay/excel365/ladies_chrono.csv",
             stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
  }, error = function(e) {
    log_warn(paste("Could not load ladies' relay chrono:", e$message))
    data.frame()
  })

  log_info(paste("Loaded", nrow(men_relay_chrono), "men's relay chronological records"))
  log_info(paste("Loaded", nrow(ladies_relay_chrono), "ladies' relay chronological records"))

  # Filter to season cutoff
  if (nrow(men_relay_chrono) > 0 && "Season" %in% names(men_relay_chrono)) {
    men_relay_chrono <- men_relay_chrono %>% filter(Season >= RELAY_SEASON_CUTOFF)
  }
  if (nrow(ladies_relay_chrono) > 0 && "Season" %in% names(ladies_relay_chrono)) {
    ladies_relay_chrono <- ladies_relay_chrono %>% filter(Season >= RELAY_SEASON_CUTOFF)
  }

  # Separate team sprint and relay data
  if (nrow(men_relay_chrono) > 0) {
    men_team_sprint_chrono <- men_relay_chrono %>% filter(Distance == "Ts")
    men_relay_only_chrono <- men_relay_chrono %>% filter(Distance == "Rel")
    log_info(paste("Men's team sprint records:", nrow(men_team_sprint_chrono)))
    log_info(paste("Men's relay records:", nrow(men_relay_only_chrono)))
  } else {
    men_team_sprint_chrono <- data.frame()
    men_relay_only_chrono <- data.frame()
  }

  if (nrow(ladies_relay_chrono) > 0) {
    ladies_team_sprint_chrono <- ladies_relay_chrono %>% filter(Distance == "Ts")
    ladies_relay_only_chrono <- ladies_relay_chrono %>% filter(Distance == "Rel")
    log_info(paste("Ladies' team sprint records:", nrow(ladies_team_sprint_chrono)))
    log_info(paste("Ladies' relay records:", nrow(ladies_relay_only_chrono)))
  } else {
    ladies_team_sprint_chrono <- data.frame()
    ladies_relay_only_chrono <- data.frame()
  }
}

# ============================================================================
# PART 3: LOAD STARTLISTS
# ============================================================================

log_info("=== PART 3: LOADING STARTLISTS ===")

# Individual startlists
if(PROCESS_INDIVIDUAL) {
  men_startlist <- if(file.exists("~/ski/elo/python/ski/polars/excel365/startlist_races_men.csv")) {
    read.csv("~/ski/elo/python/ski/polars/excel365/startlist_races_men.csv",
             stringsAsFactors = FALSE)
  } else {
    data.frame(Skier = character(0))
  }

  ladies_startlist <- if(file.exists("~/ski/elo/python/ski/polars/excel365/startlist_races_ladies.csv")) {
    read.csv("~/ski/elo/python/ski/polars/excel365/startlist_races_ladies.csv",
             stringsAsFactors = FALSE)
  } else {
    data.frame(Skier = character(0))
  }

  log_info(paste("Loaded individual startlists:", nrow(men_startlist), "men,", nrow(ladies_startlist), "ladies"))
}

# Relay startlists
if(PROCESS_RELAY) {
  men_relay_startlist <- if(file.exists("~/ski/elo/python/ski/polars/relay/excel365/startlist_races_men.csv")) {
    read.csv("~/ski/elo/python/ski/polars/relay/excel365/startlist_races_men.csv",
             stringsAsFactors = FALSE)
  } else {
    data.frame()
  }

  ladies_relay_startlist <- if(file.exists("~/ski/elo/python/ski/polars/relay/excel365/startlist_races_ladies.csv")) {
    read.csv("~/ski/elo/python/ski/polars/relay/excel365/startlist_races_ladies.csv",
             stringsAsFactors = FALSE)
  } else {
    data.frame()
  }

  log_info(paste("Loaded relay startlists:", nrow(men_relay_startlist), "men teams,", nrow(ladies_relay_startlist), "ladies teams"))
}

# Team sprint startlists
if(PROCESS_TEAM_SPRINT) {
  men_ts_startlist <- if(file.exists("~/ski/elo/python/ski/polars/relay/excel365/startlist_ts_men.csv")) {
    read.csv("~/ski/elo/python/ski/polars/relay/excel365/startlist_ts_men.csv",
             stringsAsFactors = FALSE)
  } else {
    data.frame()
  }

  ladies_ts_startlist <- if(file.exists("~/ski/elo/python/ski/polars/relay/excel365/startlist_ts_ladies.csv")) {
    read.csv("~/ski/elo/python/ski/polars/relay/excel365/startlist_ts_ladies.csv",
             stringsAsFactors = FALSE)
  } else {
    data.frame()
  }

  log_info(paste("Loaded team sprint startlists:", nrow(men_ts_startlist), "men teams,", nrow(ladies_ts_startlist), "ladies teams"))
}

# Mixed relay startlists
if(PROCESS_MIXED_RELAY) {
  mixed_relay_startlist <- if(file.exists("~/ski/elo/python/ski/polars/relay/excel365/startlist_mixed_relay.csv")) {
    read.csv("~/ski/elo/python/ski/polars/relay/excel365/startlist_mixed_relay.csv",
             stringsAsFactors = FALSE)
  } else {
    data.frame()
  }

  log_info(paste("Loaded mixed relay startlist:", nrow(mixed_relay_startlist), "teams"))
}

log_info("=== DATA LOADING COMPLETE ===")

# ============================================================================
# INDIVIDUAL RACE SIMULATION
# ============================================================================

individual_results <- list()

if(PROCESS_INDIVIDUAL) {
  log_info("=== INDIVIDUAL RACE SIMULATION ===")

  # Determine race types for today's individual races
  men_individual <- individual_races %>% filter(Sex == "M")
  ladies_individual <- individual_races %>% filter(Sex == "L")

  # Get race type keys for today's races
  today_race_types <- individual_races %>%
    mutate(race_type_key = mapply(determine_race_type_key, Distance, Technique, MS)) %>%
    filter(!is.na(race_type_key)) %>%
    pull(race_type_key) %>%
    unique()

  log_info(paste("Today's individual race types:", paste(today_race_types, collapse = ", ")))

  # Train GAM models for required race types
  log_info("Training GAM models for today's race types...")
  men_models <- list()
  ladies_models <- list()

  for (race_type_key in today_race_types) {
    men_models[[race_type_key]] <- train_points_gam(men_chrono, race_type_key, "men")
    ladies_models[[race_type_key]] <- train_points_gam(ladies_chrono, race_type_key, "ladies")
  }

  log_info(paste("Trained", sum(!sapply(men_models, is.null)), "men's models"))
  log_info(paste("Trained", sum(!sapply(ladies_models, is.null)), "ladies' models"))

  # Process each individual race
  for (i in 1:nrow(individual_races)) {
    race <- individual_races[i, ]
    race_type_key <- determine_race_type_key(race$Distance, race$Technique, race$MS)

    if (is.na(race_type_key)) {
      log_warn(paste("Could not determine race type for race", i))
      next
    }

    gender <- if(race$Sex == "M") "men" else "ladies"
    log_info(paste("Processing", gender, race_types[[race_type_key]]$name))

    # Get appropriate model and data
    model_list <- if(gender == "men") men_models else ladies_models
    chrono_data <- if(gender == "men") men_chrono else ladies_chrono
    startlist <- if(gender == "men") men_startlist else ladies_startlist

    model_info <- model_list[[race_type_key]]

    if (is.null(model_info)) {
      log_warn(paste("No model available for", race_type_key))
      next
    }

    # Get athletes from startlist
    if (nrow(startlist) == 0 || !"ID" %in% names(startlist)) {
      log_warn(paste("No startlist data for", gender))
      next
    }

    athlete_ids <- unique(startlist$ID)
    log_info(paste("Building distributions for", length(athlete_ids), "athletes"))

    # Build distributions for each athlete
    athlete_distributions <- list()

    for (athlete_id in athlete_ids) {
      # Get athlete's current features for GAM prediction
      athlete_features <- chrono_data %>%
        filter(ID == athlete_id) %>%
        arrange(desc(Date)) %>%
        slice(1)

      if (nrow(athlete_features) == 0) {
        # No history - use median values
        gam_prediction <- median(chrono_data$points, na.rm = TRUE)
      } else {
        # Predict from GAM
        gam_prediction <- tryCatch({
          predict(model_info$model, newdata = athlete_features, type = "response")
        }, error = function(e) {
          median(chrono_data$points, na.rm = TRUE)
        })
      }

      # Build distribution
      dist <- build_athlete_distribution(
        athlete_id = athlete_id,
        race_type_key = race_type_key,
        chrono_data = chrono_data,
        gam_prediction = gam_prediction,
        gam_residual_sd = model_info$residual_sd,
        reference_date = current_date
      )

      athlete_distributions[[as.character(athlete_id)]] <- dist
    }

    # Run Monte Carlo simulation
    log_info(paste("Running", N_SIMULATIONS, "Monte Carlo simulations"))
    race_results <- simulate_race_positions(athlete_distributions)

    # Add athlete names and other info
    race_results <- race_results %>%
      left_join(
        startlist %>% select(ID, Skier, Nation) %>% distinct(),
        by = c("athlete_id" = "ID")
      ) %>%
      rename(ID = athlete_id, Name = Skier) %>%
      select(Name, Nation, ID, mean_points, sd_points, n_actual_races,
             starts_with("prob_top_")) %>%
      arrange(desc(prob_top_1))

    # Store results
    race_key <- paste(gender, race_type_key, sep = "_")
    individual_results[[race_key]] <- list(
      race_info = race,
      predictions = race_results,
      gender = gender,
      race_type = race_type_key
    )

    log_info(paste("Completed", race_key, "- Top 3:"))
    print(head(race_results %>% select(Name, Nation, prob_top_1, prob_top_3), 3))
  }

  log_info(paste("Individual race simulation complete.", length(individual_results), "races processed"))
}

# ============================================================================
# RELAY SIMULATION (4-leg)
# ============================================================================

relay_results <- list()

if(PROCESS_RELAY) {
  log_info("=== RELAY SIMULATION ===")

  # Calculate leg importance from historical data
  log_info("Calculating relay leg importance weights...")
  men_relay_leg_importance <- calculate_leg_importance_from_models(
    men_relay_only_chrono, men_chrono, n_legs = 4, event_type = "Men's Relay"
  )
  ladies_relay_leg_importance <- calculate_leg_importance_from_models(
    ladies_relay_only_chrono, ladies_chrono, n_legs = 4, event_type = "Ladies' Relay"
  )

  # Train leg-specific models
  log_info("Training relay leg-specific models...")
  men_relay_leg_models <- train_relay_leg_models_for_simulation(
    men_relay_only_chrono, men_chrono, n_legs = 4, gender = "men"
  )
  ladies_relay_leg_models <- train_relay_leg_models_for_simulation(
    ladies_relay_only_chrono, ladies_chrono, n_legs = 4, gender = "ladies"
  )

  # Process each relay race
  for (i in 1:nrow(relay_races)) {
    race <- relay_races[i, ]
    gender <- if(race$Sex == "M") "men" else "ladies"

    log_info(paste("Processing", gender, "relay"))

    if (gender == "men") {
      startlist <- men_relay_startlist
      chrono_data <- men_chrono
      leg_importance <- men_relay_leg_importance
      relay_leg_models <- men_relay_leg_models
    } else {
      startlist <- ladies_relay_startlist
      chrono_data <- ladies_chrono
      leg_importance <- ladies_relay_leg_importance
      relay_leg_models <- ladies_relay_leg_models
    }

    if (nrow(startlist) == 0) {
      log_warn(paste("No startlist for", gender, "relay"))
      next
    }

    log_info(paste("Using leg importance weights:", paste(round(leg_importance * 100, 1), "%", collapse = ", ")))

    # Build teams for each nation
    nations <- unique(startlist$Nation[!is.na(startlist$Nation)])
    team_distributions <- list()

    log_info(paste("Optimizing relay teams for", length(nations), "nations..."))

    for (nation in nations) {
      nation_athletes <- startlist %>% filter(Nation == nation)

      if (nrow(nation_athletes) >= 4) {
        team_result <- select_relay_team(
          nation_athletes, n_members = 4,
          leg_models = relay_leg_models,
          chrono_data = chrono_data,
          optimize = TRUE,
          leg_importance = leg_importance
        )

        if (!is.null(team_result) && !is.null(team_result$podium_team)) {
          dist <- build_team_distribution_hybrid(
            team_result$podium_team, relay_leg_models, leg_importance,
            chrono_data = chrono_data, n_members = 4
          )
          dist$podium_team <- team_result$podium_team
          dist$win_team <- team_result$win_team
          team_distributions[[nation]] <- dist
        }
      }
    }

    log_info(paste("Built distributions for", length(team_distributions), "national teams"))

    # Simulate relay
    if (length(team_distributions) > 1) {
      race_results <- simulate_team_race(team_distributions, N_SIMULATIONS)

      race_key <- paste(gender, "Relay")
      relay_results[[race_key]] <- list(
        race_info = race,
        predictions = race_results,
        team_distributions = team_distributions,
        gender = gender,
        race_type = "Relay",
        n_legs = 4
      )

      log_info(paste("Relay simulation complete - Top 3:", paste(head(race_results$Nation, 3), collapse = ", ")))
      log_info(paste("Win probs:", paste(round(head(race_results$prob_top_1, 3) * 100, 1), "%", collapse = ", ")))
    }
  }

  log_info(paste("Relay simulation complete.", length(relay_results), "races processed"))
}

# ============================================================================
# TEAM SPRINT SIMULATION (2-leg)
# ============================================================================

team_sprint_results <- list()

if(PROCESS_TEAM_SPRINT) {
  log_info("=== TEAM SPRINT SIMULATION ===")

  # Process each team sprint race
  for (i in 1:nrow(team_sprint_races)) {
    race <- team_sprint_races[i, ]
    gender <- if(race$Sex == "M") "men" else "ladies"
    technique <- race$Technique

    log_info(paste("Processing", gender, "team sprint", technique))

    if (gender == "men") {
      startlist <- men_ts_startlist
      chrono_data <- men_chrono
      ts_chrono <- men_team_sprint_chrono %>% filter(Technique == technique)
    } else {
      startlist <- ladies_ts_startlist
      chrono_data <- ladies_chrono
      ts_chrono <- ladies_team_sprint_chrono %>% filter(Technique == technique)
    }

    if (nrow(startlist) == 0) {
      log_warn(paste("No startlist for", gender, "team sprint"))
      next
    }

    # Calculate leg importance for team sprint
    log_info("Calculating team sprint leg importance weights...")
    ts_leg_importance <- calculate_leg_importance_from_models(
      ts_chrono, chrono_data, n_legs = 2,
      event_type = paste(gender, "Team Sprint", technique),
      technique = technique
    )

    # Train leg-specific models for team sprint
    log_info("Training team sprint leg-specific models...")
    ts_leg_models <- train_relay_leg_models_for_simulation(
      ts_chrono, chrono_data, n_legs = 2,
      gender = gender, technique = technique
    )

    log_info(paste("Using team sprint leg importance weights:", paste(round(ts_leg_importance * 100, 1), "%", collapse = ", ")))

    # Build teams for each nation
    nations <- unique(startlist$Nation[!is.na(startlist$Nation)])
    team_distributions <- list()

    log_info(paste("Optimizing team sprint teams for", length(nations), "nations (technique:", technique, ")..."))

    for (nation in nations) {
      nation_athletes <- startlist %>% filter(Nation == nation)

      if (nrow(nation_athletes) >= 2) {
        team_result <- select_relay_team(
          nation_athletes, n_members = 2,
          leg_models = ts_leg_models,
          chrono_data = chrono_data,
          optimize = TRUE,
          leg_importance = ts_leg_importance,
          technique = technique
        )

        if (!is.null(team_result) && !is.null(team_result$podium_team)) {
          dist <- build_team_distribution_hybrid(
            team_result$podium_team, ts_leg_models, ts_leg_importance,
            chrono_data = chrono_data, n_members = 2,
            technique = technique
          )
          dist$podium_team <- team_result$podium_team
          dist$win_team <- team_result$win_team
          team_distributions[[nation]] <- dist

          log_info(paste("  ", nation, ":", paste(team_result$podium_team$Skier, collapse = " + "),
                         "score:", round(team_result$podium_weighted_points, 3)))
        }
      }
    }

    log_info(paste("Built distributions for", length(team_distributions), "team sprint teams"))

    # Simulate team sprint
    if (length(team_distributions) > 1) {
      race_results <- simulate_team_race(team_distributions, N_SIMULATIONS)

      race_key <- paste(gender, "Team Sprint", technique)
      team_sprint_results[[race_key]] <- list(
        race_info = race,
        predictions = race_results,
        team_distributions = team_distributions,
        gender = gender,
        race_type = paste("Team Sprint", technique),
        n_legs = 2,
        technique = technique
      )

      log_info(paste("Team sprint simulation complete - Top 3:", paste(head(race_results$Nation, 3), collapse = ", ")))
      log_info(paste("Win probs:", paste(round(head(race_results$prob_top_1, 3) * 100, 1), "%", collapse = ", ")))
    }
  }

  log_info(paste("Team sprint simulation complete.", length(team_sprint_results), "races processed"))
}

# ============================================================================
# MIXED RELAY SIMULATION (4-leg: Ladies legs 1-2, Men legs 3-4)
# ============================================================================

mixed_relay_results <- list()

if(PROCESS_MIXED_RELAY) {
  log_info("=== MIXED RELAY SIMULATION ===")

  # For mixed relay, we need to combine ladies (legs 1-2) and men (legs 3-4)
  # Each nation needs at least 2 ladies + 2 men

  # Calculate leg importance for each gender's legs
  # Ladies legs 1-2 use ladies relay data
  # Men legs 3-4 use men relay data
  log_info("Calculating mixed relay leg importance weights...")

  # Use existing relay importance or calculate fresh
  if (!exists("men_relay_leg_importance")) {
    men_relay_leg_importance <- calculate_leg_importance_from_models(
      men_relay_only_chrono, men_chrono, n_legs = 4, event_type = "Men's Relay"
    )
  }
  if (!exists("ladies_relay_leg_importance")) {
    ladies_relay_leg_importance <- calculate_leg_importance_from_models(
      ladies_relay_only_chrono, ladies_chrono, n_legs = 4, event_type = "Ladies' Relay"
    )
  }

  # Mixed relay importance: ladies legs 1-2 from ladies importance, men legs 3-4 from men importance
  mixed_relay_leg_importance <- c(
    ladies_relay_leg_importance[1:2],  # Ladies classic, Ladies freestyle (or legs 1-2)
    men_relay_leg_importance[3:4]       # Men classic, Men freestyle (or legs 3-4)
  )
  mixed_relay_leg_importance <- mixed_relay_leg_importance / sum(mixed_relay_leg_importance)  # Re-normalize

  log_info(paste("Mixed relay leg importance:", paste(round(mixed_relay_leg_importance * 100, 1), "%", collapse = ", ")))

  # Train leg-specific models if not already done
  if (!exists("men_relay_leg_models")) {
    men_relay_leg_models <- train_relay_leg_models_for_simulation(
      men_relay_only_chrono, men_chrono, n_legs = 4, gender = "men"
    )
  }
  if (!exists("ladies_relay_leg_models")) {
    ladies_relay_leg_models <- train_relay_leg_models_for_simulation(
      ladies_relay_only_chrono, ladies_chrono, n_legs = 4, gender = "ladies"
    )
  }

  # Process each mixed relay race
  for (i in 1:nrow(mixed_relay_races)) {
    race <- mixed_relay_races[i, ]

    log_info("Processing mixed relay")

    # Load mixed relay startlist if available
    if (nrow(mixed_relay_startlist) == 0) {
      log_warn("No startlist for mixed relay")
      next
    }

    # Get nations that have both men and ladies athletes
    nations <- unique(mixed_relay_startlist$Nation[!is.na(mixed_relay_startlist$Nation)])

    team_distributions <- list()

    log_info(paste("Building mixed relay teams for", length(nations), "nations..."))

    for (nation in nations) {
      # Get nation athletes from mixed startlist (should have both genders)
      nation_athletes <- mixed_relay_startlist %>% filter(Nation == nation)

      # Separate by gender (assuming Sex column exists)
      ladies_athletes <- nation_athletes %>% filter(Sex == "L")
      men_athletes <- nation_athletes %>% filter(Sex == "M")

      if (nrow(ladies_athletes) < 2 || nrow(men_athletes) < 2) {
        log_info(paste("  ", nation, ": skipping - need at least 2 ladies and 2 men"))
        next
      }

      # Build optimal team
      # For mixed relay: select best 2 ladies for legs 1-2, best 2 men for legs 3-4

      # Select ladies team (legs 1-2 using ladies relay models/importance)
      ladies_team_result <- select_relay_team(
        ladies_athletes, n_members = 2,
        leg_models = ladies_relay_leg_models,
        chrono_data = ladies_chrono,
        optimize = TRUE,
        leg_importance = ladies_relay_leg_importance[1:2] / sum(ladies_relay_leg_importance[1:2])
      )

      # Select men team (legs 3-4 using men relay models/importance)
      men_team_result <- select_relay_team(
        men_athletes, n_members = 2,
        leg_models = men_relay_leg_models,
        chrono_data = men_chrono,
        optimize = TRUE,
        leg_importance = men_relay_leg_importance[3:4] / sum(men_relay_leg_importance[3:4])
      )

      if (is.null(ladies_team_result) || is.null(men_team_result)) {
        log_info(paste("  ", nation, ": could not form complete team"))
        next
      }

      # Combine into 4-member team
      mixed_team <- bind_rows(
        ladies_team_result$podium_team,
        men_team_result$podium_team
      )

      if (nrow(mixed_team) < 4) {
        next
      }

      # Build team distribution for mixed relay
      # Manual calculation since we mix gender-specific models

      leg_probs <- numeric(4)
      leg_sds <- numeric(4)

      # Legs 1-2: Ladies
      for (leg in 1:2) {
        member <- mixed_team[leg, , drop = FALSE]
        leg_key <- paste0("leg_", leg)

        if (!is.null(ladies_relay_leg_models) && leg_key %in% names(ladies_relay_leg_models$models)) {
          pred_prob <- tryCatch({
            p <- predict(ladies_relay_leg_models$models[[leg_key]]$model,
                         newdata = member, type = "response")
            pmax(0.01, pmin(0.99, p))
          }, error = function(e) 0.25)
        } else {
          pred_prob <- 0.25
        }

        leg_probs[leg] <- pred_prob
        leg_sds[leg] <- sqrt(pred_prob * (1 - pred_prob))
      }

      # Legs 3-4: Men
      for (leg in 3:4) {
        member <- mixed_team[leg, , drop = FALSE]
        leg_key <- paste0("leg_", leg)

        if (!is.null(men_relay_leg_models) && leg_key %in% names(men_relay_leg_models$models)) {
          pred_prob <- tryCatch({
            p <- predict(men_relay_leg_models$models[[leg_key]]$model,
                         newdata = member, type = "response")
            pmax(0.01, pmin(0.99, p))
          }, error = function(e) 0.25)
        } else {
          pred_prob <- 0.25
        }

        leg_probs[leg] <- pred_prob
        leg_sds[leg] <- sqrt(pred_prob * (1 - pred_prob))
      }

      # Calculate team score
      team_prob <- sum(leg_probs * mixed_relay_leg_importance)
      team_score <- log(team_prob / (1 - team_prob))
      team_sd <- sqrt(sum((mixed_relay_leg_importance * leg_sds)^2))
      score_sd <- team_sd / (team_prob * (1 - team_prob))
      score_sd <- pmax(RELAY_SCORE_SD_MIN, pmin(RELAY_SCORE_SD_MAX, score_sd))

      team_distributions[[nation]] <- list(
        mean = team_score,
        sd = score_sd,
        team_prob = team_prob,
        leg_probs = leg_probs,
        leg_importance = mixed_relay_leg_importance,
        member_ids = mixed_team$ID,
        member_names = mixed_team$Skier,
        team_members = mixed_team,
        member_predictions = leg_probs,
        member_sds = leg_sds
      )

      log_info(paste("  ", nation, ": L1=", mixed_team$Skier[1], "+ L2=", mixed_team$Skier[2],
                     "+ M1=", mixed_team$Skier[3], "+ M2=", mixed_team$Skier[4],
                     "prob:", round(team_prob, 3)))
    }

    log_info(paste("Built distributions for", length(team_distributions), "mixed relay teams"))

    # Simulate mixed relay
    if (length(team_distributions) > 1) {
      race_results <- simulate_team_race(team_distributions, N_SIMULATIONS)

      mixed_relay_results[["Mixed Relay"]] <- list(
        race_info = race,
        predictions = race_results,
        team_distributions = team_distributions,
        gender = "Mixed",
        race_type = "Mixed Relay",
        n_legs = 4
      )

      log_info(paste("Mixed relay simulation complete - Top 3:", paste(head(race_results$Nation, 3), collapse = ", ")))
      log_info(paste("Win probs:", paste(round(head(race_results$prob_top_1, 3) * 100, 1), "%", collapse = ", ")))
    }
  }

  log_info(paste("Mixed relay simulation complete.", length(mixed_relay_results), "races processed"))
}

# ============================================================================
# PLACEHOLDER: OUTPUT
# ============================================================================

log_info("=== OUTPUT GENERATION (TODO) ===")
# TODO: Format Excel output
# - Individual predictions
# - Relay predictions
# - Team sprint predictions
# - Mixed relay predictions

log_info("=== RACE-PICKS-SIMULATION.R COMPLETE ===")
