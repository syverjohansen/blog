# Cross-Country Championships Predictions: Simulation-Based Approach
#
# This replaces the normalization-based position probabilities with Monte Carlo simulation.
# Key difference: Instead of predicting probabilities and normalizing, we:
# 1. Build a points distribution for each athlete (history + GAM samples)
# 2. Simulate N races by sampling from distributions and ranking
# 3. Count position frequencies to get probabilities naturally
#
# Benefits:
# - No normalization needed (exactly 1 winner, 3 podium per simulation)
# - Large fields don't unfairly penalize top athletes
# - Field size handled naturally

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

N_HISTORY_REQUIRED <- 10      # Target number of historical races per athlete
N_GAM_SAMPLES <- 0           # Number of GAM samples (equal total weight to history)
GAM_FILL_WEIGHT_FACTOR <- .25 # Weight multiplier for GAM-filled history slots
N_SIMULATIONS <- 1000         # Number of Monte Carlo simulations per race
DECAY_LAMBDA <- 0.002         # Exponential decay rate (0.002 = 50% weight after 1 year)

# Variance control parameters (calibrated 2026-02-05)
SD_SCALE_FACTOR <- 0.77        # Multiply all SDs by this (lower = more deterministic, favorites win more)
SD_MIN <- 4                   # Minimum SD (prevents degenerate distributions)
SD_MAX <- 16                  # Maximum SD (prevents too much randomness)

# Relay variance control (for hybrid approach)
RELAY_SCORE_SD_MIN <- 0.5     # Minimum score SD for relay simulation
RELAY_SCORE_SD_MAX <- 1.15     # Maximum score SD for relay simulation

# Team Sprint variance control (for hybrid approach)
TS_SCORE_SD_MIN <- 0.45        # Minimum score SD for team sprint simulation
TS_SCORE_SD_MAX <- .8        # Maximum score SD for team sprint simulation

# Calibration settings (can run separately since calibration takes time)
RUN_CALIBRATION <- FALSE              # Calibrate individual race parameters
RUN_RELAY_CALIBRATION <- FALSE        # Calibrate relay parameters (4 legs)
RUN_TEAM_SPRINT_CALIBRATION <- FALSE  # Calibrate team sprint parameters (2 legs)
CALIBRATION_START_SEASON <- 2018      # Calibrate on races from this season onward

# Position thresholds to track
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)

# Set up logging
log_dir <- "~/ski/elo/python/ski/polars/excel365/champs-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "champs_simulation_processing.log")))
log_info("Starting Cross-Country Championships predictions (SIMULATION approach)")
log_info(paste("Config: N_HISTORY =", N_HISTORY_REQUIRED,
               ", N_GAM_SAMPLES =", N_GAM_SAMPLES,
               ", N_SIMULATIONS =", N_SIMULATIONS))

# World Cup points system
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,
               40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,
               10,9,8,7,6,5,4,3,2,1)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

get_points <- function(place) {
  if (is.na(place) || place < 1 || place > length(wc_points)) {
    return(0)
  } else {
    return(wc_points[place])
  }
}

replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Filter features to keep only those with positive coefficients
# For points models (Gaussian): higher feature values should predict higher points
# For podium models (binomial): higher feature values should predict higher P(podium)
# Iteratively removes features with negative coefficients until all are positive
filter_positive_coefficients <- function(data, response_var, candidate_vars, family = "gaussian") {
  if (length(candidate_vars) == 0) return(character(0))

  current_vars <- candidate_vars
  max_iterations <- length(candidate_vars)  # Prevent infinite loop

  for (iter in 1:max_iterations) {
    if (length(current_vars) == 0) break

    # Fit linear model to check coefficient signs
    formula_str <- paste(response_var, "~", paste(current_vars, collapse = " + "))

    model <- tryCatch({
      if (family == "binomial") {
        glm(as.formula(formula_str), data = data, family = binomial())
      } else {
        lm(as.formula(formula_str), data = data)
      }
    }, error = function(e) NULL)

    if (is.null(model)) {
      log_warn(paste("Model fitting failed in positive coefficient filter, returning remaining vars"))
      break
    }

    # Get coefficients (excluding intercept)
    coefs <- coef(model)
    coefs <- coefs[names(coefs) != "(Intercept)"]

    # Find features with negative coefficients
    negative_vars <- names(coefs[coefs < 0])

    if (length(negative_vars) == 0) {
      # All coefficients are positive, we're done
      break
    }

    # Remove negative coefficient features
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

# ============================================================================
# PART 1: DATA LOADING (same as champs-predictions.R)
# ============================================================================

log_info("=== PART 1: DATA LOADING ===")

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
  mutate(points = sapply(Place, get_points)) %>%
  filter(City != "Tour de Ski", Place != 0)

ladies_chrono <- ladies_chrono %>%
  mutate(points = sapply(Place, get_points)) %>%
  filter(City != "Tour de Ski", Place != 0)

# ============================================================================
# LOAD RELAY CHRONO DATA (for team sprint and relay predictions)
# ============================================================================

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

# Filter relay data to 2010 onwards for training
RELAY_SEASON_CUTOFF <- 2010

if (nrow(men_relay_chrono) > 0 && "Season" %in% names(men_relay_chrono)) {
  men_relay_chrono <- men_relay_chrono %>% filter(Season >= RELAY_SEASON_CUTOFF)
  log_info(paste("Filtered men's relay to Season >=", RELAY_SEASON_CUTOFF, ":", nrow(men_relay_chrono), "records"))
}

if (nrow(ladies_relay_chrono) > 0 && "Season" %in% names(ladies_relay_chrono)) {
  ladies_relay_chrono <- ladies_relay_chrono %>% filter(Season >= RELAY_SEASON_CUTOFF)
  log_info(paste("Filtered ladies' relay to Season >=", RELAY_SEASON_CUTOFF, ":", nrow(ladies_relay_chrono), "records"))
}

# Separate team sprint and relay data from relay chrono
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

# ============================================================================
# CALCULATE LEG IMPORTANCE USING MODEL DEVIANCE EXPLAINED
# Matches the production champs-predictions.R approach:
# - Train a leg-specific GAM model predicting team podium (Place <= 3)
# - Leg importance = deviance explained by each model
# - Higher deviance = that leg's athlete quality better predicts team success
# ============================================================================

log_info("=== CALCULATING LEG IMPORTANCE FROM MODEL DEVIANCE ===")

# Function to get leg-specific explanatory variables (matches production)
# For team sprint (n_legs=2), technique parameter determines C or F features
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
    # 2-leg team sprint: technique-specific features (matches champs-predictions.R)
    if (!is.null(technique) && technique == "C") {
      # Classic team sprint: only classic and technique-agnostic PELOs
      return(c(base_vars, "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Classic_Pelo_pct",
               "Distance_Pelo_pct", "Distance_C_Pelo_pct"))
    } else if (!is.null(technique) && technique == "F") {
      # Freestyle team sprint: only freestyle and technique-agnostic PELOs
      return(c(base_vars, "Sprint_Pelo_pct", "Sprint_F_Pelo_pct", "Freestyle_Pelo_pct",
               "Distance_Pelo_pct", "Distance_F_Pelo_pct"))
    } else {
      # Unknown technique: include all options as fallback
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

# Function to calculate leg importance based on model deviance explained
# individual_chrono: the individual race chrono data (for calculating prev_points_weighted)
# technique: for team sprint (n_legs=2), specifies C or F for technique-specific features
calculate_leg_importance_from_models <- function(relay_chrono, individual_chrono, n_legs = 4, event_type = "Relay", technique = NULL) {
  if (is.null(relay_chrono) || nrow(relay_chrono) == 0 || !"Leg" %in% names(relay_chrono)) {
    log_warn(paste("No leg data available for", event_type, "- using equal weights"))
    return(rep(1/n_legs, n_legs))
  }

  # Calculate prev_points_weighted from INDIVIDUAL chrono using exponential decay
  # For each relay race, calculate athlete's prev_points based on races BEFORE that date
  if (!is.null(individual_chrono) && nrow(individual_chrono) > 0 && "ID" %in% names(relay_chrono)) {
    log_info(paste("Calculating prev_points_weighted with exponential decay for", event_type))

    # Helper function to calculate exponential decay weighted prev_points
    calc_exp_decay_prev_points <- function(athlete_id, reference_date, chrono_subset) {
      athlete_races <- chrono_subset %>%
        filter(ID == athlete_id, Date < reference_date) %>%
        arrange(desc(Date))

      if (nrow(athlete_races) == 0) return(0)

      # Calculate exponential decay weights
      days_ago <- as.numeric(difftime(reference_date, athlete_races$Date, units = "days"))
      weights <- exp(-DECAY_LAMBDA * days_ago)

      weighted.mean(athlete_races$points, weights, na.rm = TRUE)
    }

    # Prepare filtered chrono subsets
    classic_chrono <- individual_chrono %>% filter(Distance != "Sprint", Technique == "C")
    freestyle_chrono <- individual_chrono %>% filter(Distance != "Sprint", Technique == "F")
    sprint_chrono <- individual_chrono %>% filter(Distance == "Sprint")

    # Calculate prev_points_weighted for each row in relay_chrono
    relay_chrono$prev_points_weighted <- sapply(1:nrow(relay_chrono), function(i) {
      row <- relay_chrono[i, ]
      athlete_id <- row$ID
      ref_date <- row$Date

      if (n_legs == 2) {
        # Team sprint: use sprint history
        calc_exp_decay_prev_points(athlete_id, ref_date, sprint_chrono)
      } else if (row$Leg %in% c(1, 2)) {
        # Relay classic legs
        calc_exp_decay_prev_points(athlete_id, ref_date, classic_chrono)
      } else {
        # Relay freestyle legs
        calc_exp_decay_prev_points(athlete_id, ref_date, freestyle_chrono)
      }
    })

    log_info(paste("  Average prev_points_weighted:", round(mean(relay_chrono$prev_points_weighted, na.rm = TRUE), 1)))
  } else {
    log_warn("No individual chrono available - using default prev_points_weighted = 25")
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

  # Filter to valid leg data
  leg_data <- relay_chrono %>%
    filter(!is.na(Leg), Leg >= 1, Leg <= n_legs, !is.na(Place), Place > 0)

  if (nrow(leg_data) < 50) {
    log_warn(paste("Insufficient leg data for", event_type, "(need 50, have", nrow(leg_data), ") - using equal weights"))
    return(rep(1/n_legs, n_legs))
  }

  log_info(paste("Training leg-specific models for", event_type, "with", nrow(leg_data), "records"))

  # Train a model for each leg and extract deviance explained
  leg_deviances <- numeric(n_legs)

  for (leg in 1:n_legs) {
    leg_subset <- leg_data %>% filter(Leg == leg)

    if (nrow(leg_subset) < 20) {
      log_warn(paste("Insufficient data for leg", leg, "(need 20, have", nrow(leg_subset), ")"))
      leg_deviances[leg] <- 0
      next
    }

    # Create binary outcome: did team podium?
    leg_subset <- leg_subset %>%
      mutate(podium = as.numeric(Place <= 3))

    # Get leg-specific explanatory variables (pass technique for team sprint)
    explanatory_vars <- get_relay_explanatory_vars(leg, n_legs, technique)

    # Keep only variables that exist in data
    available_vars <- explanatory_vars[explanatory_vars %in% names(leg_subset)]

    if (length(available_vars) < 2) {
      log_warn(paste("Not enough variables for leg", leg, "model"))
      leg_deviances[leg] <- 0
      next
    }

    # Replace NAs with quartile values
    for (var in available_vars) {
      leg_subset[[var]] <- replace_na_with_quartile(leg_subset[[var]])
    }

    # Filter to keep only features with positive coefficients
    positive_vars <- filter_positive_coefficients(leg_subset, "podium", available_vars, family = "binomial")

    if (length(positive_vars) == 0) {
      log_warn(paste("  Leg", leg, ": no positive coefficient features - using prev_points_weighted only"))
      positive_vars <- "prev_points_weighted"
    }

    # Try to train GAM model - start with k=3, fall back to linear if needed
    leg_model <- NULL

    # Try GAM with k=3 first
    tryCatch({
      smooth_terms <- paste("s(", positive_vars, ", k=3)", collapse = " + ")
      gam_formula <- as.formula(paste("podium ~", smooth_terms))
      leg_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
    }, error = function(e) {
      log_info(paste("  Leg", leg, ": GAM k=3 failed, trying linear terms"))
    })

    # Fall back to linear terms if GAM failed
    if (is.null(leg_model)) {
      tryCatch({
        linear_terms <- paste(positive_vars, collapse = " + ")
        gam_formula <- as.formula(paste("podium ~", linear_terms))
        leg_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
      }, error = function(e) {
        log_warn(paste("  Leg", leg, ": linear model also failed:", e$message))
      })
    }

    if (!is.null(leg_model)) {
      # Extract deviance explained
      dev_explained <- summary(leg_model)$dev.expl
      leg_deviances[leg] <- dev_explained
      log_info(paste("  Leg", leg, ": n=", nrow(leg_subset),
                     ", deviance explained =", round(dev_explained * 100, 1), "%"))
    } else {
      leg_deviances[leg] <- 0
    }
  }

  # Convert deviances to importance weights
  total_deviance <- sum(leg_deviances, na.rm = TRUE)

  if (total_deviance > 0) {
    importance <- leg_deviances / total_deviance
  } else {
    log_warn(paste("All leg deviances are 0 for", event_type, "- using equal weights"))
    importance <- rep(1/n_legs, n_legs)
  }

  # Normalize to sum to 1
  importance <- importance / sum(importance)

  log_info(paste(event_type, "leg importance weights (from model deviance):"))
  for (i in 1:n_legs) {
    log_info(paste("  Leg", i, ":", round(importance[i] * 100, 1), "%"))
  }

  return(importance)
}

# Calculate relay leg importance (4 legs) using model deviance
# Pass individual chrono to calculate prev_points_weighted from individual race history
men_relay_leg_importance <- calculate_leg_importance_from_models(men_relay_only_chrono, men_chrono, 4, "Men's Relay")
ladies_relay_leg_importance <- calculate_leg_importance_from_models(ladies_relay_only_chrono, ladies_chrono, 4, "Ladies' Relay")

# Calculate team sprint leg importance (2 legs) using model deviance
men_ts_leg_importance <- calculate_leg_importance_from_models(men_team_sprint_chrono, men_chrono, 2, "Men's Team Sprint")
ladies_ts_leg_importance <- calculate_leg_importance_from_models(ladies_team_sprint_chrono, ladies_chrono, 2, "Ladies' Team Sprint")

log_info("=== END LEG IMPORTANCE CALCULATION ===\n")

# Calculate weighted prev_points for GAM training using exponential decay
# Uses DECAY_LAMBDA for date-based weighting (consistent with testing/prediction)
calculate_weighted_prev_points <- function(chrono_data) {
  chrono_data %>%
    arrange(ID, Date) %>%
    group_by(ID) %>%
    mutate(
      prev_points_weighted = sapply(1:n(), function(i) {
        if (i == 1) return(0)

        current_date <- Date[i]
        current_distance <- Distance[i]
        current_technique <- Technique[i]
        prev_distances <- Distance[1:(i-1)]
        prev_techniques <- Technique[1:(i-1)]
        prev_points_values <- points[1:(i-1)]
        prev_dates <- Date[1:(i-1)]

        # Determine matching race type
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

        # Calculate exponential decay weights based on days ago
        days_ago <- as.numeric(difftime(current_date, matching_dates, units = "days"))
        weights <- exp(-DECAY_LAMBDA * days_ago)

        weighted.mean(matching_points, weights, na.rm = TRUE)
      })
    ) %>%
    ungroup()
}

log_info("Calculating weighted prev_points...")
men_chrono <- calculate_weighted_prev_points(men_chrono)
ladies_chrono <- calculate_weighted_prev_points(ladies_chrono)

# Filter to last 10 seasons
current_season <- max(men_chrono$Season, na.rm = TRUE)
season_cutoff <- current_season - 10

men_chrono <- men_chrono %>% filter(Season >= season_cutoff)
ladies_chrono <- ladies_chrono %>% filter(Season >= season_cutoff)

log_info(paste("Filtered to seasons", season_cutoff, "to", current_season))

# Calculate PELO percentage columns
pelo_cols <- c("Pelo", "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
               "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo", "Classic_Pelo", "Freestyle_Pelo")

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

men_chrono <- calculate_percentage_columns(men_chrono)
ladies_chrono <- calculate_percentage_columns(ladies_chrono)

# Quartile imputation
for (col in pelo_cols) {
  if (col %in% names(men_chrono)) {
    men_chrono[[col]] <- replace_na_with_quartile(men_chrono[[col]])
  }
  if (col %in% names(ladies_chrono)) {
    ladies_chrono[[col]] <- replace_na_with_quartile(ladies_chrono[[col]])
  }
}

log_info("Data loading and preparation complete")

# ============================================================================
# PART 2: GAM MODEL TRAINING (for points prediction, not position probability)
# ============================================================================

log_info("=== PART 2: GAM MODEL TRAINING ===")

# Race type definitions
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

# Get technique-dependent explanatory variables
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

# Train GAM for POINTS prediction (not position probability)
train_points_gam <- function(chrono_data, race_type_key, gender) {
  race_info <- race_types[[race_type_key]]
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
    # (higher Elo/points should predict higher race points)
    positive_vars <- filter_positive_coefficients(filtered_data, "points", best_bic_vars, family = "gaussian")

    if (length(positive_vars) == 0) {
      log_warn("No positive coefficient features - using prev_points_weighted only")
      positive_vars <- "prev_points_weighted"
    }

    # GAM for points (Gaussian, not binomial)
    smooth_terms <- paste("s(", positive_vars, ")", collapse = " + ")
    gam_formula <- as.formula(paste("points ~", smooth_terms))

    points_model <- gam(gam_formula, data = filtered_data, method = "REML")

    # Get residual SD for distribution building
    residual_sd <- sqrt(points_model$sig2)
    if (is.null(residual_sd) || is.na(residual_sd)) {
      residual_sd <- sqrt(points_model$deviance / points_model$df.residual)
    }
    residual_sd <- max(residual_sd, 5)  # Minimum SD

    log_info(paste("GAM trained. Residual SD:", round(residual_sd, 2)))
    log_info(paste("Final features (positive coefficients only):", paste(positive_vars, collapse = ", ")))

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

# Read weekends to determine required race types
weekends <- read.csv("~/ski/elo/python/ski/polars/excel365/weekends.csv", stringsAsFactors = FALSE) %>%
  mutate(Date = mdy(Date), Race_Date = mdy(Race_Date))

champs_races <- weekends %>% filter(Championship == 1) %>% arrange(Race_Date)

determine_race_type_key <- function(distance, technique, ms = 0) {
  if (distance == "Sprint" && technique == "C") return("Sprint_C")
  if (distance == "Sprint" && technique == "F") return("Sprint_F")
  if (distance != "Sprint" && technique == "C" && ms == 1) return("Distance_C_Ms")
  if (distance != "Sprint" && technique == "C" && ms == 0) return("Distance_C_Ind")
  if (distance != "Sprint" && technique == "F" && ms == 1) return("Distance_F_Ms")
  if (distance != "Sprint" && technique == "F" && ms == 0) return("Distance_F_Ind")
  if (distance != "Sprint" && ms == 1) return("Distance_Ms")
  if (distance != "Sprint" && ms == 0) return("Distance_Ind")
  return(NA)
}

required_race_types <- champs_races %>%
  filter(!Distance %in% c("Rel", "Ts")) %>%
  mutate(race_type_key = mapply(determine_race_type_key, Distance, Technique, MS)) %>%
  filter(!is.na(race_type_key)) %>%
  pull(race_type_key) %>%
  unique()

log_info(paste("Required race types:", paste(required_race_types, collapse = ", ")))

# Train models
men_models <- list()
ladies_models <- list()

for (race_type_key in required_race_types) {
  men_models[[race_type_key]] <- train_points_gam(men_chrono, race_type_key, "men")
  ladies_models[[race_type_key]] <- train_points_gam(ladies_chrono, race_type_key, "ladies")
}

log_info(paste("Trained", sum(!sapply(men_models, is.null)), "men's models"))
log_info(paste("Trained", sum(!sapply(ladies_models, is.null)), "ladies' models"))

# ============================================================================
# PART 2B: TEAM SPRINT AND RELAY MODEL TRAINING
# ============================================================================

log_info("=== PART 2B: TEAM SPRINT AND RELAY MODEL TRAINING ===")

# Function to calculate Weighted_Last_5 from individual sprint races
# This matches race-picks-team-sprint.R approach
calculate_sprint_weighted_last_5 <- function(individual_chrono, technique) {
  # Filter to sprint races of the specified technique
  sprint_races <- individual_chrono %>%
    filter(Distance == "Sprint", Technique == technique) %>%
    arrange(ID, Date)

  if (nrow(sprint_races) == 0) return(data.frame())

  # Calculate Weighted_Last_5 for each athlete at each race
  sprint_races %>%
    group_by(ID) %>%
    mutate(
      Weighted_Last_5 = sapply(row_number(), function(i) {
        if (i == 1) return(0)
        prev_points <- points[max(1, i-5):(i-1)]
        if (length(prev_points) > 0) {
          weights <- seq(1, length(prev_points))
          weighted.mean(prev_points, weights, na.rm = TRUE)
        } else {
          0
        }
      })
    ) %>%
    ungroup()
}

# Function to process team sprint data - combines team sprint results with individual sprint history
process_team_sprint_for_training <- function(team_sprint_chrono, individual_chrono, technique) {
  if (nrow(team_sprint_chrono) == 0) {
    log_warn(paste("No team sprint data for technique", technique))
    return(data.frame())
  }

  # Get sprint data with Weighted_Last_5
  sprint_data <- calculate_sprint_weighted_last_5(individual_chrono, technique)

  if (nrow(sprint_data) == 0) {
    log_warn(paste("No individual sprint data for technique", technique))
    return(data.frame())
  }

  # Filter team sprints to this technique
  ts_data <- team_sprint_chrono %>%
    filter(Technique == technique) %>%
    mutate(points = sapply(Place, get_points))

  if (nrow(ts_data) == 0) {
    log_warn(paste("No team sprint races for technique", technique))
    return(data.frame())
  }

  # Get all PELO column names (raw, not _pct)
  pelo_cols <- names(ts_data)[grep("^[A-Za-z_]*Pelo$", names(ts_data))]
  log_info(paste("PELO columns in team sprint data:", paste(pelo_cols, collapse = ", ")))

  # Combine team sprint with individual sprint data
  # The key insight: fill Weighted_Last_5 from individual sprint history
  combined <- bind_rows(
    ts_data %>% select(any_of(c("ID", "Skier", "Nation", "Season", "Race", "Date",
                                 "Distance", "Technique", "Leg", "Place", "points",
                                 pelo_cols))),
    sprint_data %>% select(any_of(c("ID", "Skier", "Nation", "Season", "Race", "Date",
                                     "Distance", "Technique", "Place", "points", "Weighted_Last_5",
                                     pelo_cols)))
  ) %>%
    group_by(ID) %>%
    arrange(Date) %>%
    fill(Weighted_Last_5, .direction = "down") %>%
    filter(Distance == "Ts") %>%
    ungroup()

  # Replace NA Weighted_Last_5 with first quartile
  if (nrow(combined) > 0 && "Weighted_Last_5" %in% names(combined)) {
    q1 <- quantile(combined$Weighted_Last_5, 0.25, na.rm = TRUE)
    if (is.na(q1)) q1 <- 0
    combined$Weighted_Last_5[is.na(combined$Weighted_Last_5)] <- q1
  }

  # Calculate PELO_pct columns within each race (this is the key fix!)
  if (nrow(combined) > 0 && length(pelo_cols) > 0) {
    log_info("Calculating PELO_pct columns...")
    combined <- combined %>%
      group_by(Season, Race) %>%
      mutate(across(
        all_of(pelo_cols),
        ~ {
          max_val <- max(., na.rm = TRUE)
          if (is.na(max_val) || max_val == 0) {
            rep(0.5, n())  # Default to middle value if no max
          } else {
            . / max_val
          }
        },
        .names = "{.col}_pct"
      )) %>%
      ungroup()

    # Log sample of calculated values
    pct_cols <- paste0(pelo_cols, "_pct")
    pct_cols_exist <- pct_cols[pct_cols %in% names(combined)]
    if (length(pct_cols_exist) > 0) {
      sample_vals <- head(combined[[pct_cols_exist[1]]], 5)
      log_info(paste("Sample", pct_cols_exist[1], "values:", paste(round(sample_vals, 3), collapse = ", ")))
    }
  }

  log_info(paste("Processed team sprint data:", nrow(combined), "records for technique", technique))
  log_info(paste("Columns in processed data:", paste(names(combined), collapse = ", ")))
  return(combined)
}

# Function to train team sprint GAM model for points
train_team_sprint_gam <- function(ts_data, technique, gender) {
  if (is.null(ts_data) || nrow(ts_data) < 30) {
    log_warn(paste("Insufficient team sprint data for", gender, technique, "- need 30, have",
                   if(is.null(ts_data)) 0 else nrow(ts_data)))
    return(NULL)
  }

  log_info(paste("Training team sprint GAM for", gender, technique, "with", nrow(ts_data), "records"))

  # Calculate PELO percentages if not present
  pelo_cols <- names(ts_data)[grep("Pelo$", names(ts_data))]
  for (col in pelo_cols) {
    pct_col <- paste0(col, "_pct")
    if (!pct_col %in% names(ts_data)) {
      ts_data <- ts_data %>%
        group_by(Season, Race) %>%
        mutate(!!pct_col := .data[[col]] / max(.data[[col]], na.rm = TRUE)) %>%
        ungroup()
    }
  }

  # Define explanatory variables based on technique
  if (technique == "C") {
    explanatory_vars <- c("Weighted_Last_5", "Pelo_pct", "Sprint_Pelo_pct",
                          "Sprint_C_Pelo_pct", "Classic_Pelo_pct")
  } else {
    explanatory_vars <- c("Weighted_Last_5", "Pelo_pct", "Sprint_Pelo_pct",
                          "Sprint_F_Pelo_pct", "Freestyle_Pelo_pct")
  }

  # Keep only variables that exist in data
  available_vars <- explanatory_vars[explanatory_vars %in% names(ts_data)]

  if (length(available_vars) < 2) {
    log_warn(paste("Not enough explanatory variables for team sprint", technique))
    return(NULL)
  }

  # Handle NAs in explanatory variables
  for (var in available_vars) {
    ts_data[[var]] <- replace_na_with_quartile(ts_data[[var]])
  }

  # Try GAM with low k, fall back to linear model if needed
  ts_model <- NULL
  model_type <- "none"

  # First try GAM with low k to avoid "too few unique covariate combinations" error
  ts_model <- tryCatch({
    smooth_terms <- paste("s(", available_vars, ", k=5)", collapse = " + ")
    gam_formula <- as.formula(paste("points ~", smooth_terms))
    model <- gam(gam_formula, data = ts_data, method = "REML")
    model_type <<- "gam_smooth"
    log_info("Team sprint GAM with smooth terms trained successfully")
    model
  }, error = function(e) {
    log_warn(paste("GAM smooth failed:", e$message, "- trying linear terms"))
    NULL
  })

  # Fall back to GAM with linear terms
  if (is.null(ts_model)) {
    ts_model <- tryCatch({
      linear_terms <- paste(available_vars, collapse = " + ")
      gam_formula <- as.formula(paste("points ~", linear_terms))
      model <- gam(gam_formula, data = ts_data, method = "REML")
      model_type <<- "gam_linear"
      log_info("Team sprint GAM with linear terms trained successfully")
      model
    }, error = function(e) {
      log_warn(paste("GAM linear also failed:", e$message, "- trying simple lm"))
      NULL
    })
  }

  # Fall back to simple linear model
  if (is.null(ts_model)) {
    ts_model <- tryCatch({
      linear_terms <- paste(available_vars, collapse = " + ")
      lm_formula <- as.formula(paste("points ~", linear_terms))
      model <- lm(lm_formula, data = ts_data)
      model_type <<- "lm"
      log_info("Team sprint simple LM trained successfully")
      model
    }, error = function(e) {
      log_error(paste("Even simple LM failed:", e$message))
      NULL
    })
  }

  if (!is.null(ts_model)) {
    # Get residual SD
    if (model_type == "lm") {
      residual_sd <- sigma(ts_model)
    } else {
      residual_sd <- sqrt(ts_model$sig2)
      if (is.null(residual_sd) || is.na(residual_sd)) {
        residual_sd <- sqrt(ts_model$deviance / ts_model$df.residual)
      }
    }
    residual_sd <- max(residual_sd, 5)

    log_info(paste("Team sprint model trained (", model_type, "). Residual SD:", round(residual_sd, 2)))
    log_info(paste("Features used:", paste(available_vars, collapse = ", ")))

    return(list(
      model = ts_model,
      residual_sd = residual_sd,
      features = available_vars,
      race_type = paste0("TeamSprint_", technique),
      model_type = model_type
    ))
  }

  # If all failed
  log_error("All model training attempts failed for team sprint")
  return(NULL)
}

# Train team sprint models
men_ts_models <- list()
ladies_ts_models <- list()

# Check if there are team sprint races at the championship
ts_at_champs <- champs_races %>% filter(Distance == "Ts")
if (nrow(ts_at_champs) > 0) {
  ts_techniques <- unique(ts_at_champs$Technique)
  log_info(paste("Championship has team sprint with techniques:", paste(ts_techniques, collapse = ", ")))

  for (technique in ts_techniques) {
    # Process and train men's team sprint model
    men_ts_data <- process_team_sprint_for_training(men_team_sprint_chrono, men_chrono, technique)
    men_ts_models[[paste0("TeamSprint_", technique)]] <- train_team_sprint_gam(men_ts_data, technique, "men")

    # Process and train ladies' team sprint model
    ladies_ts_data <- process_team_sprint_for_training(ladies_team_sprint_chrono, ladies_chrono, technique)
    ladies_ts_models[[paste0("TeamSprint_", technique)]] <- train_team_sprint_gam(ladies_ts_data, technique, "ladies")
  }
}

log_info(paste("Trained", sum(!sapply(men_ts_models, is.null)), "men's team sprint models"))
log_info(paste("Trained", sum(!sapply(ladies_ts_models, is.null)), "ladies' team sprint models"))

# ============================================================================
# PART 2C: RELAY MODEL TRAINING
# ============================================================================

log_info("=== PART 2C: RELAY MODEL TRAINING ===")

# Function to calculate Weighted_Last_5 from individual distance races
calculate_distance_weighted_last_5 <- function(individual_chrono, technique) {
  # Filter to distance races of the specified technique
  distance_races <- individual_chrono %>%
    filter(Distance != "Sprint", Technique == technique) %>%
    arrange(ID, Date)

  if (nrow(distance_races) == 0) return(data.frame())

  # Calculate Weighted_Last_5 for each athlete at each race
  distance_races %>%
    group_by(ID) %>%
    mutate(
      Weighted_Last_5 = sapply(row_number(), function(i) {
        if (i == 1) return(0)
        prev_points <- points[max(1, i-5):(i-1)]
        if (length(prev_points) > 0) {
          weights <- seq(1, length(prev_points))
          weighted.mean(prev_points, weights, na.rm = TRUE)
        } else {
          0
        }
      })
    ) %>%
    ungroup()
}

# ============================================================================
# LEG-SPECIFIC BINOMIAL MODELS FOR RELAY
# These predict team outcome (podium) from individual leg athlete quality
# This matches production's approach (champs-predictions.R)
# ============================================================================

log_info("=== TRAINING LEG-SPECIFIC RELAY MODELS ===")

# Function to train leg-specific binomial GAMs predicting team podium
# Returns models for each leg that can be used in simulation
# For team sprint (n_legs=2), technique parameter specifies C or F
train_relay_leg_models_for_simulation <- function(relay_chrono, individual_chrono, n_legs = 4, gender = "men", technique = NULL) {
  if (is.null(relay_chrono) || nrow(relay_chrono) == 0 || !"Leg" %in% names(relay_chrono)) {
    log_warn(paste("No relay data for", gender, "leg-specific models"))
    return(NULL)
  }

  log_info(paste("Training", gender, "leg-specific models for simulation",
                 if (n_legs == 2 && !is.null(technique)) paste("(technique:", technique, ")") else ""))

  # Calculate prev_points_weighted from individual chrono using exponential decay
  if (!is.null(individual_chrono) && nrow(individual_chrono) > 0 && "ID" %in% names(relay_chrono)) {
    log_info("  Calculating prev_points_weighted with exponential decay...")

    # Helper function to calculate exponential decay weighted prev_points
    calc_exp_decay_prev_points <- function(athlete_id, reference_date, chrono_subset) {
      athlete_races <- chrono_subset %>%
        filter(ID == athlete_id, Date < reference_date) %>%
        arrange(desc(Date))

      if (nrow(athlete_races) == 0) return(0)

      # Calculate exponential decay weights
      days_ago <- as.numeric(difftime(reference_date, athlete_races$Date, units = "days"))
      weights <- exp(-DECAY_LAMBDA * days_ago)

      weighted.mean(athlete_races$points, weights, na.rm = TRUE)
    }

    # Prepare filtered chrono subsets based on event type
    if (n_legs == 4) {
      # 4-leg relay: Classic (legs 1-2), Freestyle (legs 3-4)
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
      # 2-leg team sprint: technique-specific sprint prev_points
      sprint_chrono <- individual_chrono %>% filter(Distance == "Sprint", Technique == technique)

      relay_chrono$prev_points_weighted <- sapply(1:nrow(relay_chrono), function(i) {
        row <- relay_chrono[i, ]
        calc_exp_decay_prev_points(row$ID, row$Date, sprint_chrono)
      })
    } else {
      # Fallback: use all sprint history
      sprint_chrono <- individual_chrono %>% filter(Distance == "Sprint")

      relay_chrono$prev_points_weighted <- sapply(1:nrow(relay_chrono), function(i) {
        row <- relay_chrono[i, ]
        calc_exp_decay_prev_points(row$ID, row$Date, sprint_chrono)
      })
    }

    log_info(paste("  Average prev_points_weighted:", round(mean(relay_chrono$prev_points_weighted, na.rm = TRUE), 1)))
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

  # Filter to valid data
  relay_data <- relay_chrono %>%
    filter(!is.na(Leg), Leg >= 1, Leg <= n_legs, !is.na(Place), Place > 0)

  if (nrow(relay_data) < 50) {
    log_warn(paste("Insufficient data for", gender, "leg-specific models"))
    return(NULL)
  }

  # Train model for each leg
  leg_models <- list()

  for (leg in 1:n_legs) {
    leg_subset <- relay_data %>% filter(Leg == leg)

    if (nrow(leg_subset) < 30) {
      log_warn(paste("  Leg", leg, ": insufficient data (", nrow(leg_subset), ")"))
      next
    }

    # Create outcome variables for different thresholds
    leg_subset <- leg_subset %>%
      mutate(
        is_podium = as.numeric(Place <= 3),
        is_win = as.numeric(Place == 1),
        is_top5 = as.numeric(Place <= 5),
        is_top10 = as.numeric(Place <= 10)
      )

    # Get leg-specific features (pass technique for team sprint)
    explanatory_vars <- get_relay_explanatory_vars(leg, n_legs, technique)
    available_vars <- explanatory_vars[explanatory_vars %in% names(leg_subset)]

    if (length(available_vars) < 2) {
      log_warn(paste("  Leg", leg, ": not enough features"))
      next
    }

    # Replace NAs
    for (var in available_vars) {
      leg_subset[[var]] <- replace_na_with_quartile(leg_subset[[var]])
    }

    # Filter to keep only features with positive coefficients
    # (higher Elo should predict higher P(podium))
    log_info(paste("  Leg", leg, ": filtering for positive coefficients"))
    positive_vars <- filter_positive_coefficients(leg_subset, "is_podium", available_vars, family = "binomial")

    if (length(positive_vars) == 0) {
      log_warn(paste("  Leg", leg, ": no positive coefficient features - using prev_points_weighted only"))
      positive_vars <- "prev_points_weighted"
    }

    # Train binomial GAM for podium prediction (primary model)
    podium_model <- NULL
    win_model <- NULL

    # Try GAM with k=3 for podium
    tryCatch({
      smooth_terms <- paste("s(", positive_vars, ", k=3)", collapse = " + ")
      gam_formula <- as.formula(paste("is_podium ~", smooth_terms))
      podium_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
    }, error = function(e) NULL)

    # Fall back to linear for podium
    if (is.null(podium_model)) {
      tryCatch({
        linear_terms <- paste(positive_vars, collapse = " + ")
        gam_formula <- as.formula(paste("is_podium ~", linear_terms))
        podium_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
      }, error = function(e) {
        log_warn(paste("  Leg", leg, ": podium model training failed"))
      })
    }

    # Train win model (threshold=1) using same features
    tryCatch({
      smooth_terms <- paste("s(", positive_vars, ", k=3)", collapse = " + ")
      gam_formula <- as.formula(paste("is_win ~", smooth_terms))
      win_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
    }, error = function(e) NULL)

    # Fall back to linear for win
    if (is.null(win_model)) {
      tryCatch({
        linear_terms <- paste(positive_vars, collapse = " + ")
        gam_formula <- as.formula(paste("is_win ~", linear_terms))
        win_model <- gam(gam_formula, data = leg_subset, family = binomial(), method = "REML")
      }, error = function(e) {
        log_warn(paste("  Leg", leg, ": win model training failed"))
      })
    }

    if (!is.null(podium_model)) {
      dev_explained <- summary(podium_model)$dev.expl
      win_dev <- if (!is.null(win_model)) round(summary(win_model)$dev.expl * 100, 1) else "N/A"
      log_info(paste("  Leg", leg, ": n=", nrow(leg_subset),
                     ", features=", length(positive_vars),
                     ", podium_dev=", round(dev_explained * 100, 1), "%",
                     ", win_dev=", win_dev, "%"))
      log_info(paste("    Features (positive coef only):", paste(positive_vars, collapse = ", ")))

      leg_models[[paste0("leg_", leg)]] <- list(
        podium_model = podium_model,
        win_model = win_model,
        model = podium_model,  # For backward compatibility
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

  return(list(
    models = leg_models,
    n_legs = n_legs,
    gender = gender
  ))
}

# Train leg-specific models for relay
if (nrow(relay_at_champs) > 0) {
  men_relay_leg_models <- train_relay_leg_models_for_simulation(
    men_relay_only_chrono, men_chrono, 4, "men"
  )
  ladies_relay_leg_models <- train_relay_leg_models_for_simulation(
    ladies_relay_only_chrono, ladies_chrono, 4, "ladies"
  )
} else {
  men_relay_leg_models <- NULL
  ladies_relay_leg_models <- NULL
}

# Train leg-specific models for team sprint (technique-specific like champs-predictions.R)
ts_at_champs <- champs_races %>% filter(Distance == "Ts")
men_ts_leg_models <- list()
ladies_ts_leg_models <- list()

if (nrow(ts_at_champs) > 0) {
  ts_techniques <- unique(ts_at_champs$Technique)
  log_info(paste("Championship has team sprint with techniques:", paste(ts_techniques, collapse = ", ")))

  for (technique in ts_techniques) {
    log_info(paste("Training team sprint models for technique:", technique))

    # Filter chrono data to matching technique
    men_ts_data_tech <- men_team_sprint_chrono %>%
      filter(Technique == technique)
    ladies_ts_data_tech <- ladies_team_sprint_chrono %>%
      filter(Technique == technique)

    # Train technique-specific models
    men_model <- train_relay_leg_models_for_simulation(
      men_ts_data_tech, men_chrono, 2, "men", technique = technique
    )
    ladies_model <- train_relay_leg_models_for_simulation(
      ladies_ts_data_tech, ladies_chrono, 2, "ladies", technique = technique
    )

    # Store with technique key
    if (!is.null(men_model)) {
      men_ts_leg_models[[technique]] <- men_model
    }
    if (!is.null(ladies_model)) {
      ladies_ts_leg_models[[technique]] <- ladies_model
    }
  }

  log_info(paste("Trained", length(men_ts_leg_models), "men's team sprint model sets"))
  log_info(paste("Trained", length(ladies_ts_leg_models), "ladies' team sprint model sets"))
}

log_info("=== END LEG-SPECIFIC MODEL TRAINING ===")

# ============================================================================
# MODEL TRAINING SUMMARY - Features Selected and Model Details
# ============================================================================

log_info("=== MODEL TRAINING SUMMARY ===")

# Function to print model summary
print_model_summary <- function(models, model_type, gender) {
  log_info(paste("\n", toupper(gender), model_type, "MODELS:"))
  log_info(paste(rep("-", 60), collapse = ""))

  for (name in names(models)) {
    model_info <- models[[name]]
    if (!is.null(model_info)) {
      features <- if (!is.null(model_info$features)) model_info$features else "N/A"
      residual_sd <- if (!is.null(model_info$residual_sd)) round(model_info$residual_sd, 2) else "N/A"
      model_type_used <- if (!is.null(model_info$model_type)) model_info$model_type else "gam"

      log_info(paste("  ", name, ":"))
      log_info(paste("    Model type:", model_type_used))
      log_info(paste("    Residual SD:", residual_sd))
      log_info(paste("    Features:", paste(features, collapse = ", ")))

      # If GAM, show smooth terms summary
      if (!is.null(model_info$model) && inherits(model_info$model, "gam")) {
        gam_summary <- summary(model_info$model)
        if (!is.null(gam_summary$s.table)) {
          log_info("    Smooth term significance:")
          for (i in 1:nrow(gam_summary$s.table)) {
            term_name <- rownames(gam_summary$s.table)[i]
            p_value <- gam_summary$s.table[i, "p-value"]
            edf <- gam_summary$s.table[i, "edf"]
            log_info(paste("      ", term_name, "- edf:", round(edf, 2), ", p-value:", format(p_value, digits = 3)))
          }
        }
      }
    }
  }
}

# Print individual race models
print_model_summary(men_models, "Individual Race", "Men")
print_model_summary(ladies_models, "Individual Race", "Ladies")

# Print team sprint models
print_model_summary(men_ts_models, "Team Sprint", "Men")
print_model_summary(ladies_ts_models, "Team Sprint", "Ladies")

# Print relay models
print_model_summary(men_relay_models, "Relay", "Men")
print_model_summary(ladies_relay_models, "Relay", "Ladies")

log_info("=== END MODEL TRAINING SUMMARY ===\n")

# ============================================================================
# PART 3: SIMULATION FUNCTIONS
# ============================================================================

log_info("=== PART 3: SIMULATION FUNCTIONS ===")

# Build athlete distribution combining history + GAM samples
# Key: GAM fill cascades DOWN from actual history position
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

  # -------------------------------------------------------------------------
  # Part 1: Historical races with exponential decay weighting based on date
  # weight = exp(-lambda * days_ago)
  # -------------------------------------------------------------------------
  if (n_actual_races > 0) {
    history_points <- sapply(athlete_history$Place, get_points)

    # Use reference_date if provided, otherwise use most recent date in chrono_data
    if (is.null(reference_date)) {
      reference_date <- max(chrono_data$Date, na.rm = TRUE)
    }

    # Calculate days ago for each race and apply exponential decay
    days_ago <- as.numeric(reference_date - athlete_history$Date)
    history_weights <- exp(-decay_lambda * days_ago)

    all_points <- c(all_points, history_points)
    all_weights <- c(all_weights, history_weights)
  }

  # -------------------------------------------------------------------------
  # Part 2: GAM fill for missing history - use reduced weight based on
  # median decay weight from actual history (or default if no history)
  # -------------------------------------------------------------------------
  n_missing_history <- n_history - n_actual_races

  if (n_missing_history > 0) {
    # GAM samples to fill missing history slots
    gam_fill_points <- rnorm(n_missing_history, mean = gam_prediction, sd = gam_residual_sd)
    gam_fill_points <- pmax(0, pmin(100, gam_fill_points))

    # Use median weight from actual history, or a default decay weight (~1 year old)
    if (n_actual_races > 0) {
      median_weight <- median(all_weights) * gam_fill_weight_factor
    } else {
      # Default: treat GAM fill as ~1 year old data
      median_weight <- exp(-decay_lambda * 365) * gam_fill_weight_factor
    }
    gam_fill_weights <- rep(median_weight, n_missing_history)

    all_points <- c(all_points, gam_fill_points)
    all_weights <- c(all_weights, gam_fill_weights)
  }

  # -------------------------------------------------------------------------
  # Part 3: GAM samples spanning variance (equal total weight to history)
  # -------------------------------------------------------------------------
  total_history_weight <- sum(all_weights)
  gam_sample_weight <- total_history_weight / n_gam_samples

  # Sample from GAM distribution (spanning variance, not just point estimate)
  gam_samples <- rnorm(n_gam_samples, mean = gam_prediction, sd = gam_residual_sd)
  gam_samples <- pmax(0, pmin(100, gam_samples))

  gam_sample_weights <- rep(gam_sample_weight, n_gam_samples)

  all_points <- c(all_points, gam_samples)
  all_weights <- c(all_weights, gam_sample_weights)

  # -------------------------------------------------------------------------
  # Calculate distribution parameters
  # -------------------------------------------------------------------------
  weighted_mean <- weighted.mean(all_points, all_weights, na.rm = TRUE)
  weighted_var <- sum(all_weights * (all_points - weighted_mean)^2) / sum(all_weights)
  weighted_sd <- sqrt(weighted_var)
  weighted_sd <- max(weighted_sd, 5)  # Minimum SD to prevent degenerate distributions

  # Debug logging for invalid distributions

  if (is.na(weighted_mean) || is.na(weighted_sd)) {
    log_warn(paste("Invalid distribution built for athlete:", athlete_id,
                   "- gam_prediction:", gam_prediction,
                   "- gam_residual_sd:", gam_residual_sd,
                   "- n_actual_races:", n_actual_races,
                   "- all_points length:", length(all_points),
                   "- all_weights length:", length(all_weights)))
  }

  return(list(
    athlete_id = athlete_id,
    mean = weighted_mean,
    sd = weighted_sd,
    n_actual_races = n_actual_races,
    n_gam_fill = n_missing_history
  ))
}

# Simulate race positions
simulate_race_positions <- function(athlete_distributions, n_simulations = N_SIMULATIONS,
                                     position_thresholds = POSITION_THRESHOLDS) {

  n_athletes <- length(athlete_distributions)
  athlete_ids <- sapply(athlete_distributions, function(x) x$athlete_id)

  # Initialize position counts
  position_counts <- matrix(0, nrow = n_athletes, ncol = length(position_thresholds),
                            dimnames = list(athlete_ids, paste0("top_", position_thresholds)))

  # Check for invalid distributions before simulation
  for (i in seq_along(athlete_distributions)) {
    dist <- athlete_distributions[[i]]
    if (is.null(dist$mean) || is.na(dist$mean) || is.null(dist$sd) || is.na(dist$sd)) {
      log_warn(paste("Invalid distribution for athlete:", dist$athlete_id,
                     "- mean:", dist$mean, "sd:", dist$sd,
                     "n_actual_races:", dist$n_actual_races))
    }
  }

  # Run simulations
  for (sim in 1:n_simulations) {
    # Sample points from each athlete's distribution
    # Apply SD_SCALE_FACTOR to control randomness (lower = favorites win more)
    simulated_points <- sapply(athlete_distributions, function(dist) {
      # Skip if invalid
      if (is.null(dist$mean) || is.na(dist$mean) || is.null(dist$sd) || is.na(dist$sd)) {
        return(0)  # Return 0 for invalid distributions
      }
      # Apply variance controls: scale, then bound
      scaled_sd <- dist$sd * SD_SCALE_FACTOR
      bounded_sd <- pmax(SD_MIN, pmin(SD_MAX, scaled_sd))
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
# CALIBRATION: Find optimal variance parameters using historical data
# ============================================================================

calibrate_variance_params <- function(chrono_data, models, gender,
                                       start_season = CALIBRATION_START_SEASON,
                                       n_sim = 200) {  # Fewer sims for speed during calibration

  log_info(paste("=== CALIBRATING VARIANCE PARAMETERS FOR", toupper(gender), "==="))

  # Get unique races from start_season onward (excluding Summer/Tour)
  test_races <- chrono_data %>%
    filter(Season >= start_season, City != "Summer", City != "Tour De Ski",
           Distance != "Rel", Distance != "Ts", Place > 0) %>%
    distinct(Season, Race, Distance, Technique, MS, Date) %>%
    arrange(Season, Race)

  log_info(paste("Found", nrow(test_races), "races for calibration"))

  if (nrow(test_races) < 20) {
    log_warn("Not enough races for calibration")
    return(list(sd_scale = 0.6, sd_min = 3, sd_max = 12, decay_lambda = 0.002))
  }

  # Sample races if too many (for speed)
  if (nrow(test_races) > 100) {
    set.seed(42)
    test_races <- test_races[sample(nrow(test_races), 100), ]
    log_info("Sampled 100 races for calibration")
  }

  # Grid search parameters (reduced based on prior calibration)
  scale_values <- c(0.7, 0.85, 1.0)
  min_values <- c(3, 4)
  max_values <- c(15, 18)
  # Wide range of decay rates: 0.0005=82% after 1yr, 0.01=2.5% after 1yr
  lambda_values <- c(0.0005, 0.001, 0.002, 0.004, 0.006, 0.01)

  best_brier <- Inf
  best_params <- list(sd_scale = 0.6, sd_min = 3, sd_max = 12, decay_lambda = 0.002)

  results_log <- data.frame()

  for (decay_lambda in lambda_values) {
    for (sd_scale in scale_values) {
      for (sd_min in min_values) {
        for (sd_max in max_values) {
          if (sd_min >= sd_max) next  # Skip invalid combinations

          # Run predictions on test races with these parameters
          brier_scores <- c()

          for (r in 1:nrow(test_races)) {
            race_info <- test_races[r, ]

            # Get race type key
            race_type_key <- get_race_type_key(race_info$Distance, race_info$Technique,
                                                if(is.null(race_info$MS)) 0 else race_info$MS)

            # Get model for this race type
            model_info <- models[[race_type_key]]
            if (is.null(model_info)) next

            # Get athletes in this race and race date
            race_athletes <- chrono_data %>%
              filter(Season == race_info$Season, Race == race_info$Race,
                     Distance == race_info$Distance, Technique == race_info$Technique)

            if (nrow(race_athletes) < 10) next

            race_date <- min(race_athletes$Date)

            # Get data BEFORE this race for building distributions
            pre_race_data <- chrono_data %>%
              filter(Date < race_date)

            # Build distributions for each athlete
            athlete_dists <- list()
            for (i in 1:nrow(race_athletes)) {
              athlete <- race_athletes[i, ]

              # Get GAM prediction
              gam_pred <- tryCatch({
                predict(model_info$model, newdata = athlete, type = "response")
              }, error = function(e) 25)

              # Build distribution using pre-race data with current decay_lambda
              dist <- tryCatch({
                build_athlete_distribution(
                  athlete_id = athlete$ID,
                  race_type_key = race_type_key,
                  chrono_data = pre_race_data,
                  gam_prediction = gam_pred,
                  gam_residual_sd = model_info$residual_sd,
                  decay_lambda = decay_lambda,
                  reference_date = race_date
                )
              }, error = function(e) NULL)

              if (!is.null(dist)) {
                athlete_dists[[as.character(athlete$ID)]] <- dist
              }
            }

            if (length(athlete_dists) < 5) next

            # Simulate with current parameters
            n_athletes <- length(athlete_dists)
            win_counts <- rep(0, n_athletes)

          for (sim in 1:n_sim) {
            sim_points <- sapply(athlete_dists, function(d) {
              scaled_sd <- d$sd * sd_scale
              bounded_sd <- pmax(sd_min, pmin(sd_max, scaled_sd))
              rnorm(1, mean = d$mean, sd = bounded_sd)
            })
            winner_idx <- which.max(sim_points)
            win_counts[winner_idx] <- win_counts[winner_idx] + 1
          }

          win_probs <- win_counts / n_sim

          # Get actual winner
          actual_winner_id <- race_athletes$ID[race_athletes$Place == 1]
          if (length(actual_winner_id) == 0) next

          # Calculate Brier score for this race
          actual_outcomes <- as.numeric(names(athlete_dists) == as.character(actual_winner_id[1]))
          race_brier <- mean((win_probs - actual_outcomes)^2)
          brier_scores <- c(brier_scores, race_brier)
        }

        if (length(brier_scores) > 0) {
          mean_brier <- mean(brier_scores, na.rm = TRUE)

          results_log <- rbind(results_log, data.frame(
            decay_lambda = decay_lambda, sd_scale = sd_scale,
            sd_min = sd_min, sd_max = sd_max,
            brier = mean_brier, n_races = length(brier_scores)
          ))

          if (mean_brier < best_brier) {
            best_brier <- mean_brier
            best_params <- list(decay_lambda = decay_lambda, sd_scale = sd_scale,
                                sd_min = sd_min, sd_max = sd_max)
            log_info(paste("New best: lambda=", decay_lambda, "scale=", sd_scale,
                           "min=", sd_min, "max=", sd_max, "brier=", round(mean_brier, 4)))
          }
        }
        }
      }
    }
  }

  log_info("=== CALIBRATION RESULTS ===")
  log_info(paste("Best parameters: DECAY_LAMBDA =", best_params$decay_lambda,
                 ", SD_SCALE_FACTOR =", best_params$sd_scale,
                 ", SD_MIN =", best_params$sd_min, ", SD_MAX =", best_params$sd_max))
  log_info(paste("Best Brier score:", round(best_brier, 4)))

  # Show top 5 parameter combinations
  if (nrow(results_log) > 0) {
    top5 <- results_log %>% arrange(brier) %>% head(5)
    log_info("Top 5 parameter combinations:")
    for (i in 1:nrow(top5)) {
      log_info(paste("  ", i, ": lambda=", top5$decay_lambda[i], "scale=", top5$sd_scale[i],
                     "min=", top5$sd_min[i], "max=", top5$sd_max[i],
                     "brier=", round(top5$brier[i], 4)))
    }
  }

  return(best_params)
}

# Helper function to get race type key
get_race_type_key <- function(distance, technique, ms) {
  if (distance == "Sprint") {
    return(paste0("Sprint_", technique))
  } else {
    suffix <- if (ms == 1) "Ms" else "Ind"
    return(paste0("Distance_", technique, "_", suffix))
  }
}

# ============================================================================
# RELAY CALIBRATION: Find optimal score_sd bounds for hybrid approach
# ============================================================================

calibrate_relay_params <- function(relay_chrono, individual_chrono, leg_models,
                                    leg_importance, gender, n_legs = 4,
                                    start_season = CALIBRATION_START_SEASON,
                                    n_sim = 200) {

  log_info(paste("=== CALIBRATING RELAY PARAMETERS FOR", toupper(gender), "==="))

  event_type <- if (n_legs == 4) "Relay" else "Team Sprint"
  distance_filter <- if (n_legs == 4) "Rel" else "Ts"

  # Get unique relay races from start_season onward
  test_races <- relay_chrono %>%
    filter(Season >= start_season, Distance == distance_filter, !is.na(Place), Place > 0) %>%
    distinct(Season, Race) %>%
    arrange(Season, Race)

  log_info(paste("Found", nrow(test_races), event_type, "races for calibration"))

  if (nrow(test_races) < 10) {
    log_warn(paste("Not enough", event_type, "races for calibration"))
    return(list(score_sd_min = 0.3, score_sd_max = 1.0))
  }

  # Grid search parameters for score_sd bounds
  sd_min_values <- c(0.2, 0.3, 0.4, 0.5)
  sd_max_values <- c(0.6, 0.8, 1.0, 1.2, 1.5)

  best_brier <- Inf
  best_params <- list(score_sd_min = 0.3, score_sd_max = 1.0)
  results_log <- data.frame()

  for (sd_min in sd_min_values) {
    for (sd_max in sd_max_values) {
      if (sd_min >= sd_max) next

      brier_scores <- c()

      for (r in 1:nrow(test_races)) {
        race_info <- test_races[r, ]

        # Get teams in this race
        race_data <- relay_chrono %>%
          filter(Season == race_info$Season, Race == race_info$Race,
                 Distance == distance_filter, Leg == 1)  # One row per team

        if (nrow(race_data) < 5) next

        nations <- unique(race_data$Nation)
        if (length(nations) < 5) next

        # Build team distributions for each nation
        team_dists <- list()

        for (nation in nations) {
          # Get team members for this nation in this race
          team_members_data <- relay_chrono %>%
            filter(Season == race_info$Season, Race == race_info$Race,
                   Distance == distance_filter, Nation == nation) %>%
            arrange(Leg)

          if (nrow(team_members_data) < n_legs) next

          # Get the team's actual place (from any leg, they're all the same)
          team_place <- team_members_data$Place[1]

          # Build simplified distribution based on leg importance and historical performance
          # Get prev_points_weighted from individual chrono for each team member
          leg_probs <- numeric(n_legs)

          for (leg in 1:n_legs) {
            member <- team_members_data %>% filter(Leg == leg)
            if (nrow(member) == 0) {
              leg_probs[leg] <- 0.25
              next
            }

            # Get athlete's historical performance
            athlete_id <- member$ID[1]
            if (n_legs == 4) {
              # Relay: classic for legs 1-2, freestyle for 3-4
              tech_filter <- if (leg <= 2) "C" else "F"
              hist_data <- individual_chrono %>%
                filter(ID == athlete_id, Distance != "Sprint", Technique == tech_filter)
            } else {
              # Team sprint: use sprint data
              hist_data <- individual_chrono %>%
                filter(ID == athlete_id, Distance == "Sprint")
            }

            if (nrow(hist_data) > 0) {
              recent_pts <- tail(hist_data$points, 5)
              avg_pts <- mean(recent_pts, na.rm = TRUE)
              leg_probs[leg] <- pmax(0.05, pmin(0.9, avg_pts / 100))
            } else {
              leg_probs[leg] <- 0.25
            }
          }

          team_prob <- sum(leg_probs * leg_importance)
          team_score <- log(team_prob / (1 - team_prob))

          # Apply the SD bounds being tested
          team_sd <- sqrt(sum((leg_importance * 0.2)^2))
          score_sd <- team_sd / (team_prob * (1 - team_prob))
          score_sd <- pmax(sd_min, pmin(sd_max, score_sd))

          team_dists[[nation]] <- list(
            mean = team_score,
            sd = score_sd,
            actual_place = team_place
          )
        }

        if (length(team_dists) < 5) next

        # Simulate with current parameters
        win_counts <- rep(0, length(team_dists))
        names(win_counts) <- names(team_dists)

        for (sim in 1:n_sim) {
          sim_scores <- sapply(team_dists, function(d) {
            rnorm(1, mean = d$mean, sd = d$sd)
          })
          winner <- names(which.max(sim_scores))
          win_counts[winner] <- win_counts[winner] + 1
        }

        win_probs <- win_counts / n_sim

        # Get actual winner
        actual_winner <- names(team_dists)[sapply(team_dists, function(d) d$actual_place) == 1]
        if (length(actual_winner) == 0) next

        # Brier score
        actual_outcomes <- as.numeric(names(team_dists) == actual_winner[1])
        race_brier <- mean((win_probs - actual_outcomes)^2)
        brier_scores <- c(brier_scores, race_brier)
      }

      if (length(brier_scores) > 0) {
        mean_brier <- mean(brier_scores, na.rm = TRUE)

        results_log <- rbind(results_log, data.frame(
          sd_min = sd_min, sd_max = sd_max,
          brier = mean_brier, n_races = length(brier_scores)
        ))

        if (mean_brier < best_brier) {
          best_brier <- mean_brier
          best_params <- list(score_sd_min = sd_min, score_sd_max = sd_max)
          log_info(paste("New best:", event_type, "sd_min=", sd_min, "sd_max=", sd_max,
                         "brier=", round(mean_brier, 4)))
        }
      }
    }
  }

  log_info(paste("=== RELAY CALIBRATION RESULTS FOR", toupper(gender), "==="))
  log_info(paste("Best parameters: score_sd_min =", best_params$score_sd_min,
                 ", score_sd_max =", best_params$score_sd_max))
  log_info(paste("Best Brier score:", round(best_brier, 4)))

  if (nrow(results_log) > 0) {
    top5 <- results_log %>% arrange(brier) %>% head(5)
    log_info("Top 5 parameter combinations:")
    for (i in 1:nrow(top5)) {
      log_info(paste("  ", i, ": sd_min=", top5$sd_min[i], "sd_max=", top5$sd_max[i],
                     "brier=", round(top5$brier[i], 4)))
    }
  }

  return(best_params)
}

# ============================================================================
# RUN CALIBRATIONS (if enabled) - Each can be run separately
# ============================================================================

# ----- INDIVIDUAL RACE CALIBRATION -----
if (RUN_CALIBRATION) {
  log_info("=== RUNNING INDIVIDUAL RACE CALIBRATION ===")
  log_info(paste("Calibrating on seasons", CALIBRATION_START_SEASON, "to present"))

  men_best_params <- calibrate_variance_params(men_chrono, men_models, "men")
  ladies_best_params <- calibrate_variance_params(ladies_chrono, ladies_models, "ladies")

  avg_params <- list(
    decay_lambda = mean(c(men_best_params$decay_lambda, ladies_best_params$decay_lambda)),
    sd_scale = mean(c(men_best_params$sd_scale, ladies_best_params$sd_scale)),
    sd_min = round(mean(c(men_best_params$sd_min, ladies_best_params$sd_min))),
    sd_max = round(mean(c(men_best_params$sd_max, ladies_best_params$sd_max)))
  )

  log_info("\n============================================================")
  log_info("=== INDIVIDUAL RACE CALIBRATION RESULTS ===")
  log_info("============================================================")
  log_info(paste("DECAY_LAMBDA <-", round(avg_params$decay_lambda, 4)))
  log_info(paste("SD_SCALE_FACTOR <-", round(avg_params$sd_scale, 2)))
  log_info(paste("SD_MIN <-", avg_params$sd_min))
  log_info(paste("SD_MAX <-", avg_params$sd_max))
  log_info("\nCopy these to lines 31-36, set RUN_CALIBRATION <- FALSE")
  log_info("============================================================")

  stop("Individual calibration complete.")
}

# ----- RELAY CALIBRATION (4 legs) -----
if (RUN_RELAY_CALIBRATION) {
  log_info("=== RUNNING RELAY CALIBRATION (4 legs) ===")
  log_info(paste("Calibrating on seasons", CALIBRATION_START_SEASON, "to present"))

  men_relay_params <- calibrate_relay_params(
    men_relay_chrono, men_chrono, men_relay_leg_models,
    men_relay_leg_importance, "men", n_legs = 4
  )

  ladies_relay_params <- calibrate_relay_params(
    ladies_relay_chrono, ladies_chrono, ladies_relay_leg_models,
    ladies_relay_leg_importance, "ladies", n_legs = 4
  )

  avg_params <- list(
    score_sd_min = mean(c(men_relay_params$score_sd_min, ladies_relay_params$score_sd_min)),
    score_sd_max = mean(c(men_relay_params$score_sd_max, ladies_relay_params$score_sd_max))
  )

  log_info("\n============================================================")
  log_info("=== RELAY CALIBRATION RESULTS ===")
  log_info("============================================================")
  log_info(paste("RELAY_SCORE_SD_MIN <-", round(avg_params$score_sd_min, 2)))
  log_info(paste("RELAY_SCORE_SD_MAX <-", round(avg_params$score_sd_max, 2)))
  log_info("\nCopy these to lines 38-39, set RUN_RELAY_CALIBRATION <- FALSE")
  log_info("============================================================")

  stop("Relay calibration complete.")
}

# ----- TEAM SPRINT CALIBRATION (2 legs) -----
if (RUN_TEAM_SPRINT_CALIBRATION) {
  log_info("=== RUNNING TEAM SPRINT CALIBRATION (2 legs) ===")
  log_info(paste("Calibrating on seasons", CALIBRATION_START_SEASON, "to present"))

  men_ts_params <- calibrate_relay_params(
    men_relay_chrono, men_chrono, men_ts_leg_models,
    men_ts_leg_importance, "men", n_legs = 2
  )

  ladies_ts_params <- calibrate_relay_params(
    ladies_relay_chrono, ladies_chrono, ladies_ts_leg_models,
    ladies_ts_leg_importance, "ladies", n_legs = 2
  )

  avg_params <- list(
    score_sd_min = mean(c(men_ts_params$score_sd_min, ladies_ts_params$score_sd_min)),
    score_sd_max = mean(c(men_ts_params$score_sd_max, ladies_ts_params$score_sd_max))
  )

  log_info("\n============================================================")
  log_info("=== TEAM SPRINT CALIBRATION RESULTS ===")
  log_info("============================================================")
  log_info(paste("TS_SCORE_SD_MIN <-", round(avg_params$score_sd_min, 2)))
  log_info(paste("TS_SCORE_SD_MAX <-", round(avg_params$score_sd_max, 2)))
  log_info("\nCopy these to lines 42-43, set RUN_TEAM_SPRINT_CALIBRATION <- FALSE")
  log_info("============================================================")

  stop("Team sprint calibration complete.")
}

# ============================================================================
# PART 4: STARTLIST SETUP
# ============================================================================

log_info("=== PART 4: STARTLIST SETUP ===")

men_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_champs_men.csv",
                         stringsAsFactors = FALSE)
ladies_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_champs_ladies.csv",
                            stringsAsFactors = FALSE)

log_info(paste("Loaded", nrow(men_startlist), "men on startlist"))
log_info(paste("Loaded", nrow(ladies_startlist), "ladies on startlist"))

# Create PELO percentage columns for startlists
create_startlist_pelo_pct <- function(startlist_data) {
  startlist_data %>%
    mutate(
      prev_points_weighted = 0,
      Distance_Pelo_pct = Distance_Elo / max(Distance_Elo, na.rm = TRUE),
      Distance_C_Pelo_pct = Distance_C_Elo / max(Distance_C_Elo, na.rm = TRUE),
      Distance_F_Pelo_pct = Distance_F_Elo / max(Distance_F_Elo, na.rm = TRUE),
      Sprint_Pelo_pct = Sprint_Elo / max(Sprint_Elo, na.rm = TRUE),
      Sprint_C_Pelo_pct = Sprint_C_Elo / max(Sprint_C_Elo, na.rm = TRUE),
      Sprint_F_Pelo_pct = Sprint_F_Elo / max(Sprint_F_Elo, na.rm = TRUE),
      Classic_Pelo_pct = Classic_Elo / max(Classic_Elo, na.rm = TRUE),
      Freestyle_Pelo_pct = Freestyle_Elo / max(Freestyle_Elo, na.rm = TRUE),
      Pelo_pct = Elo / max(Elo, na.rm = TRUE)
    )
}

men_startlist <- create_startlist_pelo_pct(men_startlist)

ladies_startlist <- create_startlist_pelo_pct(ladies_startlist)

# ============================================================================
# PART 5: PROCESS INDIVIDUAL RACES WITH SIMULATION
# ============================================================================

log_info("=== PART 5: PROCESS INDIVIDUAL RACES ===")

individual_races <- champs_races %>%
  filter(!Distance %in% c("Rel", "Ts"), Sex %in% c("M", "L")) %>%
  arrange(Race_Date) %>%
  mutate(race_type = mapply(determine_race_type_key, Distance, Technique, MS))

log_info(paste("Processing", nrow(individual_races), "individual races"))

# Calculate start probabilities (same logic as original)
get_start_probability <- function(chronos, participant, race_type) {
  five_years_ago <- Sys.Date() - (5 * 365)

  athlete_first_race <- chronos %>%
    filter(Skier == participant) %>%
    summarise(first_date = min(Date, na.rm = TRUE)) %>%
    pull(first_date)

  if (is.na(athlete_first_race) || length(athlete_first_race) == 0) {
    cutoff_date <- five_years_ago
  } else {
    cutoff_date <- max(five_years_ago, athlete_first_race)
  }

  # Get all races and participant races based on type
  if (race_type == "Sprint_C") {
    all_races <- chronos %>% filter(Date >= cutoff_date, Distance == "Sprint", Technique == "C") %>% distinct(Date, City) %>% arrange(Date)
    part_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance == "Sprint", Technique == "C") %>% distinct(Date, City)
  } else if (race_type == "Sprint_F") {
    all_races <- chronos %>% filter(Date >= cutoff_date, Distance == "Sprint", Technique == "F") %>% distinct(Date, City) %>% arrange(Date)
    part_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance == "Sprint", Technique == "F") %>% distinct(Date, City)
  } else if (grepl("Distance_C", race_type)) {
    all_races <- chronos %>% filter(Date >= cutoff_date, Distance != "Sprint", Technique == "C") %>% distinct(Date, City) %>% arrange(Date)
    part_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance != "Sprint", Technique == "C") %>% distinct(Date, City)
  } else if (grepl("Distance_F", race_type)) {
    all_races <- chronos %>% filter(Date >= cutoff_date, Distance != "Sprint", Technique == "F") %>% distinct(Date, City) %>% arrange(Date)
    part_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance != "Sprint", Technique == "F") %>% distinct(Date, City)
  } else {
    all_races <- chronos %>% filter(Date >= cutoff_date, Distance != "Sprint") %>% distinct(Date, City) %>% arrange(Date)
    part_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance != "Sprint") %>% distinct(Date, City)
  }

  n_races <- nrow(all_races)
  if (n_races == 0) return(0)

  participation <- sapply(1:n_races, function(i) {
    as.numeric(any(part_races$Date == all_races$Date[i] & part_races$City == all_races$City[i]))
  })

  race_weights <- exp(-0.1 * ((n_races - 1):0))
  prob <- sum(participation * race_weights) / sum(race_weights)
  return(prob)
}

# Get race-specific prev_points_weighted using exponential decay
# Uses DECAY_LAMBDA for date-based weighting (same as individual races)
get_race_prev_points <- function(chrono_data, athlete_id, race_type, reference_date = NULL) {
  if (race_type == "Sprint_C") {
    races <- chrono_data %>% filter(ID == athlete_id, Distance == "Sprint", Technique == "C")
  } else if (race_type == "Sprint_F") {
    races <- chrono_data %>% filter(ID == athlete_id, Distance == "Sprint", Technique == "F")
  } else if (race_type == "Sprint") {
    # Generic sprint (all techniques)
    races <- chrono_data %>% filter(ID == athlete_id, Distance == "Sprint")
  } else if (grepl("Distance_C", race_type)) {
    races <- chrono_data %>% filter(ID == athlete_id, Distance != "Sprint", Technique == "C")
  } else if (grepl("Distance_F", race_type)) {
    races <- chrono_data %>% filter(ID == athlete_id, Distance != "Sprint", Technique == "F")
  } else {
    races <- chrono_data %>% filter(ID == athlete_id, Distance != "Sprint")
  }

  if (nrow(races) == 0) return(0)

  # Use reference date or max date in chrono
  if (is.null(reference_date)) {
    reference_date <- max(chrono_data$Date, na.rm = TRUE)
  }

  # Calculate exponential decay weights based on date
  races <- races %>%
    arrange(desc(Date)) %>%
    mutate(
      days_ago = as.numeric(difftime(reference_date, Date, units = "days")),
      weight = exp(-DECAY_LAMBDA * days_ago)
    )

  # Use all races with exponential decay (not just last 5)
  weighted.mean(races$points, races$weight, na.rm = TRUE)
}

# Process races
results_list <- list()
men_race_counter <- 0
ladies_race_counter <- 0

for (i in 1:nrow(individual_races)) {
  race <- individual_races[i, ]
  gender <- if (race$Sex == "M") "men" else "ladies"

  if (race$Sex == "M") {
    men_race_counter <- men_race_counter + 1
    gender_race_num <- men_race_counter
    models <- men_models
    startlist <- men_startlist
    chrono_data <- men_chrono
  } else {
    ladies_race_counter <- ladies_race_counter + 1
    gender_race_num <- ladies_race_counter
    models <- ladies_models
    startlist <- ladies_startlist
    chrono_data <- ladies_chrono
  }

  log_info(paste("Processing", gender, "race", gender_race_num, ":", race$Distance, race$Technique))
  log_info(paste("DEBUG: startlist rows =", nrow(startlist), "chrono_data rows =", nrow(chrono_data)))
  log_info(paste("DEBUG: race_type =", race$race_type, "MS =", race$MS))

  if (!(race$race_type %in% names(models)) || is.null(models[[race$race_type]])) {
    log_warn(paste("No model for", race$race_type, "- skipping"))
    next
  }

  model_info <- models[[race$race_type]]
  gam_model <- model_info$model
  gam_residual_sd <- model_info$residual_sd

  # Clean startlist - remove duplicates and invalid IDs
  startlist_clean <- startlist %>%
    filter(!is.na(ID), ID != "") %>%
    distinct(ID, .keep_all = TRUE)

  log_info(paste("Startlist size after cleaning:", nrow(startlist_clean), "(original:", nrow(startlist), ")"))

  # Get prev_points for each athlete
  for (j in 1:nrow(startlist_clean)) {
    startlist_clean$prev_points_weighted[j] <- get_race_prev_points(chrono_data, startlist_clean$ID[j], race$race_type)
  }

  # Get GAM predictions for all athletes
  gam_predictions <- tryCatch({
    predict(gam_model, newdata = startlist_clean, type = "response")
  }, error = function(e) {
    log_error(paste("GAM prediction failed:", e$message))
    rep(NA, nrow(startlist_clean))
  })

  # Log NA predictions
  n_na_preds <- sum(is.na(gam_predictions))
  if (n_na_preds > 0) {
    log_warn(paste("GAM produced", n_na_preds, "NA predictions out of", length(gam_predictions)))
    na_athletes <- startlist_clean$Skier[is.na(gam_predictions)]
    log_warn(paste("Athletes with NA predictions:", paste(head(na_athletes, 10), collapse = ", ")))
  }

  gam_predictions <- pmax(0, pmin(100, gam_predictions))

  # Build distributions for all athletes
  # Use character IDs as list keys to avoid R treating numeric IDs as indices
  athlete_distributions <- list()
  for (j in 1:nrow(startlist_clean)) {
    athlete_id <- startlist_clean$ID[j]
    dist <- build_athlete_distribution(
      athlete_id = athlete_id,
      race_type_key = race$race_type,
      chrono_data = chrono_data,
      gam_prediction = gam_predictions[j],
      gam_residual_sd = gam_residual_sd
    )
    athlete_distributions[[as.character(athlete_id)]] <- dist
  }

  log_info(paste("Built distributions for", length(athlete_distributions), "athletes"))

  # Run simulation
  sim_results <- simulate_race_positions(athlete_distributions, N_SIMULATIONS)

  # Calculate start probabilities
  start_probs <- sapply(startlist_clean$Skier, function(s) {
    get_start_probability(chrono_data, s, race$race_type)
  })

  # Apply 4-person quota per nation
  nations <- unique(startlist_clean$Nation[!is.na(startlist_clean$Nation)])
  for (nation in nations) {
    nation_mask <- startlist_clean$Nation == nation & !is.na(startlist_clean$Nation)
    if (sum(nation_mask) > 0) {
      nation_probs <- start_probs[nation_mask]
      current_sum <- sum(nation_probs, na.rm = TRUE)
      if (current_sum > 0) {
        scaling_factor <- 4 / current_sum
        scaled_probs <- nation_probs * scaling_factor
        scaled_probs <- pmin(scaled_probs, 1.0)
        start_probs[nation_mask] <- scaled_probs
      }
    }
  }

  # Combine results
  race_results <- startlist_clean %>%
    select(Skier, ID, Nation) %>%
    left_join(sim_results %>% select(athlete_id, mean_points, n_actual_races,
                                      prob_top_1, prob_top_3, prob_top_5, prob_top_10, prob_top_30),
              by = c("ID" = "athlete_id")) %>%
    mutate(
      start_prob = start_probs,
      Sex = ifelse(gender == "men", "M", "L")
    ) %>%
    arrange(desc(prob_top_1))

  # Store with metadata
  race_date_str <- format(race$Race_Date, "%b %d")
  race_name <- paste(gender, race$Distance, race$Technique)

  results_list[[race_name]] <- list(
    data = race_results,
    gender = gender,
    distance = race$Distance,
    technique = race$Technique,
    race_date = race_date_str,
    race_num = gender_race_num
  )

  log_info(paste("Top prediction:", race_results$Skier[1],
                 "Win:", round(race_results$prob_top_1[1] * 100, 1), "%"))
}

# ============================================================================
# PART 6: SAVE RESULTS TO EXCEL
# ============================================================================

# ============================================================================
# PART 6: PROCESS RELAY AND TEAM SPRINT RACES
# ============================================================================

log_info("=== PART 6: RELAY AND TEAM SPRINT PROCESSING ===")

# Get relay and team sprint races
relay_races <- champs_races %>%
  filter(Distance == "Rel", Sex %in% c("M", "L"))

ts_races <- champs_races %>%
  filter(Distance == "Ts", Sex %in% c("M", "L"))

log_info(paste("Found", nrow(relay_races), "relay races"))
log_info(paste("Found", nrow(ts_races), "team sprint races"))

# Function to calculate predicted team score for a given team assignment
# Uses leg-specific binomial models (like champs-predictions.R) to predict P(podium) or P(win) for each leg
# Returns weighted team score for optimization
# technique: for team sprint (n_members=2), specifies C or F for technique-specific prev_points
# opt_type: "podium" (default) or "win" - which model to use for optimization
calculate_team_points <- function(team_members, leg_models, chrono_data, n_members,
                                   leg_importance = NULL, technique = NULL, opt_type = "podium") {
  if (is.null(team_members) || nrow(team_members) < n_members) {
    return(list(total_points = 0, weighted_points = 0, member_points = rep(0, n_members)))
  }

  # Default to equal weights if not provided
  if (is.null(leg_importance) || length(leg_importance) != n_members) {
    leg_importance <- rep(1/n_members, n_members)
  }

  member_probs <- numeric(n_members)

  for (i in 1:n_members) {
    member <- team_members[i, , drop = FALSE]
    member_id <- member$ID
    leg_key <- paste0("leg_", i)

    # Get prev_points_weighted for this athlete based on leg technique
    if (n_members == 4) {
      # Relay: legs 1-2 classic, legs 3-4 freestyle
      if (i <= 2) {
        prev_pts <- get_race_prev_points(chrono_data, member_id, "Distance_C")
      } else {
        prev_pts <- get_race_prev_points(chrono_data, member_id, "Distance_F")
      }
    } else {
      # Team sprint: use technique-specific sprint history
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

    # Add PELO_pct columns if needed
    pelo_cols <- names(member)[grep("Pelo$", names(member))]
    for (col in pelo_cols) {
      pct_col <- paste0(col, "_pct")
      if (!pct_col %in% names(member) && col %in% names(member)) {
        max_pelo <- max(member[[col]], na.rm = TRUE)
        if (!is.na(max_pelo) && max_pelo > 0) {
          member[[pct_col]] <- member[[col]] / max_pelo
        } else {
          member[[pct_col]] <- 0.5
        }
      }
    }

    # Use leg-specific model if available
    if (!is.null(leg_models) && leg_key %in% names(leg_models$models)) {
      # Choose model based on optimization type
      if (opt_type == "win" && !is.null(leg_models$models[[leg_key]]$win_model)) {
        leg_model <- leg_models$models[[leg_key]]$win_model
      } else {
        leg_model <- leg_models$models[[leg_key]]$podium_model
        if (is.null(leg_model)) {
          leg_model <- leg_models$models[[leg_key]]$model  # backward compatibility
        }
      }
      pred <- tryCatch({
        p <- predict(leg_model, newdata = member, type = "response")
        pmax(0.01, pmin(0.99, p))
      }, error = function(e) {
        # Fallback: convert points to rough probability
        pmax(0.01, pmin(0.8, prev_pts / 100))
      })
    } else {
      # No model: use points-based probability estimate
      pred <- pmax(0.01, pmin(0.8, prev_pts / 100))
    }

    member_probs[i] <- as.numeric(pred)
  }

  # Calculate weighted team probability score
  weighted_score <- sum(member_probs * leg_importance, na.rm = TRUE)
  total_score <- sum(member_probs, na.rm = TRUE)

  return(list(
    total_points = total_score,
    weighted_points = weighted_score,
    member_points = member_probs
  ))
}

# Function to select best team for a nation by OPTIMIZING for weighted predicted score
# Uses leg-specific models (like champs-predictions.R) to predict P(podium) for each leg
# Uses leg_importance to weight each leg's contribution
# Tries all permutations and selects teams optimized for both podium and win
# technique: for team sprint (n_members=2), specifies C or F for technique-specific prev_points
# Returns a list with both podium_team and win_team
select_relay_team <- function(nation_athletes, n_members = 4,
                               leg_models = NULL, chrono_data = NULL,
                               optimize = TRUE, leg_importance = NULL, technique = NULL) {
  n_athletes <- nrow(nation_athletes)

  if (n_athletes < n_members) {
    return(NULL)  # Not enough athletes
  }

  # Default to equal weights if not provided
  if (is.null(leg_importance) || length(leg_importance) != n_members) {
    leg_importance <- rep(1/n_members, n_members)
  }

  # If optimization is disabled or no models, use simple Elo-based selection
  if (!optimize || is.null(leg_models) || length(leg_models$models) == 0) {
    # Fall back to Elo-based selection
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
      classic_athlete <- nation_athletes %>%
        arrange(desc(Classic_Elo)) %>%
        head(1)
      freestyle_athlete <- nation_athletes %>%
        filter(!ID %in% classic_athlete$ID) %>%
        arrange(desc(Freestyle_Elo)) %>%
        head(1)
      fallback_team <- bind_rows(classic_athlete, freestyle_athlete)
    }
    # Return same team for both optimizations (no model to differentiate)
    return(list(
      podium_team = fallback_team,
      win_team = fallback_team,
      podium_member_points = rep(0.5, n_members),
      win_member_points = rep(0.5, n_members),
      podium_weighted_points = 0.5,
      win_weighted_points = 0.5
    ))
  }

  # Track best for PODIUM optimization
  best_podium_weighted <- 0
  best_podium_team <- NULL
  best_podium_member_points <- NULL

  # Track best for WIN optimization
  best_win_weighted <- 0
  best_win_team <- NULL
  best_win_member_points <- NULL

  if (n_members == 4) {
    # Relay: try all combinations of 4 athletes in 4 positions
    # Limit to top 8 athletes to keep permutations manageable
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

            # Calculate for PODIUM optimization
            podium_result <- calculate_team_points(team, leg_models, chrono_data,
                                                    n_members, leg_importance, technique, opt_type = "podium")
            if (podium_result$weighted_points > best_podium_weighted) {
              best_podium_weighted <- podium_result$weighted_points
              best_podium_team <- team
              best_podium_member_points <- podium_result$member_points
            }

            # Calculate for WIN optimization
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
    # Team sprint: try all combinations of 2 athletes
    for (leg1 in 1:n_athletes) {
      for (leg2 in setdiff(1:n_athletes, leg1)) {
        team <- nation_athletes[c(leg1, leg2), ]

        # Calculate for PODIUM optimization
        podium_result <- calculate_team_points(team, leg_models, chrono_data,
                                                n_members, leg_importance, technique, opt_type = "podium")
        if (podium_result$weighted_points > best_podium_weighted) {
          best_podium_weighted <- podium_result$weighted_points
          best_podium_team <- team
          best_podium_member_points <- podium_result$member_points
        }

        # Calculate for WIN optimization
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

# ============================================================================
# HYBRID APPROACH: Build team distribution using leg-specific binomial models
# This uses the leg-specific models to predict P(team podium | athlete on leg X)
# Then converts to a score for simulation
# ============================================================================

build_team_distribution_hybrid <- function(team_members, leg_models, leg_importance,
                                            chrono_data = NULL, n_members = 4, technique = NULL) {

  if (is.null(team_members) || nrow(team_members) < n_members) {
    return(NULL)
  }

  # Prepare team members with prev_points_weighted for model prediction
  team_members_prepared <- team_members

  if (!is.null(chrono_data) && "ID" %in% names(team_members_prepared)) {
    for (i in 1:nrow(team_members_prepared)) {
      member_id <- team_members_prepared$ID[i]
      if (n_members == 4) {
        # Relay: legs 1-2 classic, legs 3-4 freestyle
        if (i <= 2) {
          team_members_prepared$prev_points_weighted[i] <- get_race_prev_points(chrono_data, member_id, "Distance_C")
        } else {
          team_members_prepared$prev_points_weighted[i] <- get_race_prev_points(chrono_data, member_id, "Distance_F")
        }
      } else {
        # Team sprint: technique-specific sprint history
        if (!is.null(technique) && technique == "C") {
          team_members_prepared$prev_points_weighted[i] <- get_race_prev_points(chrono_data, member_id, "Sprint_C")
        } else if (!is.null(technique) && technique == "F") {
          team_members_prepared$prev_points_weighted[i] <- get_race_prev_points(chrono_data, member_id, "Sprint_F")
        } else {
          team_members_prepared$prev_points_weighted[i] <- get_race_prev_points(chrono_data, member_id, "Sprint")
        }
      }
    }
    # Replace NAs/zeros with first quartile
    valid_pts <- team_members_prepared$prev_points_weighted[team_members_prepared$prev_points_weighted > 0]
    q1 <- if (length(valid_pts) > 0) quantile(valid_pts, 0.25, na.rm = TRUE) else 25
    team_members_prepared$prev_points_weighted[is.na(team_members_prepared$prev_points_weighted) |
                                                team_members_prepared$prev_points_weighted == 0] <- q1
  }

  # Prepare PELO_pct columns
  pelo_cols <- names(team_members)[grep("Pelo$", names(team_members))]
  for (col in pelo_cols) {
    pct_col <- paste0(col, "_pct")
    if (!pct_col %in% names(team_members_prepared) && col %in% names(team_members_prepared)) {
      max_pelo <- max(team_members_prepared[[col]], na.rm = TRUE)
      if (!is.na(max_pelo) && max_pelo > 0) {
        team_members_prepared[[pct_col]] <- team_members_prepared[[col]] / max_pelo
      }
    }
  }

  # Get leg-specific predictions
  leg_probs <- numeric(n_members)  # P(team podium | this athlete on this leg)
  leg_sds <- numeric(n_members)

  for (i in 1:n_members) {
    member <- team_members_prepared[i, , drop = FALSE]
    leg_key <- paste0("leg_", i)

    if (!is.null(leg_models) && leg_key %in% names(leg_models$models)) {
      leg_model_info <- leg_models$models[[leg_key]]
      leg_model <- leg_model_info$model

      # Predict P(podium) using leg-specific model
      pred_prob <- tryCatch({
        p <- predict(leg_model, newdata = member, type = "response")
        # Ensure valid probability
        pmax(0.01, pmin(0.99, p))
      }, error = function(e) {
        # Fallback to historical-based estimate
        if ("prev_points_weighted" %in% names(member)) {
          # Convert points to rough probability (higher points = higher prob)
          pts <- member$prev_points_weighted
          pmax(0.01, pmin(0.8, pts / 100))  # Scale 0-100 points to 0-0.8 prob
        } else {
          0.25  # Default ~25% podium probability
        }
      })

      leg_probs[i] <- pred_prob
      # SD based on binomial variance approximation
      leg_sds[i] <- sqrt(pred_prob * (1 - pred_prob))
    } else {
      # No leg-specific model - use historical-based estimate
      if ("prev_points_weighted" %in% names(member) && !is.na(member$prev_points_weighted)) {
        pts <- member$prev_points_weighted
        leg_probs[i] <- pmax(0.01, pmin(0.8, pts / 100))
      } else {
        leg_probs[i] <- 0.25
      }
      leg_sds[i] <- 0.2
    }
  }

  # Calculate team score as importance-weighted sum of leg probabilities
  # This gives us a "team quality" score between 0 and 1
  if (is.null(leg_importance) || length(leg_importance) != n_members) {
    leg_importance <- rep(1/n_members, n_members)
  }

  team_prob <- sum(leg_probs * leg_importance)

  # Convert probability to a score scale suitable for simulation
  # Use logit transform: score = log(p / (1-p))
  # This maps (0,1) to (-inf, inf) and preserves ranking
  team_score <- log(team_prob / (1 - team_prob))

  # Team SD from propagated uncertainty
  # Var(weighted sum) = sum(w^2 * var_i) for independent terms
  team_sd <- sqrt(sum((leg_importance * leg_sds)^2))

  # Convert to score scale (approximate)
  # d/dp log(p/(1-p)) = 1/(p*(1-p))
  score_sd <- team_sd / (team_prob * (1 - team_prob))
  # Use calibrated bounds - different for relay (4 legs) vs team sprint (2 legs)
  if (n_members == 4) {
    score_sd <- pmax(RELAY_SCORE_SD_MIN, pmin(RELAY_SCORE_SD_MAX, score_sd))
  } else {
    score_sd <- pmax(TS_SCORE_SD_MIN, pmin(TS_SCORE_SD_MAX, score_sd))
  }

  return(list(
    mean = team_score,
    sd = score_sd,
    team_prob = team_prob,  # Raw probability for reference
    leg_probs = leg_probs,
    leg_importance = leg_importance,
    member_ids = team_members$ID,
    member_names = team_members$Skier,
    member_nations = team_members$Nation,
    member_predictions = leg_probs,  # For compatibility
    member_sds = leg_sds,
    team_members = team_members
  ))
}

# Function to build team distribution (all-GAM based, with historical fallback)
# LEGACY: kept for backward compatibility, but hybrid approach is preferred
# Function to simulate team race positions
simulate_team_race <- function(team_distributions, n_simulations = N_SIMULATIONS) {
  n_teams <- length(team_distributions)
  team_names <- names(team_distributions)

  thresholds <- c(1, 3, 5, 10)
  position_counts <- matrix(0, nrow = n_teams, ncol = length(thresholds),
                            dimnames = list(team_names, paste0("top_", thresholds)))

  for (sim in 1:n_simulations) {
    # Sample points for each team
    # Note: For hybrid approach, SD is already bounded (0.3-1.0) on logit scale
    # For legacy approach (points), apply variance controls
    simulated_points <- sapply(team_distributions, function(dist) {
      if (is.null(dist)) return(0)
      # Check if this is hybrid (has team_prob) or legacy (points-based)
      if (!is.null(dist$team_prob)) {
        # Hybrid: SD already controlled, use as-is
        rnorm(1, mean = dist$mean, sd = dist$sd)
      } else {
        # Legacy: Apply variance controls like individual races
        scaled_sd <- dist$sd * SD_SCALE_FACTOR
        bounded_sd <- pmax(SD_MIN, pmin(SD_MAX * 2, scaled_sd))  # Allow slightly higher for teams
        rnorm(1, mean = dist$mean, sd = bounded_sd)
      }
    })

    # Rank teams
    ranks <- rank(-simulated_points, ties.method = "random")

    # Count positions
    for (t_idx in seq_along(thresholds)) {
      threshold <- thresholds[t_idx]
      achieved <- ranks <= threshold
      position_counts[achieved, t_idx] <- position_counts[achieved, t_idx] + 1
    }
  }

  # Convert to probabilities
  position_probs <- position_counts / n_simulations

  # Build results
  results <- data.frame(
    Nation = team_names,
    mean_points = sapply(team_distributions, function(d) if(is.null(d)) 0 else d$mean),
    prob_top_1 = position_probs[, 1],
    prob_top_3 = position_probs[, 2],
    prob_top_5 = position_probs[, 3],
    prob_top_10 = position_probs[, 4],
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(mean_points))

  return(results)
}

# Process relay races
relay_results_list <- list()

if (nrow(relay_races) > 0) {
  for (i in 1:nrow(relay_races)) {
    race <- relay_races[i, ]
    gender <- if (race$Sex == "M") "men" else "ladies"

    log_info(paste("Processing", gender, "relay"))

    if (gender == "men") {
      startlist <- men_startlist
      chrono_data <- men_chrono
      leg_importance <- men_relay_leg_importance
      relay_leg_models <- men_relay_leg_models
    } else {
      startlist <- ladies_startlist
      chrono_data <- ladies_chrono
      leg_importance <- ladies_relay_leg_importance
      relay_leg_models <- ladies_relay_leg_models
    }

    log_info(paste("Using leg importance weights:", paste(round(leg_importance * 100, 1), "%", collapse = ", ")))
    log_info(paste("Leg-specific models available:", !is.null(relay_leg_models) && length(relay_leg_models$models) > 0))

    # Build teams for each nation - OPTIMIZE using leg-specific models
    nations <- unique(startlist$Nation[!is.na(startlist$Nation)])
    team_distributions <- list()

    log_info(paste("Optimizing relay teams for", length(nations), "nations..."))

    for (nation in nations) {
      nation_athletes <- startlist %>% filter(Nation == nation)

      # Optimize team selection using leg-specific models (returns both podium and win teams)
      team_result <- select_relay_team(nation_athletes, n_members = 4,
                                        leg_models = relay_leg_models,
                                        chrono_data = chrono_data,
                                        optimize = TRUE,
                                        leg_importance = leg_importance)

      if (!is.null(team_result) && !is.null(team_result$podium_team)) {
        # Log optimization result
        log_info(paste("  ", nation, "- Podium score:", round(team_result$podium_weighted_points, 3),
                       "Win score:", round(team_result$win_weighted_points, 3)))

        # Build team distribution using podium-optimized team (for simulation)
        dist <- build_team_distribution_hybrid(team_result$podium_team, relay_leg_models, leg_importance,
                                                chrono_data = chrono_data, n_members = 4)
        # Store both teams in the distribution
        dist$podium_team <- team_result$podium_team
        dist$win_team <- team_result$win_team
        dist$podium_member_points <- team_result$podium_member_points
        dist$win_member_points <- team_result$win_member_points
        team_distributions[[nation]] <- dist
      }
    }

    log_info(paste("Built optimized distributions for", length(team_distributions), "national teams"))

    # Calculate and display LEG IMPORTANCE based on average predictions
    if (length(team_distributions) > 0) {
      log_info("=== RELAY LEG IMPORTANCE ANALYSIS ===")

      # Aggregate leg predictions across all teams
      leg_predictions <- matrix(0, nrow = length(team_distributions), ncol = 4)
      team_idx <- 1
      for (nation in names(team_distributions)) {
        dist <- team_distributions[[nation]]
        if (!is.null(dist$member_predictions) && length(dist$member_predictions) == 4) {
          leg_predictions[team_idx, ] <- dist$member_predictions
        }
        team_idx <- team_idx + 1
      }

      # Calculate average points per leg
      avg_leg_points <- colMeans(leg_predictions, na.rm = TRUE)
      total_avg_points <- sum(avg_leg_points)

      # Calculate leg importance as proportion of total
      leg_importance <- if (total_avg_points > 0) avg_leg_points / total_avg_points else rep(0.25, 4)

      log_info(paste("Average points per leg:"))
      log_info(paste("  Leg 1 (Classic):", round(avg_leg_points[1], 1), "pts (",
                     round(leg_importance[1] * 100, 1), "% importance)"))
      log_info(paste("  Leg 2 (Classic):", round(avg_leg_points[2], 1), "pts (",
                     round(leg_importance[2] * 100, 1), "% importance)"))
      log_info(paste("  Leg 3 (Freestyle):", round(avg_leg_points[3], 1), "pts (",
                     round(leg_importance[3] * 100, 1), "% importance)"))
      log_info(paste("  Leg 4 (Freestyle):", round(avg_leg_points[4], 1), "pts (",
                     round(leg_importance[4] * 100, 1), "% importance)"))
      log_info(paste("  Total average team points:", round(total_avg_points, 1)))

      # Show top 5 teams with leg breakdown
      log_info("Top 5 teams by predicted points with leg breakdown:")
      team_totals <- sapply(team_distributions, function(d) d$mean)
      top_teams <- names(sort(team_totals, decreasing = TRUE))[1:min(5, length(team_totals))]

      for (nation in top_teams) {
        dist <- team_distributions[[nation]]
        if (!is.null(dist$member_predictions)) {
          leg_str <- paste(round(dist$member_predictions, 3), collapse = " + ")
          if (use_hybrid && !is.null(dist$team_prob)) {
            log_info(paste("  ", nation, ": Leg probs:", leg_str))
            log_info(paste("       Team prob:", round(dist$team_prob, 3),
                           "| Score:", round(dist$mean, 2), "| SD:", round(dist$sd, 2)))
          } else {
            log_info(paste("  ", nation, ":", leg_str, "=", round(dist$mean, 1)))
          }
        }
      }
      log_info("=== END LEG IMPORTANCE ANALYSIS ===\n")
    }

    # Simulate relay
    if (length(team_distributions) > 1) {
      relay_results <- simulate_team_race(team_distributions, N_SIMULATIONS)

      race_name <- paste(gender, "Relay")
      relay_results_list[[race_name]] <- list(
        data = relay_results,
        gender = gender,
        race_type = "Relay",
        team_distributions = team_distributions,  # Store team compositions
        n_legs = 4
      )

      log_info(paste("Top 3 relay teams:", paste(head(relay_results$Nation, 3), collapse = ", ")))
    }
  }
}

# Process team sprint races
ts_results_list <- list()

if (nrow(ts_races) > 0) {
  for (i in 1:nrow(ts_races)) {
    race <- ts_races[i, ]
    gender <- if (race$Sex == "M") "men" else "ladies"
    technique <- race$Technique  # C or F

    log_info(paste("Processing", gender, "team sprint", technique))

    if (gender == "men") {
      startlist <- men_startlist
      chrono_data <- men_chrono
      leg_importance <- men_ts_leg_importance
      ts_leg_models_all <- men_ts_leg_models
    } else {
      startlist <- ladies_startlist
      chrono_data <- ladies_chrono
      leg_importance <- ladies_ts_leg_importance
      ts_leg_models_all <- ladies_ts_leg_models
    }

    # Get technique-specific models
    ts_leg_models <- if (!is.null(ts_leg_models_all) && technique %in% names(ts_leg_models_all)) {
      ts_leg_models_all[[technique]]
    } else {
      NULL
    }

    log_info(paste("Using team sprint leg importance weights:", paste(round(leg_importance * 100, 1), "%", collapse = ", ")))
    log_info(paste("Technique-specific models available for", technique, ":", !is.null(ts_leg_models) && length(ts_leg_models$models) > 0))

    # Build teams for each nation (2 athletes for team sprint) - OPTIMIZE using leg-specific models
    nations <- unique(startlist$Nation[!is.na(startlist$Nation)])
    team_distributions <- list()

    log_info(paste("Optimizing team sprint teams for", length(nations), "nations (technique:", technique, ")..."))

    for (nation in nations) {
      nation_athletes <- startlist %>% filter(Nation == nation)

      if (nrow(nation_athletes) >= 2) {
        # Optimize team selection using leg-specific models (returns both podium and win teams)
        team_result <- select_relay_team(nation_athletes, n_members = 2,
                                          leg_models = ts_leg_models,
                                          chrono_data = chrono_data,
                                          optimize = TRUE,
                                          leg_importance = leg_importance,
                                          technique = technique)

        if (!is.null(team_result) && !is.null(team_result$podium_team)) {
          # Log optimization result
          log_info(paste("  ", nation, "- Podium team:", paste(team_result$podium_team$Skier, collapse = " + "),
                         "score:", round(team_result$podium_weighted_points, 3)))

          # Build team distribution using podium-optimized team (for simulation)
          dist <- build_team_distribution_hybrid(team_result$podium_team, ts_leg_models, leg_importance,
                                                  chrono_data = chrono_data, n_members = 2,
                                                  technique = technique)
          # Store both teams in the distribution
          dist$podium_team <- team_result$podium_team
          dist$win_team <- team_result$win_team
          dist$podium_member_points <- team_result$podium_member_points
          dist$win_member_points <- team_result$win_member_points
          team_distributions[[nation]] <- dist
        }
      }
    }

    log_info(paste("Built optimized distributions for", length(team_distributions), "team sprint teams"))

    # Calculate and display LEG IMPORTANCE for team sprint
    if (length(team_distributions) > 0) {
      log_info(paste("=== TEAM SPRINT LEG IMPORTANCE ANALYSIS (", technique, ") ==="))

      # Aggregate leg predictions across all teams
      leg_predictions <- matrix(0, nrow = length(team_distributions), ncol = 2)
      team_idx <- 1
      for (nation in names(team_distributions)) {
        dist <- team_distributions[[nation]]
        if (!is.null(dist$member_predictions) && length(dist$member_predictions) == 2) {
          leg_predictions[team_idx, ] <- dist$member_predictions
        }
        team_idx <- team_idx + 1
      }

      # Calculate average points per leg
      avg_leg_points <- colMeans(leg_predictions, na.rm = TRUE)
      total_avg_points <- sum(avg_leg_points)

      # Calculate leg importance as proportion of total
      leg_importance <- if (total_avg_points > 0) avg_leg_points / total_avg_points else rep(0.5, 2)

      log_info(paste("Average points per leg:"))
      log_info(paste("  Leg 1:", round(avg_leg_points[1], 1), "pts (",
                     round(leg_importance[1] * 100, 1), "% importance)"))
      log_info(paste("  Leg 2:", round(avg_leg_points[2], 1), "pts (",
                     round(leg_importance[2] * 100, 1), "% importance)"))
      log_info(paste("  Total average team points:", round(total_avg_points, 1)))

      # Show top 5 teams with leg breakdown
      log_info("Top 5 teams by predicted points with leg breakdown:")
      team_totals <- sapply(team_distributions, function(d) d$mean)
      top_teams <- names(sort(team_totals, decreasing = TRUE))[1:min(5, length(team_totals))]

      for (nation in top_teams) {
        dist <- team_distributions[[nation]]
        if (!is.null(dist$member_predictions)) {
          leg_str <- paste(round(dist$member_predictions, 3), collapse = " + ")
          athletes <- paste(dist$member_names, collapse = " + ")
          log_info(paste("  ", nation, ":", athletes))
          if (use_hybrid && !is.null(dist$team_prob)) {
            log_info(paste("       Leg probs:", leg_str, "| Team prob:", round(dist$team_prob, 3),
                           "| Score:", round(dist$mean, 2), "| SD:", round(dist$sd, 2)))
          } else {
            log_info(paste("       Points:", leg_str, "=", round(dist$mean, 1)))
          }
        }
      }
      log_info("=== END TEAM SPRINT LEG IMPORTANCE ANALYSIS ===\n")
    }

    # Simulate team sprint
    if (length(team_distributions) > 1) {
      ts_results <- simulate_team_race(team_distributions, N_SIMULATIONS)

      race_name <- paste(gender, "Team Sprint", technique)
      ts_results_list[[race_name]] <- list(
        data = ts_results,
        gender = gender,
        race_type = paste("Team Sprint", technique),
        team_distributions = team_distributions,  # Store team compositions
        n_legs = 2,
        technique = technique
      )

      log_info(paste("Top 3 team sprint teams:", paste(head(ts_results$Nation, 3), collapse = ", ")))
    }
  }
}

log_info("Relay and Team Sprint processing complete")

# ============================================================================
# PART 7: SAVE RESULTS
# ============================================================================

log_info("=== PART 7: SAVE RESULTS ===")

current_year <- format(Sys.Date(), "%Y")
output_dir <- file.path("~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions", current_year)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Helper to expand race name
expand_race_name <- function(dist, tech) {
  tech_full <- switch(tech, "P" = "Skiathlon", "C" = "Classic", "F" = "Freestyle", tech)
  if (dist == "Sprint") {
    return(paste("Sprint", tech_full))
  } else {
    return(paste0(dist, "km ", tech_full))
  }
}

# Separate by gender
men_results <- list()
ladies_results <- list()

for (race_name in names(results_list)) {
  entry <- results_list[[race_name]]
  data <- entry$data

  # Format for output (convert to percentages)
  output_data <- data %>%
    mutate(
      Start = round(start_prob * 100, 1),
      Win = round(prob_top_1 * 100, 1),
      Podium = round(prob_top_3 * 100, 1),
      Top5 = round(prob_top_5 * 100, 1),
      `Top-10` = round(prob_top_10 * 100, 1),
      `Top-30` = round(prob_top_30 * 100, 1)
    ) %>%
    select(Skier, ID, Sex, Nation, Start, Win, Podium, Top5, `Top-10`, `Top-30`) %>%
    arrange(desc(Win))

  race_type <- if (entry$technique == "") entry$distance else expand_race_name(entry$distance, entry$technique)
  tab_name <- paste0(entry$race_num, ". ", race_type, " - ", entry$race_date)

  if (entry$gender == "men") {
    men_results[[tab_name]] <- output_data
  } else {
    ladies_results[[tab_name]] <- output_data
  }
}

# Save
if (length(men_results) > 0) {
  men_file <- file.path(output_dir, "men_position_probabilities.xlsx")
  write.xlsx(men_results, men_file)
  log_info(paste("Saved men's results to", men_file))
}

if (length(ladies_results) > 0) {
  ladies_file <- file.path(output_dir, "ladies_position_probabilities.xlsx")
  write.xlsx(ladies_results, ladies_file)
  log_info(paste("Saved ladies' results to", ladies_file))
}

# ============================================================================
# CREATE NATIONS EXCEL FILE (for Nations blog post)
# Split by gender - nations with 4+ athletes per gender get their own sheet
# ============================================================================
log_info("=== Creating Nations Excel File ===")

# Combine men's results into one data frame with Race and Gender columns
men_individual_results <- data.frame()
for (race_name in names(men_results)) {
  race_data <- men_results[[race_name]]
  # Extract just the race type (remove "N. " prefix and " - Mon DD" suffix)
  race_type_only <- sub("^\\d+\\. ", "", race_name)  # Remove "1. " prefix
  race_type_only <- sub(" - .*$", "", race_type_only)  # Remove " - Feb 12" suffix
  race_data$Race <- race_type_only
  race_data$Gender <- "Men"
  men_individual_results <- bind_rows(men_individual_results, race_data)
}

# Combine ladies' results into one data frame with Race and Gender columns
ladies_individual_results <- data.frame()
for (race_name in names(ladies_results)) {
  race_data <- ladies_results[[race_name]]
  # Extract just the race type (remove "N. " prefix and " - Mon DD" suffix)
  race_type_only <- sub("^\\d+\\. ", "", race_name)  # Remove "1. " prefix
  race_type_only <- sub(" - .*$", "", race_type_only)  # Remove " - Feb 12" suffix
  race_data$Race <- race_type_only
  race_data$Gender <- "Ladies"
  ladies_individual_results <- bind_rows(ladies_individual_results, race_data)
}

log_info(paste("Combined", nrow(men_individual_results), "men's rows"))
log_info(paste("Combined", nrow(ladies_individual_results), "ladies' rows"))

# Count unique athletes per nation per gender
men_nation_counts <- men_individual_results %>%
  filter(Start > 0) %>%
  group_by(Nation) %>%
  summarise(n_athletes = n_distinct(ID), .groups = "drop")

ladies_nation_counts <- ladies_individual_results %>%
  filter(Start > 0) %>%
  group_by(Nation) %>%
  summarise(n_athletes = n_distinct(ID), .groups = "drop")

# Nations with 4+ athletes per gender
men_main_nations <- men_nation_counts %>% filter(n_athletes >= 4) %>% pull(Nation) %>% sort()
ladies_main_nations <- ladies_nation_counts %>% filter(n_athletes >= 4) %>% pull(Nation) %>% sort()

# Nations with <4 athletes per gender
men_other_nations <- men_nation_counts %>% filter(n_athletes < 4) %>% pull(Nation)
ladies_other_nations <- ladies_nation_counts %>% filter(n_athletes < 4) %>% pull(Nation)

log_info(paste("Men nations with 4+ athletes:", paste(men_main_nations, collapse = ", ")))
log_info(paste("Ladies nations with 4+ athletes:", paste(ladies_main_nations, collapse = ", ")))

# Select and rename columns for reader-friendly output
select_and_rename_cols <- function(df, include_nation = FALSE) {
  if (include_nation) {
    df %>%
      select(Athlete = Skier, ID, Race, Nation,
             Start, Win, Podium, Top5, `Top-10`, `Top-30`) %>%
      arrange(Race, Nation, desc(Start))
  } else {
    df %>%
      select(Athlete = Skier, ID, Race,
             Start, Win, Podium, Top5, `Top-10`, `Top-30`) %>%
      arrange(Race, desc(Start))
  }
}

# Create nations workbook
nations_wb <- list()

# Process men's main nations (alphabetical order)
for (nation in men_main_nations) {
  nation_data <- men_individual_results %>%
    filter(Nation == nation, Start > 0)

  if (nrow(nation_data) > 0) {
    sheet_name <- paste(nation, "Men")
    nations_wb[[sheet_name]] <- select_and_rename_cols(nation_data, include_nation = FALSE)
    log_info(paste("Added", sheet_name, "sheet with", nrow(nation_data), "rows"))
  }
}

# Create "Other Men" sheet for nations with <4 male athletes
men_other_data <- men_individual_results %>%
  filter(Nation %in% men_other_nations, Start > 0)

if (nrow(men_other_data) > 0) {
  nations_wb[["Other Men"]] <- select_and_rename_cols(men_other_data, include_nation = TRUE)
  log_info(paste("Added Other Men sheet with", nrow(men_other_data), "rows from",
                 length(unique(men_other_data$Nation)), "nations"))
}

# Process ladies' main nations (alphabetical order)
for (nation in ladies_main_nations) {
  nation_data <- ladies_individual_results %>%
    filter(Nation == nation, Start > 0)

  if (nrow(nation_data) > 0) {
    sheet_name <- paste(nation, "Ladies")
    nations_wb[[sheet_name]] <- select_and_rename_cols(nation_data, include_nation = FALSE)
    log_info(paste("Added", sheet_name, "sheet with", nrow(nation_data), "rows"))
  }
}

# Create "Other Ladies" sheet for nations with <4 female athletes
ladies_other_data <- ladies_individual_results %>%
  filter(Nation %in% ladies_other_nations, Start > 0)

if (nrow(ladies_other_data) > 0) {
  nations_wb[["Other Ladies"]] <- select_and_rename_cols(ladies_other_data, include_nation = TRUE)
  log_info(paste("Added Other Ladies sheet with", nrow(ladies_other_data), "rows from",
                 length(unique(ladies_other_data$Nation)), "nations"))
}

# Combine all results for summary
all_individual_results <- bind_rows(men_individual_results, ladies_individual_results)

# Create Summary sheet (split by gender)
# Divide by 100 to convert from percentage to expected count
summary_data <- all_individual_results %>%
  filter(Start > 0) %>%
  mutate(
    Nation_Group = case_when(
      Gender == "Men" & Nation %in% men_main_nations ~ Nation,
      Gender == "Ladies" & Nation %in% ladies_main_nations ~ Nation,
      TRUE ~ "Other"
    )
  ) %>%
  group_by(Gender, Nation_Group) %>%
  summarise(
    `Total Win` = round(sum(Win, na.rm = TRUE) / 100, 2),
    `Total Podium` = round(sum(Podium, na.rm = TRUE) / 100, 2),
    `Total Top-10` = round(sum(`Top-10`, na.rm = TRUE) / 100, 2),
    `Athletes` = n_distinct(ID),
    .groups = "drop"
  ) %>%
  rename(Nation = Nation_Group) %>%
  # Put main nations first (alphabetical), then Other at bottom
  mutate(sort_order = ifelse(Nation == "Other", 2, 1)) %>%
  arrange(Gender, sort_order, Nation) %>%
  select(-sort_order)

nations_wb[["Summary"]] <- summary_data
log_info("Added Summary sheet")

# Save nations Excel file
if (length(nations_wb) > 0) {
  nations_file <- file.path(output_dir, "nations_individual.xlsx")
  write.xlsx(nations_wb, nations_file)
  log_info(paste("Saved nations individual results to", nations_file))
  log_info(paste("Nations tabs:", paste(names(nations_wb), collapse = ", ")))
}

# ============================================================================
# RELAY AND TEAM SPRINT OUTPUTS - PRODUCTION FORMAT
# Matches the format of champs-predictions.R for blog compatibility
# ============================================================================

log_info("=== Creating Production-Format Relay/Team Sprint Outputs ===")

# Function to format relay/team sprint predictions for Excel (production format)
# This matches the format from champs-predictions.R with per-leg threshold probabilities
# opt_type: "podium" (default) or "win" - which team roster to use
format_predictions_for_excel <- function(results_list, n_legs_expected, opt_type = "podium") {
  excel_data <- data.frame()

  for (race_name in names(results_list)) {
    entry <- results_list[[race_name]]
    if (is.null(entry$team_distributions)) next

    sim_results <- entry$data
    team_dists <- entry$team_distributions
    n_legs <- entry$n_legs

    for (nation in names(team_dists)) {
      dist <- team_dists[[nation]]
      if (is.null(dist)) next

      # Choose which team to use based on optimization type
      if (opt_type == "win" && !is.null(dist$win_team)) {
        team_members <- dist$win_team
        member_points <- dist$win_member_points
      } else {
        team_members <- if (!is.null(dist$podium_team)) dist$podium_team else dist$team_members
        member_points <- if (!is.null(dist$podium_member_points)) dist$podium_member_points else dist$member_predictions
      }

      if (is.null(team_members)) next

      # Get team probabilities from simulation results
      nation_row <- sim_results[sim_results$Nation == nation, ]
      if (nrow(nation_row) == 0) next

      team_win <- nation_row$prob_top_1
      team_podium <- nation_row$prob_top_3
      team_top5 <- nation_row$prob_top_5
      team_top10 <- nation_row$prob_top_10

      # Create one row per leg
      for (leg in 1:n_legs) {
        if (leg > nrow(team_members)) next

        # Get leg probability from model (use member_points from appropriate optimization)
        leg_podium <- if (!is.null(member_points) && length(member_points) >= leg) member_points[leg] else NA

        # Scale leg probabilities based on team-level threshold ratios
        if (!is.na(leg_podium) && !is.na(team_podium) && team_podium > 0) {
          leg_win <- min(1.0, leg_podium * (team_win / team_podium))
          leg_top5 <- min(1.0, leg_podium * (team_top5 / team_podium))
          leg_top10 <- min(1.0, leg_podium * (team_top10 / team_podium))
        } else {
          leg_win <- NA
          leg_top5 <- NA
          leg_top10 <- NA
        }

        row_data <- data.frame(
          Country = nation,
          Leg = leg,
          Athlete = team_members$Skier[leg],
          Nation = team_members$Nation[leg],
          ID = team_members$ID[leg],
          `Leg Win` = round(leg_win, 4),
          `Leg Podium` = round(leg_podium, 4),
          `Leg Top5` = round(leg_top5, 4),
          `Leg Top-10` = round(leg_top10, 4),
          `Team Win` = round(team_win, 4),
          `Team Podium` = round(team_podium, 4),
          `Team Top5` = round(team_top5, 4),
          `Team Top-10` = round(team_top10, 4),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        excel_data <- rbind(excel_data, row_data)
      }
    }
  }

  if (nrow(excel_data) > 0) {
    # Sort by Team Win descending, then Country, then Leg
    excel_data <- excel_data %>%
      arrange(desc(`Team Win`), Country, Leg)
  }

  return(excel_data)
}

# ============================================================================
# CREATE RELAY FINAL PREDICTIONS (relay_final_predictions.xlsx)
# ============================================================================

log_info("=== Creating relay_final_predictions.xlsx ===")

men_relay_excel <- NULL
ladies_relay_excel <- NULL

for (race_name in names(relay_results_list)) {
  entry <- relay_results_list[[race_name]]
  relay_excel <- format_predictions_for_excel(list(relay = entry), 4)

  if (entry$gender == "men") {
    men_relay_excel <- relay_excel
  } else {
    ladies_relay_excel <- relay_excel
  }
}

# Save relay final predictions file
relay_final_wb <- list()
if (!is.null(men_relay_excel) && nrow(men_relay_excel) > 0) {
  relay_final_wb[["Men All Thresholds Final"]] <- men_relay_excel
  log_info(paste("Added Men All Thresholds Final with", nrow(men_relay_excel), "rows"))
}
if (!is.null(ladies_relay_excel) && nrow(ladies_relay_excel) > 0) {
  relay_final_wb[["Ladies All Thresholds Final"]] <- ladies_relay_excel
  log_info(paste("Added Ladies All Thresholds Final with", nrow(ladies_relay_excel), "rows"))
}

if (length(relay_final_wb) > 0) {
  relay_final_file <- file.path(output_dir, "relay_final_predictions.xlsx")
  write.xlsx(relay_final_wb, relay_final_file)
  log_info(paste("Saved relay final predictions to", relay_final_file))
}

# ============================================================================
# CREATE TEAM SPRINT FINAL PREDICTIONS (team_sprint_final_predictions.xlsx)
# ============================================================================

log_info("=== Creating team_sprint_final_predictions.xlsx ===")

men_ts_excel <- NULL
ladies_ts_excel <- NULL

for (race_name in names(ts_results_list)) {
  entry <- ts_results_list[[race_name]]
  ts_excel <- format_predictions_for_excel(list(ts = entry), 2)

  if (entry$gender == "men") {
    if (is.null(men_ts_excel)) {
      men_ts_excel <- ts_excel
    } else {
      men_ts_excel <- rbind(men_ts_excel, ts_excel)
    }
  } else {
    if (is.null(ladies_ts_excel)) {
      ladies_ts_excel <- ts_excel
    } else {
      ladies_ts_excel <- rbind(ladies_ts_excel, ts_excel)
    }
  }
}

# Save team sprint final predictions file
ts_final_wb <- list()
if (!is.null(men_ts_excel) && nrow(men_ts_excel) > 0) {
  ts_final_wb[["Men All Thresholds Final"]] <- men_ts_excel
  log_info(paste("Added Men All Thresholds Final with", nrow(men_ts_excel), "rows"))
}
if (!is.null(ladies_ts_excel) && nrow(ladies_ts_excel) > 0) {
  ts_final_wb[["Ladies All Thresholds Final"]] <- ladies_ts_excel
  log_info(paste("Added Ladies All Thresholds Final with", nrow(ladies_ts_excel), "rows"))
}

if (length(ts_final_wb) > 0) {
  ts_final_file <- file.path(output_dir, "team_sprint_final_predictions.xlsx")
  write.xlsx(ts_final_wb, ts_final_file)
  log_info(paste("Saved team sprint final predictions to", ts_final_file))
}

# ============================================================================
# CREATE NATIONS RELAY PODIUM FILE (nations_relay_podium.xlsx)
# Per-nation sheets with relay team breakdown
# ============================================================================

log_info("=== Creating nations_relay_podium.xlsx ===")

# Function to create nations relay workbook with per-nation sheets
create_nations_relay_workbook <- function(men_data, ladies_data) {
  nations_wb <- list()

  # Process men's relay nations (alphabetical order)
  if (!is.null(men_data) && nrow(men_data) > 0) {
    men_nations <- unique(men_data$Country)
    for (nation in sort(men_nations)) {
      nation_data <- men_data %>%
        filter(Country == nation) %>%
        select(Athlete, ID, Nation, Leg, `Leg Win`, `Leg Podium`, `Leg Top5`, `Leg Top-10`,
               `Team Win`, `Team Podium`, `Team Top5`, `Team Top-10`) %>%
        arrange(Leg)

      if (nrow(nation_data) > 0) {
        sheet_name <- paste(nation, "Men")
        nations_wb[[sheet_name]] <- nation_data
      }
    }
  }

  # Process ladies' relay nations (alphabetical order)
  if (!is.null(ladies_data) && nrow(ladies_data) > 0) {
    ladies_nations <- unique(ladies_data$Country)
    for (nation in sort(ladies_nations)) {
      nation_data <- ladies_data %>%
        filter(Country == nation) %>%
        select(Athlete, ID, Nation, Leg, `Leg Win`, `Leg Podium`, `Leg Top5`, `Leg Top-10`,
               `Team Win`, `Team Podium`, `Team Top5`, `Team Top-10`) %>%
        arrange(Leg)

      if (nrow(nation_data) > 0) {
        sheet_name <- paste(nation, "Ladies")
        nations_wb[[sheet_name]] <- nation_data
      }
    }
  }

  # Create Summary sheet
  all_data <- bind_rows(
    if (!is.null(men_data) && nrow(men_data) > 0) men_data %>% mutate(Gender = "Men") else NULL,
    if (!is.null(ladies_data) && nrow(ladies_data) > 0) ladies_data %>% mutate(Gender = "Ladies") else NULL
  )

  if (nrow(all_data) > 0) {
    summary_data <- all_data %>%
      group_by(Gender, Country) %>%
      summarise(
        `Team Win` = round(first(`Team Win`), 4),
        `Team Podium` = round(first(`Team Podium`), 4),
        `Team Top5` = round(first(`Team Top5`), 4),
        `Team Top-10` = round(first(`Team Top-10`), 4),
        .groups = "drop"
      ) %>%
      rename(Nation = Country) %>%
      arrange(Gender, desc(`Team Win`))

    nations_wb[["Summary"]] <- summary_data
  }

  return(nations_wb)
}

nations_relay_podium_wb <- create_nations_relay_workbook(men_relay_excel, ladies_relay_excel)

if (length(nations_relay_podium_wb) > 0) {
  nations_relay_podium_file <- file.path(output_dir, "nations_relay_podium.xlsx")
  write.xlsx(nations_relay_podium_wb, nations_relay_podium_file)
  log_info(paste("Saved nations relay podium to", nations_relay_podium_file))
  log_info(paste("Tabs:", paste(names(nations_relay_podium_wb), collapse = ", ")))
}

# ============================================================================
# CREATE NATIONS TEAM SPRINT PODIUM FILE (nations_ts_podium.xlsx)
# Per-nation sheets with team sprint team breakdown
# ============================================================================

log_info("=== Creating nations_ts_podium.xlsx ===")

# Reuse the same function for team sprint
nations_ts_podium_wb <- create_nations_relay_workbook(men_ts_excel, ladies_ts_excel)

if (length(nations_ts_podium_wb) > 0) {
  nations_ts_podium_file <- file.path(output_dir, "nations_ts_podium.xlsx")
  write.xlsx(nations_ts_podium_wb, nations_ts_podium_file)
  log_info(paste("Saved nations team sprint podium to", nations_ts_podium_file))
  log_info(paste("Tabs:", paste(names(nations_ts_podium_wb), collapse = ", ")))
}

# ============================================================================
# CREATE WIN-OPTIMIZED OUTPUT FILES
# Same format as podium-optimized but using win-optimized team rosters
# ============================================================================

log_info("=== Creating win-optimized output files ===")

# Generate win-optimized relay data
men_relay_win_excel <- NULL
ladies_relay_win_excel <- NULL

for (race_name in names(relay_results_list)) {
  entry <- relay_results_list[[race_name]]
  relay_win_excel <- format_predictions_for_excel(list(relay = entry), 4, opt_type = "win")

  if (entry$gender == "men") {
    men_relay_win_excel <- relay_win_excel
  } else {
    ladies_relay_win_excel <- relay_win_excel
  }
}

# Create nations_relay_win.xlsx
nations_relay_win_wb <- create_nations_relay_workbook(men_relay_win_excel, ladies_relay_win_excel)

if (length(nations_relay_win_wb) > 0) {
  nations_relay_win_file <- file.path(output_dir, "nations_relay_win.xlsx")
  write.xlsx(nations_relay_win_wb, nations_relay_win_file)
  log_info(paste("Saved win-optimized nations relay to", nations_relay_win_file))
  log_info(paste("Tabs:", paste(names(nations_relay_win_wb), collapse = ", ")))
}

# Generate win-optimized team sprint data
men_ts_win_excel <- NULL
ladies_ts_win_excel <- NULL

for (race_name in names(ts_results_list)) {
  entry <- ts_results_list[[race_name]]
  ts_win_excel <- format_predictions_for_excel(list(ts = entry), 2, opt_type = "win")

  if (entry$gender == "men") {
    if (is.null(men_ts_win_excel)) {
      men_ts_win_excel <- ts_win_excel
    } else {
      men_ts_win_excel <- rbind(men_ts_win_excel, ts_win_excel)
    }
  } else {
    if (is.null(ladies_ts_win_excel)) {
      ladies_ts_win_excel <- ts_win_excel
    } else {
      ladies_ts_win_excel <- rbind(ladies_ts_win_excel, ts_win_excel)
    }
  }
}

# Create nations_ts_win.xlsx
nations_ts_win_wb <- create_nations_relay_workbook(men_ts_win_excel, ladies_ts_win_excel)

if (length(nations_ts_win_wb) > 0) {
  nations_ts_win_file <- file.path(output_dir, "nations_ts_win.xlsx")
  write.xlsx(nations_ts_win_wb, nations_ts_win_file)
  log_info(paste("Saved win-optimized nations team sprint to", nations_ts_win_file))
  log_info(paste("Tabs:", paste(names(nations_ts_win_wb), collapse = ", ")))
}

log_info("=== SIMULATION-BASED PREDICTIONS COMPLETE ===")
