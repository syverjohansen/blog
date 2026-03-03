# Biathlon Race Picks - Monte Carlo Simulation
# Simulation-based approach for individual and relay race predictions
#
# Features:
# - Monte Carlo simulation (10,000 iterations)
# - Exponential decay weighting for historical performance
# - Discipline-specific distributions (Sprint, Individual, Pursuit, Mass Start)
# - Relay support: Relay, Mixed Relay, Single Mixed Relay
# - Position probability output (Win, Podium, Top-5, Top-10, Top-30)

library(dplyr)
library(tidyr)
library(openxlsx)
library(mgcv)
library(leaps)
library(logger)
library(lubridate)

# ============================================================================
# CONFIGURATION
# ============================================================================

# Load environment variables
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
        value <- gsub("^[\"']|[\"']$", "", value)
        do.call(Sys.setenv, setNames(list(value), key))
      }
    }
    return(TRUE)
  }
  return(FALSE)
}

load_env()
TEST_MODE <- tolower(Sys.getenv("TEST_MODE", "false")) == "true"

# Simulation parameters
N_SIMULATIONS <- 10000                # Number of Monte Carlo iterations
DECAY_LAMBDA <- 0.002                 # Exponential decay rate (0.002 = 50% weight after 1 year)
SD_SCALE_FACTOR <- 0.77               # Multiply all SDs (lower = favorites win more)
SD_MIN <- 4                           # Minimum SD
SD_MAX <- 16                          # Maximum SD

# GAM parameters for athletes with insufficient history
N_HISTORY_REQUIRED <- 10              # Target number of historical races per athlete
GAM_FILL_WEIGHT_FACTOR <- 0.25        # Weight multiplier for GAM-filled history slots

# Position thresholds
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)  # Win, Podium, Top-5, Top-10, Top-30

# Points systems for Biathlon
regular_points <- c(90, 75, 65, 55, 50, 45, 41, 37, 34, 31, 30, 29, 28, 27, 26,
                    25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11,
                    10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
mass_start_points <- c(90, 75, 65, 55, 50, 45, 41, 37, 34, 31, 30, 29, 28, 27, 26,
                       25, 24, 23, 22, 21, 20, 18, 16, 14, 12, 10, 8, 6, 4, 2)

# Discipline definitions
INDIVIDUAL_DISCIPLINES <- c("Individual")
SPRINT_DISCIPLINES <- c("Sprint")
PURSUIT_DISCIPLINES <- c("Pursuit")
MASS_START_DISCIPLINES <- c("Mass Start")
RELAY_DISCIPLINES <- c("Relay", "Mixed Relay", "Single Mixed Relay")

# Relay variance parameters
RELAY_SD_SCALE_FACTOR <- 0.8
RELAY_SD_MIN <- 3
RELAY_SD_MAX <- 12

# ============================================================================
# LOGGING SETUP
# ============================================================================

log_dir <- "~/ski/elo/python/biathlon/polars/excel365/race-picks-simulation"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "simulation.log")))
log_info("=== BIATHLON RACE-PICKS-SIMULATION.R STARTED ===")
log_info(paste("TEST_MODE:", TEST_MODE))

# ============================================================================
# DATA LOADING
# ============================================================================

log_info("Loading data files...")

base_path <- "~/ski/elo/python/biathlon/polars/excel365"

# Load races with error handling
races_file <- if(TEST_MODE) {
  file.path(base_path, "test_races.csv")
} else {
  file.path(base_path, "races.csv")
}

races <- tryCatch({
  df <- read.csv(races_file, stringsAsFactors = FALSE)
  if (nrow(df) == 0) {
    log_error(paste("Races file is empty:", races_file))
    quit(save = "no", status = 1)
  }
  log_info(paste("Loaded races from:", races_file))
  df
}, error = function(e) {
  log_error(paste("Failed to load races file:", races_file, "-", e$message))
  quit(save = "no", status = 1)
})

# Get today's date
current_date <- as.Date(format(Sys.time(), tz = "UTC"))
today_mmddyyyy <- format(current_date, "%m/%d/%Y")
log_info(paste("Current date:", current_date, "(", today_mmddyyyy, ")"))

# Filter to today's races
today_races <- races %>%
  filter(Date == today_mmddyyyy)

# Try short year format if no races found
if(nrow(today_races) == 0) {
  today_short <- format(current_date, "%m/%d/%y")
  today_races <- races %>%
    filter(Date == today_short)
}

if(nrow(today_races) == 0) {
  log_info("No races found for today. Exiting.")
  quit(save = "no", status = 0)
}

log_info(paste("Found", nrow(today_races), "races for today"))

# Filter to individual races only (exclude relays)
individual_races <- today_races %>%
  filter(!RaceType %in% c("Relay", "Mixed Relay", "Single Mixed Relay"))

# Filter to relay races
men_relays <- today_races %>% filter(RaceType == "Relay", Sex == "M")
ladies_relays <- today_races %>% filter(RaceType == "Relay", Sex == "L")
mixed_relays <- today_races %>% filter(RaceType == "Mixed Relay")
single_mixed_relays <- today_races %>% filter(RaceType == "Single Mixed Relay")

has_individual <- nrow(individual_races) > 0
has_relay <- nrow(men_relays) + nrow(ladies_relays) + nrow(mixed_relays) + nrow(single_mixed_relays) > 0

if(!has_individual && !has_relay) {
  log_info("No individual or relay races found for today. Exiting.")
  quit(save = "no", status = 0)
}

log_info(paste("Relays found - Men:", nrow(men_relays), "| Ladies:", nrow(ladies_relays),
               "| Mixed:", nrow(mixed_relays), "| Single Mixed:", nrow(single_mixed_relays)))

# Separate by gender
men_races <- individual_races %>% filter(Sex == "M")
ladies_races <- individual_races %>% filter(Sex == "L")

log_info(paste("Men's races:", nrow(men_races), "| Ladies' races:", nrow(ladies_races)))

# Load chronological data with error handling
men_chrono <- tryCatch({
  df <- read.csv(file.path(base_path, "men_chrono.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded men_chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_error(paste("Failed to load men_chrono.csv:", e$message))
  data.frame()
})

ladies_chrono <- tryCatch({
  df <- read.csv(file.path(base_path, "ladies_chrono.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded ladies_chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_error(paste("Failed to load ladies_chrono.csv:", e$message))
  data.frame()
})

# Load startlists with validation
men_startlist <- tryCatch({
  df <- read.csv(file.path(base_path, "startlist_races_men.csv"), stringsAsFactors = FALSE)
  required_cols <- c("ID", "Skier", "Nation")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    log_warn(paste("Men's startlist missing columns:", paste(missing_cols, collapse = ", ")))
  }
  df
}, error = function(e) {
  log_warn(paste("Could not load men's startlist:", e$message))
  data.frame()
})

ladies_startlist <- tryCatch({
  df <- read.csv(file.path(base_path, "startlist_races_ladies.csv"), stringsAsFactors = FALSE)
  required_cols <- c("ID", "Skier", "Nation")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    log_warn(paste("Ladies' startlist missing columns:", paste(missing_cols, collapse = ", ")))
  }
  df
}, error = function(e) {
  log_warn(paste("Could not load ladies' startlist:", e$message))
  data.frame()
})

log_info(paste("Loaded startlists - Men:", nrow(men_startlist), "| Ladies:", nrow(ladies_startlist)))

# Load relay startlists
relay_base_path <- "~/ski/elo/python/biathlon/polars/relay/excel365"

men_relay_startlist <- tryCatch({
  df <- read.csv(file.path(relay_base_path, "startlist_relay_races_teams_men.csv"), stringsAsFactors = FALSE)
  log_info(paste("Loaded men's relay startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No men's relay startlist:", e$message))
  data.frame()
})

ladies_relay_startlist <- tryCatch({
  df <- read.csv(file.path(relay_base_path, "startlist_relay_races_teams_ladies.csv"), stringsAsFactors = FALSE)
  log_info(paste("Loaded ladies' relay startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No ladies' relay startlist:", e$message))
  data.frame()
})

mixed_relay_startlist <- tryCatch({
  df <- read.csv(file.path(relay_base_path, "startlist_mixed_relay_races_teams.csv"), stringsAsFactors = FALSE)
  log_info(paste("Loaded mixed relay startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No mixed relay startlist:", e$message))
  data.frame()
})

single_mixed_relay_startlist <- tryCatch({
  df <- read.csv(file.path(relay_base_path, "startlist_single_mixed_relay_races_teams.csv"), stringsAsFactors = FALSE)
  log_info(paste("Loaded single mixed relay startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No single mixed relay startlist:", e$message))
  data.frame()
})

# Load relay chronological data
men_relay_chrono <- tryCatch({
  df <- read.csv(file.path(relay_base_path, "men_relay_chrono.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded men's relay chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_info(paste("No men's relay chrono:", e$message))
  data.frame()
})

ladies_relay_chrono <- tryCatch({
  df <- read.csv(file.path(relay_base_path, "ladies_relay_chrono.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded ladies' relay chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_info(paste("No ladies' relay chrono:", e$message))
  data.frame()
})

mixed_relay_chrono <- tryCatch({
  df <- read.csv(file.path(relay_base_path, "mixed_relay_chrono.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded mixed relay chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_info(paste("No mixed relay chrono:", e$message))
  data.frame()
})

single_mixed_relay_chrono <- tryCatch({
  df <- read.csv(file.path(relay_base_path, "single_mixed_relay_chrono.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded single mixed relay chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_info(paste("No single mixed relay chrono:", e$message))
  data.frame()
})

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Get points from place
get_points <- function(place, race_type = "Sprint") {
  points_list <- if (race_type %in% MASS_START_DISCIPLINES) mass_start_points else regular_points
  if (is.na(place) || place < 1 || place > length(points_list)) {
    return(0)
  }
  return(points_list[place])
}

# Replace NA with first quartile (for feature preparation)
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Filter features to keep only those with positive coefficients
filter_positive_coefficients <- function(data, response_var, candidate_vars, family = "gaussian") {
  if (length(candidate_vars) == 0) return(character(0))
  current_vars <- candidate_vars
  for (iter in 1:length(candidate_vars)) {
    if (length(current_vars) == 0) break
    formula_str <- paste(response_var, "~", paste(current_vars, collapse = " + "))
    model <- tryCatch({
      lm(as.formula(formula_str), data = data)
    }, error = function(e) NULL)
    if (is.null(model)) break
    coefs <- coef(model)
    coefs <- coefs[names(coefs) != "(Intercept)"]
    negative_vars <- names(coefs[coefs < 0])
    if (length(negative_vars) == 0) break
    current_vars <- setdiff(current_vars, negative_vars)
  }
  return(current_vars)
}

# Get race-type-specific explanatory variables for GAM
get_explanatory_vars <- function(race_type) {
  base_vars <- c("prev_points_weighted", "Pelo_pct")
  if (race_type %in% SPRINT_DISCIPLINES) {
    return(c(base_vars, "Sprint_Pelo_pct"))
  } else if (race_type %in% INDIVIDUAL_DISCIPLINES) {
    return(c(base_vars, "Individual_Pelo_pct"))
  } else if (race_type %in% PURSUIT_DISCIPLINES) {
    return(c(base_vars, "Pursuit_Pelo_pct", "Sprint_Pelo_pct"))
  } else if (race_type %in% MASS_START_DISCIPLINES) {
    return(c(base_vars, "MassStart_Pelo_pct"))
  } else {
    return(base_vars)
  }
}

# Calculate exponential decay weighted previous points
get_weighted_prev_points <- function(chrono_data, athlete_id, race_type, reference_date) {
  # Filter for this athlete and race type
  if (race_type %in% SPRINT_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% SPRINT_DISCIPLINES)
  } else if (race_type %in% INDIVIDUAL_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% INDIVIDUAL_DISCIPLINES)
  } else if (race_type %in% PURSUIT_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% PURSUIT_DISCIPLINES)
  } else if (race_type %in% MASS_START_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% MASS_START_DISCIPLINES)
  } else {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType == race_type)
  }

  if (nrow(races) == 0) {
    # Fallback to all races
    races <- chrono_data %>% filter(ID == athlete_id)
  }

  if (nrow(races) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
  }

  # Calculate points if not present
  if (!"Points" %in% names(races)) {
    races$Points <- mapply(function(p, rt) get_points(p, rt), races$Place, races$RaceType)
  }

  # Calculate days ago and weights
  races <- races %>%
    mutate(
      days_ago = as.numeric(reference_date - Date),
      weight = exp(-DECAY_LAMBDA * pmax(0, days_ago))
    ) %>%
    filter(days_ago >= 0, !is.na(Points))

  if (nrow(races) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
  }

  # Calculate weighted statistics
  weighted_mean <- sum(races$Points * races$weight) / sum(races$weight)

  if (nrow(races) >= 2) {
    weighted_var <- sum(races$weight * (races$Points - weighted_mean)^2) / sum(races$weight)
    weighted_sd <- sqrt(weighted_var)
  } else {
    weighted_sd <- SD_MAX / 2
  }

  return(list(
    mean = weighted_mean,
    sd = weighted_sd,
    n = nrow(races)
  ))
}

# ============================================================================
# DATA PREPROCESSING FUNCTIONS
# ============================================================================

calculate_percentage_columns <- function(chrono_data) {
  chrono_data %>%
    group_by(Season, Race) %>%
    mutate(
      Pelo_pct = Pelo / max(Pelo, na.rm = TRUE),
      Individual_Pelo_pct = Individual_Pelo / max(Individual_Pelo, na.rm = TRUE),
      Sprint_Pelo_pct = Sprint_Pelo / max(Sprint_Pelo, na.rm = TRUE),
      Pursuit_Pelo_pct = Pursuit_Pelo / max(Pursuit_Pelo, na.rm = TRUE),
      MassStart_Pelo_pct = MassStart_Pelo / max(MassStart_Pelo, na.rm = TRUE)
    ) %>%
    ungroup()
}

calculate_weighted_prev_points <- function(chrono_data, decay_lambda = DECAY_LAMBDA) {
  chrono_data %>%
    arrange(ID, Date) %>%
    group_by(ID) %>%
    mutate(
      prev_points_weighted = sapply(row_number(), function(i) {
        if (i == 1) return(0)
        prev_data <- cur_data()[1:(i-1), ]
        if (nrow(prev_data) == 0) return(0)
        prev_points <- if ("Points" %in% names(prev_data)) prev_data$Points else
          mapply(function(p, rt) get_points(p, rt), prev_data$Place, prev_data$RaceType)
        prev_dates <- prev_data$Date
        current_date <- cur_data()$Date[i]
        days_ago <- as.numeric(difftime(current_date, prev_dates, units = "days"))
        weights <- exp(-decay_lambda * days_ago)
        weighted.mean(prev_points, weights, na.rm = TRUE)
      })
    ) %>%
    ungroup()
}

# ============================================================================
# GAM MODEL TRAINING
# ============================================================================

train_points_gam <- function(chrono_data, race_type, gender) {
  log_info(paste("Training", gender, race_type, "POINTS GAM"))

  if (race_type %in% SPRINT_DISCIPLINES) {
    filtered_data <- chrono_data %>% filter(RaceType %in% SPRINT_DISCIPLINES)
  } else if (race_type %in% INDIVIDUAL_DISCIPLINES) {
    filtered_data <- chrono_data %>% filter(RaceType %in% INDIVIDUAL_DISCIPLINES)
  } else if (race_type %in% PURSUIT_DISCIPLINES) {
    filtered_data <- chrono_data %>% filter(RaceType %in% PURSUIT_DISCIPLINES)
  } else if (race_type %in% MASS_START_DISCIPLINES) {
    filtered_data <- chrono_data %>% filter(RaceType %in% MASS_START_DISCIPLINES)
  } else {
    filtered_data <- chrono_data %>% filter(RaceType == race_type)
  }

  if (!"Points" %in% names(filtered_data)) {
    filtered_data$Points <- mapply(function(p, rt) get_points(p, rt),
                                    filtered_data$Place, filtered_data$RaceType)
  }

  if (nrow(filtered_data) < 50) {
    log_warn(paste("Insufficient data for", race_type))
    return(NULL)
  }

  explanatory_vars <- get_explanatory_vars(race_type)
  available_vars <- intersect(explanatory_vars, names(filtered_data))
  if (length(available_vars) == 0) return(NULL)

  tryCatch({
    formula <- as.formula(paste("Points ~", paste(available_vars, collapse = " + ")))
    feature_selection <- regsubsets(formula, data = filtered_data, nbest = 1, method = "exhaustive")
    feature_summary <- summary(feature_selection)
    best_bic_vars <- names(coef(feature_selection, which.min(feature_summary$bic)))[-1]
    positive_vars <- filter_positive_coefficients(filtered_data, "Points", best_bic_vars)
    if (length(positive_vars) == 0) positive_vars <- "prev_points_weighted"

    smooth_terms <- paste("s(", positive_vars, ")", collapse = " + ")
    gam_formula <- as.formula(paste("Points ~", smooth_terms))
    points_model <- gam(gam_formula, data = filtered_data, method = "REML")

    residual_sd <- sqrt(points_model$sig2)
    if (is.null(residual_sd) || is.na(residual_sd)) {
      residual_sd <- sqrt(points_model$deviance / points_model$df.residual)
    }
    residual_sd <- max(residual_sd, 5)

    log_info(paste("GAM trained. Residual SD:", round(residual_sd, 2)))
    return(list(model = points_model, residual_sd = residual_sd, features = positive_vars))
  }, error = function(e) {
    log_error(paste("Error training GAM:", e$message))
    return(NULL)
  })
}

# ============================================================================
# SIMULATION FUNCTIONS
# ============================================================================

# Build athlete distribution combining history + GAM fill
build_athlete_distribution <- function(athlete_id, race_type, chrono_data,
                                       gam_prediction, gam_residual_sd,
                                       n_history = N_HISTORY_REQUIRED,
                                       gam_fill_weight_factor = GAM_FILL_WEIGHT_FACTOR,
                                       decay_lambda = DECAY_LAMBDA,
                                       reference_date = NULL) {
  if (is.null(reference_date)) reference_date <- Sys.Date()

  if (race_type %in% SPRINT_DISCIPLINES) {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% SPRINT_DISCIPLINES) %>%
      arrange(desc(Date)) %>% head(n_history)
  } else if (race_type %in% INDIVIDUAL_DISCIPLINES) {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% INDIVIDUAL_DISCIPLINES) %>%
      arrange(desc(Date)) %>% head(n_history)
  } else if (race_type %in% PURSUIT_DISCIPLINES) {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% PURSUIT_DISCIPLINES) %>%
      arrange(desc(Date)) %>% head(n_history)
  } else if (race_type %in% MASS_START_DISCIPLINES) {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% MASS_START_DISCIPLINES) %>%
      arrange(desc(Date)) %>% head(n_history)
  } else {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, RaceType == race_type) %>%
      arrange(desc(Date)) %>% head(n_history)
  }

  n_actual_races <- nrow(athlete_history)
  all_points <- c()
  all_weights <- c()

  if (n_actual_races > 0) {
    if (!"Points" %in% names(athlete_history)) {
      history_points <- mapply(function(p, rt) get_points(p, rt),
                               athlete_history$Place, athlete_history$RaceType)
    } else {
      history_points <- athlete_history$Points
    }
    days_ago <- as.numeric(reference_date - athlete_history$Date)
    history_weights <- exp(-decay_lambda * days_ago)
    all_points <- c(all_points, history_points)
    all_weights <- c(all_weights, history_weights)
  }

  n_missing_history <- n_history - n_actual_races
  if (n_missing_history > 0 && !is.null(gam_prediction) && !is.na(gam_prediction)) {
    gam_fill_points <- rnorm(n_missing_history, mean = gam_prediction, sd = gam_residual_sd)
    gam_fill_points <- pmax(0, pmin(90, gam_fill_points))
    if (n_actual_races > 0) {
      median_weight <- median(all_weights) * gam_fill_weight_factor
    } else {
      median_weight <- exp(-decay_lambda * 365) * gam_fill_weight_factor
    }
    all_points <- c(all_points, gam_fill_points)
    all_weights <- c(all_weights, rep(median_weight, n_missing_history))
  }

  if (length(all_points) > 0 && length(all_weights) > 0) {
    weighted_mean <- weighted.mean(all_points, all_weights, na.rm = TRUE)
    weighted_var <- sum(all_weights * (all_points - weighted_mean)^2) / sum(all_weights)
    weighted_sd <- sqrt(weighted_var)
    weighted_sd <- max(weighted_sd, SD_MIN)
  } else if (!is.null(gam_prediction) && !is.na(gam_prediction)) {
    weighted_mean <- gam_prediction
    weighted_sd <- gam_residual_sd
  } else {
    weighted_mean <- 5
    weighted_sd <- SD_MAX
  }

  weighted_sd <- pmax(SD_MIN, pmin(SD_MAX, weighted_sd))

  return(list(athlete_id = athlete_id, mean = weighted_mean, sd = weighted_sd, n_actual_races = n_actual_races))
}

# Monte Carlo simulation for race positions (vectorized for performance)
simulate_race_positions <- function(athlete_distributions, n_simulations = N_SIMULATIONS,
                                    position_thresholds = POSITION_THRESHOLDS,
                                    max_points = 90) {
  # Filter invalid distributions
  valid_distributions <- Filter(function(dist) {
    !is.null(dist) && !is.null(dist$athlete_id) && !is.na(dist$athlete_id) &&
    !is.null(dist$mean) && !is.na(dist$mean) &&
    !is.null(dist$sd) && !is.na(dist$sd)
  }, athlete_distributions)

  if (length(valid_distributions) == 0) {
    log_error("No valid athlete distributions to simulate")
    return(data.frame())
  }

  n_athletes <- length(valid_distributions)
  athlete_ids <- sapply(valid_distributions, function(x) x$athlete_id)

  # Extract means and sds as vectors for vectorized operations
  means <- sapply(valid_distributions, function(x) x$mean)
  sds <- sapply(valid_distributions, function(x) x$sd)

  # Apply SD scaling and bounds
  scaled_sds <- pmax(SD_MIN, pmin(SD_MAX, sds * SD_SCALE_FACTOR))

  # Generate all simulations at once (n_athletes x n_simulations matrix)
  all_sims <- matrix(rnorm(n_athletes * n_simulations),
                     nrow = n_athletes, ncol = n_simulations)
  all_sims <- all_sims * scaled_sds + means
  all_sims <- pmax(0, pmin(max_points, all_sims))

  # Rank each simulation (column) - higher points = better = rank 1
  ranks_matrix <- apply(all_sims, 2, function(x) rank(-x, ties.method = "random"))

  # Count position achievements using vectorized rowSums
  position_counts <- matrix(0, nrow = n_athletes, ncol = length(position_thresholds))
  for (t_idx in seq_along(position_thresholds)) {
    threshold <- position_thresholds[t_idx]
    position_counts[, t_idx] <- rowSums(ranks_matrix <= threshold)
  }

  # Convert to probabilities
  position_probs <- position_counts / n_simulations

  # Build results
  results <- data.frame(
    athlete_id = athlete_ids,
    mean_points = means,
    sd_points = sds,
    n_actual_races = sapply(valid_distributions, function(x) x$n_actual_races),
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
# RELAY HELPER FUNCTIONS
# ============================================================================

# Get weighted previous points for a relay team (nation-based)
get_relay_weighted_prev_points <- function(relay_chrono, nation, reference_date) {
  if (nrow(relay_chrono) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
  }

  races <- relay_chrono %>%
    filter(Nation == nation)

  if (nrow(races) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
  }

  # Calculate points if not present
  if (!"Points" %in% names(races)) {
    races$Points <- mapply(function(p) get_points(p, "Relay"), races$Place)
  }

  # Calculate days ago and weights
  races <- races %>%
    mutate(
      days_ago = as.numeric(reference_date - Date),
      weight = exp(-DECAY_LAMBDA * pmax(0, days_ago))
    ) %>%
    filter(days_ago >= 0, !is.na(Points))

  if (nrow(races) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
  }

  # Calculate weighted statistics
  weighted_mean <- sum(races$Points * races$weight) / sum(races$weight)

  if (nrow(races) >= 2) {
    weighted_var <- sum(races$weight * (races$Points - weighted_mean)^2) / sum(races$weight)
    weighted_sd <- sqrt(weighted_var)
  } else {
    weighted_sd <- RELAY_SD_MAX / 2
  }

  return(list(
    mean = weighted_mean,
    sd = weighted_sd,
    n = nrow(races)
  ))
}

# Build relay team distribution for simulation
build_relay_team_distribution <- function(nation, relay_chrono, startlist_row,
                                          reference_date = NULL) {
  if (is.null(reference_date)) {
    reference_date <- Sys.Date()
  }

  # Get weighted historical team performance
  hist_stats <- get_relay_weighted_prev_points(relay_chrono, nation, reference_date)

  # Use average Pelo values from startlist if available
  avg_pelo <- NA
  pelo_cols <- grep("^Avg_.*Pelo", names(startlist_row), value = TRUE)
  if (length(pelo_cols) > 0) {
    pelo_values <- unlist(startlist_row[pelo_cols])
    avg_pelo <- mean(pelo_values, na.rm = TRUE)
  }

  # Determine mean and sd
  if (!is.na(hist_stats$mean) && hist_stats$n >= 2) {
    # Good history - use weighted historical performance
    mean_points <- hist_stats$mean
    sd_points <- hist_stats$sd
  } else if (!is.na(avg_pelo) && avg_pelo > 0) {
    # Use Pelo estimate - convert Pelo percentage to expected points
    # Assume top Pelo (~100%) correlates to ~80 points, lower Pelo to less
    mean_points <- avg_pelo * 60
    sd_points <- RELAY_SD_MAX / 2
  } else if (!is.na(hist_stats$mean)) {
    # Some history but limited
    mean_points <- hist_stats$mean
    sd_points <- if (!is.na(hist_stats$sd)) hist_stats$sd else RELAY_SD_MAX / 2
  } else {
    # No history - use low default
    mean_points <- 10
    sd_points <- RELAY_SD_MAX
  }

  # Bound sd
  sd_points <- pmax(RELAY_SD_MIN, pmin(RELAY_SD_MAX, sd_points))

  return(list(
    nation = nation,
    mean = mean_points,
    sd = sd_points,
    n_actual_races = hist_stats$n
  ))
}

# Monte Carlo simulation for relay teams (vectorized)
simulate_relay_positions <- function(team_distributions, n_simulations = N_SIMULATIONS,
                                     position_thresholds = POSITION_THRESHOLDS,
                                     max_points = 90) {
  # Filter invalid distributions
  valid_distributions <- Filter(function(dist) {
    !is.null(dist) && !is.null(dist$nation) && !is.na(dist$nation) &&
    !is.null(dist$mean) && !is.na(dist$mean) &&
    !is.null(dist$sd) && !is.na(dist$sd)
  }, team_distributions)

  if (length(valid_distributions) == 0) {
    log_error("No valid relay team distributions to simulate")
    return(data.frame())
  }

  n_teams <- length(valid_distributions)
  nations <- sapply(valid_distributions, function(x) x$nation)

  # Extract means and sds
  means <- sapply(valid_distributions, function(x) x$mean)
  sds <- sapply(valid_distributions, function(x) x$sd)

  # Apply SD scaling and bounds
  scaled_sds <- pmax(RELAY_SD_MIN, pmin(RELAY_SD_MAX, sds * RELAY_SD_SCALE_FACTOR))

  # Generate all simulations
  all_sims <- matrix(rnorm(n_teams * n_simulations),
                     nrow = n_teams, ncol = n_simulations)
  all_sims <- all_sims * scaled_sds + means
  all_sims <- pmax(0, pmin(max_points, all_sims))

  # Rank each simulation
  ranks_matrix <- apply(all_sims, 2, function(x) rank(-x, ties.method = "random"))

  # Count position achievements
  position_counts <- matrix(0, nrow = n_teams, ncol = length(position_thresholds))
  for (t_idx in seq_along(position_thresholds)) {
    threshold <- position_thresholds[t_idx]
    position_counts[, t_idx] <- rowSums(ranks_matrix <= threshold)
  }

  # Convert to probabilities
  position_probs <- position_counts / n_simulations

  # Build results
  results <- data.frame(
    Nation = nations,
    mean_points = means,
    sd_points = sds,
    n_actual_races = sapply(valid_distributions, function(x) x$n_actual_races),
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
# DATA PREPROCESSING
# ============================================================================

log_info("=== PREPROCESSING DATA FOR GAM ===")

preprocess_chrono <- function(chrono_df) {
  if (nrow(chrono_df) == 0) return(chrono_df)
  chrono_df <- calculate_percentage_columns(chrono_df)
  chrono_df <- calculate_weighted_prev_points(chrono_df)
  pelo_cols <- grep("_pct$", names(chrono_df), value = TRUE)
  for (col in pelo_cols) {
    chrono_df[[col]] <- replace_na_with_quartile(chrono_df[[col]])
  }
  return(chrono_df)
}

if (nrow(men_chrono) > 0) {
  men_chrono <- preprocess_chrono(men_chrono)
  log_info(paste("Preprocessed men's chrono data:", nrow(men_chrono), "rows"))
}

if (nrow(ladies_chrono) > 0) {
  ladies_chrono <- preprocess_chrono(ladies_chrono)
  log_info(paste("Preprocessed ladies' chrono data:", nrow(ladies_chrono), "rows"))
}

# ============================================================================
# GAM MODEL TRAINING
# ============================================================================

log_info("=== TRAINING GAM MODELS ===")

all_race_types <- unique(c(men_races$RaceType, ladies_races$RaceType))
all_race_types <- all_race_types[!is.na(all_race_types) & !all_race_types %in% RELAY_DISCIPLINES]
log_info(paste("Race types to train:", paste(all_race_types, collapse = ", ")))

men_gam_models <- list()
ladies_gam_models <- list()

for (race_type in all_race_types) {
  if (nrow(men_chrono) > 0) {
    men_gam_models[[race_type]] <- train_points_gam(men_chrono, race_type, "men")
  }
  if (nrow(ladies_chrono) > 0) {
    ladies_gam_models[[race_type]] <- train_points_gam(ladies_chrono, race_type, "ladies")
  }
}

log_info(paste("Trained", sum(!sapply(men_gam_models, is.null)), "men's GAM models"))
log_info(paste("Trained", sum(!sapply(ladies_gam_models, is.null)), "ladies' GAM models"))

# ============================================================================
# INDIVIDUAL RACE SIMULATION
# ============================================================================

log_info("=== INDIVIDUAL RACE SIMULATION ===")

individual_results <- list()

# Process each gender
for (gender in c("men", "ladies")) {
  races_df <- if (gender == "men") men_races else ladies_races
  startlist <- if (gender == "men") men_startlist else ladies_startlist
  chrono_data <- if (gender == "men") men_chrono else ladies_chrono
  gam_models <- if (gender == "men") men_gam_models else ladies_gam_models

  if (nrow(races_df) == 0) {
    log_info(paste("No", gender, "races today"))
    next
  }

  if (nrow(startlist) == 0) {
    log_warn(paste("No", gender, "startlist available"))
    next
  }

  log_info(paste("Processing", nrow(races_df), gender, "races"))

  for (i in 1:nrow(races_df)) {
    race <- races_df[i, ]
    race_type <- race$RaceType

    log_info(paste("Race", i, ":", gender, race_type))

    # Get athletes on startlist for this race
    race_startlist <- startlist %>%
      filter(!is.na(ID), !is.na(Skier))

    if (nrow(race_startlist) == 0) {
      log_warn(paste("No athletes in startlist for", gender, race_type))
      next
    }

    log_info(paste("Athletes in startlist:", nrow(race_startlist)))

    # Get GAM model for this race type
    model_info <- gam_models[[race_type]]

    # Build distributions for each athlete
    athlete_distributions <- list()

    for (j in 1:nrow(race_startlist)) {
      athlete_id <- race_startlist$ID[j]

      # Get GAM prediction for this athlete
      gam_prediction <- NULL
      gam_residual_sd <- SD_MAX / 2

      if (!is.null(model_info)) {
        athlete_features <- chrono_data %>%
          filter(ID == athlete_id) %>%
          arrange(desc(Date)) %>%
          slice(1)

        if (nrow(athlete_features) == 0) {
          gam_prediction <- median(chrono_data$Points, na.rm = TRUE)
        } else {
          gam_prediction <- tryCatch({
            predict(model_info$model, newdata = athlete_features, type = "response")
          }, error = function(e) {
            median(chrono_data$Points, na.rm = TRUE)
          })
        }
        gam_residual_sd <- model_info$residual_sd
      }

      dist <- build_athlete_distribution(
        athlete_id = athlete_id,
        race_type = race_type,
        chrono_data = chrono_data,
        gam_prediction = gam_prediction,
        gam_residual_sd = gam_residual_sd,
        reference_date = current_date
      )

      athlete_distributions[[as.character(athlete_id)]] <- dist
    }

    # Run Monte Carlo simulation
    log_info(paste("Running", N_SIMULATIONS, "Monte Carlo simulations"))
    race_results <- simulate_race_positions(athlete_distributions)

    # Add athlete names and info
    sex_col <- if ("Sex" %in% names(race_startlist)) "Sex" else NULL
    startlist_cols <- c("ID", "Skier", "Nation")
    if (!is.null(sex_col)) startlist_cols <- c(startlist_cols, sex_col)

    race_results <- race_results %>%
      left_join(
        race_startlist %>% select(all_of(startlist_cols)) %>% distinct(),
        by = c("athlete_id" = "ID")
      ) %>%
      rename(ID = athlete_id)

    # Add Sex if not present
    if (!"Sex" %in% names(race_results)) {
      race_results$Sex <- ifelse(gender == "men", "M", "L")
    }

    race_results <- race_results %>%
      select(Skier, Nation, ID, Sex, mean_points, sd_points, n_actual_races,
             starts_with("prob_top_")) %>%
      arrange(desc(prob_top_1))

    # Store results
    race_key <- paste(gender, race_type, sep = "_")
    individual_results[[race_key]] <- list(
      race_info = race,
      predictions = race_results,
      gender = gender,
      race_type = race_type,
      race_number = i
    )

    log_info(paste("Completed", race_key, "- Top 3:"))
    print(head(race_results %>% select(Skier, Nation, prob_top_1, prob_top_3), 3))
  }

  log_info(paste(gender, "simulation complete.", length(individual_results), "total races processed"))
}

# ============================================================================
# RELAY RACE SIMULATION
# ============================================================================

log_info("=== RELAY RACE SIMULATION ===")

relay_results <- list()

# Process Men's Relay
if (nrow(men_relays) > 0 && nrow(men_relay_startlist) > 0) {
  log_info("Processing Men's Relay")

  team_distributions <- list()

  for (i in 1:nrow(men_relay_startlist)) {
    nation <- men_relay_startlist$Nation[i]

    dist <- build_relay_team_distribution(
      nation = nation,
      relay_chrono = men_relay_chrono,
      startlist_row = men_relay_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  relay_sim_results <- simulate_relay_positions(team_distributions)

  # Add any additional team info from startlist
  relay_sim_results <- relay_sim_results %>%
    mutate(Sex = "M", RaceType = "Relay") %>%
    arrange(desc(prob_top_1))

  relay_results[["men_relay"]] <- list(
    race_type = "Relay",
    gender = "men",
    predictions = relay_sim_results
  )

  log_info(paste("Men's Relay complete - Top 3:"))
  print(head(relay_sim_results %>% select(Nation, prob_top_1, prob_top_3), 3))
}

# Process Ladies' Relay
if (nrow(ladies_relays) > 0 && nrow(ladies_relay_startlist) > 0) {
  log_info("Processing Ladies' Relay")

  team_distributions <- list()

  for (i in 1:nrow(ladies_relay_startlist)) {
    nation <- ladies_relay_startlist$Nation[i]

    dist <- build_relay_team_distribution(
      nation = nation,
      relay_chrono = ladies_relay_chrono,
      startlist_row = ladies_relay_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  relay_sim_results <- simulate_relay_positions(team_distributions)

  relay_sim_results <- relay_sim_results %>%
    mutate(Sex = "L", RaceType = "Relay") %>%
    arrange(desc(prob_top_1))

  relay_results[["ladies_relay"]] <- list(
    race_type = "Relay",
    gender = "ladies",
    predictions = relay_sim_results
  )

  log_info(paste("Ladies' Relay complete - Top 3:"))
  print(head(relay_sim_results %>% select(Nation, prob_top_1, prob_top_3), 3))
}

# Process Mixed Relay
if (nrow(mixed_relays) > 0 && nrow(mixed_relay_startlist) > 0) {
  log_info("Processing Mixed Relay")

  team_distributions <- list()

  for (i in 1:nrow(mixed_relay_startlist)) {
    nation <- mixed_relay_startlist$Nation[i]

    dist <- build_relay_team_distribution(
      nation = nation,
      relay_chrono = mixed_relay_chrono,
      startlist_row = mixed_relay_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  relay_sim_results <- simulate_relay_positions(team_distributions)

  relay_sim_results <- relay_sim_results %>%
    mutate(Sex = "Mixed", RaceType = "Mixed Relay") %>%
    arrange(desc(prob_top_1))

  relay_results[["mixed_relay"]] <- list(
    race_type = "Mixed Relay",
    gender = "mixed",
    predictions = relay_sim_results
  )

  log_info(paste("Mixed Relay complete - Top 3:"))
  print(head(relay_sim_results %>% select(Nation, prob_top_1, prob_top_3), 3))
}

# Process Single Mixed Relay
if (nrow(single_mixed_relays) > 0 && nrow(single_mixed_relay_startlist) > 0) {
  log_info("Processing Single Mixed Relay")

  team_distributions <- list()

  for (i in 1:nrow(single_mixed_relay_startlist)) {
    nation <- single_mixed_relay_startlist$Nation[i]

    dist <- build_relay_team_distribution(
      nation = nation,
      relay_chrono = single_mixed_relay_chrono,
      startlist_row = single_mixed_relay_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  relay_sim_results <- simulate_relay_positions(team_distributions)

  relay_sim_results <- relay_sim_results %>%
    mutate(Sex = "Mixed", RaceType = "Single Mixed Relay") %>%
    arrange(desc(prob_top_1))

  relay_results[["single_mixed_relay"]] <- list(
    race_type = "Single Mixed Relay",
    gender = "mixed",
    predictions = relay_sim_results
  )

  log_info(paste("Single Mixed Relay complete - Top 3:"))
  print(head(relay_sim_results %>% select(Nation, prob_top_1, prob_top_3), 3))
}

log_info(paste("Relay simulation complete.", length(relay_results), "relay races processed"))

# ============================================================================
# OUTPUT: GENERATE EXCEL FILES
# ============================================================================

log_info("=== OUTPUT GENERATION ===")

# Output directory with date folder
utc_date <- format(Sys.time(), "%Y%m%d", tz = "UTC")
output_dir <- paste0("~/blog/daehl-e/content/post/biathlon/drafts/race-picks/", utc_date)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Format results for Excel
format_individual_results <- function(results_list) {
  formatted <- list()

  # Track race numbers per gender
  men_race_num <- 0
  ladies_race_num <- 0

  for (race_key in names(results_list)) {
    entry <- results_list[[race_key]]
    predictions <- entry$predictions

    # Format probabilities as percentages
    output_data <- predictions %>%
      mutate(
        Participation = 100,  # All athletes in simulation are participating
        Win = round(prob_top_1 * 100, 1),
        Podium = round(prob_top_3 * 100, 1),
        `Top-5` = round(prob_top_5 * 100, 1),
        `Top-10` = round(prob_top_10 * 100, 1),
        `Top-30` = round(prob_top_30 * 100, 1)
      ) %>%
      select(Skier, ID, Nation, Sex, Participation, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
      arrange(desc(Win))

    # Create sheet name
    if (entry$gender == "men") {
      men_race_num <- men_race_num + 1
      sheet_name <- paste("Men Race", men_race_num)
    } else {
      ladies_race_num <- ladies_race_num + 1
      sheet_name <- paste("Ladies Race", ladies_race_num)
    }

    formatted[[sheet_name]] <- output_data
  }

  return(formatted)
}

# Generate output files
if (length(individual_results) > 0) {
  individual_formatted <- format_individual_results(individual_results)

  # Separate by gender
  men_individual <- individual_formatted[grep("^Men", names(individual_formatted))]
  ladies_individual <- individual_formatted[grep("^Ladies", names(individual_formatted))]

  if (length(men_individual) > 0) {
    men_file <- file.path(output_dir, "men_position_probabilities.xlsx")
    write.xlsx(men_individual, men_file)
    log_info(paste("Saved men's predictions to", men_file))
  }

  if (length(ladies_individual) > 0) {
    ladies_file <- file.path(output_dir, "ladies_position_probabilities.xlsx")
    write.xlsx(ladies_individual, ladies_file)
    log_info(paste("Saved ladies' predictions to", ladies_file))
  }
}

# Format relay results for Excel
format_relay_results <- function(results_list) {
  formatted <- list()

  for (relay_key in names(results_list)) {
    entry <- results_list[[relay_key]]
    predictions <- entry$predictions

    # Format probabilities as percentages
    output_data <- predictions %>%
      mutate(
        Win = round(prob_top_1 * 100, 1),
        Podium = round(prob_top_3 * 100, 1),
        `Top-5` = round(prob_top_5 * 100, 1),
        `Top-10` = round(prob_top_10 * 100, 1),
        `Top-30` = round(prob_top_30 * 100, 1)
      ) %>%
      select(Nation, Sex, RaceType, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
      arrange(desc(Win))

    # Create sheet name
    sheet_name <- entry$race_type
    formatted[[sheet_name]] <- output_data
  }

  return(formatted)
}

# Generate relay output files
if (length(relay_results) > 0) {
  relay_formatted <- format_relay_results(relay_results)

  # Save men's relay
  if ("Relay" %in% names(relay_formatted)) {
    men_relay_data <- relay_formatted[["Relay"]] %>% filter(Sex == "M")
    if (nrow(men_relay_data) > 0) {
      men_relay_file <- file.path(output_dir, "men_relay_position_probabilities.xlsx")
      write.xlsx(list("Men Relay" = men_relay_data), men_relay_file)
      log_info(paste("Saved men's relay predictions to", men_relay_file))
    }
  }

  # Save ladies' relay
  if ("Relay" %in% names(relay_formatted)) {
    ladies_relay_data <- relay_formatted[["Relay"]] %>% filter(Sex == "L")
    if (nrow(ladies_relay_data) > 0) {
      ladies_relay_file <- file.path(output_dir, "ladies_relay_position_probabilities.xlsx")
      write.xlsx(list("Ladies Relay" = ladies_relay_data), ladies_relay_file)
      log_info(paste("Saved ladies' relay predictions to", ladies_relay_file))
    }
  }

  # Save mixed relay
  if ("Mixed Relay" %in% names(relay_formatted)) {
    mixed_relay_file <- file.path(output_dir, "mixed_relay_position_probabilities.xlsx")
    write.xlsx(list("Mixed Relay" = relay_formatted[["Mixed Relay"]]), mixed_relay_file)
    log_info(paste("Saved mixed relay predictions to", mixed_relay_file))
  }

  # Save single mixed relay
  if ("Single Mixed Relay" %in% names(relay_formatted)) {
    single_mixed_relay_file <- file.path(output_dir, "single_mixed_relay_position_probabilities.xlsx")
    write.xlsx(list("Single Mixed Relay" = relay_formatted[["Single Mixed Relay"]]), single_mixed_relay_file)
    log_info(paste("Saved single mixed relay predictions to", single_mixed_relay_file))
  }
}

# Print summary
cat("\n=== BIATHLON RACE PICKS SIMULATION COMPLETE ===\n")
cat(paste("Date:", current_date, "\n"))
cat(paste("Individual races processed:", length(individual_results), "\n"))
cat(paste("Relay races processed:", length(relay_results), "\n"))

if (length(individual_results) > 0) {
  cat("\nIndividual Race Predictions:\n")
  for (race_key in names(individual_results)) {
    entry <- individual_results[[race_key]]
    top_pred <- entry$predictions[1, ]
    cat(paste("  ", race_key, "- Winner pick:", top_pred$Skier,
              "(", round(top_pred$prob_top_1 * 100, 1), "%)\n"))
  }
}

if (length(relay_results) > 0) {
  cat("\nRelay Race Predictions:\n")
  for (relay_key in names(relay_results)) {
    entry <- relay_results[[relay_key]]
    top_pred <- entry$predictions[1, ]
    cat(paste("  ", relay_key, "- Winner pick:", top_pred$Nation,
              "(", round(top_pred$prob_top_1 * 100, 1), "%)\n"))
  }
}

cat(paste("\nOutput directory:", output_dir, "\n"))

log_info("=== BIATHLON RACE-PICKS-SIMULATION.R COMPLETE ===")
