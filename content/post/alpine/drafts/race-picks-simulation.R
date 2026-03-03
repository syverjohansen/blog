# Alpine Skiing Race Picks - Monte Carlo Simulation
# Unified simulation script for individual race predictions
#
# Features:
# - Monte Carlo simulation (10,000 iterations)
# - Exponential decay weighting for historical performance
# - Discipline-specific distributions (Downhill, Super G, Giant Slalom, Slalom, Combined)
# - Speed vs Tech grouping
# - Position probability output (Win, Podium, Top5, Top10, Top30)

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
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)  # Win, Podium, Top5, Top10, Top30

# Points system
wc_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16,
               15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Discipline definitions
SPEED_DISCIPLINES <- c("Downhill", "Super G")
TECH_DISCIPLINES <- c("Slalom", "Giant Slalom")

# ============================================================================
# LOGGING SETUP
# ============================================================================

log_dir <- "~/ski/elo/python/alpine/polars/excel365/race-picks-simulation"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "simulation.log")))
log_info("=== ALPINE RACE-PICKS-SIMULATION.R STARTED ===")
log_info(paste("TEST_MODE:", TEST_MODE))

# ============================================================================
# DATA LOADING
# ============================================================================

log_info("Loading data files...")

base_path <- "~/ski/elo/python/alpine/polars/excel365"

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

# Separate by gender
men_races <- today_races %>% filter(Sex == "M")
ladies_races <- today_races %>% filter(Sex == "L")

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

# Load startlists with error handling
men_startlist <- tryCatch({
  df <- read.csv(file.path(base_path, "startlist_races_men.csv"), stringsAsFactors = FALSE)
  # Validate required columns
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
  # Validate required columns
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

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Get points from place
get_points <- function(place) {
  if (is.na(place) || place < 1 || place > length(wc_points)) {
    return(0)
  }
  return(wc_points[place])
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
      log_warn("Model fitting failed in positive coefficient filter")
      break
    }

    coefs <- coef(model)
    coefs <- coefs[names(coefs) != "(Intercept)"]

    negative_vars <- names(coefs[coefs < 0])

    if (length(negative_vars) == 0) break

    log_info(paste("  Removing negative coefficient features:", paste(negative_vars, collapse = ", ")))
    current_vars <- setdiff(current_vars, negative_vars)
  }

  return(current_vars)
}

# Get discipline-specific explanatory variables for GAM
get_explanatory_vars <- function(discipline) {
  base_vars <- c("prev_points_weighted", "Pelo_pct")

  if (discipline %in% c("Downhill")) {
    return(c(base_vars, "Downhill_Pelo_pct", "Speed_Pelo_pct", "Super_G_Pelo_pct"))
  } else if (discipline %in% c("Super G")) {
    return(c(base_vars, "Super_G_Pelo_pct", "Speed_Pelo_pct", "Downhill_Pelo_pct"))
  } else if (discipline %in% c("Giant Slalom")) {
    return(c(base_vars, "Giant_Slalom_Pelo_pct", "Tech_Pelo_pct", "Slalom_Pelo_pct"))
  } else if (discipline %in% c("Slalom")) {
    return(c(base_vars, "Slalom_Pelo_pct", "Tech_Pelo_pct", "Giant_Slalom_Pelo_pct"))
  } else if (discipline %in% c("Combined", "Alpine Combined")) {
    return(c(base_vars, "Combined_Pelo_pct", "Speed_Pelo_pct", "Tech_Pelo_pct"))
  } else {
    return(c(base_vars, "Speed_Pelo_pct", "Tech_Pelo_pct"))
  }
}

# Calculate exponential decay weighted previous points
get_weighted_prev_points <- function(chrono_data, athlete_id, discipline, reference_date) {
  # Filter for this athlete and discipline
  if (discipline %in% SPEED_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, Distance %in% SPEED_DISCIPLINES)
  } else if (discipline %in% TECH_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, Distance %in% TECH_DISCIPLINES)
  } else if (discipline %in% c("Combined", "Alpine Combined")) {
    races <- chrono_data %>%
      filter(ID == athlete_id, Distance %in% c("Combined", "Alpine Combined"))
  } else {
    races <- chrono_data %>%
      filter(ID == athlete_id, Distance == discipline)
  }

  if (nrow(races) == 0) {
    # Fallback to all races
    races <- chrono_data %>% filter(ID == athlete_id)
  }

  if (nrow(races) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
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

# Calculate PELO percentage columns (normalized within each race)
calculate_percentage_columns <- function(chrono_data) {
  chrono_data %>%
    group_by(Season, Race) %>%
    mutate(
      Pelo_pct = Pelo / max(Pelo, na.rm = TRUE),
      Downhill_Pelo_pct = `Downhill_Pelo` / max(`Downhill_Pelo`, na.rm = TRUE),
      Super_G_Pelo_pct = `Super G_Pelo` / max(`Super G_Pelo`, na.rm = TRUE),
      Giant_Slalom_Pelo_pct = `Giant Slalom_Pelo` / max(`Giant Slalom_Pelo`, na.rm = TRUE),
      Slalom_Pelo_pct = `Slalom_Pelo` / max(`Slalom_Pelo`, na.rm = TRUE),
      Combined_Pelo_pct = `Combined_Pelo` / max(`Combined_Pelo`, na.rm = TRUE),
      Tech_Pelo_pct = `Tech_Pelo` / max(`Tech_Pelo`, na.rm = TRUE),
      Speed_Pelo_pct = `Speed_Pelo` / max(`Speed_Pelo`, na.rm = TRUE)
    ) %>%
    ungroup()
}

# Calculate exponential decay weighted prev_points for GAM training
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

        prev_points_values <- prev_data$Points
        prev_dates <- prev_data$Date
        prev_distances <- prev_data$Distance

        current_date <- cur_data()$Date[i]
        current_distance <- cur_data()$Distance[i]

        # Match by discipline category
        if (current_distance %in% SPEED_DISCIPLINES) {
          matching <- prev_distances %in% SPEED_DISCIPLINES
        } else if (current_distance %in% TECH_DISCIPLINES) {
          matching <- prev_distances %in% TECH_DISCIPLINES
        } else {
          matching <- rep(TRUE, length(prev_distances))
        }

        matching_points <- prev_points_values[matching]
        matching_dates <- prev_dates[matching]
        if (length(matching_points) == 0) return(0)

        days_ago <- as.numeric(difftime(current_date, matching_dates, units = "days"))
        weights <- exp(-decay_lambda * days_ago)

        weighted.mean(matching_points, weights, na.rm = TRUE)
      })
    ) %>%
    ungroup()
}

# ============================================================================
# GAM MODEL TRAINING
# ============================================================================

# Train GAM for POINTS prediction
train_points_gam <- function(chrono_data, discipline, gender) {
  log_info(paste("Training", gender, discipline, "POINTS GAM"))

  # Filter for discipline
  if (discipline %in% SPEED_DISCIPLINES) {
    filtered_data <- chrono_data %>%
      filter(Distance %in% SPEED_DISCIPLINES)
  } else if (discipline %in% TECH_DISCIPLINES) {
    filtered_data <- chrono_data %>%
      filter(Distance %in% TECH_DISCIPLINES)
  } else if (discipline %in% c("Combined", "Alpine Combined")) {
    filtered_data <- chrono_data %>%
      filter(Distance %in% c("Combined", "Alpine Combined"))
  } else {
    filtered_data <- chrono_data %>%
      filter(Distance == discipline)
  }

  log_info(paste("Filtered to", nrow(filtered_data), "records"))

  if (nrow(filtered_data) < 50) {
    log_warn(paste("Insufficient data for", discipline, "- using fallback"))
    return(NULL)
  }

  explanatory_vars <- get_explanatory_vars(discipline)

  # Ensure all vars exist
  available_vars <- intersect(explanatory_vars, names(filtered_data))
  if (length(available_vars) == 0) {
    log_warn("No explanatory variables available")
    return(NULL)
  }

  tryCatch({
    # Feature selection via BIC
    formula <- as.formula(paste("Points ~", paste(available_vars, collapse = " + ")))
    feature_selection <- regsubsets(formula, data = filtered_data, nbest = 1, method = "exhaustive")
    feature_summary <- summary(feature_selection)
    best_bic_vars <- names(coef(feature_selection, which.min(feature_summary$bic)))[-1]

    log_info(paste("BIC selected features:", paste(best_bic_vars, collapse = ", ")))

    # Filter to keep only features with positive coefficients
    positive_vars <- filter_positive_coefficients(filtered_data, "Points", best_bic_vars, family = "gaussian")

    if (length(positive_vars) == 0) {
      log_warn("No positive coefficient features - using prev_points_weighted only")
      positive_vars <- "prev_points_weighted"
    }

    # GAM for points
    smooth_terms <- paste("s(", positive_vars, ")", collapse = " + ")
    gam_formula <- as.formula(paste("Points ~", smooth_terms))

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
      discipline = discipline
    ))

  }, error = function(e) {
    log_error(paste("Error training GAM:", e$message))
    return(NULL)
  })
}

# ============================================================================
# SIMULATION FUNCTIONS
# ============================================================================

# Build athlete distribution combining history + GAM fill
build_athlete_distribution <- function(athlete_id, discipline, chrono_data,
                                       gam_prediction, gam_residual_sd,
                                       n_history = N_HISTORY_REQUIRED,
                                       gam_fill_weight_factor = GAM_FILL_WEIGHT_FACTOR,
                                       decay_lambda = DECAY_LAMBDA,
                                       reference_date = NULL) {
  if (is.null(reference_date)) {
    reference_date <- Sys.Date()
  }

  # Define discipline filter
  if (discipline %in% SPEED_DISCIPLINES) {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, Distance %in% SPEED_DISCIPLINES) %>%
      arrange(desc(Date)) %>%
      head(n_history)
  } else if (discipline %in% TECH_DISCIPLINES) {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, Distance %in% TECH_DISCIPLINES) %>%
      arrange(desc(Date)) %>%
      head(n_history)
  } else if (discipline %in% c("Combined", "Alpine Combined")) {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, Distance %in% c("Combined", "Alpine Combined")) %>%
      arrange(desc(Date)) %>%
      head(n_history)
  } else {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, Distance == discipline) %>%
      arrange(desc(Date)) %>%
      head(n_history)
  }

  n_actual_races <- nrow(athlete_history)

  all_points <- c()
  all_weights <- c()
  history_points <- c()
  history_weights <- c()

  # Part 1: Historical races with exponential decay weighting
  if (n_actual_races > 0) {
    history_points <- athlete_history$Points

    days_ago <- as.numeric(reference_date - athlete_history$Date)
    history_weights <- exp(-decay_lambda * days_ago)

    all_points <- c(all_points, history_points)
    all_weights <- c(all_weights, history_weights)
  }

  # Part 2: GAM fill for missing history
  n_missing_history <- n_history - n_actual_races

  if (n_missing_history > 0 && !is.null(gam_prediction) && !is.na(gam_prediction)) {
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

  # Calculate distribution parameters
  if (length(all_points) > 0 && length(all_weights) > 0) {
    weighted_mean <- weighted.mean(all_points, all_weights, na.rm = TRUE)
    weighted_var <- sum(all_weights * (all_points - weighted_mean)^2) / sum(all_weights)
    weighted_sd <- sqrt(weighted_var)
    weighted_sd <- max(weighted_sd, SD_MIN)
  } else if (!is.null(gam_prediction) && !is.na(gam_prediction)) {
    weighted_mean <- gam_prediction
    weighted_sd <- gam_residual_sd
  } else {
    weighted_mean <- 5  # Low default for unknown athletes
    weighted_sd <- SD_MAX
  }

  # Bound sd
  weighted_sd <- pmax(SD_MIN, pmin(SD_MAX, weighted_sd))

  return(list(
    athlete_id = athlete_id,
    mean = weighted_mean,
    sd = weighted_sd,
    n_actual_races = n_actual_races
  ))
}

# Monte Carlo simulation for race positions (vectorized for performance)
simulate_race_positions <- function(athlete_distributions, n_simulations = N_SIMULATIONS,
                                    position_thresholds = POSITION_THRESHOLDS,
                                    max_points = 100) {
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
  # Each column is one simulation, each row is one athlete
  all_sims <- matrix(rnorm(n_athletes * n_simulations),
                     nrow = n_athletes, ncol = n_simulations)
  all_sims <- all_sims * scaled_sds + means  # Apply mean and sd
  all_sims <- pmax(0, pmin(max_points, all_sims))  # Bound to valid range

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
# INDIVIDUAL RACE SIMULATION
# ============================================================================

# ============================================================================
# DATA PREPROCESSING
# ============================================================================

log_info("=== PREPROCESSING DATA FOR GAM ===")

# Preprocess chrono data for both genders
preprocess_chrono <- function(chrono_df) {
  if (nrow(chrono_df) == 0) return(chrono_df)

  # Calculate percentage columns
  chrono_df <- calculate_percentage_columns(chrono_df)

  # Calculate weighted prev_points
  chrono_df <- calculate_weighted_prev_points(chrono_df)

  # Replace NAs in percentage columns
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

# Get unique disciplines from today's races
all_disciplines <- unique(c(men_races$Distance, ladies_races$Distance))
log_info(paste("Disciplines to train:", paste(all_disciplines, collapse = ", ")))

# Train GAM models for each gender and discipline
men_gam_models <- list()
ladies_gam_models <- list()

for (discipline in all_disciplines) {
  if (nrow(men_chrono) > 0) {
    men_gam_models[[discipline]] <- train_points_gam(men_chrono, discipline, "men")
  }
  if (nrow(ladies_chrono) > 0) {
    ladies_gam_models[[discipline]] <- train_points_gam(ladies_chrono, discipline, "ladies")
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
    discipline <- race$Distance

    log_info(paste("Race", i, ":", gender, discipline))

    # Get athletes on startlist for this race
    race_startlist <- startlist %>%
      filter(!is.na(ID), !is.na(Skier))

    if (nrow(race_startlist) == 0) {
      log_warn(paste("No athletes in startlist for", gender, discipline))
      next
    }

    log_info(paste("Athletes in startlist:", nrow(race_startlist)))

    # Get GAM model for this discipline
    model_info <- gam_models[[discipline]]
    if (is.null(model_info)) {
      # Try category fallback
      if (discipline %in% SPEED_DISCIPLINES) {
        model_info <- gam_models[["Downhill"]] %||% gam_models[["Super G"]]
      } else if (discipline %in% TECH_DISCIPLINES) {
        model_info <- gam_models[["Giant Slalom"]] %||% gam_models[["Slalom"]]
      }
    }

    # Build distributions for each athlete
    athlete_distributions <- list()

    for (j in 1:nrow(race_startlist)) {
      athlete_id <- race_startlist$ID[j]

      # Get GAM prediction for this athlete
      gam_prediction <- NULL
      gam_residual_sd <- SD_MAX / 2

      if (!is.null(model_info)) {
        # Get athlete's most recent features
        athlete_features <- chrono_data %>%
          filter(ID == athlete_id) %>%
          arrange(desc(Date)) %>%
          slice(1)

        if (nrow(athlete_features) == 0) {
          # No history - use median
          gam_prediction <- median(chrono_data$Points, na.rm = TRUE)
        } else {
          # Predict from GAM
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
        discipline = discipline,
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
    # Determine Sex column name in startlist
    sex_col <- if ("Sex" %in% names(race_startlist)) "Sex" else if ("Gender" %in% names(race_startlist)) "Gender" else NULL

    startlist_cols <- c("ID", "Skier", "Nation")
    if (!is.null(sex_col)) startlist_cols <- c(startlist_cols, sex_col)

    race_results <- race_results %>%
      left_join(
        race_startlist %>% select(all_of(startlist_cols)) %>% distinct(),
        by = c("athlete_id" = "ID")
      ) %>%
      rename(ID = athlete_id)

    # Rename Sex column if needed
    if (!is.null(sex_col) && sex_col == "Gender") {
      race_results <- race_results %>% rename(Sex = Gender)
    }

    # Add Sex if not present (derive from gender variable)
    if (!"Sex" %in% names(race_results)) {
      race_results$Sex <- ifelse(gender == "men", "M", "L")
    }

    race_results <- race_results %>%
      select(Skier, Nation, ID, Sex, mean_points, sd_points, n_actual_races,
             starts_with("prob_top_")) %>%
      arrange(desc(prob_top_1))

    # Store results
    race_key <- paste(gender, discipline, sep = "_")
    individual_results[[race_key]] <- list(
      race_info = race,
      predictions = race_results,
      gender = gender,
      discipline = discipline,
      race_number = i
    )

    log_info(paste("Completed", race_key, "- Top 3:"))
    print(head(race_results %>% select(Skier, Nation, prob_top_1, prob_top_3), 3))
  }

  log_info(paste(gender, "simulation complete.", length(individual_results), "total races processed"))
}

# ============================================================================
# OUTPUT: GENERATE EXCEL FILES
# ============================================================================

log_info("=== OUTPUT GENERATION ===")

# Output directory with date folder
utc_date <- format(Sys.time(), "%Y%m%d", tz = "UTC")
output_dir <- paste0("~/blog/daehl-e/content/post/alpine/drafts/race-picks/", utc_date)
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

    # Format probabilities as percentages (matching race-picks.R format)
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

    # Create sheet name matching race-picks.R format
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

# Print summary
cat("\n=== ALPINE RACE PICKS SIMULATION COMPLETE ===\n")
cat(paste("Date:", current_date, "\n"))
cat(paste("Races processed:", length(individual_results), "\n"))

if (length(individual_results) > 0) {
  for (race_key in names(individual_results)) {
    entry <- individual_results[[race_key]]
    top_pred <- entry$predictions[1, ]
    cat(paste("  ", race_key, "- Winner pick:", top_pred$Skier,
              "(", round(top_pred$prob_top_1 * 100, 1), "%)\n"))
  }
}

cat(paste("\nOutput directory:", output_dir, "\n"))

log_info("=== ALPINE RACE-PICKS-SIMULATION.R COMPLETE ===")
