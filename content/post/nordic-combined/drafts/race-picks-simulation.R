# Nordic Combined Race Picks - Monte Carlo Simulation
# Simulation-based approach for individual and team race predictions
#
# Features:
# - Monte Carlo simulation (10,000 iterations)
# - Exponential decay weighting for historical performance
# - Race type distributions (Individual, IndividualCompact, Mass Start, Sprint)
# - Team event support: Team, Team Sprint, Mixed Team, Mixed Team Sprint
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

# Team variance parameters
TEAM_SD_SCALE_FACTOR <- 0.8
TEAM_SD_MIN <- 3
TEAM_SD_MAX <- 12

# Position thresholds
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)  # Win, Podium, Top-5, Top-10, Top-30

# Points system for Nordic Combined (same for all events)
nc_points <- c(100, 90, 80, 70, 60, 55, 52, 49, 46, 43, 40, 38, 36, 34, 32,
               30, 28, 26, 24, 22, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11,
               10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Race type definitions
INDIVIDUAL_DISCIPLINES <- c("Individual", "IndividualCompact")
MASS_START_DISCIPLINES <- c("Mass Start")
SPRINT_DISCIPLINES <- c("Sprint")
TEAM_DISCIPLINES <- c("Team", "Team Sprint", "Mixed Team", "Mixed Team Sprint")

# ============================================================================
# LOGGING SETUP
# ============================================================================

log_dir <- "~/ski/elo/python/nordic-combined/polars/excel365/race-picks-simulation"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "simulation.log")))
log_info("=== NORDIC COMBINED RACE-PICKS-SIMULATION.R STARTED ===")
log_info(paste("TEST_MODE:", TEST_MODE))

# ============================================================================
# DATA LOADING
# ============================================================================

log_info("Loading data files...")

base_path <- "~/ski/elo/python/nordic-combined/polars/excel365"
team_base_path <- "~/ski/elo/python/nordic-combined/polars/relay/excel365"

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

# Filter to individual races (exclude team events)
individual_races <- today_races %>%
  filter(!RaceType %in% TEAM_DISCIPLINES)

# Filter to team races
men_teams <- today_races %>% filter(RaceType == "Team", Sex == "M")
ladies_teams <- today_races %>% filter(RaceType == "Team", Sex == "L")
men_team_sprint <- today_races %>% filter(RaceType == "Team Sprint", Sex == "M")
ladies_team_sprint <- today_races %>% filter(RaceType == "Team Sprint", Sex == "L")
mixed_teams <- today_races %>% filter(RaceType == "Mixed Team" | (RaceType == "Team" & Sex == "Mixed"))
mixed_team_sprint <- today_races %>% filter(RaceType == "Mixed Team Sprint" | (RaceType == "Team Sprint" & Sex == "Mixed"))

has_individual <- nrow(individual_races) > 0
has_team <- nrow(men_teams) + nrow(ladies_teams) + nrow(men_team_sprint) +
            nrow(ladies_team_sprint) + nrow(mixed_teams) + nrow(mixed_team_sprint) > 0

if(!has_individual && !has_team) {
  log_info("No individual or team races found for today. Exiting.")
  quit(save = "no", status = 0)
}

log_info(paste("Teams found - Men:", nrow(men_teams), "| Ladies:", nrow(ladies_teams),
               "| Men Sprint:", nrow(men_team_sprint), "| Ladies Sprint:", nrow(ladies_team_sprint),
               "| Mixed:", nrow(mixed_teams), "| Mixed Sprint:", nrow(mixed_team_sprint)))

# Separate individual races by gender
men_races <- individual_races %>% filter(Sex == "M")
ladies_races <- individual_races %>% filter(Sex == "L")

log_info(paste("Men's individual races:", nrow(men_races), "| Ladies' races:", nrow(ladies_races)))

# Load chronological data with error handling
men_chrono <- tryCatch({
  df <- read.csv(file.path(base_path, "men_chrono_elevation.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded men_chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_error(paste("Failed to load men_chrono:", e$message))
  data.frame()
})

ladies_chrono <- tryCatch({
  df <- read.csv(file.path(base_path, "ladies_chrono_elevation.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded ladies_chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_error(paste("Failed to load ladies_chrono:", e$message))
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

# Load team startlists
normalize_team_startlist_columns <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }

  elo_to_pelo <- c(
    "Avg_Elo" = "Avg_Pelo",
    "Avg_Individual_Elo" = "Avg_Individual_Pelo",
    "Avg_Sprint_Elo" = "Avg_Sprint_Pelo",
    "Avg_MassStart_Elo" = "Avg_MassStart_Pelo",
    "Avg_IndividualCompact_Elo" = "Avg_IndividualCompact_Pelo"
  )

  for (src_col in names(elo_to_pelo)) {
    dst_col <- elo_to_pelo[[src_col]]
    if (src_col %in% names(df) && !dst_col %in% names(df)) {
      df[[dst_col]] <- df[[src_col]]
    }
  }

  pelo_cols <- intersect(unname(elo_to_pelo), names(df))
  for (pelo_col in pelo_cols) {
    pct_col <- paste0(pelo_col, "_pct")
    col_max <- suppressWarnings(max(df[[pelo_col]], na.rm = TRUE))
    if (is.finite(col_max) && !is.na(col_max) && col_max > 0) {
      df[[pct_col]] <- df[[pelo_col]] / col_max
    }
  }

  df
}

men_team_startlist <- tryCatch({
  df <- read.csv(file.path(team_base_path, "startlist_team_races_men.csv"), stringsAsFactors = FALSE)
  df <- normalize_team_startlist_columns(df)
  log_info(paste("Loaded men's team startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No men's team startlist:", e$message))
  data.frame()
})

ladies_team_startlist <- tryCatch({
  df <- read.csv(file.path(team_base_path, "startlist_team_races_ladies.csv"), stringsAsFactors = FALSE)
  df <- normalize_team_startlist_columns(df)
  log_info(paste("Loaded ladies' team startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No ladies' team startlist:", e$message))
  data.frame()
})

men_team_sprint_startlist <- tryCatch({
  df <- read.csv(file.path(team_base_path, "startlist_team_sprint_races_men.csv"), stringsAsFactors = FALSE)
  df <- normalize_team_startlist_columns(df)
  log_info(paste("Loaded men's team sprint startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No men's team sprint startlist:", e$message))
  data.frame()
})

ladies_team_sprint_startlist <- tryCatch({
  df <- read.csv(file.path(team_base_path, "startlist_team_sprint_races_ladies.csv"), stringsAsFactors = FALSE)
  df <- normalize_team_startlist_columns(df)
  log_info(paste("Loaded ladies' team sprint startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No ladies' team sprint startlist:", e$message))
  data.frame()
})

mixed_team_startlist <- tryCatch({
  df <- read.csv(file.path(team_base_path, "startlist_mixed_team_races_teams.csv"), stringsAsFactors = FALSE)
  df <- normalize_team_startlist_columns(df)
  log_info(paste("Loaded mixed team startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No mixed team startlist:", e$message))
  data.frame()
})

mixed_team_sprint_startlist <- tryCatch({
  df <- read.csv(file.path(team_base_path, "startlist_mixed_team_sprint_races_teams.csv"), stringsAsFactors = FALSE)
  df <- normalize_team_startlist_columns(df)
  log_info(paste("Loaded mixed team sprint startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No mixed team sprint startlist:", e$message))
  data.frame()
})

# Load team chronological data
men_team_chrono <- tryCatch({
  df <- read.csv(file.path(team_base_path, "men_team_chrono.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded men's team chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_info(paste("No men's team chrono:", e$message))
  data.frame()
})

ladies_team_chrono <- tryCatch({
  df <- read.csv(file.path(team_base_path, "ladies_team_chrono.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded ladies' team chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_info(paste("No ladies' team chrono:", e$message))
  data.frame()
})

mixed_team_chrono <- tryCatch({
  df <- read.csv(file.path(team_base_path, "mixed_team_chrono.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded mixed team chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_info(paste("No mixed team chrono:", e$message))
  data.frame()
})

team_sprint_chrono <- tryCatch({
  df <- read.csv(file.path(team_base_path, "team_sprint_chrono.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded team sprint chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_info(paste("No team sprint chrono:", e$message))
  data.frame()
})

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Get points from place
get_points <- function(place, race_type = "Individual") {
  if (is.na(place) || place < 1 || place > length(nc_points)) {
    return(0)
  }
  return(nc_points[place])
}

if (nrow(men_chrono) > 0 && !"Points" %in% names(men_chrono) && "Place" %in% names(men_chrono)) {
  men_place <- suppressWarnings(as.integer(men_chrono$Place))
  men_chrono$Points <- vapply(men_place, get_points, numeric(1))
  log_info("Derived Points column for men_chrono from Place using get_points()")
}

if (nrow(ladies_chrono) > 0 && !"Points" %in% names(ladies_chrono) && "Place" %in% names(ladies_chrono)) {
  ladies_place <- suppressWarnings(as.integer(ladies_chrono$Place))
  ladies_chrono$Points <- vapply(ladies_place, get_points, numeric(1))
  log_info("Derived Points column for ladies_chrono from Place using get_points()")
}

# Replace NA with first quartile
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Filter features to keep only those with positive coefficients
filter_positive_coefficients <- function(data, response_var, candidate_vars) {
  if (length(candidate_vars) == 0) return(character(0))
  current_vars <- candidate_vars
  for (iter in 1:length(candidate_vars)) {
    if (length(current_vars) == 0) break
    formula_str <- paste(response_var, "~", paste(current_vars, collapse = " + "))
    model <- tryCatch({ lm(as.formula(formula_str), data = data) }, error = function(e) NULL)
    if (is.null(model)) break
    coefs <- coef(model)
    coefs <- coefs[names(coefs) != "(Intercept)"]
    negative_vars <- names(coefs[coefs < 0])
    if (length(negative_vars) == 0) break
    current_vars <- setdiff(current_vars, negative_vars)
  }
  return(current_vars)
}

# Get explanatory variables for GAM (simplified for NC)
get_explanatory_vars <- function(race_type) {
  return(c("prev_points_weighted", "Pelo_pct"))
}

# Calculate exponential decay weighted previous points
get_weighted_prev_points <- function(chrono_data, athlete_id, race_type, reference_date) {
  # Filter for this athlete and similar race types
  if (race_type %in% INDIVIDUAL_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% INDIVIDUAL_DISCIPLINES)
  } else if (race_type %in% MASS_START_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% MASS_START_DISCIPLINES)
  } else if (race_type %in% SPRINT_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% SPRINT_DISCIPLINES)
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
    races$Points <- sapply(races$Place, function(p) get_points(p, race_type))
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
  if (!"Pelo" %in% names(chrono_data)) return(chrono_data)
  chrono_data %>%
    group_by(Season, Race) %>%
    mutate(Pelo_pct = Pelo / max(Pelo, na.rm = TRUE)) %>%
    ungroup()
}

calculate_weighted_prev_points <- function(chrono_data, decay_lambda = DECAY_LAMBDA) {
  chrono_data <- chrono_data %>% arrange(ID, Date)

  n_rows <- nrow(chrono_data)
  if (n_rows == 0) {
    chrono_data$prev_points_weighted <- numeric(0)
    return(chrono_data)
  }

  if (!"Points" %in% names(chrono_data) && "Place" %in% names(chrono_data)) {
    place_num <- suppressWarnings(as.integer(chrono_data$Place))
    chrono_data$Points <- vapply(place_num, get_points, numeric(1))
  }

  prev_points_weighted <- numeric(n_rows)
  athlete_row_groups <- split(seq_len(n_rows), chrono_data$ID)

  for (row_idx in athlete_row_groups) {
    if (length(row_idx) <= 1) {
      next
    }

    athlete_points <- chrono_data$Points[row_idx]
    athlete_dates <- chrono_data$Date[row_idx]

    for (local_idx in 2:length(row_idx)) {
      prev_local_idx <- seq_len(local_idx - 1)
      days_ago <- as.numeric(difftime(athlete_dates[local_idx], athlete_dates[prev_local_idx], units = "days"))
      weights <- exp(-decay_lambda * days_ago)

      prev_points_weighted[row_idx[local_idx]] <- weighted.mean(
        athlete_points[prev_local_idx],
        weights,
        na.rm = TRUE
      )
    }
  }

  chrono_data$prev_points_weighted <- prev_points_weighted
  chrono_data
}

# ============================================================================
# GAM MODEL TRAINING
# ============================================================================

train_points_gam <- function(chrono_data, race_type, gender) {
  log_info(paste("Training", gender, race_type, "POINTS GAM"))

  if (race_type %in% INDIVIDUAL_DISCIPLINES) {
    filtered_data <- chrono_data %>% filter(RaceType %in% INDIVIDUAL_DISCIPLINES)
  } else if (race_type %in% MASS_START_DISCIPLINES) {
    filtered_data <- chrono_data %>% filter(RaceType %in% MASS_START_DISCIPLINES)
  } else if (race_type %in% SPRINT_DISCIPLINES) {
    filtered_data <- chrono_data %>% filter(RaceType %in% SPRINT_DISCIPLINES)
  } else {
    filtered_data <- chrono_data %>% filter(RaceType == race_type)
  }

  if (!"Points" %in% names(filtered_data)) {
    filtered_data$Points <- sapply(filtered_data$Place, function(p) get_points(p, race_type))
  }

  if (nrow(filtered_data) < 50) {
    log_warn(paste("Insufficient data for", race_type))
    return(NULL)
  }

  explanatory_vars <- get_explanatory_vars(race_type)
  available_vars <- intersect(explanatory_vars, names(filtered_data))
  if (length(available_vars) == 0) {
    log_warn("No explanatory variables available")
    return(NULL)
  }

  tryCatch({
    formula <- as.formula(paste("Points ~", paste(available_vars, collapse = " + ")))
    feature_selection <- regsubsets(formula, data = filtered_data, nbest = 1, method = "exhaustive")
    feature_summary <- summary(feature_selection)
    best_bic_vars <- names(coef(feature_selection, which.min(feature_summary$bic)))[-1]
    positive_vars <- filter_positive_coefficients(filtered_data, "Points", best_bic_vars)
    if (length(positive_vars) == 0) positive_vars <- available_vars[1]

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

  if (race_type %in% INDIVIDUAL_DISCIPLINES) {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% INDIVIDUAL_DISCIPLINES) %>%
      arrange(desc(Date)) %>% head(n_history)
  } else if (race_type %in% MASS_START_DISCIPLINES) {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% MASS_START_DISCIPLINES) %>%
      arrange(desc(Date)) %>% head(n_history)
  } else if (race_type %in% SPRINT_DISCIPLINES) {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% SPRINT_DISCIPLINES) %>%
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
      history_points <- sapply(athlete_history$Place, function(p) get_points(p, race_type))
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
    gam_fill_points <- pmax(0, pmin(100, gam_fill_points))
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

# Monte Carlo simulation for race positions (vectorized)
simulate_race_positions <- function(athlete_distributions, n_simulations = N_SIMULATIONS,
                                    position_thresholds = POSITION_THRESHOLDS,
                                    max_points = 100) {
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
  means <- sapply(valid_distributions, function(x) x$mean)
  sds <- sapply(valid_distributions, function(x) x$sd)

  scaled_sds <- pmax(SD_MIN, pmin(SD_MAX, sds * SD_SCALE_FACTOR))

  all_sims <- matrix(rnorm(n_athletes * n_simulations),
                     nrow = n_athletes, ncol = n_simulations)
  all_sims <- all_sims * scaled_sds + means
  all_sims[all_sims < 0] <- 0
  all_sims[all_sims > max_points] <- max_points

  if (is.null(dim(all_sims)) || nrow(all_sims) == 0 || ncol(all_sims) == 0) {
    log_error("Simulation matrix is invalid after bounds enforcement")
    return(data.frame())
  }

  ranks_matrix <- apply(all_sims, 2, function(x) rank(-x, ties.method = "random"))

  position_counts <- matrix(0, nrow = n_athletes, ncol = length(position_thresholds))
  for (t_idx in seq_along(position_thresholds)) {
    threshold <- position_thresholds[t_idx]
    position_counts[, t_idx] <- rowSums(ranks_matrix <= threshold)
  }

  position_probs <- position_counts / n_simulations

  results <- data.frame(
    athlete_id = athlete_ids,
    mean_points = means,
    sd_points = sds,
    n_actual_races = sapply(valid_distributions, function(x) x$n_actual_races),
    stringsAsFactors = FALSE
  )

  for (t_idx in seq_along(position_thresholds)) {
    col_name <- paste0("prob_top_", position_thresholds[t_idx])
    results[[col_name]] <- position_probs[, t_idx]
  }

  results <- results %>% arrange(desc(mean_points))
  return(results)
}

# ============================================================================
# TEAM HELPER FUNCTIONS
# ============================================================================

# Get weighted previous points for a team (nation-based)
get_team_weighted_prev_points <- function(team_chrono, nation, reference_date) {
  if (nrow(team_chrono) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
  }

  races <- team_chrono %>%
    filter(Nation == nation)

  if (nrow(races) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
  }

  if (!"Points" %in% names(races)) {
    races$Points <- sapply(races$Place, function(p) get_points(p, "Team"))
  }

  races <- races %>%
    mutate(
      days_ago = as.numeric(reference_date - Date),
      weight = exp(-DECAY_LAMBDA * pmax(0, days_ago))
    ) %>%
    filter(days_ago >= 0, !is.na(Points))

  if (nrow(races) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
  }

  weighted_mean <- sum(races$Points * races$weight) / sum(races$weight)

  if (nrow(races) >= 2) {
    weighted_var <- sum(races$weight * (races$Points - weighted_mean)^2) / sum(races$weight)
    weighted_sd <- sqrt(weighted_var)
  } else {
    weighted_sd <- TEAM_SD_MAX / 2
  }

  return(list(
    mean = weighted_mean,
    sd = weighted_sd,
    n = nrow(races)
  ))
}

# Build team distribution for simulation
build_team_distribution <- function(nation, team_chrono, startlist_row,
                                    reference_date = NULL) {
  if (is.null(reference_date)) {
    reference_date <- Sys.Date()
  }

  hist_stats <- get_team_weighted_prev_points(team_chrono, nation, reference_date)

  # Use normalized average Pelo values from the current lineup as the primary signal.
  avg_pelo_pct <- NA
  pelo_pct_cols <- grep("^Avg_.*Pelo_pct$", names(startlist_row), value = TRUE)
  if (length(pelo_pct_cols) > 0) {
    pelo_pct_values <- suppressWarnings(as.numeric(unlist(startlist_row[pelo_pct_cols])))
    avg_pelo_pct <- mean(pelo_pct_values, na.rm = TRUE)
  }

  if (!is.na(avg_pelo_pct) && avg_pelo_pct > 0) {
    mean_points <- avg_pelo_pct * 70
    sd_points <- if (!is.na(hist_stats$sd)) hist_stats$sd else TEAM_SD_MAX / 2
  } else if (!is.na(hist_stats$mean) && hist_stats$n >= 2) {
    mean_points <- hist_stats$mean
    sd_points <- hist_stats$sd
  } else if (!is.na(hist_stats$mean)) {
    mean_points <- hist_stats$mean
    sd_points <- if (!is.na(hist_stats$sd)) hist_stats$sd else TEAM_SD_MAX / 2
  } else {
    mean_points <- 10
    sd_points <- TEAM_SD_MAX
  }

  sd_points <- pmax(TEAM_SD_MIN, pmin(TEAM_SD_MAX, sd_points))

  return(list(
    nation = nation,
    mean = mean_points,
    sd = sd_points,
    n_actual_races = hist_stats$n
  ))
}

# Monte Carlo simulation for teams (vectorized)
simulate_team_positions <- function(team_distributions, n_simulations = N_SIMULATIONS,
                                    position_thresholds = POSITION_THRESHOLDS,
                                    max_points = 100) {
  valid_distributions <- Filter(function(dist) {
    !is.null(dist) && !is.null(dist$nation) && !is.na(dist$nation) &&
    !is.null(dist$mean) && !is.na(dist$mean) &&
    !is.null(dist$sd) && !is.na(dist$sd)
  }, team_distributions)

  if (length(valid_distributions) == 0) {
    log_error("No valid team distributions to simulate")
    return(data.frame())
  }

  n_teams <- length(valid_distributions)
  nations <- sapply(valid_distributions, function(x) x$nation)
  means <- sapply(valid_distributions, function(x) x$mean)
  sds <- sapply(valid_distributions, function(x) x$sd)

  scaled_sds <- pmax(TEAM_SD_MIN, pmin(TEAM_SD_MAX, sds * TEAM_SD_SCALE_FACTOR))

  all_sims <- matrix(rnorm(n_teams * n_simulations),
                     nrow = n_teams, ncol = n_simulations)
  all_sims <- all_sims * scaled_sds + means
  all_sims[all_sims < 0] <- 0
  all_sims[all_sims > max_points] <- max_points

  if (is.null(dim(all_sims)) || nrow(all_sims) == 0 || ncol(all_sims) == 0) {
    log_error("Simulation matrix is invalid after bounds enforcement")
    return(data.frame())
  }

  ranks_matrix <- apply(all_sims, 2, function(x) rank(-x, ties.method = "random"))

  position_counts <- matrix(0, nrow = n_teams, ncol = length(position_thresholds))
  for (t_idx in seq_along(position_thresholds)) {
    threshold <- position_thresholds[t_idx]
    position_counts[, t_idx] <- rowSums(ranks_matrix <= threshold)
  }

  position_probs <- position_counts / n_simulations

  results <- data.frame(
    Nation = nations,
    mean_points = means,
    sd_points = sds,
    n_actual_races = sapply(valid_distributions, function(x) x$n_actual_races),
    stringsAsFactors = FALSE
  )

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
all_race_types <- all_race_types[!is.na(all_race_types) & !all_race_types %in% TEAM_DISCIPLINES]
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

    race_prob_col <- NULL
    candidate_prob_cols <- c(paste0("Race", i, "_Prob"), paste0("Race_Prob", i))
    matching_prob_cols <- candidate_prob_cols[candidate_prob_cols %in% names(startlist)]
    if (length(matching_prob_cols) > 0) {
      race_prob_col <- matching_prob_cols[1]
      log_info(paste("Using startlist probability column:", race_prob_col))
    } else {
      log_warn(paste("No race probability column found for race", i, "- using full startlist"))
    }

    race_startlist <- startlist %>%
      filter(!is.na(ID), !is.na(Skier))

    if (!is.null(race_prob_col)) {
      race_startlist <- race_startlist %>%
        filter(!is.na(.data[[race_prob_col]]), .data[[race_prob_col]] > 0)
    }

    if (nrow(race_startlist) == 0) {
      log_warn(paste("No athletes in startlist for", gender, race_type))
      next
    }

    log_info(paste("Athletes in startlist:", nrow(race_startlist)))

    # Get GAM model for this race type
    model_info <- gam_models[[race_type]]

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

    log_info(paste("Running", N_SIMULATIONS, "Monte Carlo simulations"))
    race_results <- simulate_race_positions(athlete_distributions)

    sex_col <- if ("Sex" %in% names(race_startlist)) "Sex" else NULL
    startlist_cols <- c("ID", "Skier", "Nation")
    if (!is.null(sex_col)) startlist_cols <- c(startlist_cols, sex_col)

    race_results <- race_results %>%
      left_join(
        race_startlist %>% select(all_of(startlist_cols)) %>% distinct(),
        by = c("athlete_id" = "ID")
      ) %>%
      rename(ID = athlete_id)

    if (!"Sex" %in% names(race_results)) {
      race_results$Sex <- ifelse(gender == "men", "M", "L")
    }

    race_results <- race_results %>%
      select(Skier, Nation, ID, Sex, mean_points, sd_points, n_actual_races,
             starts_with("prob_top_")) %>%
      arrange(desc(prob_top_1))

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
# TEAM RACE SIMULATION
# ============================================================================

log_info("=== TEAM RACE SIMULATION ===")

team_results <- list()

# Process Men's Team
if (nrow(men_teams) > 0 && nrow(men_team_startlist) > 0) {
  log_info("Processing Men's Team")

  team_distributions <- list()

  for (i in 1:nrow(men_team_startlist)) {
    nation <- men_team_startlist$Nation[i]

    dist <- build_team_distribution(
      nation = nation,
      team_chrono = men_team_chrono,
      startlist_row = men_team_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  team_sim_results <- simulate_team_positions(team_distributions)

  team_sim_results <- team_sim_results %>%
    mutate(Sex = "M", RaceType = "Team") %>%
    arrange(desc(prob_top_1))

  team_results[["men_team"]] <- list(
    race_type = "Team",
    gender = "men",
    predictions = team_sim_results
  )

  log_info(paste("Men's Team complete - Top 3:"))
  print(head(team_sim_results %>% select(Nation, prob_top_1, prob_top_3), 3))
}

# Process Ladies' Team
if (nrow(ladies_teams) > 0 && nrow(ladies_team_startlist) > 0) {
  log_info("Processing Ladies' Team")

  team_distributions <- list()

  for (i in 1:nrow(ladies_team_startlist)) {
    nation <- ladies_team_startlist$Nation[i]

    dist <- build_team_distribution(
      nation = nation,
      team_chrono = ladies_team_chrono,
      startlist_row = ladies_team_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  team_sim_results <- simulate_team_positions(team_distributions)

  team_sim_results <- team_sim_results %>%
    mutate(Sex = "L", RaceType = "Team") %>%
    arrange(desc(prob_top_1))

  team_results[["ladies_team"]] <- list(
    race_type = "Team",
    gender = "ladies",
    predictions = team_sim_results
  )

  log_info(paste("Ladies' Team complete - Top 3:"))
  print(head(team_sim_results %>% select(Nation, prob_top_1, prob_top_3), 3))
}

# Process Men's Team Sprint
if (nrow(men_team_sprint) > 0 && nrow(men_team_sprint_startlist) > 0) {
  log_info("Processing Men's Team Sprint")

  team_distributions <- list()

  for (i in 1:nrow(men_team_sprint_startlist)) {
    nation <- men_team_sprint_startlist$Nation[i]

    dist <- build_team_distribution(
      nation = nation,
      team_chrono = men_team_chrono,
      startlist_row = men_team_sprint_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  team_sim_results <- simulate_team_positions(team_distributions)

  team_sim_results <- team_sim_results %>%
    mutate(Sex = "M", RaceType = "Team Sprint") %>%
    arrange(desc(prob_top_1))

  team_results[["men_team_sprint"]] <- list(
    race_type = "Team Sprint",
    gender = "men",
    predictions = team_sim_results
  )

  log_info(paste("Men's Team Sprint complete - Top 3:"))
  print(head(team_sim_results %>% select(Nation, prob_top_1, prob_top_3), 3))
}

# Process Ladies' Team Sprint
if (nrow(ladies_team_sprint) > 0 && nrow(ladies_team_sprint_startlist) > 0) {
  log_info("Processing Ladies' Team Sprint")

  team_distributions <- list()

  for (i in 1:nrow(ladies_team_sprint_startlist)) {
    nation <- ladies_team_sprint_startlist$Nation[i]

    dist <- build_team_distribution(
      nation = nation,
      team_chrono = ladies_team_chrono,
      startlist_row = ladies_team_sprint_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  team_sim_results <- simulate_team_positions(team_distributions)

  team_sim_results <- team_sim_results %>%
    mutate(Sex = "L", RaceType = "Team Sprint") %>%
    arrange(desc(prob_top_1))

  team_results[["ladies_team_sprint"]] <- list(
    race_type = "Team Sprint",
    gender = "ladies",
    predictions = team_sim_results
  )

  log_info(paste("Ladies' Team Sprint complete - Top 3:"))
  print(head(team_sim_results %>% select(Nation, prob_top_1, prob_top_3), 3))
}

# Process Mixed Team
if (nrow(mixed_teams) > 0 && nrow(mixed_team_startlist) > 0) {
  log_info("Processing Mixed Team")

  team_distributions <- list()

  for (i in 1:nrow(mixed_team_startlist)) {
    nation <- mixed_team_startlist$Nation[i]

    dist <- build_team_distribution(
      nation = nation,
      team_chrono = mixed_team_chrono,
      startlist_row = mixed_team_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  team_sim_results <- simulate_team_positions(team_distributions)

  team_sim_results <- team_sim_results %>%
    mutate(Sex = "Mixed", RaceType = "Mixed Team") %>%
    arrange(desc(prob_top_1))

  team_results[["mixed_team"]] <- list(
    race_type = "Mixed Team",
    gender = "mixed",
    predictions = team_sim_results
  )

  log_info(paste("Mixed Team complete - Top 3:"))
  print(head(team_sim_results %>% select(Nation, prob_top_1, prob_top_3), 3))
}

# Process Mixed Team Sprint
if (nrow(mixed_team_sprint) > 0 && nrow(mixed_team_sprint_startlist) > 0) {
  log_info("Processing Mixed Team Sprint")

  team_distributions <- list()

  for (i in 1:nrow(mixed_team_sprint_startlist)) {
    nation <- mixed_team_sprint_startlist$Nation[i]

    dist <- build_team_distribution(
      nation = nation,
      team_chrono = team_sprint_chrono,
      startlist_row = mixed_team_sprint_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  team_sim_results <- simulate_team_positions(team_distributions)

  team_sim_results <- team_sim_results %>%
    mutate(Sex = "Mixed", RaceType = "Mixed Team Sprint") %>%
    arrange(desc(prob_top_1))

  team_results[["mixed_team_sprint"]] <- list(
    race_type = "Mixed Team Sprint",
    gender = "mixed",
    predictions = team_sim_results
  )

  log_info(paste("Mixed Team Sprint complete - Top 3:"))
  print(head(team_sim_results %>% select(Nation, prob_top_1, prob_top_3), 3))
}

log_info(paste("Team simulation complete.", length(team_results), "team races processed"))

# ============================================================================
# OUTPUT: GENERATE EXCEL FILES
# ============================================================================

log_info("=== OUTPUT GENERATION ===")

utc_date <- format(Sys.time(), "%Y%m%d", tz = "UTC")
output_dir <- paste0("~/blog/daehl-e/content/post/nordic-combined/drafts/race-picks/", utc_date)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Format individual results for Excel
format_individual_results <- function(results_list) {
  formatted <- list()
  men_race_num <- 0
  ladies_race_num <- 0

  for (race_key in names(results_list)) {
    entry <- results_list[[race_key]]
    predictions <- entry$predictions

    output_data <- predictions %>%
      mutate(
        Participation = 100,
        Win = round(prob_top_1 * 100, 1),
        Podium = round(prob_top_3 * 100, 1),
        `Top-5` = round(prob_top_5 * 100, 1),
        `Top-10` = round(prob_top_10 * 100, 1),
        `Top-30` = round(prob_top_30 * 100, 1)
      ) %>%
      select(Skier, ID, Nation, Sex, Participation, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
      arrange(desc(Win))

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

# Format team results for Excel
format_team_results <- function(results_list) {
  formatted <- list()

  for (team_key in names(results_list)) {
    entry <- results_list[[team_key]]
    predictions <- entry$predictions

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

    sheet_name <- team_key
    formatted[[sheet_name]] <- output_data
  }

  return(formatted)
}

# Generate individual output files
if (length(individual_results) > 0) {
  individual_formatted <- format_individual_results(individual_results)

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

# Generate team output files
if (length(team_results) > 0) {
  team_formatted <- format_team_results(team_results)

  # Men's Team
  if ("men_team" %in% names(team_formatted)) {
    men_team_data <- team_formatted[["men_team"]] %>% filter(Sex == "M")
    if (nrow(men_team_data) > 0) {
      men_team_file <- file.path(output_dir, "men_team_position_probabilities.xlsx")
      write.xlsx(list("Men Team" = men_team_data), men_team_file)
      log_info(paste("Saved men's team predictions to", men_team_file))
    }
  }

  if ("ladies_team" %in% names(team_formatted)) {
    ladies_team_data <- team_formatted[["ladies_team"]] %>% filter(Sex == "L")
    if (nrow(ladies_team_data) > 0) {
      ladies_team_file <- file.path(output_dir, "ladies_team_position_probabilities.xlsx")
      write.xlsx(list("Ladies Team" = ladies_team_data), ladies_team_file)
      log_info(paste("Saved ladies' team predictions to", ladies_team_file))
    }
  }

  # Team Sprint
  if ("men_team_sprint" %in% names(team_formatted)) {
    men_ts_data <- team_formatted[["men_team_sprint"]] %>% filter(Sex == "M")
    if (nrow(men_ts_data) > 0) {
      men_ts_file <- file.path(output_dir, "men_team_sprint_position_probabilities.xlsx")
      write.xlsx(list("Men Team Sprint" = men_ts_data), men_ts_file)
      log_info(paste("Saved men's team sprint predictions to", men_ts_file))
    }
  }

  if ("ladies_team_sprint" %in% names(team_formatted)) {
    ladies_ts_data <- team_formatted[["ladies_team_sprint"]] %>% filter(Sex == "L")
    if (nrow(ladies_ts_data) > 0) {
      ladies_ts_file <- file.path(output_dir, "ladies_team_sprint_position_probabilities.xlsx")
      write.xlsx(list("Ladies Team Sprint" = ladies_ts_data), ladies_ts_file)
      log_info(paste("Saved ladies' team sprint predictions to", ladies_ts_file))
    }
  }

  # Mixed Team
  if ("mixed_team" %in% names(team_formatted)) {
    mixed_team_file <- file.path(output_dir, "mixed_team_position_probabilities.xlsx")
    write.xlsx(list("Mixed Team" = team_formatted[["mixed_team"]]), mixed_team_file)
    log_info(paste("Saved mixed team predictions to", mixed_team_file))
  }

  # Mixed Team Sprint
  if ("mixed_team_sprint" %in% names(team_formatted)) {
    mixed_team_sprint_file <- file.path(output_dir, "mixed_team_sprint_position_probabilities.xlsx")
    write.xlsx(list("Mixed Team Sprint" = team_formatted[["mixed_team_sprint"]]), mixed_team_sprint_file)
    log_info(paste("Saved mixed team sprint predictions to", mixed_team_sprint_file))
  }
}

# Print summary
cat("\n=== NORDIC COMBINED RACE PICKS SIMULATION COMPLETE ===\n")
cat(paste("Date:", current_date, "\n"))
cat(paste("Individual races processed:", length(individual_results), "\n"))
cat(paste("Team races processed:", length(team_results), "\n"))

if (length(individual_results) > 0) {
  cat("\nIndividual Race Predictions:\n")
  for (race_key in names(individual_results)) {
    entry <- individual_results[[race_key]]
    top_pred <- entry$predictions[1, ]
    cat(paste("  ", race_key, "- Winner pick:", top_pred$Skier,
              "(", round(top_pred$prob_top_1 * 100, 1), "%)\n"))
  }
}

if (length(team_results) > 0) {
  cat("\nTeam Race Predictions:\n")
  for (team_key in names(team_results)) {
    entry <- team_results[[team_key]]
    top_pred <- entry$predictions[1, ]
    cat(paste("  ", team_key, "- Winner pick:", top_pred$Nation,
              "(", round(top_pred$prob_top_1 * 100, 1), "%)\n"))
  }
}

cat(paste("\nOutput directory:", output_dir, "\n"))

log_info("=== NORDIC COMBINED RACE-PICKS-SIMULATION.R COMPLETE ===")
