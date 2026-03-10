# Ski Jumping Championships Predictions - Monte Carlo Simulation
# Simulation-based approach for World Championships predictions
#
# Features:
# - Monte Carlo simulation (10,000 iterations)
# - Exponential decay weighting for historical performance
# - Hill size distributions (Large, Normal, Flying)
# - Team event support: Team Large, Team Normal, Mixed Team
# - 4-person quota constraint per nation per race
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
N_SIMULATIONS <- 10000
DECAY_LAMBDA <- 0.002
SD_SCALE_FACTOR <- 0.77
SD_MIN <- 4
SD_MAX <- 16

# GAM parameters for athletes with insufficient history
N_HISTORY_REQUIRED <- 10              # Target number of historical races per athlete
GAM_FILL_WEIGHT_FACTOR <- 0.25        # Weight multiplier for GAM-filled history slots

# Team variance parameters
TEAM_SD_SCALE_FACTOR <- 0.8
TEAM_SD_MIN <- 3
TEAM_SD_MAX <- 12

# Position thresholds
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)

# Points system for Ski Jumping (top 30)
sj_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16,
               15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Hill size definitions
LARGE_HILLS <- c("Large")
NORMAL_HILLS <- c("Normal")
FLYING_HILLS <- c("Flying")
TEAM_DISCIPLINES <- c("Team Large", "Team Normal", "Mixed Team")

# ============================================================================
# LOGGING SETUP
# ============================================================================

ENHANCED_LOGGING <- FALSE
logging_utils_path <- "~/blog/daehl-e/content/post/shared/logging-utils.R"
if (file.exists(path.expand(logging_utils_path))) {
  tryCatch({
    source(logging_utils_path)
    ENHANCED_LOGGING <- TRUE
    init_logging("skijump", "champs-predictions")
    log_config(list(
      TEST_MODE = TEST_MODE,
      N_HISTORY_REQUIRED = N_HISTORY_REQUIRED,
      N_SIMULATIONS = N_SIMULATIONS,
      DECAY_LAMBDA = DECAY_LAMBDA,
      SD_SCALE_FACTOR = SD_SCALE_FACTOR,
      SD_MIN = SD_MIN,
      SD_MAX = SD_MAX,
      TEAM_SD_SCALE_FACTOR = TEAM_SD_SCALE_FACTOR,
      TEAM_SD_MIN = TEAM_SD_MIN,
      TEAM_SD_MAX = TEAM_SD_MAX,
      GAM_FILL_WEIGHT_FACTOR = GAM_FILL_WEIGHT_FACTOR
    ))
  }, error = function(e) {
    ENHANCED_LOGGING <- FALSE
  })
}

if (!ENHANCED_LOGGING) {
  log_dir <- "~/ski/elo/python/skijump/polars/excel365/champs-predictions-simulation"
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  log_threshold(DEBUG)
  log_appender(appender_file(file.path(log_dir, "simulation.log")))
}

log_info("=== SKI JUMPING CHAMPS-PREDICTIONS-SIMULATION.R STARTED ===")
log_info(paste("TEST_MODE:", TEST_MODE))
log_info(paste("Enhanced logging:", ENHANCED_LOGGING))

# ============================================================================
# DATA LOADING
# ============================================================================

log_info("Loading data files...")
if (ENHANCED_LOGGING) phase_start("Load Championship Schedule")

base_path <- "~/ski/elo/python/skijump/polars/excel365"
team_base_path <- "~/ski/elo/python/skijump/polars/relay/excel365"

# Load weekends data to find championship races
weekends <- tryCatch({
  df <- read.csv(file.path(base_path, "weekends.csv"), stringsAsFactors = FALSE)
  if (nrow(df) == 0) {
    log_error("Weekends file is empty")
    quit(save = "no", status = 1)
  }
  df %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Race_Date = as.Date(Race_Date, format = "%m/%d/%Y")
    )
}, error = function(e) {
  log_error(paste("Failed to load weekends.csv:", e$message))
  quit(save = "no", status = 1)
})

# Filter for Championships races only
log_info("Filtering for Championships races")
champs_races <- weekends %>%
  filter(Championship == 1)

if (nrow(champs_races) == 0) {
  log_info("No Championships races found. Terminating program.")
  quit(save = "no", status = 0)
}

log_info(paste("Found", nrow(champs_races), "Championships races"))

# Create race dataframes with chronological ordering
champs_races_with_race_num <- champs_races %>%
  arrange(Race_Date) %>%
  mutate(OriginalRaceNum = row_number())

# Filter individual races (not team events)
men_races <- champs_races_with_race_num %>%
  filter(Sex == "M", !grepl("Team", RaceType, ignore.case = TRUE)) %>%
  arrange(Race_Date) %>%
  dplyr::select(RaceType, Period, Country, Race_Date, OriginalRaceNum) %>%
  rename(race_type = RaceType, period = Period, country = Country,
         race_date = Race_Date, original_race_num = OriginalRaceNum)

ladies_races <- champs_races_with_race_num %>%
  filter(Sex == "L", !grepl("Team", RaceType, ignore.case = TRUE)) %>%
  arrange(Race_Date) %>%
  dplyr::select(RaceType, Period, Country, Race_Date, OriginalRaceNum) %>%
  rename(race_type = RaceType, period = Period, country = Country,
         race_date = Race_Date, original_race_num = OriginalRaceNum)

# Filter team races
men_team_races <- champs_races_with_race_num %>%
  filter(Sex == "M", grepl("Team", RaceType, ignore.case = TRUE)) %>%
  arrange(Race_Date)

ladies_team_races <- champs_races_with_race_num %>%
  filter(Sex == "L", grepl("Team", RaceType, ignore.case = TRUE)) %>%
  arrange(Race_Date)

mixed_team_races <- champs_races_with_race_num %>%
  filter(Sex == "Mixed") %>%
  arrange(Race_Date)

log_info(paste("Found", nrow(men_races), "men's individual races,", nrow(ladies_races), "ladies' individual races"))
log_info(paste("Team races - Men:", nrow(men_team_races), "| Ladies:", nrow(ladies_team_races), "| Mixed:", nrow(mixed_team_races)))
if (ENHANCED_LOGGING) {
  phase_end("Load Championship Schedule", sprintf("%d championship races", nrow(champs_races)))
  phase_start("Load Chronological Data")
}

# Get current date
current_date <- Sys.Date()
log_info(paste("Current date:", current_date))

# Load chronological data
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

if (ENHANCED_LOGGING) {
  if (nrow(men_chrono) > 0) {
    log_data_quality(men_chrono, "Men's Chrono", c("ID", "Date", "RaceType", "Points"))
  }
  if (nrow(ladies_chrono) > 0) {
    log_data_quality(ladies_chrono, "Ladies' Chrono", c("ID", "Date", "RaceType", "Points"))
  }
}

# Load championships startlists
men_startlist <- tryCatch({
  df <- read.csv(file.path(base_path, "startlist_champs_men.csv"), stringsAsFactors = FALSE)
  required_cols <- c("ID", "Skier", "Nation")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    log_warn(paste("Men's startlist missing columns:", paste(missing_cols, collapse = ", ")))
  }
  df
}, error = function(e) {
  log_warn(paste("Could not load men's championships startlist:", e$message))
  data.frame()
})

ladies_startlist <- tryCatch({
  df <- read.csv(file.path(base_path, "startlist_champs_ladies.csv"), stringsAsFactors = FALSE)
  required_cols <- c("ID", "Skier", "Nation")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    log_warn(paste("Ladies' startlist missing columns:", paste(missing_cols, collapse = ", ")))
  }
  df
}, error = function(e) {
  log_warn(paste("Could not load ladies' championships startlist:", e$message))
  data.frame()
})

log_info(paste("Loaded champs startlists - Men:", nrow(men_startlist), "| Ladies:", nrow(ladies_startlist)))

# Load team startlists
normalize_team_startlist_columns <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }

  elo_to_pelo <- c(
    "Avg_Elo" = "Avg_Pelo",
    "Avg_Small_Elo" = "Avg_Small_Pelo",
    "Avg_Medium_Elo" = "Avg_Medium_Pelo",
    "Avg_Normal_Elo" = "Avg_Normal_Pelo",
    "Avg_Large_Elo" = "Avg_Large_Pelo",
    "Avg_Flying_Elo" = "Avg_Flying_Pelo"
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
  df <- read.csv(file.path(team_base_path, "startlist_team_champs_men.csv"), stringsAsFactors = FALSE)
  df <- normalize_team_startlist_columns(df)
  log_info(paste("Loaded men's team champs startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No men's team champs startlist:", e$message))
  data.frame()
})

ladies_team_startlist <- tryCatch({
  df <- read.csv(file.path(team_base_path, "startlist_team_champs_ladies.csv"), stringsAsFactors = FALSE)
  df <- normalize_team_startlist_columns(df)
  log_info(paste("Loaded ladies' team champs startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No ladies' team champs startlist:", e$message))
  data.frame()
})

mixed_team_startlist <- tryCatch({
  df <- read.csv(file.path(team_base_path, "startlist_mixed_team_champs.csv"), stringsAsFactors = FALSE)
  df <- normalize_team_startlist_columns(df)
  log_info(paste("Loaded mixed team champs startlist:", nrow(df), "teams"))
  df
}, error = function(e) {
  log_info(paste("No mixed team champs startlist:", e$message))
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

if (ENHANCED_LOGGING) {
  phase_end("Load Chronological Data")
  phase_start("Load Startlists")
  if (nrow(men_startlist) > 0) {
    log_data_quality(men_startlist, "Men's Champs Startlist", c("ID", "Skier", "Nation"))
  }
  if (nrow(ladies_startlist) > 0) {
    log_data_quality(ladies_startlist, "Ladies' Champs Startlist", c("ID", "Skier", "Nation"))
  }
  if (nrow(men_team_startlist) > 0) {
    log_data_quality(men_team_startlist, "Men's Team Startlist", c("Nation"))
  }
  if (nrow(ladies_team_startlist) > 0) {
    log_data_quality(ladies_team_startlist, "Ladies' Team Startlist", c("Nation"))
  }
  if (nrow(mixed_team_startlist) > 0) {
    log_data_quality(mixed_team_startlist, "Mixed Team Startlist", c("Nation"))
  }
  phase_end("Load Startlists")
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

get_points <- function(place, race_type = "Large") {
  if (is.na(place) || place < 1 || place > length(sj_points)) {
    return(0)
  }
  return(sj_points[place])
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

# Get hill category for grouping similar hills
get_hill_category <- function(race_type) {
  race_type_clean <- gsub("Team ", "", race_type)
  if (race_type_clean %in% c("Flying")) return("Flying")
  if (race_type_clean %in% c("Large")) return("Large")
  if (race_type_clean %in% c("Normal")) return("Normal")
  return("Other")
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

    if (is.null(model)) break

    coefs <- coef(model)
    coefs <- coefs[names(coefs) != "(Intercept)"]

    negative_vars <- names(coefs[coefs < 0])

    if (length(negative_vars) == 0) break

    current_vars <- setdiff(current_vars, negative_vars)
  }

  return(current_vars)
}

# Get hill-specific explanatory variables for GAM
get_explanatory_vars <- function(race_type) {
  base_vars <- c("prev_points_weighted", "Pelo_pct")
  hill_cat <- get_hill_category(race_type)

  if (hill_cat == "Flying") {
    return(c(base_vars, "Flying_Pelo_pct", "Large_Pelo_pct"))
  } else if (hill_cat == "Large") {
    return(c(base_vars, "Large_Pelo_pct", "Flying_Pelo_pct"))
  } else if (hill_cat == "Normal") {
    return(c(base_vars, "Normal_Pelo_pct", "Large_Pelo_pct"))
  }
  return(base_vars)
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
      Large_Pelo_pct = if ("Large_Pelo" %in% names(.)) `Large_Pelo` / max(`Large_Pelo`, na.rm = TRUE) else NA,
      Normal_Pelo_pct = if ("Normal_Pelo" %in% names(.)) `Normal_Pelo` / max(`Normal_Pelo`, na.rm = TRUE) else NA,
      Flying_Pelo_pct = if ("Flying_Pelo" %in% names(.)) `Flying_Pelo` / max(`Flying_Pelo`, na.rm = TRUE) else NA
    ) %>%
    ungroup()
}

# Calculate exponential decay weighted prev_points for GAM training
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
    athlete_race_types <- chrono_data$RaceType[row_idx]

    for (local_idx in 2:length(row_idx)) {
      prev_local_idx <- seq_len(local_idx - 1)
      current_hill <- get_hill_category(athlete_race_types[local_idx])

      if (current_hill == "Flying") {
        matching <- athlete_race_types[prev_local_idx] %in% c("Flying", "Large")
      } else if (current_hill == "Large") {
        matching <- athlete_race_types[prev_local_idx] %in% c("Large", "Flying")
      } else if (current_hill == "Normal") {
        matching <- athlete_race_types[prev_local_idx] %in% c("Normal", "Large")
      } else {
        matching <- rep(TRUE, length(prev_local_idx))
      }

      if (!any(matching)) {
        next
      }

      matching_points <- athlete_points[prev_local_idx][matching]
      matching_dates <- athlete_dates[prev_local_idx][matching]
      days_ago <- as.numeric(difftime(athlete_dates[local_idx], matching_dates, units = "days"))
      weights <- exp(-decay_lambda * days_ago)

      prev_points_weighted[row_idx[local_idx]] <- weighted.mean(matching_points, weights, na.rm = TRUE)
    }
  }

  chrono_data$prev_points_weighted <- prev_points_weighted
  chrono_data
}

# ============================================================================
# GAM MODEL TRAINING
# ============================================================================

# Train GAM for POINTS prediction
train_points_gam <- function(chrono_data, race_type, gender) {
  log_info(paste("Training", gender, race_type, "POINTS GAM"))

  hill_cat <- get_hill_category(race_type)

  # Filter for similar hill types
  if (hill_cat == "Flying") {
    filtered_data <- chrono_data %>% filter(RaceType %in% c("Flying"))
  } else if (hill_cat == "Large") {
    filtered_data <- chrono_data %>% filter(RaceType %in% c("Large", "Flying"))
  } else if (hill_cat == "Normal") {
    filtered_data <- chrono_data %>% filter(RaceType %in% c("Normal", "Large"))
  } else {
    filtered_data <- chrono_data
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

    return(list(model = points_model, residual_sd = residual_sd, features = positive_vars, race_type = race_type))
  }, error = function(e) {
    log_error(paste("Error training GAM:", e$message))
    return(NULL)
  })
}

get_weighted_prev_points <- function(chrono_data, athlete_id, race_type, reference_date) {
  hill_cat <- get_hill_category(race_type)

  # Filter for similar hill sizes
  if (hill_cat == "Flying") {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% c("Flying"))
  } else if (hill_cat == "Large") {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% c("Large", "Flying"))
  } else if (hill_cat == "Normal") {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% c("Normal", "Large"))
  } else {
    races <- chrono_data %>%
      filter(ID == athlete_id)
  }

  if (nrow(races) == 0) {
    races <- chrono_data %>% filter(ID == athlete_id)
  }

  if (nrow(races) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
  }

  if (!"Points" %in% names(races)) {
    races$Points <- sapply(races$Place, function(p) get_points(p, race_type))
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
    weighted_sd <- SD_MAX / 2
  }

  return(list(
    mean = weighted_mean,
    sd = weighted_sd,
    n = nrow(races)
  ))
}

# Build athlete distribution combining history + GAM fill
build_athlete_distribution <- function(athlete_id, race_type, chrono_data,
                                       gam_prediction, gam_residual_sd,
                                       n_history = N_HISTORY_REQUIRED,
                                       gam_fill_weight_factor = GAM_FILL_WEIGHT_FACTOR,
                                       decay_lambda = DECAY_LAMBDA,
                                       reference_date = NULL) {
  if (is.null(reference_date)) reference_date <- Sys.Date()

  hill_cat <- get_hill_category(race_type)

  # Get athlete's recent race history for similar hill types
  if (hill_cat == "Flying") {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% c("Flying")) %>%
      arrange(desc(Date)) %>% head(n_history)
  } else if (hill_cat == "Large") {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% c("Large", "Flying")) %>%
      arrange(desc(Date)) %>% head(n_history)
  } else if (hill_cat == "Normal") {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% c("Normal", "Large")) %>%
      arrange(desc(Date)) %>% head(n_history)
  } else {
    athlete_history <- chrono_data %>%
      filter(ID == athlete_id) %>%
      arrange(desc(Date)) %>% head(n_history)
  }

  n_actual_races <- nrow(athlete_history)
  all_points <- c()
  all_weights <- c()

  # Add actual race history
  if (n_actual_races > 0) {
    history_points <- athlete_history$Points
    days_ago <- as.numeric(reference_date - athlete_history$Date)
    history_weights <- exp(-decay_lambda * days_ago)
    all_points <- c(all_points, history_points)
    all_weights <- c(all_weights, history_weights)
  }

  # Fill missing history slots with GAM predictions
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

  # Calculate weighted mean and SD
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

get_base_race_probability <- function(chrono_data, participant, race_type) {
  five_years_ago <- Sys.Date() - (5 * 365)
  hill_cat <- get_hill_category(race_type)

  athlete_first_race <- chrono_data %>%
    filter(Skier == participant) %>%
    summarise(first_date = min(Date, na.rm = TRUE)) %>%
    pull(first_date)

  if (is.na(athlete_first_race) || length(athlete_first_race) == 0) {
    cutoff_date <- five_years_ago
  } else {
    cutoff_date <- max(five_years_ago, athlete_first_race)
  }

  # Filter for similar hill categories
  if (hill_cat == "Flying") {
    all_type_races <- chrono_data %>%
      filter(Date >= cutoff_date, RaceType %in% c("Flying")) %>%
      distinct(Date, City) %>%
      arrange(Date)

    participant_type_races <- chrono_data %>%
      filter(Date >= cutoff_date, Skier == participant, RaceType %in% c("Flying")) %>%
      distinct(Date, City)
  } else if (hill_cat == "Large") {
    all_type_races <- chrono_data %>%
      filter(Date >= cutoff_date, RaceType %in% c("Large", "Flying")) %>%
      distinct(Date, City) %>%
      arrange(Date)

    participant_type_races <- chrono_data %>%
      filter(Date >= cutoff_date, Skier == participant, RaceType %in% c("Large", "Flying")) %>%
      distinct(Date, City)
  } else if (hill_cat == "Normal") {
    all_type_races <- chrono_data %>%
      filter(Date >= cutoff_date, RaceType %in% c("Normal", "Large")) %>%
      distinct(Date, City) %>%
      arrange(Date)

    participant_type_races <- chrono_data %>%
      filter(Date >= cutoff_date, Skier == participant, RaceType %in% c("Normal", "Large")) %>%
      distinct(Date, City)
  } else {
    all_type_races <- chrono_data %>%
      filter(Date >= cutoff_date) %>%
      distinct(Date, City) %>%
      arrange(Date)

    participant_type_races <- chrono_data %>%
      filter(Date >= cutoff_date, Skier == participant) %>%
      distinct(Date, City)
  }

  n_races <- nrow(all_type_races)
  if (n_races == 0) return(0)

  participation <- sapply(1:n_races, function(i) {
    race_date <- all_type_races$Date[i]
    race_city <- all_type_races$City[i]
    as.numeric(any(participant_type_races$Date == race_date &
                   participant_type_races$City == race_city))
  })

  race_weights <- exp(-0.1 * ((n_races - 1):0))

  weighted_participation <- sum(participation * race_weights)
  total_weight <- sum(race_weights)
  prob <- weighted_participation / total_weight

  return(prob)
}

apply_quota_constraint <- function(startlist, race_prob_col, target_per_nation = 4) {
  for (nation in unique(startlist$Nation)) {
    nation_mask <- startlist$Nation == nation
    nation_probs <- startlist[nation_mask, race_prob_col]
    current_sum <- sum(nation_probs, na.rm = TRUE)

    if (current_sum > 0) {
      scaling_factor <- target_per_nation / current_sum
      scaled_probs <- nation_probs * scaling_factor
      scaled_probs <- pmin(scaled_probs, 1.0)
      startlist[nation_mask, race_prob_col] <- scaled_probs
    }
  }
  return(startlist)
}

# ============================================================================
# TEAM HELPER FUNCTIONS
# ============================================================================

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

build_team_distribution <- function(nation, team_chrono, startlist_row,
                                    reference_date = NULL) {
  if (is.null(reference_date)) {
    reference_date <- Sys.Date()
  }

  hist_stats <- get_team_weighted_prev_points(team_chrono, nation, reference_date)

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

# Get unique race types from championships races
all_race_types <- unique(c(men_races$race_type, ladies_races$race_type))
all_race_types <- all_race_types[!is.na(all_race_types) & !grepl("Team", all_race_types)]
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
# CHAMPIONSHIPS SIMULATION - INDIVIDUAL
# ============================================================================

log_info("=== CHAMPIONSHIPS SIMULATION - INDIVIDUAL ===")
if (ENHANCED_LOGGING) phase_start("Individual Championships Simulation")

process_gender_championships <- function(gender, races, gam_models) {
  log_info(paste("Processing", gender, "Championships with", nrow(races), "individual races"))

  startlist <- if (gender == "men") men_startlist else ladies_startlist
  chrono_data <- if (gender == "men") men_chrono else ladies_chrono

  if (nrow(startlist) == 0) {
    log_warn(paste("No", gender, "startlist available"))
    return(NULL)
  }

  # Calculate race probabilities
  for (i in 1:nrow(races)) {
    race_prob_col <- paste0("Race", i, "_Prob")
    race_type <- races$race_type[i]

    startlist[[race_prob_col]] <- sapply(startlist$Skier, function(skier) {
      get_base_race_probability(chrono_data, skier, race_type)
    })

    startlist <- apply_quota_constraint(startlist, race_prob_col, target_per_nation = 4)
  }

  # Process each race
  position_predictions <- list()
  files_saved <- character(0)

  for (i in 1:nrow(races)) {
    race_info <- races[i, ]
    race_type <- race_info$race_type

    log_info(sprintf("Processing %s race %d: %s", gender, i, race_type))

    race_prob_col <- paste0("Race", i, "_Prob")

    race_startlist <- startlist %>%
      filter(!is.na(ID), !is.na(Skier), get(race_prob_col) > 0)

    if (nrow(race_startlist) == 0) {
      log_warn(paste("No athletes in startlist for", gender, race_type))
      next
    }

    log_info(paste("Athletes in startlist:", nrow(race_startlist)))
    if (ENHANCED_LOGGING) {
      city <- if ("country" %in% names(race_info)) race_info$country else "Unknown"
      log_race_start(i, nrow(races), gender, race_type, city, nrow(race_startlist))
    }

    # Get GAM model for this race type
    model_info <- gam_models[[race_type]]
    if (is.null(model_info)) {
      # Try to find a similar hill type model
      hill_cat <- get_hill_category(race_type)
      if (hill_cat == "Flying") {
        model_info <- gam_models[["Flying"]] %||% gam_models[["Large"]]
      } else if (hill_cat == "Large") {
        model_info <- gam_models[["Large"]] %||% gam_models[["Flying"]]
      } else if (hill_cat == "Normal") {
        model_info <- gam_models[["Normal"]] %||% gam_models[["Large"]]
      }
    }

    athlete_distributions <- list()

    for (j in 1:nrow(race_startlist)) {
      athlete_id <- race_startlist$ID[j]
      if (ENHANCED_LOGGING) {
        log_progress(j, nrow(race_startlist), "Building distributions", every = 20)
      }

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

    if (ENHANCED_LOGGING) {
      log_distribution_stats(athlete_distributions, sprintf("%s %s", gender, race_type))
    }

    log_info(paste("Running", N_SIMULATIONS, "Monte Carlo simulations"))
    sim_results <- simulate_race_positions(athlete_distributions)

    sim_results <- sim_results %>%
      left_join(
        race_startlist %>% select(ID, Skier, Nation, all_of(race_prob_col)) %>% distinct(),
        by = c("athlete_id" = "ID")
      ) %>%
      rename(ID = athlete_id, Start = !!race_prob_col)

    sim_results$Sex <- ifelse(gender == "men", "M", "L")
    sim_results$Race <- race_info$original_race_num

    if (ENHANCED_LOGGING) {
      log_race_results(sim_results %>% arrange(desc(prob_top_1)), race_type)
      validate_probabilities(sim_results)
    }

    position_predictions[[as.character(race_info$original_race_num)]] <- sim_results
  }

  all_position_predictions <- bind_rows(position_predictions)

  # Create output directory
  champs_year <- format(Sys.Date(), "%Y")
  output_dir <- paste0("~/blog/daehl-e/content/post/skijump/drafts/champs-predictions/", champs_year)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create race sheets
  race_sheets <- list()
  unique_races <- sort(unique(all_position_predictions$Race))

  race_order <- 0
  for (race_num in unique_races) {
    race_order <- race_order + 1
    race_data <- all_position_predictions[all_position_predictions$Race == race_num, ]

    race_data <- race_data %>%
      mutate(
        Start = round(Start * 100, 1),
        Win = round(prob_top_1 * 100, 1),
        Podium = round(prob_top_3 * 100, 1),
        `Top-5` = round(prob_top_5 * 100, 1),
        `Top-10` = round(prob_top_10 * 100, 1),
        `Top-30` = round(prob_top_30 * 100, 1)
      ) %>%
      dplyr::select(Skier, ID, Sex, Nation, Start, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
      arrange(desc(Win))

    race_info <- champs_races_with_race_num %>%
      filter(Sex == ifelse(gender == "men", "M", "L"), OriginalRaceNum == race_num)

    race_type <- if (nrow(race_info) > 0) race_info$RaceType[1] else paste("Race", race_num)
    race_date <- if (nrow(race_info) > 0) format(race_info$Race_Date[1], "%b %d") else ""

    sheet_name <- paste0(race_order, ". ", race_type, " - ", race_date)

    log_info(paste("Creating sheet:", sheet_name, "with", nrow(race_data), "athletes"))

    race_sheets[[sheet_name]] <- race_data
  }

  # Save position probabilities
  if (length(race_sheets) > 0) {
    race_file <- file.path(output_dir, paste0(gender, "_position_probabilities.xlsx"))
    write.xlsx(race_sheets, race_file)
    log_info(paste("Saved", gender, "race probabilities to", race_file))
    files_saved <- c(files_saved, race_file)
  }

  # Create athlete summary
  athlete_summary <- all_position_predictions %>%
    group_by(Skier, Nation) %>%
    summarise(
      `Avg Win` = round(mean(prob_top_1, na.rm = TRUE) * 100, 1),
      `Avg Podium` = round(mean(prob_top_3, na.rm = TRUE) * 100, 1),
      `Avg Top-5` = round(mean(prob_top_5, na.rm = TRUE) * 100, 1),
      `Avg Top-10` = round(mean(prob_top_10, na.rm = TRUE) * 100, 1),
      Races = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(`Avg Win`))

  summary_file <- file.path(output_dir, paste0(gender, ".xlsx"))
  write.xlsx(athlete_summary, summary_file)
  log_info(paste("Saved", gender, "summary to", summary_file))
  files_saved <- c(files_saved, summary_file)

  return(list(
    summary = athlete_summary,
    race_results = all_position_predictions,
    race_sheets = race_sheets,
    files_saved = files_saved
  ))
}

# Process both genders
men_results <- NULL
if (nrow(men_races) > 0) {
  men_results <- process_gender_championships("men", men_races, men_gam_models)
}

ladies_results <- NULL
if (nrow(ladies_races) > 0) {
  ladies_results <- process_gender_championships("ladies", ladies_races, ladies_gam_models)
}
if (ENHANCED_LOGGING) {
  phase_end("Individual Championships Simulation")
  phase_start("Team Championships Simulation")
}

# ============================================================================
# CHAMPIONSHIPS SIMULATION - TEAM EVENTS
# ============================================================================

log_info("=== CHAMPIONSHIPS SIMULATION - TEAM EVENTS ===")

team_results <- list()
champs_year <- format(Sys.Date(), "%Y")
output_dir <- paste0("~/blog/daehl-e/content/post/skijump/drafts/champs-predictions/", champs_year)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Process Men's Team
if (nrow(men_team_races) > 0 && nrow(men_team_startlist) > 0) {
  log_info("Processing Men's Team Championships")

  team_distributions <- list()

  for (i in 1:nrow(men_team_startlist)) {
    nation <- men_team_startlist$Nation[i]
    if (ENHANCED_LOGGING) {
      log_progress(i, nrow(men_team_startlist), "Building team distributions", every = 5)
    }

    dist <- build_team_distribution(
      nation = nation,
      team_chrono = men_team_chrono,
      startlist_row = men_team_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  if (ENHANCED_LOGGING) {
    log_distribution_stats(team_distributions, "Men Team")
  }

  team_sim_results <- simulate_team_positions(team_distributions)

  team_sim_results <- team_sim_results %>%
    mutate(
      Sex = "M",
      RaceType = "Team",
      Win = round(prob_top_1 * 100, 1),
      Podium = round(prob_top_3 * 100, 1),
      `Top-5` = round(prob_top_5 * 100, 1),
      `Top-10` = round(prob_top_10 * 100, 1),
      `Top-30` = round(prob_top_30 * 100, 1)
    ) %>%
    select(Nation, Sex, RaceType, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
    arrange(desc(Win))

  team_results[["men_team"]] <- team_sim_results

  men_team_file <- file.path(output_dir, "men_team_position_probabilities.xlsx")
  write.xlsx(list("Men Team" = team_sim_results), men_team_file)
  log_info(paste("Saved men's team champs predictions to", men_team_file))
  if (ENHANCED_LOGGING) {
    log_validation("Men Team probability sum", abs(sum(team_sim_results$Win, na.rm = TRUE) - 100) < 5,
                   sprintf("%.1f", sum(team_sim_results$Win, na.rm = TRUE)))
  }
}

# Process Ladies' Team
if (nrow(ladies_team_races) > 0 && nrow(ladies_team_startlist) > 0) {
  log_info("Processing Ladies' Team Championships")

  team_distributions <- list()

  for (i in 1:nrow(ladies_team_startlist)) {
    nation <- ladies_team_startlist$Nation[i]
    if (ENHANCED_LOGGING) {
      log_progress(i, nrow(ladies_team_startlist), "Building team distributions", every = 5)
    }

    dist <- build_team_distribution(
      nation = nation,
      team_chrono = ladies_team_chrono,
      startlist_row = ladies_team_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  if (ENHANCED_LOGGING) {
    log_distribution_stats(team_distributions, "Ladies Team")
  }

  team_sim_results <- simulate_team_positions(team_distributions)

  team_sim_results <- team_sim_results %>%
    mutate(
      Sex = "L",
      RaceType = "Team",
      Win = round(prob_top_1 * 100, 1),
      Podium = round(prob_top_3 * 100, 1),
      `Top-5` = round(prob_top_5 * 100, 1),
      `Top-10` = round(prob_top_10 * 100, 1),
      `Top-30` = round(prob_top_30 * 100, 1)
    ) %>%
    select(Nation, Sex, RaceType, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
    arrange(desc(Win))

  team_results[["ladies_team"]] <- team_sim_results

  ladies_team_file <- file.path(output_dir, "ladies_team_position_probabilities.xlsx")
  write.xlsx(list("Ladies Team" = team_sim_results), ladies_team_file)
  log_info(paste("Saved ladies' team champs predictions to", ladies_team_file))
  if (ENHANCED_LOGGING) {
    log_validation("Ladies Team probability sum", abs(sum(team_sim_results$Win, na.rm = TRUE) - 100) < 5,
                   sprintf("%.1f", sum(team_sim_results$Win, na.rm = TRUE)))
  }
}

# Process Mixed Team
if (nrow(mixed_team_races) > 0 && nrow(mixed_team_startlist) > 0) {
  log_info("Processing Mixed Team Championships")

  team_distributions <- list()

  for (i in 1:nrow(mixed_team_startlist)) {
    nation <- mixed_team_startlist$Nation[i]
    if (ENHANCED_LOGGING) {
      log_progress(i, nrow(mixed_team_startlist), "Building team distributions", every = 5)
    }

    dist <- build_team_distribution(
      nation = nation,
      team_chrono = mixed_team_chrono,
      startlist_row = mixed_team_startlist[i, ],
      reference_date = current_date
    )

    team_distributions[[nation]] <- dist
  }

  if (ENHANCED_LOGGING) {
    log_distribution_stats(team_distributions, "Mixed Team")
  }

  team_sim_results <- simulate_team_positions(team_distributions)

  team_sim_results <- team_sim_results %>%
    mutate(
      Sex = "Mixed",
      RaceType = "Mixed Team",
      Win = round(prob_top_1 * 100, 1),
      Podium = round(prob_top_3 * 100, 1),
      `Top-5` = round(prob_top_5 * 100, 1),
      `Top-10` = round(prob_top_10 * 100, 1),
      `Top-30` = round(prob_top_30 * 100, 1)
    ) %>%
    select(Nation, Sex, RaceType, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
    arrange(desc(Win))

  team_results[["mixed_team"]] <- team_sim_results

  mixed_team_file <- file.path(output_dir, "mixed_team_position_probabilities.xlsx")
  write.xlsx(list("Mixed Team" = team_sim_results), mixed_team_file)
  log_info(paste("Saved mixed team champs predictions to", mixed_team_file))
  if (ENHANCED_LOGGING) {
    log_validation("Mixed Team probability sum", abs(sum(team_sim_results$Win, na.rm = TRUE) - 100) < 5,
                   sprintf("%.1f", sum(team_sim_results$Win, na.rm = TRUE)))
  }
}

log_info(paste("Team simulation complete.", length(team_results), "team races processed"))
if (ENHANCED_LOGGING) {
  phase_end("Team Championships Simulation")
  phase_start("Nations Output")
}

# ============================================================================
# CREATE NATIONS EXCEL FILE
# ============================================================================

log_info("=== Creating Nations Excel File ===")

if (!is.null(men_results) || !is.null(ladies_results)) {

  men_individual_results <- data.frame()
  if (!is.null(men_results) && !is.null(men_results$race_sheets)) {
    for (race_name in names(men_results$race_sheets)) {
      race_data <- men_results$race_sheets[[race_name]]
      race_type_only <- sub("^\\d+\\. ", "", race_name)
      race_type_only <- sub(" - .*$", "", race_type_only)
      race_data$Race <- race_type_only
      race_data$Gender <- "Men"
      men_individual_results <- bind_rows(men_individual_results, race_data)
    }
  }

  ladies_individual_results <- data.frame()
  if (!is.null(ladies_results) && !is.null(ladies_results$race_sheets)) {
    for (race_name in names(ladies_results$race_sheets)) {
      race_data <- ladies_results$race_sheets[[race_name]]
      race_type_only <- sub("^\\d+\\. ", "", race_name)
      race_type_only <- sub(" - .*$", "", race_type_only)
      race_data$Race <- race_type_only
      race_data$Gender <- "Ladies"
      ladies_individual_results <- bind_rows(ladies_individual_results, race_data)
    }
  }

  log_info(paste("Combined", nrow(men_individual_results), "men's rows"))
  log_info(paste("Combined", nrow(ladies_individual_results), "ladies' rows"))

  men_nation_counts <- men_individual_results %>%
    filter(Start > 0) %>%
    group_by(Nation) %>%
    summarise(n_athletes = n_distinct(Skier), .groups = "drop")

  ladies_nation_counts <- ladies_individual_results %>%
    filter(Start > 0) %>%
    group_by(Nation) %>%
    summarise(n_athletes = n_distinct(Skier), .groups = "drop")

  men_main_nations <- men_nation_counts %>% filter(n_athletes >= 4) %>% pull(Nation) %>% sort()
  ladies_main_nations <- ladies_nation_counts %>% filter(n_athletes >= 4) %>% pull(Nation) %>% sort()

  men_other_nations <- men_nation_counts %>% filter(n_athletes < 4) %>% pull(Nation)
  ladies_other_nations <- ladies_nation_counts %>% filter(n_athletes < 4) %>% pull(Nation)

  select_and_rename_cols <- function(df, include_nation = FALSE) {
    if (include_nation) {
      df %>%
        select(Athlete = Skier, ID, Race, Nation, Start, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
        arrange(Race, Nation, desc(Start))
    } else {
      df %>%
        select(Athlete = Skier, ID, Race, Start, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
        arrange(Race, desc(Start))
    }
  }

  nations_wb <- list()

  for (nation in men_main_nations) {
    nation_data <- men_individual_results %>%
      filter(Nation == nation, Start > 0)

    if (nrow(nation_data) > 0) {
      sheet_name <- paste(nation, "Men")
      nations_wb[[sheet_name]] <- select_and_rename_cols(nation_data, include_nation = FALSE)
    }
  }

  men_other_data <- men_individual_results %>%
    filter(Nation %in% men_other_nations, Start > 0)

  if (nrow(men_other_data) > 0) {
    nations_wb[["Other Men"]] <- select_and_rename_cols(men_other_data, include_nation = TRUE)
  }

  for (nation in ladies_main_nations) {
    nation_data <- ladies_individual_results %>%
      filter(Nation == nation, Start > 0)

    if (nrow(nation_data) > 0) {
      sheet_name <- paste(nation, "Ladies")
      nations_wb[[sheet_name]] <- select_and_rename_cols(nation_data, include_nation = FALSE)
    }
  }

  ladies_other_data <- ladies_individual_results %>%
    filter(Nation %in% ladies_other_nations, Start > 0)

  if (nrow(ladies_other_data) > 0) {
    nations_wb[["Other Ladies"]] <- select_and_rename_cols(ladies_other_data, include_nation = TRUE)
  }

  all_individual_results <- bind_rows(men_individual_results, ladies_individual_results)

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
      Athletes = n_distinct(Skier),
      .groups = "drop"
    ) %>%
    rename(Nation = Nation_Group) %>%
    mutate(sort_order = ifelse(Nation == "Other", 2, 1)) %>%
    arrange(Gender, sort_order, Nation) %>%
    select(-sort_order)

  nations_wb[["Summary"]] <- summary_data

  if (length(nations_wb) > 0) {
    nations_file <- file.path(output_dir, "nations_individual.xlsx")
    write.xlsx(nations_wb, nations_file)
    log_info(paste("Saved nations individual results to", nations_file))
  }
}
if (ENHANCED_LOGGING) {
  phase_end("Nations Output")
}

# ============================================================================
# SUMMARY OUTPUT
# ============================================================================

cat("\n=== SKI JUMPING CHAMPIONSHIPS SIMULATION COMPLETE ===\n")
cat(paste("Year:", format(Sys.Date(), "%Y"), "\n"))

if (!is.null(men_results)) {
  cat(paste("Men's individual races processed:", length(men_results$race_sheets), "\n"))
}

if (!is.null(ladies_results)) {
  cat(paste("Ladies' individual races processed:", length(ladies_results$race_sheets), "\n"))
}

cat(paste("Team races processed:", length(team_results), "\n"))

cat(paste("\nOutput directory:", output_dir, "\n"))

files_saved <- c(
  if (!is.null(men_results) && "files_saved" %in% names(men_results)) men_results$files_saved else character(0),
  if (!is.null(ladies_results) && "files_saved" %in% names(ladies_results)) ladies_results$files_saved else character(0),
  if (exists("men_team_file")) men_team_file else character(0),
  if (exists("ladies_team_file")) ladies_team_file else character(0),
  if (exists("mixed_team_file")) mixed_team_file else character(0),
  if (exists("nations_file")) nations_file else character(0)
)

if (ENHANCED_LOGGING) {
  log_script_complete(
    races_processed = sum(
      if (!is.null(men_results)) length(men_results$race_sheets) else 0,
      if (!is.null(ladies_results)) length(ladies_results$race_sheets) else 0,
      length(team_results)
    ),
    files_saved = files_saved
  )
}

log_info("=== SKI JUMPING CHAMPS-PREDICTIONS-SIMULATION.R COMPLETE ===")
