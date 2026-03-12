# Weekly Fantasy Predictions: Monte Carlo Simulation Approach
#
# This script predicts fantasy points for the upcoming weekend's races using
# Monte Carlo simulation instead of GAM-based predictions.
#
# Key differences from weekly-picks2.R:
# - Uses Monte Carlo simulation (N_SIMULATIONS iterations)
# - Position probabilities come naturally from simulation counts
# - No normalization needed - simulation gives correct probabilities
# - Condition-specific adjustments (altitude/period/MS) integrated into distributions
#
# Output: Optimized fantasy team using knapsack/MIP optimization (budget + gender constraints)

library(dplyr)
library(tidyr)
library(openxlsx)
library(arrow)
library(mgcv)
library(leaps)
library(logger)
library(purrr)
library(lubridate)
library(ompr)          # For optimization model
library(ompr.roi)      # For optimization solver interface
library(ROI.plugin.glpk) # For GLPK solver

# ============================================================================
# CONFIGURATION
# ============================================================================

# ===== TEST MODE (loaded from .env) =====
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

# ===== LOAD SPORT-SPECIFIC PARAMETERS =====
# Source optimized parameters from sport_params.R
# These values are calibrated via param-optimizer.R using historical backtesting
sport_params_path <- "~/blog/daehl-e/content/post/shared/sport_params.R"
if (file.exists(path.expand(sport_params_path))) {
  source(sport_params_path)
  DEFAULT_PARAMS <- get_sport_params("cross-country")
  cat("Loaded optimized parameters from sport_params.R\n")
} else {
  # Fallback to hardcoded defaults if sport_params.R not available
  DEFAULT_PARAMS <- list(
    decay_lambda = 0.002,
    sd_scale_factor = 0.77,
    sd_min = 4,
    sd_max = 16,
    n_history_required = 10,
    gam_fill_weight_factor = 0.25
  )
  # Define fallback get_sport_params function
  get_sport_params <- function(sport, race_type = NULL) {
    return(DEFAULT_PARAMS)
  }
  cat("Using fallback default parameters (sport_params.R not found)\n")
}

# Simulation parameters (defaults, will be overridden per-race with optimized params)
N_HISTORY_REQUIRED <- DEFAULT_PARAMS$n_history_required
N_GAM_SAMPLES <- 0            # Number of GAM samples (0 = history only)
GAM_FILL_WEIGHT_FACTOR <- DEFAULT_PARAMS$gam_fill_weight_factor
N_SIMULATIONS <- 10000        # Number of Monte Carlo simulations per race
DECAY_LAMBDA <- DEFAULT_PARAMS$decay_lambda

# Variance control parameters (defaults, will be overridden per-race with optimized params)
SD_SCALE_FACTOR <- DEFAULT_PARAMS$sd_scale_factor
SD_MIN <- DEFAULT_PARAMS$sd_min
SD_MAX <- DEFAULT_PARAMS$sd_max

# Position thresholds to track
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)

# Points systems
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,
               40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,
               10,9,8,7,6,5,4,3,2,1)
stage_points <- c(50,47,44,41,38,35,32,30,28,26,24,22,20,18,16,15,14,13,12,11,
                  10,9,8,7,6,5,4,3,2,1)

# ============================================================================
# LOGGING SETUP
# ============================================================================

log_dir <- "~/ski/elo/python/ski/polars/excel365/weekly-picks-simulation"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "weekly_picks_simulation.log")))
log_info("Starting Weekly Fantasy Predictions (SIMULATION approach)")
log_info(paste("Config: N_HISTORY =", N_HISTORY_REQUIRED,
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

    if (length(negative_vars) == 0) {
      break
    }

    log_info(paste("  Removing features with negative coefficients:", paste(negative_vars, collapse = ", ")))
    current_vars <- setdiff(current_vars, negative_vars)
  }

  return(current_vars)
}

# Add Period column to chrono data
add_period_column <- function(chrono_data) {
  chrono_data %>%
    group_by(Season) %>%
    mutate(
      Num_Races = max(Race, na.rm = TRUE),
      Period = case_when(
        Num_Races <= 5 ~ 1,
        Num_Races <= 10 ~ 2,
        Num_Races <= 15 ~ 3,
        Num_Races <= 20 ~ 4,
        Num_Races <= 25 ~ 5,
        TRUE ~ ceiling((Race / (Num_Races / 5)))
      )
    ) %>%
    ungroup() %>%
    select(-Num_Races)
}

# Add AltitudeCategory column
add_altitude_category <- function(chrono_data) {
  chrono_data %>%
    mutate(AltitudeCategory = ifelse(!is.na(Elevation) & Elevation >= 1300, 1, 0))
}

# ============================================================================
# RACE PROBABILITY FUNCTIONS
# ============================================================================

# Calculate race participation probability for a skier based on historical participation
get_race_probability <- function(chronos, skier, race_type, technique) {
  log_debug(paste("Calculating probability for skier:", skier))

  # Get skier's first ever race date
  skier_first_race <- chronos %>%
    filter(Skier == skier) %>%
    arrange(Date) %>%
    dplyr::slice(1) %>%
    pull(Date)

  # Use 5 years ago or skier's first race, whichever is later
  five_years_ago <- Sys.Date() - (5 * 365)
  start_date <- if(length(skier_first_race) == 0) {
    five_years_ago
  } else {
    max(five_years_ago, as.Date(skier_first_race))
  }

  # Get all matching races since start_date
  all_races <- chronos %>%
    filter(
      Event != "Offseason",
      Date >= start_date,
      if(race_type == "Sprint") {
        Distance == "Sprint"
      } else {
        Distance != "Sprint"
      },
      if(!is.na(technique) && technique != "") {
        Technique == technique
      } else {
        TRUE
      }
    ) %>%
    distinct(Date, City)

  # Get this skier's participations
  skier_races <- chronos %>%
    filter(
      Event != "Offseason",
      Date >= start_date,
      Skier == skier,
      if(race_type == "Sprint") {
        Distance == "Sprint"
      } else {
        Distance != "Sprint"
      },
      if(!is.na(technique) && technique != "") {
        Technique == technique
      } else {
        TRUE
      }
    ) %>%
    distinct(Date, City)

  total_races <- nrow(all_races)
  if(total_races == 0) {
    return(0)
  }

  races_participated <- nrow(skier_races)
  prob <- min(1, races_participated / total_races)

  return(prob)
}

# Get additional skiers from non-config nations who raced this season
get_additional_nation_skiers <- function(chronos, startlist, nation) {
  current_season <- max(chronos$Season, na.rm = TRUE)

  # Get all skiers from this nation in current season
  season_skiers <- chronos %>%
    filter(Nation == nation, Season == current_season) %>%
    distinct(Skier) %>%
    pull(Skier)

  # Get current skiers in startlist
  current_skiers <- startlist %>%
    filter(Nation == nation) %>%
    pull(Skier)

  # Find missing skiers
  missing_skiers <- setdiff(season_skiers, current_skiers)
  return(missing_skiers)
}

# Normalize probabilities to respect nation quotas
normalize_to_quota <- function(startlist) {
  log_info("Normalizing probabilities to nation quotas")

  startlist %>%
    group_by(Nation) %>%
    mutate(
      across(
        starts_with("Race") & ends_with("_Prob"),
        ~{
          vec <- as.numeric(.)
          vec[is.na(vec)] <- 0

          quota <- as.numeric(first(Quota))
          if(is.na(quota)) quota <- 2

          # Keep track of fixed ones (exactly 1)
          fixed_ones <- vec == 1
          n_fixed <- sum(fixed_ones)
          result <- vec

          if(n_fixed >= quota) {
            return(ifelse(fixed_ones, 1, 0))
          }

          # For remaining values, normalize based on original probabilities
          remaining_quota <- quota - n_fixed
          adjustable <- !fixed_ones & vec > 0

          if(any(adjustable)) {
            adj_sum <- sum(vec[adjustable])
            if(adj_sum > 0) {
              scaling_factor <- remaining_quota / adj_sum
              result[adjustable] <- vec[adjustable] * scaling_factor
              result <- pmin(result, 1)
            }
          }
          result
        }
      )
    ) %>%
    ungroup()
}

# Process race probabilities for a gender
process_gender_probabilities <- function(startlist, chronos, races) {
  # Add Config_Nation if not present
  if(!"Config_Nation" %in% names(startlist)) {
    config_nations <- c("Norway", "Sweden", "Finland", "Germany", "Switzerland", "Russia", "Italy", "France")
    startlist$Config_Nation <- startlist$Nation %in% config_nations
  }

  # Add Quota if not present
  if(!"Quota" %in% names(startlist)) {
    startlist$Quota <- 4
    startlist$Quota[startlist$Nation %in% c("Norway", "Russia")] <- 8
    startlist$Quota[startlist$Nation %in% c("Sweden", "Finland", "Germany", "Switzerland", "Italy", "France")] <- 6
  }

  # Convert character columns to logical (handles "True"/"False" strings from Python)
  if("In_FIS_List" %in% names(startlist) && is.character(startlist$In_FIS_List)) {
    startlist$In_FIS_List <- tolower(startlist$In_FIS_List) == "true"
  }
  if("In_Config" %in% names(startlist) && is.character(startlist$In_Config)) {
    startlist$In_Config <- tolower(startlist$In_Config) == "true"
  }
  if("Config_Nation" %in% names(startlist) && is.character(startlist$Config_Nation)) {
    startlist$Config_Nation <- tolower(startlist$Config_Nation) == "true"
  }

  # Check if we have FIS startlist
  has_fis_startlist <- FALSE
  if("In_FIS_List" %in% names(startlist)) {
    has_fis_startlist <- any(startlist$In_FIS_List, na.rm = TRUE)
  }

  # Handle Race1_Prob based on FIS startlist existence
  if(has_fis_startlist) {
    log_info("FIS startlist exists - using for Race1_Prob")
    if(!"Race1_Prob" %in% names(startlist)) {
      startlist$Race1_Prob <- NA_real_
    }
    startlist$Race1_Prob <- ifelse(startlist$In_FIS_List, 1, 0)
  } else {
    log_info("No FIS startlist - using In_Config for Race1_Prob")
    if(!"Race1_Prob" %in% names(startlist)) {
      startlist$Race1_Prob <- NA_real_
    }
    if("In_Config" %in% names(startlist)) {
      startlist$Race1_Prob <- ifelse(startlist$In_Config, 1, startlist$Race1_Prob)
    }
  }

  # Handle Config_Nation athletes not in config
  if("Config_Nation" %in% names(startlist) && "In_Config" %in% names(startlist)) {
    in_fis_list <- if("In_FIS_List" %in% names(startlist)) startlist$In_FIS_List else FALSE
    config_non_included <- which(startlist$Config_Nation & !startlist$In_Config & !in_fis_list)

    if(length(config_non_included) > 0) {
      for(i in 1:nrow(races)) {
        race_prob_col <- paste0("Race", i, "_Prob")
        if(race_prob_col %in% names(startlist)) {
          startlist[config_non_included, race_prob_col] <- 0
        } else {
          startlist[[race_prob_col]] <- NA
          startlist[config_non_included, race_prob_col] <- 0
        }
      }
      log_info(paste("Set race probabilities to 0 for", length(config_non_included),
                     "skiers from config nations not in configuration"))
    }
  }

  # Process each race
  for(i in 1:nrow(races)) {
    race_prob_col <- paste0("Race", i, "_Prob")

    # Skip Race1_Prob if we have a FIS startlist and it's already set
    if(race_prob_col == "Race1_Prob" && has_fis_startlist && race_prob_col %in% names(startlist)) {
      log_info("Using existing Race1_Prob from FIS startlist")
      next
    }

    # Create the column if it doesn't exist
    if(!(race_prob_col %in% names(startlist))) {
      startlist[[race_prob_col]] <- NA_real_
    }

    # When no FIS startlist, set In_Config athletes to probability = 1
    if(!has_fis_startlist && "In_Config" %in% names(startlist)) {
      config_athletes <- which(startlist$In_Config == TRUE &
                               (is.na(startlist[[race_prob_col]]) | startlist[[race_prob_col]] != 1))
      if(length(config_athletes) > 0) {
        startlist[config_athletes, race_prob_col] <- 1
        log_info(paste("Set", race_prob_col, "= 1 for", length(config_athletes), "In_Config athletes"))
      }
    }

    # Process by nation
    nations <- unique(startlist$Nation)
    for(nation in nations) {
      nation_skiers <- startlist %>% filter(Nation == nation)
      is_config_nation <- nation_skiers$Config_Nation[1]
      quota <- nation_skiers$Quota[1]

      # Calculate probabilities for skiers without preset values
      for(j in 1:nrow(nation_skiers)) {
        skier <- nation_skiers$Skier[j]
        existing_prob <- startlist[startlist$Skier == skier, race_prob_col]

        # Preserve prob==1 (confirmed racing)
        if(!is.na(existing_prob) && existing_prob == 1) next

        # Preserve prob==0 for config nation not in config
        skier_in_config <- if("In_Config" %in% names(startlist)) {
          startlist[startlist$Skier == skier, "In_Config"]
        } else FALSE
        skier_config_nation <- startlist[startlist$Skier == skier, "Config_Nation"]

        if(!is.na(existing_prob) && existing_prob == 0 &&
           isTRUE(skier_config_nation) && !isTRUE(skier_in_config)) next

        # Calculate probability from history
        startlist[startlist$Skier == skier, race_prob_col] <-
          get_race_probability(chronos, skier, races$distance[i], races$technique[i])
      }

      # For non-config nations, add additional skiers from this season
      if(!is_config_nation) {
        additional_skiers <- get_additional_nation_skiers(chronos, startlist, nation)

        if(length(additional_skiers) > 0) {
          col_types <- sapply(startlist, class)

          additional_rows <- data.frame(
            Skier = additional_skiers,
            Nation = nation,
            Config_Nation = FALSE,
            Quota = quota,
            stringsAsFactors = FALSE
          )

          additional_rows$Race1_Prob <- 0
          additional_rows[[race_prob_col]] <- sapply(additional_skiers, function(skier) {
            get_race_probability(chronos, skier, races$distance[i], races$technique[i])
          })

          # Add required columns
          for(col in names(startlist)) {
            if(!(col %in% names(additional_rows))) {
              if(col_types[col] == "logical") {
                additional_rows[[col]] <- FALSE
              } else if(col_types[col] %in% c("numeric", "integer")) {
                additional_rows[[col]] <- 0
              } else {
                additional_rows[[col]] <- NA
              }
            }
          }

          startlist <- bind_rows(startlist, additional_rows[names(startlist)])
        }
      }
    }
  }

  # Normalize probabilities to respect quotas
  startlist <- normalize_to_quota(startlist)

  return(startlist)
}

# ============================================================================
# RACE TYPE DEFINITIONS
# ============================================================================

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
             "Sprint_Pelo_pct", "Classic_Pelo_pct", "Freestyle_Pelo_pct"))
  }
}

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
# DATA PREPROCESSING
# ============================================================================

calculate_weighted_prev_points <- function(chrono_data, decay_lambda = DECAY_LAMBDA) {
  log_info("Calculating weighted prev_points with exponential decay...")

  chrono_data <- chrono_data %>% arrange(ID, Date)

  n_rows <- nrow(chrono_data)
  if (n_rows == 0) {
    chrono_data$prev_points_weighted <- numeric(0)
    return(chrono_data)
  }

  points_col <- if ("points" %in% names(chrono_data)) "points" else "Points"
  prev_points_weighted <- numeric(n_rows)
  athlete_row_groups <- split(seq_len(n_rows), chrono_data$ID)

  for (row_idx in athlete_row_groups) {
    if (length(row_idx) <= 1) {
      next
    }

    athlete_points <- chrono_data[[points_col]][row_idx]
    athlete_dates <- chrono_data$Date[row_idx]
    athlete_distances <- chrono_data$Distance[row_idx]
    athlete_techniques <- chrono_data$Technique[row_idx]

    for (local_idx in 2:length(row_idx)) {
      prev_local_idx <- seq_len(local_idx - 1)
      current_distance <- athlete_distances[local_idx]
      current_technique <- athlete_techniques[local_idx]

      if (current_distance == "Sprint" && current_technique == "C") {
        matching <- athlete_distances[prev_local_idx] == "Sprint" &
          athlete_techniques[prev_local_idx] == "C"
      } else if (current_distance == "Sprint" && current_technique == "F") {
        matching <- athlete_distances[prev_local_idx] == "Sprint" &
          athlete_techniques[prev_local_idx] == "F"
      } else if (current_distance != "Sprint" && current_technique == "C") {
        matching <- athlete_distances[prev_local_idx] != "Sprint" &
          athlete_techniques[prev_local_idx] == "C"
      } else if (current_distance != "Sprint" && current_technique == "F") {
        matching <- athlete_distances[prev_local_idx] != "Sprint" &
          athlete_techniques[prev_local_idx] == "F"
      } else if (current_distance != "Sprint") {
        matching <- athlete_distances[prev_local_idx] != "Sprint"
      } else {
        next
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
# GAM MODEL TRAINING (for distribution building)
# ============================================================================

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
    formula <- as.formula(paste("points ~", paste(explanatory_vars, collapse = " + ")))
    feature_selection <- regsubsets(formula, data = filtered_data, nbest = 1, method = "exhaustive")
    feature_summary <- summary(feature_selection)
    best_bic_vars <- names(coef(feature_selection, which.min(feature_summary$bic)))[-1]

    log_info(paste("BIC selected features:", paste(best_bic_vars, collapse = ", ")))

    positive_vars <- filter_positive_coefficients(filtered_data, "points", best_bic_vars, family = "gaussian")

    if (length(positive_vars) == 0) {
      log_warn("No positive coefficient features - using prev_points_weighted only")
      positive_vars <- "prev_points_weighted"
    }

    smooth_terms <- paste("s(", positive_vars, ")", collapse = " + ")
    gam_formula <- as.formula(paste("points ~", smooth_terms))

    points_model <- gam(gam_formula, data = filtered_data, method = "REML")

    residual_sd <- sqrt(points_model$sig2)
    if (is.null(residual_sd) || is.na(residual_sd)) {
      residual_sd <- sqrt(points_model$deviance / points_model$df.residual)
    }
    residual_sd <- max(residual_sd, 5)

    log_info(paste("GAM trained. Residual SD:", round(residual_sd, 2)))

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

build_athlete_distribution <- function(athlete_id, race_type_key, chrono_data,
                                        gam_prediction, gam_residual_sd,
                                        n_history = N_HISTORY_REQUIRED,
                                        n_gam_samples = N_GAM_SAMPLES,
                                        gam_fill_weight_factor = GAM_FILL_WEIGHT_FACTOR,
                                        decay_lambda = DECAY_LAMBDA,
                                        reference_date = NULL,
                                        today_race = NULL,
                                        min_samples_for_adjustment = 3) {

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

  athlete_history <- chrono_data %>%
    filter(ID == athlete_id, City != "Summer", City != "Tour De Ski", eval(race_filter)) %>%
    arrange(desc(Date)) %>%
    head(n_history)

  n_actual_races <- nrow(athlete_history)

  all_points <- c()
  all_weights <- c()
  history_points <- c()
  history_weights <- c()

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

  # Calculate base distribution parameters
  weighted_mean <- weighted.mean(all_points, all_weights, na.rm = TRUE)
  weighted_var <- sum(all_weights * (all_points - weighted_mean)^2) / sum(all_weights)
  weighted_sd <- sqrt(weighted_var)
  weighted_sd <- max(weighted_sd, 5)

  # CONDITION-SPECIFIC ADJUSTMENTS
  adjustment <- 0
  altitude_adj <- 0
  period_adj <- 0
  ms_adj <- 0

  if (n_actual_races >= 2 * min_samples_for_adjustment && !is.null(today_race)) {

    history_weighted_mean <- weighted.mean(history_points, history_weights, na.rm = TRUE)

    today_altitude_cat <- if (!is.null(today_race$Elevation) && !is.na(today_race$Elevation)) {
      ifelse(today_race$Elevation >= 1300, 1, 0)
    } else 0

    today_period <- if (!is.null(today_race$Period) && !is.na(today_race$Period)) {
      today_race$Period
    } else 3

    today_ms <- if (!is.null(today_race$MS) && !is.na(today_race$MS)) {
      today_race$MS
    } else 0

    # ALTITUDE ADJUSTMENT
    if ("AltitudeCategory" %in% names(athlete_history)) {
      high_idx <- athlete_history$AltitudeCategory == 1
      low_idx <- athlete_history$AltitudeCategory == 0

      if (sum(high_idx) >= min_samples_for_adjustment && sum(low_idx) >= min_samples_for_adjustment) {
        high_points <- history_points[high_idx]
        high_weights <- history_weights[high_idx]
        low_points <- history_points[low_idx]
        low_weights <- history_weights[low_idx]

        t_result <- tryCatch({
          t.test(high_points, low_points)
        }, error = function(e) NULL)

        if (!is.null(t_result) && !is.na(t_result$p.value) && t_result$p.value < 0.05) {
          high_wmean <- weighted.mean(high_points, high_weights, na.rm = TRUE)
          low_wmean <- weighted.mean(low_points, low_weights, na.rm = TRUE)

          if (today_altitude_cat == 1) {
            altitude_adj <- high_wmean - history_weighted_mean
          } else {
            altitude_adj <- low_wmean - history_weighted_mean
          }
        }
      }
    }

    # PERIOD ADJUSTMENT
    if ("Period" %in% names(athlete_history)) {
      same_idx <- athlete_history$Period == today_period
      diff_idx <- athlete_history$Period != today_period

      if (sum(same_idx) >= min_samples_for_adjustment && sum(diff_idx) >= min_samples_for_adjustment) {
        same_points <- history_points[same_idx]
        same_weights <- history_weights[same_idx]
        diff_points <- history_points[diff_idx]

        t_result <- tryCatch({
          t.test(same_points, diff_points)
        }, error = function(e) NULL)

        if (!is.null(t_result) && !is.na(t_result$p.value) && t_result$p.value < 0.05) {
          same_wmean <- weighted.mean(same_points, same_weights, na.rm = TRUE)
          period_adj <- same_wmean - history_weighted_mean
        }
      }
    }

    # MASS START ADJUSTMENT (distance races only)
    if ("MS" %in% names(athlete_history) && !grepl("Sprint", race_type_key)) {
      ms_idx <- athlete_history$MS == 1
      ind_idx <- athlete_history$MS == 0

      if (sum(ms_idx) >= min_samples_for_adjustment && sum(ind_idx) >= min_samples_for_adjustment) {
        ms_points <- history_points[ms_idx]
        ms_weights <- history_weights[ms_idx]
        ind_points <- history_points[ind_idx]
        ind_weights <- history_weights[ind_idx]

        t_result <- tryCatch({
          t.test(ms_points, ind_points)
        }, error = function(e) NULL)

        if (!is.null(t_result) && !is.na(t_result$p.value) && t_result$p.value < 0.05) {
          ms_wmean <- weighted.mean(ms_points, ms_weights, na.rm = TRUE)
          ind_wmean <- weighted.mean(ind_points, ind_weights, na.rm = TRUE)

          if (today_ms == 1) {
            ms_adj <- ms_wmean - history_weighted_mean
          } else {
            ms_adj <- ind_wmean - history_weighted_mean
          }
        }
      }
    }

    adjustment <- altitude_adj + period_adj + ms_adj
  }

  adjusted_mean <- weighted_mean + adjustment
  adjusted_mean <- pmax(0, pmin(100, adjusted_mean))

  return(list(
    athlete_id = athlete_id,
    mean = adjusted_mean,
    sd = weighted_sd,
    n_actual_races = n_actual_races,
    n_gam_fill = n_missing_history,
    adjustment = adjustment,
    altitude_adj = altitude_adj,
    period_adj = period_adj,
    ms_adj = ms_adj
  ))
}

simulate_race_positions <- function(athlete_distributions, n_simulations = N_SIMULATIONS,
                                     position_thresholds = POSITION_THRESHOLDS,
                                     sd_scale_factor = SD_SCALE_FACTOR,
                                     sd_min = SD_MIN, sd_max = SD_MAX,
                                     max_points = 100) {

  valid_distributions <- Filter(function(dist) {
    !is.null(dist) &&
    !is.null(dist$athlete_id) && !is.na(dist$athlete_id) &&
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
  scaled_sds <- pmax(sd_min, pmin(sd_max, sds * sd_scale_factor))

  # Generate all simulations at once (n_athletes x n_simulations matrix)
  all_sims <- matrix(rnorm(n_athletes * n_simulations),
                     nrow = n_athletes, ncol = n_simulations)
  all_sims <- all_sims * scaled_sds + means
  all_sims[all_sims < 0] <- 0
  all_sims[all_sims > max_points] <- max_points

  if (is.null(dim(all_sims)) || nrow(all_sims) == 0 || ncol(all_sims) == 0) {
    log_error("Simulation matrix is invalid after bounds enforcement")
    return(data.frame())
  }

  # Rank each simulation (column) - higher points = better = rank 1
  ranks_matrix <- apply(all_sims, 2, function(x) rank(-x, ties.method = "random"))

  # Count position achievements using vectorized rowSums
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
    adjustment = sapply(valid_distributions, function(x) {
      if(!is.null(x$adjustment)) x$adjustment else 0
    }),
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
# MAIN PREDICTION FUNCTION
# ============================================================================

predict_races_simulation <- function(gender, races, startlist, chrono_data, weekend_date,
                                      points_system = wc_points) {
  log_info(paste("Processing", gender, "races using Monte Carlo simulation"))

  # Preprocess chrono data
  chrono_data <- chrono_data %>%
    mutate(
      Date = as.Date(Date),
      points = sapply(Place, function(p) get_points(p, wc_points))
    )

  chrono_data <- add_period_column(chrono_data)
  chrono_data <- add_altitude_category(chrono_data)
  chrono_data <- calculate_weighted_prev_points(chrono_data)
  chrono_data <- calculate_percentage_columns(chrono_data)

  reference_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")

  race_results <- list()

  for(i in 1:nrow(races)) {
    race <- races[i, ]
    log_info(sprintf("Processing %s Race %d: %s %s", gender, i, race$distance, race$technique))

    # Determine race type key
    race_type_key <- determine_race_type_key(race$distance, race$technique, race$ms)
    if (is.na(race_type_key)) {
      log_warn(paste("Could not determine race type for race", i))
      next
    }

    # Get race-type-specific optimized parameters
    race_params <- get_sport_params("cross-country", race_type_key)
    log_info(sprintf("Using optimized params for %s: decay=%.4f, sd_scale=%.2f, sd_min=%d, sd_max=%d",
                     race_type_key, race_params$decay_lambda, race_params$sd_scale_factor,
                     race_params$sd_min, race_params$sd_max))

    # Create today_race object for adjustments
    today_race <- list(
      Elevation = race$altitude,
      Period = race$period,
      MS = race$ms
    )

    log_info(paste("Race conditions - Elevation:", race$altitude, "m, MS:", race$ms, ", Period:", race$period))

    # Get race probability column
    race_prob_col <- paste0("Race", i, "_Prob")

    # Filter startlist for this race (athletes with probability > 0)
    race_startlist <- startlist
    if (race_prob_col %in% names(startlist)) {
      race_startlist <- startlist %>%
        filter(!is.na(!!sym(race_prob_col)) & !!sym(race_prob_col) > 0)
    }

    log_info(paste("Processing", nrow(race_startlist), "athletes for race", i))

    if (nrow(race_startlist) == 0) {
      log_warn(paste("No athletes in startlist for race", i))
      next
    }

    # Train GAM for this race type
    gam_result <- train_points_gam(chrono_data, race_type_key, gender)

    if (is.null(gam_result)) {
      log_warn(paste("Could not train GAM for", race_type_key, "- using default predictions"))
      gam_prediction <- 25
      gam_residual_sd <- 15
    } else {
      # Get GAM predictions for startlist athletes
      gam_prediction <- 25  # Default
      gam_residual_sd <- gam_result$residual_sd
    }

    # Build distributions for each athlete
    log_info(paste("Building distributions for", nrow(race_startlist), "athletes"))

    athlete_distributions <- lapply(1:nrow(race_startlist), function(j) {
      athlete <- race_startlist[j, ]

      # Get GAM prediction for this athlete if model exists
      if (!is.null(gam_result)) {
        athlete_data <- chrono_data %>%
          filter(ID == athlete$ID) %>%
          arrange(desc(Date)) %>%
          head(1)

        if (nrow(athlete_data) > 0) {
          gam_pred <- tryCatch({
            predict(gam_result$model, newdata = athlete_data, type = "response")
          }, error = function(e) 25)
        } else {
          gam_pred <- 25
        }
      } else {
        gam_pred <- 25
      }

      build_athlete_distribution(
        athlete_id = athlete$ID,
        race_type_key = race_type_key,
        chrono_data = chrono_data,
        gam_prediction = gam_pred,
        gam_residual_sd = gam_residual_sd,
        reference_date = reference_date,
        today_race = today_race,
        n_history = race_params$n_history_required,
        gam_fill_weight_factor = race_params$gam_fill_weight_factor,
        decay_lambda = race_params$decay_lambda
      )
    })

    # Run simulation with race-type-specific SD parameters
    log_info(paste("Running", N_SIMULATIONS, "Monte Carlo simulations"))
    simulation_results <- simulate_race_positions(
      athlete_distributions,
      n_simulations = N_SIMULATIONS,
      sd_scale_factor = race_params$sd_scale_factor,
      sd_min = race_params$sd_min,
      sd_max = race_params$sd_max
    )

    # Add athlete info to results
    simulation_results <- simulation_results %>%
      left_join(
        race_startlist %>% select(ID, Skier, Nation, Price),
        by = c("athlete_id" = "ID")
      ) %>%
      mutate(Race = i)

    # Add race probability
    if (race_prob_col %in% names(startlist)) {
      simulation_results <- simulation_results %>%
        left_join(
          startlist %>% select(ID, !!sym(race_prob_col)),
          by = c("athlete_id" = "ID")
        ) %>%
        rename(Race_Prob = !!sym(race_prob_col))
    } else {
      simulation_results$Race_Prob <- 1
    }

    # Calculate expected points (mean_points * race_prob)
    simulation_results <- simulation_results %>%
      mutate(Expected_Points = mean_points * Race_Prob)

    # Log top 3
    log_info(paste("Top 3 for", gender, "Race", i, ":"))
    top3 <- simulation_results %>%
      arrange(desc(mean_points)) %>%
      head(3) %>%
      select(Skier, Nation, mean_points, prob_top_1, prob_top_3)
    print(top3)

    race_results[[i]] <- simulation_results
  }

  # Combine all race results
  all_results <- bind_rows(race_results)

  # Calculate total expected points per athlete
  athlete_totals <- all_results %>%
    group_by(athlete_id, Skier, Nation, Price) %>%
    summarise(
      Total_Points = sum(Expected_Points, na.rm = TRUE),
      Races_Count = n(),
      Avg_Win_Prob = mean(prob_top_1, na.rm = TRUE),
      Avg_Podium_Prob = mean(prob_top_3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Total_Points))

  log_info(paste("Completed", gender, "predictions for", nrow(athlete_totals), "athletes"))

  return(list(
    race_results = race_results,
    all_results = all_results,
    athlete_totals = athlete_totals
  ))
}

# ============================================================================
# FANTASY TEAM OPTIMIZATION (Knapsack/MIP)
# ============================================================================

optimize_weekly_team_simulation <- function(men_results, ladies_results) {
  log_info("Running MIP optimization for fantasy team...")

  # Prepare men's data
  men_df <- if (!is.null(men_results) && nrow(men_results$athlete_totals) > 0) {
    men_results$athlete_totals %>%
      mutate(
        Sex = "M",
        Points = Total_Points,
        Price = as.numeric(Price)
      ) %>%
      select(Skier, ID = athlete_id, Sex, Nation, Price, Points)
  } else {
    data.frame()
  }

  # Prepare ladies' data
  ladies_df <- if (!is.null(ladies_results) && nrow(ladies_results$athlete_totals) > 0) {
    ladies_results$athlete_totals %>%
      mutate(
        Sex = "L",
        Points = Total_Points,
        Price = as.numeric(Price)
      ) %>%
      select(Skier, ID = athlete_id, Sex, Nation, Price, Points)
  } else {
    data.frame()
  }

  # Combine datasets
  fantasy_df <- bind_rows(men_df, ladies_df) %>%
    mutate(row_id = row_number()) %>%
    filter(!is.na(Points), !is.na(Price), Price > 0)

  if (nrow(fantasy_df) < 16) {
    log_warn(paste("Not enough athletes for optimization:", nrow(fantasy_df), "- returning top athletes"))
    return(fantasy_df %>%
             arrange(desc(Points)) %>%
             head(16) %>%
             select(Skier, ID, Sex, Nation, Price, Points))
  }

  # Get indices for men and women
  n <- nrow(fantasy_df)
  men_indices <- which(fantasy_df$Sex == "M")
  women_indices <- which(fantasy_df$Sex == "L")

  # Create optimization model
  model <- MIPModel() %>%
    # Binary decision variables for each skier
    add_variable(x[i], i = 1:n, type = "binary") %>%

    # Objective: maximize predicted points
    set_objective(sum_expr(fantasy_df$Points[i] * x[i], i = 1:n), "max") %>%

    # Budget constraint (100,000 budget)
    add_constraint(sum_expr(fantasy_df$Price[i] * x[i], i = 1:n) <= 100000) %>%

    # Team size constraint (16 skiers total)
    add_constraint(sum_expr(x[i], i = 1:n) == 16) %>%

    # Gender constraints (max 8 of each gender)
    add_constraint(sum_expr(x[i], i = men_indices) <= 8) %>%
    add_constraint(sum_expr(x[i], i = women_indices) <= 8)

  # Solve the model
  result <- tryCatch({
    solve_model(model, with_ROI(solver = "glpk"))
  }, error = function(e) {
    log_error(paste("Optimization failed:", e$message))
    return(NULL)
  })

  if (is.null(result)) {
    log_warn("Optimization failed - returning top 16 athletes by points")
    return(fantasy_df %>%
             arrange(desc(Points)) %>%
             head(16) %>%
             select(Skier, ID, Sex, Nation, Price, Points))
  }

  # Extract results
  selected <- get_solution(result, x[i]) %>%
    filter(value > 0) %>%
    pull(i)

  # Create results dataframe
  selected_team <- fantasy_df[selected, ] %>%
    arrange(Sex, desc(Points))

  # Print results summary
  log_info(sprintf("Optimized Fantasy Team - Total Points: %.2f", sum(selected_team$Points)))
  log_info(sprintf("Total Cost: $%d / $100,000", sum(selected_team$Price)))
  log_info(paste("Men:", sum(selected_team$Sex == "M"), "| Ladies:", sum(selected_team$Sex == "L")))

  return(selected_team %>%
         select(Skier, ID, Sex, Nation, Price, Points) %>%
         arrange(Sex, desc(Points)))
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

log_info("Reading weekend schedule")

weekends_file <- if(TEST_MODE) {
  "~/ski/elo/python/ski/polars/excel365/test_weekends.csv"
} else {
  "~/ski/elo/python/ski/polars/excel365/weekends.csv"
}

weekends <- read.csv(weekends_file, stringsAsFactors = FALSE) %>%
  mutate(Date = mdy(Date))

current_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")
log_info(paste("Current date:", current_date))

next_races <- weekends %>%
  filter(Date >= current_date) %>%
  arrange(Date)

next_weekend_date <- min(next_races$Date, na.rm = TRUE)
log_info(paste("Next weekend date:", next_weekend_date))

next_weekend_races <- next_races %>%
  filter(Date == next_weekend_date)

print(next_weekend_races)

# Check for individual races only
men_races <- next_weekend_races %>%
  filter(Sex == "M") %>%
  filter(!Distance %in% c("Rel", "Ts")) %>%
  dplyr::select(Distance, Technique, MS, Elevation, Period, Pursuit) %>%
  rename(distance = Distance, technique = Technique,
         ms = MS, altitude = Elevation, period = Period)

ladies_races <- next_weekend_races %>%
  filter(Sex == "L") %>%
  filter(!Distance %in% c("Rel", "Ts")) %>%
  dplyr::select(Distance, Technique, MS, Elevation, Period, Pursuit) %>%
  rename(distance = Distance, technique = Technique,
         ms = MS, altitude = Elevation, period = Period)

log_info(paste("Found", nrow(men_races), "men's races and", nrow(ladies_races), "ladies races"))

if (nrow(men_races) == 0 && nrow(ladies_races) == 0) {
  log_info("No individual races scheduled for this weekend")
  quit(save = "no")
}

# Read startlists
log_info("Reading startlists")
men_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_weekend_men.csv",
                          stringsAsFactors = FALSE)
ladies_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_weekend_ladies.csv",
                             stringsAsFactors = FALSE)

# Read chrono data
log_info("Reading chronological data")
men_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))
ladies_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv",
                          stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

# Calculate race participation probabilities
log_info("Calculating race participation probabilities")
if (nrow(men_races) > 0) {
  log_info("Processing men's race probabilities")
  men_startlist <- process_gender_probabilities(men_startlist, men_chrono, men_races)
  log_info(paste("Men's startlist now has", nrow(men_startlist), "athletes"))
}

if (nrow(ladies_races) > 0) {
  log_info("Processing ladies' race probabilities")
  ladies_startlist <- process_gender_probabilities(ladies_startlist, ladies_chrono, ladies_races)
  log_info(paste("Ladies' startlist now has", nrow(ladies_startlist), "athletes"))
}

# Run predictions
men_results <- NULL
ladies_results <- NULL

if (nrow(men_races) > 0) {
  men_results <- predict_races_simulation("men", men_races, men_startlist, men_chrono, next_weekend_date)
}

if (nrow(ladies_races) > 0) {
  ladies_results <- predict_races_simulation("ladies", ladies_races, ladies_startlist, ladies_chrono, next_weekend_date)
}

# ============================================================================
# FANTASY OUTPUT
# ============================================================================

log_info("Generating fantasy team output")

# Create output directory - fantasy picks go to race-picks directory
utc_date <- format(Sys.time(), "%Y%m%d", tz = "UTC")
output_dir <- paste0("~/blog/daehl-e/content/post/cross-country/drafts/race-picks/", utc_date)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Separate weekly-picks workbook output.
# This stays in the weekly-picks folder as a data artifact only and is not part
# of the published fantasy/race-picks post flow.
weekly_output_dir <- paste0("~/blog/daehl-e/content/post/cross-country/drafts/weekly-picks/", utc_date)
if (!dir.exists(weekly_output_dir)) {
  dir.create(weekly_output_dir, recursive = TRUE)
}

# Run knapsack optimization for fantasy team
fantasy_team <- optimize_weekly_team_simulation(men_results, ladies_results)

# Rename for output
fantasy_team_output <- fantasy_team %>%
  rename(`Predicted Points` = Points)

log_info(sprintf("Fantasy team: %d men, %d ladies",
                 sum(fantasy_team$Sex == "M"),
                 sum(fantasy_team$Sex == "L")))
log_info(sprintf("Total predicted points: %.2f", sum(fantasy_team$Points)))
log_info(sprintf("Total cost: $%d", sum(fantasy_team$Price)))

# Save fantasy team - production naming convention
fantasy_file <- file.path(output_dir, "fantasy_team.xlsx")
write.xlsx(fantasy_team_output, fantasy_file)
log_info(paste("Fantasy team saved to:", fantasy_file))

# Save combined position probabilities for fantasy (men + ladies)
combined_totals <- bind_rows(
  if (!is.null(men_results)) men_results$athlete_totals %>% mutate(Sex = "M") else NULL,
  if (!is.null(ladies_results)) ladies_results$athlete_totals %>% mutate(Sex = "L") else NULL
) %>%
  arrange(desc(Total_Points))

if (nrow(combined_totals) > 0) {
  # Disabled: do not write the intermediate fantasy position probabilities workbook.
  # fantasy_probs_file <- file.path(output_dir, "fantasy_position_probabilities.xlsx")
  # write.xlsx(combined_totals, fantasy_probs_file)
  # log_info(paste("Fantasy position probabilities saved to:", fantasy_probs_file))
}

# Save top-30 predicted weekend points workbook in weekly-picks only.
weekly_points_sheets <- list()

if (!is.null(men_results) && nrow(men_results$athlete_totals) > 0) {
  weekly_points_sheets[["Men Top 30"]] <- men_results$athlete_totals %>%
    mutate(Sex = "M") %>%
    arrange(desc(Total_Points)) %>%
    slice_head(n = 30) %>%
    transmute(
      Skier,
      Nation,
      Sex,
      Price,
      `Predicted Weekend Points` = round(Total_Points, 2),
      `Races Count` = Races_Count,
      `Avg Win Probability` = round(Avg_Win_Prob * 100, 1),
      `Avg Podium Probability` = round(Avg_Podium_Prob * 100, 1)
    )
}

if (!is.null(ladies_results) && nrow(ladies_results$athlete_totals) > 0) {
  weekly_points_sheets[["Ladies Top 30"]] <- ladies_results$athlete_totals %>%
    mutate(Sex = "L") %>%
    arrange(desc(Total_Points)) %>%
    slice_head(n = 30) %>%
    transmute(
      Skier,
      Nation,
      Sex,
      Price,
      `Predicted Weekend Points` = round(Total_Points, 2),
      `Races Count` = Races_Count,
      `Avg Win Probability` = round(Avg_Win_Prob * 100, 1),
      `Avg Podium Probability` = round(Avg_Podium_Prob * 100, 1)
    )
}

if (length(weekly_points_sheets) > 0) {
  weekly_points_file <- file.path(weekly_output_dir, "weekend_top_30_predicted_points.xlsx")
  write.xlsx(weekly_points_sheets, weekly_points_file)
  log_info(paste("Weekly points workbook saved to:", weekly_points_file))
}

# Print summary
cat("\n========================================\n")
cat("WEEKLY FANTASY PICKS (SIMULATION)\n")
cat("========================================\n")
cat(paste("Weekend:", next_weekend_date, "\n"))
cat(paste("Races: Men -", nrow(men_races), ", Ladies -", nrow(ladies_races), "\n"))
cat(paste("Simulations:", N_SIMULATIONS, "\n"))
cat(sprintf("Budget: $%d / $100,000\n", sum(fantasy_team$Price)))
cat(sprintf("Total Points: %.2f\n", sum(fantasy_team$Points)))
cat("\n--- Optimized Team (16 athletes) ---\n")
print(fantasy_team %>% select(Skier, Sex, Nation, Price, Points))
cat("\n========================================\n")
cat(paste("Weekly points workbook:", file.path(weekly_output_dir, "weekend_top_30_predicted_points.xlsx"), "\n"))

log_info("Weekly picks simulation complete")
