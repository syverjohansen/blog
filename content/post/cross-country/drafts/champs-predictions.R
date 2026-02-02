# Cross-Country Championships Predictions: Clean Implementation
library(dplyr)
library(tidyr)
library(openxlsx)
library(arrow)
library(mgcv)
library(leaps)
library(logger)
library(purrr)
library(lubridate)

# Set up logging
log_dir <- "~/ski/elo/python/ski/polars/excel365/champs-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "champs_picks_processing.log")))
log_info("Starting Cross-Country Championships predictions process")

# PART 1: INDIVIDUAL RACES - TRAIN SETUP
log_info("=== PART 1: INDIVIDUAL RACES - TRAIN SETUP ===")

# Read chronological data
log_info("Reading chronological data files")

men_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv", 
                      stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

ladies_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv", 
                         stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

log_info(paste("Loaded", nrow(men_chrono), "men's chronological records"))
log_info(paste("Loaded", nrow(ladies_chrono), "ladies' chronological records"))

# Define world cup points system
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)

# Function to assign points based on place
get_points <- function(place) {
  if (is.na(place) || place < 1 || place > length(wc_points)) {
    return(0)
  } else {
    return(wc_points[place])
  }
}

# Add points column to chronological data
log_info("Adding points column based on world cup points system")

men_chrono <- men_chrono %>%
  mutate(points = sapply(Place, get_points)) %>%
  filter(City != "Tour de Ski", Place != 0)


ladies_chrono <- ladies_chrono %>%
  mutate(points = sapply(Place, get_points)) %>%
  filter(City != "Tour de Ski", Place != 0)

log_info(paste("Added points - Men average points:", round(mean(men_chrono$points, na.rm = TRUE), 2)))
log_info(paste("Added points - Ladies average points:", round(mean(ladies_chrono$points, na.rm = TRUE), 2)))

# Calculate weighted prev_points for each discipline
log_info("Calculating weighted prev_points by discipline")

calculate_weighted_prev_points <- function(chrono_data) {
  chrono_data %>%
    arrange(ID, Date) %>%
    group_by(ID) %>%
    mutate(
      prev_points_weighted = sapply(1:n(), function(i) {
        if (i == 1) return(0)  # First race has no previous races
        
        current_distance <- Distance[i]
        current_technique <- Technique[i]
        
        # Get all races up to (but not including) current race
        prev_distances <- Distance[1:(i-1)]
        prev_techniques <- Technique[1:(i-1)]
        prev_points_values <- points[1:(i-1)]
        
        # Filter for matching race type
        if (current_distance == "Sprint" && current_technique == "C") {
          # Previous Sprint Classic races
          matching <- prev_distances == "Sprint" & prev_techniques == "C"
        } else if (current_distance == "Sprint" && current_technique == "F") {
          # Previous Sprint Freestyle races  
          matching <- prev_distances == "Sprint" & prev_techniques == "F"
        } else if (current_distance != "Sprint" && current_technique == "C") {
          # Previous Distance Classic races
          matching <- prev_distances != "Sprint" & prev_techniques == "C"
        } else if (current_distance != "Sprint" && current_technique == "F") {
          # Previous Distance Freestyle races
          matching <- prev_distances != "Sprint" & prev_techniques == "F"
        } else if (current_distance != "Sprint") {
          # Previous Distance races (any technique) - for pursuits
          matching <- prev_distances != "Sprint"
        } else {
          return(0)  # Unknown race type
        }
        
        # Get matching previous points
        matching_points <- prev_points_values[matching]
        
        if (length(matching_points) == 0) {
          return(0)  # No previous races of this type
        }
        
        # Take the most recent 5 races
        recent_points <- tail(matching_points, 5)
        
        # Calculate weighted average (weights 1, 2, 3, 4, 5 for oldest to newest)
        weights <- seq(1, length(recent_points))
        
        return(weighted.mean(recent_points, weights, na.rm = TRUE))
      })
    ) %>%
    ungroup()
}

men_chrono <- calculate_weighted_prev_points(men_chrono)
ladies_chrono <- calculate_weighted_prev_points(ladies_chrono)
#View(men_chrono %>% filter(Skier == "Johannes Høsflot Klæbo", Distance!="Sprint", Technique=="F") %>% select(Skier, Season, Race, Distance, Technique, points, prev_points_weighted))
log_info(paste("Added weighted prev_points - Men average:", round(mean(men_chrono$prev_points_weighted, na.rm = TRUE), 2)))
log_info(paste("Added weighted prev_points - Ladies average:", round(mean(ladies_chrono$prev_points_weighted, na.rm = TRUE), 2)))

# Filter to last 10 seasons
log_info("Filtering to last 10 seasons of results")

current_season <- max(men_chrono$Season, na.rm = TRUE)
season_cutoff <- current_season - 10

men_chrono <- men_chrono %>%
  filter(Season >= season_cutoff)

ladies_chrono <- ladies_chrono %>%
  filter(Season >= season_cutoff)

log_info(paste("Filtered to seasons", season_cutoff, "to", current_season))
log_info(paste("Men records after filtering:", nrow(men_chrono)))
log_info(paste("Ladies records after filtering:", nrow(ladies_chrono)))


# Calculate ELO and PELO percentage columns by race
log_info("Calculating ELO and PELO percentage columns by race")

# Cross-country ELO/PELO columns (9 systems)
elo_cols <- c("Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo", 
              "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", "Classic_Elo", "Freestyle_Elo")
pelo_cols <- c("Pelo", "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
               "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo", "Classic_Pelo", "Freestyle_Pelo")

calculate_percentage_columns <- function(chrono_data) {
  chrono_data %>%
    group_by(Season, Race) %>%
    mutate(
      # ELO percentage columns (skier's elo / max elo for that race)
      Elo_pct = Elo / max(Elo, na.rm = TRUE),
      Distance_Elo_pct = Distance_Elo / max(Distance_Elo, na.rm = TRUE),
      Distance_C_Elo_pct = Distance_C_Elo / max(Distance_C_Elo, na.rm = TRUE),
      Distance_F_Elo_pct = Distance_F_Elo / max(Distance_F_Elo, na.rm = TRUE),
      Sprint_Elo_pct = Sprint_Elo / max(Sprint_Elo, na.rm = TRUE),
      Sprint_C_Elo_pct = Sprint_C_Elo / max(Sprint_C_Elo, na.rm = TRUE),
      Sprint_F_Elo_pct = Sprint_F_Elo / max(Sprint_F_Elo, na.rm = TRUE),
      Classic_Elo_pct = Classic_Elo / max(Classic_Elo, na.rm = TRUE),
      Freestyle_Elo_pct = Freestyle_Elo / max(Freestyle_Elo, na.rm = TRUE),
      
      # PELO percentage columns (skier's pelo / max pelo for that race)
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

log_info("Added ELO and PELO percentage columns normalized by race")


# Function to replace NAs with first quartile value (from biathlon implementation)
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Apply quartile imputation to all ELO and PELO columns
log_info("Applying quartile imputation to ELO and PELO columns")

# All ELO/PELO columns to impute
imputation_cols <- c(elo_cols, pelo_cols)

# Apply imputation to men's data
for (col in imputation_cols) {
  if (col %in% names(men_chrono)) {
    before_na_count <- sum(is.na(men_chrono[[col]]))
    men_chrono[[col]] <- replace_na_with_quartile(men_chrono[[col]])
    log_info(paste("Men", col, "- Replaced", before_na_count, "NAs with quartile values"))
  }
}



# Apply imputation to ladies' data
for (col in imputation_cols) {
  if (col %in% names(ladies_chrono)) {
    before_na_count <- sum(is.na(ladies_chrono[[col]]))
    ladies_chrono[[col]] <- replace_na_with_quartile(ladies_chrono[[col]])
    log_info(paste("Ladies", col, "- Replaced", before_na_count, "NAs with quartile values"))
  }
}

log_info("Quartile imputation completed for all ELO and PELO columns")
log_info(paste("ELO pct range - Men:", round(min(men_chrono$Elo_pct, na.rm = TRUE), 3), 
               "to", round(max(men_chrono$Elo_pct, na.rm = TRUE), 3)))
log_info(paste("PELO pct range - Ladies:", round(min(ladies_chrono$Pelo_pct, na.rm = TRUE), 3), 
               "to", round(max(ladies_chrono$Pelo_pct, na.rm = TRUE), 3)))

log_info("Individual train setup complete - chrono data filtered, imputed, with points, prev_points_weighted, and pct columns")

# Helper function: Iterative constrained normalization
# This properly handles the case where some athletes hit the 100% cap
# by locking capped athletes and only normalizing the remaining probability budget
normalize_with_cap <- function(probs, target_sum, max_prob = 1.0, max_iterations = 100) {
  if (sum(probs, na.rm = TRUE) == 0) {
    # Edge case: all zeros - distribute evenly
    return(rep(target_sum / length(probs), length(probs)))
  }

  for (iter in 1:max_iterations) {
    # Identify capped vs uncapped
    capped <- probs >= max_prob

    # Lock capped values at max_prob
    probs[capped] <- max_prob

    # Calculate remaining budget for uncapped athletes
    capped_total <- sum(capped) * max_prob
    remaining_target <- target_sum - capped_total
    uncapped_sum <- sum(probs[!capped], na.rm = TRUE)

    if (remaining_target <= 0) {
      # Edge case: too many athletes at cap - cap everyone who's capped,
      # set others to 0 (target exceeded by capped athletes alone)
      probs[!capped] <- 0
      break
    }

    if (uncapped_sum <= 0) {
      # Edge case: no probability mass in uncapped - distribute remaining evenly
      n_uncapped <- sum(!capped)
      if (n_uncapped > 0) {
        probs[!capped] <- remaining_target / n_uncapped
      }
      break
    }

    # Scale only uncapped values to hit remaining target
    scaling_factor <- remaining_target / uncapped_sum
    probs[!capped] <- probs[!capped] * scaling_factor

    # Check if any newly exceed cap - if not, we've converged
    if (!any(probs[!capped] > max_prob, na.rm = TRUE)) {
      break
    }
  }

  return(probs)
}

# PART 2: INDIVIDUAL RACES - TRAIN EXECUTION
log_info("=== PART 2: INDIVIDUAL RACES - TRAIN EXECUTION ===")

# Helper function to determine race type key from distance and technique
determine_race_type_key <- function(distance, technique, ms = 0) {
  if (distance == "Sprint" && technique == "C") return("Sprint_C")
  if (distance == "Sprint" && technique == "F") return("Sprint_F") 
  if (distance != "Sprint" && technique == "C" && ms == 1) return("Distance_C_Ms")
  if (distance != "Sprint" && technique == "C" && ms == 0) return("Distance_C_Ind")
  if (distance != "Sprint" && technique == "F" && ms == 1) return("Distance_F_Ms")
  if (distance != "Sprint" && technique == "F" && ms == 0) return("Distance_F_Ind")
  if (distance != "Sprint" && ms == 1) return("Distance_Ms")  # Pursuits, mass start
  if (distance != "Sprint" && ms == 0) return("Distance_Ind")  # Pursuits, individual start
  return(NA)
}

# Define race types for model training
race_types <- list(
  "Sprint_C" = list(filter = "Distance == 'Sprint' & Technique == 'C'", name = "Sprint Classic"),
  "Sprint_F" = list(filter = "Distance == 'Sprint' & Technique == 'F'", name = "Sprint Freestyle"),
  "Distance_C_Ind" = list(filter = "Distance != 'Sprint' & Technique == 'C' & MS == 0", name = "Distance Classic Individual"),
  "Distance_C_Ms" = list(filter = "Distance != 'Sprint' & Technique == 'C' & MS == 1", name = "Distance Classic Mass Start"),
  "Distance_F_Ind" = list(filter = "Distance != 'Sprint' & Technique == 'F' & MS == 0", name = "Distance Freestyle Individual"),
  "Distance_F_Ms" = list(filter = "Distance != 'Sprint' & Technique == 'F' & MS == 1", name = "Distance Freestyle Mass Start"),
  "Distance_Ind" = list(filter = "Distance != 'Sprint' & MS == 0", name = "Distance Individual (All Techniques)"),
  "Distance_Ms" = list(filter = "Distance != 'Sprint' & MS == 1", name = "Distance Mass Start (All Techniques)")
)

# Read weekends.csv to determine which race types are actually needed
log_info("Reading championship schedule to determine required race types")
weekends <- read.csv("~/ski/elo/python/ski/polars/excel365/weekends.csv", stringsAsFactors = FALSE)
champs_races <- weekends %>% filter(Championship == 1)

# Extract unique race types that actually exist in championships
required_race_types <- champs_races %>%
  filter(!Distance %in% c("Rel", "Ts")) %>%  # Only individual races
  mutate(race_type_key = mapply(determine_race_type_key, Distance, Technique, MS)) %>%
  filter(!is.na(race_type_key)) %>%
  pull(race_type_key) %>%
  unique()

log_info(paste("Championship races require models for:", paste(required_race_types, collapse = ", ")))
all_race_types <- names(race_types)
skipped_types <- setdiff(all_race_types, required_race_types)
if (length(skipped_types) > 0) {
  log_info(paste("Skipping race types not in championships:", paste(skipped_types, collapse = ", ")))
}

# Function to get technique-dependent explanatory variables
get_explanatory_vars <- function(race_type_key) {
  base_vars <- c("prev_points_weighted", "Pelo_pct")
  
  if (grepl("Sprint_C", race_type_key)) {
    # Sprint Classic races - use only Classic and Sprint Classic variables
    return(c(base_vars, "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Classic_Pelo_pct"))
  } else if (grepl("Sprint_F", race_type_key)) {
    # Sprint Freestyle races - use only Freestyle and Sprint Freestyle variables  
    return(c(base_vars, "Sprint_Pelo_pct", "Sprint_F_Pelo_pct", "Freestyle_Pelo_pct"))
  } else if (grepl("Distance.*_C", race_type_key)) {
    # Distance Classic races (Ind or Ms) - use only Classic and Distance Classic variables
    return(c(base_vars, "Distance_Pelo_pct", "Distance_C_Pelo_pct", "Classic_Pelo_pct"))
  } else if (grepl("Distance.*_F", race_type_key)) {
    # Distance Freestyle races (Ind or Ms) - use only Freestyle and Distance Freestyle variables
    return(c(base_vars, "Distance_Pelo_pct", "Distance_F_Pelo_pct", "Freestyle_Pelo_pct"))
  } else {
    # Pursuit or mixed technique - use all variables
    return(c(base_vars, "Distance_Pelo_pct", "Distance_C_Pelo_pct", "Distance_F_Pelo_pct",
             "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Sprint_F_Pelo_pct", 
             "Classic_Pelo_pct", "Freestyle_Pelo_pct"))
  }
}

# Define position thresholds (same as biathlon)
position_thresholds <- c(1, 3, 5, 10, 30)  # Win, Podium, Top 5, Top 10, Top 30

# Function to train position threshold models for specific race type
train_race_type_model <- function(chrono_data, race_type_key, gender) {
  race_info <- race_types[[race_type_key]]
  log_info(paste("Training", gender, race_info$name, "position threshold models"))
  
  # Filter data to specific race type
  filtered_data <- chrono_data %>%
    filter(eval(parse(text = race_info$filter)))
  
  log_info(paste("Filtered to", nrow(filtered_data), "records for", race_info$name))
  
  if (nrow(filtered_data) < 50) {
    log_warn(paste("Insufficient data for", race_info$name, "- skipping"))
    return(NULL)
  }
  
  # Store models for each position threshold
  position_models <- list()
  selected_features <- list()
  
  # Train a separate model for each position threshold
  for (threshold in position_thresholds) {
    log_info(paste("Training", race_info$name, "model for top", threshold, "positions"))
    
    tryCatch({
      # Create binary outcome variable for this threshold
      threshold_data <- filtered_data %>%
        mutate(position_achieved = Place <= threshold)
      
      # Get technique-dependent explanatory variables
      explanatory_vars <- get_explanatory_vars(race_type_key)
      
      # Feature selection using regsubsets for this threshold
      formula <- as.formula(paste("position_achieved ~", paste(explanatory_vars, collapse = " + ")))
      
      # Use regsubsets to select best features with BIC
      feature_selection <- regsubsets(formula, data = threshold_data, nbest = 1, method = "exhaustive")
      feature_summary <- summary(feature_selection)
      best_bic_vars <- names(coef(feature_selection, which.min(feature_summary$bic)))
      
      # Create GAM model with selected features (binomial family for probabilities)
      smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")  # Remove intercept
      gam_formula <- as.formula(paste("position_achieved ~", smooth_terms))
      
      log_info(paste("Selected features for top", threshold, ":", paste(best_bic_vars[-1], collapse=", ")))
      
      # Fit binomial GAM model
      position_model <- gam(gam_formula, data = threshold_data, family = binomial())
      
      # Calculate Brier score for model evaluation
      predicted_probs <- predict(position_model, newdata = threshold_data, type = "response")
      brier_score <- mean((threshold_data$position_achieved - predicted_probs)^2, na.rm = TRUE)
      log_info(paste("Brier score for top", threshold, ":", round(brier_score, 4)))
      
      # Store the model and features
      position_models[[paste0("threshold_", threshold)]] <- position_model
      selected_features[[paste0("threshold_", threshold)]] <- best_bic_vars[-1]
      
    }, error = function(e) {
      log_error(paste("Error training top", threshold, "model for", race_info$name, ":", e$message))
    })
  }
  
  if (length(position_models) == 0) {
    log_warn(paste("No successful models trained for", race_info$name))
    return(NULL)
  }
  
  log_info(paste("Successfully trained", length(position_models), "position models for", race_info$name))
  
  return(list(
    models = position_models,
    features = selected_features,
    race_type = race_type_key,
    race_name = race_info$name,
    training_data_size = nrow(filtered_data),
    thresholds = position_thresholds[1:length(position_models)]
  ))
}

# Train models for men (only required types)
log_info("Training men's individual race models")
men_models <- list()

for (race_type_key in names(race_types)) {
  if (race_type_key %in% required_race_types) {
    model_result <- train_race_type_model(men_chrono, race_type_key, "men")
    if (!is.null(model_result)) {
      men_models[[race_type_key]] <- model_result
    }
  } else {
    log_info(paste("Skipping", race_type_key, "- not required for championships"))
  }
}

log_info(paste("Successfully trained", length(men_models), "men's models out of", length(required_race_types), "required"))

log_info("Training ladies' individual race models")
ladies_models <- list()

for (race_type_key in names(race_types)) {
  if (race_type_key %in% required_race_types) {
    model_result <- train_race_type_model(ladies_chrono, race_type_key, "ladies")
    if (!is.null(model_result)) {
      ladies_models[[race_type_key]] <- model_result
    }
  } else {
    log_info(paste("Skipping", race_type_key, "- not required for championships"))
  }
}

log_info(paste("Successfully trained", length(ladies_models), "ladies' models out of", length(required_race_types), "required"))

# Summary of trained models
log_info("=== MODEL TRAINING SUMMARY ===")
for (race_type in names(race_types)) {
  men_status <- if (race_type %in% names(men_models)) "✓" else "✗"
  ladies_status <- if (race_type %in% names(ladies_models)) "✓" else "✗"
  log_info(paste(race_types[[race_type]]$name, "- Men:", men_status, "Ladies:", ladies_status))
}
log_info("Individual train execution complete - all models trained")

# PART 3: INDIVIDUAL RACES - TEST SETUP
log_info("=== PART 3: INDIVIDUAL RACES - TEST SETUP ===")

# Read weekends CSV to get championship race schedule
log_info("Reading weekends data and championship startlists")

weekends <- read.csv("~/ski/elo/python/ski/polars/excel365/weekends.csv",
                     stringsAsFactors = FALSE) %>%
  mutate(
    Date = mdy(Date),
    Race_Date = mdy(Race_Date)
  )

# Filter for Championships races only (Championship == 1) and order chronologically
champs_races <- weekends %>%
  filter(Championship == 1) %>%
  arrange(Race_Date) %>%
  mutate(OriginalRaceNum = row_number())

if (nrow(champs_races) == 0) {
  log_warn("No Championships races found in weekends.csv")
} else {
  log_info(paste("Found", nrow(champs_races), "Championships races"))
}

# Read championship startlists
men_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_champs_men.csv",
                         stringsAsFactors = FALSE)

ladies_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_champs_ladies.csv",
                           stringsAsFactors = FALSE)

log_info(paste("Loaded", nrow(men_startlist), "men on championship startlist"))
log_info(paste("Loaded", nrow(ladies_startlist), "ladies on championship startlist"))

# Display championship race schedule
log_info("Championship race schedule:")
for (i in 1:nrow(champs_races)) {
  race <- champs_races[i, ]
  log_info(paste("Race", i, ":", race$Sex, race$Distance, race$Technique))
}

log_info("Individual test setup - loaded weekends and startlists")

# Create PELO percentage columns for test data (using current ELO values)
log_info("Creating PELO percentage columns for test startlists using current ELO values")

# Function to create test PELO percentage columns
create_test_pelo_pct_columns <- function(startlist_data) {
  startlist_data %>%
    mutate(
      # Calculate ELO percentages but name as PELO for model compatibility
      # Each athlete's current ELO / max ELO in the startlist
      prev_points_weighted = 0,  # Will be updated later with race-specific values
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

# Apply to both startlists
men_startlist <- create_test_pelo_pct_columns(men_startlist)
ladies_startlist <- create_test_pelo_pct_columns(ladies_startlist)
log_info("Created test PELO percentage columns")
log_info(paste("Men test PELO_pct range:",
               round(min(men_startlist$Pelo_pct, na.rm = TRUE), 3), "to",
               round(max(men_startlist$Pelo_pct, na.rm = TRUE), 3)))
log_info(paste("Ladies test PELO_pct range:",
              round(min(ladies_startlist$Pelo_pct, na.rm = TRUE), 3), "to",
              round(max(ladies_startlist$Pelo_pct, na.rm = TRUE), 3)))

log_info("Individual test setup - loaded data and created test PELO percentage columns")

# PART 4: INDIVIDUAL RACES - TEST EXECUTION
log_info("=== PART 4: INDIVIDUAL RACES - TEST EXECUTION ===")

# Function to determine race type from Distance and Technique
determine_race_type <- function(distance, technique, ms = 0) {
  if (distance == "Sprint" && technique == "C") return("Sprint_C")
  if (distance == "Sprint" && technique == "F") return("Sprint_F")
  if (distance != "Sprint" && technique == "C" && ms == 1) return("Distance_C_Ms")
  if (distance != "Sprint" && technique == "C" && ms == 0) return("Distance_C_Ind")
  if (distance != "Sprint" && technique == "F" && ms == 1) return("Distance_F_Ms")
  if (distance != "Sprint" && technique == "F" && ms == 0) return("Distance_F_Ind")
  if (distance != "Sprint" && ms == 1) return("Distance_Ms")  # Pursuits, mass start
  if (distance != "Sprint" && ms == 0) return("Distance_Ind")  # Pursuits, individual start
  return(NA)
}

# Function to get race-specific prev_points_weighted
get_race_specific_prev_points <- function(chrono_data, startlist_ids, race_type) {
  # Ensure unique startlist IDs
  unique_ids <- unique(startlist_ids)
  
  # Calculate fresh prev_points_weighted for each athlete based on their race history
  results <- data.frame(ID = unique_ids, prev_points_weighted = 0)
  
  for (i in 1:length(unique_ids)) {
    athlete_id <- unique_ids[i]
    
    # Get all historical races for this athlete
    athlete_races <- chrono_data %>%
      filter(ID == athlete_id) %>%
      arrange(Date)
    
    if (nrow(athlete_races) == 0) {
      next  # Keep prev_points_weighted = 0
    }
    
    # Filter to races matching the target race type
    if (race_type == "Sprint_C") {
      matching_races <- athlete_races %>% filter(Distance == "Sprint", Technique == "C")
    } else if (race_type == "Sprint_F") {
      matching_races <- athlete_races %>% filter(Distance == "Sprint", Technique == "F")
    } else if (race_type == "Distance_C_Ind") {
      matching_races <- athlete_races %>% filter(Distance != "Sprint", Technique == "C", MS == 0)
    } else if (race_type == "Distance_C_Ms") {
      matching_races <- athlete_races %>% filter(Distance != "Sprint", Technique == "C", MS == 1)
    } else if (race_type == "Distance_F_Ind") {
      matching_races <- athlete_races %>% filter(Distance != "Sprint", Technique == "F", MS == 0)
    } else if (race_type == "Distance_F_Ms") {
      matching_races <- athlete_races %>% filter(Distance != "Sprint", Technique == "F", MS == 1)
    } else if (race_type == "Distance_Ind") {
      matching_races <- athlete_races %>% filter(Distance != "Sprint", MS == 0)
    } else if (race_type == "Distance_Ms") {
      matching_races <- athlete_races %>% filter(Distance != "Sprint", MS == 1)
    } else {
      next  # Unknown race type, keep 0
    }
    
    if (nrow(matching_races) == 0) {
      next  # No historical races of this type, keep 0
    }
    
    # Take the most recent 5 races of this type
    recent_races <- tail(matching_races, 5)
    
    # Calculate weighted average (weights 1, 2, 3, 4, 5 for oldest to newest)
    weights <- seq(1, nrow(recent_races))
    weighted_avg <- weighted.mean(recent_races$points, weights, na.rm = TRUE)
    
    # Store the result
    results$prev_points_weighted[i] <- weighted_avg
  }
  

  return(results)
}

# Function to calculate position probabilities using trained threshold models
calculate_position_probabilities <- function(model_info, test_data) {
  position_models <- model_info$models
  
  # Initialize probability columns
  probs <- data.frame(
    win_prob = rep(0, nrow(test_data)),
    podium_prob = rep(0, nrow(test_data)),
    top5_prob = rep(0, nrow(test_data)),
    top10_prob = rep(0, nrow(test_data)),
    top30_prob = rep(0, nrow(test_data))
  )
  
  # Calculate probabilities for each threshold using appropriate model
  threshold_to_col <- list(
    "1" = "win_prob",
    "3" = "podium_prob", 
    "5" = "top5_prob",
    "10" = "top10_prob",
    "30" = "top30_prob"
  )
  
  for (threshold in position_thresholds) {
    model_name <- paste0("threshold_", threshold)
    col_name <- threshold_to_col[[as.character(threshold)]]
    
    if (model_name %in% names(position_models) && !is.null(col_name)) {
      tryCatch({
        # Get probability predictions from the binomial GAM
        threshold_probs <- predict(position_models[[model_name]], 
                                   newdata = test_data, 
                                   type = "response")
        
        # Store in appropriate column
        probs[[col_name]] <- pmax(0, pmin(1, threshold_probs))
        
      }, error = function(e) {
        log_warn(paste("Error predicting for threshold", threshold, ":", e$message))
        # Use a simple fallback based on threshold
        probs[[col_name]] <- rep(threshold / 100, nrow(test_data))
      })
    }
  }
  
  return(probs)
}

# Function to process individual races
process_individual_races <- function() {
  log_info("=== PROCESSING INDIVIDUAL RACES ===")
  
  # Get individual races (ordered chronologically via OriginalRaceNum)
  individual_races <- champs_races %>%
    filter(!Distance %in% c("Rel", "Ts"), Sex %in% c("M", "L")) %>%  # Both men's and ladies' individual races
    arrange(Race_Date) %>%
    mutate(race_type = mapply(determine_race_type, Distance, Technique, MS))
  
  log_info(paste("Found", nrow(individual_races), "individual races to process"))
  
  if (nrow(individual_races) == 0) {
    log_info("No individual races found")
    return(NULL)
  }

  # ============================================
  # STEP 3: Calculate start probabilities FIRST
  # (Moved here so Race_Prob columns exist before position prediction loop)
  # ============================================
  log_info("=== STEP 3: START PROBABILITY CALCULATIONS ===")
  log_info(paste("Processing", nrow(individual_races), "individual races for start probability calculation"))

  # Function to calculate base race participation probability using exponential decay
  # Recent races are weighted more heavily (alpha = 0.1)
  # Time window: later of 5 years ago OR athlete's first race date (fairer for newer athletes)
  get_base_race_probability_local <- function(chronos, participant, race_type) {
    five_years_ago <- Sys.Date() - (5 * 365)

    # Get athlete's first race date (any race type)
    athlete_first_race <- chronos %>%
      filter(Skier == participant) %>%
      summarise(first_date = min(Date, na.rm = TRUE)) %>%
      pull(first_date)

    # Use the later of: 5 years ago OR athlete's first race date
    # This prevents penalizing newer athletes for races before they started
    if (is.na(athlete_first_race) || length(athlete_first_race) == 0) {
      cutoff_date <- five_years_ago
    } else {
      cutoff_date <- max(five_years_ago, athlete_first_race)
    }

    # Get all races of this type in the time window, sorted by date
    if (race_type == "Sprint_C") {
      all_type_races <- chronos %>% filter(Date >= cutoff_date, Distance == "Sprint", Technique == "C") %>% distinct(Date, City) %>% arrange(Date)
      participant_type_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance == "Sprint", Technique == "C") %>% distinct(Date, City)
    } else if (race_type == "Sprint_F") {
      all_type_races <- chronos %>% filter(Date >= cutoff_date, Distance == "Sprint", Technique == "F") %>% distinct(Date, City) %>% arrange(Date)
      participant_type_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance == "Sprint", Technique == "F") %>% distinct(Date, City)
    } else if (race_type == "Distance_C_Ind") {
      all_type_races <- chronos %>% filter(Date >= cutoff_date, Distance != "Sprint", Technique == "C", MS == 0) %>% distinct(Date, City) %>% arrange(Date)
      participant_type_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance != "Sprint", Technique == "C", MS == 0) %>% distinct(Date, City)
    } else if (race_type == "Distance_C_Ms") {
      all_type_races <- chronos %>% filter(Date >= cutoff_date, Distance != "Sprint", Technique == "C", MS == 1) %>% distinct(Date, City) %>% arrange(Date)
      participant_type_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance != "Sprint", Technique == "C", MS == 1) %>% distinct(Date, City)
    } else if (race_type == "Distance_F_Ind") {
      all_type_races <- chronos %>% filter(Date >= cutoff_date, Distance != "Sprint", Technique == "F", MS == 0) %>% distinct(Date, City) %>% arrange(Date)
      participant_type_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance != "Sprint", Technique == "F", MS == 0) %>% distinct(Date, City)
    } else if (race_type == "Distance_F_Ms") {
      all_type_races <- chronos %>% filter(Date >= cutoff_date, Distance != "Sprint", Technique == "F", MS == 1) %>% distinct(Date, City) %>% arrange(Date)
      participant_type_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance != "Sprint", Technique == "F", MS == 1) %>% distinct(Date, City)
    } else if (race_type == "Distance_Ind") {
      all_type_races <- chronos %>% filter(Date >= cutoff_date, Distance != "Sprint", MS == 0) %>% distinct(Date, City) %>% arrange(Date)
      participant_type_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance != "Sprint", MS == 0) %>% distinct(Date, City)
    } else if (race_type == "Distance_Ms") {
      all_type_races <- chronos %>% filter(Date >= cutoff_date, Distance != "Sprint", MS == 1) %>% distinct(Date, City) %>% arrange(Date)
      participant_type_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant, Distance != "Sprint", MS == 1) %>% distinct(Date, City)
    } else {
      all_type_races <- chronos %>% filter(Date >= cutoff_date) %>% distinct(Date, City) %>% arrange(Date)
      participant_type_races <- chronos %>% filter(Date >= cutoff_date, Skier == participant) %>% distinct(Date, City)
    }

    n_races <- nrow(all_type_races)
    if(n_races == 0) return(0)

    # Create participation vector: 1 if athlete participated in that race, 0 if not
    participation <- sapply(1:n_races, function(i) {
      race_date <- all_type_races$Date[i]
      race_city <- all_type_races$City[i]
      as.numeric(any(participant_type_races$Date == race_date & participant_type_races$City == race_city))
    })

    # Apply exponential decay weights (alpha = 0.1)
    # Most recent race gets weight 1.0, older races get exponentially less weight
    race_weights <- exp(-0.1 * ((n_races - 1):0))

    # Calculate weighted participation probability
    weighted_participation <- sum(participation * race_weights)
    total_weight <- sum(race_weights)
    prob <- weighted_participation / total_weight

    return(prob)
  }

  # Calculate base race probabilities
  log_info("Calculating base race participation probabilities")
  men_race_counter_step3 <- 0
  ladies_race_counter_step3 <- 0

  for (i in 1:nrow(individual_races)) {
    race <- individual_races[i, ]

    if (race$Sex == "M") {
      men_race_counter_step3 <- men_race_counter_step3 + 1
      race_col <- paste0("Race", men_race_counter_step3, "_Prob")
      log_info(paste("Calculating probabilities for men's race", men_race_counter_step3, ":", race$Distance, race$Technique))
      men_startlist[[race_col]] <<- sapply(men_startlist$Skier, function(skier) {
        get_base_race_probability_local(men_chrono, skier, race$race_type)
      })
      log_info(paste("Men's", race_col, "calculated. Mean:", round(mean(men_startlist[[race_col]], na.rm = TRUE), 3)))
    } else if (race$Sex == "L") {
      ladies_race_counter_step3 <- ladies_race_counter_step3 + 1
      race_col <- paste0("Race", ladies_race_counter_step3, "_Prob")
      log_info(paste("Calculating probabilities for ladies' race", ladies_race_counter_step3, ":", race$Distance, race$Technique))
      ladies_startlist[[race_col]] <<- sapply(ladies_startlist$Skier, function(skier) {
        get_base_race_probability_local(ladies_chrono, skier, race$race_type)
      })
      log_info(paste("Ladies'", race_col, "calculated. Mean:", round(mean(ladies_startlist[[race_col]], na.rm = TRUE), 3)))
    }
  }

  # Apply 4-person quota constraint per nation per race
  log_info("Applying 4-person quota constraints per nation")
  men_race_counter_step3 <- 0
  ladies_race_counter_step3 <- 0

  for (i in 1:nrow(individual_races)) {
    race <- individual_races[i, ]

    if (race$Sex == "M") {
      men_race_counter_step3 <- men_race_counter_step3 + 1
      race_col <- paste0("Race", men_race_counter_step3, "_Prob")
      log_info(paste("Applying quota constraint for men's race", men_race_counter_step3, ":", race$Distance, race$Technique))

      nations <- unique(men_startlist$Nation[!is.na(men_startlist$Nation)])
      for (nation in nations) {
        nation_mask <- men_startlist$Nation == nation & !is.na(men_startlist$Nation)
        if (sum(nation_mask) > 0) {
          nation_probs <- men_startlist[nation_mask, race_col]
          current_sum <- sum(nation_probs, na.rm = TRUE)
          if (current_sum > 0) {
            scaling_factor <- 4 / current_sum
            scaled_probs <- nation_probs * scaling_factor
            # Cap individual probabilities at 1.0
            scaled_probs <- pmin(scaled_probs, 1.0)
            men_startlist[nation_mask, race_col] <<- scaled_probs
          }
        }
      }

      n_nations <- length(nations)
      final_mean <- mean(men_startlist[[race_col]], na.rm = TRUE)
      final_sum <- sum(men_startlist[[race_col]], na.rm = TRUE)
      expected_sum <- n_nations * 4
      log_info(paste("Final stats for men's", race_col, "- Mean prob:", round(final_mean, 3), "Total sum:", round(final_sum, 1), "Expected:", expected_sum))

    } else if (race$Sex == "L") {
      ladies_race_counter_step3 <- ladies_race_counter_step3 + 1
      race_col <- paste0("Race", ladies_race_counter_step3, "_Prob")
      log_info(paste("Applying quota constraint for ladies' race", ladies_race_counter_step3, ":", race$Distance, race$Technique))

      nations <- unique(ladies_startlist$Nation[!is.na(ladies_startlist$Nation)])
      for (nation in nations) {
        nation_mask <- ladies_startlist$Nation == nation & !is.na(ladies_startlist$Nation)
        if (sum(nation_mask) > 0) {
          nation_probs <- ladies_startlist[nation_mask, race_col]
          current_sum <- sum(nation_probs, na.rm = TRUE)
          if (current_sum > 0) {
            scaling_factor <- 4 / current_sum
            scaled_probs <- nation_probs * scaling_factor
            # Cap individual probabilities at 1.0
            scaled_probs <- pmin(scaled_probs, 1.0)
            ladies_startlist[nation_mask, race_col] <<- scaled_probs
          }
        }
      }

      n_nations <- length(nations)
      final_mean <- mean(ladies_startlist[[race_col]], na.rm = TRUE)
      final_sum <- sum(ladies_startlist[[race_col]], na.rm = TRUE)
      expected_sum <- n_nations * 4
      log_info(paste("Final stats for ladies'", race_col, "- Mean prob:", round(final_mean, 3), "Total sum:", round(final_sum, 1), "Expected:", expected_sum))
    }
  }

  log_info("Start probability calculations complete - Race_Prob columns now exist in startlists")
  # ============================================
  # END STEP 3
  # ============================================

  results_list <- list()

  # Reset counters for prediction loop
  men_race_counter <- 0
  ladies_race_counter <- 0

  for (i in 1:nrow(individual_races)) {
  race <- individual_races[i, ]
  gender <- if (race$Sex == "M") "men" else "ladies"
  
  # Track gender-specific race number
  if (race$Sex == "M") {
    men_race_counter <- men_race_counter + 1
    gender_race_num <- men_race_counter
  } else {
    ladies_race_counter <- ladies_race_counter + 1
    gender_race_num <- ladies_race_counter
  }
  
  log_info(paste("Processing", gender, "race", gender_race_num, ":", race$Distance, race$Technique, "- Race type:", race$race_type))
  
  # Get appropriate model and startlist
  if (gender == "men") {
    models <- men_models
    startlist <- men_startlist
    chrono_data <- men_chrono
  } else {
    models <- ladies_models
    startlist <- ladies_startlist
    chrono_data <- ladies_chrono
  }
  
  # Check if we have trained models for this race type
  if (!(race$race_type %in% names(models))) {
    log_warn(paste("No trained models for", race$race_type, "- skipping"))
    next
  }
  
  model_info <- models[[race$race_type]]
  
  # Get race-specific prev_points_weighted
  prev_points_data <- get_race_specific_prev_points(chrono_data, startlist$ID, race$race_type)
  
  # Debug: Check for duplicates
  duplicate_ids_startlist <- startlist$ID[duplicated(startlist$ID)]
  duplicate_ids_prevpoints <- prev_points_data$ID[duplicated(prev_points_data$ID)]
  
  if (length(duplicate_ids_startlist) > 0) {
    log_warn(paste("Duplicate IDs in startlist:", paste(duplicate_ids_startlist, collapse = ", ")))
  }
  if (length(duplicate_ids_prevpoints) > 0) {
    log_warn(paste("Duplicate IDs in prev_points_data:", paste(duplicate_ids_prevpoints, collapse = ", ")))
  }
  
  # Remove duplicates from startlist (keep first occurrence)
  startlist_clean <- startlist %>%
    distinct(ID, .keep_all = TRUE)
  
  # Ensure prev_points_data has unique IDs
  prev_points_data_clean <- prev_points_data %>%
    distinct(ID, .keep_all = TRUE)

  # Update startlist with race-specific prev_points_weighted
  test_data <- startlist_clean %>%
    select(-any_of("prev_points_weighted")) %>%  # Remove the placeholder column if it exists
    left_join(prev_points_data_clean, by = "ID")
  
  # Calculate position probabilities using the position threshold models
  position_probs <- calculate_position_probabilities(model_info, test_data)
  
  # Create results dataframe with position probabilities
  race_results <- test_data %>%
    select(Skier, Nation, ID) %>%
    mutate(
      # Store raw position probabilities
      raw_win_prob = position_probs$win_prob,
      raw_podium_prob = position_probs$podium_prob,
      raw_top5_prob = position_probs$top5_prob,
      raw_top10_prob = position_probs$top10_prob,
      raw_top30_prob = position_probs$top30_prob
    )
  
  # Get start probability for this race and multiply with position probabilities
  race_col <- paste0("Race", gender_race_num, "_Prob")
  
  # Check if the race probability column exists in the current startlist
  if (race_col %in% names(startlist_clean)) {
    # Add start probabilities to results dataframe
    # NOTE: Start probability multiplication commented out for testing (2026-02-01)
    race_results <- race_results %>%
      left_join(startlist_clean %>% select(ID, !!sym(race_col)), by = "ID") %>%
      mutate(
        start_prob = get(race_col),
        # Multiply position probabilities by start probabilities - COMMENTED OUT FOR TESTING
        # win_prob = raw_win_prob * start_prob,
        # podium_prob = raw_podium_prob * start_prob,
        # top5_prob = raw_top5_prob * start_prob,
        # top10_prob = raw_top10_prob * start_prob,
        # top30_prob = raw_top30_prob * start_prob
        win_prob = raw_win_prob,
        podium_prob = raw_podium_prob,
        top5_prob = raw_top5_prob,
        top10_prob = raw_top10_prob,
        top30_prob = raw_top30_prob
      ) %>%
      arrange(desc(win_prob))
  } else {
    # If no start probability column exists, use raw position probabilities
    log_warn(paste("Start probability column", race_col, "not found in startlist for", gender, "- using raw position probabilities"))
    race_results <- race_results %>%
      mutate(
        start_prob = 1.0,  # Default to 100% start probability
        win_prob = raw_win_prob,
        podium_prob = raw_podium_prob,
        top5_prob = raw_top5_prob,
        top10_prob = raw_top10_prob,
        top30_prob = raw_top30_prob
      ) %>%
      arrange(desc(win_prob))
  }
  
  # Store results with metadata for sheet naming
  race_name <- paste(gender, race$Distance, race$Technique)
  race_date_str <- format(race$Race_Date, "%b %d")  # Format as "Feb 12"
  results_list[[race_name]] <- list(
    data = race_results,
    gender = gender,
    distance = race$Distance,
    technique = race$Technique,
    race_date = race_date_str,
    race_num = gender_race_num
  )

  log_info(paste("Completed predictions for", race_name, "- Top prediction:",
                 race_results$Skier[1], "with", round(race_results$win_prob[1], 3), "win probability"))
}

# NOTE: Step 3 (start probability calculations) was moved to run BEFORE the prediction loop above
# This ensures Race_Prob columns exist when we make position predictions

# Step 4: Normalize and apply monotonic constraints (matching race-picks.R approach)
# Order: Normalize → Monotonic constraints → Re-normalize
log_info("=== Step 4: Normalization and Monotonic Constraints ===")

# Expected totals for each position threshold (as percentages: 100%, 300%, etc.)
position_thresholds <- c(1, 3, 5, 10, 30)
prob_cols <- c("win_prob", "podium_prob", "top5_prob", "top10_prob", "top30_prob")

for (race_name in names(results_list)) {
  race_entry <- results_list[[race_name]]
  race_results <- race_entry$data
  log_info(paste("Processing", race_name))

  # Store pre-normalization values for debugging
  race_results <- race_results %>%
    mutate(
      pre_norm_win = win_prob,
      pre_norm_podium = podium_prob,
      pre_norm_top5 = top5_prob,
      pre_norm_top10 = top10_prob,
      pre_norm_top30 = top30_prob
    )

  # Log initial sums before normalization
  log_info("  Sums BEFORE normalization:")
  for (i in seq_along(prob_cols)) {
    col <- prob_cols[i]
    threshold <- position_thresholds[i]
    initial_sum <- sum(race_results[[col]], na.rm = TRUE)
    log_info(sprintf("    %s: %.4f (target: %.1f)", col, initial_sum, threshold))
  }

  # PHASE 1: Initial normalization with iterative constrained capping
  # This properly handles athletes at the 100% cap by locking them and
  # distributing remaining probability budget among uncapped athletes
  for (i in seq_along(prob_cols)) {
    col <- prob_cols[i]
    threshold <- position_thresholds[i]

    probs_before <- race_results[[col]]
    n_capped_before <- sum(probs_before >= 1.0, na.rm = TRUE)

    # Apply iterative constrained normalization
    race_results[[col]] <- normalize_with_cap(race_results[[col]], target_sum = threshold, max_prob = 1.0)

    n_capped_after <- sum(race_results[[col]] >= 1.0, na.rm = TRUE)
    if (n_capped_after > 0) {
      log_info(sprintf("    %s: %d athletes at 100%% cap", col, n_capped_after))
    }
  }

  # PHASE 2: Apply monotonic constraints (win <= podium <= top5 <= top10 <= top30 <= start)
  log_info("  Applying monotonic constraints (including start_prob ceiling)...")

  # Store pre-hierarchy values
  race_results <- race_results %>%
    mutate(
      pre_hierarchy_win = win_prob,
      pre_hierarchy_podium = podium_prob,
      pre_hierarchy_top5 = top5_prob,
      pre_hierarchy_top10 = top10_prob,
      pre_hierarchy_top30 = top30_prob
    )

  # For each row, ensure probabilities are monotonically non-decreasing
  # NOTE: start_prob capping commented out for testing (2026-02-01)
  for (row_i in 1:nrow(race_results)) {
    # start_ceiling <- race_results$start_prob[row_i]

    probs <- c(
      race_results$win_prob[row_i],
      race_results$podium_prob[row_i],
      race_results$top5_prob[row_i],
      race_results$top10_prob[row_i],
      race_results$top30_prob[row_i]
    )

    # First, cap all position probabilities at start_prob - COMMENTED OUT FOR TESTING
    # probs <- pmin(probs, start_ceiling)

    # Then enforce: each probability >= previous one
    for (j in 2:length(probs)) {
      if (probs[j] < probs[j-1]) {
        probs[j] <- probs[j-1]
      }
    }

    # Final cap at start_prob (in case monotonic adjustment pushed values up) - COMMENTED OUT FOR TESTING
    # probs <- pmin(probs, start_ceiling)

    # Update row
    race_results$win_prob[row_i] <- probs[1]
    race_results$podium_prob[row_i] <- probs[2]
    race_results$top5_prob[row_i] <- probs[3]
    race_results$top10_prob[row_i] <- probs[4]
    race_results$top30_prob[row_i] <- probs[5]
  }

  # PHASE 3: Re-normalize after monotonic adjustment using iterative constrained normalization
  log_info("  Re-normalizing after monotonic constraints (iterative constrained)...")
  for (i in seq_along(prob_cols)) {
    col <- prob_cols[i]
    threshold <- position_thresholds[i]

    # Apply iterative constrained normalization - preserves athletes at 100% cap
    race_results[[col]] <- normalize_with_cap(race_results[[col]], target_sum = threshold, max_prob = 1.0)
  }

  # PHASE 4: Final cap at start_prob - COMMENTED OUT FOR TESTING (2026-02-01)
  # log_info("  Applying final start_prob ceiling...")
  # violations_fixed <- 0
  # for (row_i in 1:nrow(race_results)) {
  #   start_ceiling <- race_results$start_prob[row_i]
  #   for (col in prob_cols) {
  #     if (race_results[[col]][row_i] > start_ceiling) {
  #       race_results[[col]][row_i] <- start_ceiling
  #       violations_fixed <- violations_fixed + 1
  #     }
  #   }
  # }
  # if (violations_fixed > 0) {
  #   log_info(sprintf("    Fixed %d cases where position prob exceeded start_prob", violations_fixed))
  # }

  # PHASE 5: Final monotonic constraint enforcement
  # This is critical - no prediction is credible if win > podium > top5 etc.
  log_info("  PHASE 5: Final monotonic constraint enforcement...")
  monotonic_fixes <- 0
  for (row_i in 1:nrow(race_results)) {
    probs <- c(
      race_results$win_prob[row_i],
      race_results$podium_prob[row_i],
      race_results$top5_prob[row_i],
      race_results$top10_prob[row_i],
      race_results$top30_prob[row_i]
    )

    # Check if any violations exist
    needs_fix <- FALSE
    for (j in 2:length(probs)) {
      if (probs[j] < probs[j-1]) {
        needs_fix <- TRUE
        break
      }
    }

    if (needs_fix) {
      # Enforce monotonic: each probability >= previous one
      for (j in 2:length(probs)) {
        if (probs[j] < probs[j-1]) {
          probs[j] <- probs[j-1]
        }
      }

      # Update row
      race_results$win_prob[row_i] <- probs[1]
      race_results$podium_prob[row_i] <- probs[2]
      race_results$top5_prob[row_i] <- probs[3]
      race_results$top10_prob[row_i] <- probs[4]
      race_results$top30_prob[row_i] <- probs[5]
      monotonic_fixes <- monotonic_fixes + 1
    }
  }
  if (monotonic_fixes > 0) {
    log_info(sprintf("    Fixed monotonic violations in %d rows", monotonic_fixes))
  }

  # Log final sums
  log_info("  Sums AFTER 5-phase normalization:")
  for (i in seq_along(prob_cols)) {
    col <- prob_cols[i]
    threshold <- position_thresholds[i]
    final_sum <- sum(race_results[[col]], na.rm = TRUE)
    log_info(sprintf("    %s: %.4f (target: %.1f)", col, final_sum, threshold))
  }

  # Update results list (preserve metadata, update data)
  race_entry$data <- race_results
  results_list[[race_name]] <- race_entry
}

log_info("Normalization and monotonic constraints complete")

# Save results to Excel files in biathlon format
log_info("Saving individual race results to Excel files")

# Create output directory with today's date
current_year <- format(Sys.Date(), "%Y")
output_dir <- file.path("~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions", current_year)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Separate results by gender
men_results <- list()
ladies_results <- list()

for (race_name in names(results_list)) {
  race_entry <- results_list[[race_name]]
  race_data <- race_entry$data

  # Select columns, convert to percentages, and rename for user-friendly display
  # ID is second column (after Skier)
  race_data <- race_data %>%
    mutate(
      start_prob = round(start_prob * 100, 1),
      win_prob = round(win_prob * 100, 1),
      podium_prob = round(podium_prob * 100, 1),
      top5_prob = round(top5_prob * 100, 1),
      top10_prob = round(top10_prob * 100, 1),
      top30_prob = round(top30_prob * 100, 1)
    ) %>%
    select(Skier, ID, Nation, start_prob, win_prob, podium_prob, top5_prob, top10_prob, top30_prob) %>%
    rename(
      Start = start_prob,
      Win = win_prob,
      Podium = podium_prob,
      Top5 = top5_prob,
      `Top-10` = top10_prob,
      `Top-30` = top30_prob
    ) %>%
    arrange(desc(Win))

  # Get metadata for tab naming
  gender <- race_entry$gender
  distance <- race_entry$distance
  technique <- race_entry$technique
  race_date <- race_entry$race_date
  race_num <- race_entry$race_num

  # Helper function to expand distance and technique to full names
  expand_race_name <- function(dist, tech) {
    # Expand technique
    tech_full <- switch(tech,
      "P" = "Skiathlon",
      "C" = "Classic",
      "F" = "Freestyle",
      tech  # Default: keep as-is
    )

    # Handle distance
    if (dist == "Sprint") {
      return(paste("Sprint", tech_full))
    } else if (tech == "P") {
      # Skiathlon: just show "Xkm Skiathlon"
      return(paste0(dist, "km ", tech_full))
    } else {
      # Regular distance race: "Xkm Technique"
      return(paste0(dist, "km ", tech_full))
    }
  }

  # Create tab name with format: "1. 10km Freestyle - Feb 12"
  race_type <- if (technique == "") distance else expand_race_name(distance, technique)
  tab_name <- paste0(race_num, ". ", race_type, " - ", race_date)

  # Store in appropriate gender list
  if (gender == "men") {
    men_results[[tab_name]] <- race_data
  } else {
    ladies_results[[tab_name]] <- race_data
  }
}

# Save men's results
if (length(men_results) > 0) {
  men_file <- file.path(output_dir, "men_position_probabilities.xlsx")
  write.xlsx(men_results, men_file)
  log_info(paste("Saved men's results to", men_file))
  log_info(paste("Men's tabs:", paste(names(men_results), collapse = ", ")))
}

# Save ladies' results
if (length(ladies_results) > 0) {
  ladies_file <- file.path(output_dir, "ladies_position_probabilities.xlsx")
  write.xlsx(ladies_results, ladies_file)
  log_info(paste("Saved ladies' results to", ladies_file))
  log_info(paste("Ladies' tabs:", paste(names(ladies_results), collapse = ", ")))
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

  log_info("Individual test execution complete - all race predictions generated and saved")

  return(list(men_results = men_results, ladies_results = ladies_results))
}

# Function to process relay races
process_relay_races <- function() {
  log_info("=== PROCESSING RELAY RACES ===")
  
  # Get relay races
  relay_races <- champs_races %>%
    filter(Distance == "Rel", Sex %in% c("M", "L")) %>%
    arrange(Sex)
  
  log_info(paste("Found", nrow(relay_races), "relay races to process"))
  
  if (nrow(relay_races) == 0) {
    log_info("No relay races found")
    return(NULL)
  }
  
  # Read relay chronological data
  log_info("Reading relay chronological data files")
  
  men_relay_chrono <- read.csv("~/ski/elo/python/ski/polars/relay/excel365/men_chrono.csv", 
                               stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date))
  
  ladies_relay_chrono <- read.csv("~/ski/elo/python/ski/polars/relay/excel365/ladies_chrono.csv", 
                                  stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date))
  
  log_info(paste("Loaded", nrow(men_relay_chrono), "men's relay chronological records"))
  log_info(paste("Loaded", nrow(ladies_relay_chrono), "ladies' relay chronological records"))
  
  # Filter out team sprint races and Place == 0 from relay data
  log_info("Filtering relay chronological data")
  
  men_relay_chrono <- men_relay_chrono %>%
    filter(Distance != "Ts", Place != 0)
  
  ladies_relay_chrono <- ladies_relay_chrono %>%
    filter(Distance != "Ts", Place != 0)
  
  log_info(paste("After filtering - Men:", nrow(men_relay_chrono), "records"))
  log_info(paste("After filtering - Ladies:", nrow(ladies_relay_chrono), "records"))
  
  # Define world cup points system (same as individual)
  wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
  
  # Function to assign points based on place (same as individual)
  get_points <- function(place) {
    if (is.na(place) || place < 1 || place > length(wc_points)) {
      return(0)
    } else {
      return(wc_points[place])
    }
  }
  
  # Add points column to relay chronological data
  log_info("Adding points column based on world cup points system")
  
  men_relay_chrono <- men_relay_chrono %>%
    mutate(points = sapply(Place, get_points))
  
  ladies_relay_chrono <- ladies_relay_chrono %>%
    mutate(points = sapply(Place, get_points))
  
  log_info(paste("Added points - Men relay average points:", round(mean(men_relay_chrono$points, na.rm = TRUE), 2)))
  log_info(paste("Added points - Ladies relay average points:", round(mean(ladies_relay_chrono$points, na.rm = TRUE), 2)))
  
  log_info("Relay chronological data setup complete")
  
  # Calculate weighted prev_points for relay races
  log_info("Calculating weighted prev_points for relay races")
  
  calculate_relay_weighted_prev_points <- function(chrono_data) {
    chrono_data %>%
      arrange(Skier, Date) %>%  # Sort by skier and date for individual histories
      group_by(Skier) %>%
      mutate(
        prev_points_weighted = sapply(1:n(), function(i) {
          if (i == 1) return(0)  # First race has no previous races
          
          current_distance <- Distance[i]
          current_technique <- Technique[i]
          current_leg <- Leg[i]  # Relay leg number (1, 2, 3, 4)
          
          # Get all races up to (but not including) current race
          prev_distances <- Distance[1:(i-1)]
          prev_techniques <- Technique[1:(i-1)]
          prev_legs <- Leg[1:(i-1)]
          prev_points_values <- points[1:(i-1)]
          
          # Filter for matching race type based on relay logic
          # IMPORTANT: Relay races (Distance == "Rel") do NOT contribute to future averages
          # Only use non-relay races for calculating prev_points_weighted
          
          if (current_distance == "Rel") {
            # Relay race: use technique based on leg position, but EXCLUDE relay races from history
            if (current_leg %in% c(1, 2)) {
              # Legs 1 or 2: use Distance Classic prev points (excluding relays)
              matching <- prev_distances != "Sprint" & prev_techniques == "C" & prev_distances != "Rel"
            } else if (current_leg %in% c(3, 4)) {
              # Legs 3 or 4: use Distance Freestyle prev points (excluding relays)
              matching <- prev_distances != "Sprint" & prev_techniques == "F" & prev_distances != "Rel"
            } else {
              # Unknown leg, use all distance races (excluding relays)
              matching <- prev_distances != "Sprint" & prev_distances != "Rel"
            }
          } else {
            # Non-relay races: use same logic as individual (excluding relays)
            if (current_distance == "Sprint" && current_technique == "C") {
              matching <- prev_distances == "Sprint" & prev_techniques == "C" & prev_distances != "Rel"
            } else if (current_distance == "Sprint" && current_technique == "F") {
              matching <- prev_distances == "Sprint" & prev_techniques == "F" & prev_distances != "Rel"
            } else if (current_distance != "Sprint" && current_technique == "C") {
              matching <- prev_distances != "Sprint" & prev_techniques == "C" & prev_distances != "Rel"
            } else if (current_distance != "Sprint" && current_technique == "F") {
              matching <- prev_distances != "Sprint" & prev_techniques == "F" & prev_distances != "Rel"
            } else if (current_distance != "Sprint") {
              matching <- prev_distances != "Sprint" & prev_distances != "Rel"
            } else {
              matching <- rep(FALSE, length(prev_distances))
            }
          }
          
          # Get matching races
          matching_points <- prev_points_values[matching]
          
          if (length(matching_points) == 0) {
            return(0)  # No previous races of this type
          }
          
          # Take most recent 5 races and calculate weighted average
          recent_points <- tail(matching_points, 5)
          weights <- seq(1, length(recent_points))
          weighted_avg <- weighted.mean(recent_points, weights, na.rm = TRUE)
          
          return(ifelse(is.na(weighted_avg), 0, weighted_avg))
        })
      ) %>%
      ungroup()
  }
  
  # Apply prev_points calculation to relay data
  men_relay_chrono <- calculate_relay_weighted_prev_points(men_relay_chrono)

  ladies_relay_chrono <- calculate_relay_weighted_prev_points(ladies_relay_chrono)
  
  log_info(paste("Calculated prev_points - Men relay average:", round(mean(men_relay_chrono$prev_points_weighted, na.rm = TRUE), 2)))
  log_info(paste("Calculated prev_points - Ladies relay average:", round(mean(ladies_relay_chrono$prev_points_weighted, na.rm = TRUE), 2)))
  
  # Filter for last 20 seasons
  log_info("Filtering for last 20 seasons")
  current_season <- max(men_relay_chrono$Season, na.rm = TRUE)
  min_season <- current_season - 20
  
  men_relay_chrono <- men_relay_chrono %>%
    filter(Season >= min_season)
  
  ladies_relay_chrono <- ladies_relay_chrono %>%
    filter(Season >= min_season)
  
  log_info(paste("After season filtering - Men:", nrow(men_relay_chrono), "records"))
  log_info(paste("After season filtering - Ladies:", nrow(ladies_relay_chrono), "records"))
  
  # Quartile imputation for relay data (same function as individual)
  log_info("Applying quartile imputation to relay ELO and PELO columns")
  
  # Function to replace NAs with first quartile value (same as individual)
  replace_na_with_quartile <- function(x) {
    if(all(is.na(x))) return(rep(0, length(x)))
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    ifelse(is.na(x), q1, x)
  }
  
  # Define columns for imputation (both ELO and PELO)
  imputation_cols <- c("Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
                       "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", 
                       "Classic_Elo", "Freestyle_Elo", "Elo",
                       "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                       "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo", 
                       "Classic_Pelo", "Freestyle_Pelo", "Pelo")
  
  # Apply imputation to men's relay data
  for (col in imputation_cols) {
    if (col %in% names(men_relay_chrono)) {
      before_na_count <- sum(is.na(men_relay_chrono[[col]]))
      men_relay_chrono[[col]] <- replace_na_with_quartile(men_relay_chrono[[col]])
      log_info(paste("Men relay", col, "- Replaced", before_na_count, "NAs with quartile values"))
    }
  }
  
  # Apply imputation to ladies' relay data
  for (col in imputation_cols) {
    if (col %in% names(ladies_relay_chrono)) {
      before_na_count <- sum(is.na(ladies_relay_chrono[[col]]))
      ladies_relay_chrono[[col]] <- replace_na_with_quartile(ladies_relay_chrono[[col]])
      log_info(paste("Ladies relay", col, "- Replaced", before_na_count, "NAs with quartile values"))
    }
  }
  
  log_info("Relay quartile imputation complete")
  
  # Create ELO and PELO percentage columns for relay data
  log_info("Creating ELO and PELO percentage columns using current values")
  
  create_relay_pelo_percentages <- function(chrono_data) {
    # For relay races, group by race and leg; for other races, group by race only
    if ("Leg" %in% names(chrono_data)) {
      chrono_data <- chrono_data %>%
        group_by(Season, Race, Distance, Leg) %>%
        mutate(
          # Create Elo_pct and Pelo_pct columns (keep originals intact)
          Elo_pct = Elo / max(Elo, na.rm = TRUE),
          Pelo_pct = Pelo / max(Pelo, na.rm = TRUE),
          # Create other _pct columns for ELO columns
          Distance_Elo_pct = Distance_Elo / max(Distance_Elo, na.rm = TRUE),
          Distance_C_Elo_pct = Distance_C_Elo / max(Distance_C_Elo, na.rm = TRUE),
          Distance_F_Elo_pct = Distance_F_Elo / max(Distance_F_Elo, na.rm = TRUE),
          Sprint_Elo_pct = Sprint_Elo / max(Sprint_Elo, na.rm = TRUE),
          Sprint_C_Elo_pct = Sprint_C_Elo / max(Sprint_C_Elo, na.rm = TRUE),
          Sprint_F_Elo_pct = Sprint_F_Elo / max(Sprint_F_Elo, na.rm = TRUE),
          Classic_Elo_pct = Classic_Elo / max(Classic_Elo, na.rm = TRUE),
          Freestyle_Elo_pct = Freestyle_Elo / max(Freestyle_Elo, na.rm = TRUE),
          # Create other _pct columns for PELO columns
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
    } else {
      chrono_data <- chrono_data %>%
        group_by(Season, Race, Distance) %>%
        mutate(
          # Create Elo_pct and Pelo_pct columns (keep originals intact)
          Elo_pct = Elo / max(Elo, na.rm = TRUE),
          Pelo_pct = Pelo / max(Pelo, na.rm = TRUE),
          # Create other _pct columns for ELO columns
          Distance_Elo_pct = Distance_Elo / max(Distance_Elo, na.rm = TRUE),
          Distance_C_Elo_pct = Distance_C_Elo / max(Distance_C_Elo, na.rm = TRUE),
          Distance_F_Elo_pct = Distance_F_Elo / max(Distance_F_Elo, na.rm = TRUE),
          Sprint_Elo_pct = Sprint_Elo / max(Sprint_Elo, na.rm = TRUE),
          Sprint_C_Elo_pct = Sprint_C_Elo / max(Sprint_C_Elo, na.rm = TRUE),
          Sprint_F_Elo_pct = Sprint_F_Elo / max(Sprint_F_Elo, na.rm = TRUE),
          Classic_Elo_pct = Classic_Elo / max(Classic_Elo, na.rm = TRUE),
          Freestyle_Elo_pct = Freestyle_Elo / max(Freestyle_Elo, na.rm = TRUE),
          # Create other _pct columns for PELO columns
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
    
    return(chrono_data)
  }
  
  men_relay_chrono <- create_relay_pelo_percentages(men_relay_chrono)
  ladies_relay_chrono <- create_relay_pelo_percentages(ladies_relay_chrono)
  #View(men_relay_chrono %>% filter(Skier == "Erik Valnes"))
  log_info("Created PELO percentage columns")
  log_info(paste("Men relay PELO_pct range:", round(min(men_relay_chrono$Pelo_pct, na.rm = TRUE), 3), 
                 "to", round(max(men_relay_chrono$Pelo_pct, na.rm = TRUE), 3)))
  log_info(paste("Ladies relay PELO_pct range:", round(min(ladies_relay_chrono$Pelo_pct, na.rm = TRUE), 3), 
                 "to", round(max(ladies_relay_chrono$Pelo_pct, na.rm = TRUE), 3)))
  
  log_info("Relay data processing complete")
  
  # RELAY FEATURE SELECTION AND MODEL TRAINING
  log_info("=== RELAY FEATURE SELECTION AND MODEL TRAINING ===")
  
  # Function to get leg-specific explanatory variables
  get_relay_explanatory_vars <- function(leg_number) {
    base_vars <- c("prev_points_weighted", "Pelo_pct")
    
    if (leg_number == 1) {
      # Leg 1: Classic technique with sprint options
      return(c(base_vars, "Distance_Pelo_pct", "Distance_C_Pelo_pct", 
               "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Classic_Pelo_pct"))
    } else if (leg_number == 2) {
      # Leg 2: Classic technique, distance only (no sprint)
      return(c(base_vars, "Distance_Pelo_pct", "Distance_C_Pelo_pct", "Classic_Pelo_pct"))
    } else if (leg_number == 3) {
      # Leg 3: Freestyle technique, distance only (no sprint)
      return(c(base_vars, "Distance_Pelo_pct", "Distance_F_Pelo_pct", "Freestyle_Pelo_pct"))
    } else if (leg_number == 4) {
      # Leg 4: Freestyle technique with sprint options
      return(c(base_vars, "Distance_Pelo_pct", "Distance_F_Pelo_pct", 
               "Sprint_Pelo_pct", "Sprint_F_Pelo_pct", "Freestyle_Pelo_pct"))
    } else {
      return(c(base_vars, "Distance_Pelo_pct", "Distance_C_Pelo_pct", "Distance_F_Pelo_pct",
               "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Sprint_F_Pelo_pct", 
               "Classic_Pelo_pct", "Freestyle_Pelo_pct"))
    }
  }
  
  # Function to train relay leg models for all position thresholds
  train_relay_leg_models <- function(relay_chrono_data, gender) {
    log_info(paste("Training", gender, "relay leg models for all position thresholds"))
    
    # Filter to only relay races (Distance == "Rel")
    relay_data <- relay_chrono_data %>%
      filter(Distance == "Rel")
    
    log_info(paste("Filtered to", nrow(relay_data), "relay records"))
    
    if (nrow(relay_data) < 50) {  # Lower threshold for total relay data
      log_warn(paste("Insufficient relay data for", gender, "- skipping"))
      return(NULL)
    }

    # Define position thresholds (same as individual races)
    position_thresholds <- c(1, 3, 5, 10)  # win, podium, top5, top10
    
    # Store models for each leg and threshold combination
    leg_models <- list()
    selected_features <- list()
    
    # Train models for each leg (1-4) and each threshold
    for (leg in 1:4) {
      log_info(paste("Training", gender, "relay leg", leg, "models for all thresholds"))
      
      # Filter to specific leg
      leg_data <- relay_data %>%
        filter(Leg == leg)
      
      log_info(paste("Leg", leg, "has", nrow(leg_data), "records"))
      
      if (nrow(leg_data) == 0) {
        log_warn(paste("No data for leg", leg, "- skipping"))
        next
      }
      
      # Train a model for each position threshold
      for (threshold in position_thresholds) {
        log_info(paste("Training leg", leg, "threshold", threshold, "model"))
        
        tryCatch({
          # Create binary outcome variable for this threshold
          threshold_data <- leg_data %>%
            mutate(position_achieved = Place <= threshold)
          
          # Get leg-specific explanatory variables
          explanatory_vars <- get_relay_explanatory_vars(leg)
          
          # Feature selection using regsubsets
          formula <- as.formula(paste("position_achieved ~", paste(explanatory_vars, collapse = " + ")))
          
          feature_selection <- regsubsets(formula, data = threshold_data, nbest = 1, method = "exhaustive")
          feature_summary <- summary(feature_selection)
          best_bic_vars <- names(coef(feature_selection, which.min(feature_summary$bic)))
          
          # Create GAM model with selected features
          smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")  # Remove intercept
          gam_formula <- as.formula(paste("position_achieved ~", smooth_terms))
          
          log_info(paste("Selected features for leg", leg, "threshold", threshold, ":", paste(best_bic_vars[-1], collapse=", ")))
          
          # Fit binomial GAM model
          leg_model <- gam(gam_formula, data = threshold_data, family = binomial())
          
          # Calculate Brier score
          predicted_probs <- predict(leg_model, newdata = threshold_data, type = "response")
          brier_score <- mean((threshold_data$position_achieved - predicted_probs)^2, na.rm = TRUE)
          log_info(paste("Leg", leg, "threshold", threshold, "Brier score:", round(brier_score, 4)))
          
          # Store the model and features with combined key
          model_key <- paste0("leg_", leg, "_threshold_", threshold)
          leg_models[[model_key]] <- leg_model
          selected_features[[model_key]] <- best_bic_vars[-1]
          
          # Also store with old key format for podium (threshold=3) for backward compatibility
          if (threshold == 3) {
            leg_models[[paste0("leg_", leg)]] <- leg_model
            selected_features[[paste0("leg_", leg)]] <- best_bic_vars[-1]
          }
          
        }, error = function(e) {
          log_error(paste("Error training leg", leg, "threshold", threshold, "model:", e$message))
        })
      }
    }
    
    if (length(leg_models) == 0) {
      log_warn(paste("No successful leg models trained for", gender))
      return(NULL)
    }
    
    # Count successful models by threshold
    for (threshold in position_thresholds) {
      threshold_models <- sum(grepl(paste0("_threshold_", threshold), names(leg_models)))
      log_info(paste("Successfully trained", threshold_models, "models for threshold", threshold, "for", gender))
    }
    
    return(list(
      models = leg_models,
      features = selected_features,
      training_data_size = nrow(relay_data),
      thresholds = position_thresholds
    ))
  }
  
  # Train relay models for both genders
  men_relay_models <- train_relay_leg_models(men_relay_chrono, "men")
  ladies_relay_models <- train_relay_leg_models(ladies_relay_chrono, "ladies")
  
  # Summary of trained relay models
  log_info("=== RELAY MODEL TRAINING SUMMARY ===")
  if (!is.null(men_relay_models)) {
    log_info(paste("Men's relay models trained for legs:", paste(names(men_relay_models$models), collapse = ", ")))
  } else {
    log_info("No men's relay models trained")
  }
  
  if (!is.null(ladies_relay_models)) {
    log_info(paste("Ladies' relay models trained for legs:", paste(names(ladies_relay_models$models), collapse = ", ")))
  } else {
    log_info("No ladies' relay models trained")
  }
  
  log_info("Relay feature selection and model training complete")
  
  # STEP 2: CALCULATE LEG IMPORTANCE USING MODEL DEVIANCE EXPLAINED
  log_info("=== STEP 2: CALCULATING LEG IMPORTANCE ===")
  
  # Function to calculate leg importance based on model performance
  calculate_leg_importance <- function(relay_models, gender) {
    if (is.null(relay_models) || length(relay_models$models) == 0) {
      log_warn(paste("No", gender, "relay models available for importance calculation"))
      return(rep(0.25, 4))  # Equal weights if no models
    }
    
    log_info(paste("Calculating leg importance for", gender, "relay models"))
    
    # Extract deviance explained for each leg model
    leg_deviances <- numeric(4)
    leg_names <- paste0("leg_", 1:4)
    
    for (i in 1:4) {
      leg_name <- leg_names[i]
      if (leg_name %in% names(relay_models$models)) {
        model <- relay_models$models[[leg_name]]
        # Get deviance explained (R-squared equivalent for GAM)
        dev_explained <- summary(model)$dev.expl
        leg_deviances[i] <- dev_explained
        log_info(paste("Leg", i, "deviance explained:", round(dev_explained, 4)))
      } else {
        leg_deviances[i] <- 0
        log_warn(paste("No model found for leg", i, "- using 0 deviance"))
      }
    }
    
    # Convert deviances to importance weights (normalize to sum to 1)
    total_deviance <- sum(leg_deviances)
    if (total_deviance > 0) {
      importance_weights <- leg_deviances / total_deviance
    } else {
      # Fallback to equal weights if all deviances are 0
      importance_weights <- rep(0.25, 4)
      log_warn(paste("All", gender, "leg deviances are 0 - using equal weights"))
    }
    
    # Log final importance weights
    for (i in 1:4) {
      log_info(paste(gender, "Leg", i, "importance weight:", round(importance_weights[i], 4)))
    }
    
    return(importance_weights)
  }
  
  # Calculate importance weights for both genders
  men_leg_importance <- calculate_leg_importance(men_relay_models, "men")
  ladies_leg_importance <- calculate_leg_importance(ladies_relay_models, "ladies")
  
  # Summary of leg importance
  log_info("=== LEG IMPORTANCE SUMMARY ===")
  log_info("Men's leg importance weights:")
  for (i in 1:4) {
    log_info(paste("  Leg", i, ":", round(men_leg_importance[i], 4)))
  }
  
  log_info("Ladies' leg importance weights:")
  for (i in 1:4) {
    log_info(paste("  Leg", i, ":", round(ladies_leg_importance[i], 4)))
  }
  
  log_info("Leg importance calculation complete")
  
  # RELAY TEST SETUP
  log_info("=== RELAY TEST SETUP ===")
  
  # Read championship startlists for relays
  log_info("Reading championship startlists for relay optimization")
  
  men_relay_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_champs_men.csv", 
                                 stringsAsFactors = FALSE)
  
  ladies_relay_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_champs_ladies.csv", 
                                    stringsAsFactors = FALSE)
  
  log_info(paste("Loaded", nrow(men_relay_startlist), "men on relay startlist"))
  log_info(paste("Loaded", nrow(ladies_relay_startlist), "ladies on relay startlist"))
  
  # Define ELO columns for imputation
  elo_cols <- c("Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
                "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", 
                "Classic_Elo", "Freestyle_Elo", "Elo")
  
  # Function to replace NAs with first quartile value (same as used earlier)
  replace_na_with_quartile <- function(x) {
    if(all(is.na(x))) return(rep(0, length(x)))
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    ifelse(is.na(x), q1, x)
  }
  
  # Apply quartile imputation to men's relay startlist
  log_info("Applying quartile imputation to men's relay startlist ELO columns")
  for (col in elo_cols) {
    if (col %in% names(men_relay_startlist)) {
      before_na_count <- sum(is.na(men_relay_startlist[[col]]))
      men_relay_startlist[[col]] <- replace_na_with_quartile(men_relay_startlist[[col]])
      log_info(paste("Men relay startlist", col, "- Replaced", before_na_count, "NAs with quartile values"))
    }
  }
  
  # Apply quartile imputation to ladies' relay startlist
  log_info("Applying quartile imputation to ladies' relay startlist ELO columns")
  for (col in elo_cols) {
    if (col %in% names(ladies_relay_startlist)) {
      before_na_count <- sum(is.na(ladies_relay_startlist[[col]]))
      ladies_relay_startlist[[col]] <- replace_na_with_quartile(ladies_relay_startlist[[col]])
      log_info(paste("Ladies relay startlist", col, "- Replaced", before_na_count, "NAs with quartile values"))
    }
  }
  
  # Create PELO percentage columns (ELO value / max ELO in startlist)
  log_info("Creating PELO percentage columns for relay startlists")
  
  create_relay_startlist_pelo_pct <- function(startlist_data) {
    startlist_data %>%
      mutate(
        Pelo_pct = Elo / max(Elo, na.rm = TRUE),
        Distance_Pelo_pct = Distance_Elo / max(Distance_Elo, na.rm = TRUE),
        Distance_C_Pelo_pct = Distance_C_Elo / max(Distance_C_Elo, na.rm = TRUE),
        Distance_F_Pelo_pct = Distance_F_Elo / max(Distance_F_Elo, na.rm = TRUE),
        Sprint_Pelo_pct = Sprint_Elo / max(Sprint_Elo, na.rm = TRUE),
        Sprint_C_Pelo_pct = Sprint_C_Elo / max(Sprint_C_Elo, na.rm = TRUE),
        Sprint_F_Pelo_pct = Sprint_F_Elo / max(Sprint_F_Elo, na.rm = TRUE),
        Classic_Pelo_pct = Classic_Elo / max(Classic_Elo, na.rm = TRUE),
        Freestyle_Pelo_pct = Freestyle_Elo / max(Freestyle_Elo, na.rm = TRUE)
      )
  }
  
  men_relay_startlist <- create_relay_startlist_pelo_pct(men_relay_startlist)
  ladies_relay_startlist <- create_relay_startlist_pelo_pct(ladies_relay_startlist)
  
  log_info("Created PELO percentage columns for relay startlists")
  
  # Calculate prev_points_weighted for Classic and Freestyle
  log_info("Calculating prev_points_weighted_classic and prev_points_weighted_freestyle")
  
  # Function to get technique-specific prev_points_weighted
  get_technique_prev_points <- function(chrono_data, startlist_ids, technique) {
    unique_ids <- unique(startlist_ids)
    results <- data.frame(ID = unique_ids, prev_points_weighted = 0)
    
    for (i in 1:length(unique_ids)) {
      athlete_id <- unique_ids[i]
      
      # Get all historical races for this athlete
      athlete_races <- chrono_data %>%
        filter(ID == athlete_id) %>%
        arrange(Date)
      
      if (nrow(athlete_races) == 0) {
        next  # Keep prev_points_weighted = 0
      }
      
      # Filter to races matching the target technique
      if (technique == "Classic") {
        matching_races <- athlete_races %>% filter(Distance != "Sprint", Technique == "C")
      } else if (technique == "Freestyle") {
        matching_races <- athlete_races %>% filter(Distance != "Sprint", Technique == "F")
      } else {
        next  # Unknown technique
      }
      
      if (nrow(matching_races) == 0) {
        next  # No historical races of this type, keep 0
      }
      
      # Take the most recent 5 races of this type
      recent_races <- tail(matching_races, 5)
      
      # Calculate weighted average (weights 1, 2, 3, 4, 5 for oldest to newest)
      weights <- seq(1, nrow(recent_races))
      weighted_avg <- weighted.mean(recent_races$points, weights, na.rm = TRUE)
      
      # Store the result
      results$prev_points_weighted[i] <- weighted_avg
    }
    
    return(results)
  }
  
  # Calculate Classic prev_points for men
  log_info("Calculating men's prev_points_weighted_classic")
  men_classic_prev_points <- get_technique_prev_points(men_chrono, men_relay_startlist$ID, "Classic")
  men_relay_startlist <- men_relay_startlist %>%
    left_join(men_classic_prev_points %>% rename(prev_points_weighted_classic = prev_points_weighted), by = "ID")
  
  # Calculate Freestyle prev_points for men
  log_info("Calculating men's prev_points_weighted_freestyle")
  men_freestyle_prev_points <- get_technique_prev_points(men_chrono, men_relay_startlist$ID, "Freestyle")
  men_relay_startlist <- men_relay_startlist %>%
    left_join(men_freestyle_prev_points %>% rename(prev_points_weighted_freestyle = prev_points_weighted), by = "ID")
  
  # Calculate Classic prev_points for ladies
  log_info("Calculating ladies' prev_points_weighted_classic")
  ladies_classic_prev_points <- get_technique_prev_points(ladies_chrono, ladies_relay_startlist$ID, "Classic")
  ladies_relay_startlist <- ladies_relay_startlist %>%
    left_join(ladies_classic_prev_points %>% rename(prev_points_weighted_classic = prev_points_weighted), by = "ID")
  
  # Calculate Freestyle prev_points for ladies
  log_info("Calculating ladies' prev_points_weighted_freestyle")
  ladies_freestyle_prev_points <- get_technique_prev_points(ladies_chrono, ladies_relay_startlist$ID, "Freestyle")
  ladies_relay_startlist <- ladies_relay_startlist %>%
    left_join(ladies_freestyle_prev_points %>% rename(prev_points_weighted_freestyle = prev_points_weighted), by = "ID")
  
  log_info("Relay test setup complete")
  log_info(paste("Men relay startlist final size:", nrow(men_relay_startlist), "athletes"))
  log_info(paste("Ladies relay startlist final size:", nrow(ladies_relay_startlist), "athletes"))
  
  # STEP 3: ROSTER OPTIMIZATION
  log_info("=== STEP 3: ROSTER OPTIMIZATION ===")
  
  # Function to calculate team probability for a specific threshold
  # threshold: 1 = win, 3 = podium, 5 = top5, 10 = top10
  calculate_team_prob_for_threshold <- function(team_athletes, relay_models, leg_importance, threshold = 3) {
    leg_probs <- numeric(4)

    for (leg in 1:4) {
      # Try threshold-specific model first, then fall back to generic leg model
      model_key <- paste0("leg_", leg, "_threshold_", threshold)
      fallback_key <- paste0("leg_", leg)

      if (model_key %in% names(relay_models$models)) {
        athlete <- team_athletes[leg, ]

        # Create prediction data with appropriate prev_points_weighted
        pred_data <- athlete
        if (leg %in% c(1, 2)) {
          pred_data$prev_points_weighted <- athlete$prev_points_weighted_classic
        } else {
          pred_data$prev_points_weighted <- athlete$prev_points_weighted_freestyle
        }

        model <- relay_models$models[[model_key]]
        leg_probs[leg] <- predict(model, newdata = pred_data, type = "response")
      } else if (fallback_key %in% names(relay_models$models)) {
        athlete <- team_athletes[leg, ]
        pred_data <- athlete
        if (leg %in% c(1, 2)) {
          pred_data$prev_points_weighted <- athlete$prev_points_weighted_classic
        } else {
          pred_data$prev_points_weighted <- athlete$prev_points_weighted_freestyle
        }

        model <- relay_models$models[[fallback_key]]
        leg_probs[leg] <- predict(model, newdata = pred_data, type = "response")
      } else {
        leg_probs[leg] <- 0
      }
    }

    team_prob <- sum(leg_probs * leg_importance)
    return(list(team_prob = team_prob, leg_probs = leg_probs))
  }

  # Wrapper for podium probability (backward compatibility)
  calculate_team_podium_prob <- function(team_athletes, relay_models, leg_importance) {
    calculate_team_prob_for_threshold(team_athletes, relay_models, leg_importance, threshold = 3)
  }

  # Wrapper for win probability
  calculate_team_win_prob <- function(team_athletes, relay_models, leg_importance) {
    calculate_team_prob_for_threshold(team_athletes, relay_models, leg_importance, threshold = 1)
  }
  
  # Function to optimize team for a specific country (tracks both win and podium optimization)
  optimize_country_team <- function(country_athletes, relay_models, leg_importance, country_name) {
    n_athletes <- nrow(country_athletes)
    if (n_athletes < 4) {
      log_info(paste("Country", country_name, "has only", n_athletes, "athletes - skipping"))
      return(NULL)
    }

    log_info(paste("Optimizing team for", country_name, "with", n_athletes, "athletes"))

    # Track best for podium optimization
    best_podium_prob <- 0
    best_podium_team <- NULL
    best_podium_leg_probs <- NULL
    best_podium_legs <- NULL

    # Track best for win optimization
    best_win_prob <- 0
    best_win_team <- NULL
    best_win_leg_probs <- NULL
    best_win_legs <- NULL

    # Try all permutations (brute force - feasible with ≤8 athletes)
    for (leg1 in 1:n_athletes) {
      for (leg2 in 1:n_athletes) {
        if (leg2 == leg1) next
        for (leg3 in 1:n_athletes) {
          if (leg3 %in% c(leg1, leg2)) next
          for (leg4 in 1:n_athletes) {
            if (leg4 %in% c(leg1, leg2, leg3)) next

            team <- country_athletes[c(leg1, leg2, leg3, leg4), ]

            # Calculate both win and podium probabilities
            podium_result <- calculate_team_podium_prob(team, relay_models, leg_importance)
            win_result <- calculate_team_win_prob(team, relay_models, leg_importance)

            # Update best podium team
            if (podium_result$team_prob > best_podium_prob) {
              best_podium_prob <- podium_result$team_prob
              best_podium_team <- team
              best_podium_leg_probs <- podium_result$leg_probs
              best_podium_legs <- c(leg1, leg2, leg3, leg4)
            }

            # Update best win team
            if (win_result$team_prob > best_win_prob) {
              best_win_prob <- win_result$team_prob
              best_win_team <- team
              best_win_leg_probs <- win_result$leg_probs
              best_win_legs <- c(leg1, leg2, leg3, leg4)
            }
          }
        }
      }
    }

    log_info(paste("Best podium probability for", country_name, ":", round(best_podium_prob, 4)))
    log_info(paste("Best win probability for", country_name, ":", round(best_win_prob, 4)))

    # Check if win-optimized team differs from podium-optimized team
    teams_differ <- !identical(best_podium_legs, best_win_legs)
    if (teams_differ) {
      log_info(paste("  Note: Win-optimized team differs from podium-optimized team"))
    }

    return(list(
      country = country_name,
      # Podium optimization results
      podium_team = best_podium_team,
      team_podium_prob = best_podium_prob,
      podium_leg_probs = best_podium_leg_probs,
      podium_leg_assignments = best_podium_legs,
      # Win optimization results
      win_team = best_win_team,
      team_win_prob = best_win_prob,
      win_leg_probs = best_win_leg_probs,
      win_leg_assignments = best_win_legs,
      # For backward compatibility, 'team' refers to podium-optimized
      team = best_podium_team,
      leg_probs = best_podium_leg_probs,
      leg_assignments = best_podium_legs
    ))
  }
  
  # Function to process all countries for one gender
  process_relay_optimization <- function(relay_startlist, relay_models, leg_importance, gender) {
    log_info(paste("Processing relay optimization for", gender))
    
    if (is.null(relay_models) || is.null(relay_startlist)) {
      log_warn(paste("Missing models or startlist for", gender, "- skipping optimization"))
      return(NULL)
    }
    
    # Get countries with at least 4 athletes
    country_counts <- relay_startlist %>%
      group_by(Nation) %>%
      summarise(athlete_count = n(), .groups = "drop") %>%
      filter(athlete_count >= 4)
    
    log_info(paste("Found", nrow(country_counts), "countries with ≥4 athletes for", gender))
    
    # Optimize team for each eligible country
    optimization_results <- list()
    
    for (i in 1:nrow(country_counts)) {
      country <- country_counts$Nation[i]
      country_athletes <- relay_startlist %>%
        filter(Nation == country)
      
      result <- optimize_country_team(country_athletes, relay_models, leg_importance, country)
      
      if (!is.null(result)) {
        optimization_results[[country]] <- result
      }
    }
    
    log_info(paste("Successfully optimized teams for", length(optimization_results), "countries"))
    return(optimization_results)
  }
  
  # Optimize teams for both genders
  men_relay_optimization <- process_relay_optimization(
    men_relay_startlist, men_relay_models, men_leg_importance, "men"
  )
  
  ladies_relay_optimization <- process_relay_optimization(
    ladies_relay_startlist, ladies_relay_models, ladies_leg_importance, "ladies"
  )
  
  # Create Excel output format
  log_info("Creating Excel output for relay optimization results")

  # Function to format optimization results for Excel (supports both podium and win optimization)
  format_relay_results_for_excel <- function(optimization_results, optimization_type = "podium") {
    if (is.null(optimization_results) || length(optimization_results) == 0) {
      return(NULL)
    }

    excel_data <- data.frame()

    for (country in names(optimization_results)) {
      result <- optimization_results[[country]]

      # Select appropriate team based on optimization type
      if (optimization_type == "win") {
        team <- result$win_team
        leg_probs <- result$win_leg_probs
        team_prob <- result$team_win_prob
      } else {
        team <- result$podium_team
        leg_probs <- result$podium_leg_probs
        team_prob <- result$team_podium_prob
      }

      if (is.null(team)) next

      # Create one row per leg
      for (leg in 1:4) {
        row_data <- data.frame(
          Country = country,
          Leg = leg,
          Athlete = team$Skier[leg],
          Nation = team$Nation[leg],
          ID = team$ID[leg],
          Leg_Prob = round(leg_probs[leg], 4),
          Team_Prob = round(team_prob, 4)
        )
        excel_data <- rbind(excel_data, row_data)
      }
    }

    # Rename columns based on optimization type (clean names without underscores)
    if (optimization_type == "win") {
      names(excel_data)[names(excel_data) == "Leg_Prob"] <- "Leg Win"
      names(excel_data)[names(excel_data) == "Team_Prob"] <- "Team Win"
    } else {
      names(excel_data)[names(excel_data) == "Leg_Prob"] <- "Leg Podium"
      names(excel_data)[names(excel_data) == "Team_Prob"] <- "Team Podium"
    }

    return(excel_data)
  }

  # Create Excel data for both optimization types
  men_podium_excel <- format_relay_results_for_excel(men_relay_optimization, "podium")
  ladies_podium_excel <- format_relay_results_for_excel(ladies_relay_optimization, "podium")
  men_win_excel <- format_relay_results_for_excel(men_relay_optimization, "win")
  ladies_win_excel <- format_relay_results_for_excel(ladies_relay_optimization, "win")

  # Save to Excel files
  current_year <- format(Sys.Date(), "%Y")
  output_dir <- file.path("~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions", current_year)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save podium-optimized teams
  podium_results_list <- list()
  if (!is.null(men_podium_excel)) {
    podium_results_list[["Men Relay Teams"]] <- men_podium_excel
  }
  if (!is.null(ladies_podium_excel)) {
    podium_results_list[["Ladies Relay Teams"]] <- ladies_podium_excel
  }

  if (length(podium_results_list) > 0) {
    podium_file <- file.path(output_dir, "relay_team_optimization_podium.xlsx")
    write.xlsx(podium_results_list, podium_file)
    log_info(paste("Saved podium-optimized relay teams to", podium_file))
  }

  # Save win-optimized teams
  win_results_list <- list()
  if (!is.null(men_win_excel)) {
    win_results_list[["Men Relay Teams"]] <- men_win_excel
  }
  if (!is.null(ladies_win_excel)) {
    win_results_list[["Ladies Relay Teams"]] <- ladies_win_excel
  }

  if (length(win_results_list) > 0) {
    win_file <- file.path(output_dir, "relay_team_optimization_win.xlsx")
    write.xlsx(win_results_list, win_file)
    log_info(paste("Saved win-optimized relay teams to", win_file))
  }

  log_info("Roster optimization complete (both podium and win optimizations saved)")
  
  # STEP 4: GENERATE ALL THRESHOLD PREDICTIONS FOR OPTIMIZED TEAMS
  log_info("=== STEP 4: GENERATING ALL THRESHOLD PREDICTIONS ===")
  
  # Function to calculate all threshold probabilities for a team
  calculate_team_all_thresholds <- function(team_athletes, relay_models, leg_importance) {
    position_thresholds <- c(1, 3, 5, 10)  # win, podium, top5, top10
    
    # Initialize results
    threshold_results <- list()
    
    for (threshold in position_thresholds) {
      leg_probs <- numeric(4)
      
      for (leg in 1:4) {
        model_key <- paste0("leg_", leg, "_threshold_", threshold)
        
        if (model_key %in% names(relay_models$models)) {
          athlete <- team_athletes[leg, ]
          
          # Create prediction data with appropriate prev_points_weighted
          pred_data <- athlete
          if (leg %in% c(1, 2)) {
            # Classic legs - use classic prev_points
            pred_data$prev_points_weighted <- athlete$prev_points_weighted_classic
          } else {
            # Freestyle legs - use freestyle prev_points  
            pred_data$prev_points_weighted <- athlete$prev_points_weighted_freestyle
          }
          
          # Get leg model and predict
          model <- relay_models$models[[model_key]]
          leg_probs[leg] <- predict(model, newdata = pred_data, type = "response")
        } else {
          # If no model for this threshold/leg combination, use 0 probability
          leg_probs[leg] <- 0
        }
      }
      
      # Weight by leg importance
      team_prob <- sum(leg_probs * leg_importance)
      threshold_results[[paste0("threshold_", threshold)]] <- list(
        team_prob = team_prob,
        leg_probs = leg_probs
      )
    }
    
    return(threshold_results)
  }
  
  # Function to generate all threshold predictions for all countries
  generate_all_threshold_predictions <- function(optimization_results, relay_models, leg_importance, gender) {
    if (is.null(optimization_results) || length(optimization_results) == 0) {
      log_warn(paste("No optimization results for", gender, "- skipping threshold predictions"))
      return(NULL)
    }

    log_info(paste("Generating all threshold predictions for", gender, "relay teams"))

    all_predictions <- list()

    for (country in names(optimization_results)) {
      result <- optimization_results[[country]]
      podium_team <- result$podium_team
      win_team <- result$win_team

      # Calculate all threshold probabilities for BOTH team arrangements
      podium_predictions <- calculate_team_all_thresholds(podium_team, relay_models, leg_importance)
      win_predictions <- calculate_team_all_thresholds(win_team, relay_models, leg_importance)

      all_predictions[[country]] <- list(
        podium_team = podium_team,
        win_team = win_team,
        podium_predictions = podium_predictions,
        win_predictions = win_predictions,
        # For backward compatibility
        team = podium_team,
        predictions = podium_predictions
      )

      log_info(paste("Generated threshold predictions for", country))
    }

    return(all_predictions)
  }
  
  # Generate all threshold predictions for both genders
  men_all_predictions <- generate_all_threshold_predictions(
    men_relay_optimization, men_relay_models, men_leg_importance, "men"
  )
  
  ladies_all_predictions <- generate_all_threshold_predictions(
    ladies_relay_optimization, ladies_relay_models, ladies_leg_importance, "ladies"
  )
  
  # Format for Excel output
  log_info("Creating Excel output for all threshold predictions")
  
  # Function to format all threshold predictions for Excel
  format_all_threshold_predictions_for_excel <- function(all_predictions) {
    if (is.null(all_predictions) || length(all_predictions) == 0) {
      return(NULL)
    }

    excel_data <- data.frame()

    for (country in names(all_predictions)) {
      country_data <- all_predictions[[country]]
      team <- country_data$team
      predictions <- country_data$predictions

      # Create one row per leg with all threshold probabilities
      for (leg in 1:4) {
        row_data <- data.frame(
          Country = country,
          Leg = leg,
          Athlete = team$Skier[leg],
          Nation = team$Nation[leg],
          ID = team$ID[leg],
          `Leg Win` = round(predictions$threshold_1$leg_probs[leg], 4),
          `Leg Podium` = round(predictions$threshold_3$leg_probs[leg], 4),
          `Leg Top5` = round(predictions$threshold_5$leg_probs[leg], 4),
          `Leg Top-10` = round(predictions$threshold_10$leg_probs[leg], 4),
          `Team Win` = round(predictions$threshold_1$team_prob, 4),
          `Team Podium` = round(predictions$threshold_3$team_prob, 4),
          `Team Top5` = round(predictions$threshold_5$team_prob, 4),
          `Team Top-10` = round(predictions$threshold_10$team_prob, 4),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        excel_data <- bind_rows(excel_data, row_data)
      }
    }

    return(excel_data)
  }
  
  men_threshold_excel <- format_all_threshold_predictions_for_excel(men_all_predictions)
  ladies_threshold_excel <- format_all_threshold_predictions_for_excel(ladies_all_predictions)
  
  # Save all threshold predictions to Excel
  threshold_results_list <- list()
  if (!is.null(men_threshold_excel)) {
    threshold_results_list[["Men All Thresholds"]] <- men_threshold_excel
  }
  if (!is.null(ladies_threshold_excel)) {
    threshold_results_list[["Ladies All Thresholds"]] <- ladies_threshold_excel
  }
  
  if (length(threshold_results_list) > 0) {
    threshold_file <- file.path(output_dir, "relay_all_threshold_predictions.xlsx")
    write.xlsx(threshold_results_list, threshold_file)
    log_info(paste("Saved all threshold predictions to", threshold_file))
  }
  
  log_info("All threshold prediction generation complete")
  
  # NORMALIZATION AND MONOTONIC CONSTRAINTS (matching race-picks.R approach)
  # Order: Normalize → Monotonic constraints → Re-normalize
  log_info("=== NORMALIZATION AND MONOTONIC CONSTRAINTS ===")

  # Combined function: normalize, apply monotonic constraints, re-normalize
  normalize_and_constrain_relay <- function(all_predictions, gender) {
    if (is.null(all_predictions) || length(all_predictions) == 0) {
      return(all_predictions)
    }

    log_info(paste("Processing", gender, "relay predictions"))

    expected_totals <- c(1.0, 3.0, 5.0, 10.0)  # win, podium, top5, top10
    threshold_names <- c("threshold_1", "threshold_3", "threshold_5", "threshold_10")
    countries <- names(all_predictions)
    n_countries <- length(countries)

    # Helper to get current totals
    get_totals <- function() {
      totals <- numeric(4)
      for (i in 1:4) {
        for (country in countries) {
          totals[i] <- totals[i] + all_predictions[[country]]$predictions[[threshold_names[i]]]$team_prob
        }
      }
      totals
    }

    # Log initial totals
    initial_totals <- get_totals()
    log_info(paste("  Initial totals:", paste(round(initial_totals, 4), collapse = ", ")))

    # PHASE 1: Normalize with iterative constrained capping
    log_info("  Phase 1: Normalizing with iterative constrained capping...")
    for (i in 1:4) {
      threshold_name <- threshold_names[i]
      target <- expected_totals[i]

      # Extract probabilities as a vector
      probs <- sapply(countries, function(c) all_predictions[[c]]$predictions[[threshold_name]]$team_prob)

      # Apply iterative constrained normalization
      normalized_probs <- normalize_with_cap(probs, target_sum = target, max_prob = 1.0)

      # Put back into structure
      for (j in seq_along(countries)) {
        all_predictions[[countries[j]]]$predictions[[threshold_name]]$team_prob <- normalized_probs[j]
      }
    }

    # PHASE 2: Monotonic constraints (win <= podium <= top5 <= top10)
    log_info("  Phase 2: Applying monotonic constraints...")
    for (country in countries) {
      probs <- c(
        all_predictions[[country]]$predictions$threshold_1$team_prob,
        all_predictions[[country]]$predictions$threshold_3$team_prob,
        all_predictions[[country]]$predictions$threshold_5$team_prob,
        all_predictions[[country]]$predictions$threshold_10$team_prob
      )

      # Enforce monotonic (each >= previous)
      for (j in 2:4) {
        if (probs[j] < probs[j-1]) {
          probs[j] <- probs[j-1]
        }
      }

      all_predictions[[country]]$predictions$threshold_1$team_prob <- probs[1]
      all_predictions[[country]]$predictions$threshold_3$team_prob <- probs[2]
      all_predictions[[country]]$predictions$threshold_5$team_prob <- probs[3]
      all_predictions[[country]]$predictions$threshold_10$team_prob <- probs[4]
    }

    # PHASE 3: Re-normalize after monotonic adjustment using iterative constrained normalization
    log_info("  Phase 3: Re-normalizing (iterative constrained)...")
    for (i in 1:4) {
      threshold_name <- threshold_names[i]
      target <- expected_totals[i]

      # Extract probabilities as a vector
      probs <- sapply(countries, function(c) all_predictions[[c]]$predictions[[threshold_name]]$team_prob)

      # Apply iterative constrained normalization
      normalized_probs <- normalize_with_cap(probs, target_sum = target, max_prob = 1.0)

      # Put back into structure
      for (j in seq_along(countries)) {
        all_predictions[[countries[j]]]$predictions[[threshold_name]]$team_prob <- normalized_probs[j]
      }
    }

    # Log final totals
    final_totals <- get_totals()
    log_info(paste("  Final totals:", paste(round(final_totals, 4), collapse = ", ")))
    log_info(paste("  Expected:", paste(expected_totals, collapse = ", ")))

    return(all_predictions)
  }

  # Apply to both genders
  men_all_predictions <- normalize_and_constrain_relay(men_all_predictions, "men")
  ladies_all_predictions <- normalize_and_constrain_relay(ladies_all_predictions, "ladies")

  log_info("Normalization and monotonic constraints complete")
  
  # Update Excel output with hierarchy-enforced and normalized predictions
  log_info("Creating final Excel output with hierarchy-enforced and normalized predictions")
  
  men_threshold_excel_final <- format_all_threshold_predictions_for_excel(men_all_predictions)
  ladies_threshold_excel_final <- format_all_threshold_predictions_for_excel(ladies_all_predictions)
  
  # Save final normalized predictions to Excel
  final_threshold_results_list <- list()
  if (!is.null(men_threshold_excel_final)) {
    final_threshold_results_list[["Men All Thresholds Final"]] <- men_threshold_excel_final
  }
  if (!is.null(ladies_threshold_excel_final)) {
    final_threshold_results_list[["Ladies All Thresholds Final"]] <- ladies_threshold_excel_final
  }
  
  if (length(final_threshold_results_list) > 0) {
    final_threshold_file <- file.path(output_dir, "relay_final_predictions.xlsx")
    write.xlsx(final_threshold_results_list, final_threshold_file)
    log_info(paste("Saved final hierarchy-enforced and normalized predictions to", final_threshold_file))
  }
  
  log_info("Final relay prediction processing complete")

  # ============================================================================
  # CREATE NATIONS RELAY EXCEL FILES (Podium and Win Optimized)
  # Split by gender - one sheet per nation
  # ============================================================================
  log_info("=== Creating Nations Relay Excel Files (Podium & Win Optimized) ===")

  # Function to format relay nations data for a specific optimization type
  format_relay_nations_data <- function(all_predictions, optimization_results, gender, opt_type = "podium") {
    if (is.null(all_predictions) || length(all_predictions) == 0) {
      return(data.frame())
    }

    results <- data.frame()

    for (country in names(all_predictions)) {
      country_data <- all_predictions[[country]]

      # Select team AND predictions based on optimization type
      if (opt_type == "win") {
        team <- country_data$win_team
        predictions <- country_data$win_predictions
      } else {
        team <- country_data$podium_team
        predictions <- country_data$podium_predictions
      }

      if (is.null(team)) next

      for (leg in 1:4) {
        row_data <- data.frame(
          Athlete = team$Skier[leg],
          ID = team$ID[leg],
          Nation = country,
          Leg = leg,
          `Leg Win` = round(predictions$threshold_1$leg_probs[leg], 4),
          `Leg Podium` = round(predictions$threshold_3$leg_probs[leg], 4),
          `Leg Top5` = round(predictions$threshold_5$leg_probs[leg], 4),
          `Leg Top-10` = round(predictions$threshold_10$leg_probs[leg], 4),
          `Team Win` = round(predictions$threshold_1$team_prob, 4),
          `Team Podium` = round(predictions$threshold_3$team_prob, 4),
          `Team Top5` = round(predictions$threshold_5$team_prob, 4),
          `Team Top-10` = round(predictions$threshold_10$team_prob, 4),
          Gender = gender,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        results <- bind_rows(results, row_data)
      }
    }

    return(results)
  }

  # Function to create nations relay workbook for a given optimization type
  create_nations_relay_workbook <- function(men_data, ladies_data, opt_type) {
    nations_wb <- list()

    men_nations <- unique(men_data$Nation)
    ladies_nations <- unique(ladies_data$Nation)

    # Process men's relay nations (alphabetical order)
    for (nation in sort(men_nations)) {
      nation_data <- men_data %>%
        filter(Nation == nation) %>%
        select(-Gender) %>%
        arrange(Leg)

      if (nrow(nation_data) > 0) {
        sheet_name <- paste(nation, "Men")
        nations_wb[[sheet_name]] <- nation_data
      }
    }

    # Process ladies' relay nations (alphabetical order)
    for (nation in sort(ladies_nations)) {
      nation_data <- ladies_data %>%
        filter(Nation == nation) %>%
        select(-Gender) %>%
        arrange(Leg)

      if (nrow(nation_data) > 0) {
        sheet_name <- paste(nation, "Ladies")
        nations_wb[[sheet_name]] <- nation_data
      }
    }

    # Create Summary sheet
    all_data <- bind_rows(men_data, ladies_data)

    summary_data <- all_data %>%
      group_by(Gender, Nation) %>%
      summarise(
        `Team Win` = first(`Team Win`),
        `Team Podium` = first(`Team Podium`),
        `Team Top5` = first(`Team Top5`),
        `Team Top-10` = first(`Team Top-10`),
        .groups = "drop"
      ) %>%
      arrange(Gender, desc(`Team Win`))

    nations_wb[["Summary"]] <- summary_data

    return(nations_wb)
  }

  # Create PODIUM-optimized nations relay file
  log_info("Creating podium-optimized nations relay file...")
  men_relay_podium_data <- format_relay_nations_data(men_all_predictions, men_relay_optimization, "Men", "podium")
  ladies_relay_podium_data <- format_relay_nations_data(ladies_all_predictions, ladies_relay_optimization, "Ladies", "podium")

  nations_relay_podium_wb <- create_nations_relay_workbook(men_relay_podium_data, ladies_relay_podium_data, "podium")

  if (length(nations_relay_podium_wb) > 0) {
    nations_relay_podium_file <- file.path(output_dir, "nations_relay_podium.xlsx")
    write.xlsx(nations_relay_podium_wb, nations_relay_podium_file)
    log_info(paste("Saved podium-optimized nations relay to", nations_relay_podium_file))
    log_info(paste("Tabs:", paste(names(nations_relay_podium_wb), collapse = ", ")))
  }

  # Create WIN-optimized nations relay file
  log_info("Creating win-optimized nations relay file...")
  men_relay_win_data <- format_relay_nations_data(men_all_predictions, men_relay_optimization, "Men", "win")
  ladies_relay_win_data <- format_relay_nations_data(ladies_all_predictions, ladies_relay_optimization, "Ladies", "win")

  nations_relay_win_wb <- create_nations_relay_workbook(men_relay_win_data, ladies_relay_win_data, "win")

  if (length(nations_relay_win_wb) > 0) {
    nations_relay_win_file <- file.path(output_dir, "nations_relay_win.xlsx")
    write.xlsx(nations_relay_win_wb, nations_relay_win_file)
    log_info(paste("Saved win-optimized nations relay to", nations_relay_win_file))
    log_info(paste("Tabs:", paste(names(nations_relay_win_wb), collapse = ", ")))
  }

  return(list(
    men_models = men_relay_models,
    ladies_models = ladies_relay_models,
    men_leg_importance = men_leg_importance,
    ladies_leg_importance = ladies_leg_importance,
    men_relay_startlist = men_relay_startlist,
    ladies_relay_startlist = ladies_relay_startlist,
    men_optimization = men_relay_optimization,
    ladies_optimization = ladies_relay_optimization
  ))
}

# Function to process team sprint races
process_ts_races <- function() {
  log_info("=== PROCESSING TEAM SPRINT RACES ===")
  
  # Get team sprint races
  ts_races <- champs_races %>%
    filter(Distance == "Ts", Sex %in% c("M", "L")) %>%
    arrange(Sex)
  
  log_info(paste("Found", nrow(ts_races), "team sprint races to process"))
  
  if (nrow(ts_races) == 0) {
    log_info("No team sprint races found")
    return(NULL)
  }
  
  # Read team sprint chronological data
  log_info("Reading team sprint chronological data files")
  
  men_ts_chrono <- read.csv("~/ski/elo/python/ski/polars/relay/excel365/men_chrono.csv", 
                               stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date))
  
  ladies_ts_chrono <- read.csv("~/ski/elo/python/ski/polars/relay/excel365/ladies_chrono.csv", 
                                  stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date))
  
  log_info(paste("Loaded", nrow(men_ts_chrono), "men's team sprint chronological records"))
  log_info(paste("Loaded", nrow(ladies_ts_chrono), "ladies' team sprint chronological records"))
  
  # Filter out relay races and Place == 0 from team sprint data
  log_info("Filtering team sprint chronological data")
  
  men_ts_chrono <- men_ts_chrono %>%
    filter(Distance != "Rel", Place != 0)
  
  ladies_ts_chrono <- ladies_ts_chrono %>%
    filter(Distance != "Rel", Place != 0)
  
  log_info(paste("After filtering - Men:", nrow(men_ts_chrono), "records"))
  log_info(paste("After filtering - Ladies:", nrow(ladies_ts_chrono), "records"))
  
  # Define world cup points system (same as individual)
  wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
  
  # Function to assign points based on place (same as individual)
  get_points <- function(place) {
    if (is.na(place) || place < 1 || place > length(wc_points)) {
      return(0)
    } else {
      return(wc_points[place])
    }
  }
  
  # Add points column to team sprint chronological data
  log_info("Adding points column based on world cup points system")
  
  men_ts_chrono <- men_ts_chrono %>%
    mutate(points = sapply(Place, get_points))
  
  ladies_ts_chrono <- ladies_ts_chrono %>%
    mutate(points = sapply(Place, get_points))
  
  log_info(paste("Added points - Men team sprint average points:", round(mean(men_ts_chrono$points, na.rm = TRUE), 2)))
  log_info(paste("Added points - Ladies team sprint average points:", round(mean(ladies_ts_chrono$points, na.rm = TRUE), 2)))
  
  log_info("Team sprint chronological data setup complete")
  
  # Calculate weighted prev_points for team sprint races
  log_info("Calculating weighted prev_points for team sprint races")
  
  calculate_ts_weighted_prev_points <- function(chrono_data) {
    chrono_data %>%
      arrange(Skier, Date) %>%  # Sort by skier and date for individual histories
      group_by(Skier) %>%
      mutate(
        prev_points_weighted = sapply(1:n(), function(i) {
          if (i == 1) return(0)  # First race has no previous races
          
          current_distance <- Distance[i]
          current_technique <- Technique[i]
          current_leg <- Leg[i]  # Team sprint leg number (1, 2)
          
          # Get all races up to (but not including) current race
          prev_distances <- Distance[1:(i-1)]
          prev_techniques <- Technique[1:(i-1)]
          prev_legs <- Leg[1:(i-1)]
          prev_points_values <- points[1:(i-1)]
          
          # Filter for matching race type based on team sprint logic
          # IMPORTANT: Team sprint races (Distance == "Ts") do NOT contribute to future averages
          # Only use non-team sprint races for calculating prev_points_weighted
          
          if (current_distance == "Ts") {
            # Team sprint race: use sprint technique history, but EXCLUDE team sprints from history
            if (current_technique == "C") {
              # Classic team sprint: use Sprint Classic prev points (excluding team sprints)
              matching <- prev_distances == "Sprint" & prev_techniques == "C" & prev_distances != "Ts"
            } else if (current_technique == "F") {
              # Freestyle team sprint: use Sprint Freestyle prev points (excluding team sprints)
              matching <- prev_distances == "Sprint" & prev_techniques == "F" & prev_distances != "Ts"
            } else {
              # Unknown technique, use all sprint races (excluding team sprints)
              matching <- prev_distances == "Sprint" & prev_distances != "Ts"
            }
          } else {
            # Non-team sprint races: use same logic as individual (excluding team sprints)
            if (current_distance == "Sprint" && current_technique == "C") {
              matching <- prev_distances == "Sprint" & prev_techniques == "C" & prev_distances != "Ts"
            } else if (current_distance == "Sprint" && current_technique == "F") {
              matching <- prev_distances == "Sprint" & prev_techniques == "F" & prev_distances != "Ts"
            } else if (current_distance != "Sprint" && current_technique == "C") {
              matching <- prev_distances != "Sprint" & prev_techniques == "C" & prev_distances != "Ts"
            } else if (current_distance != "Sprint" && current_technique == "F") {
              matching <- prev_distances != "Sprint" & prev_techniques == "F" & prev_distances != "Ts"
            } else if (current_distance != "Sprint") {
              matching <- prev_distances != "Sprint" & prev_distances != "Ts"
            } else {
              matching <- rep(FALSE, length(prev_distances))
            }
          }
          
          # Get matching races
          matching_points <- prev_points_values[matching]
          
          if (length(matching_points) == 0) {
            return(0)  # No previous races of this type
          }
          
          # Take most recent 5 races and calculate weighted average
          recent_points <- tail(matching_points, 5)
          weights <- seq(1, length(recent_points))
          weighted_avg <- weighted.mean(recent_points, weights, na.rm = TRUE)
          
          return(ifelse(is.na(weighted_avg), 0, weighted_avg))
        })
      ) %>%
      ungroup()
  }
  
  # Apply prev_points calculation to team sprint data
  men_ts_chrono <- calculate_ts_weighted_prev_points(men_ts_chrono)

  ladies_ts_chrono <- calculate_ts_weighted_prev_points(ladies_ts_chrono)
  
  log_info(paste("Calculated prev_points - Men team sprint average:", round(mean(men_ts_chrono$prev_points_weighted, na.rm = TRUE), 2)))
  log_info(paste("Calculated prev_points - Ladies team sprint average:", round(mean(ladies_ts_chrono$prev_points_weighted, na.rm = TRUE), 2)))
  
  # Filter for last 20 seasons
  log_info("Filtering for last 20 seasons")
  current_season <- max(men_ts_chrono$Season, na.rm = TRUE)
  min_season <- current_season - 20
  
  men_ts_chrono <- men_ts_chrono %>%
    filter(Season >= min_season)
  
  ladies_ts_chrono <- ladies_ts_chrono %>%
    filter(Season >= min_season)
  
  log_info(paste("After season filtering - Men:", nrow(men_ts_chrono), "records"))
  log_info(paste("After season filtering - Ladies:", nrow(ladies_ts_chrono), "records"))
  
  # Quartile imputation for team sprint data (same function as individual)
  log_info("Applying quartile imputation to team sprint ELO and PELO columns")
  
  # Function to replace NAs with first quartile value (same as individual)
  replace_na_with_quartile <- function(x) {
    if(all(is.na(x))) return(rep(0, length(x)))
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    ifelse(is.na(x), q1, x)
  }
  
  # Define columns for imputation (both ELO and PELO)
  imputation_cols <- c("Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
                       "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", 
                       "Classic_Elo", "Freestyle_Elo", "Elo",
                       "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                       "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo", 
                       "Classic_Pelo", "Freestyle_Pelo", "Pelo")
  
  # Apply imputation to men's team sprint data
  for (col in imputation_cols) {
    if (col %in% names(men_ts_chrono)) {
      before_na_count <- sum(is.na(men_ts_chrono[[col]]))
      men_ts_chrono[[col]] <- replace_na_with_quartile(men_ts_chrono[[col]])
      log_info(paste("Men team sprint", col, "- Replaced", before_na_count, "NAs with quartile values"))
    }
  }
  
  # Apply imputation to ladies' team sprint data
  for (col in imputation_cols) {
    if (col %in% names(ladies_ts_chrono)) {
      before_na_count <- sum(is.na(ladies_ts_chrono[[col]]))
      ladies_ts_chrono[[col]] <- replace_na_with_quartile(ladies_ts_chrono[[col]])
      log_info(paste("Ladies team sprint", col, "- Replaced", before_na_count, "NAs with quartile values"))
    }
  }
  
  log_info("Team sprint quartile imputation complete")
  
  # Create ELO and PELO percentage columns for team sprint data
  log_info("Creating ELO and PELO percentage columns using current values")
  
  create_ts_pelo_percentages <- function(chrono_data) {
    # For team sprint races, group by race and leg; for other races, group by race only
    if ("Leg" %in% names(chrono_data)) {
      chrono_data <- chrono_data %>%
        group_by(Season, Race, Distance, Leg) %>%
        mutate(
          # Create Elo_pct and Pelo_pct columns (keep originals intact)
          Elo_pct = Elo / max(Elo, na.rm = TRUE),
          Pelo_pct = Pelo / max(Pelo, na.rm = TRUE),
          # Create other _pct columns for ELO columns
          Distance_Elo_pct = Distance_Elo / max(Distance_Elo, na.rm = TRUE),
          Distance_C_Elo_pct = Distance_C_Elo / max(Distance_C_Elo, na.rm = TRUE),
          Distance_F_Elo_pct = Distance_F_Elo / max(Distance_F_Elo, na.rm = TRUE),
          Sprint_Elo_pct = Sprint_Elo / max(Sprint_Elo, na.rm = TRUE),
          Sprint_C_Elo_pct = Sprint_C_Elo / max(Sprint_C_Elo, na.rm = TRUE),
          Sprint_F_Elo_pct = Sprint_F_Elo / max(Sprint_F_Elo, na.rm = TRUE),
          Classic_Elo_pct = Classic_Elo / max(Classic_Elo, na.rm = TRUE),
          Freestyle_Elo_pct = Freestyle_Elo / max(Freestyle_Elo, na.rm = TRUE),
          # Create other _pct columns for PELO columns
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
    } else {
      chrono_data <- chrono_data %>%
        group_by(Season, Race, Distance) %>%
        mutate(
          # Create Elo_pct and Pelo_pct columns (keep originals intact)
          Elo_pct = Elo / max(Elo, na.rm = TRUE),
          Pelo_pct = Pelo / max(Pelo, na.rm = TRUE),
          # Create other _pct columns for ELO columns
          Distance_Elo_pct = Distance_Elo / max(Distance_Elo, na.rm = TRUE),
          Distance_C_Elo_pct = Distance_C_Elo / max(Distance_C_Elo, na.rm = TRUE),
          Distance_F_Elo_pct = Distance_F_Elo / max(Distance_F_Elo, na.rm = TRUE),
          Sprint_Elo_pct = Sprint_Elo / max(Sprint_Elo, na.rm = TRUE),
          Sprint_C_Elo_pct = Sprint_C_Elo / max(Sprint_C_Elo, na.rm = TRUE),
          Sprint_F_Elo_pct = Sprint_F_Elo / max(Sprint_F_Elo, na.rm = TRUE),
          Classic_Elo_pct = Classic_Elo / max(Classic_Elo, na.rm = TRUE),
          Freestyle_Elo_pct = Freestyle_Elo / max(Freestyle_Elo, na.rm = TRUE),
          # Create other _pct columns for PELO columns
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
    
    return(chrono_data)
  }
  
  men_ts_chrono <- create_ts_pelo_percentages(men_ts_chrono)
  ladies_ts_chrono <- create_ts_pelo_percentages(ladies_ts_chrono)
  
  log_info("Created PELO percentage columns")
  log_info(paste("Men team sprint PELO_pct range:", round(min(men_ts_chrono$Pelo_pct, na.rm = TRUE), 3), 
                 "to", round(max(men_ts_chrono$Pelo_pct, na.rm = TRUE), 3)))
  log_info(paste("Ladies team sprint PELO_pct range:", round(min(ladies_ts_chrono$Pelo_pct, na.rm = TRUE), 3), 
                 "to", round(max(ladies_ts_chrono$Pelo_pct, na.rm = TRUE), 3)))
  
  log_info("Team sprint data processing complete")
  
  # TEAM SPRINT FEATURE SELECTION AND MODEL TRAINING
  log_info("=== TEAM SPRINT FEATURE SELECTION AND MODEL TRAINING ===")
  
  # Function to get technique-specific explanatory variables for team sprint training
  get_ts_explanatory_vars <- function(leg_number, technique) {
    base_vars <- c("prev_points_weighted", "Pelo_pct")
    
    # Team sprints: filter features based on technique during training
    if (technique == "C") {
      # Classic team sprint: only classic and technique-agnostic PELOs
      return(c(base_vars, "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Classic_Pelo_pct", 
               "Distance_Pelo_pct", "Distance_C_Pelo_pct"))
    } else if (technique == "F") {
      # Freestyle team sprint: only freestyle and technique-agnostic PELOs
      return(c(base_vars, "Sprint_Pelo_pct", "Sprint_F_Pelo_pct", "Freestyle_Pelo_pct", 
               "Distance_Pelo_pct", "Distance_F_Pelo_pct"))
    } else {
      # Unknown technique: include all options as fallback
      return(c(base_vars, "Sprint_Pelo_pct", "Sprint_C_Pelo_pct", "Sprint_F_Pelo_pct", 
               "Classic_Pelo_pct", "Freestyle_Pelo_pct", "Distance_Pelo_pct", "Distance_C_Pelo_pct", "Distance_F_Pelo_pct"))
    }
  }
  
  # Function to train team sprint leg models for all position thresholds
  train_ts_leg_models <- function(ts_chrono_data, gender) {
    log_info(paste("Training", gender, "team sprint leg models for all position thresholds"))
    
    # Filter to only team sprint races (Distance == "Ts")
    ts_data <- ts_chrono_data %>%
      filter(Distance == "Ts")
    
    log_info(paste("Filtered to", nrow(ts_data), "team sprint records"))
    
    if (nrow(ts_data) < 50) {  # Lower threshold for total team sprint data
      log_warn(paste("Insufficient team sprint data for", gender, "- skipping"))
      return(NULL)
    }

    # Define position thresholds (same as individual races)
    position_thresholds <- c(1, 3, 5, 10)  # win, podium, top5, top10
    
    # Store models for each technique, leg and threshold combination
    leg_models <- list()
    selected_features <- list()
    
    # Train models for each technique separately, then for each leg and threshold
    techniques <- unique(ts_data$Technique)
    log_info(paste("Found team sprint techniques:", paste(techniques, collapse = ", ")))
    
    for (technique in techniques) {
      technique_data <- ts_data %>%
        filter(Technique == technique)
      
      log_info(paste("Training models for", technique, "technique with", nrow(technique_data), "records"))
      
      # Train models for each leg (1-2 for team sprint) and each threshold
      for (leg in 1:2) {  # Team sprints only have 2 legs
        log_info(paste("Training", gender, "team sprint", technique, "leg", leg, "models for all thresholds"))
        
        # Filter to specific leg
        leg_data <- technique_data %>%
          filter(Leg == leg)
        
        log_info(paste("Technique", technique, "Leg", leg, "has", nrow(leg_data), "records"))
        
        if (nrow(leg_data) < 10) {  # Lower threshold per technique/leg combo
          log_warn(paste("Insufficient data for", technique, "leg", leg, "- skipping"))
          next
        }
        
        # Train a model for each position threshold
        for (threshold in position_thresholds) {
          log_info(paste("Training", technique, "leg", leg, "threshold", threshold, "model"))
          
          tryCatch({
            # Create binary outcome variable for this threshold
            threshold_data <- leg_data %>%
              mutate(position_achieved = Place <= threshold)
            
            # Get technique-specific explanatory variables for training
            explanatory_vars <- get_ts_explanatory_vars(leg, technique)
            
            # Feature selection using regsubsets
            formula <- as.formula(paste("position_achieved ~", paste(explanatory_vars, collapse = " + ")))
            
            feature_selection <- regsubsets(formula, data = threshold_data, nbest = 1, method = "exhaustive")
            feature_summary <- summary(feature_selection)
            best_bic_vars <- names(coef(feature_selection, which.min(feature_summary$bic)))
            
            # Create GAM model with selected features
            smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")  # Remove intercept
            gam_formula <- as.formula(paste("position_achieved ~", smooth_terms))
            
            log_info(paste("Selected features for", technique, "leg", leg, "threshold", threshold, ":", paste(best_bic_vars[-1], collapse=", ")))
            
            # Fit binomial GAM model
            leg_model <- gam(gam_formula, data = threshold_data, family = binomial())
            
            # Calculate Brier score
            predicted_probs <- predict(leg_model, newdata = threshold_data, type = "response")
            brier_score <- mean((threshold_data$position_achieved - predicted_probs)^2, na.rm = TRUE)
            log_info(paste("Technique", technique, "Leg", leg, "threshold", threshold, "Brier score:", round(brier_score, 4)))
            
            # Store the model and features with combined key including technique
            model_key <- paste0(technique, "_leg_", leg, "_threshold_", threshold)
            leg_models[[model_key]] <- leg_model
            selected_features[[model_key]] <- best_bic_vars[-1]
            
            # Also store with old key format for podium (threshold=3) for backward compatibility
            if (threshold == 3) {
              old_key <- paste0(technique, "_leg_", leg)
              leg_models[[old_key]] <- leg_model
              selected_features[[old_key]] <- best_bic_vars[-1]
              # Also store without technique for very old compatibility
              leg_models[[paste0("leg_", leg)]] <- leg_model
              selected_features[[paste0("leg_", leg)]] <- best_bic_vars[-1]
            }
            
          }, error = function(e) {
            log_error(paste("Error training", technique, "leg", leg, "threshold", threshold, "model:", e$message))
          })
        }
      }
    }
    
    if (length(leg_models) == 0) {
      log_warn(paste("No successful leg models trained for", gender))
      return(NULL)
    }
    
    # Count successful models by technique and threshold
    for (technique in techniques) {
      for (threshold in position_thresholds) {
        threshold_models <- sum(grepl(paste0(technique, "_leg_.*_threshold_", threshold), names(leg_models)))
        log_info(paste("Successfully trained", threshold_models, "models for", technique, "threshold", threshold, "for", gender))
      }
    }
    
    return(list(
      models = leg_models,
      features = selected_features,
      training_data_size = nrow(ts_data),
      thresholds = position_thresholds,
      techniques = techniques
    ))
  }
  
  # Train team sprint models for both genders
  men_ts_models <- train_ts_leg_models(men_ts_chrono, "men")
  ladies_ts_models <- train_ts_leg_models(ladies_ts_chrono, "ladies")
  
  # Summary of trained team sprint models
  log_info("=== TEAM SPRINT MODEL TRAINING SUMMARY ===")
  if (!is.null(men_ts_models)) {
    log_info(paste("Men's team sprint models trained for legs:", paste(names(men_ts_models$models), collapse = ", ")))
  } else {
    log_info("No men's team sprint models trained")
  }
  
  if (!is.null(ladies_ts_models)) {
    log_info(paste("Ladies' team sprint models trained for legs:", paste(names(ladies_ts_models$models), collapse = ", ")))
  } else {
    log_info("No ladies' team sprint models trained")
  }
  
  log_info("Team sprint feature selection and model training complete")
  
  # STEP 2: CALCULATE LEG IMPORTANCE USING MODEL DEVIANCE EXPLAINED
  log_info("=== STEP 2: CALCULATING LEG IMPORTANCE ===")
  
  # Function to calculate leg importance based on model performance for team sprint
  calculate_ts_leg_importance <- function(ts_models, gender) {
    if (is.null(ts_models) || length(ts_models$models) == 0) {
      log_warn(paste("No", gender, "team sprint models available for importance calculation"))
      return(rep(0.5, 2))  # Equal weights for 2 legs if no models
    }
    
    log_info(paste("Calculating leg importance for", gender, "team sprint models"))
    
    # Extract deviance explained for each leg model
    leg_deviances <- numeric(2)  # Team sprints have 2 legs
    leg_names <- paste0("leg_", 1:2)
    
    for (i in 1:2) {
      leg_name <- leg_names[i]
      if (leg_name %in% names(ts_models$models)) {
        model <- ts_models$models[[leg_name]]
        # Get deviance explained (R-squared equivalent for GAM)
        dev_explained <- summary(model)$dev.expl
        leg_deviances[i] <- dev_explained
        log_info(paste("Leg", i, "deviance explained:", round(dev_explained, 4)))
      } else {
        leg_deviances[i] <- 0
        log_warn(paste("No model found for leg", i, "- using 0 deviance"))
      }
    }
    
    # Convert deviances to importance weights (normalize to sum to 1)
    total_deviance <- sum(leg_deviances)
    if (total_deviance > 0) {
      importance_weights <- leg_deviances / total_deviance
    } else {
      # Fallback to equal weights if all deviances are 0
      importance_weights <- rep(0.5, 2)  # Equal weights for 2 legs
      log_warn(paste("All", gender, "leg deviances are 0 - using equal weights"))
    }
    
    # Log final importance weights
    for (i in 1:2) {
      log_info(paste(gender, "Leg", i, "importance weight:", round(importance_weights[i], 4)))
    }
    
    return(importance_weights)
  }
  
  # Calculate importance weights for both genders
  men_ts_leg_importance <- calculate_ts_leg_importance(men_ts_models, "men")
  ladies_ts_leg_importance <- calculate_ts_leg_importance(ladies_ts_models, "ladies")
  
  # Summary of leg importance
  log_info("=== LEG IMPORTANCE SUMMARY ===")
  log_info("Men's team sprint leg importance weights:")
  for (i in 1:2) {
    log_info(paste("  Leg", i, ":", round(men_ts_leg_importance[i], 4)))
  }
  
  log_info("Ladies' team sprint leg importance weights:")
  for (i in 1:2) {
    log_info(paste("  Leg", i, ":", round(ladies_ts_leg_importance[i], 4)))
  }
  
  log_info("Team sprint leg importance calculation complete")
  
  # TEAM SPRINT TEST SETUP
  log_info("=== TEAM SPRINT TEST SETUP ===")
  
  # Read championship startlists for team sprints
  log_info("Reading championship startlists for team sprint optimization")
  
  men_ts_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_champs_men.csv", 
                                 stringsAsFactors = FALSE)
  
  ladies_ts_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_champs_ladies.csv", 
                                    stringsAsFactors = FALSE)
  
  log_info(paste("Loaded", nrow(men_ts_startlist), "men on team sprint startlist"))
  log_info(paste("Loaded", nrow(ladies_ts_startlist), "ladies on team sprint startlist"))
  
  # Define ELO columns for imputation
  elo_cols <- c("Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
                "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", 
                "Classic_Elo", "Freestyle_Elo", "Elo")
  
  # Function to replace NAs with first quartile value (same as used earlier)
  replace_na_with_quartile <- function(x) {
    if(all(is.na(x))) return(rep(0, length(x)))
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    ifelse(is.na(x), q1, x)
  }
  
  # Apply quartile imputation to men's team sprint startlist
  log_info("Applying quartile imputation to men's team sprint startlist ELO columns")
  for (col in elo_cols) {
    if (col %in% names(men_ts_startlist)) {
      before_na_count <- sum(is.na(men_ts_startlist[[col]]))
      men_ts_startlist[[col]] <- replace_na_with_quartile(men_ts_startlist[[col]])
      log_info(paste("Men team sprint startlist", col, "- Replaced", before_na_count, "NAs with quartile values"))
    }
  }
  
  # Apply quartile imputation to ladies' team sprint startlist
  log_info("Applying quartile imputation to ladies' team sprint startlist ELO columns")
  for (col in elo_cols) {
    if (col %in% names(ladies_ts_startlist)) {
      before_na_count <- sum(is.na(ladies_ts_startlist[[col]]))
      ladies_ts_startlist[[col]] <- replace_na_with_quartile(ladies_ts_startlist[[col]])
      log_info(paste("Ladies team sprint startlist", col, "- Replaced", before_na_count, "NAs with quartile values"))
    }
  }
  
  # Create PELO percentage columns (ELO value / max ELO in startlist)
  log_info("Creating PELO percentage columns for team sprint startlists")
  
  create_ts_startlist_pelo_pct <- function(startlist_data) {
    startlist_data %>%
      mutate(
        Pelo_pct = Elo / max(Elo, na.rm = TRUE),
        Distance_Pelo_pct = Distance_Elo / max(Distance_Elo, na.rm = TRUE),
        Distance_C_Pelo_pct = Distance_C_Elo / max(Distance_C_Elo, na.rm = TRUE),
        Distance_F_Pelo_pct = Distance_F_Elo / max(Distance_F_Elo, na.rm = TRUE),
        Sprint_Pelo_pct = Sprint_Elo / max(Sprint_Elo, na.rm = TRUE),
        Sprint_C_Pelo_pct = Sprint_C_Elo / max(Sprint_C_Elo, na.rm = TRUE),
        Sprint_F_Pelo_pct = Sprint_F_Elo / max(Sprint_F_Elo, na.rm = TRUE),
        Classic_Pelo_pct = Classic_Elo / max(Classic_Elo, na.rm = TRUE),
        Freestyle_Pelo_pct = Freestyle_Elo / max(Freestyle_Elo, na.rm = TRUE)
      )
  }
  
  men_ts_startlist <- create_ts_startlist_pelo_pct(men_ts_startlist)
  ladies_ts_startlist <- create_ts_startlist_pelo_pct(ladies_ts_startlist)
  
  log_info("Created PELO percentage columns for team sprint startlists")
  
  # Calculate prev_points_weighted for Sprint Classic and Sprint Freestyle
  log_info("Calculating prev_points_weighted_sprint_classic and prev_points_weighted_sprint_freestyle")
  
  # Function to get technique-specific prev_points_weighted for team sprint
  get_ts_technique_prev_points <- function(chrono_data, startlist_ids, technique) {
    unique_ids <- unique(startlist_ids)
    results <- data.frame(ID = unique_ids, prev_points_weighted = 0)
    
    for (i in 1:length(unique_ids)) {
      athlete_id <- unique_ids[i]
      
      # Get all historical races for this athlete
      athlete_races <- chrono_data %>%
        filter(ID == athlete_id) %>%
        arrange(Date)
      
      if (nrow(athlete_races) == 0) {
        next  # Keep prev_points_weighted = 0
      }
      
      # Filter to races matching the target sprint technique
      if (technique == "Sprint_Classic") {
        matching_races <- athlete_races %>% filter(Distance == "Sprint", Technique == "C")
      } else if (technique == "Sprint_Freestyle") {
        matching_races <- athlete_races %>% filter(Distance == "Sprint", Technique == "F")
      } else {
        next  # Unknown technique
      }
      
      if (nrow(matching_races) == 0) {
        next  # No historical races of this type, keep 0
      }
      
      # Take the most recent 5 races of this type
      recent_races <- tail(matching_races, 5)
      
      # Calculate weighted average (weights 1, 2, 3, 4, 5 for oldest to newest)
      weights <- seq(1, nrow(recent_races))
      weighted_avg <- weighted.mean(recent_races$points, weights, na.rm = TRUE)
      
      # Store the result
      results$prev_points_weighted[i] <- weighted_avg
    }
    
    return(results)
  }
  
  # Calculate Sprint Classic prev_points for men
  log_info("Calculating men's prev_points_weighted_sprint_classic")
  men_sprint_classic_prev_points <- get_ts_technique_prev_points(men_chrono, men_ts_startlist$ID, "Sprint_Classic")
  men_ts_startlist <- men_ts_startlist %>%
    left_join(men_sprint_classic_prev_points %>% rename(prev_points_weighted_sprint_classic = prev_points_weighted), by = "ID")
  
  # Calculate Sprint Freestyle prev_points for men
  log_info("Calculating men's prev_points_weighted_sprint_freestyle")
  men_sprint_freestyle_prev_points <- get_ts_technique_prev_points(men_chrono, men_ts_startlist$ID, "Sprint_Freestyle")
  men_ts_startlist <- men_ts_startlist %>%
    left_join(men_sprint_freestyle_prev_points %>% rename(prev_points_weighted_sprint_freestyle = prev_points_weighted), by = "ID")
  
  # Calculate Sprint Classic prev_points for ladies
  log_info("Calculating ladies' prev_points_weighted_sprint_classic")
  ladies_sprint_classic_prev_points <- get_ts_technique_prev_points(ladies_chrono, ladies_ts_startlist$ID, "Sprint_Classic")
  ladies_ts_startlist <- ladies_ts_startlist %>%
    left_join(ladies_sprint_classic_prev_points %>% rename(prev_points_weighted_sprint_classic = prev_points_weighted), by = "ID")
  
  # Calculate Sprint Freestyle prev_points for ladies
  log_info("Calculating ladies' prev_points_weighted_sprint_freestyle")
  ladies_sprint_freestyle_prev_points <- get_ts_technique_prev_points(ladies_chrono, ladies_ts_startlist$ID, "Sprint_Freestyle")
  ladies_ts_startlist <- ladies_ts_startlist %>%
    left_join(ladies_sprint_freestyle_prev_points %>% rename(prev_points_weighted_sprint_freestyle = prev_points_weighted), by = "ID")
  
  log_info("Team sprint test setup complete")
  log_info(paste("Men team sprint startlist final size:", nrow(men_ts_startlist), "athletes"))
  log_info(paste("Ladies team sprint startlist final size:", nrow(ladies_ts_startlist), "athletes"))
  
  # STEP 3: ROSTER OPTIMIZATION
  log_info("=== STEP 3: ROSTER OPTIMIZATION ===")
  
  # Note: Technique-specific feature filtering is now done during training,
  # so prediction functions can use the athlete data directly with technique-specific models

  # Function to calculate team sprint team probability for a specific threshold
  # threshold: 1 = win, 3 = podium, 5 = top5, 10 = top10
  calculate_ts_team_prob_for_threshold <- function(team_athletes, ts_models, leg_importance, race_technique, threshold = 3) {
    leg_probs <- numeric(2)  # Team sprint has 2 legs

    for (leg in 1:2) {
      # Try threshold-specific model keys
      model_key <- paste0(race_technique, "_leg_", leg, "_threshold_", threshold)
      fallback_key <- paste0("leg_", leg, "_threshold_", threshold)
      old_model_key <- paste0(race_technique, "_leg_", leg)
      old_fallback_key <- paste0("leg_", leg)

      # Create prediction data
      athlete <- team_athletes[leg, ]
      pred_data <- athlete
      if (race_technique == "C") {
        pred_data$prev_points_weighted <- athlete$prev_points_weighted_sprint_classic
      } else {
        pred_data$prev_points_weighted <- athlete$prev_points_weighted_sprint_freestyle
      }

      if (model_key %in% names(ts_models$models)) {
        model <- ts_models$models[[model_key]]
        leg_probs[leg] <- predict(model, newdata = pred_data, type = "response")
      } else if (fallback_key %in% names(ts_models$models)) {
        model <- ts_models$models[[fallback_key]]
        leg_probs[leg] <- predict(model, newdata = pred_data, type = "response")
      } else if (old_model_key %in% names(ts_models$models)) {
        # Fall back to non-threshold model
        model <- ts_models$models[[old_model_key]]
        leg_probs[leg] <- predict(model, newdata = pred_data, type = "response")
      } else if (old_fallback_key %in% names(ts_models$models)) {
        model <- ts_models$models[[old_fallback_key]]
        leg_probs[leg] <- predict(model, newdata = pred_data, type = "response")
      } else {
        leg_probs[leg] <- 0
      }
    }

    team_prob <- sum(leg_probs * leg_importance)
    return(list(team_prob = team_prob, leg_probs = leg_probs))
  }

  # Wrapper for podium probability (backward compatibility)
  calculate_ts_team_podium_prob <- function(team_athletes, ts_models, leg_importance, race_technique) {
    calculate_ts_team_prob_for_threshold(team_athletes, ts_models, leg_importance, race_technique, threshold = 3)
  }

  # Wrapper for win probability
  calculate_ts_team_win_prob <- function(team_athletes, ts_models, leg_importance, race_technique) {
    calculate_ts_team_prob_for_threshold(team_athletes, ts_models, leg_importance, race_technique, threshold = 1)
  }
  
  # Function to optimize team sprint team for a specific country (tracks both win and podium optimization)
  optimize_ts_country_team <- function(country_athletes, ts_models, leg_importance, country_name, race_technique) {
    n_athletes <- nrow(country_athletes)
    if (n_athletes < 2) {  # Team sprint only needs 2 athletes
      log_info(paste("Country", country_name, "has only", n_athletes, "athletes - skipping"))
      return(NULL)
    }

    log_info(paste("Optimizing team sprint team for", country_name, "with", n_athletes, "athletes"))

    # Track best for podium optimization
    best_podium_prob <- 0
    best_podium_team <- NULL
    best_podium_leg_probs <- NULL
    best_podium_legs <- NULL

    # Track best for win optimization
    best_win_prob <- 0
    best_win_team <- NULL
    best_win_leg_probs <- NULL
    best_win_legs <- NULL

    # Try all permutations (brute force - feasible with ≤8 athletes, 2 legs)
    for (leg1 in 1:n_athletes) {
      for (leg2 in 1:n_athletes) {
        if (leg2 == leg1) next  # Same athlete can't run both legs

        team <- country_athletes[c(leg1, leg2), ]

        # Calculate both win and podium probabilities
        podium_result <- calculate_ts_team_podium_prob(team, ts_models, leg_importance, race_technique)
        win_result <- calculate_ts_team_win_prob(team, ts_models, leg_importance, race_technique)

        # Update best podium team
        if (podium_result$team_prob > best_podium_prob) {
          best_podium_prob <- podium_result$team_prob
          best_podium_team <- team
          best_podium_leg_probs <- podium_result$leg_probs
          best_podium_legs <- c(leg1, leg2)
        }

        # Update best win team
        if (win_result$team_prob > best_win_prob) {
          best_win_prob <- win_result$team_prob
          best_win_team <- team
          best_win_leg_probs <- win_result$leg_probs
          best_win_legs <- c(leg1, leg2)
        }
      }
    }

    log_info(paste("Best podium probability for", country_name, ":", round(best_podium_prob, 4)))
    log_info(paste("Best win probability for", country_name, ":", round(best_win_prob, 4)))

    # Check if win-optimized team differs from podium-optimized team
    teams_differ <- !identical(best_podium_legs, best_win_legs)
    if (teams_differ) {
      log_info(paste("  Note: Win-optimized team differs from podium-optimized team"))
    }

    return(list(
      country = country_name,
      # Podium optimization results
      podium_team = best_podium_team,
      team_podium_prob = best_podium_prob,
      podium_leg_probs = best_podium_leg_probs,
      podium_leg_assignments = best_podium_legs,
      # Win optimization results
      win_team = best_win_team,
      team_win_prob = best_win_prob,
      win_leg_probs = best_win_leg_probs,
      win_leg_assignments = best_win_legs,
      # For backward compatibility, 'team' refers to podium-optimized
      team = best_podium_team,
      leg_probs = best_podium_leg_probs,
      leg_assignments = best_podium_legs
    ))
  }
  
  # Function to process all countries for one gender
  process_ts_optimization <- function(ts_startlist, ts_models, leg_importance, gender, race_technique) {
    log_info(paste("Processing team sprint optimization for", gender))
    
    if (is.null(ts_models) || is.null(ts_startlist)) {
      log_warn(paste("Missing models or startlist for", gender, "- skipping optimization"))
      return(NULL)
    }
    
    # Get countries with at least 2 athletes (team sprint minimum)
    country_counts <- ts_startlist %>%
      group_by(Nation) %>%
      summarise(athlete_count = n(), .groups = "drop") %>%
      filter(athlete_count >= 2)
    
    log_info(paste("Found", nrow(country_counts), "countries with ≥2 athletes for", gender))
    
    # Optimize team for each eligible country
    optimization_results <- list()
    
    for (i in 1:nrow(country_counts)) {
      country <- country_counts$Nation[i]
      country_athletes <- ts_startlist %>%
        filter(Nation == country)
      
      result <- optimize_ts_country_team(country_athletes, ts_models, leg_importance, country, race_technique)
      
      if (!is.null(result)) {
        optimization_results[[country]] <- result
      }
    }
    
    log_info(paste("Successfully optimized teams for", length(optimization_results), "countries"))
    return(optimization_results)
  }
  
  # Note: Race technique would need to be determined from the championship schedule
  # For now, assuming we optimize for freestyle team sprints (most common)
  race_technique <- "F"  # This should be determined from champs_races data for team sprint
  
  # Optimize teams for both genders
  men_ts_optimization <- process_ts_optimization(
    men_ts_startlist, men_ts_models, men_ts_leg_importance, "men", race_technique
  )
  
  ladies_ts_optimization <- process_ts_optimization(
    ladies_ts_startlist, ladies_ts_models, ladies_ts_leg_importance, "ladies", race_technique
  )
  
  # Create Excel output format
  log_info("Creating Excel output for team sprint optimization results")

  # Function to format optimization results for Excel (supports both podium and win optimization)
  format_ts_results_for_excel <- function(optimization_results, optimization_type = "podium") {
    if (is.null(optimization_results) || length(optimization_results) == 0) {
      return(NULL)
    }

    excel_data <- data.frame()

    for (country in names(optimization_results)) {
      result <- optimization_results[[country]]

      # Select appropriate team based on optimization type
      if (optimization_type == "win") {
        team <- result$win_team
        leg_probs <- result$win_leg_probs
        team_prob <- result$team_win_prob
      } else {
        team <- result$podium_team
        leg_probs <- result$podium_leg_probs
        team_prob <- result$team_podium_prob
      }

      if (is.null(team)) next

      # Create one row per leg (2 legs for team sprint)
      for (leg in 1:2) {
        row_data <- data.frame(
          Country = country,
          Leg = leg,
          Athlete = team$Skier[leg],
          Nation = team$Nation[leg],
          ID = team$ID[leg],
          Leg_Prob = round(leg_probs[leg], 4),
          Team_Prob = round(team_prob, 4)
        )
        excel_data <- rbind(excel_data, row_data)
      }
    }

    # Rename columns based on optimization type (clean names without underscores)
    if (optimization_type == "win") {
      names(excel_data)[names(excel_data) == "Leg_Prob"] <- "Leg Win"
      names(excel_data)[names(excel_data) == "Team_Prob"] <- "Team Win"
    } else {
      names(excel_data)[names(excel_data) == "Leg_Prob"] <- "Leg Podium"
      names(excel_data)[names(excel_data) == "Team_Prob"] <- "Team Podium"
    }

    return(excel_data)
  }

  # Create Excel data for both optimization types
  men_ts_podium_excel <- format_ts_results_for_excel(men_ts_optimization, "podium")
  ladies_ts_podium_excel <- format_ts_results_for_excel(ladies_ts_optimization, "podium")
  men_ts_win_excel <- format_ts_results_for_excel(men_ts_optimization, "win")
  ladies_ts_win_excel <- format_ts_results_for_excel(ladies_ts_optimization, "win")

  # Save to Excel files
  current_year <- format(Sys.Date(), "%Y")
  output_dir <- file.path("~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions", current_year)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save podium-optimized teams
  ts_podium_results_list <- list()
  if (!is.null(men_ts_podium_excel)) {
    ts_podium_results_list[["Men Team Sprint Teams"]] <- men_ts_podium_excel
  }
  if (!is.null(ladies_ts_podium_excel)) {
    ts_podium_results_list[["Ladies Team Sprint Teams"]] <- ladies_ts_podium_excel
  }

  if (length(ts_podium_results_list) > 0) {
    ts_podium_file <- file.path(output_dir, "team_sprint_optimization_podium.xlsx")
    write.xlsx(ts_podium_results_list, ts_podium_file)
    log_info(paste("Saved podium-optimized team sprint teams to", ts_podium_file))
  }

  # Save win-optimized teams
  ts_win_results_list <- list()
  if (!is.null(men_ts_win_excel)) {
    ts_win_results_list[["Men Team Sprint Teams"]] <- men_ts_win_excel
  }
  if (!is.null(ladies_ts_win_excel)) {
    ts_win_results_list[["Ladies Team Sprint Teams"]] <- ladies_ts_win_excel
  }

  if (length(ts_win_results_list) > 0) {
    ts_win_file <- file.path(output_dir, "team_sprint_optimization_win.xlsx")
    write.xlsx(ts_win_results_list, ts_win_file)
    log_info(paste("Saved win-optimized team sprint teams to", ts_win_file))
  }

  log_info("Team sprint roster optimization complete (both podium and win optimizations saved)")
  
  # STEP 4: GENERATE ALL THRESHOLD PREDICTIONS FOR OPTIMIZED TEAMS
  log_info("=== STEP 4: GENERATING ALL THRESHOLD PREDICTIONS ===")
  
  # Function to calculate all threshold probabilities for a team sprint team
  calculate_ts_team_all_thresholds <- function(team_athletes, ts_models, leg_importance, race_technique) {
    position_thresholds <- c(1, 3, 5, 10)  # win, podium, top5, top10
    
    # Initialize results
    threshold_results <- list()
    
    for (threshold in position_thresholds) {
      leg_probs <- numeric(2)  # Team sprint has 2 legs
      
      for (leg in 1:2) {
        # Try technique-specific model key first, fallback to old format
        model_key <- paste0(race_technique, "_leg_", leg, "_threshold_", threshold)
        fallback_key <- paste0("leg_", leg, "_threshold_", threshold)
        
        if (model_key %in% names(ts_models$models)) {
          athlete <- team_athletes[leg, ]
          
          # Create prediction data with appropriate prev_points_weighted based on race technique
          pred_data <- athlete
          if (race_technique == "C") {
            # Classic team sprint - use sprint classic prev_points
            pred_data$prev_points_weighted <- athlete$prev_points_weighted_sprint_classic
          } else {
            # Freestyle team sprint - use sprint freestyle prev_points  
            pred_data$prev_points_weighted <- athlete$prev_points_weighted_sprint_freestyle
          }
          
          # Technique-specific filtering is handled by using technique-specific models
          
          # Get leg model and predict
          model <- ts_models$models[[model_key]]
          leg_probs[leg] <- predict(model, newdata = pred_data, type = "response")
        } else if (fallback_key %in% names(ts_models$models)) {
          # Fallback to old model format
          athlete <- team_athletes[leg, ]
          
          # Create prediction data with appropriate prev_points_weighted based on race technique
          pred_data <- athlete
          if (race_technique == "C") {
            pred_data$prev_points_weighted <- athlete$prev_points_weighted_sprint_classic
          } else {
            pred_data$prev_points_weighted <- athlete$prev_points_weighted_sprint_freestyle
          }
          
          # Technique-specific filtering is handled by using technique-specific models
          
          model <- ts_models$models[[fallback_key]]
          leg_probs[leg] <- predict(model, newdata = pred_data, type = "response")
        } else {
          # If no model for this threshold/leg combination, use 0 probability
          leg_probs[leg] <- 0
        }
      }
      
      # Weight by leg importance
      team_prob <- sum(leg_probs * leg_importance)
      threshold_results[[paste0("threshold_", threshold)]] <- list(
        team_prob = team_prob,
        leg_probs = leg_probs
      )
    }
    
    return(threshold_results)
  }
  
  # Function to generate all threshold predictions for all countries
  generate_ts_all_threshold_predictions <- function(optimization_results, ts_models, leg_importance, gender, race_technique) {
    if (is.null(optimization_results) || length(optimization_results) == 0) {
      log_warn(paste("No optimization results for", gender, "- skipping threshold predictions"))
      return(NULL)
    }

    log_info(paste("Generating all threshold predictions for", gender, "team sprint teams"))

    all_predictions <- list()

    for (country in names(optimization_results)) {
      result <- optimization_results[[country]]
      podium_team <- result$podium_team
      win_team <- result$win_team

      # Calculate all threshold probabilities for BOTH team arrangements
      podium_predictions <- calculate_ts_team_all_thresholds(podium_team, ts_models, leg_importance, race_technique)
      win_predictions <- calculate_ts_team_all_thresholds(win_team, ts_models, leg_importance, race_technique)

      all_predictions[[country]] <- list(
        podium_team = podium_team,
        win_team = win_team,
        podium_predictions = podium_predictions,
        win_predictions = win_predictions,
        # For backward compatibility
        team = podium_team,
        predictions = podium_predictions
      )

      log_info(paste("Generated threshold predictions for", country))
    }

    return(all_predictions)
  }
  
  # Generate all threshold predictions for both genders
  men_ts_all_predictions <- generate_ts_all_threshold_predictions(
    men_ts_optimization, men_ts_models, men_ts_leg_importance, "men", race_technique
  )
  
  ladies_ts_all_predictions <- generate_ts_all_threshold_predictions(
    ladies_ts_optimization, ladies_ts_models, ladies_ts_leg_importance, "ladies", race_technique
  )
  
  # Format for Excel output
  log_info("Creating Excel output for all team sprint threshold predictions")
  
  # Function to format all threshold predictions for Excel
  format_ts_all_threshold_predictions_for_excel <- function(all_predictions) {
    if (is.null(all_predictions) || length(all_predictions) == 0) {
      return(NULL)
    }

    excel_data <- data.frame()

    for (country in names(all_predictions)) {
      country_data <- all_predictions[[country]]
      team <- country_data$team
      predictions <- country_data$predictions

      # Create one row per leg with all threshold probabilities (2 legs for team sprint)
      for (leg in 1:2) {
        row_data <- data.frame(
          Country = country,
          Leg = leg,
          Athlete = team$Skier[leg],
          Nation = team$Nation[leg],
          ID = team$ID[leg],
          `Leg Win` = round(predictions$threshold_1$leg_probs[leg], 4),
          `Leg Podium` = round(predictions$threshold_3$leg_probs[leg], 4),
          `Leg Top5` = round(predictions$threshold_5$leg_probs[leg], 4),
          `Leg Top-10` = round(predictions$threshold_10$leg_probs[leg], 4),
          `Team Win` = round(predictions$threshold_1$team_prob, 4),
          `Team Podium` = round(predictions$threshold_3$team_prob, 4),
          `Team Top5` = round(predictions$threshold_5$team_prob, 4),
          `Team Top-10` = round(predictions$threshold_10$team_prob, 4),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        excel_data <- bind_rows(excel_data, row_data)
      }
    }

    return(excel_data)
  }
  
  men_ts_threshold_excel <- format_ts_all_threshold_predictions_for_excel(men_ts_all_predictions)
  ladies_ts_threshold_excel <- format_ts_all_threshold_predictions_for_excel(ladies_ts_all_predictions)
  
  # Save all threshold predictions to Excel
  ts_threshold_results_list <- list()
  if (!is.null(men_ts_threshold_excel)) {
    ts_threshold_results_list[["Men All Thresholds"]] <- men_ts_threshold_excel
  }
  if (!is.null(ladies_ts_threshold_excel)) {
    ts_threshold_results_list[["Ladies All Thresholds"]] <- ladies_ts_threshold_excel
  }
  
  if (length(ts_threshold_results_list) > 0) {
    ts_threshold_file <- file.path(output_dir, "team_sprint_all_threshold_predictions.xlsx")
    write.xlsx(ts_threshold_results_list, ts_threshold_file)
    log_info(paste("Saved team sprint all threshold predictions to", ts_threshold_file))
  }
  
  log_info("Team sprint all threshold prediction generation complete")
  
  # NORMALIZATION AND MONOTONIC CONSTRAINTS (matching race-picks.R approach)
  # Order: Normalize → Monotonic constraints → Re-normalize
  log_info("=== NORMALIZATION AND MONOTONIC CONSTRAINTS ===")

  # Combined function: normalize, apply monotonic constraints, re-normalize
  normalize_and_constrain_ts <- function(all_predictions, gender) {
    if (is.null(all_predictions) || length(all_predictions) == 0) {
      return(all_predictions)
    }

    log_info(paste("Processing", gender, "team sprint predictions"))

    expected_totals <- c(1.0, 3.0, 5.0, 10.0)  # win, podium, top5, top10
    threshold_names <- c("threshold_1", "threshold_3", "threshold_5", "threshold_10")
    countries <- names(all_predictions)
    n_countries <- length(countries)

    # Helper to get current totals
    get_totals <- function() {
      totals <- numeric(4)
      for (i in 1:4) {
        for (country in countries) {
          totals[i] <- totals[i] + all_predictions[[country]]$predictions[[threshold_names[i]]]$team_prob
        }
      }
      totals
    }

    # Log initial totals
    initial_totals <- get_totals()
    log_info(paste("  Initial totals:", paste(round(initial_totals, 4), collapse = ", ")))

    # PHASE 1: Normalize with iterative constrained capping
    log_info("  Phase 1: Normalizing with iterative constrained capping...")
    for (i in 1:4) {
      threshold_name <- threshold_names[i]
      target <- expected_totals[i]

      # Extract probabilities as a vector
      probs <- sapply(countries, function(c) all_predictions[[c]]$predictions[[threshold_name]]$team_prob)

      # Apply iterative constrained normalization
      normalized_probs <- normalize_with_cap(probs, target_sum = target, max_prob = 1.0)

      # Put back into structure
      for (j in seq_along(countries)) {
        all_predictions[[countries[j]]]$predictions[[threshold_name]]$team_prob <- normalized_probs[j]
      }
    }

    # PHASE 2: Monotonic constraints (win <= podium <= top5 <= top10)
    log_info("  Phase 2: Applying monotonic constraints...")
    for (country in countries) {
      probs <- c(
        all_predictions[[country]]$predictions$threshold_1$team_prob,
        all_predictions[[country]]$predictions$threshold_3$team_prob,
        all_predictions[[country]]$predictions$threshold_5$team_prob,
        all_predictions[[country]]$predictions$threshold_10$team_prob
      )

      # Enforce monotonic (each >= previous)
      for (j in 2:4) {
        if (probs[j] < probs[j-1]) {
          probs[j] <- probs[j-1]
        }
      }

      all_predictions[[country]]$predictions$threshold_1$team_prob <- probs[1]
      all_predictions[[country]]$predictions$threshold_3$team_prob <- probs[2]
      all_predictions[[country]]$predictions$threshold_5$team_prob <- probs[3]
      all_predictions[[country]]$predictions$threshold_10$team_prob <- probs[4]
    }

    # PHASE 3: Re-normalize after monotonic adjustment using iterative constrained normalization
    log_info("  Phase 3: Re-normalizing (iterative constrained)...")
    for (i in 1:4) {
      threshold_name <- threshold_names[i]
      target <- expected_totals[i]

      # Extract probabilities as a vector
      probs <- sapply(countries, function(c) all_predictions[[c]]$predictions[[threshold_name]]$team_prob)

      # Apply iterative constrained normalization
      normalized_probs <- normalize_with_cap(probs, target_sum = target, max_prob = 1.0)

      # Put back into structure
      for (j in seq_along(countries)) {
        all_predictions[[countries[j]]]$predictions[[threshold_name]]$team_prob <- normalized_probs[j]
      }
    }

    # Log final totals
    final_totals <- get_totals()
    log_info(paste("  Final totals:", paste(round(final_totals, 4), collapse = ", ")))
    log_info(paste("  Expected:", paste(expected_totals, collapse = ", ")))

    return(all_predictions)
  }

  # Apply to both genders
  men_ts_all_predictions <- normalize_and_constrain_ts(men_ts_all_predictions, "men")
  ladies_ts_all_predictions <- normalize_and_constrain_ts(ladies_ts_all_predictions, "ladies")

  log_info("Team sprint normalization and monotonic constraints complete")
  
  # Update Excel output with hierarchy-enforced and normalized predictions
  log_info("Creating final Excel output with hierarchy-enforced and normalized team sprint predictions")
  
  men_ts_threshold_excel_final <- format_ts_all_threshold_predictions_for_excel(men_ts_all_predictions)
  ladies_ts_threshold_excel_final <- format_ts_all_threshold_predictions_for_excel(ladies_ts_all_predictions)
  
  # Save final normalized predictions to Excel
  ts_final_threshold_results_list <- list()
  if (!is.null(men_ts_threshold_excel_final)) {
    ts_final_threshold_results_list[["Men All Thresholds Final"]] <- men_ts_threshold_excel_final
  }
  if (!is.null(ladies_ts_threshold_excel_final)) {
    ts_final_threshold_results_list[["Ladies All Thresholds Final"]] <- ladies_ts_threshold_excel_final
  }
  
  if (length(ts_final_threshold_results_list) > 0) {
    ts_final_threshold_file <- file.path(output_dir, "team_sprint_final_predictions.xlsx")
    write.xlsx(ts_final_threshold_results_list, ts_final_threshold_file)
    log_info(paste("Saved final hierarchy-enforced and normalized team sprint predictions to", ts_final_threshold_file))
  }
  
  log_info("Final team sprint prediction processing complete")

  # ============================================================================
  # CREATE NATIONS TEAM SPRINT EXCEL FILES (Podium and Win Optimized)
  # Split by gender - one sheet per nation
  # ============================================================================
  log_info("=== Creating Nations Team Sprint Excel Files (Podium & Win Optimized) ===")

  # Function to format team sprint nations data for a specific optimization type
  format_ts_nations_data <- function(all_predictions, optimization_results, gender, opt_type = "podium") {
    if (is.null(all_predictions) || length(all_predictions) == 0) {
      return(data.frame())
    }

    results <- data.frame()

    for (country in names(all_predictions)) {
      country_data <- all_predictions[[country]]

      # Select team AND predictions based on optimization type
      if (opt_type == "win") {
        team <- country_data$win_team
        predictions <- country_data$win_predictions
      } else {
        team <- country_data$podium_team
        predictions <- country_data$podium_predictions
      }

      if (is.null(team)) next

      for (leg in 1:2) {  # Team sprint has 2 legs
        row_data <- data.frame(
          Athlete = team$Skier[leg],
          ID = team$ID[leg],
          Nation = country,
          Leg = leg,
          `Leg Win` = round(predictions$threshold_1$leg_probs[leg], 4),
          `Leg Podium` = round(predictions$threshold_3$leg_probs[leg], 4),
          `Leg Top5` = round(predictions$threshold_5$leg_probs[leg], 4),
          `Leg Top-10` = round(predictions$threshold_10$leg_probs[leg], 4),
          `Team Win` = round(predictions$threshold_1$team_prob, 4),
          `Team Podium` = round(predictions$threshold_3$team_prob, 4),
          `Team Top5` = round(predictions$threshold_5$team_prob, 4),
          `Team Top-10` = round(predictions$threshold_10$team_prob, 4),
          Gender = gender,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        results <- bind_rows(results, row_data)
      }
    }

    return(results)
  }

  # Function to create nations team sprint workbook for a given optimization type
  create_nations_ts_workbook <- function(men_data, ladies_data, opt_type) {
    nations_wb <- list()

    men_nations <- unique(men_data$Nation)
    ladies_nations <- unique(ladies_data$Nation)

    # Process men's team sprint nations (alphabetical order)
    for (nation in sort(men_nations)) {
      nation_data <- men_data %>%
        filter(Nation == nation) %>%
        select(-Gender) %>%
        arrange(Leg)

      if (nrow(nation_data) > 0) {
        sheet_name <- paste(nation, "Men")
        nations_wb[[sheet_name]] <- nation_data
      }
    }

    # Process ladies' team sprint nations (alphabetical order)
    for (nation in sort(ladies_nations)) {
      nation_data <- ladies_data %>%
        filter(Nation == nation) %>%
        select(-Gender) %>%
        arrange(Leg)

      if (nrow(nation_data) > 0) {
        sheet_name <- paste(nation, "Ladies")
        nations_wb[[sheet_name]] <- nation_data
      }
    }

    # Create Summary sheet
    all_data <- bind_rows(men_data, ladies_data)

    summary_data <- all_data %>%
      group_by(Gender, Nation) %>%
      summarise(
        `Team Win` = first(`Team Win`),
        `Team Podium` = first(`Team Podium`),
        `Team Top5` = first(`Team Top5`),
        `Team Top-10` = first(`Team Top-10`),
        .groups = "drop"
      ) %>%
      arrange(Gender, desc(`Team Win`))

    nations_wb[["Summary"]] <- summary_data

    return(nations_wb)
  }

  # Create PODIUM-optimized nations team sprint file
  log_info("Creating podium-optimized nations team sprint file...")
  men_ts_podium_data <- format_ts_nations_data(men_ts_all_predictions, men_ts_optimization, "Men", "podium")
  ladies_ts_podium_data <- format_ts_nations_data(ladies_ts_all_predictions, ladies_ts_optimization, "Ladies", "podium")

  nations_ts_podium_wb <- create_nations_ts_workbook(men_ts_podium_data, ladies_ts_podium_data, "podium")

  if (length(nations_ts_podium_wb) > 0) {
    nations_ts_podium_file <- file.path(output_dir, "nations_ts_podium.xlsx")
    write.xlsx(nations_ts_podium_wb, nations_ts_podium_file)
    log_info(paste("Saved podium-optimized nations team sprint to", nations_ts_podium_file))
    log_info(paste("Tabs:", paste(names(nations_ts_podium_wb), collapse = ", ")))
  }

  # Create WIN-optimized nations team sprint file
  log_info("Creating win-optimized nations team sprint file...")
  men_ts_win_data <- format_ts_nations_data(men_ts_all_predictions, men_ts_optimization, "Men", "win")
  ladies_ts_win_data <- format_ts_nations_data(ladies_ts_all_predictions, ladies_ts_optimization, "Ladies", "win")

  nations_ts_win_wb <- create_nations_ts_workbook(men_ts_win_data, ladies_ts_win_data, "win")

  if (length(nations_ts_win_wb) > 0) {
    nations_ts_win_file <- file.path(output_dir, "nations_ts_win.xlsx")
    write.xlsx(nations_ts_win_wb, nations_ts_win_file)
    log_info(paste("Saved win-optimized nations team sprint to", nations_ts_win_file))
    log_info(paste("Tabs:", paste(names(nations_ts_win_wb), collapse = ", ")))
  }

  return(list(
    men_models = men_ts_models,
    ladies_models = ladies_ts_models,
    men_leg_importance = men_ts_leg_importance,
    ladies_leg_importance = ladies_ts_leg_importance,
    men_ts_startlist = men_ts_startlist,
    ladies_ts_startlist = ladies_ts_startlist,
    men_optimization = men_ts_optimization,
    ladies_optimization = ladies_ts_optimization,
    men_all_predictions = men_ts_all_predictions,
    ladies_all_predictions = ladies_ts_all_predictions
  ))
}

# Main execution - call processing functions
log_info("=== CROSS-COUNTRY CHAMPIONSHIPS PREDICTIONS ===")

# Process individual races
individual_results <- process_individual_races()

# Process relay races
relay_results <- process_relay_races()

# Extract trained relay models, leg importance, and prepared startlists
if (!is.null(relay_results)) {
  men_relay_models <- relay_results$men_models
  ladies_relay_models <- relay_results$ladies_models
  men_leg_importance <- relay_results$men_leg_importance
  ladies_leg_importance <- relay_results$ladies_leg_importance
  men_relay_startlist <- relay_results$men_relay_startlist
  ladies_relay_startlist <- relay_results$ladies_relay_startlist

  log_info("Relay models, leg importance weights, and startlists successfully prepared")
} else {
  log_warn("No relay models were trained")
  men_relay_models <- NULL
  ladies_relay_models <- NULL
  men_leg_importance <- rep(0.25, 4)  # Equal weights fallback
  ladies_leg_importance <- rep(0.25, 4)  # Equal weights fallback
  men_relay_startlist <- NULL
  ladies_relay_startlist <- NULL
}

# Process team sprint races
team_sprint_results <- process_ts_races()

# Extract trained team sprint models, leg importance, and prepared startlists
if (!is.null(team_sprint_results)) {
  men_ts_models <- team_sprint_results$men_models
  ladies_ts_models <- team_sprint_results$ladies_models
  men_ts_leg_importance <- team_sprint_results$men_leg_importance
  ladies_ts_leg_importance <- team_sprint_results$ladies_leg_importance
  men_ts_startlist <- team_sprint_results$men_ts_startlist
  ladies_ts_startlist <- team_sprint_results$ladies_ts_startlist
  
  log_info("Team sprint models, leg importance weights, and startlists successfully prepared")
} else {
  log_warn("No team sprint models were trained")
  men_ts_models <- NULL
  ladies_ts_models <- NULL
  men_ts_leg_importance <- rep(0.5, 2)  # Equal weights fallback
  ladies_ts_leg_importance <- rep(0.5, 2)  # Equal weights fallback
  men_ts_startlist <- NULL
  ladies_ts_startlist <- NULL
}

log_info("=== CHAMPIONSHIPS PROCESSING COMPLETE ===")
