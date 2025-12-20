# Final Climb Predictions: Points and Probability Modeling
# Based on tds-picks.R structure with Final Climb specific methodology
options(rstudio.viewer.max.columns = 0)
# Load required libraries
library(dplyr)
library(tidyr)
library(openxlsx)
library(arrow)
library(mgcv)
library(leaps)
library(logger)
library(purrr)
library(lubridate)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(slider)
library(xgboost)
library(readr)

# Define points systems
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
stage_points <- c(50,47,44,41,38,35,32,30,28,26,24,22,20,18,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
fc_points <- c(300,285,270,255,240,216,207,198,189,180,174,168,162,156,150,144,138,132,126,120,114,108,102,96,90,84,78,72,66,60,57,54,51,48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3)

# Function to replace NAs with first quartile value
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Function to safely fetch points based on Place
get_points <- function(place, points_list) {
  if (place >= 1 && place <= length(points_list)) {
    return(points_list[place])
  }
  return(0)
}

# Function to calculate weighted average for last 5 races
calc_weighted_last_5 <- function(places) {
  if(length(places) > 0) {
    weights <- seq(length(places), 1)
    return(weighted.mean(places, weights, na.rm = TRUE))
  } else {
    return(NA_real_)
  }
}

# Set up logging
log_dir <- "~/ski/elo/python/ski/polars/excel365/final-climb-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "final_climb_processing.log")))
log_info("Starting Final Climb predictions process")

# ============================================================================
# DATA GATHERING
# ============================================================================

log_info("=== DATA GATHERING ===")

# Read chronological data for both genders
log_info("Reading chronological data...")
men_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv", 
                       stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Distance != "Ts", Distance != "Rel",  # Exclude team events
         Event != "Offseason")  # Exclude offseason events

ladies_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv", 
                         stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Distance != "Ts", Distance != "Rel",  # Exclude team events
         Event != "Offseason")  # Exclude offseason events

log_info(paste("Loaded", nrow(men_chrono), "men's race records"))
log_info(paste("Loaded", nrow(ladies_chrono), "ladies' race records"))

# Read Final Climb startlists
log_info("Reading Final Climb startlists...")
men_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_races_men.csv", 
                         stringsAsFactors = FALSE)

men_startlist$Sex = "M"

ladies_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_races_ladies.csv", 
                            stringsAsFactors = FALSE)
ladies_startlist$Sex = "L"

log_info(paste("Loaded", nrow(men_startlist), "men in Final Climb startlist"))
log_info(paste("Loaded", nrow(ladies_startlist), "ladies in Final Climb startlist"))

# Create combined chronological data with proper points assignment
log_info("Assigning points based on race type...")

# First create Final_Climb flags based on criteria
log_info("Creating Final_Climb markers...")
men_df <- men_chrono %>%
  mutate(
    Final_Climb = case_when(
      Event == "Tour de Ski" & 
      City == "Val Di Fiemme" & 
      Distance != "Sprint" & 
      Technique == "F" ~ 1,
      TRUE ~ 0
    )
  )

ladies_df <- ladies_chrono %>%
  mutate(
    Final_Climb = case_when(
      Event == "Tour de Ski" & 
      City == "Val Di Fiemme" & 
      Distance != "Sprint" & 
      Technique == "F" ~ 1,
      TRUE ~ 0
    )
  )

# Log Final Climb identification statistics
fc_identified_men <- sum(men_df$Final_Climb == 1, na.rm = TRUE)
fc_identified_ladies <- sum(ladies_df$Final_Climb == 1, na.rm = TRUE)
log_info(paste("Identified", fc_identified_men, "Final Climb races in men's data"))
log_info(paste("Identified", fc_identified_ladies, "Final Climb races in ladies' data"))

# Now apply points logic - all races use stage points for consistency
men_df <- men_df %>%
  mutate(
    Points = map_int(Place, ~ get_points(.x, stage_points))
  )

ladies_df <- ladies_df %>%
  mutate(
    Points = map_int(Place, ~ get_points(.x, stage_points))
  )

log_info(paste("Applied Final Climb points system to", nrow(men_df), "men's records"))
log_info(paste("Applied Final Climb points system to", nrow(ladies_df), "ladies' records"))

# ============================================================================
# FEATURE ENGINEERING - LAST_5 AND LAST_5_2 COLUMNS
# ============================================================================

log_info("=== CREATING LAST_5 AND LAST_5_2 FEATURES ===")

# Function to calculate weighted average for last 5 races
calc_weighted_last_5 <- function(places) {
  if(length(places) > 0) {
    weights <- seq(1, length(places))
    return(weighted.mean(places, weights, na.rm = TRUE))
  } else {
    return(NA_real_)
  }
}

# Function to process dataframe and add Last_5 features
process_dataframe <- function(df) {
  df %>%
    group_by(ID) %>%
    arrange(Date) %>%
    mutate(
      # Sprint Classic Last 5
      Sprint_C_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        all_prev_tech <- Technique[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist == "Sprint" & all_prev_tech == "C")
        if(length(filtered_idx) > 0) {
          last_5 <- tail(all_prev_races[filtered_idx], 5)
          return(calc_weighted_last_5(last_5))
        } else {
          return(NA_real_)
        }
      }),
      
      # Sprint Freestyle Last 5
      Sprint_F_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        all_prev_tech <- Technique[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist == "Sprint" & all_prev_tech == "F")
        if(length(filtered_idx) > 0) {
          last_5 <- tail(all_prev_races[filtered_idx], 5)
          return(calc_weighted_last_5(last_5))
        } else {
          return(NA_real_)
        }
      }),
      
      # Distance Classic Last 5
      Distance_C_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        all_prev_tech <- Technique[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist != "Sprint" & all_prev_tech == "C")
        if(length(filtered_idx) > 0) {
          last_5 <- tail(all_prev_races[filtered_idx], 5)
          return(calc_weighted_last_5(last_5))
        } else {
          return(NA_real_)
        }
      }),
      
      # Distance Freestyle Last 5
      Distance_F_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        all_prev_tech <- Technique[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist != "Sprint" & all_prev_tech == "F")
        if(length(filtered_idx) > 0) {
          last_5 <- tail(all_prev_races[filtered_idx], 5)
          return(calc_weighted_last_5(last_5))
        } else {
          return(NA_real_)
        }
      }),
      
      # Overall Distance Last 5
      Distance_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist != "Sprint")
        if(length(filtered_idx) > 0) {
          last_5 <- tail(all_prev_races[filtered_idx], 5)
          return(calc_weighted_last_5(last_5))
        } else {
          return(NA_real_)
        }
      }),
      
      # Overall Sprint Last 5
      Sprint_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist == "Sprint")
        if(length(filtered_idx) > 0) {
          last_5 <- tail(all_prev_races[filtered_idx], 5)
          return(calc_weighted_last_5(last_5))
        } else {
          return(NA_real_)
        }
      }),
      
    ) %>%
    ungroup()
}

# Process both dataframes to add Last_5 features
log_info("Processing men's data for Last_5 features...")
men_df <- process_dataframe(men_df)

log_info("Processing ladies' data for Last_5 features...")
ladies_df <- process_dataframe(ladies_df)

# Add Last_5_2 features (second order features)
log_info("Creating Last_5_2 features...")

add_last_5_2_features <- function(df) {
  df %>%
    group_by(ID) %>%
    arrange(Date) %>%
    mutate(
      # Sprint Classic Last 5_2 (including current race)
      Sprint_C_Last_5_2 = sapply(row_number(), function(i) {
        all_races <- Points[1:i]
        all_dist <- Distance[1:i]
        all_tech <- Technique[1:i]
        
        filtered_idx <- which(all_dist == "Sprint" & all_tech == "C")
        filtered_points <- all_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      }),
      
      # Sprint Freestyle Last 5_2 (including current race)
      Sprint_F_Last_5_2 = sapply(row_number(), function(i) {
        all_races <- Points[1:i]
        all_dist <- Distance[1:i]
        all_tech <- Technique[1:i]
        
        filtered_idx <- which(all_dist == "Sprint" & all_tech == "F")
        filtered_points <- all_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      }),
      
      # Distance Classic Last 5_2 (including current race)
      Distance_C_Last_5_2 = sapply(row_number(), function(i) {
        all_races <- Points[1:i]
        all_dist <- Distance[1:i]
        all_tech <- Technique[1:i]
        
        filtered_idx <- which(all_dist != "Sprint" & all_tech == "C")
        filtered_points <- all_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      }),
      
      # Distance Freestyle Last 5_2 (including current race)
      Distance_F_Last_5_2 = sapply(row_number(), function(i) {
        all_races <- Points[1:i]
        all_dist <- Distance[1:i]
        all_tech <- Technique[1:i]
        
        filtered_idx <- which(all_dist != "Sprint" & all_tech == "F")
        filtered_points <- all_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      }),
      
      # Overall Distance Last 5_2 (including current race)
      Distance_Last_5_2 = sapply(row_number(), function(i) {
        all_races <- Points[1:i]
        all_dist <- Distance[1:i]
        
        filtered_idx <- which(all_dist != "Sprint")
        filtered_points <- all_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      }),
      
      # Overall Sprint Last 5_2 (including current race)
      Sprint_Last_5_2 = sapply(row_number(), function(i) {
        all_races <- Points[1:i]
        all_dist <- Distance[1:i]
        
        filtered_idx <- which(all_dist == "Sprint")
        filtered_points <- all_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      })
    ) %>%
    ungroup()
}

log_info("Processing men's data for Last_5_2 features...")
men_df <- add_last_5_2_features(men_df)


log_info("Processing ladies' data for Last_5_2 features...")
ladies_df <- add_last_5_2_features(ladies_df)

log_info("=== FEATURE ENGINEERING COMPLETE ===")

# Filter data for post-2007 seasons to ensure modern relevance (AFTER feature creation)
log_info("Filtering for post-2007 data...")
men_points_training <- men_df %>% filter(Season >= 2007)
ladies_points_training <- ladies_df %>% filter(Season >= 2007)

# Add Final_Climb markers to training data and altitude categories
men_points_training <- men_points_training %>%
  mutate(
    # Add altitude categories
    AltitudeCategory = ifelse(Elevation >= 1300, 1, 0)
  )

ladies_points_training <- ladies_points_training %>%
  mutate(
    # Add altitude categories  
    AltitudeCategory = ifelse(Elevation >= 1300, 1, 0)
  )

log_info(paste("Filtered to", nrow(men_points_training), "men's training records (2007+)"))
log_info(paste("Filtered to", nrow(ladies_points_training), "ladies' training records (2007+)"))

# Log Final Climb specific statistics
fc_men_stats <- men_points_training %>%
  filter(Final_Climb == 1) %>%
  summarise(
    races = n(),
    unique_athletes = n_distinct(ID),
    seasons = n_distinct(Season),
    avg_points = mean(Points, na.rm = TRUE)
  )

fc_ladies_stats <- ladies_points_training %>%
  filter(Final_Climb == 1) %>%
  summarise(
    races = n(),
    unique_athletes = n_distinct(ID),
    seasons = n_distinct(Season),
    avg_points = mean(Points, na.rm = TRUE)
  )

log_info("Final Climb historical data statistics:")
log_info(paste("Men - Races:", fc_men_stats$races, "| Athletes:", fc_men_stats$unique_athletes, 
              "| Seasons:", fc_men_stats$seasons, "| Avg points:", round(fc_men_stats$avg_points, 1)))
log_info(paste("Ladies - Races:", fc_ladies_stats$races, "| Athletes:", fc_ladies_stats$unique_athletes,
              "| Seasons:", fc_ladies_stats$seasons, "| Avg points:", round(fc_ladies_stats$avg_points, 1)))

# ============================================================================
# PELO PERCENTAGE CALCULATIONS
# ============================================================================

log_info("=== ELO/PELO PERCENTAGE CALCULATIONS ===")

# Function to create percentage columns relative to Season/Race maximums
create_percentage_columns <- function(df) {
  log_info("Calculating percentage columns relative to Season/Race maximums")
  
  # Define Elo and Pelo columns
  elo_cols <- c("Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo", 
                "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", 
                "Classic_Elo", "Freestyle_Elo")
  
  pelo_cols <- c("Pelo", "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                 "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo",
                 "Classic_Pelo", "Freestyle_Pelo")
  
  # Calculate max values by Season/Race for each column
  df_with_pct <- df
  
  # Process Elo columns
  for(col in elo_cols) {
    if(col %in% names(df)) {
      pct_col_name <- paste0(col, "_Pct")
      log_info(paste("Creating", pct_col_name))
      
      df_with_pct <- df_with_pct %>%
        group_by(Season, Race) %>%
        mutate(
          !!pct_col_name := ifelse(!is.na(get(col)) & !is.na(max(get(col), na.rm = TRUE)) & max(get(col), na.rm = TRUE) > 0,
                                   get(col) / max(get(col), na.rm = TRUE),
                                   NA_real_)
        ) %>%
        ungroup()
    }
  }
  
  # Process Pelo columns  
  for(col in pelo_cols) {
    if(col %in% names(df)) {
      pct_col_name <- paste0(col, "_Pct")
      log_info(paste("Creating", pct_col_name))
      
      df_with_pct <- df_with_pct %>%
        group_by(Season, Race) %>%
        mutate(
          !!pct_col_name := ifelse(!is.na(get(col)) & !is.na(max(get(col), na.rm = TRUE)) & max(get(col), na.rm = TRUE) > 0,
                                   get(col) / max(get(col), na.rm = TRUE),
                                   NA_real_)
        ) %>%
        ungroup()
    }
  }
  
  return(df_with_pct)
}

# Apply percentage calculations to both training datasets
log_info("Processing men's data for Elo/Pelo percentages...")
men_points_training <- create_percentage_columns(men_points_training)

log_info("Processing ladies' data for Elo/Pelo percentages...")
ladies_points_training <- create_percentage_columns(ladies_points_training)

log_info("=== ELO/PELO PERCENTAGE CALCULATIONS COMPLETE ===")

# ============================================================================
# NA IMPUTATION
# ============================================================================

log_info("=== NA IMPUTATION ===")

impute_features <- function(df) {
  log_info("Imputing NAs in features with first quartile values by Season/Race")
  
  # Define the columns to impute
  weighted_cols <- c("Sprint_C_Last_5", "Sprint_C_Last_5_2", "Distance_C_Last_5", "Distance_C_Last_5_2",
                     "Sprint_F_Last_5", "Sprint_F_Last_5_2", "Distance_F_Last_5", "Distance_F_Last_5_2",
                     "Distance_Last_5", "Distance_Last_5_2", "Sprint_Last_5", "Sprint_Last_5_2", "Overall_Last_5")
  
  elo_cols <- c("Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo", 
                "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", 
                "Classic_Elo", "Freestyle_Elo")
  
  pelo_cols <- c("Pelo", "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                 "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo",
                 "Classic_Pelo", "Freestyle_Pelo")
  
  # Add percentage columns (Elo_Pct, Pelo_Pct variants)
  elo_pct_cols <- paste0(elo_cols, "_Pct")
  pelo_pct_cols <- paste0(pelo_cols, "_Pct")
  
  # Combine all columns to impute
  all_cols_to_impute <- c(weighted_cols, elo_cols, pelo_cols, elo_pct_cols, pelo_pct_cols)
  
  df_imputed <- df
  
  for(col in all_cols_to_impute) {
    if(col %in% names(df_imputed)) {
      log_info(paste("Imputing", col))
      
      # Calculate first quartile by Season/Race for this column
      quartiles <- df_imputed %>%
        filter(!is.na(get(col))) %>%
        group_by(Season, Race) %>%
        summarise(
          quartile_value = quantile(get(col), 0.25, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Join back and impute NAs
      df_imputed <- df_imputed %>%
        left_join(quartiles, by = c("Season", "Race")) %>%
        mutate(
          !!col := ifelse(is.na(get(col)), quartile_value, get(col))
        ) %>%
        select(-quartile_value)
      
      # Log imputation statistics
      na_count_before <- sum(is.na(df[[col]]))
      na_count_after <- sum(is.na(df_imputed[[col]]))
      log_info(paste("  ", col, "- NAs before:", na_count_before, "after:", na_count_after))
    }
  }
  
  return(df_imputed)
}

# Apply imputation to both datasets
log_info("Applying imputation to men's training data...")
men_points_training <- impute_features(men_points_training)

log_info("Applying imputation to ladies' training data...")
ladies_points_training <- impute_features(ladies_points_training)

log_info("=== NA IMPUTATION COMPLETE ===")

# ============================================================================
# FILTER TO FINAL CLIMB RACES ONLY
# ============================================================================

log_info("=== FILTERING TO FINAL CLIMB RACES ONLY ===")

# Filter training data to only Final Climb races
men_fc_training <- men_points_training %>%
  filter(Final_Climb == 1)

ladies_fc_training <- ladies_points_training %>%
  filter(Final_Climb == 1)

log_info(paste("Filtered to", nrow(men_fc_training), "men's Final Climb training records"))
log_info(paste("Filtered to", nrow(ladies_fc_training), "ladies' Final Climb training records"))

if(nrow(men_fc_training) == 0) {
  log_info("WARNING: No men's Final Climb training data available")
}

if(nrow(ladies_fc_training) == 0) {
  log_info("WARNING: No ladies' Final Climb training data available")
}

# ============================================================================
# FC_PRED FEATURE SELECTION AND MODELING
# ============================================================================

log_info("=== FC_PRED FEATURE SELECTION AND MODELING ===")

# Define FC_pred features (for Final Climb prediction)
fc_pred_features <- c("Distance_F_Pelo_Pct", "Distance_F_Last_5")

log_info(paste("FC_pred features:", paste(fc_pred_features, collapse=", ")))

# Function to perform feature selection for FC_pred
fc_feature_selection <- function(training_data, gender) {
  log_info(paste("Performing FC_pred feature selection for", gender))
  
  # Check feature availability
  available_features <- fc_pred_features[fc_pred_features %in% names(training_data)]
  missing_features <- setdiff(fc_pred_features, available_features)
  
  if(length(missing_features) > 0) {
    log_info(paste("Missing features for", gender, ":", paste(missing_features, collapse=", ")))
  }
  
  if(length(available_features) == 0) {
    log_info(paste("No features available for", gender, "FC_pred model"))
    return(NULL)
  }
  
  log_info(paste("Available features for", gender, ":", paste(available_features, collapse=", ")))
  
  # Remove rows with missing data in key features
  complete_data <- training_data %>%
    filter(complete.cases(.[available_features]) & !is.na(Points))
  
  log_info(paste("Complete cases for", gender, ":", nrow(complete_data), "of", nrow(training_data), "total"))
  
  if(nrow(complete_data) < 10) {
    log_info(paste("Insufficient data for", gender, "FC_pred model"))
    return(NULL)
  }
  
  # Use exhaustive feature selection with regsubsets
  log_info(paste("Running exhaustive feature selection for", gender))
  
  # Create formula
  formula_str <- paste("Points ~", paste(available_features, collapse = " + "))
  log_info(paste("Formula:", formula_str))
  
  # Perform exhaustive search
  subset_search <- regsubsets(as.formula(formula_str),
                            data = complete_data,
                            nvmax = length(available_features),
                            method = "exhaustive",
                            really.big = TRUE)
  
  # Get BIC for all models
  subset_summary <- summary(subset_search)
  bic_scores <- subset_summary$bic
  
  # Find best model by BIC
  best_model_idx <- which.min(bic_scores)
  best_vars <- subset_summary$which[best_model_idx, ]
  selected_vars <- names(best_vars)[best_vars][-1]  # Remove intercept
  
  # Log results
  log_info(paste("Best FC_pred model for", gender, "with", length(selected_vars), "variables"))
  log_info(paste("Selected variables:", paste(selected_vars, collapse=", ")))
  log_info(paste("BIC score:", min(bic_scores)))
  
  return(list(
    selected_vars = selected_vars,
    training_data = complete_data,
    bic_score = min(bic_scores),
    formula = as.formula(paste("Points ~", paste(selected_vars, collapse = " + ")))
  ))
}

# Perform feature selection for both genders
men_fc_selection <- fc_feature_selection(men_fc_training, "men")
ladies_fc_selection <- fc_feature_selection(ladies_fc_training, "ladies")

log_info("Performing training data quality checks...")

# Check feature completeness
check_feature_completeness <- function(data, gender) {
  feature_cols <- c("Sprint_C_Last_5", "Sprint_F_Last_5", "Distance_C_Last_5", 
                    "Distance_F_Last_5", "Sprint_Last_5", "Distance_Last_5", "Overall_Last_5")
  
  for(col in feature_cols) {
    if(col %in% names(data)) {
      na_count <- sum(is.na(data[[col]]))
      total_count <- nrow(data)
      log_info(paste(gender, col, "NA count:", na_count, "/", total_count, 
                    paste0("(", round(100*na_count/total_count, 1), "%)")))
    }
  }
}

check_feature_completeness(men_points_training, "Men's")
check_feature_completeness(ladies_points_training, "Ladies'")

# FC_pred GAM modeling based on selected features
log_info("Training FC_pred GAM models...")

fc_pred_models <- list()

# Train GAM models for both genders if feature selection was successful
if(!is.null(men_fc_selection)) {
  log_info("Training men's FC_pred GAM model...")
  
  # Create formula using selected features
  men_fc_formula <- men_fc_selection$formula
  log_info(paste("Men's FC_pred formula:", deparse(men_fc_formula)))
  
  # Train GAM model
  men_fc_model <- tryCatch({
    gam(men_fc_formula, data = men_fc_selection$training_data, method = "REML")
  }, error = function(e) {
    log_info(paste("Error training men's FC_pred model:", e$message))
    return(NULL)
  })
  
  if(!is.null(men_fc_model)) {
    fc_pred_models$men <- men_fc_model
    log_info("Men's FC_pred GAM model trained successfully")
    log_info(paste("Model summary - AIC:", AIC(men_fc_model), "R-squared:", summary(men_fc_model)$r.sq))
  }
}

if(!is.null(ladies_fc_selection)) {
  log_info("Training ladies' FC_pred GAM model...")
  
  # Create formula using selected features
  ladies_fc_formula <- ladies_fc_selection$formula
  log_info(paste("Ladies' FC_pred formula:", deparse(ladies_fc_formula)))
  
  # Train GAM model
  ladies_fc_model <- tryCatch({
    gam(ladies_fc_formula, data = ladies_fc_selection$training_data, method = "REML")
  }, error = function(e) {
    log_info(paste("Error training ladies' FC_pred model:", e$message))
    return(NULL)
  })
  
  if(!is.null(ladies_fc_model)) {
    fc_pred_models$ladies <- ladies_fc_model
    log_info("Ladies' FC_pred GAM model trained successfully")
    log_info(paste("Model summary - AIC:", AIC(ladies_fc_model), "R-squared:", summary(ladies_fc_model)$r.sq))
  }
}

# Function to create FC_pred column for training data
create_fc_pred_column <- function(df, model, gender) {
  log_info(paste("Creating FC_pred column for", gender))
  
  if(is.null(model)) {
    log_info(paste("No FC_pred model available for", gender, "- setting FC_pred to NA"))
    df$FC_pred <- NA_real_
    return(df)
  }
  
  # Create predictions for all rows (will be NA where features are missing)
  df$FC_pred <- tryCatch({
    predict(model, newdata = df, type = "response")
  }, error = function(e) {
    log_info(paste("Error creating FC_pred for", gender, ":", e$message))
    return(rep(NA_real_, nrow(df)))
  })
  
  # Log statistics
  fc_pred_stats <- df %>%
    filter(!is.na(FC_pred)) %>%
    summarise(
      count = n(),
      mean = mean(FC_pred, na.rm = TRUE),
      median = median(FC_pred, na.rm = TRUE),
      min = min(FC_pred, na.rm = TRUE),
      max = max(FC_pred, na.rm = TRUE)
    )
  
  log_info(paste("FC_pred stats for", gender, ":"))
  log_info(paste("  Valid predictions:", fc_pred_stats$count, "of", nrow(df)))
  log_info(paste("  Mean:", round(fc_pred_stats$mean, 2)))
  log_info(paste("  Range:", round(fc_pred_stats$min, 2), "-", round(fc_pred_stats$max, 2)))
  
  return(df)
}

# Add FC_pred column to training datasets
log_info("Adding FC_pred column to training datasets...")

men_points_training <- create_fc_pred_column(men_points_training, fc_pred_models$men, "men")
ladies_points_training <- create_fc_pred_column(ladies_points_training, fc_pred_models$ladies, "ladies")

log_info("=== FC_PRED MODELING COMPLETE ===")

# ============================================================================
# FC_LAST_5 AND FC_LAST_5_2 FEATURE CREATION
# ============================================================================

log_info("=== CREATING FC_LAST_5 AND FC_LAST_5_2 FEATURES ===")

# Function to create FC_Last_5 and FC_Last_5_2 features
create_fc_weighted_features <- function(df) {
  log_info("Creating FC_Last_5 and FC_Last_5_2 weighted features")
  
  df %>%
    group_by(ID) %>%
    arrange(Date) %>%
    mutate(
      # FC_Last_5: Excludes current row, uses up to last 3 actual + predicted fill
      FC_Last_5 = sapply(row_number(), function(i) {
        current_fc_pred <- FC_pred[i]
        
        # If no FC_pred available, return NA
        if(is.na(current_fc_pred)) return(NA_real_)
        
        # Get all previous Final Climb races for this skier (empty for first row)
        if(i == 1) {
          prev_fc_indices <- integer(0)
        } else {
          prev_fc_indices <- which(Final_Climb[1:(i-1)] == 1)
        }
        prev_fc_points <- Points[prev_fc_indices]
        
        # Take up to last 3 actual FC results
        actual_fc <- tail(prev_fc_points, 3)
        
        # Create the 5-value weighted average
        # Order: most recent gets weight 5, then 4, 3, 2, 1
        if(length(actual_fc) >= 3) {
          # Use last 3 actual + 2 predicted
          # Weight 5: most recent actual, Weight 4: 2nd recent, Weight 3: 3rd recent, Weight 2&1: predicted
          values <- c(actual_fc[3], actual_fc[2], actual_fc[1], current_fc_pred, current_fc_pred)
        } else if(length(actual_fc) == 2) {
          # Use 2 actual + 3 predicted
          # Weight 5: most recent actual, Weight 4: 2nd recent, Weight 3,2,1: predicted
          values <- c(actual_fc[2], actual_fc[1], current_fc_pred, current_fc_pred, current_fc_pred)
        } else if(length(actual_fc) == 1) {
          # Use 1 actual + 4 predicted  
          # Weight 5: most recent actual, Weight 4,3,2,1: predicted
          values <- c(actual_fc[1], current_fc_pred, current_fc_pred, current_fc_pred, current_fc_pred)
        } else {
          # Use all predicted (includes first row case)
          values <- rep(current_fc_pred, 5)
        }
        
        # Apply weights 5, 4, 3, 2, 1 and calculate weighted average
        weights <- c(5, 4, 3, 2, 1)
        weighted.mean(values, weights, na.rm = TRUE)
      }),
      
      # FC_Last_5_2: Includes current row, uses current + up to last 2 actual + predicted fill
      FC_Last_5_2 = sapply(row_number(), function(i) {
        current_fc_pred <- FC_pred[i]
        
        # If no FC_pred available, return NA
        if(is.na(current_fc_pred)) return(NA_real_)
        
        # If current row is not a Final Climb, use predicted value for current
        if(Final_Climb[i] == 1) {
          current_points <- Points[i]
        } else {
          current_points <- current_fc_pred
        }
        
        # Get previous Final Climb races for this skier
        if(i == 1) {
          prev_fc_indices <- integer(0)
        } else {
          prev_fc_indices <- which(Final_Climb[1:(i-1)] == 1)
        }
        prev_fc_points <- Points[prev_fc_indices]
        
        # Take up to last 2 actual FC results
        actual_fc <- tail(prev_fc_points, 2)
        
        # Create the 5-value weighted average
        # Order: most recent gets weight 5, then 4, 3, 2, 1
        if(length(actual_fc) >= 2) {
          # Use current + last 2 actual + 2 predicted
          # Weight 5: current, Weight 4: most recent actual, Weight 3: 2nd recent actual, Weight 2&1: predicted
          values <- c(current_points, actual_fc[2], actual_fc[1], current_fc_pred, current_fc_pred)
        } else if(length(actual_fc) == 1) {
          # Use current + 1 actual + 3 predicted
          # Weight 5: current, Weight 4: most recent actual, Weight 3,2,1: predicted
          values <- c(current_points, actual_fc[1], current_fc_pred, current_fc_pred, current_fc_pred)
        } else {
          # Use current + 4 predicted
          values <- c(current_points, current_fc_pred, current_fc_pred, current_fc_pred, current_fc_pred)
        }
        
        # Apply weights 5, 4, 3, 2, 1 and calculate weighted average
        weights <- c(5, 4, 3, 2, 1)
        weighted.mean(values, weights, na.rm = TRUE)
      })
    ) %>%
    ungroup()
}

# Apply FC weighted features to both datasets
log_info("Processing men's FC weighted features...")
men_points_training <- create_fc_weighted_features(men_points_training)

log_info("Processing ladies' FC weighted features...")
ladies_points_training <- create_fc_weighted_features(ladies_points_training)

# Log statistics for the new features
log_fc_stats <- function(df, gender) {
  fc_last_5_stats <- df %>%
    filter(!is.na(FC_Last_5)) %>%
    summarise(
      count = n(),
      mean = mean(FC_Last_5, na.rm = TRUE),
      median = median(FC_Last_5, na.rm = TRUE),
      min = min(FC_Last_5, na.rm = TRUE),
      max = max(FC_Last_5, na.rm = TRUE)
    )
  
  fc_last_5_2_stats <- df %>%
    filter(!is.na(FC_Last_5_2)) %>%
    summarise(
      count = n(),
      mean = mean(FC_Last_5_2, na.rm = TRUE),
      median = median(FC_Last_5_2, na.rm = TRUE),
      min = min(FC_Last_5_2, na.rm = TRUE),
      max = max(FC_Last_5_2, na.rm = TRUE)
    )
  
  log_info(paste("FC_Last_5 stats for", gender, ":"))
  log_info(paste("  Valid values:", fc_last_5_stats$count, "of", nrow(df)))
  log_info(paste("  Mean:", round(fc_last_5_stats$mean, 2)))
  log_info(paste("  Range:", round(fc_last_5_stats$min, 2), "-", round(fc_last_5_stats$max, 2)))
  
  log_info(paste("FC_Last_5_2 stats for", gender, ":"))
  log_info(paste("  Valid values:", fc_last_5_2_stats$count, "of", nrow(df)))
  log_info(paste("  Mean:", round(fc_last_5_2_stats$mean, 2)))
  log_info(paste("  Range:", round(fc_last_5_2_stats$min, 2), "-", round(fc_last_5_2_stats$max, 2)))
}

log_fc_stats(men_points_training, "men")
log_fc_stats(ladies_points_training, "ladies")

log_info("=== FC_LAST_5 AND FC_LAST_5_2 FEATURE CREATION COMPLETE ===")

# ============================================================================
# FINAL CLIMB FEATURE SELECTION WITH FC_LAST_5
# ============================================================================

log_info("=== FINAL CLIMB FEATURE SELECTION ===")

# Define Final Climb features (including FC_Last_5 as an explanatory variable)
fc_features <- c("Distance_F_Pelo_Pct", "Distance_F_Last_5", "FC_Last_5")

log_info(paste("Final Climb features:", paste(fc_features, collapse=", ")))

# Function to perform feature selection for Final Climb prediction
fc_final_feature_selection <- function(training_data, gender) {
  log_info(paste("Performing Final Climb feature selection for", gender))
  
  # Check feature availability
  available_features <- fc_features[fc_features %in% names(training_data)]
  missing_features <- setdiff(fc_features, available_features)
  
  if(length(missing_features) > 0) {
    log_info(paste("Missing features for", gender, ":", paste(missing_features, collapse=", ")))
  }
  
  if(length(available_features) == 0) {
    log_info(paste("No features available for", gender, "Final Climb model"))
    return(NULL)
  }
  
  log_info(paste("Available features for", gender, ":", paste(available_features, collapse=", ")))
  
  # Remove rows with missing data in key features
  complete_data <- training_data %>%
    filter(complete.cases(.[available_features]) & !is.na(Points))
  
  log_info(paste("Complete cases for", gender, ":", nrow(complete_data), "of", nrow(training_data), "total"))
  
  if(nrow(complete_data) < 10) {
    log_info(paste("Insufficient data for", gender, "Final Climb model"))
    return(NULL)
  }
  
  # Use exhaustive feature selection with regsubsets
  log_info(paste("Running exhaustive feature selection for", gender))
  
  # Create formula
  formula_str <- paste("Points ~", paste(available_features, collapse = " + "))
  log_info(paste("Formula:", formula_str))
  
  # Perform exhaustive search
  subset_search <- regsubsets(as.formula(formula_str),
                            data = complete_data,
                            nvmax = length(available_features),
                            method = "exhaustive",
                            really.big = TRUE)
  
  # Get BIC for all models
  subset_summary <- summary(subset_search)
  bic_scores <- subset_summary$bic
  
  # Find best model by BIC
  best_model_idx <- which.min(bic_scores)
  best_vars <- subset_summary$which[best_model_idx, ]
  selected_vars <- names(best_vars)[best_vars][-1]  # Remove intercept
  
  # Log results
  log_info(paste("Best Final Climb model for", gender, "with", length(selected_vars), "variables"))
  log_info(paste("Selected variables:", paste(selected_vars, collapse=", ")))
  log_info(paste("BIC score:", min(bic_scores)))
  
  return(list(
    selected_vars = selected_vars,
    training_data = complete_data,
    bic_score = min(bic_scores),
    formula = as.formula(paste("Points ~", paste(selected_vars, collapse = " + ")))
  ))
}

# Create Final Climb datasets with FC_Last_5 features
men_fc_training_with_features <- men_points_training %>% filter(Final_Climb == 1)
ladies_fc_training_with_features <- ladies_points_training %>% filter(Final_Climb == 1)

log_info(paste("Men's FC training with features:", nrow(men_fc_training_with_features), "races"))
log_info(paste("Ladies' FC training with features:", nrow(ladies_fc_training_with_features), "races"))

# Perform feature selection for both genders using Final Climb races with FC_Last_5
men_fc_final_selection <- fc_final_feature_selection(men_fc_training_with_features, "men")
ladies_fc_final_selection <- fc_final_feature_selection(ladies_fc_training_with_features, "ladies")

log_info("=== FINAL CLIMB FEATURE SELECTION COMPLETE ===")

# ============================================================================
# FINAL CLIMB GAM MODELING
# ============================================================================

log_info("=== FINAL CLIMB GAM MODELING ===")

fc_final_models <- list()

# Train Final Climb GAM models for both genders if feature selection was successful
if(!is.null(men_fc_final_selection)) {
  log_info("Training men's Final Climb GAM model...")
  
  # Create formula using selected features
  men_fc_final_formula <- men_fc_final_selection$formula
  log_info(paste("Men's Final Climb formula:", deparse(men_fc_final_formula)))
  
  # Train GAM model
  men_fc_final_model <- tryCatch({
    gam(men_fc_final_formula, data = men_fc_final_selection$training_data, method = "REML")
  }, error = function(e) {
    log_info(paste("Error training men's Final Climb model:", e$message))
    return(NULL)
  })
  
  if(!is.null(men_fc_final_model)) {
    fc_final_models$men <- men_fc_final_model
    log_info("Men's Final Climb GAM model trained successfully")
    log_info(paste("Model summary - AIC:", AIC(men_fc_final_model), "R-squared:", summary(men_fc_final_model)$r.sq))
    
    # Log model coefficients
    coef_summary <- summary(men_fc_final_model)
    if(length(coef_summary$p.coeff) > 0) {
      log_info("Men's Final Climb model coefficients:")
      for(i in seq_along(coef_summary$p.coeff)) {
        coef_name <- names(coef_summary$p.coeff)[i]
        coef_value <- coef_summary$p.coeff[i]
        coef_pvalue <- coef_summary$p.pv[i]
        log_info(paste("  ", coef_name, ":", round(coef_value, 4), "(p =", round(coef_pvalue, 4), ")"))
      }
    }
  }
} else {
  log_info("No feature selection results available for men's Final Climb model")
}


log_info("=== FINAL CLIMB GAM MODELING COMPLETE ===")

# ============================================================================
# STARTLIST PREPARATION - PELO_PCT COLUMNS
# ============================================================================

log_info("=== STARTLIST PREPARATION ===")

# Function to create Pelo_Pct columns from Elo columns for startlist
create_startlist_pelo_pct <- function(startlist_df, training_df, gender) {
  log_info(paste("Creating Pelo_Pct columns for", gender, "startlist"))
  
  # Define Elo columns that should be converted to Pelo_Pct
  elo_cols <- c("Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo", 
                "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", 
                "Classic_Elo", "Freestyle_Elo")
  
  startlist_processed <- startlist_df
  
  # Convert Elo columns to Pelo_Pct columns using max from startlist
  for(col in elo_cols) {
    if(col %in% names(startlist_processed)) {
      # Create Pelo_Pct column name (replace "Elo" with "Pelo_Pct")
      pct_col <- gsub("_Elo$", "_Pelo_Pct", col)
      if(col == "Elo") pct_col <- "Pelo_Pct"  # Handle base "Elo" case
      
      # Find max value in the startlist for this column
      max_val <- max(startlist_processed[[col]], na.rm = TRUE)
      
      if(!is.na(max_val) && max_val > 0) {
        # Create percentage column
        startlist_processed[[pct_col]] <- ifelse(
          !is.na(startlist_processed[[col]]),
          startlist_processed[[col]] / max_val,
          NA_real_
        )
        
        log_info(paste("Created", pct_col, "from", col, "using startlist max value:", round(max_val, 2)))
      } else {
        log_info(paste("No valid max found for", col, "in startlist"))
        startlist_processed[[pct_col]] <- NA_real_
      }
    }
  }
  
  # Also handle existing Pelo columns if they exist (convert to Pelo_Pct)
  pelo_cols <- c("Pelo", "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                 "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo",
                 "Classic_Pelo", "Freestyle_Pelo")
  
  for(col in pelo_cols) {
    if(col %in% names(startlist_processed)) {
      pct_col <- paste0(col, "_Pct")
      
      # Find max value in the startlist for this column
      max_val <- max(startlist_processed[[col]], na.rm = TRUE)
      
      if(!is.na(max_val) && max_val > 0) {
        # Create percentage column
        startlist_processed[[pct_col]] <- ifelse(
          !is.na(startlist_processed[[col]]),
          startlist_processed[[col]] / max_val,
          NA_real_
        )
        
        log_info(paste("Created", pct_col, "using startlist max value:", round(max_val, 2)))
      } else {
        log_info(paste("No valid max found for", col, "in startlist"))
        startlist_processed[[pct_col]] <- NA_real_
      }
    }
  }
  
  # Log statistics on created columns
  pct_cols_created <- names(startlist_processed)[grepl("_Pct$", names(startlist_processed))]
  log_info(paste("Created", length(pct_cols_created), "Pelo_Pct columns for", gender))
  
  return(startlist_processed)
}

# Apply Pelo_Pct creation to both startlists
log_info("Processing men's startlist Pelo_Pct columns...")
men_startlist <- create_startlist_pelo_pct(men_startlist, men_points_training, "men")
print(names(men_startlist))
log_info("Processing ladies' startlist Pelo_Pct columns...")
ladies_startlist <- create_startlist_pelo_pct(ladies_startlist, ladies_points_training, "ladies")

log_info("=== STARTLIST PELO_PCT CREATION COMPLETE ===")

# ============================================================================
# STARTLIST PREPARATION - LAST_5 FEATURES  
# ============================================================================

log_info("=== ADDING LAST_5 FEATURES TO STARTLIST ===")

# Function to add all latest Last_5 features to startlist from training data
add_latest_basic_last_5_features <- function(startlist_df, training_df, gender) {
  log_info(paste("Adding all latest Last_5_2 features for", gender, "startlist"))
  
  # Define all Last_5_2 features and their corresponding Last_5 names
  last_5_2_features <- c("Distance_Last_5_2", "Sprint_Last_5_2", "Distance_C_Last_5_2", 
                         "Distance_F_Last_5_2", "Sprint_C_Last_5_2", "Sprint_F_Last_5_2")
  last_5_names <- c("Distance_Last_5", "Sprint_Last_5", "Distance_C_Last_5", 
                    "Distance_F_Last_5", "Sprint_C_Last_5", "Sprint_F_Last_5")
  
  # Check which features are available in the training data
  available_features <- intersect(last_5_2_features, names(training_df))
  log_info(paste("Available Last_5_2 features:", paste(available_features, collapse = ", ")))
  
  if(length(available_features) == 0) {
    log_info("No Last_5_2 features found in training data")
    return(startlist_df)
  }
  
  # Get the most recent Last_5_2 values for available features
  select_cols <- c("ID", available_features)
  latest_basic_features <- training_df %>%
    group_by(ID) %>%
    arrange(Date) %>%
    slice_tail(n = 1) %>%
    select(all_of(select_cols)) %>%
    ungroup()
  
  # Create temporary column names to avoid conflicts
  temp_names <- paste0(available_features, "_new")
  names(latest_basic_features)[2:ncol(latest_basic_features)] <- temp_names

  log_info(paste("Found latest basic features for", nrow(latest_basic_features), "athletes"))
  
  # Remove existing Last_5 columns that correspond to available features
  corresponding_last_5 <- last_5_names[last_5_2_features %in% available_features]
  startlist_df <- startlist_df %>%
    select(-any_of(corresponding_last_5))

  # Merge with startlist using temporary column names
  startlist_with_features <- startlist_df %>%
    left_join(latest_basic_features, by = "ID")
  
  # Rename temporary columns to final Last_5 names
  for(i in seq_along(available_features)) {
    temp_name <- temp_names[i]
    final_name <- last_5_names[last_5_2_features == available_features[i]]
    if(temp_name %in% names(startlist_with_features)) {
      names(startlist_with_features)[names(startlist_with_features) == temp_name] <- final_name
    }
  }

  # Log statistics for all features
  for(feature in corresponding_last_5) {
    if(feature %in% names(startlist_with_features)) {
      matched_count <- sum(!is.na(startlist_with_features[[feature]]))
      log_info(paste("  ", feature, "- Matched:", matched_count, "of", nrow(startlist_with_features), "athletes"))
    }
  }
  
  return(startlist_with_features)
}

# Apply Last_5 addition to both startlists
log_info("Processing men's startlist Last_5...")
men_startlist <- add_latest_basic_last_5_features(men_startlist, men_points_training, "men")
print(names(men_startlist))
log_info("Processing ladies' startlist Last_5...")
ladies_startlist <- add_latest_basic_last_5_features(ladies_startlist, ladies_points_training, "ladies")

log_info("=== STARTLIST LAST_5 ADDITION COMPLETE ===")

# ============================================================================
# STARTLIST PREPARATION - FC_PRED PREDICTIONS
# ============================================================================

log_info("=== ADDING FC_PRED TO STARTLIST ===")

# Function to add FC_pred to startlist using trained models
add_fc_pred_to_startlist <- function(startlist_df, fc_pred_model, gender) {
  log_info(paste("Adding FC_pred to", gender, "startlist"))
  
  if(is.null(fc_pred_model)) {
    log_info(paste("No FC_pred model available for", gender, "- setting FC_pred to first quartile"))
    startlist_df$FC_pred <- quantile(c(stage_points), 0.25, na.rm = TRUE)  # Use first quartile as fallback
    return(startlist_df)
  }
  
  # Predict FC_pred for startlist using Distance_F_Pelo_Pct and Last_5
  startlist_df$FC_pred <- tryCatch({
    predict(fc_pred_model, newdata = startlist_df, type = "response")
  }, error = function(e) {
    log_info(paste("Error predicting FC_pred for", gender, ":", e$message))
    quantile(c(stage_points), 0.25, na.rm = TRUE)  # Use first quartile as fallback
  })
  
  # Handle any NAs with first quartile value
  na_count <- sum(is.na(startlist_df$FC_pred))
  if(na_count > 0) {
    q1_fc_pred <- quantile(startlist_df$FC_pred, 0.25, na.rm = TRUE)
    if(is.na(q1_fc_pred)) q1_fc_pred <- quantile(c(stage_points), 0.25, na.rm = TRUE)
    
    startlist_df$FC_pred[is.na(startlist_df$FC_pred)] <- q1_fc_pred
    log_info(paste("Imputed", na_count, "missing FC_pred values with first quartile:", round(q1_fc_pred, 2)))
  }
  
  log_info(paste("FC_pred stats for", gender, "startlist:"))
  log_info(paste("  Mean:", round(mean(startlist_df$FC_pred, na.rm = TRUE), 2)))
  log_info(paste("  Range:", round(min(startlist_df$FC_pred, na.rm = TRUE), 2), "-", 
               round(max(startlist_df$FC_pred, na.rm = TRUE), 2)))
  
  return(startlist_df)
}

# Apply FC_pred addition to both startlists using the trained FC_pred models
log_info("Processing men's startlist FC_pred...")
men_startlist <- add_fc_pred_to_startlist(men_startlist, fc_pred_models$men, "men")
print(names(men_startlist))
log_info("Processing ladies' startlist FC_pred...")
ladies_startlist <- add_fc_pred_to_startlist(ladies_startlist, fc_pred_models$ladies, "ladies")

log_info("=== STARTLIST FC_PRED ADDITION COMPLETE ===")

# ============================================================================
# STARTLIST PREPARATION - FC_LAST_5 FEATURES
# ============================================================================

log_info("=== ADDING FC_LAST_5 TO STARTLIST ===")

# Function to add FC_Last_5 features to startlist using recent training data
add_fc_last_5_to_startlist <- function(startlist_df, training_df, gender) {
  log_info(paste("Adding FC_Last_5 features to", gender, "startlist"))
  
  # Check if required columns exist
  if(!"ID" %in% names(startlist_df)) {
    log_info(paste("ID column not found in", gender, "startlist. Available columns:", paste(names(startlist_df), collapse=", ")))
    # Add a default FC_Last_5 column if ID is missing
    startlist_df$FC_Last_5 <- quantile(c(stage_points), 0.25, na.rm = TRUE)
    return(startlist_df)
  }
  
  if(!"FC_pred" %in% names(startlist_df)) {
    log_info(paste("FC_pred column not found in", gender, "startlist"))
    startlist_df$FC_pred <- quantile(c(stage_points), 0.25, na.rm = TRUE)
  }
  
  # Create FC_Last_5 for each skier
  fc_last_5_values <- sapply(1:nrow(startlist_df), function(i) {
    current_id <- startlist_df$ID[i]
    current_fc_pred <- startlist_df$FC_pred[i]
    
    # Get this skier's training history
    skier_history <- training_df %>%
      filter(ID == current_id) %>%
      arrange(Date)
    
    if(nrow(skier_history) == 0) {
      # No history - use FC_pred only
      return(weighted.mean(rep(current_fc_pred, 5), c(5,4,3,2,1)))
    } else {
      # Get Final Climb history
      fc_races <- skier_history %>% filter(Final_Climb == 1)
      fc_points <- fc_races$Points
      
      # Take up to last 3 actual FC results
      actual_fc <- tail(fc_points, 3)
      
      # Create weighted average as in training
      if(length(actual_fc) >= 3) {
        values <- c(actual_fc[3], actual_fc[2], actual_fc[1], current_fc_pred, current_fc_pred)
      } else if(length(actual_fc) == 2) {
        values <- c(actual_fc[2], actual_fc[1], current_fc_pred, current_fc_pred, current_fc_pred)
      } else if(length(actual_fc) == 1) {
        values <- c(actual_fc[1], current_fc_pred, current_fc_pred, current_fc_pred, current_fc_pred)
      } else {
        values <- rep(current_fc_pred, 5)
      }
      
      weights <- c(5, 4, 3, 2, 1)
      return(weighted.mean(values, weights, na.rm = TRUE))
    }
  })
  
  # Add FC_Last_5 column to startlist
  startlist_df$FC_Last_5 <- fc_last_5_values
  
  log_info(paste("FC_Last_5 features added for", nrow(startlist_df), gender, "skiers"))
  
  return(startlist_df)
}

# Apply FC_Last_5 addition to both startlists
log_info("Processing men's startlist FC_Last_5...")
men_startlist <- add_fc_last_5_to_startlist(men_startlist, men_points_training, "men")

log_info("Processing ladies' startlist FC_Last_5...")
ladies_startlist <- add_fc_last_5_to_startlist(ladies_startlist, ladies_points_training, "ladies")

log_info("=== STARTLIST FC_LAST_5 ADDITION COMPLETE ===")

# ============================================================================
# FINAL CLIMB PREDICTIONS
# ============================================================================

log_info("=== GENERATING FINAL CLIMB PREDICTIONS ===")

# Function to generate Final Climb predictions using trained models
generate_fc_predictions <- function(startlist_df, fc_model, gender) {
  log_info(paste("Generating Final Climb predictions for", gender))
  
  if(is.null(fc_model)) {
    log_info(paste("No Final Climb model available for", gender, "- using FC_pred as fallback"))
    startlist_df$Points_Pred <- startlist_df$FC_pred
    return(startlist_df)
  }
  
  # Generate points predictions using Distance_F_Pelo_Pct, Last_5, and FC_Last_5
  startlist_df$Points_Pred <- tryCatch({
    predict(fc_model, newdata = startlist_df, type = "response")
  }, error = function(e) {
    log_info(paste("Error predicting points for", gender, ":", e$message))
    return(startlist_df$FC_pred)  # Fallback to FC_pred
  })
  
  # Handle any NAs with first quartile value
  na_count <- sum(is.na(startlist_df$Points_Pred))
  if(na_count > 0) {
    q1_pred <- quantile(startlist_df$Points_Pred, 0.25, na.rm = TRUE)
    if(is.na(q1_pred)) q1_pred <- quantile(startlist_df$FC_pred, 0.25, na.rm = TRUE)
    
    startlist_df$Points_Pred[is.na(startlist_df$Points_Pred)] <- q1_pred
    log_info(paste("Imputed", na_count, "missing predictions with first quartile:", round(q1_pred, 2)))
  }
  
  log_info(paste("Final Climb predictions for", gender, ":"))
  log_info(paste("  Points range:", round(min(startlist_df$Points_Pred), 2), "-", 
               round(max(startlist_df$Points_Pred), 2)))
  
  return(startlist_df)
}

# Generate predictions for both genders using the trained Final Climb models
log_info("Processing men's Final Climb predictions...")
men_startlist <- generate_fc_predictions(men_startlist, fc_final_models$men, "men")

log_info("Processing ladies' Final Climb predictions...")
ladies_startlist <- generate_fc_predictions(ladies_startlist, fc_final_models$ladies, "ladies")

log_info("=== FINAL CLIMB PREDICTION GENERATION COMPLETE ===")

# ============================================================================
# POINTS PREDICTION NORMALIZATION
# ============================================================================

log_info("=== NORMALIZING POINTS PREDICTIONS ===")

# Function to apply bounds and normalize predictions to sum to stage points total
normalize_points_predictions <- function(startlist_df, gender) {
  log_info(paste("Normalizing points predictions for", gender))
  
  # Apply bounds: min = 0, max = max stage points
  max_stage_points <- max(stage_points)
  min_stage_points <- 0
  
  # Log raw prediction statistics
  log_info(paste("Raw predictions for", gender, ":"))
  log_info(paste("  Range:", round(min(startlist_df$Points_Pred), 2), "-", round(max(startlist_df$Points_Pred), 2)))
  log_info(paste("  Sum:", round(sum(startlist_df$Points_Pred), 2)))
  
  # Apply bounds
  startlist_df$Points_Pred <- pmax(min_stage_points, pmin(max_stage_points, startlist_df$Points_Pred))
  
  # Calculate target sum (sum of available stage points)
  num_athletes <- nrow(startlist_df)
  if(num_athletes <= length(stage_points)) {
    # Use exact stage points if we have fewer athletes than stage point positions
    target_sum <- sum(stage_points[1:num_athletes])
  } else {
    # If more athletes than stage point positions, extend with zeros
    target_sum <- sum(stage_points)
  }
  
  # Normalize predictions to sum to target
  current_sum <- sum(startlist_df$Points_Pred)
  if(current_sum > 0) {
    scaling_factor <- target_sum / current_sum
    startlist_df$Points_Pred <- startlist_df$Points_Pred * scaling_factor
    
    # Re-apply bounds after scaling
    startlist_df$Points_Pred <- pmax(min_stage_points, pmin(max_stage_points, startlist_df$Points_Pred))
    
    log_info(paste("Applied scaling factor:", round(scaling_factor, 4)))
  } else {
    log_info(paste("All predictions were zero for", gender, "- using default distribution"))
    # If all predictions are zero, distribute stage points evenly among top athletes
    top_athletes <- min(num_athletes, length(stage_points))
    startlist_df$Points_Pred[1:top_athletes] <- stage_points[1:top_athletes]
    if(num_athletes > length(stage_points)) {
      startlist_df$Points_Pred[(length(stage_points)+1):num_athletes] <- 0
    }
  }
  
  # Log final statistics
  final_sum <- sum(startlist_df$Points_Pred)
  log_info(paste("Normalized predictions for", gender, ":"))
  log_info(paste("  Range:", round(min(startlist_df$Points_Pred), 2), "-", round(max(startlist_df$Points_Pred), 2)))
  log_info(paste("  Sum:", round(final_sum, 2), "/ Target:", round(target_sum, 2)))
  log_info(paste("  Athletes with points > 0:", sum(startlist_df$Points_Pred > 0)))
  
  return(startlist_df)
}

# Apply normalization to both genders
log_info("Normalizing men's predictions...")
men_startlist <- normalize_points_predictions(men_startlist, "men")

log_info("Normalizing ladies' predictions...")
ladies_startlist <- normalize_points_predictions(ladies_startlist, "ladies")

log_info("=== POINTS PREDICTION NORMALIZATION COMPLETE ===")

# ============================================================================
# OUTPUT RESULTS
# ============================================================================

log_info("=== SAVING FINAL CLIMB RESULTS ===")

# Function to create Excel output with specified columns
create_excel_output <- function(startlist_df, gender) {
  output_df <- startlist_df %>%
    select(Skier, ID, Nation, Sex, Points_Pred) %>%
    rename(Points = Points_Pred) %>%
    arrange(desc(Points)) %>%
    mutate(
      Points = round(Points, 2)
    )
  
  log_info(paste("Created", gender, "Excel output with", nrow(output_df), "skiers"))
  return(output_df)
}

# Create output datasets
men_excel_output <- create_excel_output(men_startlist, "men")
ladies_excel_output <- create_excel_output(ladies_startlist, "ladies")

# Create output directory structure (UTC time)
today_date_utc <- format(Sys.time(), "%Y%m%d", tz = "UTC")
output_base_dir <- "~/blog/daehl-e/content/post/cross-country/drafts/race-picks"
output_dir <- file.path(output_base_dir, today_date_utc)

# Create directory if it doesn't exist
if (!dir.exists(file.path(output_base_dir))) {
  dir.create(file.path(output_base_dir), recursive = TRUE)
}
if (!dir.exists(file.path(output_dir))) {
  dir.create(file.path(output_dir), recursive = TRUE)
}

# Expand tilde in paths
output_dir <- path.expand(output_dir)

# Save Excel files
men_excel_path <- file.path(output_dir, "men.xlsx")
ladies_excel_path <- file.path(output_dir, "ladies.xlsx")

write.xlsx(men_excel_output, men_excel_path, rowNames = FALSE)
write.xlsx(ladies_excel_output, ladies_excel_path, rowNames = FALSE)

log_info("Final Climb Excel files saved:")
log_info(paste("  Men's predictions:", men_excel_path))
log_info(paste("  Ladies' predictions:", ladies_excel_path))

# ============================================================================
# POSITION PROBABILITY TRAINING DATA SETUP
# ============================================================================

log_info("=== SETTING UP POSITION PROBABILITY TRAINING DATA ===")

# Define position thresholds
position_thresholds <- c(1, 3, 5, 10, 30)  # Win, Podium, Top 5, Top 10, Top 30

# Function to add position threshold columns to training data
add_position_thresholds <- function(training_df, gender) {
  log_info(paste("Adding position threshold columns for", gender, "Final Climb training data"))
  
  # Add binary columns for each threshold
  result_df <- training_df
  
  for(threshold in position_thresholds) {
    threshold_col <- paste0("top_", threshold)
    result_df[[threshold_col]] <- as.numeric(result_df$Place <= threshold & !is.na(result_df$Place))
    
    # Count how many TRUE values we have
    true_count <- sum(result_df[[threshold_col]], na.rm = TRUE)
    log_info(paste("  ", threshold_col, "- TRUE values:", true_count, "out of", nrow(result_df), "records"))
  }
  
  return(result_df)
}

# Add position thresholds to both training datasets (use full training data, not just Final Climb subset)
men_training_with_thresholds <- add_position_thresholds(men_points_training, "men")
ladies_training_with_thresholds <- add_position_thresholds(ladies_points_training, "ladies")

log_info("=== POSITION PROBABILITY TRAINING DATA SETUP COMPLETE ===")

# ============================================================================
# POSITION PROBABILITY FEATURE SELECTION
# ============================================================================

log_info("=== POSITION PROBABILITY FEATURE SELECTION ===")

# Define Final Climb features for probability models
final_fc_features <- c("Distance_F_Pelo_Pct", "Distance_F_Last_5", "FC_Last_5")

# Function to perform feature selection for position probability models
perform_position_feature_selection <- function(training_df, gender) {
  log_info(paste("Performing feature selection for", gender, "position probability models"))
  
  # Prepare the data - filter out invalid records and handle NAs
  model_df <- training_df %>%
    filter(!is.na(Place), Place > 0) %>%
    mutate(across(all_of(final_fc_features), replace_na_with_quartile))
  
  log_info(paste("Using", nrow(model_df), "records for feature selection"))
  
  # Storage for selected features by threshold
  selected_features <- list()
  
  # Perform feature selection for each threshold
  for(threshold in position_thresholds) {
    log_info(paste("Feature selection for top", threshold, "positions"))
    
    threshold_col <- paste0("top_", threshold)
    
    # Create formula using all final FC features
    pos_formula <- as.formula(paste(threshold_col, "~", paste(final_fc_features, collapse = " + ")))
    
    tryCatch({
      # Use regsubsets for feature selection
      pos_selection <- regsubsets(pos_formula, data = model_df, nbest = 1, method = "exhaustive")
      pos_summary <- summary(pos_selection)
      
      # Get best features based on BIC
      best_model_idx <- which.min(pos_summary$bic)
      pos_best_bic_vars <- names(coef(pos_selection, best_model_idx))
      
      # Remove intercept from variable names
      selected_vars <- pos_best_bic_vars[pos_best_bic_vars != "(Intercept)"]
      
      # Store selected features
      selected_features[[threshold_col]] <- selected_vars
      
      log_info(paste("  Selected", length(selected_vars), "features:", paste(selected_vars, collapse = ", ")))
      log_info(paste("  Best BIC:", round(pos_summary$bic[best_model_idx], 2)))
      
    }, error = function(e) {
      log_warn(paste("Error in feature selection for threshold", threshold, ":", e$message))
      
      # Fallback to key features if selection fails
      fallback_features <- c("Distance_F_Pelo_Pct", "FC_Last_5")
      fallback_features <- fallback_features[fallback_features %in% final_fc_features]
      selected_features[[threshold_col]] <- fallback_features
      
      log_info(paste("  Using fallback features:", paste(fallback_features, collapse = ", ")))
    })
  }
  
  return(selected_features)
}

# Perform feature selection for both genders
men_position_features <- perform_position_feature_selection(men_training_with_thresholds, "men")
ladies_position_features <- perform_position_feature_selection(ladies_training_with_thresholds, "ladies")

# Display feature selection summary
log_info("=== FEATURE SELECTION SUMMARY ===")
log_info("Men's position probability features:")
for(threshold in position_thresholds) {
  threshold_col <- paste0("top_", threshold)
  if(threshold_col %in% names(men_position_features)) {
    features <- men_position_features[[threshold_col]]
    log_info(paste("  Top", threshold, "(", length(features), "features):", paste(features, collapse = ", ")))
  }
}

log_info("Ladies' position probability features:")
for(threshold in position_thresholds) {
  threshold_col <- paste0("top_", threshold)
  if(threshold_col %in% names(ladies_position_features)) {
    features <- ladies_position_features[[threshold_col]]
    log_info(paste("  Top", threshold, "(", length(features), "features):", paste(features, collapse = ", ")))
  }
}

log_info("=== POSITION PROBABILITY FEATURE SELECTION COMPLETE ===")

# ============================================================================
# POSITION PROBABILITY GAM MODELS
# ============================================================================

log_info("=== BUILDING POSITION PROBABILITY GAM MODELS ===")

# Function to build GAM models for position probabilities
build_position_gam_models <- function(training_df, selected_features, gender) {
  log_info(paste("Building GAM models for", gender, "position probabilities"))
  
  # Prepare the data
  model_df <- training_df %>%
    filter(!is.na(Place), Place > 0) %>%
    mutate(across(all_of(final_fc_features), replace_na_with_quartile))
  
  log_info(paste("Using", nrow(model_df), "records for GAM model training"))
  
  # Storage for models
  gam_models <- list()
  
  # Build GAM model for each threshold
  for(threshold in position_thresholds) {
    log_info(paste("Building GAM model for top", threshold, "positions"))
    
    threshold_col <- paste0("top_", threshold)
    
    if(threshold_col %in% names(selected_features)) {
      features <- selected_features[[threshold_col]]
      
      # Check that all features exist in the data
      available_features <- features[features %in% names(model_df)]
      if(length(available_features) < length(features)) {
        missing_features <- setdiff(features, available_features)
        log_warn(paste("Missing features for", threshold_col, ":", paste(missing_features, collapse = ", ")))
      }
      
      if(length(available_features) > 0) {
        log_info(paste("Available features for", threshold_col, "(", length(available_features), "):", paste(available_features, collapse = ", ")))
        
        tryCatch({
          # Create smooth terms for GAM
          smooth_terms <- paste("s(", available_features, ")", collapse = " + ")
          gam_formula <- as.formula(paste(threshold_col, "~", smooth_terms))
          
          log_info(paste("GAM formula for", threshold_col, ":", paste(deparse(gam_formula), collapse = " ")))
          
          # Fit GAM model with binomial family for binary outcomes
          gam_model <- gam(gam_formula,
                          data = model_df,
                          family = binomial,
                          method = "REML")
          
          # Calculate model performance metrics
          predicted_probs <- predict(gam_model, newdata = model_df, type = "response")
          brier_score <- mean((model_df[[threshold_col]] - predicted_probs)^2, na.rm = TRUE)
          
          # Calculate AUC if possible
          tryCatch({
            library(pROC)
            roc_obj <- roc(model_df[[threshold_col]], predicted_probs, quiet = TRUE)
            auc_value <- auc(roc_obj)
            log_info(paste("Model", threshold_col, "- AUC:", round(auc_value, 3), "Brier Score:", round(brier_score, 4)))
          }, error = function(e) {
            log_info(paste("Model", threshold_col, "- Brier Score:", round(brier_score, 4)))
          })
          
          # Store the model
          gam_models[[threshold_col]] <- gam_model
          
          log_info(paste("Successfully built GAM model for", threshold_col))
          
        }, error = function(e) {
          log_warn(paste("Error building GAM model for", threshold_col, ":", e$message))
          
          # Check if it's a convergence issue and try with fewer smoothing parameters
          if(grepl("convergence|Hessian|singular", e$message, ignore.case = TRUE)) {
            log_info(paste("Convergence issue detected for", threshold_col, "- trying simpler smoothing"))
            
            tryCatch({
              # Try with lower k (basis dimension) for smoothing
              simple_terms <- paste("s(", available_features, ", k=3)", collapse = " + ")
              simple_formula <- as.formula(paste(threshold_col, "~", simple_terms))
              
              gam_models[[threshold_col]] <- gam(simple_formula,
                                               data = model_df,
                                               family = binomial,
                                               method = "REML")
              
              log_info(paste("Created GAM model with simpler smoothing for", threshold_col))
              
            }, error = function(e_simple) {
              log_warn(paste("Simpler smoothing also failed for", threshold_col, ":", e_simple$message))
              
              # Final fallback: Use just the first 2 features
              if(length(available_features) > 2) {
                simple_features <- available_features[1:2]
                log_info(paste("Final fallback - using 2 features:", paste(simple_features, collapse = ", ")))
                
                tryCatch({
                  simple_terms <- paste("s(", simple_features, ")", collapse = " + ")
                  simple_formula <- as.formula(paste(threshold_col, "~", simple_terms))
                  
                  gam_models[[threshold_col]] <- gam(simple_formula,
                                                   data = model_df,
                                                   family = binomial,
                                                   method = "REML")
                  
                  log_info(paste("Created minimal GAM model for", threshold_col))
                  
                }, error = function(e3) {
                  log_warn(paste("All model attempts failed for", threshold_col, ":", e3$message))
                })
              }
            })
          } else {
            # Try simpler fallback model with fewer features
            if(length(available_features) > 2) {
              # Use just the first 2 features
              simple_features <- available_features[1:2]
              log_info(paste("Trying simplified model with features:", paste(simple_features, collapse = ", ")))
              
              tryCatch({
                simple_terms <- paste("s(", simple_features, ")", collapse = " + ")
                simple_formula <- as.formula(paste(threshold_col, "~", simple_terms))
                
                gam_models[[threshold_col]] <- gam(simple_formula,
                                                 data = model_df,
                                                 family = binomial,
                                                 method = "REML")
                
                log_info(paste("Created simplified GAM model for", threshold_col))
                
              }, error = function(e2) {
                log_warn(paste("Simplified model also failed for", threshold_col, ":", e2$message))
              })
            }
          }
        })
      } else {
        log_warn(paste("No available features for", threshold_col))
      }
    } else {
      log_warn(paste("No selected features found for", threshold_col))
    }
  }
  
  log_info(paste("Built", length(gam_models), "GAM models for", gender))
  return(gam_models)
}

# Build GAM models for both genders
men_position_gam_models <- build_position_gam_models(men_training_with_thresholds, men_position_features, "men")
ladies_position_gam_models <- build_position_gam_models(ladies_training_with_thresholds, ladies_position_features, "ladies")

log_info("=== POSITION PROBABILITY GAM MODELS COMPLETE ===")

# ============================================================================
# APPLY POSITION PROBABILITY MODELS TO STARTLIST
# ============================================================================

log_info("=== APPLYING POSITION PROBABILITY MODELS TO STARTLIST ===")

# Function to apply position probability models to startlist
apply_position_models_to_startlist <- function(startlist_df, position_models, gender) {
  log_info(paste("Applying position probability models to", gender, "startlist"))
  
  # Create a copy of the startlist for predictions
  predictions_df <- startlist_df
  
  # Apply each model to generate probabilities
  for(threshold in position_thresholds) {
    threshold_col <- paste0("top_", threshold)
    prob_col <- paste0("prob_top", threshold)
    
    if(threshold_col %in% names(position_models)) {
      model <- position_models[[threshold_col]]
      
      log_info(paste("Applying", threshold_col, "model to generate", prob_col))
      
      tryCatch({
        # Generate predictions (probabilities between 0 and 1)
        raw_probs <- predict(model, newdata = predictions_df, type = "response")
        
        # Convert to percentages (0-100)
        predictions_df[[prob_col]] <- round(raw_probs * 100, 2)
        
        # Log some statistics
        valid_probs <- predictions_df[[prob_col]][!is.na(predictions_df[[prob_col]])]
        if(length(valid_probs) > 0) {
          log_info(paste("  ", prob_col, "- Generated for", length(valid_probs), "athletes"))
          log_info(paste("    Range:", round(min(valid_probs), 1), "% to", round(max(valid_probs), 1), "%"))
          log_info(paste("    Sum:", round(sum(valid_probs), 1), "% (target:", 100 * threshold, "%)"))
        }
        
      }, error = function(e) {
        log_warn(paste("Error applying", threshold_col, "model:", e$message))
        # Set default probabilities based on threshold
        predictions_df[[prob_col]] <- rep(threshold, nrow(predictions_df))
      })
    } else {
      log_warn(paste("No model available for", threshold_col, "- setting default probabilities"))
      # Set default probabilities based on threshold
      predictions_df[[prob_col]] <- rep(threshold, nrow(predictions_df))
    }
  }
  
  log_info(paste("Completed probability generation for", gender, "startlist"))
  return(predictions_df)
}

# Apply models to both startlists
men_startlist_with_probs <- apply_position_models_to_startlist(men_startlist, men_position_gam_models, "men")
ladies_startlist_with_probs <- apply_position_models_to_startlist(ladies_startlist, ladies_position_gam_models, "ladies")

log_info("=== POSITION PROBABILITY APPLICATION COMPLETE ===")

# ============================================================================
# NORMALIZE POSITION PROBABILITIES AND APPLY CONSTRAINTS
# ============================================================================

log_info("=== NORMALIZING POSITION PROBABILITIES ===")

# Function to normalize and apply constraints to probabilities
normalize_startlist_probabilities <- function(predictions_df, gender) {
  log_info(paste("Normalizing position probabilities for", gender, "startlist"))
  
  normalized_df <- predictions_df
  
  # Log initial sums before normalization
  log_info("Position probability sums BEFORE normalization:")
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    if(prob_col %in% names(normalized_df)) {
      initial_sum <- sum(normalized_df[[prob_col]], na.rm = TRUE)
      target_sum <- 100 * threshold
      log_info(sprintf("  %s: %.1f%% (target: %.0f%%)", prob_col, initial_sum, target_sum))
    }
  }
  
  # Normalize each probability column to sum to target
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    
    if(prob_col %in% names(normalized_df)) {
      current_sum <- sum(normalized_df[[prob_col]], na.rm = TRUE)
      target_sum <- 100 * threshold
      
      if(current_sum > 0) {
        # Apply scaling factor
        scaling_factor <- target_sum / current_sum
        normalized_df[[prob_col]] <- normalized_df[[prob_col]] * scaling_factor
        
        # Cap at 100% and redistribute excess
        over_hundred <- which(normalized_df[[prob_col]] > 100)
        if(length(over_hundred) > 0) {
          excess_total <- sum(normalized_df[[prob_col]][over_hundred] - 100)
          normalized_df[[prob_col]][over_hundred] <- 100
          
          # Redistribute excess to other athletes proportionally
          eligible_indices <- which(normalized_df[[prob_col]] < 100)
          if(length(eligible_indices) > 0 && excess_total > 0) {
            current_eligible <- normalized_df[[prob_col]][eligible_indices]
            room_available <- 100 - current_eligible
            total_room <- sum(room_available)
            
            if(total_room > 0) {
              redistribution <- pmin(room_available, 
                                   excess_total * room_available / total_room)
              normalized_df[[prob_col]][eligible_indices] <- 
                current_eligible + redistribution
            }
          }
        }
      }
    }
  }
  
  # Apply monotonic constraints: Win <= Podium <= Top5 <= Top10 <= Top30
  log_info("Applying monotonic constraints...")
  
  prob_cols <- paste0("prob_top", position_thresholds)
  prob_cols <- prob_cols[prob_cols %in% names(normalized_df)]
  
  for(i in 1:nrow(normalized_df)) {
    probs <- numeric(length(prob_cols))
    for(j in 1:length(prob_cols)) {
      probs[j] <- normalized_df[[prob_cols[j]]][i]
    }
    
    # Ensure monotonic non-decreasing order
    for(j in 2:length(probs)) {
      if(probs[j] < probs[j-1]) {
        probs[j] <- probs[j-1]
      }
    }
    
    # Update the dataframe
    for(j in 1:length(prob_cols)) {
      normalized_df[[prob_cols[j]]][i] <- probs[j]
    }
  }
  
  # Re-normalize after monotonic constraints
  log_info("Re-normalizing after monotonic constraints...")
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    
    if(prob_col %in% names(normalized_df)) {
      current_sum <- sum(normalized_df[[prob_col]], na.rm = TRUE)
      target_sum <- 100 * threshold
      
      if(current_sum > 0) {
        scaling_factor <- target_sum / current_sum
        normalized_df[[prob_col]] <- normalized_df[[prob_col]] * scaling_factor
        normalized_df[[prob_col]][normalized_df[[prob_col]] > 100] <- 100
      }
    }
  }
  
  # Log final sums
  log_info("Position probability sums AFTER normalization and constraints:")
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    if(prob_col %in% names(normalized_df)) {
      final_sum <- sum(normalized_df[[prob_col]], na.rm = TRUE)
      target_sum <- 100 * threshold
      log_info(sprintf("  %s: %.1f%% (target: %.0f%%)", prob_col, final_sum, target_sum))
    }
  }
  
  return(normalized_df)
}

# Apply normalization to both startlists
men_startlist_normalized <- normalize_startlist_probabilities(men_startlist_with_probs, "men")
ladies_startlist_normalized <- normalize_startlist_probabilities(ladies_startlist_with_probs, "ladies")

log_info("=== POSITION PROBABILITY NORMALIZATION COMPLETE ===")

# ============================================================================
# SAVE POSITION PROBABILITY RESULTS
# ============================================================================

log_info("=== SAVING POSITION PROBABILITY RESULTS ===")

# Function to create position probability Excel output
create_position_probability_output <- function(startlist_df, gender) {
  # Create output with specified columns and rename probabilities to user-friendly names
  output_df <- startlist_df %>%
    select(Skier, ID, Nation, Sex, prob_top1, prob_top3, prob_top5, prob_top10, prob_top30) %>%
    rename(
      Win = prob_top1,
      Podium = prob_top3,
      `Top-5` = prob_top5,
      `Top-10` = prob_top10,
      `Top-30` = prob_top30
    ) %>%
    arrange(desc(Win)) %>%
    mutate(
      Win = round(Win, 1),
      Podium = round(Podium, 1),
      `Top-5` = round(`Top-5`, 1),
      `Top-10` = round(`Top-10`, 1),
      `Top-30` = round(`Top-30`, 1)
    )
  
  log_info(paste("Created position probability output for", gender, "with", nrow(output_df), "skiers"))
  return(output_df)
}

# Create position probability outputs
men_prob_output <- create_position_probability_output(men_startlist_normalized, "men")
ladies_prob_output <- create_position_probability_output(ladies_startlist_normalized, "ladies")

# Create output directory structure for probabilities (UTC time)
prob_output_base_dir <- "~/blog/daehl-e/content/post/cross-country/drafts/race-picks"
prob_output_dir <- file.path(prob_output_base_dir, today_date_utc)

# Create directory if it doesn't exist
if (!dir.exists(file.path(prob_output_base_dir))) {
  dir.create(file.path(prob_output_base_dir), recursive = TRUE)
}
if (!dir.exists(file.path(prob_output_dir))) {
  dir.create(file.path(prob_output_dir), recursive = TRUE)
}

# Expand tilde in paths
prob_output_dir <- path.expand(prob_output_dir)

# Save position probability Excel files
men_prob_path <- file.path(prob_output_dir, "men_position_probabilities.xlsx")
ladies_prob_path <- file.path(prob_output_dir, "ladies_position_probabilities.xlsx")

# Create workbooks with custom sheet names
men_wb <- createWorkbook()
addWorksheet(men_wb, "Men Race 1")
writeData(men_wb, "Men Race 1", men_prob_output)

ladies_wb <- createWorkbook()
addWorksheet(ladies_wb, "Ladies Race 1")
writeData(ladies_wb, "Ladies Race 1", ladies_prob_output)

# Save the workbooks
saveWorkbook(men_wb, men_prob_path, overwrite = TRUE)
saveWorkbook(ladies_wb, ladies_prob_path, overwrite = TRUE)

log_info("Position probability Excel files saved:")
log_info(paste("  Men's probabilities:", men_prob_path))
log_info(paste("  Ladies' probabilities:", ladies_prob_path))

log_info("=== POSITION PROBABILITY OUTPUT COMPLETE ===")

# ============================================================================
# FANTASY TEAM OPTIMIZATION
# ============================================================================

log_info("=== FINAL CLIMB FANTASY TEAM OPTIMIZATION ===")

# Fantasy team optimization - standard format
FANTASY_BUDGET <- 100000  # Total budget for combined team
MAX_MEN <- 8              # Maximum men in team
MAX_LADIES <- 8           # Maximum ladies in team

# Function to optimize combined fantasy team using knapsack approach
optimize_fc_combined_team <- function(men_startlist, ladies_startlist, total_budget = FANTASY_BUDGET) {
  log_info(paste("Optimizing combined Final Climb fantasy team using knapsack approach"))
  log_info(paste("Budget:", total_budget, "| Max team: up to", MAX_MEN, "men and", MAX_LADIES, "ladies"))
  
  # Prepare men's candidates
  men_candidates <- NULL
  if(!is.null(men_startlist)) {
    men_candidates <- men_startlist %>%
      filter(!is.na(Price), Price > 0, !is.na(Points_Pred)) %>%
      mutate(
        Gender = "M",
        expected_value = Points_Pred,  # Only use predicted points
        athlete_id = paste0("M_", row_number())
      ) %>%
      select(athlete_id, Skier, ID, Nation, Price, Gender, Points_Pred, expected_value)
    
    log_info(paste("Found", nrow(men_candidates), "valid men candidates"))
  }
  
  # Prepare ladies' candidates  
  ladies_candidates <- NULL
  if(!is.null(ladies_startlist)) {
    ladies_candidates <- ladies_startlist %>%
      filter(!is.na(Price), Price > 0, !is.na(Points_Pred)) %>%
      mutate(
        Gender = "L", 
        expected_value = Points_Pred,  # Only use predicted points
        athlete_id = paste0("L_", row_number())
      ) %>%
      select(athlete_id, Skier, ID, Nation, Price, Gender, Points_Pred, expected_value)
    
    log_info(paste("Found", nrow(ladies_candidates), "valid ladies candidates"))
  }
  
  # Check if we have any candidates
  if(is.null(men_candidates) && is.null(ladies_candidates)) {
    log_info("No valid candidates found for fantasy optimization")
    return(NULL)
  }
  
  # Combine all candidates for optimization
  all_candidates <- rbind(
    if(!is.null(men_candidates)) men_candidates else data.frame(),
    if(!is.null(ladies_candidates)) ladies_candidates else data.frame()
  )
  
  if(nrow(all_candidates) == 0) {
    log_info("No candidates available for team optimization")
    return(NULL)
  }
  
  log_info(paste("Total candidates for knapsack optimization:", nrow(all_candidates)))
  
  # Knapsack optimization using ompr
  tryCatch({
    log_info("Setting up knapsack optimization model...")
    
    n_athletes <- nrow(all_candidates)
    n_men <- ifelse(!is.null(men_candidates), nrow(men_candidates), 0)
    n_ladies <- ifelse(!is.null(ladies_candidates), nrow(ladies_candidates), 0)
    
    # Create the optimization model
    model <- MIPModel() %>%
      # Decision variables: binary selection for each athlete
      add_variable(x[i], i = 1:n_athletes, type = "binary") %>%
      
      # Objective: maximize total expected value
      set_objective(sum_expr(all_candidates$expected_value[i] * x[i], i = 1:n_athletes), "max") %>%
      
      # Budget constraint
      add_constraint(sum_expr(all_candidates$Price[i] * x[i], i = 1:n_athletes) <= total_budget) %>%
      
      # Men count constraint (if we have men candidates)
      {if(n_men > 0) {
        men_indices <- which(all_candidates$Gender == "M")
        add_constraint(., sum_expr(x[i], i = men_indices) <= MAX_MEN)
      } else .} %>%
      
      # Ladies count constraint (if we have ladies candidates)  
      {if(n_ladies > 0) {
        ladies_indices <- which(all_candidates$Gender == "L")
        add_constraint(., sum_expr(x[i], i = ladies_indices) <= MAX_LADIES)
      } else .}
    
    log_info("Solving knapsack optimization...")
    
    # Solve the model
    result <- solve_model(model, with_ROI(solver = "glpk", verbose = FALSE))
    
    if(result$status == "optimal" || result$status == "success") {
      log_info(paste("Found optimal solution! Status:", result$status))
      
      # Extract selected athletes
      solution <- get_solution(result, x[i])
      selected_indices <- solution[solution$value == 1, ]$i
      selected_team <- all_candidates[selected_indices, ]
      
      # Calculate final team statistics
      total_cost <- sum(selected_team$Price)
      total_expected_points <- sum(selected_team$Points_Pred)
      men_count <- sum(selected_team$Gender == "M")
      ladies_count <- sum(selected_team$Gender == "L")
      
      log_info(paste("=== OPTIMAL FINAL CLIMB FANTASY TEAM (KNAPSACK) ==="))
      log_info(paste("Team size:", nrow(selected_team), "athletes"))
      log_info(paste("  Men:", men_count, "/", MAX_MEN))
      log_info(paste("  Ladies:", ladies_count, "/", MAX_LADIES))
      log_info(paste("Total cost:", total_cost, "/", total_budget, "(", round(100*total_cost/total_budget, 1), "% of budget)"))
      log_info(paste("Expected Final Climb points:", round(total_expected_points, 1)))
      log_info(paste("Remaining budget:", total_budget - total_cost))
      log_info(paste("Objective value:", result$objective_value))
      
      # Log selected athletes
      log_info("Selected athletes:")
      for(i in 1:nrow(selected_team)) {
        athlete <- selected_team[i, ]
        log_info(paste("  ", athlete$Gender, "-", athlete$Skier, "from", athlete$Nation,
                      "- Price:", athlete$Price, "- Expected:", round(athlete$expected_value, 1)))
      }
      
      # Team composition analysis
      team_summary <- selected_team %>%
        group_by(Gender, Nation) %>%
        summarise(
          Count = n(), 
          Total_Cost = sum(Price), 
          Avg_Value = mean(expected_value),
          .groups = "drop"
        ) %>%
        arrange(Gender, desc(Count))
      
      log_info("Team composition by nation:")
      for(i in 1:nrow(team_summary)) {
        log_info(paste("  ", team_summary$Gender[i], "-", team_summary$Nation[i], ":", 
                      team_summary$Count[i], "athletes, cost:", team_summary$Total_Cost[i],
                      "avg value:", round(team_summary$Avg_Value[i], 1)))
      }
      
      # Format final team output
      fantasy_team <- selected_team %>%
        select(Skier, ID, Nation, Gender, Price, Points_Pred) %>%
        rename(Sex = Gender, `Predicted Points` = Points_Pred) %>%
        arrange(desc(`Predicted Points`))
      
      return(fantasy_team)
      
    } else {
      log_info(paste("Optimization failed with status:", result$status))
      return(NULL)
    }
    
  }, error = function(e) {
    log_info(paste("Error in knapsack optimization:", e$message))
    log_info("Attempting fallback to greedy approach...")
    
    # Fallback to simple greedy approach if knapsack fails
    return(optimize_fc_greedy_fallback(all_candidates, total_budget))
  })
}

# Fallback greedy function for when knapsack optimization fails
optimize_fc_greedy_fallback <- function(all_candidates, total_budget) {
  log_info("Using greedy fallback optimization...")
  
  # Simple greedy by value per price
  all_candidates$value_per_price <- all_candidates$expected_value / all_candidates$Price
  all_candidates <- all_candidates %>% arrange(desc(value_per_price))
  
  selected_team <- data.frame()
  remaining_budget <- total_budget
  men_count <- 0
  ladies_count <- 0
  
  for(i in 1:nrow(all_candidates)) {
    candidate <- all_candidates[i, ]
    
    # Check constraints
    can_select <- FALSE
    if(candidate$Gender == "M" && men_count < MAX_MEN) {
      can_select <- TRUE
    } else if(candidate$Gender == "L" && ladies_count < MAX_LADIES) {
      can_select <- TRUE
    }
    
    if(can_select && candidate$Price <= remaining_budget) {
      selected_team <- rbind(selected_team, candidate)
      remaining_budget <- remaining_budget - candidate$Price
      
      if(candidate$Gender == "M") {
        men_count <- men_count + 1
      } else {
        ladies_count <- ladies_count + 1
      }
      
      if(men_count == MAX_MEN && ladies_count == MAX_LADIES) break
    }
  }
  
  if(nrow(selected_team) > 0) {
    log_info(paste("Fallback solution: selected", nrow(selected_team), "athletes"))
    
    fantasy_team <- selected_team %>%
      select(Skier, ID, Nation, Gender, Price, Points_Pred) %>%
      rename(Sex = Gender, `Predicted Points` = Points_Pred) %>%
      arrange(desc(`Predicted Points`))
    
    return(fantasy_team)
  }
  
  return(NULL)
}

# Run combined fantasy team optimization
fc_fantasy_team <- NULL

if(exists("men_startlist_normalized") && exists("ladies_startlist_normalized")) {
  fc_fantasy_team <- optimize_fc_combined_team(men_startlist_normalized, ladies_startlist_normalized)
} else {
  log_info("Missing normalized startlist data - skipping fantasy optimization")
  log_info(paste("men_startlist_normalized exists:", exists("men_startlist_normalized")))
  log_info(paste("ladies_startlist_normalized exists:", exists("ladies_startlist_normalized")))
}

# Save combined fantasy team
if(!is.null(fc_fantasy_team)) {
  log_info("=== SAVING FANTASY TEAM ===")
  
  fantasy_team_path <- file.path(output_dir, "fantasy_team.xlsx")
  write.xlsx(fc_fantasy_team, fantasy_team_path, rowNames = FALSE)
  log_info(paste("Saved Final Climb fantasy team to:", fantasy_team_path))
  
  # Also create summary by gender
  men_team <- fc_fantasy_team %>% filter(Sex == "M")
  ladies_team <- fc_fantasy_team %>% filter(Sex == "L")
  
  if(nrow(men_team) > 0) {
    log_info(paste("Men's team:", nrow(men_team), "athletes, total cost:", sum(men_team$Price)))
  }
  if(nrow(ladies_team) > 0) {
    log_info(paste("Ladies' team:", nrow(ladies_team), "athletes, total cost:", sum(ladies_team$Price)))
  }
}

log_info("=== FANTASY TEAM OPTIMIZATION COMPLETE ===")

log_info("Final Climb predictions process completed successfully")


