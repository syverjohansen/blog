# Alpine Skiing Championships Predictions: Based on weekly-picks2.R with Championship modifications
library(dplyr)
library(tidyr)
library(openxlsx)
library(arrow)
library(mgcv)
library(leaps)
library(logger)
library(purrr)
library(lubridate) # For better date handling
library(slider)    # For sliding window operations

# Define points systems for Alpine Skiing
# World Cup: Top 30 get points
world_cup_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Function to replace NAs with first quartile value
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Set up logging
log_dir <- "~/ski/elo/python/alpine/polars/excel365/champs-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "champs_picks_processing.log")))
log_info("Starting Alpine Skiing Championships predictions process")

# Read in the race schedule from weekends.csv with proper date parsing
log_info("Reading weekends data")
weekends <- read.csv("~/ski/elo/python/alpine/polars/excel365/weekends.csv", 
                     stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y"))

# Filter for Championships races only (Championship == 1)
log_info("Filtering for Championships races")
champs_races <- weekends %>%
  filter(Championship == 1)

if (nrow(champs_races) == 0) {
  log_info("No Championships races found. Terminating program.")
  quit(save = "no", status = 0)
}

log_info(paste("Found", nrow(champs_races), "Championships races"))

# Create race dataframes for men and ladies (all alpine races are individual)
men_races <- champs_races %>%
  filter(Sex == "M") %>%
  dplyr::select(Distance, Period, Country) %>%
  rename(discipline = Distance, period = Period, country = Country)

ladies_races <- champs_races %>%
  filter(Sex == "L") %>%
  dplyr::select(Distance, Period, Country) %>%
  rename(discipline = Distance, period = Period, country = Country)

log_info(paste("Found", nrow(men_races), "men's races,", nrow(ladies_races), "ladies' races"))

# Function to get points based on place for Alpine Skiing
get_points <- function(place, discipline = NULL) {
  # All Alpine events use World Cup points system
  points_list <- world_cup_points
  
  if (place >= 1 && place <= length(points_list)) {
    return(points_list[place])
  }
  return(0)
}

# Normalization function for position probabilities
normalize_position_probabilities <- function(predictions, race_prob_col, position_thresholds) {
  # Make a copy to avoid modifying the original data frame
  normalized <- predictions
  
  # Log initial sums before any modifications
  log_info("Position probability sums BEFORE normalization:")
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    if(prob_col %in% names(normalized)) {
      initial_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
      log_info(sprintf("  %s: %.2f%% (target: %d%%)", 
                       prob_col, initial_sum, 100 * threshold))
    }
  }
  
  # For each threshold, adjust and normalize probabilities
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    
    # First, adjust by race participation probability
    if(race_prob_col %in% names(normalized)) {
      # Log sum before race probability adjustment
      sum_before_race_adj <- sum(normalized[[prob_col]], na.rm = TRUE)
      
      # Apply race probability adjustment
      normalized[[prob_col]] <- normalized[[prob_col]] * normalized[[race_prob_col]]
      
      # Log sum after race probability adjustment
      sum_after_race_adj <- sum(normalized[[prob_col]], na.rm = TRUE)
      log_info(sprintf("  %s after race prob adjustment: %.2f%% (scaling by race participation)", 
                       prob_col, sum_after_race_adj))
    }
    
    # Calculate the current sum
    current_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
    
    # Target sum should be 100 * threshold (e.g., 100% for top 1, 300% for top 3)
    target_sum <- 100 * threshold
    
    # Normalize only if current sum is not zero to avoid division by zero
    if(current_sum > 0) {
      # Apply scaling factor to adjust the probabilities
      scaling_factor <- target_sum / current_sum
      normalized[[prob_col]] <- normalized[[prob_col]] * scaling_factor
      
      # Cap individual probabilities at 100%
      over_hundred <- which(normalized[[prob_col]] > 100)
      if(length(over_hundred) > 0) {
        log_info(sprintf("  Capping %d participants with >100%% probability for %s", 
                         length(over_hundred), prob_col))
        
        # Calculate excess probability that needs to be redistributed
        excess <- sum(normalized[[prob_col]][over_hundred] - 100)
        
        # Cap values at 100%
        normalized[[prob_col]][over_hundred] <- 100
        
        # Redistribute the excess to other participants proportionally
        under_hundred <- which(normalized[[prob_col]] < 100)
        if(length(under_hundred) > 0 && excess > 0) {
          # Get current sum of under-100 probabilities
          under_sum <- sum(normalized[[prob_col]][under_hundred])
          
          # Calculate scaling factor for redistribution
          if(under_sum > 0) {
            redistrib_factor <- (under_sum + excess) / under_sum
            normalized[[prob_col]][under_hundred] <- normalized[[prob_col]][under_hundred] * redistrib_factor
            
            # Recursively cap again if needed (unlikely but possible)
            if(any(normalized[[prob_col]][under_hundred] > 100)) {
              log_info("  Recursive capping needed after redistribution")
              # This is a simplification - in practice you might want a more robust approach
              normalized[[prob_col]][normalized[[prob_col]] > 100] <- 100
            }
          }
        }
      }
      
      log_info(sprintf("  %s normalization: applied scaling factor of %.4f", 
                       prob_col, scaling_factor))
    } else {
      # If sum is zero, distribute evenly among all participants
      # This is a fallback that should rarely be needed
      log_warn(paste("Zero sum for", prob_col, "- distributing evenly"))
      normalized[[prob_col]] <- target_sum / nrow(normalized)
    }
    
    # Final check to ensure we're close to the target sum
    final_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
    if(abs(final_sum - target_sum) > 1) {  # Allow for small rounding differences
      log_warn(sprintf("  %s sum after capping: %.2f%% (target: %.2f%%)", 
                       prob_col, final_sum, target_sum))
    }
  }
  
  # Log final sums after all adjustments
  log_info("Position probability sums AFTER normalization:")
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    if(prob_col %in% names(normalized)) {
      final_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
      log_info(sprintf("  %s: %.2f%% (target: %d%%)", 
                       prob_col, final_sum, 100 * threshold))
    }
  }
  
  return(normalized)
}

# Function to prepare startlist data with ELO information (from weekly-picks2.R)
prepare_startlist_data <- function(startlist, race_df, elo_col) {
  log_info(paste("Preparing startlist data for", elo_col))
  
  # Dynamically get race probability columns - important to preserve these!
  race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
  log_info(paste("Race probability columns found:", paste(race_prob_cols, collapse=", ")))
  
  # For individual races
  base_df <- startlist %>%
    dplyr::select(Skier, Nation, Price, all_of(race_prob_cols))
  
  # For alpine races - check if Elo columns exist
  elo_cols <- c("Downhill_Elo", "Super.G_Elo", "Giant.Slalom_Elo", "Slalom_Elo", "Combined_Elo", "Tech_Elo", "Speed_Elo", "Elo")
  
  # Get most recent Elo values
  most_recent_elos <- race_df %>%
    filter(Skier %in% base_df$Skier) %>%
    group_by(Skier) %>%
    arrange(Date, Season, Race) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    dplyr::select(Skier, any_of(elo_cols))
  
  # Debug: Check elo columns
  log_info(paste("Available elo columns:", paste(names(most_recent_elos), collapse=", ")))
  
  # Get recent points for specific discipline
  recent_points <- race_df %>%
    filter(Skier %in% base_df$Skier) %>%
    group_by(Skier) %>%
    arrange(Season, Race) %>%
    slice_tail(n = 5) %>%
    summarise(
      Prev_Points_Weighted = if(n() > 0) 
        weighted.mean(Points, w = seq_len(n()), na.rm = TRUE) 
      else 0
    )
  
  # Combine all data
  result_df <- base_df %>%
    left_join(most_recent_elos, by = "Skier") %>%
    left_join(recent_points, by = "Skier")
  
  # Create percentage columns for each Elo column
  elo_columns_to_process <- c("Downhill_Elo", "Super.G_Elo", "Giant.Slalom_Elo", "Slalom_Elo", "Combined_Elo", "Tech_Elo", "Speed_Elo", "Elo")
  
  # Create percentage columns for each Elo column
  for(col in elo_columns_to_process) {
    pct_col <- paste0(col, "_Pct")
    
    if(col %in% names(result_df)) {
      # If we have race_df with this column, get max values for normalization
      if(col %in% names(race_df)) {
        max_val <- max(race_df[[col]], na.rm = TRUE)
        if(!is.na(max_val) && max_val > 0) {
          log_info(paste("Calculating", pct_col, "from", col))
          result_df[[pct_col]] <- result_df[[col]] / max_val
        } else {
          log_info(paste("Using default value for", pct_col, "(max value issue)"))
          result_df[[pct_col]] <- 0.5
        }
      } else {
        # If not available in race_df, normalize within the current dataset
        max_val <- max(result_df[[col]], na.rm = TRUE)
        if(!is.na(max_val) && max_val > 0) {
          log_info(paste("Calculating", pct_col, "from", col, "(internal max)"))
          result_df[[pct_col]] <- result_df[[col]] / max_val
        } else {
          log_info(paste("Using default value for", pct_col, "(internal max issue)"))
          result_df[[pct_col]] <- 0.5
        }
      }
    } else if(!pct_col %in% names(result_df)) {
      # If we don't have the Elo column and the PCT doesn't exist yet
      log_info(paste("Creating missing Elo Pct column:", pct_col))
      result_df[[pct_col]] <- 0.5  # Default to 0.5 (middle value)
    }
  }
  
  # Replace NAs with first quartile
  result_df <- result_df %>%
    mutate(
      across(
        ends_with("_Pct"),
        ~replace_na_with_quartile(.x)
      ),
      Prev_Points_Weighted = replace_na(Prev_Points_Weighted, 0)
    )
  
  # Check race probability columns in result
  for(col in race_prob_cols) {
    if(!is.null(result_df[[col]])) {
      log_info(paste("Race probability column", col, "summary:"))
      log_info(paste("  Mean:", mean(result_df[[col]], na.rm = TRUE)))
      log_info(paste("  Sum:", sum(result_df[[col]], na.rm = TRUE)))
      log_info(paste("  Max:", max(result_df[[col]], na.rm = TRUE)))
    } else {
      log_warn(paste("Race probability column", col, "is missing from result_df!"))
    }
  }
  
  # Ensure result_df has all the columns needed by the model
  log_info(paste("Final columns in result_df:", paste(names(result_df), collapse=", ")))
  
  return(result_df)
}

# Preprocessing function for historical race data (from weekly-picks2.R)
preprocess_data <- function(df) {
  # Load weekends data to determine points systems for historical races
  weekends_data <- read.csv("~/ski/elo/python/alpine/polars/excel365/weekends.csv", 
                            stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date, format="%Y-%m-%d"))
  
  participant_col <- "Skier"
  
  # First calculate points using data with appropriate points system
  df_with_points <- df %>%
    # Add points based on the discipline if they don't already exist
    mutate(
      Points = if("Points" %in% names(df)) {
        Points
      } else {
        mapply(function(place, distance) {
          get_points(place, distance)
        }, Place, Distance)
      }
    ) %>%
    # Sort
    arrange(Season, Race, Place)
  
  # Calculate weighted previous points separately for each discipline
  df_with_points <- df_with_points %>%
    # Group by Skier and discipline
    group_by(!!sym(participant_col), Distance) %>%
    arrange(Season, Race) %>%
    mutate(Prev_Points_Weighted = sapply(1:n(), function(j) {
      if (j == 1) return(0)
      start_index <- max(1, j - 5)
      num_races <- j - start_index
      weights <- seq(1, num_races)
      weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
    })) %>%
    ungroup()
  
  # Check if Elo columns exist, if not create them
  elo_cols <- c("Downhill_Elo", "Super.G_Elo", "Giant.Slalom_Elo", "Slalom_Elo", "Combined_Elo", "Tech_Elo", "Speed_Elo", "Elo")
  
  # Make sure these columns exist (create if missing)
  for (col in elo_cols) {
    if (!col %in% names(df_with_points)) {
      log_info(paste("Creating missing column:", col))
      df_with_points[[col]] <- 0
    }
  }
  
  # Now apply other preprocessing steps and filter for recent data
  processed_df <- df_with_points %>%
    # Add period (Alpine has 4 periods per season)
    group_by(Season) %>%
    mutate(
      Num_Races = max(Race),
      Period = case_when(
        Num_Races <= 8 ~ 1,   # Early season (Oct-Nov)
        Num_Races <= 16 ~ 2,  # Mid season (Dec-Jan)
        Num_Races <= 24 ~ 3,  # Late season (Feb)
        TRUE ~ 4              # Final season (Mar)
      )
    ) %>%
    ungroup() %>%
    # Add discipline flags for alpine
    mutate(
      Tech_Flag = ifelse(Distance %in% c("Slalom", "Giant Slalom"), 1, 0),
      Speed_Flag = ifelse(Distance %in% c("Downhill", "Super G"), 1, 0),
      Combined_Flag = ifelse(Distance %in% c("Combined", "Alpine Combined"), 1, 0)
    ) %>%
    # Filter relevant races and add cumulative points
    filter(
      Season >= max(Season-10)
    ) %>%
    group_by(!!sym(participant_col), Season) %>%
    mutate(Cumulative_Points = cumsum(Points)) %>%
    ungroup() %>%
    # Handle NAs and calculate percentages
    group_by(Season, Race) %>%
    mutate(
      across(
        all_of(elo_cols),
        ~replace_na_with_quartile(.x)
      )
    ) %>%
    # Calculate percentages for each Elo column
    mutate(
      across(
        all_of(elo_cols),
        ~{
          max_val <- max(.x, na.rm = TRUE)
          if (max_val == 0) return(rep(0, length(.x)))
          .x / max_val
        },
        .names = "{.col}_Pct"
      )
    ) %>%
    ungroup()
  
  # Ensure all required Elo_Pct columns exist
  pct_cols <- paste0(elo_cols, "_Pct")
  for (col in pct_cols) {
    if (!col %in% names(processed_df)) {
      log_info(paste("Creating missing percentage column:", col))
      processed_df[[col]] <- 0
    }
  }
  
  log_info(paste("Preprocessing complete - final dimensions:", nrow(processed_df), "x", ncol(processed_df)))
  log_info(paste("Final columns:", paste(names(processed_df)[1:min(10, length(names(processed_df)))], collapse=", "), "..."))
  
  return(processed_df)
}

# Function to process Championships for a specific gender (exact copy of weekly-picks2.R predict_races function)
process_gender_championships <- function(gender, races) {
  log_info(paste("Processing", gender, "Championships with", nrow(races), "races"))
  
  # Read Championships startlist (generated by startlist-scrape-champs.py) 
  startlist_file <- paste0("~/ski/elo/python/alpine/polars/excel365/startlist_champs_", gender, ".csv")
  
  if (!file.exists(startlist_file)) {
    log_info(paste("Championships startlist not found:", startlist_file))
    return(NULL)
  }
  
  startlist <- read.csv(startlist_file, stringsAsFactors = FALSE)
  log_info(paste("Read Championships startlist with", nrow(startlist), "athletes"))
  
  # Determine the race type and paths based on gender
  chrono_path <- paste0("~/ski/elo/python/alpine/polars/excel365/", gender, "_chrono.csv")
  log_info(paste("Using chronological data from:", chrono_path))
  
  participant_col <- "Skier"
  
  # Load chronological data
  log_info(paste("Loading chronological data from", chrono_path))
  
  # Standard individual race handling
  df <- read.csv(chrono_path, stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date)) %>%
    preprocess_data()
  
  # For debugging
  log_info(paste("Chronological data dimensions:", nrow(df), "x", ncol(df)))
  log_info(paste("First few columns:", paste(names(df)[1:min(10, length(names(df)))], collapse=", ")))
  
  # Initialize results list
  race_predictions <- list()
  race_dfs <- list()
  
  # Initialize position predictions list
  position_predictions <- list()
  
  # Define position thresholds
  position_thresholds <- c(1, 3, 5, 10, 30)  # Win, Podium, Top 5, Top 10, Top 30
  
  # Debug: Show race probability columns in startlist
  race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
  log_info(paste("Race probability columns in startlist:", paste(race_prob_cols, collapse=", ")))
  
  # Process each race
  for(i in 1:nrow(races)) {
    race_info <- races[i, ]
    discipline <- race_info$discipline
    
    log_info(sprintf("Processing %s race %d: %s", gender, i, discipline))
    
    # Skip Team Combined races
    if(discipline == "Team Combined") {
      log_info(paste("Skipping Team Combined race", i))
      next
    }
    
    # Get race probability column name for this race
    race_prob_col <- paste0("Race", i, "_Prob")
    
    # Debug: Check if this race probability coxslumn exists in startlist
    if(race_prob_col %in% names(startlist)) {
      log_info(paste("Race probability column", race_prob_col, "exists in startlist"))
      # Show some stats
      log_info(paste("  Mean:", mean(startlist[[race_prob_col]], na.rm = TRUE)))
      log_info(paste("  Sum:", sum(startlist[[race_prob_col]], na.rm = TRUE)))
    } else {
      log_warn(paste("Race probability column", race_prob_col, "NOT FOUND in startlist!"))
    }
    
    # Filter base dataset for discipline
    race_df <- df %>%
      filter(Distance == discipline)
    
    # Get relevant Elo column based on discipline (match column names in data)
    if(discipline == "Downhill") {
      elo_col <- "Downhill_Elo_Pct"
    } else if(discipline == "Super G") {
      elo_col <- "Super.G_Elo_Pct"
    } else if(discipline == "Giant Slalom") {
      elo_col <- "Giant.Slalom_Elo_Pct" 
    } else if(discipline == "Slalom") {
      elo_col <- "Slalom_Elo_Pct"
    } else if(discipline %in% c("Combined", "Alpine Combined")) {
      elo_col <- "Combined_Elo_Pct"
    } else {
      elo_col <- "Elo_Pct"
    }
    
    # Define explanatory variables based on discipline
    if(discipline %in% c("Downhill", "Super G")) {
      explanatory_vars <- c("Prev_Points_Weighted", 
                            "Downhill_Elo_Pct", "Super.G_Elo_Pct", "Giant.Slalom_Elo_Pct", "Speed_Elo_Pct", "Elo_Pct")
    } else if(discipline %in% c("Slalom", "Giant Slalom")) {
      explanatory_vars <- c("Prev_Points_Weighted", "Super.G_Elo_Pct",
                            "Slalom_Elo_Pct", "Giant.Slalom_Elo_Pct", "Tech_Elo_Pct", "Elo_Pct")
    } else {
      explanatory_vars <- c("Prev_Points_Weighted", 
                            "Combined_Elo_Pct", "Tech_Elo_Pct", "Speed_Elo_Pct", "Elo_Pct")
    }
    
    # Filter for top performers and add previous points
    race_df_75 <- race_df %>%
      filter(get(elo_col) > 0.75) %>%
      group_by(!!sym(participant_col)) %>%
      arrange(Season, Race) %>%
      ungroup()
    
    # NEW CODE: Create position probability models using the same variables as points model
    position_models <- list()
    position_adjustments <- list()  # To store adjustments for each threshold
    
    # Feature selection - use the same explanatory variables as the points model
    position_feature_vars <- explanatory_vars
    
    # Create models for each position threshold
    for(threshold in position_thresholds) {
      log_info(paste("Creating model for top", threshold, "positions"))
      
      # Create binary outcome variable for position threshold
      race_df$position_achieved <- race_df$Place <= threshold
      
      # Create formula for regsubsets using the same explanatory variables as the points model
      pos_formula <- as.formula(paste("position_achieved ~", paste(position_feature_vars, collapse = " + ")))
      
      # Use regsubsets to select best features for this position threshold
      tryCatch({
        pos_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")
        pos_summary <- summary(pos_selection)
        pos_best_bic_vars <- names(coef(pos_selection, which.min(pos_summary$bic)))
        
        # Create smooth terms for GAM using best BIC variables (remove intercept)
        pos_smooth_terms <- paste("s(", pos_best_bic_vars[-1], ")", collapse=" + ")
        pos_gam_formula <- as.formula(paste("position_achieved ~", pos_smooth_terms))
        
        # Fit the position model with binomial family
        position_model <- gam(pos_gam_formula,
                              data = race_df,
                              family = binomial,
                              method = "REML")
        
        # Calculate Brier score for model evaluation
        predicted_probs <- predict(position_model, newdata = race_df, type = "response")
        brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
        log_info(paste("Brier score for threshold", threshold, ":", round(brier_score, 4)))
        
        # Log selected variables
        log_info(paste("Variables selected for", threshold, "position model:", 
                       paste(pos_best_bic_vars[-1], collapse=", ")))
        
        # Store the model
        position_models[[paste0("threshold_", threshold)]] <- position_model
        
        # Calculate adjustments for period for this threshold
        position_df <- race_df %>%
          arrange(Date) %>%
          group_by(!!sym(participant_col)) %>%
          mutate(
            row_id = row_number()
          ) %>%
          ungroup()
        
        # Add predictions separately (outside of mutate)
        position_df$initial_prob <- predict(position_model, newdata = position_df, type = "response")
        
        # Continue with period adjustment calculations
        position_df <- position_df %>%
          group_by(!!sym(participant_col)) %>%
          mutate(
            prob_diff = as.numeric(position_achieved) - initial_prob,
            
            # Calculate period adjustments
            period_p = purrr::map_dbl(row_id, function(r) {
              if(r <= 1) return(1)
              prior_period_curr <- prob_diff[Period == Period[r] & row_id < r]
              prior_period_other <- prob_diff[Period != Period[r] & row_id < r]
              if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
              tryCatch({
                t.test(prior_period_curr, prior_period_other)$p.value
              }, error = function(e) 1)
            }),
            period_correction = ifelse(period_p < 0.05,
                                       mean(prob_diff[Period == Period], na.rm = TRUE),
                                       0),
            period_adjusted = pmin(pmax(initial_prob + period_correction, 0), 1)
          ) %>%
          ungroup()
        
        # Get final adjustments for each participant
        participant_pos_adjustments <- position_df %>%
          group_by(!!sym(participant_col)) %>%
          summarise(
            period_effect = last(period_correction)
          )
        
        # Store adjustments for this threshold
        position_adjustments[[paste0("threshold_", threshold)]] <- participant_pos_adjustments
        
      }, error = function(e) {
        log_warn(paste("Error in position model for threshold", threshold, ":", e$message))
        
        # Create a simpler fallback model with just the elo column
        fallback_vars <- c("Prev_Points_Weighted", elo_col)
        fallback_vars <- fallback_vars[fallback_vars %in% names(race_df)]
        
        if(length(fallback_vars) > 0) {
          fallback_terms <- paste("s(", fallback_vars, ")", collapse=" + ")
          fallback_formula <- as.formula(paste("position_achieved ~", fallback_terms))
          
          position_models[[paste0("threshold_", threshold)]] <- gam(
            fallback_formula,
            data = race_df,
            family = binomial,
            method = "REML"
          )
          
          # Create empty adjustments object since we can't calculate them for the fallback model
          empty_adjustments <- data.frame(x = unique(race_df[[participant_col]]))
          names(empty_adjustments)[1] <- participant_col
          empty_adjustments$period_effect <- 0
          position_adjustments[[paste0("threshold_", threshold)]] <- empty_adjustments
          
          log_info(paste("Created fallback model for threshold", threshold, 
                         "using variables:", paste(fallback_vars, collapse=", ")))
        } else {
          # Last resort fallback - just use the elo column (same as weekly-picks2.R)
          fallback_formula <- as.formula(paste("position_achieved ~ s(", elo_col, ")"))
          position_models[[paste0("threshold_", threshold)]] <- gam(
            fallback_formula,
            data = race_df,
            family = binomial,
            method = "REML"
          )
          
          # Create empty adjustments object
          empty_adjustments <- data.frame(x = unique(race_df[[participant_col]]))
          names(empty_adjustments)[1] <- participant_col
          empty_adjustments$period_effect <- 0
          position_adjustments[[paste0("threshold_", threshold)]] <- empty_adjustments
          
          log_info(paste("Created last-resort fallback model for threshold", threshold, 
                         "using only", elo_col))
        }
      })
    }
    
    # Prepare startlist data
    startlist_prepared <- prepare_startlist_data(startlist, race_df, elo_col)
    
    # Ensure race probability column exists
    if(!(race_prob_col %in% names(startlist_prepared))) {
      log_warn(paste("Race probability column missing:", race_prob_col))
      if(race_prob_col %in% names(startlist)) {
        # Copy from original startlist if available
        log_info("Copying from original startlist")
        startlist_prepared[[race_prob_col]] <- startlist[match(startlist_prepared[[participant_col]], startlist[[participant_col]]), race_prob_col]
      } else {
        # Default to race probabilities from startlist
        log_info("Setting default probabilities")
        startlist_prepared[[race_prob_col]] <- if(i == 1) 1 else 0
      }
    }
    
    # NEW CODE: Make position probability predictions with adjustments
    position_preds <- data.frame(startlist_prepared[[participant_col]])
    names(position_preds)[1] <- participant_col
    
    # Add Nation for individual races
    position_preds$Nation <- startlist_prepared$Nation
    position_preds$Sex <- ifelse(gender == "men", "M", "L")
    
    # Add race number
    position_preds$Race <- i
    
    # Add race probability column for later normalization
    if(race_prob_col %in% names(startlist_prepared)) {
      position_preds[[race_prob_col]] <- startlist_prepared[[race_prob_col]]
    }
    
    # Make predictions for each threshold
    for(threshold in position_thresholds) {
      model_name <- paste0("threshold_", threshold)
      adj_name <- paste0("threshold_", threshold)
      prob_col <- paste0("prob_top", threshold)
      
      if(model_name %in% names(position_models)) {
        tryCatch({
          # Get the model
          pos_model <- position_models[[model_name]]
          
          # Make predictions with explicit try-catch
          base_predictions <- tryCatch({
            # Debug output
            log_info(paste("Attempting prediction for threshold", threshold, "with", nrow(startlist_prepared), "rows"))
            
            # Explicit call to mgcv::predict.gam to avoid method dispatch issues
            mgcv::predict.gam(pos_model, newdata = startlist_prepared, type = "response")
          }, error = function(e) {
            log_warn(paste("Prediction call failed:", e$message))
            
            # Try alternative prediction approach with one row at a time
            log_info("Trying row-by-row prediction as fallback")
            result <- numeric(nrow(startlist_prepared))
            
            for(j in 1:nrow(startlist_prepared)) {
              single_row <- startlist_prepared[j,, drop = FALSE]
              result[j] <- tryCatch({
                mgcv::predict.gam(pos_model, newdata = single_row, type = "response")
              }, error = function(e2) {
                log_warn(paste("Failed on row", j, ":", e2$message))
                threshold/100  # Default value based on threshold
              })
            }
            return(result)
          })
          
          # Store predictions
          position_preds[[paste0(prob_col, "_base")]] <- base_predictions
          
          # Apply adjustments if available
          if(adj_name %in% names(position_adjustments)) {
            # Get adjustments
            pos_adj <- position_adjustments[[adj_name]]
            
            # Join with predictions
            position_preds <- position_preds %>%
              left_join(pos_adj, by = participant_col) %>%
              mutate(
                # Replace NAs with zeros
                period_effect = replace_na(period_effect, 0),
                
                # Apply adjustments
                period_adjustment = period_effect,
                
                # Calculate adjusted probabilities
                adjusted_prob = get(paste0(prob_col, "_base")) + period_adjustment,
                
                # Ensure probabilities are between 0 and 1
                adjusted_prob = pmin(pmax(adjusted_prob, 0), 1)
              )
            
            # Use adjusted probability as final
            position_preds[[prob_col]] <- position_preds$adjusted_prob
            
            # Clean up temporary columns
            position_preds <- position_preds %>%
              dplyr::select(-period_effect, -period_adjustment, -adjusted_prob)
          } else {
            # Use base prediction if no adjustments
            position_preds[[prob_col]] <- position_preds[[paste0(prob_col, "_base")]]
          }
          
          # Clean up base prediction column
          position_preds <- position_preds %>%
            dplyr::select(-paste0(prob_col, "_base"))
          
          # Convert to percentage and round
          position_preds[[prob_col]] <- round(position_preds[[prob_col]] * 100, 1)
          
          log_info(paste("Made predictions with adjustments for position threshold", threshold))
          
        }, error = function(e) {
          log_warn(paste("Complete failure for threshold", threshold, ":", e$message))
          # Set a reasonable default based on threshold (1% for top1, 3% for top3, etc.)
          position_preds[[prob_col]] <- rep(threshold, nrow(position_preds))
        })
      } else {
        log_warn(paste("No model found for threshold", threshold))
        position_preds[[prob_col]] <- NA
      }
    }
    
    # Normalize position probabilities to ensure they sum to the correct totals
    position_preds <- normalize_position_probabilities(position_preds, race_prob_col, position_thresholds)
    
    # Add verification logging for each threshold
    log_info(sprintf("Race %d position probability sums after normalization:", i))
    for(threshold in position_thresholds) {
      prob_col <- paste0("prob_top", threshold)
      if(prob_col %in% names(position_preds)) {
        sum_val <- sum(position_preds[[prob_col]], na.rm = TRUE)
        log_info(sprintf("  %s: %.2f%% (should be %d%%)", 
                         prob_col, sum_val, 100 * threshold))
      }
    }
    
    # Store position predictions for this race
    position_predictions[[i]] <- position_preds
  }
  
  # Get number of races from races dataframe
  n_races <- nrow(races)
  
  # Combine all position predictions into one dataframe
  all_position_predictions <- bind_rows(position_predictions)
  
  # Create summary by athlete from position predictions
  athlete_summary <- all_position_predictions %>%
    group_by(Skier, Nation) %>%
    summarise(
      Avg_Win_Prob = mean(prob_top1, na.rm = TRUE),
      Avg_Podium_Prob = mean(prob_top3, na.rm = TRUE),
      Avg_Top5_Prob = mean(prob_top5, na.rm = TRUE),
      Avg_Top10_Prob = mean(prob_top10, na.rm = TRUE),
      Races_Participating = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(Avg_Win_Prob))
  
  # Create output directory
  champs_date <- format(Sys.Date(), "%Y%m%d")
  dir_path <- paste0("~/blog/daehl-e/content/post/alpine/drafts/champs-predictions/", champs_date)
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Save athlete summary
  summary_file <- file.path(dir_path, paste0(gender, ".xlsx"))
  write.xlsx(athlete_summary, summary_file)
  log_info(paste("Saved", gender, "Championships summary to", summary_file))
  
  # Save detailed race-by-race results
  race_dfs <- list()
  for(race_num in unique(all_position_predictions$Race)) {
    race_data <- all_position_predictions[all_position_predictions$Race == race_num, ]
    race_data <- race_data %>%
      dplyr::select(Skier, Nation, prob_top1, prob_top3, prob_top5, prob_top10, prob_top30) %>%
      rename(
        Win_Prob = prob_top1,
        Podium_Prob = prob_top3,
        Top5_Prob = prob_top5,
        Top10_Prob = prob_top10,
        Top30_Prob = prob_top30
      ) %>%
      arrange(desc(Win_Prob))
    
    # Get race discipline for sheet naming
    race_disciplines <- champs_races %>%
      filter(Sex == ifelse(gender == "men", "M", "L")) %>%
      slice(race_num) %>%
      pull(Distance)
    
    discipline <- if(length(race_disciplines) > 0) race_disciplines[1] else paste("Race", race_num)
    sheet_name <- paste(ifelse(gender == "men", "Men", "Ladies"), discipline)
    race_dfs[[sheet_name]] <- race_data
  }
  
  # Save race-by-race results
  race_file <- file.path(dir_path, paste0(gender, "_position_probabilities.xlsx"))
  write.xlsx(race_dfs, race_file)
  log_info(paste("Saved", gender, "race probabilities to", race_file))
  
  return(list(
    summary = athlete_summary,
    race_results = all_position_predictions,
    race_sheets = race_dfs
  ))
}

# Calculate race probabilities for Championships (like weekly-picks2.R but with 4-person quota)
calculate_championships_race_probabilities <- function() {
  log_info("Calculating Championships race participation probabilities with 4-person quota constraint")
  
  # Function to get base race probability for a skier (same as weekly-picks2.R)
  get_base_race_probability <- function(chronos, participant, discipline) {
    # Get participant's first ever race date
    participant_first_race <- chronos %>%
      filter(Skier == participant) %>%
      arrange(Date) %>%
      slice(1) %>%
      pull(Date)
    
    # Calculate date from 5 years ago
    five_years_ago <- Sys.Date() - (5 * 365)
    
    # Use 5 years ago or participant's first race, whichever is later
    start_date <- if(length(participant_first_race) == 0) {
      five_years_ago
    } else {
      max(five_years_ago, participant_first_race, na.rm = TRUE)
    }
    
    # Count all races in this discipline since start_date
    all_races <- chronos %>%
      filter(Date >= start_date, Distance == discipline) %>%
      distinct(Date, City) %>%
      nrow()
    
    # Count participant's races in this discipline since start_date
    participant_races <- chronos %>%
      filter(Date >= start_date, Skier == participant, Distance == discipline) %>%
      distinct(Date, City) %>%
      nrow()
    
    # Calculate base probability (capped at 1)
    if(all_races == 0) return(0)
    base_prob <- min(1, participant_races / all_races)
    
    return(base_prob)
  }
  
  # Process Championships startlists and add race probabilities
  if(nrow(men_races) > 0) {
    log_info("Processing men's Championships race probabilities")
    men_chrono <- read.csv("~/ski/elo/python/alpine/polars/excel365/men_chrono.csv", 
                           stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
    
    men_startlist <- read.csv("~/ski/elo/python/alpine/polars/excel365/startlist_champs_men.csv", 
                              stringsAsFactors = FALSE)
    
    # Calculate base probabilities for each race/discipline
    for(i in 1:nrow(men_races)) {
      race_col <- paste0("Race", i, "_Prob")
      discipline <- men_races$discipline[i]
      
      men_startlist[[race_col]] <- sapply(men_startlist$Skier, function(skier) {
        get_base_race_probability(men_chrono, skier, discipline)
      })
    }
    
    # Apply 4-person quota constraint per nation per race
    for(i in 1:nrow(men_races)) {
      race_col <- paste0("Race", i, "_Prob")
      
      # For each nation, normalize probabilities so sum ≈ 4
      for(nation in unique(men_startlist$Nation)) {
        nation_mask <- men_startlist$Nation == nation
        nation_probs <- men_startlist[nation_mask, race_col]
        current_sum <- sum(nation_probs, na.rm = TRUE)
        
        if(current_sum > 0) {
          # Scale to target 4 participants per nation
          scaling_factor <- 4 / current_sum
          scaled_probs <- nation_probs * scaling_factor
          # Cap individual probabilities at 1.0
          scaled_probs <- pmin(scaled_probs, 1.0)
          men_startlist[nation_mask, race_col] <- scaled_probs
        }
      }
    }
    
    # Save updated startlist
    write.csv(men_startlist, 
              "~/ski/elo/python/alpine/polars/excel365/startlist_champs_men.csv", 
              row.names = FALSE)
  }
  
  if(nrow(ladies_races) > 0) {
    log_info("Processing ladies' Championships race probabilities")
    ladies_chrono <- read.csv("~/ski/elo/python/alpine/polars/excel365/ladies_chrono.csv", 
                              stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
    
    ladies_startlist <- read.csv("~/ski/elo/python/alpine/polars/excel365/startlist_champs_ladies.csv", 
                                 stringsAsFactors = FALSE)
    
    # Calculate base probabilities for each race/discipline
    for(i in 1:nrow(ladies_races)) {
      race_col <- paste0("Race", i, "_Prob")
      discipline <- ladies_races$discipline[i]
      
      ladies_startlist[[race_col]] <- sapply(ladies_startlist$Skier, function(skier) {
        get_base_race_probability(ladies_chrono, skier, discipline)
      })
    }
    
    # Apply 4-person quota constraint per nation per race
    for(i in 1:nrow(ladies_races)) {
      race_col <- paste0("Race", i, "_Prob")
      
      # For each nation, normalize probabilities so sum ≈ 4
      for(nation in unique(ladies_startlist$Nation)) {
        nation_mask <- ladies_startlist$Nation == nation
        nation_probs <- ladies_startlist[nation_mask, race_col]
        current_sum <- sum(nation_probs, na.rm = TRUE)
        
        if(current_sum > 0) {
          # Scale to target 4 participants per nation
          scaling_factor <- 4 / current_sum
          scaled_probs <- nation_probs * scaling_factor
          # Cap individual probabilities at 1.0
          scaled_probs <- pmin(scaled_probs, 1.0)
          ladies_startlist[nation_mask, race_col] <- scaled_probs
        }
      }
    }
    
    # Save updated startlist
    write.csv(ladies_startlist, 
              "~/ski/elo/python/alpine/polars/excel365/startlist_champs_ladies.csv", 
              row.names = FALSE)
  }
  
  log_info("Championships race probability calculation complete")
}

# Calculate race probabilities before processing predictions
calculate_championships_race_probabilities()

# Process men's Championships
men_results <- NULL
if(nrow(men_races) > 0) {
  men_results <- process_gender_championships("men", men_races)
}

# Process ladies' Championships  
ladies_results <- NULL
if(nrow(ladies_races) > 0) {
  ladies_results <- process_gender_championships("ladies", ladies_races)
}

log_info("Alpine Championships predictions completed successfully")

