# Nordic Combined Championships Predictions: Based on weekly-picks2.R with Championship modifications
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

# Define points systems for Nordic Combined
# World Cup: Top 30 get points
world_cup_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Function to replace NAs with first quartile value
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Set up logging
log_dir <- "~/ski/elo/python/nordic-combined/polars/excel365/champs-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "champs_picks_processing.log")))
log_info("Starting Nordic Combined Championships predictions process")

# Read in the race schedule from weekends.csv with proper date parsing
log_info("Reading weekends data")
weekends <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/weekends.csv", 
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

# Create race dataframes for men and ladies (individual races)
# Add row numbers first to preserve original race order
champs_races_with_race_num <- champs_races %>%
  mutate(OriginalRaceNum = row_number())

men_races <- champs_races_with_race_num %>%
  filter(Sex == "M", 
         !grepl("Team", RaceType, ignore.case = TRUE)) %>%  # Exclude any race type containing "Team"
  dplyr::select(RaceType, Period, Country, OriginalRaceNum) %>%
  rename(race_type = RaceType, period = Period, country = Country, original_race_num = OriginalRaceNum)

ladies_races <- champs_races_with_race_num %>%
  filter(Sex == "L", 
         !grepl("Team", RaceType, ignore.case = TRUE)) %>%  # Exclude any race type containing "Team"
  dplyr::select(RaceType, Period, Country, OriginalRaceNum) %>%
  rename(race_type = RaceType, period = Period, country = Country, original_race_num = OriginalRaceNum)

# Create race dataframes for teams
men_teams <- champs_races_with_race_num %>%
  filter(grepl("Team", RaceType, ignore.case = TRUE) & Sex == "M") %>%  # RaceType contains "Team" and Sex is "M"
  dplyr::select(RaceType, Period, Country, OriginalRaceNum) %>%
  rename(race_type = RaceType, period = Period, country = Country, original_race_num = OriginalRaceNum)

ladies_teams <- champs_races_with_race_num %>%
  filter(grepl("Team", RaceType, ignore.case = TRUE) & Sex == "L") %>%  # RaceType contains "Team" and Sex is "L"
  dplyr::select(RaceType, Period, Country, OriginalRaceNum) %>%
  rename(race_type = RaceType, period = Period, country = Country, original_race_num = OriginalRaceNum)

# Mixed team races (Sex == "Mixed")
mixed_teams <- champs_races_with_race_num %>%
  filter(Sex == "Mixed") %>%  # Mixed team events have Sex == "Mixed"
  dplyr::select(RaceType, Period, Country, OriginalRaceNum) %>%
  rename(race_type = RaceType, period = Period, country = Country, original_race_num = OriginalRaceNum)

log_info(paste("Found", nrow(men_races), "men's individual races,", nrow(ladies_races), "ladies' individual races"))
log_info(paste("Found", nrow(men_teams), "men's team races,", nrow(ladies_teams), "ladies' team races"))
log_info(paste("Found", nrow(mixed_teams), "mixed team races"))

# Function to get points based on place for Nordic Combined
get_points <- function(place, race_type = NULL) {
  # All Nordic Combined events use World Cup points system
  points_list <- world_cup_points
  
  if (place >= 1 && place <= length(points_list)) {
    return(points_list[place])
  }
  return(0)
}

# Function to enforce probability constraints: Win <= Podium <= Top5
enforce_probability_constraints <- function(predictions) {
  # Ensure logical ordering of probabilities
  if("prob_top1" %in% names(predictions) && "prob_top3" %in% names(predictions)) {
    predictions$prob_top3 <- pmax(predictions$prob_top3, predictions$prob_top1)
  }
  
  if("prob_top3" %in% names(predictions) && "prob_top5" %in% names(predictions)) {
    predictions$prob_top5 <- pmax(predictions$prob_top5, predictions$prob_top3)
  }
  
  if("prob_top1" %in% names(predictions) && "prob_top5" %in% names(predictions)) {
    predictions$prob_top5 <- pmax(predictions$prob_top5, predictions$prob_top1)
  }
  
  return(predictions)
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
  
  # Check if this is a team startlist
  is_team <- "Nation" %in% names(startlist) && !"Skier" %in% names(startlist)
  participant_col <- if(is_team) "Nation" else "Skier"
  
  if(is_team) {
    # The team startlist already has Avg_* columns, use them directly
    result_df <- startlist %>%
      # Select relevant columns from the team startlist
      dplyr::select(Nation, any_of(race_prob_cols), starts_with("Avg_"))
    
    log_info(paste("Using team startlist with", nrow(result_df), "teams"))
    
    # Calculate team Prev_Points_Weighted for prediction startlist
    # Extract race type from elo_col (e.g., "Avg_Individual_Elo_Pct" -> "Individual")
    race_type_from_col <- gsub("Avg_(.+)_Elo_Pct", "\\1", elo_col)
    
    tryCatch({
      # Determine individual chrono path based on race data
      if("Mixed" %in% race_df$Sex) {
        # Mixed teams - load both chrono files
        log_info("Loading individual chrono for mixed team predictions")
        men_chrono <- read.csv("~/ski/elo/python/nordic-combined/polars/relay/excel365/men_chrono.csv", stringsAsFactors = FALSE) %>%
          mutate(Date = as.Date(Date), Points = sapply(Place, get_points))
        ladies_chrono <- read.csv("~/ski/elo/python/nordic-combined/polars/relay/excel365/ladies_chrono.csv", stringsAsFactors = FALSE) %>%
          mutate(Date = as.Date(Date), Points = sapply(Place, get_points))
        individual_chrono <- bind_rows(men_chrono, ladies_chrono)
      } else {
        # Regular teams - load appropriate gender chrono
        gender_for_chrono <- if("M" %in% race_df$Sex) "men" else "ladies"
        chrono_path <- paste0("~/ski/elo/python/nordic-combined/polars/relay/excel365/", gender_for_chrono, "_chrono.csv")
        log_info(paste("Loading individual chrono for team predictions:", chrono_path))
        individual_chrono <- read.csv(chrono_path, stringsAsFactors = FALSE) %>%
          mutate(Date = as.Date(Date), Points = sapply(Place, get_points))
      }
      
      # Get current date for team event (use most recent date from race_df)
      current_event_date <- max(race_df$Date, na.rm = TRUE)
      
      # Calculate Prev_Points_Weighted for each team in startlist
      if("TeamMembers" %in% names(startlist)) {
        # Team startlist has TeamMembers column
        result_df <- result_df %>%
          left_join(
            startlist %>% dplyr::select(Nation, TeamMembers),
            by = "Nation"
          ) %>%
          rowwise() %>%
          mutate(
            Prev_Points_Weighted = {
              if(!is.na(TeamMembers) && TeamMembers != "") {
                team_members <- trimws(strsplit(TeamMembers, ",")[[1]])
                calculate_team_prev_points(team_members, current_event_date, race_type_from_col, individual_chrono)
              } else {
                0
              }
            }
          ) %>%
          ungroup() %>%
          dplyr::select(-TeamMembers)  # Remove TeamMembers after calculation
      } else {
        # No TeamMembers column, set default
        result_df$Prev_Points_Weighted <- 0
      }
      
    }, error = function(e) {
      log_warn(paste("Error calculating team Prev_Points_Weighted:", e$message))
      result_df$Prev_Points_Weighted <- 0
    })
  } else {
    # For individual races
    elo_cols <- c("Sprint_Elo", "Individual_Elo", "MassStart_Elo", "IndividualCompact_Elo", "Elo")

    # Keep essential columns from startlist including Elo columns (already from chrono_pred via Python)
    available_elo_cols <- intersect(elo_cols, names(startlist))
    log_info(paste("Available Elo columns in startlist:", paste(available_elo_cols, collapse=", ")))

    base_df <- startlist %>%
      dplyr::select(Skier, Nation, Price, all_of(race_prob_cols), any_of(elo_cols))

    # Get recent points for individuals
    recent_points <- race_df %>%
      dplyr::filter(Skier %in% base_df$Skier) %>%
      group_by(Skier) %>%
      arrange(Season, Race) %>%
      slice_tail(n = 5) %>%
      summarise(
        Prev_Points_Weighted = if(n() > 0)
          weighted.mean(Points, w = seq(n(), 1), na.rm = TRUE)  # Most recent gets highest weight
        else 0,
        .groups = 'drop'
      )

    # Combine all data - Elos already in base_df from startlist, just add points
    result_df <- base_df %>%
      left_join(recent_points, by = "Skier")
  }
  
  # Debug: Check elo columns
  log_info(paste("Available elo columns:", paste(names(result_df), collapse=", ")))
  
  # Set up Elo columns to process based on team vs individual
  if(is_team) {
    elo_columns_to_process <- c("Avg_Individual_Elo", "Avg_IndividualCompact_Elo", "Avg_MassStart_Elo", "Avg_Elo")
  } else {
    elo_columns_to_process <- c("Individual_Elo", "IndividualCompact_Elo", "MassStart_Elo", "Elo")
  }
  
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
      )
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

# Preprocessing function for historical race data (adapted for nordic combined)
preprocess_data <- function(df) {
  # Load weekends data to determine points systems for historical races
  weekends_data <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/weekends.csv", 
                            stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date, format="%Y-%m-%d"))
  
  participant_col <- "Skier"
  
  # First calculate points using data with appropriate points system
  df_with_points <- df %>%
    # Add points based on placement if they don't already exist
    mutate(
      Points = if("Points" %in% names(df)) {
        Points
      } else {
        sapply(Place, get_points)
      }
    ) %>%
    # Sort
    arrange(Season, Race, Place)
  
  # Calculate weighted previous points separately for each race type
  df_with_points <- df_with_points %>%
    # Group by Skier and race type
    group_by(!!sym(participant_col), RaceType) %>%
    arrange(Season, Race) %>%
    mutate(Prev_Points_Weighted = sapply(1:n(), function(j) {
      if (j == 1) return(0)
      start_index <- max(1, j - 5)
      num_races <- j - start_index
      weights <- seq(num_races, 1)  # Most recent race gets highest weight
      weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
    })) %>%
    ungroup()
  
  # Check if Elo and Pelo columns exist, if not create them
  elo_cols <- c("Individual_Elo", "IndividualCompact_Elo", "MassStart_Elo", "Elo")
  pelo_cols <- c("Individual_Pelo", "IndividualCompact_Pelo", "MassStart_Pelo", "Pelo")
  
  # Make sure these columns exist (create if missing)
  for (col in c(elo_cols, pelo_cols)) {
    if (!col %in% names(df_with_points)) {
      log_info(paste("Creating missing column:", col))
      df_with_points[[col]] <- 0
    }
  }
  
  # Now apply other preprocessing steps and filter for recent data
  processed_df <- df_with_points %>%
    # Add period (Nordic Combined has 4 periods per season)
    group_by(Season) %>%
    mutate(
      Num_Races = max(Race),
      Period = case_when(
        Num_Races <= 8 ~ 1,   # Early season (Nov-Dec)
        Num_Races <= 16 ~ 2,  # Mid season (Jan)
        Num_Races <= 24 ~ 3,  # Late season (Feb)
        TRUE ~ 4              # Final season (Mar)
      )
    ) %>%
    ungroup() %>%
    # Filter relevant races and add cumulative points
    filter(
      Season >= max(Season-10),
      # Filter for individual race types only (exclude Offseason)
      RaceType %in% c("Mass Start", "IndividualCompact", "Individual")
    ) %>%
    group_by(!!sym(participant_col), Season) %>%
    mutate(Cumulative_Points = cumsum(Points)) %>%
    ungroup() %>%
    # Handle NAs and calculate percentages
    group_by(Season, Race) %>%
    mutate(
      across(
        all_of(c(elo_cols, pelo_cols)),
        ~replace_na_with_quartile(.x)
      )
    ) %>%
    # Calculate percentages for both Elo (for prediction) and Pelo (for training) columns
    mutate(
      across(
        all_of(elo_cols),
        ~{
          max_val <- max(.x, na.rm = TRUE)
          if (max_val == 0) return(rep(0, length(.x)))
          .x / max_val
        },
        .names = "{.col}_Pct"
      ),
      across(
        all_of(pelo_cols),
        ~{
          max_val <- max(.x, na.rm = TRUE)
          if (max_val == 0) return(rep(0, length(.x)))
          .x / max_val
        },
        .names = "{.col}_Pct"
      )
    ) %>%
    ungroup()
  
  # Ensure all required percentage columns exist
  elo_pct_cols <- paste0(elo_cols, "_Pct")
  pelo_pct_cols <- paste0(pelo_cols, "_Pct")
  pct_cols <- c(elo_pct_cols, pelo_pct_cols)
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
  startlist_file <- paste0("~/ski/elo/python/nordic-combined/polars/excel365/startlist_champs_", gender, ".csv")
  
  if (!file.exists(startlist_file)) {
    log_info(paste("Championships startlist not found:", startlist_file))
    return(NULL)
  }
  
  startlist <- read.csv(startlist_file, stringsAsFactors = FALSE)
  log_info(paste("Read Championships startlist with", nrow(startlist), "athletes"))
  
  # Determine the race type and paths based on gender
  chrono_path <- paste0("~/ski/elo/python/nordic-combined/polars/excel365/", gender, "_chrono.csv")
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
    race_type <- race_info$race_type
    
    log_info(sprintf("Processing %s race %d: %s", gender, i, race_type))
    
    # Get race probability column name for this race
    race_prob_col <- paste0("Race", i, "_Prob")
    
    # Debug: Check if this race probability column exists in startlist
    if(race_prob_col %in% names(startlist)) {
      log_info(paste("Race probability column", race_prob_col, "exists in startlist"))
      # Show some stats
      log_info(paste("  Mean:", mean(startlist[[race_prob_col]], na.rm = TRUE)))
      log_info(paste("  Sum:", sum(startlist[[race_prob_col]], na.rm = TRUE)))
    } else {
      log_warn(paste("Race probability column", race_prob_col, "NOT FOUND in startlist!"))
    }
    
    # Filter base dataset for race type
    race_df <- df %>%
      filter(RaceType == race_type)
    
    # Get relevant columns for training (Pelo_Pct) and prediction (Elo_Pct)
    if(race_type == "Individual") {
      training_elo_col <- "Individual_Pelo_Pct"  # Pre-race ELO for training
      prediction_elo_col <- "Individual_Elo_Pct"  # Post-race ELO for prediction
    } else if(race_type == "IndividualCompact") {
      training_elo_col <- "IndividualCompact_Pelo_Pct"
      prediction_elo_col <- "IndividualCompact_Elo_Pct"
    } else if(race_type == "Mass Start") {
      training_elo_col <- "MassStart_Pelo_Pct"
      prediction_elo_col <- "MassStart_Elo_Pct"
    } else {
      training_elo_col <- "Pelo_Pct"
      prediction_elo_col <- "Elo_Pct"
    }
    
    # Use all PELO columns as explanatory variables for training (pre-race data)
    explanatory_vars <- c("Prev_Points_Weighted", 
                          "Individual_Pelo_Pct", "IndividualCompact_Pelo_Pct", "MassStart_Pelo_Pct", "Pelo_Pct")
    
    # Filter for top performers and add previous points (use training ELO for filtering)
    race_df_75 <- race_df %>%
      filter(get(training_elo_col) > 0.75) %>%
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
        fallback_vars <- c("Prev_Points_Weighted", training_elo_col)
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
          # Last resort fallback - just use the elo column
          fallback_formula <- as.formula(paste("position_achieved ~ s(", training_elo_col, ")"))
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
                         "using only", training_elo_col))
        }
      })
    }

    # Prepare startlist data - use prediction ELO (post-race) for startlist
    startlist_prepared <- prepare_startlist_data(startlist, race_df, prediction_elo_col)
    
    # Create Pelo_Pct columns for prediction by copying from Elo_Pct columns
    # The model was trained on Pelo_Pct but for prediction we use current Elo_Pct values
    pelo_pct_columns <- c("Individual_Pelo_Pct", "IndividualCompact_Pelo_Pct", "MassStart_Pelo_Pct", "Pelo_Pct")
    elo_pct_columns <- c("Individual_Elo_Pct", "IndividualCompact_Elo_Pct", "MassStart_Elo_Pct", "Elo_Pct")
    
    for(j in seq_along(pelo_pct_columns)) {
      pelo_col <- pelo_pct_columns[j]
      elo_col <- elo_pct_columns[j]
      if(elo_col %in% names(startlist_prepared)) {
        startlist_prepared[[pelo_col]] <- startlist_prepared[[elo_col]]
        log_info(paste("Created", pelo_col, "from", elo_col, "for prediction"))
      } else {
        startlist_prepared[[pelo_col]] <- 0.5  # Default value
        log_warn(paste("Missing", elo_col, "- using default for", pelo_col))
      }
    }
    
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
    
    # Add original race number
    position_preds$Race <- race_info$original_race_num
    
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
            
            for(k in 1:nrow(startlist_prepared)) {
              single_row <- startlist_prepared[k,, drop = FALSE]
              result[k] <- tryCatch({
                mgcv::predict.gam(pos_model, newdata = single_row, type = "response")
              }, error = function(e2) {
                log_warn(paste("Failed on row", k, ":", e2$message))
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
    
    # Enforce probability constraints AFTER normalization: Win <= Podium <= Top5
    position_preds <- enforce_probability_constraints(position_preds)
    
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
    
    # Store position predictions for this race (use original race number as key)
    position_predictions[[as.character(race_info$original_race_num)]] <- position_preds
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
  dir_path <- paste0("~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions/", champs_date)
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Save athlete summary
  summary_file <- file.path(dir_path, paste0(gender, ".xlsx"))
  write.xlsx(athlete_summary, summary_file)
  log_info(paste("Saved", gender, "Championships summary to", summary_file))
  
  # Save detailed race-by-race results
  race_dfs <- list()
  unique_races <- unique(all_position_predictions$Race)
  log_info(paste("Creating sheets for races:", paste(unique_races, collapse=", ")))
  
  for(race_num in unique_races) {
    log_info(paste("Processing sheet for race", race_num))
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
    
    # Get race type for sheet naming using original race number
    race_types <- champs_races_with_race_num %>%
      filter(Sex == ifelse(gender == "men", "M", "L"), OriginalRaceNum == race_num) %>%
      pull(RaceType)
    
    race_type <- if(length(race_types) > 0) race_types[1] else paste("Race", race_num)
    sheet_name <- paste(ifelse(gender == "men", "Men", "Ladies"), race_type)
    
    log_info(paste("Race", race_num, "- Race type:", race_type, "- Sheet name:", sheet_name))
    log_info(paste("Race data dimensions:", nrow(race_data), "x", ncol(race_data)))
    
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

# Function to calculate team Prev_Points_Weighted from team members
calculate_team_prev_points <- function(team_members, current_date, race_type, individual_chrono) {
  if(length(team_members) == 0) return(0)
  
  # Get points for each team member
  member_points <- sapply(team_members, function(member) {
    member_data <- individual_chrono %>%
      filter(Skier == member, 
             RaceType %in% c("Mass Start", "IndividualCompact", "Individual"),  # All valid individual race types, no Offseason
             Date < current_date) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 5)
    
    if(nrow(member_data) == 0) return(0)
    
    # Calculate weighted average (most recent gets highest weight)
    weights <- seq(nrow(member_data), 1)
    weighted.mean(member_data$Points, w = weights, na.rm = TRUE)
  })
  
  # Return average of team member weighted points
  mean(member_points, na.rm = TRUE)
}

# Function to process team Championships (adapted from ski jumping)
process_team_championships <- function(gender, races) {
  log_info(paste("Processing", gender, "team Championships with", nrow(races), "races"))
  
  # Determine startlist file based on gender and race type
  if(gender == "mixed") {
    startlist_file <- "~/ski/elo/python/nordic-combined/polars/relay/excel365/startlist_champs_mixed_team.csv"
  } else {
    # Check if it's team sprint based on race type
    race_type <- races$race_type[1]
    if(grepl("Sprint", race_type, ignore.case = TRUE)) {
      startlist_file <- paste0("~/ski/elo/python/nordic-combined/polars/relay/excel365/startlist_champs_", gender, "_team_sprint.csv")
    } else {
      startlist_file <- paste0("~/ski/elo/python/nordic-combined/polars/relay/excel365/startlist_champs_", gender, "_team.csv")
    }
  }
  
  if (!file.exists(startlist_file)) {
    log_info(paste("Team startlist not found:", startlist_file))
    return(NULL)
  }
  
  startlist <- read.csv(startlist_file, stringsAsFactors = FALSE)
  log_info(paste("Read team startlist with", nrow(startlist), "teams"))
  
  # Load team chronological data
  if(gender == "mixed") {
    chrono_path <- "~/ski/elo/python/nordic-combined/polars/relay/excel365/mixed_team_chrono.csv"
  } else if(grepl("Sprint", races$race_type[1], ignore.case = TRUE)) {
    chrono_path <- "~/ski/elo/python/nordic-combined/polars/relay/excel365/team_sprint_chrono.csv"
  } else {
    chrono_path <- paste0("~/ski/elo/python/nordic-combined/polars/relay/excel365/", gender, "_team_chrono.csv")
  }
  
  log_info(paste("Using team chronological data from:", chrono_path))
  
  if (!file.exists(chrono_path)) {
    log_warn(paste("Team chronological data not found:", chrono_path))
    return(NULL)
  }
  
  # Load team chronological data
  df <- read.csv(chrono_path, stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date)) %>%
    # Add points based on placement
    mutate(Points = sapply(Place, get_points)) %>%
    # Filter for recent seasons
    filter(Season >= max(Season) - 10) %>%
    # Calculate weighted previous points for teams
    group_by(Nation) %>%
    arrange(Season, Race) %>%
    mutate(Prev_Points_Weighted = sapply(1:n(), function(j) {
      if (j == 1) return(0)
      start_index <- max(1, j - 5)
      num_races <- j - start_index
      weights <- seq(num_races, 1)  # Most recent race gets highest weight
      weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
    })) %>%
    ungroup()
  
  log_info("Team chronological data loaded. Available columns:")
  log_info(paste(names(df), collapse=", "))
  
  # Create percentage columns for team Elo data (teams have Avg_* columns)
  team_elo_cols <- c("Avg_Individual_Elo", "Avg_IndividualCompact_Elo", "Avg_MassStart_Elo", "Avg_Elo")
  
  # Calculate percentage columns - normalize within each race
  df <- df %>%
    group_by(Season, Race) %>%
    mutate(
      across(any_of(team_elo_cols), 
             ~{max_val <- max(.x, na.rm = TRUE); if(is.finite(max_val) && max_val > 0) .x / max_val else 0.5}, 
             .names = "{.col}_Pct")
    ) %>%
    ungroup()
  
  # Create Pelo_Pct columns by copying from Elo_Pct columns (teams don't have pre-race data)
  team_elo_pct_cols <- paste0(team_elo_cols, "_Pct")
  team_pelo_pct_cols <- c("Avg_Individual_Pelo_Pct", "Avg_IndividualCompact_Pelo_Pct", "Avg_MassStart_Pelo_Pct", "Avg_Pelo_Pct")
  
  for(i in seq_along(team_elo_pct_cols)) {
    elo_pct_col <- team_elo_pct_cols[i]
    pelo_pct_col <- team_pelo_pct_cols[i]
    
    if(elo_pct_col %in% names(df)) {
      df[[pelo_pct_col]] <- df[[elo_pct_col]]
      log_info(paste("Created", pelo_pct_col, "from", elo_pct_col))
    } else {
      df[[pelo_pct_col]] <- 0.5  # Default value
      log_warn(paste("Missing", elo_pct_col, "- created", pelo_pct_col, "with default value"))
    }
  }
  
  log_info("After percentage and Pelo column creation:")
  log_info(paste(names(df), collapse=", "))
  
  # Initialize results list
  race_predictions <- list()
  position_predictions <- list()
  position_thresholds <- c(1, 3, 5, 10)  # Fewer positions for teams
  
  # Process each race
  for(i in 1:nrow(races)) {
    race_info <- races[i, ]
    race_type <- race_info$race_type
    
    log_info(sprintf("Processing %s team race %d: %s", gender, i, race_type))
    
    # Get race probability column name for this race
    race_prob_col <- paste0("Race", i, "_Prob")
    
    # Filter dataset for race type
    race_df <- df %>%
      filter(RaceType == race_type)
    
    # Use team Elo columns for Nordic Combined teams (Pelo for training, Elo for prediction)
    if(race_type == "Team") {
      training_elo_col <- "Avg_Individual_Pelo_Pct"  # Pre-race ELO for training
      prediction_elo_col <- "Avg_Individual_Elo_Pct"  # Post-race ELO for prediction
    } else if(race_type == "Team Sprint") {
      training_elo_col <- "Avg_Individual_Pelo_Pct"  # Pre-race ELO for training
      prediction_elo_col <- "Avg_Individual_Elo_Pct"  # Post-race ELO for prediction
    } else {
      training_elo_col <- "Avg_Pelo_Pct"  # Pre-race ELO for training
      prediction_elo_col <- "Avg_Elo_Pct"  # Post-race ELO for prediction
    }
    
    # Prepare startlist data for teams
    startlist_prepared <- prepare_startlist_data(startlist, race_df, prediction_elo_col)
    
    # Create missing Pelo_Pct columns for teams by copying from Elo_Pct columns
    # Teams don't have pre-race data, so we copy current Elo values to Pelo for training
    pelo_pct_columns <- c("Avg_Individual_Pelo_Pct", "Avg_IndividualCompact_Pelo_Pct", "Avg_MassStart_Pelo_Pct", "Avg_Pelo_Pct")
    elo_pct_columns <- c("Avg_Individual_Elo_Pct", "Avg_IndividualCompact_Elo_Pct", "Avg_MassStart_Elo_Pct", "Avg_Elo_Pct")
    
    for(j in seq_along(pelo_pct_columns)) {
      pelo_col <- pelo_pct_columns[j]
      elo_col <- elo_pct_columns[j]
      if(elo_col %in% names(startlist_prepared)) {
        startlist_prepared[[pelo_col]] <- startlist_prepared[[elo_col]]
        log_info(paste("Created", pelo_col, "from", elo_col, "for team training"))
      } else {
        startlist_prepared[[pelo_col]] <- 0.5  # Default value
        log_warn(paste("Missing", elo_col, "- using default for", pelo_col))
      }
    }
    
    # Ensure race probability column exists
    if(!(race_prob_col %in% names(startlist_prepared))) {
      log_warn(paste("Race probability column missing:", race_prob_col))
      startlist_prepared[[race_prob_col]] <- 1.0  # Default to 100% participation for Championships
    }
    
    # Create position predictions for teams
    position_preds <- data.frame(Nation = startlist_prepared$Nation)
    position_preds$Sex <- gender
    position_preds$Race <- race_info$original_race_num
    
    # Add race probability column
    if(race_prob_col %in% names(startlist_prepared)) {
      position_preds[[race_prob_col]] <- startlist_prepared[[race_prob_col]]
    }
    
    # Use proper GAM models for team position predictions (like ski jumping)
    # Use team-specific explanatory variables for training (pre-race data)
    explanatory_vars <- c("Prev_Points_Weighted",
                          "Avg_Individual_Pelo_Pct", "Avg_IndividualCompact_Pelo_Pct", 
                          "Avg_MassStart_Pelo_Pct", "Avg_Pelo_Pct")
    
    # Filter for teams with reasonable performance (use training ELO for filtering)
    race_df_filtered <- race_df %>%
      filter(!is.na(get(training_elo_col)), get(training_elo_col) > 0)
    
    # Create position models for each threshold
    position_models <- list()
    
    for(threshold in position_thresholds) {
      log_info(paste("Creating team model for top", threshold, "positions"))
      
      # Create binary outcome variable for position threshold
      race_df_filtered$position_achieved <- race_df_filtered$Place <= threshold
      
      # Create formula for regsubsets using the explanatory variables
      pos_formula <- as.formula(paste("position_achieved ~", paste(explanatory_vars, collapse = " + ")))
      
      # Use regsubsets to select best features for this position threshold
      tryCatch({
        pos_selection <- regsubsets(pos_formula, data = race_df_filtered, nbest = 1, method = "exhaustive")
        pos_summary <- summary(pos_selection)
        pos_best_bic_vars <- names(coef(pos_selection, which.min(pos_summary$bic)))
        pos_smooth_terms <- paste("s(", pos_best_bic_vars[-1], ")", collapse=" + ")
        
        pos_gam_formula <- as.formula(paste("position_achieved ~", pos_smooth_terms))
        
        # Fit the position model with binomial family
        position_model <- gam(pos_gam_formula,
                             family = binomial(),
                             data = race_df_filtered)
        
        position_models[[paste0("threshold_", threshold)]] <- position_model
        
        # Calculate Brier score
        predictions <- predict(position_model, type = "response")
        brier_score <- mean((predictions - race_df_filtered$position_achieved)^2)
        log_info(paste("Brier score for team threshold", threshold, ":", round(brier_score, 4)))
        
      }, error = function(e) {
        log_warn(paste("Error in team position model for threshold", threshold, ":", e$message))
        # Fallback to simple model
        fallback_formula <- as.formula(paste("position_achieved ~ s(", training_elo_col, ")"))
        position_model <- gam(fallback_formula,
                             family = binomial(),
                             data = race_df_filtered)
        position_models[[paste0("threshold_", threshold)]] <- position_model
      })
    }
    
    # Make position probability predictions using the GAM models
    for(threshold in position_thresholds) {
      prob_col <- paste0("prob_top", threshold)
      model_name <- paste0("threshold_", threshold)
      
      if(model_name %in% names(position_models)) {
        tryCatch({
          model <- position_models[[model_name]]
          # Make predictions
          predictions <- predict(model, newdata = startlist_prepared, type = "response")
          # Convert to percentage and round
          position_preds[[prob_col]] <- round(predictions * 100, 1)
          
        }, error = function(e) {
          log_warn(paste("Error making predictions for threshold", threshold, ":", e$message))
          # Fallback: equal probabilities
          position_preds[[prob_col]] <- rep(threshold * 5, nrow(position_preds))
        })
      } else {
        # Fallback: equal probabilities
        position_preds[[prob_col]] <- rep(threshold * 5, nrow(position_preds))
      }
    }
    
    # Normalize team position probabilities
    position_preds <- normalize_position_probabilities(position_preds, race_prob_col, position_thresholds)
    
    # Enforce probability constraints
    position_preds <- enforce_probability_constraints(position_preds)
    
    # Store position predictions for this race
    position_predictions[[as.character(race_info$original_race_num)]] <- position_preds
  }
  
  # Combine all position predictions
  all_position_predictions <- bind_rows(position_predictions)
  
  # Create summary by team
  team_summary <- all_position_predictions %>%
    group_by(Nation) %>%
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
  dir_path <- paste0("~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions/", champs_date)
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Save team summary
  summary_file <- file.path(dir_path, paste0(gender, "_teams.xlsx"))
  write.xlsx(team_summary, summary_file)
  log_info(paste("Saved", gender, "team Championships summary to", summary_file))
  
  # Save detailed race-by-race results
  race_dfs <- list()
  unique_races <- unique(all_position_predictions$Race)
  
  for(race_num in unique_races) {
    race_data <- all_position_predictions[all_position_predictions$Race == race_num, ]
    race_data <- race_data %>%
      dplyr::select(Nation, prob_top1, prob_top3, prob_top5, prob_top10) %>%
      rename(
        Win_Prob = prob_top1,
        Podium_Prob = prob_top3,
        Top5_Prob = prob_top5,
        Top10_Prob = prob_top10
      ) %>%
      arrange(desc(Win_Prob))
    
    # Get race type for sheet naming
    race_types <- champs_races_with_race_num %>%
      filter(Sex == ifelse(gender == "men", "M", ifelse(gender == "ladies", "L", "Mixed")), 
             OriginalRaceNum == race_num) %>%
      pull(RaceType)
    
    race_type <- if(length(race_types) > 0) race_types[1] else paste("Race", race_num)
    sheet_name <- paste(ifelse(gender == "men", "Men", ifelse(gender == "ladies", "Ladies", "Mixed")), race_type)
    
    race_dfs[[sheet_name]] <- race_data
  }
  
  # Save race-by-race results
  race_file <- file.path(dir_path, paste0(gender, "_teams_position_probabilities.xlsx"))
  write.xlsx(race_dfs, race_file)
  log_info(paste("Saved", gender, "team race probabilities to", race_file))
  
  return(list(
    summary = team_summary,
    race_results = all_position_predictions,
    race_sheets = race_dfs
  ))
}

# Calculate race probabilities for Championships (like weekly-picks2.R but with 3-person quota)
calculate_championships_race_probabilities <- function() {
  log_info("Calculating Championships race participation probabilities with 3-person quota constraint")
  
  # Function to get base race probability for a skier (same as weekly-picks2.R)
  get_base_race_probability <- function(chronos, participant, race_type) {
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
    
    # Count all races in this race type since start_date
    all_races <- chronos %>%
      filter(Date >= start_date, RaceType == race_type) %>%
      distinct(Date, City) %>%
      nrow()
    
    # Count participant's races in this race type since start_date
    participant_races <- chronos %>%
      filter(Date >= start_date, Skier == participant, RaceType == race_type) %>%
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
    men_chrono <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/men_chrono.csv", 
                           stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
    
    men_startlist <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/startlist_champs_men.csv", 
                              stringsAsFactors = FALSE)
    
    # Calculate base probabilities for each race/race type
    for(i in 1:nrow(men_races)) {
      race_col <- paste0("Race", i, "_Prob")
      race_type <- men_races$race_type[i]
      
      men_startlist[[race_col]] <- sapply(men_startlist$Skier, function(skier) {
        get_base_race_probability(men_chrono, skier, race_type)
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
          # Scale to target 3 participants per nation
          scaling_factor <- 3 / current_sum
          scaled_probs <- nation_probs * scaling_factor
          # Cap individual probabilities at 1.0
          scaled_probs <- pmin(scaled_probs, 1.0)
          men_startlist[nation_mask, race_col] <- scaled_probs
        }
      }
    }
    
    # Save updated startlist
    write.csv(men_startlist, 
              "~/ski/elo/python/nordic-combined/polars/excel365/startlist_champs_men.csv", 
              row.names = FALSE)
  }
  
  if(nrow(ladies_races) > 0) {
    log_info("Processing ladies' Championships race probabilities")
    ladies_chrono <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/ladies_chrono.csv", 
                              stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
    
    ladies_startlist <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/startlist_champs_ladies.csv", 
                                 stringsAsFactors = FALSE)
    
    # Calculate base probabilities for each race/race type
    for(i in 1:nrow(ladies_races)) {
      race_col <- paste0("Race", i, "_Prob")
      race_type <- ladies_races$race_type[i]
      
      ladies_startlist[[race_col]] <- sapply(ladies_startlist$Skier, function(skier) {
        get_base_race_probability(ladies_chrono, skier, race_type)
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
          # Scale to target 3 participants per nation
          scaling_factor <- 3 / current_sum
          scaled_probs <- nation_probs * scaling_factor
          # Cap individual probabilities at 1.0
          scaled_probs <- pmin(scaled_probs, 1.0)
          ladies_startlist[nation_mask, race_col] <- scaled_probs
        }
      }
    }
    
    # Save updated startlist
    write.csv(ladies_startlist, 
              "~/ski/elo/python/nordic-combined/polars/excel365/startlist_champs_ladies.csv", 
              row.names = FALSE)
  }
  
  log_info("Championships race probability calculation complete")
}

# Calculate race probabilities before processing predictions
calculate_championships_race_probabilities()

# Process men's Championships
# men_results <- NULL
# if(nrow(men_races) > 0) {
#   men_results <- process_gender_championships("men", men_races)
# }

# Process ladies' Championships  
# ladies_results <- NULL
# if(nrow(ladies_races) > 0) {
#   ladies_results <- process_gender_championships("ladies", ladies_races)
# }

# Process team Championships
if(nrow(men_teams) > 0) {
  log_info("Processing men's team Championships")
  men_team_results <- process_team_championships("men", men_teams)
}

if(nrow(ladies_teams) > 0) {
  log_info("Processing ladies' team Championships") 
  ladies_team_results <- process_team_championships("ladies", ladies_teams)
}

if(nrow(mixed_teams) > 0) {
  log_info("Processing mixed team Championships")
  mixed_team_results <- process_team_championships("mixed", mixed_teams)
}

log_info("Nordic Combined Championships predictions completed successfully")