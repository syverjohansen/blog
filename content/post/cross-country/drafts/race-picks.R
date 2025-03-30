# Race Day Predictions: Methodology for Same-Day Race Predictions
library(dplyr)
library(tidyr)
library(openxlsx)
library(arrow)
library(mgcv)
library(leaps)
library(logger)
library(purrr)
library(lubridate) # For better date handling

# Define points systems
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
stage_points <- c(50,47,44,41,38,35,32,30,28,26,24,22,20,18,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
tds_points <- c(300,285,270,255,240,216,207,198,189,180,174,168,162,156,150,144,138,132,126,120,114,108,102,96,90,84,78,72,66,60,57,54,51,48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3)

# Function to replace NAs with first quartile value
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Set up logging
log_dir <- "~/ski/elo/python/ski/polars/excel365/race-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "race_picks_processing.log")))
log_info("Starting race day predictions process")

# Read in today's races from races.csv
log_info("Reading races data")
races <- read.csv("~/ski/elo/python/ski/polars/excel365/races.csv", 
                  stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format="%m/%d/%y")) # Use lubridate's mdy function to parse MM/DD/YY format

# Find races happening today
current_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")
log_info(paste("Current date (UTC):", current_date))

# Filter races for today's date
today_races <- races %>%
  filter(Date == current_date) %>%
  arrange(Date)

# Exit if no races today
if(nrow(today_races) == 0) {
  log_info("No races scheduled for today. Exiting.")
  quit(save = "no")
}

log_info(paste("Found", nrow(today_races), "races scheduled for today"))

# Create race dataframes for men and ladies
men_races <- today_races %>%
  filter(Sex == "M") %>%
  dplyr::select(Distance, Technique, MS, Elevation, Period, Pursuit) %>%
  rename(distance = Distance, technique = Technique, 
         ms = MS, altitude = Elevation, period = Period)

ladies_races <- today_races %>%
  filter(Sex == "L") %>%
  dplyr::select(Distance, Technique, MS, Elevation, Period, Pursuit) %>%
  rename(distance = Distance, technique = Technique, 
         ms = MS, altitude = Elevation, period = Period)

log_info(paste("Found", nrow(men_races), "men's races and", nrow(ladies_races), "ladies races"))

# Read startlists for today's races
log_info("Reading race startlists")
men_startlist <- if(nrow(men_races) > 0) {
  read.csv("~/ski/elo/python/ski/polars/excel365/startlist_races_men.csv", 
           stringsAsFactors = FALSE) %>%
    mutate(Sex = "M")
} else {
  data.frame() # Empty dataframe if no men's races
}

ladies_startlist <- if(nrow(ladies_races) > 0) {
  read.csv("~/ski/elo/python/ski/polars/excel365/startlist_races_ladies.csv", 
           stringsAsFactors = FALSE) %>%
    mutate(Sex = "L")
} else {
  data.frame() # Empty dataframe if no ladies' races
}

# Since we're assuming everyone on the startlist will race, set Race*_Prob to 1
# Add Race*_Prob columns for each race if they don't exist
if(nrow(men_startlist) > 0) {
  for(i in 1:nrow(men_races)) {
    race_prob_col <- paste0("Race", i, "_Prob")
    if(!(race_prob_col %in% names(men_startlist))) {
      men_startlist[[race_prob_col]] <- 1.0
    } else {
      men_startlist[[race_prob_col]] <- 1.0
    }
  }
}

if(nrow(ladies_startlist) > 0) {
  for(i in 1:nrow(ladies_races)) {
    race_prob_col <- paste0("Race", i, "_Prob")
    if(!(race_prob_col %in% names(ladies_startlist))) {
      ladies_startlist[[race_prob_col]] <- 1.0
    } else {
      ladies_startlist[[race_prob_col]] <- 1.0
    }
  }
}

# Modified combine_predictions function for race day predictions
combine_predictions <- function(race_dfs, startlist) {
  log_info("Combining race predictions")
  
  # Start with first race
  log_info("Starting with first race data")
  final_predictions <- race_dfs[[1]] %>%
    rename(
      Race1_Base = Base_Prediction,
      Race1_Altitude = altitude_adjustment,
      Race1_Period = period_adjustment,
      Race1_MS = ms_adjustment,
      Race1_Points = Final_Prediction,
      Race1_Safe = Safe_Prediction,
      Race1_Upside = Upside_Prediction,
      Race1_Volatility = prediction_volatility,
      Race1_Ratio = volatility_ratio,
      Race1_Confidence = confidence_factor,
      Race1_Probability = Race1_Prob
    ) %>%
    left_join(
      startlist %>% dplyr::select(Skier, ID, Price, Sex),
      by = "Skier"
    )

  # Add remaining races dynamically
  if(length(race_dfs) > 1) {
    for(i in 2:length(race_dfs)) {
      log_info(paste("Adding race", i, "to combined predictions"))
      
      # Get probability column name
      race_prob_col <- paste0("Race", i, "_Prob")
      
      # Check if Race{i}_Prob exists in this race_df
      if(!race_prob_col %in% names(race_dfs[[i]])) {
        log_warn(paste("Race probability column", race_prob_col, "not found in race_dfs[[", i, "]]"))
        
        # Since we're assuming everyone races, just set to 1
        race_dfs[[i]][[race_prob_col]] <- 1.0
      }
      
      # Handle Race{i}_Probability column properly
      final_predictions <- final_predictions %>%
        left_join(
          race_dfs[[i]] %>%
            rename(
              !!paste0("Race", i, "_Base") := Base_Prediction,
              !!paste0("Race", i, "_Altitude") := altitude_adjustment,
              !!paste0("Race", i, "_Period") := period_adjustment,
              !!paste0("Race", i, "_MS") := ms_adjustment,
              !!paste0("Race", i, "_Points") := Final_Prediction,
              !!paste0("Race", i, "_Safe") := Safe_Prediction,
              !!paste0("Race", i, "_Upside") := Upside_Prediction,
              !!paste0("Race", i, "_Volatility") := prediction_volatility,
              !!paste0("Race", i, "_Ratio") := volatility_ratio,
              !!paste0("Race", i, "_Confidence") := confidence_factor,
              !!paste0("Race", i, "_Probability") := !!sym(paste0("Race", i, "_Prob"))
            ) %>%
            dplyr::select(Skier, 
                          !!paste0("Race", i, "_Base"),
                          !!paste0("Race", i, "_Altitude"),
                          !!paste0("Race", i, "_Period"),
                          !!paste0("Race", i, "_MS"),
                          !!paste0("Race", i, "_Points"),
                          !!paste0("Race", i, "_Safe"),
                          !!paste0("Race", i, "_Upside"),
                          !!paste0("Race", i, "_Volatility"),
                          !!paste0("Race", i, "_Ratio"),
                          !!paste0("Race", i, "_Confidence"),
                          !!paste0("Race", i, "_Probability")),
          by = "Skier"
        )
    }
  }
  
  # Check if probability columns exist in final predictions
  prob_cols <- paste0("Race", 1:length(race_dfs), "_Probability")
  for(col in prob_cols) {
    if(!col %in% names(final_predictions)) {
      log_warn(paste("Probability column", col, "missing from final predictions!"))
    } else {
      log_info(paste("Probability column", col, "exists in final predictions"))
      log_info(paste("  Mean:", mean(final_predictions[[col]], na.rm = TRUE)))
      log_info(paste("  Sum:", sum(final_predictions[[col]], na.rm = TRUE)))
    }
  }
  
  # Create expressions for summing columns dynamically
  sum_expr <- function(prefix, n_races) {
    syms <- paste0("Race", 1:n_races, "_", prefix)
    parse(text = paste(syms, collapse = " + "))
  }
  
  # Weighted sum expression that accounts for Race{i}_Probability
  weighted_sum_expr <- function(prefix, n_races) {
    terms <- sapply(1:n_races, function(i) {
      paste0("Race", i, "_", prefix, " * Race", i, "_Probability")
    })
    parse(text = paste(terms, collapse = " + "))
  }
  
  avg_expr <- function(prefix, n_races) {
    syms <- paste0("Race", 1:n_races, "_", prefix)
    parse(text = paste0("(", paste(syms, collapse = " + "), ")/", n_races))
  }
  
  # Calculate totals dynamically based on number of races, using probability-weighted sums
  log_info("Calculating totals with probability weighting")
  final_predictions <- final_predictions %>%
    mutate(
      # Use probability-weighted sums for point calculations
      Total_Points = eval(weighted_sum_expr("Points", length(race_dfs))),
      Total_Altitude = eval(weighted_sum_expr("Altitude", length(race_dfs))),
      Total_Period = eval(weighted_sum_expr("Period", length(race_dfs))),
      Total_MS = eval(weighted_sum_expr("MS", length(race_dfs))),
      Total_Safe = eval(weighted_sum_expr("Safe", length(race_dfs))),
      Total_Upside = eval(weighted_sum_expr("Upside", length(race_dfs))),
      Avg_Volatility = eval(avg_expr("Volatility", length(race_dfs))),
      Avg_Confidence = eval(avg_expr("Confidence", length(race_dfs)))
    ) %>%
    arrange(desc(Total_Points))
  
  # Select columns dynamically based on number of races
  select_cols <- c("Skier", "ID", "Nation", "Sex", "Price")
  for(i in 1:length(race_dfs)) {
    select_cols <- c(select_cols,
                     paste0("Race", i, "_Base"),
                     paste0("Race", i, "_Altitude"),
                     paste0("Race", i, "_Period"),
                     paste0("Race", i, "_MS"),
                     paste0("Race", i, "_Points"),
                     paste0("Race", i, "_Safe"),
                     paste0("Race", i, "_Upside"),
                     paste0("Race", i, "_Volatility"),
                     paste0("Race", i, "_Ratio"),
                     paste0("Race", i, "_Confidence"),
                     paste0("Race", i, "_Probability"))
  }
  select_cols <- c(select_cols,
                   "Total_Points", "Total_Safe", "Total_Upside",
                   "Total_Altitude", 
                   "Total_Period", "Total_MS",
                   "Avg_Volatility", "Avg_Confidence")
  
  log_info("Returning final predictions")
  final_predictions %>%
    dplyr::select(all_of(select_cols))

}

# Function to prepare startlist data with ELO information
prepare_startlist_data <- function(startlist, race_df, pelo_col) {
  # Print some debug info
  log_info(paste("Preparing startlist data for", pelo_col))
  
  # Dynamically get race probability columns - important to preserve these!
  race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
  log_info(paste("Race probability columns found:", paste(race_prob_cols, collapse=", ")))
  
  # Keep only essential columns from startlist
  base_df <- startlist %>%
    dplyr::select(Skier, ID, Nation, Sex, Price, all_of(race_prob_cols))
  
  # Get all required Elo columns and their corresponding Pelo names
  elo_cols <- c("Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
                "Elo", "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo",
                "Freestyle_Elo", "Classic_Elo")
  
  pelo_cols <- c("Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                 "Pelo", "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo",
                 "Freestyle_Pelo", "Classic_Pelo")
  
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
  
  # Get recent points for specific race type
  recent_points <- race_df %>%
    filter(Skier %in% base_df$Skier) %>%
    filter(
      if(grepl("^Sprint", pelo_col)) {
        Distance == "Sprint" & 
          Technique == substr(pelo_col, 8, 8)
      } else {
        Distance != "Sprint" & 
          (Technique == substr(pelo_col, 10, 10) | substr(pelo_col, 10, 10) == "")  # Allow empty technique
      }
    ) %>%
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
  
  # Ensure we have all the required Pelo_Pct columns for model prediction
  # Even if we can't calculate them from Elo values
  for(i in seq_along(pelo_cols)) {
    pelo_pct_col <- paste0(pelo_cols[i], "_Pct")
    if(!pelo_pct_col %in% names(result_df)) {
      log_info(paste("Creating missing Pelo Pct column:", pelo_pct_col))
      result_df[[pelo_pct_col]] <- 0.5  # Default to 0.5 (middle value)
    }
  }
  
  # Calculate max values for normalization - only for available columns
  available_elo_cols <- intersect(names(race_df), elo_cols)
  if(length(available_elo_cols) > 0) {
    max_values <- race_df %>%
      summarise(across(all_of(available_elo_cols), ~max(.x, na.rm = TRUE)))
    
    # Calculate both Elo and Pelo percentages for available columns
    for(i in seq_along(elo_cols)) {
      elo_col <- elo_cols[i]
      pelo_col_i <- pelo_cols[i]
      
      # Check if column exists in both datasets
      if(elo_col %in% names(result_df) && elo_col %in% names(max_values)) {
        max_val <- max_values[[elo_col]]
        # Only calculate if max value is not zero or NA
        if(!is.na(max_val) && max_val > 0) {
          # Calculate the percentage
          pct_value <- result_df[[elo_col]] / max_val
          
          # Assign to both Elo and Pelo percentage columns
          result_df[[paste0(elo_col, "_Pct")]] <- pct_value
          result_df[[paste0(pelo_col_i, "_Pct")]] <- pct_value
          
          log_info(paste("Calculated percentage for", elo_col))
        }
      }
    }
  } else {
    log_info("No Elo columns available in race data for percentage calculation")
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

get_points <- function(place, points_list) {
  if (place >= 1 && place <= length(points_list)) {
    return(points_list[place])
  }
  return(0)
}

# Preprocessing function for historical race data
preprocess_data <- function(df) {
  # Load races data to determine points systems for historical races
  races_data <- read.csv("~/ski/elo/python/ski/polars/excel365/races.csv", 
                         stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date, format="%m/%d/%y"))
  
  # Determine points system based on today's race
  # Find races for today
  current_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")
  today_race <- races_data %>%
    filter(Date == current_date) %>%
    arrange(Date) %>%
    dplyr::slice(1)
  
  # Check if today's race is a stage race
  is_stage_race <- !is.na(today_race$Stage) && today_race$Stage == 1
  
  # Select the appropriate points system for ALL races
  global_points_system <- if(is_stage_race) {
    log_info("Using STAGE points system for today's race")
    stage_points
  } else {
    log_info("Using WORLD CUP points system for today's race") 
    wc_points
  }
  
  # First calculate points using historical data but with the GLOBAL points system
  df_with_points <- df %>%
    # Add points based on the global points system determined by today's race
    mutate(
      Points = mapply(function(place) {
        if (place >= 1 && place <= length(global_points_system)) {
          return(global_points_system[place])
        }
        return(0)
      }, Place)
    ) %>%
    # Sort
    arrange(Season, Race, Place)
  
  # Calculate weighted previous points separately for each race type/technique combination
  df_with_points <- df_with_points %>%
    # First, create a race type column that distinguishes between sprint and distance
    mutate(RaceType = ifelse(Distance == "Sprint", "Sprint", "Distance")) %>%
    # Group by ID and the broader race type category
    group_by(ID, RaceType, Technique) %>%
    arrange(Season, Race) %>%
    mutate(Prev_Points_Weighted = sapply(1:n(), function(j) {
      if (j == 1) return(0)
      start_index <- max(1, j - 5)
      num_races <- j - start_index
      weights <- seq(1, num_races)
      weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
    })) %>%
    ungroup()
  
  # Check if Pelo columns exist, if not create them
  pelo_cols <- c("Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                 "Pelo", "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo",
                 "Freestyle_Pelo", "Classic_Pelo")
  
  # Make sure these columns exist (create if missing)
  for (col in pelo_cols) {
    if (!col %in% names(df_with_points)) {
      log_info(paste("Creating missing column:", col))
      df_with_points[[col]] <- 0
    }
  }
  
  # Now apply other preprocessing steps and filter for recent data
  processed_df <- df_with_points %>%
    # Add period
    group_by(Season) %>%
    mutate(
      Num_Races = max(Race),
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
    # Filter relevant races and add cumulative points
    filter(
      Season >= max(Season-10),
      Event %in% c("World Cup", "Nordic Opening", "Tour de Ski", 
                   "Olympic Winter Games", "World Championship", 
                   "World Cup Final", "Ski Tour Canada")
    ) %>%
    group_by(ID, Season) %>%
    mutate(Cumulative_Points = cumsum(Points)) %>%
    ungroup() %>%
    # Handle NAs and calculate percentages
    group_by(Season, Race) %>%
    mutate(
      across(
        all_of(pelo_cols),
        ~replace_na_with_quartile(.x)
      )
    ) %>%
    # Calculate percentages for each Pelo column
    mutate(
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
    ungroup() %>%
    # Filter out team sprint and relay
    filter(!Distance %in% c("Ts", "Rel"))
  
  # Ensure all required Pelo_Pct columns exist
  pct_cols <- paste0(pelo_cols, "_Pct")
  for (col in pct_cols) {
    if (!col %in% names(processed_df)) {
      log_info(paste("Creating missing percentage column:", col))
      processed_df[[col]] <- 0
    }
  }
  
  return(processed_df)
}

# Function to create post predictions for blog output
create_post_predictions <- function(final_predictions, n_races, gender=NULL) {
  # Select columns dynamically based on number of races
  select_cols <- c("Skier", "ID", "Nation")

  if("Sex" %in% names(final_predictions)) {
    select_cols <- c(select_cols, "Sex")  # Include Sex if it exists
  }
  
  for(i in 1:n_races) {
    select_cols <- c(select_cols,
                     paste0("Race", i, "_Points"),
                     paste0("Race", i, "_Probability"))
  }
  select_cols <- c(select_cols, "Total_Points")
  
  post_predictions <- final_predictions %>%
    dplyr::select(all_of(select_cols)) %>%
    # Add Sex if not present
    mutate(
      Sex = if("Sex" %in% names(.)) {
        Sex  # Use existing Sex column
      } else {
        ifelse(gender == "men", "M", "L")  # Fallback
      }
    ) %>%
    # Make sure Sex is in the right position (after Nation)
    dplyr::select(Skier, ID, Nation, Sex, everything()) %>%
    # Arrange by total points
    arrange(desc(Total_Points))
  
  # Replace underscores with spaces in column names
  post_predictions <- post_predictions %>%
    rename_with(~ gsub("_", " ", .x), .cols = contains("_"))
  
  return(post_predictions)
}

# Continuing normalize_position_probabilities function
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
        log_info(sprintf("  Capping %d skiers with >100%% probability for %s", 
                         length(over_hundred), prob_col))
        
        # Calculate excess probability that needs to be redistributed
        excess <- sum(normalized[[prob_col]][over_hundred] - 100)
        
        # Cap values at 100%
        normalized[[prob_col]][over_hundred] <- 100
        
        # Redistribute the excess to other skiers proportionally
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
      # If sum is zero, distribute evenly among all skiers
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
  
  # Return normalized probabilities
  return(normalized)
}

# Main function to predict races for a specific gender
predict_races <- function(gender) {
  # Load chronological data
  chrono_path <- ifelse(gender == "men", 
                        "~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv",
                        "~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv")
  
  # Get appropriate startlist and races
  startlist <- if(gender == "men") men_startlist else ladies_startlist
  races <- if(gender == "men") men_races else ladies_races
  
  # Skip if no races for this gender
  if(nrow(races) == 0) {
    log_info(paste("No races for", gender, "today"))
    return(NULL)
  }
  
  log_info(paste("Processing", gender, "data"))
  
  # Read chronos
  df <- read.csv(chrono_path, stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date)) %>%
    preprocess_data()
  
  # Initialize results list
  race_predictions <- list()
  race_dfs <- list()
  position_predictions <- list() # List to store position probabilities for each race
  
  # Define position thresholds
  position_thresholds <- c(1, 3, 5, 10, 30)  # Win, Podium, Top 5, Top 10, Top 30
  
  # Debug: Show race probability columns in startlist
  race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
  log_info(paste("Race probability columns in startlist:", paste(race_prob_cols, collapse=", ")))
  
  # Process each race
  for(i in 1:nrow(races)) {
    log_info(sprintf("Processing %s race %d: %s %s", gender, i, races$distance[i], races$technique[i]))
    
    # Get race probability column name for this race
    race_prob_col <- paste0("Race", i, "_Prob")
    race_info <- today_races %>%
      filter(Sex == ifelse(gender == "men", "M", "L")) %>%
      dplyr::slice(i)
    
    # Check if this is a stage race
    is_stage_race <- !is.null(race_info$Stage) && race_info$Stage == 1
    
    # Use the appropriate points system
    points_system <- if(is_stage_race) stage_points else wc_points
    
    log_info(paste("Using", if(is_stage_race) "stage points" else "World Cup points", "for this race"))
    
    # Debug: Check if this race probability column exists in startlist
    if(race_prob_col %in% names(startlist)) {
      log_info(paste("Race probability column", race_prob_col, "exists in startlist"))
      # Show some stats
      log_info(paste("  Mean:", mean(startlist[[race_prob_col]], na.rm = TRUE)))
      log_info(paste("  Sum:", sum(startlist[[race_prob_col]], na.rm = TRUE)))
    } else {
      log_warn(paste("Race probability column", race_prob_col, "NOT FOUND in startlist!"))
      # Since everyone on startlist will race, add the column
      startlist[[race_prob_col]] <- 1.0
    }
    
    # Filter base dataset for race type
    race_df <- df %>%
      {if(races$distance[i] == "Sprint") {
        if(races$technique[i] == "") {
          filter(., Distance == "Sprint")  # Don't filter by technique if it's empty
        } else {
          filter(., Distance == "Sprint", Technique == races$technique[i])
        }
      } else {
        if(races$technique[i] == "") {
          filter(., Distance != "Sprint")  # Don't filter by technique if it's empty
        } else {
          filter(., Distance != "Sprint", Technique == races$technique[i])
        }
      }}
    
    # Add altitude categories for historical data
    race_df <- race_df %>%
      mutate(
        AltitudeCategory = ifelse(Elevation >= 1300, 1, 0)
      )        
    
    # Get relevant Pelo column
    pelo_col <- case_when(
      races$distance[i] == "Sprint" & races$technique[i] == "C" ~ "Sprint_C_Pelo_Pct",
      races$distance[i] == "Sprint" & races$technique[i] == "F" ~ "Sprint_F_Pelo_Pct",
      races$distance[i] != "Sprint" & races$technique[i] == "C" ~ "Distance_C_Pelo_Pct",
      races$distance[i] != "Sprint" & races$technique[i] == "F" ~ "Distance_F_Pelo_Pct",
      races$distance[i] != "Sprint" & races$technique[i] == "" ~ "Distance_Pelo_Pct",
      races$distance[i] == "Sprint" & races$technique[i] == "" ~ "Sprint_Pelo_Pct",
      TRUE ~ "Pelo_Pct"
    )
    
    # Filter for top performers and add previous points
    race_df_75 <- race_df %>%
      filter(get(pelo_col) > 0.75) %>%
      group_by(ID) %>%
      arrange(Season, Race) %>%
      ungroup()
    
    # Feature selection and model fitting for points prediction
    response_variable <- "Points"
    explanatory_vars <- c("Prev_Points_Weighted", "Distance_Pelo_Pct", "Sprint_Pelo_Pct", 
                          "Sprint_C_Pelo_Pct", "Distance_F_Pelo_Pct", "Distance_C_Pelo_Pct", 
                          "Classic_Pelo_Pct", "Freestyle_Pelo_Pct", "Sprint_F_Pelo_Pct", "Pelo_Pct")
    
    # Create and fit model for points
    formula <- as.formula(paste(response_variable, "~", paste(explanatory_vars, collapse = " + ")))
    tryCatch({
      exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
      summary_exhaustive <- summary(exhaustive_selection)
      best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
      smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
      gam_formula <- as.formula(paste("Points ~", smooth_terms))
      
      model <- gam(gam_formula, data = race_df_75)
    }, error = function(e) {
      log_warn(paste("Error in model selection:", e$message))
      # Fallback to a simpler model
      simple_formula <- as.formula(paste("Points ~ s(", pelo_col, ")"))
      model <<- gam(simple_formula, data = race_df_75)
    })
    
    # Calculate adjustments for historical data step by step
    race_df_75 <- race_df_75 %>%
      arrange(Date) %>%
      group_by(Skier) %>%
      mutate(
        row_id = row_number()
      ) %>%
      ungroup() %>%
      # Step 1: Initial predictions
      mutate(
        Initial_Prediction = predict(model, newdata = .)
      ) %>%
      group_by(Skier) %>%
      mutate(
        Prediction_Diff = Points - Initial_Prediction
      ) %>%
      # Step 2: Calculate altitude p-values and effects
      mutate(
        altitude_p = purrr::map_dbl(row_id, function(r) {
          if(r <= 1) return(1)
          prior_alt_curr <- Prediction_Diff[AltitudeCategory == AltitudeCategory[r] & row_id < r]
          prior_alt_other <- Prediction_Diff[AltitudeCategory != AltitudeCategory[r] & row_id < r]
          if(length(prior_alt_curr) < 3 || length(prior_alt_other) < 3) return(1)
          tryCatch({
            t.test(prior_alt_curr, prior_alt_other)$p.value
          }, error = function(e) 1)
        }),
        altitude_correction = ifelse(altitude_p < 0.05,
                                     mean(Prediction_Diff[AltitudeCategory == AltitudeCategory], na.rm = TRUE),
                                     0)
      ) %>%
      # Step 3: Calculate course-adjusted predictions
      mutate(
        Course_Adjusted = Initial_Prediction + altitude_correction,
        Course_Diff = Points - Course_Adjusted
      ) %>%
      # Step 4: Calculate Period adjustments
      mutate(
        period_p = purrr::map_dbl(row_id, function(r) {
          if(r <= 1) return(1)
          prior_period_curr <- Course_Diff[Period == Period[r] & row_id < r]
          prior_period_other <- Course_Diff[Period != Period[r] & row_id < r]
          if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
          tryCatch({
            t.test(prior_period_curr, prior_period_other)$p.value
          }, error = function(e) 1)
        }),
        period_correction = ifelse(period_p < 0.05,
                                   mean(Course_Diff[Period == Period], na.rm = TRUE),
                                   0),
        Period_Adjusted = Course_Adjusted + period_correction,
        Period_Diff = Points - Period_Adjusted
      ) %>%
      # Step 5: Calculate Mass Start adjustments
      mutate(
        ms_p = purrr::map_dbl(row_id, function(r) {
          if(r <= 1) return(1)
          prior_ms_curr <- Period_Diff[MS == MS[r] & row_id < r]
          prior_ms_other <- Period_Diff[MS != MS[r] & row_id < r]
          if(length(prior_ms_curr) < 3 || length(prior_ms_other) < 3) return(1)
          tryCatch({
            t.test(prior_ms_curr, prior_ms_other)$p.value
          }, error = function(e) 1)
        }),
        ms_correction = ifelse(ms_p < 0.05,
                               mean(Period_Diff[MS == MS], na.rm = TRUE),
                               0),
      ) %>%
      ungroup()
    
    # NEW: Create position probability models using the same variables as points model
    position_models <- list()
    position_adjustments <- list()  # To store adjustments for each threshold
    
    # Feature selection - use the same explanatory variables as the points model
    position_feature_vars <- explanatory_vars  # Use the same variables defined for points
    
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
        
        # Calculate adjustments for altitude, period, and mass start for this threshold
        position_df <- race_df %>%
          arrange(Date) %>%
          group_by(Skier) %>%
          mutate(
            row_id = row_number()
          ) %>%
          ungroup()
        
        # Then add predictions separately (outside of mutate)
        position_df$initial_prob <- predict(position_model, newdata = position_df, type = "response")
        
        # Then continue with the rest of the operations
        position_df <- position_df %>%
          group_by(Skier) %>%
          mutate(
            prob_diff = as.numeric(position_achieved) - initial_prob,
            
            # Calculate altitude adjustments
            altitude_p = purrr::map_dbl(row_id, function(r) {
              if(r <= 1) return(1)
              prior_alt_curr <- prob_diff[AltitudeCategory == AltitudeCategory[r] & row_id < r]
              prior_alt_other <- prob_diff[AltitudeCategory != AltitudeCategory[r] & row_id < r]
              if(length(prior_alt_curr) < 3 || length(prior_alt_other) < 3) return(1)
              tryCatch({
                t.test(prior_alt_curr, prior_alt_other)$p.value
              }, error = function(e) 1)
            }),
            altitude_correction = ifelse(altitude_p < 0.05,
                                         mean(prob_diff[AltitudeCategory == AltitudeCategory], na.rm = TRUE),
                                         0),
            altitude_adjusted = pmin(pmax(initial_prob + altitude_correction, 0), 1),
            altitude_diff = as.numeric(position_achieved) - altitude_adjusted
          ) %>%
          # Calculate period adjustments
          mutate(
            period_p = purrr::map_dbl(row_id, function(r) {
              if(r <= 1) return(1)
              prior_period_curr <- altitude_diff[Period == Period[r] & row_id < r]
              prior_period_other <- altitude_diff[Period != Period[r] & row_id < r]
              if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
              tryCatch({
                t.test(prior_period_curr, prior_period_other)$p.value
              }, error = function(e) 1)
            }),
            period_correction = ifelse(period_p < 0.05,
                                       mean(altitude_diff[Period == Period], na.rm = TRUE),
                                       0),
            period_adjusted = pmin(pmax(altitude_adjusted + period_correction, 0), 1),
            period_diff = as.numeric(position_achieved) - period_adjusted
          ) %>%
          # Calculate mass start adjustments
          mutate(
            ms_p = purrr::map_dbl(row_id, function(r) {
              if(r <= 1) return(1)
              prior_ms_curr <- period_diff[MS == MS[r] & row_id < r]
              prior_ms_other <- period_diff[MS != MS[r] & row_id < r]
              if(length(prior_ms_curr) < 3 || length(prior_ms_other) < 3) return(1)
              tryCatch({
                t.test(prior_ms_curr, prior_ms_other)$p.value
              }, error = function(e) 1)
            }),
            ms_correction = ifelse(ms_p < 0.05,
                                   mean(period_diff[MS == MS], na.rm = TRUE),
                                   0)
          ) %>%
          ungroup()
        
        # Get final adjustments for each skier
        skier_pos_adjustments <- position_df %>%
          group_by(Skier) %>%
          summarise(
            altitude_effect = last(altitude_correction),
            period_effect = last(period_correction),
            ms_effect = last(ms_correction)
          )
        
        # Store adjustments for this threshold
        position_adjustments[[paste0("threshold_", threshold)]] <- skier_pos_adjustments
        
      }, error = function(e) {
        log_warn(paste("Error in position model for threshold", threshold, ":", e$message))
        
        # Create a simpler fallback model with just the pelo column
        fallback_vars <- c("Prev_Points_Weighted", pelo_col)
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
          empty_adjustments <- data.frame(Skier = unique(race_df$Skier))
          empty_adjustments$altitude_effect <- 0
          empty_adjustments$period_effect <- 0
          empty_adjustments$ms_effect <- 0
          position_adjustments[[paste0("threshold_", threshold)]] <- empty_adjustments
          
          log_info(paste("Created fallback model for threshold", threshold, 
                         "using variables:", paste(fallback_vars, collapse=", ")))
        } else {
          # Last resort fallback - just use the pelo column
          fallback_formula <- as.formula(paste("position_achieved ~ s(", pelo_col, ")"))
          position_models[[paste0("threshold_", threshold)]] <- gam(
            fallback_formula,
            data = race_df,
            family = binomial,
            method = "REML"
          )
          
          # Create empty adjustments object
          empty_adjustments <- data.frame(Skier = unique(race_df$Skier))
          empty_adjustments$altitude_effect <- 0
          empty_adjustments$period_effect <- 0
          empty_adjustments$ms_effect <- 0
          position_adjustments[[paste0("threshold_", threshold)]] <- empty_adjustments
          
          log_info(paste("Created last-resort fallback model for threshold", threshold, 
                         "using only", pelo_col))
        }
      })
    }
    
    # Calculate volatility metrics using recent races
    race_df_75 <- race_df_75 %>%
      group_by(Skier) %>%
      arrange(Date) %>%  # Ensure chronological order
      mutate(
        # Create rolling window calculations for last 10 races
        recent_prediction_volatility = slider::slide_dbl(
          Points - Initial_Prediction,
          sd,
          .before = 9,  # Look at current race plus 9 previous
          .complete = FALSE  # Allow partial windows
        ),
        
        recent_consistency_score = slider::slide_dbl(
          abs(Points - Initial_Prediction),
          mean,
          .before = 9,
          .complete = FALSE
        ),
        
        recent_upside_potential = slider::slide_dbl(
          Points - Initial_Prediction,
          ~quantile(.x, 0.9, na.rm = TRUE),
          .before = 9,
          .complete = FALSE
        ),
        
        recent_downside_risk = slider::slide_dbl(
          Points - Initial_Prediction,
          ~quantile(.x, 0.1, na.rm = TRUE),
          .before = 9,
          .complete = FALSE
        )
      ) %>%
      mutate(
        recent_volatility_ratio = recent_upside_potential / abs(recent_downside_risk)
      ) %>%
      ungroup()
    
    # Get final adjustments for each skier
    skier_adjustments <- race_df_75 %>%
      group_by(Skier) %>%
      summarise(
        altitude_effect = last(altitude_correction),
        period_effect = last(period_correction),
        ms_effect = last(ms_correction),
        
        # Recent volatility metrics
        prediction_volatility = last(recent_prediction_volatility),
        consistency_score = last(recent_consistency_score),
        upside_potential = last(recent_upside_potential),
        downside_risk = last(recent_downside_risk),
        volatility_ratio = last(recent_volatility_ratio),
        
        # Add number of recent races for confidence
        n_recent_races = sum(!is.na(tail(Points, 10)))
      )
    
    # Prepare startlist data
    startlist_prepared <- prepare_startlist_data(startlist, race_df, pelo_col)
    
    # Ensure race probability column exists
    if(!(race_prob_col %in% names(startlist_prepared))) {
      log_warn(paste("Race probability column missing:", race_prob_col))
      # For race day predictions, everyone on startlist races
      log_info("Setting race probability to 1.0 for all skiers")
      startlist_prepared[[race_prob_col]] <- 1.0
    }
    
    # Make position probability predictions with adjustments
    position_preds <- data.frame(
      Skier = startlist_prepared$Skier,
      ID = startlist_prepared$ID,
      Nation = startlist_prepared$Nation,
      Sex = startlist_prepared$Sex,
      Race = i
    )
    
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
          
          # Check what variables the model actually needs
          model_vars <- names(pos_model$var.summary)
          log_info(paste("Model for threshold", threshold, "requires variables:", paste(model_vars, collapse=", ")))
          
          # Create a clean subset of prediction data with only required variables
          prediction_subset <- startlist_prepared
          
          # Explicitly check for each variable
          for(var in model_vars) {
            if(!(var %in% names(prediction_subset))) {
              log_warn(paste("Missing required variable:", var, "- adding with default values"))
              # Add missing variable with appropriate default value
              prediction_subset[[var]] <- 0
            } else {
              # Ensure the variable has the right type
              model_var_type <- class(pos_model$var.summary[[var]])
              data_var_type <- class(prediction_subset[[var]])
              
              if(!identical(model_var_type, data_var_type)) {
                log_warn(paste("Variable type mismatch for", var, ":", 
                               "model expects", model_var_type, "but got", data_var_type))
                # Convert to correct type
                if(model_var_type == "numeric") {
                  prediction_subset[[var]] <- as.numeric(prediction_subset[[var]])
                } else if(model_var_type == "factor") {
                  prediction_subset[[var]] <- as.factor(prediction_subset[[var]])
                }
              }
              
              # Handle NAs
              if(any(is.na(prediction_subset[[var]]))) {
                log_info(paste("Replacing NAs in", var))
                if(is.numeric(prediction_subset[[var]])) {
                  prediction_subset[[var]] <- replace_na_with_quartile(prediction_subset[[var]])
                } else {
                  # For non-numeric, use most common value
                  most_common <- names(sort(table(prediction_subset[[var]], useNA = "no"), decreasing = TRUE))[1]
                  prediction_subset[[var]][is.na(prediction_subset[[var]])] <- most_common
                }
              }
            }
          }
          
          # Make predictions with explicit try-catch
          base_predictions <- tryCatch({
            # Debug output
            log_info(paste("Attempting prediction for threshold", threshold, "with", nrow(prediction_subset), "rows"))
            
            # Explicit call to mgcv::predict.gam to avoid method dispatch issues
            mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response")
          }, error = function(e) {
            log_warn(paste("Prediction call failed:", e$message))
            
            # Continuing the predict_races function - position prediction section
            
            # Try alternative prediction approach with one row at a time
            log_info("Trying row-by-row prediction as fallback")
            result <- numeric(nrow(prediction_subset))
            
            for(i in 1:nrow(prediction_subset)) {
              single_row <- prediction_subset[i,, drop = FALSE]
              result[i] <- tryCatch({
                mgcv::predict.gam(pos_model, newdata = single_row, type = "response")
              }, error = function(e2) {
                log_warn(paste("Failed on row", i, ":", e2$message))
                threshold/100  # Default value based on threshold
              })
            }
            return(result)
          })
          
          # Store predictions
          position_preds[[paste0(prob_col, "_base")]] <- base_predictions
          
          # Create a dataframe to store base predictions
          base_pred_df <- data.frame(
            Skier = position_preds$Skier,
            Nation = position_preds$Nation,
            ID = position_preds$ID,
            BaseProbability = base_predictions * 100  # Convert to percentage
          )
          
          # If we have adjustments available for this threshold
          if(adj_name %in% names(position_adjustments)) {
            # Get the adjustments
            pos_adj <- position_adjustments[[adj_name]]
            
            # Join adjustments with base predictions
            adjustment_df <- base_pred_df %>%
              left_join(pos_adj, by = "Skier") %>%
              mutate(
                Altitude = replace_na(altitude_effect, 0) * 100,
                Period = replace_na(period_effect, 0) * 100,
                MassStart = replace_na(ms_effect, 0) * 100,
                TotalAdjustment = Altitude + Period + MassStart,
                FinalProbability = pmin(pmax(BaseProbability + TotalAdjustment, 0), 100)
              ) %>%
              arrange(desc(BaseProbability))
            
            # Print header and data
            cat(paste("\n\n=== Top", threshold, "Position Probabilities and Adjustments ===\n"))
          }
          
          # Apply adjustments if available
          if(adj_name %in% names(position_adjustments)) {
            # Get adjustments
            pos_adj <- position_adjustments[[adj_name]]
            
            # Join with predictions
            position_preds <- position_preds %>%
              left_join(pos_adj, by = "Skier") %>%
              mutate(
                # Replace NAs with zeros
                altitude_effect = replace_na(altitude_effect, 0),
                period_effect = replace_na(period_effect, 0),
                ms_effect = replace_na(ms_effect, 0),
                
                # Apply adjustments
                altitude_adjustment = altitude_effect,
                period_adjustment = period_effect,
                ms_adjustment = ms_effect,
                
                # Calculate adjusted probabilities
                adjusted_prob = get(paste0(prob_col, "_base")) + 
                  altitude_adjustment + period_adjustment + ms_adjustment,
                
                # Ensure probabilities are between 0 and 1
                adjusted_prob = pmin(pmax(adjusted_prob, 0), 1)
              )
            
            # Use adjusted probability as final
            position_preds[[prob_col]] <- position_preds$adjusted_prob
            
            # Clean up temporary columns
            position_preds <- position_preds %>%
              dplyr::select(-altitude_effect, -period_effect, -ms_effect,
                            -altitude_adjustment, -period_adjustment, -ms_adjustment, -adjusted_prob)
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
        
        # Extra verification - print top 3 skiers and their probabilities for this threshold
        top_skiers <- position_preds %>%
          arrange(desc(!!sym(prob_col))) %>%
          head(3) %>%
          dplyr::select(Skier, Nation, !!sym(prob_col))
        log_info("  Top 3 skiers for this threshold:")
      }
    }
    
    # Store position predictions for this race
    position_predictions[[i]] <- position_preds
    View(position_predictions[[i]])
    
    # Check if probabilities are getting lost
    log_info(paste("Race", i, "probability check:"))
    prob_summary <- startlist_prepared %>%
      group_by(Nation) %>%
      summarise(
        mean_prob = mean(get(race_prob_col), na.rm = TRUE),
        sum_prob = sum(get(race_prob_col), na.rm = TRUE),
        n = n()
      ) %>%
      arrange(desc(sum_prob))
    
    # Prepare startlist points predictions (original functionality)
    race_dfs[[i]] <- startlist_prepared %>%
      mutate(
        Base_Prediction = predict(model, newdata = .),
      ) %>%
      left_join(skier_adjustments, by = "Skier") %>%
      mutate(
        # Regular adjustments
        altitude_effect = replace_na(altitude_effect, 0),
        period_effect = replace_na(period_effect, 0),
        ms_effect = replace_na(ms_effect, 0),
        
        # Volatility metrics
        prediction_volatility = replace_na(prediction_volatility, 0),
        consistency_score = replace_na(consistency_score, 0),
        upside_potential = replace_na(upside_potential, 0),
        downside_risk = replace_na(downside_risk, 0),
        volatility_ratio = replace_na(volatility_ratio, 1),
        n_recent_races = replace_na(n_recent_races, 0),
        
        # Using your existing adjustment approach
        altitude_adjustment = altitude_effect,
        period_adjustment = period_effect,
        ms_adjustment = ms_effect,
        
        # Base prediction and adjustments
        Predicted_Points = Base_Prediction + altitude_adjustment + 
          period_adjustment + ms_adjustment,
        Predicted_Points = pmax(pmin(Predicted_Points, 100), 0),
        
        # Apply race probability to predictions - for race day this is 1.0
        Race_Prob = get(race_prob_col),
        Final_Prediction = Predicted_Points,
        
        # Different scoring scenarios - adjusted by race probability
        confidence_factor = pmin(n_recent_races / 10, 1),
        scaled_upside_potential = upside_potential * (Predicted_Points/100),
        scaled_downside_potential = downside_risk * (Predicted_Points/100),
        
        # Safe prediction (downside)
        Safe_Prediction = pmax(
          (Predicted_Points - (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
          0
        ),
        Safe_Prediction = pmax(
          (Predicted_Points - (abs(scaled_downside_potential) * volatility_ratio * confidence_factor)) * Race_Prob, 
          0
        ),
        
        # Upside prediction
        Upside_Prediction = pmin(
          (Predicted_Points + (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
          100 * Race_Prob  # Cap at 100 * probability
        ),
        Upside_Prediction = pmin(
          (Predicted_Points + (scaled_upside_potential * volatility_ratio * confidence_factor)) * Race_Prob, 
          100 * Race_Prob  # Cap at 100 * probability
        )
      ) %>%
      dplyr::select(Skier, Nation, 
                    Base_Prediction, altitude_adjustment, 
                    period_adjustment, ms_adjustment,
                    prediction_volatility, volatility_ratio, confidence_factor,
                    Final_Prediction, Safe_Prediction, Upside_Prediction,
                    race_prob_col)
    View(race_dfs[[i]])
    
    # Apply pursuit handling if needed
    if(races$Pursuit[i] == 1) {
      # Get startlist points
      startlist_points <- startlist %>%
        mutate(
          # Use the appropriate points system based on the Stage flag
          startlist_points = case_when(
            row_number() <= length(if(is_stage_race) stage_points else wc_points) ~ 
              (if(is_stage_race) stage_points else wc_points)[row_number()],
            TRUE ~ 0
          )
        ) %>%
        dplyr::select(Skier, startlist_points)
      
      # Apply pursuit logic - average predicted points with startlist points
      # Also account for race probability (which is 1.0 for everyone on race day)
      race_dfs[[i]] <- race_dfs[[i]] %>%
        left_join(startlist_points, by = "Skier") %>%
        mutate(
          startlist_points = replace_na(startlist_points, 0),
          Race_Prob = get(race_prob_col),
          # Weighted average of predicted points and startlist points, then apply race probability
          Final_Prediction = ((Predicted_Points + startlist_points) / 2) * Race_Prob,
          Safe_Prediction = ((Safe_Prediction/Race_Prob + startlist_points) / 2) * Race_Prob,
          Upside_Prediction = ((Upside_Prediction/Race_Prob + startlist_points) / 2) * Race_Prob
        ) %>%
        dplyr::select(-startlist_points)
      
      log_info(paste("Applied pursuit logic to race", i))
    }
    
    # Extra check to ensure race probability column exists and is properly named
    if(!race_prob_col %in% names(race_dfs[[i]])) {
      log_warn(paste("Race probability column", race_prob_col, "not in final race_dfs!"))
      # Try to fix - should be 1.0 on race day
      race_dfs[[i]][[race_prob_col]] <- 1.0
    }
  }
  
  # Get number of races from races dataframe
  n_races <- nrow(races)
  
  # Combine all race predictions (points)

  final_predictions <- combine_predictions(race_dfs, startlist)

  log_info(paste("Final predictions calculated for", gender))
  
  # Create post predictions for blog (points)
  post_predictions <- create_post_predictions(final_predictions, n_races)
  
  # Combine all position predictions into one dataframe
  all_position_predictions <- bind_rows(position_predictions)
  
  # Create formatted position probabilities
  formatted_position_results <- format_position_results(all_position_predictions, current_date, gender)
  
  # Create folder path based on today's date for output
  today_folder <- format(current_date, "%Y%m%d")
  dir_path <- paste0("/Users/syverjohansen/blog/daehl-e/content/post/cross-country/drafts/race-picks/", today_folder)
  
  # Create directory if it doesn't exist
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Save points predictions to Excel
  points_file_path <- file.path(dir_path, paste0(ifelse(gender == "men", "men", "ladies"), "-points.xlsx"))
  write.xlsx(post_predictions, file = points_file_path)
  
  log_info(paste("Saved", gender, "points predictions to", points_file_path))
  
  # Return both points and position predictions
  return(list(
    full_predictions = final_predictions,
    post_predictions = post_predictions,
    position_predictions = all_position_predictions,
    formatted_position_results = formatted_position_results
  ))
}

# Helper function to format position probability results
format_position_results <- function(position_results, current_date, gender) {
  View(position_results)
  # Create a more reader-friendly version
  formatted_results <- position_results %>%
    rowwise() %>%
    mutate(
      # Get the appropriate race probability column for the current row's Race number
      race_prob_col = paste0("Race", Race, "_Prob"),
      Participation = if(race_prob_col %in% names(position_results)) position_results[[race_prob_col]][cur_group_id()] else NA_real_,
      Win = prob_top1,
      Podium = prob_top3,
      Top5 = prob_top5,
      Top10 = prob_top10,
      Top30 = prob_top30
    ) %>%
    ungroup() %>%
    dplyr::select(Skier, ID, Nation, Sex, Race, Participation, Win, Podium, Top5, Top10, Top30) %>%
    arrange(Race, desc(Win))
  
  # Create directory path for today's date
  today_folder <- format(current_date, "%Y%m%d")
  dir_path <- paste0(
    "/Users/syverjohansen/blog/daehl-e/content/post/cross-country/drafts/race-picks/", 
    today_folder
  )
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Split by race
  races <- unique(formatted_results$Race)
  race_dfs <- list()
  
  for(race_num in races) {
    race_df <- formatted_results %>%
      filter(Race == race_num) %>%
      dplyr::select(-Race)
    
    sheet_name <- paste0(
      ifelse(gender == "men", "Men", "Ladies"),
      " Race ", race_num
    )
    
    race_dfs[[sheet_name]] <- race_df
  }
  
  # Save to Excel
  output_file <- file.path(
    dir_path,
    paste0(ifelse(gender == "men", "men", "ladies"), "_position_probabilities.xlsx")
  )
  
  write.xlsx(race_dfs, output_file)
  
  log_info(paste("Formatted position probabilities saved to", output_file))
  
  return(race_dfs)
}

# Function to create top contenders summary
create_top_contenders_summary <- function(men_results, ladies_results) {
  # Get position predictions
  men_positions <- if(!is.null(men_results)) men_results$position_predictions else NULL
  ladies_positions <- if(!is.null(ladies_results)) ladies_results$position_predictions else NULL
  
  # Create folder based on today's date
  current_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")
  today_folder <- format(current_date, "%Y%m%d")
  dir_path <- paste0(
    "/Users/syverjohansen/blog/daehl-e/content/post/cross-country/drafts/race-predictions/", 
    today_folder
  )
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  top_contenders <- list()
  
  # Process men's results if they exist
  if(!is.null(men_positions) && nrow(men_positions) > 0) {
    men_races <- unique(men_positions$Race)
    for(race_num in men_races) {
      race_df <- men_positions %>%
        filter(Race == race_num)
      
      # Top 5 for win probability
      win_contenders <- race_df %>%
        arrange(desc(prob_top1)) %>%
        head(5) %>%
        dplyr::select(Skier, Nation, prob_top1) %>%
        rename(`Win%` = prob_top1)
      
      sheet_name <- paste0("Men Race ", race_num, " - Win")
      top_contenders[[sheet_name]] <- win_contenders
      
      # Top 5 for podium probability
      podium_contenders <- race_df %>%
        arrange(desc(prob_top3)) %>%
        head(5) %>%
        dplyr::select(Skier, Nation, prob_top3) %>%
        rename(`Podium%` = prob_top3)
      
      sheet_name <- paste0("Men Race ", race_num, " - Podium")
      top_contenders[[sheet_name]] <- podium_contenders
      
      # Top 5 for Top-5 probability
      top5_contenders <- race_df %>%
        arrange(desc(prob_top5)) %>%
        head(5) %>%
        dplyr::select(Skier, Nation, prob_top5) %>%
        rename(`Top5%` = prob_top5)
      
      sheet_name <- paste0("Men Race ", race_num, " - Top5")
      top_contenders[[sheet_name]] <- top5_contenders
    }
  }
  
  # Process ladies' results if they exist
  if(!is.null(ladies_positions) && nrow(ladies_positions) > 0) {
    ladies_races <- unique(ladies_positions$Race)
    for(race_num in ladies_races) {
      race_df <- ladies_positions %>%
        filter(Race == race_num)
      
      # Top 5 for win probability
      win_contenders <- race_df %>%
        arrange(desc(prob_top1)) %>%
        head(5) %>%
        dplyr::select(Skier, Nation, prob_top1) %>%
        rename(`Win%` = prob_top1)
      
      sheet_name <- paste0("Ladies Race ", race_num, " - Win")
      top_contenders[[sheet_name]] <- win_contenders
      
      # Top 5 for podium probability
      podium_contenders <- race_df %>%
        arrange(desc(prob_top3)) %>%
        head(5) %>%
        dplyr::select(Skier, Nation, prob_top3) %>%
        rename(`Podium%` = prob_top3)
      
      sheet_name <- paste0("Ladies Race ", race_num, " - Podium")
      top_contenders[[sheet_name]] <- podium_contenders
      
      # Top 5 for Top-5 probability
      top5_contenders <- race_df %>%
        arrange(desc(prob_top5)) %>%
        head(5) %>%
        dplyr::select(Skier, Nation, prob_top5) %>%
        rename(`Top5%` = prob_top5)
      
      sheet_name <- paste0("Ladies Race ", race_num, " - Top5")
      top_contenders[[sheet_name]] <- top5_contenders
    }
  }
  
  # Save to Excel if we have any top contenders
  if(length(top_contenders) > 0) {
    output_file <- file.path(dir_path, "top_contenders.xlsx")
    write.xlsx(top_contenders, output_file)
    
    log_info(paste("Top contenders summary saved to", output_file))
  } else {
    log_info("No top contenders to save")
  }
  
  return(top_contenders)
}

# Main function to run the integrated predictions workflow
run_race_predictions_workflow <- function() {
  log_info("Running race day predictions workflow")
  
  # Check for men's races
  if(nrow(men_races) > 0) {
    log_info("Processing men's predictions")
    men_results <- predict_races("men")
  } else {
    log_info("No men's races today")
    men_results <- NULL
  }
  
  # Check for ladies' races
  if(nrow(ladies_races) > 0) {
    log_info("Processing ladies predictions")
    ladies_results <- predict_races("ladies")
  } else {
    log_info("No ladies races today")
    ladies_results <- NULL
  }
  
  # Create top contenders summary if we have any results
  if(!is.null(men_results) || !is.null(ladies_results)) {
    log_info("Creating top contenders summary")
    top_contenders <- create_top_contenders_summary(men_results, ladies_results)
  } else {
    log_info("No results to create top contenders summary")
    top_contenders <- NULL
  }
  
  log_info("Race day prediction workflow complete")
  
  # Return results
  return(list(
    men = men_results,
    ladies = ladies_results,
    top_contenders = top_contenders
  ))
}

# Execute the main workflow
log_info("Starting race-picks.R execution")
results <- run_race_predictions_workflow()
log_info("race-picks.R execution complete")
