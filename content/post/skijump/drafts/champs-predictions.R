# Ski Jumping Championships Predictions: Based on weekly-picks2.R with Championship modifications
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

# Define points systems for Ski Jumping
# World Cup: Top 30 get points
world_cup_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Function to replace NAs with first quartile value
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Set up logging
log_dir <- "~/ski/elo/python/skijump/polars/excel365/champs-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "champs_picks_processing.log")))
log_info("Starting Ski Jumping Championships predictions process")

# Read in the race schedule from weekends.csv with proper date parsing
log_info("Reading weekends data")
weekends <- read.csv("~/ski/elo/python/skijump/polars/excel365/weekends.csv", 
                     stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y"))

# Filter for Championships races only (Championship == 1)
log_info("Filtering for Championships races")
champs_races <- weekends %>%
  filter(Championship == 1) %>%
  arrange(Date)  # Sort chronologically

if (nrow(champs_races) == 0) {
  log_info("No Championships races found. Terminating program.")
  quit(save = "no", status = 0)
}

log_info(paste("Found", nrow(champs_races), "Championships races"))

# Create race dataframes for men and ladies (individual races)
# Add row numbers after chronological sorting to ensure proper order
champs_races_with_race_num <- champs_races %>%
  mutate(OriginalRaceNum = row_number())

men_races <- champs_races_with_race_num %>%
  filter(Sex == "M",
         !grepl("Team", RaceType, ignore.case = TRUE)) %>%  # Exclude any race type containing "Team"
  mutate(race_date = format(Date, "%b %d")) %>%
  dplyr::select(RaceType, Period, Country, OriginalRaceNum, race_date) %>%
  rename(race_type = RaceType, period = Period, country = Country, original_race_num = OriginalRaceNum)

ladies_races <- champs_races_with_race_num %>%
  filter(Sex == "L",
         !grepl("Team", RaceType, ignore.case = TRUE)) %>%  # Exclude any race type containing "Team"
  mutate(race_date = format(Date, "%b %d")) %>%
  dplyr::select(RaceType, Period, Country, OriginalRaceNum, race_date) %>%
  rename(race_type = RaceType, period = Period, country = Country, original_race_num = OriginalRaceNum)

# Create race dataframes for teams
men_teams <- champs_races_with_race_num %>%
  filter(grepl("Team", RaceType, ignore.case = TRUE) & Sex == "M") %>%  # RaceType contains "Team" and Sex is "M"
  mutate(race_date = format(Date, "%b %d")) %>%
  dplyr::select(RaceType, Period, Country, OriginalRaceNum, race_date) %>%
  rename(race_type = RaceType, period = Period, country = Country, original_race_num = OriginalRaceNum)

ladies_teams <- champs_races_with_race_num %>%
  filter(grepl("Team", RaceType, ignore.case = TRUE) & Sex == "L") %>%  # RaceType contains "Team" and Sex is "L"
  mutate(race_date = format(Date, "%b %d")) %>%
  dplyr::select(RaceType, Period, Country, OriginalRaceNum, race_date) %>%
  rename(race_type = RaceType, period = Period, country = Country, original_race_num = OriginalRaceNum)

# Mixed team races (Sex == "Mixed")
mixed_teams <- champs_races_with_race_num %>%
  filter(Sex == "Mixed") %>%  # Mixed team events have Sex == "Mixed"
  mutate(race_date = format(Date, "%b %d")) %>%
  dplyr::select(RaceType, Period, Country, OriginalRaceNum, race_date) %>%
  rename(race_type = RaceType, period = Period, country = Country, original_race_num = OriginalRaceNum)

log_info(paste("Found", nrow(men_races), "men's individual races,", nrow(ladies_races), "ladies' individual races"))
log_info(paste("Found", nrow(men_teams), "men's team races,", nrow(ladies_teams), "ladies' team races"))
log_info(paste("Found", nrow(mixed_teams), "mixed team races"))

# Function to get points based on place for Ski Jumping
get_points <- function(place, hill_size = NULL) {
  # All Ski Jumping events use World Cup points system
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

# Helper function to calculate individual skier's previous points weighted average
calculate_individual_prev_points <- function(skier_name, event_date, race_type, chrono_data) {
  # Filter for this skier's races before the event date, matching race type
  skier_races <- chrono_data %>%
    filter(Skier == skier_name,
           Date < as.Date(event_date),
           RaceType == race_type) %>%
    arrange(desc(Date)) %>%  # Most recent first
    slice_head(n = 5)  # Take last 5 races
  
  if(nrow(skier_races) == 0) {
    return(0)  # No previous races
  }
  
  # Calculate weighted average (most recent gets highest weight)
  weights <- seq(nrow(skier_races), 1)  # Reverse order so most recent = highest weight
  weighted_avg <- weighted.mean(skier_races$Points, w = weights, na.rm = TRUE)
  
  return(weighted_avg)
}

# Helper function to calculate team previous points from individual members
calculate_team_prev_points <- function(team_members, event_date, race_type, individual_chrono_data) {
  if(length(team_members) == 0) {
    return(0)
  }
  
  # Calculate each member's previous points
  member_points <- sapply(team_members, function(member) {
    calculate_individual_prev_points(member, event_date, race_type, individual_chrono_data)
  })
  
  # Average across team members
  team_avg <- mean(member_points, na.rm = TRUE)
  return(team_avg)
}

# Helper function: Iterative constrained normalization
# This properly handles the case where some athletes hit the cap (100% or max_prob)
# by locking capped athletes and only normalizing the remaining probability budget
normalize_with_cap <- function(probs, target_sum, max_prob = 100, max_iterations = 100) {
  if (sum(probs, na.rm = TRUE) == 0) {
    # Edge case: all zeros - distribute evenly
    return(rep(target_sum / length(probs), length(probs)))
  }

  # Phase A: Scale proportionally to target sum first (no capping)
  # This ensures over-predicted athletes are scaled down fairly before any capping
  current_sum <- sum(probs, na.rm = TRUE)
  if (current_sum > 0) {
    probs <- probs * (target_sum / current_sum)
  }

  # Phase B: Cap at max_prob and redistribute excess (iterative)
  for (iter in 1:max_iterations) {
    # Identify athletes above cap
    above_cap <- probs > max_prob

    if (!any(above_cap, na.rm = TRUE)) {
      # No one above cap - we're done
      break
    }

    # Cap those above the limit
    probs[above_cap] <- max_prob

    # Calculate remaining budget for uncapped athletes
    capped_total <- sum(above_cap) * max_prob
    remaining_target <- target_sum - capped_total
    uncapped_sum <- sum(probs[!above_cap], na.rm = TRUE)

    if (remaining_target <= 0) {
      # Edge case: capped athletes alone exceed target
      # This shouldn't happen after Phase A scaling, but handle gracefully
      probs[!above_cap] <- 0
      break
    }

    if (uncapped_sum <= 0) {
      # Edge case: no probability mass in uncapped - distribute remaining evenly
      n_uncapped <- sum(!above_cap)
      if (n_uncapped > 0) {
        probs[!above_cap] <- remaining_target / n_uncapped
      }
      break
    }

    # Scale only uncapped values to hit remaining target
    scaling_factor <- remaining_target / uncapped_sum
    probs[!above_cap] <- probs[!above_cap] * scaling_factor
  }

  return(probs)
}

# Normalization function for position probabilities (5-phase approach)
# Phase 1: Scale to target sum with capping and redistribution
# Phase 2: Apply monotonic constraints + cap at start_prob
# Phase 3: Re-normalize after constraint adjustments
# Phase 4: Final cap at start_prob
# Phase 5: Final monotonic constraint enforcement (critical for credibility)
normalize_position_probabilities <- function(predictions, race_prob_col, position_thresholds) {
  # Make a copy to avoid modifying the original data frame
  normalized <- predictions

  # Store start_prob for later use in capping position probabilities
  if(race_prob_col %in% names(normalized)) {
    normalized$start_prob <- normalized[[race_prob_col]]
  }

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

  # PHASE 1: Initial normalization with iterative constrained capping
  # This properly handles athletes at the 100% cap by locking them and
  # distributing remaining probability budget among uncapped athletes
  log_info("PHASE 1: Iterative constrained normalization...")
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)

    # NOTE: Start probability multiplication commented out for testing (2026-02-01)
    # First, adjust by race participation probability
    # if(race_prob_col %in% names(normalized)) {
    #   normalized[[prob_col]] <- normalized[[prob_col]] * normalized[[race_prob_col]]
    # }

    # Target sum should be 100 * threshold (e.g., 100% for top 1, 300% for top 3)
    target_sum <- 100 * threshold

    n_capped_before <- sum(normalized[[prob_col]] >= 100, na.rm = TRUE)

    # Apply iterative constrained normalization (max_prob = 100 for percentage scale)
    normalized[[prob_col]] <- normalize_with_cap(normalized[[prob_col]], target_sum = target_sum, max_prob = 100)

    n_capped_after <- sum(normalized[[prob_col]] >= 100, na.rm = TRUE)
    if(n_capped_after > 0) {
      log_info(sprintf("    %s: %d athletes at 100%% cap", prob_col, n_capped_after))
    }
  }

  # PHASE 2: Monotonic constraints + cap at start_prob
  log_info("PHASE 2: Applying monotonic constraints...")
  prob_cols <- paste0("prob_top", position_thresholds)
  for(row_i in 1:nrow(normalized)) {
    # NOTE: start_prob capping commented out for testing (2026-02-01)
    # Get start_prob ceiling for this row (handle NA)
    # Note: start_prob is stored as decimal (0-1), but position probs are percentages (0-100)
    # So we multiply start_prob by 100 to get the ceiling in percentage terms
    # start_ceiling <- if("start_prob" %in% names(normalized)) normalized$start_prob[row_i] * 100 else NA
    # if(is.na(start_ceiling)) start_ceiling <- 100  # Default to 100% if no start_prob

    # Get current probabilities
    probs <- sapply(prob_cols, function(col) {
      val <- normalized[[col]][row_i]
      if(is.na(val)) 0 else val
    })

    # NOTE: start_prob capping commented out for testing (2026-02-01)
    # First, cap all position probabilities at start_prob
    # probs <- pmin(probs, start_ceiling)

    # Then enforce monotonic: each probability >= previous one (Win <= Podium <= Top5 <= etc)
    for(j in 2:length(probs)) {
      if(probs[j] < probs[j-1]) {
        probs[j] <- probs[j-1]
      }
    }

    # NOTE: start_prob capping commented out for testing (2026-02-01)
    # Final cap at start_prob (in case monotonic adjustment pushed values up)
    # probs <- pmin(probs, start_ceiling)

    # Update row
    for(k in seq_along(prob_cols)) {
      normalized[[prob_cols[k]]][row_i] <- probs[k]
    }
  }

  # PHASE 3: Re-normalize after monotonic adjustment using iterative constrained normalization
  log_info("PHASE 3: Re-normalizing after monotonic constraints (iterative constrained)...")
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    target_sum <- 100 * threshold

    # Apply iterative constrained normalization - preserves athletes at 100% cap
    normalized[[prob_col]] <- normalize_with_cap(normalized[[prob_col]], target_sum = target_sum, max_prob = 100)
  }

  # PHASE 4: Final cap at start_prob - COMMENTED OUT FOR TESTING (2026-02-01)
  # log_info("PHASE 4: Applying final start_prob ceiling...")
  # if("start_prob" %in% names(normalized)) {
  #   violations_fixed <- 0
  #   for(row_i in 1:nrow(normalized)) {
  #     # Convert start_prob from decimal (0-1) to percentage (0-100) for comparison
  #     start_ceiling <- normalized$start_prob[row_i] * 100
  #     if(is.na(start_ceiling)) next
  #     for(col in prob_cols) {
  #       if(!is.na(normalized[[col]][row_i]) && normalized[[col]][row_i] > start_ceiling) {
  #         normalized[[col]][row_i] <- start_ceiling
  #         violations_fixed <- violations_fixed + 1
  #       }
  #     }
  #   }
  #   if(violations_fixed > 0) {
  #     log_info(sprintf("  Fixed %d cases where position prob exceeded start_prob", violations_fixed))
  #   }
  # }

  # PHASE 5: Final monotonic constraint enforcement
  # This is critical - no prediction is credible if win > podium > top5 etc.
  log_info("PHASE 5: Final monotonic constraint enforcement...")
  monotonic_fixes <- 0
  for(row_i in 1:nrow(normalized)) {
    # Get current probabilities
    probs <- sapply(prob_cols, function(col) {
      val <- normalized[[col]][row_i]
      if(is.na(val)) 0 else val
    })

    # Check if any violations exist
    needs_fix <- FALSE
    for(j in 2:length(probs)) {
      if(probs[j] < probs[j-1]) {
        needs_fix <- TRUE
        break
      }
    }

    if(needs_fix) {
      # Enforce monotonic: each probability >= previous one
      for(j in 2:length(probs)) {
        if(probs[j] < probs[j-1]) {
          probs[j] <- probs[j-1]
        }
      }

      # Update row
      for(k in seq_along(prob_cols)) {
        normalized[[prob_cols[k]]][row_i] <- probs[k]
      }
      monotonic_fixes <- monotonic_fixes + 1
    }
  }
  if(monotonic_fixes > 0) {
    log_info(sprintf("  Fixed monotonic violations in %d rows", monotonic_fixes))
  }

  # Log final sums after all adjustments
  log_info("Position probability sums AFTER 5-phase normalization:")
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

# Function to prepare startlist data with ELO information (adapted for ski jumping)
prepare_startlist_data <- function(startlist, race_df, elo_col, is_team = FALSE) {
  log_info(paste("Preparing startlist data for", elo_col))
  
  # Dynamically get race probability columns - important to preserve these!
  race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
  log_info(paste("Race probability columns found:", paste(race_prob_cols, collapse=", ")))
  
  # Handle team vs individual races
  if(is_team) {
    # For team races, startlist already contains pre-aggregated team data
    # Use it directly without further aggregation
    log_info("Processing team startlist - using pre-aggregated team data")
    
    # The team startlist already has Avg_* columns, use them directly
    result_df <- startlist %>%
      # Select relevant columns from the team startlist
      dplyr::select(Nation, any_of(race_prob_cols), starts_with("Avg_"))
    
    log_info(paste("Using team startlist with", nrow(result_df), "teams"))
    
    # Calculate team Prev_Points_Weighted for prediction startlist
    # Extract race type from elo_col (e.g., "Avg_Large_Elo_Pct" -> "Large")
    race_type_from_col <- gsub("Avg_(.+)_Elo_Pct", "\\1", elo_col)
    
    tryCatch({
      # Determine individual chrono path based on race data
      if("Mixed" %in% race_df$Sex) {
        # Mixed teams - load both chrono files
        log_info("Loading individual chrono for mixed team predictions")
        men_chrono <- read.csv("~/ski/elo/python/skijump/polars/relay/excel365/men_chrono.csv", stringsAsFactors = FALSE) %>%
          mutate(Date = as.Date(Date), Points = sapply(Place, get_points))
        ladies_chrono <- read.csv("~/ski/elo/python/skijump/polars/relay/excel365/ladies_chrono.csv", stringsAsFactors = FALSE) %>%
          mutate(Date = as.Date(Date), Points = sapply(Place, get_points))
        individual_chrono <- bind_rows(men_chrono, ladies_chrono)
      } else {
        # Regular teams - load appropriate gender chrono
        gender_for_chrono <- if("M" %in% race_df$Sex) "men" else "ladies"
        chrono_path <- paste0("~/ski/elo/python/skijump/polars/relay/excel365/", gender_for_chrono, "_chrono.csv")
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
          dplyr::select(-TeamMembers)  # Remove temporary column
      } else {
        # Fallback: set to 0 if no team member info available
        log_warn("No TeamMembers column found in startlist - setting Prev_Points_Weighted to 0")
        result_df$Prev_Points_Weighted <- 0
      }
      
      log_info("Calculated team Prev_Points_Weighted for prediction startlist")
      
    }, error = function(e) {
      log_warn(paste("Error calculating team Prev_Points_Weighted for predictions:", e$message))
      result_df$Prev_Points_Weighted <- 0
    })
  } else {
    # For individual races - check if Elo columns exist
    elo_cols <- c("Normal_Elo", "Large_Elo", "Flying_Elo", "Elo")

    # Keep essential columns from startlist including Elo columns (already from chrono_pred via Python)
    available_elo_cols <- intersect(elo_cols, names(startlist))
    log_info(paste("Available Elo columns in startlist:", paste(available_elo_cols, collapse=", ")))

    base_df <- startlist %>%
      dplyr::select(Skier, ID, Nation, Price, all_of(race_prob_cols), any_of(elo_cols))

    # Get recent points for individuals
    recent_points <- race_df %>%
      filter(Skier %in% base_df$Skier) %>%
      group_by(Skier) %>%
      arrange(Season, Race) %>%
      slice_tail(n = 5) %>%
      summarise(
        Prev_Points_Weighted = if(n() > 0)
          weighted.mean(Points, w = seq_len(n()), na.rm = TRUE)
        else 0,
        .groups = 'drop'
      )

    # Combine individual data
    result_df <- base_df %>%
      left_join(recent_points, by = "Skier")
  }
  
  # Debug: Check elo columns
  log_info(paste("Available elo columns:", paste(names(result_df), collapse=", ")))
  
  # Set up Elo columns to process based on team vs individual
  if(is_team) {
    elo_columns_to_process <- c("Avg_Normal_Elo", "Avg_Large_Elo", "Avg_Flying_Elo", "Avg_Elo")
  } else {
    elo_columns_to_process <- c("Normal_Elo", "Large_Elo", "Flying_Elo", "Elo")
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

# Preprocessing function for historical race data (adapted for ski jumping)
preprocess_data <- function(df) {
  # Load weekends data to determine points systems for historical races
  weekends_data <- read.csv("~/ski/elo/python/skijump/polars/excel365/weekends.csv", 
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
      weights <- seq(1, num_races)
      weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
    })) %>%
    ungroup()

  
  # Check if Elo and Pelo columns exist, if not create them
  elo_cols <- c("Normal_Elo", "Large_Elo", "Flying_Elo", "Elo")
  pelo_cols <- c("Normal_Pelo", "Large_Pelo", "Flying_Pelo", "Pelo")
  
  # Make sure these columns exist (create if missing)
  for (col in c(elo_cols, pelo_cols)) {
    if (!col %in% names(df_with_points)) {
      log_info(paste("Creating missing column:", col))
      df_with_points[[col]] <- 0
    }
  }
  
  # Now apply other preprocessing steps and filter for recent data
  processed_df <- df_with_points %>%
    # Add period (Ski jumping has 4 periods per season)
    group_by(Season) %>%
    mutate(
      Num_Races = max(Race),
      Period = case_when(
        Num_Races <= 8 ~ 1,   # Early season (Nov-Dec)
        Num_Races <= 16 ~ 2,  # Mid season (Dec-Jan)
        Num_Races <= 24 ~ 3,  # Late season (Jan-Feb)
        TRUE ~ 4              # Final season (Mar)
      )
    ) %>%
    ungroup() %>%
    # Add hill size flags for ski jumping
    mutate(
      Small_Flag = ifelse(HillSize <= 70, 1, 0),
      Medium_Flag = ifelse(HillSize > 70 & HillSize <= 85, 1, 0),
      Normal_Flag = ifelse(HillSize > 85 & HillSize <= 115, 1, 0),
      Large_Flag = ifelse(HillSize > 115 & HillSize <= 185, 1, 0),
      Flying_Flag = ifelse(HillSize > 185, 1, 0)
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

# Function to process Championships for a specific gender (adapted for ski jumping)
process_gender_championships <- function(gender, races) {
  log_info(paste("Processing", gender, "Championships with", nrow(races), "races"))
  
  # Read Championships startlist (generated by startlist-scrape-champs.py) 
  startlist_file <- paste0("~/ski/elo/python/skijump/polars/excel365/startlist_champs_", gender, ".csv")
  
  if (!file.exists(startlist_file)) {
    log_info(paste("Championships startlist not found:", startlist_file))
    return(NULL)
  }
  
  startlist <- read.csv(startlist_file, stringsAsFactors = FALSE)
  log_info(paste("Read Championships startlist with", nrow(startlist), "athletes"))
  
  # Determine the race type and paths based on gender
  chrono_path <- paste0("~/ski/elo/python/skijump/polars/excel365/", gender, "_chrono.csv")
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
  
  # Debug: Show races dataframe
  log_info(paste("Races dataframe dimensions:", nrow(races), "x", ncol(races)))
  log_info(paste("Race types:", paste(races$race_type, collapse=", ")))
  if("original_race_num" %in% names(races)) {
    log_info(paste("Original race numbers:", paste(races$original_race_num, collapse=", ")))
  }
  
  # Process each race
  for(i in 1:nrow(races)) {
    race_info <- races[i, ]
    race_type <- race_info$race_type
    
    log_info(sprintf("Processing %s race %d: Race Type %s", gender, i, race_type))
    
    # Get race probability column name for this race (use original race number)
    race_prob_col <- paste0("Race", race_info$original_race_num, "_Prob")
    
    # Debug: Check if this race probability column exists in startlist
    if(race_prob_col %in% names(startlist)) {
      log_info(paste("Race probability column", race_prob_col, "exists in startlist"))
      # Show some stats
      log_info(paste("  Mean:", mean(startlist[[race_prob_col]], na.rm = TRUE)))
      log_info(paste("  Sum:", sum(startlist[[race_prob_col]], na.rm = TRUE)))
    } else {
      log_warn(paste("Race probability column", race_prob_col, "NOT FOUND in startlist!"))
    }
    
    # Filter base dataset for race type (not exact hill size)
    race_df <- df %>%
      filter(RaceType == race_type)

    # Get relevant columns for training (Pelo_Pct) and prediction (Elo_Pct)
    training_elo_col <- paste0(race_type, "_Pelo_Pct")  # Pre-race ELO for training
    prediction_elo_col <- paste0(race_type, "_Elo_Pct")  # Post-race ELO for prediction
    
    # Use all PELO columns as explanatory variables for training (pre-race data)
    explanatory_vars <- c("Prev_Points_Weighted", 
                          "Normal_Pelo_Pct", "Large_Pelo_Pct", "Flying_Pelo_Pct", "Pelo_Pct")
    
    # Filter for top performers and add previous points (use training ELO for filtering)
    race_df_75 <- race_df %>%
      filter(get(training_elo_col) > 0.75) %>%
      group_by(!!sym(participant_col)) %>%
      arrange(Season, Race) %>%
      ungroup()

    # Create position probability models using the same variables as points model
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
        log_info(paste("Starting regsubsets for threshold", threshold))
        log_info(paste("Formula:", paste(deparse(pos_formula), collapse = " ")))
        log_info(paste("Data dimensions:", nrow(race_df), "x", ncol(race_df)))
        log_info(paste("Number of TRUE outcomes:", sum(race_df$position_achieved)))
        log_info(paste("Number of FALSE outcomes:", sum(!race_df$position_achieved)))
        
        pos_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")
        log_info("regsubsets completed successfully")
        
        pos_summary <- summary(pos_selection)
        log_info("summary() completed successfully")
        
        pos_best_bic_vars <- names(coef(pos_selection, which.min(pos_summary$bic)))
        log_info(paste("Best BIC variables selected:", paste(pos_best_bic_vars, collapse=", ")))
        
        # Create smooth terms for GAM using best BIC variables (remove intercept)
        pos_smooth_terms <- paste("s(", pos_best_bic_vars[-1], ")", collapse=" + ")
        log_info(paste("Smooth terms created:", pos_smooth_terms))
        
        pos_gam_formula <- as.formula(paste("position_achieved ~", pos_smooth_terms))
        log_info(paste("GAM formula:", paste(deparse(pos_gam_formula), collapse = " ")))
        
        # Fit the position model with binomial family
        log_info("Attempting to fit GAM model...")
        position_model <- gam(pos_gam_formula,
                              data = race_df,
                              family = binomial,
                              method = "REML")
        log_info("GAM model fitted successfully")
        
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
        log_warn(paste("ERROR in position model for threshold", threshold))
        log_warn(paste("Error message:", e$message))
        log_warn(paste("Error class:", class(e)))
        log_warn(paste("Call stack:", paste(sys.calls(), collapse=" -> ")))
        
        # Log data state when error occurred
        log_warn(paste("Data dimensions at error:", nrow(race_df), "x", ncol(race_df)))
        log_warn(paste("Available columns:", paste(names(race_df), collapse=", ")))
        log_warn(paste("Missing values in key columns:"))
        for(col in position_feature_vars) {
          if(col %in% names(race_df)) {
            na_count <- sum(is.na(race_df[[col]]))
            log_warn(paste("  ", col, ":", na_count, "NAs"))
          } else {
            log_warn(paste("  ", col, ": COLUMN MISSING"))
          }
        }
        
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
          # Last resort fallback - just use the elo column
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

    # Prepare startlist data - use prediction ELO (post-race) for startlist
    startlist_prepared <- prepare_startlist_data(startlist, race_df, prediction_elo_col)
    
    # Create Pelo_Pct columns for prediction by copying from Elo_Pct columns
    # The model was trained on Pelo_Pct but for prediction we use current Elo_Pct values
    pelo_pct_columns <- c("Normal_Pelo_Pct", "Large_Pelo_Pct", "Flying_Pelo_Pct", "Pelo_Pct")
    elo_pct_columns <- c("Normal_Elo_Pct", "Large_Elo_Pct", "Flying_Elo_Pct", "Elo_Pct")
    
    for(i in seq_along(pelo_pct_columns)) {
      pelo_col <- pelo_pct_columns[i]
      elo_col <- elo_pct_columns[i]
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
    
    # Make position probability predictions with adjustments
    position_preds <- data.frame(startlist_prepared[[participant_col]])
    names(position_preds)[1] <- participant_col

    # Add ID and Nation for individual races
    position_preds$ID <- startlist_prepared$ID
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
          
          # Debug model and data before prediction
          log_info(paste("Model formula:", paste(deparse(pos_model$formula), collapse = " ")))
          log_info(paste("Startlist columns:", paste(names(startlist_prepared)[1:min(15, length(names(startlist_prepared)))], collapse=", ")))
          
          # Check if required variables exist in startlist
          model_vars <- all.vars(pos_model$formula)[-1]  # Remove response variable
          missing_vars <- model_vars[!model_vars %in% names(startlist_prepared)]
          if(length(missing_vars) > 0) {
            log_warn(paste("Missing model variables in startlist:", paste(missing_vars, collapse=", ")))
          } else {
            log_info("All model variables found in startlist")
          }
          
          # Make predictions with explicit try-catch
          base_predictions <- tryCatch({
            # Debug output
            log_info(paste("Attempting prediction for threshold", threshold, "with", nrow(startlist_prepared), "rows"))
            
            # Explicit call to mgcv::predict.gam to avoid method dispatch issues
            predictions <- mgcv::predict.gam(pos_model, newdata = startlist_prepared, type = "response")
            log_info(paste("Raw predictions summary - Min:", round(min(predictions), 4), "Max:", round(max(predictions), 4), "Mean:", round(mean(predictions), 4)))
            predictions
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
    
    # Add original race number to position predictions
    position_preds$Race <- race_info$original_race_num
    
    # Store position predictions for this race (use original race number as key)
    position_predictions[[as.character(race_info$original_race_num)]] <- position_preds
  }
  
  # Get number of races from races dataframe
  n_races <- nrow(races)
  
  # Combine all position predictions into one dataframe
  all_position_predictions <- bind_rows(position_predictions)
  
  # Debug: Check the combined data
  log_info(paste("Combined position predictions dimensions:", nrow(all_position_predictions), "x", ncol(all_position_predictions)))
  if("Race" %in% names(all_position_predictions)) {
    log_info(paste("Unique Race values:", paste(unique(all_position_predictions$Race), collapse=", ")))
    log_info(paste("Race column type:", class(all_position_predictions$Race)))
  } else {
    log_warn("Race column NOT FOUND in all_position_predictions!")
  }
  
  # Create summary by athlete from position predictions
  athlete_summary <- all_position_predictions %>%
    group_by(Skier, Nation) %>%
    summarise(
      Win = mean(prob_top1, na.rm = TRUE),
      Podium = mean(prob_top3, na.rm = TRUE),
      `Top-5` = mean(prob_top5, na.rm = TRUE),
      `Top-10` = mean(prob_top10, na.rm = TRUE),
      Races = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(Win))
  
  # Create output directory
  champs_date <- format(Sys.Date(), "%Y")
  dir_path <- paste0("~/blog/daehl-e/content/post/skijump/drafts/champs-predictions/", champs_date)
  
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

  # Track race type counts to handle duplicates (e.g., two "Normal Hill" races)
  race_type_counts <- list()

  for(race_num in unique_races) {
    log_info(paste("Processing sheet for race", race_num))
    race_data <- all_position_predictions[all_position_predictions$Race == race_num, ]

    # Select and rename columns to simplified format
    race_data <- race_data %>%
      mutate(
        Start = round(if("start_prob" %in% names(.)) start_prob * 100 else 100, 1)
      ) %>%
      dplyr::select(Skier, ID, Nation, Start, prob_top1, prob_top3, prob_top5, prob_top10, prob_top30) %>%
      rename(
        Win = prob_top1,
        Podium = prob_top3,
        Top5 = prob_top5,
        `Top-10` = prob_top10,
        `Top-30` = prob_top30
      ) %>%
      arrange(desc(Win))

    # Get race info for sheet naming using original race number
    race_info_for_sheet <- races %>%
      filter(original_race_num == race_num)

    race_type <- if(nrow(race_info_for_sheet) > 0) race_info_for_sheet$race_type[1] else paste("Race", race_num)
    race_date <- if(nrow(race_info_for_sheet) > 0) race_info_for_sheet$race_date[1] else ""

    # Get the order of this race within this gender (1, 2, 3, etc.)
    race_order <- which(races$original_race_num == race_num)

    # Create sheet name with format "N. RaceType - Mon DD"
    sheet_name <- paste0(race_order, ". ", race_type, " - ", race_date)

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

# Calculate race probabilities for Championships (like weekly-picks2.R but with 4-person quota)
calculate_championships_race_probabilities <- function() {
  log_info("Calculating Championships race participation probabilities with 4-person quota constraint")
  
  # Function to get base race probability for a skier (all races, not hill-specific)
  get_base_race_probability <- function(chronos, participant) {
    # Get participant's first ever race date
    participant_first_race <- chronos %>%
      filter(Skier == participant) %>%
      arrange(Date) %>%
      slice_head(n = 1) %>%
      pull(Date)

    # Calculate date from 5 years ago
    five_years_ago <- Sys.Date() - (5 * 365)

    # Use 5 years ago or participant's first race, whichever is later
    # This ensures newer athletes are only compared against races since they started
    cutoff_date <- if(length(participant_first_race) == 0) {
      five_years_ago
    } else {
      max(five_years_ago, participant_first_race, na.rm = TRUE)
    }

    # Get all races since cutoff date, sorted by date
    all_races <- chronos %>%
      filter(Date >= cutoff_date) %>%
      distinct(Date, City) %>%
      arrange(Date)

    if(nrow(all_races) == 0) return(0)

    # For each race, check if participant was in it
    participant_races <- chronos %>%
      filter(Date >= cutoff_date, Skier == participant) %>%
      distinct(Date, City)

    # Create participation vector (1 = participated, 0 = did not)
    all_races$participated <- as.integer(paste(all_races$Date, all_races$City) %in%
                                          paste(participant_races$Date, participant_races$City))

    n_races <- nrow(all_races)

    # Exponential decay weighting (alpha = 0.1)
    # Most recent race gets weight 1.0, older races get exponentially less weight
    # Weight = exp(-0.1 * distance_from_most_recent)
    race_weights <- exp(-0.1 * ((n_races - 1):0))

    # Calculate weighted participation probability
    weighted_participation <- sum(all_races$participated * race_weights)
    total_weight <- sum(race_weights)

    if(total_weight == 0) return(0)

    base_prob <- weighted_participation / total_weight

    return(min(1, base_prob))
  }
  
  # Process Championships startlists and add race probabilities
  if(nrow(men_races) > 0) {
    log_info("Processing men's Championships race probabilities")
    men_chrono <- read.csv("~/ski/elo/python/skijump/polars/excel365/men_chrono.csv", 
                           stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
    
    men_startlist <- read.csv("~/ski/elo/python/skijump/polars/excel365/startlist_champs_men.csv", 
                              stringsAsFactors = FALSE)
    
    # Calculate base probabilities for each race (same for all races since ski jumpers compete on all hill sizes)
    for(i in 1:nrow(men_races)) {
      original_race_num <- men_races$original_race_num[i]
      race_col <- paste0("Race", original_race_num, "_Prob")
      
      men_startlist[[race_col]] <- sapply(men_startlist$Skier, function(skier) {
        get_base_race_probability(men_chrono, skier)
      })
    }
    
    # Apply 4-person quota constraint per nation per race
    for(i in 1:nrow(men_races)) {
      original_race_num <- men_races$original_race_num[i]
      race_col <- paste0("Race", original_race_num, "_Prob")
      
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
              "~/ski/elo/python/skijump/polars/excel365/startlist_champs_men.csv", 
              row.names = FALSE)
  }
  
  if(nrow(ladies_races) > 0) {
    log_info("Processing ladies' Championships race probabilities")
    ladies_chrono <- read.csv("~/ski/elo/python/skijump/polars/excel365/ladies_chrono.csv", 
                              stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
    
    ladies_startlist <- read.csv("~/ski/elo/python/skijump/polars/excel365/startlist_champs_ladies.csv", 
                                 stringsAsFactors = FALSE)
    
    # Calculate base probabilities for each race (same for all races since ski jumpers compete on all hill sizes)
    for(i in 1:nrow(ladies_races)) {
      original_race_num <- ladies_races$original_race_num[i]
      race_col <- paste0("Race", original_race_num, "_Prob")
      
      ladies_startlist[[race_col]] <- sapply(ladies_startlist$Skier, function(skier) {
        get_base_race_probability(ladies_chrono, skier)
      })
    }
    
    # Apply 4-person quota constraint per nation per race
    for(i in 1:nrow(ladies_races)) {
      original_race_num <- ladies_races$original_race_num[i]
      race_col <- paste0("Race", original_race_num, "_Prob")
      
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
              "~/ski/elo/python/skijump/polars/excel365/startlist_champs_ladies.csv", 
              row.names = FALSE)
  }
  
  log_info("Championships race probability calculation complete")
}

# Function to process team Championships for a specific gender
process_team_championships <- function(gender, races) {
  log_info(paste("Processing", gender, "team Championships with", nrow(races), "races"))
  
  # Read Championships team startlist (generated by startlist-scrape-champs.py) 
  if(gender == "Mixed") {
    startlist_file <- "~/ski/elo/python/skijump/polars/relay/excel365/startlist_champs_mixed_team.csv"
  } else {
    startlist_file <- paste0("~/ski/elo/python/skijump/polars/relay/excel365/startlist_champs_", gender, "_team.csv")
  }
  
  if (!file.exists(startlist_file)) {
    log_warn(paste("Team Championships startlist not found:", startlist_file))
    log_info("This likely means insufficient athletes per nation for team composition:")
    if(gender == "Mixed") {
      log_info("  Mixed teams require 2+ men AND 2+ ladies per nation")
    } else {
      log_info(paste("  Regular teams require 4+", gender, "athletes per nation"))
    }
    return(NULL)
  }
  
  startlist <- read.csv(startlist_file, stringsAsFactors = FALSE)
  log_info(paste("Read team Championships startlist with", nrow(startlist), "teams"))
  
  # For team competitions, use Nation as participant column
  participant_col <- "Nation"
  
  # Load team chronological data (try both sources)
  if(gender == "Mixed") {
    # For mixed teams, use pre-aggregated mixed team chrono file
    mixed_chrono_path <- "~/ski/elo/python/skijump/polars/relay/excel365/mixed_team_chrono.csv"
    
    tryCatch({
      df <- read.csv(mixed_chrono_path, stringsAsFactors = FALSE) %>%
        mutate(Date = as.Date(Date))
      
      # Mixed team data is already pre-processed, just rename columns to match expected format
      df <- df %>%
        rename_with(~gsub("^Team_", "Avg_", .x), starts_with("Team_")) %>%
        # Add required columns if missing
        mutate(
          # Calculate team points from placement using ski jumping points system
          Points = sapply(Place, get_points)
        )
      
      # Load individual chrono data to calculate mixed team Prev_Points_Weighted
      tryCatch({
        log_info("Loading both men's and ladies' individual chrono data for mixed teams")
        men_chrono <- read.csv("~/ski/elo/python/skijump/polars/relay/excel365/men_chrono.csv", stringsAsFactors = FALSE) %>%
          mutate(Date = as.Date(Date), Points = sapply(Place, get_points), Sex = "M")
        ladies_chrono <- read.csv("~/ski/elo/python/skijump/polars/relay/excel365/ladies_chrono.csv", stringsAsFactors = FALSE) %>%
          mutate(Date = as.Date(Date), Points = sapply(Place, get_points), Sex = "L")
        
        # Combine both for mixed team calculations
        combined_chrono <- bind_rows(men_chrono, ladies_chrono)
        
        log_info(paste("Loaded combined individual chrono with", nrow(combined_chrono), "rows"))
        
        # Calculate mixed team Prev_Points_Weighted for each team event
        df <- df %>%
          rowwise() %>%
          mutate(
            Prev_Points_Weighted = {
              # Extract team members from the TeamMembers column (comma-separated)
              if(!is.na(TeamMembers) && TeamMembers != "") {
                team_members <- trimws(strsplit(TeamMembers, ",")[[1]])
                calculate_team_prev_points(team_members, Date, RaceType, combined_chrono)
              } else {
                0
              }
            }
          ) %>%
          ungroup()
        
        log_info("Calculated mixed team Prev_Points_Weighted successfully")
        
      }, error = function(e) {
        log_warn(paste("Error loading individual chrono data for mixed teams:", e$message))
        log_warn("Setting Prev_Points_Weighted to 0 for all mixed teams")
        df$Prev_Points_Weighted <- 0
      })
      
      # Create percentage columns for mixed team data
      team_elo_cols <- c("Avg_Normal_Elo", "Avg_Large_Elo", "Avg_Flying_Elo", "Avg_Elo")
      team_pelo_cols <- c("Avg_Normal_Pelo", "Avg_Large_Pelo", "Avg_Flying_Pelo", "Avg_Pelo")
      
      # Calculate percentage columns
      df <- df %>%
        group_by(Season, Race) %>%
        mutate(
          across(all_of(team_elo_cols), 
                 ~{max_val <- max(.x, na.rm = TRUE); if(max_val > 0) .x / max_val else 0.5}, 
                 .names = "{.col}_Pct"),
          across(all_of(team_pelo_cols), 
                 ~{max_val <- max(.x, na.rm = TRUE); if(max_val > 0) .x / max_val else 0.5}, 
                 .names = "{.col}_Pct")
        ) %>%
        ungroup()
      
      log_info("Using pre-aggregated mixed team data for predictions")
      log_info(paste("Mixed team data loaded with columns:", paste(names(df)[1:min(15, ncol(df))], collapse=", ")))
      
    }, error = function(e) {
      log_warn(paste("Error reading mixed team chrono data:", e$message))
      return(NULL)
    })
  } else {
    # Regular team data - use pre-aggregated team chrono files
    chrono_path <- paste0("~/ski/elo/python/skijump/polars/relay/excel365/", gender, "_team_chrono.csv")
    
    tryCatch({
      df <- read.csv(chrono_path, stringsAsFactors = FALSE) %>%
        mutate(Date = as.Date(Date))
      
      # Team data is already pre-processed, just rename columns to match expected format
      log_info("Before renaming - sample columns:")
      log_info(paste(names(df)[1:min(15, ncol(df))], collapse=", "))
      
      df <- df %>%
        rename_with(~gsub("^Team_", "Avg_", .x), starts_with("Team_")) %>%
        # Add required columns if missing
        mutate(
          # Calculate team points from placement using ski jumping points system
          Points = sapply(Place, get_points)
        )
      
      # Load individual chrono data to calculate team Prev_Points_Weighted
      individual_chrono_path <- paste0("~/ski/elo/python/skijump/polars/relay/excel365/", tolower(gender), "_chrono.csv")
      
      tryCatch({
        log_info(paste("Loading individual chrono data from:", individual_chrono_path))
        individual_chrono <- read.csv(individual_chrono_path, stringsAsFactors = FALSE) %>%
          mutate(Date = as.Date(Date),
                 Points = sapply(Place, get_points))
        
        log_info(paste("Loaded individual chrono with", nrow(individual_chrono), "rows"))
        
        # Calculate team Prev_Points_Weighted for each team event
        df <- df %>%
          rowwise() %>%
          mutate(
            Prev_Points_Weighted = {
              # Extract team members from the TeamMembers column (comma-separated)
              if(!is.na(TeamMembers) && TeamMembers != "") {
                team_members <- trimws(strsplit(TeamMembers, ",")[[1]])
                calculate_team_prev_points(team_members, Date, RaceType, individual_chrono)
              } else {
                0
              }
            }
          ) %>%
          ungroup()
        
        log_info("Calculated team Prev_Points_Weighted successfully")
        
      }, error = function(e) {
        log_warn(paste("Error loading individual chrono data:", e$message))
        log_warn("Setting Prev_Points_Weighted to 0 for all teams")
        df$Prev_Points_Weighted <- 0
      })
      
      log_info("After renaming - sample columns:")
      log_info(paste(names(df)[1:min(15, ncol(df))], collapse=", "))
      
      # Create percentage columns for team data
      team_elo_cols <- c("Avg_Normal_Elo", "Avg_Large_Elo", "Avg_Flying_Elo", "Avg_Elo")
      team_pelo_cols <- c("Avg_Normal_Pelo", "Avg_Large_Pelo", "Avg_Flying_Pelo", "Avg_Pelo")
      
      # Calculate percentage columns - handle missing columns gracefully
      df <- df %>%
        group_by(Season, Race) %>%
        mutate(
          across(any_of(team_elo_cols), 
                 ~{max_val <- max(.x, na.rm = TRUE); if(is.finite(max_val) && max_val > 0) .x / max_val else 0.5}, 
                 .names = "{.col}_Pct"),
          across(any_of(team_pelo_cols), 
                 ~{max_val <- max(.x, na.rm = TRUE); if(is.finite(max_val) && max_val > 0) .x / max_val else 0.5}, 
                 .names = "{.col}_Pct")
        ) %>%
        ungroup()
      
      # Ensure all required percentage columns exist (create with default values if missing)
      required_pct_cols <- c(paste0(team_elo_cols, "_Pct"), paste0(team_pelo_cols, "_Pct"))
      for(col in required_pct_cols) {
        if(!col %in% names(df)) {
          log_info(paste("Creating missing percentage column:", col))
          df[[col]] <- 0.5  # Default value
        }
      }
      
      log_info("After percentage calculation - final columns:")
      log_info(paste(names(df)[1:min(15, ncol(df))], collapse=", "))
      
      # Check if required percentage columns exist
      required_cols <- c("Avg_Large_Pelo_Pct", "Avg_Large_Elo_Pct")
      for(col in required_cols) {
        if(col %in% names(df)) {
          log_info(paste("✓", col, "exists"))
        } else {
          log_warn(paste("✗", col, "missing"))
        }
      }
      
    }, error = function(e) {
      log_warn(paste("Error reading team chrono data:", e$message))
      return(NULL)
    })
  }
  
  if(is.null(df)) {
    log_error("Could not load chronological data for team predictions")
    return(NULL)
  }
  
  # Use full ML approach for team predictions (same as race-picks.R)
  # Process team predictions using GAM models with position probabilities
  
  participant_col <- "Nation"
  
  # Initialize results list  
  race_predictions <- list()
  position_predictions <- list()
  
  # Define position thresholds for teams
  position_thresholds <- c(1, 3, 5)  # Win, Podium, Top 5, Top 10, Top 30
  
  # Process each team race
  for(i in 1:nrow(races)) {
    race_info <- races[i, ]
    race_type <- race_info$race_type
    
    log_info(sprintf("Processing %s team race %d: Race Type %s", gender, i, race_type))
    
    # Get race probability column name for this race (use original race number)
    race_prob_col <- paste0("Race", race_info$original_race_num, "_Prob")

    # Construct the chrono RaceType filter value
    # Chrono files have "Team Normal", "Team Large", etc.
    # Schedule may have just "Normal" or "Team Normal"
    chrono_race_type <- if(grepl("Team", race_type)) {
      race_type  # Already has "Team" prefix
    } else {
      paste("Team", race_type)  # Add "Team" prefix
    }

    # Filter base dataset for race type
    race_df <- df %>%
      filter(RaceType == chrono_race_type)

    log_info(paste("Filtering for RaceType:", chrono_race_type))
    log_info(paste("Filtered data dimensions:", nrow(race_df), "x", ncol(race_df)))

    # Skip if no historical data for this race type
    if(nrow(race_df) == 0) {
      log_warn(paste("No historical data for race type:", chrono_race_type, "- skipping"))
      next
    }

    log_info("Available columns in team race_df:")
    log_info(paste(names(race_df), collapse=", "))

    # Define ELO columns for training (Pelo) and prediction (Elo)
    # Extract the hill size from race type (e.g., "Team Large" -> "Large", or "Normal" -> "Normal")
    hill_size <- if(grepl("Team", race_type)) {
      gsub("Team ", "", race_type)
    } else {
      race_type
    }
    training_elo_col <- paste0("Avg_", hill_size, "_Pelo_Pct")  # Pre-race ELO for training
    prediction_elo_col <- paste0("Avg_", hill_size, "_Elo_Pct")  # Post-race ELO for prediction

    # Use team-specific explanatory variables for training (pre-race data)
    explanatory_vars <- c("Prev_Points_Weighted",
                          "Avg_Normal_Pelo_Pct", "Avg_Large_Pelo_Pct",
                          "Avg_Flying_Pelo_Pct", "Avg_Pelo_Pct")

    # Filter for top performers and add previous points (use training ELO for filtering)
    race_df_75 <- race_df %>%
      filter(get(training_elo_col) > 0.75) %>%
      group_by(Nation) %>%
      arrange(Season, Race) %>%
      ungroup()

    # Check if we have enough data for modeling
    if(nrow(race_df_75) < 10) {
      log_warn(paste("Not enough data for team race", i, "- only", nrow(race_df_75), "rows after filtering. Skipping."))
      next
    }

    # Feature selection and model fitting for points prediction
    response_variable <- "Points"
    
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
      log_warn(paste("Error in team model selection:", e$message))
      # Fallback to simpler model
      fallback_formula <- as.formula(paste("Points ~ s(", training_elo_col, ", k=3)"))
      model <- gam(fallback_formula, data = race_df_75)
    })
    
    # Store race model
    race_predictions[[i]] <- list(
      race_type = race_type,
      model = model,
      data = race_df_75
    )
    
    # Create position models for each threshold
    position_models <- list()
    position_adjustments <- list()
    
    for(threshold in position_thresholds) {
      log_info(paste("Creating team model for top", threshold, "positions"))
      
      # Create binary outcome variable for position threshold
      race_df$position_achieved <- race_df$Place <= threshold
      
      # Create formula for regsubsets using the same explanatory variables as the points model
      pos_formula <- as.formula(paste("position_achieved ~", paste(explanatory_vars, collapse = " + ")))
      
      # Use regsubsets to select best features for this position threshold
      tryCatch({
        pos_exhaustive_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")
        pos_summary_exhaustive <- summary(pos_exhaustive_selection)
        pos_best_bic_vars <- names(coef(pos_exhaustive_selection, which.min(pos_summary_exhaustive$bic)))
        pos_smooth_terms <- paste("s(", pos_best_bic_vars[-1], ")", collapse=" + ")
        
        pos_gam_formula <- as.formula(paste("position_achieved ~", pos_smooth_terms))
        
        # Fit the position model with binomial family
        position_model <- gam(pos_gam_formula,
                             family = binomial(),
                             data = race_df)
        
        position_models[[paste0("threshold_", threshold)]] <- position_model
        
        # Calculate Brier score
        predictions <- predict(position_model, type = "response")
        brier_score <- mean((predictions - race_df$position_achieved)^2)
        log_info(paste("Brier score for team threshold", threshold, ":", round(brier_score, 4)))
        
      }, error = function(e) {
        log_warn(paste("ERROR in team position model for threshold", threshold, ":", e$message))
        position_models[[paste0("threshold_", threshold)]] <- NULL
      })
    }
    
    # Prepare startlist data - use prediction ELO (post-race) for startlist
    startlist_prepared <- prepare_startlist_data(startlist, race_df, prediction_elo_col, is_team = TRUE)
    
    # Create Pelo_Pct columns for prediction by copying from Elo_Pct columns
    # The model was trained on Pelo_Pct but for prediction we use current Elo_Pct values
    pelo_pct_columns <- c("Avg_Normal_Pelo_Pct", "Avg_Large_Pelo_Pct", "Avg_Flying_Pelo_Pct", "Avg_Pelo_Pct")
    elo_pct_columns <- c("Avg_Normal_Elo_Pct", "Avg_Large_Elo_Pct", "Avg_Flying_Elo_Pct", "Avg_Elo_Pct")
    
    for(j in seq_along(pelo_pct_columns)) {
      pelo_col <- pelo_pct_columns[j]
      elo_col <- elo_pct_columns[j]
      if(elo_col %in% names(startlist_prepared)) {
        startlist_prepared[[pelo_col]] <- startlist_prepared[[elo_col]]
        log_info(paste("Created", pelo_col, "from", elo_col, "for team prediction"))
      } else {
        startlist_prepared[[pelo_col]] <- 0.5  # Default value
        log_warn(paste("Missing", elo_col, "- using default for team", pelo_col))
      }
    }
    
    # Make position probability predictions
    position_preds <- data.frame(startlist_prepared[[participant_col]])
    names(position_preds)[1] <- participant_col
    
    for(threshold in position_thresholds) {
      prob_col <- paste0("prob_top", threshold)
      
      if(!is.null(position_models[[paste0("threshold_", threshold)]])) {
        tryCatch({
          model <- position_models[[paste0("threshold_", threshold)]]
          position_preds[[paste0(prob_col, "_base")]] <- predict(model, newdata = startlist_prepared, type = "response")
          position_preds[[prob_col]] <- position_preds[[paste0(prob_col, "_base")]]
          
          # Convert to percentage and round
          position_preds[[prob_col]] <- round(position_preds[[prob_col]] * 100, 1)
          
          # Clean up base prediction column
          position_preds <- position_preds %>%
            dplyr::select(-paste0(prob_col, "_base"))
          
          log_info(paste("Made team predictions for position threshold", threshold))
          
        }, error = function(e) {
          log_warn(paste("Team prediction failed for threshold", threshold, ":", e$message))
          position_preds[[prob_col]] <- rep(threshold, nrow(position_preds))
        })
      } else {
        log_warn(paste("No team model found for threshold", threshold))
        position_preds[[prob_col]] <- NA
      }
    }
    
    # Add race probability column for normalization
    if(race_prob_col %in% names(startlist_prepared)) {
      position_preds[[race_prob_col]] <- startlist_prepared[[race_prob_col]]
    }
    
    # Normalize team position probabilities to ensure they sum to the correct totals
    position_preds <- normalize_position_probabilities(position_preds, race_prob_col, position_thresholds)
    
    # Enforce probability constraints AFTER normalization: Win <= Podium <= Top5
    position_preds <- enforce_probability_constraints(position_preds)
    
    # Add verification logging for each threshold
    log_info(sprintf("Team Race %d position probability sums after normalization:", i))
    for(threshold in position_thresholds) {
      prob_col <- paste0("prob_top", threshold)
      if(prob_col %in% names(position_preds)) {
        sum_val <- sum(position_preds[[prob_col]], na.rm = TRUE)
        log_info(sprintf("  %s: %.2f%% (should be %d%%)", 
                         prob_col, sum_val, 100 * threshold))
      }
    }
    
    # Add original race number to position predictions
    position_preds$Race <- race_info$original_race_num
    
    # Store position predictions for this race (use original race number as key)
    position_predictions[[as.character(race_info$original_race_num)]] <- position_preds
  }
  
  # Combine all position predictions into one dataframe
  all_position_predictions <- bind_rows(position_predictions)
  
  # Create summary by team from position predictions
  team_predictions <- all_position_predictions %>%
    group_by(Nation) %>%
    summarise(
      Win = mean(prob_top1, na.rm = TRUE),
      Podium = mean(prob_top3, na.rm = TRUE),
      `Top-5` = mean(prob_top5, na.rm = TRUE),
      Races = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(Win))

  # Add TeamMembers column from startlist if available
  if("TeamMembers" %in% names(startlist)) {
    team_members_lookup <- startlist %>%
      dplyr::select(Nation, TeamMembers) %>%
      distinct()
    team_predictions <- team_predictions %>%
      left_join(team_members_lookup, by = "Nation") %>%
      rename(Team = TeamMembers) %>%
      dplyr::select(Nation, Team, everything())
  }

  # Create output directory
  champs_date <- format(Sys.Date(), "%Y")
  dir_path <- paste0("~/blog/daehl-e/content/post/skijump/drafts/champs-predictions/", champs_date)
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Save team predictions
  if(gender == "Mixed") {
    output_file <- file.path(dir_path, "mixed_team.xlsx")
  } else {
    output_file <- file.path(dir_path, paste0(gender, "_team.xlsx"))
  }
  
  write.xlsx(team_predictions, output_file)
  log_info(paste("Saved", gender, "team Championships predictions to", output_file))
  
  return(team_predictions)
}

# Calculate race probabilities before processing predictions
calculate_championships_race_probabilities()

# Process individual Championships
men_results <- NULL
if(nrow(men_races) > 0) {
  men_results <- process_gender_championships("men", men_races)
}

ladies_results <- NULL
if(nrow(ladies_races) > 0) {
  ladies_results <- process_gender_championships("ladies", ladies_races)
}

# Process team Championships
men_team_results <- NULL
if(nrow(men_teams) > 0) {
  log_info("Men's team races scheduled - attempting to process")
  men_team_results <- process_team_championships("men", men_teams)
} else {
  log_info("No men's team races scheduled")
}

ladies_team_results <- NULL
if(nrow(ladies_teams) > 0) {
  log_info("Ladies' team races scheduled - attempting to process")
  ladies_team_results <- process_team_championships("ladies", ladies_teams)
} else {
  log_info("No ladies' team races scheduled")
}

mixed_team_results <- NULL
if(nrow(mixed_teams) > 0) {
  log_info("Mixed team races scheduled - attempting to process")
  mixed_team_results <- process_team_championships("Mixed", mixed_teams)
} else {
  log_info("No mixed team races scheduled")
}

# ============================================================================
# Create Nations Excel File
# ============================================================================
log_info("=== Creating Nations Excel File ===")

# Check if we have any results to process
has_men_results <- exists("men_results") && !is.null(men_results)
has_ladies_results <- exists("ladies_results") && !is.null(ladies_results)

if (has_men_results || has_ladies_results) {
  # Create output directory
  output_dir <- paste0("~/blog/daehl-e/content/post/skijump/drafts/champs-predictions/", format(Sys.Date(), "%Y"))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Combine men's race results with Race column (extract just race type from sheet name)
  men_individual_results <- data.frame()
  if (has_men_results && !is.null(men_results$race_sheets)) {
    for (race_name in names(men_results$race_sheets)) {
      race_data <- men_results$race_sheets[[race_name]]
      # Extract just race type from "N. RaceType - Mon DD" format
      race_type <- sub("^\\d+\\. ", "", race_name)  # Remove leading number
      race_type <- sub(" - .*$", "", race_type)      # Remove date suffix
      race_data$Race <- race_type
      race_data$Gender <- "Men"
      men_individual_results <- bind_rows(men_individual_results, race_data)
    }
  }

  # Combine ladies' race results with Race column (extract just race type from sheet name)
  ladies_individual_results <- data.frame()
  if (has_ladies_results && !is.null(ladies_results$race_sheets)) {
    for (race_name in names(ladies_results$race_sheets)) {
      race_data <- ladies_results$race_sheets[[race_name]]
      # Extract just race type from "N. RaceType - Mon DD" format
      race_type <- sub("^\\d+\\. ", "", race_name)  # Remove leading number
      race_type <- sub(" - .*$", "", race_type)      # Remove date suffix
      race_data$Race <- race_type
      race_data$Gender <- "Ladies"
      ladies_individual_results <- bind_rows(ladies_individual_results, race_data)
    }
  }

  log_info(paste("Combined", nrow(men_individual_results), "men's rows"))
  log_info(paste("Combined", nrow(ladies_individual_results), "ladies' rows"))

  # Only proceed if we have data
  if (nrow(men_individual_results) > 0 || nrow(ladies_individual_results) > 0) {
    # Helper function to select and rename columns consistently
    select_and_rename_cols <- function(df, include_nation = FALSE) {
      if (include_nation) {
        df %>%
          dplyr::select(Skier, ID, Nation, Race, Start, Win, Podium, Top5, `Top-10`, `Top-30`) %>%
          rename(Athlete = Skier)
      } else {
        df %>%
          dplyr::select(Skier, ID, Race, Start, Win, Podium, Top5, `Top-10`, `Top-30`) %>%
          rename(Athlete = Skier)
      }
    }

    # Count unique athletes per nation per gender (using Skier as identifier)
    men_nation_counts <- if(nrow(men_individual_results) > 0) {
      men_individual_results %>%
        filter(Start > 0) %>%
        group_by(Nation) %>%
        summarise(n_athletes = n_distinct(Skier), .groups = "drop")
    } else {
      data.frame(Nation = character(), n_athletes = integer())
    }

    ladies_nation_counts <- if(nrow(ladies_individual_results) > 0) {
      ladies_individual_results %>%
        filter(Start > 0) %>%
        group_by(Nation) %>%
        summarise(n_athletes = n_distinct(Skier), .groups = "drop")
    } else {
      data.frame(Nation = character(), n_athletes = integer())
    }

    # Use 4-athlete threshold for Ski Jumping
    men_main_nations <- men_nation_counts %>% filter(n_athletes >= 4) %>% pull(Nation) %>% sort()
    men_other_nations <- men_nation_counts %>% filter(n_athletes < 4) %>% pull(Nation)

    ladies_main_nations <- ladies_nation_counts %>% filter(n_athletes >= 4) %>% pull(Nation) %>% sort()
    ladies_other_nations <- ladies_nation_counts %>% filter(n_athletes < 4) %>% pull(Nation)

    log_info(paste("Men's main nations (4+ athletes):", paste(men_main_nations, collapse=", ")))
    log_info(paste("Ladies' main nations (4+ athletes):", paste(ladies_main_nations, collapse=", ")))

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
    if (nrow(men_individual_results) > 0) {
      men_other_data <- men_individual_results %>%
        filter(Nation %in% men_other_nations, Start > 0)

      if (nrow(men_other_data) > 0) {
        nations_wb[["Other Men"]] <- select_and_rename_cols(men_other_data, include_nation = TRUE)
        log_info(paste("Added Other Men sheet with", nrow(men_other_data), "rows from",
                       length(unique(men_other_data$Nation)), "nations"))
      }
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
    if (nrow(ladies_individual_results) > 0) {
      ladies_other_data <- ladies_individual_results %>%
        filter(Nation %in% ladies_other_nations, Start > 0)

      if (nrow(ladies_other_data) > 0) {
        nations_wb[["Other Ladies"]] <- select_and_rename_cols(ladies_other_data, include_nation = TRUE)
        log_info(paste("Added Other Ladies sheet with", nrow(ladies_other_data), "rows from",
                       length(unique(ladies_other_data$Nation)), "nations"))
      }
    }

    # Combine all results for summary
    all_individual_results <- bind_rows(men_individual_results, ladies_individual_results)

    # Create Summary sheet with expected medal counts (split by gender)
    if (nrow(all_individual_results) > 0) {
      summary_data <- all_individual_results %>%
        filter(Start > 0) %>%
        group_by(Nation, Gender) %>%
        summarise(
          Athletes = n_distinct(Skier),
          Races = n(),
          `Exp Wins` = round(sum(Win, na.rm = TRUE) / 100, 2),
          `Exp Podiums` = round(sum(Podium, na.rm = TRUE) / 100, 2),
          `Exp Top5` = round(sum(Top5, na.rm = TRUE) / 100, 2),
          .groups = "drop"
        ) %>%
        arrange(Gender, desc(`Exp Wins`))

      nations_wb[["Summary"]] <- summary_data
      log_info(paste("Added Summary sheet with", nrow(summary_data), "nation-gender combinations"))
    }

    # Save nations workbook
    if (length(nations_wb) > 0) {
      nations_file <- file.path(output_dir, "nations_individual.xlsx")
      write.xlsx(nations_wb, nations_file)
      log_info(paste("Saved nations breakdown to", nations_file))
    }
  }
} else {
  log_info("No individual results to process for nations breakdown")
}

log_info("Ski Jumping Championships predictions completed successfully")
