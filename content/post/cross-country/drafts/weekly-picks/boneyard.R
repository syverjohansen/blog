```{r read-in}
library(arrow)
library(dplyr)
library(tidyr)
library(logger)

# Set up logging directory and logging
log_dir <- "~/ski/elo/python/ski/polars/excel365/weekly-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "startlist_processing.log")))

# Read startlists
startlist_men <- read_feather("~/ski/elo/python/ski/polars/excel365/startlist_scraped_men.feather")
startlist_ladies <- read_feather("~/ski/elo/python/ski/polars/excel365/startlist_scraped_ladies.feather")

# Read chronos
chrono_men <- read_feather("~/ski/elo/python/ski/polars/excel365/men_merged.feather")
chrono_ladies <- read_feather("~/ski/elo/python/ski/polars/excel365/ladies_merged.feather")

create_weekend_groups <- function(chronos) {
  chronos %>%
    filter(Event == "World Cup") %>%
    group_by(Season, City) %>%
    summarise(
      Distance = as.integer(any(Distance != "Sprint")),
      Distance_C = as.integer(any(Distance != "Sprint" & Technique == "C")),
      Distance_F = as.integer(any(Distance != "Sprint" & Technique == "F")),
      Sprint_C = as.integer(any(Distance == "Sprint" & Technique == "C")), 
      Sprint_F = as.integer(any(Distance == "Sprint" & Technique == "F")),
      .groups = "drop"
    )
}

calculate_participation_rates <- function(chronos, weekends, skier) {
  last_10_weekends <- weekends %>%
    arrange(desc(Season), desc(City)) %>%
    slice_head(n = 10)
  
  skier_participation <- chronos %>%
    filter(
      Skier == skier,
      Season >= min(last_10_weekends$Season)
    ) %>%
    inner_join(last_10_weekends, by = c("Season", "City"))
  
  # Calculate participation rates by race type
  participation_rates <- skier_participation %>%
    summarise(
      Distance_Rate = mean(Distance),
      Distance_C_Rate = mean(Distance_C),
      Distance_F_Rate = mean(Distance_F),
      Sprint_C_Rate = mean(Sprint_C),
      Sprint_F_Rate = mean(Sprint_F)
    )
  
  return(participation_rates)
}

# Function to get additional skiers for non-config nations
get_additional_nation_skiers <- function(chronos, startlist, nation) {
  log_debug("Getting additional skiers for {nation}")
  
  # Get all skiers from this nation in 2025
  season_skiers <- chronos %>%
    filter(
      Nation == nation,
      Season == 2025
    ) %>%
    distinct(Skier) %>%
    pull(Skier)
  
  # Get current skiers in startlist
  current_skiers <- startlist %>%
    filter(Nation == nation) %>%
    pull(Skier)
  
  # Find missing skiers
  missing_skiers <- setdiff(season_skiers, current_skiers)
  
  log_debug("Found {length(missing_skiers)} additional skiers for {nation}")
  return(missing_skiers)
}

# New function to get non-config nation probabilities
get_nation_race_probabilities <- function(chronos, nation, race_type, technique) {
  # Get all skiers from this nation in 2025
  chronos %>%
    filter(
      Nation == nation,
      Season == 2025,
      Event == "World Cup",
      # Match race type
      if(race_type == "Distance") {
        Distance != "Sprint"
      } else {
        Distance == "Sprint"
      },
      # Match technique if specified
      if(!is.na(technique)) {
        Technique == technique
      } else {
        TRUE
      }
    ) %>%
    group_by(Skier) %>%
    summarise(
      Prob = mean(n() > 0),
      .groups = "drop"
    ) %>%
    summarise(
      Avg_Prob = mean(Prob)
    ) %>%
    pull(Avg_Prob)
}

# Function to get participation probability by race type
# Function to get participation probability by race type
# Keep get_race_probability function the same
get_race_probability <- function(chronos, skier, race_type, technique) {
  log_debug("Calculating probability for skier: {skier}")
  
  # Get skier's first ever race date
  skier_first_race <- chronos %>%
    filter(Skier == skier) %>%
    arrange(Date) %>%
    slice(1) %>%
    pull(Date)
  
  # Use 2020-01-01 or skier's first race, whichever is later
  start_date <- max("2020-01-01", skier_first_race)
  
  # First get all matching races since start_date
  all_races <- chronos %>%
    filter(
      Event == "World Cup",
      Date >= start_date,
      if(race_type == "Distance") {
        Distance != "Sprint"
      } else {
        Distance == "Sprint"
      },
      if(!is.na(technique)) {
        Technique == technique
      } else {
        TRUE
      }
    ) %>%
    distinct(Date, City)
  
  # Then get this skier's participations
  skier_races <- chronos %>%
    filter(
      Event == "World Cup",
      Date >= start_date,
      Skier == skier,
      if(race_type == "Distance") {
        Distance != "Sprint"
      } else {
        Distance == "Sprint"
      },
      if(!is.na(technique)) {
        Technique == technique
      } else {
        TRUE
      }
    ) %>%
    distinct(Date, City)
  
  total_races <- nrow(all_races)
  
  if(total_races == 0) {
    log_debug("No races found for type: {race_type}, technique: {technique}")
    return(0)
  }
  
  races_participated <- nrow(skier_races)
  # Cap probability at 1
  prob <- min(1, races_participated / total_races)
  
  log_debug("Probability for {skier} since {start_date}: {prob} ({races_participated}/{total_races} races)")
  
  return(prob)
}

# Modify normalize_to_quota to handle NAs and add logging
normalize_to_quota <- function(startlist) {
  log_info("Starting quota normalization")
  
  startlist %>%
    group_by(Nation) %>%
    mutate(
      across(
        starts_with("Race"),
        ~{
          # Ensure numeric and replace NAs with 0
          original_probs <- as.numeric(.)  # Store original probabilities for weighting
          vec <- original_probs
          vec[is.na(vec)] <- 0
          
          # Get quota and ensure it's numeric
          quota <- as.numeric(first(Quota))
          if(is.na(quota)) {
            quota <- 2
          }
          
          # Step 1: Keep track of fixed ones (exactly 1)
          fixed_ones <- vec == 1
          n_fixed <- sum(fixed_ones)
          result <- vec
          
          # If we already exceed quota just from fixed ones, we can't add more
          if(n_fixed >= quota) {
            return(ifelse(fixed_ones, 1, 0))
          }
          
          # Step 2: For remaining values, normalize based on original probabilities
          remaining_quota <- quota - n_fixed
          adjustable <- !fixed_ones & vec > 0
          
          if(any(adjustable)) {
            # Get sum of adjustable probabilities
            adj_sum <- sum(vec[adjustable])
            
            if(adj_sum > 0) {
              # Scale adjustable values while maintaining relative proportions
              scaling_factor <- remaining_quota / adj_sum
              result[adjustable] <- vec[adjustable] * scaling_factor
              
              # Cap at 1
              result <- pmin(result, 1)
            }
          }
          
          log_debug(sprintf("Final sum for %s: %.2f (Quota: %d)", first(Nation), sum(result), quota))
          result
        }
      )
    ) %>%
    ungroup()
}

# Modify update_race_probabilities to handle skiers one at a time
# Modify update_race_probabilities to add missing skiers first
update_race_probabilities <- function(startlist, chronos, race_types) {
  log_info("Starting race probability updates")
  
  # First, identify non-config nations
  non_config_nations <- startlist %>%
    filter(!Config_Nation) %>%
    distinct(Nation) %>%
    pull(Nation)
  
  # Create dataframe of additional skiers
  additional_skiers <- tibble()
  
  for(nation in non_config_nations) {
    # Get all 2025 skiers for this nation
    season_skiers <- chronos %>%
      filter(Nation == nation, Season == 2025) %>%
      distinct(Skier)
    
    # Get current skiers in startlist
    current_skiers <- startlist %>%
      filter(Nation == nation) %>%
      pull(Skier)
    
    # Find missing skiers
    missing_skiers <- season_skiers %>%
      filter(!Skier %in% current_skiers)
    
    if(nrow(missing_skiers) > 0) {
      new_rows <- missing_skiers %>%
        mutate(
          Nation = nation,
          Config_Nation = FALSE,
          In_Config = FALSE,
          Race1_Prob = 0,  # Not in FIS list
          Base_Quota = startlist %>% filter(Nation == nation) %>% pull(Base_Quota) %>% first(),
          Quota = startlist %>% filter(Nation == nation) %>% pull(Quota) %>% first(),
          Is_Host_Nation = startlist %>% filter(Nation == nation) %>% pull(Is_Host_Nation) %>% first()
        )
      
      additional_skiers <- bind_rows(additional_skiers, new_rows)
    }
  }
  
  # Add additional skiers to startlist
  startlist <- bind_rows(startlist, additional_skiers)
  
  # Now process each race after Race1
  for(i in 2:nrow(race_types)) {
    race_col <- paste0("Race", i, "_Prob")
    race_type <- race_types$type[i]
    race_tech <- race_types$technique[i]
    
    log_info("Processing {race_col} ({race_type} {race_tech})")
    
    startlist <- startlist %>%
      rowwise() %>%
      mutate(
        !!race_col := case_when(
          !is.na(get(race_col)) ~ get(race_col),
          !Config_Nation ~ {
            prob <- get_race_probability(chronos, Skier, race_type, race_tech)
            if(is.na(prob)) 0 else prob
          },
          TRUE ~ {
            prob <- get_race_probability(chronos, Skier, race_type, race_tech)
            if(is.na(prob)) 0 else prob
          }
        )
      ) %>%
      ungroup()
  }
  
  return(startlist)
}

# Main function to process startlist
# Main function to process startlist
process_startlist <- function(startlist, chronos, race_types) {
  log_info("Starting startlist processing")
  
  print("Initial state for Great Britain:")
  print(startlist %>% filter(Nation == "Great Britain") %>% 
          dplyr::select(Skier, In_FIS_List, Race1_Prob, Race2_Prob))
  
  # First ensure all numeric columns are actually numeric
  startlist <- startlist %>%
    mutate(
      Race1_Prob = as.numeric(Race1_Prob),
      Race2_Prob = as.numeric(Race2_Prob),
      In_FIS_List = as.logical(In_FIS_List)
    )
  
  # Set initial probabilities
  startlist <- startlist %>%
    mutate(
      Race1_Prob = ifelse(In_FIS_List, 1, 0),
      Race2_Prob = 0  # Initialize all Race2_Prob to 0
    )
  
  print("After initial probability setup:")
  print(startlist %>% filter(Nation == "Great Britain") %>% 
          dplyr::select(Skier, In_FIS_List, Race1_Prob, Race2_Prob))
  
  # Update probabilities for Race2
  race_type <- race_types$type[2]  # Get second race type
  race_tech <- race_types$technique[2]  # Get second race technique
  
  log_info("Processing Race2 ({race_type} {race_tech})")
  
  startlist <- startlist %>%
    rowwise() %>%
    mutate(
      Race2_Prob = {
        prob <- get_race_probability(chronos, Skier, race_type, race_tech)
        if(is.na(prob)) 0 else min(1, prob)  # Ensure we cap at 1 and handle NAs
      }
    ) %>%
    ungroup()
  
  print("After probability calculations:")
  print(startlist %>% filter(Nation == "Great Britain") %>% 
          dplyr::select(Skier, In_FIS_List, Race1_Prob, Race2_Prob))
  
  # Normalize to quotas
  result <- startlist %>%
    normalize_to_quota()
  
  print("After quota normalization:")
  print(result %>% filter(Nation == "Great Britain") %>% 
          dplyr::select(Skier, In_FIS_List, Race1_Prob, Race2_Prob))
  
  log_info("Completed startlist processing")
  return(result)
}
# Example usage
race_types <- tibble(
  type = c("Sprint", "Distance"),
  technique = c("F", "C")
)
# Process men's data
log_info("Starting men's data processing")
startlist_men_processed <- process_startlist(startlist_men, chrono_men, race_types)

startlist_men_processed %>% filter(Nation=="Norway") %>% dplyr::select(Skier, ID, Price, Nation, Race1_Prob, Race2_Prob)
chrono_men %>% filter(Nation=="Austria", Season==2025) %>% dplyr::select((Skier))

# Process ladies' data
log_info("Starting ladies' data processing")
startlist_ladies_processed <- process_startlist(startlist_ladies, chrono_ladies, race_types)

# Save processed data
log_info("Saving processed data")
write_feather(startlist_men_processed, 
              "~/ski/elo/python/ski/polars/excel365/startlist_with_probs_men.feather")
write_feather(startlist_ladies_processed, 
              "~/ski/elo/python/ski/polars/excel365/startlist_with_probs_ladies.feather")
log_info("Processing complete")

startlist_men_processed
```










```{r part1}
library(dplyr)
library(purrr)
library(tidyr)
library(arrow)
library(mgcv)
library(leaps)
library(openxlsx)


# Define points systems
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
stage_points <- c(50,47,44,41,38,35,32,30,28,26,24,22,20,18,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
tds_points <- c(300,285,270,255,240,216,207,198,189,180,174,168,162,156,150,144,138,132,126,120,114,108,102,96,90,84,78,72,66,60,57,54,51,48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3)

# Define race schedule
races <- data.frame(
  distance = c("Sprint", "Distance"),
  technique = c("F", "C"),
  ms = c(0, 0),
  altitude = c(1540, 1540),
  #grade = c(2.53, 2.21),
  period = c(1, 1)
)

# Calculate total points including all scenarios - modify this part
combine_predictions <- function(race_dfs, startlist) {
  # Start with first race
  final_predictions <- race_dfs[[1]] %>%
    rename(
      Race1_Base = Base_Prediction,
      Race1_Altitude = altitude_adjustment,
      Race1_Grade = grade_adjustment,
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
      startlist %>% dplyr::select(Skier, Price),
      by = "Skier"
    )
  
  # Add remaining races dynamically
  if(length(race_dfs) > 1) {
    for(i in 2:length(race_dfs)) {
      final_predictions <- final_predictions %>%
        left_join(
          race_dfs[[i]] %>%
            rename(
              !!paste0("Race", i, "_Base") := Base_Prediction,
              !!paste0("Race", i, "_Altitude") := altitude_adjustment,
              !!paste0("Race", i, "_Grade") := grade_adjustment,
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
                          !!paste0("Race", i, "_Grade"),
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
  
  # Create expressions for summing columns dynamically
  sum_expr <- function(prefix, n_races) {
    syms <- paste0("Race", 1:n_races, "_", prefix)
    parse(text = paste(syms, collapse = " + "))
  }
  
  avg_expr <- function(prefix, n_races) {
    syms <- paste0("Race", 1:n_races, "_", prefix)
    parse(text = paste0("(", paste(syms, collapse = " + "), ")/", n_races))
  }
  
  # Calculate totals dynamically based on number of races
  final_predictions <- final_predictions %>%
    mutate(
      Total_Points = eval(sum_expr("Points", length(race_dfs))),
      Total_Altitude = eval(sum_expr("Altitude", length(race_dfs))),
      Total_Grade = eval(sum_expr("Grade", length(race_dfs))),
      Total_Period = eval(sum_expr("Period", length(race_dfs))),
      Total_MS = eval(sum_expr("MS", length(race_dfs))),
      Total_Safe = eval(sum_expr("Safe", length(race_dfs))),
      Total_Upside = eval(sum_expr("Upside", length(race_dfs))),
      Avg_Volatility = eval(avg_expr("Volatility", length(race_dfs))),
      Avg_Confidence = eval(avg_expr("Confidence", length(race_dfs)))
    ) %>%
    arrange(desc(Total_Points))
  
  # Select columns dynamically based on number of races
  select_cols <- c("Skier", "Nation", "Price")
  for(i in 1:length(race_dfs)) {
    select_cols <- c(select_cols,
                     paste0("Race", i, "_Base"),
                     paste0("Race", i, "_Altitude"),
                     paste0("Race", i, "_Grade"),
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
                   "Total_Altitude", "Total_Grade", "Total_Period", "Total_MS",
                   "Avg_Volatility", "Avg_Confidence")
  
  final_predictions %>%
    dplyr::select(all_of(select_cols))
}

# Modify the post-processing for final output
create_post_predictions <- function(predictions, n_races) {
  # Create base columns
  base_cols <- c("Skier", "Nation")
  
  # Add race point columns dynamically
  race_cols <- paste0("Race", 1:n_races, "_Points")
  
  # Add total columns
  total_cols <- c("Total_Points", "Total_Safe")
  
  # Combine all columns
  all_cols <- c(base_cols, race_cols, total_cols)
  
  predictions[, all_cols]
}


prepare_startlist_data <- function(startlist, race_df, pelo_col) {
  # Dynamically get race probability columns
  race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
  
  # Keep only essential columns from startlist
  base_df <- startlist %>%
    dplyr::select(Skier, ID, Nation, Price, all_of(race_prob_cols))
  
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
    arrange(Date, Race) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    dplyr::select(Skier, all_of(elo_cols))
  
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
    arrange(Date, Race) %>%
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
  
  # Calculate max values for normalization
  max_values <- race_df %>%
    summarise(across(all_of(elo_cols), max, na.rm = TRUE))
  
  # Calculate both Elo and Pelo percentages (they'll be the same values)
  for(i in seq_along(elo_cols)) {
    elo_col <- elo_cols[i]
    pelo_col <- pelo_cols[i]
    
    # Calculate the percentage (same for both)
    pct_value <- result_df[[elo_col]] / max_values[[elo_col]]
    
    # Assign to both Elo and Pelo percentage columns
    result_df[[paste0(elo_col, "_Pct")]] <- pct_value
    result_df[[paste0(pelo_col, "_Pct")]] <- pct_value
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
  
  return(result_df)
}

# Helper functions
get_points <- function(place, points_list) {
  if (place >= 1 && place <= length(points_list)) {
    return(points_list[place])
  }
  return(0)
}


normalize_predictions <- function(predictions, points_vector) {
  # First ensure predictions are between 0 and 100
  predictions <- pmax(pmin(predictions, 100), 0)
  
  # Calculate target sum based on number of predictions
  target_sum <- if(length(predictions) > length(points_vector)) {
    sum(points_vector)
  } else {
    sum(points_vector[1:length(predictions)])
  }
  
  # Calculate current sum
  current_sum <- sum(predictions)
  
  # Apply normalization only if current_sum is not 0
  if(current_sum > 0) {
    normalized <- predictions * (target_sum / current_sum)
    # Ensure again that no prediction exceeds 100 after normalization
    normalized <- pmax(pmin(normalized, 100), 0)
  } else {
    normalized <- predictions
  }
  
  return(normalized)
  
}


replace_na_with_quartile <- function(x) {
  quartile_1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), quartile_1, x)
}

preprocess_data <- function(df) {
  # First calculate points using ALL historical data
  df_with_points <- df %>%
    # Add points
    mutate(Points = map_int(Place, ~ get_points(.x, wc_points))) %>%
    # Sort
    arrange(Date, Race, Place)
  
  # Calculate weighted previous points separately for each race type/technique combination
  df_with_points <- df_with_points %>%
    # Group by ID and race type
    group_by(ID, Distance, Technique) %>%
    arrange(Date, Race) %>%
    mutate(Prev_Points_Weighted = sapply(1:n(), function(j) {
      if (j == 1) return(0)
      start_index <- max(1, j - 5)
      num_races <- j - start_index
      weights <- seq(1, num_races)
      weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
    })) %>%
    ungroup()
  
  # Now apply other preprocessing steps and filter for recent data
  df_with_points %>%
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
      Season > 2019,
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
        c(Distance_Pelo, Distance_C_Pelo, Distance_F_Pelo,
          Pelo, Sprint_Pelo, Sprint_C_Pelo, Sprint_F_Pelo,
          Freestyle_Pelo, Classic_Pelo),
        replace_na_with_quartile
      )
    ) %>%
    mutate(
      across(
        c(Distance_Pelo, Distance_C_Pelo, Distance_F_Pelo,
          Pelo, Sprint_Pelo, Sprint_C_Pelo, Sprint_F_Pelo,
          Freestyle_Pelo, Classic_Pelo),
        list(Pct = ~./max(.)), 
        .names = "{.col}_Pct"
      )
    ) %>%
    ungroup() %>%
    # Filter out team sprint and relay
    filter(!Distance %in% c("Ts", "Rel"))
}

predict_races <- function(gender) {
  # Load chronological data
  chrono_path <- ifelse(gender == "men", 
                        "~/ski/elo/python/ski/polars/excel365/men_merged.feather",
                        "~/ski/elo/python/ski/polars/excel365/ladies_merged.feather")
  
  startlist_path <- sprintf("~/ski/elo/python/ski/polars/excel365/startlist_with_probs_%s.feather", gender)
  
  df <- arrow::read_feather(chrono_path) %>%
    preprocess_data()
  
  print(df %>%
          arrange(-Season, -Race))
  startlist <- arrow::read_feather(startlist_path)
  
  # Initialize results list
  race_predictions <- list()
  race_dfs <- list()
  
  # Process each race
  for(i in 1:nrow(races)) {
    print(sprintf("Processing %s race %d: %s %s", gender, i, races$distance[i], races$technique[i]))
    
    # Filter base dataset for race type
    race_df <- df %>%
      {if(races$distance[i] == "Distance") {
        if(races$technique[i] == "") {
          filter(., Distance != "Sprint")  # Don't filter by technique if it's empty
        } else {
          filter(., Distance != "Sprint", Technique == races$technique[i])
        }
      } else {
        if(races$technique[i] == "") {
          filter(., Distance == races$distance[i])  # Don't filter by technique if it's empty
        } else {
          filter(., Distance == races$distance[i], Technique == races$technique[i])
        }
      }}
    print(race_df)
    # Add altitude and grade categories for historical data
    race_df <- race_df %>%
      mutate(
        AltitudeCategory = ifelse(Altitude >= 1300, 1, 0),
        GradeCategory = ifelse(`Average Grade` >= quantile(`Average Grade`, 0.75, na.rm=TRUE), 1, 0)
      )        
    
    # Get relevant Pelo column
    pelo_col <- case_when(
      races$distance[i] == "Sprint" & races$technique[i] == "C" ~ "Sprint_C_Pelo_Pct",
      races$distance[i] == "Sprint" & races$technique[i] == "F" ~ "Sprint_F_Pelo_Pct",
      races$distance[i] == "Distance" & races$technique[i] == "C" ~ "Distance_C_Pelo_Pct",
      races$distance[i] == "Distance" & races$technique[i] == "F" ~ "Distance_F_Pelo_Pct",
      races$distance[i] == "Distance" & races$technique[i] == "" ~ "Distance_Pelo_Pct",
      races$distance[i] == "Sprint" & races$technique[i] == "" ~ "Sprint_Pelo_Pct",
      TRUE ~ "Pelo_Pct"
    )
    # Filter for top performers and add previous points
    race_df_75 <- race_df %>%
      filter(get(pelo_col) > 0.75) %>%
      group_by(ID) %>%
      arrange(Season, Race) %>%
      ungroup()
    
    # Feature selection and model fitting
    response_variable <- "Points"
    explanatory_vars <- c("Prev_Points_Weighted", "Distance_Pelo_Pct", "Sprint_Pelo_Pct", 
                          "Sprint_C_Pelo_Pct", "Distance_F_Pelo_Pct", "Distance_C_Pelo_Pct", 
                          "Classic_Pelo_Pct", "Freestyle_Pelo_Pct", "Sprint_F_Pelo_Pct", "Pelo_Pct")
    
    # Create and fit model
    formula <- as.formula(paste(response_variable, "~", paste(explanatory_vars, collapse = " + ")))
    exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
    summary_exhaustive <- summary(exhaustive_selection)
    best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
    smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
    gam_formula <- as.formula(paste("Points ~", smooth_terms))
    
    
    model <- gam(gam_formula, data = race_df_75)
    # Calculate adjustments for historical data
    # Calculate predictions and adjustments
    # Calculate predictions and adjustments step by step
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
          prior_alt_1 <- Prediction_Diff[AltitudeCategory == 1 & row_id < r]
          prior_alt_0 <- Prediction_Diff[AltitudeCategory == 0 & row_id < r]
          if(length(prior_alt_1) < 3 || length(prior_alt_0) < 3) return(1)
          tryCatch({
            t.test(prior_alt_1, prior_alt_0)$p.value
          }, error = function(e) 1)
        }),
        altitude_correction = ifelse(altitude_p < 0.05 & AltitudeCategory == 1,
                                     mean(Prediction_Diff[AltitudeCategory == 1], na.rm = TRUE),
                                     0)
      ) %>%
      # Step 3: Calculate grade p-values and effects
      mutate(
        grade_p = purrr::map_dbl(row_id, function(r) {
          if(r <= 1) return(1)
          prior_grade_1 <- Prediction_Diff[GradeCategory == 1 & row_id < r]
          prior_grade_0 <- Prediction_Diff[GradeCategory == 0 & row_id < r]
          if(length(prior_grade_1) < 3 || length(prior_grade_0) < 3) return(1)
          tryCatch({
            t.test(prior_grade_1, prior_grade_0)$p.value
          }, error = function(e) 1)
        }),
        grade_correction = ifelse(grade_p < 0.05 & GradeCategory == 1,
                                  mean(Prediction_Diff[GradeCategory == 1], na.rm = TRUE),
                                  0)
      ) %>%
      # Step 4: Calculate course-adjusted predictions
      mutate(
        Course_Adjusted = Initial_Prediction + altitude_correction + grade_correction,
        Course_Diff = Points - Course_Adjusted
      ) %>%
      # Step 5: Calculate Period adjustments
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
      # Step 6: Calculate Mass Start adjustments
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
                               0)
      ) %>%
      ungroup()
    
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
        grade_effect = last(grade_correction),
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
    # Prepare startlist predictions
    # Update race_dfs to include mass start adjustments
    race_dfs[[i]] <- startlist %>%
      prepare_startlist_data(race_df, pelo_col) %>%
      mutate(
        Base_Prediction = predict(model, newdata = .),
      ) %>%
      left_join(skier_adjustments, by = "Skier") %>%
      mutate(
        # Regular adjustments
        altitude_effect = replace_na(altitude_effect, 0),
        grade_effect = replace_na(grade_effect, 0),
        period_effect = replace_na(period_effect, 0),
        ms_effect = replace_na(ms_effect, 0),
        # Volatility metrics
        prediction_volatility = replace_na(prediction_volatility, 0),
        consistency_score = replace_na(consistency_score, 0),
        upside_potential = replace_na(upside_potential, 0),
        downside_risk = replace_na(downside_risk, 0),
        volatility_ratio = replace_na(volatility_ratio, 1),
        n_recent_races = replace_na(n_recent_races, 0),
        
        # Regular adjustments
        altitude_adjustment = if(races$altitude[i] >= 1300) altitude_effect else 0,
        grade_adjustment = if(races$grade[i] >= quantile(race_df$`Average Grade`, 0.75, na.rm=TRUE)) 
          grade_effect else 0,
        period_adjustment = period_effect,
        ms_adjustment = if(races$ms[i] == 1) ms_effect else 0,
        
        # Base prediction and adjustments
        Predicted_Points = Base_Prediction + altitude_adjustment + grade_adjustment + 
          period_adjustment + ms_adjustment,
        Predicted_Points = pmax(pmin(Predicted_Points, 100), 0),
        Initial_Final = Predicted_Points * get(sprintf("Race%d_Prob", i)),
        Final_Prediction = Initial_Final,
        
        # Different scoring scenarios
        confidence_factor = pmin(n_recent_races / 10, 1),
        scaled_upside_potential = upside_potential*(Initial_Final/100),
        scaled_downside_potential = downside_risk*(Initial_Final/100),
        Safe_Prediction = pmax(Initial_Final - (prediction_volatility * 1.5 * confidence_factor)* get(sprintf("Race%d_Prob", i)), 0),
        Safe_Prediction = pmax(Initial_Final-(abs(scaled_downside_potential)*volatility_ratio*confidence_factor)* get(sprintf("Race%d_Prob", i)), 0),
        Upside_Prediction = pmin(Initial_Final + (prediction_volatility * 1.5 * confidence_factor)* get(sprintf("Race%d_Prob", i)), 100),
        Upside_Prediction = pmin(Initial_Final+(scaled_upside_potential*volatility_ratio*confidence_factor)* get(sprintf("Race%d_Prob", i)), 100)
      ) %>%
      mutate(
        #Upside_Prediction = pmin(Final_Prediction + (upside_potential * volatility_ratio * confidence_factor)* get(sprintf("Race%d_Prob", i)), 100),
        #Final_Prediction = normalize_predictions(Final_Prediction, wc_points)
        Final_Prediction = Initial_Final
      ) %>%
      dplyr::select(Skier, Nation, 
                    Base_Prediction, altitude_adjustment, grade_adjustment, 
                    period_adjustment, ms_adjustment,
                    prediction_volatility, volatility_ratio, consistency_score, confidence_factor,
                    Final_Prediction, Safe_Prediction, Upside_Prediction,
                    sprintf("Race%d_Prob", i))
  }
  
  # Get number of races from races dataframe
  n_races <- nrow(races)
  
  final_predictions <- combine_predictions(race_dfs, startlist)
  
  print("\nFinal Predictions (Top 10):")
  print(head(final_predictions, 10))
  
  # Create post predictions using new function
  post_predictions <- create_post_predictions(final_predictions, n_races)
  
  # Save to Excel
  file_path <- sprintf("/Users/syverjohansen/blog/daehl-e/content/post/drafts/weekly-picks/2025Davos/%s-points.xlsx", 
                       ifelse(gender == "men", "men", "ladies"))
  write.xlsx(post_predictions, file = file_path)
  
  # Return both predictions (removed duplicate return statement)
  return(list(
    full_predictions = final_predictions,
    post_predictions = post_predictions
  ))
}

# Run predictions
men_results <- predict_races("men")

ladies_results <- predict_races("ladies")
```


```{r knapsack}
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
prepare_tds_data <- function(men_startlist, ladies_startlist, prediction_type = "normal") {
  # Add gender to predictions
  men_df <- men_startlist %>%
    mutate(sex = "m")
  ladies_df <- ladies_startlist %>%
    mutate(sex = "f")
  
  # Choose which prediction column to use
  points_col <- case_when(
    prediction_type == "safe" ~ "Safe_Prediction",
    prediction_type == "upside" ~ "Upside_Prediction",
    TRUE ~ "Predicted_Points"
  )
  
  # Combine predictions and ensure numeric price
  combined_df <- bind_rows(men_df, ladies_df) %>%
    mutate(
      row_id = row_number(),
      Price = as.numeric(Price),
      Points = get(points_col)  # Use selected prediction column
    )
  return(combined_df)
}

optimize_tds_team <- function(fantasy_df) {
  n <- nrow(fantasy_df)
  
  # Create index sets for men and women
  men_indices <- which(fantasy_df$sex == "m")
  women_indices <- which(fantasy_df$sex == "f")
  
  # Create optimization model
  model <- MIPModel() %>%
    # Binary decision variables for each skier
    add_variable(x[i], i = 1:n, type = "binary") %>%
    
    # Objective: maximize predicted points
    set_objective(sum_expr(fantasy_df$Points[i] * x[i], i = 1:n), "max") %>%
    
    # Budget constraint
    add_constraint(sum_expr(fantasy_df$Price[i] * x[i], i = 1:n) <= 100000) %>%
    
    # Team size constraint
    add_constraint(sum_expr(x[i], i = 1:n) == 16) %>%
    
    # Gender constraints
    add_constraint(sum_expr(x[i], i = men_indices) <= 8) %>%
    add_constraint(sum_expr(x[i], i = women_indices) <= 8)
  
  # Solve the model
  result <- solve_model(model, with_ROI(solver = "glpk"))
  
  # Extract results
  selected <- get_solution(result, x[i]) %>%
    filter(value > 0) %>%
    pull(i)
  
  # Create results dataframe
  selected_team <- fantasy_df[selected, ] %>%
    arrange(sex, desc(Price))
  
  # Print results summary
  cat("\nOptimized Tour de Ski Fantasy Team:\n")
  cat(sprintf("Total Predicted Points: %.2f\n", sum(selected_team$Points)))
  cat("Total Cost: $", sum(selected_team$Price), "\n\n")
  
  cat("Team Composition:\n")
  cat("Men:", sum(selected_team$sex == "m"), "\n")
  cat("Women:", sum(selected_team$sex == "f"), "\n\n")
  
  cat("Selected Team:\n")
  print(selected_team %>%
          dplyr::select(Skier, sex, Nation, Price, Points) %>%
          arrange(sex, desc(Points)))
  
  return(selected_team)
}

# Run all three optimizations
combined_df_normal <- prepare_tds_data(men_fc_predictions, ladies_fc_predictions, "normal")
combined_df_safe <- prepare_tds_data(men_fc_predictions, ladies_fc_predictions, "safe")
combined_df_upside <- prepare_tds_data(men_fc_predictions, ladies_fc_predictions, "upside")

tds_team_normal <- optimize_tds_team(combined_df_normal)
tds_team_safe <- optimize_tds_team(combined_df_safe)
tds_team_upside <- optimize_tds_team(combined_df_upside)

# Save all teams to Excel
write.xlsx(list(
  "Normal Team" = tds_team_normal %>% 
    dplyr::select(Skier, sex, Nation, Price, Points) %>%
    rename(`Predicted Points` = Points),
  "Safe Team" = tds_team_safe %>% 
    dplyr::select(Skier, sex, Nation, Price, Points) %>%
    rename(`Safe Points` = Points),
  "Upside Team" = tds_team_upside %>% 
    dplyr::select(Skier, sex, Nation, Price, Points) %>%
    rename(`Upside Points` = Points)
),"/Users/syverjohansen/blog/daehl-e/content/post/drafts/weekly-picks/2025TdS/fantasy-teams.xlsx")

print(tds_team_normal)
print(tds_team_safe)
print(tds_team_upside)


# Save to Excel with renamed column
tds_team_output <- tds_team_upside %>% 
  dplyr::select(Skier, sex, Nation, Price, Points) %>%
  rename(`Predicted Points` = Points) %>%
  arrange(sex, desc(`Predicted Points`))

print(tds_team_output)

write.xlsx(tds_team_output, 
           "/Users/syverjohansen/blog/daehl-e/content/post/drafts/weekly-picks/2025TdS/fantasy-team.xlsx")
```

```{r brier-odds}
```{r knapsack}
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
prepare_tds_data <- function(men_startlist, ladies_startlist, prediction_type = "normal") {
  # Add gender to predictions
  men_df <- men_startlist %>%
    mutate(sex = "m")
  ladies_df <- ladies_startlist %>%
    mutate(sex = "f")
  
  # Choose which prediction column to use
  points_col <- case_when(
    prediction_type == "safe" ~ "Safe_Prediction",
    prediction_type == "upside" ~ "Upside_Prediction",
    TRUE ~ "Predicted_Points"
  )
  
  # Combine predictions and ensure numeric price
  combined_df <- bind_rows(men_df, ladies_df) %>%
    mutate(
      row_id = row_number(),
      Price = as.numeric(Price),
      Points = get(points_col)  # Use selected prediction column
    )
  return(combined_df)
}

optimize_tds_team <- function(fantasy_df) {
  n <- nrow(fantasy_df)
  
  # Create index sets for men and women
  men_indices <- which(fantasy_df$sex == "m")
  women_indices <- which(fantasy_df$sex == "f")
  
  # Create optimization model
  model <- MIPModel() %>%
    # Binary decision variables for each skier
    add_variable(x[i], i = 1:n, type = "binary") %>%
    
    # Objective: maximize predicted points
    set_objective(sum_expr(fantasy_df$Points[i] * x[i], i = 1:n), "max") %>%
    
    # Budget constraint
    add_constraint(sum_expr(fantasy_df$Price[i] * x[i], i = 1:n) <= 100000) %>%
    
    # Team size constraint
    add_constraint(sum_expr(x[i], i = 1:n) == 16) %>%
    
    # Gender constraints
    add_constraint(sum_expr(x[i], i = men_indices) <= 8) %>%
    add_constraint(sum_expr(x[i], i = women_indices) <= 8)
  
  # Solve the model
  result <- solve_model(model, with_ROI(solver = "glpk"))
  
  # Extract results
  selected <- get_solution(result, x[i]) %>%
    filter(value > 0) %>%
    pull(i)
  
  # Create results dataframe
  selected_team <- fantasy_df[selected, ] %>%
    arrange(sex, desc(Price))
  
  # Print results summary
  cat("\nOptimized Tour de Ski Fantasy Team:\n")
  cat(sprintf("Total Predicted Points: %.2f\n", sum(selected_team$Points)))
  cat("Total Cost: $", sum(selected_team$Price), "\n\n")
  
  cat("Team Composition:\n")
  cat("Men:", sum(selected_team$sex == "m"), "\n")
  cat("Women:", sum(selected_team$sex == "f"), "\n\n")
  
  cat("Selected Team:\n")
  print(selected_team %>%
          dplyr::select(Skier, sex, Nation, Price, Points) %>%
          arrange(sex, desc(Points)))
  
  return(selected_team)
}

# Run all three optimizations
combined_df_normal <- prepare_tds_data(men_fc_predictions, ladies_fc_predictions, "normal")
combined_df_safe <- prepare_tds_data(men_fc_predictions, ladies_fc_predictions, "safe")
combined_df_upside <- prepare_tds_data(men_fc_predictions, ladies_fc_predictions, "upside")

tds_team_normal <- optimize_tds_team(combined_df_normal)
tds_team_safe <- optimize_tds_team(combined_df_safe)
tds_team_upside <- optimize_tds_team(combined_df_upside)

# Save all teams to Excel
write.xlsx(list(
  "Normal Team" = tds_team_normal %>% 
    dplyr::select(Skier, sex, Nation, Price, Points) %>%
    rename(`Predicted Points` = Points),
  "Safe Team" = tds_team_safe %>% 
    dplyr::select(Skier, sex, Nation, Price, Points) %>%
    rename(`Safe Points` = Points),
  "Upside Team" = tds_team_upside %>% 
    dplyr::select(Skier, sex, Nation, Price, Points) %>%
    rename(`Upside Points` = Points)
),"/Users/syverjohansen/blog/daehl-e/content/post/drafts/weekly-picks/2025TdS/fantasy-teams.xlsx")

print(tds_team_normal)
print(tds_team_safe)
print(tds_team_upside)


# Save to Excel with renamed column
tds_team_output <- tds_team_upside %>% 
  dplyr::select(Skier, sex, Nation, Price, Points) %>%
  rename(`Predicted Points` = Points) %>%
  arrange(sex, desc(`Predicted Points`))

print(tds_team_output)

write.xlsx(tds_team_output, 
           "/Users/syverjohansen/blog/daehl-e/content/post/drafts/weekly-picks/2025TdS/fantasy-team.xlsx")
```




```{r brier-odds}
men_tds_history
men_fc_history

create_position_model <- function(data, threshold, selected_vars) {
  # Create binary outcome
  data$position_achieved <- data$Place <= threshold
  
  # Create formula for GAM
  formula_str <- paste("position_achieved ~",
                       paste(paste0("s(", selected_vars, ")"),
                             collapse = " + "))
  
  # Fit GAM model
  model <- gam(as.formula(formula_str),
               data = data,
               family = binomial,
               method = "REML")
  
  return(model)
}

# Function to calculate Brier score
calculate_brier <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

select_features <- function(data, threshold, max_vars = 8) {  # Reduced from 8 to 5
  # Create binary outcome
  data$position_achieved <- data$Place <= threshold
  
  # Get predictor columns (excluding non-predictors)
  pred_cols <- names(data)[!names(data) %in% c("ID", "Season", "Race", "Date", 
                                               "Points", "Place", "position_achieved",
                                               "TdS_Last_5_2", "FC_Last_5_2", "Skier", "TdS_Historical")]
  
  print(paste("Selecting features for threshold", threshold, 
              "with", length(pred_cols), "potential predictors"))
  
  # Create formula for regsubsets
  formula_str <- paste("position_achieved ~", paste(pred_cols, collapse = " + "))
  
  # Use forward selection instead of exhaustive
  subset_search <- regsubsets(as.formula(formula_str),
                              data = data,
                              nvmax = max_vars,
                              method = "forward",  # Changed from exhaustive to forward
                              really.big = TRUE)
  
  return(subset_search)
}

# Add Place to Final Climb history
men_fc_history <- men_fc_history2 %>%
  left_join(men_df %>% 
              dplyr::select(ID, Season, Race, Skier, Place),
            by = c("ID", "Season", "Race"))

ladies_fc_history <- ladies_fc_history2 %>%
  left_join(ladies_df %>% 
              dplyr::select(ID, Season, Race, Skier, Place),
            by = c("ID", "Season", "Race"))

# Add Place to TdS history
men_tds_history <- men_tds_history2 %>%
  left_join(men_df %>% 
              dplyr::select(ID, Season, Race, Skier, Place),
            by = c("ID", "Season", "Race"))

ladies_tds_history <- ladies_tds_history2 %>%
  left_join(ladies_df %>% 
              dplyr::select(ID, Season, Race, Skier, Place),
            by = c("ID", "Season", "Race"))

# Now let's create our position thresholds
position_thresholds <- c(1, 3, 10, 30)  # Win, Podium, Top 10, Top 30

# Function for feature selection and model creation
create_position_models <- function(history_data, thresholds = position_thresholds, max_vars = 8) {
  models <- list()
  selected_features <- list()
  
  for(threshold in thresholds) {
    # Feature selection
    features <- select_features(history_data, threshold, max_vars)
    print(features)
    best_model_idx <- which.min(summary(features)$bic)
    selected_vars <- names(coef(features, best_model_idx))[-1]  # Remove intercept
    
    # Create and store model
    model <- create_position_model(history_data, threshold, selected_vars)
    
    models[[paste0("place_", threshold)]] <- model
    selected_features[[paste0("place_", threshold)]] <- selected_vars
  }
  
  return(list(models = models, selected_features = selected_features))
}

# Create models for men's Final Climb
men_fc_models <- create_position_models(men_fc_history)

# Create models for women's Final Climb
ladies_fc_models <- create_position_models(ladies_fc_history)

# Create models for men's TdS
men_tds_models <- create_position_models(men_tds_history)

# Create models for women's TdS
ladies_tds_models <- create_position_models(ladies_tds_history)

# Now let's create a function to apply these models to the startlists
predict_positions <- function(startlist, models, selected_features) {
  predictions <- data.frame(
    Skier = startlist$Skier,
    ID = startlist$ID
  )
  
  for(threshold in position_thresholds) {
    model_name <- paste0("place_", threshold)
    model <- models$models[[model_name]]
    
    # Get predictions
    pred_probs <- predict(model, newdata = startlist, type = "response")
    predictions[[paste0("prob_top", threshold)]] <- pred_probs
  }
  
  return(predictions)
}

# Apply models to startlists
men_fc_predictions <- predict_positions(men_startlist, men_fc_models)
ladies_fc_predictions <- predict_positions(ladies_startlist, ladies_fc_models)
men_tds_predictions <- predict_positions(men_startlist, men_tds_models)
ladies_tds_predictions <- predict_positions(ladies_startlist, ladies_tds_models)

# Function to create summary report
create_model_summary <- function(models, selected_features, history_data, event_type, gender) {
  sink(file.path(tds_log_dir, paste0(gender, "_", event_type, "_model_summary.txt")))
  
  cat(paste(gender, event_type, "Models Summary\n"))
  cat("============================\n\n")
  
  for(threshold in position_thresholds) {
    model_name <- paste0("place_", threshold)
    cat(paste("\nTop", threshold, "Model:\n"))
    cat("Selected variables:", paste(selected_features[[model_name]], collapse=", "), "\n")
    
    # Calculate Brier score on training data
    actual <- history_data$Place <= threshold
    predicted <- predict(models$models[[model_name]], type = "response")
    brier <- calculate_brier(actual, predicted)
    
    cat("Brier Score:", round(brier, 4), "\n")
  }
  
  sink()
}

# Create summaries
create_model_summary(men_fc_models, men_fc_models$selected_features, men_fc_history, "FC", "Men")
create_model_summary(ladies_fc_models, ladies_fc_models$selected_features, ladies_fc_history, "FC", "Women")
create_model_summary(men_tds_models, men_tds_models$selected_features, men_tds_history, "TdS", "Men")
create_model_summary(ladies_tds_models, ladies_tds_models$selected_features, ladies_tds_history, "TdS", "Women")

men_tds_predictions

library(writexl)

clean_predictions <- function(df) {
  df %>%
    dplyr::select(-ID) %>%
    rename(
      "Top 1" = prob_top1,
      "Top 3" = prob_top3,
      "Top 10" = prob_top10,
      "Top 30" = prob_top30
    )
}

# Clean and combine all predictions
all_predictions <- list(
  men_fc = clean_predictions(men_fc_predictions),
  ladies_fc = clean_predictions(ladies_fc_predictions),
  men_tds = clean_predictions(men_tds_predictions),
  ladies_tds = clean_predictions(ladies_tds_predictions)
)

# Write to Excel
write_xlsx(all_predictions, "/Users/syverjohansen/blog/daehl-e/content/post/drafts/weekly-picks/2025TdS/TdS_brier_odds.xlsx")
```
```

