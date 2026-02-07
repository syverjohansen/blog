
# Weekly Predictions: Updated Methodology with Race Probabilities
library(dplyr)
library(tidyr)
library(openxlsx)
library(arrow)
library(mgcv)
library(leaps)
library(logger)
library(purrr)
library(lubridate) # For better date handling
library(ompr)          # For optimization model
library(ompr.roi)      # For optimization solver interface
library(ROI.plugin.glpk) # For GLPK solver
library(slider)        # For sliding window operations

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
log_dir <- "~/ski/elo/python/ski/polars/excel365/weekly-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "weekly_picks_processing.log")))
log_info("Starting weekly predictions process")

# Read in the race schedule from weekends.csv with proper date parsing
log_info("Reading weekends data")
weekends <- read.csv("~/ski/elo/python/ski/polars/excel365/weekends.csv", 
                     stringsAsFactors = FALSE) %>%
  mutate(Date = mdy(Date)) # Use lubridate's mdy function to parse MM/DD/YY format

# Find the next race weekend after today (March 1, 2025)
current_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")
log_info(paste("Current date:", current_date))

# Filter races after the current date and get the next date
next_races <- weekends %>%
  filter(Date >= current_date) %>%
  arrange(Date)

# Get the date of the next race weekend
next_weekend_date <- min(next_races$Date, na.rm = TRUE)
log_info(paste("Next weekend date:", next_weekend_date))

# Filter races for just the next weekend
next_weekend_races <- next_races %>%
  filter(Date == next_weekend_date)
print(next_weekend_races)
# Create race dataframes for men and ladies
men_races <- next_weekend_races %>%
  filter(Sex == "M") %>%
  filter(!Distance %in% c("Rel", "Ts")) %>%  # Exclude relay and team sprint
  dplyr::select(Distance, Technique, MS, Elevation, Period, Pursuit) %>%
  rename(distance = Distance, technique = Technique, 
         ms = MS, altitude = Elevation, period = Period)

ladies_races <- next_weekend_races %>%
  filter(Sex == "L") %>%
  filter(!Distance %in% c("Rel", "Ts")) %>%  # Exclude relay and team sprint
  dplyr::select(Distance, Technique, MS, Elevation, Period, Pursuit) %>%
  rename(distance = Distance, technique = Technique, 
         ms = MS, altitude = Elevation, period = Period)

log_info(paste("Found", nrow(men_races), "men's races and", nrow(ladies_races), "ladies races"))


## Calculate Race Probabilities


# Corrected race probability calculation function
calculate_race_probabilities <- function() {
  log_info("Calculating race participation probabilities")
  
  # Read chronological data
  log_info("Reading chronological data")
  men_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv", 
                       stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date))
  
  ladies_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv", 
                          stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date))
  
  # Read startlists
  log_info("Reading startlists")
  men_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_weekend_men.csv", 
                          stringsAsFactors = FALSE)
  men_startlist$Sex = "M"
  
  ladies_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_weekend_ladies.csv", 
                             stringsAsFactors = FALSE)
  ladies_startlist$Sex = "L"
  # Add config nation information if not present
  if(!"Config_Nation" %in% names(men_startlist)) {
    log_info("Adding Config_Nation column to men's startlist")
    config_nations <- c("Norway", "Sweden", "Finland", "Germany", "Switzerland", "Russia", "Italy", "France")
    men_startlist$Config_Nation <- men_startlist$Nation %in% config_nations
  }
  
  if(!"Config_Nation" %in% names(ladies_startlist)) {
    log_info("Adding Config_Nation column to ladies' startlist")
    config_nations <- c("Norway", "Sweden", "Finland", "Germany", "Switzerland", "Russia", "Italy", "France")
    ladies_startlist$Config_Nation <- ladies_startlist$Nation %in% config_nations
  }
  
  # Add national quotas if not present
  if(!"Quota" %in% names(men_startlist)) {
    log_info("Adding Quota column to men's startlist")
    # Base quotas for most nations
    men_startlist$Quota <- 4
    
    # Special quotas for some nations
    men_startlist$Quota[men_startlist$Nation %in% c("Norway", "Russia")] <- 8
    men_startlist$Quota[men_startlist$Nation %in% c("Sweden", "Finland", "Germany", "Switzerland", "Italy", "France")] <- 6
    
    # Host nation bonus
    men_startlist$Is_Host_Nation <- men_startlist$Nation == next_weekend_races$Nation[1]
    men_startlist$Quota[men_startlist$Is_Host_Nation] <- men_startlist$Quota[men_startlist$Is_Host_Nation] + 2
  }
  
  if(!"Quota" %in% names(ladies_startlist)) {
    log_info("Adding Quota column to ladies' startlist")
    # Base quotas for most nations
    ladies_startlist$Quota <- 4
    
    # Special quotas for some nations
    ladies_startlist$Quota[ladies_startlist$Nation %in% c("Norway", "Russia")] <- 8
    ladies_startlist$Quota[ladies_startlist$Nation %in% c("Sweden", "Finland", "Germany", "Switzerland", "Italy", "France")] <- 6
    
    # Host nation bonus
    ladies_startlist$Is_Host_Nation <- ladies_startlist$Nation == next_weekend_races$Nation[1]
    ladies_startlist$Quota[ladies_startlist$Is_Host_Nation] <- ladies_startlist$Quota[ladies_startlist$Is_Host_Nation] + 2
  }
  
  # Function to get race probability for a skier
  get_race_probability <- function(chronos, skier, race_type, technique) {
    log_debug(paste("Calculating probability for skier:", skier))
    
    # Get skier's first ever race date
    skier_first_race <- chronos %>%
      filter(Skier == skier) %>%
      arrange(Date) %>%
      dplyr::slice(1) %>%
      pull(Date)
    
    # Use 2020-01-01 or skier's first race, whichever is later
# Calculate date from 5 years ago
    five_years_ago <- Sys.Date() - (5 * 365)
      
    # Use 5 years ago or skier's first race, whichever is later
    start_date <- if(length(skier_first_race) == 0) {
      five_years_ago
    } else {
      max(five_years_ago, as.Date(skier_first_race))
    }
    log_debug(paste("Using start date:", format(start_date, "%Y-%m-%d"), "for skier:", skier))
    # First get all matching races since start_date
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
    # Then get this skier's participations
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
      log_debug(paste("No races found for type:", race_type, ", technique:", technique))
      return(0)
    }
    
    races_participated <- nrow(skier_races)
    # Cap probability at 1
    prob <- min(1, races_participated / total_races)
    
    log_debug(paste("Probability for", skier, "since", start_date, ":", prob, 
                   "(", races_participated, "/", total_races, " races)"))
    
    return(prob)
  }
  
  # Function to get additional skiers for non-config nations
  get_additional_nation_skiers <- function(chronos, startlist, nation) {
    log_debug(paste("Getting additional skiers for", nation))
    
    current_season <- max(chronos$Season, na.rm = TRUE)
    
    # Get all skiers from this nation in current season
    season_skiers <- chronos %>%
      filter(
        Nation == nation,
        Season == current_season
      ) %>%
      distinct(Skier) %>%
      pull(Skier)
    
    # Get current skiers in startlist
    current_skiers <- startlist %>%
      filter(Nation == nation) %>%
      pull(Skier)
    
    # Find missing skiers
    missing_skiers <- setdiff(season_skiers, current_skiers)
    
    log_debug(paste("Found", length(missing_skiers), "additional skiers for", nation))
    return(missing_skiers)
  }
  
  # Normalize probabilities to respect nation quotas
  normalize_to_quota <- function(startlist) {
    log_info("Normalizing probabilities to nation quotas")
    
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
              quota <- 2  # Default quota if missing
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
  
  # Process men's and ladies' race probabilities
process_gender_probabilities <- function(startlist, chronos, races) {
  # Check if we have FIS startlist (at least one skier has In_FIS_List=True)
  has_fis_startlist <- FALSE
  if("In_FIS_List" %in% names(startlist)) {
    has_fis_startlist <- any(startlist$In_FIS_List, na.rm = TRUE)
  }
  
  # Handle Race1_Prob based on FIS startlist existence
  if(has_fis_startlist) {
    log_info("FIS startlist exists (at least one In_FIS_List=True)")
    # When FIS startlist exists: In_FIS_List=True → 1, In_FIS_List=False → 0
    # This overrides whatever Python set, because FIS list is authoritative for Race 1
    if(!"Race1_Prob" %in% names(startlist)) {
      startlist$Race1_Prob <- NA_real_
    }
    startlist$Race1_Prob <- ifelse(startlist$In_FIS_List, 1, 0)
    log_info(paste("Set Race1_Prob: ", sum(startlist$Race1_Prob == 1), "athletes with prob=1,",
                   sum(startlist$Race1_Prob == 0), "with prob=0"))
  } else {
    log_info("No FIS startlist (all In_FIS_List=False)")
    # No FIS startlist - use In_Config to set probability
    if(!"Race1_Prob" %in% names(startlist)) {
      startlist$Race1_Prob <- NA_real_
    }
    # Config athletes get probability = 1 when no FIS startlist exists
    if("In_Config" %in% names(startlist)) {
      startlist$Race1_Prob <- ifelse(startlist$In_Config, 1, startlist$Race1_Prob)
      log_info(paste("Set Race1_Prob from In_Config:", sum(startlist$Race1_Prob == 1, na.rm = TRUE),
                     "athletes with prob=1"))
    }
    # OLD METHOD (commented out): Race1_Prob will be calculated like other races below
    # log_info("No FIS startlist (all In_FIS_List=False), will calculate Race1_Prob")
  }
  
  # Handle Config_Nation and In_Config status for all races
# Handle Config_Nation and In_Config status for all races
if("Config_Nation" %in% names(startlist) && "In_Config" %in% names(startlist)) {
  # First ensure both columns are logical type
  if(!is.logical(startlist$Config_Nation)) {
    startlist$Config_Nation <- as.logical(startlist$Config_Nation)
  }
  if(!is.logical(startlist$In_Config)) {
    startlist$In_Config <- as.logical(startlist$In_Config)
  }
  if("In_FIS_List" %in% names(startlist) && !is.logical(startlist$In_FIS_List)) {
    startlist$In_FIS_List <- as.logical(startlist$In_FIS_List)
  }

  # Set prob=0 for config nation skiers NOT in config AND NOT in FIS list
  # These are athletes from nations that announced squads but weren't included
  # Note: In_FIS_List=True athletes are NOT included here, so their Race2+ probs
  # will be calculated from history (they're at the venue, might race)
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
      log_info("Using existing Race1_Prob from FIS startlist, skipping calculation")
      next
    }
    
    # Create the column if it doesn't exist
    if(!(race_prob_col %in% names(startlist))) {
      startlist[[race_prob_col]] <- NA_real_
    }

    # When no FIS startlist exists, set In_Config athletes to probability = 1
    if(!has_fis_startlist && "In_Config" %in% names(startlist)) {
      config_athletes <- which(startlist$In_Config == TRUE & (is.na(startlist[[race_prob_col]]) | startlist[[race_prob_col]] != 1))
      if(length(config_athletes) > 0) {
        startlist[config_athletes, race_prob_col] <- 1
        log_info(paste("Set", race_prob_col, "= 1 for", length(config_athletes), "In_Config athletes (no FIS startlist)"))
      }
    }

    # Rest of the existing processing by nation logic...
      
      # Process by nation
      nations <- unique(startlist$Nation)
      for(nation in nations) {
        log_info(paste("Processing", nation, "for race", i))
        
        nation_skiers <- startlist %>% filter(Nation == nation)
        is_config_nation <- nation_skiers$Config_Nation[1]
        quota <- nation_skiers$Quota[1]
        
        if(is_config_nation) {
          # For config nations, use only startlist skiers
          
          # Check if sum of probabilities already equals quota
          existing_probs <- nation_skiers[[race_prob_col]]
          existing_probs[is.na(existing_probs)] <- 0
          
          if(sum(existing_probs) == quota) {
            log_info(paste("Sum of probabilities already equals quota for", nation))
            next
          }
          
          # Calculate probabilities for all skiers
          for(j in 1:nrow(nation_skiers)) {
            skier <- nation_skiers$Skier[j]
            # Check if probability is already preset from confirmed source
            existing_prob <- startlist[startlist$Skier == skier, race_prob_col]
            skier_in_fis <- startlist[startlist$Skier == skier, "In_FIS_List"]
            skier_in_config <- startlist[startlist$Skier == skier, "In_Config"]
            skier_config_nation <- startlist[startlist$Skier == skier, "Config_Nation"]

            # Preserve prob==1 (confirmed racing via FIS or config yes list)
            if(!is.na(existing_prob) && existing_prob == 1) {
              log_debug(paste("Using preset probability 1 for", skier))
              next
            }
            # Preserve prob==0 only for config nation athletes not in config
            # (these were explicitly set to 0 and should not race)
            # Also preserve if In_Config with explicit no list (prob==0)
            if(!is.na(existing_prob) && existing_prob == 0 &&
               (isTRUE(skier_config_nation) && !isTRUE(skier_in_config) && !isTRUE(skier_in_fis))) {
              log_debug(paste("Using preset probability 0 (config nation not in config) for", skier))
              next
            }
            if(!is.na(existing_prob) && existing_prob == 0 && isTRUE(skier_in_config)) {
              log_debug(paste("Using preset probability 0 (explicit config no list) for", skier))
              next
            }

            # Calculate probability from history for athletes with NA prob
            startlist[startlist$Skier == skier, race_prob_col] <-
              get_race_probability(chronos, skier, races$distance[i], races$technique[i])
          }
        } else {
          # For non-config nations, get all active skiers
          additional_skiers <- get_additional_nation_skiers(chronos, startlist, nation)

          # Calculate probabilities for startlist skiers
          for(j in 1:nrow(nation_skiers)) {
            skier <- nation_skiers$Skier[j]
            # Check if probability is already preset from confirmed source
            existing_prob <- startlist[startlist$Skier == skier, race_prob_col]
            skier_in_fis <- startlist[startlist$Skier == skier, "In_FIS_List"]
            skier_in_config <- startlist[startlist$Skier == skier, "In_Config"]
            skier_config_nation <- startlist[startlist$Skier == skier, "Config_Nation"]

            # Preserve prob==1 (confirmed racing via FIS or config yes list)
            if(!is.na(existing_prob) && existing_prob == 1) {
              log_debug(paste("Using preset probability 1 for", skier))
              next
            }
            # Preserve prob==0 only for config nation athletes not in config
            # (these were explicitly set to 0 and should not race)
            # Also preserve if In_Config with explicit no list (prob==0)
            if(!is.na(existing_prob) && existing_prob == 0 &&
               (isTRUE(skier_config_nation) && !isTRUE(skier_in_config) && !isTRUE(skier_in_fis))) {
              log_debug(paste("Using preset probability 0 (config nation not in config) for", skier))
              next
            }
            if(!is.na(existing_prob) && existing_prob == 0 && isTRUE(skier_in_config)) {
              log_debug(paste("Using preset probability 0 (explicit config no list) for", skier))
              next
            }
            startlist[startlist$Skier == skier, race_prob_col] <-
              get_race_probability(chronos, skier, races$distance[i], races$technique[i])
          }
          
          # Now add additional skiers if any
          if(length(additional_skiers) > 0) {
            # Get the column types from original startlist
            col_types <- sapply(startlist, class)
            
            # Create temporary dataframe for new skiers with matching types
            additional_rows <- data.frame(
              Skier = additional_skiers,
              Nation = nation,
              # This is the key fix - use logical instead of character
              Config_Nation = FALSE,
              Quota = quota,
              stringsAsFactors = FALSE
            )
            
            # Set Race1_Prob to 0 (they're not in the Race 1 startlist)
            additional_rows$Race1_Prob <- 0
            
            # Calculate Race i probability
            additional_rows[[race_prob_col]] <- sapply(additional_skiers, function(skier) {
              get_race_probability(chronos, skier, races$distance[i], races$technique[i])
            })
            
            # Add required columns that might be missing and ensure correct types
            for(col in names(startlist)) {
              if(!(col %in% names(additional_rows))) {
                # Create the missing column with the correct type
                if(col_types[col] == "logical") {
                  additional_rows[[col]] <- FALSE
                } else if(col_types[col] == "numeric") {
                  additional_rows[[col]] <- 0
                } else if(col_types[col] == "integer") {
                  additional_rows[[col]] <- 0L
                } else {
                  additional_rows[[col]] <- NA
                }
              } else {
                # Convert existing column to correct type if needed
                if(col_types[col] == "logical" && !is.logical(additional_rows[[col]])) {
                  additional_rows[[col]] <- as.logical(additional_rows[[col]])
                } else if(col_types[col] == "numeric" && !is.numeric(additional_rows[[col]])) {
                  additional_rows[[col]] <- as.numeric(additional_rows[[col]])
                } else if(col_types[col] == "integer" && !is.integer(additional_rows[[col]])) {
                  additional_rows[[col]] <- as.integer(additional_rows[[col]])
                } else if(col_types[col] == "character" && !is.character(additional_rows[[col]])) {
                  additional_rows[[col]] <- as.character(additional_rows[[col]])
                }
              }
            }
            
            # Append to startlist
            startlist <- bind_rows(startlist, additional_rows[names(startlist)])
          }
        }
      }
    }
    
    # Normalize probabilities to respect quotas
    startlist <- normalize_to_quota(startlist)
    
    return(startlist)
  }
  
  # Process each gender
  log_info("Processing men's race probabilities")
  men_startlist_with_probs <- process_gender_probabilities(men_startlist, men_chrono, men_races)
  
  log_info("Processing ladies' race probabilities")
  ladies_startlist_with_probs <- process_gender_probabilities(ladies_startlist, ladies_chrono, ladies_races)
  
  # Save results
  log_info("Saving race probability results")
  write.csv(men_startlist_with_probs, 
            "~/ski/elo/python/ski/polars/excel365/startlist_weekend_men.csv", 
            row.names = FALSE)
  
  write.csv(ladies_startlist_with_probs, 
            "~/ski/elo/python/ski/polars/excel365/startlist_weekend_ladies.csv", 
            row.names = FALSE)
  
  log_info("Race probability calculation complete")
  
  # Display samples
  # cat("\nSample of Men's Race Probabilities:\n")
  # print(men_startlist_with_probs %>% 
  #   dplyr::select(Skier, ID, Nation, Config_Nation, contains("Race")) %>% 
  #   arrange(Nation) )
  #   
  # cat("\nSample of Ladies' Race Probabilities:\n")
  # print(ladies_startlist_with_probs %>% 
  #   dplyr::select(Skier, ID, Nation, Config_Nation, contains("Race")) %>% 
  #   arrange(Nation) )
  return(list(
    men = men_startlist_with_probs,
    ladies = ladies_startlist_with_probs
  ))
}



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
  #View(final_predictions)
  # Rest of the function remains the same...

    # Add remaining races dynamically
    if(length(race_dfs) > 1) {
        for(i in 2:length(race_dfs)) {
            log_info(paste("Adding race", i, "to combined predictions"))
            
            # Get probability column name
            race_prob_col <- paste0("Race", i, "_Prob")
            
            # Check if Race{i}_Prob exists in this race_df
            if(!race_prob_col %in% names(race_dfs[[i]])) {
                log_warn(paste("Race probability column", race_prob_col, "not found in race_dfs[[", i, "]]"))
                
                # Try to fix by getting from startlist
                if(race_prob_col %in% names(startlist)) {
                    log_info("Copying probability from original startlist")
                    race_dfs[[i]][[race_prob_col]] <- startlist[match(race_dfs[[i]]$Skier, startlist$Skier), race_prob_col]
                } else {
                    log_warn("Cannot find probability in startlist either, using default value")
                    race_dfs[[i]][[race_prob_col]] <- 0
                }
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
    select_cols <- c("Skier", "ID", "Nation", "Price")
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

prepare_startlist_data <- function(startlist, race_df, pelo_col) {
    # Print some debug info
    log_info(paste("Preparing startlist data for", pelo_col))

    # Dynamically get race probability columns - important to preserve these!
    race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
    log_info(paste("Race probability columns found:", paste(race_prob_cols, collapse=", ")))

    # Get all required Elo columns and their corresponding Pelo names
    elo_cols <- c("Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
                  "Elo", "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo",
                  "Freestyle_Elo", "Classic_Elo")

    pelo_cols <- c("Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                   "Pelo", "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo",
                   "Freestyle_Pelo", "Classic_Pelo")

    # Keep essential columns from startlist including Elo columns (already from chrono_pred via Python)
    available_elo_cols <- intersect(elo_cols, names(startlist))
    log_info(paste("Using Elo columns from startlist:", paste(available_elo_cols, collapse=", ")))

    base_df <- startlist %>%
        dplyr::select(Skier, ID, Nation, Sex, Price, all_of(race_prob_cols), any_of(elo_cols))

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

    # Combine all data (Elos already in base_df from startlist, just add points)
    result_df <- base_df %>%
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
    
    # Calculate max values for normalization using startlist maximum (consistent with race-picks.R)
    available_elo_cols <- intersect(names(result_df), elo_cols)
    if(length(available_elo_cols) > 0) {
        # Calculate both Elo and Pelo percentages for available columns
        for(i in seq_along(elo_cols)) {
            elo_col <- elo_cols[i]
            pelo_col_i <- pelo_cols[i]
            
            # Check if column exists in startlist data
            if(elo_col %in% names(result_df)) {
                # Use startlist maximum instead of historical maximum
                max_val <- max(result_df[[elo_col]], na.rm = TRUE)
                # Only calculate if max value is not zero or NA
                if(!is.na(max_val) && max_val > 0) {
                    # Calculate the percentage
                    pct_value <- result_df[[elo_col]] / max_val
                    
                    # Assign to both Elo and Pelo percentage columns
                    result_df[[paste0(elo_col, "_Pct")]] <- pct_value
                    result_df[[paste0(pelo_col_i, "_Pct")]] <- pct_value
                    
                    log_info(paste("Calculated percentage for", elo_col, "using startlist maximum"))
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

# Function to prepare startlist data with ELO information


preprocess_data <- function(df) {
    # Load weekends data to determine points systems for historical races
  weekends_data <- read.csv("~/ski/elo/python/ski/polars/excel365/weekends.csv", 
                       stringsAsFactors = FALSE) %>%
    mutate(Date = mdy(Date)) # This will work for MM/DD/YYYY format (4-digit year)
  
    
    # Determine points system based on next race weekend
    # Find the next race after today
    print(weekends_data)
    current_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")
    next_weekend <- weekends_data %>%
      filter(Date >= current_date) %>%
      arrange(Date) %>%
      dplyr::slice(1)
    print(next_weekend)
    # Check if the next race is a stage race
    is_stage_weekend <- !is.na(next_weekend$Stage) && next_weekend$Stage == 1
    
    # Select the appropriate points system for ALL races
    global_points_system <- if(is_stage_weekend) {
      log_info("Using STAGE points system for upcoming weekend")
      stage_points
    } else {
      log_info("Using WORLD CUP points system for upcoming weekend") 
      wc_points
    }
    
    # First calculate points using historical data but with the GLOBAL points system
    df_with_points <- df %>%
        # Add points based on the global points system determined by next weekend
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
    #print(unique(df$Distance))
    
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
    #print(df_with_points %>% filter(Distance!="Sprint") %>% filter(Technique=="F") %>% filter(Skier=="Therese Johaug"))
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

# Weekly Fantasy Team Optimization Function
optimize_weekly_team <- function(men_results, ladies_results, prediction_type = "normal") {
  # Get the appropriate prediction columns based on prediction_type
  points_col <- case_when(
    prediction_type == "safe" ~ "Total_Safe",
    prediction_type == "upside" ~ "Total_Upside",
    TRUE ~ "Total_Points"
  )
  
  # Prepare men's data
  men_df <- men_results$full_predictions %>%
    mutate(
      Sex = "M",
      Points = get(points_col),  # Use the selected prediction column
      Price = as.numeric(Price)  # Ensure price is numeric
    )
  
  # Prepare ladies' data
  ladies_df <- ladies_results$full_predictions %>%
    mutate(
      Sex = "L",
      Points = get(points_col),  # Use the selected prediction column
      Price = as.numeric(Price)  # Ensure price is numeric
    )
  
  # Combine datasets
  fantasy_df <- bind_rows(men_df, ladies_df) %>%
    mutate(row_id = row_number()) %>%
    filter(!is.na(Points), !is.na(Price))  # Remove rows with missing values
  
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
  result <- solve_model(model, with_ROI(solver = "glpk"))
  
  # Extract results
  selected <- get_solution(result, x[i]) %>%
    filter(value > 0) %>%
    pull(i)
  
  # Create results dataframe
  selected_team <- fantasy_df[selected, ] %>%
    arrange(Sex, desc(Points))
  
  # Print results summary
  log_info(paste("Optimized Weekly Fantasy Team:", prediction_type))
  log_info(sprintf("Total Predicted Points: %.2f", sum(selected_team$Points)))
  log_info(sprintf("Total Cost: $%d", sum(selected_team$Price)))
  
  log_info(paste("Men:", sum(selected_team$Sex == "M")))
  log_info(paste("Women:", sum(selected_team$Sex == "L")))
  
  # Return selected team
  return(selected_team %>%
         dplyr::select(Skier, ID, Sex, Nation, Price, Points) %>%
         arrange(Sex, desc(Points)))
}

# Run this function after running predictions
run_fantasy_optimization <- function(men_results, ladies_results, weekend_date) {
  # Create directory for output - use race-picks instead of weekly-picks
  # Use UTC time for consistent date formatting
  utc_date <- format(Sys.time(), "%Y%m%d", tz = "UTC")
  race_picks_dir_path <- paste0("~/blog/daehl-e/content/post/cross-country/drafts/race-picks/", utc_date)
  
  
  if (!dir.exists(race_picks_dir_path)) {
    dir.create(race_picks_dir_path, recursive = TRUE)
  }
  
  log_info("Optimizing fantasy teams...")

  # OLD METHOD (MIP optimization with budget constraints) - commented out for now
  # normal_team <- optimize_weekly_team(men_results, ladies_results, "normal")
  # safe_team <- optimize_weekly_team(men_results, ladies_results, "safe")
  # upside_team <- optimize_weekly_team(men_results, ladies_results, "upside")

  # NEW METHOD: Simply take top 8 men and top 8 ladies by predicted points
  log_info("Selecting top 8 men and top 8 ladies by Total_Points...")

  top8_men <- men_results$full_predictions %>%
    arrange(desc(Total_Points)) %>%
    head(8) %>%
    mutate(Sex = "M") %>%
    dplyr::select(Skier, ID, Sex, Nation, Price, Points = Total_Points)

  top8_ladies <- ladies_results$full_predictions %>%
    arrange(desc(Total_Points)) %>%
    head(8) %>%
    mutate(Sex = "L") %>%
    dplyr::select(Skier, ID, Sex, Nation, Price, Points = Total_Points)

  # Combine into fantasy team
  normal_team <- bind_rows(top8_men, top8_ladies) %>%
    arrange(Sex, desc(Points))

  log_info(sprintf("Fantasy team total predicted points: %.2f", sum(normal_team$Points)))
  log_info(paste("Men:", nrow(top8_men), "| Ladies:", nrow(top8_ladies)))

  # Create placeholder teams for backward compatibility (same as normal for now)
  safe_team <- normal_team
  upside_team <- normal_team

  # Save all teams to one Excel file with multiple sheets
  log_info("Saving fantasy team results...")
  # write.xlsx(list(
  #   "Normal Team" = normal_team %>% rename(`Predicted Points` = Points),
  #   "Safe Team" = safe_team %>% rename(`Safe Points` = Points),
  #   "Upside Team" = upside_team %>% rename(`Upside Points` = Points)
  # ), file.path(race_picks_dir_path, "fantasy-teams.xlsx"))

  # Save the fantasy team to race-picks directory
  normal_team %>%
    rename(`Predicted Points` = Points) %>%
    write.xlsx(file.path(race_picks_dir_path, "fantasy_team.xlsx"))
  
  log_info("Fantasy optimization complete")
  
  return(list(
    normal_team = normal_team,
    safe_team = safe_team,
    upside_team = upside_team
  ))
}



















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
  
  # APPLY MONOTONIC CONSTRAINTS: Ensure Win <= Podium <= Top5 <= Top10 <= Top30
  log_info("Applying monotonic constraints...")
  
  # Get available probability columns in ascending order
  prob_cols <- paste0("prob_top", sort(position_thresholds))
  prob_cols <- prob_cols[prob_cols %in% names(normalized)]
  
  # For each skier, ensure probabilities are monotonically non-decreasing
  for(i in 1:nrow(normalized)) {
    probs <- numeric(length(prob_cols))
    for(j in 1:length(prob_cols)) {
      probs[j] <- normalized[[prob_cols[j]]][i]
    }
    
    # Apply monotonic adjustment: each probability should be >= previous one
    for(j in 2:length(probs)) {
      if(probs[j] < probs[j-1]) {
        probs[j] <- probs[j-1]  # Set to previous value
      }
    }
    
    # Update the normalized dataframe
    for(j in 1:length(prob_cols)) {
      normalized[[prob_cols[j]]][i] <- probs[j]
    }
  }
  
  # RE-NORMALIZE after monotonic adjustment to maintain target sums
  log_info("Re-normalizing after monotonic constraints...")
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    
    if(prob_col %in% names(normalized)) {
      current_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
      target_sum <- 100 * threshold
      
      if(current_sum > 0) {
        scaling_factor <- target_sum / current_sum
        normalized[[prob_col]] <- normalized[[prob_col]] * scaling_factor
        
        # Cap at 100% again
        normalized[[prob_col]][normalized[[prob_col]] > 100] <- 100
      }
    }
  }
  
  # Log final sums after all adjustments
  log_info("Position probability sums AFTER normalization and monotonic constraints:")
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

# Enhanced predict_races function with position probability adjustments
predict_races <- function(gender) {
  # Load chronological data
  chrono_path <- ifelse(gender == "men", 
                     "~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv",
                     "~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv")
  
  # Get appropriate startlist and races
  startlist <- if(gender == "men") men_startlist else ladies_startlist
  races <- if(gender == "men") men_races else ladies_races
  
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
    race_info <- next_weekend_races %>%
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
    exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
    summary_exhaustive <- summary(exhaustive_selection)
    best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
    smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
    gam_formula <- as.formula(paste("Points ~", smooth_terms))
    
    model <- gam(gam_formula, data = race_df_75)
    
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
    #View(startlist_prepared)
    
    # Ensure race probability column exists
    if(!(race_prob_col %in% names(startlist_prepared))) {
      log_warn(paste("Race probability column missing:", race_prob_col))
      if(race_prob_col %in% names(startlist)) {
        # Copy from original startlist if available
        log_info("Copying from original startlist")
        startlist_prepared[[race_prob_col]] <- startlist[match(startlist_prepared$Skier, startlist$Skier), race_prob_col]
      } else {
        # Default to 1 for first race, 0 for others if not available
        log_info("Setting default probabilities")
        startlist_prepared[[race_prob_col]] <- if(i == 1) 1 else 0
      }
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
  #print(startlist_prepared)
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
  # Place this code right after you store base_predictions but before applying adjustments

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
  #View(adjustment_df)
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
        #print(top_skiers)
      }
    }
    
    # Remove race probability column from position_preds (it's already accounted for now)
    # if(race_prob_col %in% names(position_preds)) {
    #  position_preds <- position_preds %>% dplyr::select(-all_of(race_prob_col))
    # }
    #View(position_preds)
    # Store position predictions for this race
    position_predictions[[i]] <- position_preds

    
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
    
    #print(prob_summary %>% head(5))
    
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
        
        # Apply race probability to predictions
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
                   Predicted_Points,
                   Final_Prediction, Safe_Prediction, Upside_Prediction,
                   race_prob_col)
    
    #View(race_dfs[[i]])

    
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
      # Also account for race probability
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
      # Try to fix
      if("Race_Prob" %in% names(race_dfs[[i]])) {
        log_info("Renaming Race_Prob column to correct race probability column name")
        race_dfs[[i]][[race_prob_col]] <- race_dfs[[i]][["Race_Prob"]]
      }
    }
  }

  # Get number of races from races dataframe
  n_races <- nrow(races)
  
  # Combine all race predictions (points)
  final_predictions <- combine_predictions(race_dfs, startlist)
  
  log_info(paste("Final predictions calculated for", gender))
  
  # Create post predictions for blog (points)
  post_predictions <- create_post_predictions(final_predictions, n_races, gender)


  # NEW: Combine all position predictions into one dataframe
  all_position_predictions <- bind_rows(position_predictions)

  
  # NEW: Create formatted position probabilities
  formatted_position_results <- format_position_results(all_position_predictions, next_weekend_date, gender)
  
  # Create folder path based on next race weekend date
  weekend_folder <- format(next_weekend_date, "%Y%m%d")
  dir_path <- paste0("~/blog/daehl-e/content/post/cross-country/drafts/weekly-picks/", weekend_folder)
  
  # Create directory if it doesn't exist
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  
  
  # Save points predictions to Excel
  points_file_path <- file.path(dir_path, paste0(ifelse(gender == "men", "men", "ladies"), ".xlsx"))
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
# Helper function to format position probability results
format_position_results <- function(position_results, next_weekend_date, gender) {
  # Create a more reader-friendly version
  #View(position_results)
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
  

  weekend_folder <- format(next_weekend_date, "%Y%m%d")
  dir_path <- paste0(
    "~/blog/daehl-e/content/post/cross-country/drafts/weekly-picks/", 
    weekend_folder
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
  men_positions <- men_results$position_predictions
  ladies_positions <- ladies_results$position_predictions
  
  weekend_folder <- format(next_weekend_date, "%Y%m%d")
  dir_path <- paste0(
    "~/blog/daehl-e/content/post/cross-country/drafts/weekly-picks/", 
    weekend_folder
  )
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  top_contenders <- list()
  
  # Process men's results
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
  
  # Process ladies' results
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
  
  # Save to Excel
  output_file <- file.path(dir_path, "top_contenders.xlsx")
  #write.xlsx(top_contenders, output_file)
  
  log_info(paste("Top contenders summary saved to", output_file))
  
  return(top_contenders)
}

# Run the integrated predictions workflow
run_integrated_predictions_workflow <- function() {
  log_info("Running integrated predictions workflow (points and position probabilities)")
  
  # Run for men
  log_info("Processing men's predictions")
  men_results <- predict_races("men")

  
  # Run for ladies
  log_info("Processing ladies predictions")
  ladies_results <- predict_races("ladies")
  
  # Create top contenders summary
  top_contenders <- create_top_contenders_summary(men_results, ladies_results)
  
  # Run fantasy optimization
  fantasy_results <- run_fantasy_optimization(men_results, ladies_results, next_weekend_date)
  
  # Display sample results
  log_info("Prediction workflow complete")
  

  
  # Return results
  return(list(
    men = men_results,
    ladies = ladies_results,
    top_contenders = top_contenders,
    fantasy = fantasy_results
  ))
}

# First calculate race probabilities
log_info("Calculating race probabilities")
prob_results <- calculate_race_probabilities()

log_info("Updating startlist variables with calculated probabilities")
men_startlist <- prob_results$men
ladies_startlist <- prob_results$ladies


# Run integrated predictions workflow
log_info("Running integrated predictions workflow")
integrated_results <- run_integrated_predictions_workflow()


# Display race probability impact on fantasy picks
cat("\nRace Probability Impact on Top Fantasy Picks:\n")
top_fantasy_skiers <- c(
  integrated_results$fantasy$normal_team$Skier[1:5], 
  integrated_results$fantasy$safe_team$Skier[1:2], 
  integrated_results$fantasy$upside_team$Skier[1:2]
)

top_skiers_probs <- integrated_results$men$full_predictions %>%
  filter(Skier %in% top_fantasy_skiers) %>%
  dplyr::select(Skier, Nation, contains("Race1_Prob"), contains("Race2_Prob"), Total_Points) %>%
  arrange(desc(Total_Points))

# Show comparison of optimized teams
cat("\nComparison of Different Optimization Strategies:\n")
cat("Normal Team Total Points:", sum(integrated_results$fantasy$normal_team$Points), "\n")
cat("Safe Team Total Points:", sum(integrated_results$fantasy$safe_team$Points), "\n")
cat("Upside Team Total Points:", sum(integrated_results$fantasy$upside_team$Points), "\n")









































