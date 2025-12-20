# Tour de Ski Predictions: Points and Probability Modeling
# Based on weekly-picks2.R structure with TdS-predictions.Rmd methodology
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
tds_points <- c(300,285,270,255,240,216,207,198,189,180,174,168,162,156,150,144,138,132,126,120,114,108,102,96,90,84,78,72,66,60,57,54,51,48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3)

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
log_dir <- "~/ski/elo/python/ski/polars/excel365/tds-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "tds_picks_processing.log")))
log_info("Starting Tour de Ski predictions process")

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

# Read TdS startlists
log_info("Reading Tour de Ski startlists...")
men_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_tds_men.csv", 
                         stringsAsFactors = FALSE)
men_startlist$Sex = "M"

ladies_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_tds_ladies.csv", 
                            stringsAsFactors = FALSE)
ladies_startlist$Sex = "L"

log_info(paste("Loaded", nrow(men_startlist), "men in TdS startlist"))
log_info(paste("Loaded", nrow(ladies_startlist), "ladies in TdS startlist"))

# Create combined chronological data with proper points assignment
log_info("Assigning points based on race type...")

# Apply points logic for men
men_df <- men_chrono %>%
  mutate(Points = case_when(
    City == "Tour de Ski" ~ map_int(Place, ~ get_points(.x, tds_points)),
    TRUE ~ map_int(Place, ~ get_points(.x, wc_points))
  )) %>%
  arrange(Date, Race, Place)



# Apply points logic for ladies
ladies_df <- ladies_chrono %>%
  mutate(Points = case_when(
    City == "Tour de Ski" ~ map_int(Place, ~ get_points(.x, tds_points)),
    TRUE ~ map_int(Place, ~ get_points(.x, wc_points))
  )) %>%
  arrange(Date, Race, Place)

log_info(paste("Processed", nrow(men_df), "men's race records with points"))
log_info(paste("Processed", nrow(ladies_df), "ladies' race records with points"))

# Add period information to historical data
log_info("Adding period information to historical data...")

men_df <- men_df %>%
  group_by(Season) %>%
  mutate(Num_Races = max(Race)) %>%
  mutate(Period = case_when(
    Num_Races <= 5 ~ 1,
    Num_Races <= 10 ~ 2,
    Num_Races <= 15 ~ 3,
    Num_Races <= 20 ~ 4,
    Num_Races <= 25 ~ 5,
    TRUE ~ ceiling((Race / (Num_Races / 5)))
  )) %>%
  ungroup()

ladies_df <- ladies_df %>%
  group_by(Season) %>%
  mutate(Num_Races = max(Race)) %>%
  mutate(Period = case_when(
    Num_Races <= 5 ~ 1,
    Num_Races <= 10 ~ 2,
    Num_Races <= 15 ~ 3,
    Num_Races <= 20 ~ 4,
    Num_Races <= 25 ~ 5,
    TRUE ~ ceiling((Race / (Num_Races / 5)))
  )) %>%
  ungroup()

# Read race schedule information for TdS races
log_info("Reading race schedule for Tour de Ski...")
weekends <- read.csv("~/ski/elo/python/ski/polars/excel365/weekends.csv", 
                     stringsAsFactors = FALSE) %>%
  mutate(Date = mdy(Date))

# Filter for TdS races (Period == 2, excluding summary race)
tds_races <- weekends %>%
  filter(Period == 2, City != "Tour de Ski") %>%
  arrange(Race)

log_info(paste("Found", nrow(tds_races), "Tour de Ski races in schedule"))

# Create race information dataframes for men and ladies
men_races <- tds_races %>%
  filter(Sex == "M") %>%
  filter(!Distance %in% c("Rel", "Ts")) %>%
  select(Race, Distance, Technique, MS, Elevation, Period, Pursuit, Final_Climb) %>%
  arrange(Race)


ladies_races <- tds_races %>%
  filter(Sex == "L") %>%
  filter(!Distance %in% c("Rel", "Ts")) %>%
  select(Race, Distance, Technique, MS, Elevation, Period, Pursuit, Final_Climb) %>%
  arrange(Race)

log_info(paste("Men's TdS races:", nrow(men_races)))
log_info(paste("Ladies' TdS races:", nrow(ladies_races)))

# Log race details
log_info("Men's TdS race details:")
for(i in 1:nrow(men_races)) {
  race <- men_races[i,]
  log_info(paste("  Race", race$Race, ":", race$Distance, race$Technique, 
                "MS:", race$MS, "Elevation:", race$Elevation, 
                "Final_Climb:", race$Final_Climb))
}

log_info("Ladies' TdS race details:")
for(i in 1:nrow(ladies_races)) {
  race <- ladies_races[i,]
  log_info(paste("  Race", race$Race, ":", race$Distance, race$Technique, 
                "MS:", race$MS, "Elevation:", race$Elevation, 
                "Final_Climb:", race$Final_Climb))
}

# Verify startlist probability columns match race count
log_info("Verifying startlist probability columns...")
men_prob_cols <- names(men_startlist)[grepl("Race.*_Prob", names(men_startlist))]
ladies_prob_cols <- names(ladies_startlist)[grepl("Race.*_Prob", names(ladies_startlist))]

log_info(paste("Men's startlist has probability columns:", paste(men_prob_cols, collapse=", ")))
log_info(paste("Ladies' startlist has probability columns:", paste(ladies_prob_cols, collapse=", ")))
log_info(paste("Expected columns for", nrow(men_races), "men's races and", nrow(ladies_races), "ladies' races"))

# Data quality checks
log_info("Performing data quality checks...")

# Check for missing IDs in startlists
men_missing_ids <- sum(is.na(men_startlist$ID))
ladies_missing_ids <- sum(is.na(ladies_startlist$ID))
log_info(paste("Missing IDs - Men:", men_missing_ids, "Ladies:", ladies_missing_ids))

# Check chronological data coverage
current_season <- max(men_df$Season, na.rm = TRUE)
log_info(paste("Current season:", current_season))

men_current_season <- men_df %>% filter(Season == current_season)
ladies_current_season <- ladies_df %>% filter(Season == current_season)
log_info(paste("Current season records - Men:", nrow(men_current_season), "Ladies:", nrow(ladies_current_season)))

# Check overlap between startlists and chronological data
men_startlist_ids <- unique(men_startlist$ID[!is.na(men_startlist$ID)])
ladies_startlist_ids <- unique(ladies_startlist$ID[!is.na(ladies_startlist$ID)])

men_chrono_ids <- unique(men_df$ID[!is.na(men_df$ID)])
ladies_chrono_ids <- unique(ladies_df$ID[!is.na(ladies_df$ID)])

men_overlap <- length(intersect(men_startlist_ids, men_chrono_ids))
ladies_overlap <- length(intersect(ladies_startlist_ids, ladies_chrono_ids))

log_info(paste("ID overlap with historical data - Men:", men_overlap, "/", length(men_startlist_ids), 
               "Ladies:", ladies_overlap, "/", length(ladies_startlist_ids)))

log_info("=== DATA GATHERING COMPLETE ===")
log_info(paste("Ready to proceed with", nrow(men_races) + nrow(ladies_races), "total TdS races"))
log_info(paste("Startlist sizes - Men:", nrow(men_startlist), "Ladies:", nrow(ladies_startlist)))
log_info(paste("Historical data - Men:", nrow(men_df), "records, Ladies:", nrow(ladies_df), "records"))

# ============================================================================
# POINTS TRAINING SETUP
# ============================================================================

log_info("=== POINTS TRAINING SETUP ===")

# Function to create rolling features for TdS prediction
# Based on TdS-predictions.Rmd methodology
process_dataframe_for_points <- function(df) {
  log_info("Creating rolling weighted features for points prediction")
  
  df %>%
    group_by(ID) %>%
    arrange(Date) %>%
    mutate(
      # Sprint Classic - Last 5 weighted average (excluding current race)
      Sprint_C_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        all_prev_tech <- Technique[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist == "Sprint" & all_prev_tech == "C")
        filtered_points <- all_prev_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      }),
      
      # Sprint Classic - Last 5 weighted average (including current race)
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
      
      # Distance Classic
      Distance_C_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        all_prev_tech <- Technique[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist != "Sprint" & all_prev_tech == "C")
        filtered_points <- all_prev_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      }),
      
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
      
      # Sprint Freestyle
      Sprint_F_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        all_prev_tech <- Technique[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist == "Sprint" & all_prev_tech == "F")
        filtered_points <- all_prev_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      }),
      
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
      
      # Distance Freestyle
      Distance_F_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        all_prev_tech <- Technique[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist != "Sprint" & all_prev_tech == "F")
        filtered_points <- all_prev_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      }),
      
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
      
      # General distance and sprint categories (any technique)
      Distance_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist != "Sprint")
        filtered_points <- all_prev_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      }),
      
      Sprint_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        
        all_prev_races <- Points[1:(i-1)]
        all_prev_dist <- Distance[1:(i-1)]
        
        filtered_idx <- which(all_prev_dist == "Sprint")
        filtered_points <- all_prev_races[filtered_idx]
        
        if(length(filtered_points) > 0) {
          recent_points <- tail(filtered_points, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      }),
      
      # Overall last 5 races (any distance/technique)
      Overall_Last_5 = sapply(row_number(), function(i) {
        if(i == 1) return(NA_real_)
        
        all_prev_races <- Points[1:(i-1)]
        
        if(length(all_prev_races) > 0) {
          recent_points <- tail(all_prev_races, 5)
          calc_weighted_last_5(recent_points)
        } else {
          NA_real_
        }
      })
    ) %>%
    ungroup()
}

# Create training datasets for both genders
log_info("Processing men's data for points prediction...")
men_points_training <- process_dataframe_for_points(men_df)

log_info("Processing ladies' data for points prediction...")
ladies_points_training <- process_dataframe_for_points(ladies_df)

# Function to impute NAs in features with first quartile by Season/Race
impute_features <- function(df) {
  log_info("Imputing NAs in features with first quartile values by Season/Race")
  
  # Define the columns to impute
  weighted_cols <- c("Sprint_C_Last_5", "Sprint_C_Last_5_2", "Distance_C_Last_5", "Distance_C_Last_5_2",
                     "Sprint_F_Last_5", "Sprint_F_Last_5_2", "Distance_F_Last_5", "Distance_F_Last_5_2",
                     "Distance_Last_5", "Sprint_Last_5", "Overall_Last_5")
  
  elo_cols <- c("Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo", 
                "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", 
                "Classic_Elo", "Freestyle_Elo")
  
  pelo_cols <- c("Pelo", "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
                 "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo",
                 "Classic_Pelo", "Freestyle_Pelo")
  
  # Combine all columns to impute
  all_cols_to_impute <- c(weighted_cols, elo_cols, pelo_cols)
  
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

# Add Distance_Last_5_2 and Sprint_Last_5_2 before filtering to 2007+
log_info("Adding Distance_Last_5_2 and Sprint_Last_5_2 using all historical data...")

add_basic_last_5_2_features <- function(df) {
  log_info("Creating Distance_Last_5_2 and Sprint_Last_5_2 features")
  
  df %>%
    group_by(ID) %>%
    arrange(Date) %>%
    mutate(
      # Distance_Last_5_2: Includes current row
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
      
      # Sprint_Last_5_2: Includes current row
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

men_points_training <- add_basic_last_5_2_features(men_points_training)
ladies_points_training <- add_basic_last_5_2_features(ladies_points_training)

# Prepare training data specifically for Tour de Ski prediction
log_info("Preparing training data for Tour de Ski prediction...")

# Add Final Climb identification
log_info("Identifying Final Climb races...")
men_points_training <- men_points_training %>%
  mutate(
    Final_Climb = case_when(
      Event == "Tour de Ski" & 
      City == "Val Di Fiemme" & 
      Distance != "Sprint" & 
      Technique == "F" ~ 1,
      TRUE ~ 0
    ),
    # Add altitude categories
    AltitudeCategory = ifelse(Elevation >= 1300, 1, 0)
  )

ladies_points_training <- ladies_points_training %>%
  mutate(
    Final_Climb = case_when(
      Event == "Tour de Ski" & 
      City == "Val Di Fiemme" & 
      Distance != "Sprint" & 
      Technique == "F" ~ 1,
      TRUE ~ 0
    ),
    # Add altitude categories
    AltitudeCategory = ifelse(Elevation >= 1300, 1, 0)
  )

# Filter for Tour de Ski era (2007 onwards) and quality data
log_info("Filtering for Tour de Ski era data (2007 onwards)...")
men_points_training <- men_points_training %>%
  filter(Season >= 2007,
         !is.na(Points))  # Include all finishers, not just point scorers

ladies_points_training <- ladies_points_training %>%
  filter(Season >= 2007,
         !is.na(Points))

# Create Elo_Pct and Pelo_Pct columns (percentage of max for each Season/Race)
log_info("Creating Elo_Pct and Pelo_Pct columns...")

# Function to create percentage columns
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

# Apply to both datasets
log_info("Processing men's percentage columns...")
men_points_training <- create_percentage_columns(men_points_training)

log_info("Processing ladies' percentage columns...")
ladies_points_training <- create_percentage_columns(ladies_points_training)

log_info(paste("Men's TdS training data:", nrow(men_points_training), "race results"))
log_info(paste("Ladies' TdS training data:", nrow(ladies_points_training), "race results"))

# Log Final Climb identification statistics
men_fc_count <- sum(men_points_training$Final_Climb == 1)
ladies_fc_count <- sum(ladies_points_training$Final_Climb == 1)
log_info(paste("Final Climb races identified - Men:", men_fc_count, "Ladies:", ladies_fc_count))

# Create Final Climb specific training datasets
log_info("Creating Final Climb training datasets...")

men_fc_training <- men_points_training %>%
  filter(Final_Climb == 1)

ladies_fc_training <- ladies_points_training %>%
  filter(Final_Climb == 1)

log_info(paste("Men's Final Climb training data:", nrow(men_fc_training), "race results"))
log_info(paste("Ladies' Final Climb training data:", nrow(ladies_fc_training), "race results"))

# Verify Final Climb data coverage
log_info("Final Climb data coverage by season:")
men_fc_seasons <- men_fc_training %>% 
  group_by(Season) %>% 
  summarise(races = n(), athletes = n_distinct(ID)) %>%
  arrange(Season)

ladies_fc_seasons <- ladies_fc_training %>% 
  group_by(Season) %>% 
  summarise(races = n(), athletes = n_distinct(ID)) %>%
  arrange(Season)

log_info("Men's Final Climb by season:")
for(i in 1:nrow(men_fc_seasons)) {
  season_data <- men_fc_seasons[i,]
  log_info(paste("  Season", season_data$Season, ":", season_data$races, "results from", season_data$athletes, "athletes"))
}

log_info("Ladies' Final Climb by season:")
for(i in 1:nrow(ladies_fc_seasons)) {
  season_data <- ladies_fc_seasons[i,]
  log_info(paste("  Season", season_data$Season, ":", season_data$races, "results from", season_data$athletes, "athletes"))
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

# Data quality checks for training data
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
        if(i == 1) return(NA_real_)
        
        # Get all previous Final Climb races for this skier
        prev_fc_indices <- which(Final_Climb[1:(i-1)] == 1)
        prev_fc_points <- Points[prev_fc_indices]
        
        # Take up to last 3 actual FC results
        actual_fc <- tail(prev_fc_points, 3)
        current_fc_pred <- FC_pred[i]
        
        # If no FC_pred available, return NA
        if(is.na(current_fc_pred)) return(NA_real_)
        
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
          # Use all predicted
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
# FINAL TDS TRAINING DATASET CREATION
# ============================================================================

log_info("=== CREATING FINAL TDS TRAINING DATASET ===")

# Filter to Tour de Ski races only and adjust points to TdS points system
create_final_tds_dataset <- function(df, gender) {
  log_info(paste("Creating final TdS training dataset for", gender))
  
  # Filter to Tour de Ski races only
  tds_df <- df %>%
    filter(City == "Tour De Ski")
  
  log_info(paste("Filtered to", nrow(tds_df), "Tour de Ski race results for", gender))
  
  if(nrow(tds_df) == 0) {
    log_info(paste("No Tour de Ski races found for", gender))
    return(tds_df)
  }
  
  # Convert points from WC points to TdS points
  tds_df <- tds_df %>%
    mutate(
      Points = map_int(Place, ~ get_points(.x, tds_points))
    )
  
  log_info(paste("Converted", gender, "points to TdS points system"))
  log_info(paste("Final", gender, "TdS training dataset:", nrow(tds_df), "race results"))
  
  # Log some statistics
  seasons_covered <- tds_df %>% 
    summarise(
      min_season = min(Season, na.rm = TRUE),
      max_season = max(Season, na.rm = TRUE),
      unique_seasons = n_distinct(Season),
      unique_athletes = n_distinct(ID),
      races_per_season = round(nrow(tds_df) / n_distinct(Season), 1)
    )
  
  log_info(paste("  Seasons covered:", seasons_covered$min_season, "-", seasons_covered$max_season))
  log_info(paste("  Unique seasons:", seasons_covered$unique_seasons))
  log_info(paste("  Unique athletes:", seasons_covered$unique_athletes))
  log_info(paste("  Average races per season:", seasons_covered$races_per_season))
  
  # Log Final Climb statistics
  fc_stats <- tds_df %>%
    summarise(
      total_fc_races = sum(Final_Climb == 1, na.rm = TRUE),
      fc_seasons = n_distinct(Season[Final_Climb == 1]),
      fc_athletes = n_distinct(ID[Final_Climb == 1])
    )
  
  log_info(paste("  Final Climb races:", fc_stats$total_fc_races))
  log_info(paste("  Final Climb seasons:", fc_stats$fc_seasons))
  log_info(paste("  Final Climb athletes:", fc_stats$fc_athletes))
  
  return(tds_df)
}

# Create final TdS training datasets
men_tds_training <- create_final_tds_dataset(men_points_training, "men")

ladies_tds_training <- create_final_tds_dataset(ladies_points_training, "ladies")

log_info("=== FINAL TDS TRAINING DATASET CREATION COMPLETE ===")

# ============================================================================
# TDS_PRED FEATURE SELECTION AND MODELING
# ============================================================================

log_info("=== TDS_PRED FEATURE SELECTION AND MODELING ===")

# Define TDS_pred features for feature selection
tds_pred_features <- c("Pelo_Pct", "Distance_C_Pelo_Pct", "Distance_F_Pelo_Pct", 
                       "Sprint_C_Pelo_Pct", "Sprint_F_Pelo_Pct", "Distance_Last_5", 
                       "Sprint_Last_5", "FC_Last_5")

log_info(paste("TDS_pred candidate features:", paste(tds_pred_features, collapse=", ")))

# Function to perform feature selection for TDS_pred
tds_feature_selection <- function(training_data, gender) {
  log_info(paste("Performing TDS_pred feature selection for", gender))
  
  # Check feature availability
  available_features <- tds_pred_features[tds_pred_features %in% names(training_data)]
  missing_features <- setdiff(tds_pred_features, available_features)
  
  if(length(missing_features) > 0) {
    log_info(paste("Missing features for", gender, ":", paste(missing_features, collapse=", ")))
  }
  
  if(length(available_features) == 0) {
    log_info(paste("No features available for", gender, "TDS_pred model"))
    return(NULL)
  }
  
  log_info(paste("Available features for", gender, ":", paste(available_features, collapse=", ")))
  
  # Remove rows with missing data in key features
  complete_data <- training_data %>%
    filter(complete.cases(.[available_features]) & !is.na(Points))
  
  log_info(paste("Complete cases for", gender, ":", nrow(complete_data), "of", nrow(training_data), "total"))
  
  if(nrow(complete_data) < 20) {
    log_info(paste("Insufficient data for", gender, "TDS_pred model"))
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
  log_info(paste("Best TDS_pred model for", gender, "with", length(selected_vars), "variables"))
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
men_tds_selection <- tds_feature_selection(men_tds_training, "men")
ladies_tds_selection <- tds_feature_selection(ladies_tds_training, "ladies")


# TDS_pred GAM modeling based on selected features
log_info("Training TDS_pred GAM models...")

tds_pred_models <- list()

# Train GAM models for both genders if feature selection was successful
if(!is.null(men_tds_selection)) {
  log_info("Training men's TDS_pred GAM model...")
  
  # Create formula using selected features
  men_tds_formula <- men_tds_selection$formula
  log_info(paste("Men's TDS_pred formula:", paste(deparse(men_tds_formula), collapse = " ")))
  
  # Train GAM model
  men_tds_model <- tryCatch({
    gam(men_tds_formula, data = men_tds_selection$training_data, method = "REML")
  }, error = function(e) {
    log_info(paste("Error training men's TDS_pred model:", e$message))
    return(NULL)
  })
  
  if(!is.null(men_tds_model)) {
    tds_pred_models$men <- men_tds_model
    log_info("Men's TDS_pred GAM model trained successfully")
    log_info(paste("Model summary - AIC:", AIC(men_tds_model), "R-squared:", summary(men_tds_model)$r.sq))
  }
}

if(!is.null(ladies_tds_selection)) {
  log_info("Training ladies' TDS_pred GAM model...")
  
  # Create formula using selected features
  ladies_tds_formula <- ladies_tds_selection$formula
  log_info(paste("Ladies' TDS_pred formula:", paste(deparse(ladies_tds_formula), collapse = " ")))
  
  # Train GAM model
  ladies_tds_model <- tryCatch({
    gam(ladies_tds_formula, data = ladies_tds_selection$training_data, method = "REML")
  }, error = function(e) {
    log_info(paste("Error training ladies' TDS_pred model:", e$message))
    return(NULL)
  })
  
  if(!is.null(ladies_tds_model)) {
    tds_pred_models$ladies <- ladies_tds_model
    log_info("Ladies' TDS_pred GAM model trained successfully")
    log_info(paste("Model summary - AIC:", AIC(ladies_tds_model), "R-squared:", summary(ladies_tds_model)$r.sq))
  }
}

# Function to create TDS_pred column for training data
create_tds_pred_column <- function(df, model, gender) {
  log_info(paste("Creating TDS_pred column for", gender))
  
  if(is.null(model)) {
    log_info(paste("No TDS_pred model available for", gender, "- setting TDS_pred to NA"))
    df$TDS_pred <- NA_real_
    return(df)
  }
  
  # Create predictions for all rows (will be NA where features are missing)
  df$TDS_pred <- tryCatch({
    predict(model, newdata = df, type = "response")
  }, error = function(e) {
    log_info(paste("Error creating TDS_pred for", gender, ":", e$message))
    return(rep(NA_real_, nrow(df)))
  })
  
  # Log statistics
  tds_pred_stats <- df %>%
    filter(!is.na(TDS_pred)) %>%
    summarise(
      count = n(),
      mean = mean(TDS_pred, na.rm = TRUE),
      median = median(TDS_pred, na.rm = TRUE),
      min = min(TDS_pred, na.rm = TRUE),
      max = max(TDS_pred, na.rm = TRUE)
    )
  
  log_info(paste("TDS_pred stats for", gender, ":"))
  log_info(paste("  Valid predictions:", tds_pred_stats$count, "of", nrow(df)))
  log_info(paste("  Mean:", round(tds_pred_stats$mean, 2)))
  log_info(paste("  Range:", round(tds_pred_stats$min, 2), "-", round(tds_pred_stats$max, 2)))
  
  return(df)
}

# Add TDS_pred column to TDS training datasets
log_info("Adding TDS_pred column to TDS training datasets...")

men_tds_training <- create_tds_pred_column(men_tds_training, tds_pred_models$men, "men")

ladies_tds_training <- create_tds_pred_column(ladies_tds_training, tds_pred_models$ladies, "ladies")

log_info("=== TDS_PRED FEATURE SELECTION AND MODELING COMPLETE ===")

# ============================================================================
# TDS_LAST_5 AND TDS_LAST_5_2 FEATURE CREATION
# ============================================================================

log_info("=== CREATING TDS_LAST_5 AND TDS_LAST_5_2 FEATURES ===")

# Function to create TDS_Last_5 and TDS_Last_5_2 features
create_tds_weighted_features <- function(df) {
  log_info("Creating TDS_Last_5 and TDS_Last_5_2 weighted features")
  
  df %>%
    group_by(ID) %>%
    arrange(Date) %>%
    mutate(
      # TDS_Last_5: Excludes current row, uses up to last 3 actual TdS + predicted fill
      TDS_Last_5 = sapply(row_number(), function(i) {
        # Get all previous Tour de Ski races for this skier
        if(i == 1) {
          all_prev_races <- numeric(0)
          all_prev_city <- character(0)
        } else {
          all_prev_races <- Points[1:(i-1)]
          all_prev_city <- City[1:(i-1)]
        }
        
        filtered_idx <- which(all_prev_city == "Tour De Ski")
        filtered_points <- all_prev_races[filtered_idx]
        
        # Take up to last 3 actual TdS results
        actual_tds <- tail(filtered_points, 3)
        current_tds_pred <- TDS_pred[i]
        
        # If no TDS_pred available, return NA
        if(is.na(current_tds_pred)) return(NA_real_)
        
        # Create the 5-value weighted average
        # Order: most recent gets weight 5, then 4, 3, 2, 1
        if(length(actual_tds) >= 3) {
          # Use last 3 actual + 2 predicted
          values <- c(actual_tds[3], actual_tds[2], actual_tds[1], current_tds_pred, current_tds_pred)
        } else if(length(actual_tds) == 2) {
          # Use 2 actual + 3 predicted
          values <- c(actual_tds[2], actual_tds[1], current_tds_pred, current_tds_pred, current_tds_pred)
        } else if(length(actual_tds) == 1) {
          # Use 1 actual + 4 predicted  
          values <- c(actual_tds[1], current_tds_pred, current_tds_pred, current_tds_pred, current_tds_pred)
        } else {
          # Use all predicted
          values <- rep(current_tds_pred, 5)
        }
        
        # Apply weights 5, 4, 3, 2, 1 and calculate weighted average
        weights <- c(5, 4, 3, 2, 1)
        weighted.mean(values, weights, na.rm = TRUE)
      }),
      
      # TDS_Last_5_2: Includes current row, uses current + up to last 2 actual TdS + predicted fill
      TDS_Last_5_2 = sapply(row_number(), function(i) {
        current_tds_pred <- TDS_pred[i]
        
        # If no TDS_pred available, return NA
        if(is.na(current_tds_pred)) return(NA_real_)
        
        # If current row is Tour De Ski, use actual points, otherwise use predicted value
        if(City[i] == "Tour De Ski") {
          current_points <- Points[i]
        } else {
          current_points <- current_tds_pred
        }
        
        # Get previous Tour de Ski races for this skier
        if(i == 1) {
          all_prev_races <- numeric(0)
          all_prev_city <- character(0)
        } else {
          all_prev_races <- Points[1:(i-1)]
          all_prev_city <- City[1:(i-1)]
        }
        
        filtered_idx <- which(all_prev_city == "Tour De Ski")
        filtered_points <- all_prev_races[filtered_idx]
        
        # Take up to last 2 actual TdS results
        actual_tds <- tail(filtered_points, 2)
        
        # Create the 5-value weighted average
        # Order: most recent gets weight 5, then 4, 3, 2, 1
        if(length(actual_tds) >= 2) {
          # Use current + last 2 actual + 2 predicted
          values <- c(current_points, actual_tds[2], actual_tds[1], current_tds_pred, current_tds_pred)
        } else if(length(actual_tds) == 1) {
          # Use current + 1 actual + 3 predicted
          values <- c(current_points, actual_tds[1], current_tds_pred, current_tds_pred, current_tds_pred)
        } else {
          # Use current + 4 predicted
          values <- c(current_points, current_tds_pred, current_tds_pred, current_tds_pred, current_tds_pred)
        }
        
        # Apply weights 5, 4, 3, 2, 1 and calculate weighted average
        weights <- c(5, 4, 3, 2, 1)
        weighted.mean(values, weights, na.rm = TRUE)
      })
    ) %>%
    ungroup()
}

# Apply TDS weighted features to both datasets
log_info("Processing men's TDS weighted features...")
men_tds_training <- create_tds_weighted_features(men_tds_training)

log_info("Processing ladies' TDS weighted features...")
ladies_tds_training <- create_tds_weighted_features(ladies_tds_training)

# Log statistics for the new TDS features
log_tds_stats <- function(df, gender) {
  tds_last_5_stats <- df %>%
    filter(!is.na(TDS_Last_5)) %>%
    summarise(
      count = n(),
      mean = mean(TDS_Last_5, na.rm = TRUE),
      median = median(TDS_Last_5, na.rm = TRUE),
      min = min(TDS_Last_5, na.rm = TRUE),
      max = max(TDS_Last_5, na.rm = TRUE)
    )
  
  tds_last_5_2_stats <- df %>%
    filter(!is.na(TDS_Last_5_2)) %>%
    summarise(
      count = n(),
      mean = mean(TDS_Last_5_2, na.rm = TRUE),
      median = median(TDS_Last_5_2, na.rm = TRUE),
      min = min(TDS_Last_5_2, na.rm = TRUE),
      max = max(TDS_Last_5_2, na.rm = TRUE)
    )
  
  log_info(paste("TDS_Last_5 stats for", gender, ":"))
  log_info(paste("  Valid values:", tds_last_5_stats$count, "of", nrow(df)))
  log_info(paste("  Mean:", round(tds_last_5_stats$mean, 2)))
  log_info(paste("  Range:", round(tds_last_5_stats$min, 2), "-", round(tds_last_5_stats$max, 2)))
  
  log_info(paste("TDS_Last_5_2 stats for", gender, ":"))
  log_info(paste("  Valid values:", tds_last_5_2_stats$count, "of", nrow(df)))
  log_info(paste("  Mean:", round(tds_last_5_2_stats$mean, 2)))
  log_info(paste("  Range:", round(tds_last_5_2_stats$min, 2), "-", round(tds_last_5_2_stats$max, 2)))
}

log_tds_stats(men_tds_training, "men")
log_tds_stats(ladies_tds_training, "ladies")

log_info("=== TDS_LAST_5 AND TDS_LAST_5_2 FEATURE CREATION COMPLETE ===")

# ============================================================================
# FINAL TDS FEATURE SELECTION AND MODELING
# ============================================================================

log_info("=== FINAL TDS FEATURE SELECTION AND MODELING ===")

# Define final TDS features for feature selection
final_tds_features <- c("Pelo_Pct", "Distance_C_Pelo_Pct", "Distance_F_Pelo_Pct", 
                        "Sprint_C_Pelo_Pct", "Sprint_F_Pelo_Pct", "Distance_Last_5", 
                        "Sprint_Last_5", "FC_Last_5", "TDS_Last_5")

log_info(paste("Final TDS candidate features:", paste(final_tds_features, collapse=", ")))

# Function to perform final feature selection for TDS prediction
final_tds_feature_selection <- function(training_data, gender) {
  log_info(paste("Performing final TDS feature selection for", gender))
  
  # Check feature availability
  available_features <- final_tds_features[final_tds_features %in% names(training_data)]
  missing_features <- setdiff(final_tds_features, available_features)
  
  if(length(missing_features) > 0) {
    log_info(paste("Missing features for", gender, ":", paste(missing_features, collapse=", ")))
  }
  
  if(length(available_features) == 0) {
    log_info(paste("No features available for", gender, "final TDS model"))
    return(NULL)
  }
  
  log_info(paste("Available features for", gender, ":", paste(available_features, collapse=", ")))
  
  # Remove rows with missing data in key features
  complete_data <- training_data %>%
    filter(complete.cases(.[available_features]) & !is.na(Points))
  
  log_info(paste("Complete cases for", gender, ":", nrow(complete_data), "of", nrow(training_data), "total"))
  
  if(nrow(complete_data) < 20) {
    log_info(paste("Insufficient data for", gender, "final TDS model"))
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
  log_info(paste("Best final TDS model for", gender, "with", length(selected_vars), "variables"))
  log_info(paste("Selected variables:", paste(selected_vars, collapse=", ")))
  log_info(paste("BIC score:", min(bic_scores)))
  
  return(list(
    selected_vars = selected_vars,
    training_data = complete_data,
    bic_score = min(bic_scores),
    formula = as.formula(paste("Points ~", paste(selected_vars, collapse = " + ")))
  ))
}

# Perform final feature selection for both genders
men_final_tds_selection <- final_tds_feature_selection(men_tds_training, "men")

ladies_final_tds_selection <- final_tds_feature_selection(ladies_tds_training, "ladies")

# Final TDS GAM modeling based on selected features
log_info("Training final TDS GAM models...")

final_tds_models <- list()

# Train GAM models for both genders if feature selection was successful
if(!is.null(men_final_tds_selection)) {
  log_info("Training men's final TDS GAM model...")
  
  # Create formula using selected features
  men_final_tds_formula <- men_final_tds_selection$formula
  log_info(paste("Men's final TDS formula:", paste(deparse(men_final_tds_formula), collapse = " ")))
  
  # Train GAM model
  men_final_tds_model <- tryCatch({
    gam(men_final_tds_formula, data = men_final_tds_selection$training_data, method = "REML")
  }, error = function(e) {
    log_info(paste("Error training men's final TDS model:", e$message))
    return(NULL)
  })
  
  if(!is.null(men_final_tds_model)) {
    final_tds_models$men <- men_final_tds_model
    log_info("Men's final TDS GAM model trained successfully")
    log_info(paste("Model summary - AIC:", AIC(men_final_tds_model), "R-squared:", summary(men_final_tds_model)$r.sq))
  }
}

if(!is.null(ladies_final_tds_selection)) {
  log_info("Training ladies' final TDS GAM model...")
  
  # Create formula using selected features
  ladies_final_tds_formula <- ladies_final_tds_selection$formula
  log_info(paste("Ladies' final TDS formula:", paste(deparse(ladies_final_tds_formula), collapse = " ")))
  
  # Train GAM model
  ladies_final_tds_model <- tryCatch({
    gam(ladies_final_tds_formula, data = ladies_final_tds_selection$training_data, method = "REML")
  }, error = function(e) {
    log_info(paste("Error training ladies' final TDS model:", e$message))
    return(NULL)
  })
  
  if(!is.null(ladies_final_tds_model)) {
    final_tds_models$ladies <- ladies_final_tds_model
    log_info("Ladies' final TDS GAM model trained successfully")
    log_info(paste("Model summary - AIC:", AIC(ladies_final_tds_model), "R-squared:", summary(ladies_final_tds_model)$r.sq))
  }
}

# Function to create final TDS predictions
create_final_tds_predictions <- function(df, model, gender) {
  log_info(paste("Creating final TDS predictions for", gender))
  
  if(is.null(model)) {
    log_info(paste("No final TDS model available for", gender, "- setting Final_TDS_pred to NA"))
    df$Final_TDS_pred <- NA_real_
    return(df)
  }
  
  # Create predictions for all rows (will be NA where features are missing)
  df$Final_TDS_pred <- tryCatch({
    predict(model, newdata = df, type = "response")
  }, error = function(e) {
    log_info(paste("Error creating final TDS predictions for", gender, ":", e$message))
    return(rep(NA_real_, nrow(df)))
  })
  
  # Log statistics
  final_tds_pred_stats <- df %>%
    filter(!is.na(Final_TDS_pred)) %>%
    summarise(
      count = n(),
      mean = mean(Final_TDS_pred, na.rm = TRUE),
      median = median(Final_TDS_pred, na.rm = TRUE),
      min = min(Final_TDS_pred, na.rm = TRUE),
      max = max(Final_TDS_pred, na.rm = TRUE)
    )
  
  log_info(paste("Final TDS predictions stats for", gender, ":"))
  log_info(paste("  Valid predictions:", final_tds_pred_stats$count, "of", nrow(df)))
  log_info(paste("  Mean:", round(final_tds_pred_stats$mean, 2)))
  log_info(paste("  Range:", round(final_tds_pred_stats$min, 2), "-", round(final_tds_pred_stats$max, 2)))
  
  return(df)
}

# Add final TDS predictions to TDS training datasets
log_info("Adding final TDS predictions to training datasets...")

men_tds_training <- create_final_tds_predictions(men_tds_training, final_tds_models$men, "men")
ladies_tds_training <- create_final_tds_predictions(ladies_tds_training, final_tds_models$ladies, "ladies")



log_info("=== FINAL TDS FEATURE SELECTION AND MODELING COMPLETE ===")

# ============================================================================
# STARTLIST DATA PROCESSING
# ============================================================================

log_info("=== PROCESSING STARTLIST DATA ===")

# Function to create Pelo_Pct columns from Elo columns for startlist data
create_startlist_pelo_pct <- function(df, gender) {
  log_info(paste("Creating Pelo_Pct columns for", gender, "startlist"))
  
  # Define Elo columns that need to be converted to Pelo_Pct
  elo_cols <- c("Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo", 
                "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", 
                "Classic_Elo", "Freestyle_Elo")
  
  # Create corresponding Pelo_Pct column names
  pelo_pct_cols <- gsub("Elo", "Pelo_Pct", elo_cols)
  
  # Process each Elo column
  for(i in 1:length(elo_cols)) {
    elo_col <- elo_cols[i]
    pct_col <- pelo_pct_cols[i]
    
    if(elo_col %in% names(df)) {
      log_info(paste("Converting", elo_col, "to", pct_col))
      
      # Calculate percentage relative to maximum value in the startlist
      max_val <- max(df[[elo_col]], na.rm = TRUE)
      
      if(max_val > 0) {
        df[[pct_col]] <- ifelse(!is.na(df[[elo_col]]), 
                               df[[elo_col]] / max_val, 
                               NA_real_)
      } else {
        df[[pct_col]] <- NA_real_
      }
      
      # Log statistics
      valid_count <- sum(!is.na(df[[pct_col]]))
      log_info(paste("  ", pct_col, "- Valid values:", valid_count, "of", nrow(df), 
                    "Range:", round(min(df[[pct_col]], na.rm = TRUE), 3), "-", 
                    round(max(df[[pct_col]], na.rm = TRUE), 3)))
    } else {
      log_info(paste("Column", elo_col, "not found in", gender, "startlist"))
      df[[pct_col]] <- NA_real_
    }
  }
  
  return(df)
}

# Process both startlists
log_info("Processing men's startlist...")
men_startlist <- create_startlist_pelo_pct(men_startlist, "men")
log_info("Processing ladies' startlist...")
ladies_startlist <- create_startlist_pelo_pct(ladies_startlist, "ladies")
save_men_startlist = men_startlist
save_ladies_startlist = ladies_startlist
# Function to get all latest Last_5_2 features from training data
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

# Function to calculate FC_pred for startlist using trained model
calculate_startlist_fc_pred <- function(df, fc_model, gender) {
  log_info(paste("Calculating FC_pred for", gender, "startlist"))
  
  # Calculate FC_pred if model available
  if(!is.null(fc_model)) {
    df$FC_pred <- tryCatch({
      predict(fc_model, newdata = df, type = "response")
    }, error = function(e) {
      log_info(paste("Error calculating FC_pred for", gender, ":", e$message))
      return(rep(NA_real_, nrow(df)))
    })
    
    fc_count <- sum(!is.na(df$FC_pred))
    log_info(paste("  FC_pred - Valid predictions:", fc_count, "of", nrow(df)))
  } else {
    log_info(paste("  No FC model available for", gender))
    df$FC_pred <- NA_real_
  }
  
  return(df)
}

# Function to calculate TDS_pred for startlist using trained model
calculate_startlist_tds_pred <- function(df, tds_model, gender) {
  log_info(paste("Calculating TDS_pred for", gender, "startlist"))
  
  # Calculate TDS_pred if model available
  if(!is.null(tds_model)) {
    df$TDS_pred <- tryCatch({
      predict(tds_model, newdata = df, type = "response")
    }, error = function(e) {
      log_info(paste("Error calculating TDS_pred for", gender, ":", e$message))
      return(rep(NA_real_, nrow(df)))
    })
    
    tds_count <- sum(!is.na(df$TDS_pred))
    log_info(paste("  TDS_pred - Valid predictions:", tds_count, "of", nrow(df)))
  } else {
    log_info(paste("  No TDS model available for", gender))
    df$TDS_pred <- NA_real_
  }
  
  return(df)
}

# Add missing prediction features with default NA values
add_missing_prediction_features <- function(df, gender) {
  log_info(paste("Adding any remaining missing features for", gender, "startlist"))
  
  # Features that might still be missing
  prediction_features <- c("Distance_Last_5", "Sprint_Last_5", "FC_Last_5", "TDS_Last_5")
  
  for(feature in prediction_features) {
    if(!feature %in% names(df)) {
      log_info(paste("Adding missing feature:", feature, "- setting to NA"))
      df[[feature]] <- NA_real_
    }
  }
  
  return(df)
}

# Function to calculate FC_Last_5 for startlist athletes
calculate_startlist_fc_last5 <- function(startlist_df, training_df, gender) {
  log_info(paste("Calculating FC_Last_5 for", gender, "startlist"))
  
  # Create enhanced startlist with historical data for calculations
  startlist_enhanced <- startlist_df
  
  # Calculate FC_Last_5 for each athlete
  for(i in 1:nrow(startlist_enhanced)) {
    athlete_id <- startlist_enhanced$ID[i]
    fc_pred <- startlist_enhanced$FC_pred[i]
    
    if(!is.na(athlete_id)) {
      # Get athlete's historical data
      athlete_history <- training_df %>%
        filter(ID == athlete_id) %>%
        arrange(Date)
      
      if(nrow(athlete_history) > 0) {
        # Calculate FC_Last_5 (up to 3 actual FC + predicted fill)
        if(!is.na(fc_pred)) {
          fc_history <- athlete_history$Points[athlete_history$Final_Climb == 1]
          actual_fc <- tail(fc_history, 3)
          
          if(length(actual_fc) >= 3) {
            values <- c(actual_fc[3], actual_fc[2], actual_fc[1], fc_pred, fc_pred)
          } else if(length(actual_fc) == 2) {
            values <- c(actual_fc[2], actual_fc[1], fc_pred, fc_pred, fc_pred)
          } else if(length(actual_fc) == 1) {
            values <- c(actual_fc[1], fc_pred, fc_pred, fc_pred, fc_pred)
          } else {
            values <- rep(fc_pred, 5)
          }
          
          weights <- c(5, 4, 3, 2, 1)
          startlist_enhanced$FC_Last_5[i] <- weighted.mean(values, weights, na.rm = TRUE)
        }
      }
    }
  }
  
  # Log statistics
  fc_count <- sum(!is.na(startlist_enhanced$FC_Last_5))
  log_info(paste("  FC_Last_5 - Calculated for:", fc_count, "of", nrow(startlist_enhanced), "athletes"))
  
  return(startlist_enhanced)
}

# Function to calculate TDS_Last_5 for startlist athletes
calculate_startlist_tds_last5 <- function(startlist_df, training_df, gender) {
  log_info(paste("Calculating TDS_Last_5 for", gender, "startlist"))
  
  # Create enhanced startlist with historical data for calculations
  startlist_enhanced <- startlist_df
  
  # Calculate TDS_Last_5 for each athlete
  for(i in 1:nrow(startlist_enhanced)) {
    athlete_id <- startlist_enhanced$ID[i]
    tds_pred <- startlist_enhanced$TDS_pred[i]
    
    if(!is.na(athlete_id)) {
      # Get athlete's historical data
      athlete_history <- training_df %>%
        filter(ID == athlete_id) %>%
        arrange(Date)
      
      if(nrow(athlete_history) > 0) {
        # Calculate TDS_Last_5 (up to 3 actual TdS + predicted fill)
        if(!is.na(tds_pred)) {
          tds_history <- athlete_history$Points[athlete_history$City == "Tour De Ski"]
          actual_tds <- tail(tds_history, 3)
          
          if(length(actual_tds) >= 3) {
            values <- c(actual_tds[3], actual_tds[2], actual_tds[1], tds_pred, tds_pred)
          } else if(length(actual_tds) == 2) {
            values <- c(actual_tds[2], actual_tds[1], tds_pred, tds_pred, tds_pred)
          } else if(length(actual_tds) == 1) {
            values <- c(actual_tds[1], tds_pred, tds_pred, tds_pred, tds_pred)
          } else {
            values <- rep(tds_pred, 5)
          }
          
          weights <- c(5, 4, 3, 2, 1)
          startlist_enhanced$TDS_Last_5[i] <- weighted.mean(values, weights, na.rm = TRUE)
        }
      }
    }
  }
  
  # Log statistics
  tds_count <- sum(!is.na(startlist_enhanced$TDS_Last_5))
  log_info(paste("  TDS_Last_5 - Calculated for:", tds_count, "of", nrow(startlist_enhanced), "athletes"))
  
  return(startlist_enhanced)
}

# PROPER SEQUENCE FOR PREDICTIONS:
# 1. Add basic Last_5 features (needed as inputs for FC prediction models)
men_startlist <- add_latest_basic_last_5_features(men_startlist, men_points_training, "men")

ladies_startlist <- add_latest_basic_last_5_features(ladies_startlist, ladies_points_training, "ladies")

# 2. Calculate FC_pred using basic features
men_startlist <- calculate_startlist_fc_pred(men_startlist, fc_pred_models$men, "men")
ladies_startlist <- calculate_startlist_fc_pred(ladies_startlist, fc_pred_models$ladies, "ladies")

# 3. Calculate FC_Last_5 using FC_pred
men_startlist <- calculate_startlist_fc_last5(men_startlist, men_points_training, "men")
ladies_startlist <- calculate_startlist_fc_last5(ladies_startlist, ladies_points_training, "ladies")

# 4. Calculate TDS_pred using FC_Last_5
men_startlist <- calculate_startlist_tds_pred(men_startlist, tds_pred_models$men, "men")
ladies_startlist <- calculate_startlist_tds_pred(ladies_startlist, tds_pred_models$ladies, "ladies")

# 5. Calculate TDS_Last_5 using TDS_pred
men_points_training_tds <- men_points_training %>% mutate(
  Points = map_int(Place, ~ get_points(.x, tds_points))
)
ladies_points_training_tds <- ladies_points_training %>% mutate(
  Points = map_int(Place, ~ get_points(.x, tds_points))
)
men_startlist <- calculate_startlist_tds_last5(men_startlist, men_points_training_tds, "men")
ladies_startlist <- calculate_startlist_tds_last5(ladies_startlist, ladies_points_training_tds, "ladies")




# Add any remaining missing features
men_startlist <- add_missing_prediction_features(men_startlist, "men")
ladies_startlist <- add_missing_prediction_features(ladies_startlist, "ladies")

# Impute NAs with first quartile for prediction features
log_info("=== IMPUTING MISSING VALUES ===")

impute_missing_values <- function(df, gender) {
  log_info(paste("Imputing missing values for", gender, "startlist using first quartile"))
  
  # Get prediction feature columns (exclude non-numeric and ID columns)
  numeric_cols <- sapply(df, is.numeric)
  exclude_cols <- c("ID", "Price", "Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo", 
                   "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", "Classic_Elo", "Freestyle_Elo")
  
  prediction_features <- names(df)[numeric_cols & !names(df) %in% exclude_cols]
  
  log_info(paste("Found", length(prediction_features), "numeric prediction features to impute"))
  
  for(feature in prediction_features) {
    if(feature %in% names(df)) {
      na_count_before <- sum(is.na(df[[feature]]))
      
      if(na_count_before > 0) {
        df[[feature]] <- replace_na_with_quartile(df[[feature]])
        na_count_after <- sum(is.na(df[[feature]]))
        
        log_info(paste("  ", feature, "- Imputed", na_count_before - na_count_after, "NAs"))
      }
    }
  }
  
  return(df)
}

men_startlist <- impute_missing_values(men_startlist, "men")
ladies_startlist <- impute_missing_values(ladies_startlist, "ladies")

# Log final startlist statistics
log_info(paste("Men's startlist final size:", nrow(men_startlist), "athletes with", ncol(men_startlist), "columns"))
log_info(paste("Ladies' startlist final size:", nrow(ladies_startlist), "athletes with", ncol(ladies_startlist), "columns"))

# Check which prediction features are available
men_pred_features <- intersect(final_tds_features, names(men_startlist))
ladies_pred_features <- intersect(final_tds_features, names(ladies_startlist))

log_info(paste("Men's startlist has", length(men_pred_features), "of", length(final_tds_features), "prediction features"))
log_info(paste("Ladies' startlist has", length(ladies_pred_features), "of", length(final_tds_features), "prediction features"))

# Generate final TdS predictions using the trained final model
log_info("=== GENERATING FINAL TDS PREDICTIONS ===")

generate_final_tds_predictions <- function(df, model, features, gender) {
  log_info(paste("Generating final TdS predictions for", gender, "using", length(features), "features"))
  
  if(is.null(model)) {
    log_info(paste("No final TdS model available for", gender))
    df$TdS_Final_Prediction <- NA_real_
    return(df)
  }
  
  # Check that all required features are available
  missing_features <- setdiff(features, names(df))
  if(length(missing_features) > 0) {
    log_info(paste("Missing features for", gender, "prediction:", paste(missing_features, collapse = ", ")))
    df$TdS_Final_Prediction <- NA_real_
    return(df)
  }
  
  # Generate predictions
  df$TdS_Final_Prediction <- tryCatch({
    predict(model, newdata = df, type = "response")
  }, error = function(e) {
    log_info(paste("Error generating final TdS predictions for", gender, ":", e$message))
    return(rep(NA_real_, nrow(df)))
  })
  
  # Log statistics
  valid_predictions <- sum(!is.na(df$TdS_Final_Prediction))
  log_info(paste("  Generated", valid_predictions, "valid predictions out of", nrow(df), "athletes"))
  
  if(valid_predictions > 0) {
    log_info(paste("  Prediction range:", round(min(df$TdS_Final_Prediction, na.rm = TRUE), 1), 
                   "to", round(max(df$TdS_Final_Prediction, na.rm = TRUE), 1)))
  }
  
  return(df)
}

# Generate final predictions for both genders
men_startlist <- generate_final_tds_predictions(men_startlist, final_tds_models$men, final_tds_features, "men")
ladies_startlist <- generate_final_tds_predictions(ladies_startlist, final_tds_models$ladies, final_tds_features, "ladies")

# ============================================================================
# APPLY FLOORS AND CEILINGS TO PREDICTIONS
# ============================================================================

log_info("=== APPLYING PREDICTION BOUNDS (0-300 POINTS) ===")

# Function to apply realistic bounds to predictions
apply_prediction_bounds <- function(startlist_df, gender, min_points = 0, max_points = 300) {
  log_info(paste("Applying prediction bounds for", gender, ": [", min_points, ",", max_points, "]"))
  
  if("TdS_Final_Prediction" %in% names(startlist_df)) {
    # Count predictions outside bounds before correction
    below_min <- sum(startlist_df$TdS_Final_Prediction < min_points, na.rm = TRUE)
    above_max <- sum(startlist_df$TdS_Final_Prediction > max_points, na.rm = TRUE)
    valid_predictions <- sum(!is.na(startlist_df$TdS_Final_Prediction))
    
    log_info(paste("  Before bounds - Below", min_points, ":", below_min, "| Above", max_points, ":", above_max, "| Valid:", valid_predictions))
    
    # Apply bounds
    startlist_df$TdS_Final_Prediction <- pmax(
      pmin(startlist_df$TdS_Final_Prediction, max_points, na.rm = TRUE), 
      min_points, 
      na.rm = TRUE
    )
    
    # Log statistics after bounds
    if(valid_predictions > 0) {
      final_min <- min(startlist_df$TdS_Final_Prediction, na.rm = TRUE)
      final_max <- max(startlist_df$TdS_Final_Prediction, na.rm = TRUE)
      final_mean <- mean(startlist_df$TdS_Final_Prediction, na.rm = TRUE)
      
      log_info(paste("  After bounds - Min:", round(final_min, 1), "| Max:", round(final_max, 1), "| Mean:", round(final_mean, 1)))
      log_info(paste("  Corrections - Floored to", min_points, ":", below_min, "| Capped to", max_points, ":", above_max))
    }
  } else {
    log_info(paste("No TdS_Final_Prediction column found for", gender))
  }
  
  return(startlist_df)
}

# Apply bounds to both genders
men_startlist <- apply_prediction_bounds(men_startlist, "men", min_points = 0, max_points = 300)
ladies_startlist <- apply_prediction_bounds(ladies_startlist, "ladies", min_points = 0, max_points = 300)

# Save final results
log_info("=== SAVING FINAL RESULTS ===")

# Prepare output directory with today's date
today_date <- format(Sys.time(), tz = "UTC", "%Y%m%d")
output_dir <- paste0("~/blog/daehl-e/content/post/cross-country/drafts/tds-picks/", today_date)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
log_info(paste("Output directory:", output_dir))

# Prepare points output data (for men.xlsx and ladies.xlsx)
prepare_points_output <- function(df, gender) {
  output_df <- df %>%
    select(Skier, ID, Nation, Points = TdS_Final_Prediction) %>%
    filter(!is.na(Points)) %>%
    arrange(desc(Points))
  
  log_info(paste("Prepared", gender, "points output with", nrow(output_df), "athletes"))
  return(output_df)
}

# Prepare position probabilities output (for position_probabilities files)
prepare_probabilities_output <- function(df, gender) {
  # Define probability columns to include
  prob_columns <- paste0("prob_top", position_thresholds)
  
  # Create output with probabilities only
  output_df <- df %>%
    select(Skier, ID, Nation, any_of(prob_columns)) %>%
    filter(!is.na(Skier)) %>%
    arrange(desc(prob_top1))
  
  # Rename probability columns to more readable names
  if("prob_top1" %in% names(output_df)) {
    names(output_df)[names(output_df) == "prob_top1"] <- "Win"
  }
  if("prob_top3" %in% names(output_df)) {
    names(output_df)[names(output_df) == "prob_top3"] <- "Podium"
  }
  if("prob_top5" %in% names(output_df)) {
    names(output_df)[names(output_df) == "prob_top5"] <- "Top 5"
  }
  if("prob_top10" %in% names(output_df)) {
    names(output_df)[names(output_df) == "prob_top10"] <- "Top 10"
  }
  if("prob_top30" %in% names(output_df)) {
    names(output_df)[names(output_df) == "prob_top30"] <- "Top 30"
  }
  
  log_info(paste("Prepared", gender, "probabilities output with", nrow(output_df), "athletes"))
  log_info(paste("  Probability columns:", paste(names(output_df), collapse = ", ")))
  return(output_df)
}

# ============================================================================
# NORMALIZE POINTS PREDICTIONS
# ============================================================================

log_info("=== NORMALIZING POINTS PREDICTIONS ===")

# Function to normalize points predictions to sum to actual TdS total
normalize_points_predictions <- function(startlist_df, gender) {
  log_info(paste("Normalizing points predictions for", gender))
  
  # Filter out athletes with valid predictions
  valid_predictions <- startlist_df %>%
    filter(!is.na(TdS_Final_Prediction)) %>%
    arrange(desc(TdS_Final_Prediction))
  
  if(nrow(valid_predictions) == 0) {
    log_info(paste("No valid predictions for", gender, "- skipping normalization"))
    return(startlist_df)
  }
  
  # Determine expected number of finishers (use length of TdS points or actual startlist size)
  expected_finishers <- min(length(tds_points), nrow(valid_predictions))
  log_info(paste("Expected finishers for", gender, ":", expected_finishers))
  
  # Calculate target total points (sum of TdS points for expected finishers)
  target_total_points <- sum(tds_points[1:expected_finishers])
  log_info(paste("Target total points for", gender, ":", target_total_points))
  
  # Calculate current total of predictions
  current_total_points <- sum(valid_predictions$TdS_Final_Prediction, na.rm = TRUE)
  log_info(paste("Current total points for", gender, ":", round(current_total_points, 1)))
  
  # Apply scaling factor if current total > 0
  if(current_total_points > 0) {
    scaling_factor <- target_total_points / current_total_points
    log_info(paste("Scaling factor for", gender, ":", round(scaling_factor, 4)))
    
    # Apply scaling to create normalized predictions
    startlist_df$TdS_Normalized_Prediction <- ifelse(
      !is.na(startlist_df$TdS_Final_Prediction),
      startlist_df$TdS_Final_Prediction * scaling_factor,
      NA_real_
    )
    
    # Verify the normalization
    new_total <- sum(startlist_df$TdS_Normalized_Prediction, na.rm = TRUE)
    log_info(paste("Normalized total points for", gender, ":", round(new_total, 1)))
    log_info(paste("Normalization accuracy for", gender, ":", round(100 * new_total / target_total_points, 2), "%"))
    
  } else {
    log_info(paste("Current total is 0 for", gender, "- cannot normalize"))
    startlist_df$TdS_Normalized_Prediction <- startlist_df$TdS_Final_Prediction
  }
  
  return(startlist_df)
}

# Apply points normalization to both genders
men_startlist_normalized <- normalize_points_predictions(men_startlist, "men")
ladies_startlist_normalized <- normalize_points_predictions(ladies_startlist, "ladies")

# Update the prepare_points_output function to use normalized predictions
prepare_points_output <- function(df, gender, use_normalized = TRUE) {
  points_column <- if(use_normalized && "TdS_Normalized_Prediction" %in% names(df)) {
    "TdS_Normalized_Prediction"
  } else {
    "TdS_Final_Prediction"
  }
  
  log_info(paste("Preparing", gender, "points output using", points_column))
  
  output_df <- df %>%
    filter(!is.na(.data[[points_column]])) %>%
    select(Skier, ID, Nation, Points = !!sym(points_column)) %>%
    arrange(desc(Points))
  
  log_info(paste("Prepared", gender, "points output with", nrow(output_df), "athletes"))
  log_info(paste("Points range:", round(min(output_df$Points, na.rm = TRUE), 1), "to", round(max(output_df$Points, na.rm = TRUE), 1)))
  log_info(paste("Total points:", round(sum(output_df$Points, na.rm = TRUE), 1)))
  return(output_df)
}

# Create points output using normalized predictions
men_points_output <- prepare_points_output(men_startlist_normalized, "men's", use_normalized = TRUE)
ladies_points_output <- prepare_points_output(ladies_startlist_normalized, "ladies'", use_normalized = TRUE)

# Save Points Excel files
men_points_path <- file.path(output_dir, "men.xlsx")
ladies_points_path <- file.path(output_dir, "ladies.xlsx")

write.xlsx(men_points_output, men_points_path, rowNames = FALSE)
write.xlsx(ladies_points_output, ladies_points_path, rowNames = FALSE)

log_info(paste("Saved men's TdS points predictions to:", men_points_path))
log_info(paste("Saved ladies' TdS points predictions to:", ladies_points_path))

# Position probabilities will be saved after probability modeling is complete

# Display top predicted performers
log_info("=== TOP PREDICTED PERFORMERS ===")

show_top_predictions <- function(output_df, gender, n = 10) {
  log_info(paste("Top", n, "predicted", gender, "TdS performers:"))
  
  top_predictions <- output_df %>%
    head(n)
  
  for(i in 1:nrow(top_predictions)) {
    log_info(paste("  ", i, ".", top_predictions$Skier[i], "(", top_predictions$Nation[i], ") -", 
                   "Points:", round(top_predictions$Points[i], 1)))
  }
  
  return(top_predictions)
}

men_top <- show_top_predictions(men_points_output, "men's")
ladies_top <- show_top_predictions(ladies_points_output, "ladies'")

# ============================================================================
# POSITION PROBABILITY TRAINING DATA SETUP
# ============================================================================

log_info("=== SETTING UP POSITION PROBABILITY TRAINING DATA ===")

# Define position thresholds
position_thresholds <- c(1, 3, 5, 10, 30)  # Win, Podium, Top 5, Top 10, Top 30

# Function to add position threshold columns to training data
add_position_thresholds <- function(training_df, gender) {
  log_info(paste("Adding position threshold columns for", gender, "TdS training data"))
  
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

# Add position thresholds to both training datasets
men_tds_training_with_thresholds <- add_position_thresholds(men_tds_training, "men")
ladies_tds_training_with_thresholds <- add_position_thresholds(ladies_tds_training, "ladies")

log_info("Position threshold training data setup complete")

# ============================================================================
# POSITION PROBABILITY FEATURE SELECTION
# ============================================================================

log_info("=== POSITION PROBABILITY FEATURE SELECTION ===")

# Function to perform feature selection for position probability models
perform_position_feature_selection <- function(training_df, gender) {
  log_info(paste("Performing feature selection for", gender, "position probability models"))
  
  # Prepare the data - filter out invalid records and handle NAs
  model_df <- training_df %>%
    filter(!is.na(Place), Place > 0) %>%
    mutate(across(all_of(final_tds_features), replace_na_with_quartile))
  
  log_info(paste("Using", nrow(model_df), "records for feature selection"))
  
  # Storage for selected features by threshold
  selected_features <- list()
  
  # Perform feature selection for each threshold
  for(threshold in position_thresholds) {
    log_info(paste("Feature selection for top", threshold, "positions"))
    
    threshold_col <- paste0("top_", threshold)
    
    # Create formula using all final TdS features
    pos_formula <- as.formula(paste(threshold_col, "~", paste(final_tds_features, collapse = " + ")))
    
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
      fallback_features <- c("Pelo_Pct", "TDS_Last_5")
      fallback_features <- fallback_features[fallback_features %in% final_tds_features]
      selected_features[[threshold_col]] <- fallback_features
      
      log_info(paste("  Using fallback features:", paste(fallback_features, collapse = ", ")))
    })
  }
  
  return(selected_features)
}

# Perform feature selection for both genders
men_position_features <- perform_position_feature_selection(men_tds_training_with_thresholds, "men")
ladies_position_features <- perform_position_feature_selection(ladies_tds_training_with_thresholds, "ladies")

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
    mutate(across(all_of(final_tds_features), replace_na_with_quartile))
  
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
men_position_gam_models <- build_position_gam_models(men_tds_training_with_thresholds, men_position_features, "men")
ladies_position_gam_models <- build_position_gam_models(ladies_tds_training_with_thresholds, ladies_position_features, "ladies")

# Log final model summary
log_info("=== POSITION GAM MODELS SUMMARY ===")
log_info(paste("Men's models built:", length(men_position_gam_models), "out of", length(position_thresholds), "thresholds"))
for(threshold in position_thresholds) {
  threshold_col <- paste0("top_", threshold)
  if(threshold_col %in% names(men_position_gam_models)) {
    log_info(paste("  ✓", threshold_col))
  } else {
    log_info(paste("  ✗", threshold_col))
  }
}

log_info(paste("Ladies' models built:", length(ladies_position_gam_models), "out of", length(position_thresholds), "thresholds"))
for(threshold in position_thresholds) {
  threshold_col <- paste0("top_", threshold)
  if(threshold_col %in% names(ladies_position_gam_models)) {
    log_info(paste("  ✓", threshold_col))
  } else {
    log_info(paste("  ✗", threshold_col))
  }
}

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

# Normalize probabilities for both genders
men_startlist_final <- normalize_startlist_probabilities(men_startlist_with_probs, "men")
ladies_startlist_final <- normalize_startlist_probabilities(ladies_startlist_with_probs, "ladies")

# ============================================================================
# CREATE AND SAVE POSITION PROBABILITY OUTPUTS
# ============================================================================

log_info("=== CREATING POSITION PROBABILITY OUTPUTS ===")

# Create probability outputs (now that probability modeling is complete)
men_probs_output <- prepare_probabilities_output(men_startlist_final, "men's")
ladies_probs_output <- prepare_probabilities_output(ladies_startlist_final, "ladies'")

# Save Position Probabilities Excel files with custom sheet names
men_probs_path <- file.path(output_dir, "men_position_probabilities.xlsx")
ladies_probs_path <- file.path(output_dir, "ladies_position_probabilities.xlsx")

# Create workbooks with custom sheet names
men_wb <- createWorkbook()
addWorksheet(men_wb, "Men Tour de Ski")
writeData(men_wb, "Men Tour de Ski", men_probs_output)
saveWorkbook(men_wb, men_probs_path, overwrite = TRUE)

ladies_wb <- createWorkbook()
addWorksheet(ladies_wb, "Ladies Tour de Ski")
writeData(ladies_wb, "Ladies Tour de Ski", ladies_probs_output)
saveWorkbook(ladies_wb, ladies_probs_path, overwrite = TRUE)

log_info(paste("Saved men's TdS position probabilities to:", men_probs_path))
log_info(paste("Saved ladies' TdS position probabilities to:", ladies_probs_path))

# ============================================================================
# TDS PREDICTIONS COMPLETE
# ============================================================================

# ============================================================================
# FANTASY TEAM OPTIMIZATION
# ============================================================================

log_info("=== TOUR DE SKI FANTASY TEAM OPTIMIZATION ===")

# Fantasy team optimization - standard format
FANTASY_BUDGET <- 100000  # Total budget for combined team
MAX_MEN <- 8              # Maximum men in team
MAX_LADIES <- 8           # Maximum ladies in team

# Function to optimize combined fantasy team using knapsack approach
optimize_tds_combined_team <- function(men_startlist, ladies_startlist, total_budget = FANTASY_BUDGET) {
  log_info(paste("Optimizing combined TdS fantasy team using knapsack approach"))
  log_info(paste("Budget:", total_budget, "| Max team: up to", MAX_MEN, "men and", MAX_LADIES, "ladies"))
  
  # Prepare men's candidates
  men_candidates <- NULL
  if(!is.null(men_startlist)) {
    men_candidates <- men_startlist %>%
      filter(!is.na(Price), Price > 0, !is.na(TdS_Normalized_Prediction)) %>%
      mutate(
        Gender = "M",
        expected_value = TdS_Normalized_Prediction,  # Only use predicted points
        athlete_id = paste0("M_", row_number())
      ) %>%
      select(athlete_id, Skier, ID, Nation, Price, Gender, TdS_Normalized_Prediction, expected_value)
    
    log_info(paste("Found", nrow(men_candidates), "valid men candidates"))
  }
  
  # Prepare ladies' candidates  
  ladies_candidates <- NULL
  if(!is.null(ladies_startlist)) {
    ladies_candidates <- ladies_startlist %>%
      filter(!is.na(Price), Price > 0, !is.na(TdS_Normalized_Prediction)) %>%
      mutate(
        Gender = "L", 
        expected_value = TdS_Normalized_Prediction,  # Only use predicted points
        athlete_id = paste0("L_", row_number())
      ) %>%
      select(athlete_id, Skier, ID, Nation, Price, Gender, TdS_Normalized_Prediction, expected_value)
    
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
      total_expected_points <- sum(selected_team$TdS_Normalized_Prediction)
      men_count <- sum(selected_team$Gender == "M")
      ladies_count <- sum(selected_team$Gender == "L")
      
      log_info(paste("=== OPTIMAL TDS FANTASY TEAM (KNAPSACK) ==="))
      log_info(paste("Team size:", nrow(selected_team), "athletes"))
      log_info(paste("  Men:", men_count, "/", MAX_MEN))
      log_info(paste("  Ladies:", ladies_count, "/", MAX_LADIES))
      log_info(paste("Total cost:", total_cost, "/", total_budget, "(", round(100*total_cost/total_budget, 1), "% of budget)"))
      log_info(paste("Expected TdS points:", round(total_expected_points, 1)))
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
        select(Skier, ID, Nation, Gender, Price, TdS_Normalized_Prediction) %>%
        rename(Sex = Gender, `Predicted Points` = TdS_Normalized_Prediction) %>%
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
    return(optimize_tds_greedy_fallback(all_candidates, total_budget))
  })
}

# Fallback greedy function for when knapsack optimization fails
optimize_tds_greedy_fallback <- function(all_candidates, total_budget) {
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
      select(Skier, ID, Nation, Gender, Price, TdS_Normalized_Prediction) %>%
      rename(Sex = Gender, `Predicted Points` = TdS_Normalized_Prediction) %>%
      arrange(desc(`Predicted Points`))
    
    return(fantasy_team)
  }
  
  return(NULL)
}

# Run combined fantasy team optimization
tds_fantasy_team <- NULL

if(exists("men_startlist_normalized") && exists("ladies_startlist_normalized")) {
  tds_fantasy_team <- optimize_tds_combined_team(men_startlist_normalized, ladies_startlist_normalized)
} else {
  log_info("Missing normalized startlist data - skipping fantasy optimization")
  log_info(paste("men_startlist_normalized exists:", exists("men_startlist_normalized")))
  log_info(paste("ladies_startlist_normalized exists:", exists("ladies_startlist_normalized")))
}

# Save combined fantasy team
if(!is.null(tds_fantasy_team)) {
  log_info("=== SAVING FANTASY TEAM ===")
  
  fantasy_team_path <- file.path(output_dir, "fantasy_team.xlsx")
  write.xlsx(tds_fantasy_team, fantasy_team_path, rowNames = FALSE)
  log_info(paste("Saved TdS fantasy team to:", fantasy_team_path))
  
  # Also create summary by gender
  men_team <- tds_fantasy_team %>% filter(Sex == "M")
  ladies_team <- tds_fantasy_team %>% filter(Sex == "L")
  
  if(nrow(men_team) > 0) {
    log_info(paste("Men's team:", nrow(men_team), "athletes, total cost:", sum(men_team$Price)))
  }
  if(nrow(ladies_team) > 0) {
    log_info(paste("Ladies' team:", nrow(ladies_team), "athletes, total cost:", sum(ladies_team$Price)))
  }
}

log_info("=== TDS PREDICTIONS COMPLETE ===")

log_info("Tour de Ski predictions generation finished successfully!")

