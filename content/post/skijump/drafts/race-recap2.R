# Load required libraries
# library(arrow)  # no longer needed for CSV files
library(readxl) # for reading Excel files
library(tidyverse)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(writexl)
library(logger)
library(mgcv)     # for GAM models
library(leaps)    # for feature selection
library(lubridate) # for date handling

# Define file paths
men_chrono_path <- "~/ski/elo/python/skijump/polars/excel365/men_chrono_elevation.csv"
ladies_chrono_path <- "~/ski/elo/python/skijump/polars/excel365/ladies_chrono_elevation.csv"
races_path <- "~/ski/elo/python/skijump/polars/excel365/races.csv"
base_dir <- "~/blog/daehl-e"

# Function to get the current date in GMT formatted as YYYYMMDD
get_gmt_date_formatted <- function() {
  format(as.POSIXct(Sys.time(), tz = "GMT"), "%Y%m%d")
}

# Define the base output directory function that uses the GMT date
get_output_dir <- function() {
  date_str <- get_gmt_date_formatted()
  file.path("~/blog/daehl-e/content/post/skijump/drafts/weekly-recap", date_str)
}

# Read the CSV files
men_chrono <- read.csv(men_chrono_path)
ladies_chrono <- read.csv(ladies_chrono_path)

# Create a list to store all dataframes
ski_data <- list(
  men_chrono = men_chrono,
  ladies_chrono = ladies_chrono
)

# Print the dimensions of each dataframe to verify loading
lapply(ski_data, dim)

#----------------------------------------------
# NEW SECTION: Weekly Elo Changes
#----------------------------------------------

# Function to generate weekly Elo change report
generate_weekly_elo_change <- function(chrono_data, gender, base_dir) {
  # Get current date
  current_date <- Sys.Date()
  
  # Get date from one week ago
  one_week_ago <- current_date - 7
  
  # Get unique skiers for the current season (using max Season in the data)
  max_season <- max(chrono_data$Season)
  current_skiers <- chrono_data %>%
    filter(Season == max_season) %>%
    distinct(Skier, ID) %>%
    pull(ID)
  
  # Filter data for these skiers
  skier_data <- chrono_data %>%
    filter(ID %in% current_skiers)
  
  # Create dataframe to store Elo changes
  elo_changes <- data.frame(
    Skier = character(),
    ID = character(),
    Nation = character(),
    Current_Elo = numeric(),
    Previous_Week_Elo = numeric(),
    Elo_Change = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each skier
  for (skier_id in current_skiers) {
    # Get skier data
    sk_data <- skier_data %>%
      filter(ID == skier_id) %>%
      arrange(Date)
    
    if (nrow(sk_data) > 0) {
      # Get the skier name and nation
      skier_name <- sk_data$Skier[1]
      skier_nation <- sk_data$Nation[1]
      
      # Get most recent date
      most_recent <- sk_data %>%
        filter(!is.na(Date)) %>%
        arrange(desc(Date)) %>%
        dplyr::slice(1)
      
      # Get date from previous week or earlier
      previous_week <- sk_data %>%
        filter(!is.na(Date), Date <= one_week_ago) %>%
        arrange(desc(Date)) %>%
        dplyr::slice(1)
      
      # Process only the overall Elo column
      elo_col <- "Elo"
      
      # Check if we have both current and previous data
      if (nrow(most_recent) > 0 && nrow(previous_week) > 0) {
        current_elo <- most_recent[[elo_col]]
        previous_elo <- previous_week[[elo_col]]
        
        # Skip if either Elo is NA
        if (!is.na(current_elo) && !is.na(previous_elo)) {
          # Calculate Elo change
          elo_change <- current_elo - previous_elo
          
          # Only add if Elo change is not zero
          if (elo_change != 0) {
            # Add to dataframe
            elo_changes <- rbind(elo_changes, data.frame(
              Skier = skier_name,
              ID = skier_id,
              Nation = skier_nation,
              Current_Elo = current_elo,
              Previous_Week_Elo = previous_elo,
              Elo_Change = elo_change,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  # Create weekly output directory with GMT date
  week_dir <- get_output_dir()
  dir.create(week_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Sort by Elo change (descending)
  elo_changes <- elo_changes %>%
    arrange(desc(Elo_Change))
  
  # Save to Excel
  file_path <- file.path(week_dir, paste0(gender, "_elo_change.xlsx"))
  write_xlsx(elo_changes, file_path)
  
  # Return the full dataframe
  return(elo_changes)
}

# Generate weekly Elo change reports
men_elo_changes <- generate_weekly_elo_change(men_chrono, "men", base_dir)
women_elo_changes <- generate_weekly_elo_change(ladies_chrono, "ladies", base_dir)

# Ski jumping points system setup
individual_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Function to replace both NAs and NaNs in a vector
replace_nas_vector <- function(x) {
  if(is.null(x)) return(NULL)
  # Convert to numeric if it's not already
  x <- as.numeric(unlist(x))
  # Replace both NAs and NaNs
  x[is.na(x) | is.nan(x) | !is.finite(x)] <- 0
  return(x)
}

# Function to replace NAs with first quartile
replace_na_with_quartile <- function(x) {
  quartile_1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), quartile_1, x)
}

# Create points conversion function for ski jumping
place_to_points <- Vectorize(function(place) {
  if (is.na(place)) return(0)
  if (place <= length(individual_points)) individual_points[place] else 0
})

create_points_columns <- function(df) {
  df %>%
    mutate(
      Points = place_to_points(Place)
    )
}

calculate_weighted_average <- function(df) {
  df %>%
    group_by(ID) %>%
    arrange(Season, Race) %>%
    mutate(
      Weighted_Last_5 = sapply(row_number(), function(i) {
        # Get previous races (not including current race)
        prev_races <- Points[max(1, i-5):(i-1)]
        if(length(prev_races) > 0) {
          # Create weights (most recent gets highest weight)
          weights <- seq(1,length(prev_races))
          # Calculate weighted mean
          weighted.mean(prev_races, weights, na.rm = TRUE)
        } else {
          NA_real_
        }
      })
    ) %>%
    ungroup()
}

calculate_weighted_average2 <- function(df) {
  df %>%
    group_by(ID) %>%
    arrange(Season, Race) %>%
    mutate(
      Weighted_Last_5_2 = sapply(row_number(), function(i) {
        # Get previous races (not including current race)
        prev_races <- Points[max(1, i-4):(i)]
        if(length(prev_races) > 0) {
          # Create weights (most recent gets highest weight)
          weights <- seq(1,length(prev_races))
          # Calculate weighted mean
          weighted.mean(prev_races, weights, na.rm = TRUE)
        } else {
          NA_real_
        }
      })
    ) %>%
    ungroup()
}

# Create function to calculate Pelo percentages
create_pelo_percentages <- function(df) {
  # Get all column names ending with _Pelo
  pelo_cols <- names(df)[grepl("Pelo$", names(df))]
  
  df %>%
    group_by(Season, Race) %>%
    mutate(
      across(
        all_of(pelo_cols),
        ~ . / max(., na.rm = TRUE),
        .names = "{.col}_Pct"
      )
    ) %>%
    ungroup()
}

# Function to select best features and create GAM
select_gam_features <- function(df, points_col = "Points") {
  # Get predictor columns (all Pelo percentages and weighted average)
  # Get predictor columns (all Pelo percentages and weighted average)
  all_pelo_predictors <- names(df)[grepl("Pelo_Pct$", names(df))]
  
  # Filter out predictors with insufficient data (e.g., Small_Elo, Medium_Elo)
  valid_predictors <- c()
  for(pred in all_pelo_predictors) {
    non_na_count <- sum(!is.na(df[[pred]]) & df[[pred]] > 0)
    if(non_na_count >= 10) {  # Require at least 10 valid observations
      valid_predictors <- c(valid_predictors, pred)
    }
  }
  
  predictors <- c(valid_predictors, "Weighted_Last_5")
  
  # Skip if no valid predictors
  if(length(predictors) <= 1) {
    return(list(
      selected_features = character(),
      gam_model = NULL,
      bic_scores = NULL,
      feature_selection = NULL
    ))
  }
  
  df <- df %>%
    mutate(across(
      all_of(predictors),
      replace_na_with_quartile
    ))
  
  # Create formula for feature selection
  base_formula <- as.formula(paste(points_col, "~", paste(predictors, collapse = " + ")))
  
  # Perform exhaustive feature selection
  feature_selection <- regsubsets(base_formula, 
                                  data = df, 
                                  nvmax = length(predictors),
                                  method = "exhaustive")
  
  # Get summary and find model with lowest BIC
  selection_summary <- summary(feature_selection)
  best_model_idx <- which.min(selection_summary$bic)
  selected_vars <- names(coef(feature_selection, best_model_idx))[-1]
  
  # Create GAM formula with selected features using smooth terms
  gam_formula <- as.formula(paste(points_col, "~", 
                                  paste(paste0("s(", selected_vars, ")"), 
                                        collapse = " + ")))
  
  # Fit GAM with selected features
  final_gam <- gam(gam_formula, data = df, method = "REML")
  
  return(list(
    selected_features = selected_vars,
    gam_model = final_gam,
    bic_scores = selection_summary$bic,
    feature_selection = feature_selection
  ))
}

# Function to generate predictions for Ski Jumping race types
generate_race_predictions <- function(chrono_data, race_type, gender) {
  # Define discipline mapping for Pelo columns
  discipline_mapping <- list(
    "Normal" = "Normal_Pelo_Pct",
    "Large" = "Large_Pelo_Pct",
    "Flying" = "Flying_Pelo_Pct"
  )
  
  # Get the corresponding Pelo_Pct column for this race type
  pelo_pct_col <- discipline_mapping[[race_type]]
  
  # For ladies, skip Flying since they don't compete in it
  if (gender == "L" && race_type == "Flying") {
    return(list(
      predictions = data.frame(
        Skier = character(),
        ID = character(),
        predicted_points = numeric(),
        gender = character(),
        race_type = character()
      ),
      model = NULL,
      feature_summary = paste("Skipping", gender, race_type, "- not applicable")
    ))
  }
  
  # Process and filter training data
  train_df <- chrono_data %>%
    filter(
      City != "Summer",
      RaceType == race_type,
      !grepl("Team", RaceType, ignore.case = TRUE)  # Exclude team races
    ) %>%
    create_points_columns() %>%
    group_by(ID) %>%
    arrange(Season, Race) %>%
    mutate(
      Weighted_Last_5 = sapply(row_number(), function(i) {
        prev_races <- Points[max(1, i-5):(i-1)]
        if(length(prev_races) > 0) {
          weights <- seq(1,length(prev_races))
          weighted.mean(prev_races, weights, na.rm = TRUE)
        } else {
          NA_real_
        }
      })
    ) %>%
    mutate(
      Weighted_Last_5_2 = sapply(row_number(), function(i) {
        prev_races <- Points[max(1, i-4):(i-0)]
        if(length(prev_races) > 0) {
          weights <- seq(1,length(prev_races))
          weighted.mean(prev_races, weights, na.rm = TRUE)
        } else {
          NA_real_
        }
      })
    ) %>%
    ungroup()

  train_df <- train_df %>%
    filter(
      Season > max(Season)-11
    ) %>%
    create_pelo_percentages() %>%
    filter(!!sym(discipline_mapping[[race_type]]) > 0.75)

  # Build model
  # Build model
  model_results <- select_gam_features(train_df, "Points")
  
  # Skip prediction if model building failed
  if(is.null(model_results$gam_model)) {
    return(list(
      predictions = data.frame(
        Skier = character(),
        ID = character(),
        predicted_points = numeric(),
        gender = character(),
        race_type = character()
      ),
      model = NULL,
      feature_summary = paste("Insufficient data for", gender, race_type, "predictions")
    ))
  }
  
  # Process test data
  test_df <- chrono_data %>%
    filter(
      City == "Summer",
      Season == max(Season)
    )
  
  # Update Elo columns (convert Pelo to Elo for current season)
  pelo_cols <- names(test_df)[grep("Pelo$", names(test_df))]
  elo_cols <- gsub("Pelo$", "Elo", pelo_cols)
  
  test_df <- test_df %>%
    mutate(across(
      all_of(elo_cols),
      ~ case_when(
        Season == max(Season) ~ get(gsub("Elo$", "Pelo", cur_column())),
        TRUE ~ .
      ),
      .names = "{.col}"
    )) %>%
    group_by(Skier) %>%
    arrange(desc(Date)) %>%
    dplyr::slice(1) %>%
    ungroup()
  
  # Get weighted averages
  recent_weighted_avgs <- train_df %>%
    group_by(ID) %>%
    arrange(desc(Date)) %>%
    dplyr::slice(1) %>%
    dplyr::select(ID, Weighted_Last_5_2) %>%
    ungroup()
  
  # Update test_df
  test_df <- test_df %>%
    left_join(recent_weighted_avgs, by = "ID") %>%
    rename(Weighted_Last_5 = Weighted_Last_5_2)
  
  # Replace NAs with first quartiles in predictor columns
  predictor_cols <- c(
    pelo_cols,  # All Pelo columns
    "Weighted_Last_5"  # Weighted average
  )
  
  test_df <- test_df %>%
    mutate(across(
      all_of(predictor_cols),
      replace_na_with_quartile
    ))
  
  # Create percentages and predictions
  test_df_with_pct <- test_df %>%
    mutate(across(
      ends_with("Pelo"),
      ~ . / max(., na.rm = TRUE),
      .names = "{.col}_Pct"
    ))
  
  # Generate predictions
  gam_df <- test_df_with_pct %>%
    mutate(
      predicted_points = predict(model_results$gam_model, newdata = .),
      gender = gender
    ) %>%
    dplyr::select(
      Skier,
      ID,
      predicted_points,
      gender
    ) %>%
    mutate(
      predicted_points = round(predicted_points, 1),
      race_type = race_type
    ) %>%
    arrange(desc(predicted_points))
  print(gam_df)
  return(list(
    predictions = gam_df,
    model = model_results,
    feature_summary = paste("Selected features for", gender, race_type, ":", 
                            paste(model_results$selected_features, collapse = ", "))
  ))
}

# Generate predictions for both genders
race_types <- c("Normal", "Large", "Flying")
all_predictions <- list()
all_models <- list()

# Generate predictions for men
for(race_type in race_types) {
  cat("\nProcessing Men's", race_type, "...\n")
  results <- generate_race_predictions(men_chrono, race_type, "M")
  all_predictions[[paste0("M_", race_type)]] <- results$predictions
  all_models[[paste0("M_", race_type)]] <- results$model
}

# Generate predictions for women (excluding Flying)
for(race_type in race_types) {
  cat("\nProcessing Women's", race_type, "...\n")
  results <- generate_race_predictions(ladies_chrono, race_type, "L")
  all_predictions[[paste0("L_", race_type)]] <- results$predictions
  all_models[[paste0("L_", race_type)]] <- results$model
}

# Combine and format predictions
# Filter out empty predictions and ensure consistent data types
non_empty_predictions <- all_predictions[sapply(all_predictions, function(x) nrow(x) > 0)]

# Combine and format predictions
if(length(non_empty_predictions) > 0) {
  # Ensure ID column is consistent type (character)
  non_empty_predictions <- lapply(non_empty_predictions, function(x) {
    x$ID <- as.character(x$ID)
    return(x)
  })
  
  final_predictions <- bind_rows(non_empty_predictions) %>%
    group_by(Skier, ID, gender) %>%
    summarise(
      race_type = first(race_type),
      predicted_points = max(predicted_points, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Replace -Inf values with NA
    mutate(predicted_points = ifelse(is.infinite(predicted_points), NA_real_, predicted_points))
} else {
  final_predictions <- data.frame(
    Skier = character(),
    ID = character(),
    gender = character(),
    race_type = character(),
    predicted_points = numeric()
  )
}

print(final_predictions)
print(all_predictions)

# Load standings for both genders
standings_df <- read_csv("~/ski/elo/python/skijump/polars/excel365/men_standings.csv")
ladies_standings_df <- read_csv("~/ski/elo/python/skijump/polars/excel365/ladies_standings.csv")

# Get top 30 skiers from each gender
top_30_men <- standings_df %>%
  arrange(desc(Points)) %>%
  head(30) %>%
  pull(Skier)

top_30_women <- ladies_standings_df %>%
  arrange(desc(Points)) %>%
  head(30) %>%
  pull(Skier)

# Function to calculate race participation probabilities for ski jumping simulation
calculate_skier_race_probabilities_skijump <- function(chrono_data, skier_name) {
  log_info("Calculating race probabilities for skier: {skier_name}")
  
  # Get skier's first ever race date
  skier_first_race <- chrono_data %>%
    filter(Skier == skier_name) %>%
    arrange(Date) %>%
    dplyr::slice(1) %>%
    pull(Date)
  
  # Calculate date from 5 years ago
  five_years_ago <- Sys.Date() - (5 * 365)
  
  # Use 5 years ago or skier's first race, whichever is later
  start_date <- if(length(skier_first_race) == 0) {
    five_years_ago
  } else {
    max(five_years_ago, as.Date(skier_first_race))
  }
  
  # Function to get race probability for a specific discipline
  get_discipline_probability <- function(discipline) {
    # For ski jumping disciplines
    all_races <- chrono_data %>%
      filter(
        RaceType == discipline,
        Date >= start_date,
        City != "Summer",
        !grepl("Team", RaceType, ignore.case = TRUE)  # Exclude team races
      ) %>%
      arrange(Date, Race) %>%
      distinct(Date, Race)
    
    skier_races <- chrono_data %>%
      filter(
        RaceType == discipline,
        Date >= start_date,
        City != "Summer",
        Skier == skier_name,
        !grepl("Team", RaceType, ignore.case = TRUE)  # Exclude team races
      ) %>%
      arrange(Date, Race) %>%
      distinct(Date, Race)
    
    total_races <- nrow(all_races)
    
    if(total_races == 0) {
      return(0.5)  # Default probability if no historical data
    }
    
    # Create exponential decay weights (α = 0.1)
    # Most recent race gets weight = 1, oldest gets weight = exp(-0.1 * (total_races-1))
    race_weights <- exp(-0.1 * ((total_races-1):0))
    
    # Create participation vector (1 if skier participated, 0 if not)
    participation <- rep(0, total_races)
    
    # Mark races where skier participated
    if(nrow(skier_races) > 0) {
      # Match skier races to all races by date and race number
      for(i in 1:nrow(skier_races)) {
        match_idx <- which(all_races$Date == skier_races$Date[i] & 
                          all_races$Race == skier_races$Race[i])
        if(length(match_idx) > 0) {
          participation[match_idx] <- 1
        }
      }
    }
    
    # Calculate weighted probability
    weighted_participation <- sum(participation * race_weights)
    total_weight <- sum(race_weights)
    prob <- weighted_participation / total_weight
    
    log_debug("Probability for {skier_name} in {discipline}: {prob} (weighted: {round(weighted_participation, 3)}/{round(total_weight, 3)})")
    
    return(prob)
  }
  
  # Calculate probabilities for all ski jumping disciplines
  probabilities <- list(
    Normal = get_discipline_probability("Normal"),
    Large = get_discipline_probability("Large"),
    Flying = get_discipline_probability("Flying")
  )
  
  return(probabilities)
}

create_race_probability_table_skijump <- function(men_chrono, ladies_chrono, top_30_men, top_30_women) {
  log_info("Pre-calculating race probabilities for all ski jumping skiers")
  
  # Function to calculate probabilities for a single skier
  calculate_single_skier_probabilities_skijump <- function(chrono_data, skier_name) {
    # Get skier's first ever race date
    skier_first_race <- chrono_data %>%
      filter(Skier == skier_name) %>%
      arrange(Date) %>%
      dplyr::slice(1) %>%
      pull(Date)
    
    # Calculate date from 5 years ago
    five_years_ago <- Sys.Date() - (5 * 365)
    
    # Use 5 years ago or skier's first race, whichever is later
    start_date <- if(length(skier_first_race) == 0) {
      five_years_ago
    } else {
      max(five_years_ago, as.Date(skier_first_race))
    }
    
    # Function to get race probability for a specific discipline
    get_discipline_probability <- function(discipline) {
      all_races <- chrono_data %>%
        filter(
          RaceType == discipline,
          Date >= start_date,
          City != "Summer",
          !grepl("Team", RaceType, ignore.case = TRUE)  # Exclude team races
        ) %>%
        arrange(Date, Race) %>%
        distinct(Date, Race)
      
      skier_races <- chrono_data %>%
        filter(
          RaceType == discipline,
          Date >= start_date,
          City != "Summer",
          Skier == skier_name,
          !grepl("Team", RaceType, ignore.case = TRUE)  # Exclude team races
        ) %>%
        arrange(Date, Race) %>%
        distinct(Date, Race)
      
      total_races <- nrow(all_races)
      
      if(total_races == 0) {
        return(0.5)  # Default probability if no historical data
      }
      
      # Create exponential decay weights (α = 0.1)
      # Most recent race gets weight = 1, oldest gets weight = exp(-0.1 * (total_races-1))
      race_weights <- exp(-0.1 * ((total_races-1):0))
      
      # Create participation vector (1 if skier participated, 0 if not)
      participation <- rep(0, total_races)
      
      # Mark races where skier participated
      if(nrow(skier_races) > 0) {
        # Match skier races to all races by date and race number
        for(i in 1:nrow(skier_races)) {
          match_idx <- which(all_races$Date == skier_races$Date[i] & 
                            all_races$Race == skier_races$Race[i])
          if(length(match_idx) > 0) {
            participation[match_idx] <- 1
          }
        }
      }
      
      # Calculate weighted probability
      weighted_participation <- sum(participation * race_weights)
      total_weight <- sum(race_weights)
      prob <- weighted_participation / total_weight
      
      return(prob)
    }
    
    # Calculate probabilities for all ski jumping disciplines
    return(data.frame(
      Skier = skier_name,
      Normal = get_discipline_probability("Normal"),
      Large = get_discipline_probability("Large"),
      Flying = get_discipline_probability("Flying"),
      stringsAsFactors = FALSE
    ))
  }
  
  # Calculate for all men
  men_probabilities <- do.call(rbind, lapply(top_30_men, function(skier) {
    cat("Calculating probabilities for male skier:", skier, "\n")
    calculate_single_skier_probabilities_skijump(men_chrono, skier)
  }))
  men_probabilities$Gender <- "M"
  
  # Calculate for all women  
  women_probabilities <- do.call(rbind, lapply(top_30_women, function(skier) {
    cat("Calculating probabilities for female skier:", skier, "\n")
    calculate_single_skier_probabilities_skijump(ladies_chrono, skier)
  }))
  women_probabilities$Gender <- "L"
  
  # Combine into single table
  all_probabilities <- rbind(men_probabilities, women_probabilities)
  
  log_info("Race probability table created with {nrow(all_probabilities)} skiers")
  
  # Save to file for future use
  output_dir <- get_output_dir()
  write_xlsx(all_probabilities, file.path(output_dir, "skijump_race_probabilities.xlsx"))
  
  return(all_probabilities)
}

# Function to get race history for ski jumping
get_race_history <- function(skier_id, race_type, predictions_list, n_required = 10, gender="M") {
  chrono_data <- if(gender == "M") men_chrono else ladies_chrono
  
  # For ladies, skip Flying since they don't compete in it
  if (gender == "L" && race_type == "Flying") {
    return(rep(0, n_required))
  }
  
  # Create points columns
  chrono_with_points <- chrono_data %>%
    create_points_columns()
  
  # Get actual results based on race type
  real_results <- chrono_with_points %>%
    filter(
      Skier == skier_id,
      City != "Summer",
      RaceType == race_type,
      !grepl("Team", RaceType, ignore.case = TRUE),  # Exclude team races
      Season > max(Season)-11
    ) %>%
    arrange(desc(Date)) %>%
    dplyr::slice_head(n = n_required) %>%
    pull(Points)
  
  print(paste("Race type:", race_type))
  
  # If we have enough results, return them
  if(length(real_results) >= n_required) {
    print("Met the requirement")
    return(real_results)
  } else {
    print(paste("The skier has participated in:", length(real_results)))
  }
  
  # Get predicted score from all_predictions
  pred_type <- paste0(gender, "_", race_type)
  
  predicted_score <- predictions_list[[pred_type]] %>%
    filter(Skier == skier_id) %>%
    pull(predicted_points)
  
  if(length(real_results) > 1) {
    variation_sd <- sd(unlist(real_results))
  } else {
    variation_sd <- predicted_score * 0.15
  }
  
  n_to_generate <- n_required - length(real_results)
  generated_results <- numeric(n_to_generate)
  
  for(i in 1:n_to_generate) {
    new_score <- predicted_score + rnorm(1, 0, variation_sd)
    generated_results[i] <- pmin(pmax(round(new_score), 0), 100)  # Ski jumping max is 100
  }
  
  all_results <- c(real_results, generated_results)
  return(all_results)
}

# Create storage for all histories
all_histories <- list()

# Process men
for(skier in top_30_men) {
  cat("\nProcessing male skier:", skier, "\n")
  
  all_histories[[skier]] <- list()
  
  for(race_type in race_types) {
    all_histories[[skier]][[race_type]] <- 
      get_race_history(skier, race_type, all_predictions, 10, gender = "M")
  }
}

# Process women
for(skier in top_30_women) {
  cat("\nProcessing female skier:", skier, "\n")
  
  all_histories[[skier]] <- list()
  
  for(race_type in race_types) {
    all_histories[[skier]][[race_type]] <- 
      get_race_history(skier, race_type, all_predictions, 10, gender = "L")
  }
}

# Clean up NAs and NaNs
all_histories_clean <- lapply(all_histories, function(skier_list) {
  lapply(skier_list, replace_nas_vector)
})

# Modified summarize function to include gender
summarize_histories <- function(histories) {
  summary_df <- data.frame()
  
  for(skier in names(histories)) {
    # Determine gender based on whether skier is in top_30_men or top_30_women
    gender <- if(skier %in% top_30_men) "M" else "L"
    
    for(race_type in names(histories[[skier]])) {
      results <- histories[[skier]][[race_type]]
      summary_df <- rbind(summary_df, data.frame(
        Skier = skier,
        Gender = gender,
        Race_Type = race_type,
        N_Real_Results = sum(!is.na(results)),
        Mean_Score = mean(results, na.rm = TRUE),
        SD_Score = sd(results, na.rm = TRUE)
      ))
    }
  }
  
  return(summary_df)
}

# Generate and print summary
summary_df <- summarize_histories(all_histories_clean)

# Print summary grouped by gender
print("\nSummary of all histories by gender:")
summary_df %>%
  group_by(Gender) %>%
  slice_head(n = 10) %>%
  print(n = 20)

# Modified race distribution function to handle sparse data
# Continuation of ski jump race recap analysis
# This file continues from the race distribution function

# Modified race distribution function to handle sparse data (continued)
create_race_distribution <- function(race_results, n_simulations = 1, max_points = 100) {
  points <- race_results$Points
  weights <- race_results$Weight
  n_actual_races <- length(points)
  
  # If very few points, use simple method
  if (n_actual_races < 2) {
    mean_points <- weighted.mean(points, weights)
    sd_points <- max(5, mean_points * 0.15)
    simulated_points <- rnorm(n_simulations, mean_points, sd_points)
    return(pmin(pmax(round(simulated_points), 0), max_points))
  }
  
  # Calculate proportion of maximum points
  prop_max <- weighted.mean(points == max_points, weights)
  
  # Initialize simulated points
  simulated_points <- numeric(n_simulations)
  
  # Determine number of max point races
  n_max <- rbinom(1, n_simulations, prop_max)
  if(n_max > 0) {
    simulated_points[1:n_max] <- max_points
  }
  
  if (n_max < n_simulations) {
    # Handle non-maximum points
    non_max_mask <- points < max_points
    if(sum(non_max_mask) == 0) {
      # If no non-max points, use high but not perfect score
      simulated_points[(n_max + 1):n_simulations] <- max_points * 0.95
      return(simulated_points)
    }
    
    non_max_points <- points[non_max_mask]
    non_max_weights <- weights[non_max_mask]
    
    # Initialize remaining_points
    remaining_points <- numeric(n_simulations - n_max)
    
    # Try KDE method first
    kde_success <- tryCatch({
      if(length(unique(non_max_points)) < 3) stop("Too few unique points for KDE")
      
      # Continuing from kde <- density line...
      
      kde <- density(non_max_points, 
                     weights = non_max_weights/sum(non_max_weights),
                     kernel = "gaussian",
                     bw = "nrd",
                     from = max(min(non_max_points) - 5, 0),
                     to = max_points - 1,  # Leave room for maximum
                     n = 512)
      
      kde$y[kde$x < min(non_max_points)] <- 0
      kde$y <- kde$y / sum(kde$y)
      
      remaining_points <- sample(kde$x, 
                                 n_simulations - n_max, 
                                 prob = kde$y, 
                                 replace = TRUE)
      TRUE
    }, error = function(e) {
      FALSE
    })
    
    # If KDE failed, use bootstrap method
    if (!kde_success) {
      if(length(non_max_points) == 0) {
        # If no non-max points available, use a reasonable default
        remaining_points <- rnorm(n_simulations - n_max, max_points * 0.85, max_points * 0.10)
      } else {
        # Ensure weights match points length
        if(length(non_max_weights) != length(non_max_points)) {
          log_info("Weight length mismatch: adjusting weights")
          non_max_weights <- rep(1, length(non_max_points))
        }
        
        # Sample with error checking
        tryCatch({
          remaining_points <- sample(non_max_points,
                                     n_simulations - n_max,
                                     prob = non_max_weights,
                                     replace = TRUE)
        }, error = function(e) {
          remaining_points <- sample(non_max_points,
                                     n_simulations - n_max,
                                     replace = TRUE)
        })
        
        # Add variation
        variation <- rnorm(n_simulations - n_max, 
                           0, 
                           max(5, sd(non_max_points)/2))
        remaining_points <- remaining_points + variation
      }
    }
    
    # Ensure valid range
    simulated_points[(n_max + 1):n_simulations] <- 
      pmin(pmax(round(remaining_points), 0), max_points - 1)
  }
  
  return(simulated_points)
}

# Function to calculate remaining races for ski jumping
calculate_remaining_races_skijump <- function(races_file) {
  # Read races file
  races <- read.csv(races_file, stringsAsFactors = FALSE)
  
  # Convert dates to Date objects
  races$Date <- as.Date(races$Date, format = "%m/%d/%Y")
  
  # Get current date in UTC
  today_utc <- as.Date(Sys.time(), tz = "UTC")
  
  # Filter for remaining races (exclude team races)
  remaining <- races %>%
    filter(
      Date >= today_utc, 
      Sex %in% c("M", "L"), Championship!=1,
      !grepl("Team", RaceType, ignore.case = TRUE)
    )
  
  # Initialize counts
  counts <- list(
    M = list(
      Normal = 0,
      Large = 0,
      Flying = 0
    ),
    L = list(
      Normal = 0,
      Large = 0,
      Flying = 0
    )
  )
  
  if(nrow(remaining) == 0) {
    return(counts)
  }
  
  # Create race type categories for each race
  remaining_categorized <- remaining %>%
    mutate(
      # Create race type categories
      race_category = case_when(
        RaceType == "Normal" ~ "Normal",
        RaceType == "Large" ~ "Large",
        RaceType == "Flying" ~ "Flying",
        TRUE ~ "Other"
      )
    )
  
  # Debug: Show categorization
  cat("Race categorization:\n")
  print(remaining_categorized %>% 
          select(Sex, RaceType, race_category) %>%
          arrange(Sex, race_category))
  
  # Count unique race types by gender
  for(gender in c("M", "L")) {
    gender_races <- remaining_categorized %>%
      filter(Sex == gender)
    
    # Get unique race categories for this gender
    unique_categories <- unique(gender_races$race_category)
    
    cat("\nUnique race categories for", gender, ":", paste(unique_categories, collapse = ", "), "\n")
    
    # Count each category
    for(category in unique_categories) {
      if(category != "Other") {
        category_count <- sum(gender_races$race_category == category)
        counts[[gender]][[category]] <- counts[[gender]][[category]] + category_count
      }
    }
  }
  
  return(counts)
}

# Calculate remaining races dynamically
races_path <- "~/ski/elo/python/skijump/polars/excel365/races.csv"
remaining_races_by_gender <- calculate_remaining_races_skijump(races_path)
print(remaining_races_by_gender)

# Function to simulate a single ski jumping race for all skiers
simulate_race_skijump <- function(histories, race_type) {
  log_info("Simulating ski jumping race of type {race_type}")
  
  # Get appropriate race histories for each skier
  results <- sapply(names(histories), function(skier) {
    race_history <- histories[[skier]][[race_type]]
    
    # Check for valid history
    if(is.null(race_history) || length(race_history) == 0) {
      log_info("No history for {skier} in {race_type}")
      return(0)
    }
    
    # Remove any NAs or NaNs from history
    race_history <- race_history[!is.na(race_history) & !is.nan(race_history)]
    
    if(length(race_history) >= 3) {
      # Create distribution and sample one result
      fake_data <- data.frame(
        Points = race_history,
        Weight = seq(length(race_history), 1)
      )
      
      tryCatch({
        simulated_points <- create_race_distribution(fake_data, n_simulations = 1, max_points = 100)
        return(simulated_points[1])
      }, error = function(e) {
        log_info("{skier}'s race_distribution failed")
        
        # If distribution creation fails, use simple mean with variation
        mean_points <- mean(race_history)
        result <- mean_points + rnorm(1, 0, max(5, mean_points * 0.15))
        return(max(0, min(100, round(result))))
      })
    } else {
      log_info("Not enough history for {skier} in {race_type}")
      mean_points <- mean(race_history)
      if(is.na(mean_points) || is.nan(mean_points)) {
        log_info("Invalid mean for {skier} in {race_type}")
        return(0)
      }
      result <- mean_points + rnorm(1, 0, max(5, mean_points * 0.15))
      return(max(0, min(100, round(result))))
    }
  })
  
  # Final check for any NAs or NaNs
  results[is.na(results) | is.nan(results)] <- 0
  return(results)
}

simulate_race_with_probability_skijump <- function(histories, race_type, probability_table) {
  log_info("Simulating ski jumping race of type {race_type} with pre-computed probabilities")
  
  # Get the probability column name (use race_type directly)
  prob_col <- race_type
  
  results <- sapply(names(histories), function(skier) {
    # Get participation probability from pre-computed table
    skier_prob_row <- probability_table[probability_table$Skier == skier, ]
    
    if(nrow(skier_prob_row) == 0) {
      participation_prob <- 0.5  # Default if skier not found
      log_debug("Skier {skier} not found in probability table, using default 0.5")
    } else {
      participation_prob <- skier_prob_row[[prob_col]]
    }
    
    # Randomly determine if skier participates
    participates <- runif(1) < participation_prob
    
    if(!participates) {
      return(0)  # No points if doesn't participate
    }
    
    # If participates, simulate their performance (same as before)
    race_history <- histories[[skier]][[race_type]]
    
    if(is.null(race_history) || length(race_history) == 0) {
      return(0)
    }
    
    race_history <- race_history[!is.na(race_history) & !is.nan(race_history)]
    
    if(length(race_history) >= 3) {
      fake_data <- data.frame(
        Points = race_history,
        Weight = seq(length(race_history), 1)
      )
      
      tryCatch({
        simulated_points <- create_race_distribution(fake_data, n_simulations = 1, max_points = 100)
        return(simulated_points[1])
      }, error = function(e) {
        mean_points <- mean(race_history)
        result <- mean_points + rnorm(1, 0, max(5, mean_points * 0.15))
        return(max(0, min(100, round(result))))
      })
    } else {
      mean_points <- mean(race_history)
      if(is.na(mean_points) || is.nan(mean_points)) {
        return(0)
      }
      result <- mean_points + rnorm(1, 0, max(5, mean_points * 0.15))
      return(max(0, min(100, round(result))))
    }
  })
  
  results[is.na(results) | is.nan(results)] <- 0
  return(results)
}

# Modified simulate_season function for ski jumping
simulate_season_skijump <- function(histories, standings_df, gender="M", n_simulations = 100, remaining_races_by_gender, probability_table) {
  print("Simulating Ski Jumping Season with Pre-computed Participation Probabilities")
  log_info("Starting {if(gender=='M') 'men' else 'women'}'s ski jumping season simulation with pre-computed probabilities")
  
  # Filter probability table for current gender
  gender_probabilities <- probability_table[probability_table$Gender == gender, ]
  
  # Get race counts for this specific gender
  gender_races <- remaining_races_by_gender[[gender]]
  
  # For ladies, set Flying races to 0 since they don't compete
  if(gender == "L") {
    gender_races$Flying <- 0
  }
  
  # Log race schedule for this gender
  log_info("{if(gender=='M') 'Men' else 'Women'}'s Ski Jumping races remaining: {paste(names(gender_races), gender_races, sep=': ', collapse=', ')}")
  
  # Filter histories for current gender
  gender_histories <- histories[names(histories) %in% standings_df$Skier]
  all_skiers <- names(gender_histories)
  
  log_info("Number of {if(gender=='M') 'male' else 'female'} skiers: {length(all_skiers)}")
  
  # Initialize results matrix
  season_results <- matrix(0, nrow = n_simulations, ncol = length(all_skiers))
  colnames(season_results) <- all_skiers
  
  # Add current points to all simulations
  for(skier in all_skiers) {
    current_points <- standings_df %>%
      filter(Skier == skier) %>%
      pull(Points) %>%
      first()
    
    if(length(current_points) == 0) {
      current_points <- 0
    }
    season_results[, skier] <- current_points
  }
  
  log_info("Current {if(gender=='M') 'men' else 'women'}'s standings loaded. Top 5:")
  top_5_current <- sort(season_results[1,], decreasing = TRUE)[1:5]
  for(i in 1:5) {
    log_info("{names(top_5_current)[i]}: {top_5_current[i]}")
  }
  
  # Run simulations
  for(sim in 1:n_simulations) {
    if(sim %% 10 == 0) {
      log_info("Running {if(gender=='M') 'men' else 'women'}'s simulation {sim}")
    }
    
    # Simulate races for each discipline
    for(discipline in names(gender_races)) {
      n_races <- gender_races[[discipline]]
      if(n_races > 0) {
        for(i in 1:n_races) {
          results <- simulate_race_with_probability_skijump(gender_histories, discipline, gender_probabilities)
          season_results[sim,] <- season_results[sim,] + results
        }
      }
    }
  }
  
  # Calculate winning probabilities
  winners <- apply(season_results, 1, which.max)
  win_probs <- table(all_skiers[winners]) / n_simulations
  
  # Sort by probability
  win_probs <- sort(win_probs, decreasing = TRUE)
  
  # Enhanced summary with point differentials
  final_points_summary <- data.frame(
    Skier = all_skiers,
    Gender = gender,
    Current_Points = season_results[1,],
    Mean_Final_Points = colMeans(season_results),
    Mean_Points_Gained = colMeans(season_results) - season_results[1,],
    Win_Prob = ifelse(all_skiers %in% names(win_probs), 
                      win_probs[all_skiers], 0)
  ) %>%
    arrange(desc(Mean_Final_Points))
  
  return(list(
    probabilities = win_probs,
    simulation_results = season_results,
    summary = final_points_summary
  ))
}

# Create standings summary function for ski jumping
create_standings_summary_skijump <- function(simulation_results, standings_df, gender) {
  # First replace any NaN values with 0
  simulation_results[is.nan(simulation_results)] <- 0
  
  # Calculate ranks for all simulations at once
  ranks_per_sim <- t(apply(-simulation_results, 1, rank))
  
  # Get current standings
  current_standings <- standings_df %>%
    filter(Skier %in% colnames(simulation_results)) %>%
    dplyr::select(Skier, Points) %>%
    arrange(desc(Points))
  
  # Calculate summary statistics
  simulation_summary <- data.frame(
    Skier = colnames(simulation_results),
    
    # Basic statistics
    Simulated_Standings = colMeans(simulation_results, na.rm = TRUE),
    Low_Range = apply(simulation_results, 2, min, na.rm = TRUE),
    High_Range = apply(simulation_results, 2, max, na.rm = TRUE),
    
    # Win percentage (finishing first)
    Win_Pct = apply(ranks_per_sim, 2, function(x) mean(x == 1, na.rm = TRUE) * 100),
    
    # Top N percentages
    Top3_Pct = apply(ranks_per_sim, 2, function(x) mean(x <= 3, na.rm = TRUE) * 100),
    Top5_Pct = apply(ranks_per_sim, 2, function(x) mean(x <= 5, na.rm = TRUE) * 100),
    Top10_Pct = apply(ranks_per_sim, 2, function(x) mean(x <= 10, na.rm = TRUE) * 100)
  )
  
  # Create final table with clean formatting
  final_table <- current_standings %>%
    left_join(simulation_summary, by = "Skier") %>%
    # Add this line to get ID from the standings dataframe
    left_join(standings_df %>% select(Skier, ID), by = "Skier") %>%
    # Reorder columns to put ID after Skier
    select(Skier, ID, everything()) %>%
    arrange(desc(Simulated_Standings)) %>%
    rename(
      "Current Standings" = Points,
      "Simulated Standings" = Simulated_Standings,
      "Low Range" = Low_Range,
      "High Range" = High_Range,
      "Win%" = Win_Pct,
      "Top 3%" = Top3_Pct,
      "Top 5%" = Top5_Pct,
      "Top 10%" = Top10_Pct
    ) %>%
    mutate(
      across(c("Current Standings", "Simulated Standings", "Low Range", "High Range"),
             ~round(as.numeric(.), 1)),
      across(c("Win%", "Top 3%", "Top 5%", "Top 10%"),
             ~round(as.numeric(.), 2))
    )
  
  # Create GMT date-based directory if it doesn't exist
  output_dir <- get_output_dir()
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  return(final_table)
}

# Set up logging
log_file <- file.path(get_output_dir(), "skijump_simulation_log.log")
log_appender(appender_file(log_file))

# Step 1: Create race probability table (pre-compute probabilities for faster simulation)
race_probability_table <- create_race_probability_table_skijump(men_chrono, ladies_chrono, top_30_men, top_30_women)

# Step 2: Run simulations using pre-computed probabilities
men_sims <- simulate_season_skijump(
  all_histories_clean, 
  standings_df, 
  "M", 
  500, 
  remaining_races_by_gender, 
  race_probability_table
)

women_sims <- simulate_season_skijump(
  all_histories_clean, 
  ladies_standings_df, 
  "L", 
  500, 
  remaining_races_by_gender, 
  race_probability_table
)

# Create summaries for both genders
men_summary <- create_standings_summary_skijump(men_sims$simulation_results, standings_df, "M")
women_summary <- create_standings_summary_skijump(women_sims$simulation_results, ladies_standings_df, "L")

# Save all outputs
output_dir <- get_output_dir()
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save prediction results
write_xlsx(final_predictions, file.path(output_dir, "skijump_predictions.xlsx"))

# Save simulation results
write_xlsx(men_summary, file.path(output_dir, "men_standings_predictions.xlsx"))
write_xlsx(women_summary, file.path(output_dir, "ladies_standings_predictions.xlsx"))

# SKI JUMPING - MAGIC NUMBERS IMPLEMENTATION
# Add this to the end of the Ski Jumping race-recap file (after the simulation results)

# Function to calculate total points remaining for Ski Jumping
calculate_total_points_remaining_skijump <- function(gender_races) {
  # Ski Jumping: Max points per race = 100 (from individual_points system)
  total_races <- sum(unlist(gender_races))
  return(total_races * 100)
}

# Function to calculate magic numbers for Ski Jumping championship elimination
calculate_magic_numbers_skijump <- function(standings_df, remaining_races_by_gender, gender = "M") {
  
  leader_points <- max(standings_df$Points, na.rm = TRUE)
  gender_races <- remaining_races_by_gender[[gender]]
  total_points_remaining <- sum(unlist(gender_races)) * 100  # Ski Jumping max = 100
  
  magic_numbers <- standings_df %>%
    filter(Points > 0) %>%
    mutate(
      Current_Place = rank(desc(Points), ties.method = "min"),
      Magic_Number = pmax(0, Points + total_points_remaining + 1 - leader_points),
      Max_Possible_Points = Points + total_points_remaining,
      Mathematical_Chance = Max_Possible_Points > leader_points
    ) %>%
    filter(Mathematical_Chance == TRUE) %>%
    select(Skier, ID, Current_Place, Points, Magic_Number) %>%
    arrange(Current_Place)
  
  return(magic_numbers)
}

# Calculate magic numbers for both genders
men_magic_numbers <- calculate_magic_numbers_skijump(standings_df, remaining_races_by_gender, "M")
women_magic_numbers <- calculate_magic_numbers_skijump(ladies_standings_df, remaining_races_by_gender, "L")

# Save to Excel files
output_dir <- get_output_dir()
write_xlsx(men_magic_numbers, file.path(output_dir, "men_magic_numbers.xlsx"))
write_xlsx(women_magic_numbers, file.path(output_dir, "ladies_magic_numbers.xlsx"))
