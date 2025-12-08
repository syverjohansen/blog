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
men_chrono_path <- "~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv"
ladies_chrono_path <- "~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv"
races_path <- "~/ski/elo/python/ski/polars/excel365/races.csv"
base_dir <- "~/blog/daehl-e"


# Function to get the current date in GMT formatted as YYYYMMDD
get_gmt_date_formatted <- function() {
  format(as.POSIXct(Sys.time(), tz = "GMT"), "%Y%m%d")
}

# Define the base output directory function that uses the GMT date
get_output_dir <- function() {
  date_str <- get_gmt_date_formatted()
  file.path("~/blog/daehl-e/content/post/cross-country/drafts/weekly-recap", date_str)
}


# Read the CSV files
men_chrono <- read_csv(men_chrono_path)

ladies_chrono <- read_csv(ladies_chrono_path)



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
# Update in the generate_weekly_elo_change function
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
  
  # Create a top gainers/losers summary
  top_gainers <- elo_changes %>%
    arrange(desc(Elo_Change)) %>%
    head(10)
  
  top_losers <- elo_changes %>%
    arrange(Elo_Change) %>%
    head(10)
  
  # Save top gainers/losers
  #write_xlsx(top_gainers, file.path(week_dir, paste0(gender, "_top_gainers.xlsx")))
  #write_xlsx(top_losers, file.path(week_dir, paste0(gender, "_top_losers.xlsx")))
  
  # Return the full dataframe
  return(elo_changes)
}

# Generate weekly Elo change reports
men_elo_changes <- generate_weekly_elo_change(men_chrono, "men", base_dir)
women_elo_changes <- generate_weekly_elo_change(ladies_chrono, "ladies", base_dir)


# Points system setup
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
stage_points <- c(50,47,44,41,38,35,32,30,28,26,24,22,20,18,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
tds_points <- c(300,285,270,255,240,216,207,198,189,180,174,168,162,156,150,144,138,132,126,120,114,108,102,96,90,84,78,72,66,60,57,54,51,48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3)

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

# Create points conversion function that handles all three point systems

create_points_columns <- function(df) {
  df %>%
    mutate(
      Points = map_dbl(Place, ~if(.x <= length(wc_points)) wc_points[.x] else 0),
      Stage_Points = map_dbl(Place, ~if(.x <= length(stage_points)) stage_points[.x] else 0),
      TdS_Points = map_dbl(Place, ~if(.x <= length(tds_points)) tds_points[.x] else 0)
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


# Modified version of weighted average calculation specifically for TDS
calculate_weighted_average_tds <- function(df) {

  df %>%
    group_by(ID) %>%
    arrange(Season, Race) %>%
    mutate(
      Weighted_Last_5 = sapply(row_number(), function(i) {
        # Use TdS_Points instead of Points
        prev_races <- TdS_Points[max(1, i-3):(i-1)]
        if(length(prev_races) > 0) {
          weights <- seq(1,length(prev_races))
          weighted.mean(prev_races, weights, na.rm = TRUE)
        } else {
          NA_real_
        }
      })
    ) %>%
    ungroup()
}

calculate_weighted_average2_tds <- function(df) {
  df %>%
    group_by(ID) %>%
    arrange(Season, Race) %>%
    mutate(
      Weighted_Last_5_2 = sapply(row_number(), function(i) {
        # Use TdS_Points instead of Points
        prev_races <- TdS_Points[max(1, i-2):(i)]
        if(length(prev_races) > 0) {
          weights <- seq(1,length(prev_races))
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

# Function to select best features and create GAM with specified points type
select_gam_features <- function(df, points_col = "Points") {
  # Get predictor columns (all Pelo percentages and weighted average)
  predictors <- c(
    names(df)[grepl("Pelo_Pct$", names(df))],
    "Weighted_Last_5"
  )
  
  df <- df %>%
    mutate(across(
      all_of(predictors),
      replace_na_with_quartile
    ))
    
  # Create formula for feature selection using specified points column
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

# Function to generate predictions for a specific race type
generate_race_predictions <- function(chrono_data, race_type, points_type = "wc", gender) {
  points_col <- if(points_type == "wc") "Points" else "Stage_Points"
  # Define filter conditions based on race type
  filter_conditions <- list(
    "Distance" = list(
      distance_filter = "Distance != 'Sprint'",
      technique_filter = "TRUE"
    ),
    "Distance_C" = list(
      distance_filter = "Distance != 'Sprint'",
      technique_filter = "Technique == 'C'"
    ),
    "Distance_F" = list(
      distance_filter = "Distance != 'Sprint'",
      technique_filter = "Technique == 'F'"
    ),
    "Sprint_C" = list(
      distance_filter = "Distance == 'Sprint'",
      technique_filter = "Technique == 'C'"
    ),
    "Sprint_F" = list(
      distance_filter = "Distance == 'Sprint'",
      technique_filter = "Technique == 'F'"
    )
  )
  
  conditions <- filter_conditions[[race_type]]
  
    # Define mapping between race types and their corresponding Pelo_Pct columns
    pelo_pct_mapping <- list(
        "Distance" = "Distance_Pelo_Pct",
        "Distance_C" = "Distance_C_Pelo_Pct",
        "Distance_F" = "Distance_F_Pelo_Pct",
        "Sprint_C" = "Sprint_C_Pelo_Pct",
        "Sprint_F" = "Sprint_F_Pelo_Pct"
    )
    
    # Get the corresponding Pelo_Pct column for this race type
    pelo_pct_col <- pelo_pct_mapping[[race_type]]
    
    # Process and filter training data
    train_df <- chrono_data %>%
        filter(
            City != "Summer",
            City!="Tour De Ski",
            #Season > max(Season)-11,
            eval(parse(text = conditions$distance_filter)),
            eval(parse(text = conditions$technique_filter))
        ) %>%
        create_points_columns() %>%
        group_by(ID) %>%
        arrange(Season, Race) %>%
        mutate(
            Weighted_Last_5 = sapply(row_number(), function(i) {
                prev_races <- get(points_col)[max(1, i-5):(i-1)]
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
                prev_races <- get(points_col)[max(1, i-4):(i-0)]
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
  filter(!!sym(pelo_pct_mapping[[race_type]]) > 0.75)
  
  
  # Build model
  model_results <- select_gam_features(train_df, points_col)
  
  # Process test data
  test_df <- chrono_data %>%
    filter(
      City == "Summer",
      Season == max(Season)
    )
  
  # Update Elo columns
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
  
  return(list(
    predictions = gam_df,
    model = model_results,
    feature_summary = paste("Selected features for", gender, race_type, ":", 
                          paste(model_results$selected_features, collapse = ", "))
  ))
}



# Create separate function for TDS predictions
generate_tds_predictions <- function(chrono_data, gender) {
  print(chrono_data %>%
          filter(City == "Tour De Ski"))
  
    # Process TDS training data
      train_df <- chrono_data %>%
          filter(
              City == "Tour De Ski",
              #Season > max(Season)-11
          ) %>%
          create_points_columns() %>%
          # Store original Points column
          rename(Original_Points = Points) %>%
          # Use TdS_Points as main Points column for weighted average calculations
          mutate(Points = TdS_Points) %>%
          calculate_weighted_average_tds() %>%
          calculate_weighted_average2_tds() %>%
          create_pelo_percentages() %>%
          filter(Pelo_Pct > 0.75) %>%
          filter(Season > max(Season)-11)
    print(train_df)
      
      # Build TDS-specific model using TdS_Points as response
      model_results <- select_gam_features(train_df %>% 
                                         mutate(Points = TdS_Points))

  # Process test data
  test_df <- chrono_data %>%
    filter(
      City == "Summer",
      Season == max(Season)
    )
  
  # Update Elo columns
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

    # Return predictions
    gam_df <- test_df_with_pct %>%
        mutate(
            predicted_tds_points = predict(model_results$gam_model, newdata = .),
            race_type = "TDS"
        ) %>%
        dplyr::select(Skier, ID, predicted_tds_points) %>%
        arrange(desc(predicted_tds_points))

    return(list(
        predictions = gam_df,
        model = model_results,
        feature_summary = paste("Selected features for TDS for ", gender,
                              paste(model_results$selected_features, collapse = ", "))
    ))
}

# Generate predictions for both genders
race_types <- c("Distance", "Distance_C", "Distance_F", "Sprint_C", "Sprint_F")
all_predictions <- list()
all_models <- list()

# Generate predictions for men
for(race_type in race_types) {
  cat("\nProcessing Men's WC", race_type, "...\n")
  results <- generate_race_predictions(men_chrono, race_type, "wc", "M")
  all_predictions[[paste0("M_WC_", race_type)]] <- results$predictions
  all_models[[paste0("M_WC_", race_type)]] <- results$model
  
  cat("\nProcessing Men's Stage", race_type, "...\n")
  results <- generate_race_predictions(men_chrono, race_type, "stage", "M")
  all_predictions[[paste0("M_Stage_", race_type)]] <- results$predictions
  all_models[[paste0("M_Stage_", race_type)]] <- results$model
}

print(all_predictions)


# Generate predictions for women
for(race_type in race_types) {
  cat("\nProcessing Women's WC", race_type, "...\n")
  results <- generate_race_predictions(ladies_chrono, race_type, "wc", "L")
  all_predictions[[paste0("L_WC_", race_type)]] <- results$predictions
  all_models[[paste0("L_WC_", race_type)]] <- results$model
  
  cat("\nProcessing Women's Stage", race_type, "...\n")
  results <- generate_race_predictions(ladies_chrono, race_type, "stage", "L")
  all_predictions[[paste0("L_Stage_", race_type)]] <- results$predictions
  all_models[[paste0("L_Stage_", race_type)]] <- results$model
}

# TDS predictions for both genders
cat("\nProcessing Men's Tour De Ski...\n")
mens_tds_results <- generate_tds_predictions(men_chrono, "M")
all_predictions[["M_TDS"]] <- mens_tds_results$predictions
all_models[["M_TDS"]] <- mens_tds_results$model


cat("\nProcessing Women's Tour De Ski...\n")
womens_tds_results <- generate_tds_predictions(ladies_chrono, "L")
all_predictions[["L_TDS"]] <- womens_tds_results$predictions
all_models[["L_TDS"]] <- womens_tds_results$model



# Combine and format predictions
# First standardize all prediction columns
for(pred_name in names(all_predictions)) {
  if(grepl("TDS", pred_name)) {
    # Handle TDS predictions
    all_predictions[[pred_name]] <- all_predictions[[pred_name]] %>%
      mutate(
        wc_predicted_points = NA_real_,
        stage_predicted_points = NA_real_,
        gender = substr(pred_name, 1, 1),
        race_type = "TDS"
      )
  } else if(grepl("WC", pred_name)) {
    # Handle World Cup predictions
    all_predictions[[pred_name]] <- all_predictions[[pred_name]] %>%
      rename(wc_predicted_points = predicted_points) %>%
      mutate(
        stage_predicted_points = NA_real_,
        predicted_tds_points = NA_real_
      )
  } else if(grepl("Stage", pred_name)) {
    # Handle Stage predictions
    all_predictions[[pred_name]] <- all_predictions[[pred_name]] %>%
      rename(stage_predicted_points = predicted_points) %>%
      mutate(
        wc_predicted_points = NA_real_,
        predicted_tds_points = NA_real_
      )
  }
}

# Now bind all predictions together and summarize
final_predictions <- bind_rows(all_predictions) %>%
  group_by(Skier, ID, gender) %>%
  summarise(
    race_type = first(race_type),
    wc_predicted_points = max(wc_predicted_points, na.rm = TRUE),
    stage_predicted_points = max(stage_predicted_points, na.rm = TRUE),
    tds_predicted_points = max(predicted_tds_points, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Replace -Inf values (from max(na.rm=TRUE) when all values are NA) with NA
  mutate(across(ends_with("_points"), 
                ~ifelse(is.infinite(.), NA_real_, .)))

final_predictions
all_predictions

library(arrow)

# Function to get race history for any point system
# Fix the points conversion function first
place_to_points <- Vectorize(function(place) {
    if (is.na(place)) return(0)
    if (place <= length(wc_points)) wc_points[place] else 0
})

stage_to_points <- Vectorize(function(place) {
    if (is.na(place)) return(0)
    if (place <= length(stage_points)) stage_points[place] else 0
})

tds_to_points <- Vectorize(function(place) {
    if (is.na(place)) return(0)
    if (place <= length(tds_points)) tds_points[place] else 0
})

# Modified create_points_columns function
create_points_columns <- function(df) {
    df %>%
        mutate(
            Points = place_to_points(Place),
            Stage_Points = stage_to_points(Place),
            TdS_Points = tds_to_points(Place)
        )
}

# Function to calculate race participation probabilities for simulation
calculate_skier_race_probabilities <- function(chrono_data, skier_name) {
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
  
  # Function to get race probability for a specific race type
  get_race_type_probability <- function(race_type, technique = NULL) {
    # Define race filtering logic
    if(race_type == "TDS") {
      # For Tour de Ski
      all_races <- chrono_data %>%
        filter(
          City == "Tour De Ski",
          Date >= start_date
        ) %>%
        distinct(Date, Race)  # Count individual TDS stages
      
      skier_races <- chrono_data %>%
        filter(
          City == "Tour De Ski",
          Date >= start_date,
          Skier == skier_name
        ) %>%
        distinct(Date, Race)
      
    } else if(race_type == "Sprint") {
      # For Sprint races
      all_races <- chrono_data %>%
        filter(
          Distance == "Sprint",
          Date >= start_date,
          City != "Summer",
          if(!is.null(technique)) Technique == technique else TRUE
        ) %>%
        distinct(Date, Race)
      
      skier_races <- chrono_data %>%
        filter(
          Distance == "Sprint",
          Date >= start_date,
          City != "Summer",
          Skier == skier_name,
          if(!is.null(technique)) Technique == technique else TRUE
        ) %>%
        distinct(Date, Race)
      
    } else {
      # For Distance races
      all_races <- chrono_data %>%
        filter(
          Distance != "Sprint",
          Date >= start_date,
          City != "Summer",
          City != "Tour De Ski",
          if(!is.null(technique)) Technique == technique else TRUE
        ) %>%
        distinct(Date, Race)
      
      skier_races <- chrono_data %>%
        filter(
          Distance != "Sprint",
          Date >= start_date,
          City != "Summer", 
          City != "Tour De Ski",
          Skier == skier_name,
          if(!is.null(technique)) Technique == technique else TRUE
        ) %>%
        distinct(Date, Race)
    }
    
    total_races <- nrow(all_races)
    
    if(total_races == 0) {
      return(0.5)  # Default probability if no historical data
    }
    
    races_participated <- nrow(skier_races)
    prob <- min(1, races_participated / total_races)
    
    log_debug("Probability for {skier_name} in {race_type} {technique}: {prob} ({races_participated}/{total_races})")
    
    return(prob)
  }
  
  # Calculate probabilities for all race types
  probabilities <- list(
    Distance = get_race_type_probability("Distance"),
    Distance_C = get_race_type_probability("Distance", "C"),
    Distance_F = get_race_type_probability("Distance", "F"),
    Sprint_C = get_race_type_probability("Sprint", "C"),
    Sprint_F = get_race_type_probability("Sprint", "F"),
    TDS = get_race_type_probability("TDS")
  )
  
  return(probabilities)
}

create_race_probability_table <- function(men_chrono, ladies_chrono, top_30_men, top_30_women) {
  log_info("Pre-calculating race probabilities for all skiers")
  
  # Function to calculate probabilities for a single skier
  calculate_single_skier_probabilities <- function(chrono_data, skier_name) {
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
    
    # Function to get race probability for a specific race type
    get_race_type_probability <- function(race_type, technique = NULL) {
      if(race_type == "TDS") {
        # For Tour de Ski
        all_races <- chrono_data %>%
          filter(
            City == "Tour De Ski",
            Date >= start_date
          ) %>%
          distinct(Date, Race)
        
        skier_races <- chrono_data %>%
          filter(
            City == "Tour De Ski",
            Date >= start_date,
            Skier == skier_name
          ) %>%
          distinct(Date, Race)
        
      } else if(race_type == "Sprint") {
        # For Sprint races
        all_races <- chrono_data %>%
          filter(
            Distance == "Sprint",
            Date >= start_date,
            City != "Summer",
            if(!is.null(technique)) Technique == technique else TRUE
          ) %>%
          distinct(Date, Race)
        
        skier_races <- chrono_data %>%
          filter(
            Distance == "Sprint",
            Date >= start_date,
            City != "Summer",
            Skier == skier_name,
            if(!is.null(technique)) Technique == technique else TRUE
          ) %>%
          distinct(Date, Race)
        
      } else {
        # For Distance races
        all_races <- chrono_data %>%
          filter(
            Distance != "Sprint",
            Date >= start_date,
            City != "Summer",
            City != "Tour De Ski",
            if(!is.null(technique)) Technique == technique else TRUE
          ) %>%
          distinct(Date, Race)
        
        skier_races <- chrono_data %>%
          filter(
            Distance != "Sprint",
            Date >= start_date,
            City != "Summer", 
            City != "Tour De Ski",
            Skier == skier_name,
            if(!is.null(technique)) Technique == technique else TRUE
          ) %>%
          distinct(Date, Race)
      }
      
      total_races <- nrow(all_races)
      
      if(total_races == 0) {
        return(0.5)  # Default probability if no historical data
      }
      
      races_participated <- nrow(skier_races)
      prob <- min(1, races_participated / total_races)
      
      return(prob)
    }
    
    # Calculate probabilities for all race types
    return(data.frame(
      Skier = skier_name,
      Distance = get_race_type_probability("Distance"),
      Distance_C = get_race_type_probability("Distance", "C"),
      Distance_F = get_race_type_probability("Distance", "F"),
      Sprint_C = get_race_type_probability("Sprint", "C"),
      Sprint_F = get_race_type_probability("Sprint", "F"),
      TDS = get_race_type_probability("TDS"),
      stringsAsFactors = FALSE
    ))
  }
  
  # Calculate for all men
  men_probabilities <- do.call(rbind, lapply(top_30_men, function(skier) {
    cat("Calculating probabilities for male skier:", skier, "\n")
    calculate_single_skier_probabilities(men_chrono, skier)
  }))
  men_probabilities$Gender <- "M"
  
  # Calculate for all women  
  women_probabilities <- do.call(rbind, lapply(top_30_women, function(skier) {
    cat("Calculating probabilities for female skier:", skier, "\n")
    calculate_single_skier_probabilities(ladies_chrono, skier)
  }))
  women_probabilities$Gender <- "L"
  
  # Combine into single table
  all_probabilities <- rbind(men_probabilities, women_probabilities)
  
  log_info("Race probability table created with {nrow(all_probabilities)} skiers")
  
  # Save to file for future use
  output_dir <- get_output_dir()
  write_xlsx(all_probabilities, file.path(output_dir, "race_probabilities.xlsx"))
  
  return(all_probabilities)
}
get_race_history <- function(skier_id, race_type, predictions_list, points_type = "wc", n_required = 10, gender="M") {
  chrono_data <- if(gender == "M") men_chrono else ladies_chrono
    # First create points columns in men_chrono
    chrono_with_points <- chrono_data %>%
        create_points_columns()

    # Select appropriate points column based on type
    points_col <- case_when(
        points_type == "wc" ~ "Points",
        points_type == "stage" ~ "Stage_Points",
        points_type == "tds" ~ "TdS_Points"
    )
    
    # Get actual results based on race type
    real_results <- chrono_with_points %>%
        filter(
            Skier == skier_id,
            City != "Summer",
            case_when(
                race_type == "Distance_C" ~ Distance != "Sprint" & Technique == "C",
                race_type == "Distance_F" ~ Distance != "Sprint" & Technique == "F",
                race_type == "Sprint_C" ~ Distance == "Sprint" & Technique == "C",
                race_type == "Sprint_F" ~ Distance == "Sprint" & Technique == "F",
                race_type == "Distance" ~ Distance != "Sprint",
                race_type == "TDS" ~ City == "Tour De Ski",
                TRUE ~ FALSE
            ),
            Season > max(Season)-11
        ) %>%
        arrange(desc(Date)) %>%
        dplyr::slice_head(n = n_required) %>%
        pull(!!sym(points_col))
      

    if(race_type=="TDS"){
      n_required = 5
    }
    
    print(race_type)
    # If we have enough results, return them
    if(length(real_results) >= n_required) {
      print("Met the requirement")
        return(real_results)
    }
    else{
      print("The skier has participated in: ")
      print(length(real_results))
    }
    
   # Get predicted score from all_predictions
    pred_type <- if(points_type == "wc") {
        paste0(gender, "_WC_", race_type)  # Add gender prefix
    } else if(points_type == "stage") {
        paste0(gender, "_Stage_", race_type)  # Add gender prefix
    } else {
        paste0(gender, "_TDS")  # Add gender prefix
    }
    
    # Select correct prediction column
    pred_col <- if(points_type == "tds") {
        "predicted_tds_points"
    } else if(points_type == "wc") {
        "wc_predicted_points"
    } else {
        "stage_predicted_points"
    }
    
    

    predicted_score <- predictions_list[[pred_type]] %>%
        filter(Skier == skier_id) %>%
        pull(!!sym(pred_col))
    
    # Rest of the function remains the same
    if(length(real_results) > 1) {
        variation_sd <- sd(unlist(real_results))
    } else {
        variation_sd <- predicted_score * 0.15
    }
    
    n_to_generate <- n_required - length(real_results)

    generated_results <- numeric(n_to_generate)
    
    max_points <- case_when(
        points_type == "wc" ~ 100,
        points_type == "stage" ~ 50,
        points_type == "tds" ~ 300
    )
    
    for(i in 1:n_to_generate) {
        new_score <- predicted_score + rnorm(1, 0, variation_sd)
        generated_results[i] <- pmin(pmax(round(new_score), 0), max_points)
    }
    
    all_results <- c(real_results, generated_results)
    return(all_results)

}
standings_df <- read_csv("~/ski/elo/python/ski/polars/excel365/men_standings.csv")
ladies_standings_df <- read_csv("~/ski/elo/python/ski/polars/excel365/ladies_standings.csv")



# Get all 2025 skiers
# skiers_2025 <- men_chrono %>%
#   filter(Season == 2025, City == "Summer") %>%
#   distinct(Skier) %>%
#   pull(Skier)
top_30_men <- standings_df %>%
    arrange(desc(Points)) %>%
    head(30) %>%
    pull(Skier)

top_30_women <- ladies_standings_df %>%
    arrange(desc(Points)) %>%
    head(30) %>%
    pull(Skier)

# Create storage for all histories
all_histories <- list()
print(all_predictions)
# Process men
for(skier in top_30_men) {
  cat("\nProcessing male skier:", skier, "\n")
  
  all_histories[[skier]] <- list()
  
  for(race_type in c("Distance", "Distance_C", "Distance_F", "Sprint_C", "Sprint_F")) {
    all_histories[[skier]][[paste0("WC_", race_type)]] <- 
      get_race_history(skier, race_type, all_predictions, "wc", gender = "M")
    
    all_histories[[skier]][[paste0("Stage_", race_type)]] <- 
      get_race_history(skier, race_type, all_predictions, "stage", gender = "M")
  }
  
  all_histories[[skier]][["TDS"]] <- 
    get_race_history(skier, "TDS", all_predictions, "tds", 3, gender = "M")
}

# Process women
for(skier in top_30_women) {
  cat("\nProcessing female skier:", skier, "\n")
  
  all_histories[[skier]] <- list()
  
  for(race_type in c("Distance", "Distance_C", "Distance_F", "Sprint_C", "Sprint_F")) {
    all_histories[[skier]][[paste0("WC_", race_type)]] <- 
      get_race_history(skier, race_type, all_predictions, "wc", gender = "L")
    
    all_histories[[skier]][[paste0("Stage_", race_type)]] <- 
      get_race_history(skier, race_type, all_predictions, "stage", gender = "L")
  }
  
  all_histories[[skier]][["TDS"]] <- 
    get_race_history(skier, "TDS", all_predictions, "tds", 3, gender = "L")
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
library(logger)
library(dplyr)
library(arrow)
library(writexl)
#gc()
# Get current loaded packages
loaded_packages <- (.packages())

# Store histories
#temp_histories <- all_histories_clean

# Clear everything EXCEPT temp_histories
#rm(list = setdiff(ls(), "temp_histories"))

# Restore our saved object
#all_histories_clean <- temp_histories

# Remove our temporary variable
#rm(temp_histories)


# Load standings for both genders
standings_df <- read_csv("~/ski/elo/python/ski/polars/excel365/men_standings.csv")
ladies_standings_df <- read_csv("~/ski/elo/python/ski/polars/excel365/ladies_standings.csv")

# Set up logging
log_file <- "~/blog/daehl-e/content/post/cross-country/drafts/weekly-recap/simulation_log.log"
log_appender(appender_file(log_file))

create_race_distribution <- function(race_results, n_simulations = 1, max_points = 100) {
    points <- race_results$Points
    weights <- race_results$Weight
    n_actual_races <- length(points)
    #gc()
    # If very few points, use simple method
    if (n_actual_races < 2) {
        #log_info("Using simple method - too few points")
        mean_points <- weighted.mean(points, weights)
        sd_points <- max(5, mean_points * 0.15)
        simulated_points <- rnorm(n_simulations, mean_points, sd_points)
        return(pmin(pmax(round(simulated_points), 0), max_points))
    }
    
    # Calculate proportion of maximum points
    prop_max <- weighted.mean(points == max_points, weights)
    #log_info("Prop max: {prop_max}")
    
    # Initialize simulated points
    simulated_points <- numeric(n_simulations)
    
    # Determine number of max point races
    n_max <- rbinom(1, n_simulations, prop_max)
    if(n_max > 0) {
        simulated_points[1:n_max] <- max_points
    }
    
    #log_info("n_max: {n_max}")
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
        
        #log_info("non_max_points: {non_max_points}")
        #log_info("non_max_weights: {non_max_weights}")
        
        # Initialize remaining_points
        remaining_points <- numeric(n_simulations - n_max)
        
        # Try KDE method first
        kde_success <- tryCatch({
            if(length(unique(non_max_points)) < 3) stop("Too few unique points for KDE")
            
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
                    #log_info("Sampling error: falling back to uniform sampling")
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

fake_data <- data.frame(
    Points = c(45, 132, 204, 190, 245),
    Weight = seq(5, 1)
)
simulated_points <- create_race_distribution(fake_data, n_simulations = 1)


# Modified function to calculate remaining races based on UTC time and proper race categorization
# Corrected function to count unique race types for season simulation
calculate_remaining_races <- function(races_file) {
  # Read races file
  races <- read.csv(races_file, stringsAsFactors = FALSE)
  
  # Convert dates to Date objects (assuming MM/DD/YYYY format)
  races$Date <- as.Date(races$Date, format = "%m/%d/%Y")
  
  # Get current date in UTC
  today_utc <- as.Date(Sys.time(), tz = "UTC")
  
  # Filter for remaining races (today or after) and exclude Mixed sex races
  remaining <- races %>%
    filter(Date >= today_utc, Sex %in% c("M", "L"), Championship!=1)
  
  # Initialize counts for both genders
  counts <- list(
    M = list(
      WC = list(
        Distance = 0,
        Distance_C = 0,
        Distance_F = 0,
        Sprint_C = 0,
        Sprint_F = 0
      ),
      Stage = list(
        Distance = 0,
        Distance_C = 0,
        Distance_F = 0,
        Sprint_C = 0,
        Sprint_F = 0
      ),
      TDS = 0
    ),
    L = list(
      WC = list(
        Distance = 0,
        Distance_C = 0,
        Distance_F = 0,
        Sprint_C = 0,
        Sprint_F = 0
      ),
      Stage = list(
        Distance = 0,
        Distance_C = 0,
        Distance_F = 0,
        Sprint_C = 0,
        Sprint_F = 0
      ),
      TDS = 0
    )
  )
  
  if(nrow(remaining) == 0) {
    return(counts)
  }
  
  # Create race type categories for each race
  remaining_categorized <- remaining %>%
    # First filter out relay and team sprint races
    filter(!Distance %in% c("Rel", "Ts")) %>%
    mutate(
      # Determine race category
      is_sprint = (Distance == "Sprint"),
      is_distance = !is_sprint,  # Individual distance races (20, 10, 50, etc.)
      is_world_cup = (Stage == 0),
      is_stage_race = (Stage == 1),
      is_tds = (City=="Tour de Ski"),
      
      # Clean technique (handle NAs and empty strings)
      technique_clean = case_when(
        is.na(Technique) | Technique == "" ~ "",
        TRUE ~ Technique
      ),
      
      # Create race type categories
      race_category = case_when(
        is_tds ~ "TDS",
        is_sprint & technique_clean == "C" & is_world_cup ~ "WC_Sprint_C",
        is_sprint & technique_clean == "F" & is_world_cup ~ "WC_Sprint_F",
        is_sprint & technique_clean == "C" & is_stage_race ~ "Stage_Sprint_C",
        is_sprint & technique_clean == "F" & is_stage_race ~ "Stage_Sprint_F",
        is_distance & technique_clean == "" & is_world_cup ~ "WC_Distance",
        is_distance & technique_clean == "C" & is_world_cup ~ "WC_Distance_C",
        is_distance & technique_clean == "F" & is_world_cup ~ "WC_Distance_F",
        is_distance & technique_clean == "" & is_stage_race ~ "Stage_Distance",
        is_distance & technique_clean == "C" & is_stage_race ~ "Stage_Distance_C",
        is_distance & technique_clean == "F" & is_stage_race ~ "Stage_Distance_F",
        TRUE ~ "Other"
      )
    )
  
  # Debug: Show categorization
  cat("Race categorization (excluding Rel/Ts):\n")
  print(remaining_categorized %>% 
          select(Sex, Distance, Technique, Stage, Final_Climb, race_category) %>%
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
      category_count <- sum(gender_races$race_category == category)
      
      # Map to counts structure
      if(category == "TDS") {
        counts[[gender]]$TDS <- counts[[gender]]$TDS + category_count
      } else if(grepl("^WC_", category)) {
        race_type <- gsub("^WC_", "", category)
        if(race_type %in% names(counts[[gender]]$WC)) {
          counts[[gender]]$WC[[race_type]] <- counts[[gender]]$WC[[race_type]] + category_count
        }
      } else if(grepl("^Stage_", category)) {
        race_type <- gsub("^Stage_", "", category)
        if(race_type %in% names(counts[[gender]]$Stage)) {
          counts[[gender]]$Stage[[race_type]] <- counts[[gender]]$Stage[[race_type]] + category_count
        }
      }
    }
  }
  
  return(counts)
}

# Calculate remaining races dynamically
races_path <- "~/ski/elo/python/ski/polars/excel365/races.csv"
remaining_races_by_gender <- calculate_remaining_races(races_path)
print(remaining_races_by_gender)

remaining_races <- remaining_races_by_gender$M


# Function to simulate a single race for all skiers
simulate_race <- function(histories, race_type, points_type = "wc") {
  #log_info("Simulating {points_type} race of type {race_type}")
  
  # Set max points based on race type
  max_points <- if(points_type == "wc") 100 else if(points_type == "stage") 50 else 300
  
  # Get appropriate race histories for each skier
  race_key <- if(points_type == "tds") {
    "TDS"  # TDS races have their own key
  } else {
    paste0(if(points_type == "wc") "WC_" else "Stage_", race_type)
  }
  
  results <- sapply(names(histories), function(skier) {
    race_history <- histories[[skier]][[race_key]]
    
    # Check for valid history
    if(is.null(race_history) || length(race_history) == 0) {
      log_info("No history for {skier} in {race_key}")
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
        simulated_points <- create_race_distribution(fake_data, n_simulations = 1, max_points = max_points)
        return(simulated_points[1])
      }, error = function(e) {
        log_info("{skier}'s race_distribution failed")
        
        # If distribution creation fails, use simple mean with variation
        mean_points <- mean(race_history)
        result <- mean_points + rnorm(1, 0, max(5, mean_points * 0.15))
        return(max(0, min(max_points, round(result))))
      })
    } else {
      log_info("Not enough history for {skier} in {race_key}")
      mean_points <- mean(race_history)
      if(is.na(mean_points) || is.nan(mean_points)) {
        log_info("Invalid mean for {skier} in {race_key}")
        return(0)
      }
      result <- mean_points + rnorm(1, 0, max(5, mean_points * 0.15))
      return(max(0, min(max_points, round(result))))
    }
  })
  
  # Final check for any NAs or NaNs
  results[is.na(results) | is.nan(results)] <- 0
  return(results)
}

simulate_race_with_probability <- function(histories, race_type, points_type = "wc", probability_table) {
  log_info("Simulating {points_type} race of type {race_type} with pre-computed probabilities")
  
  # Set max points based on race type
  max_points <- if(points_type == "wc") 100 else if(points_type == "stage") 50 else 300
  
  # Get appropriate race histories for each skier
  race_key <- if(points_type == "tds") {
    "TDS"
  } else {
    paste0(if(points_type == "wc") "WC_" else "Stage_", race_type)
  }
  
  # Get the probability column name
  prob_col <- if(points_type == "tds") "TDS" else race_type
  
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
    race_history <- histories[[skier]][[race_key]]
    
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
        simulated_points <- create_race_distribution(fake_data, n_simulations = 1, max_points = max_points)
        return(simulated_points[1])
      }, error = function(e) {
        mean_points <- mean(race_history)
        result <- mean_points + rnorm(1, 0, max(5, mean_points * 0.15))
        return(max(0, min(max_points, round(result))))
      })
    } else {
      mean_points <- mean(race_history)
      if(is.na(mean_points) || is.nan(mean_points)) {
        return(0)
      }
      result <- mean_points + rnorm(1, 0, max(5, mean_points * 0.15))
      return(max(0, min(max_points, round(result))))
    }
  })
  
  results[is.na(results) | is.nan(results)] <- 0
  return(results)
}


# Modified simulate_season function to handle gender-specific race counts
simulate_season <- function(histories, standings_df, gender="M", n_simulations = 100, remaining_races_by_gender) {
  print("Simulating Season")
  log_info("Starting {if(gender=='M') 'men' else 'women'}'s season simulation")
  log_info("Starting season simulation with {n_simulations} iterations")
  log_info("Number of skiers: {length(names(histories))}")
  
  # Get race counts for this specific gender
  gender_races <- remaining_races_by_gender[[gender]]
  
  # Log race schedule for this gender
  log_info("{if(gender=='M') 'Men' else 'Women'}'s World Cup races remaining: {paste(names(gender_races$WC), gender_races$WC, sep=': ', collapse=', ')}")
  log_info("{if(gender=='M') 'Men' else 'Women'}'s Stage races remaining: {paste(names(gender_races$Stage), gender_races$Stage, sep=': ', collapse=', ')}")
  log_info("{if(gender=='M') 'Men' else 'Women'}'s TdS races remaining: {gender_races$TDS}")
  
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
    
    # If skier not in standings, assume 0 points
    if(length(current_points) == 0) {
      log_info("{skier} is not in the standings")
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
    
    # Simulate World Cup races
    for(race_type in names(gender_races$WC)) {
      n_races <- gender_races$WC[[race_type]]
      if(n_races > 0) {
        for(i in 1:n_races) {
          results <- simulate_race(gender_histories, race_type, "wc")
          season_results[sim,] <- season_results[sim,] + results
        }
      }
    }
    
    # Simulate Stage races
    for(race_type in names(gender_races$Stage)) {
      n_races <- gender_races$Stage[[race_type]]
      if(n_races > 0) {
        for(i in 1:n_races) {
          results <- simulate_race(gender_histories, race_type, "stage")
          season_results[sim,] <- season_results[sim,] + results
        }
      }
    }
    
    # Simulate Tour De Ski
    if(gender_races$TDS > 0) {
      for(i in 1:gender_races$TDS) {
        tds_results <- simulate_race(gender_histories, "TDS", "tds")
        season_results[sim,] <- season_results[sim,] + tds_results
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
simulate_season_with_probabilities <- function(histories, standings_df, gender="M", n_simulations = 100, remaining_races_by_gender, probability_table) {
  print("Simulating Season with Pre-computed Participation Probabilities")
  log_info("Starting {if(gender=='M') 'men' else 'women'}'s season simulation with pre-computed probabilities")
  
  # Filter probability table for current gender
  gender_probabilities <- probability_table[probability_table$Gender == gender, ]
  
  # Get race counts for this specific gender
  gender_races <- remaining_races_by_gender[[gender]]
  
  # Filter histories for current gender
  gender_histories <- histories[names(histories) %in% standings_df$Skier]
  all_skiers <- names(gender_histories)
  
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
  
  # Run simulations
  for(sim in 1:n_simulations) {
    if(sim %% 10 == 0) {
      log_info("Running {if(gender=='M') 'men' else 'women'}'s simulation {sim}")
    }
    
    # Simulate World Cup races
    for(race_type in names(gender_races$WC)) {
      n_races <- gender_races$WC[[race_type]]
      if(n_races > 0) {
        for(i in 1:n_races) {
          results <- simulate_race_with_probability(gender_histories, race_type, "wc", gender_probabilities)
          season_results[sim,] <- season_results[sim,] + results
        }
      }
    }
    
    # Simulate Stage races
    for(race_type in names(gender_races$Stage)) {
      n_races <- gender_races$Stage[[race_type]]
      if(n_races > 0) {
        for(i in 1:n_races) {
          results <- simulate_race_with_probability(gender_histories, race_type, "stage", gender_probabilities)
          season_results[sim,] <- season_results[sim,] + results
        }
      }
    }
    
    # Simulate Tour De Ski
    if(gender_races$TDS > 0) {
      for(i in 1:gender_races$TDS) {
        tds_results <- simulate_race_with_probability(gender_histories, "TDS", "tds", gender_probabilities)
        season_results[sim,] <- season_results[sim,] + tds_results
      }
    }
  }
  
  # Calculate winning probabilities (same as before)
  winners <- apply(season_results, 1, which.max)
  win_probs <- table(all_skiers[winners]) / n_simulations
  win_probs <- sort(win_probs, decreasing = TRUE)
  
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
library(writexl)
# Modified create_standings_summary function to handle both genders
create_standings_summary <- function(simulation_results, standings_df, gender) {
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
  
  # Save to Excel
  #write_xlsx(final_table, file.path(output_dir, paste0(gender, "_standings_predictions.xlsx")))
  
  return(final_table)
}



# Run simulations for both genders
#men_sims <- simulate_season(all_histories_clean, standings_df, "M", 500, remaining_races_by_gender)
#women_sims <- simulate_season(all_histories_clean, ladies_standings_df, "L", 500, remaining_races_by_gender)

# Run simulations with participation probabilities
race_probability_table <- create_race_probability_table(men_chrono, ladies_chrono, top_30_men, top_30_women)

# Step 2: Run simulations using pre-computed probabilities (much faster!)
men_sims <- simulate_season_with_probabilities(
  all_histories_clean, 
  standings_df, 
  "M", 
  500, 
  remaining_races_by_gender, 
  race_probability_table
)

women_sims <- simulate_season_with_probabilities(
  all_histories_clean, 
  ladies_standings_df, 
  "L", 
  500, 
  remaining_races_by_gender, 
  race_probability_table
)
# Create summaries for both genders
men_summary <- create_standings_summary(men_sims$simulation_results, standings_df, "M")
women_summary <- create_standings_summary(women_sims$simulation_results, ladies_standings_df, "L")

# Save separate Excel files for each gender
output_dir = get_output_dir()
# Instead of hardcoded paths:
write_xlsx(men_summary, file.path(output_dir, "men_standings_predictions.xlsx"))
write_xlsx(women_summary, file.path(output_dir, "ladies_standings_predictions.xlsx"))

# CROSS-COUNTRY - MAGIC NUMBERS IMPLEMENTATION
# Add this to the end of the Cross-Country race-recap2.R file

# Function to calculate total points remaining for Cross-Country
calculate_total_points_remaining_cross_country <- function(gender_races) {
  # Cross-Country has different point systems:
  # WC races = 100 points max
  # Stage races = 50 points max  
  # TDS = 300 points max per stage
  
  wc_points <- sum(unlist(gender_races$WC)) * 100
  stage_points <- sum(unlist(gender_races$Stage)) * 50
  tds_points <- gender_races$TDS * 300
  
  return(wc_points + stage_points + tds_points)
}

# Function to calculate magic numbers for Cross-Country championship elimination
calculate_magic_numbers_cross_country <- function(standings_df, remaining_races_by_gender, gender = "M") {
  
  leader_points <- max(standings_df$Points, na.rm = TRUE)
  gender_races <- remaining_races_by_gender[[gender]]
  
  # Cross-Country complex calculation
  wc_points <- sum(unlist(gender_races$WC)) * 100
  stage_points <- sum(unlist(gender_races$Stage)) * 50
  tds_points <- gender_races$TDS * 300
  total_points_remaining <- wc_points + stage_points + tds_points
  
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
men_magic_numbers <- calculate_magic_numbers_cross_country(standings_df, remaining_races_by_gender, "M")
women_magic_numbers <- calculate_magic_numbers_cross_country(ladies_standings_df, remaining_races_by_gender, "L")

# Save to Excel files
output_dir <- get_output_dir()
write_xlsx(men_magic_numbers, file.path(output_dir, "men_magic_numbers.xlsx"))
write_xlsx(women_magic_numbers, file.path(output_dir, "ladies_magic_numbers.xlsx"))

