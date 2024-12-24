# Load required libraries
library(arrow)  # for reading feather files
library(readxl) # for reading Excel files
library(tidyverse)

# Define file paths
men_chrono_path <- "~/ski/elo/python/ski/polars/excel365/men_chrono.csv"
ladies_chrono_path <- "~/ski/elo/python/ski/polars/excel365/ladies_chrono.csv"
men_points_path <- "~/blog/daehl-e/content/post/drafts/weekly-picks/2025Davos/men-points.xlsx"
ladies_points_path <- "~/blog/daehl-e/content/post/drafts/weekly-picks/2025Davos/ladies-points.xlsx"

# Read the feather files
men_chrono <- read.csv(men_chrono_path)

ladies_chrono <- read.csv(ladies_chrono_path)

# Read the Excel files
men_points <- read_xlsx(men_points_path)
ladies_points <- read_xlsx(ladies_points_path)

# Create a list to store all dataframes
ski_data <- list(
  men_chrono = men_chrono,
  ladies_chrono = ladies_chrono,
  men_points = men_points,
  ladies_points = ladies_points
)

# Print the dimensions of each dataframe to verify loading
lapply(ski_data, dim)

library(dplyr)
library(ggplot2)
library(openxlsx)
# Filter and sort men's data
men_2025 <- men_chrono %>%
  filter(Season == 2025, City != "Summer") %>% 
  #filter(Season == 2025) %>% 
  arrange(Season, Race, Place)

men_before <- men_chrono %>%
  filter(Season < 2025 | (Season == 2025 & Race < 4 & City!="Summer")) %>%
  arrange(Season, Date, Race, Place)

men_end <- men_chrono %>%
  #filter(Season == 2025, City != "Summer") %>% 
  filter(Season == 2025) %>% 
  arrange(Season, Race, Place)


# Filter and sort ladies' data
ladies_2025 <- ladies_chrono %>%
  filter(Season == 2025, City != "Summer") %>% 
  #filter(Season == 2025) %>% 
  arrange(Season, Race, Place)

ladies_end <- ladies_chrono %>%
  #filter(Season == 2025, City != "Summer") %>% 
  filter(Season == 2025) %>% 
  arrange(Season, Race, Place)

ladies_before <- ladies_chrono %>%
  filter(Season < 2025 | (Season == 2025 & Race < 4 & City!="Summer")) %>%
  arrange(Season, Date, Race, Place)

# Define World Cup points vector
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)

library(dplyr)
library(ggplot2)
library(openxlsx)

# Define World Cup points vector
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)

analyze_race <- function(race_type, race_num, race_part, season, city, base_dir) {
  # Filter season data without City filter
  men_season <- men_chrono %>%
    filter(Season == season) %>%
    arrange(Season, Race, Place)
  
  ladies_season <- ladies_chrono %>%
    filter(Season == season) %>%
    arrange(Season, Race, Place)
  
  # Helper function to create race-specific dataframe
  create_race_df <- function(data, points_data, pelo_col, elo_col) {
    race_df <- data %>%
      filter(Race == race_num) %>%
      mutate(Points = case_when(
        Place > 0 & Place <= length(wc_points) ~ wc_points[Place],
        TRUE ~ 0
      )) %>%
      dplyr::select(Skier, Nation, Points, !!sym(paste0(pelo_col)), !!sym(paste0(elo_col))) %>%
      left_join(
        points_data %>% 
          dplyr::select(Skier, !!paste0("Race", race_part, "_Points")), 
        by = "Skier"
      ) %>%
      rename(Predicted_Points = !!paste0("Race", race_part, "_Points")) %>%
      mutate(
        Predicted_Points = as.numeric(Predicted_Points),
        Points_Difference = Points - as.double(Predicted_Points),
        Elo_Difference = !!sym(paste0(elo_col)) - !!sym(paste0(pelo_col))
      ) %>%
      dplyr::select(
        Skier, 
        Nation, 
        !!sym(paste0(pelo_col)), 
        !!sym(paste0(elo_col)),
        Elo_Difference, 
        Predicted_Points, 
        Points,
        Points_Difference
      )
    
    # Rename columns
    colnames(race_df) <- c("Skier", "Nation", "Before Elo", "After Elo", 
                           "Elo Difference", "Predicted Points", "Points", 
                           "Points Difference")
    
    return(race_df)
  }
  
  # Helper function to create plots
  create_plot <- function(data, gender, race_part, base_dir) {
    max_diff <- data %>% slice_max(`Points Difference`, n = 3)
    min_diff <- data %>% slice_min(`Points Difference`, n = 3)
    
    p <- ggplot(data, aes(x = `Predicted Points`, y = Points)) +
      geom_point() +
      geom_point(data = max_diff, color = "green", size = 3) +
      geom_text(data = max_diff, aes(label = Skier), vjust = -1) +
      geom_point(data = min_diff, color = "red", size = 3) +
      geom_text(data = min_diff, aes(label = Skier), vjust = -1) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      scale_x_continuous(limits = c(0, 100), breaks = c(25, 50, 75, 100)) +
      labs(title = "Predicted vs Actual Points",
           x = "Predicted Points",
           y = "Actual Points") +
      theme_minimal()
    
    filename <- file.path(base_dir, "static/img/weekly-recap", 
                          paste0(season, city),
                          paste0(gender, "_race", race_part, ".png"))
    ggsave(filename, plot = p, width = 10, height = 8, dpi = 300)
  }
  
  # Helper function to get performance stats
  get_performance_stats <- function(data) {
    # Calculate error metrics
    rmse <- sqrt(mean((data$Points - data$`Predicted Points`)^2, na.rm = TRUE))
    mae <- mean(abs(data$Points - data$`Predicted Points`), na.rm = TRUE)
    mse <- mean((data$Points - data$`Predicted Points`)^2, na.rm = TRUE)
    
    # Top 3 overperformers
    top_over <- data %>%
      arrange(desc(`Points Difference`)) %>%
      dplyr::select(Skier, Points, `Predicted Points`, `Points Difference`) %>%
      head(3)
    
    # Top 3 underperformers
    top_under <- data %>%
      arrange(`Points Difference`) %>%
      dplyr::select(Skier, Points, `Predicted Points`, `Points Difference`) %>%
      head(3)
    
    # Add error metrics to the output
    print(paste("RMSE:", round(rmse, 2)))
    print(paste("MAE:", round(mae, 2)))
    print(paste("MSE:", round(mse, 2)))
    
    return(list(
      overperformers = top_over, 
      underperformers = top_under,
      metrics = list(
        rmse = rmse,
        mae = mae,
        mse = mse
      )
    ))
  }
  
  # Create directories if they don't exist
  dir.create(file.path(base_dir, "static/img/weekly-recap", paste0(season, city)), 
             recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(base_dir, "content/post/drafts/weekly-recap", paste0(season, city)), 
             recursive = TRUE, showWarnings = FALSE)
  
  # Determine Elo columns based on race type
  pelo_col <- paste0(race_type, "_Pelo")
  elo_col <- paste0(race_type, "_Elo")
  
  # Create race dataframes
  men_race <- create_race_df(men_season, men_points, pelo_col, elo_col)
  ladies_race <- create_race_df(ladies_season, ladies_points, pelo_col, elo_col)
  
  # Create plots
  create_plot(men_race, "men", race_part, base_dir)
  create_plot(ladies_race, "ladies", race_part, base_dir)
  
  # Get performance stats
  men_stats <- get_performance_stats(men_race)
  ladies_stats <- get_performance_stats(ladies_race)
  
  # Save to Excel
  write.xlsx(men_race, 
             file.path(base_dir, "content/post/drafts/weekly-recap", 
                       paste0(season, city), 
                       paste0("men_race", race_part, ".xlsx")))
  write.xlsx(ladies_race, 
             file.path(base_dir, "content/post/drafts/weekly-recap", 
                       paste0(season, city), 
                       paste0("ladies_race", race_part, ".xlsx")))
  
  return(list(
    men = list(data = men_race, stats = men_stats),
    ladies = list(data = ladies_race, stats = ladies_stats)
  ))
}

analyze_weekend <- function(season, city, base_dir) {
  create_weekend_df <- function(data, points_data, before_data) {
    weekend_df <- data %>%
      filter(Race %in% c(4, 5, 6)) %>%
      mutate(Points = case_when(
        Place > 0 & Place <= length(wc_points) ~ wc_points[Place],
        TRUE ~ 0
      )) %>%
      dplyr::select(Skier, Nation, Race, Points, Elo) %>%
      group_by(Skier, Race) %>%
      left_join(
        before_data %>%
          group_by(Skier) %>%
          slice_tail(n = 1) %>%
          dplyr::select(Skier, Before_Elo = Elo),
        by = "Skier"
      ) %>%
      left_join(
        points_data %>%
          dplyr::select(Skier, Predicted_Points = Total_Points) %>%
          mutate(Predicted_Points = as.numeric(Predicted_Points)),
        by = "Skier"
      ) %>%
      group_by(Skier) %>%
      mutate(Total_Points = cumsum(Points)) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      mutate(
        After_Elo = Elo,
        Elo_Difference = After_Elo - Before_Elo,
        Points_Difference = Total_Points - Predicted_Points
      ) %>%
      dplyr::select(
        Skier, Nation, 
        `Before Elo` = Before_Elo, 
        `After Elo` = After_Elo, 
        `Elo Difference` = Elo_Difference,
        `Predicted Points` = Predicted_Points, 
        `Total Points` = Total_Points, 
        `Points Difference` = Points_Difference
      ) %>%
      arrange(-`Total Points`)
    
    return(weekend_df)
  }
  
  create_weekend_plot <- function(data, gender, base_dir) {
    max_diff <- data %>% 
      slice_max(`Points Difference`, n = 3)
    
    min_diff <- data %>%
      slice_min(`Points Difference`, n = 3)
    
    p <- ggplot(data, aes(x = `Predicted Points`, y = `Total Points`)) +
      geom_point() +
      geom_point(data = max_diff, color = "green", size = 3) +
      geom_text(data = max_diff, aes(label = Skier), vjust = -1) +
      geom_point(data = min_diff, color = "red", size = 3) +
      geom_text(data = min_diff, aes(label = Skier), vjust = -1) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      scale_x_continuous(breaks = c(75, 100, 150, 300)) +
      labs(title = "Predicted vs Actual Points",
           x = "Predicted Points",
           y = "Actual Points") +
      theme_minimal()
    
    filename <- file.path(base_dir, "static/img/weekly-recap", 
                          paste0(season, city),
                          paste0(gender, "_weekend.png"))
    ggsave(filename, plot = p, width = 10, height = 8, dpi = 300)
  }
  
  # Create directories if they don't exist
  dir.create(file.path(base_dir, "static/img/weekly-recap", paste0(season, city)), 
             recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(base_dir, "content/post/drafts/weekly-recap", paste0(season, city)), 
             recursive = TRUE, showWarnings = FALSE)
  
  # Filter data
  men_season <- men_chrono %>%
    filter(Season == season) %>%
    arrange(Season, Race, Place)
  
  ladies_season <- ladies_chrono %>%
    filter(Season == season) %>%
    arrange(Season, Race, Place)
  
  # Create weekend dataframes
  men_weekend <- create_weekend_df(men_season, men_points, men_before)
  ladies_weekend <- create_weekend_df(ladies_season, ladies_points, ladies_before)
  
  # Create plots
  create_weekend_plot(men_weekend, "men", base_dir)
  create_weekend_plot(ladies_weekend, "ladies", base_dir)
  
  # Save to Excel
  write.xlsx(men_weekend, 
             file.path(base_dir, "content/post/drafts/weekly-recap", 
                       paste0(season, city), 
                       "men_weekend.xlsx"))
  write.xlsx(ladies_weekend, 
             file.path(base_dir, "content/post/drafts/weekly-recap", 
                       paste0(season, city), 
                       "ladies_weekend.xlsx"))
  
  return(list(men = men_weekend, ladies = ladies_weekend))
}
base_dir <- "~/blog/daehl-e"

# Analyze individual races
race1_results <- analyze_race("Sprint_F", 8, 1, 2025, "Davos", base_dir)
race2_results <- analyze_race("Distance_C", 9,  2, 2025, "Davos", base_dir)


# Analyze weekend results
weekend_results <- analyze_weekend(2025, "Davos", base_dir)
weekend_results

# Print metrics for Race 1
print("Men's Sprint:")
print(race1_results$men$stats$metrics)
print("\nLadies' Distance C:")
print(race1_results$ladies$stats$metrics)

print("\nMen's Sprint C:")
print(race2_results$men$stats$metrics)
print("\nLadies' Sprint C:")
print(race2_results$ladies$stats$metrics)

# print("\nMen's Distance F:")
# print(race3_results$men$stats$metrics)
# print("\nLadies' Distance F:")
# print(race3_results$ladies$stats$metrics)


# Men's combined metrics across all races
men_all_races <- rbind(
  race1_results$men$data,
  race2_results$men$data
  # race3_results$men$data
)

men_combined_rmse <- sqrt(mean((men_all_races$Points - men_all_races$`Predicted Points`)^2, na.rm = TRUE))
men_combined_mae <- mean(abs(men_all_races$Points - men_all_races$`Predicted Points`), na.rm = TRUE)
men_combined_mse <- mean((men_all_races$Points - men_all_races$`Predicted Points`)^2, na.rm = TRUE)

# Ladies' combined metrics across all races
ladies_all_races <- rbind(
  race1_results$ladies$data,
  race2_results$ladies$data
  #race3_results$ladies$data
)

ladies_combined_rmse <- sqrt(mean((ladies_all_races$Points - ladies_all_races$`Predicted Points`)^2, na.rm = TRUE))
ladies_combined_mae <- mean(abs(ladies_all_races$Points - ladies_all_races$`Predicted Points`), na.rm = TRUE)
ladies_combined_mse <- mean((ladies_all_races$Points - ladies_all_races$`Predicted Points`)^2, na.rm = TRUE)

print("Men's Combined Metrics:")
print(paste("RMSE:", round(men_combined_rmse, 2)))
print(paste("MAE:", round(men_combined_mae, 2)))
print(paste("MSE:", round(men_combined_mse, 2)))

print("\nLadies' Combined Metrics:")
print(paste("RMSE:", round(ladies_combined_rmse, 2)))
print(paste("MAE:", round(ladies_combined_mae, 2)))
print(paste("MSE:", round(ladies_combined_mse, 2)))
library(dplyr)
library(mgcv)
library(leaps)

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
          weights <- seq(length(prev_races), 1)
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
          weights <- seq(length(prev_races), 1)
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
          weights <- seq(length(prev_races), 1)
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
          weights <- seq(length(prev_races), 1)
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
            City!="Tour de Ski",
            #Season > 2014,
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
                    weights <- seq(length(prev_races), 1)
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
                    weights <- seq(length(prev_races), 1)
                    weighted.mean(prev_races, weights, na.rm = TRUE)
                } else {
                    NA_real_
                }
            })
        ) %>%
        ungroup()

  train_df <- train_df %>%
    filter(
      Season > 2014
    ) %>%
      create_pelo_percentages() %>%
  filter(!!sym(pelo_pct_mapping[[race_type]]) > 0.75)
  
  
  # Build model
  model_results <- select_gam_features(train_df, points_col)
  
  # Process test data
  test_df <- chrono_data %>%
    filter(
      City == "Summer",
      Season == 2025
    )
  
  # Update Elo columns
  pelo_cols <- names(test_df)[grep("Pelo$", names(test_df))]
  elo_cols <- gsub("Pelo$", "Elo", pelo_cols)
  
  test_df <- test_df %>%
    mutate(across(
      all_of(elo_cols),
      ~ case_when(
        Season == 2025 ~ get(gsub("Elo$", "Pelo", cur_column())),
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
    # Process TDS training data
      train_df <- chrono_data %>%
          filter(
              City == "Tour de Ski",
              #Season > 2014
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
          filter(Season > 2014)
      
      # Build TDS-specific model using TdS_Points as response
      model_results <- select_gam_features(train_df %>% 
                                         mutate(Points = TdS_Points))

  # Process test data
  test_df <- chrono_data %>%
    filter(
      City == "Summer",
      Season == 2025
    )
  
  # Update Elo columns
  pelo_cols <- names(test_df)[grep("Pelo$", names(test_df))]
  elo_cols <- gsub("Pelo$", "Elo", pelo_cols)
  
  test_df <- test_df %>%
    mutate(across(
      all_of(elo_cols),
      ~ case_when(
        Season == 2025 ~ get(gsub("Elo$", "Pelo", cur_column())),
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

# Generate predictions for women
for(race_type in race_types) {
  cat("\nProcessing Women's WC", race_type, "...\n")
  results <- generate_race_predictions(ladies_chrono, race_type, "wc", "F")
  all_predictions[[paste0("F_WC_", race_type)]] <- results$predictions
  all_models[[paste0("F_WC_", race_type)]] <- results$model
  
  cat("\nProcessing Women's Stage", race_type, "...\n")
  results <- generate_race_predictions(ladies_chrono, race_type, "stage", "F")
  all_predictions[[paste0("F_Stage_", race_type)]] <- results$predictions
  all_models[[paste0("F_Stage_", race_type)]] <- results$model
}

# TDS predictions for both genders
cat("\nProcessing Men's Tour de Ski...\n")
mens_tds_results <- generate_tds_predictions(men_chrono, "M")
all_predictions[["M_TDS"]] <- mens_tds_results$predictions
all_models[["M_TDS"]] <- mens_tds_results$model

cat("\nProcessing Women's Tour de Ski...\n")
womens_tds_results <- generate_tds_predictions(ladies_chrono, "F")
all_predictions[["F_TDS"]] <- womens_tds_results$predictions
all_models[["F_TDS"]] <- womens_tds_results$model



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
                race_type == "TDS" ~ City == "Tour de Ski",
                TRUE ~ FALSE
            ),
            Season > 2014
        ) %>%
        arrange(desc(Date)) %>%
        slice_head(n = n_required) %>%
        pull(!!sym(points_col))
    

    if(race_type=="TDS"){
      n_required = 5
    }
    
    
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
standings_df <- read_feather("~/ski/elo/python/ski/polars/excel365/men_standings.feather")
ladies_standings_df <- read_feather("~/ski/elo/python/ski/polars/excel365/ladies_standings.feather")



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
      get_race_history(skier, race_type, all_predictions, "wc", gender = "F")
    
    all_histories[[skier]][[paste0("Stage_", race_type)]] <- 
      get_race_history(skier, race_type, all_predictions, "stage", gender = "F")
  }
  
  all_histories[[skier]][["TDS"]] <- 
    get_race_history(skier, "TDS", all_predictions, "tds", 3, gender = "F")
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
    gender <- if(skier %in% top_30_men) "M" else "F"
    
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
gc()
# Get current loaded packages
loaded_packages <- (.packages())

# Store histories
temp_histories <- all_histories_clean

# Clear everything EXCEPT temp_histories
rm(list = setdiff(ls(), "temp_histories"))

# Restore our saved object
all_histories_clean <- temp_histories

# Remove our temporary variable
rm(temp_histories)


# Load standings for both genders
standings_df <- read_feather("~/ski/elo/python/ski/polars/excel365/men_standings.feather")
ladies_standings_df <- read_feather("~/ski/elo/python/ski/polars/excel365/ladies_standings.feather")

# Set up logging
log_file <- "~/blog/daehl-e/content/post/drafts/weekly-recap/simulation_log.txt"
log_appender(appender_file(log_file))

create_race_distribution <- function(race_results, n_simulations = 1, max_points = 100) {
    points <- race_results$Points
    weights <- race_results$Weight
    n_actual_races <- length(points)
    
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
log_info("{simulated_points}")

# Define remaining races
remaining_races <- list(
    WC = list(
        Distance = 0,
        Distance_C = 4,
        Distance_F = 4,
        Sprint_C = 3,
        Sprint_F = 3
    ),
    Stage = list(
        Distance = 1,
        Distance_C = 2,
        Distance_F = 2,
        Sprint_C = 1,
        Sprint_F = 1
    ),
    TDS = 1
)

# Function to simulate a single race for all skiers
simulate_race <- function(histories, race_type, points_type = "wc") {
    #log_info("Simulating {points_type} race of type {race_type}")

    # Set max points based on race type
    max_points <- if(points_type == "wc") 100 else if(points_type == "stage") 50 else 300
    
    # Get appropriate race histories for each skier
    race_key <- paste0(if(points_type == "wc") "WC_" else "Stage_", race_type)
    
    
    results <- sapply(names(histories), function(skier) {
        race_history <- histories[[skier]][[race_key]]
        # if(skier=="Gus Schumacher"){
        #   log_info("Getting races for Gus Schumacher: {race_type}")
        # }
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

                simulated_points <- create_race_distribution(fake_data, n_simulations = 1)

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

# Function to simulate entire season
simulate_season <- function(histories, standings_df, gender="M", n_simulations = 100) {
    log_info("Starting {if(gender=='M') 'men' else 'women'}'s season simulation")
    log_info("Starting season simulation with {n_simulations} iterations")
    log_info("Number of skiers: {length(names(histories))}")
        # Log race schedule
    log_info("World Cup races remaining: {paste(names(remaining_races$WC), remaining_races$WC, sep=': ', collapse=', ')}")
    log_info("Stage races remaining: {paste(names(remaining_races$Stage), remaining_races$Stage, sep=': ', collapse=', ')}")
    log_info("TdS races remaining: {remaining_races$TDS}")

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
          log_info("{skier} is not not in the standings")
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
        for(race_type in names(remaining_races$WC)) {
          n_races <- remaining_races$WC[[race_type]]
          if(n_races > 0) {
                #log_info("Simulating {n_races} World Cup {race_type} races")
                for(i in 1:n_races) {
              
                results <- simulate_race(gender_histories, race_type, "wc")
                season_results[sim,] <- season_results[sim,] + results
            }
          }
        }
        # Simulate Stage races
        for(race_type in names(remaining_races$Stage)) {
          n_races <- remaining_races$Stage[[race_type]]
            if(n_races > 0) {
                #log_info("Simulating {n_races} Stage {race_type} races")
                for(i in 1:n_races) {
                results <- simulate_race(gender_histories, race_type, "stage")
                season_results[sim,] <- season_results[sim,] + results
            }
            }
        }
        # Simulate Tour de Ski
        if(remaining_races$TDS > 0) {
            tds_results <- sapply(all_skiers, function(skier) {
                history <- gender_histories[[skier]][["TDS"]]
                if(length(history) >= 3) {
                    fake_data <- data.frame(
                        Points = history,
                        Weight = seq(length(history), 1)
                    )
                    simulated_points <- create_race_distribution(fake_data, n_simulations = 1, max_points = 300)
                    if(skier=="Gus Schumacher"){
                        log_info("History of Points for Gus Schumacher: {fake_data}")
                    }
                    return(simulated_points[1])
                } else {
                    mean_points <- mean(history)
                    result <- mean_points + rnorm(1, 0, mean_points * 0.15)
                    return(max(0, min(300, round(result))))
                }
            })
            season_results[sim,] <- season_results[sim,] + tds_results
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

library(writexl)
# Modified create_standings_summary function to handle both genders
create_standings_summary_by_gender <- function(simulation_results, standings_df, gender) {
    # Create base summary
    summary <- create_standings_summary(simulation_results, standings_df)
    
    # Add gender column
    summary$Gender <- gender
    
    return(summary)
}

# Create enhanced summary function with fixes
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
    # Create directory if it doesn't exist
dir.create("~/blog/daehl-e/content/post/drafts/weekly-recap/2025Davos", 
           recursive = TRUE,  # This creates parent directories if needed
           showWarnings = FALSE)  # Don't warn if directory already exists
    # Save to Excel
    write_xlsx(final_table, "~/blog/daehl-e/content/post/drafts/weekly-recap/2025Davos/standings_predictions.xlsx")
    
    return(final_table)
}

all_histories_clean$`Therese Johaug`$WC_Sprint_F = c(0,0,0,0,0,0,0,0,0,0)
all_histories_clean$`Therese Johaug`$WC_Sprint_C = c(0,0,0,0,0,0,0,0,0,0)
# Run simulations for both genders
men_sims <- simulate_season(all_histories_clean, standings_df, "M", 500)
women_sims <- simulate_season(all_histories_clean, ladies_standings_df, "F", 500)

# Create summaries for both genders
men_summary <- create_standings_summary_by_gender(men_sims$simulation_results, standings_df, "M")
women_summary <- create_standings_summary_by_gender(women_sims$simulation_results, ladies_standings_df, "F")

# Save separate Excel files for each gender
write_xlsx(men_summary, "~/blog/daehl-e/content/post/drafts/weekly-recap/2025Davos/mens_standings_predictions.xlsx")
write_xlsx(women_summary, "~/blog/daehl-e/content/post/drafts/weekly-recap/2025Davos/womens_standings_predictions.xlsx")

# Create combined markdown summary
summary_file <- "~/blog/daehl-e/content/post/drafts/weekly-recap/simulation_summary.md"
writeLines(
    c("---",
      "title: 'Cross-Country Skiing Season Simulation Results'",
      "date: '2024-12-07'",
      "---",
      "",
      "## Men's Results",
      "",
      "### Top 10 Current Standings (Men)",
      paste(1:10, ". ", head(men_summary$Skier, 10), 
            " (", round(head(men_summary$`Current Standings`, 10)), " points)",
            sep=""),
      "",
      "### Projected Final Top 10 (Men)",
      paste(1:10, ". ", head(men_summary$Skier, 10),
            " (", round(head(men_summary$`Simulated Standings`, 10)), " points, ",
            sprintf("%.1f%%", head(men_summary$`Win%`, 10)), " win probability)",
            sep=""),
      "",
      "## Women's Results",
      "",
      "### Top 10 Current Standings (Women)",
      paste(1:10, ". ", head(women_summary$Skier, 10), 
            " (", round(head(women_summary$`Current Standings`, 10)), " points)",
            sep=""),
      "",
      "### Projected Final Top 10 (Women)",
      paste(1:10, ". ", head(women_summary$Skier, 10),
            " (", round(head(women_summary$`Simulated Standings`, 10)), " points, ",
            sprintf("%.1f%%", head(women_summary$`Win%`, 10)), " win probability)",
            sep="")),
    summary_file
)

# Function to print debug info for both genders
print_skier_debug_by_gender <- function(skier, histories, gender) {
    cat("\nDebugging", if(gender=="M") "Male" else "Female", "skier:", skier, "\n")
    if(skier %in% names(histories)) {
        cat("Available race types:\n")
        print(names(histories[[skier]]))
        
        cat("\nSample of race histories:\n")
        for(race_type in names(histories[[skier]])) {
            cat("\n", race_type, ":\n")
            print(histories[[skier]][[race_type]])
        }
    } else {
        cat("Skier not found in histories\n")
    }
}