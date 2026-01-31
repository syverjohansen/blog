# Race Day Mixed Relay Predictions
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(logger)
library(caret)  # For model training
library(mgcv)   # For GAM models if needed
library(caret)

# Set up logging
log_dir <- "~/ski/elo/python/ski/polars/excel365/race-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_file <- file.path(log_dir, paste0("mixed_relay_predictions_", format(Sys.Date(), "%Y%m%d"), ".log"))
log_appender(appender_file(log_file))
log_threshold(INFO)
log_info("Starting mixed relay race day predictions process")

# Define points system
relay_points <- c(200, 160, 120, 100, 90, 80, 72, 64, 58, 52, 48, 44, 40, 36, 
                  32, 30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10, 8, 6, 4, 2)

# Function to find today's date
get_today_date <- function() {
  current_utc_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")
  return(current_utc_date)
}

# Function to replace NAs with first quartile values
replace_na_with_quartile <- function(x) {
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Function to load races.csv and find mixed relay races for today
load_today_mixed_relay_races <- function() {
  log_info("Loading race data for today")
  
  # Read in the race schedule
  races_path <- "~/ski/elo/python/ski/polars/excel365/races.csv"
  races <- read.csv(races_path, stringsAsFactors = FALSE) %>%
    mutate(Date = mdy(Date))
  
  # Find today's date
  today <- get_today_date()
  log_info(paste("Today's date:", today))
  
  # Filter for today's races
  today_races <- races %>%
    filter(Date == today)
  
  # Filter to only mixed relay races
  mixed_relay_races <- today_races %>%
    filter(Distance == "Rel", Sex == "Mixed")
  
  if (nrow(mixed_relay_races) == 0) {
    log_info("No mixed relay races found today")
    return(NULL)
  }
  
  log_info(paste("Found", nrow(mixed_relay_races), "mixed relay races"))
  
  return(list(
    mixed = mixed_relay_races,
    race_date = today
  ))
}

# Function to load chrono files and create combined dataframe for mixed relay
load_chrono_data <- function() {
  log_info("Loading chrono data")
  
  # Define file paths
  men_chrono_path <- "~/ski/elo/python/ski/polars/relay/excel365/men_chrono.csv"
  ladies_chrono_path <- "~/ski/elo/python/ski/polars/relay/excel365/ladies_chrono.csv"
  
  # Load data
  men_chrono <- read.csv(men_chrono_path, stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date)) %>%
    mutate(Sex = "M")  # Add gender column
  
  ladies_chrono <- read.csv(ladies_chrono_path, stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date)) %>%
    # Add 100000 to ladies' ID numbers to avoid confusion with men's IDs
    mutate(ID = ID + 100000) %>%
    mutate(Sex = "F")  # Add gender column
  
  log_info(paste("Loaded men's chrono with", nrow(men_chrono), "rows and", 
                 ncol(men_chrono), "columns"))
  log_info(paste("Loaded ladies' chrono with", nrow(ladies_chrono), "rows and", 
                 ncol(ladies_chrono), "columns"))
  
  # Combine men and ladies chrono data
  combined_chrono <- bind_rows(men_chrono, ladies_chrono)
  
  log_info(paste("Created combined chrono with", nrow(combined_chrono), "rows"))
  
  # Create separate dataframes for relays and individual races from combined data
  relays <- combined_chrono %>% 
    filter(Distance %in% c("Rel", "MRel"))  # Include both normal and mixed relays
  
  individuals <- combined_chrono %>% 
    filter(!Distance %in% c("Rel", "MRel", "Ts"))
  
  log_info(paste("Created relay df with", nrow(relays), "rows"))
  log_info(paste("Created individual df with", nrow(individuals), "rows"))
  
  return(list(
    combined_chrono = combined_chrono,
    relays = relays,
    individuals = individuals
  ))
}

# Function to add points to race results
add_points_to_results <- function(df, is_relay = FALSE) {
  df %>%
    mutate(Points = mapply(function(place) {
      if (place >= 1 && place <= length(relay_points)) {
        return(relay_points[place])
      }
      return(0)
    }, Place))
}

# Function to calculate Pelo percentages with better error handling
create_pelo_pcts <- function(df) {
  # Define pelo_cols inside the function
  pelo_cols <- names(df)[grep("Pelo$", names(df))]
  
  # Check if any Pelo columns were found
  if(length(pelo_cols) == 0) {
    # Try with Elo columns instead and rename them to Pelo
    elo_cols <- names(df)[grep("Elo$", names(df))]
    
    if(length(elo_cols) > 0) {
      log_info("Using Elo columns instead of Pelo columns")
      
      # Create Pelo columns from Elo columns (just rename for compatibility)
      for(col in elo_cols) {
        pelo_col <- gsub("Elo$", "Pelo", col)
        df[[pelo_col]] <- df[[col]]
      }
      
      # Redefine pelo_cols after creating them
      pelo_cols <- names(df)[grep("Pelo$", names(df))]
    } else {
      log_warn("No Pelo or Elo columns found in data")
      return(df)  # Return original data if no relevant columns
    }
  }
  
  # For each race and each Pelo column
  df_transformed <- df %>%
    group_by(Sex, Date, Race) %>%
    mutate(across(
      all_of(pelo_cols),
      function(x) {
        # Replace NAs with first quartile
        q1 <- quantile(x, 0.25, na.rm = TRUE)
        x_filled <- replace(x, is.na(x), q1)
        # Calculate percentage of max
        x_filled / max(x_filled) * 100
      },
      .names = "{.col}_Pct"
    )) %>%
    ungroup()
  
  return(df_transformed)
}

# Function to process data for classic and freestyle races
process_discipline_data <- function(df_individuals, min_season = NULL) {
  # Set min_season to 11 years ago from max season if not provided
  if(is.null(min_season)) {
    min_season = max(df_individuals$Season) - 11
  }
  
  # First add points
  df_with_points <- add_points_to_results(df_individuals, is_relay = FALSE)
  
  # Process classic races
  classic_df <- df_with_points %>%
    filter(Distance != "Sprint", Technique == "C") %>%
    group_by(ID, Sex) %>%  # Include sex in grouping
    arrange(Season, Race) %>%
    mutate(
      # Calculate weighted average of previous 5 races
      Weighted_Last_5 = sapply(row_number(), function(i) {
        prev_races <- Points[max(1, i-5):(i-1)]
        if (length(prev_races) > 0) {
          weights <- seq(1, length(prev_races))
          weighted.mean(prev_races, weights, na.rm = TRUE)
        } else {
          0
        }
      })
    ) %>%
    ungroup()
  
  # Process freestyle races
  freestyle_df <- df_with_points %>%
    filter(Distance != "Sprint", Technique == "F") %>%
    group_by(ID, Sex) %>%  # Include sex in grouping
    arrange(Season, Race) %>%
    mutate(
      # Calculate weighted average of previous 5 races
      Weighted_Last_5 = sapply(row_number(), function(i) {
        prev_races <- Points[max(1, i-5):(i-1)]
        if (length(prev_races) > 0) {
          weights <- seq(1, length(prev_races))
          weighted.mean(prev_races, weights, na.rm = TRUE)
        } else {
          0
        }
      })
    ) %>%
    ungroup()
  
  return(list(
    classic = classic_df,
    freestyle = freestyle_df
  ))
}

# Function to process relay data with classic and freestyle legs
process_relay_data <- function(df_relays, classic_df, freestyle_df, min_season = NULL) {
  # Set min_season to 11 years ago from max season if not provided
  if(is.null(min_season)) {
    min_season = max(classic_df$Season) - 11
  }
  
  # Add points to relay results
  relay_with_points <- add_points_to_results(df_relays, is_relay = TRUE)
  
  # Process classic legs (typically 1-2) - don't filter by season yet
  classic_legs_all <- relay_with_points %>%
    filter(Distance %in% c("Rel"), Leg %in% c(1, 2))
  
  # Process freestyle legs (typically 3-4) - don't filter by season yet
  freestyle_legs_all <- relay_with_points %>%
    filter(Distance %in% c("Rel"), Leg %in% c(3, 4))
  
  # Combine classic legs with classic individual data
  classic_combined <- bind_rows(
    classic_legs_all,
    classic_df
  ) %>%
    group_by(ID, Sex) %>%  # Include sex in grouping
    arrange(Date, Season, Race, desc(Distance)) %>%  # Use Date for chronological order
    fill(Weighted_Last_5, .direction = "down") %>%
    filter(Distance %in% c("Rel"), Season > min_season) %>%  # Apply season filter AFTER filling
    group_by(Season, Race) %>%  # Regroup by race for quartile replacement
    mutate(
      Weighted_Last_5 = ifelse(
        is.na(Weighted_Last_5),
        quantile(Weighted_Last_5, 0.25, na.rm = TRUE),
        Weighted_Last_5
      )
    ) %>%
    ungroup()
  
  # Combine freestyle legs with freestyle individual data
  freestyle_combined <- bind_rows(
    freestyle_legs_all,
    freestyle_df
  ) %>%
    group_by(ID, Sex) %>%  # Include sex in grouping
    arrange(Date, Season, Race, desc(Distance)) %>%  # Use Date for chronological order
    fill(Weighted_Last_5, .direction = "down") %>%
    filter(Distance %in% c("Rel"), Season > min_season) %>%  # Apply season filter AFTER filling
    group_by(Season, Race) %>%  # Regroup by race for quartile replacement
    mutate(
      Weighted_Last_5 = ifelse(
        is.na(Weighted_Last_5),
        quantile(Weighted_Last_5, 0.25, na.rm = TRUE),
        Weighted_Last_5
      )
    ) %>%
    ungroup()
  
  return(list(
    classic_legs = classic_combined,
    freestyle_legs = freestyle_combined
  ))
}

# Function to prepare leg-specific datasets
prepare_leg_data <- function(classic_legs, freestyle_legs) {
  # For mixed relay, we need to handle legs by gender as well
  leg_data <- list()
  
  # Process each of the 4 positions in mixed relay
  
  # Leg 1 (usually female classic)
  leg_data[[1]] <- classic_legs %>%
    filter(Leg == 1, Sex == "F") %>%
    mutate(
      is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
      is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
      is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
      is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  # Leg 2 (usually male classic)
  leg_data[[2]] <- classic_legs %>%
    filter(Leg == 2, Sex == "M") %>%
    mutate(
      is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
      is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
      is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
      is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  # Leg 3 (usually female freestyle)
  leg_data[[3]] <- freestyle_legs %>%
    filter(Leg == 3, Sex == "F") %>%
    mutate(
      is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
      is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
      is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
      is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  # Leg 4 (usually male freestyle)
  leg_data[[4]] <- freestyle_legs %>%
    filter(Leg == 4, Sex == "M") %>%
    mutate(
      is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
      is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
      is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
      is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  # Check if we have data for each leg
  for(i in 1:4) {
    log_info(paste("Leg", i, "dataset contains", nrow(leg_data[[i]]), "rows"))
    
    # If we don't have enough data for a specific leg, fall back to using relay data with less restrictive filters
    if(nrow(leg_data[[i]]) < 10) {
      log_warn(paste("Not enough data for Leg", i, "- expanding to include all relay data for this position"))
      
      # Determine which filter set to use
      if(i <= 2) {
        # Classic legs
        leg_data[[i]] <- classic_legs %>%
          filter(Leg == i) %>%  # Keep leg position but don't filter by gender
          mutate(
            is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
            is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
            is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
            is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
          )
      } else {
        # Freestyle legs
        leg_data[[i]] <- freestyle_legs %>%
          filter(Leg == i) %>%  # Keep leg position but don't filter by gender
          mutate(
            is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
            is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
            is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
            is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
          )
      }
      
      log_info(paste("After expanding, Leg", i, "dataset contains", nrow(leg_data[[i]]), "rows"))
    }
  }
  
  return(leg_data)
}

# Function to get leg-specific predictor columns for mixed relay
get_leg_predictors <- function(leg, leg_data) {
  # Get column names from the leg data
  base_cols <- names(leg_data[[leg]])
  
  # Define predictors based on leg and sex
  if(leg == 1) {
    # Leg 1 (Female Classic)
    predictors <- c(
      grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE),
      grep("Classic.*Pelo_Pct$", base_cols, value = TRUE),
      "Distance_Pelo_Pct",
      "Pelo_Pct",
      "Weighted_Last_5"
    )
  } else if(leg == 2) {
    # Leg 2 (Male Classic)
    predictors <- c(
      grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE),
      grep("Classic.*Pelo_Pct$", base_cols, value = TRUE),
      "Distance_Pelo_Pct",
      "Pelo_Pct",
      "Weighted_Last_5"
    )
  } else if(leg == 3) {
    # Leg 3 (Female Freestyle)
    predictors <- c(
      grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE),
      grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE),
      "Distance_Pelo_Pct",
      "Pelo_Pct",
      "Weighted_Last_5"
    )
  } else if(leg == 4) {
    # Leg 4 (Male Freestyle)
    predictors <- c(
      grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE),
      grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE),
      "Distance_Pelo_Pct",
      "Sprint_Pelo_Pct",
      "Pelo_Pct",
      "Weighted_Last_5"
    )
  }
  
  # Remove any NA or invalid column names
  predictors <- predictors[predictors %in% names(leg_data[[leg]])]
  
  return(predictors)
}

# Function to train models for each leg
train_leg_models <- function(leg_data) {
  # Set up control parameters for cross-validation
  control <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = defaultSummary,
    savePredictions = "final"
  )
  
  # Function to safely train a model with fallbacks
  train_model_safe <- function(formula, data, method = "glm", target_name) {
    log_info(paste("Training", target_name, "model using", method))
    
    if (method == "glm") {
      # Try GLM first
      tryCatch({
        model <- train(
          formula,
          data = data,
          method = "glm",
          family = "binomial",
          trControl = control
        )
        return(model)
      }, error = function(e) {
        log_warn(paste("GLM training failed:", e$message, "- using basic glm"))
        # Direct GLM as last resort
        model <- glm(formula, data = data, family = binomial)
        # Wrap in a caret-compatible structure
        result <- list(
          finalModel = model,
          xNames = attr(terms(model), "term.labels"),
          method = "glm.basic"
        )
        class(result) <- "train"
        return(result)
      })
    } else {
      log_warn(paste("Unknown method", method, "- using glm instead"))
      # Fall back to GLM
      tryCatch({
        model <- train(
          formula,
          data = data,
          method = "glm",
          family = "binomial",
          trControl = control
        )
        return(model)
      }, error = function(e) {
        log_warn(paste("GLM training failed:", e$message, "- using basic glm"))
        # Direct GLM as last resort
        model <- glm(formula, data = data, family = binomial)
        # Wrap in a caret-compatible structure
        result <- list(
          finalModel = model,
          xNames = attr(terms(model), "term.labels"),
          method = "glm.basic"
        )
        class(result) <- "train"
        return(result)
      })
    }
  }
  
  # Train models for each leg
  leg_models <- list()
  for(leg in 1:4) {
    log_info(paste("Training models for Leg", leg))
    
    leg_predictors <- get_leg_predictors(leg, leg_data)
    log_info(paste("Using predictors:", paste(leg_predictors, collapse = ", ")))
    
    # Skip if no predictors or data
    if(length(leg_predictors) == 0 || nrow(leg_data[[leg]]) == 0) {
      log_warn(paste("Not enough data or predictors for Leg", leg))
      leg_models[[leg]] <- list(
        podium = NULL,
        win = NULL,
        top5 = NULL,
        top10 = NULL,
        features = leg_predictors
      )
      next
    }
    
    # Create formulas
    podium_formula <- as.formula(paste("is_podium ~", paste(leg_predictors, collapse = "+")))
    win_formula <- as.formula(paste("is_win ~", paste(leg_predictors, collapse = "+")))
    top5_formula <- as.formula(paste("is_top5 ~", paste(leg_predictors, collapse = "+")))
    top10_formula <- as.formula(paste("is_top10 ~", paste(leg_predictors, collapse = "+")))
    
    # Train models
    podium_model <- train_model_safe(podium_formula, leg_data[[leg]], "glm", "podium")
    win_model <- train_model_safe(win_formula, leg_data[[leg]], "glm", "win")
    top5_model <- train_model_safe(top5_formula, leg_data[[leg]], "glm", "top5")
    top10_model <- train_model_safe(top10_formula, leg_data[[leg]], "glm", "top10")
    
    # Store models
    leg_models[[leg]] <- list(
      podium = podium_model,
      win = win_model,
      top5 = top5_model,
      top10 = top10_model,
      features = leg_predictors
    )
  }
  
  return(leg_models)
}

# Function to load mixed relay startlists
load_mixed_relay_startlists <- function() {
  # Define file paths
  teams_path <- "~/ski/elo/python/ski/polars/relay/excel365/startlist_mixed_relay_races_teams.csv"
  individuals_path <- "~/ski/elo/python/ski/polars/relay/excel365/startlist_mixed_relay_races_individuals.csv"
  
  # Load data - with error handling
  teams <- tryCatch({
    read.csv(teams_path, stringsAsFactors = FALSE)
  }, error = function(e) {
    log_warn(paste("Could not read teams file:", e$message))
    return(data.frame())
  })
  
  individuals <- tryCatch({
    read.csv(individuals_path, stringsAsFactors = FALSE)
  }, error = function(e) {
    log_warn(paste("Could not read individuals file:", e$message))
    return(data.frame())
  })
  
  log_info(paste("Loaded mixed relay teams startlist with", nrow(teams), "rows"))
  log_info(paste("Loaded mixed relay individuals startlist with", nrow(individuals), "rows"))
  
  return(list(
    teams = teams,
    individuals = individuals
  ))
}

# Function to check if startlist has valid entries
has_valid_startlist <- function(startlist) {
  return(nrow(startlist$teams) > 0 && nrow(startlist$individuals) > 0)
}

# Function to prepare current skier data for prediction
prepare_current_skiers <- function(chrono_data, current_season, startlist_individuals = NULL) {
  log_info("Preparing current skier data for mixed relay")

  # Get all skiers from current season
  current_skiers <- chrono_data$combined_chrono %>%
    filter(Season == current_season) %>%
    select(ID, Skier, Nation, Sex) %>%
    distinct()

  # Get latest Elo values - prefer startlist (from chrono_pred), fallback to chrono_data
  if (!is.null(startlist_individuals) && any(grepl("Elo$", names(startlist_individuals)))) {
    log_info("Using Elo values from startlist (chrono_pred source)")
    elo_cols <- names(startlist_individuals)[grepl("Elo$", names(startlist_individuals))]
    # For mixed relay, need to preserve Sex column
    latest_elo <- startlist_individuals %>%
      filter(ID %in% current_skiers$ID) %>%
      select(ID, any_of(elo_cols), any_of("Sex")) %>%
      distinct()
  } else {
    log_info("Falling back to chrono_data for Elo values")
    latest_elo <- chrono_data$combined_chrono %>%
      filter(ID %in% current_skiers$ID) %>%
      group_by(ID) %>%
      arrange(desc(Season), desc(Race)) %>%
      slice_head(n = 1) %>%
      select(ID, ends_with("Elo"), Sex) %>%
      ungroup()
  }
  
  # Get discipline data
  discipline_data <- process_discipline_data(chrono_data$individuals)
  
  # Get classic last 5 values
  classic_last5 <- discipline_data$classic %>%
    filter(ID %in% current_skiers$ID) %>%
    group_by(ID) %>%
    arrange(Date, Season, Race) %>%
    mutate(
      # Calculate weighted average of previous races including current
      Classic_Last_5 = sapply(row_number(), function(i) {
        prev_races <- Points[max(1, i-4):i]  # Include current row
        if (length(prev_races) > 0) {
          weights <- seq(1, length(prev_races))
          weighted.mean(prev_races, weights, na.rm = TRUE)
        } else {
          0
        }
      })
    ) %>%
    arrange(desc(Date), desc(Season), desc(Race)) %>% 
    slice_head(n = 1) %>%
    select(ID, Classic_Last_5, Sex) %>%
    ungroup()
  
  # Get freestyle last 5 values
  freestyle_last5 <- discipline_data$freestyle %>%
    filter(ID %in% current_skiers$ID) %>%
    group_by(ID) %>%
    arrange(Date, Season, Race) %>%
    mutate(
      # Calculate weighted average of previous races including current
      Freestyle_Last_5 = sapply(row_number(), function(i) {
        prev_races <- Points[max(1, i-4):i]  # Include current row
        if (length(prev_races) > 0) {
          weights <- seq(1, length(prev_races))
          weighted.mean(prev_races, weights, na.rm = TRUE)
        } else {
          0
        }
      })
    ) %>%
    arrange(desc(Date), desc(Season), desc(Race)) %>% 
    slice_head(n = 1) %>%
    select(ID, Freestyle_Last_5, Sex) %>%
    ungroup()
  
  # Combine all data
  current_df <- current_skiers %>%
    left_join(latest_elo, by = c("ID", "Sex")) %>%
    left_join(classic_last5, by = c("ID", "Sex")) %>%
    left_join(freestyle_last5, by = c("ID", "Sex"))
  
  # Replace NAs with quartile values
  for(col in names(current_df)) {
    if(is.numeric(current_df[[col]]) && any(is.na(current_df[[col]]))) {
      q1 <- quantile(current_df[[col]], 0.25, na.rm = TRUE)
      if(is.na(q1)) q1 <- 0  # Default to 0 if quartile calculation fails
      
      current_df[[col]] <- ifelse(is.na(current_df[[col]]), q1, current_df[[col]])
      log_info(paste("Replaced NAs in", col, "with first quartile:", q1))
    }
  }
  
  # Calculate Pelo_Pct values directly from Elo columns
  elo_cols <- names(current_df)[grepl("Elo$", names(current_df))]
  
  if(length(elo_cols) > 0) {
    for(col in elo_cols) {
      max_val <- max(current_df[[col]], na.rm = TRUE)
      if(max_val > 0) {
        # Create Pelo_Pct name but calculate from Elo values
        pct_col <- paste0(gsub("Elo", "Pelo", col), "_Pct")
        current_df[[pct_col]] <- (current_df[[col]] / max_val) * 100
        log_info(paste("Created", pct_col, "from", col, "with max value:", max_val))
      }
    }
  }
  
  # Log summary of prepared data
  log_info(paste("Prepared data for", nrow(current_df), "skiers with", 
                 ncol(current_df), "features"))
  
  return(current_df)
}

# Function to get leg predictions
get_leg_predictions <- function(leg_number, skier_data, leg_models) {
  # Select appropriate Last_5 column based on leg
  if(leg_number <= 2) {
    skier_data$Weighted_Last_5 <- skier_data$Classic_Last_5
  } else {
    skier_data$Weighted_Last_5 <- skier_data$Freestyle_Last_5
  }
  
  # Make sure we have the right predictors for this model
  required_predictors <- leg_models[[leg_number]]$features
  
  # Check if we have all required predictors
  missing_predictors <- setdiff(required_predictors, names(skier_data))
  if(length(missing_predictors) > 0) {
    log_warn(paste("Missing predictors for leg", leg_number, ":", 
                   paste(missing_predictors, collapse=", ")))
    
    # Initialize missing predictors with 0 or appropriate default values
    for(pred in missing_predictors) {
      skier_data[[pred]] <- 0
    }
  }
  
  # Create prediction data with only the columns needed for prediction
  pred_data <- skier_data
  
  # Handle NAs in prediction data
  for(col in names(pred_data)) {
    if(is.numeric(pred_data[[col]]) && any(is.na(pred_data[[col]]))) {
      # Replace NAs with first quartile (or 0 if all NA)
      q1_val <- quantile(pred_data[[col]], 0.25, na.rm = TRUE)
      if(is.na(q1_val)) q1_val <- 0
      pred_data[[col]][is.na(pred_data[[col]])] <- q1_val
      
      # Log the imputation
      log_info(paste("Imputed NAs in column", col, "with first quartile value:", q1_val))
    }
  }
  
  # Get predictions safely
  safe_predict <- function(model, data, type = "prob") {
    if(is.null(model)) return(rep(0, nrow(data)))
    
    # Get needed variables more safely
    vars_needed <- tryCatch({
      if(inherits(model, "train")) {
        # For caret train objects
        if(!is.null(model$finalModel) && is.list(model$finalModel)) {
          # Extract from finalModel if available
          if(inherits(model$finalModel, "glm")) {
            names(model$finalModel$coefficients)[-1]  # Remove intercept
          } else if(!is.null(model$xNames)) {
            model$xNames  # Use xNames if available
          } else {
            character(0)  # Empty if nothing found
          }
        } else if(!is.null(model$xNames)) {
          model$xNames
        } else {
          character(0)
        }
      } else if(inherits(model, "glm")) {
        # For direct glm models
        names(model$coefficients)[-1]  # Remove intercept
      } else {
        # Default empty
        character(0)
      }
    }, error = function(e) {
      log_warn(paste("Error getting model variables:", e$message))
      character(0)  # Return empty on error
    })
    
    # Ensure data has all required predictors for the model
    for(var in vars_needed) {
      if(!(var %in% names(data))) {
        data[[var]] <- 0
      }
    }
    
    tryCatch({
      probs <- predict(model, newdata = data, type = type)
      if(is.data.frame(probs) && "Yes" %in% names(probs)) {
        return(probs[,"Yes"])
      } else if(is.numeric(probs)) {
        return(probs)
      } else {
        return(rep(0.25, nrow(data)))
      }
    }, error = function(e) {
      log_warn(paste("Prediction error:", e$message))
      return(rep(0.25, nrow(data)))
    })
  }
  
  # Get predictions for each outcome
  win_probs <- safe_predict(leg_models[[leg_number]]$win, pred_data)
  podium_probs <- safe_predict(leg_models[[leg_number]]$podium, pred_data)
  top5_probs <- safe_predict(leg_models[[leg_number]]$top5, pred_data)
  top10_probs <- safe_predict(leg_models[[leg_number]]$top10, pred_data)
  
  # Make sure all vectors are the same length as the data
  if(length(win_probs) != nrow(pred_data)) {
    log_warn(paste("Size mismatch in win_probs:", length(win_probs), "vs", nrow(pred_data)))
    win_probs <- rep(win_probs[1], nrow(pred_data))
  }
  if(length(podium_probs) != nrow(pred_data)) {
    log_warn(paste("Size mismatch in podium_probs:", length(podium_probs), "vs", nrow(pred_data)))
    podium_probs <- rep(podium_probs[1], nrow(pred_data))
  }
  if(length(top5_probs) != nrow(pred_data)) {
    log_warn(paste("Size mismatch in top5_probs:", length(top5_probs), "vs", nrow(pred_data)))
    top5_probs <- rep(top5_probs[1], nrow(pred_data))
  }
  if(length(top10_probs) != nrow(pred_data)) {
    log_warn(paste("Size mismatch in top10_probs:", length(top10_probs), "vs", nrow(pred_data)))
    top10_probs <- rep(top10_probs[1], nrow(pred_data))
  }
  
  # Create results dataframe
  return(data.frame(
    ID = pred_data$ID,
    Skier = pred_data$Skier,
    Nation = pred_data$Nation,
    Sex = pred_data$Sex,
    Leg = leg_number,
    Win_Prob = win_probs,
    Podium_Prob = podium_probs,
    Top5_Prob = top5_probs,
    Top10_Prob = top10_probs
  ))
}

# Function to get leg-specific predictions for startlist entries
get_leg_predictions_with_startlist <- function(current_skiers, leg_models, startlist_individuals) {
  # Create a list to store predictions for each leg
  leg_predictions <- list()
  
  # Process each leg
  for(leg in 1:4) {
    # Determine required sex for this leg position
    
    
    # Filter the startlist to get skiers for this leg
    leg_skiers <- startlist_individuals %>%
      filter(Team_Position == leg) %>%
      select(ID, Skier, Nation, Team_Name, Sex)
    required_sex = leg_skiers$Sex[1]
    # Adjust ladies' IDs to match our internal processing
    leg_skiers <- leg_skiers %>%
      mutate(ID = ifelse(Sex == "F", ID + 100000, ID))
    
    # Filter current_skiers to only include those in this leg's startlist
    leg_data <- current_skiers %>%
      filter(ID %in% leg_skiers$ID, Sex == required_sex) %>%
      # Add Team information from startlist
      left_join(
        leg_skiers %>% select(ID, Team_Name),
        by = "ID"
      )

    # Make predictions for this specific leg
    leg_predictions[[leg]] <- get_leg_predictions(leg, leg_data, leg_models)
  }
  
  return(leg_predictions)
}

# Function to calculate leg importance from historical data
calculate_leg_importance <- function(leg_models) {
  # Default weights for mixed relay
  default_weights <- c(0.2, 0.25, 0.25, 0.3)  # Slight emphasis on later legs
  return(default_weights)
}

# Function to generate team predictions
generate_team_predictions <- function(teams_df, individual_predictions, leg_models) {
  # Find all the exact Member_N columns
  member_cols <- c()
  for(i in 1:4) {
    name_col <- paste0("Member_", i)
    if(name_col %in% names(teams_df)) {
      member_cols <- c(member_cols, name_col)
    }
  }
  
  # Initialize results dataframe
  team_predictions <- teams_df %>%
    select(Team_Name, Nation, Team_Rank, Price, Is_Present, all_of(member_cols)) %>%
    mutate(
      Podium_Prob = 0,
      Win_Prob = 0,
      Top5_Prob = 0,
      Top10_Prob = 0,
      Expected_Points = 0
    )
  print(teams_df)
  # Calculate leg importance weights
  leg_importance <- calculate_leg_importance(leg_models)
  
  log_info(paste("Leg importance weights:", 
                 paste(sprintf("Leg %d: %.2f", 1:4, leg_importance), collapse=", ")))
  
  # For each team, calculate probabilities based on their members
  for(i in 1:nrow(team_predictions)) {
    team_name <- team_predictions$Team_Name[i]
    
    # Extract team members
    members <- c()
    for(leg in 1:4) {
      member_col <- paste0("Member_", leg)
      if(member_col %in% names(teams_df)) {
        members[leg] <- teams_df[[member_col]][i]
      }
    }
    
    # Get predictions for each member
    member_probs <- list(
      Podium = numeric(4),
      Win = numeric(4),
      Top5 = numeric(4),
      Top10 = numeric(4)
    )
    
    for(leg in 1:4) {
      if(!is.na(members[leg]) && members[leg] != "") {
        # Access the specific leg's dataframe and then filter
        if(!is.null(individual_predictions[[leg]])) {
          skier_pred <- individual_predictions[[leg]] %>%
            filter(Skier == members[leg])
          
          if(nrow(skier_pred) > 0) {
            # Store probabilities safely
            member_probs$Podium[leg] <- skier_pred$Podium_Prob[1]
            member_probs$Win[leg] <- skier_pred$Win_Prob[1]
            member_probs$Top5[leg] <- skier_pred$Top5_Prob[1]
            member_probs$Top10[leg] <- skier_pred$Top10_Prob[1]
          }
        }
      }
    }
    
    # Calculate weighted probabilities
    if(sum(member_probs$Podium > 0) > 0) {  # Only if we have any valid probabilities
      # Calculate weighted probabilities using leg importance
      weighted_podium <- sum(member_probs$Podium * leg_importance)
      weighted_win <- sum(member_probs$Win * leg_importance)
      weighted_top5 <- sum(member_probs$Top5 * leg_importance)
      weighted_top10 <- sum(member_probs$Top10 * leg_importance)
      
      team_predictions$Podium_Prob[i] <- weighted_podium
      team_predictions$Win_Prob[i] <- weighted_win
      team_predictions$Top5_Prob[i] <- weighted_top5
      team_predictions$Top10_Prob[i] <- weighted_top10
      
      # Calculate expected points based on probabilities
      team_predictions$Expected_Points[i] <- 
        team_predictions$Win_Prob[i] * relay_points[1] +
        (team_predictions$Podium_Prob[i] - team_predictions$Win_Prob[i]) * mean(relay_points[2:3]) +
        (team_predictions$Top5_Prob[i] - team_predictions$Podium_Prob[i]) * mean(relay_points[4:5]) +
        (team_predictions$Top10_Prob[i] - team_predictions$Top5_Prob[i]) * mean(relay_points[6:10])
    }
  }
  
  return(team_predictions)
}

# Function to normalize probabilities to proper ranges
normalize_probabilities <- function(team_predictions) {
  # Define normalization targets
  targets <- list(
    Win_Prob = 1,      # Win probability sums to 1
    Podium_Prob = 3,   # Podium probability sums to 3
    Top5_Prob = 5,     # Top5 probability sums to 5
    Top10_Prob = 10    # Top10 probability sums to 10
  )
  
  # For each probability column, normalize to the target sum
  for(prob_col in names(targets)) {
    if(prob_col %in% names(team_predictions)) {
      # Get current sum
      current_sum <- sum(team_predictions[[prob_col]], na.rm = TRUE)
      
      # Skip if current sum is 0 (to avoid division by zero)
      if(current_sum > 0) {
        # Apply normalization factor
        target_sum <- targets[[prob_col]]
        normalization_factor <- target_sum / current_sum
        
        # Normalize probabilities
        team_predictions[[prob_col]] <- team_predictions[[prob_col]] * normalization_factor
        
        # Cap individual probabilities at 1
        team_predictions[[prob_col]] <- pmin(team_predictions[[prob_col]], 1)
        
        # Log the normalization
        log_info(paste("Normalized", prob_col, "from sum of", round(current_sum, 2), 
                       "to", round(sum(team_predictions[[prob_col]], na.rm = TRUE), 2)))
      }
    }
  }
  
  # APPLY MONOTONIC CONSTRAINTS: Ensure Win_Prob <= Podium_Prob <= Top5_Prob <= Top10_Prob
  log_info("Applying monotonic constraints...")
  
  prob_cols <- c("Win_Prob", "Podium_Prob", "Top5_Prob", "Top10_Prob")
  prob_cols <- prob_cols[prob_cols %in% names(team_predictions)]
  
  # For each team, ensure probabilities are monotonically non-decreasing
  for(i in 1:nrow(team_predictions)) {
    probs <- numeric(length(prob_cols))
    for(j in 1:length(prob_cols)) {
      probs[j] <- team_predictions[[prob_cols[j]]][i]
    }
    
    # Apply monotonic adjustment: each probability should be >= previous one
    for(j in 2:length(probs)) {
      if(probs[j] < probs[j-1]) {
        probs[j] <- probs[j-1]  # Set to previous value
      }
    }
    
    # Update the team_predictions dataframe
    for(j in 1:length(prob_cols)) {
      team_predictions[[prob_cols[j]]][i] <- probs[j]
    }
  }
  
  # RE-NORMALIZE after monotonic adjustment to maintain target sums
  log_info("Re-normalizing after monotonic constraints...")
  for(prob_col in names(targets)) {
    if(prob_col %in% names(team_predictions)) {
      current_sum <- sum(team_predictions[[prob_col]], na.rm = TRUE)
      target_sum <- targets[[prob_col]]
      
      if(current_sum > 0) {
        scaling_factor <- target_sum / current_sum
        team_predictions[[prob_col]] <- team_predictions[[prob_col]] * scaling_factor
        
        # Cap at 1.0 again
        team_predictions[[prob_col]] <- pmin(team_predictions[[prob_col]], 1.0)
      }
    }
  }

  # FINAL MONOTONIC CONSTRAINT CHECK after re-normalization
  # This ensures no inversions were introduced by the re-normalization step
  log_info("Applying final monotonic constraints after re-normalization...")
  for(i in 1:nrow(team_predictions)) {
    probs <- numeric(length(prob_cols))
    for(j in 1:length(prob_cols)) {
      probs[j] <- team_predictions[[prob_cols[j]]][i]
    }

    # Apply monotonic adjustment: each probability should be >= previous one
    for(j in 2:length(probs)) {
      if(probs[j] < probs[j-1]) {
        probs[j] <- probs[j-1]  # Set to previous value
      }
    }

    # Update the team_predictions dataframe
    for(j in 1:length(prob_cols)) {
      team_predictions[[prob_cols[j]]][i] <- probs[j]
    }
  }

  # Recalculate Expected_Points based on normalized probabilities
  if("Expected_Points" %in% names(team_predictions)) {
    team_predictions <- team_predictions %>%
      mutate(
        Expected_Points = Win_Prob * relay_points[1] +
          (Podium_Prob - Win_Prob) * mean(relay_points[2:3]) +
          (Top5_Prob - Podium_Prob) * mean(relay_points[4:5]) +
          (Top10_Prob - Top5_Prob) * mean(relay_points[6:10])
      )
    
    log_info("Recalculated Expected_Points based on normalized probabilities")
  }
  
  return(team_predictions)
}

# Function to save prediction results to Excel files
save_prediction_results <- function(team_predictions, race_date, output_dir = NULL) {
  # Create formatted date string
  date_str <- format(race_date, "%Y%m%d")
  
  # Set default output directory if not provided
  if(is.null(output_dir)) {
    output_dir <- paste0("~/blog/daehl-e/content/post/cross-country/drafts/archive/", date_str)
  }
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Check if member columns exist
  has_members <- any(grepl("^Member_", names(team_predictions)))
  
  # Prepare points data with reordered columns
  if(has_members) {
    points_df <- team_predictions %>%
      select(
        Team_Name,
        starts_with("Member_"),  # Add member columns right after Team_Name
        Expected_Points
      ) %>%
      arrange(desc(Expected_Points))
  } else {
    points_df <- team_predictions %>%
      select(Team_Name, Expected_Points) %>%
      arrange(desc(Expected_Points))
  }
  
  # Prepare probability data with reordered columns
  if(has_members) {
    prob_df <- team_predictions %>%
      select(
        Team_Name,
        starts_with("Member_"),  # Add member columns right after Team_Name
        Win_Prob, 
        Podium_Prob, 
        Top5_Prob, 
        Top10_Prob
      ) %>%
      arrange(desc(Podium_Prob))
  } else {
    prob_df <- team_predictions %>%
      select(
        Team_Name,
        Win_Prob, 
        Podium_Prob, 
        Top5_Prob, 
        Top10_Prob
      ) %>%
      arrange(desc(Podium_Prob))
  }
  
  # Save points results
  points_file <- file.path(output_dir, "mixed_relay.xlsx")
  write.xlsx(points_df, points_file)
  log_info(paste("Saved points predictions to", points_file))
  
  # Save probability results
  prob_file <- file.path(output_dir, "mixed_relay_position_probabilities.xlsx")
  write.xlsx(prob_df, prob_file)
  log_info(paste("Saved probability predictions to", prob_file))
  
  # Return file paths
  return(list(
    points_file = points_file,
    prob_file = prob_file
  ))
}

# Main function to run the mixed relay predictions pipeline
run_mixed_relay_predictions <- function() {
  # Step 1: Find today's races
  race_info <- load_today_mixed_relay_races()
  if(is.null(race_info)) {
    log_info("No mixed relay races today. Exiting.")
    return(NULL)
  }
  
  # Step 2: Load chrono data
  chrono_data <- load_chrono_data()
  
  # Step 3: Process data with Pelo percentages
  chrono_data$combined_chrono <- create_pelo_pcts(chrono_data$combined_chrono)
  chrono_data$relays <- create_pelo_pcts(chrono_data$relays)
  chrono_data$individuals <- create_pelo_pcts(chrono_data$individuals)
  
  # Step 4: Process discipline data
  discipline_data <- process_discipline_data(chrono_data$individuals)
  
  # Step 5: Process relay data
  relay_processed <- process_relay_data(
    chrono_data$relays,
    discipline_data$classic,
    discipline_data$freestyle
  )
  
  # Step 6: Prepare leg data and train models
  leg_data <- prepare_leg_data(
    relay_processed$classic_legs,
    relay_processed$freestyle_legs
  )
  
  # Train leg models
  leg_models <- train_leg_models(leg_data)
  
  # Get current season
  current_season <- max(chrono_data$combined_chrono$Season, na.rm = TRUE)
  
  # Step 7: Process startlists and make predictions
  log_info("Processing mixed relay predictions")
  
  # Load mixed relay startlists
  mixed_startlists <- load_mixed_relay_startlists()
  
  # Check if startlists have valid entries
  has_startlist <- has_valid_startlist(mixed_startlists)
  
  if(!has_startlist) {
    log_warn("No valid mixed relay startlist found. Cannot generate predictions.")
    return(NULL)
  }
  
  # Prepare current skier data (pass startlist for Elo values from chrono_pred)
  current_skiers <- prepare_current_skiers(chrono_data, current_season, mixed_startlists$individuals)

  # Get leg predictions
  leg_predictions <- get_leg_predictions_with_startlist(
    current_skiers,
    leg_models,
    mixed_startlists$individuals
  )
 
  # Generate team predictions
  log_info("Generating team predictions")
  team_predictions <- generate_team_predictions(
    mixed_startlists$teams, 
    leg_predictions, 
    leg_models
  )
  
  # Normalize team probabilities
  log_info("Normalizing team probabilities")
  team_predictions <- normalize_probabilities(team_predictions)
  
  # Save results
  prediction_files <- save_prediction_results(team_predictions, race_info$race_date)
  
  log_info("Mixed relay predictions completed successfully")
  
  return(list(
    team_predictions = team_predictions,
    race_date = race_info$race_date,
    output_files = prediction_files
  ))
}

# Execute the main function when the script is run
results <- run_mixed_relay_predictions()