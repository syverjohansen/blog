
  ## Load Required Libraries

# Load required packages
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(caret)
library(mgcv)
library(leaps)
library(logger)
library(purrr)
library(xgboost)
library(forcats)
library(stringr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

# Set up logging
log_dir <- "~/ski/elo/python/ski/polars/excel365/weekly-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_file <- file.path(log_dir, paste0("mixed_relay_predictions_", format(Sys.Date(), "%Y%m%d"), ".log"))
log_appender(appender_file(log_file))
log_threshold(INFO)
log_info("Starting mixed relay predictions process")


## Step 1: Load Data and Find Tomorrow's Races


# Function to find tomorrow's date
get_tomorrow_date <- function() {
  current_utc_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")
  # Calculate tomorrow by adding 1 day
  tomorrow <- current_utc_date
  return(tomorrow)
}

# Function to load weekends.csv and find races for tomorrow
load_upcoming_races <- function() {
  log_info("Loading weekend race data")
  
  # Read in the race schedule
  weekends_path <- "~/ski/elo/python/ski/polars/excel365/weekends.csv"
  weekends <- read.csv(weekends_path, stringsAsFactors = FALSE) %>%
    mutate(Date = mdy(Date))
  
  # Find tomorrow's date
  tomorrow <- get_tomorrow_date()
  log_info(paste("Tomorrow's date:", tomorrow))
  
  # Filter for tomorrow's races
  tomorrow_races <- weekends %>%
    filter(Date == tomorrow)
  
  # Filter to only mixed relay races
  mixed_relay_races <- tomorrow_races %>%
    filter(Distance == "Rel" && Sex=="Mixed")
  
  # If no mixed relay races found for tomorrow, look for the next mixed relay races
  if (nrow(mixed_relay_races) == 0) {
    log_info("No mixed relay races found for tomorrow, finding next mixed relay race")
    next_mixed_relay_races <- weekends %>%
      filter(Date > Sys.Date(), Distance == "Rel") %>%
      arrange(Date)
    
    if (nrow(next_mixed_relay_races) > 0) {
      next_race_date <- min(next_mixed_relay_races$Date)
      log_info(paste("Next mixed relay race found on:", next_race_date))
      mixed_relay_races <- weekends %>%
        filter(Date == next_race_date, Distance == "Rel")
    } else {
      log_warn("No upcoming mixed relay races found in the schedule")
    }
  }
  
  log_info(paste("Found", nrow(mixed_relay_races), "mixed relay races"))
  
  return(list(
    mixed = mixed_relay_races,
    race_date = if(nrow(mixed_relay_races) > 0) unique(mixed_relay_races$Date) else NULL
  ))
}


## Step 2: Load Chrono Files and Create Combined Dataframe


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


## Step 3: Define Points Systems

# Define points systems
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)

relay_points <- c(200, 160, 120, 100, 90, 80, 72, 64, 58, 52, 48, 44, 40, 36, 
                  32, 30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10, 8, 6, 4, 2)

# Function to assign points based on place
assign_points <- function(place, is_relay = FALSE) {
  points_system <- if(is_relay) relay_points else wc_points
  
  if (is.na(place) || place < 1 || place > length(points_system)) {
    return(0)
  }
  
  return(points_system[place])
}

# Function to add points to race results
add_points_to_results <- function(df, is_relay = FALSE) {
  df %>%
    mutate(Points = mapply(function(place) assign_points(place, is_relay), Place))
}


## Step 4: Process Data and Calculate Pelo Percentages

# Function to replace NAs with first quartile values
replace_na_with_quartile <- function(x) {
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
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
process_discipline_data <- function(df_individuals, min_season = 2014) {
  min_seaason = max(df_individuals$Season)-11
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
process_relay_data <- function(df_relays, classic_df, freestyle_df, min_season = 2014) {
  min_seaason = max(classic_df$Season)-11
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


## Step 5: Create Leg-specific Models

# Function to prepare leg-specific datasets
prepare_leg_data <- function(classic_legs, freestyle_legs) {
  # For mixed relay, we need to handle legs by gender as well
  leg_data <- list()
  
  # Process each of the 4 positions in mixed relay
  
  # Leg 1 (usually female classic)
  leg_data[[1]] <- classic_legs %>%
    filter(Leg == 1) %>%
    mutate(
      is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
      is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
      is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
      is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  # Leg 2 (usually male classic)
  leg_data[[2]] <- classic_legs %>%
    filter(Leg == 2) %>%
    mutate(
      is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
      is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
      is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
      is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  # Leg 3 (usually female freestyle)
  leg_data[[3]] <- freestyle_legs %>%
    filter(Leg == 3) %>%
    mutate(
      is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
      is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
      is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
      is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  # Leg 4 (usually male freestyle)
  leg_data[[4]] <- freestyle_legs %>%
    filter(Leg == 4) %>%
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

# Function to safely extract team feature importance
safe_team_importance <- function(model) {
  tryCatch({
    # For GLM models
    if(inherits(model$finalModel, "glm")) {
      coefs <- coef(model$finalModel)
      # Remove intercept
      coefs <- coefs[!names(coefs) == "(Intercept)"]
      # Take absolute values
      importance <- abs(coefs)
      return(importance)
    } else {
      # Try using varImp for other model types
      imp <- varImp(model)
      if(is.list(imp) && "importance" %in% names(imp)) {
        # Extract the importance values
        importance <- imp$importance$Overall
        names(importance) <- rownames(imp$importance)
        return(importance)
      }
    }
    # Default if we couldn't extract importance
    return(c(leg1_prob = 0.25, leg2_prob = 0.25, leg3_prob = 0.25, leg4_prob = 0.25))
  }, error = function(e) {
    log_warn(paste("Error extracting team feature importance:", e$message))
    # Default equal weights
    return(c(leg1_prob = 0.25, leg2_prob = 0.25, leg3_prob = 0.25, leg4_prob = 0.25))
  })
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
  
  # Safe function to extract feature importance
  safe_importance <- function(model) {
    tryCatch({
      # Try to get importance directly from XGBoost model
      if ("finalModel" %in% names(model) && inherits(model$finalModel, "xgb.Booster")) {
        # Direct XGBoost importance
        imp_matrix <- xgb.importance(feature_names = model$xNames, model = model$finalModel)
        if (nrow(imp_matrix) > 0) {
          return(data.frame(
            Overall = imp_matrix$Gain,
            row.names = imp_matrix$Feature
          ))
        }
      }
      
      # Fall back to caret's varImp if direct method fails
      imp <- varImp(model)
      if (is.list(imp) && "importance" %in% names(imp)) {
        return(imp$importance)
      } else {
        # If all fails, return equal importance
        equal_imp <- data.frame(
          Overall = rep(1, length(model$xNames)),
          row.names = model$xNames
        )
        return(equal_imp)
      }
    }, error = function(e) {
      log_warn(paste("Error extracting feature importance:", e$message))
      # Return equal weights if extraction fails
      equal_imp <- data.frame(
        Overall = rep(1, length(model$xNames)),
        row.names = model$xNames
      )
      return(equal_imp)
    })
  }
  
  # Function to safely train a model with fallbacks
  # Function to safely train a model with fallbacks
  train_model_safe <- function(formula, data, method = "glm", target_name) {
    log_info(paste("Training", target_name, "model using", method))
    
    if (method == "glm") {
      # Try GLM first (switched to be the primary model)
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
        log_warn(paste("GLM training failed:", e$message, "- falling back to xgbTree"))
        
        # Fall back to XGBoost if GLM fails
        tryCatch({
          xgb_grid <- expand.grid(
            nrounds = c(50, 100),
            max_depth = c(3, 4),
            eta = 0.03,
            gamma = 0.1,
            colsample_bytree = 0.8,
            min_child_weight = 1,
            subsample = 0.8
          )
          
          model <- train(
            formula,
            data = data,
            method = "xgbTree",
            trControl = control,
            tuneGrid = xgb_grid,
            verbose = FALSE
          )
          return(model)
        }, error = function(e2) {
          log_warn(paste("XGBoost fallback also failed:", e2$message, "- using basic glm"))
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
      })
    } else if (method == "xgbTree") {
      # If xgbTree was explicitly requested
      tryCatch({
        xgb_grid <- expand.grid(
          nrounds = c(50, 100),
          max_depth = c(3, 4),
          eta = 0.03,
          gamma = 0.1,
          colsample_bytree = 0.8,
          min_child_weight = 1,
          subsample = 0.8
        )
        
        model <- train(
          formula,
          data = data,
          method = "xgbTree",
          trControl = control,
          tuneGrid = xgb_grid,
          verbose = FALSE
        )
        return(model)
      }, error = function(e) {
        log_warn(paste("XGBoost training failed:", e$message, "- falling back to glm"))
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
        }, error = function(e2) {
          log_warn(paste("GLM fallback also failed:", e2$message, "- using basic glm"))
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
      })
    } else {
      # For any other method requested
      tryCatch({
        model <- train(
          formula,
          data = data,
          method = method,
          trControl = control
        )
        return(model)
      }, error = function(e) {
        log_warn(paste(method, "training failed:", e$message, "- falling back to glm"))
        # Fall back to GLM as default fallback
        tryCatch({
          model <- train(
            formula,
            data = data,
            method = "glm",
            family = "binomial",
            trControl = control
          )
          return(model)
        }, error = function(e2) {
          log_warn(paste("GLM fallback also failed:", e2$message, "- using basic glm"))
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
    
    # Choose model type based on data size
    method <- ifelse(nrow(leg_data[[leg]]) < 500, "glm", "xgbTree")
    
    # Create formulas
    podium_formula <- as.formula(paste("is_podium ~", paste(leg_predictors, collapse = "+")))
    win_formula <- as.formula(paste("is_win ~", paste(leg_predictors, collapse = "+")))
    top5_formula <- as.formula(paste("is_top5 ~", paste(leg_predictors, collapse = "+")))
    top10_formula <- as.formula(paste("is_top10 ~", paste(leg_predictors, collapse = "+")))
    
    # Train models
    podium_model <- train_model_safe(podium_formula, leg_data[[leg]], method, "podium")
    win_model <- train_model_safe(win_formula, leg_data[[leg]], method, "win")
    top5_model <- train_model_safe(top5_formula, leg_data[[leg]], method, "top5")
    top10_model <- train_model_safe(top10_formula, leg_data[[leg]], method, "top10")
    
    # Store models
    leg_models[[leg]] <- list(
      podium = podium_model,
      win = win_model,
      top5 = top5_model,
      top10 = top10_model,
      features = leg_predictors
    )
    
    # Print feature importance safely
    log_info(paste("Top features for Leg", leg, "podium prediction:"))
    importance <- safe_importance(podium_model)
    if(!is.null(importance) && nrow(importance) > 0) {
      # Sort by importance and print top 5
      importance <- importance[order(importance$Overall, decreasing = TRUE), , drop = FALSE]
      print(head(importance, 5))
    } else {
      log_info("No feature importance available")
    }
  }
  
  return(leg_models)
}

# Function to get leg predictions with FIS startlist
get_leg_predictions_with_startlist <- function(current_skiers, leg_models, startlist_individuals) {
  # Create a list to store predictions for each leg
  leg_predictions <- list()
  
  # Process each leg
  for(leg in 1:4) {
    # Filter the startlist to get skiers for this leg
    leg_skiers <- startlist_individuals %>%
      filter(Team_Position == leg) %>%
      select(ID, Skier, Nation, Team_Name, Sex)
    
    # Adjust ladies' IDs to match our internal processing
    leg_skiers <- leg_skiers %>%
      mutate(ID = ifelse(Sex == "F", ID + 100000, ID))
    
    # Filter current_skiers to only include those in this leg's startlist
    leg_data <- current_skiers %>%
      filter(ID %in% leg_skiers$ID) %>%
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

# Function to get leg-specific predictions
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
  
  # Create results dataframe, including Sex info
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


## Step 6: Process Current Startlists

# Function to load current mixed relay startlists
load_mixed_relay_startlists <- function() {
  # Define file paths
  teams_path <- "~/ski/elo/python/ski/polars/relay/excel365/startlist_mixed_relay_teams.csv"
  individuals_path <- "~/ski/elo/python/ski/polars/relay/excel365/startlist_mixed_relay_individuals.csv"
  
  # Load data
  teams <- read.csv(teams_path, stringsAsFactors = FALSE)
  individuals <- read.csv(individuals_path, stringsAsFactors = FALSE)
  
  log_info(paste("Loaded mixed relay teams startlist with", nrow(teams), "rows"))
  log_info(paste("Loaded mixed relay individuals startlist with", nrow(individuals), "rows"))
  
  return(list(
    teams = teams,
    individuals = individuals
  ))
}

# Function to check if startlist has valid FIS entries
has_valid_fis_entries <- function(individuals_df) {
  if ("In_FIS_List" %in% names(individuals_df)) {
    return(any(individuals_df$In_FIS_List, na.rm = TRUE))
  }
  return(FALSE)
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
      dplyr::slice(1) %>%
      select(ID, ends_with("Elo"), Sex) %>%
      ungroup()
  }
  
  # Get latest classic Weighted_Last_5 values
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
    dplyr::slice(1) %>%
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
    dplyr::slice(1) %>%
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


## Step 7: Build Team Models and Generate Predictions

# Function to generate team predictions using individual leg probabilities
generate_team_predictions <- function(teams_df, individual_predictions, leg_models) {
  # Find all the exact Member_N and Member_N_ID columns
  member_cols <- c()
  for(i in 1:4) {
    name_col <- paste0("Member_", i)
    #id_col <- paste0("Member_", i, "_ID")
    
    if(name_col %in% names(teams_df)) {
      member_cols <- c(member_cols, name_col)
    }
    # if(id_col %in% names(teams_df)) {
    #   member_cols <- c(member_cols, id_col)
    # }
  }
  
  # Initialize results dataframe with the exact member columns
  team_predictions <- teams_df %>%
    select(Team_Name, Nation, Team_Rank, Price, Is_Present, all_of(member_cols)) %>%
    mutate(
      Podium_Prob = 0,
      Win_Prob = 0,
      Top5_Prob = 0,
      Top10_Prob = 0,
      Expected_Points = 0
    )
  
  # Calculate leg importance weights from historical data
  leg_importance <- calculate_leg_importance(leg_models)
  
  log_info(paste("Leg importance weights:", 
                 paste(sprintf("Leg %d: %.2f", 1:4, leg_importance), collapse=", ")))
  
  # For each team, calculate probabilities based on their members
  for(i in 1:nrow(team_predictions)) {
    team_name <- team_predictions$Team_Name[i]
    
    # Extract team members
    members <- c()
    member_sex <- c()
    for(leg in 1:4) {
      member_col <- paste0("Member_", leg)
      sex_col <- paste0("Member_", leg, "_Sex")
      
      if(member_col %in% names(teams_df) && sex_col %in% names(teams_df)) {
        members[leg] <- teams_df[[member_col]][i]
        member_sex[leg] <- teams_df[[sex_col]][i]
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
          # First try to find the exact match by name
          skier_pred <- individual_predictions[[leg]] %>%
            filter(Skier == members[leg])
          
          # If we found the skier, use those predictions
          if(nrow(skier_pred) > 0) {
            # Store probabilities
            member_probs$Podium[leg] <- skier_pred$Podium_Prob[1]
            member_probs$Win[leg] <- skier_pred$Win_Prob[1]
            member_probs$Top5[leg] <- skier_pred$Top5_Prob[1]
            member_probs$Top10[leg] <- skier_pred$Top10_Prob[1]
            
            log_info(paste("Found probabilities for", members[leg], "in leg", leg, 
                           "- Podium:", member_probs$Podium[leg]))
          } else {
            log_warn(paste("No match found for", members[leg], "in leg", leg))
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

# Function to calculate leg importance from historical data
calculate_leg_importance <- function(leg_models) {
  # Debug which option is being used
  log_info("Attempting Option 1: Team model coefficients")
  
  # Option 1: Use model coefficients if available
  option1_result <- tryCatch({
    # Try extracting from team-level model if it exists
    if("team_podium" %in% names(leg_models)) {
      team_importance <- safe_team_importance(leg_models$team_podium)
      log_info("Option 1 succeeded: Found team model coefficients")
      # Normalize to sum to 1
      return(team_importance / sum(team_importance))
    } else {
      log_info("Option 1 failed: No team_podium model found")
      stop("No team model")
    }
  }, error = function(e) {
    log_info(paste("Option 1 failed:", e$message))
    return(NULL)
  })
  
  if(!is.null(option1_result)) return(option1_result)
  
  # Option 2: Use individual model performance as proxy for importance
  log_info("Attempting Option 2: Individual model accuracy")
  option2_result <- tryCatch({
    # Use individual leg model accuracy as proxy for importance
    leg_accuracy <- sapply(1:4, function(leg) {
      # Extract model performance metrics from cross-validation results
      if("results" %in% names(leg_models[[leg]]$podium)) {
        acc <- max(leg_models[[leg]]$podium$results$Accuracy)
        log_info(paste("Leg", leg, "accuracy:", acc))
        return(acc)
      } else {
        log_info(paste("Leg", leg, "has no accuracy results"))
        return(NA)
      }
    })
    
    if(all(is.na(leg_accuracy))) {
      log_info("Option 2 failed: No accuracy metrics found")
      stop("No accuracy metrics")
    }
    
    # Replace NAs with mean of non-NA values
    leg_accuracy[is.na(leg_accuracy)] <- mean(leg_accuracy, na.rm = TRUE)
    
    log_info("Option 2 succeeded: Using model accuracy weights")
    # Normalize to sum to 1
    return(leg_accuracy / sum(leg_accuracy))
  }, error = function(e) {
    log_info(paste("Option 2 failed:", e$message))
    return(NULL)
  })
  
  if(!is.null(option2_result)) return(option2_result)
  
  # Option 3: Use default weights for mixed relay (slightly more weight on anchor legs)
  log_info("Using Option 3: Default leg importance weights for mixed relay")
  default_weights <- c(0.2, 0.25, 0.25, 0.3)  # Slight emphasis on later legs
  return(default_weights)
}

# Function to normalize probabilities in team predictions
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

# Function to reset probabilities that occur more than twice OR are less than the max repeated value
reset_mode_probabilities <- function(team_predictions) {
  # Probability columns to process
  prob_cols <- c("Win_Prob", "Podium_Prob", "Top5_Prob", "Top10_Prob")
  
  # Process each probability column
  for(prob_col in prob_cols) {
    if(prob_col %in% names(team_predictions)) {
      # Get the values
      values <- team_predictions[[prob_col]]
      
      # Skip if no values
      if(length(values) == 0) next
      
      # Count frequency of each value (rounded to avoid floating point issues)
      rounded_values <- round(values, 6)  # Round to 6 decimal places
      value_counts <- table(rounded_values)
      
      # Find values that appear more than twice
      repeated_values <- as.numeric(names(value_counts[value_counts >= 2]))
      
      if(length(repeated_values) > 0) {
        # Find maximum of the repeated values
        max_repeated <- max(repeated_values)
        
        # Identify which values need to be reset (either repeated or less than max_repeated)
        to_reset <- sapply(rounded_values, function(v) 
          any(abs(v - repeated_values) < 1e-6) || v < max_repeated)
        
        reset_count <- sum(to_reset)
        
        # Set identified values to zero
        team_predictions[[prob_col]] <- ifelse(to_reset, 0, values)
        
        log_info(paste("Reset", reset_count, "values to 0 for", prob_col, 
                       "(repeated values or values < ", round(max_repeated, 4), ")"))
        log_info(paste("Repeated values:", paste(round(repeated_values, 4), collapse=", ")))
      } else {
        log_info(paste("No values appear more than twice for", prob_col))
      }
    }
  }
  
  return(team_predictions)
}


## Step 8: Optimize Teams When No FIS Startlist

# Improved function to build optimized teams
build_optimized_teams <- function(current_skiers, leg_predictions) {
  # Get leg importance weights
  leg_importance <- calculate_leg_importance(leg_models)
  log_info(paste("Using leg importance weights for team optimization:", 
                 paste(sprintf("Leg %d: %.2f", 1:4, leg_importance), collapse=", ")))
  
  # Get list of nations
  nations <- unique(current_skiers$Nation)
  
  # Initialize results list
  optimized_teams <- list()
  
  # Get optimal team for each nation
  for(nation in nations) {
    log_info(paste("Optimizing team for", nation))
    optimized_teams[[nation]] <- get_optimal_mixed_team(nation, leg_predictions, leg_importance)
  }
  
  # Combine all teams into a single dataframe
  all_teams <- bind_rows(optimized_teams)
  
  # Calculate probabilities and expected points using the team model
  all_teams <- all_teams %>%
    rowwise() %>%
    mutate(
      Win_Prob = calculate_team_score_weighted(
        c(Member_1, Member_2, Member_3, Member_4), 
        Nation, 
        leg_predictions, 
        "Win_Prob",
        leg_importance
      ),
      Top5_Prob = calculate_team_score_weighted(
        c(Member_1, Member_2, Member_3, Member_4), 
        Nation, 
        leg_predictions, 
        "Top5_Prob",
        leg_importance
      ),
      Top10_Prob = calculate_team_score_weighted(
        c(Member_1, Member_2, Member_3, Member_4), 
        Nation, 
        leg_predictions, 
        "Top10_Prob",
        leg_importance
      ),
      Expected_Points = Win_Prob * relay_points[1] +
        (Podium_Prob - Win_Prob) * mean(relay_points[2:3]) +
        (Top5_Prob - Podium_Prob) * mean(relay_points[4:5]) +
        (Top10_Prob - Top5_Prob) * mean(relay_points[6:10])
    ) %>%
    ungroup()
  
  # Load startlist file to get team names and prices
  individuals_file <- "~/ski/elo/python/ski/polars/relay/excel365/startlist_mixed_relay_individuals.csv"
  teams_file <- "~/ski/elo/python/ski/polars/relay/excel365/startlist_mixed_relay_teams.csv"
  
  # Read the files if they exist
  individuals_data <- tryCatch({
    read.csv(individuals_file, stringsAsFactors = FALSE)
  }, error = function(e) {
    log_warn(paste("Could not read individuals file:", e$message))
    return(NULL)
  })
  
  teams_data <- tryCatch({
    read.csv(teams_file, stringsAsFactors = FALSE)
  }, error = function(e) {
    log_warn(paste("Could not read teams file:", e$message))
    return(NULL)
  })
  
  # Match team names and prices if data is available
  if(!is.null(individuals_data) && !is.null(teams_data)) {
    log_info("Matching optimized teams with startlist team names and prices")
    
    # For each optimized team, look for member skiers in the individuals file
    all_teams <- all_teams %>%
      rowwise() %>%
      mutate(
        # Try to find team name by matching any member to individuals file
        Found_Team_Name = find_team_name(c(Member_1, Member_2, Member_3, Member_4), individuals_data),
        # Update team name if found
        Team_Name = ifelse(!is.na(Found_Team_Name), Found_Team_Name, Team_Name),
        # Look up price from teams file
        Price = lookup_team_price(Team_Name, teams_data)
      ) %>%
      select(-Found_Team_Name) %>%
      ungroup()
  }
  
  return(all_teams)
}

# Helper function to find team name by matching any member
find_team_name <- function(members, individuals_data) {
  for(member in members) {
    if(!is.na(member) && member != "" && member != "NO ELIGIBLE SKIER") {
      # Look for this skier in individuals data
      match_idx <- which(individuals_data$Skier == member)
      if(length(match_idx) > 0) {
        return(individuals_data$Team_Name[match_idx[1]])
      }
    }
  }
  return(NA)
}

# Helper function to look up team price
lookup_team_price <- function(team_name, teams_data) {
  match_idx <- which(teams_data$Team_Name == team_name)
  if(length(match_idx) > 0) {
    return(teams_data$Price[match_idx[1]])
  }
  return(0)  # Default to 0 if not found
}

# Weighted team score calculation
calculate_team_score_weighted <- function(skiers, nation, leg_predictions, prob_col = "Podium_Prob", leg_weights) {
  # Initialize scores
  leg_scores <- numeric(4)
  
  # For each leg
  for(leg in 1:4) {
    if(skiers[leg] != "NO ELIGIBLE SKIER") {
      # Get the required sex for this leg
      required_sex <- if(leg %in% c(1, 3)) "F" else "M"
      
      # Get skier's probability for this leg
      leg_pred <- leg_predictions[[leg]] %>%
        filter(Nation == nation, Skier == skiers[leg], Sex == required_sex)
      
      if(nrow(leg_pred) > 0) {
        # Use the specified probability column
        if(prob_col %in% names(leg_pred)) {
          leg_scores[leg] <- leg_pred[[prob_col]]
        } else {
          # Fall back to Podium_Prob if requested column doesn't exist
          leg_scores[leg] <- leg_pred$Podium_Prob
        }
      }
    }
  }
  
  # Calculate weighted sum
  weighted_score <- sum(leg_scores * leg_weights)
  return(weighted_score)
}

# Optimized team selection with weighted importance for mixed relay
get_optimal_mixed_team <- function(nation, leg_predictions, leg_weights, iterations = 3) {
  # Get candidates for each leg, sorted by their weighted probability
  # Ensure gender constraints: legs 1,3 = female, legs 2,4 = male
  
  leg_candidates <- list()
  
  # Leg 1 (female)
  leg_candidates[[1]] <- leg_predictions[[1]] %>%
    filter(Nation == nation, Sex == "F") %>%
    mutate(Weighted_Prob = Podium_Prob * leg_weights[1]) %>%
    arrange(desc(Weighted_Prob))
  
  # Leg 2 (male)
  leg_candidates[[2]] <- leg_predictions[[2]] %>%
    filter(Nation == nation, Sex == "M") %>%
    mutate(Weighted_Prob = Podium_Prob * leg_weights[2]) %>%
    arrange(desc(Weighted_Prob))
  
  # Leg 3 (female)
  leg_candidates[[3]] <- leg_predictions[[3]] %>%
    filter(Nation == nation, Sex == "F") %>%
    mutate(Weighted_Prob = Podium_Prob * leg_weights[3]) %>%
    arrange(desc(Weighted_Prob))
  
  # Leg 4 (male)
  leg_candidates[[4]] <- leg_predictions[[4]] %>%
    filter(Nation == nation, Sex == "M") %>%
    mutate(Weighted_Prob = Podium_Prob * leg_weights[4]) %>%
    arrange(desc(Weighted_Prob))
  
  # Check if we have enough skiers to form a team (need at least one for each position)
  has_enough_skiers <- TRUE
  for(leg in 1:4) {
    if(nrow(leg_candidates[[leg]]) == 0) {
      log_warn(paste("Nation", nation, "doesn't have skiers for leg", leg))
      has_enough_skiers <- FALSE
    }
  }
  
  if(!has_enough_skiers) {
    # Return empty team if we don't have enough skiers
    return(data.frame(
      Team_Name = paste(nation, "I"),
      Nation = nation,
      Team_Rank = 0,
      Member_1 = "NO ELIGIBLE SKIER",
      Member_2 = "NO ELIGIBLE SKIER",
      Member_3 = "NO ELIGIBLE SKIER",
      Member_4 = "NO ELIGIBLE SKIER",
      Is_Present = FALSE,
      Price = 0,
      Podium_Prob = 0,
      stringsAsFactors = FALSE
    ))
  }
  
  # Greedy algorithm: Start with best skiers in each leg
  team <- character(4)  # Team sprint only has 4 members
  for(leg in 1:4) {
    if(nrow(leg_candidates[[leg]]) > 0) {
      team[leg] <- leg_candidates[[leg]]$Skier[1]
    } else {
      team[leg] <- "NO ELIGIBLE SKIER"
    }
  }
  
  # Handle case where a skier might be assigned to multiple legs (shouldn't happen in mixed relay
  # with proper sex filtering, but include as safeguard)
  for(i in 1:4) {
    # If this skier appears elsewhere in the team
    if(team[i] != "NO ELIGIBLE SKIER" && sum(team == team[i]) > 1) {
      log_warn(paste("Skier", team[i], "appears in multiple legs - adjusting"))
      
      # Get positions where this skier appears
      positions <- which(team == team[i])
      
      # Keep the position with highest weighted probability
      best_pos <- positions[1]
      best_prob <- 0
      
      for(pos in positions) {
        idx <- match(team[pos], leg_candidates[[pos]]$Skier)
        if(!is.na(idx)) {
          prob <- leg_candidates[[pos]]$Weighted_Prob[idx]
          if(prob > best_prob) {
            best_prob <- prob
            best_pos <- pos
          }
        }
      }
      
      # Replace skier in other positions
      for(pos in positions) {
        if(pos != best_pos) {
          # Find replacement (next best not already in team)
          candidates <- leg_candidates[[pos]]
          for(j in 1:nrow(candidates)) {
            if(!candidates$Skier[j] %in% team) {
              team[pos] <- candidates$Skier[j]
              break
            }
          }
          # If no replacement found
          if(team[pos] == team[best_pos]) {
            team[pos] <- "NO ELIGIBLE SKIER"
          }
        }
      }
    }
  }
  
  # Refinement steps - try to improve by swapping skiers
  best_team <- team
  best_score <- calculate_team_score_weighted(team, nation, leg_predictions, "Podium_Prob", leg_weights)
  
  for(iter in 1:iterations) {
    improved <- FALSE
    
    # Try improving each leg
    for(leg in 1:4) {
      if(nrow(leg_candidates[[leg]]) <= 1) next
      
      current_skier <- team[leg]
      for(j in 1:min(nrow(leg_candidates[[leg]]), 10)) {
        candidate <- leg_candidates[[leg]]$Skier[j]
        
        # Skip if already in team or same as current
        if(candidate %in% team || candidate == current_skier) next
        
        # Try this swap
        test_team <- team
        test_team[leg] <- candidate
        test_score <- calculate_team_score_weighted(test_team, nation, leg_predictions, "Podium_Prob", leg_weights)
        
        if(test_score > best_score) {
          best_team <- test_team
          best_score <- test_score
          improved <- TRUE
        }
      }
    }
    
    # Update team if improved
    if(improved) {
      team <- best_team
    } else {
      # No further improvement
      break
    }
  }
  
  # Create team dataframe
  optimized_team <- data.frame(
    Team_Name = paste(nation, "I"),
    Nation = nation,
    Team_Rank = 0,
    Member_1 = best_team[1],
    Member_2 = best_team[2],
    Member_3 = best_team[3],
    Member_4 = best_team[4],
    Is_Present = FALSE,
    Price = 0,
    Podium_Prob = best_score,
    stringsAsFactors = FALSE
  )
  
  return(optimized_team)
}


## Step 9: Fantasy Team Optimization with Knapsack

# Function to optimize fantasy team selection using knapsack algorithm
optimize_fantasy_team <- function(team_predictions, max_price = 100000, max_teams = 12) {
  # Ensure Price is numeric
  team_predictions <- team_predictions %>%
    mutate(Price = as.numeric(Price))
  
  # Remove teams with NA prices
  team_predictions <- team_predictions %>%
    filter(!is.na(Price) & Price > 0)
  
  # Set up the model
  n <- nrow(team_predictions)
  model <- MIPModel() %>%
    # Binary decision variables (1 if selected, 0 otherwise)
    add_variable(x[i], i = 1:n, type = "binary") %>%
    
    # Maximize expected points
    set_objective(sum_expr(team_predictions$Expected_Points[i] * x[i], i = 1:n), "max") %>%
    
    # Budget constraint
    add_constraint(sum_expr(team_predictions$Price[i] * x[i], i = 1:n) <= max_price) %>%
    
    # Maximum number of teams constraint
    add_constraint(sum_expr(x[i], i = 1:n) <= max_teams)
  
  # Solve the model
  result <- solve_model(model, with_ROI(solver = "glpk"))
  
  # Extract selected teams
  selected <- get_solution(result, x[i]) %>%
    filter(value > 0) %>%
    pull(i)
  
  # Create results dataframe
  fantasy_team <- team_predictions[selected, ] %>%
    arrange(desc(Expected_Points))
  
  # Calculate total stats
  total_price <- sum(fantasy_team$Price)
  total_points <- sum(fantasy_team$Expected_Points)
  
  # Also get teams per nation for reference
  teams_per_nation <- fantasy_team %>%
    group_by(Nation) %>%
    summarise(Count = n())
  
  log_info(paste("Fantasy team optimization complete. Selected", nrow(fantasy_team), "teams."))
  log_info(paste("Total price:", total_price, "of max", max_price))
  log_info(paste("Expected total points:", round(total_points, 1)))
  
  return(list(
    team = fantasy_team,
    total_price = total_price,
    total_points = total_points,
    teams_per_nation = teams_per_nation
  ))
}


## Step 10: Save Results to Excel Files

# Function to save prediction results to Excel files
# Function to save prediction results to Excel files
save_prediction_results <- function(team_predictions, race_date, gender, output_dir = NULL) {
  # Create formatted date string
  date_str <- format(race_date, "%Y%m%d")
  
  # Set default output directory if not provided
  if(is.null(output_dir)) {
    output_dir <- paste0("~/blog/daehl-e/content/post/cross-country/drafts/race-picks/", date_str)
  }
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Check if we have team member columns
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
  prob_file <- file.path(output_dir, "mixed_position_probabilities.xlsx")
  write.xlsx(prob_df, prob_file)
  log_info(paste("Saved probability predictions to", prob_file))
  
  # Return file paths
  return(list(
    points_file = points_file,
    prob_file = prob_file
  ))
}

# Function to save fantasy team results to Excel
save_fantasy_results <- function(fantasy_team, race_date, output_dir = NULL) {
  # Use UTC time for consistent date formatting
  utc_date_str <- format(Sys.time(), "%Y%m%d", tz = "UTC")
  
  # Set default output directory if not provided - use race-picks instead of weekly-picks
  if(is.null(output_dir)) {
    output_dir <- paste0("~/blog/daehl-e/content/post/cross-country/drafts/race-picks/", utc_date_str)
  }
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Prepare fantasy team data - top 20 teams by Expected_Points (mixed relay teams are already mixed gender)
  fantasy_df <- fantasy_team$team %>%
    arrange(desc(Expected_Points)) %>%
    head(20) %>%
    select(Team_Name, Nation, Price, Expected_Points)

  # Add team members if available
  if("Member_1" %in% names(fantasy_team$team)) {
    top20_teams <- fantasy_team$team %>%
      arrange(desc(Expected_Points)) %>%
      head(20)
    fantasy_df <- bind_cols(
      fantasy_df,
      top20_teams %>% select(starts_with("Member_"))
    )
  }

  # Rename columns to user-friendly format
  fantasy_df <- fantasy_df %>%
    rename(
      "Team" = Team_Name,
      "Expected Points" = Expected_Points
    )
  # Rename Member columns to Leg columns (mixed relay has 4 legs)
  for (i in 1:4) {
    old_name <- paste0("Member_", i)
    new_name <- paste0("Leg ", i)
    if (old_name %in% names(fantasy_df)) {
      fantasy_df <- fantasy_df %>% rename(!!new_name := !!old_name)
    }
  }

  # Save fantasy team results
  fantasy_file <- file.path(output_dir, "fantasy_mixed_relay_team.xlsx")
  write.xlsx(list(
    Team = fantasy_df
  ), fantasy_file)
  log_info(paste("Saved top 20 fantasy mixed relay team results to", fantasy_file))
  
  return(fantasy_file)
}


## Execution Section

run_mixed_relay_predictions <- function() {
  # Step 1: Find upcoming races
  race_info <- load_upcoming_races()
  if(is.null(race_info$race_date)) {
    log_error("No upcoming mixed relay races found. Exiting.")
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
  
  
  # Check if startlists have valid FIS entries
  has_fis <- has_valid_fis_entries(mixed_startlists$individuals)
  
  # Prepare current skier data (pass startlist for Elo values from chrono_pred)
  current_skiers <- prepare_current_skiers(chrono_data, current_season, mixed_startlists$individuals)
  
  
  # Get leg predictions
  if(has_fis) {
    log_info("Using FIS startlist for mixed relay leg predictions")
    leg_predictions <- get_leg_predictions_with_startlist(
      current_skiers,
      leg_models,
      mixed_startlists$individuals
    )
  } else {
    log_info("No FIS startlist, predicting for all skiers in all legs")
    leg_predictions <- list()
    for(leg in 1:4) {
      # Filter by appropriate sex for each leg
      required_sex <- if(leg %in% c(1, 3)) "F" else "M"
      leg_skiers <- current_skiers %>% filter(Sex == required_sex)
      
      leg_predictions[[leg]] <- get_leg_predictions(leg, leg_skiers, leg_models)
    }
  }
  print(leg_predictions)
  # Generate team predictions
  if(has_fis) {
    log_info("Using FIS startlist for mixed relay teams")
    team_predictions <- generate_team_predictions(mixed_startlists$teams, leg_predictions, leg_models)
  } else {
    log_info("No valid FIS startlist, building optimized teams")
    team_predictions <- build_optimized_teams(current_skiers, leg_predictions)
  }
  print(team_predictions)
  # First reset probabilities at or below the mode
  log_info("Resetting probabilities at or below the mode to zero")
  team_predictions <- reset_mode_probabilities(team_predictions)
  
  # Normalize team probabilities
  log_info("Normalizing team probabilities")
  team_predictions <- normalize_probabilities(team_predictions)
  
  # OLD METHOD: Knapsack optimization (commented out)
  # log_info("Optimizing fantasy mixed relay team")
  # fantasy_team <- optimize_fantasy_team(team_predictions)

  # NEW METHOD: Simply pass all teams - save_fantasy_results will take top 20 men + top 20 ladies
  log_info("Preparing fantasy mixed relay team (top 20 per gender)")
  fantasy_team <- list(team = team_predictions)

  # Save results
  prediction_files <- save_prediction_results(team_predictions, race_info$race_date)
  fantasy_file <- save_fantasy_results(fantasy_team, race_info$race_date)
  
  log_info("Mixed relay predictions completed successfully")
  
  return(list(
    team_predictions = team_predictions,
    fantasy_team = fantasy_team,
    race_date = race_info$race_date,
    output_files = c(prediction_files, fantasy_file)
  ))
  #return(-1)
}

# Uncomment to run the full pipeline
results <- run_mixed_relay_predictions()




