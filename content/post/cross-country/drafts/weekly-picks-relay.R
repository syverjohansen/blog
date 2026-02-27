
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

log_file <- file.path(log_dir, paste0("relay_predictions_", format(Sys.Date(), "%Y%m%d"), ".log"))
log_appender(appender_file(log_file))
log_threshold(INFO)
log_info("Starting relay predictions process")

# ===== TEST MODE (loaded from .env) =====
# Reads TEST_MODE from ~/ski/elo/.env for centralized pipeline configuration
load_env <- function(env_path = "~/ski/elo/.env") {
  env_file <- path.expand(env_path)
  if (file.exists(env_file)) {
    lines <- readLines(env_file, warn = FALSE)
    for (line in lines) {
      line <- trimws(line)
      if (nchar(line) > 0 && !startsWith(line, "#") && grepl("=", line)) {
        parts <- strsplit(line, "=", fixed = TRUE)[[1]]
        key <- trimws(parts[1])
        value <- trimws(paste(parts[-1], collapse = "="))
        value <- gsub("^[\"']|[\"']$", "", value)
        do.call(Sys.setenv, setNames(list(value), key))  # Dynamically set env var
      }
    }
  }
}
load_env()
TEST_MODE <- tolower(Sys.getenv("TEST_MODE", "false")) == "true"

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
  weekends_path <- if(TEST_MODE) {
    "~/ski/elo/python/ski/polars/excel365/test_weekends.csv"
  } else {
    "~/ski/elo/python/ski/polars/excel365/weekends.csv"
  }
  log_info(paste("Reading weekends from:", weekends_path))
  weekends <- read.csv(weekends_path, stringsAsFactors = FALSE) %>%
    mutate(Date = mdy(Date))
  
  # Find tomorrow's date
  tomorrow <- get_tomorrow_date()
  log_info(paste("Tomorrow's date:", tomorrow))
  
  # Filter for tomorrow's races
  tomorrow_races <- weekends %>%
    filter(Date == tomorrow)
  
  # Filter to only relay races
  relay_races <- tomorrow_races %>%
    filter(Distance == "Rel")
  
  # If no relay races found for tomorrow, look for the next relay races
  if (nrow(relay_races) == 0) {
    log_info("No relay races found for tomorrow, finding next relay race")
    next_relay_races <- weekends %>%
      filter(Date > Sys.Date(), Distance == "Rel") %>%
      arrange(Date)
    
    if (nrow(next_relay_races) > 0) {
      next_race_date <- min(next_relay_races$Date)
      log_info(paste("Next relay race found on:", next_race_date))
      relay_races <- weekends %>%
        filter(Date == next_race_date, Distance == "Rel")
    } else {
      log_warn("No upcoming relay races found in the schedule")
    }
  }
  
  # Split into men and ladies races
  men_races <- relay_races %>%
    filter(Sex == "M")
  
  ladies_races <- relay_races %>%
    filter(Sex == "L")
  
  log_info(paste("Found", nrow(men_races), "men's relay races and", 
                 nrow(ladies_races), "ladies' relay races"))
  
  return(list(
    men = men_races,
    ladies = ladies_races,
    race_date = if(nrow(relay_races) > 0) unique(relay_races$Date) else NULL
  ))
}


## Step 2: Load Chrono Files and Create Dataframes


# Function to load chrono files and create separate dataframes for relays and individual races
load_chrono_data <- function() {
  log_info("Loading chrono data")
  
  # Define file paths
  men_chrono_path <- "~/ski/elo/python/ski/polars/relay/excel365/men_chrono.csv"
  ladies_chrono_path <- "~/ski/elo/python/ski/polars/relay/excel365/ladies_chrono.csv"
  
  # Load data
  men_chrono <- read.csv(men_chrono_path, stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date))
  
  ladies_chrono <- read.csv(ladies_chrono_path, stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date))
  
  log_info(paste("Loaded men's chrono with", nrow(men_chrono), "rows and", 
                 ncol(men_chrono), "columns"))
  log_info(paste("Loaded ladies' chrono with", nrow(ladies_chrono), "rows and", 
                 ncol(ladies_chrono), "columns"))
  
  # Create separate dataframes for relays and individual races
  men_relays <- men_chrono %>% 
    filter(Distance == "Rel")
  
  men_individuals <- men_chrono %>% 
    filter(Distance != "Rel" & Distance != "Ts")
  
  ladies_relays <- ladies_chrono %>% 
    filter(Distance == "Rel")
  
  ladies_individuals <- ladies_chrono %>% 
    filter(Distance != "Rel" & Distance != "Ts")
  
  log_info(paste("Created men's relay df with", nrow(men_relays), "rows"))
  log_info(paste("Created men's individual df with", nrow(men_individuals), "rows"))
  log_info(paste("Created ladies' relay df with", nrow(ladies_relays), "rows"))
  log_info(paste("Created ladies' individual df with", nrow(ladies_individuals), "rows"))
  
  return(list(
    men_chrono = men_chrono,
    men_relays = men_relays,
    men_individuals = men_individuals,
    ladies_chrono = ladies_chrono,
    ladies_relays = ladies_relays,
    ladies_individuals = ladies_individuals
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

# Function to calculate Pelo percentages
# Improved function to calculate Pelo percentages with better error handling
# Inside create_pelo_pcts function, make sure the function returns the transformed data
create_pelo_pcts <- function(df) {
  # Define pelo_cols inside the function
  pelo_cols <- names(df)[grep("Pelo$", names(df))]
  
  # Check if any Pelo columns were found
  if(length(pelo_cols) == 0) {
    log_warn("No Pelo columns found in data")
    return(df)  # Return original data if no Pelo columns
  }
  
  # For each race and each Pelo column
  df_transformed <- df %>%
    group_by(Date, Race) %>%
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

# Then verify that these columns carry through all transformations
# After calling process_discipline_data, check if pelo_pct columns exist


# Function to process data for classic and freestyle races
process_discipline_data <- function(df_individuals, min_season = 2014) {
  min_season = max(df_individuals$Season)-11
  # First add points
  df_with_points <- add_points_to_results(df_individuals, is_relay = FALSE)
  
  # Process classic races
  classic_df <- df_with_points %>%
    filter(Distance != "Sprint", Technique == "C") %>%
    group_by(ID) %>%
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
          #NA_real_
        }
      })
    ) %>%
    ungroup()
  #print(classic_df %>% filter(Skier == "Johannes Høsflot Klæbo"))
  
  # Process freestyle races
  freestyle_df <- df_with_points %>%
    filter(Distance != "Sprint", Technique == "F") %>%
    group_by(ID) %>%
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
          #NA_real_
        }
      })
    ) %>%
    ungroup()
  #print(freestyle_df %>% filter(Skier == "Johannes Høsflot Klæbo"))
  
  return(list(
    classic = classic_df,
    freestyle = freestyle_df
  ))
}

# Function to process relay data with classic and freestyle legs
process_relay_data <- function(df_relays, classic_df, freestyle_df, min_season = 2014) {
  # Add points to relay results
  min_season = max(classic_df$Season)-11
  relay_with_points <- add_points_to_results(df_relays, is_relay = TRUE)
  
  # Process classic legs (1-2) - don't filter by season yet
  classic_legs_all <- relay_with_points %>%
    filter(Distance == "Rel", Leg < 3)
  
  # Process freestyle legs (3-4) - don't filter by season yet
  freestyle_legs_all <- relay_with_points %>%
    filter(Distance == "Rel", Leg > 2)
  
  # Combine classic legs with classic individual data
  classic_combined <- bind_rows(
    classic_legs_all,
    classic_df
  ) %>%
    group_by(ID) %>%
    arrange(Date, Season, Race, desc(Distance)) %>%  # Use Date for chronological order
    fill(Weighted_Last_5, .direction = "down") %>%
    filter(Distance == "Rel", Season > min_season) %>%  # Apply season filter AFTER filling
    group_by(Season, Race) %>%  # Regroup by race for quartile replacement
    mutate(
      Weighted_Last_5 = ifelse(
        is.na(Weighted_Last_5),
        quantile(Weighted_Last_5, 0.25, na.rm = TRUE),
        Weighted_Last_5
      )
    ) %>%
    ungroup()
  #print(classic_combined %>% filter(Skier == "Johannes Høsflot Klæbo"))
  # Combine freestyle legs with freestyle individual data
  freestyle_combined <- bind_rows(
    freestyle_legs_all,
    freestyle_df
  ) %>%
    group_by(ID) %>%
    arrange(Date, Season, Race, desc(Distance)) %>%  # Use Date for chronological order
    fill(Weighted_Last_5, .direction = "down") %>%
    filter(Distance == "Rel", Season > min_season) %>%  # Apply season filter AFTER filling
    group_by(Season, Race) %>%  # Regroup by race for quartile replacement
    mutate(
      Weighted_Last_5 = ifelse(
        is.na(Weighted_Last_5),
        quantile(Weighted_Last_5, 0.25, na.rm = TRUE),
        Weighted_Last_5
      )
    ) %>%
    ungroup()
  #print(freestyle_combined %>% filter(Skier == "Johannes Høsflot Klæbo"))
  
  return(list(
    classic_legs = classic_combined,
    freestyle_legs = freestyle_combined
  ))
}

# Function to prepare leg-specific datasets
prepare_leg_data <- function(classic_legs, freestyle_legs) {
  # Create datasets for each leg
  leg_data <- list()
  
  # Legs 1 and 2 (Classic)
  for(i in 1:2) {
    leg_data[[i]] <- classic_legs %>%
      filter(Leg == i) %>%
      mutate(
        is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
        is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
        is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
        is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
      )
  }
  
  # Legs 3 and 4 (Freestyle)
  for(i in 3:4) {
    leg_data[[i]] <- freestyle_legs %>%
      filter(Leg == i) %>%
      mutate(
        is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
        is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
        is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
        is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
      )
  }
  return(leg_data)
}

# Function to get leg-specific predictor columns
get_leg_predictors <- function(leg, leg_data) {
  # Get column names from the leg data
  base_cols <- names(leg_data[[leg]])
  
  # Define predictors based on leg
  if(leg <= 2) {
    # Classic legs (1 and 2)
    predictors <- c(
      grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE),
      grep("Classic.*Pelo_Pct$", base_cols, value = TRUE),
      "Distance_Pelo_Pct",
      "Pelo_Pct",
      "Weighted_Last_5"
    )
  } else if (leg==3){
    # Freestyle legs (3 and 4)
    predictors <- c(
      grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE),
      grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE),
      "Distance_Pelo_Pct",
      "Pelo_Pct",
      "Weighted_Last_5"
    )
  }
  else if(leg ==4){
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
# Fixed function to train models for each leg
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
  train_model_safe <- function(formula, data, method = "glm", target_name) {
    log_info(paste("Training", target_name, "model using", method))
    
    if (method == "xgbTree") {
      # Try XGBoost first
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
          log_warn(paste("GLM training also failed:", e2$message, "- using basic glm"))
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
      # Try GLM directly
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



# Get leg predictions for all current skiers when you have a FIS startlist
get_leg_predictions_with_startlist <- function(men_current, leg_models, startlist_individuals) {
  # Create a list to store predictions for each leg
  leg_predictions <- list()
  
  # Process each leg
  for(leg in 1:4) {
    # Filter the startlist to get skiers for this leg
    leg_skiers <- startlist_individuals %>%
      filter(Team_Position == leg) %>%
      select(ID, Skier, Nation, Team_Name)
    
    # Filter current_skiers to only include those in this leg's startlist
    leg_data <- men_current %>%
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


# Modified function to get leg-specific predictions
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
        return(pmin(probs[,"Yes"], 1))  # Cap at 1
      } else if(is.numeric(probs)) {
        return(pmin(probs, 1))  # Cap at 1
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
    Skier = pred_data$Skier,
    ID = pred_data$ID,
    Nation = pred_data$Nation,
    Sex = pred_data$Sex,
    Leg = leg_number,
    Win_Prob = pmin(win_probs, 1),  # Ensure capped at 1
    Podium_Prob = pmin(podium_probs, 1),  # Ensure capped at 1
    Top5_Prob = pmin(top5_probs, 1),  # Ensure capped at 1
    Top10_Prob = pmin(top10_probs, 1)  # Ensure capped at 1
  ))
}

# Function to load current relay startlists
load_relay_startlists <- function(gender) {
  gender_prefix <- ifelse(gender == "men", "men", "ladies")
  
  # Define file paths
  teams_path <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_relay_teams_%s.csv", gender_prefix)
  individuals_path <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_relay_individuals_%s.csv", gender_prefix)
  
  # Load data
  teams <- read.csv(teams_path, stringsAsFactors = FALSE)
  individuals <- read.csv(individuals_path, stringsAsFactors = FALSE)
  if(gender=="men"){
    teams$Sex = "M"
    individuals$Sex = "M"
  }
  else{
    teams$Sex = "L"
    individuals$Sex = "L"    
  }
  
  log_info(paste("Loaded", gender_prefix, "relay teams startlist with", nrow(teams), "rows"))
  log_info(paste("Loaded", gender_prefix, "relay individuals startlist with", nrow(individuals), "rows"))
  
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
prepare_current_skiers <- function(chrono_data, current_season, gender = "men", startlist_individuals = NULL) {
  log_info(paste("Preparing current", gender, "skier data"))

  # Use the appropriate gender data
  gender_prefix <- ifelse(gender == "men", "men", "ladies")
  chrono_gender <- chrono_data[[paste0(gender_prefix, "_chrono")]]
  classic_df <- chrono_data$processed[[gender_prefix]]$classic
  freestyle_df <- chrono_data$processed[[gender_prefix]]$freestyle

  # Get all skiers from current season
  current_skiers <- chrono_gender %>%
    filter(Season == current_season) %>%
    select(Skier, ID, Nation, Sex) %>%
    distinct()

  # Get latest Elo values - prefer startlist (from chrono_pred), fallback to chrono_data
  if (!is.null(startlist_individuals) && any(grepl("Elo$", names(startlist_individuals)))) {
    log_info("Using Elo values from startlist (chrono_pred source)")
    elo_cols <- names(startlist_individuals)[grepl("Elo$", names(startlist_individuals))]
    latest_elo <- startlist_individuals %>%
      filter(ID %in% current_skiers$ID) %>%
      select(ID, any_of(elo_cols)) %>%
      distinct()
    log_info(paste("Found", length(elo_cols), "Elo columns from startlist"))
  } else {
    log_info("Falling back to chrono_data for Elo values")
    latest_elo <- chrono_gender %>%
      filter(ID %in% current_skiers$ID) %>%
      group_by(ID) %>%
      arrange(desc(Season), desc(Race)) %>%
      dplyr::slice(1) %>%
      select(ID, ends_with("Elo")) %>%
      ungroup()
    log_info(paste("Found", sum(grepl("Elo$", names(latest_elo))), "Elo columns for current skiers"))
  }
  
  # Recalculate Weighted_Last_5 for classic races
  classic_last5 <- classic_df %>%
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
    select(ID, Classic_Last_5) %>%
    ungroup()
  
  # Recalculate Weighted_Last_5 for freestyle races
  freestyle_last5 <- freestyle_df %>%
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
    select(ID, Freestyle_Last_5) %>%
    ungroup()
  
  # Combine all data
  current_df <- current_skiers %>%
    left_join(latest_elo, by = "ID") %>%
    left_join(classic_last5, by = "ID") %>%
    left_join(freestyle_last5, by = "ID")
  
  # Replace NAs with quartile values
  for(col in names(current_df)) {
    if(is.numeric(current_df[[col]]) && any(is.na(current_df[[col]]))) {
      q1 <- quantile(current_df[[col]], 0.25, na.rm = TRUE)
      if(is.na(q1)) q1 <- 0  # Default to 0 if quartile calculation fails
      
      current_df[[col]] <- ifelse(is.na(current_df[[col]]), q1, current_df[[col]])
      log_info(paste("Replaced NAs in", col, "with first quartile:", q1))
    }
  }
  
  # Calculate Pelo_Pct values
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
  log_info(paste("Prepared data for", nrow(current_df), gender, "skiers with", 
                 ncol(current_df), "features"))
  
  return(current_df)
}




# Function to generate team predictions using individual leg probabilities
# Function to generate team predictions using individual leg probabilities
# Modified function to generate team predictions
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
      
      # Cap at 1
      team_predictions$Podium_Prob[i] <- min(weighted_podium, 1)
      team_predictions$Win_Prob[i] <- min(weighted_win, 1)
      team_predictions$Top5_Prob[i] <- min(weighted_top5, 1)
      team_predictions$Top10_Prob[i] <- min(weighted_top10, 1)
      
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

train_leg_importance_model <- function(relay_data, leg_models) {
  # First, create a dataset of historical team results with leg probabilities
  team_data <- data.frame()
  print(relay_data)
  # Group relay data by team and race
  relay_teams <- relay_data %>%
    group_by(Season, Race, Place, Nation) %>%
    summarize(
      Team_Place = first(Place),
      Team_Podium = as.factor(ifelse(Team_Place <= 3, "Yes", "No")),
      .groups = "drop"
    )
  
  # For each team and race, get the individual leg probabilities
  for(i in 1:nrow(relay_teams)) {
    season <- relay_teams$Season[i]
    race <- relay_teams$Race[i]
    team <- relay_teams$Nation[i]
    place <- relay_teams$Place[i]
    
    # Get the individual skiers for this team
    team_skiers <- relay_data %>%
      filter(Season == season, Race == race, Nation == team, Place==place) %>%
      arrange(Leg)
    
    # Only include complete teams
    if(nrow(team_skiers) == 4) {
      row_data <- data.frame(
        Season = season,
        Race = race,
        Team_Name = team,
        Team_Podium = relay_teams$Team_Podium[i]
      )
      
      # For each leg, predict podium probability
      for(leg in 1:4) {
        skier_data <- team_skiers %>% filter(Leg == leg)
        
        if(nrow(skier_data) == 1 && !is.null(leg_models[[leg]]$podium)) {
          # Predict using the leg-specific model
          row_data[[paste0("leg", leg, "_prob")]] <- predict(
            leg_models[[leg]]$podium, 
            newdata = skier_data, 
            type = "prob"
          )[, "Yes"]
        } else {
          row_data[[paste0("leg", leg, "_prob")]] <- NA
        }
      }
      
      team_data <- bind_rows(team_data, row_data)
    }
  }
  
  # Remove rows with missing data
  team_data <- team_data %>%
    filter(complete.cases(select(., starts_with("leg"))))
  
  # Create training control
  control <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE
  )
  
  # Train a logistic regression model with no intercept
  # This forces the model to learn weights for each leg
  importance_model <- train(
    as.formula("Team_Podium ~ 0 + leg1_prob + leg2_prob + leg3_prob + leg4_prob"),
    data = team_data,
    method = "glm",
    family = "binomial",
    trControl = control
  )
  
  # Extract the coefficients as importance weights
  coeffs <- coef(importance_model$finalModel)
  leg_importance <- abs(coeffs)
  
  # Normalize to sum to 1
  leg_importance <- leg_importance / sum(leg_importance)
  
  # Name the weights for clarity
  names(leg_importance) <- paste0("Leg", 1:4)
  
  return(list(
    model = importance_model,
    weights = leg_importance,
    training_data = team_data
  ))
}

# Function to calculate leg importance from historical data
calculate_leg_importance <- function(leg_models) {
  # Debug which option is being used
  log_info("Attempting Option 1: Team model coefficients")
  
  # Option 1: Use model coefficients if available
  option1_result <- tryCatch({
    # Try extracting from team-level model if it exists
    if("team_podium" %in% names(leg_models)) {
      team_importance <- varImp(leg_models$team_podium)$importance$Overall
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
  
  # Option 3: Use default weights with emphasis on later legs
  log_info("Using Option 3: Default leg importance weights")
  default_weights <- c(0.2, 0.2, 0.25, 0.35)  # Slight emphasis on later legs
  return(default_weights)
}


# Function to cap probability values at 1
cap_probabilities <- function(team_predictions) {
  # Probability columns to process
  prob_cols <- c("Win_Prob", "Podium_Prob", "Top5_Prob", "Top10_Prob")
  
  # Cap each probability column at 1
  for(prob_col in prob_cols) {
    if(prob_col %in% names(team_predictions)) {
      # Cap values at 1
      team_predictions[[prob_col]] <- pmin(team_predictions[[prob_col]], 1)
      
      # Log any modifications
      capped_count <- sum(team_predictions[[prob_col]] == 1)
      if(capped_count > 0) {
        log_info(paste("Capped", capped_count, "values at 1 for", prob_col))
      }
    }
  }
  
  return(team_predictions)
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
  
  # Cap all probability values at 1
  team_predictions <- cap_probabilities(team_predictions)
  
  return(team_predictions)
}

# Function to reset probabilities at or below the mode to zero
# Function to reset probabilities that occur more than twice to zero
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


# Modified function to build optimized teams
build_optimized_teams <- function(current_skiers, leg_predictions, leg_models, gender = NULL) {
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
    optimized_teams[[nation]] <- get_optimal_weighted_team(nation, leg_predictions, leg_importance)
  }
  
  # Combine all teams into a single dataframe
  all_teams <- bind_rows(optimized_teams)

  # Calculate probabilities and expected points using the team model
  all_teams <- all_teams %>%
    rowwise() %>%
    mutate(
      Win_Prob = min(calculate_team_score_weighted(
        c(Member_1, Member_2, Member_3, Member_4), 
        Nation, 
        leg_predictions, 
        "Win_Prob",
        leg_importance
      ), 1),
      Podium_Prob = min(calculate_team_score_weighted(
        c(Member_1, Member_2, Member_3, Member_4), 
        Nation, 
        leg_predictions, 
        "Podium_Prob",
        leg_importance
      ), 1),
      Top5_Prob = min(calculate_team_score_weighted(
        c(Member_1, Member_2, Member_3, Member_4), 
        Nation, 
        leg_predictions, 
        "Top5_Prob",
        leg_importance
      ), 1),
      Top10_Prob = min(calculate_team_score_weighted(
        c(Member_1, Member_2, Member_3, Member_4), 
        Nation, 
        leg_predictions, 
        "Top10_Prob",
        leg_importance
      ), 1),
      Expected_Points = Win_Prob * relay_points[1] +
        (Podium_Prob - Win_Prob) * mean(relay_points[2:3]) +
        (Top5_Prob - Podium_Prob) * mean(relay_points[4:5]) +
        (Top10_Prob - Top5_Prob) * mean(relay_points[6:10])
    ) %>%
    ungroup()

  
  # Load startlist files to get team names and prices
  gender_suffix <- ifelse(gender == "men", "men", "ladies")
  individuals_file <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_relay_individuals_%s.csv", gender_suffix)
  teams_file <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_relay_teams_%s.csv", gender_suffix)
  
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
  
  # Add gender column
  all_teams$Gender <- gender

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
      # Get skier's probability for this leg
      leg_pred <- leg_predictions[[leg]] %>%
        filter(Nation == nation, Skier == skiers[leg])
      
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

# Optimized team selection with weighted importance
get_optimal_weighted_team <- function(nation, leg_predictions, leg_weights, iterations = 3) {
  # Get candidates for each leg, sorted by their weighted probability
  leg_candidates <- list()
  for(leg in 1:4) {
    # Get skiers for this nation and leg
    skiers <- leg_predictions[[leg]] %>%
      filter(Nation == nation) %>%
      mutate(Weighted_Prob = Podium_Prob * leg_weights[leg]) %>%
      arrange(desc(Weighted_Prob))
    
    if(nrow(skiers) > 0) {
      # Take top candidates (maximum 15 per leg)
      leg_candidates[[leg]] <- skiers %>% 
        head(min(15, nrow(skiers))) %>%
        select(Skier, Podium_Prob, Weighted_Prob)
    } else {
      leg_candidates[[leg]] <- data.frame(
        Skier = character(0),
        Podium_Prob = numeric(0),
        Weighted_Prob = numeric(0)
      )
    }
  }
  
  # Greedy algorithm: Start with best skiers in each leg
  team <- character(4)
  for(leg in 1:4) {
    if(nrow(leg_candidates[[leg]]) > 0) {
      team[leg] <- leg_candidates[[leg]]$Skier[1]
    } else {
      team[leg] <- "NO ELIGIBLE SKIER"
    }
  }
  
  # Remove duplicates by replacing with next best
  for(i in 1:4) {
    # If this skier appears elsewhere in the team
    if(team[i] != "NO ELIGIBLE SKIER" && sum(team == team[i]) > 1) {
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

# Function to optimize fantasy team selection using knapsack algorithm
optimize_fantasy_team <- function(team_predictions, max_price = 100000, max_per_gender = 6) {
  # Ensure Price is numeric
  team_predictions <- team_predictions %>%
    mutate(Price = as.numeric(Price))

  
  # Remove teams with NA prices
  team_predictions <- team_predictions %>%
    filter(!is.na(Price) & Price > 0)
  
  # Get indices for men and women
  men_indices <- which(team_predictions$Gender == "men")
  women_indices <- which(team_predictions$Gender == "ladies")
  
  # Set up the model
  n <- nrow(team_predictions)
  model <- MIPModel() %>%
    # Binary decision variables (1 if selected, 0 otherwise)
    add_variable(x[i], i = 1:n, type = "binary") %>%
    
    # Maximize expected points
    set_objective(sum_expr(team_predictions$Expected_Points[i] * x[i], i = 1:n), "max") %>%
    
    # Budget constraint
    add_constraint(sum_expr(team_predictions$Price[i] * x[i], i = 1:n) <= max_price)
  
  # Add gender constraints if there are teams for each gender
  if(length(men_indices) > 0) {
    model <- model %>%
      add_constraint(sum_expr(x[i], i = men_indices) <= max_per_gender)
  }
  
  if(length(women_indices) > 0) {
    model <- model %>%
      add_constraint(sum_expr(x[i], i = women_indices) <= max_per_gender)
  }
  
  # Solve the model
  result <- solve_model(model, with_ROI(solver = "glpk"))
  
  # Extract selected teams
  selected <- get_solution(result, x[i]) %>%
    filter(value > 0) %>%
    pull(i)
  
  # Create results dataframe
  fantasy_team <- team_predictions[selected, ] %>%
    # Add simple M/L gender column
    mutate(Gender_Code = ifelse(Gender == "men", "M", "L")) %>%
    arrange(Gender, desc(Expected_Points))
  
  # Calculate total stats
  total_price <- sum(fantasy_team$Price)
  total_points <- sum(fantasy_team$Expected_Points)
  
  # Get teams per gender
  teams_per_gender <- fantasy_team %>%
    group_by(Gender) %>%
    summarise(Count = n())
  
  # Also get teams per nation for reference
  teams_per_nation <- fantasy_team %>%
    group_by(Nation) %>%
    summarise(Count = n())
  
  log_info(paste("Fantasy team optimization complete. Selected", nrow(fantasy_team), "teams."))
  log_info(paste("Total price:", total_price, "of max", max_price))
  log_info(paste("Expected total points:", round(total_points, 1)))
  log_info(paste("Men's teams:", sum(fantasy_team$Gender == "men"), "(" , sum(fantasy_team$Gender_Code == "M"), ")"))
  log_info(paste("Ladies' teams:", sum(fantasy_team$Gender == "ladies"), "(" , sum(fantasy_team$Gender_Code == "L"), ")"))
  
  return(list(
    team = fantasy_team,
    total_price = total_price,
    total_points = total_points,
    teams_per_gender = teams_per_gender,
    teams_per_nation = teams_per_nation
  ))
}

# Function to save prediction results to Excel files
save_prediction_results <- function(team_predictions, race_date, gender, output_dir = NULL) {
  # Create formatted date string
  date_str <- format(race_date, "%Y%m%d")
  
  # Set default output directory if not provided
  if(is.null(output_dir)) {
    output_dir <- paste0("~/blog/daehl-e/content/post/cross-country/drafts/weekly-picks/", date_str)
  }
  
  # Create directory if it doesn't exist
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Check if member columns exist
  has_members <- any(grepl("^Member_", names(team_predictions)))
  
  # Create points dataframe with correct columns
  if(has_members) {
    # Include member columns in the original selection to maintain row correspondence
    points_df <- team_predictions %>%
      select(Team_Name, starts_with("Member_"), Expected_Points) %>%
      arrange(desc(Expected_Points))
  } else {
    points_df <- team_predictions %>%
      select(Team_Name, Expected_Points) %>%
      arrange(desc(Expected_Points))
  }
  
  # Create probability dataframe with correct columns
  if(has_members) {
    # Include member columns in the original selection to maintain row correspondence
    prob_df <- team_predictions %>%
      select(Team_Name, starts_with("Member_"), Win_Prob, Podium_Prob, Top5_Prob, Top10_Prob) %>%
      arrange(desc(Podium_Prob))
  } else {
    prob_df <- team_predictions %>%
      select(Team_Name, Win_Prob, Podium_Prob, Top5_Prob, Top10_Prob) %>%
      arrange(desc(Podium_Prob))
  }
  
  # Save points results
  points_file <- file.path(output_dir, paste0(gender, "_relay.xlsx"))
  write.xlsx(points_df, points_file)
  log_info(paste("Saved points predictions to", points_file))
  
  # Save probability results
  prob_file <- file.path(output_dir, paste0(gender, "_relay_position_probabilities.xlsx"))
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
  
  # Prepare fantasy team data - top 20 men and top 20 ladies by Expected_Points (or max available)
  men_teams <- fantasy_team$team %>%
    filter(Gender == "men") %>%
    arrange(desc(Expected_Points)) %>%
    head(20)

  ladies_teams <- fantasy_team$team %>%
    filter(Gender == "ladies") %>%
    arrange(desc(Expected_Points)) %>%
    head(20)

  fantasy_df <- bind_rows(men_teams, ladies_teams) %>%
    select(Team_Name, Gender, Price, Expected_Points)

  # Add team members if available
  if("Member_1" %in% names(fantasy_team$team)) {
    combined_teams <- bind_rows(men_teams, ladies_teams)
    fantasy_df <- bind_cols(
      fantasy_df,
      combined_teams %>% select(starts_with("Member_"))
    )
  }

  # Sort by Gender then Expected_Points
  fantasy_df <- fantasy_df %>%
    arrange(Gender, desc(Expected_Points))

  # Rename columns to user-friendly format
  fantasy_df <- fantasy_df %>%
    rename(
      "Team" = Team_Name,
      "Expected Points" = Expected_Points
    )
  # Rename Member columns to Leg columns
  for (i in 1:4) {
    old_name <- paste0("Member_", i)
    new_name <- paste0("Leg ", i)
    if (old_name %in% names(fantasy_df)) {
      fantasy_df <- fantasy_df %>% rename(!!new_name := !!old_name)
    }
  }

  # Save fantasy team results
  fantasy_file <- file.path(output_dir, "fantasy_relay_team.xlsx")
  write.xlsx(list(
    Team = fantasy_df
  ), fantasy_file)
  log_info(paste("Saved top 20 men and top 20 ladies fantasy team results to", fantasy_file))
  
  return(fantasy_file)
}

run_relay_predictions <- function() {
  # Step 1: Find upcoming races
  race_info <- load_upcoming_races()
  if(is.null(race_info$race_date)) {
    log_error("No upcoming relay races found. Exiting.")
    return(NULL)
  }
  
  
  
  # Step 2: Load chrono data
  chrono_data <- load_chrono_data()
  
  # Step 3: Process data with Pelo percentages
  chrono_data$men_chrono <- create_pelo_pcts(chrono_data$men_chrono)
  chrono_data$ladies_chrono <- create_pelo_pcts(chrono_data$ladies_chrono)
  chrono_data$men_relays <- create_pelo_pcts(chrono_data$men_relays)
  chrono_data$ladies_relays <- create_pelo_pcts(chrono_data$ladies_relays)
  chrono_data$men_individuals <- create_pelo_pcts(chrono_data$men_individuals)
  chrono_data$ladies_individuals <- create_pelo_pcts(chrono_data$ladies_individuals)
  
  
  # Step 4: Process classic and freestyle data
  men_discipline <- process_discipline_data(chrono_data$men_individuals)
  ladies_discipline <- process_discipline_data(chrono_data$ladies_individuals)
  
  # Add processed data to chrono_data
  chrono_data$processed <- list(
    men = men_discipline,
    ladies = ladies_discipline
  )
  
  
  # Step 5: Process relay data
  men_relay_processed <- process_relay_data(
    chrono_data$men_relays,
    men_discipline$classic,
    men_discipline$freestyle
  )
  
  
  
  ladies_relay_processed <- process_relay_data(
    chrono_data$ladies_relays,
    ladies_discipline$classic,
    ladies_discipline$freestyle
  )
  
  # Step 6: Prepare leg data and train models
  men_leg_data <- prepare_leg_data(
    men_relay_processed$classic_legs,
    men_relay_processed$freestyle_legs
  )
  
  
  ladies_leg_data <- prepare_leg_data(
    ladies_relay_processed$classic_legs,
    ladies_relay_processed$freestyle_legs
  )
  
  # Train leg models
  men_leg_models <- train_leg_models(men_leg_data)
  
  
  ladies_leg_models <- train_leg_models(ladies_leg_data)
  
  #   log_info("Training men's leg importance model")
  # men_leg_importance_results <- train_leg_importance_model(
  #   bind_rows(men_relay_processed$classic_legs, men_relay_processed$freestyle_legs),
  #   men_leg_models
  # )
  # 
  # # Store the model in leg_models
  # men_leg_models$team_podium <- men_leg_importance_results$model
  # 
  # # Train leg importance model for ladies
  # log_info("Training ladies' leg importance model")
  # ladies_leg_importance_results <- train_leg_importance_model(
  #   bind_rows(ladies_relay_processed$classic_legs, ladies_relay_processed$freestyle_legs),
  #   ladies_leg_models
  # )
  
  # Store the model in leg_models
  # ladies_leg_models$team_podium <- ladies_leg_importance_results$model
  # 
  # # Display the learned leg importance weights
  # log_info("Men's leg importance weights:")
  # print(men_leg_importance_results$weights)
  # 
  # log_info("Ladies' leg importance weights:")
  # print(ladies_leg_importance_results$weights)
  # 
  # # Get current season
  current_season <- max(chrono_data$men_chrono$Season, na.rm = TRUE)
  
  # Process for men
  log_info("Processing men's relay predictions")
  
  # Load men's startlists
  men_startlists <- load_relay_startlists("men")
  
  # # Check if startlists have valid FIS entries
  men_has_fis <- has_valid_fis_entries(men_startlists$individuals)
  
  # Prepare current skier data (pass startlist for Elo values from chrono_pred)
  men_current <- prepare_current_skiers(chrono_data, current_season, "men", men_startlists$individuals)
  print(men_current)
  # Get leg predictions for all current skiers
  # In your main function, replace:
  men_leg_predictions <- list()
  for(leg in 1:4) {
    men_leg_predictions[[leg]] <- get_leg_predictions(leg, men_current, men_leg_models)
  }
  
  # With:
  if(men_has_fis) {
    log_info("Using FIS startlist for men's leg predictions")
    men_leg_predictions <- get_leg_predictions_with_startlist(
      men_current,
      men_leg_models,
      men_startlists$individuals
    )
  } else {
    log_info("No FIS startlist, predicting for all skiers in all legs")
    men_leg_predictions <- list()
    for(leg in 1:4) {
      men_leg_predictions[[leg]] <- get_leg_predictions(leg, men_current, men_leg_models)
    }
  }
  print(men_leg_predictions)
  # Generate team predictions
  if(men_has_fis) {
    log_info("Using FIS startlist for men")
    men_team_predictions <- generate_team_predictions(men_startlists$teams, men_leg_predictions, men_leg_models)
  } else {
    log_info("No valid FIS startlist for men, building optimized teams")
    men_team_predictions <- build_optimized_teams(men_current, men_leg_predictions, men_leg_models, "men")
  }

  
  
  
  
  
  # Process for ladies
  log_info("Processing ladies's relay predictions")
  
  # Load ladies's startlists
  ladies_startlists <- load_relay_startlists("ladies")
  
  # # Check if startlists have valid FIS entries
  ladies_has_fis <- has_valid_fis_entries(ladies_startlists$individuals)
  
  # Prepare current skier data (pass startlist for Elo values from chrono_pred)
  ladies_current <- prepare_current_skiers(chrono_data, current_season, "ladies", ladies_startlists$individuals)
  print(ladies_current)
  # Get leg predictions for all current skiers
  # In your main function, replace:
  ladies_leg_predictions <- list()
  for(leg in 1:4) {
    ladies_leg_predictions[[leg]] <- get_leg_predictions(leg, ladies_current, ladies_leg_models)
  }
  
  # With:
  if(ladies_has_fis) {
    log_info("Using FIS startlist for ladies's leg predictions")
    ladies_leg_predictions <- get_leg_predictions_with_startlist(
      ladies_current, 
      ladies_leg_models, 
      ladies_startlists$individuals
    )
  } else {
    log_info("No FIS startlist, predicting for all skiers in all legs")
    ladies_leg_predictions <- list()
    for(leg in 1:4) {
      ladies_leg_predictions[[leg]] <- get_leg_predictions(leg, ladies_current, ladies_leg_models)
    }
  }
  print(ladies_leg_predictions)
  # Generate team predictions
  if(ladies_has_fis) {
    log_info("Using FIS startlist for ladies")
    ladies_team_predictions <- generate_team_predictions(ladies_startlists$teams, ladies_leg_predictions, ladies_leg_models)
  } else {
    log_info("No valid FIS startlist for ladies, building optimized teams")
    ladies_team_predictions <- build_optimized_teams(ladies_current, ladies_leg_predictions, ladies_leg_models, "ladies")
  }

  
  # First reset probabilities at or below the mode
  log_info("Resetting men's probabilities at or below the mode to zero")
  men_team_predictions <- reset_mode_probabilities(men_team_predictions)
  
  log_info("Resetting ladies' probabilities at or below the mode to zero")
  ladies_team_predictions <- reset_mode_probabilities(ladies_team_predictions)

  
  # After generating team predictions but before fantasy optimization
  log_info("Normalizing men's team probabilities")
  men_team_predictions <- normalize_probabilities(men_team_predictions)
  
  log_info("Normalizing ladies' team probabilities")
  ladies_team_predictions <- normalize_probabilities(ladies_team_predictions)
  log_info("Final capping of probabilities to maximum of 1")
  men_team_predictions <- cap_probabilities(men_team_predictions)
  ladies_team_predictions <- cap_probabilities(ladies_team_predictions)
  print(men_team_predictions)
  print(ladies_team_predictions)

  
  
  # Optimize fantasy team
  log_info("Optimizing fantasy relay team")
  
  # Define the essential columns needed for optimization
  essential_columns <- c("Team_Name", "Nation", "Gender", "Price", "Expected_Points", 
                         "Podium_Prob", "Win_Prob", "Top5_Prob", "Top10_Prob")
  
  # Create a combined dataset with only the essential columns
  combined_teams <- bind_rows(
    men_team_predictions %>% 
      mutate(Gender = "men") %>%
      select(any_of(essential_columns)),
    
    ladies_team_predictions %>% 
      mutate(Gender = "ladies") %>%
      select(any_of(essential_columns))
  )
  
  # Ensure all required columns exist
  if(!"Price" %in% names(combined_teams)) {
    log_warn("Price column missing - setting to 0")
    combined_teams$Price <- 0
  }
  
  
  if(!"Expected_Points" %in% names(combined_teams)) {
    log_warn("Expected_Points column missing - calculating from probabilities")
    combined_teams <- combined_teams %>%
      mutate(
        Expected_Points = Win_Prob * relay_points[1] +
          (Podium_Prob - Win_Prob) * mean(relay_points[2:3]) +
          (Top5_Prob - Podium_Prob) * mean(relay_points[4:5]) +
          (Top10_Prob - Top5_Prob) * mean(relay_points[6:10])
      )
  }
  

  # OLD METHOD: Knapsack optimization (commented out)
  # fantasy_team <- optimize_fantasy_team(combined_teams)

  # NEW METHOD: Simply pass all teams - save_fantasy_results will take top 20 men + top 20 ladies
  fantasy_team <- list(team = combined_teams)

  # Save results
  men_files <- save_prediction_results(men_team_predictions, race_info$race_date, "men")
  ladies_files <- save_prediction_results(ladies_team_predictions, race_info$race_date, "ladies")
  fantasy_file <- save_fantasy_results(fantasy_team, race_info$race_date)
  

  log_info("Relay predictions completed successfully")
  
  return(list(
    men_predictions = men_team_predictions,
    ladies_predictions = ladies_team_predictions,
    fantasy_team = fantasy_team,
    race_date = race_info$race_date,
    output_files = c(men_files, ladies_files, fantasy_file)
  ))
  
}

# Uncomment to run the full pipeline
results <- run_relay_predictions()











