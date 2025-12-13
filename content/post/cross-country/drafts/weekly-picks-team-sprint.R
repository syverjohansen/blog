
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

log_file <- file.path(log_dir, paste0("team_sprint_predictions_", format(Sys.Date(), "%Y%m%d"), ".log"))
log_appender(appender_file(log_file))
log_threshold(INFO)
log_info("Starting team sprint predictions process")


## Step 1: Load Data and Find Tomorrow's Races


# Function to find tomorrow's date
get_tomorrow_date <- function() {
  current_utc_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")
  # Calculate tomorrow by adding 1 day
  tomorrow <- current_utc_date
  return(tomorrow)
}

# Function to load weekends.csv and find races for tomorrow
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
  
  # Filter to only team sprint races 
  # In FIS schedule, team sprint is often labeled as "Ts" in the Distance column
  team_sprint_races <- tomorrow_races %>%
    filter(Distance == "Ts")
  
  # If no team sprint races found for tomorrow, look for the next team sprint races
  if (nrow(team_sprint_races) == 0) {
    log_info("No team sprint races found for tomorrow, finding next team sprint race")
    next_team_sprint_races <- weekends %>%
      filter(Date > Sys.Date(), Distance == "Ts") %>%
      arrange(Date)
    
    if (nrow(next_team_sprint_races) > 0) {
      next_race_date <- min(next_team_sprint_races$Date)
      log_info(paste("Next team sprint race found on:", next_race_date))
      team_sprint_races <- weekends %>%
        filter(Date == next_race_date, Distance == "Ts")
    } else {
      log_warn("No upcoming team sprint races found in the schedule")
    }
  }
  
  # Split into men and ladies races and extract technique information
  men_races <- team_sprint_races %>%
    filter(Sex == "M") %>%
    mutate(
      # Add a technique variable if not already present
      Technique = ifelse(is.na(Technique) | Technique == "", 
                         # Default to classic if unclear
                         "C", 
                         Technique)
    )
  
  ladies_races <- team_sprint_races %>%
    filter(Sex == "L") %>%
    mutate(
      # Add a technique variable if not already present
      Technique = ifelse(is.na(Technique) | Technique == "", 
                         # Default to classic if unclear
                         "C", 
                         Technique)
    )
  
  log_info(paste("Found", nrow(men_races), "men's team sprint races and", 
                 nrow(ladies_races), "ladies' team sprint races"))
  
  # Log technique information
  if(nrow(men_races) > 0) {
    log_info(paste("Men's team sprint technique:", men_races$Technique[1]))
  }
  if(nrow(ladies_races) > 0) {
    log_info(paste("Ladies' team sprint technique:", ladies_races$Technique[1]))
  }
  
  return(list(
    men = men_races,
    ladies = ladies_races,
    race_date = if(nrow(team_sprint_races) > 0) unique(team_sprint_races$Date) else NULL
  ))
}


## Step 2: Load Chrono Files and Create Dataframes


# Function to load chrono files and create separate dataframes for team sprints and individual races
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
  
  # Create separate dataframes for team sprints and individual races
  men_team_sprints <- men_chrono %>% 
    filter(Distance == "Ts")
  
  men_individuals <- men_chrono %>% 
    filter(Distance != "Ts" & Distance != "Rel")
  
  ladies_team_sprints <- ladies_chrono %>% 
    filter(Distance == "Ts")
  
  ladies_individuals <- ladies_chrono %>% 
    filter(Distance != "Ts" & Distance != "Rel")
  
  log_info(paste("Created men's team sprint df with", nrow(men_team_sprints), "rows"))
  log_info(paste("Created men's individual df with", nrow(men_individuals), "rows"))
  log_info(paste("Created ladies' team sprint df with", nrow(ladies_team_sprints), "rows"))
  log_info(paste("Created ladies' individual df with", nrow(ladies_individuals), "rows"))
  
  return(list(
    men_chrono = men_chrono,
    men_team_sprints = men_team_sprints,
    men_individuals = men_individuals,
    ladies_chrono = ladies_chrono,
    ladies_team_sprints = ladies_team_sprints,
    ladies_individuals = ladies_individuals
  ))
}


## Step 3: Define Points Systems

# Define points systems
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)

team_sprint_points <- c(200, 160, 120, 100, 90, 80, 72, 64, 58, 52, 48, 44, 40, 36, 
                        32, 30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10, 8, 6, 4, 2)

# Function to assign points based on place
assign_points <- function(place, is_team_sprint = FALSE) {
  points_system <- if(is_team_sprint) team_sprint_points else wc_points
  
  if (is.na(place) || place < 1 || place > length(points_system)) {
    return(0)
  }
  
  return(points_system[place])
}

# Function to add points to race results
add_points_to_results <- function(df, is_team_sprint = FALSE) {
  df %>%
    mutate(Points = mapply(function(place) assign_points(place, is_team_sprint), Place))
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

# Function to process data for sprint races
process_sprint_data <- function(df_individuals, technique) {
  
  min_season = max(df_individuals$Season)-11
  # First add points
  df_with_points <- add_points_to_results(df_individuals, is_team_sprint = FALSE)
  
  if(technique=="C"){
    # Process classic sprint races
    classic_sprint_df <- df_with_points %>%
      filter(Distance == "Sprint", Technique == "C") %>%
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
          }
        })
      ) %>%
      ungroup()
    return(list(sprint = classic_sprint_df))
  }
  
  else{
    # Process freestyle sprint races
    freestyle_sprint_df <- df_with_points %>%
      filter(Distance == "Sprint", Technique == "F") %>%
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
          }
        })
      ) %>%
      ungroup()

    return(list(sprint = freestyle_sprint_df
    ))
  }
}

# Function to process team sprint data with classic and freestyle legs
process_team_sprint_data <- function(df_team_sprints, sprint_df, technique, min_season = 2014) {
  # Add points to team sprint results
  min_season = max(sprint_df$Season)-11

  team_sprint_with_points <- add_points_to_results(df_team_sprints, is_team_sprint = TRUE)
  if(technique=="C"){
    # Process classic team sprints - don't filter by season yet
    classic_team_sprints_all <- team_sprint_with_points %>%
      filter(Distance == "Ts", Technique == "C")   
    # Combine classic team sprints with classic sprint individual data
    classic_combined <- bind_rows(
      classic_team_sprints_all,
      sprint_df
    ) %>%
      group_by(ID) %>%
      arrange(Date, Season, Race, desc(Distance)) %>%  # Use Date for chronological order, desc(Distance) so Sprint comes before Ts
      fill(Weighted_Last_5, .direction = "down") %>%
      filter(Distance == "Ts", Season > min_season) %>%  # Apply season filter AFTER filling
      group_by(Season, Race) %>%  # Regroup by race for quartile replacement
      mutate(
        Weighted_Last_5 = ifelse(
          is.na(Weighted_Last_5),
          quantile(Weighted_Last_5, 0.25, na.rm = TRUE),
          Weighted_Last_5
        )
      ) %>%
      ungroup()
    
    return(list(team_sprints = classic_combined))
  }
  
  
  
  else{
    # Process freestyle team sprints - don't filter by season yet
    freestyle_team_sprints_all <- team_sprint_with_points %>%
      filter(Distance == "Ts", Technique == "F")


    
    # Debug: Check which skiers are in both datasets
    team_sprint_skiers <- unique(freestyle_team_sprints_all$ID)
    sprint_skiers <- unique(sprint_df$ID)
    common_skiers <- intersect(team_sprint_skiers, sprint_skiers)
    
    print(paste("Team sprint skiers:", length(team_sprint_skiers)))
    print(paste("Sprint skiers:", length(sprint_skiers))) 
    print(paste("Common skiers:", length(common_skiers)))
    
    # Combine freestyle team sprints with freestyle sprint individual data
    freestyle_combined_debug <- bind_rows(
      freestyle_team_sprints_all,
      sprint_df
    ) %>%
      group_by(ID) %>%
      arrange(Date, Season, Race, desc(Distance))
    
    # Debug: Check a sample skier before and after fill
    sample_skier <- common_skiers[1]
    print(paste("Sample skier ID:", sample_skier))
    
    before_fill <- freestyle_combined_debug %>% 
      filter(ID == sample_skier) %>%
      select(ID, Date, Season, Race, Distance, Weighted_Last_5) %>%
      arrange(Date, Season, Race, desc(Distance))
    print("Before fill:")
    print(before_fill)
    
    freestyle_combined <- freestyle_combined_debug %>%
      fill(Weighted_Last_5, .direction = "down") %>%
      filter(Distance == "Ts", Season > min_season) %>%  # Apply season filter AFTER filling
      group_by(Season, Race) %>%  # Regroup by race for quartile replacement
      mutate(
      Weighted_Last_5 = ifelse(
        is.na(Weighted_Last_5),
        quantile(Weighted_Last_5, 0.25, na.rm = TRUE),
        Weighted_Last_5
      )
    ) %>%
      ungroup()

    return(list(team_sprints = freestyle_combined))
  }
}


## Step 5: Create Leg-specific Models

# Function to prepare leg-specific datasets
prepare_leg_data <- function(team_sprints) {
  # Create datasets for each leg (only 2 for team sprint)
  leg_data <- list()
  
  # Leg 1 (Classic or Freestyle, depending on technique)
  leg_data[[1]] <- bind_rows(
    team_sprints %>% filter(Leg == 1)
  ) %>%
    mutate(
      is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
      is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
      is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
      is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  # Leg 2 (Classic or Freestyle, depending on technique)
  leg_data[[2]] <- bind_rows(
    team_sprints %>% filter(Leg == 2)
  ) %>%
    mutate(
      is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
      is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
      is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
      is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  return(leg_data)
}

# Function to get leg-specific predictor columns based on technique
# Function to get leg-specific predictor columns based on race technique
get_leg_predictors <- function(leg, leg_data, technique, race_info = NULL) {
  # Get column names from the leg data
  base_cols <- names(leg_data[[leg]])
  
  # Determine technique - first try from race_info if provided
  is_classic <- FALSE
  is_freestyle <- FALSE
  
  if(!is.null(race_info)) {
    
    is_classic <- technique == "C"
    is_freestyle <- technique == "F"
    log_info(paste("Using technique from race_info:", technique))
  } else {
    # Fall back to checking the leg data
    if("Technique" %in% base_cols) {
      techniques <- table(leg_data[[leg]]$Technique)
      if(length(techniques) > 0) {
        most_common_technique <- names(techniques)[which.max(techniques)]
        is_classic <- most_common_technique == "C"
        is_freestyle <- most_common_technique == "F"
        log_info(paste("Determined technique from leg data:", most_common_technique))
      }
    }
  }
  
  # Define base predictors common to all
  predictors <- c(
    "Pelo_Pct",
    "Weighted_Last_5"
  )
  
  # Add technique-specific predictors based on race technique
  if(is_classic) {
    log_info(paste("Race is classic technique - using classic-specific predictors"))
    technique_predictors <- c(
      "Sprint_Pelo_Pct",
      "Distance_Pelo_Pct",# General sprint ability  
      "Sprint_C_Pelo_Pct",           # Sprint classic specific
      "Classic_Pelo_Pct"             # General classic ability
    )
  } else if(is_freestyle) {
    log_info(paste("Race is freestyle technique - using freestyle-specific predictors"))
    technique_predictors <- c(
      "Sprint_Pelo_Pct",             # General sprint ability
      "Sprint_F_Pelo_Pct",           # Sprint freestyle specific  
      "Freestyle_Pelo_Pct"           # General freestyle ability
    )
  } else {
    log_info(paste("Race technique not determined - using all predictors"))
    # If technique can't be determined, include all potentially useful predictors
    technique_predictors <- c(
      "Sprint_Pelo_Pct",             # General sprint ability
      "Sprint_C_Pelo_Pct",           # Sprint classic specific
      "Sprint_F_Pelo_Pct",           # Sprint freestyle specific
      "Classic_Pelo_Pct",            # General classic ability
      "Freestyle_Pelo_Pct",
      "Distance_Pelo_Pct"# General freestyle ability
    )
  }
  
  # Add technique-specific predictors to base predictors
  predictors <- c(predictors, technique_predictors)
  
  # Find which of these predictors actually exist in the data
  predictors <- predictors[predictors %in% base_cols]
  
  # Add any additional sprint-related predictors that might be in the data
  additional_sprint_predictors <- grep("Sprint.*Pelo_Pct$", base_cols, value = TRUE)
  predictors <- unique(c(predictors, additional_sprint_predictors))
  
  log_info(paste("Selected predictors for leg", leg, ":", paste(predictors, collapse=", ")))
  
  return(predictors)
}
# Function to extract feature importance safely
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

# Function to safely extract team importance
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
    return(c(leg1_prob = 0.5, leg2_prob = 0.5))
  }, error = function(e) {
    log_warn(paste("Error extracting team feature importance:", e$message))
    # Default equal weights
    return(c(leg1_prob = 0.5, leg2_prob = 0.5))
  })
}

# Function to train models safely
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
        trControl = trainControl(
          method = "cv",
          number = 5,
          classProbs = TRUE,
          summaryFunction = defaultSummary,
          savePredictions = "final"
        ),
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
          trControl = trainControl(
            method = "cv",
            number = 5,
            classProbs = TRUE,
            summaryFunction = defaultSummary,
            savePredictions = "final"
          )
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
        trControl = trainControl(
          method = "cv",
          number = 5,
          classProbs = TRUE,
          summaryFunction = defaultSummary,
          savePredictions = "final"
        )
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

# Function to train leg models
train_leg_models <- function(leg_data, race_info = NULL) {
  # Train models for each leg
  leg_models <- list()
  for(leg in 1:2) {  # Team sprint only has 2 legs
    log_info(paste("Training models for Leg", leg))
    
    leg_predictors <- get_leg_predictors(leg, leg_data, race_info)
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
    
    # Create formulas for different outcomes
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
  
  # Process each leg (team sprint only has 2 legs)
  for(leg in 1:2) {
    # Filter the startlist to get skiers for this leg
    leg_skiers <- startlist_individuals %>%
      filter(Team_Position == leg) %>%
      select(ID, Skier, Nation, Team_Name)
    
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

# Function to get leg predictions
get_leg_predictions <- function(leg_number, skier_data, leg_models) {
  # Select appropriate Last_5 column based on leg
  if("Sprint_Last_5" %in% names(skier_data)) {
    # If we have technique-specific columns, use those
    skier_data$Weighted_Last_5 <- skier_data$Sprint_Last_5
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
  
  # Function to safely make predictions
  # Function to safely make predictions
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
    Leg = leg_number,
    Win_Prob = win_probs,
    Podium_Prob = podium_probs,
    Top5_Prob = top5_probs,
    Top10_Prob = top10_probs
  ))
}


## Step 6: Process Current Startlists

# Function to load current team sprint startlists
load_team_sprint_startlists <- function(gender) {
  gender_prefix <- ifelse(gender == "men", "men", "ladies")
  
  # Define file paths
  teams_path <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_team_sprint_teams_%s.csv", gender_prefix)
  individuals_path <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_team_sprint_individuals_%s.csv", gender_prefix)
  
  # Load data
  teams <- read.csv(teams_path, stringsAsFactors = FALSE)
  individuals <- read.csv(individuals_path, stringsAsFactors = FALSE)
  
  log_info(paste("Loaded", gender_prefix, "team sprint teams startlist with", nrow(teams), "rows"))
  log_info(paste("Loaded", gender_prefix, "team sprint individuals startlist with", nrow(individuals), "rows"))
  
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
prepare_current_skiers <- function(chrono_data, current_season, technique, gender = "men") {
  log_info(paste("Preparing current", gender, "skier data"))
  
  # Use the appropriate gender data
  gender_prefix <- ifelse(gender == "men", "men", "ladies")
  print(gender_prefix)
  chrono_gender <- chrono_data[[paste0(gender_prefix, "_chrono")]]
  sprint_df <- chrono_data$processed[[gender_prefix]]
  
  # Get all skiers from current season
  current_skiers <- chrono_gender %>%
    filter(Season == current_season) %>%
    select(ID, Skier, Nation) %>%
    distinct()
  
  # Get latest Elo values for these skiers
  latest_elo <- chrono_gender %>%
    filter(ID %in% current_skiers$ID) %>%
    group_by(ID) %>%
    arrange(desc(Season), desc(Race)) %>%
    dplyr::slice(1) %>%
    select(ID, ends_with("Elo")) %>%
    ungroup()
  
  # Recalculate Weighted_Last_5 for classic sprint races
  sprint_last5 <- sprint_df$sprint %>%
    filter(ID %in% current_skiers$ID) %>%
    group_by(ID) %>%
    arrange(Date, Season, Race) %>%
    mutate(
      # Calculate weighted average of previous races including current
      Sprint_Last_5 = sapply(row_number(), function(i) {
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
    select(ID, Sprint_Last_5) %>%
    ungroup()
  
  
  
  # Combine all data
  current_df <- current_skiers %>%
    left_join(latest_elo, by = "ID") %>%
    left_join(sprint_last5, by = "ID") 
  
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
  log_info(paste("Prepared data for", nrow(current_df), gender, "skiers with", 
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
  
  # Calculate leg importance weights from historical data
  leg_importance <- calculate_leg_importance(leg_models)
  
  log_info(paste("Leg importance weights:", 
                 paste(sprintf("Leg %d: %.2f", 1:2, leg_importance), collapse=", ")))
  
  # For each team, calculate probabilities based on their members
  for(i in 1:nrow(team_predictions)) {
    team_name <- team_predictions$Team_Name[i]
    
    # Extract team members
    members <- c()
    for(leg in 1:2) {  # Team sprint has only 2 members
      member_col <- paste0("Member_", leg)
      if(member_col %in% names(teams_df)) {
        members[leg] <- teams_df[[member_col]][i]
      }
    }
    
    # Get predictions for each member
    member_probs <- list(
      Podium = numeric(2),
      Win = numeric(2),
      Top5 = numeric(2),
      Top10 = numeric(2)
    )
    
    for(leg in 1:2) {
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
        team_predictions$Win_Prob[i] * team_sprint_points[1] +
        (team_predictions$Podium_Prob[i] - team_predictions$Win_Prob[i]) * mean(team_sprint_points[2:3]) +
        (team_predictions$Top5_Prob[i] - team_predictions$Podium_Prob[i]) * mean(team_sprint_points[4:5]) +
        (team_predictions$Top10_Prob[i] - team_predictions$Top5_Prob[i]) * mean(team_sprint_points[6:10])
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
    leg_accuracy <- sapply(1:2, function(leg) {  # Only 2 legs for team sprint
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
  
  # Option 3: Use default even weights for both legs
  log_info("Using Option 3: Default leg importance weights")
  default_weights <- c(0.5, 0.5)  # Equal weights for both legs in team sprint
  return(default_weights)
}

# Function to normalize probabilities in team predictions
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
        
        # Cap individual probabilities at 1.0
        team_predictions[[prob_col]] <- pmin(team_predictions[[prob_col]], 1.0)
        
        # Log the normalization
        log_info(paste("Normalized", prob_col, "from sum of", round(current_sum, 2), 
                       "to", round(sum(team_predictions[[prob_col]], na.rm = TRUE), 2),
                       "and capped individual values at 1.0"))
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
        Expected_Points = Win_Prob * team_sprint_points[1] +
          (Podium_Prob - Win_Prob) * mean(team_sprint_points[2:3]) +
          (Top5_Prob - Podium_Prob) * mean(team_sprint_points[4:5]) +
          (Top10_Prob - Top5_Prob) * mean(team_sprint_points[6:10])
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

# Function to build optimized teams
build_optimized_teams <- function(current_skiers, leg_predictions, leg_models, gender = "men") {
  # Get leg importance weights
  leg_importance <- calculate_leg_importance(leg_models)
  log_info(paste("Using leg importance weights for team optimization:", 
                 paste(sprintf("Leg %d: %.2f", 1:2, leg_importance), collapse=", ")))
  
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
      Win_Prob = calculate_team_score_weighted(
        c(Member_1, Member_2), 
        Nation, 
        leg_predictions, 
        "Win_Prob",
        leg_importance
      ),
      Top5_Prob = calculate_team_score_weighted(
        c(Member_1, Member_2), 
        Nation, 
        leg_predictions, 
        "Top5_Prob",
        leg_importance
      ),
      Top10_Prob = calculate_team_score_weighted(
        c(Member_1, Member_2), 
        Nation, 
        leg_predictions, 
        "Top10_Prob",
        leg_importance
      ),
      Expected_Points = Win_Prob * team_sprint_points[1] +
        (Podium_Prob - Win_Prob) * mean(team_sprint_points[2:3]) +
        (Top5_Prob - Podium_Prob) * mean(team_sprint_points[4:5]) +
        (Top10_Prob - Top5_Prob) * mean(team_sprint_points[6:10])
    ) %>%
    ungroup()

  # Load startlist files to get team names and prices
  gender_suffix <- ifelse(gender == "men", "men", "ladies")
  individuals_file <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_team_sprint_individuals_%s.csv", gender_suffix)
  teams_file <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_team_sprint_teams_%s.csv", gender_suffix)
  
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
        Found_Team_Name = find_team_name(c(Member_1, Member_2), individuals_data),
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
  leg_scores <- numeric(2)  # Team sprint only has 2 legs
  
  # For each leg
  for(leg in 1:2) {
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
  for(leg in 1:2) {  # Team sprint only has 2 legs
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
  # Add this near the beginning of the function
  if(nrow(leg_candidates[[1]]) == 0 || nrow(leg_candidates[[2]]) == 0) {
    log_warn(paste("Not enough candidates for", nation, "- returning empty team"))
    return(data.frame(
      Team_Name = paste(nation, "I"),
      Nation = nation,
      Team_Rank = 0,
      Member_1 = "NO ELIGIBLE SKIER",
      Member_2 = "NO ELIGIBLE SKIER",
      Is_Present = FALSE,
      Price = 0,
      Podium_Prob = 0,
      stringsAsFactors = FALSE
    ))
  }
  # Greedy algorithm: Start with best skiers in each leg
  team <- character(2)  # Team sprint only has 2 members
  for(leg in 1:2) {
    if(nrow(leg_candidates[[leg]]) > 0) {
      team[leg] <- leg_candidates[[leg]]$Skier[1]
    } else {
      team[leg] <- "NO ELIGIBLE SKIER"
    }
  }
  
  # Remove duplicates by replacing with next best
  if(!is.na(team[1]) && !is.na(team[2]) && team[1] != "NO ELIGIBLE SKIER" && team[2] != "NO ELIGIBLE SKIER" && team[1] == team[2]) {
    # Determine which leg should keep this skier based on higher weighted probability
    leg1_prob <- leg_candidates[[1]]$Weighted_Prob[1]
    leg2_prob <- leg_candidates[[2]]$Weighted_Prob[1]
    
    if(leg1_prob >= leg2_prob) {
      # Keep skier for leg 1, find next best for leg 2
      for(j in 2:nrow(leg_candidates[[2]])) {
        # Add NA check before comparison
        if(!is.na(leg_candidates[[2]]$Skier[j]) && !is.na(team[1]) && 
           leg_candidates[[2]]$Skier[j] != team[1]) {
          team[2] <- leg_candidates[[2]]$Skier[j]
          break
        }
      }
      # If no replacement found
      if(is.na(team[2]) || team[1] == team[2]) {
        team[2] <- "NO ELIGIBLE SKIER"
      }
    } else {
      # Keep skier for leg 2, find next best for leg 1
      for(j in 2:nrow(leg_candidates[[1]])) {
        # Add NA check before comparison
        if(!is.na(leg_candidates[[1]]$Skier[j]) && !is.na(team[2]) && 
           leg_candidates[[1]]$Skier[j] != team[2]) {
          team[1] <- leg_candidates[[1]]$Skier[j]
          break
        }
      }
      # If no replacement found
      if(is.na(team[1]) || team[1] == team[2]) {
        team[1] <- "NO ELIGIBLE SKIER"
      }
    }
  }
  
  # Refinement steps - try to improve by swapping skiers
  best_team <- team
  best_score <- calculate_team_score_weighted(team, nation, leg_predictions, "Podium_Prob", leg_weights)
  
  
  for(iter in 1:iterations) {
    improved <- FALSE
    
    # Try improving each leg
    for(leg in 1:2) {
      if(nrow(leg_candidates[[leg]]) <= 1) next
      
      current_skier <- team[leg]
      for(j in 1:min(nrow(leg_candidates[[leg]]), 10)) {
        candidate <- leg_candidates[[leg]]$Skier[j]
        
        # Skip if already in team or same as current
        # Add NA checks
        if(is.na(candidate) || any(is.na(team)) || 
           candidate %in% team || candidate == current_skier) next
        
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
    Is_Present = FALSE,
    Price = 0,
    Podium_Prob = best_score,
    stringsAsFactors = FALSE
  )
  
  return(optimized_team)
}


## Step 9: Fantasy Team Optimization with Knapsack

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
  log_info(paste("Men's teams:", sum(fantasy_team$Gender == "men"), "(", sum(fantasy_team$Gender_Code == "M"), ")"))
  log_info(paste("Ladies' teams:", sum(fantasy_team$Gender == "ladies"), "(", sum(fantasy_team$Gender_Code == "L"), ")"))
  
  return(list(
    team = fantasy_team,
    total_price = total_price,
    total_points = total_points,
    teams_per_gender = teams_per_gender,
    teams_per_nation = teams_per_nation
  ))
}


## Step 10: Save Results to Excel Files

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
  points_file <- file.path(output_dir, paste0(gender, "_team_sprint.xlsx"))
  write.xlsx(points_df, points_file)
  log_info(paste("Saved points predictions to", points_file))
  
  # Save probability results
  prob_file <- file.path(output_dir, paste0(gender, "_team_sprint_position_probabilities.xlsx"))
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
  
  # Prepare fantasy team data
  fantasy_df <- fantasy_team$team %>%
    select(Team_Name, Nation, Gender, Price, Expected_Points) %>%
    arrange(desc(Expected_Points))
  
  # Add team members if available
  if("Member_1" %in% names(fantasy_team$team)) {
    fantasy_df <- bind_cols(
      fantasy_df,
      fantasy_team$team %>% select(starts_with("Member_"))
    )
  }
  
  # Save fantasy team results
  fantasy_file <- file.path(output_dir, "fantasy_team_sprint_team.xlsx")
  write.xlsx(list(
    Team = fantasy_df
  ), fantasy_file)
  log_info(paste("Saved fantasy team results to", fantasy_file))
  
  return(fantasy_file)
}


## Execution Section

run_team_sprint_predictions <- function() {
  # Step 1: Find upcoming races and identify technique
  race_info <- load_upcoming_races()
  if(is.null(race_info$race_date)) {
    log_error("No upcoming team sprint races found. Exiting.")
    return(NULL)
  }
  
  # Extract technique information
  men_technique <- if(nrow(race_info$men) > 0) race_info$men$Technique[1] else NA
  
  ladies_technique <- if(nrow(race_info$ladies) > 0) race_info$ladies$Technique[1] else NA
  
  log_info(paste("Men's team sprint technique:", ifelse(is.na(men_technique), "Unknown", men_technique)))
  log_info(paste("Ladies' team sprint technique:", ifelse(is.na(ladies_technique), "Unknown", ladies_technique)))
  
  # Step 2: Load chrono data
  chrono_data <- load_chrono_data()
  
  # Step 3: Process data with Pelo percentages
  chrono_data$men_chrono <- create_pelo_pcts(chrono_data$men_chrono)
  chrono_data$ladies_chrono <- create_pelo_pcts(chrono_data$ladies_chrono)
  chrono_data$men_team_sprints <- create_pelo_pcts(chrono_data$men_team_sprints)
  chrono_data$ladies_team_sprints <- create_pelo_pcts(chrono_data$ladies_team_sprints)
  chrono_data$men_individuals <- create_pelo_pcts(chrono_data$men_individuals)
  chrono_data$ladies_individuals <- create_pelo_pcts(chrono_data$ladies_individuals)
  # Only process team sprint data with the matching technique
  if(!is.na(men_technique)) {
    men_team_sprints_filtered <- chrono_data$men_team_sprints %>%
      filter(Technique == men_technique | Technique == "")  # Include records with missing technique
    
    log_info(paste("Filtered men's team sprint data by technique", men_technique, 
                   "- kept", nrow(men_team_sprints_filtered), "of", nrow(chrono_data$men_team_sprints), "records"))
    
    
  } 
  
  if(!is.na(ladies_technique)) {
    ladies_team_sprints_filtered <- chrono_data$ladies_team_sprints %>%
      filter(Technique == ladies_technique | Technique == "")  # Include records with missing technique
    
    log_info(paste("Filtered ladies' team sprint data by technique", ladies_technique, 
                   "- kept", nrow(ladies_team_sprints_filtered), "of", nrow(chrono_data$ladies_team_sprints), "records"))
    
  } 
  
  
  
  # Process individual data (all data needed for Elo/Pelo values)
  
  
  # Step 4: Process sprint data with focus on technique
  # For men
  men_sprint <- process_sprint_data(chrono_data$men_individuals, men_technique)

  
  # For ladies
  ladies_sprint <- process_sprint_data(chrono_data$ladies_individuals, ladies_technique)
  
  # Add processed data to chrono_data
  chrono_data$processed <- list(
    men = men_sprint,
    ladies = ladies_sprint
  )
  
  
  # Step 5: Process team sprint data - use filtered data by technique
  men_team_sprint_processed <- process_team_sprint_data(
    men_team_sprints_filtered,
    men_sprint$sprint,
    men_technique
  )
  
  
  ladies_team_sprint_processed <- process_team_sprint_data(
    ladies_team_sprints_filtered,
    ladies_sprint$sprint,
    ladies_technique
  )
  
  
  #Step 6: Prepare leg data and train models - focus on appropriate technique
  men_leg_data <- prepare_leg_data(men_team_sprint_processed$team_sprints)

  
  ladies_leg_data <- prepare_leg_data(ladies_team_sprint_processed$team_sprints)
  
  
  
  
  # Train leg models with technique information
  men_leg_models <- train_leg_models(men_leg_data, race_info$men[1])  # Pass first men's race with technique info
  ladies_leg_models <- train_leg_models(ladies_leg_data, race_info$ladies[1]) # Pass first ladies' race with technique info
  
  
  
  # Get current season
  current_season <- max(chrono_data$men_chrono$Season, na.rm = TRUE)
  
  # Process for men
  log_info("Processing men's team sprint predictions")
  #   
  # Load men's startlists
  men_startlists <- load_team_sprint_startlists("men")

  
  # Check if startlists have valid FIS entries
  men_has_fis <- has_valid_fis_entries(men_startlists$individuals)
  
  # Prepare current skier data
  men_current <- prepare_current_skiers(chrono_data, current_season)
  
  
  # Get leg predictions for all current skiers
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
    for(leg in 1:2) {  # Team sprint only has 2 legs
      men_leg_predictions[[leg]] <- get_leg_predictions(leg, men_current, men_leg_models)
    }
  }
  

  # Generate team predictions
  if(men_has_fis) {
    log_info("Using FIS startlist for men")
    men_team_predictions <- generate_team_predictions(men_startlists$teams, men_leg_predictions, men_leg_models)

     } else {
    log_info("No valid FIS startlist for men, building optimized teams")
    men_team_predictions <- build_optimized_teams(men_current, men_leg_predictions, men_leg_models, "men")
  }
  print(men_team_predictions)
  
  # Process for ladies
  log_info("Processing ladies' team sprint predictions")
  
  # Load ladies' startlists
  ladies_startlists <- load_team_sprint_startlists("ladies")
  
  
  # Check if startlists have valid FIS entries
  ladies_has_fis <- has_valid_fis_entries(ladies_startlists$individuals)
  
  # Prepare current skier data
  ladies_current <- prepare_current_skiers(chrono_data, current_season, ladies_technique, "ladies")
  print(ladies_current)
  # Get leg predictions for all current skiers
  if(ladies_has_fis) {
    log_info("Using FIS startlist for ladies' leg predictions")
    ladies_leg_predictions <- get_leg_predictions_with_startlist(
      ladies_current,
      ladies_leg_models,
      ladies_startlists$individuals
    )
  } else {
    log_info("No FIS startlist, predicting for all skiers in all legs")
    ladies_leg_predictions <- list()
    for(leg in 1:2) {  # Team sprint only has 2 legs
      ladies_leg_predictions[[leg]] <- get_leg_predictions(leg, ladies_current, ladies_leg_models)
    }
  }
  
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
  #men_team_predictions <- reset_mode_probabilities(men_team_predictions)
  
  log_info("Resetting ladies' probabilities at or below the mode to zero")
  #ladies_team_predictions <- reset_mode_probabilities(ladies_team_predictions)
  
  # Normalize team probabilities
  log_info("Normalizing men's team probabilities")
  men_team_predictions <- normalize_probabilities(men_team_predictions)
  
  log_info("Normalizing ladies' team probabilities")
  ladies_team_predictions <- normalize_probabilities(ladies_team_predictions)
  print(ladies_team_predictions)
  
  # Optimize fantasy team
  log_info("Optimizing fantasy team sprint team")
  
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
        Expected_Points = Win_Prob * team_sprint_points[1] +
          (Podium_Prob - Win_Prob) * mean(team_sprint_points[2:3]) +
          (Top5_Prob - Podium_Prob) * mean(team_sprint_points[4:5]) +
          (Top10_Prob - Top5_Prob) * mean(team_sprint_points[6:10])
      )
  }
  
  # Run the optimization
  fantasy_team <- optimize_fantasy_team(combined_teams)
  
  # Save results
  men_files <- save_prediction_results(men_team_predictions, race_info$race_date, "men")
  ladies_files <- save_prediction_results(ladies_team_predictions, race_info$race_date, "ladies")
  fantasy_file <- save_fantasy_results(fantasy_team, race_info$race_date)
  
  log_info("Team sprint predictions completed successfully")
  
  return(list(
    men_predictions = men_team_predictions,
    ladies_predictions = ladies_team_predictions,
    fantasy_team = fantasy_team,
    race_date = race_info$race_date,
    output_files = c(men_files, ladies_files, fantasy_file)
  ))
  
}

# Uncomment to run the full pipeline
results <- run_team_sprint_predictions()




