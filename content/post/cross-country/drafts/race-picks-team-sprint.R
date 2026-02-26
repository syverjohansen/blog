# Race Day Team Sprint Predictions
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(logger)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
library(caret)

# Set up logging
log_dir <- "~/ski/elo/python/ski/polars/excel365/race-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_file <- file.path(log_dir, paste0("team_sprint_predictions_", format(Sys.Date(), "%Y%m%d"), ".log"))
log_appender(appender_file(log_file))
log_threshold(INFO)
log_info("Starting team sprint race day predictions process")

# ===== TEST MODE =====
# Set to TRUE to use test_races.csv for EDA/sandbox testing
TEST_MODE <- FALSE

# Define points system
team_sprint_points <- c(200, 160, 120, 100, 90, 80, 72, 64, 58, 52, 48, 44, 40, 36,
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

# Function to load races.csv and find team sprint races for today
load_today_team_sprint_races <- function() {
  log_info("Loading race data for today")

  # Read in the race schedule
  races_path <- if(TEST_MODE) {
    "~/ski/elo/python/ski/polars/excel365/test_races.csv"
  } else {
    "~/ski/elo/python/ski/polars/excel365/races.csv"
  }
  log_info(paste("Reading races from:", races_path))
  races <- read.csv(races_path, stringsAsFactors = FALSE) %>%
    mutate(Date = mdy(Date))
  
  # Find today's date
  today <- get_today_date()
  log_info(paste("Today's date:", today))
  
  # Filter for today's races
  today_races <- races %>%
    filter(Date == today)
  
  # Filter to only team sprint races (Ts)
  team_sprint_races <- today_races %>%
    filter(Distance == "Ts")
  
  if (nrow(team_sprint_races) == 0) {
    log_info("No team sprint races found today")
    return(NULL)
  }
  
  # Split into men and ladies races
  men_races <- team_sprint_races %>%
    filter(Sex == "M")
  
  ladies_races <- team_sprint_races %>%
    filter(Sex == "L")
  
  log_info(paste("Found", nrow(men_races), "men's team sprint races and", 
                 nrow(ladies_races), "ladies' team sprint races"))
  
  return(list(
    men = men_races,
    ladies = ladies_races,
    race_date = today
  ))
}

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

# Function to add points to race results
add_points_to_results <- function(df, is_team_sprint = FALSE) {
  df %>%
    mutate(Points = mapply(function(place) {
      if (place >= 1 && place <= length(team_sprint_points)) {
        return(team_sprint_points[place])
      }
      return(0)
    }, Place))
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

# Function to process sprint data (for team sprint)
process_sprint_data <- function(df_individuals, technique, min_season = NULL) {
  # Set min_season to 11 years ago from max season if not provided
  if(is.null(min_season)) {
    min_season = max(df_individuals$Season) - 11
  }
  
  # First add points
  df_with_points <- add_points_to_results(df_individuals, is_team_sprint = FALSE)
  
  if(technique == "C") {
    # Process classic sprint races
    sprint_df <- df_with_points %>%
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
    
    return(list(sprint = sprint_df))
  } else {
    # Process freestyle sprint races
    sprint_df <- df_with_points %>%
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
    
    return(list(sprint = sprint_df))
  }
}

# Function to process team sprint data with classic and freestyle legs
process_team_sprint_data <- function(df_team_sprints, sprint_df, technique, min_season = NULL) {
  # Set min_season to 11 years ago from max season if not provided
  if(is.null(min_season)) {
    min_season = max(sprint_df$Season) - 11
  }
  
  # Add points to team sprint results
  team_sprint_with_points <- add_points_to_results(df_team_sprints, is_team_sprint = TRUE)
  
  if(technique == "C") {
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
  } else {
    # Process freestyle team sprints - don't filter by season yet
    freestyle_team_sprints_all <- team_sprint_with_points %>%
      filter(Distance == "Ts", Technique == "F")
    
    # Combine freestyle team sprints with freestyle sprint individual data
    freestyle_combined <- bind_rows(
      freestyle_team_sprints_all,
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
    
    return(list(team_sprints = freestyle_combined))
  }
}

# Function to prepare leg-specific datasets for team sprint
prepare_leg_data <- function(team_sprints) {
  # Create datasets for each leg (only 2 for team sprint)
  leg_data <- list()
  
  # Leg 1
  leg_data[[1]] <- team_sprints %>%
    filter(Leg == 1) %>%
    mutate(
      is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
      is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
      is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
      is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  # Leg 2
  leg_data[[2]] <- team_sprints %>%
    filter(Leg == 2) %>%
    mutate(
      is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
      is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
      is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
      is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
    )
  
  return(leg_data)
}

# Function to get leg-specific predictor columns based on technique
get_leg_predictors <- function(leg, leg_data, technique) {

  # Get column names from the leg data
  base_cols <- names(leg_data[[leg]])
  
  # Determine technique
  is_classic <- technique == "C"
  is_freestyle <- technique == "F"
  
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
      "Distance_Pelo_Pct", # General sprint ability  
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
      "Distance_Pelo_Pct"            # General freestyle ability
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

# Function to train leg models
train_leg_models <- function(leg_data, race_info = NULL, technique=NULL) {
  # Get the technique from race info if provided
  technique <- technique
  print(technique)
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
  for(leg in 1:2) {  # Team sprint only has 2 legs
    log_info(paste("Training models for Leg", leg))

    
    leg_predictors <- get_leg_predictors(leg, leg_data, technique)
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

# Function to load team sprint startlists
load_team_sprint_startlists <- function(gender) {
  gender_prefix <- ifelse(gender == "men", "men", "ladies")
  
  # Define file paths
  teams_path <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_team_sprint_races_teams_%s.csv", gender_prefix)
  individuals_path <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_team_sprint_races_individuals_%s.csv", gender_prefix)
  
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
prepare_current_skiers <- function(chrono_data, current_season, technique, gender = "men", startlist_individuals = NULL) {
  log_info(paste("Preparing current", gender, "skier data"))

  # Use the appropriate gender data
  gender_prefix <- ifelse(gender == "men", "men", "ladies")
  chrono_gender <- chrono_data[[paste0(gender_prefix, "_chrono")]]
  sprint_df <- chrono_data$processed[[gender_prefix]]$sprint

  # Get all skiers from current season
  current_skiers <- chrono_gender %>%
    filter(Season == current_season) %>%
    select(ID, Skier, Nation) %>%
    distinct()

  # Get latest Elo values - prefer startlist (from chrono_pred), fallback to chrono_data
  if (!is.null(startlist_individuals) && any(grepl("Elo$", names(startlist_individuals)))) {
    log_info("Using Elo values from startlist (chrono_pred source)")
    elo_cols <- names(startlist_individuals)[grepl("Elo$", names(startlist_individuals))]
    latest_elo <- startlist_individuals %>%
      filter(ID %in% current_skiers$ID) %>%
      select(ID, any_of(elo_cols)) %>%
      distinct()
  } else {
    log_info("Falling back to chrono_data for Elo values")
    latest_elo <- chrono_gender %>%
      filter(ID %in% current_skiers$ID) %>%
      group_by(ID) %>%
      arrange(desc(Season), desc(Race)) %>%
      dplyr::slice(1) %>%
      select(ID, ends_with("Elo")) %>%
      ungroup()
  }
  
  # Recalculate Weighted_Last_5 for sprint races
  sprint_last5 <- sprint_df %>%
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

# Helper function to safely extract team importance from model
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

# Function to generate team predictions using individual leg probabilities
generate_team_predictions <- function(teams_df, individual_predictions, leg_models) {
  # Find all the exact Member_N columns
  member_cols <- c()
  for(i in 1:2) {  # Team sprint has only 2 members
    name_col <- paste0("Member_", i)
    
    if(name_col %in% names(teams_df)) {
      member_cols <- c(member_cols, name_col)
    }
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

# Two-phase normalization helper function (0-1 scale for team sprint)
# Phase A: Scale proportionally to target sum first
# Phase B: Cap at max_prob and redistribute excess iteratively
normalize_with_cap <- function(probs, target_sum, max_prob = 1.0, max_iterations = 100) {
  if (sum(probs, na.rm = TRUE) == 0) {
    return(rep(target_sum / length(probs), length(probs)))
  }

  # Phase A: Scale proportionally to target sum first (no capping)
  current_sum <- sum(probs, na.rm = TRUE)
  if (current_sum > 0) {
    probs <- probs * (target_sum / current_sum)
  }

  # Phase B: Cap at max_prob and redistribute excess (iterative)
  for (iter in 1:max_iterations) {
    above_cap <- probs > max_prob

    if (!any(above_cap, na.rm = TRUE)) {
      break
    }

    probs[above_cap] <- max_prob

    capped_total <- sum(above_cap) * max_prob
    remaining_target <- target_sum - capped_total
    uncapped_sum <- sum(probs[!above_cap], na.rm = TRUE)

    if (remaining_target <= 0) {
      probs[!above_cap] <- 0
      break
    }

    if (uncapped_sum <= 0) {
      n_uncapped <- sum(!above_cap)
      if (n_uncapped > 0) {
        probs[!above_cap] <- remaining_target / n_uncapped
      }
      break
    }

    scaling_factor <- remaining_target / uncapped_sum
    probs[!above_cap] <- probs[!above_cap] * scaling_factor
  }

  return(probs)
}

# Function to normalize probabilities in team predictions
normalize_probabilities <- function(team_predictions) {
  # Define normalization targets (0-1 scale)
  targets <- list(
    Win_Prob = 1,      # Win probability sums to 1
    Podium_Prob = 3,   # Podium probability sums to 3
    Top5_Prob = 5,     # Top5 probability sums to 5
    Top10_Prob = 10    # Top10 probability sums to 10
  )

  # For each probability column, apply two-phase normalization
  for(prob_col in names(targets)) {
    if(prob_col %in% names(team_predictions)) {
      target_sum <- targets[[prob_col]]
      team_predictions[[prob_col]] <- normalize_with_cap(team_predictions[[prob_col]], target_sum = target_sum, max_prob = 1.0)
      log_info(sprintf("Normalized %s to target sum %.0f", prob_col, target_sum))
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
      target_sum <- targets[[prob_col]]
      team_predictions[[prob_col]] <- normalize_with_cap(team_predictions[[prob_col]], target_sum = target_sum, max_prob = 1.0)
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

  # Cap individual probabilities at 1
  for(prob_col in prob_cols) {
    if(prob_col %in% names(team_predictions)) {
      team_predictions[[prob_col]] <- pmin(team_predictions[[prob_col]], 1)
    }
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

  # Rename columns to user-friendly format for points_df
  points_df <- points_df %>%
    rename("Team" = Team_Name, "Expected Points" = Expected_Points)
  for (i in 1:2) {
    old_name <- paste0("Member_", i)
    new_name <- paste0("Leg ", i)
    if (old_name %in% names(points_df)) {
      points_df <- points_df %>% rename(!!new_name := !!old_name)
    }
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

  # Rename columns to user-friendly format for prob_df
  prob_df <- prob_df %>%
    rename(
      "Team" = Team_Name,
      "Win" = Win_Prob,
      "Podium" = Podium_Prob,
      "Top 5" = Top5_Prob,
      "Top 10" = Top10_Prob
    )
  for (i in 1:2) {
    old_name <- paste0("Member_", i)
    new_name <- paste0("Leg ", i)
    if (old_name %in% names(prob_df)) {
      prob_df <- prob_df %>% rename(!!new_name := !!old_name)
    }
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

# Main function to run team sprint predictions
run_team_sprint_predictions <- function() {
  # Step 1: Find today's races
  race_info <- load_today_team_sprint_races()
  if(is.null(race_info)) {
    log_info("No team sprint races found today. Exiting.")
    return(NULL)
  }
  
  # Step 2: Load chrono data
  chrono_data <- load_chrono_data()
  
  # Step 3: Process data with Pelo percentages
  chrono_data$men_chrono <- create_pelo_pcts(chrono_data$men_chrono)
  chrono_data$ladies_chrono <- create_pelo_pcts(chrono_data$ladies_chrono)
  chrono_data$men_team_sprints <- create_pelo_pcts(chrono_data$men_team_sprints)
  chrono_data$ladies_team_sprints <- create_pelo_pcts(chrono_data$ladies_team_sprints)
  chrono_data$men_individuals <- create_pelo_pcts(chrono_data$men_individuals)
  chrono_data$ladies_individuals <- create_pelo_pcts(chrono_data$ladies_individuals)
  
  # Process men's predictions if there are men's races
  men_team_predictions <- NULL
  if(nrow(race_info$men) > 0) {
    # Extract technique for men's races
    men_technique <- race_info$men$Technique[1]

    # Step 4: Process sprint data with focus on technique
    men_sprint <- process_sprint_data(chrono_data$men_individuals, men_technique)
    
    # Add processed data to chrono_data
    chrono_data$processed <- list(men = men_sprint)
    
    # Step 5: Process team sprint data - use filtered data by technique
    men_team_sprint_processed <- process_team_sprint_data(
      chrono_data$men_team_sprints,
      men_sprint$sprint,
      men_technique
    )
    
    # Step 6: Prepare leg data and train models
    men_leg_data <- prepare_leg_data(men_team_sprint_processed$team_sprints)
    # Train leg models with technique information
    men_leg_models <- train_leg_models(men_leg_data, race_info$men[1], men_technique)
    
    
    # Get current season
    current_season <- max(chrono_data$men_chrono$Season, na.rm = TRUE)
    
    log_info("Processing men's team sprint predictions")
    
    # Load men's startlists
    men_startlists <- load_team_sprint_startlists("men")

    # Check if startlists have valid FIS entries
    men_has_fis <- has_valid_fis_entries(men_startlists$individuals)
    
    # Prepare current skier data (pass startlist for Elo values from chrono_pred)
    men_current <- prepare_current_skiers(chrono_data, current_season, men_technique, "men", men_startlists$individuals)
    
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

      # Reset probabilities at or below the mode
      log_info("Resetting men's probabilities at or below the mode to zero")
      men_team_predictions <- reset_mode_probabilities(men_team_predictions)
      
      
      # Normalize probabilities
      log_info("Normalizing men's team probabilities")
      men_team_predictions <- normalize_probabilities(men_team_predictions)

    } else {
      log_info("No valid FIS startlist for men, skipping team predictions")
    }
  }
  
  # Process ladies' predictions if there are ladies' races
  ladies_team_predictions <- NULL
  if(nrow(race_info$ladies) > 0) {
    # Extract technique for ladies' races
    ladies_technique <- race_info$ladies$Technique[1]
    print(ladies_technique)
    print("Checkpoint 1")
    # Step 4: Process sprint data with focus on technique
    ladies_sprint <- process_sprint_data(chrono_data$ladies_individuals, ladies_technique)
    print("Checkpoint 2")
    # Add processed data to chrono_data
    if(!"processed" %in% names(chrono_data)) {
      chrono_data$processed <- list()
    }
    chrono_data$processed$ladies <- ladies_sprint
    
    # Step 5: Process team sprint data - use filtered data by technique
    ladies_team_sprint_processed <- process_team_sprint_data(
      chrono_data$ladies_team_sprints,
      ladies_sprint$sprint,
      ladies_technique
    )
    print("Checkpoint 3")
    # Step 6: Prepare leg data and train models
    ladies_leg_data <- prepare_leg_data(ladies_team_sprint_processed$team_sprints)
    print("Checkpoint 4")
    # Train leg models with technique information
    ladies_leg_models <- train_leg_models(ladies_leg_data, race_info$ladies[1], ladies_technique)
    print("Checkpoint 5")
    # Get current season
    current_season <- max(chrono_data$ladies_chrono$Season, na.rm = TRUE)
    print("Checkpoint 6")
    log_info("Processing ladies' team sprint predictions")
    
    # Load ladies' startlists
    ladies_startlists <- load_team_sprint_startlists("ladies")
    print("Checkpoint 7")
    # Check if startlists have valid FIS entries
    ladies_has_fis <- has_valid_fis_entries(ladies_startlists$individuals)
    print("Checkpoint 8")
    # Prepare current skier data (pass startlist for Elo values from chrono_pred)
    ladies_current <- prepare_current_skiers(chrono_data, current_season, ladies_technique, "ladies", ladies_startlists$individuals)
    print("Checkpoint 9")
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
      
      # Reset probabilities at or below the mode
      log_info("Resetting ladies' probabilities at or below the mode to zero")
      ladies_team_predictions <- reset_mode_probabilities(ladies_team_predictions)
      
      # Normalize probabilities
      log_info("Normalizing ladies' team probabilities")
      ladies_team_predictions <- normalize_probabilities(ladies_team_predictions)
    } else {
      log_info("No valid FIS startlist for ladies, skipping team predictions")
    }
  }
  print(ladies_team_predictions)
  # Save results
  output_files <- c()
  if(!is.null(men_team_predictions)) {
    men_files <- save_prediction_results(men_team_predictions, race_info$race_date, "men")
    output_files <- c(output_files, men_files)
  }
  
  if(!is.null(ladies_team_predictions)) {
    ladies_files <- save_prediction_results(ladies_team_predictions, race_info$race_date, "ladies")
    output_files <- c(output_files, ladies_files)
  }
  
  log_info("Team sprint race day predictions completed successfully")
  
  return(list(
    men_predictions = men_team_predictions,
    ladies_predictions = ladies_team_predictions,
    race_date = race_info$race_date,
    output_files = output_files
  ))
}

# Run the predictions
results <- run_team_sprint_predictions()