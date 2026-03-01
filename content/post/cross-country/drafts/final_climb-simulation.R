# Final Climb Predictions: Monte Carlo Simulation Approach
#
# Simpler simulation approach that:
# 1. Builds distributions for each athlete based on historical Final Climb performance
# 2. Uses distance freestyle GAM prediction as fallback
# 3. Runs Monte Carlo simulation for Final Climb standings
# 4. Position probabilities come naturally from simulation counts
#
# Final Climb: Tour de Ski final stage at Val Di Fiemme (Distance Freestyle uphill)
# Uses same points system as TdS overall (fc_points = 300, 285, 270...)

library(dplyr)
library(tidyr)
library(openxlsx)
library(mgcv)
library(leaps)
library(logger)
library(purrr)
library(lubridate)

# ============================================================================
# CONFIGURATION
# ============================================================================

# Load environment variables
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
        do.call(Sys.setenv, setNames(list(value), key))
      }
    }
    return(TRUE)
  }
  return(FALSE)
}

load_env()
TEST_MODE <- tolower(Sys.getenv("TEST_MODE", "false")) == "true"

# Simulation parameters
N_SIMULATIONS <- 10000        # Monte Carlo iterations
DECAY_LAMBDA <- 0.002         # Exponential decay rate
MIN_FC_HISTORY <- 2           # Minimum Final Climb races for historical distribution
SD_SCALE_FACTOR <- 0.8        # Scale factor for SD
SD_MIN <- 5                   # Minimum SD for FC points
SD_MAX <- 25                  # Maximum SD for FC points

# Position thresholds
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)

# Points systems
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,
               40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,
               10,9,8,7,6,5,4,3,2,1)

stage_points <- c(50,47,44,41,38,35,32,30,28,26,24,22,20,18,16,15,14,13,12,11,
                  10,9,8,7,6,5,4,3,2,1)

fc_points <- c(300,285,270,255,240,216,207,198,189,180,174,168,162,156,150,
               144,138,132,126,120,114,108,102,96,90,84,78,72,66,60,57,54,
               51,48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3)

# ============================================================================
# LOGGING SETUP
# ============================================================================

log_dir <- "~/ski/elo/python/ski/polars/excel365/final-climb-simulation"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "final_climb_simulation.log")))
log_info("Starting Final Climb Simulation")
log_info(paste("Config: N_SIMULATIONS =", N_SIMULATIONS, ", TEST_MODE =", TEST_MODE))

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

get_points <- function(place, points_list = stage_points) {
  if (is.na(place) || place < 1 || place > length(points_list)) {
    return(0)
  }
  return(points_list[place])
}

replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# ============================================================================
# DATA LOADING
# ============================================================================

log_info("=== LOADING DATA ===")

# Read chronological data
log_info("Reading chronological data...")
men_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Distance != "Ts", Distance != "Rel", Event != "Offseason")

ladies_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv",
                          stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Distance != "Ts", Distance != "Rel", Event != "Offseason")

log_info(paste("Loaded", nrow(men_chrono), "men's and", nrow(ladies_chrono), "ladies' race records"))

# Read Final Climb startlists (using race startlists)
log_info("Reading Final Climb startlists...")
men_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_races_men.csv",
                          stringsAsFactors = FALSE) %>%
  mutate(Sex = "M")

ladies_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_races_ladies.csv",
                             stringsAsFactors = FALSE) %>%
  mutate(Sex = "L")

# Filter for athletes with race probability > 0
filter_active_athletes <- function(startlist, gender) {
  race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)

  if (length(race_prob_cols) > 0) {
    initial_count <- nrow(startlist)
    startlist <- startlist %>%
      filter(if_any(all_of(race_prob_cols), ~ .x > 0))
    log_info(paste("Filtered", gender, "from", initial_count, "to", nrow(startlist), "active athletes"))
  }

  return(startlist)
}

men_startlist <- filter_active_athletes(men_startlist, "men")
ladies_startlist <- filter_active_athletes(ladies_startlist, "ladies")

log_info(paste("Loaded", nrow(men_startlist), "men and", nrow(ladies_startlist), "ladies in startlist"))

# ============================================================================
# FINAL CLIMB DATA PREPARATION
# ============================================================================

log_info("=== PREPARING FINAL CLIMB DATA ===")

# Identify Final Climb races and extract history
prepare_fc_history <- function(chrono_data, gender) {
  log_info(paste("Preparing Final Climb history for", gender))

  # Final Climb criteria: Tour de Ski + Val Di Fiemme + Distance + Freestyle
  fc_history <- chrono_data %>%
    mutate(
      Final_Climb = case_when(
        Event == "Tour de Ski" &
        City == "Val Di Fiemme" &
        Distance != "Sprint" &
        Technique == "F" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    filter(Final_Climb == 1) %>%
    mutate(
      FC_Points = sapply(Place, function(p) get_points(p, stage_points))
    ) %>%
    arrange(ID, Date)

  log_info(paste("Found", nrow(fc_history), "Final Climb race records for", gender))
  log_info(paste("Unique athletes with FC history:", n_distinct(fc_history$ID)))

  return(fc_history)
}

men_fc_history <- prepare_fc_history(men_chrono, "men")
ladies_fc_history <- prepare_fc_history(ladies_chrono, "ladies")

# Prepare distance freestyle history (for fallback)
prepare_distance_f_history <- function(chrono_data, gender) {
  log_info(paste("Preparing Distance Freestyle history for", gender))

  df_history <- chrono_data %>%
    filter(Distance != "Sprint", Technique == "F") %>%
    mutate(
      Points = sapply(Place, function(p) get_points(p, wc_points))
    ) %>%
    arrange(ID, Date)

  log_info(paste("Found", nrow(df_history), "Distance Freestyle records for", gender))

  return(df_history)
}

men_df_history <- prepare_distance_f_history(men_chrono, "men")
ladies_df_history <- prepare_distance_f_history(ladies_chrono, "ladies")

# ============================================================================
# GAM MODEL FOR FALLBACK PREDICTIONS
# ============================================================================

log_info("=== TRAINING FALLBACK GAM MODEL ===")

train_fc_fallback_gam <- function(chrono_data, gender) {
  log_info(paste("Training FC fallback GAM for", gender))

  # Use distance freestyle races
  training_data <- chrono_data %>%
    filter(Distance != "Sprint", Technique == "F") %>%
    mutate(Points = sapply(Place, function(p) get_points(p, wc_points))) %>%
    group_by(Season, Race) %>%
    mutate(
      Pelo_Pct = Pelo / max(Pelo, na.rm = TRUE),
      Distance_F_Pelo_Pct = Distance_F_Pelo / max(Distance_F_Pelo, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(!is.na(Distance_F_Pelo_Pct), !is.na(Points))

  tryCatch({
    model <- gam(Points ~ s(Distance_F_Pelo_Pct), data = training_data, method = "REML")

    residual_sd <- sqrt(model$sig2)
    if (is.null(residual_sd) || is.na(residual_sd)) {
      residual_sd <- sqrt(model$deviance / model$df.residual)
    }
    residual_sd <- max(residual_sd, 8)

    log_info(paste("GAM trained for", gender, "- Residual SD:", round(residual_sd, 2)))

    return(list(model = model, residual_sd = residual_sd))
  }, error = function(e) {
    log_warn(paste("Error training GAM for", gender, ":", e$message))
    return(NULL)
  })
}

men_fallback_gam <- train_fc_fallback_gam(men_chrono, "men")
ladies_fallback_gam <- train_fc_fallback_gam(ladies_chrono, "ladies")

# ============================================================================
# BUILD ATHLETE DISTRIBUTIONS
# ============================================================================

log_info("=== BUILDING ATHLETE DISTRIBUTIONS ===")

build_fc_distribution <- function(athlete_id, fc_history, df_history,
                                   fallback_gam, startlist_row,
                                   min_fc_history = MIN_FC_HISTORY,
                                   decay_lambda = DECAY_LAMBDA) {

  reference_date <- Sys.Date()

  # Get athlete's Final Climb history
  athlete_fc <- fc_history %>%
    filter(ID == athlete_id) %>%
    arrange(desc(Date))

  n_fc_races <- nrow(athlete_fc)

  # Get athlete's distance freestyle history
  athlete_df <- df_history %>%
    filter(ID == athlete_id) %>%
    arrange(desc(Date))

  n_df_races <- nrow(athlete_df)

  # Initialize distribution parameters
  mean_points <- NA
  sd_points <- NA
  source <- "none"

  if (n_fc_races >= min_fc_history) {
    # Use historical FC performance with decay weighting
    days_ago <- as.numeric(reference_date - athlete_fc$Date)
    weights <- exp(-decay_lambda * days_ago)

    mean_points <- weighted.mean(athlete_fc$FC_Points, weights, na.rm = TRUE)

    # Calculate weighted SD
    weighted_var <- sum(weights * (athlete_fc$FC_Points - mean_points)^2) / sum(weights)
    sd_points <- sqrt(weighted_var)
    sd_points <- max(sd_points, SD_MIN)

    source <- "fc_history"

  } else if (n_fc_races > 0) {
    # Some FC history but not enough - blend with distance freestyle
    days_ago <- as.numeric(reference_date - athlete_fc$Date)
    weights <- exp(-decay_lambda * days_ago)

    fc_mean <- weighted.mean(athlete_fc$FC_Points, weights, na.rm = TRUE)
    fc_weight <- n_fc_races / min_fc_history

    # Get distance freestyle mean
    df_mean <- 20  # Default
    if (n_df_races > 0) {
      df_days_ago <- as.numeric(reference_date - athlete_df$Date[1:min(5, n_df_races)])
      df_weights <- exp(-decay_lambda * df_days_ago)
      df_mean <- weighted.mean(athlete_df$Points[1:min(5, n_df_races)], df_weights, na.rm = TRUE)
    }

    # Blend FC history with DF performance
    mean_points <- fc_weight * fc_mean + (1 - fc_weight) * df_mean
    sd_points <- 12  # Higher uncertainty for blended

    source <- "blended"

  } else if (n_df_races >= 3) {
    # No FC history but has distance freestyle history
    days_ago <- as.numeric(reference_date - athlete_df$Date[1:min(10, n_df_races)])
    weights <- exp(-decay_lambda * days_ago)

    mean_points <- weighted.mean(athlete_df$Points[1:min(10, n_df_races)], weights, na.rm = TRUE)

    weighted_var <- sum(weights * (athlete_df$Points[1:min(10, n_df_races)] - mean_points)^2) / sum(weights)
    sd_points <- sqrt(weighted_var)
    sd_points <- max(sd_points, SD_MIN)

    source <- "df_history"

  } else if (!is.null(fallback_gam) && n_df_races > 0) {
    # Use GAM prediction
    athlete_recent <- athlete_df %>% head(1)

    if (nrow(athlete_recent) > 0) {
      # Calculate Distance_F_Pelo_Pct if needed
      if (!"Distance_F_Pelo_Pct" %in% names(athlete_recent)) {
        athlete_recent <- athlete_recent %>%
          mutate(Distance_F_Pelo_Pct = Distance_F_Pelo / max(df_history$Distance_F_Pelo, na.rm = TRUE))
      }

      gam_pred <- tryCatch({
        predict(fallback_gam$model, newdata = athlete_recent, type = "response")
      }, error = function(e) 25)

      mean_points <- gam_pred
      sd_points <- fallback_gam$residual_sd

      source <- "gam"
    } else {
      mean_points <- 20
      sd_points <- 15
      source <- "default"
    }
  } else {
    # No history at all
    mean_points <- 15
    sd_points <- 15
    source <- "default"
  }

  # Apply bounds (stage points max is 50)
  mean_points <- max(0, min(50, mean_points))
  sd_points <- max(SD_MIN, min(SD_MAX, sd_points * SD_SCALE_FACTOR))

  return(list(
    athlete_id = athlete_id,
    mean = mean_points,
    sd = sd_points,
    n_fc_races = n_fc_races,
    n_df_races = n_df_races,
    source = source
  ))
}

# Build distributions for all athletes
build_all_fc_distributions <- function(startlist, fc_history, df_history,
                                        fallback_gam, gender) {
  log_info(paste("Building distributions for", nrow(startlist), gender, "athletes"))

  distributions <- lapply(1:nrow(startlist), function(i) {
    athlete <- startlist[i, ]
    build_fc_distribution(
      athlete_id = athlete$ID,
      fc_history = fc_history,
      df_history = df_history,
      fallback_gam = fallback_gam,
      startlist_row = athlete
    )
  })

  # Log distribution sources
  sources <- sapply(distributions, function(d) d$source)
  source_counts <- table(sources)
  log_info("Distribution sources:")
  for (src in names(source_counts)) {
    log_info(paste("  ", src, ":", source_counts[src]))
  }

  return(distributions)
}

men_distributions <- build_all_fc_distributions(
  men_startlist, men_fc_history, men_df_history, men_fallback_gam, "men"
)

ladies_distributions <- build_all_fc_distributions(
  ladies_startlist, ladies_fc_history, ladies_df_history, ladies_fallback_gam, "ladies"
)

# ============================================================================
# MONTE CARLO SIMULATION
# ============================================================================

log_info("=== RUNNING MONTE CARLO SIMULATION ===")

simulate_fc_standings <- function(distributions, n_simulations = N_SIMULATIONS,
                                   position_thresholds = POSITION_THRESHOLDS,
                                   max_points = 50) {

  # Filter valid distributions
  valid_distributions <- Filter(function(d) {
    !is.null(d) && !is.na(d$mean) && !is.na(d$sd)
  }, distributions)

  if (length(valid_distributions) == 0) {
    log_error("No valid distributions to simulate")
    return(data.frame())
  }

  n_athletes <- length(valid_distributions)
  athlete_ids <- sapply(valid_distributions, function(x) x$athlete_id)

  log_info(paste("Simulating", n_athletes, "athletes over", n_simulations, "iterations"))

  # Extract means and sds as vectors for vectorized operations
  means <- sapply(valid_distributions, function(x) x$mean)
  sds <- sapply(valid_distributions, function(x) x$sd)

  # Generate all simulations at once (n_athletes x n_simulations matrix)
  all_sims <- matrix(rnorm(n_athletes * n_simulations),
                     nrow = n_athletes, ncol = n_simulations)
  all_sims <- all_sims * sds + means
  all_sims <- pmax(0, pmin(max_points, all_sims))

  # Rank each simulation (column) - higher points = better = rank 1
  ranks_matrix <- apply(all_sims, 2, function(x) rank(-x, ties.method = "random"))

  # Count position achievements using vectorized rowSums
  position_counts <- matrix(0, nrow = n_athletes, ncol = length(position_thresholds))
  for (t_idx in seq_along(position_thresholds)) {
    threshold <- position_thresholds[t_idx]
    position_counts[, t_idx] <- rowSums(ranks_matrix <= threshold)
  }

  # Convert to probabilities
  position_probs <- position_counts / n_simulations

  # Build results dataframe
  results <- data.frame(
    athlete_id = athlete_ids,
    mean_points = means,
    sd_points = sds,
    n_fc_races = sapply(valid_distributions, function(x) x$n_fc_races),
    n_df_races = sapply(valid_distributions, function(x) x$n_df_races),
    source = sapply(valid_distributions, function(x) x$source),
    stringsAsFactors = FALSE
  )

  # Add probability columns
  for (t_idx in seq_along(position_thresholds)) {
    col_name <- paste0("prob_top_", position_thresholds[t_idx])
    results[[col_name]] <- round(position_probs[, t_idx] * 100, 2)
  }

  results <- results %>% arrange(desc(mean_points))

  return(results)
}

# Run simulations
log_info("Running men's Final Climb simulation...")
men_simulation_results <- simulate_fc_standings(men_distributions)

log_info("Running ladies' Final Climb simulation...")
ladies_simulation_results <- simulate_fc_standings(ladies_distributions)

# ============================================================================
# COMBINE RESULTS WITH STARTLIST INFO
# ============================================================================

log_info("=== COMBINING RESULTS ===")

combine_fc_results <- function(simulation_results, startlist, gender) {
  log_info(paste("Combining results for", gender))

  combined <- simulation_results %>%
    left_join(
      startlist %>% select(ID, Skier, Nation, Price),
      by = c("athlete_id" = "ID")
    ) %>%
    select(
      Skier, ID = athlete_id, Nation, Price,
      Predicted_Points = mean_points,
      SD = sd_points,
      FC_History = n_fc_races,
      DF_History = n_df_races,
      Source = source,
      starts_with("prob_top_")
    ) %>%
    arrange(desc(Predicted_Points))

  log_info(paste("Combined", nrow(combined), "athletes for", gender))

  return(combined)
}

men_final_results <- combine_fc_results(men_simulation_results, men_startlist, "men")
ladies_final_results <- combine_fc_results(ladies_simulation_results, ladies_startlist, "ladies")

# Log top 5 for each gender
log_info("Top 5 Men's Final Climb Predictions:")
for (i in 1:min(5, nrow(men_final_results))) {
  r <- men_final_results[i, ]
  log_info(sprintf("  %d. %s (%s) - %.1f pts, Win: %.1f%%, Podium: %.1f%%",
                   i, r$Skier, r$Nation, r$Predicted_Points,
                   r$prob_top_1, r$prob_top_3))
}

log_info("Top 5 Ladies' Final Climb Predictions:")
for (i in 1:min(5, nrow(ladies_final_results))) {
  r <- ladies_final_results[i, ]
  log_info(sprintf("  %d. %s (%s) - %.1f pts, Win: %.1f%%, Podium: %.1f%%",
                   i, r$Skier, r$Nation, r$Predicted_Points,
                   r$prob_top_1, r$prob_top_3))
}

# ============================================================================
# SAVE RESULTS
# ============================================================================

log_info("=== SAVING RESULTS ===")

# Create output directory
today_date <- format(Sys.time(), tz = "UTC", "%Y%m%d")
output_dir <- paste0("~/blog/daehl-e/content/post/cross-country/drafts/final-climb/", today_date)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
log_info(paste("Output directory:", output_dir))

# Prepare points output
prepare_points_output <- function(results, gender) {
  results %>%
    select(Skier, ID, Nation, Points = Predicted_Points) %>%
    arrange(desc(Points))
}

men_points_output <- prepare_points_output(men_final_results, "men")
ladies_points_output <- prepare_points_output(ladies_final_results, "ladies")

# Prepare probabilities output
prepare_probabilities_output <- function(results, gender) {
  results %>%
    select(
      Skier, ID, Nation,
      Win = prob_top_1,
      Podium = prob_top_3,
      `Top-5` = prob_top_5,
      `Top-10` = prob_top_10,
      `Top-30` = prob_top_30
    ) %>%
    arrange(desc(Win))
}

men_probs_output <- prepare_probabilities_output(men_final_results, "men")
ladies_probs_output <- prepare_probabilities_output(ladies_final_results, "ladies")

# Save Excel files (matching final_climb.R output format)
write.xlsx(men_points_output, file.path(output_dir, "men.xlsx"))
write.xlsx(ladies_points_output, file.path(output_dir, "ladies.xlsx"))

# Save position probabilities with custom sheet names (matching final_climb.R)
men_probs_wb <- createWorkbook()
addWorksheet(men_probs_wb, "Men Race 1")
writeData(men_probs_wb, "Men Race 1", men_probs_output)
saveWorkbook(men_probs_wb, file.path(output_dir, "men_position_probabilities.xlsx"), overwrite = TRUE)

ladies_probs_wb <- createWorkbook()
addWorksheet(ladies_probs_wb, "Ladies Race 1")
writeData(ladies_probs_wb, "Ladies Race 1", ladies_probs_output)
saveWorkbook(ladies_probs_wb, file.path(output_dir, "ladies_position_probabilities.xlsx"), overwrite = TRUE)

log_info("Saved points and probability files")

log_info("Saved full simulation results")

# ============================================================================
# PRINT SUMMARY
# ============================================================================

cat("\n")
cat("============================================\n")
cat("FINAL CLIMB SIMULATION RESULTS\n")
cat("============================================\n")
cat(paste("Simulations:", N_SIMULATIONS, "\n"))
cat(paste("Output directory:", output_dir, "\n"))
cat("\n")

cat("--- Top 10 Men ---\n")
print(men_final_results %>%
        select(Skier, Nation, Predicted_Points, prob_top_1, prob_top_3) %>%
        head(10))

cat("\n--- Top 10 Ladies ---\n")
print(ladies_final_results %>%
        select(Skier, Nation, Predicted_Points, prob_top_1, prob_top_3) %>%
        head(10))

cat("\n============================================\n")

log_info("Final Climb simulation complete")
