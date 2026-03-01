# Tour de Ski Predictions: Monte Carlo Simulation Approach
#
# Simpler simulation approach that:
# 1. Builds distributions for each athlete based on historical TdS performance
# 2. Uses GAM prediction as fallback for athletes with limited TdS history
# 3. Runs Monte Carlo simulation for overall TdS standings
# 4. Position probabilities come naturally from simulation counts
#
# Output: TdS overall standings with win/podium/top5/top10/top30 probabilities

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
MIN_TDS_HISTORY <- 3          # Minimum TdS races for historical distribution
SD_SCALE_FACTOR <- 0.8        # Scale factor for SD (lower = more deterministic)
SD_MIN <- 8                   # Minimum SD for TdS points
SD_MAX <- 40                  # Maximum SD for TdS points

# Position thresholds
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)

# Points systems
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,
               40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,
               10,9,8,7,6,5,4,3,2,1)

tds_points <- c(300,285,270,255,240,216,207,198,189,180,174,168,162,156,150,
                144,138,132,126,120,114,108,102,96,90,84,78,72,66,60,57,54,
                51,48,45,42,39,36,33,30,27,24,21,18,15,12,9,6,3)

# ============================================================================
# LOGGING SETUP
# ============================================================================

log_dir <- "~/ski/elo/python/ski/polars/excel365/tds-simulation"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "tds_simulation.log")))
log_info("Starting Tour de Ski Simulation")
log_info(paste("Config: N_SIMULATIONS =", N_SIMULATIONS, ", TEST_MODE =", TEST_MODE))

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

get_points <- function(place, points_list = tds_points) {
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

# Read TdS startlists
log_info("Reading Tour de Ski startlists...")
men_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_tds_men.csv",
                          stringsAsFactors = FALSE) %>%
  mutate(Sex = "M")

ladies_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_tds_ladies.csv",
                             stringsAsFactors = FALSE) %>%
  mutate(Sex = "L")

log_info(paste("Loaded", nrow(men_startlist), "men and", nrow(ladies_startlist), "ladies in TdS startlist"))

# ============================================================================
# TDS-SPECIFIC DATA PREPARATION
# ============================================================================

log_info("=== PREPARING TDS-SPECIFIC DATA ===")

# Extract TdS-specific historical results
prepare_tds_history <- function(chrono_data, gender) {
  log_info(paste("Preparing TdS history for", gender))

  # Filter for Tour de Ski races and calculate TdS points
  tds_history <- chrono_data %>%
    filter(City == "Tour De Ski" | Event == "Tour de Ski") %>%
    mutate(
      TdS_Points = sapply(Place, function(p) get_points(p, tds_points))
    ) %>%
    arrange(ID, Date)

  log_info(paste("Found", nrow(tds_history), "TdS race records for", gender))
  log_info(paste("Unique athletes with TdS history:", n_distinct(tds_history$ID)))

  return(tds_history)
}

men_tds_history <- prepare_tds_history(men_chrono, "men")
ladies_tds_history <- prepare_tds_history(ladies_chrono, "ladies")

# Prepare general race history with WC points (for GAM fallback)
prepare_general_history <- function(chrono_data, gender) {
  log_info(paste("Preparing general history for", gender))

  chrono_data %>%
    mutate(
      Points = sapply(Place, function(p) get_points(p, wc_points))
    ) %>%
    arrange(ID, Date)
}

men_general_history <- prepare_general_history(men_chrono, "men")
ladies_general_history <- prepare_general_history(ladies_chrono, "ladies")

# ============================================================================
# GAM MODEL FOR FALLBACK PREDICTIONS
# ============================================================================

log_info("=== TRAINING FALLBACK GAM MODEL ===")

# Train a simple GAM model for athletes without TdS history
train_tds_fallback_gam <- function(chrono_data, gender) {
  log_info(paste("Training TdS fallback GAM for", gender))

  # Use distance freestyle races as proxy for TdS performance
  training_data <- chrono_data %>%
    filter(Distance != "Sprint") %>%
    group_by(Season, Race) %>%
    mutate(
      Pelo_Pct = Pelo / max(Pelo, na.rm = TRUE),
      Distance_F_Pelo_Pct = Distance_F_Pelo / max(Distance_F_Pelo, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(!is.na(Pelo_Pct), !is.na(Points))

  # Simple GAM with Pelo percentage
  tryCatch({
    model <- gam(Points ~ s(Pelo_Pct), data = training_data, method = "REML")

    residual_sd <- sqrt(model$sig2)
    if (is.null(residual_sd) || is.na(residual_sd)) {
      residual_sd <- sqrt(model$deviance / model$df.residual)
    }
    residual_sd <- max(residual_sd, 10)

    log_info(paste("GAM trained for", gender, "- Residual SD:", round(residual_sd, 2)))

    return(list(model = model, residual_sd = residual_sd))
  }, error = function(e) {
    log_warn(paste("Error training GAM for", gender, ":", e$message))
    return(NULL)
  })
}

men_fallback_gam <- train_tds_fallback_gam(men_general_history, "men")
ladies_fallback_gam <- train_tds_fallback_gam(ladies_general_history, "ladies")

# ============================================================================
# BUILD ATHLETE DISTRIBUTIONS
# ============================================================================

log_info("=== BUILDING ATHLETE DISTRIBUTIONS ===")

build_tds_distribution <- function(athlete_id, tds_history, general_history,
                                    fallback_gam, startlist_row,
                                    min_tds_history = MIN_TDS_HISTORY,
                                    decay_lambda = DECAY_LAMBDA) {

  reference_date <- Sys.Date()

  # Get athlete's TdS history
  athlete_tds <- tds_history %>%
    filter(ID == athlete_id) %>%
    arrange(desc(Date))

  n_tds_races <- nrow(athlete_tds)

  # Initialize distribution parameters
  mean_points <- NA

  sd_points <- NA
  source <- "none"

  if (n_tds_races >= min_tds_history) {
    # Use historical TdS performance with decay weighting
    days_ago <- as.numeric(reference_date - athlete_tds$Date)
    weights <- exp(-decay_lambda * days_ago)

    mean_points <- weighted.mean(athlete_tds$TdS_Points, weights, na.rm = TRUE)

    # Calculate weighted SD
    weighted_var <- sum(weights * (athlete_tds$TdS_Points - mean_points)^2) / sum(weights)
    sd_points <- sqrt(weighted_var)
    sd_points <- max(sd_points, SD_MIN)

    source <- "tds_history"

  } else if (n_tds_races > 0) {
    # Some TdS history but not enough - blend with GAM prediction
    days_ago <- as.numeric(reference_date - athlete_tds$Date)
    weights <- exp(-decay_lambda * days_ago)

    tds_mean <- weighted.mean(athlete_tds$TdS_Points, weights, na.rm = TRUE)
    tds_weight <- n_tds_races / min_tds_history

    # Get GAM prediction
    gam_pred <- 100  # Default
    if (!is.null(fallback_gam)) {
      athlete_general <- general_history %>%
        filter(ID == athlete_id) %>%
        arrange(desc(Date)) %>%
        head(1)

      if (nrow(athlete_general) > 0 && "Pelo_Pct" %in% names(athlete_general)) {
        gam_pred <- tryCatch({
          predict(fallback_gam$model, newdata = athlete_general, type = "response")
        }, error = function(e) 100)
      }
    }

    # Scale GAM prediction to TdS points range (roughly 3x WC points for winner)
    gam_tds_scaled <- gam_pred * 2.5

    # Blend TdS history with GAM
    mean_points <- tds_weight * tds_mean + (1 - tds_weight) * gam_tds_scaled
    sd_points <- fallback_gam$residual_sd * 2  # Higher uncertainty for blended

    source <- "blended"

  } else {
    # No TdS history - use GAM prediction scaled to TdS
    if (!is.null(fallback_gam)) {
      athlete_general <- general_history %>%
        filter(ID == athlete_id) %>%
        arrange(desc(Date)) %>%
        head(1)

      if (nrow(athlete_general) > 0) {
        # Calculate Pelo_Pct if not present
        if (!"Pelo_Pct" %in% names(athlete_general)) {
          athlete_general <- athlete_general %>%
            mutate(Pelo_Pct = Pelo / max(general_history$Pelo, na.rm = TRUE))
        }

        gam_pred <- tryCatch({
          predict(fallback_gam$model, newdata = athlete_general, type = "response")
        }, error = function(e) 50)

        # Scale to TdS points
        mean_points <- gam_pred * 2.5
        sd_points <- fallback_gam$residual_sd * 2.5

        source <- "gam_scaled"
      } else {
        # No history at all - use startlist position as proxy
        mean_points <- 80  # Conservative default
        sd_points <- 30
        source <- "default"
      }
    } else {
      mean_points <- 80
      sd_points <- 30
      source <- "default"
    }
  }

  # Apply bounds
  mean_points <- max(0, min(300, mean_points))
  sd_points <- max(SD_MIN, min(SD_MAX, sd_points * SD_SCALE_FACTOR))

  return(list(
    athlete_id = athlete_id,
    mean = mean_points,
    sd = sd_points,
    n_tds_races = n_tds_races,
    source = source
  ))
}

# Build distributions for all athletes
build_all_distributions <- function(startlist, tds_history, general_history,
                                     fallback_gam, gender) {
  log_info(paste("Building distributions for", nrow(startlist), gender, "athletes"))

  distributions <- lapply(1:nrow(startlist), function(i) {
    athlete <- startlist[i, ]
    build_tds_distribution(
      athlete_id = athlete$ID,
      tds_history = tds_history,
      general_history = general_history,
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

men_distributions <- build_all_distributions(
  men_startlist, men_tds_history, men_general_history, men_fallback_gam, "men"
)

ladies_distributions <- build_all_distributions(
  ladies_startlist, ladies_tds_history, ladies_general_history, ladies_fallback_gam, "ladies"
)

# ============================================================================
# MONTE CARLO SIMULATION
# ============================================================================

log_info("=== RUNNING MONTE CARLO SIMULATION ===")

simulate_tds_standings <- function(distributions, n_simulations = N_SIMULATIONS,
                                    position_thresholds = POSITION_THRESHOLDS,
                                    max_points = 300) {

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
    n_tds_races = sapply(valid_distributions, function(x) x$n_tds_races),
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
log_info("Running men's TdS simulation...")
men_simulation_results <- simulate_tds_standings(men_distributions)

log_info("Running ladies' TdS simulation...")
ladies_simulation_results <- simulate_tds_standings(ladies_distributions)

# ============================================================================
# COMBINE RESULTS WITH STARTLIST INFO
# ============================================================================

log_info("=== COMBINING RESULTS ===")

combine_results <- function(simulation_results, startlist, gender) {
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
      TdS_History = n_tds_races,
      Source = source,
      starts_with("prob_top_")
    ) %>%
    arrange(desc(Predicted_Points))

  log_info(paste("Combined", nrow(combined), "athletes for", gender))

  return(combined)
}

men_final_results <- combine_results(men_simulation_results, men_startlist, "men")
ladies_final_results <- combine_results(ladies_simulation_results, ladies_startlist, "ladies")

# Log top 5 for each gender
log_info("Top 5 Men's TdS Predictions:")
for (i in 1:min(5, nrow(men_final_results))) {
  r <- men_final_results[i, ]
  log_info(sprintf("  %d. %s (%s) - %.1f pts, Win: %.1f%%, Podium: %.1f%%",
                   i, r$Skier, r$Nation, r$Predicted_Points,
                   r$prob_top_1, r$prob_top_3))
}

log_info("Top 5 Ladies' TdS Predictions:")
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
output_dir <- paste0("~/blog/daehl-e/content/post/cross-country/drafts/tds-picks/", today_date)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
log_info(paste("Output directory:", output_dir))

# Prepare points output (matching tds-picks.R format)
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

# Save Excel files (matching tds-picks.R output format)
write.xlsx(men_points_output, file.path(output_dir, "men.xlsx"))
write.xlsx(ladies_points_output, file.path(output_dir, "ladies.xlsx"))

# Save position probabilities with custom sheet names (matching tds-picks.R)
men_probs_wb <- createWorkbook()
addWorksheet(men_probs_wb, "Men Tour de Ski")
writeData(men_probs_wb, "Men Tour de Ski", men_probs_output)
saveWorkbook(men_probs_wb, file.path(output_dir, "men_position_probabilities.xlsx"), overwrite = TRUE)

ladies_probs_wb <- createWorkbook()
addWorksheet(ladies_probs_wb, "Ladies Tour de Ski")
writeData(ladies_probs_wb, "Ladies Tour de Ski", ladies_probs_output)
saveWorkbook(ladies_probs_wb, file.path(output_dir, "ladies_position_probabilities.xlsx"), overwrite = TRUE)

log_info("Saved points and probability files")

log_info("Saved full simulation results")

# ============================================================================
# PRINT SUMMARY
# ============================================================================

cat("\n")
cat("============================================\n")
cat("TOUR DE SKI SIMULATION RESULTS\n")
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

log_info("Tour de Ski simulation complete")
