# Position Simulation Prototype for Championships Predictions
# This replaces normalization-based position probabilities with simulation-based approach
#
# Key concepts:
# - Individual races: 10 historical races (recency weighted) + 10 GAM samples (equal total weight)
# - If < 10 races, GAM fills remaining slots at lower weight
# - Distribution built from combined pool, then simulate N times and count positions
# - Relays/Team Sprints: All-GAM approach for team distribution

library(dplyr)
library(tidyr)
library(mgcv)
library(logger)

# ============================================================================
# CONFIGURATION
# ============================================================================

N_HISTORY_REQUIRED <- 10      # Target number of historical races
N_GAM_SAMPLES <- 10           # Number of GAM samples to add
GAM_FILL_WEIGHT_FACTOR <- 0.5 # Weight factor for GAM-filled history (lower than real)
N_SIMULATIONS <- 1000         # Number of Monte Carlo simulations

# World Cup points system (for reference - points indicate performance level)
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,
               40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,
               10,9,8,7,6,5,4,3,2,1)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Function to assign points based on place
get_points <- function(place) {
  if (is.na(place) || place < 1 || place > length(wc_points)) {
    return(0)
  } else {
    return(wc_points[place])
  }
}

# Replace NAs with first quartile value
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# ============================================================================
# CORE FUNCTION: Build Athlete Distribution
# ============================================================================
#
# This function builds a points distribution for an athlete based on:
# 1. Last N_HISTORY_REQUIRED races of the specific race type (recency weighted)
# 2. If < N_HISTORY_REQUIRED races, GAM predictions fill the gap (at lower weight)
# 3. N_GAM_SAMPLES from GAM distribution (spanning GAM variance)
#
# Returns: list with mean, sd, and the combined points/weights for simulation

build_athlete_distribution <- function(athlete_id,
                                        race_type_key,
                                        chrono_data,
                                        gam_model,
                                        gam_residual_sd,
                                        athlete_gam_prediction,
                                        n_history = N_HISTORY_REQUIRED,
                                        n_gam_samples = N_GAM_SAMPLES,
                                        gam_fill_weight_factor = GAM_FILL_WEIGHT_FACTOR) {

  # Define race type filter
  race_filter <- switch(race_type_key,
    "Sprint_C" = quote(Distance == "Sprint" & Technique == "C"),
    "Sprint_F" = quote(Distance == "Sprint" & Technique == "F"),
    "Distance_C_Ind" = quote(Distance != "Sprint" & Technique == "C" & MS == 0),
    "Distance_C_Ms" = quote(Distance != "Sprint" & Technique == "C" & MS == 1),
    "Distance_F_Ind" = quote(Distance != "Sprint" & Technique == "F" & MS == 0),
    "Distance_F_Ms" = quote(Distance != "Sprint" & Technique == "F" & MS == 1),
    "Distance_Ind" = quote(Distance != "Sprint" & MS == 0),
    "Distance_Ms" = quote(Distance != "Sprint" & MS == 1),
    # Fallback
    quote(TRUE)
  )

  # Get athlete's historical results for this race type
  athlete_history <- chrono_data %>%
    filter(ID == athlete_id,
           City != "Summer",
           City != "Tour De Ski",
           eval(race_filter)) %>%
    arrange(desc(Date)) %>%
    head(n_history)

  n_actual_races <- nrow(athlete_history)

  # Initialize points and weights vectors
  all_points <- c()
  all_weights <- c()

  # -------------------------------------------------------------------------
  # Part 1: Historical races with recency weighting
  # -------------------------------------------------------------------------
  if (n_actual_races > 0) {
    # Get points from history (already ordered newest to oldest)
    history_points <- sapply(athlete_history$Place, get_points)

    # Recency weights: oldest gets 1, newest gets n_actual_races
    # Since data is ordered newest to oldest, reverse the weights
    history_weights <- rev(seq(1, n_actual_races))

    all_points <- c(all_points, history_points)
    all_weights <- c(all_weights, history_weights)
  }

  # -------------------------------------------------------------------------
  # Part 2: GAM fill-in for missing history (at lower weight)
  # -------------------------------------------------------------------------
  n_missing_history <- n_history - n_actual_races

  if (n_missing_history > 0) {
    # Generate GAM samples to fill missing history
    # These are sampled from GAM distribution (mean + variance)
    gam_fill_points <- rnorm(n_missing_history,
                              mean = athlete_gam_prediction,
                              sd = gam_residual_sd)

    # Clamp to valid range [0, 100]
    gam_fill_points <- pmax(0, pmin(100, gam_fill_points))

    # Weights for GAM fill-in: use positions that would have been occupied
    # but at reduced weight factor
    # If we have 6 races, missing 4, those would be positions 7,8,9,10 in recency
    # But they're GAM estimates so weight them less
    fill_positions <- seq(n_actual_races + 1, n_history)
    gam_fill_weights <- fill_positions * gam_fill_weight_factor

    all_points <- c(all_points, gam_fill_points)
    all_weights <- c(all_weights, gam_fill_weights)
  }

  # -------------------------------------------------------------------------
  # Part 3: GAM samples (equal total weight to history)
  # -------------------------------------------------------------------------
  # Total history weight = sum of all history weights (including GAM fill)
  # For full 10 races: sum(1:10) = 55
  total_history_weight <- sum(all_weights)

  # Each GAM sample gets weight = total_history_weight / n_gam_samples
  # This makes GAM predictions have equal total weight to history
  gam_sample_weight <- total_history_weight / n_gam_samples

  # Generate GAM samples spanning the GAM distribution variance
  gam_samples <- rnorm(n_gam_samples,
                        mean = athlete_gam_prediction,
                        sd = gam_residual_sd)

  # Clamp to valid range [0, 100]
  gam_samples <- pmax(0, pmin(100, gam_samples))

  gam_sample_weights <- rep(gam_sample_weight, n_gam_samples)

  all_points <- c(all_points, gam_samples)
  all_weights <- c(all_weights, gam_sample_weights)

  # -------------------------------------------------------------------------
  # Calculate distribution parameters
  # -------------------------------------------------------------------------
  weighted_mean <- weighted.mean(all_points, all_weights, na.rm = TRUE)

  # Weighted variance calculation
  # Var = sum(w * (x - mean)^2) / sum(w)
  weighted_var <- sum(all_weights * (all_points - weighted_mean)^2) / sum(all_weights)
  weighted_sd <- sqrt(weighted_var)

  # Ensure minimum SD (prevent degenerate distributions)
  weighted_sd <- max(weighted_sd, 5)

  return(list(
    athlete_id = athlete_id,
    mean = weighted_mean,
    sd = weighted_sd,
    n_actual_races = n_actual_races,
    n_gam_fill = n_missing_history,
    n_gam_samples = n_gam_samples,
    all_points = all_points,
    all_weights = all_weights
  ))
}

# ============================================================================
# CORE FUNCTION: Simulate Race Positions
# ============================================================================
#
# Given distributions for all athletes in a race, simulate N races and
# count how often each athlete finishes in each position threshold.

simulate_race_positions <- function(athlete_distributions,
                                     n_simulations = N_SIMULATIONS,
                                     position_thresholds = c(1, 3, 5, 10, 30)) {

  n_athletes <- length(athlete_distributions)
  athlete_ids <- sapply(athlete_distributions, function(x) x$athlete_id)

  # Initialize position counts matrix
  # Rows = athletes, Columns = position thresholds
  position_counts <- matrix(0,
                            nrow = n_athletes,
                            ncol = length(position_thresholds),
                            dimnames = list(athlete_ids,
                                          paste0("top_", position_thresholds)))

  # Run simulations
  for (sim in 1:n_simulations) {
    # Sample points for each athlete from their distribution
    simulated_points <- sapply(athlete_distributions, function(dist) {
      rnorm(1, mean = dist$mean, sd = dist$sd)
    })

    # Clamp to valid range
    simulated_points <- pmax(0, pmin(100, simulated_points))

    # Rank athletes (higher points = better = lower rank number)
    # ties.method = "random" to break ties randomly
    ranks <- rank(-simulated_points, ties.method = "random")

    # Count positions for each threshold
    for (t_idx in seq_along(position_thresholds)) {
      threshold <- position_thresholds[t_idx]
      # Athletes with rank <= threshold achieved that position
      achieved <- ranks <= threshold
      position_counts[achieved, t_idx] <- position_counts[achieved, t_idx] + 1
    }
  }

  # Convert counts to probabilities
  position_probs <- position_counts / n_simulations

  # Create results dataframe
  results <- data.frame(
    athlete_id = athlete_ids,
    mean_points = sapply(athlete_distributions, function(x) x$mean),
    sd_points = sapply(athlete_distributions, function(x) x$sd),
    n_actual_races = sapply(athlete_distributions, function(x) x$n_actual_races),
    stringsAsFactors = FALSE
  )

  # Add probability columns
  for (t_idx in seq_along(position_thresholds)) {
    col_name <- paste0("prob_top_", position_thresholds[t_idx])
    results[[col_name]] <- position_probs[, t_idx]
  }

  # Sort by mean points descending
  results <- results %>% arrange(desc(mean_points))

  return(results)
}

# ============================================================================
# RELAY/TEAM SPRINT: Build Team Distribution (All-GAM)
# ============================================================================
#
# For relays and team sprints, we use an all-GAM approach since:
# 1. Teams change composition frequently
# 2. Historical team results are sparse
# 3. Individual GAM predictions can be combined

build_team_distribution <- function(team_member_ids,
                                     team_member_gam_predictions,
                                     team_member_gam_sds,
                                     team_name = "Team") {

  n_members <- length(team_member_ids)

  # Team mean = sum of individual means
  team_mean <- sum(team_member_gam_predictions)


  # Team variance = sum of individual variances (assuming independence)
  # This is a simplification - in reality there may be correlation
  team_var <- sum(team_member_gam_sds^2)
  team_sd <- sqrt(team_var)

  return(list(
    team_name = team_name,
    member_ids = team_member_ids,
    mean = team_mean,
    sd = team_sd,
    n_members = n_members
  ))
}

# ============================================================================
# RELAY/TEAM SPRINT: Simulate Team Positions
# ============================================================================

simulate_team_positions <- function(team_distributions,
                                     n_simulations = N_SIMULATIONS,
                                     position_thresholds = c(1, 3, 5, 10)) {

  n_teams <- length(team_distributions)
  team_names <- sapply(team_distributions, function(x) x$team_name)

  # Initialize position counts matrix
  position_counts <- matrix(0,
                            nrow = n_teams,
                            ncol = length(position_thresholds),
                            dimnames = list(team_names,
                                          paste0("top_", position_thresholds)))

  # Run simulations
  for (sim in 1:n_simulations) {
    # Sample points for each team from their distribution
    simulated_points <- sapply(team_distributions, function(dist) {
      rnorm(1, mean = dist$mean, sd = dist$sd)
    })

    # Rank teams (higher points = better = lower rank number)
    ranks <- rank(-simulated_points, ties.method = "random")

    # Count positions for each threshold
    for (t_idx in seq_along(position_thresholds)) {
      threshold <- position_thresholds[t_idx]
      achieved <- ranks <= threshold
      position_counts[achieved, t_idx] <- position_counts[achieved, t_idx] + 1
    }
  }

  # Convert counts to probabilities
  position_probs <- position_counts / n_simulations

  # Create results dataframe
  results <- data.frame(
    team_name = team_names,
    mean_points = sapply(team_distributions, function(x) x$mean),
    sd_points = sapply(team_distributions, function(x) x$sd),
    n_members = sapply(team_distributions, function(x) x$n_members),
    stringsAsFactors = FALSE
  )

  # Add probability columns
  for (t_idx in seq_along(position_thresholds)) {
    col_name <- paste0("prob_top_", position_thresholds[t_idx])
    results[[col_name]] <- position_probs[, t_idx]
  }

  # Sort by mean points descending
  results <- results %>% arrange(desc(mean_points))

  return(results)
}

# ============================================================================
# INTEGRATION FUNCTION: Process Individual Race
# ============================================================================
#
# This function takes a race's startlist, trains/uses GAM model,
# builds distributions for all athletes, and simulates positions.

process_individual_race <- function(race_info,
                                     startlist,
                                     chrono_data,
                                     gam_model,
                                     gender,
                                     n_simulations = N_SIMULATIONS) {

  race_type_key <- race_info$race_type_key
  log_info(paste("Processing", gender, race_type_key, "with", nrow(startlist), "athletes"))

 # Get GAM residual SD for this model
  gam_residual_sd <- sqrt(gam_model$sig2)
  if (is.null(gam_residual_sd) || is.na(gam_residual_sd)) {
    # Fallback: estimate from model deviance
    gam_residual_sd <- sqrt(gam_model$deviance / gam_model$df.residual)
  }
  # Ensure reasonable SD
  gam_residual_sd <- max(gam_residual_sd, 5)

  log_info(paste("GAM residual SD:", round(gam_residual_sd, 2)))

  # Generate GAM predictions for all athletes in startlist
  # This requires the startlist to have the necessary predictor columns
  gam_predictions <- predict(gam_model, newdata = startlist, type = "response")

  # Clamp predictions to valid range
  gam_predictions <- pmax(0, pmin(100, gam_predictions))

  # Build distribution for each athlete
  athlete_distributions <- list()

  for (i in 1:nrow(startlist)) {
    athlete_id <- startlist$ID[i]
    athlete_gam_pred <- gam_predictions[i]

    dist <- build_athlete_distribution(
      athlete_id = athlete_id,
      race_type_key = race_type_key,
      chrono_data = chrono_data,
      gam_model = gam_model,
      gam_residual_sd = gam_residual_sd,
      athlete_gam_prediction = athlete_gam_pred
    )

    athlete_distributions[[athlete_id]] <- dist
  }

  log_info(paste("Built distributions for", length(athlete_distributions), "athletes"))

  # Simulate race positions
  results <- simulate_race_positions(
    athlete_distributions = athlete_distributions,
    n_simulations = n_simulations
  )

  # Add athlete names from startlist
  results <- results %>%
    left_join(startlist %>% select(ID, Skier), by = c("athlete_id" = "ID")) %>%
    select(Skier, athlete_id, everything())

  log_info(paste("Simulation complete. Top 3 by mean points:"))
  log_info(paste(head(results$Skier, 3), collapse = ", "))

  return(results)
}

# ============================================================================
# INTEGRATION FUNCTION: Process Relay/Team Sprint
# ============================================================================

process_team_race <- function(race_info,
                               team_selections,
                               gam_predictions_by_athlete,
                               gam_sds_by_athlete,
                               n_simulations = N_SIMULATIONS) {

  log_info(paste("Processing team race:", race_info$name))

  # Build distribution for each team
  team_distributions <- list()

  for (team_name in names(team_selections)) {
    member_ids <- team_selections[[team_name]]

    # Get GAM predictions and SDs for team members
    member_preds <- gam_predictions_by_athlete[member_ids]
    member_sds <- gam_sds_by_athlete[member_ids]

    # Handle missing predictions (use defaults)
    member_preds[is.na(member_preds)] <- 25  # Default to mid-range
    member_sds[is.na(member_sds)] <- 15

    dist <- build_team_distribution(
      team_member_ids = member_ids,
      team_member_gam_predictions = member_preds,
      team_member_gam_sds = member_sds,
      team_name = team_name
    )

    team_distributions[[team_name]] <- dist
  }

  log_info(paste("Built distributions for", length(team_distributions), "teams"))

  # Simulate team positions
  results <- simulate_team_positions(
    team_distributions = team_distributions,
    n_simulations = n_simulations
  )

  log_info(paste("Team simulation complete. Top 3:"))
  log_info(paste(head(results$team_name, 3), collapse = ", "))

  return(results)
}

# ============================================================================
# EXAMPLE USAGE (commented out - for testing)
# ============================================================================

# # Example: Create mock data to test the functions
# test_simulation <- function() {
#   # Create mock athlete distributions
#   mock_distributions <- list(
#     "athlete_1" = list(athlete_id = "athlete_1", mean = 85, sd = 10, n_actual_races = 10),
#     "athlete_2" = list(athlete_id = "athlete_2", mean = 75, sd = 12, n_actual_races = 8),
#     "athlete_3" = list(athlete_id = "athlete_3", mean = 70, sd = 15, n_actual_races = 5),
#     "athlete_4" = list(athlete_id = "athlete_4", mean = 65, sd = 8, n_actual_races = 10),
#     "athlete_5" = list(athlete_id = "athlete_5", mean = 60, sd = 20, n_actual_races = 3)
#   )
#
#   # Run simulation
#   results <- simulate_race_positions(mock_distributions, n_simulations = 10000)
#
#   print("Individual Race Simulation Results:")
#   print(results)
#
#   # Test team simulation
#   mock_team_distributions <- list(
#     "Norway" = list(team_name = "Norway", mean = 320, sd = 25, n_members = 4),
#     "Sweden" = list(team_name = "Sweden", mean = 290, sd = 30, n_members = 4),
#     "Finland" = list(team_name = "Finland", mean = 270, sd = 28, n_members = 4),
#     "USA" = list(team_name = "USA", mean = 250, sd = 35, n_members = 4)
#   )
#
#   team_results <- simulate_team_positions(mock_team_distributions, n_simulations = 10000)
#
#   print("Team Race Simulation Results:")
#   print(team_results)
# }
#
# # Uncomment to test:
# # test_simulation()

log_info("Position simulation prototype loaded successfully")
log_info(paste("Configuration: N_HISTORY =", N_HISTORY_REQUIRED,
               ", N_GAM_SAMPLES =", N_GAM_SAMPLES,
               ", N_SIMULATIONS =", N_SIMULATIONS))
