# Position Simulation Test Script
# Tests the simulation-based position prediction approach with real data
#
# This script:
# 1. Loads chrono data and trains GAM models (like champs-predictions.R)
# 2. Uses the new simulation approach to generate position probabilities
# 3. Compares results to show the simulation is working

library(dplyr)
library(tidyr)
library(mgcv)
library(leaps)
library(logger)

# Source the prototype
source("~/blog/daehl-e/content/post/cross-country/drafts/position-simulation-prototype.R")

log_threshold(INFO)
log_info("=== POSITION SIMULATION TEST ===")

# ============================================================================
# STEP 1: Load and Prepare Data (same as champs-predictions.R)
# ============================================================================

log_info("Loading chronological data...")

men_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

ladies_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv",
                          stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

log_info(paste("Loaded", nrow(men_chrono), "men's records"))
log_info(paste("Loaded", nrow(ladies_chrono), "ladies' records"))

# Add points column
men_chrono <- men_chrono %>%
  mutate(points = sapply(Place, get_points)) %>%
  filter(City != "Tour de Ski", Place != 0)

ladies_chrono <- ladies_chrono %>%
  mutate(points = sapply(Place, get_points)) %>%
  filter(City != "Tour de Ski", Place != 0)

# Calculate weighted prev_points for each discipline
calculate_weighted_prev_points <- function(chrono_data) {
  chrono_data %>%
    arrange(ID, Date) %>%
    group_by(ID) %>%
    mutate(
      prev_points_weighted = sapply(1:n(), function(i) {
        if (i == 1) return(0)

        current_distance <- Distance[i]
        current_technique <- Technique[i]
        prev_distances <- Distance[1:(i-1)]
        prev_techniques <- Technique[1:(i-1)]
        prev_points_values <- points[1:(i-1)]

        if (current_distance == "Sprint" && current_technique == "C") {
          matching <- prev_distances == "Sprint" & prev_techniques == "C"
        } else if (current_distance == "Sprint" && current_technique == "F") {
          matching <- prev_distances == "Sprint" & prev_techniques == "F"
        } else if (current_distance != "Sprint" && current_technique == "C") {
          matching <- prev_distances != "Sprint" & prev_techniques == "C"
        } else if (current_distance != "Sprint" && current_technique == "F") {
          matching <- prev_distances != "Sprint" & prev_techniques == "F"
        } else if (current_distance != "Sprint") {
          matching <- prev_distances != "Sprint"
        } else {
          return(0)
        }

        matching_points <- prev_points_values[matching]
        if (length(matching_points) == 0) return(0)

        recent_points <- tail(matching_points, 5)
        weights <- seq(1, length(recent_points))
        weighted.mean(recent_points, weights, na.rm = TRUE)
      })
    ) %>%
    ungroup()
}

log_info("Calculating weighted prev_points...")
men_chrono <- calculate_weighted_prev_points(men_chrono)
ladies_chrono <- calculate_weighted_prev_points(ladies_chrono)

# Filter to last 10 seasons
current_season <- max(men_chrono$Season, na.rm = TRUE)
season_cutoff <- current_season - 10

men_chrono <- men_chrono %>% filter(Season >= season_cutoff)
ladies_chrono <- ladies_chrono %>% filter(Season >= season_cutoff)

# Calculate PELO percentage columns
log_info("Calculating PELO percentage columns...")

pelo_cols <- c("Pelo", "Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
               "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo", "Classic_Pelo", "Freestyle_Pelo")

calculate_percentage_columns <- function(chrono_data) {
  chrono_data %>%
    group_by(Season, Race) %>%
    mutate(
      Pelo_pct = Pelo / max(Pelo, na.rm = TRUE),
      Distance_Pelo_pct = Distance_Pelo / max(Distance_Pelo, na.rm = TRUE),
      Distance_C_Pelo_pct = Distance_C_Pelo / max(Distance_C_Pelo, na.rm = TRUE),
      Distance_F_Pelo_pct = Distance_F_Pelo / max(Distance_F_Pelo, na.rm = TRUE),
      Sprint_Pelo_pct = Sprint_Pelo / max(Sprint_Pelo, na.rm = TRUE),
      Sprint_C_Pelo_pct = Sprint_C_Pelo / max(Sprint_C_Pelo, na.rm = TRUE),
      Sprint_F_Pelo_pct = Sprint_F_Pelo / max(Sprint_F_Pelo, na.rm = TRUE),
      Classic_Pelo_pct = Classic_Pelo / max(Classic_Pelo, na.rm = TRUE),
      Freestyle_Pelo_pct = Freestyle_Pelo / max(Freestyle_Pelo, na.rm = TRUE)
    ) %>%
    ungroup()
}

men_chrono <- calculate_percentage_columns(men_chrono)
ladies_chrono <- calculate_percentage_columns(ladies_chrono)

# Apply quartile imputation
for (col in pelo_cols) {
  if (col %in% names(men_chrono)) {
    men_chrono[[col]] <- replace_na_with_quartile(men_chrono[[col]])
  }
  if (col %in% names(ladies_chrono)) {
    ladies_chrono[[col]] <- replace_na_with_quartile(ladies_chrono[[col]])
  }
}

log_info("Data preparation complete")

# ============================================================================
# STEP 2: Train a GAM Model for Testing (Sprint Classic)
# ============================================================================

log_info("Training GAM model for Sprint Classic...")

# Filter to Sprint Classic races
men_sprint_c <- men_chrono %>%
  filter(Distance == "Sprint", Technique == "C")

log_info(paste("Sprint Classic training data:", nrow(men_sprint_c), "records"))

# Feature selection and GAM training
explanatory_vars <- c("prev_points_weighted", "Pelo_pct", "Sprint_Pelo_pct",
                      "Sprint_C_Pelo_pct", "Classic_Pelo_pct")

# Simple GAM model for points
gam_formula <- as.formula(paste("points ~",
                                paste("s(", explanatory_vars, ")", collapse = " + ")))

# Train GAM
men_sprint_c_model <- gam(gam_formula, data = men_sprint_c, method = "REML")

log_info("GAM model trained successfully")
log_info(paste("GAM residual SD:", round(sqrt(men_sprint_c_model$sig2), 2)))

# ============================================================================
# STEP 3: Create Mock Startlist (Top 30 Current Athletes)
# ============================================================================

log_info("Creating mock startlist from current season athletes...")

# Get athletes who competed in current season
current_athletes <- men_chrono %>%
  filter(Season == current_season, Distance == "Sprint", Technique == "C") %>%
  group_by(ID, Skier) %>%
  summarise(
    n_races = n(),
    avg_points = mean(points, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_points)) %>%
  head(30)

log_info(paste("Selected", nrow(current_athletes), "athletes for test"))

# Get most recent PELO values for these athletes
startlist <- men_chrono %>%
  filter(ID %in% current_athletes$ID, City == "Summer", Season == current_season) %>%
  group_by(ID) %>%
  arrange(desc(Date)) %>%
  slice(1) %>%
  ungroup() %>%
  select(ID, Skier, Pelo_pct, Sprint_Pelo_pct, Sprint_C_Pelo_pct,
         Classic_Pelo_pct, prev_points_weighted)

# Fill NA values with quartiles for prediction
for (col in c("Pelo_pct", "Sprint_Pelo_pct", "Sprint_C_Pelo_pct",
              "Classic_Pelo_pct", "prev_points_weighted")) {
  startlist[[col]] <- replace_na_with_quartile(startlist[[col]])
}

log_info(paste("Startlist created with", nrow(startlist), "athletes"))

# ============================================================================
# STEP 4: Run Position Simulation
# ============================================================================

log_info("Running position simulation...")

# Get GAM residual SD
gam_residual_sd <- sqrt(men_sprint_c_model$sig2)

# Generate GAM predictions
startlist$gam_prediction <- predict(men_sprint_c_model, newdata = startlist, type = "response")
startlist$gam_prediction <- pmax(0, pmin(100, startlist$gam_prediction))

# Build distributions for all athletes
athlete_distributions <- list()

for (i in 1:nrow(startlist)) {
  athlete_id <- startlist$ID[i]
  athlete_gam_pred <- startlist$gam_prediction[i]

  dist <- build_athlete_distribution(
    athlete_id = athlete_id,
    race_type_key = "Sprint_C",
    chrono_data = men_chrono,
    gam_model = men_sprint_c_model,
    gam_residual_sd = gam_residual_sd,
    athlete_gam_prediction = athlete_gam_pred
  )

  athlete_distributions[[athlete_id]] <- dist
}

log_info(paste("Built distributions for", length(athlete_distributions), "athletes"))

# Check distribution stats for top athletes
log_info("Distribution stats for top 5 athletes:")
for (i in 1:min(5, length(athlete_distributions))) {
  dist <- athlete_distributions[[i]]
  log_info(paste("  ", names(athlete_distributions)[i],
                 "- Mean:", round(dist$mean, 1),
                 "SD:", round(dist$sd, 1),
                 "Actual races:", dist$n_actual_races,
                 "GAM fill:", dist$n_gam_fill))
}

# Run simulation
log_info(paste("Running", N_SIMULATIONS, "simulations..."))

results <- simulate_race_positions(
  athlete_distributions = athlete_distributions,
  n_simulations = N_SIMULATIONS,
  position_thresholds = c(1, 3, 5, 10, 30)
)

# Add skier names
results <- results %>%
  left_join(startlist %>% select(ID, Skier), by = c("athlete_id" = "ID")) %>%
  select(Skier, athlete_id, mean_points, sd_points, n_actual_races, everything())

# ============================================================================
# STEP 5: Display Results
# ============================================================================

log_info("=== SIMULATION RESULTS ===")

cat("\n")
cat("Men's Sprint Classic - Position Probabilities (Top 15)\n")
cat("======================================================\n")
cat(sprintf("%-25s %6s %6s %6s %7s %7s %7s %7s %7s\n",
            "Athlete", "Mean", "SD", "Races", "Win%", "Top3%", "Top5%", "Top10%", "Top30%"))
cat("------------------------------------------------------\n")

for (i in 1:min(15, nrow(results))) {
  row <- results[i, ]
  cat(sprintf("%-25s %6.1f %6.1f %6d %6.1f%% %6.1f%% %6.1f%% %6.1f%% %6.1f%%\n",
              substr(row$Skier, 1, 25),
              row$mean_points,
              row$sd_points,
              row$n_actual_races,
              row$prob_top_1 * 100,
              row$prob_top_3 * 100,
              row$prob_top_5 * 100,
              row$prob_top_10 * 100,
              row$prob_top_30 * 100))
}

cat("\n")

# ============================================================================
# STEP 6: Test Team/Relay Simulation
# ============================================================================

log_info("Testing team/relay simulation...")

# Create mock team selections (4 athletes per team)
team_selections <- list(
  "Norway" = head(results$athlete_id[grepl("NOR", results$athlete_id) |
                                       results$Skier %in% c("Johannes Høsflot Klæbo", "Erik Valnes")], 4),
  "Sweden" = head(results$athlete_id[grepl("SWE", results$athlete_id)], 4),
  "France" = head(results$athlete_id[grepl("FRA", results$athlete_id)], 4),
  "Finland" = head(results$athlete_id[grepl("FIN", results$athlete_id)], 4)
)

# Filter out empty teams
team_selections <- team_selections[sapply(team_selections, length) >= 2]

if (length(team_selections) >= 2) {
  # Get GAM predictions and SDs for all athletes
  gam_predictions_by_athlete <- setNames(
    sapply(athlete_distributions, function(x) x$mean),
    names(athlete_distributions)
  )

  gam_sds_by_athlete <- setNames(
    sapply(athlete_distributions, function(x) x$sd),
    names(athlete_distributions)
  )

  # Build team distributions
  team_distributions <- list()

  for (team_name in names(team_selections)) {
    member_ids <- team_selections[[team_name]]
    member_preds <- gam_predictions_by_athlete[member_ids]
    member_sds <- gam_sds_by_athlete[member_ids]

    member_preds[is.na(member_preds)] <- 25
    member_sds[is.na(member_sds)] <- 15

    dist <- build_team_distribution(
      team_member_ids = member_ids,
      team_member_gam_predictions = member_preds,
      team_member_gam_sds = member_sds,
      team_name = team_name
    )

    team_distributions[[team_name]] <- dist
  }

  # Simulate team positions
  team_results <- simulate_team_positions(
    team_distributions = team_distributions,
    n_simulations = N_SIMULATIONS,
    position_thresholds = c(1, 3, 5)
  )

  cat("\nTeam Sprint - Position Probabilities\n")
  cat("=====================================\n")
  cat(sprintf("%-15s %8s %8s %8s %8s %8s\n",
              "Team", "Mean", "SD", "Win%", "Top3%", "Top5%"))
  cat("-------------------------------------\n")

  for (i in 1:nrow(team_results)) {
    row <- team_results[i, ]
    cat(sprintf("%-15s %8.1f %8.1f %7.1f%% %7.1f%% %7.1f%%\n",
                row$team_name,
                row$mean_points,
                row$sd_points,
                row$prob_top_1 * 100,
                row$prob_top_3 * 100,
                row$prob_top_5 * 100))
  }
} else {
  log_info("Not enough teams identified for team simulation test")
}

log_info("=== TEST COMPLETE ===")
