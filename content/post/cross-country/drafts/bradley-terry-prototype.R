# Bradley-Terry Prototype for Championships Predictions
#
# This approach:
# 1. Converts race results to pairwise comparisons (who beat whom)
# 2. Applies recency weighting (recent races count more)
# 3. Fits Bradley-Terry model to estimate athlete strengths
# 4. Simulates races by sampling from BT probabilities
#
# Advantage over points-based: captures actual competitive dynamics

library(dplyr)
library(tidyr)
library(BradleyTerry2)
library(logger)

# ============================================================================
# CONFIGURATION
# ============================================================================

N_RECENT_RACES <- 10          # Max races to consider per athlete pair
RECENCY_DECAY <- 0.1          # Exponential decay rate (higher = faster decay)
N_SIMULATIONS <- 1000         # Monte Carlo simulations
MIN_COMPARISONS <- 1          # Minimum comparisons needed (others use BT estimate)

log_threshold(INFO)
log_info("=== BRADLEY-TERRY PROTOTYPE ===")

# ============================================================================
# STEP 1: LOAD DATA
# ============================================================================

log_info("Loading chronological data...")

men_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv",
                       stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

ladies_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv",
                          stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

# Filter to recent seasons and valid results
current_season <- max(men_chrono$Season, na.rm = TRUE)
season_cutoff <- current_season - 5  # Last 5 seasons for BT

men_chrono <- men_chrono %>%
  filter(Season >= season_cutoff, Place > 0, City != "Tour de Ski") %>%
  mutate(ID = as.character(ID))

ladies_chrono <- ladies_chrono %>%
  filter(Season >= season_cutoff, Place > 0, City != "Tour de Ski") %>%
  mutate(ID = as.character(ID))

log_info(paste("Men's records (last 5 seasons):", nrow(men_chrono)))
log_info(paste("Ladies' records (last 5 seasons):", nrow(ladies_chrono)))

# ============================================================================
# STEP 2: CONVERT RACE RESULTS TO PAIRWISE COMPARISONS
# ============================================================================

# For each race, create all pairwise comparisons (winner, loser)
# This is O(n^2) per race but captures the actual competitive structure

create_pairwise_comparisons <- function(chrono_data, race_type_filter = NULL) {
  log_info("Creating pairwise comparisons from race results...")

  # Apply race type filter if specified
  if (!is.null(race_type_filter)) {
    chrono_data <- chrono_data %>% filter(eval(parse(text = race_type_filter)))
  }

  # Get unique races
  races <- chrono_data %>%
    distinct(Season, Race, Date) %>%
    arrange(Date)

  log_info(paste("Processing", nrow(races), "races"))

  all_comparisons <- list()

  for (i in 1:nrow(races)) {
    race_info <- races[i, ]

    # Get results for this race
    race_results <- chrono_data %>%
      filter(Season == race_info$Season, Race == race_info$Race) %>%
      select(ID, Skier, Place, Date) %>%
      filter(!is.na(Place), Place > 0) %>%
      arrange(Place)

    n_finishers <- nrow(race_results)
    if (n_finishers < 2) next

    # Create all pairwise comparisons
    for (j in 1:(n_finishers - 1)) {
      for (k in (j + 1):n_finishers) {
        # j beat k (lower place = better)
        comparison <- data.frame(
          winner_id = race_results$ID[j],
          winner_name = race_results$Skier[j],
          loser_id = race_results$ID[k],
          loser_name = race_results$Skier[k],
          date = race_results$Date[j],
          stringsAsFactors = FALSE
        )
        all_comparisons[[length(all_comparisons) + 1]] <- comparison
      }
    }

    if (i %% 100 == 0) {
      log_info(paste("Processed", i, "/", nrow(races), "races"))
    }
  }

  comparisons_df <- bind_rows(all_comparisons)
  log_info(paste("Created", nrow(comparisons_df), "pairwise comparisons"))

  return(comparisons_df)
}

# ============================================================================
# STEP 3: APPLY RECENCY WEIGHTING
# ============================================================================

apply_recency_weights <- function(comparisons_df, decay_rate = RECENCY_DECAY) {
  log_info("Applying recency weights...")

  max_date <- max(comparisons_df$date, na.rm = TRUE)

  comparisons_df <- comparisons_df %>%
    mutate(
      days_ago = as.numeric(max_date - date),
      weight = exp(-decay_rate * days_ago / 365)  # Decay per year
    )

  log_info(paste("Weight range:", round(min(comparisons_df$weight), 4),
                 "to", round(max(comparisons_df$weight), 4)))

  return(comparisons_df)
}

# ============================================================================
# STEP 4: FIT BRADLEY-TERRY MODEL
# ============================================================================

fit_bradley_terry <- function(comparisons_df, startlist_ids = NULL) {
  log_info("Fitting Bradley-Terry model...")

  # If startlist provided, filter to comparisons involving those athletes
  if (!is.null(startlist_ids)) {
    comparisons_df <- comparisons_df %>%
      filter(winner_id %in% startlist_ids | loser_id %in% startlist_ids)
    log_info(paste("Filtered to", nrow(comparisons_df), "comparisons involving startlist athletes"))
  }

  if (nrow(comparisons_df) < 10) {
    log_warn("Too few comparisons for Bradley-Terry model")
    return(NULL)
  }

  # Convert IDs to factors (required by BradleyTerry2)
  all_athletes <- unique(c(comparisons_df$winner_id, comparisons_df$loser_id))

  comparisons_df <- comparisons_df %>%
    mutate(
      winner = factor(winner_id, levels = all_athletes),
      loser = factor(loser_id, levels = all_athletes)
    )

  # Fit model with weights
  # BTm expects: outcome (1 = player1 wins), player1, player2
  # We have winner/loser, so outcome is always 1

  bt_data <- data.frame(
    player1 = comparisons_df$winner,
    player2 = comparisons_df$loser,
    outcome = 1,  # player1 always wins in our format
    weight = comparisons_df$weight
  )

  tryCatch({
    # Fit Bradley-Terry model
    bt_model <- BTm(
      outcome = outcome,
      player1 = player1,
      player2 = player2,
      weights = weight,
      data = bt_data
    )

    # Extract abilities (log-strengths)
    abilities <- BTabilities(bt_model)

    # Convert to dataframe
    # Keep athlete_id as character to match startlist IDs
    strength_df <- data.frame(
      athlete_id = as.character(rownames(abilities)),
      ability = abilities[, 1],
      se = abilities[, 2],
      stringsAsFactors = FALSE
    ) %>%
      arrange(desc(ability))

    log_info(paste("Fitted model for", nrow(strength_df), "athletes"))
    log_info(paste("Top 3 by ability:",
                   paste(head(strength_df$athlete_id, 3), collapse = ", ")))

    return(list(
      model = bt_model,
      strengths = strength_df,
      all_athletes = all_athletes
    ))

  }, error = function(e) {
    log_error(paste("Bradley-Terry fitting failed:", e$message))
    return(NULL)
  })
}

# ============================================================================
# STEP 5: CALCULATE WIN PROBABILITIES
# ============================================================================

# P(A beats B) = exp(ability_A) / (exp(ability_A) + exp(ability_B))
#              = 1 / (1 + exp(ability_B - ability_A))

calc_win_prob <- function(ability_a, ability_b) {
  1 / (1 + exp(ability_b - ability_a))
}

# ============================================================================
# STEP 6: SIMULATE RACE USING BRADLEY-TERRY PROBABILITIES
# ============================================================================

simulate_race_bt <- function(bt_result, startlist_ids, n_simulations = N_SIMULATIONS) {
  log_info(paste("Simulating race with", length(startlist_ids), "athletes,",
                 n_simulations, "simulations"))

  strengths <- bt_result$strengths

  # Get default ability for unknown athletes
  # BT abilities are relative - unknown athletes should be weaker than all known ones
  # Use minimum ability minus 1 SD (standard deviation of abilities)
  min_ability <- min(strengths$ability, na.rm = TRUE)
  sd_ability <- sd(strengths$ability, na.rm = TRUE)
  default_ability <- min_ability - sd_ability

  log_info(paste("BT ability range:", round(min_ability, 2), "to",
                 round(max(strengths$ability, na.rm = TRUE), 2),
                 "| Default for unknowns:", round(default_ability, 2)))

  # Get abilities for startlist athletes
  # Athletes not in model get default_ability (weaker than all known athletes)
  athlete_abilities <- sapply(startlist_ids, function(id) {
    idx <- which(strengths$athlete_id == id)
    if (length(idx) == 0) {
      return(default_ability)  # Default ability for unknown athletes
    }
    return(strengths$ability[idx])
  })

  names(athlete_abilities) <- startlist_ids
  n_athletes <- length(startlist_ids)

  # Position thresholds to track
  thresholds <- c(1, 3, 5, 10, 30)
  position_counts <- matrix(0, nrow = n_athletes, ncol = length(thresholds),
                            dimnames = list(startlist_ids, paste0("top_", thresholds)))

  # Run simulations
  for (sim in 1:n_simulations) {
    # Method: Sample ranking via pairwise comparisons
    # For efficiency, use a sorting approach with random pairwise outcomes

    # Add small random noise to abilities for this simulation
    # This creates variation in rankings
    noisy_abilities <- athlete_abilities + rnorm(n_athletes, 0, 0.5)

    # Rank by noisy ability (higher = better = lower rank number)
    ranks <- rank(-noisy_abilities, ties.method = "random")

    # Alternative: Full pairwise simulation (slower but more accurate)
    # Uncomment below for true pairwise simulation:
    # ranks <- simulate_pairwise_ranking(athlete_abilities)

    # Count positions
    for (t_idx in seq_along(thresholds)) {
      threshold <- thresholds[t_idx]
      achieved <- ranks <= threshold
      position_counts[achieved, t_idx] <- position_counts[achieved, t_idx] + 1
    }
  }

  # Convert to probabilities
  position_probs <- position_counts / n_simulations

  # Build results
  results <- data.frame(
    athlete_id = startlist_ids,
    ability = athlete_abilities,
    stringsAsFactors = FALSE
  )

  for (t_idx in seq_along(thresholds)) {
    col_name <- paste0("prob_top_", thresholds[t_idx])
    results[[col_name]] <- position_probs[, t_idx]
  }

  results <- results %>% arrange(desc(ability))

  return(results)
}

# Full pairwise simulation (more accurate but slower)
simulate_pairwise_ranking <- function(abilities) {
  n <- length(abilities)
  ids <- names(abilities)

  # Win matrix: wins[i,j] = number of simulated wins for i over j
  wins <- matrix(0, nrow = n, ncol = n)

  # Simulate each pair
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      # P(i beats j)
      p_i_wins <- calc_win_prob(abilities[i], abilities[j])

      # Sample outcome
      if (runif(1) < p_i_wins) {
        wins[i, j] <- 1
      } else {
        wins[j, i] <- 1
      }
    }
  }

  # Rank by total wins
  total_wins <- rowSums(wins)
  ranks <- rank(-total_wins, ties.method = "random")

  return(ranks)
}

# ============================================================================
# STEP 7: TEST WITH CHAMPIONSHIP STARTLIST
# ============================================================================

log_info("=== TESTING WITH CHAMPIONSHIP STARTLIST ===")

# Load championship startlists
men_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_champs_men.csv",
                          stringsAsFactors = FALSE)
ladies_startlist <- read.csv("~/ski/elo/python/ski/polars/excel365/startlist_champs_ladies.csv",
                             stringsAsFactors = FALSE)

log_info(paste("Loaded", nrow(men_startlist), "men on championship startlist"))
log_info(paste("Loaded", nrow(ladies_startlist), "ladies on championship startlist"))

# Name lookup - use chrono data for full coverage (all athletes in BT model)
# Predictions are still filtered to startlist athletes only
# Convert ID to character for consistent type matching
men_name_lookup <- men_chrono %>%
  mutate(ID = as.character(ID)) %>%
  distinct(ID, Skier)
ladies_name_lookup <- ladies_chrono %>%
  mutate(ID = as.character(ID)) %>%
  distinct(ID, Skier)

# Function to get top N athletes by specific Elo
get_top_startlist_by_elo <- function(startlist, elo_column, n = 60) {
  startlist %>%
    filter(!is.na(ID), ID != "", !is.na(.data[[elo_column]])) %>%
    arrange(desc(.data[[elo_column]])) %>%
    head(n) %>%
    mutate(ID = as.character(ID)) %>%
    pull(ID)
}

# ============================================================================
# TEST: Men's Sprint Classic
# ============================================================================

log_info("=== MEN'S SPRINT CLASSIC ===")

# Create comparisons for Sprint Classic
sprint_c_comparisons <- create_pairwise_comparisons(
  men_chrono,
  race_type_filter = "Distance == 'Sprint' & Technique == 'C'"
)

# Apply recency weights
sprint_c_comparisons <- apply_recency_weights(sprint_c_comparisons)

# Fit Bradley-Terry (using all data, will filter to startlist for simulation)
bt_sprint_c <- fit_bradley_terry(sprint_c_comparisons)

if (!is.null(bt_sprint_c)) {
  # Show top 10 athletes by BT ability (from full model)
  log_info("Top 10 athletes by Bradley-Terry ability (all athletes):")
  top_10 <- head(bt_sprint_c$strengths, 10)

  top_10 <- top_10 %>%
    left_join(men_name_lookup, by = c("athlete_id" = "ID"))

  for (i in 1:nrow(top_10)) {
    log_info(paste(i, ".", top_10$Skier[i],
                   "- Ability:", round(top_10$ability[i], 3),
                   "SE:", round(top_10$se[i], 3)))
  }

  # Get top 60 athletes by Sprint_C_Elo
  men_sprint_c_ids <- get_top_startlist_by_elo(men_startlist, "Sprint_C_Elo", n = 60)
  log_info(paste("Simulating with top", length(men_sprint_c_ids), "athletes by Sprint_C_Elo"))

  sim_results <- simulate_race_bt(bt_sprint_c, men_sprint_c_ids, n_simulations = N_SIMULATIONS)

  # Add names
  sim_results <- sim_results %>%
    left_join(men_name_lookup, by = c("athlete_id" = "ID"))

  cat("\n")
  cat("Men's Sprint Classic - Championship Startlist Position Probabilities\n")
  cat("=====================================================================\n")
  cat(sprintf("%-25s %8s %7s %7s %7s %7s %7s\n",
              "Athlete", "Ability", "Win%", "Top3%", "Top5%", "Top10%", "Top30%"))
  cat("---------------------------------------------------------------------\n")

  for (i in 1:min(20, nrow(sim_results))) {
    row <- sim_results[i, ]
    cat(sprintf("%-25s %8.2f %6.1f%% %6.1f%% %6.1f%% %6.1f%% %6.1f%%\n",
                substr(row$Skier, 1, 25),
                row$ability,
                row$prob_top_1 * 100,
                row$prob_top_3 * 100,
                row$prob_top_5 * 100,
                row$prob_top_10 * 100,
                row$prob_top_30 * 100))
  }

  cat("\n")
  cat(paste("Total athletes in simulation:", nrow(sim_results), "\n"))
  # Count athletes with the minimum ability (those not in BT model)
  min_ab <- min(sim_results$ability, na.rm = TRUE)
  cat(paste("Athletes not in BT model (min ability):",
            sum(sim_results$ability == min_ab), "\n"))
}

# ============================================================================
# TEST: Ladies' Skiathlon (Distance_Ms / Technique P)
# ============================================================================

log_info("=== LADIES' SKIATHLON ===")

# Create comparisons for Skiathlon
skiathlon_comparisons <- create_pairwise_comparisons(
  ladies_chrono,
  race_type_filter = "Technique == 'P'"
)

# Apply recency weights
skiathlon_comparisons <- apply_recency_weights(skiathlon_comparisons)

# Fit Bradley-Terry
bt_skiathlon <- fit_bradley_terry(skiathlon_comparisons)

if (!is.null(bt_skiathlon)) {
  # Show top 10 athletes by BT ability
  log_info("Top 10 athletes by Bradley-Terry ability (Skiathlon):")
  top_10 <- head(bt_skiathlon$strengths, 10)

  top_10 <- top_10 %>%
    left_join(ladies_name_lookup, by = c("athlete_id" = "ID"))

  for (i in 1:nrow(top_10)) {
    log_info(paste(i, ".", top_10$Skier[i],
                   "- Ability:", round(top_10$ability[i], 3),
                   "SE:", round(top_10$se[i], 3)))
  }

  # Get top 60 athletes by Distance_Elo (for Skiathlon)
  ladies_skiathlon_ids <- get_top_startlist_by_elo(ladies_startlist, "Distance_Elo", n = 60)
  log_info(paste("Simulating with top", length(ladies_skiathlon_ids), "athletes by Distance_Elo"))

  sim_results <- simulate_race_bt(bt_skiathlon, ladies_skiathlon_ids, n_simulations = N_SIMULATIONS)

  # Add names
  sim_results <- sim_results %>%
    left_join(ladies_name_lookup, by = c("athlete_id" = "ID"))

  cat("\n")
  cat("Ladies' Skiathlon - Championship Startlist Position Probabilities\n")
  cat("==================================================================\n")
  cat(sprintf("%-25s %8s %7s %7s %7s %7s %7s\n",
              "Athlete", "Ability", "Win%", "Top3%", "Top5%", "Top10%", "Top30%"))
  cat("------------------------------------------------------------------\n")

  for (i in 1:min(20, nrow(sim_results))) {
    row <- sim_results[i, ]
    cat(sprintf("%-25s %8.2f %6.1f%% %6.1f%% %6.1f%% %6.1f%% %6.1f%%\n",
                substr(row$Skier, 1, 25),
                row$ability,
                row$prob_top_1 * 100,
                row$prob_top_3 * 100,
                row$prob_top_5 * 100,
                row$prob_top_10 * 100,
                row$prob_top_30 * 100))
  }

  cat("\n")
  cat(paste("Total athletes in simulation:", nrow(sim_results), "\n"))
  # Count athletes with the minimum ability (those not in BT model)
  min_ab <- min(sim_results$ability, na.rm = TRUE)
  cat(paste("Athletes not in BT model (min ability):",
            sum(sim_results$ability == min_ab), "\n"))
}

log_info("=== BRADLEY-TERRY PROTOTYPE COMPLETE ===")
