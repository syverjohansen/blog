# backtest-engine.R
# Backtesting engine for parameter optimization
# Runs simulations against historical races and evaluates predictions

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(mgcv)
})

# Source scoring metrics
source("~/blog/daehl-e/content/post/optimization/scoring-metrics.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

# Default number of simulations for backtesting (lower for speed)
BACKTEST_N_SIMULATIONS <- 500

# Position thresholds to evaluate
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)

# =============================================================================
# CORE SIMULATION FUNCTIONS (Simplified for backtesting)
# =============================================================================

#' Calculate exponential decay weights for historical races
#'
#' @param days_ago Vector of days since each race
#' @param decay_lambda Decay rate (0.002 = ~50% weight after 1 year)
#' @return Vector of weights
calculate_decay_weights <- function(days_ago, decay_lambda) {
  exp(-decay_lambda * days_ago)
}

is_team_race_type <- function(sport, race_type = NULL) {
  if (is.null(race_type)) {
    return(FALSE)
  }

  race_types <- get_sport_race_types(sport)
  race_type_info <- race_types[[race_type]]
  isTRUE(race_type_info$is_team)
}

get_team_bucket_race_types <- function(sport, race_type) {
  if (sport == "skijump" && race_type %in% c("Team_Large", "Team_Normal")) {
    return(c("Team Large", "Team Normal", "Team"))
  }
  if (sport == "skijump" && race_type == "Mixed_Team") {
    return(c("Team Large", "Team Normal", "Team Flying", "Mixed Team"))
  }

  switch(race_type,
    "Relay" = "Relay",
    "Mixed_Relay" = "Mixed Relay",
    "Single_Mixed_Relay" = "Single Mixed Relay",
    "Team" = "Team",
    "Team_Sprint" = "Team Sprint",
    "Mixed_Team" = "Mixed Team",
    "Mixed_Team_Sprint" = "Mixed Team Sprint",
    "Team_Large" = "Team Large",
    "Team_Normal" = "Team Normal",
    race_type
  )
}

replace_na_with_quartile_backtest <- function(x) {
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }

  q1 <- suppressWarnings(as.numeric(stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE)))
  if (!is.finite(q1) || is.na(q1)) {
    q1 <- suppressWarnings(median(x, na.rm = TRUE))
  }
  if (!is.finite(q1) || is.na(q1)) {
    q1 <- 0
  }
  x[is.na(x)] <- q1
  x
}

filter_positive_coefficients_backtest <- function(data, response_var, candidate_vars) {
  if (length(candidate_vars) == 0) {
    return(character())
  }

  formula <- stats::as.formula(paste(response_var, "~", paste(candidate_vars, collapse = " + ")))
  fit <- tryCatch(stats::lm(formula, data = data), error = function(e) NULL)
  if (is.null(fit)) {
    return(candidate_vars)
  }

  coeffs <- stats::coef(fit)
  coeffs <- coeffs[intersect(names(coeffs), candidate_vars)]
  positive_vars <- names(coeffs)[is.finite(coeffs) & coeffs > 0]

  if (length(positive_vars) == 0) {
    return(candidate_vars)
  }

  positive_vars
}

load_individual_chrono_for_team <- function(sport, gender, race_type) {
  mixed_race_types <- c("Mixed_Relay", "Single_Mixed_Relay", "Mixed_Team", "Mixed_Team_Sprint")

  if (race_type %in% mixed_race_types) {
    men <- load_chrono_data(sport, "men")
    ladies <- load_chrono_data(sport, "ladies")
    return(bind_rows(men, ladies))
  }

  load_chrono_data(sport, gender)
}

ensure_team_pelo_columns_backtest <- function(df) {
  elo_cols <- grep("^Avg_.*Elo$", names(df), value = TRUE)

  for (src_col in elo_cols) {
    dst_col <- sub("_Elo$", "_Pelo", src_col)
    if (!dst_col %in% names(df)) {
      df[[dst_col]] <- df[[src_col]]
    }
  }

  if ("Avg_Elo" %in% names(df) && !"Avg_Pelo" %in% names(df)) {
    df$Avg_Pelo <- df$Avg_Elo
  }

  df
}

calculate_team_percentage_columns_backtest <- function(team_chrono) {
  team_chrono <- ensure_team_pelo_columns_backtest(team_chrono)
  team_pelo_cols <- grep("^Avg_.*Pelo$", names(team_chrono), value = TRUE)

  if (length(team_pelo_cols) == 0) {
    return(team_chrono)
  }

  group_cols <- intersect(c("Season", "Race", "Date"), names(team_chrono))
  if (length(group_cols) == 0) {
    return(team_chrono)
  }

  team_chrono %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(across(all_of(team_pelo_cols), ~ {
      col_max <- suppressWarnings(max(.x, na.rm = TRUE))
      if (!is.finite(col_max) || is.na(col_max) || col_max <= 0) {
        NA_real_
      } else {
        .x / col_max
      }
    }, .names = "{.col}_pct")) %>%
    ungroup()
}

extract_member_ids_backtest <- function(row_df) {
  member_cols <- grep("^Member_\\d+_ID$", names(row_df), value = TRUE)
  if (length(member_cols) > 0) {
    ids <- unlist(row_df[1, member_cols], use.names = FALSE)
    ids <- suppressWarnings(as.numeric(ids))
    return(ids[is.finite(ids)])
  }

  if ("MemberIDs" %in% names(row_df)) {
    member_string <- row_df$MemberIDs[[1]]
    if (is.na(member_string) || !nzchar(member_string)) {
      return(numeric(0))
    }
    ids <- suppressWarnings(as.numeric(trimws(strsplit(member_string, ",")[[1]])))
    return(ids[is.finite(ids)])
  }

  numeric(0)
}

get_weighted_prev_points_all_backtest <- function(chrono_data, athlete_id, reference_date, params) {
  races <- chrono_data %>%
    filter(ID == athlete_id, Date < reference_date) %>%
    arrange(desc(Date)) %>%
    head(params$n_history_required)

  if (!"points" %in% names(races) && "Place" %in% names(races)) {
    wc_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26,
                   24, 22, 20, 18, 16, 15, 14, 13, 12, 11,
                   10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    races$points <- vapply(races$Place, function(p) {
      if (is.na(p) || p < 1 || p > 30) return(0)
      wc_points[p]
    }, numeric(1))
  }

  races <- races %>%
    mutate(
      days_ago = as.numeric(reference_date - Date),
      weight = calculate_decay_weights(days_ago, params$decay_lambda)
    ) %>%
    filter(days_ago > 0, !is.na(points))

  if (nrow(races) == 0) {
    return(list(mean = NA_real_, n = 0L))
  }

  weights <- races$weight / sum(races$weight)
  list(
    mean = sum(races$points * weights),
    n = nrow(races)
  )
}

compute_team_member_weighted_points_backtest <- function(member_ids, chrono_data, reference_date, params) {
  if (length(member_ids) == 0) {
    return(list(
      team_prev_points_weighted_avg = NA_real_,
      team_prev_points_weighted_sum = NA_real_,
      team_prev_points_weighted_count = 0L
    ))
  }

  weighted_points <- vapply(member_ids, function(member_id) {
    get_weighted_prev_points_all_backtest(chrono_data, member_id, reference_date, params)$mean
  }, numeric(1))

  valid_points <- weighted_points[is.finite(weighted_points) & !is.na(weighted_points)]
  if (length(valid_points) == 0) {
    return(list(
      team_prev_points_weighted_avg = NA_real_,
      team_prev_points_weighted_sum = NA_real_,
      team_prev_points_weighted_count = 0L
    ))
  }

  list(
    team_prev_points_weighted_avg = mean(valid_points),
    team_prev_points_weighted_sum = sum(valid_points),
    team_prev_points_weighted_count = length(valid_points)
  )
}

get_team_explanatory_vars_backtest <- function(sport) {
  switch(sport,
    "biathlon" = c(
      "team_prev_points_weighted_avg",
      "team_prev_points_weighted_sum",
      "Avg_Pelo_pct",
      "Avg_Individual_Pelo_pct",
      "Avg_Sprint_Pelo_pct",
      "Avg_Pursuit_Pelo_pct",
      "Avg_MassStart_Pelo_pct"
    ),
    "nordic-combined" = c(
      "team_prev_points_weighted_avg",
      "team_prev_points_weighted_sum",
      "Avg_Pelo_pct",
      "Avg_Individual_Pelo_pct",
      "Avg_IndividualCompact_Pelo_pct",
      "Avg_Sprint_Pelo_pct",
      "Avg_MassStart_Pelo_pct"
    ),
    "skijump" = c(
      "team_prev_points_weighted_avg",
      "team_prev_points_weighted_sum",
      "Avg_Pelo_pct",
      "Avg_Normal_Pelo_pct",
      "Avg_Large_Pelo_pct",
      "Avg_Flying_Pelo_pct"
    ),
    "cross-country" = c(
      "team_prev_points_weighted_avg",
      "team_prev_points_weighted_sum",
      "Avg_Pelo_pct"
    ),
    c("team_prev_points_weighted_avg", "team_prev_points_weighted_sum", "Avg_Pelo_pct")
  )
}

prepare_team_training_data_backtest <- function(team_chrono, individual_chrono, params) {
  if (nrow(team_chrono) == 0) {
    return(team_chrono)
  }

  team_chrono <- team_chrono %>%
    mutate(Date = as.Date(Date))
  team_chrono <- calculate_team_percentage_columns_backtest(team_chrono)

  if (!"Points" %in% names(team_chrono) && "points" %in% names(team_chrono)) {
    team_chrono$Points <- team_chrono$points
  }

  team_features <- lapply(seq_len(nrow(team_chrono)), function(i) {
    history_row <- team_chrono[i, , drop = FALSE]
    member_ids <- extract_member_ids_backtest(history_row)
    compute_team_member_weighted_points_backtest(member_ids, individual_chrono, history_row$Date[[1]], params)
  })

  bind_cols(team_chrono, bind_rows(team_features))
}

train_team_gam_backtest <- function(team_chrono, individual_chrono, sport, race_type, gender, params) {
  bucket_types <- get_team_bucket_race_types(sport, race_type)
  filtered_data <- team_chrono %>%
    mutate(Date = as.Date(Date)) %>%
    filter(RaceType %in% bucket_types)

  if (gender == "men" && "Sex" %in% names(filtered_data) && !(race_type %in% c("Mixed_Relay", "Single_Mixed_Relay", "Mixed_Team", "Mixed_Team_Sprint"))) {
    filtered_data <- filtered_data %>% filter(Sex %in% c("M", "Men", "men"))
  } else if (gender == "ladies" && "Sex" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>% filter(Sex %in% c("L", "Ladies", "ladies", "F"))
  } else if (race_type %in% c("Mixed_Relay", "Single_Mixed_Relay", "Mixed_Team", "Mixed_Team_Sprint") && "Sex" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>% filter(Sex %in% c("Mixed", "X"))
  }

  if (nrow(filtered_data) < 10) {
    return(NULL)
  }

  filtered_data <- prepare_team_training_data_backtest(filtered_data, individual_chrono, params) %>%
    filter(!is.na(Points))

  explanatory_vars <- intersect(get_team_explanatory_vars_backtest(sport), names(filtered_data))
  if (length(explanatory_vars) == 0 || nrow(filtered_data) < 10) {
    return(NULL)
  }

  for (col in explanatory_vars) {
    filtered_data[[col]] <- replace_na_with_quartile_backtest(filtered_data[[col]])
  }

  positive_vars <- filter_positive_coefficients_backtest(filtered_data, "Points", explanatory_vars)
  if (length(positive_vars) == 0) {
    positive_vars <- explanatory_vars[1]
  }

  smooth_terms <- paste("s(", positive_vars, ")", collapse = " + ")
  gam_formula <- as.formula(paste("Points ~", smooth_terms))
  points_model <- tryCatch(gam(gam_formula, data = filtered_data, method = "REML"), error = function(e) NULL)

  if (is.null(points_model)) {
    return(NULL)
  }

  residual_sd <- sqrt(points_model$sig2)
  if (!is.finite(residual_sd) || is.na(residual_sd)) {
    residual_sd <- stats::sd(filtered_data$Points, na.rm = TRUE)
  }
  if (!is.finite(residual_sd) || is.na(residual_sd)) {
    residual_sd <- params$sd_max
  }

  feature_defaults <- lapply(positive_vars, function(col) replace_na_with_quartile_backtest(filtered_data[[col]])[1])
  names(feature_defaults) <- positive_vars

  list(
    model = points_model,
    residual_sd = residual_sd,
    features = positive_vars,
    feature_defaults = feature_defaults,
    fallback_points = median(filtered_data$Points, na.rm = TRUE)
  )
}

#' Build athlete distribution from historical data
#'
#' @param athlete_id Athlete ID
#' @param chrono_data Historical race data
#' @param race_date Date of race being predicted
#' @param params List of parameters (decay_lambda, n_history_required, etc.)
#' @return List with mean and sd for athlete's point distribution
build_athlete_distribution_backtest <- function(athlete_id, chrono_data, race_date, params) {

  # Get athlete's historical races BEFORE this race
  athlete_history <- chrono_data %>%
    filter(ID == athlete_id, Date < race_date) %>%
    arrange(desc(Date)) %>%
    head(params$n_history_required)

  n_races <- nrow(athlete_history)

  if (n_races == 0) {
    # No history - use population median with high variance
    pop_median <- median(chrono_data$points, na.rm = TRUE)
    return(list(
      mean = ifelse(is.na(pop_median), 10, pop_median),
      sd = params$sd_max,
      n_races = 0
    ))
  }

  # Calculate decay weights
  days_ago <- as.numeric(race_date - athlete_history$Date)
  weights <- calculate_decay_weights(days_ago, params$decay_lambda)
  weights <- weights / sum(weights)  # Normalize

  # Weighted mean and SD
  weighted_mean <- sum(athlete_history$points * weights)
  weighted_var <- sum(weights * (athlete_history$points - weighted_mean)^2)
  weighted_sd <- sqrt(weighted_var)

  # Apply scaling and bounds
  scaled_sd <- weighted_sd * params$sd_scale_factor
  bounded_sd <- pmax(params$sd_min, pmin(params$sd_max, scaled_sd))

  # If insufficient history, increase uncertainty
  if (n_races < params$n_history_required) {
    history_factor <- n_races / params$n_history_required
    bounded_sd <- bounded_sd + (params$sd_max - bounded_sd) * (1 - history_factor) * params$gam_fill_weight_factor
  }

  return(list(
    mean = weighted_mean,
    sd = bounded_sd,
    n_races = n_races
  ))
}

#' Run Monte Carlo simulation for a race
#'
#' @param athlete_distributions Named list of distributions (name = athlete_id)
#' @param n_simulations Number of simulations
#' @param position_thresholds Vector of position thresholds to calculate
#' @return Data frame with athlete_id and prob_top_X columns
simulate_race_backtest <- function(athlete_distributions, n_simulations = BACKTEST_N_SIMULATIONS,
                                    position_thresholds = POSITION_THRESHOLDS) {

  n_athletes <- length(athlete_distributions)
  athlete_ids <- names(athlete_distributions)

  if (n_athletes < 2) {
    return(data.frame())
  }

  # Extract means and SDs
  means <- sapply(athlete_distributions, function(d) d$mean)
  sds <- sapply(athlete_distributions, function(d) d$sd)

  # Replace any NA/Inf values
  means[is.na(means) | !is.finite(means)] <- 10
  sds[is.na(sds) | !is.finite(sds)] <- 16

  # Run simulations
  position_counts <- matrix(0, nrow = n_athletes, ncol = length(position_thresholds))

  for (sim in 1:n_simulations) {
    # Generate random points for each athlete
    sim_points <- rnorm(n_athletes, mean = means, sd = sds)

    # Rank by points (higher is better)
    ranks <- rank(-sim_points, ties.method = "random")

    # Count position achievements
    for (i in seq_along(position_thresholds)) {
      threshold <- position_thresholds[i]
      position_counts[, i] <- position_counts[, i] + as.numeric(ranks <= threshold)
    }
  }

  # Convert counts to probabilities
  position_probs <- position_counts / n_simulations

  # Build result dataframe
  result <- data.frame(
    athlete_id = as.character(athlete_ids),
    mean_points = means,
    sd_points = sds,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(position_thresholds)) {
    col_name <- paste0("prob_top_", position_thresholds[i])
    result[[col_name]] <- position_probs[, i]
  }

  return(result)
}

# =============================================================================
# BACKTEST FUNCTIONS
# =============================================================================

#' Get actual race results from chrono data
#'
#' @param race_info Data frame row with race metadata (Date, Sex, Distance, etc.)
#' @param chrono_data Full chrono data
#' @return Data frame with athlete_id and Place columns
get_race_results <- function(race_info, chrono_data) {
  # Filter to this exact race using all shared grouping columns.
  results <- chrono_data %>%
    filter(Date == race_info$Date, Sex == race_info$Sex)

  race_cols <- intersect(names(race_info), names(results))
  skip_cols <- c("n_competitors", "Date", "Sex")

  for (col in setdiff(race_cols, skip_cols)) {
    value <- race_info[[col]]
    if (length(value) == 0 || is.na(value)) next
    results <- results %>% filter(.data[[col]] == value)
  }

  # Return athlete_id and Place
  results %>%
    mutate(athlete_id = as.character(ID)) %>%
    select(athlete_id, Place) %>%
    filter(!is.na(Place), Place > 0)
}

#' Get startlist for a historical race (athletes who competed)
#'
#' @param race_info Race metadata
#' @param chrono_data Full chrono data
#' @return Vector of athlete IDs
get_race_startlist <- function(race_info, chrono_data) {
  results <- get_race_results(race_info, chrono_data)
  return(results$athlete_id)
}

backtest_team_race <- function(race_info, team_chrono, individual_chrono, params,
                               sport, gender, race_type,
                               n_simulations = BACKTEST_N_SIMULATIONS) {
  race_date <- as.Date(race_info$Date)

  training_rows <- team_chrono %>% filter(Date < race_date)
  model_info <- train_team_gam_backtest(training_rows, individual_chrono, sport, race_type, gender, params)

  current_rows <- team_chrono %>%
    mutate(Date = as.Date(Date)) %>%
    filter(Date == race_date)

  race_cols <- intersect(names(race_info), names(current_rows))
  skip_cols <- c("n_competitors", "Date", "Sex")
  for (col in setdiff(race_cols, skip_cols)) {
    value <- race_info[[col]]
    if (length(value) == 0 || is.na(value)) next
    current_rows <- current_rows %>% filter(.data[[col]] == value)
  }

  if (nrow(current_rows) < 2) {
    return(NULL)
  }

  current_rows <- calculate_team_percentage_columns_backtest(current_rows)

  team_distributions <- list()
  for (i in seq_len(nrow(current_rows))) {
    team_row <- current_rows[i, , drop = FALSE]
    nation <- if ("Nation" %in% names(team_row)) team_row$Nation[[1]] else as.character(team_row$ID[[1]])
    member_ids <- extract_member_ids_backtest(team_row)
    member_features <- compute_team_member_weighted_points_backtest(member_ids, individual_chrono, race_date, params)

    feature_row <- team_row
    for (feature_name in names(member_features)) {
      feature_row[[feature_name]] <- member_features[[feature_name]]
    }

    mean_points <- if (!is.null(model_info)) {
      for (feature_name in model_info$features) {
        if (!feature_name %in% names(feature_row) || is.na(feature_row[[feature_name]][1])) {
          feature_row[[feature_name]] <- model_info$feature_defaults[[feature_name]]
        }
      }

      tryCatch(as.numeric(predict(model_info$model, newdata = feature_row, type = "response"))[1], error = function(e) NA_real_)
    } else {
      NA_real_
    }

    if (!is.finite(mean_points) || is.na(mean_points)) {
      mean_points <- if (!is.null(model_info)) model_info$fallback_points else median(team_chrono$points, na.rm = TRUE)
    }
    if (!is.finite(mean_points) || is.na(mean_points)) {
      mean_points <- 10
    }

    raw_sd <- if (!is.null(model_info)) model_info$residual_sd else params$sd_max
    scaled_sd <- pmax(params$sd_min, pmin(params$sd_max, raw_sd * params$sd_scale_factor))

    team_distributions[[as.character(nation)]] <- list(
      mean = mean_points,
      sd = scaled_sd
    )
  }

  predictions <- simulate_race_backtest(
    athlete_distributions = team_distributions,
    n_simulations = n_simulations
  )

  if (nrow(predictions) == 0) {
    return(NULL)
  }
  actuals <- current_rows %>%
    transmute(athlete_id = as.character(ID), Place) %>%
    filter(!is.na(Place), Place > 0)

  list(
    predictions = predictions,
    actuals = actuals,
    race_info = race_info,
    n_athletes = nrow(current_rows)
  )
}

#' Backtest a single race with given parameters
#'
#' @param race_info Data frame row with race metadata
#' @param chrono_data Full chrono data
#' @param params List of simulation parameters
#' @param n_simulations Number of Monte Carlo simulations
#' @return List with predictions, actuals, and race_info
backtest_race <- function(race_info, chrono_data, params,
                          n_simulations = BACKTEST_N_SIMULATIONS,
                          sport = NULL, gender = NULL, race_type = NULL,
                          individual_chrono = NULL) {

  if (!is.null(sport) && !is.null(race_type) && is_team_race_type(sport, race_type)) {
    return(backtest_team_race(
      race_info = race_info,
      team_chrono = chrono_data,
      individual_chrono = individual_chrono,
      params = params,
      sport = sport,
      gender = gender,
      race_type = race_type,
      n_simulations = n_simulations
    ))
  }

  race_date <- as.Date(race_info$Date)

  # Get athletes who competed in this race
  athlete_ids <- get_race_startlist(race_info, chrono_data)

  if (length(athlete_ids) < 5) {
    # Skip races with very few competitors
    return(NULL)
  }

  # Build distributions for each athlete
  athlete_distributions <- list()
  for (id in athlete_ids) {
    dist <- build_athlete_distribution_backtest(
      athlete_id = id,
      chrono_data = chrono_data,
      race_date = race_date,
      params = params
    )
    athlete_distributions[[as.character(id)]] <- dist
  }

  # Run simulation
  predictions <- simulate_race_backtest(
    athlete_distributions = athlete_distributions,
    n_simulations = n_simulations
  )

  if (nrow(predictions) == 0) {
    return(NULL)
  }

  # Get actual results
  actuals <- get_race_results(race_info, chrono_data)

  return(list(
    predictions = predictions,
    actuals = actuals,
    race_info = race_info,
    n_athletes = length(athlete_ids)
  ))
}

#' Backtest multiple races with given parameters
#'
#' @param races Data frame of races to backtest
#' @param chrono_data Full chrono data
#' @param params List of simulation parameters
#' @param n_simulations Number of Monte Carlo simulations
#' @param verbose Print progress
#' @return List of backtest results
backtest_races <- function(races, chrono_data, params,
                            n_simulations = BACKTEST_N_SIMULATIONS,
                            verbose = FALSE,
                            sport = NULL, gender = NULL, race_type = NULL) {

  results <- list()
  n_races <- nrow(races)
  individual_chrono <- NULL

  if (!is.null(sport) && !is.null(race_type) && is_team_race_type(sport, race_type)) {
    individual_chrono <- load_individual_chrono_for_team(sport, gender, race_type)
  }

  for (i in 1:n_races) {
    if (verbose && i %% 10 == 0) {
      cat(sprintf("Backtesting race %d/%d\n", i, n_races))
    }

    race_result <- tryCatch({
      backtest_race(races[i, ], chrono_data, params, n_simulations,
                    sport = sport, gender = gender, race_type = race_type,
                    individual_chrono = individual_chrono)
    }, error = function(e) {
      if (verbose) cat(sprintf("Error on race %d: %s\n", i, e$message))
      NULL
    })

    if (!is.null(race_result)) {
      results[[length(results) + 1]] <- race_result
    }
  }

  return(results)
}

#' Evaluate a parameter set on calibration races
#'
#' @param params List of simulation parameters
#' @param races Data frame of calibration races
#' @param chrono_data Full chrono data
#' @param n_simulations Number of Monte Carlo simulations
#' @return List with metrics
evaluate_params <- function(params, races, chrono_data,
                            n_simulations = BACKTEST_N_SIMULATIONS,
                            sport = NULL, gender = NULL, race_type = NULL) {

  # Run backtests
  backtest_results <- backtest_races(
    races = races,
    chrono_data = chrono_data,
    params = params,
    n_simulations = n_simulations,
    verbose = FALSE,
    sport = sport,
    gender = gender,
    race_type = race_type
  )

  if (length(backtest_results) == 0) {
    return(list(
      composite_score = Inf,
      brier_score = NA,
      log_loss = NA,
      calibration_ece = NA,
      n_races = 0,
      n_predictions = 0
    ))
  }

  # Calculate metrics
  metrics <- calculate_all_metrics(backtest_results)

  return(metrics)
}

# =============================================================================
# DATA LOADING HELPERS
# =============================================================================

#' Load chrono data for a sport
#'
#' @param sport Sport name ("cross-country", "alpine", "biathlon", "nordic-combined", "skijump")
#' @param gender "men" or "ladies"
#' @return Chrono data frame with standardized columns
load_chrono_data <- function(sport, gender, race_type = NULL) {
  # Map sport to directory
  sport_dirs <- list(
    "cross-country" = "ski",
    "alpine" = "alpine",
    "biathlon" = "biathlon",
    "nordic-combined" = "nordic-combined",
    "skijump" = "skijump"
  )

  sport_dir <- sport_dirs[[sport]]
  if (is.null(sport_dir)) {
    stop(paste("Unknown sport:", sport))
  }

  race_types <- get_sport_race_types(sport)
  race_type_info <- if (!is.null(race_type) && race_type %in% names(race_types)) race_types[[race_type]] else NULL
  is_team_race <- isTRUE(race_type_info$is_team)

  if (is_team_race) {
    relay_base <- file.path("~/ski/elo/python", sport_dir, "polars/relay/excel365")

    team_file <- switch(
      sport,
      "cross-country" = paste0(gender, "_relay_chrono.csv"),
      "biathlon" = switch(
        race_type,
        "Mixed_Relay" = "mixed_relay_chrono.csv",
        "Single_Mixed_Relay" = "single_mixed_relay_chrono.csv",
        paste0(gender, "_relay_chrono.csv")
      ),
      "nordic-combined" = switch(
        race_type,
        "Mixed_Team" = "mixed_team_chrono.csv",
        "Mixed_Team_Sprint" = "team_sprint_chrono.csv",
        "Team_Sprint" = "team_sprint_chrono.csv",
        paste0(gender, "_team_chrono.csv")
      ),
      "skijump" = switch(
        race_type,
        "Mixed_Team" = "mixed_team_chrono.csv",
        paste0(gender, "_team_chrono.csv")
      ),
      NULL
    )

    if (is.null(team_file)) {
      stop(paste("No team chrono mapping found for", sport, race_type))
    }

    file_path <- path.expand(file.path(relay_base, team_file))
  } else {
    # Build file path
    file_name <- paste0(gender, "_chrono_elevation.csv")
    file_path <- file.path("~/ski/elo/python", sport_dir, "polars/excel365", file_name)

    if (!file.exists(file_path)) {
      # Try without _elevation suffix
      file_name <- paste0(gender, "_chrono.csv")
      file_path <- file.path("~/ski/elo/python", sport_dir, "polars/excel365", file_name)
    }
  }

  if (!file.exists(file_path)) {
    stop(paste("Chrono file not found:", file_path))
  }

  chrono <- read.csv(file_path, stringsAsFactors = FALSE)

  if (!"ID" %in% names(chrono) && "Nation" %in% names(chrono)) {
    chrono$ID <- chrono$Nation
  }

  # Standardize Date column
  if ("Date" %in% names(chrono)) {
    chrono$Date <- as.Date(chrono$Date)
  }

  # Ensure points column exists
  if (!"points" %in% names(chrono)) {
    # Create points from Place using standard WC points system
    wc_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26,
                   24, 22, 20, 18, 16, 15, 14, 13, 12, 11,
                   10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    chrono$points <- sapply(chrono$Place, function(p) {
      if (is.na(p) || p < 1 || p > 30) return(0)
      wc_points[p]
    })
  }

  return(chrono)
}

#' Get calibration races for a sport
#'
#' @param sport Sport name
#' @param chrono_data Chrono data (to filter to valid races)
#' @param season_range Vector of seasons to include
#' @param race_type Optional race type filter
#' @param min_field_size Minimum number of competitors
#' @return Data frame of races suitable for calibration
get_calibration_races <- function(sport, chrono_data,
                                   season_range = NULL,
                                   race_type = NULL,
                                   min_field_size = NULL) {
  if (is.null(season_range)) {
    current_season <- lubridate::year(Sys.Date())
    season_range <- (current_season - 7):current_season
  }

  if (is.null(min_field_size)) {
    race_types <- get_sport_race_types(sport)
    race_type_info <- if (!is.null(race_type) && race_type %in% names(race_types)) race_types[[race_type]] else NULL
    min_field_size <- if (isTRUE(race_type_info$is_team)) 6 else 20
  }

  # Filter out offseason/reset rows before grouping
  filtered_data <- chrono_data %>%
    filter(Season %in% season_range)

  # Normalize schema differences across sports for grouping/filtering.
  if (!"Distance" %in% names(filtered_data) && "HillSize" %in% names(filtered_data)) {
    filtered_data$Distance <- as.character(filtered_data$HillSize)
  } else if (!"Distance" %in% names(filtered_data)) {
    filtered_data$Distance <- NA_character_
  }
  if (!"Technique" %in% names(filtered_data)) {
    filtered_data$Technique <- NA_character_
  }
  if (!"Hill" %in% names(filtered_data) && "HillSize" %in% names(filtered_data)) {
    filtered_data$Hill <- dplyr::case_when(
      filtered_data$HillSize >= 185 ~ "Flying",
      filtered_data$HillSize >= 120 ~ "Large",
      filtered_data$HillSize >= 85 ~ "Normal",
      TRUE ~ "Other"
    )
  }
  if (!"MS" %in% names(filtered_data) && "MassStart" %in% names(filtered_data)) {
    filtered_data$MS <- filtered_data$MassStart
  }

  # Exclude offseason rows: Event = "Offseason", Distance = "0", Place = 0
  if ("Event" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>% filter(Event != "Offseason")
  }
  if ("Place" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>% filter(Place > 0)
  }
  # Exclude reset rows only if Distance exists for this sport.
  if ("Distance" %in% names(filtered_data)) {
    filtered_data <- filtered_data %>% filter(!is.na(Distance), Distance != "0")
  }

  # Sport-specific grouping columns.
  group_cols <- switch(
    sport,
    "cross-country" = c("Date", "Sex", "Distance", "Technique", "City", "MS"),
    "alpine" = c("Date", "Sex", "Distance", "City"),
    "biathlon" = c("Date", "Sex", "RaceType", "Distance", "City", "MassStart"),
    "nordic-combined" = c("Date", "Sex", "RaceType", "Distance", "City", "MassStart"),
    "skijump" = c("Date", "Sex", "Hill", "HillSize", "RaceType", "City", "TeamEvent"),
    c("Date", "Sex", "Distance", "Technique", "City")
  )
  group_cols <- intersect(group_cols, names(filtered_data))

  races <- filtered_data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n_competitors = n(),
      .groups = "drop"
    ) %>%
    filter(n_competitors >= min_field_size)

  # Apply race type filter if specified
  if (!is.null(race_type)) {
    # Get the filter function from RACE_TYPES defined in param-grid.R
    race_types <- get_sport_race_types(sport)

    if (!is.null(race_types) && race_type %in% names(race_types)) {
      filter_func <- race_types[[race_type]]$filter
      if (!is.null(filter_func)) {
        # Apply the filter function row by row
        keep_rows <- vapply(seq_len(nrow(races)), function(i) {
          tryCatch({
            result <- filter_func(races[i, ])
            if (is.data.frame(result)) {
              result <- unlist(result, use.names = FALSE)
            } else if (is.list(result)) {
              result <- unlist(result, use.names = FALSE)
            }
            result <- as.logical(result)
            result <- result[!is.na(result)]
            if (length(result) == 0) FALSE else result[1]
          }, error = function(e) FALSE)
        }, logical(1))
        races <- races[keep_rows, ]
      }
    } else {
      # Fallback: try to parse race_type string (e.g., "Sprint_C" -> Distance="Sprint", Technique="C")
      parts <- strsplit(race_type, "_")[[1]]
      if (length(parts) >= 2) {
        distance_part <- parts[1]
        technique_part <- parts[2]

        if (distance_part == "Sprint") {
          races <- races %>% filter(Distance == "Sprint")
        } else if (distance_part == "Distance") {
          races <- races %>% filter(!Distance %in% c("Sprint", "Rel", "Ts"))
        }

        if (technique_part == "C") {
          races <- races %>% filter(Technique == "C")
        } else if (technique_part == "F") {
          races <- races %>% filter(Technique == "F")
        }

        # Check for mass start vs individual (third part)
        if (length(parts) >= 3) {
          if (parts[3] == "Ms") {
            races <- races %>% filter(grepl("Ms|Mass", Distance, ignore.case = TRUE) |
                                       grepl("Ms|Mass", Technique, ignore.case = TRUE))
          } else if (parts[3] == "Ind") {
            races <- races %>% filter(!grepl("Ms|Mass", Distance, ignore.case = TRUE) &
                                       !grepl("Ms|Mass", Technique, ignore.case = TRUE))
          }
        }
      }
    }
  }

  return(races)
}

# =============================================================================
# CONVENIENCE WRAPPERS
# =============================================================================

#' Quick evaluation of default parameters on a sport
#'
#' @param sport Sport name
#' @param gender "men" or "ladies"
#' @param n_races Number of races to sample (NULL = all)
#' @return Metrics list
quick_evaluate_defaults <- function(sport, gender = "men", n_races = 50) {
  # Load data
  chrono <- load_chrono_data(sport, gender)

  # Get calibration races
  races <- get_calibration_races(sport, chrono)

  # Sample if too many
  if (!is.null(n_races) && nrow(races) > n_races) {
    races <- races[sample(nrow(races), n_races), ]
  }

  # Default parameters
  default_params <- list(
    decay_lambda = 0.002,
    sd_scale_factor = 0.77,
    sd_min = 4,
    sd_max = 16,
    n_history_required = 10,
    gam_fill_weight_factor = 0.25
  )

  # Evaluate
  cat(sprintf("Evaluating %d races for %s %s...\n", nrow(races), gender, sport))
  metrics <- evaluate_params(default_params, races, chrono, n_simulations = 200)

  cat(sprintf("Results: %s\n", format_metrics(metrics)))

  return(metrics)
}

cat("backtest-engine.R loaded successfully\n")
