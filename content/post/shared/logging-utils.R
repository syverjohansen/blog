# ============================================================================
# LOGGING UTILITIES FOR RACE PICKS SIMULATION
# ============================================================================
#
# Enhanced logging utilities for comprehensive pipeline visibility.
# Provides timing, distribution stats, data quality checks, and progress tracking.
#
# Usage:
#   source("~/blog/daehl-e/content/post/shared/logging-utils.R")
#
#   # Initialize logging for a sport
#   init_logging("cross-country", "race-picks")
#
#   # Log phases with timing
#   phase_start("Data Loading")
#   # ... do work ...
#   phase_end("Data Loading")
#
#   # Log distribution stats
#   log_distribution_stats(distributions, "Individual Race")
#
#   # Log data quality
#   log_data_quality(df, "chrono_data")

library(logger)

# ============================================================================
# GLOBAL STATE
# ============================================================================

# Phase timing storage
.logging_state <- new.env(parent = emptyenv())
.logging_state$phase_times <- list()
.logging_state$sport <- NULL
.logging_state$script_type <- NULL
.logging_state$start_time <- NULL
.logging_state$log_file <- NULL

# ============================================================================
# INITIALIZATION
# ============================================================================

#' Initialize logging for a script
#'
#' @param sport Sport name (e.g., "cross-country", "alpine")
#' @param script_type Script type (e.g., "race-picks", "champs-predictions")
#' @param log_dir Optional custom log directory
#' @return Path to log file
init_logging <- function(sport, script_type, log_dir = NULL) {
  .logging_state$sport <- sport
  .logging_state$script_type <- script_type
  .logging_state$start_time <- Sys.time()
  .logging_state$phase_times <- list()

  # Determine log directory
  if (is.null(log_dir)) {
    sport_dir <- switch(sport,
      "cross-country" = "ski",
      "nordic-combined" = "nordic-combined",
      sport
    )
    log_dir <- file.path("~/ski/elo/python", sport_dir, "polars/excel365",
                         paste0(script_type, "-simulation"))
  }

  log_dir <- path.expand(log_dir)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  # Create log file with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(log_dir, paste0(script_type, "_", timestamp, ".log"))
  .logging_state$log_file <- log_file

  # Configure logger
  log_threshold(DEBUG)
  log_appender(appender_tee(log_file))

  # Log header
  log_info("============================================================")
  log_info(paste("SCRIPT:", toupper(sport), toupper(script_type), "SIMULATION"))
  log_info(paste("STARTED:", format(.logging_state$start_time, "%Y-%m-%d %H:%M:%S")))
  log_info("============================================================")

  return(log_file)
}

# ============================================================================
# PHASE TIMING
# ============================================================================

#' Start timing a phase
#'
#' @param phase_name Name of the phase
phase_start <- function(phase_name) {
  .logging_state$phase_times[[phase_name]] <- list(
    start = Sys.time(),
    end = NULL
  )

  log_info("")
  log_info(paste(">>> PHASE:", phase_name))
  log_info(paste("    Started:", format(Sys.time(), "%H:%M:%S")))
}

#' End timing a phase and log duration
#'
#' @param phase_name Name of the phase
#' @param summary Optional summary message
phase_end <- function(phase_name, summary = NULL) {
  if (!is.null(.logging_state$phase_times[[phase_name]])) {
    .logging_state$phase_times[[phase_name]]$end <- Sys.time()

    duration <- difftime(
      .logging_state$phase_times[[phase_name]]$end,
      .logging_state$phase_times[[phase_name]]$start,
      units = "secs"
    )

    duration_str <- format_duration(as.numeric(duration))

    log_info(paste("<<< PHASE COMPLETE:", phase_name))
    log_info(paste("    Duration:", duration_str))
    if (!is.null(summary)) {
      log_info(paste("    Summary:", summary))
    }
  }
}

#' Format duration in human-readable format
#'
#' @param seconds Duration in seconds
#' @return Formatted string
format_duration <- function(seconds) {
  if (seconds < 60) {
    return(sprintf("%.1f seconds", seconds))
  } else if (seconds < 3600) {
    mins <- floor(seconds / 60)
    secs <- seconds %% 60
    return(sprintf("%d min %.1f sec", mins, secs))
  } else {
    hours <- floor(seconds / 3600)
    mins <- floor((seconds %% 3600) / 60)
    return(sprintf("%d hr %d min", hours, mins))
  }
}

# ============================================================================
# DATA QUALITY LOGGING
# ============================================================================

#' Log data quality metrics for a dataframe
#'
#' @param df Dataframe to analyze
#' @param label Label for the data
#' @param key_columns Optional vector of key columns to check for NAs
log_data_quality <- function(df, label, key_columns = NULL) {
  log_info(paste("    [DATA QUALITY]", label))
  log_info(paste("      Rows:", nrow(df), "| Columns:", ncol(df)))

  # Overall NA stats
  total_cells <- nrow(df) * ncol(df)
  total_nas <- sum(is.na(df))
  na_pct <- round(total_nas / total_cells * 100, 2)
  log_info(paste("      NA cells:", total_nas, "(", na_pct, "%)"))

  # Key column NA check
  if (!is.null(key_columns)) {
    for (col in key_columns) {
      if (col %in% names(df)) {
        col_nas <- sum(is.na(df[[col]]))
        col_na_pct <- round(col_nas / nrow(df) * 100, 1)
        if (col_nas > 0) {
          log_warn(paste("      Column '", col, "': ", col_nas, " NAs (", col_na_pct, "%)", sep = ""))
        }
      }
    }
  }

  # Date range if Date column exists
  if ("Date" %in% names(df)) {
    date_range <- range(df$Date, na.rm = TRUE)
    log_info(paste("      Date range:", date_range[1], "to", date_range[2]))
  }

  # Season info if Season column exists
  if ("Season" %in% names(df)) {
    seasons <- sort(unique(df$Season))
    log_info(paste("      Seasons:", paste(range(seasons), collapse = " - ")))
  }
}

#' Log unique value counts for categorical columns
#'
#' @param df Dataframe
#' @param columns Vector of column names to check
log_categorical_summary <- function(df, columns) {
  for (col in columns) {
    if (col %in% names(df)) {
      unique_vals <- unique(df[[col]])
      log_info(paste("      ", col, ": ", length(unique_vals), " unique values", sep = ""))
      if (length(unique_vals) <= 10) {
        log_info(paste("        Values:", paste(unique_vals, collapse = ", ")))
      }
    }
  }
}

# ============================================================================
# DISTRIBUTION STATS LOGGING
# ============================================================================

#' Log statistics for athlete distributions
#'
#' @param distributions List of distribution objects
#' @param label Label for the distributions
log_distribution_stats <- function(distributions, label) {
  if (length(distributions) == 0) {
    log_warn(paste("    [DISTRIBUTIONS]", label, "- No distributions to summarize"))
    return()
  }

  means <- sapply(distributions, function(x) x$mean)
  sds <- sapply(distributions, function(x) x$sd)
  n_races <- sapply(distributions, function(x) x$n_actual_races)

  log_info(paste("    [DISTRIBUTIONS]", label))
  log_info(paste("      Athletes:", length(distributions)))
  log_info(sprintf("      Mean points: min=%.1f, median=%.1f, max=%.1f",
                   min(means, na.rm = TRUE),
                   median(means, na.rm = TRUE),
                   max(means, na.rm = TRUE)))
  log_info(sprintf("      SD points: min=%.1f, median=%.1f, max=%.1f",
                   min(sds, na.rm = TRUE),
                   median(sds, na.rm = TRUE),
                   max(sds, na.rm = TRUE)))
  log_info(sprintf("      Historical races: min=%d, median=%d, max=%d",
                   min(n_races, na.rm = TRUE),
                   as.integer(median(n_races, na.rm = TRUE)),
                   max(n_races, na.rm = TRUE)))

  # Athletes with no history (GAM-only)
  gam_only <- sum(n_races == 0)
  if (gam_only > 0) {
    log_info(paste("      Athletes with no history (GAM-only):", gam_only))
  }
}

# ============================================================================
# PROGRESS LOGGING
# ============================================================================

#' Log progress for iterative operations
#'
#' @param current Current iteration
#' @param total Total iterations
#' @param label Label for the operation
#' @param every Log every N iterations (default 10)
log_progress <- function(current, total, label, every = 10) {
  if (current == 1 || current == total || current %% every == 0) {
    pct <- round(current / total * 100, 0)
    log_info(sprintf("      %s: %d/%d (%d%%)", label, current, total, pct))
  }
}

# ============================================================================
# GAM MODEL LOGGING
# ============================================================================

#' Log GAM model training details
#'
#' @param model Trained GAM model
#' @param features Features used
#' @param residual_sd Residual standard deviation
#' @param race_type Race type
#' @param gender Gender
log_gam_training <- function(model, features, residual_sd, race_type, gender) {
  log_info(paste("    [GAM TRAINED]", gender, race_type))
  log_info(paste("      Features:", paste(features, collapse = ", ")))
  log_info(sprintf("      Residual SD: %.2f", residual_sd))
  log_info(sprintf("      Deviance explained: %.1f%%", summary(model)$dev.expl * 100))
  log_info(sprintf("      N observations: %d", nrow(model$model)))
}

# ============================================================================
# RACE PROCESSING LOGGING
# ============================================================================

#' Log race processing start
#'
#' @param race_num Race number
#' @param total_races Total races
#' @param gender Gender
#' @param race_type Race type
#' @param city City
#' @param n_athletes Number of athletes
log_race_start <- function(race_num, total_races, gender, race_type, city, n_athletes) {
  log_info("")
  log_info(sprintf("  [RACE %d/%d] %s %s @ %s", race_num, total_races, gender, race_type, city))
  log_info(sprintf("    Athletes in startlist: %d", n_athletes))
}

#' Log race results summary
#'
#' @param results Results dataframe
#' @param race_type Race type
log_race_results <- function(results, race_type) {
  if (nrow(results) == 0) {
    log_warn("    No results generated")
    return()
  }

  # Get top 3
  top3 <- head(results, 3)
  log_info("    Top 3 predictions:")
  for (i in 1:min(3, nrow(top3))) {
    athlete <- top3[i, ]
    win_prob <- if ("prob_top_1" %in% names(athlete)) {
      sprintf("%.1f%%", athlete$prob_top_1 * 100)
    } else if ("Win" %in% names(athlete)) {
      sprintf("%.1f%%", athlete$Win * 100)
    } else {
      "N/A"
    }
    log_info(sprintf("      %d. %s (Win: %s)", i, athlete$Skier, win_prob))
  }

  # Probability sanity check
  if ("prob_top_1" %in% names(results)) {
    total_win_prob <- sum(results$prob_top_1, na.rm = TRUE)
    if (abs(total_win_prob - 1.0) > 0.01) {
      log_warn(sprintf("    Win probabilities sum to %.3f (expected ~1.0)", total_win_prob))
    }
  }
}

# ============================================================================
# SCRIPT COMPLETION LOGGING
# ============================================================================

#' Log script completion with summary
#'
#' @param races_processed Number of races processed
#' @param files_saved Vector of files saved
log_script_complete <- function(races_processed = NULL, files_saved = NULL) {
  end_time <- Sys.time()
  total_duration <- difftime(end_time, .logging_state$start_time, units = "secs")

  log_info("")
  log_info("============================================================")
  log_info(paste("SCRIPT COMPLETE:", toupper(.logging_state$sport),
                 toupper(.logging_state$script_type)))
  log_info(paste("FINISHED:", format(end_time, "%Y-%m-%d %H:%M:%S")))
  log_info(paste("TOTAL DURATION:", format_duration(as.numeric(total_duration))))

  if (!is.null(races_processed)) {
    log_info(paste("RACES PROCESSED:", races_processed))
  }

  if (!is.null(files_saved) && length(files_saved) > 0) {
    log_info("FILES SAVED:")
    for (f in files_saved) {
      log_info(paste("  -", f))
    }
  }

  # Phase timing summary
  if (length(.logging_state$phase_times) > 0) {
    log_info("")
    log_info("PHASE TIMING SUMMARY:")
    for (phase_name in names(.logging_state$phase_times)) {
      phase <- .logging_state$phase_times[[phase_name]]
      if (!is.null(phase$end)) {
        duration <- difftime(phase$end, phase$start, units = "secs")
        log_info(sprintf("  %-30s %s", phase_name, format_duration(as.numeric(duration))))
      }
    }
  }

  log_info("============================================================")
}

# ============================================================================
# VALIDATION LOGGING
# ============================================================================

#' Log validation checks
#'
#' @param check_name Name of the check
#' @param passed Whether the check passed
#' @param message Optional message
log_validation <- function(check_name, passed, message = NULL) {
  if (passed) {
    status <- "[PASS]"
    log_fn <- log_info
  } else {
    status <- "[FAIL]"
    log_fn <- log_warn
  }

  if (!is.null(message)) {
    log_fn(sprintf("    %s %s: %s", status, check_name, message))
  } else {
    log_fn(sprintf("    %s %s", status, check_name))
  }
}

#' Validate probability columns sum correctly
#'
#' @param results Results dataframe
#' @param tolerance Tolerance for sum check
validate_probabilities <- function(results, tolerance = 0.05) {
  if (nrow(results) == 0) return(TRUE)

  prob_cols <- grep("^prob_top_|^Win$|^Podium$", names(results), value = TRUE)

  for (col in prob_cols) {
    total <- sum(results[[col]], na.rm = TRUE)

    # Win should sum to ~1, others depend on threshold
    if (col == "prob_top_1" || col == "Win") {
      expected <- 1.0
      passed <- abs(total - expected) < tolerance
      log_validation(paste("Sum of", col), passed, sprintf("%.3f (expected ~%.1f)", total, expected))
    }
  }
}

# ============================================================================
# CONFIGURATION LOGGING
# ============================================================================

#' Log configuration parameters
#'
#' @param config Named list of configuration parameters
log_config <- function(config) {
  log_info("CONFIGURATION:")
  for (name in names(config)) {
    value <- config[[name]]
    if (is.logical(value)) {
      log_info(sprintf("  %s: %s", name, toupper(as.character(value))))
    } else if (is.numeric(value)) {
      log_info(sprintf("  %s: %s", name, format(value, scientific = FALSE)))
    } else {
      log_info(sprintf("  %s: %s", name, as.character(value)))
    }
  }
}

# ============================================================================
# TRACER ATHLETE/TEAM LOGGING
# ============================================================================
# Tracer logging follows one athlete (or team) through the entire pipeline
# to provide end-to-end visibility into how predictions are generated.

# Global tracer state
.tracer_state <- new.env(parent = emptyenv())
.tracer_state$men_id <- NULL
.tracer_state$men_name <- NULL
.tracer_state$ladies_id <- NULL
.tracer_state$ladies_name <- NULL
.tracer_state$men_team <- NULL
.tracer_state$ladies_team <- NULL
.tracer_state$mixed_team <- NULL

#' Initialize tracer from startlist (uses first athlete/team)
#'
#' @param startlist Startlist dataframe
#' @param gender "men" or "ladies"
#' @param type "individual" or "team"
init_tracer <- function(startlist, gender, type = "individual") {
  if (nrow(startlist) == 0) {
    log_warn(paste("Cannot initialize tracer for", gender, "- empty startlist"))
    return(NULL)
  }

  if (type == "individual") {
    # Get first athlete with valid ID
    if ("ID" %in% names(startlist) && "Skier" %in% names(startlist)) {
      first_row <- startlist[1, ]
      tracer_id <- first_row$ID
      tracer_name <- first_row$Skier

      if (gender == "men") {
        .tracer_state$men_id <- tracer_id
        .tracer_state$men_name <- tracer_name
      } else {
        .tracer_state$ladies_id <- tracer_id
        .tracer_state$ladies_name <- tracer_name
      }

      log_info("")
      log_info(sprintf("========== TRACER ATHLETE (%s): %s (ID: %s) ==========",
                       toupper(gender), tracer_name, tracer_id))
      return(list(id = tracer_id, name = tracer_name))
    }
  } else if (type == "team") {
    # Get first team/nation
    if ("Nation" %in% names(startlist)) {
      first_team <- startlist$Nation[1]
      if (!is.na(first_team)) {
        if (gender == "men") {
          .tracer_state$men_team <- first_team
        } else if (gender == "ladies") {
          .tracer_state$ladies_team <- first_team
        } else {
          .tracer_state$mixed_team <- first_team
        }

        log_info("")
        log_info(sprintf("========== TRACER TEAM (%s): %s ==========",
                         toupper(gender), first_team))
        return(list(nation = first_team))
      }
    } else if ("Team_Name" %in% names(startlist)) {
      first_team <- startlist$Team_Name[1]
      first_nation <- if ("Nation" %in% names(startlist)) startlist$Nation[1] else first_team
      if (!is.na(first_team)) {
        if (gender == "men") {
          .tracer_state$men_team <- first_nation
        } else if (gender == "ladies") {
          .tracer_state$ladies_team <- first_nation
        } else {
          .tracer_state$mixed_team <- first_nation
        }

        log_info("")
        log_info(sprintf("========== TRACER TEAM (%s): %s ==========",
                         toupper(gender), first_team))
        return(list(nation = first_nation, team_name = first_team))
      }
    }
  }

  log_warn(paste("Could not initialize tracer for", gender, type))
  return(NULL)
}

#' Check if an athlete is the tracer
#'
#' @param athlete_id Athlete ID to check
#' @param gender "men" or "ladies"
#' @return TRUE if this is the tracer athlete
is_tracer <- function(athlete_id, gender) {
  # Handle NA/NULL athlete_id

  if (is.null(athlete_id) || is.na(athlete_id)) return(FALSE)

  if (gender == "men") {
    tracer_id <- .tracer_state$men_id
    return(!is.null(tracer_id) && !is.na(tracer_id) && athlete_id == tracer_id)
  } else {
    tracer_id <- .tracer_state$ladies_id
    return(!is.null(tracer_id) && !is.na(tracer_id) && athlete_id == tracer_id)
  }
}

#' Check if a team is the tracer
#'
#' @param nation Nation/team code to check
#' @param gender "men", "ladies", or "mixed"
#' @return TRUE if this is the tracer team
is_tracer_team <- function(nation, gender) {
  # Handle NA/NULL nation
  if (is.null(nation) || is.na(nation)) return(FALSE)

  if (gender == "men") {
    tracer_team <- .tracer_state$men_team
    return(!is.null(tracer_team) && !is.na(tracer_team) && nation == tracer_team)
  } else if (gender == "ladies") {
    tracer_team <- .tracer_state$ladies_team
    return(!is.null(tracer_team) && !is.na(tracer_team) && nation == tracer_team)
  } else {
    tracer_team <- .tracer_state$mixed_team
    return(!is.null(tracer_team) && !is.na(tracer_team) && nation == tracer_team)
  }
}

#' Get tracer name for logging
#'
#' @param gender "men" or "ladies"
#' @return Tracer name or NULL
get_tracer_name <- function(gender) {
  if (gender == "men") {
    return(.tracer_state$men_name)
  } else {
    return(.tracer_state$ladies_name)
  }
}

#' Log tracer data loading info
#'
#' @param athlete_history Dataframe of athlete's historical races
#' @param gender "men" or "ladies"
log_tracer_data_load <- function(athlete_history, gender) {
  tracer_name <- get_tracer_name(gender)
  if (is.null(tracer_name)) return()

  log_info("")
  log_info(sprintf("[TRACER %s] DATA LOAD", tracer_name))
  log_info(sprintf("  Historical races found: %d", nrow(athlete_history)))

  if (nrow(athlete_history) > 0) {
    most_recent <- athlete_history[1, ]

    # Calculate points using get_points if available, otherwise use Place directly
    points_str <- tryCatch({
      if (exists("get_points") && exists("wc_points")) {
        sprintf("%d pts", round(get_points(most_recent$Place, wc_points), 0))
      } else if ("points" %in% names(most_recent)) {
        sprintf("%d pts", round(most_recent$points, 0))
      } else {
        ""
      }
    }, error = function(e) "")

    # Build log message
    city_str <- if("City" %in% names(most_recent)) most_recent$City else "Unknown"
    log_info(sprintf("  Most recent: %s %s (%s place%s)",
                     most_recent$Date, city_str,
                     ordinal(most_recent$Place),
                     if(nchar(points_str) > 0) paste0(", ", points_str) else ""))

    date_range <- range(athlete_history$Date, na.rm = TRUE)
    log_info(sprintf("  Date range: %s to %s", date_range[1], date_range[2]))

    # Log race type breakdown if available
    if ("Distance" %in% names(athlete_history) && "Technique" %in% names(athlete_history)) {
      race_types <- paste(athlete_history$Distance, athlete_history$Technique)
      type_counts <- table(race_types)
      top_types <- sort(type_counts, decreasing = TRUE)[1:min(3, length(type_counts))]
      log_info(sprintf("  Race types: %s",
                       paste(names(top_types), "(", top_types, ")", collapse = ", ")))
    }

    # Log average performance if points available
    if ("points" %in% names(athlete_history)) {
      log_info(sprintf("  Performance: mean=%.1f pts, best=%d pts, recent trend=%s",
                       mean(athlete_history$points, na.rm = TRUE),
                       max(athlete_history$points, na.rm = TRUE),
                       if(nrow(athlete_history) >= 3) {
                         recent_mean <- mean(head(athlete_history$points, 3), na.rm = TRUE)
                         older_mean <- mean(tail(athlete_history$points, -3), na.rm = TRUE)
                         if(is.na(older_mean) || length(tail(athlete_history$points, -3)) == 0) {
                           "N/A"
                         } else if(recent_mean > older_mean * 1.1) {
                           "improving"
                         } else if(recent_mean < older_mean * 0.9) {
                           "declining"
                         } else {
                           "stable"
                         }
                       } else "N/A"))
    }
  }
}

#' Log tracer GAM prediction
#'
#' @param athlete_id Athlete ID
#' @param gender "men" or "ladies"
#' @param race_type_key Race type key
#' @param features Named list/vector of feature values
#' @param gam_prediction GAM predicted points
#' @param residual_sd GAM residual standard deviation
log_tracer_gam <- function(athlete_id, gender, race_type_key, features, gam_prediction, residual_sd) {
  if (!is_tracer(athlete_id, gender)) return()

  tracer_name <- get_tracer_name(gender)
  log_info("")
  log_info(sprintf("[TRACER %s] GAM PREDICTION", tracer_name))
  log_info(sprintf("  Race type: %s", race_type_key))

  # Log key features
  feature_names <- c("prev_points_weighted", "Pelo_pct", "Distance_Pelo_pct", "Sprint_Pelo_pct")
  log_info("  Features:")
  for (fname in feature_names) {
    if (fname %in% names(features)) {
      log_info(sprintf("    %s: %.3f", fname, features[[fname]]))
    }
  }

  log_info(sprintf("  GAM prediction: %.1f points", gam_prediction))
  log_info(sprintf("  Residual SD: %.1f", residual_sd))
}

#' Log tracer distribution building
#'
#' @param athlete_id Athlete ID
#' @param gender "men" or "ladies"
#' @param dist Distribution object from build_athlete_distribution
log_tracer_distribution <- function(athlete_id, gender, dist) {
  if (!is_tracer(athlete_id, gender)) return()

  tracer_name <- get_tracer_name(gender)
  log_info("")
  log_info(sprintf("[TRACER %s] DISTRIBUTION BUILDING", tracer_name))
  log_info(sprintf("  Historical races (filtered): %d", dist$n_actual_races))
  log_info(sprintf("  GAM fill samples: %d", dist$n_gam_fill))
  log_info(sprintf("  Unadjusted mean: %.1f", dist$unadjusted_mean))

  if (dist$altitude_adj != 0) {
    log_info(sprintf("  Altitude adjustment: %+.1f", dist$altitude_adj))
  }
  if (dist$period_adj != 0) {
    log_info(sprintf("  Period adjustment: %+.1f", dist$period_adj))
  }
  log_info(sprintf("  Total adjustment: %+.1f", dist$adjustment))
  log_info(sprintf("  Final mean: %.1f, SD: %.1f", dist$mean, dist$sd))
}

#' Log tracer simulation results
#'
#' @param athlete_id Athlete ID
#' @param gender "men" or "ladies"
#' @param results Race results dataframe
log_tracer_simulation <- function(athlete_id, gender, results) {
  if (!is_tracer(athlete_id, gender)) return()

  tracer_name <- get_tracer_name(gender)
  tracer_result <- results %>% filter(ID == athlete_id)

  if (nrow(tracer_result) == 0) {
    log_warn(sprintf("[TRACER %s] Not found in simulation results", tracer_name))
    return()
  }

  log_info("")
  log_info(sprintf("[TRACER %s] SIMULATION RESULTS", tracer_name))
  log_info(sprintf("  Predicted rank: %d of %d",
                   which(results$ID == athlete_id), nrow(results)))
  log_info(sprintf("  Win probability: %.1f%%", tracer_result$prob_top_1 * 100))
  log_info(sprintf("  Podium probability: %.1f%%", tracer_result$prob_top_3 * 100))
  if ("prob_top_5" %in% names(tracer_result)) {
    log_info(sprintf("  Top-5 probability: %.1f%%", tracer_result$prob_top_5 * 100))
  }
  if ("prob_top_10" %in% names(tracer_result)) {
    log_info(sprintf("  Top-10 probability: %.1f%%", tracer_result$prob_top_10 * 100))
  }
}

#' Log tracer team composition and results
#'
#' @param nation Nation code
#' @param gender "men", "ladies", or "mixed"
#' @param team_dist Team distribution object
#' @param results Team race results dataframe (optional)
log_tracer_team <- function(nation, gender, team_dist, results = NULL) {
  if (!is_tracer_team(nation, gender)) return()

  log_info("")
  log_info(sprintf("[TRACER TEAM %s] COMPOSITION", nation))

  # Log team members if available
  if (!is.null(team_dist$podium_team)) {
    log_info("  Podium optimization team:")
    for (i in seq_along(team_dist$podium_team$members)) {
      member <- team_dist$podium_team$members[[i]]
      if (!is.null(member$name)) {
        log_info(sprintf("    Leg %d: %s (mean=%.1f, sd=%.1f)",
                         i, member$name, member$mean, member$sd))
      }
    }
  }

  log_info(sprintf("  Team mean: %.1f", team_dist$mean))
  log_info(sprintf("  Team SD: %.1f", team_dist$sd))

  # Log simulation results if provided
  if (!is.null(results)) {
    team_result <- results %>% filter(Nation == nation)
    if (nrow(team_result) > 0) {
      log_info("")
      log_info(sprintf("[TRACER TEAM %s] SIMULATION RESULTS", nation))
      log_info(sprintf("  Predicted rank: %d of %d",
                       which(results$Nation == nation), nrow(results)))
      log_info(sprintf("  Win probability: %.1f%%", team_result$prob_top_1 * 100))
      log_info(sprintf("  Podium probability: %.1f%%", team_result$prob_top_3 * 100))
    }
  }
}

#' Helper function for ordinal numbers
#'
#' @param n Number
#' @return String with ordinal suffix (1st, 2nd, 3rd, etc.)
ordinal <- function(n) {
  if (is.na(n)) return("NA")
  n <- as.integer(n)
  suffix <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
  if (n %% 100 >= 11 && n %% 100 <= 13) {
    return(paste0(n, "th"))
  }
  return(paste0(n, suffix[(n %% 10) + 1]))
}
