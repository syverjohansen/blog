# param-optimizer.R
# Main orchestration script for parameter optimization
# Implements grid search, random search, and results aggregation

suppressPackageStartupMessages({
  library(dplyr)
  library(parallel)
  library(foreach)
  library(doParallel)
  library(logger)
})

# Source dependencies
source("~/blog/daehl-e/content/post/optimization/scoring-metrics.R")
source("~/blog/daehl-e/content/post/optimization/backtest-engine.R")
source("~/blog/daehl-e/content/post/optimization/param-grid.R")

# =============================================================================
# CONFIGURATION
# =============================================================================

# Number of parallel cores to use
detected_cores <- suppressWarnings(parallel::detectCores())
if (!is.finite(detected_cores) || is.na(detected_cores)) {
  detected_cores <- 2
}
N_CORES <- max(1L, as.integer(detected_cores) - 1L)

# Simulation counts for different phases
COARSE_N_SIMS <- 200
FINE_N_SIMS <- 500
FINAL_N_SIMS <- 2000

# Results directory
RESULTS_DIR <- path.expand("~/blog/daehl-e/content/post/optimization/results")
LOG_DIR <- path.expand("~/blog/daehl-e/content/post/optimization/logs")
SUPPORTED_SPORTS <- c("cross-country", "biathlon", "alpine", "skijump", "nordic-combined")

# =============================================================================
# LOGGING INFRASTRUCTURE
# =============================================================================

# Global logging state
.optim_log <- new.env(parent = emptyenv())
.optim_log$log_file <- NULL
.optim_log$start_time <- NULL
.optim_log$phase_times <- list()
.optim_log$sport <- NULL
.optim_log$gender <- NULL
.optim_log$race_type <- NULL
.optim_log$total_evaluations <- 0
.optim_log$best_score <- Inf

#' Initialize optimization logging
#'
#' @param sport Sport being optimized
#' @param gender Gender
#' @param race_type Race type (optional)
#' @return Path to log file
init_optim_logging <- function(sport, gender, race_type = NULL) {
  .optim_log$sport <- sport
  .optim_log$gender <- gender
  .optim_log$race_type <- race_type
  .optim_log$start_time <- Sys.time()
  .optim_log$phase_times <- list()
  .optim_log$total_evaluations <- 0
  .optim_log$best_score <- Inf

  # Create log directory
  if (!dir.exists(LOG_DIR)) {
    dir.create(LOG_DIR, recursive = TRUE)
  }

  # Create log file with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  rt_suffix <- if (!is.null(race_type)) paste0("_", race_type) else ""
  log_file <- file.path(LOG_DIR,
    sprintf("optimization_%s_%s%s_%s.log", sport, gender, rt_suffix, timestamp))
  .optim_log$log_file <- log_file

  # Configure logger to write to both console and file
  log_threshold(DEBUG)
  log_appender(appender_tee(log_file))

  # Log header
  log_info("================================================================================")
  log_info("  PARAMETER OPTIMIZATION RUN")
  log_info("================================================================================")
  log_info(sprintf("  Sport:      %s", toupper(sport)))
  log_info(sprintf("  Gender:     %s", gender))
  log_info(sprintf("  Race Type:  %s", ifelse(is.null(race_type), "ALL (default)", race_type)))
  log_info(sprintf("  Started:    %s", format(.optim_log$start_time, "%Y-%m-%d %H:%M:%S")))
  log_info(sprintf("  Log File:   %s", basename(log_file)))
  log_info("================================================================================")
  log_info("")
  log_info("CONFIGURATION:")
  log_info(sprintf("  Parallel Cores:     %d", N_CORES))
  log_info(sprintf("  Coarse Phase Sims:  %d", COARSE_N_SIMS))
  log_info(sprintf("  Fine Phase Sims:    %d", FINE_N_SIMS))
  log_info(sprintf("  Final Phase Sims:   %d", FINAL_N_SIMS))
  log_info("")

  return(log_file)
}

#' Log phase start
optim_phase_start <- function(phase_name) {
  .optim_log$phase_times[[phase_name]] <- list(start = Sys.time(), end = NULL)

  log_info("")
  log_info("--------------------------------------------------------------------------------")
  log_info(sprintf(">>> PHASE: %s", toupper(phase_name)))
  log_info(sprintf("    Started: %s", format(Sys.time(), "%H:%M:%S")))
  log_info("--------------------------------------------------------------------------------")
}

#' Log phase end
optim_phase_end <- function(phase_name, summary = NULL) {
  if (!is.null(.optim_log$phase_times[[phase_name]])) {
    .optim_log$phase_times[[phase_name]]$end <- Sys.time()
    duration <- as.numeric(difftime(
      .optim_log$phase_times[[phase_name]]$end,
      .optim_log$phase_times[[phase_name]]$start,
      units = "secs"
    ))

    log_info("")
    log_info(sprintf("<<< PHASE COMPLETE: %s", toupper(phase_name)))
    log_info(sprintf("    Duration: %s", format_optim_duration(duration)))
    if (!is.null(summary)) {
      log_info(sprintf("    Summary: %s", summary))
    }
    log_info("--------------------------------------------------------------------------------")
  }
}

#' Format duration nicely
format_optim_duration <- function(seconds) {
  if (seconds < 60) {
    sprintf("%.1f seconds", seconds)
  } else if (seconds < 3600) {
    mins <- floor(seconds / 60)
    secs <- seconds %% 60
    sprintf("%d min %.0f sec", mins, secs)
  } else {
    hours <- floor(seconds / 3600)
    mins <- floor((seconds %% 3600) / 60)
    sprintf("%d hr %d min", hours, mins)
  }
}

#' Log data loading summary
log_data_loaded <- function(chrono, races, sport, gender) {
  log_info("")
  log_info("DATA LOADED:")
  log_info(sprintf("  Chrono Records:     %d rows", nrow(chrono)))
  log_info(sprintf("  Date Range:         %s to %s",
                   min(chrono$Date, na.rm = TRUE),
                   max(chrono$Date, na.rm = TRUE)))
  log_info(sprintf("  Unique Athletes:    %d", length(unique(chrono$ID))))
  log_info(sprintf("  Calibration Races:  %d", nrow(races)))

  # Seasons breakdown
  if ("Season" %in% names(chrono)) {
    seasons <- sort(unique(chrono$Season))
    log_info(sprintf("  Seasons:            %d (%d - %d)",
                     length(seasons), min(seasons), max(seasons)))
  }
}

#' Log grid search setup
log_grid_setup <- function(param_grid, n_combinations, max_races, n_simulations) {
  log_info("")
  log_info("GRID SEARCH SETUP:")
  log_info(sprintf("  Parameter Combinations: %d", n_combinations))
  log_info(sprintf("  Races per Evaluation:   %d", max_races))
  log_info(sprintf("  Simulations per Eval:   %d", n_simulations))
  log_info("")
  log_info("  Parameter Ranges:")
  for (param in names(param_grid)) {
    vals <- param_grid[[param]]
    log_info(sprintf("    %-25s: %s", param,
                     paste(sprintf("%.4f", vals), collapse = ", ")))
  }

  # Estimate runtime
  est_secs_per_eval <- 5  # rough estimate
  est_total_secs <- n_combinations * est_secs_per_eval
  log_info("")
  log_info(sprintf("  Estimated Runtime:      ~%s", format_optim_duration(est_total_secs)))
}

#' Log evaluation progress
log_evaluation_progress <- function(current, total, params, metrics, is_new_best = FALSE) {
  .optim_log$total_evaluations <- .optim_log$total_evaluations + 1

  pct <- round(current / total * 100, 1)
  score <- if (!is.null(metrics)) sprintf("%.4f", metrics$composite_score) else "ERROR"

  # Only log every 10th evaluation or if new best (to avoid spam)
  if (current %% 10 == 0 || current == 1 || current == total || is_new_best) {
    if (is_new_best) {
      log_info(sprintf("  [%d/%d] (%5.1f%%) Score: %s  *** NEW BEST ***",
                       current, total, pct, score))
      log_info(sprintf("           Params: decay=%.4f, sd_scale=%.2f, sd_min=%d, sd_max=%d",
                       params$decay_lambda, params$sd_scale_factor,
                       as.integer(round(params$sd_min)), as.integer(round(params$sd_max))))
    } else {
      log_info(sprintf("  [%d/%d] (%5.1f%%) Score: %s", current, total, pct, score))
    }
  }
}

#' Log best result found
log_best_result <- function(best_params, best_metrics, phase_name) {
  log_info("")
  log_info(sprintf("BEST RESULT (%s):", toupper(phase_name)))
  log_info("")
  log_info("  Parameters:")
  log_info(sprintf("    decay_lambda:          %.6f", best_params$decay_lambda))
  log_info(sprintf("    sd_scale_factor:       %.4f", best_params$sd_scale_factor))
  log_info(sprintf("    sd_min:                %d", as.integer(best_params$sd_min)))
  log_info(sprintf("    sd_max:                %d", as.integer(best_params$sd_max)))
  log_info(sprintf("    n_history_required:    %d", as.integer(best_params$n_history_required)))
  log_info(sprintf("    gam_fill_weight_factor: %.4f", best_params$gam_fill_weight_factor))
  log_info("")
  log_info("  Metrics:")
  log_info(sprintf("    Composite Score:       %.4f", best_metrics$composite_score))
  log_info(sprintf("    Brier Score:           %.4f", best_metrics$brier_score))
  log_info(sprintf("    Log Loss:              %.4f", best_metrics$log_loss))
  log_info(sprintf("    Calibration ECE:       %.4f", best_metrics$calibration_ece))
  log_info(sprintf("    Races Evaluated:       %d", best_metrics$n_races))
  log_info(sprintf("    Total Predictions:     %d", best_metrics$n_predictions))

  # Log per-threshold breakdown
  if (!is.null(best_metrics$brier_by_threshold)) {
    log_info("")
    log_info("  Brier Score by Threshold:")
    for (name in names(best_metrics$brier_by_threshold)) {
      log_info(sprintf("    %-10s: %.4f", name, best_metrics$brier_by_threshold[[name]]))
    }
  }
}

#' Log comparison between parameter sets
log_comparison <- function(name1, metrics1, name2, metrics2) {
  log_info("")
  log_info("COMPARISON:")
  log_info(sprintf("  %-20s  %-12s  %-12s  %-10s", "Metric", name1, name2, "Diff"))
  log_info(sprintf("  %-20s  %-12s  %-12s  %-10s", "------", "----", "----", "----"))

  diff_composite <- metrics2$composite_score - metrics1$composite_score
  diff_brier <- metrics2$brier_score - metrics1$brier_score
  diff_logloss <- metrics2$log_loss - metrics1$log_loss
  diff_ece <- metrics2$calibration_ece - metrics1$calibration_ece

  log_info(sprintf("  %-20s  %-12.4f  %-12.4f  %+.4f", "Composite",
                   metrics1$composite_score, metrics2$composite_score, diff_composite))
  log_info(sprintf("  %-20s  %-12.4f  %-12.4f  %+.4f", "Brier",
                   metrics1$brier_score, metrics2$brier_score, diff_brier))
  log_info(sprintf("  %-20s  %-12.4f  %-12.4f  %+.4f", "Log Loss",
                   metrics1$log_loss, metrics2$log_loss, diff_logloss))
  log_info(sprintf("  %-20s  %-12.4f  %-12.4f  %+.4f", "ECE",
                   metrics1$calibration_ece, metrics2$calibration_ece, diff_ece))

  improvement <- (metrics1$composite_score - metrics2$composite_score) /
                  metrics1$composite_score * 100
  log_info("")
  log_info(sprintf("  Improvement: %.1f%%", improvement))
}

#' Log final summary
log_optimization_summary <- function() {
  end_time <- Sys.time()
  total_duration <- as.numeric(difftime(end_time, .optim_log$start_time, units = "secs"))

  log_info("")
  log_info("================================================================================")
  log_info("  OPTIMIZATION COMPLETE")
  log_info("================================================================================")
  log_info(sprintf("  Sport:            %s", .optim_log$sport))
  log_info(sprintf("  Gender:           %s", .optim_log$gender))
  log_info(sprintf("  Race Type:        %s",
                   ifelse(is.null(.optim_log$race_type), "ALL", .optim_log$race_type)))
  log_info(sprintf("  Total Duration:   %s", format_optim_duration(total_duration)))
  log_info(sprintf("  Total Evaluations: %d", .optim_log$total_evaluations))
  log_info(sprintf("  Best Score:       %.4f", .optim_log$best_score))
  log_info("")

  # Phase breakdown
  log_info("  Phase Timings:")
  for (phase_name in names(.optim_log$phase_times)) {
    phase <- .optim_log$phase_times[[phase_name]]
    if (!is.null(phase$end)) {
      duration <- as.numeric(difftime(phase$end, phase$start, units = "secs"))
      log_info(sprintf("    %-25s: %s", phase_name, format_optim_duration(duration)))
    }
  }

  log_info("")
  log_info(sprintf("  Log File: %s", .optim_log$log_file))
  log_info("================================================================================")
}

#' Log error
log_optim_error <- function(context, error_msg) {
  log_error(sprintf("[%s] ERROR: %s", context, error_msg))
}

#' Log warning
log_optim_warning <- function(context, warning_msg) {
  log_warn(sprintf("[%s] WARNING: %s", context, warning_msg))
}

# =============================================================================
# GRID SEARCH
# =============================================================================

#' Run grid search optimization
#'
#' @param sport Sport name
#' @param gender "men" or "ladies"
#' @param race_type Optional race type filter
#' @param param_grid Parameter grid to search
#' @param n_simulations Simulations per evaluation
#' @param max_races Maximum races to use (NULL = all)
#' @param parallel Use parallel processing
#' @param verbose Print progress
#' @return Data frame with results sorted by composite score
run_grid_search <- function(sport, gender, race_type = NULL,
                             param_grid = COARSE_PARAM_GRID,
                             n_simulations = COARSE_N_SIMS,
                             max_races = 100,
                             parallel = TRUE,
                             verbose = TRUE) {

  optim_phase_start("Grid Search - Data Loading")

  # Load data
  log_info("Loading chrono data...")
  chrono <- tryCatch({
    load_chrono_data(sport, gender, race_type = race_type)
  }, error = function(e) {
    log_optim_error("Data Loading", e$message)
    return(NULL)
  })

  if (is.null(chrono)) {
    optim_phase_end("Grid Search - Data Loading", "FAILED - no chrono data")
    return(data.frame())
  }

  # Get calibration races
  log_info("Getting calibration races...")
  races <- get_calibration_races(sport, chrono, race_type = race_type)

  if (nrow(races) == 0) {
    log_optim_warning("Data Loading", "No calibration races found")
    optim_phase_end("Grid Search - Data Loading", "FAILED - no races")
    return(data.frame())
  }

  # Sample if too many
  original_race_count <- nrow(races)
  if (!is.null(max_races) && nrow(races) > max_races) {
    races <- races[sample(nrow(races), max_races), ]
    log_info(sprintf("Sampled %d races from %d available", max_races, original_race_count))
  }

  # Log data summary
  log_data_loaded(chrono, races, sport, gender)
  optim_phase_end("Grid Search - Data Loading",
                  sprintf("%d races, %d chrono records", nrow(races), nrow(chrono)))

  # Expand parameter grid
  optim_phase_start("Grid Search - Evaluation")

  combinations <- expand_param_grid(param_grid)
  n_combos <- nrow(combinations)

  log_grid_setup(param_grid, n_combos, nrow(races), n_simulations)

  best_score <- Inf
  best_idx <- 1

  # Run evaluations
  use_parallel <- isTRUE(parallel) && n_combos > 10 && N_CORES > 1

  if (use_parallel) {
    log_info("")
    log_info(sprintf("PARALLEL EXECUTION: %d cores", N_CORES))
    log_info("(Progress logging limited in parallel mode)")
    log_info("")

    cl <- tryCatch(makeCluster(N_CORES), error = function(e) {
      log_optim_warning("Parallel Setup", paste("Falling back to sequential:", e$message))
      NULL
    })

    if (is.null(cl)) {
      use_parallel <- FALSE
    }
  }

  if (use_parallel) {
    registerDoParallel(cl)

    # Load the same source files on each worker so helper functions stay in sync.
    clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(dplyr)
        library(lubridate)
        library(mgcv)
      })
      source("~/blog/daehl-e/content/post/optimization/scoring-metrics.R")
      source("~/blog/daehl-e/content/post/optimization/backtest-engine.R")
      source("~/blog/daehl-e/content/post/optimization/param-grid.R")
      NULL
    })

    clusterExport(cl, c(
      "chrono", "races", "n_simulations", "sport", "gender", "race_type"
    ), envir = environment())

    eval_start <- Sys.time()

    # First, run a single test evaluation to catch export errors early
    log_info("Running test evaluation to verify worker setup...")
    test_result <- tryCatch({
      clusterCall(cl, function() {
        params <- list(decay_lambda = 0.002, sd_scale_factor = 0.77,
                       sd_min = 4, sd_max = 16, n_history_required = 10,
                       gam_fill_weight_factor = 0.25)
        exists("evaluate_params") && exists("calculate_all_metrics") && exists("is_team_race_type")
      })
    }, error = function(e) {
      log_optim_error("Worker Test", e$message)
      return(NULL)
    })

    if (is.null(test_result) || !all(unlist(test_result))) {
      log_optim_error("Worker Setup", "Required functions not available in worker processes")
      stopCluster(cl)
      return(data.frame())
    }
    log_info("Worker setup verified successfully")

    results <- foreach(i = 1:n_combos,
                       .combine = rbind,
                       .packages = c("dplyr", "lubridate", "mgcv"),
                       .errorhandling = "pass") %dopar% {

      tryCatch({
        params <- as.list(combinations[i, ])
        metrics <- evaluate_params(params, races, chrono, n_simulations,
                                   sport = sport, gender = gender, race_type = race_type)

        data.frame(
          combo_id = i,
          composite_score = metrics$composite_score,
          brier_score = metrics$brier_score,
          log_loss = metrics$log_loss,
          calibration_ece = metrics$calibration_ece,
          n_races = metrics$n_races,
          n_predictions = metrics$n_predictions,
          decay_lambda = params$decay_lambda,
          sd_scale_factor = params$sd_scale_factor,
          sd_min = as.integer(round(params$sd_min)),
          sd_max = as.integer(round(params$sd_max)),
          n_history_required = as.integer(round(params$n_history_required)),
          gam_fill_weight_factor = params$gam_fill_weight_factor,
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        # Return error info instead of failing silently
        data.frame(
          combo_id = i,
          composite_score = NA,
          brier_score = NA,
          log_loss = NA,
          calibration_ece = NA,
          n_races = 0,
          n_predictions = 0,
          decay_lambda = NA,
          sd_scale_factor = NA,
          sd_min = NA,
          sd_max = NA,
          n_history_required = NA,
          gam_fill_weight_factor = NA,
          error_msg = e$message,
          stringsAsFactors = FALSE
        )
      })
    }

    stopCluster(cl)

    eval_duration <- as.numeric(difftime(Sys.time(), eval_start, units = "secs"))
    log_info("")
    log_info(sprintf("Parallel evaluation completed in %s", format_optim_duration(eval_duration)))
    log_info(sprintf("Average time per combination: %.1f seconds", eval_duration / n_combos))

    # Check for errors
    if (is.null(results) || nrow(results) == 0) {
      log_optim_error("Grid Search", "No results returned from parallel evaluation")
      return(data.frame())
    }

    # Report any errors
    if ("error_msg" %in% names(results)) {
      n_errors <- sum(!is.na(results$error_msg))
      if (n_errors > 0) {
        log_optim_warning("Grid Search", sprintf("%d/%d evaluations failed", n_errors, n_combos))
        # Log first few unique errors
        unique_errors <- unique(na.omit(results$error_msg))
        for (err in head(unique_errors, 3)) {
          log_info(sprintf("  Error: %s", substr(err, 1, 100)))
        }
        # Remove failed rows
        results <- results[is.na(results$error_msg), ]
        results$error_msg <- NULL
      } else {
        results$error_msg <- NULL
      }
    }

    if (nrow(results) == 0) {
      log_optim_error("Grid Search", "All evaluations failed")
      return(data.frame())
    }

    log_info(sprintf("Successful evaluations: %d/%d", nrow(results), n_combos))

  } else {
    # Sequential evaluation with detailed logging
    log_info("")
    log_info("SEQUENTIAL EXECUTION:")
    log_info("")

    results <- data.frame()

    for (i in 1:n_combos) {
      params <- as.list(combinations[i, ])
      metrics <- tryCatch({
        evaluate_params(params, races, chrono, n_simulations,
                        sport = sport, gender = gender, race_type = race_type)
      }, error = function(e) {
        log_optim_error(sprintf("Combo %d", i), e$message)
        return(list(composite_score = Inf, brier_score = NA, log_loss = NA,
                    calibration_ece = NA, n_races = 0, n_predictions = 0))
      })

      is_new_best <- !is.na(metrics$composite_score) && metrics$composite_score < best_score
      if (is_new_best) {
        best_score <- metrics$composite_score
        best_idx <- i
        .optim_log$best_score <- best_score
      }

      log_evaluation_progress(i, n_combos, params, metrics, is_new_best)

      row <- data.frame(
        combo_id = i,
        composite_score = metrics$composite_score,
        brier_score = metrics$brier_score,
        log_loss = metrics$log_loss,
        calibration_ece = metrics$calibration_ece,
        n_races = metrics$n_races,
        n_predictions = metrics$n_predictions,
        decay_lambda = params$decay_lambda,
        sd_scale_factor = params$sd_scale_factor,
        sd_min = as.integer(round(params$sd_min)),
        sd_max = as.integer(round(params$sd_max)),
        n_history_required = as.integer(round(params$n_history_required)),
        gam_fill_weight_factor = params$gam_fill_weight_factor,
        stringsAsFactors = FALSE
      )

      results <- rbind(results, row)
    }
  }

  # Sort by composite score (lower is better)
  results <- results %>%
    arrange(composite_score) %>%
    mutate(rank = row_number())

  optim_phase_end("Grid Search - Evaluation",
                  sprintf("%d combinations evaluated", n_combos))

  # Log top results
  log_info("")
  log_info("TOP 10 RESULTS:")
  log_info(sprintf("  %-5s  %-12s  %-10s  %-10s  %-8s  %-8s  %-8s  %-8s",
                   "Rank", "Composite", "Brier", "LogLoss", "Decay", "SD_Scale", "SD_Min", "SD_Max"))
  log_info(sprintf("  %-5s  %-12s  %-10s  %-10s  %-8s  %-8s  %-8s  %-8s",
                   "----", "---------", "-----", "-------", "-----", "--------", "------", "------"))

  for (i in 1:min(10, nrow(results))) {
    r <- results[i, ]
    log_info(sprintf("  %-5d  %-12.4f  %-10.4f  %-10.4f  %-8.4f  %-8.2f  %-8d  %-8d",
                     r$rank, r$composite_score, r$brier_score, r$log_loss,
                     r$decay_lambda, r$sd_scale_factor, r$sd_min, r$sd_max))
  }

  # Log best result details
  best_row <- results[1, ]
  best_params <- list(
    decay_lambda = best_row$decay_lambda,
    sd_scale_factor = best_row$sd_scale_factor,
    sd_min = best_row$sd_min,
    sd_max = best_row$sd_max,
    n_history_required = best_row$n_history_required,
    gam_fill_weight_factor = best_row$gam_fill_weight_factor
  )
  best_metrics <- list(
    composite_score = best_row$composite_score,
    brier_score = best_row$brier_score,
    log_loss = best_row$log_loss,
    calibration_ece = best_row$calibration_ece,
    n_races = best_row$n_races,
    n_predictions = best_row$n_predictions
  )

  log_best_result(best_params, best_metrics, "Grid Search")

  .optim_log$best_score <- best_row$composite_score

  return(results)
}

# =============================================================================
# RANDOM SEARCH
# =============================================================================

#' Run random search within a parameter region
#'
#' @param sport Sport name
#' @param gender "men" or "ladies"
#' @param race_type Optional race type filter
#' @param search_region Parameter bounds (from create_fine_grid or similar)
#' @param n_samples Number of random samples
#' @param n_simulations Simulations per evaluation
#' @param max_races Maximum races to use
#' @param verbose Print progress
#' @return Data frame with results
run_random_search <- function(sport, gender, race_type = NULL,
                               search_region = NULL,
                               n_samples = 50,
                               n_simulations = FINE_N_SIMS,
                               max_races = 100,
                               verbose = TRUE) {

  if (is.null(search_region)) {
    search_region <- DEFAULT_PARAM_GRID
  }

  optim_phase_start("Random Search")

  log_info("")
  log_info("RANDOM SEARCH CONFIGURATION:")
  log_info(sprintf("  Samples:           %d", n_samples))
  log_info(sprintf("  Simulations:       %d", n_simulations))
  log_info(sprintf("  Max Races:         %d", max_races))
  log_info("")
  log_info("  Search Region:")
  for (param in names(search_region)) {
    vals <- search_region[[param]]
    log_info(sprintf("    %-25s: [%.4f - %.4f]", param, min(vals), max(vals)))
  }

  # Load data
  chrono <- load_chrono_data(sport, gender, race_type = race_type)
  races <- get_calibration_races(sport, chrono, race_type = race_type)

  if (!is.null(max_races) && nrow(races) > max_races) {
    races <- races[sample(nrow(races), max_races), ]
  }

  log_info(sprintf("  Races Used:        %d", nrow(races)))
  log_info("")

  # Generate random samples
  samples <- sample_params(search_region, n_samples)

  # Evaluate each
  results <- data.frame()
  best_score <- Inf

  log_info("RANDOM SAMPLING:")

  for (i in 1:n_samples) {
    params <- as.list(samples[i, ])
      metrics <- tryCatch({
        evaluate_params(params, races, chrono, n_simulations,
                        sport = sport, gender = gender, race_type = race_type)
    }, error = function(e) {
      log_optim_error(sprintf("Sample %d", i), e$message)
      return(list(composite_score = Inf, brier_score = NA, log_loss = NA,
                  calibration_ece = NA, n_races = 0, n_predictions = 0))
    })

    is_new_best <- !is.na(metrics$composite_score) && metrics$composite_score < best_score
    if (is_new_best) {
      best_score <- metrics$composite_score
    }

    log_evaluation_progress(i, n_samples, params, metrics, is_new_best)

    row <- data.frame(
      sample_id = i,
      composite_score = metrics$composite_score,
      brier_score = metrics$brier_score,
      log_loss = metrics$log_loss,
      calibration_ece = metrics$calibration_ece,
      n_races = metrics$n_races,
      n_predictions = metrics$n_predictions,
      decay_lambda = params$decay_lambda,
      sd_scale_factor = params$sd_scale_factor,
      sd_min = as.integer(round(params$sd_min)),
      sd_max = as.integer(round(params$sd_max)),
      n_history_required = as.integer(round(params$n_history_required)),
      gam_fill_weight_factor = params$gam_fill_weight_factor,
      stringsAsFactors = FALSE
    )

    results <- rbind(results, row)
  }

  results <- results %>%
    arrange(composite_score) %>%
    mutate(rank = row_number())

  optim_phase_end("Random Search", sprintf("%d samples, best score: %.4f", n_samples, best_score))

  # Log top results
  log_info("")
  log_info("TOP 5 RANDOM SEARCH RESULTS:")
  for (i in 1:min(5, nrow(results))) {
    r <- results[i, ]
    log_info(sprintf("  %d. Score: %.4f (decay=%.4f, sd_scale=%.2f)",
                     i, r$composite_score, r$decay_lambda, r$sd_scale_factor))
  }

  return(results)
}

# =============================================================================
# FULL OPTIMIZATION PIPELINE
# =============================================================================

#' Run full optimization for a sport/race-type combination
#'
#' @param sport Sport name
#' @param gender "men" or "ladies"
#' @param race_type Optional race type (NULL = optimize all)
#' @param verbose Print progress
#' @return List with best parameters and full results
optimize_params <- function(sport, gender, race_type = NULL, verbose = TRUE) {

  # Initialize logging for this optimization run
  init_optim_logging(sport, gender, race_type)

  # Phase 1: Coarse grid search
  log_info("")
  log_info("================================================================================")
  log_info("  PHASE 1: COARSE GRID SEARCH")
  log_info("================================================================================")

  coarse_results <- run_grid_search(
    sport = sport,
    gender = gender,
    race_type = race_type,
    param_grid = COARSE_PARAM_GRID,
    n_simulations = COARSE_N_SIMS,
    max_races = 80,
    parallel = TRUE,
    verbose = verbose
  )

  if (nrow(coarse_results) == 0) {
    log_optim_warning("Coarse Search", "No results returned - using defaults")
    log_optimization_summary()
    return(list(best_params = DEFAULT_PARAMS, results = NULL))
  }

  # Get best from coarse search
  best_coarse <- as.list(coarse_results[1, c("decay_lambda", "sd_scale_factor",
                                              "sd_min", "sd_max", "n_history_required",
                                              "gam_fill_weight_factor")])

  # Phase 2: Random search refinement around best
  log_info("")
  log_info("================================================================================")
  log_info("  PHASE 2: RANDOM SEARCH REFINEMENT")
  log_info("================================================================================")
  log_info("")
  log_info("Refining around best coarse result:")
  log_info(sprintf("  decay_lambda: %.4f", best_coarse$decay_lambda))
  log_info(sprintf("  sd_scale_factor: %.2f", best_coarse$sd_scale_factor))

  fine_grid <- create_fine_grid(best_coarse, margin = 0.25)

  random_results <- run_random_search(
    sport = sport,
    gender = gender,
    race_type = race_type,
    search_region = fine_grid,
    n_samples = 30,
    n_simulations = FINE_N_SIMS,
    max_races = 100,
    verbose = verbose
  )

  # Combine and get overall best
  all_results <- bind_rows(
    coarse_results %>% mutate(phase = "coarse"),
    random_results %>% mutate(phase = "random")
  ) %>%
    arrange(composite_score)

  best_params <- as.list(all_results[1, c("decay_lambda", "sd_scale_factor",
                                           "sd_min", "sd_max", "n_history_required",
                                           "gam_fill_weight_factor")])

  # Phase 3: Final validation with more simulations
  log_info("")
  log_info("================================================================================")
  log_info("  PHASE 3: FINAL VALIDATION")
  log_info("================================================================================")

  optim_phase_start("Final Validation")

  log_info("")
  log_info("Validating best parameters with high-fidelity simulation:")
  log_info(sprintf("  Simulations: %d", FINAL_N_SIMS))

  chrono <- load_chrono_data(sport, gender, race_type = race_type)
  races <- get_calibration_races(sport, chrono, race_type = race_type)

  log_info(sprintf("  Races: %d", nrow(races)))

  final_metrics <- evaluate_params(best_params, races, chrono,
                                   n_simulations = FINAL_N_SIMS,
                                   sport = sport, gender = gender, race_type = race_type)

  optim_phase_end("Final Validation",
                  sprintf("Composite: %.4f, Brier: %.4f", final_metrics$composite_score, final_metrics$brier_score))

  # Log final best result
  log_best_result(best_params, final_metrics, "Final")

  # Compare to defaults
  log_info("")
  log_info("COMPARISON TO DEFAULTS:")
  default_metrics <- evaluate_params(DEFAULT_PARAMS, races, chrono,
                                     n_simulations = 500,
                                     sport = sport, gender = gender, race_type = race_type)
  log_comparison("Default", default_metrics, "Optimized", final_metrics)

  # Update global best score
  .optim_log$best_score <- final_metrics$composite_score

  # Log summary

  log_optimization_summary()

  return(list(
    best_params = best_params,
    final_metrics = final_metrics,
    coarse_results = coarse_results,
    random_results = random_results,
    all_results = all_results
  ))
}

#' Optimize all race types for a sport
#'
#' @param sport Sport name
#' @param gender "men" or "ladies"
#' @param verbose Print progress
#' @return List of results per race type
optimize_sport <- function(sport, gender = "men", verbose = TRUE) {

  race_types <- get_optimizable_race_types(sport, gender)

  log_info("")
  log_info("################################################################################")
  log_info(sprintf("#  SPORT OPTIMIZATION: %s (%s)", toupper(sport), gender))
  log_info(sprintf("#  Race Types: %d", length(race_types)))
  log_info(sprintf("#  Types: %s", paste(race_types, collapse = ", ")))
  log_info("################################################################################")

  results <- list()
  sport_start_time <- Sys.time()

  # First optimize overall (all race types combined)
  log_info("")
  log_info(">>> OPTIMIZING: DEFAULT (all race types combined) <<<")
  results[["default"]] <- optimize_params(sport, gender, race_type = NULL, verbose = verbose)

  # Then optimize each race type
  for (rt_idx in seq_along(race_types)) {
    rt <- race_types[rt_idx]

    log_info("")
    log_info(sprintf(">>> OPTIMIZING RACE TYPE %d/%d: %s <<<", rt_idx, length(race_types), rt))

    results[[rt]] <- tryCatch({
      optimize_params(sport, gender, race_type = rt, verbose = verbose)
    }, error = function(e) {
      log_optim_error(rt, e$message)
      list(best_params = DEFAULT_PARAMS, final_metrics = NULL)
    })
  }

  sport_duration <- as.numeric(difftime(Sys.time(), sport_start_time, units = "hours"))

  log_info("")
  log_info("################################################################################")
  log_info(sprintf("#  SPORT OPTIMIZATION COMPLETE: %s", toupper(sport)))
  log_info(sprintf("#  Total Duration: %.1f hours", sport_duration))
  log_info(sprintf("#  Race Types Optimized: %d", length(race_types) + 1))
  log_info("################################################################################")

  # Summary table of best params per race type
  log_info("")
  log_info("SUMMARY - Best Parameters by Race Type:")
  log_info(sprintf("  %-20s  %-8s  %-8s  %-6s  %-6s  %-10s",
                   "Race Type", "Decay", "SD_Scale", "SD_Min", "SD_Max", "Composite"))
  log_info(sprintf("  %-20s  %-8s  %-8s  %-6s  %-6s  %-10s",
                   "---------", "-----", "--------", "------", "------", "---------"))

  for (rt_name in names(results)) {
    if (!is.null(results[[rt_name]]$best_params)) {
      bp <- results[[rt_name]]$best_params
      fm <- results[[rt_name]]$final_metrics
      score <- if (!is.null(fm)) sprintf("%.4f", fm$composite_score) else "N/A"
      log_info(sprintf("  %-20s  %-8.4f  %-8.2f  %-6d  %-6d  %-10s",
                       rt_name, bp$decay_lambda, bp$sd_scale_factor,
                       as.integer(bp$sd_min), as.integer(bp$sd_max), score))
    }
  }

  return(results)
}

# =============================================================================
# RESULTS MANAGEMENT
# =============================================================================

#' Save optimization results
#'
#' @param results Results from optimize_sport or optimize_params
#' @param sport Sport name
#' @param gender Gender
#' @param filename Optional custom filename
save_results <- function(results, sport, gender, filename = NULL) {
  # Ensure results directory exists
  if (!dir.exists(RESULTS_DIR)) {
    dir.create(RESULTS_DIR, recursive = TRUE)
  }

  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- sprintf("optimization_%s_%s_%s.rds", sport, gender, timestamp)
  }

  filepath <- file.path(RESULTS_DIR, filename)
  saveRDS(results, filepath)
  cat(sprintf("Results saved to: %s\n", filepath))

  return(filepath)
}

#' Load optimization results
#'
#' @param filepath Path to saved results
#' @return Results list
load_results <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(paste("File not found:", filepath))
  }
  readRDS(filepath)
}

# =============================================================================
# SPORT_PARAMS.R GENERATION
# =============================================================================

#' Generate sport_params.R from optimization results
#'
#' @param all_sport_results Named list of results per sport
#' @param output_path Output file path
generate_sport_params_file <- function(all_sport_results,
                                        output_path = "~/blog/daehl-e/content/post/shared/sport_params.R") {
  if (length(all_sport_results) == 0) {
    stop("No valid optimization results available - refusing to overwrite sport_params.R")
  }
  output_path <- path.expand(output_path)

  # Build the params structure
  lines <- c(
    "# sport_params.R",
    "# Optimized simulation parameters by sport and race type",
    sprintf("# Generated: %s", Sys.time()),
    "#",
    "# Usage:",
    "#   source('~/blog/daehl-e/content/post/shared/sport_params.R')",
    "#   params <- get_sport_params('cross-country', 'Sprint_C')",
    "",
    "SPORT_PARAMS <- list(",
    ""
  )

  sport_names <- names(all_sport_results)

  for (s_idx in seq_along(sport_names)) {
    sport <- sport_names[s_idx]
    sport_results <- all_sport_results[[sport]]
    if (is.null(sport_results$default) || is.null(sport_results$default$best_params)) {
      stop(sprintf("Missing default best_params for sport '%s' - refusing to generate partial sport_params.R", sport))
    }

    lines <- c(lines, sprintf('  "%s" = list(', sport))

    # Default params
    default_params <- sport_results$default$best_params
    lines <- c(lines,
      "    default = list(",
      sprintf("      decay_lambda = %.6f,", default_params$decay_lambda),
      sprintf("      sd_scale_factor = %.4f,", default_params$sd_scale_factor),
      sprintf("      sd_min = %d,", as.integer(default_params$sd_min)),
      sprintf("      sd_max = %d,", as.integer(default_params$sd_max)),
      sprintf("      n_history_required = %d,", as.integer(default_params$n_history_required)),
      sprintf("      gam_fill_weight_factor = %.4f", default_params$gam_fill_weight_factor),
      "    ),"
    )

    # Race type params
    race_types <- setdiff(names(sport_results), "default")
    if (length(race_types) > 0) {
      lines <- c(lines, "    race_types = list(")

      for (rt_idx in seq_along(race_types)) {
        rt <- race_types[rt_idx]
        rt_params <- sport_results[[rt]]$best_params

        comma <- if (rt_idx < length(race_types)) "," else ""

        lines <- c(lines,
          sprintf('      "%s" = list(', rt),
          sprintf("        decay_lambda = %.6f,", rt_params$decay_lambda),
          sprintf("        sd_scale_factor = %.4f,", rt_params$sd_scale_factor),
          sprintf("        sd_min = %d,", as.integer(rt_params$sd_min)),
          sprintf("        sd_max = %d,", as.integer(rt_params$sd_max)),
          sprintf("        n_history_required = %d,", as.integer(rt_params$n_history_required)),
          sprintf("        gam_fill_weight_factor = %.4f", rt_params$gam_fill_weight_factor),
          sprintf("      )%s", comma)
        )
      }

      lines <- c(lines, "    )")
    }

    sport_comma <- if (s_idx < length(sport_names)) ")," else ")"
    lines <- c(lines, sprintf("  %s", sport_comma), "")
  }

  lines <- c(lines, ")", "")

  # Add helper function
  lines <- c(lines,
    "#' Get parameters for a sport/race-type combination",
    "#'",
    "#' @param sport Sport name",
    "#' @param race_type Optional race type (uses default if NULL)",
    "#' @param event_type Backward-compatible alias for race_type",
    "#' @return List of parameters",
    "get_sport_params <- function(sport, race_type = NULL, event_type = NULL) {",
    "  if (is.null(race_type) && !is.null(event_type)) {",
    "    race_type <- event_type",
    "  }",
    "  if (identical(sport, 'cross-country') && identical(race_type, 'Mixed_Relay')) {",
    "    race_type <- 'Relay'",
    "  }",
    "  sport_config <- SPORT_PARAMS[[sport]]",
    "  ",
    "  if (is.null(sport_config)) {",
    '    warning(paste("Unknown sport:", sport, "- using global defaults"))',
    "    return(list(",
    "      decay_lambda = 0.002,",
    "      sd_scale_factor = 0.77,",
    "      sd_min = 4,",
    "      sd_max = 16,",
    "      n_history_required = 10,",
    "      gam_fill_weight_factor = 0.25",
    "    ))",
    "  }",
    "  ",
    "  # Start with defaults",
    "  params <- sport_config$default",
    "  ",
    "  # Override with race-type specific if available",
    "  if (!is.null(race_type) && !is.null(sport_config$race_types[[race_type]])) {",
    "    race_params <- sport_config$race_types[[race_type]]",
    "    for (param in names(race_params)) {",
    "      params[[param]] <- race_params[[param]]",
    "    }",
    "  }",
    "  ",
    "  return(params)",
    "}",
    "",
    'cat("sport_params.R loaded successfully\\n")'
  )

  # Write file
  writeLines(lines, output_path)
  cat(sprintf("Generated: %s\n", output_path))

  return(output_path)
}

# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

#' Quick test of the optimization pipeline
#'
#' @param sport Sport to test
#' @param gender Gender
#' @return Test results
test_optimization <- function(sport = "cross-country", gender = "men") {
  cat("Testing optimization pipeline (sequential)...\n\n")

  # Run minimal optimization
  results <- run_grid_search(
    sport = sport,
    gender = gender,
    param_grid = list(
      decay_lambda = c(0.001, 0.002, 0.003),
      sd_scale_factor = c(0.7, 0.8),
      sd_min = c(4),
      sd_max = c(16),
      n_history_required = c(10),
      gam_fill_weight_factor = c(0.25)
    ),
    n_simulations = 100,
    max_races = 30,
    parallel = FALSE,
    verbose = TRUE
  )

  cat("\nTest complete!\n")
  return(results)
}

#' Quick test of sd_min/sd_max ranges with overlapping values
#'
#' Runs a small grid focusing on SD parameters to see if they differentiate
#' Uses overlapping ranges to test if sd_min and sd_max converge or separate
#' Should complete in ~5-10 minutes
#'
#' @param sport Sport to test
#' @param gender Gender
#' @return Results data frame with best sd_min/sd_max
test_sd_ranges <- function(sport = "cross-country", gender = "men") {
  cat("================================================================================\n")
  cat("  TESTING SD PARAMETER RANGES (OVERLAPPING)\n")
  cat("================================================================================\n")
  cat(sprintf("  Sport: %s, Gender: %s\n", sport, gender))
  cat("  Testing: sd_min = 12,16,20,24 | sd_max = 12,16,20,24\n")
  cat("  Same values in both - allows sd_min == sd_max (fixed SD)\n")
  cat("  Other params: fixed at reasonable defaults\n")
  cat("  Expected time: ~5-10 minutes\n")
  cat("================================================================================\n\n")

  results <- run_grid_search(
    sport = sport,
    gender = gender,
    param_grid = list(
      decay_lambda = c(0.002),
      sd_scale_factor = c(0.9),
      sd_min = c(12, 16, 20, 24),
      sd_max = c(12, 16, 20, 24),
      n_history_required = c(12),
      gam_fill_weight_factor = c(0.25)
    ),
    n_simulations = 100,
    max_races = 40,
    parallel = TRUE,
    verbose = TRUE
  )

  cat("\n================================================================================\n")
  cat("  RESULTS\n")
  cat("================================================================================\n")

  if (!is.null(results) && nrow(results) > 0) {
    # Show top 5 by composite score
    top5 <- head(results[order(results$composite_score), ], 5)
    cat("\nTop 5 sd_min/sd_max combinations:\n")
    print(top5[, c("sd_min", "sd_max", "composite_score", "brier_score")])

    best_min <- top5$sd_min[1]
    best_max <- top5$sd_max[1]
    gap <- best_max - best_min

    cat(sprintf("\nBest: sd_min = %d, sd_max = %d (gap = %d)\n", best_min, best_max, gap))

    if (gap <= 4) {
      cat("NOTE: Small gap suggests per-athlete SD differentiation may not help.\n")
      cat("      Consider switching to fixed_sd approach.\n")
    } else {
      cat("NOTE: Meaningful gap - per-athlete SD differentiation may be valuable.\n")
    }

    if (best_min == 22) {
      cat("WARNING: sd_min hit upper bound (22)\n")
    }
    if (best_max == 26) {
      cat("WARNING: sd_max hit upper bound (26)\n")
    }
  }

  return(results)
}

#' Quick test of parallel execution
#'
#' Runs a tiny grid search in parallel to verify worker setup
#' Should complete in ~1-2 minutes
#'
#' @param sport Sport to test
#' @param gender Gender
#' @return Results data frame
test_parallel <- function(sport = "cross-country", gender = "men") {
  cat("================================================================================\n")
  cat("  TESTING PARALLEL EXECUTION\n")
  cat("================================================================================\n")
  cat(sprintf("  Sport: %s, Gender: %s\n", sport, gender))
  cat(sprintf("  Cores: %d\n", N_CORES))
  cat("  Grid:  6 combinations (2x3), 50 sims, 20 races\n")
  cat("  Expected time: ~1-2 minutes\n")
  cat("================================================================================\n\n")

  start_time <- Sys.time()

  results <- tryCatch({
    run_grid_search(
      sport = sport,
      gender = gender,
      param_grid = list(
        decay_lambda = c(0.001, 0.002),
        sd_scale_factor = c(0.7, 0.8, 0.9),
        sd_min = c(4),
        sd_max = c(16),
        n_history_required = c(10),
        gam_fill_weight_factor = c(0.25)
      ),
      n_simulations = 50,
      max_races = 20,
      parallel = TRUE,
      verbose = TRUE
    )
  }, error = function(e) {
    cat(sprintf("\n\nERROR: %s\n", e$message))
    return(NULL)
  })

  duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  cat("\n================================================================================\n")
  if (!is.null(results) && nrow(results) > 0) {
    cat("  TEST PASSED!\n")
    cat(sprintf("  Duration: %.1f seconds\n", duration))
    cat(sprintf("  Results:  %d rows\n", nrow(results)))
    cat("  Top 3:\n")
    print(head(results[, c("rank", "composite_score", "decay_lambda", "sd_scale_factor")], 3))
  } else {
    cat("  TEST FAILED!\n")
    cat("  Check error messages above.\n")
  }
  cat("================================================================================\n")

  return(results)
}

#' Compare optimized vs default parameters
#'
#' @param sport Sport name
#' @param gender Gender
#' @param optimized_params Optimized parameter list
#' @param n_races Number of races for comparison
#' @return Comparison data frame
compare_to_default <- function(sport, gender, optimized_params, n_races = 50) {
  chrono <- load_chrono_data(sport, gender)
  races <- get_calibration_races(sport, chrono)

  if (nrow(races) > n_races) {
    races <- races[sample(nrow(races), n_races), ]
  }

  cat("Evaluating default parameters...\n")
  default_metrics <- evaluate_params(DEFAULT_PARAMS, races, chrono, n_simulations = 500,
                                     sport = sport, gender = gender, race_type = NULL)

  cat("Evaluating optimized parameters...\n")
  optimized_metrics <- evaluate_params(optimized_params, races, chrono, n_simulations = 500,
                                       sport = sport, gender = gender, race_type = NULL)

  comparison <- compare_metrics(default_metrics, optimized_metrics,
                                 name1 = "Default", name2 = "Optimized")

  cat("\nComparison:\n")
  print(comparison)

  improvement <- (default_metrics$composite_score - optimized_metrics$composite_score) /
                  default_metrics$composite_score * 100
  cat(sprintf("\nComposite score improvement: %.1f%%\n", improvement))

  return(list(
    default = default_metrics,
    optimized = optimized_metrics,
    comparison = comparison,
    improvement_pct = improvement
  ))
}

#' Run full optimization for all sports
#'
#' Optimizes parameters for all 5 sports and generates updated sport_params.R
#'
#' @param sports Vector of sports to optimize (default: all)
#' @param genders Vector of genders (default: c("men", "ladies"))
#' @param verbose Print progress
#' @return Named list of all results
run_full_optimization <- function(sports = c("cross-country", "biathlon", "alpine",
                                              "skijump", "nordic-combined"),
                                   genders = c("men", "ladies"),
                                   verbose = TRUE) {

  all_results <- list()
  start_time <- Sys.time()

  # Create master log file for full optimization
  if (!dir.exists(LOG_DIR)) {
    dir.create(LOG_DIR, recursive = TRUE)
  }
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  master_log_file <- file.path(LOG_DIR, sprintf("full_optimization_%s.log", timestamp))
  log_threshold(DEBUG)
  log_appender(appender_tee(master_log_file))

  log_info("")
  log_info("################################################################################")
  log_info("################################################################################")
  log_info("##")
  log_info("##  FULL PARAMETER OPTIMIZATION")
  log_info("##")
  log_info(sprintf("##  Started:  %s", format(start_time, "%Y-%m-%d %H:%M:%S")))
  log_info(sprintf("##  Sports:   %s", paste(sports, collapse = ", ")))
  log_info(sprintf("##  Genders:  %s", paste(genders, collapse = ", ")))
  log_info(sprintf("##  Log:      %s", basename(master_log_file)))
  log_info("##")
  log_info("################################################################################")
  log_info("################################################################################")
  log_info("")

  total_combinations <- length(sports) * length(genders)
  current_combo <- 0
  sport_timings <- list()

  for (sport in sports) {
    for (gender in genders) {
      current_combo <- current_combo + 1
      key <- paste(sport, gender, sep = "_")
      sport_start <- Sys.time()

      log_info("")
      log_info(sprintf(">>> [%d/%d] STARTING: %s %s <<<", current_combo, total_combinations, toupper(sport), gender))
      log_info("")

      tryCatch({
        results <- optimize_sport(sport, gender, verbose = verbose)
        all_results[[key]] <- results

        # Save intermediate results
        filepath <- save_results(results, sport, gender)
        log_info(sprintf("Results saved: %s", basename(filepath)))

        sport_duration <- as.numeric(difftime(Sys.time(), sport_start, units = "hours"))
        sport_timings[[key]] <- sport_duration

        log_info("")
        log_info(sprintf("<<< [%d/%d] COMPLETE: %s %s (%.1f hours) <<<",
                         current_combo, total_combinations, toupper(sport), gender, sport_duration))

      }, error = function(e) {
        log_optim_error(sprintf("%s %s", sport, gender), e$message)
        all_results[[key]] <- list(error = e$message)
        sport_timings[[key]] <- NA
      })

      # Estimate remaining time
      if (current_combo < total_combinations) {
        avg_time <- mean(unlist(sport_timings[!is.na(sport_timings)]), na.rm = TRUE)
        remaining <- (total_combinations - current_combo) * avg_time
        log_info(sprintf("Estimated remaining time: %.1f hours", remaining))
      }
    }
  }

  # Aggregate results by sport (combine genders by averaging)
  log_info("")
  log_info(">>> AGGREGATING RESULTS <<<")
  sport_results <- aggregate_gender_results(all_results, sports, genders)
  merged_sport_results <- merge_with_saved_sport_results(
    current_sport_results = sport_results,
    sports = SUPPORTED_SPORTS,
    preferred_genders = genders
  )

  output_path <- NULL
  if (length(merged_sport_results) > 0) {
    log_info("")
    log_info(">>> GENERATING sport_params.R <<<")
    output_path <- generate_sport_params_file(merged_sport_results)
    log_info(sprintf("Generated: %s", output_path))
  } else {
    log_warn("No valid sport results were produced; sport_params.R was not regenerated")
  }

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "hours"))

  # Final summary
  log_info("")
  log_info("################################################################################")
  log_info("################################################################################")
  log_info("##")
  log_info("##  FULL OPTIMIZATION COMPLETE")
  log_info("##")
  log_info(sprintf("##  Total Duration: %.1f hours", duration))
  log_info(sprintf("##  Combinations:   %d", length(all_results)))
  log_info("##")
  log_info("##  Timing Breakdown:")
  for (key in names(sport_timings)) {
    timing <- sport_timings[[key]]
    status <- if (is.na(timing)) "FAILED" else sprintf("%.1f hours", timing)
    log_info(sprintf("##    %-25s: %s", key, status))
  }
  log_info("##")
  log_info("##  Output Files:")
  log_info(sprintf("##    Parameters: %s", ifelse(is.null(output_path), "NOT GENERATED", output_path)))
  log_info(sprintf("##    Master Log: %s", master_log_file))
  log_info("##")
  log_info("################################################################################")
  log_info("################################################################################")

  return(all_results)
}

#' Aggregate results across genders (use best or average)
#'
#' @param all_results Results from run_full_optimization
#' @param sports Vector of sport names
#' @param genders Vector of gender names
#' @return Aggregated results by sport
average_optimization_entries <- function(entries) {
  valid_entries <- Filter(function(entry) {
    !is.null(entry) && !is.null(entry$best_params)
  }, entries)

  if (length(valid_entries) == 0) {
    return(NULL)
  }

  param_names <- names(valid_entries[[1]]$best_params)
  averaged_params <- list()

  for (param_name in param_names) {
    values <- vapply(valid_entries, function(entry) entry$best_params[[param_name]], numeric(1))
    averaged_value <- mean(values, na.rm = TRUE)
    if (param_name %in% c("sd_min", "sd_max", "n_history_required")) {
      averaged_value <- as.integer(round(averaged_value))
    }
    averaged_params[[param_name]] <- averaged_value
  }

  list(best_params = averaged_params, final_metrics = NULL)
}

average_optimization_results <- function(results) {
  valid_results <- Filter(is_valid_optimization_result, results)

  if (length(valid_results) == 0) {
    return(NULL)
  }

  result_names <- unique(unlist(lapply(valid_results, names)))
  aggregated_result <- list()

  for (result_name in result_names) {
    averaged_entry <- average_optimization_entries(lapply(valid_results, function(result) result[[result_name]]))
    if (!is.null(averaged_entry)) {
      aggregated_result[[result_name]] <- averaged_entry
    }
  }

  if (is.null(aggregated_result$default)) {
    return(NULL)
  }

  aggregated_result
}

aggregate_gender_results <- function(all_results, sports, genders) {
  sport_results <- list()

  for (sport in sports) {
    gender_results <- lapply(genders, function(gender) {
      key <- paste(sport, gender, sep = "_")
      result <- all_results[[key]]
      if (is_valid_optimization_result(result)) result else NULL
    })
    gender_results <- Filter(Negate(is.null), gender_results)

    if (length(gender_results) == 0) {
      next
    }
    aggregated_sport <- average_optimization_results(gender_results)
    if (!is.null(aggregated_sport$default)) {
      sport_results[[sport]] <- aggregated_sport
    }
  }

  return(sport_results)
}

#' Check whether an optimization result contains usable best parameters
#'
#' @param result Optimization result object
#' @return TRUE if valid
is_valid_optimization_result <- function(result) {
  !is.null(result) &&
    is.null(result$error) &&
    !is.null(result$default) &&
    !is.null(result$default$best_params)
}

#' Load previously saved optimization results
#'
#' @param sport Sport name
#' @param gender Gender
#' @param timestamp Optional specific timestamp (default: latest)
#' @return Loaded results or NULL
load_optimization_results <- function(sport, gender, timestamp = NULL) {
  if (!dir.exists(RESULTS_DIR)) {
    cat("No results directory found\n")
    return(NULL)
  }

  # Find matching files
  pattern <- sprintf("optimization_%s_%s_.*\\.rds", sport, gender)
  files <- list.files(RESULTS_DIR, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) {
    cat(sprintf("No results found for %s %s\n", sport, gender))
    return(NULL)
  }

  # Get latest or specific timestamp
  if (is.null(timestamp)) {
    # Sort by modification time, get latest
    file_info <- file.info(files)
    latest_file <- files[which.max(file_info$mtime)]
  } else {
    matches <- grep(timestamp, files, value = TRUE)
    if (length(matches) == 0) {
      cat(sprintf("No results found with timestamp %s\n", timestamp))
      return(NULL)
    }
    latest_file <- matches[1]
  }

  cat(sprintf("Loading: %s\n", basename(latest_file)))
  return(readRDS(latest_file))
}

#' Load the latest valid optimization result for a specific sport/gender
#'
#' @param sport Sport name
#' @param gender Gender
#' @return Optimization result or NULL
load_latest_valid_result_for_sport_gender <- function(sport, gender) {
  result <- tryCatch(load_optimization_results(sport, gender), error = function(e) NULL)
  if (is_valid_optimization_result(result)) {
    return(result)
  }
  NULL
}

#' Merge a single event optimization result into a saved sport result
#'
#' @param sport Sport name
#' @param gender Gender
#' @param race_type Race type, or NULL for default
#' @param event_result Result from optimize_params()
#' @return Updated sport result object
merge_event_into_sport_result <- function(sport, gender, race_type = NULL, event_result) {
  existing_result <- load_latest_valid_result_for_sport_gender(sport, gender)

  if (is.null(existing_result)) {
    if (!is.null(race_type)) {
      stop(sprintf("No existing saved result for %s %s. Run the default optimization first.", sport, gender))
    }
    existing_result <- list()
  }

  key <- if (is.null(race_type)) "default" else race_type
  existing_result[[key]] <- event_result

  if (is.null(existing_result$default) || is.null(existing_result$default$best_params)) {
    stop(sprintf("Updated result for %s %s is missing a valid default block.", sport, gender))
  }

  existing_result
}

#' Optimize a single event and update saved sport params
#'
#' @param sport Sport name
#' @param gender Gender
#' @param race_type Race type, or NULL for default/all-race sport block
#' @param verbose Print progress
#' @return List with event result, saved result path, and sport_params path
optimize_event_and_update_sport_params <- function(sport, gender, race_type = NULL, verbose = TRUE) {
  event_result <- optimize_params(sport, gender, race_type = race_type, verbose = verbose)
  updated_sport_result <- merge_event_into_sport_result(sport, gender, race_type, event_result)
  saved_result_path <- save_results(updated_sport_result, sport, gender)
  sport_params_path <- regenerate_sport_params()

  list(
    event_result = event_result,
    saved_result_path = saved_result_path,
    sport_params_path = sport_params_path
  )
}

#' Load the latest valid optimization result for a sport
#'
#' @param sport Sport name
#' @param preferred_genders Ordered vector of genders to try
#' @return Optimization result or NULL
load_latest_valid_result_for_sport <- function(sport, preferred_genders = c("men", "ladies")) {
  gender_results <- lapply(preferred_genders, function(gender) {
    tryCatch(load_optimization_results(sport, gender), error = function(e) NULL)
  })
  average_optimization_results(gender_results)
}

#' Merge current sport results with latest saved valid results on disk
#'
#' @param current_sport_results Named list of current in-memory sport results
#' @param sports Sports to include
#' @param preferred_genders Ordered vector of genders to try for saved results
#' @return Named list of merged sport results
merge_with_saved_sport_results <- function(current_sport_results,
                                           sports = SUPPORTED_SPORTS,
                                           preferred_genders = c("men", "ladies")) {
  merged_results <- list()

  for (sport in sports) {
    if (!is.null(current_sport_results[[sport]]) &&
        is_valid_optimization_result(current_sport_results[[sport]])) {
      merged_results[[sport]] <- current_sport_results[[sport]]
      next
    }

    saved_result <- load_latest_valid_result_for_sport(sport, preferred_genders)
    if (!is.null(saved_result)) {
      merged_results[[sport]] <- saved_result
    }
  }

  merged_results
}

#' List all saved optimization results
#'
#' @return Data frame of available results
list_optimization_results <- function() {
  if (!dir.exists(RESULTS_DIR)) {
    cat("No results directory found\n")
    return(NULL)
  }

  files <- list.files(RESULTS_DIR, pattern = "\\.rds$", full.names = TRUE)

  if (length(files) == 0) {
    cat("No results found\n")
    return(NULL)
  }

  file_info <- file.info(files)
  results_df <- data.frame(
    file = basename(files),
    size_kb = round(file_info$size / 1024, 1),
    modified = file_info$mtime,
    stringsAsFactors = FALSE
  )

  results_df <- results_df[order(results_df$modified, decreasing = TRUE), ]
  rownames(results_df) <- NULL

  return(results_df)
}

#' Generate sport_params.R from saved results files
#'
#' Loads the latest results for each sport and generates sport_params.R
#'
#' @param sports Sports to include
#' @param preferred_genders Ordered vector of genders to try
#' @return Path to generated file
regenerate_sport_params <- function(sports = SUPPORTED_SPORTS,
                                     preferred_genders = c("men", "ladies")) {
  sport_results <- merge_with_saved_sport_results(
    current_sport_results = list(),
    sports = sports,
    preferred_genders = preferred_genders
  )

  if (length(sport_results) == 0) {
    stop("No results loaded - cannot generate sport_params.R")
  }

  return(generate_sport_params_file(sport_results))
}

cat("param-optimizer.R loaded successfully\n")
