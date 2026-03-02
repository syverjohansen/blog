# Nordic Combined Championships Predictions - Monte Carlo Simulation
# Simulation-based approach for World Championships predictions
#
# Features:
# - Monte Carlo simulation (10,000 iterations)
# - Exponential decay weighting for historical performance
# - Race type distributions (Individual, IndividualCompact, Mass Start, Sprint)
# - Team event support: Team, Team Sprint, Mixed Team
# - 4-person quota constraint per nation per race
# - Position probability output (Win, Podium, Top-5, Top-10, Top-30)

library(dplyr)
library(tidyr)
library(openxlsx)
library(logger)
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
N_SIMULATIONS <- 10000
DECAY_LAMBDA <- 0.002
SD_SCALE_FACTOR <- 0.77
SD_MIN <- 4
SD_MAX <- 16

# Position thresholds
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)

# Points system for Nordic Combined
nc_points <- c(100, 90, 80, 70, 60, 55, 52, 49, 46, 43, 40, 38, 36, 34, 32,
               30, 28, 26, 24, 22, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11,
               10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Race type definitions
INDIVIDUAL_DISCIPLINES <- c("Individual", "IndividualCompact")
MASS_START_DISCIPLINES <- c("Mass Start")
SPRINT_DISCIPLINES <- c("Sprint")
TEAM_DISCIPLINES <- c("Team", "Team Sprint", "Mixed Team")

# ============================================================================
# LOGGING SETUP
# ============================================================================

log_dir <- "~/ski/elo/python/nordic-combined/polars/excel365/champs-predictions-simulation"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "simulation.log")))
log_info("=== NORDIC COMBINED CHAMPS-PREDICTIONS-SIMULATION.R STARTED ===")
log_info(paste("TEST_MODE:", TEST_MODE))

# ============================================================================
# DATA LOADING
# ============================================================================

log_info("Loading data files...")

base_path <- "~/ski/elo/python/nordic-combined/polars/excel365"

# Load weekends data to find championship races
weekends <- tryCatch({
  df <- read.csv(file.path(base_path, "weekends.csv"), stringsAsFactors = FALSE)
  if (nrow(df) == 0) {
    log_error("Weekends file is empty")
    quit(save = "no", status = 1)
  }
  df %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),
      Race_Date = as.Date(Race_Date, format = "%m/%d/%Y")
    )
}, error = function(e) {
  log_error(paste("Failed to load weekends.csv:", e$message))
  quit(save = "no", status = 1)
})

# Filter for Championships races only
log_info("Filtering for Championships races")
champs_races <- weekends %>%
  filter(Championship == 1)

if (nrow(champs_races) == 0) {
  log_info("No Championships races found. Terminating program.")
  quit(save = "no", status = 0)
}

log_info(paste("Found", nrow(champs_races), "Championships races"))

# Create race dataframes with chronological ordering
champs_races_with_race_num <- champs_races %>%
  arrange(Race_Date) %>%
  mutate(OriginalRaceNum = row_number())

men_races <- champs_races_with_race_num %>%
  filter(Sex == "M", !grepl("Team", RaceType, ignore.case = TRUE)) %>%
  arrange(Race_Date) %>%
  dplyr::select(RaceType, Period, Country, Race_Date, OriginalRaceNum) %>%
  rename(race_type = RaceType, period = Period, country = Country,
         race_date = Race_Date, original_race_num = OriginalRaceNum)

ladies_races <- champs_races_with_race_num %>%
  filter(Sex == "L", !grepl("Team", RaceType, ignore.case = TRUE)) %>%
  arrange(Race_Date) %>%
  dplyr::select(RaceType, Period, Country, Race_Date, OriginalRaceNum) %>%
  rename(race_type = RaceType, period = Period, country = Country,
         race_date = Race_Date, original_race_num = OriginalRaceNum)

log_info(paste("Found", nrow(men_races), "men's races,", nrow(ladies_races), "ladies' races"))

# Get current date
current_date <- Sys.Date()
log_info(paste("Current date:", current_date))

# Load chronological data
men_chrono <- tryCatch({
  df <- read.csv(file.path(base_path, "men_chrono_elevation.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded men_chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_error(paste("Failed to load men_chrono:", e$message))
  data.frame()
})

ladies_chrono <- tryCatch({
  df <- read.csv(file.path(base_path, "ladies_chrono_elevation.csv"), stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  log_info(paste("Loaded ladies_chrono:", nrow(df), "rows"))
  df
}, error = function(e) {
  log_error(paste("Failed to load ladies_chrono:", e$message))
  data.frame()
})

# Load championships startlists
men_startlist <- tryCatch({
  df <- read.csv(file.path(base_path, "startlist_champs_men.csv"), stringsAsFactors = FALSE)
  required_cols <- c("ID", "Skier", "Nation")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    log_warn(paste("Men's startlist missing columns:", paste(missing_cols, collapse = ", ")))
  }
  df
}, error = function(e) {
  log_warn(paste("Could not load men's championships startlist:", e$message))
  data.frame()
})

ladies_startlist <- tryCatch({
  df <- read.csv(file.path(base_path, "startlist_champs_ladies.csv"), stringsAsFactors = FALSE)
  required_cols <- c("ID", "Skier", "Nation")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    log_warn(paste("Ladies' startlist missing columns:", paste(missing_cols, collapse = ", ")))
  }
  df
}, error = function(e) {
  log_warn(paste("Could not load ladies' championships startlist:", e$message))
  data.frame()
})

log_info(paste("Loaded champs startlists - Men:", nrow(men_startlist), "| Ladies:", nrow(ladies_startlist)))

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

get_points <- function(place, race_type = "Individual") {
  if (is.na(place) || place < 1 || place > length(nc_points)) {
    return(0)
  }
  return(nc_points[place])
}

get_weighted_prev_points <- function(chrono_data, athlete_id, race_type, reference_date) {
  if (race_type %in% INDIVIDUAL_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% INDIVIDUAL_DISCIPLINES)
  } else if (race_type %in% MASS_START_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% MASS_START_DISCIPLINES)
  } else if (race_type %in% SPRINT_DISCIPLINES) {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType %in% SPRINT_DISCIPLINES)
  } else {
    races <- chrono_data %>%
      filter(ID == athlete_id, RaceType == race_type)
  }

  if (nrow(races) == 0) {
    races <- chrono_data %>% filter(ID == athlete_id)
  }

  if (nrow(races) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
  }

  if (!"Points" %in% names(races)) {
    races$Points <- sapply(races$Place, function(p) get_points(p, race_type))
  }

  races <- races %>%
    mutate(
      days_ago = as.numeric(reference_date - Date),
      weight = exp(-DECAY_LAMBDA * pmax(0, days_ago))
    ) %>%
    filter(days_ago >= 0, !is.na(Points))

  if (nrow(races) == 0) {
    return(list(mean = NA, sd = NA, n = 0))
  }

  weighted_mean <- sum(races$Points * races$weight) / sum(races$weight)

  if (nrow(races) >= 2) {
    weighted_var <- sum(races$weight * (races$Points - weighted_mean)^2) / sum(races$weight)
    weighted_sd <- sqrt(weighted_var)
  } else {
    weighted_sd <- SD_MAX / 2
  }

  return(list(
    mean = weighted_mean,
    sd = weighted_sd,
    n = nrow(races)
  ))
}

build_athlete_distribution <- function(athlete_id, race_type, chrono_data,
                                       reference_date = NULL) {
  if (is.null(reference_date)) {
    reference_date <- Sys.Date()
  }

  hist_stats <- get_weighted_prev_points(chrono_data, athlete_id, race_type, reference_date)

  if (!is.na(hist_stats$mean) && hist_stats$n >= 3) {
    mean_points <- hist_stats$mean
    sd_points <- hist_stats$sd
  } else if (!is.na(hist_stats$mean)) {
    mean_points <- hist_stats$mean
    sd_points <- if (!is.na(hist_stats$sd)) hist_stats$sd else SD_MAX / 2
  } else {
    mean_points <- 5
    sd_points <- SD_MAX
  }

  sd_points <- pmax(SD_MIN, pmin(SD_MAX, sd_points))

  return(list(
    athlete_id = athlete_id,
    mean = mean_points,
    sd = sd_points,
    n_actual_races = hist_stats$n
  ))
}

simulate_race_positions <- function(athlete_distributions, n_simulations = N_SIMULATIONS,
                                    position_thresholds = POSITION_THRESHOLDS,
                                    max_points = 100) {
  valid_distributions <- Filter(function(dist) {
    !is.null(dist) && !is.null(dist$athlete_id) && !is.na(dist$athlete_id) &&
    !is.null(dist$mean) && !is.na(dist$mean) &&
    !is.null(dist$sd) && !is.na(dist$sd)
  }, athlete_distributions)

  if (length(valid_distributions) == 0) {
    log_error("No valid athlete distributions to simulate")
    return(data.frame())
  }

  n_athletes <- length(valid_distributions)
  athlete_ids <- sapply(valid_distributions, function(x) x$athlete_id)
  means <- sapply(valid_distributions, function(x) x$mean)
  sds <- sapply(valid_distributions, function(x) x$sd)

  scaled_sds <- pmax(SD_MIN, pmin(SD_MAX, sds * SD_SCALE_FACTOR))

  all_sims <- matrix(rnorm(n_athletes * n_simulations),
                     nrow = n_athletes, ncol = n_simulations)
  all_sims <- all_sims * scaled_sds + means
  all_sims <- pmax(0, pmin(max_points, all_sims))

  ranks_matrix <- apply(all_sims, 2, function(x) rank(-x, ties.method = "random"))

  position_counts <- matrix(0, nrow = n_athletes, ncol = length(position_thresholds))
  for (t_idx in seq_along(position_thresholds)) {
    threshold <- position_thresholds[t_idx]
    position_counts[, t_idx] <- rowSums(ranks_matrix <= threshold)
  }

  position_probs <- position_counts / n_simulations

  results <- data.frame(
    athlete_id = athlete_ids,
    mean_points = means,
    sd_points = sds,
    n_actual_races = sapply(valid_distributions, function(x) x$n_actual_races),
    stringsAsFactors = FALSE
  )

  for (t_idx in seq_along(position_thresholds)) {
    col_name <- paste0("prob_top_", position_thresholds[t_idx])
    results[[col_name]] <- position_probs[, t_idx]
  }

  results <- results %>% arrange(desc(mean_points))
  return(results)
}

get_base_race_probability <- function(chrono_data, participant, race_type) {
  five_years_ago <- Sys.Date() - (5 * 365)

  athlete_first_race <- chrono_data %>%
    filter(Skier == participant) %>%
    summarise(first_date = min(Date, na.rm = TRUE)) %>%
    pull(first_date)

  if (is.na(athlete_first_race) || length(athlete_first_race) == 0) {
    cutoff_date <- five_years_ago
  } else {
    cutoff_date <- max(five_years_ago, athlete_first_race)
  }

  all_type_races <- chrono_data %>%
    filter(Date >= cutoff_date, RaceType == race_type) %>%
    distinct(Date, City) %>%
    arrange(Date)

  participant_type_races <- chrono_data %>%
    filter(Date >= cutoff_date, Skier == participant, RaceType == race_type) %>%
    distinct(Date, City)

  n_races <- nrow(all_type_races)
  if (n_races == 0) return(0)

  participation <- sapply(1:n_races, function(i) {
    race_date <- all_type_races$Date[i]
    race_city <- all_type_races$City[i]
    as.numeric(any(participant_type_races$Date == race_date &
                   participant_type_races$City == race_city))
  })

  race_weights <- exp(-0.1 * ((n_races - 1):0))

  weighted_participation <- sum(participation * race_weights)
  total_weight <- sum(race_weights)
  prob <- weighted_participation / total_weight

  return(prob)
}

apply_quota_constraint <- function(startlist, race_prob_col, target_per_nation = 4) {
  for (nation in unique(startlist$Nation)) {
    nation_mask <- startlist$Nation == nation
    nation_probs <- startlist[nation_mask, race_prob_col]
    current_sum <- sum(nation_probs, na.rm = TRUE)

    if (current_sum > 0) {
      scaling_factor <- target_per_nation / current_sum
      scaled_probs <- nation_probs * scaling_factor
      scaled_probs <- pmin(scaled_probs, 1.0)
      startlist[nation_mask, race_prob_col] <- scaled_probs
    }
  }
  return(startlist)
}

# ============================================================================
# CHAMPIONSHIPS SIMULATION
# ============================================================================

log_info("=== CHAMPIONSHIPS SIMULATION ===")

process_gender_championships <- function(gender, races) {
  log_info(paste("Processing", gender, "Championships with", nrow(races), "races"))

  startlist <- if (gender == "men") men_startlist else ladies_startlist
  chrono_data <- if (gender == "men") men_chrono else ladies_chrono

  if (nrow(startlist) == 0) {
    log_warn(paste("No", gender, "startlist available"))
    return(NULL)
  }

  # Calculate race probabilities
  for (i in 1:nrow(races)) {
    race_prob_col <- paste0("Race", i, "_Prob")
    race_type <- races$race_type[i]

    startlist[[race_prob_col]] <- sapply(startlist$Skier, function(skier) {
      get_base_race_probability(chrono_data, skier, race_type)
    })

    startlist <- apply_quota_constraint(startlist, race_prob_col, target_per_nation = 4)
  }

  # Process each race
  race_results <- list()
  position_predictions <- list()

  for (i in 1:nrow(races)) {
    race_info <- races[i, ]
    race_type <- race_info$race_type

    log_info(sprintf("Processing %s race %d: %s", gender, i, race_type))

    race_prob_col <- paste0("Race", i, "_Prob")

    race_startlist <- startlist %>%
      filter(!is.na(ID), !is.na(Skier), get(race_prob_col) > 0)

    if (nrow(race_startlist) == 0) {
      log_warn(paste("No athletes in startlist for", gender, race_type))
      next
    }

    log_info(paste("Athletes in startlist:", nrow(race_startlist)))

    athlete_distributions <- list()

    for (j in 1:nrow(race_startlist)) {
      athlete_id <- race_startlist$ID[j]

      dist <- build_athlete_distribution(
        athlete_id = athlete_id,
        race_type = race_type,
        chrono_data = chrono_data,
        reference_date = current_date
      )

      athlete_distributions[[as.character(athlete_id)]] <- dist
    }

    log_info(paste("Running", N_SIMULATIONS, "Monte Carlo simulations"))
    sim_results <- simulate_race_positions(athlete_distributions)

    sim_results <- sim_results %>%
      left_join(
        race_startlist %>% select(ID, Skier, Nation, all_of(race_prob_col)) %>% distinct(),
        by = c("athlete_id" = "ID")
      ) %>%
      rename(ID = athlete_id, Start = !!race_prob_col)

    sim_results$Sex <- ifelse(gender == "men", "M", "L")
    sim_results$Race <- race_info$original_race_num

    position_predictions[[as.character(race_info$original_race_num)]] <- sim_results
  }

  all_position_predictions <- bind_rows(position_predictions)

  # Create output directory
  champs_year <- format(Sys.Date(), "%Y")
  output_dir <- paste0("~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions/", champs_year)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Create race sheets
  race_sheets <- list()
  unique_races <- sort(unique(all_position_predictions$Race))

  race_order <- 0
  for (race_num in unique_races) {
    race_order <- race_order + 1
    race_data <- all_position_predictions[all_position_predictions$Race == race_num, ]

    race_data <- race_data %>%
      mutate(
        Start = round(Start * 100, 1),
        Win = round(prob_top_1 * 100, 1),
        Podium = round(prob_top_3 * 100, 1),
        `Top-5` = round(prob_top_5 * 100, 1),
        `Top-10` = round(prob_top_10 * 100, 1),
        `Top-30` = round(prob_top_30 * 100, 1)
      ) %>%
      dplyr::select(Skier, ID, Sex, Nation, Start, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
      arrange(desc(Win))

    race_info <- champs_races_with_race_num %>%
      filter(Sex == ifelse(gender == "men", "M", "L"), OriginalRaceNum == race_num)

    race_type <- if (nrow(race_info) > 0) race_info$RaceType[1] else paste("Race", race_num)
    race_date <- if (nrow(race_info) > 0) format(race_info$Race_Date[1], "%b %d") else ""

    sheet_name <- paste0(race_order, ". ", race_type, " - ", race_date)

    log_info(paste("Creating sheet:", sheet_name, "with", nrow(race_data), "athletes"))

    race_sheets[[sheet_name]] <- race_data
  }

  # Save position probabilities
  if (length(race_sheets) > 0) {
    race_file <- file.path(output_dir, paste0(gender, "_position_probabilities.xlsx"))
    write.xlsx(race_sheets, race_file)
    log_info(paste("Saved", gender, "race probabilities to", race_file))
  }

  # Create athlete summary
  athlete_summary <- all_position_predictions %>%
    group_by(Skier, Nation) %>%
    summarise(
      `Avg Win` = round(mean(prob_top_1, na.rm = TRUE) * 100, 1),
      `Avg Podium` = round(mean(prob_top_3, na.rm = TRUE) * 100, 1),
      `Avg Top-5` = round(mean(prob_top_5, na.rm = TRUE) * 100, 1),
      `Avg Top-10` = round(mean(prob_top_10, na.rm = TRUE) * 100, 1),
      Races = n(),
      .groups = 'drop'
    ) %>%
    arrange(desc(`Avg Win`))

  summary_file <- file.path(output_dir, paste0(gender, ".xlsx"))
  write.xlsx(athlete_summary, summary_file)
  log_info(paste("Saved", gender, "summary to", summary_file))

  return(list(
    summary = athlete_summary,
    race_results = all_position_predictions,
    race_sheets = race_sheets
  ))
}

# Process both genders
men_results <- NULL
if (nrow(men_races) > 0) {
  men_results <- process_gender_championships("men", men_races)
}

ladies_results <- NULL
if (nrow(ladies_races) > 0) {
  ladies_results <- process_gender_championships("ladies", ladies_races)
}

# ============================================================================
# CREATE NATIONS EXCEL FILE
# ============================================================================

log_info("=== Creating Nations Excel File ===")

if (!is.null(men_results) || !is.null(ladies_results)) {

  output_dir <- paste0("~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions/", format(Sys.Date(), "%Y"))

  men_individual_results <- data.frame()
  if (!is.null(men_results) && !is.null(men_results$race_sheets)) {
    for (race_name in names(men_results$race_sheets)) {
      race_data <- men_results$race_sheets[[race_name]]
      race_type_only <- sub("^\\d+\\. ", "", race_name)
      race_type_only <- sub(" - .*$", "", race_type_only)
      race_data$Race <- race_type_only
      race_data$Gender <- "Men"
      men_individual_results <- bind_rows(men_individual_results, race_data)
    }
  }

  ladies_individual_results <- data.frame()
  if (!is.null(ladies_results) && !is.null(ladies_results$race_sheets)) {
    for (race_name in names(ladies_results$race_sheets)) {
      race_data <- ladies_results$race_sheets[[race_name]]
      race_type_only <- sub("^\\d+\\. ", "", race_name)
      race_type_only <- sub(" - .*$", "", race_type_only)
      race_data$Race <- race_type_only
      race_data$Gender <- "Ladies"
      ladies_individual_results <- bind_rows(ladies_individual_results, race_data)
    }
  }

  log_info(paste("Combined", nrow(men_individual_results), "men's rows"))
  log_info(paste("Combined", nrow(ladies_individual_results), "ladies' rows"))

  men_nation_counts <- men_individual_results %>%
    filter(Start > 0) %>%
    group_by(Nation) %>%
    summarise(n_athletes = n_distinct(Skier), .groups = "drop")

  ladies_nation_counts <- ladies_individual_results %>%
    filter(Start > 0) %>%
    group_by(Nation) %>%
    summarise(n_athletes = n_distinct(Skier), .groups = "drop")

  men_main_nations <- men_nation_counts %>% filter(n_athletes >= 4) %>% pull(Nation) %>% sort()
  ladies_main_nations <- ladies_nation_counts %>% filter(n_athletes >= 4) %>% pull(Nation) %>% sort()

  men_other_nations <- men_nation_counts %>% filter(n_athletes < 4) %>% pull(Nation)
  ladies_other_nations <- ladies_nation_counts %>% filter(n_athletes < 4) %>% pull(Nation)

  select_and_rename_cols <- function(df, include_nation = FALSE) {
    if (include_nation) {
      df %>%
        select(Athlete = Skier, ID, Race, Nation, Start, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
        arrange(Race, Nation, desc(Start))
    } else {
      df %>%
        select(Athlete = Skier, ID, Race, Start, Win, Podium, `Top-5`, `Top-10`, `Top-30`) %>%
        arrange(Race, desc(Start))
    }
  }

  nations_wb <- list()

  for (nation in men_main_nations) {
    nation_data <- men_individual_results %>%
      filter(Nation == nation, Start > 0)

    if (nrow(nation_data) > 0) {
      sheet_name <- paste(nation, "Men")
      nations_wb[[sheet_name]] <- select_and_rename_cols(nation_data, include_nation = FALSE)
    }
  }

  men_other_data <- men_individual_results %>%
    filter(Nation %in% men_other_nations, Start > 0)

  if (nrow(men_other_data) > 0) {
    nations_wb[["Other Men"]] <- select_and_rename_cols(men_other_data, include_nation = TRUE)
  }

  for (nation in ladies_main_nations) {
    nation_data <- ladies_individual_results %>%
      filter(Nation == nation, Start > 0)

    if (nrow(nation_data) > 0) {
      sheet_name <- paste(nation, "Ladies")
      nations_wb[[sheet_name]] <- select_and_rename_cols(nation_data, include_nation = FALSE)
    }
  }

  ladies_other_data <- ladies_individual_results %>%
    filter(Nation %in% ladies_other_nations, Start > 0)

  if (nrow(ladies_other_data) > 0) {
    nations_wb[["Other Ladies"]] <- select_and_rename_cols(ladies_other_data, include_nation = TRUE)
  }

  all_individual_results <- bind_rows(men_individual_results, ladies_individual_results)

  summary_data <- all_individual_results %>%
    filter(Start > 0) %>%
    mutate(
      Nation_Group = case_when(
        Gender == "Men" & Nation %in% men_main_nations ~ Nation,
        Gender == "Ladies" & Nation %in% ladies_main_nations ~ Nation,
        TRUE ~ "Other"
      )
    ) %>%
    group_by(Gender, Nation_Group) %>%
    summarise(
      `Total Win` = round(sum(Win, na.rm = TRUE) / 100, 2),
      `Total Podium` = round(sum(Podium, na.rm = TRUE) / 100, 2),
      `Total Top-10` = round(sum(`Top-10`, na.rm = TRUE) / 100, 2),
      Athletes = n_distinct(Skier),
      .groups = "drop"
    ) %>%
    rename(Nation = Nation_Group) %>%
    mutate(sort_order = ifelse(Nation == "Other", 2, 1)) %>%
    arrange(Gender, sort_order, Nation) %>%
    select(-sort_order)

  nations_wb[["Summary"]] <- summary_data

  if (length(nations_wb) > 0) {
    nations_file <- file.path(output_dir, "nations_individual.xlsx")
    write.xlsx(nations_wb, nations_file)
    log_info(paste("Saved nations individual results to", nations_file))
  }
}

# ============================================================================
# SUMMARY OUTPUT
# ============================================================================

cat("\n=== NORDIC COMBINED CHAMPIONSHIPS SIMULATION COMPLETE ===\n")
cat(paste("Year:", format(Sys.Date(), "%Y"), "\n"))

if (!is.null(men_results)) {
  cat(paste("Men's races processed:", length(men_results$race_sheets), "\n"))
}

if (!is.null(ladies_results)) {
  cat(paste("Ladies' races processed:", length(ladies_results$race_sheets), "\n"))
}

cat(paste("\nOutput directory:", paste0("~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions/", format(Sys.Date(), "%Y")), "\n"))

log_info("=== NORDIC COMBINED CHAMPS-PREDICTIONS-SIMULATION.R COMPLETE ===")
