# param-grid.R
# Parameter ranges and race type mappings for optimization

# =============================================================================
# PARAMETER RANGES
# =============================================================================

#' Default parameter grid for optimization
#' Each parameter has a range of values to search
DEFAULT_PARAM_GRID <- list(
  decay_lambda = seq(0.0005, 0.005, by = 0.0005),    # 10 values
  sd_scale_factor = seq(0.50, 1.00, by = 0.05),      # 11 values
  sd_min = seq(10, 24, by = 2),                        # 8 values
  sd_max = seq(16, 30, by = 2),                        # 8 values
  n_history_required = seq(5, 20, by = 5),            # 4 values
  gam_fill_weight_factor = seq(0.10, 0.50, by = 0.10) # 5 values
)

#' Coarse grid for initial search (fewer values for speed)
COARSE_PARAM_GRID <- list(
  decay_lambda = c(0.001, 0.002, 0.003, 0.004),       # 4 values
  sd_scale_factor = c(0.60, 0.70, 0.80, 0.90),        # 4 values
  sd_min = c(12, 18, 24),                              # 3 values
  sd_max = c(18, 24, 30),                              # 3 values
  n_history_required = c(8, 12, 16),                   # 3 values
  gam_fill_weight_factor = c(0.15, 0.25, 0.35)        # 3 values
)

#' Fine grid for refinement (narrow ranges around best)
create_fine_grid <- function(best_params, margin = 0.2) {
  list(
    decay_lambda = seq(
      best_params$decay_lambda * (1 - margin),
      best_params$decay_lambda * (1 + margin),
      length.out = 5
    ),
    sd_scale_factor = seq(
      max(0.5, best_params$sd_scale_factor - 0.1),
      min(1.0, best_params$sd_scale_factor + 0.1),
      length.out = 5
    ),
    sd_min = seq(
      max(10, best_params$sd_min - 2),
      min(24, best_params$sd_min + 2),
      length.out = 3
    ),
    sd_max = seq(
      max(16, best_params$sd_max - 2),
      min(30, best_params$sd_max + 2),
      length.out = 3
    ),
    n_history_required = c(
      max(5, best_params$n_history_required - 2),
      best_params$n_history_required,
      min(20, best_params$n_history_required + 2)
    ),
    gam_fill_weight_factor = seq(
      max(0.1, best_params$gam_fill_weight_factor - 0.05),
      min(0.5, best_params$gam_fill_weight_factor + 0.05),
      length.out = 3
    )
  )
}

#' Team event parameter grid
TEAM_PARAM_GRID <- list(
  team_sd_scale_factor = seq(0.60, 1.00, by = 0.10),  # 5 values
  team_sd_min = seq(2, 6, by = 1),                     # 5 values
  team_sd_max = seq(8, 16, by = 2)                     # 5 values
)

# =============================================================================
# DEFAULT PARAMETERS (Current production values)
# =============================================================================

DEFAULT_PARAMS <- list(
  decay_lambda = 0.002,
  sd_scale_factor = 0.9,
  sd_min = 16,
  sd_max = 24,
  n_history_required = 10,
  gam_fill_weight_factor = 0.25
)

DEFAULT_TEAM_PARAMS <- list(
  team_sd_scale_factor = 0.80,
  team_sd_min = 3,
  team_sd_max = 12
)

# =============================================================================
# RACE TYPE MAPPINGS BY SPORT
# =============================================================================

#' Cross-Country race types and their identifiers
CROSS_COUNTRY_RACE_TYPES <- list(
  # Individual events
  Sprint_C = list(
    name = "Sprint Classic",
    filter = function(d) d$Distance == "Sprint" & d$Technique == "C"
  ),
  Sprint_F = list(
    name = "Sprint Freestyle",
    filter = function(d) d$Distance == "Sprint" & d$Technique == "F"
  ),
  Distance_C_Ind = list(
    name = "Distance Classic Individual",
    filter = function(d) !d$Distance %in% c("Sprint", "Rel", "Ts") &
                         d$Technique == "C" & (is.na(d$MS) | d$MS == 0)
  ),
  Distance_C_Ms = list(
    name = "Distance Classic Mass Start",
    filter = function(d) !d$Distance %in% c("Sprint", "Rel", "Ts") &
                         d$Technique == "C" & d$MS == 1
  ),
  Distance_F_Ind = list(
    name = "Distance Freestyle Individual",
    filter = function(d) !d$Distance %in% c("Sprint", "Rel", "Ts") &
                         d$Technique == "F" & (is.na(d$MS) | d$MS == 0)
  ),
  Distance_F_Ms = list(
    name = "Distance Freestyle Mass Start",
    filter = function(d) !d$Distance %in% c("Sprint", "Rel", "Ts") &
                         d$Technique == "F" & d$MS == 1
  ),
  # Combined mass start - includes all techniques (C, F, P/skiathlon)
  # Used to predict skiathlons since they're distance mass start races
  Distance_Ms = list(
    name = "Distance Mass Start (All)",
    filter = function(d) !d$Distance %in% c("Sprint", "Rel", "Ts") & d$MS == 1
  ),
  # Team events
  Relay = list(
    name = "Relay",
    filter = function(d) d$Distance == "Rel",
    is_team = TRUE
  ),
  Team_Sprint = list(
    name = "Team Sprint",
    filter = function(d) d$Distance == "Ts",
    is_team = TRUE
  )
)

#' Alpine race types
ALPINE_RACE_TYPES <- list(
  Downhill = list(
    name = "Downhill",
    filter = function(d) d$Distance == "Downhill"
  ),
  Super_G = list(
    name = "Super G",
    filter = function(d) d$Distance == "Super G"
  ),
  Giant_Slalom = list(
    name = "Giant Slalom",
    filter = function(d) d$Distance == "Giant Slalom"
  ),
  Slalom = list(
    name = "Slalom",
    filter = function(d) d$Distance == "Slalom"
  ),
  Combined = list(
    name = "Combined",
    filter = function(d) d$Distance == "Combined" | d$Distance == "Alpine Combined"
  )
)

#' Biathlon race types
BIATHLON_RACE_TYPES <- list(
  Sprint = list(
    name = "Sprint",
    filter = function(d) d$RaceType == "Sprint" | d$Distance == 10 | d$Distance == 7.5
  ),
  Individual = list(
    name = "Individual",
    filter = function(d) d$RaceType == "Individual" | d$Distance == 20 | d$Distance == 15
  ),
  Pursuit = list(
    name = "Pursuit",
    filter = function(d) d$RaceType == "Pursuit" | d$Distance == 12.5 | d$Distance == 10
  ),
  Mass_Start = list(
    name = "Mass Start",
    filter = function(d) d$RaceType == "Mass Start" | d$Distance == 15 | d$Distance == 12.5
  ),
  # Team events
  Relay = list(
    name = "Relay",
    filter = function(d) d$RaceType == "Relay",
    is_team = TRUE
  ),
  Mixed_Relay = list(
    name = "Mixed Relay",
    filter = function(d) d$RaceType == "Mixed Relay",
    is_team = TRUE
  ),
  Single_Mixed_Relay = list(
    name = "Single Mixed Relay",
    filter = function(d) d$RaceType == "Single Mixed Relay",
    is_team = TRUE
  )
)

#' Nordic Combined race types
NORDIC_COMBINED_RACE_TYPES <- list(
  Individual = list(
    name = "Individual",
    filter = function(d) d$Distance == "Individual" | d$RaceType == "Individual"
  ),
  Individual_Compact = list(
    name = "Individual Compact",
    filter = function(d) d$Distance == "IndividualCompact" | d$RaceType == "Compact"
  ),
  Mass_Start = list(
    name = "Mass Start",
    filter = function(d) d$Distance == "Mass Start" | d$RaceType == "Mass Start"
  ),
  Sprint = list(
    name = "Sprint",
    filter = function(d) d$Distance == "Sprint" | d$RaceType == "Sprint"
  ),
  # Team events
  Team = list(
    name = "Team",
    filter = function(d) d$Distance == "Team" | d$RaceType == "Team",
    is_team = TRUE
  ),
  Team_Sprint = list(
    name = "Team Sprint",
    filter = function(d) d$Distance == "Team Sprint" | d$RaceType == "Team Sprint",
    is_team = TRUE
  )
)

#' Ski Jumping race types
SKI_JUMPING_RACE_TYPES <- list(
  Large_Hill = list(
    name = "Large Hill",
    filter = function(d) d$Hill == "Large" | (d$HillSize >= 120 & d$HillSize < 185)
  ),
  Normal_Hill = list(
    name = "Normal Hill",
    filter = function(d) d$Hill == "Normal" | (d$HillSize >= 85 & d$HillSize < 120)
  ),
  Flying_Hill = list(
    name = "Flying Hill",
    filter = function(d) d$Hill == "Flying" | d$HillSize >= 185
  ),
  # Team events
  Team_Large = list(
    name = "Team Large Hill",
    filter = function(d) (d$Hill == "Large" | d$HillSize >= 120) & d$Event == "Team",
    is_team = TRUE
  ),
  Team_Normal = list(
    name = "Team Normal Hill",
    filter = function(d) (d$Hill == "Normal" | d$HillSize < 120) & d$Event == "Team",
    is_team = TRUE
  ),
  Mixed_Team = list(
    name = "Mixed Team",
    filter = function(d) d$Event == "Mixed Team",
    is_team = TRUE
  )
)

# =============================================================================
# SPORT CONFIGURATION
# =============================================================================

#' Get race types for a sport
#'
#' @param sport Sport name
#' @return List of race types
get_sport_race_types <- function(sport) {
  race_types <- switch(sport,
    "cross-country" = CROSS_COUNTRY_RACE_TYPES,
    "alpine" = ALPINE_RACE_TYPES,
    "biathlon" = BIATHLON_RACE_TYPES,
    "nordic-combined" = NORDIC_COMBINED_RACE_TYPES,
    "skijump" = SKI_JUMPING_RACE_TYPES,
    stop(paste("Unknown sport:", sport))
  )
  return(race_types)
}

#' Get individual (non-team) race types for a sport
#'
#' @param sport Sport name
#' @return Vector of race type keys
get_individual_race_types <- function(sport) {
  race_types <- get_sport_race_types(sport)
  individual_types <- names(race_types)[
    !sapply(race_types, function(rt) isTRUE(rt$is_team))
  ]
  return(individual_types)
}

#' Get team race types for a sport
#'
#' @param sport Sport name
#' @return Vector of race type keys
get_team_race_types <- function(sport) {
  race_types <- get_sport_race_types(sport)
  team_types <- names(race_types)[
    sapply(race_types, function(rt) isTRUE(rt$is_team))
  ]
  return(team_types)
}

# =============================================================================
# VALIDATION
# =============================================================================

#' Validate parameter set
#'
#' @param params Parameter list
#' @return TRUE if valid, error message if not
validate_params <- function(params) {
  # Check required parameters
  required <- c("decay_lambda", "sd_scale_factor", "sd_min", "sd_max",
                "n_history_required", "gam_fill_weight_factor")

  missing <- setdiff(required, names(params))
  if (length(missing) > 0) {
    return(paste("Missing parameters:", paste(missing, collapse = ", ")))
  }

  # Check constraints
  if (params$sd_min > params$sd_max) {
    return("sd_min must be less than or equal to sd_max")
  }

  if (params$decay_lambda <= 0) {
    return("decay_lambda must be positive")
  }

  if (params$sd_scale_factor <= 0 || params$sd_scale_factor > 1.5) {
    return("sd_scale_factor should be between 0 and 1.5")
  }

  if (params$n_history_required < 1) {
    return("n_history_required must be at least 1")
  }

  if (params$gam_fill_weight_factor < 0 || params$gam_fill_weight_factor > 1) {
    return("gam_fill_weight_factor must be between 0 and 1")
  }

  return(TRUE)
}

#' Generate all parameter combinations from a grid
#'
#' @param param_grid List of parameter vectors
#' @return Data frame with all combinations
expand_param_grid <- function(param_grid) {
  grid <- expand.grid(param_grid, stringsAsFactors = FALSE)

  # Filter invalid combinations (sd_min > sd_max)
  # Note: sd_min == sd_max is allowed (equivalent to fixed_sd)
  if ("sd_min" %in% names(grid) && "sd_max" %in% names(grid)) {
    grid <- grid[grid$sd_min <= grid$sd_max, ]
  }

  return(grid)
}

#' Sample random parameters within ranges
#'
#' @param param_grid Parameter grid defining ranges
#' @param n Number of samples
#' @return Data frame with sampled parameters
sample_params <- function(param_grid, n) {
  samples <- data.frame(
    decay_lambda = runif(n, min(param_grid$decay_lambda), max(param_grid$decay_lambda)),
    sd_scale_factor = runif(n, min(param_grid$sd_scale_factor), max(param_grid$sd_scale_factor)),
    sd_min = sample(param_grid$sd_min, n, replace = TRUE),
    sd_max = sample(param_grid$sd_max, n, replace = TRUE),
    n_history_required = sample(param_grid$n_history_required, n, replace = TRUE),
    gam_fill_weight_factor = runif(n, min(param_grid$gam_fill_weight_factor),
                                    max(param_grid$gam_fill_weight_factor))
  )

  # Fix invalid combinations (sd_min > sd_max)
  invalid <- samples$sd_min > samples$sd_max
  samples$sd_max[invalid] <- samples$sd_min[invalid]

  return(samples)
}

cat("param-grid.R loaded successfully\n")
