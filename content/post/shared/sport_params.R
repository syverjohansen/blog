# sport_params.R
# Optimized simulation parameters by sport and race type
# Generated: 2026-03-11 18:15:48.851367
#
# Usage:
#   source('~/blog/daehl-e/content/post/shared/sport_params.R')
#   params <- get_sport_params('cross-country', 'Sprint_C')

SPORT_PARAMS <- list(

  "cross-country" = list(
    default = list(
      decay_lambda = 0.001000,
      sd_scale_factor = 0.9000,
      sd_min = 18,
      sd_max = 24,
      n_history_required = 16,
      gam_fill_weight_factor = 0.1500
    ),
    race_types = list(
      "Sprint_C" = list(
        decay_lambda = 0.002367,
        sd_scale_factor = 0.7980,
        sd_min = 16,
        sd_max = 22,
        n_history_required = 16,
        gam_fill_weight_factor = 0.1492
      ),
      "Sprint_F" = list(
        decay_lambda = 0.001532,
        sd_scale_factor = 0.5638,
        sd_min = 16,
        sd_max = 16,
        n_history_required = 18,
        gam_fill_weight_factor = 0.2280
      ),
      "Distance_C_Ind" = list(
        decay_lambda = 0.003233,
        sd_scale_factor = 0.9639,
        sd_min = 18,
        sd_max = 26,
        n_history_required = 14,
        gam_fill_weight_factor = 0.3664
      ),
      "Distance_C_Ms" = list(
        decay_lambda = 0.002262,
        sd_scale_factor = 0.6240,
        sd_min = 18,
        sd_max = 24,
        n_history_required = 18,
        gam_fill_weight_factor = 0.3405
      ),
      "Distance_F_Ind" = list(
        decay_lambda = 0.003069,
        sd_scale_factor = 0.9736,
        sd_min = 18,
        sd_max = 22,
        n_history_required = 16,
        gam_fill_weight_factor = 0.1642
      ),
      "Distance_F_Ms" = list(
        decay_lambda = 0.002455,
        sd_scale_factor = 0.6486,
        sd_min = 22,
        sd_max = 24,
        n_history_required = 14,
        gam_fill_weight_factor = 0.2211
      ),
      "Distance_Ms" = list(
        decay_lambda = 0.004035,
        sd_scale_factor = 0.8924,
        sd_min = 18,
        sd_max = 24,
        n_history_required = 18,
        gam_fill_weight_factor = 0.3243
      )
    )
  ),

  "alpine" = list(
    default = list(
      decay_lambda = 0.002,
      sd_scale_factor = 0.77,
      sd_min = 4,
      sd_max = 16,
      n_history_required = 10,
      gam_fill_weight_factor = 0.25
    ),
    race_types = list()
  ),

  "biathlon" = list(
    default = list(
      decay_lambda = 0.002,
      sd_scale_factor = 0.77,
      sd_min = 4,
      sd_max = 16,
      n_history_required = 10,
      gam_fill_weight_factor = 0.25
    ),
    race_types = list(
      "Relay" = list(
        sd_scale_factor = 0.8,
        sd_min = 3,
        sd_max = 12
      )
    )
  ),

  "nordic-combined" = list(
    default = list(
      decay_lambda = 0.002,
      sd_scale_factor = 0.77,
      sd_min = 4,
      sd_max = 16,
      n_history_required = 10,
      gam_fill_weight_factor = 0.25
    ),
    race_types = list(
      "Team" = list(
        sd_scale_factor = 0.8,
        sd_min = 3,
        sd_max = 12
      )
    )
  ),

  "skijump" = list(
    default = list(
      decay_lambda = 0.002,
      sd_scale_factor = 0.77,
      sd_min = 4,
      sd_max = 16,
      n_history_required = 10,
      gam_fill_weight_factor = 0.25
    ),
    race_types = list(
      "Team_Large" = list(
        sd_scale_factor = 0.8,
        sd_min = 3,
        sd_max = 12
      )
    )
  )

)

#' Get parameters for a sport/race-type combination
#'
#' @param sport Sport name
#' @param race_type Optional race type (uses default if NULL)
#' @param event_type Backward-compatible alias for race_type
#' @return List of parameters
get_sport_params <- function(sport, race_type = NULL, event_type = NULL) {
  if (is.null(race_type) && !is.null(event_type)) {
    race_type <- event_type
  }

  sport_config <- SPORT_PARAMS[[sport]]
  
  if (is.null(sport_config)) {
    warning(paste("Unknown sport:", sport, "- using global defaults"))
    return(list(
      decay_lambda = 0.002,
      sd_scale_factor = 0.77,
      sd_min = 4,
      sd_max = 16,
      n_history_required = 10,
      gam_fill_weight_factor = 0.25
    ))
  }
  
  # Start with defaults
  params <- sport_config$default
  
  # Override with race-type specific if available
  if (!is.null(race_type) && !is.null(sport_config$race_types[[race_type]])) {
    race_params <- sport_config$race_types[[race_type]]
    for (param in names(race_params)) {
      params[[param]] <- race_params[[param]]
    }
  }
  
  return(params)
}

cat("sport_params.R loaded successfully\n")
