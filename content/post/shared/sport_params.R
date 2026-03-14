# sport_params.R
# Optimized simulation parameters by sport and race type
# Generated: 2026-03-13 20:09:39.425968
#
# Usage:
#   source('~/blog/daehl-e/content/post/shared/sport_params.R')
#   params <- get_sport_params('cross-country', 'Sprint_C')

SPORT_PARAMS <- list(

  "cross-country" = list(
    default = list(
      decay_lambda = 0.002500,
      sd_scale_factor = 0.8500,
      sd_min = 18,
      sd_max = 24,
      n_history_required = 16,
      gam_fill_weight_factor = 0.2000
    ),
    race_types = list(
      "Sprint_C" = list(
        decay_lambda = 0.003512,
        sd_scale_factor = 0.8233,
        sd_min = 19,
        sd_max = 25,
        n_history_required = 15,
        gam_fill_weight_factor = 0.1270
      ),
      "Sprint_F" = list(
        decay_lambda = 0.002397,
        sd_scale_factor = 0.7545,
        sd_min = 17,
        sd_max = 19,
        n_history_required = 18,
        gam_fill_weight_factor = 0.1927
      ),
      "Distance_C_Ind" = list(
        decay_lambda = 0.003280,
        sd_scale_factor = 0.9520,
        sd_min = 14,
        sd_max = 25,
        n_history_required = 16,
        gam_fill_weight_factor = 0.2824
      ),
      "Distance_C_Ms" = list(
        decay_lambda = 0.002131,
        sd_scale_factor = 0.7620,
        sd_min = 15,
        sd_max = 24,
        n_history_required = 17,
        gam_fill_weight_factor = 0.2953
      ),
      "Distance_F_Ind" = list(
        decay_lambda = 0.003329,
        sd_scale_factor = 0.8326,
        sd_min = 17,
        sd_max = 20,
        n_history_required = 16,
        gam_fill_weight_factor = 0.2754
      ),
      "Distance_F_Ms" = list(
        decay_lambda = 0.001816,
        sd_scale_factor = 0.5886,
        sd_min = 21,
        sd_max = 26,
        n_history_required = 16,
        gam_fill_weight_factor = 0.1930
      ),
      "Distance_Ms" = list(
        decay_lambda = 0.003556,
        sd_scale_factor = 0.7901,
        sd_min = 18,
        sd_max = 22,
        n_history_required = 17,
        gam_fill_weight_factor = 0.3551
      )
    )
  ),

  "biathlon" = list(
    default = list(
      decay_lambda = 0.003037,
      sd_scale_factor = 0.8386,
      sd_min = 20,
      sd_max = 20,
      n_history_required = 16,
      gam_fill_weight_factor = 0.1567
    ),
    race_types = list(
      "Sprint" = list(
        decay_lambda = 0.002783,
        sd_scale_factor = 0.7994,
        sd_min = 19,
        sd_max = 20,
        n_history_required = 13,
        gam_fill_weight_factor = 0.1814
      ),
      "Individual" = list(
        decay_lambda = 0.003223,
        sd_scale_factor = 0.6121,
        sd_min = 23,
        sd_max = 24,
        n_history_required = 18,
        gam_fill_weight_factor = 0.3338
      ),
      "Pursuit" = list(
        decay_lambda = 0.004439,
        sd_scale_factor = 0.7686,
        sd_min = 17,
        sd_max = 23,
        n_history_required = 18,
        gam_fill_weight_factor = 0.1674
      ),
      "Mass_Start" = list(
        decay_lambda = 0.003229,
        sd_scale_factor = 0.6784,
        sd_min = 20,
        sd_max = 23,
        n_history_required = 17,
        gam_fill_weight_factor = 0.2391
      )
    )
  ),

  "alpine" = list(
    default = list(
      decay_lambda = 0.002000,
      sd_scale_factor = 0.8500,
      sd_min = 18,
      sd_max = 24,
      n_history_required = 16,
      gam_fill_weight_factor = 0.2000
    ),
    race_types = list(
      "Downhill" = list(
        decay_lambda = 0.002124,
        sd_scale_factor = 0.7945,
        sd_min = 21,
        sd_max = 26,
        n_history_required = 14,
        gam_fill_weight_factor = 0.1418
      ),
      "Super_G" = list(
        decay_lambda = 0.001687,
        sd_scale_factor = 0.7284,
        sd_min = 20,
        sd_max = 25,
        n_history_required = 15,
        gam_fill_weight_factor = 0.2067
      ),
      "Giant_Slalom" = list(
        decay_lambda = 0.003437,
        sd_scale_factor = 0.7300,
        sd_min = 20,
        sd_max = 23,
        n_history_required = 17,
        gam_fill_weight_factor = 0.2601
      ),
      "Slalom" = list(
        decay_lambda = 0.004000,
        sd_scale_factor = 0.8500,
        sd_min = 15,
        sd_max = 21,
        n_history_required = 16,
        gam_fill_weight_factor = 0.2500
      ),
      "Combined" = list(
        decay_lambda = 0.002500,
        sd_scale_factor = 0.9000,
        sd_min = 21,
        sd_max = 27,
        n_history_required = 16,
        gam_fill_weight_factor = 0.2500
      )
    )
  ),

  "skijump" = list(
    default = list(
      decay_lambda = 0.003846,
      sd_scale_factor = 0.7730,
      sd_min = 21,
      sd_max = 26,
      n_history_required = 11,
      gam_fill_weight_factor = 0.1641
    ),
    race_types = list(
      "Large_Hill" = list(
        decay_lambda = 0.003773,
        sd_scale_factor = 0.7991,
        sd_min = 20,
        sd_max = 24,
        n_history_required = 14,
        gam_fill_weight_factor = 0.2029
      ),
      "Normal_Hill" = list(
        decay_lambda = 0.004264,
        sd_scale_factor = 0.7712,
        sd_min = 17,
        sd_max = 26,
        n_history_required = 11,
        gam_fill_weight_factor = 0.1736
      ),
      "Flying_Hill" = list(
        decay_lambda = 0.002377,
        sd_scale_factor = 0.8029,
        sd_min = 21,
        sd_max = 27,
        n_history_required = 8,
        gam_fill_weight_factor = 0.1899
      )
    )
  ),

  "nordic-combined" = list(
    default = list(
      decay_lambda = 0.003681,
      sd_scale_factor = 0.9376,
      sd_min = 14,
      sd_max = 20,
      n_history_required = 13,
      gam_fill_weight_factor = 0.1413
    ),
    race_types = list(
      "Individual" = list(
        decay_lambda = 0.004862,
        sd_scale_factor = 0.8742,
        sd_min = 13,
        sd_max = 21,
        n_history_required = 14,
        gam_fill_weight_factor = 0.1841
      ),
      "Individual_Compact" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Mass_Start" = list(
        decay_lambda = 0.002788,
        sd_scale_factor = 0.7102,
        sd_min = 17,
        sd_max = 19,
        n_history_required = 13,
        gam_fill_weight_factor = 0.3116
      ),
      "Sprint" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
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
  if (identical(sport, 'cross-country') && identical(race_type, 'Mixed_Relay')) {
    race_type <- 'Relay'
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
