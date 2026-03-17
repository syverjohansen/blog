# sport_params.R
# Optimized simulation parameters by sport and race type
# Generated: 2026-03-16 19:21:54.388671
#
# Usage:
#   source('~/blog/daehl-e/content/post/shared/sport_params.R')
#   params <- get_sport_params('cross-country', 'Sprint_C')

SPORT_PARAMS <- list(

  "cross-country" = list(
    default = list(
      decay_lambda = 0.004036,
      sd_scale_factor = 1.0714,
      sd_min = 16,
      sd_max = 28,
      n_history_required = 22,
      gam_fill_weight_factor = 0.2674
    ),
    race_types = list(
      "Sprint_C" = list(
        decay_lambda = 0.002903,
        sd_scale_factor = 0.9741,
        sd_min = 16,
        sd_max = 23,
        n_history_required = 16,
        gam_fill_weight_factor = 0.2377
      ),
      "Sprint_F" = list(
        decay_lambda = 0.001752,
        sd_scale_factor = 0.9956,
        sd_min = 16,
        sd_max = 23,
        n_history_required = 12,
        gam_fill_weight_factor = 0.3296
      ),
      "Distance_C_Ind" = list(
        decay_lambda = 0.002476,
        sd_scale_factor = 0.9908,
        sd_min = 16,
        sd_max = 26,
        n_history_required = 16,
        gam_fill_weight_factor = 0.1541
      ),
      "Distance_C_Ms" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.8000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 16,
        gam_fill_weight_factor = 0.2500
      ),
      "Distance_F_Ind" = list(
        decay_lambda = 0.002330,
        sd_scale_factor = 0.9782,
        sd_min = 16,
        sd_max = 26,
        n_history_required = 15,
        gam_fill_weight_factor = 0.3365
      ),
      "Distance_F_Ms" = list(
        decay_lambda = 0.001254,
        sd_scale_factor = 0.8848,
        sd_min = 18,
        sd_max = 28,
        n_history_required = 12,
        gam_fill_weight_factor = 0.1671
      ),
      "Distance_Ms" = list(
        decay_lambda = 0.001861,
        sd_scale_factor = 0.8810,
        sd_min = 16,
        sd_max = 23,
        n_history_required = 16,
        gam_fill_weight_factor = 0.1687
      ),
      "Relay" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Mixed_Relay" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Team_Sprint" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      )
    )
  ),

  "biathlon" = list(
    default = list(
      decay_lambda = 0.004552,
      sd_scale_factor = 1.0928,
      sd_min = 16,
      sd_max = 24,
      n_history_required = 21,
      gam_fill_weight_factor = 0.0695
    ),
    race_types = list(
      "Sprint" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Individual" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Pursuit" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Mass_Start" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Relay" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Mixed_Relay" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Single_Mixed_Relay" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      )
    )
  ),

  "alpine" = list(
    default = list(
      decay_lambda = 0.003500,
      sd_scale_factor = 0.8000,
      sd_min = 17,
      sd_max = 25,
      n_history_required = 22,
      gam_fill_weight_factor = 0.2500
    ),
    race_types = list(
      "Downhill" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Super_G" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Giant_Slalom" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Slalom" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Combined" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      )
    )
  ),

  "skijump" = list(
    default = list(
      decay_lambda = 0.004753,
      sd_scale_factor = 0.8311,
      sd_min = 19,
      sd_max = 26,
      n_history_required = 16,
      gam_fill_weight_factor = 0.2944
    ),
    race_types = list(
      "Large_Hill" = list(
        decay_lambda = 0.003000,
        sd_scale_factor = 0.7500,
        sd_min = 20,
        sd_max = 24,
        n_history_required = 11,
        gam_fill_weight_factor = 0.2500
      ),
      "Normal_Hill" = list(
        decay_lambda = 0.003000,
        sd_scale_factor = 0.7500,
        sd_min = 17,
        sd_max = 24,
        n_history_required = 13,
        gam_fill_weight_factor = 0.2000
      ),
      "Flying_Hill" = list(
        decay_lambda = 0.001377,
        sd_scale_factor = 0.9029,
        sd_min = 20,
        sd_max = 24,
        n_history_required = 9,
        gam_fill_weight_factor = 0.2399
      ),
      "Team_Large" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      )
    )
  ),

  "nordic-combined" = list(
    default = list(
      decay_lambda = 0.004681,
      sd_scale_factor = 1.0376,
      sd_min = 9,
      sd_max = 20,
      n_history_required = 18,
      gam_fill_weight_factor = 0.1913
    ),
    race_types = list(
      "Individual" = list(
        decay_lambda = 0.003383,
        sd_scale_factor = 0.8671,
        sd_min = 13,
        sd_max = 21,
        n_history_required = 12,
        gam_fill_weight_factor = 0.1814
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
        decay_lambda = 0.001500,
        sd_scale_factor = 0.8000,
        sd_min = 17,
        sd_max = 21,
        n_history_required = 9,
        gam_fill_weight_factor = 0.2500
      ),
      "Sprint" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Team" = list(
        decay_lambda = 0.002000,
        sd_scale_factor = 0.9000,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 10,
        gam_fill_weight_factor = 0.2500
      ),
      "Team_Sprint" = list(
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
