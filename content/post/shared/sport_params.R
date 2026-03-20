# sport_params.R
# Optimized simulation parameters by sport and race type
# Generated: 2026-03-20 03:42:35.317659
#
# Usage:
#   source('~/blog/daehl-e/content/post/shared/sport_params.R')
#   params <- get_sport_params('cross-country', 'Sprint_C')

SPORT_PARAMS <- list(

  "cross-country" = list(
    default = list(
      decay_lambda = 0.001837,
      sd_scale_factor = 1.0424,
      sd_min = 16,
      sd_max = 24,
      n_history_required = 22,
      gam_fill_weight_factor = 0.1839
    ),
    race_types = list(
      "Sprint_C" = list(
        decay_lambda = 0.003022,
        sd_scale_factor = 1.0037,
        sd_min = 16,
        sd_max = 28,
        n_history_required = 22,
        gam_fill_weight_factor = 0.1514
      ),
      "Sprint_F" = list(
        decay_lambda = 0.002876,
        sd_scale_factor = 1.0955,
        sd_min = 17,
        sd_max = 24,
        n_history_required = 14,
        gam_fill_weight_factor = 0.2343
      ),
      "Distance_C_Ind" = list(
        decay_lambda = 0.003255,
        sd_scale_factor = 1.0410,
        sd_min = 12,
        sd_max = 25,
        n_history_required = 22,
        gam_fill_weight_factor = 0.1404
      ),
      "Distance_C_Ms" = list(
        decay_lambda = 0.001769,
        sd_scale_factor = 0.8510,
        sd_min = 12,
        sd_max = 28,
        n_history_required = 18,
        gam_fill_weight_factor = 0.1674
      ),
      "Distance_F_Ind" = list(
        decay_lambda = 0.003492,
        sd_scale_factor = 0.7901,
        sd_min = 16,
        sd_max = 28,
        n_history_required = 18,
        gam_fill_weight_factor = 0.3606
      ),
      "Distance_F_Ms" = list(
        decay_lambda = 0.000504,
        sd_scale_factor = 0.9848,
        sd_min = 18,
        sd_max = 28,
        n_history_required = 18,
        gam_fill_weight_factor = 0.0671
      ),
      "Distance_Ms" = list(
        decay_lambda = 0.002110,
        sd_scale_factor = 0.8140,
        sd_min = 16,
        sd_max = 22,
        n_history_required = 22,
        gam_fill_weight_factor = 0.0885
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
      decay_lambda = 0.003658,
      sd_scale_factor = 1.0547,
      sd_min = 17,
      sd_max = 22,
      n_history_required = 18,
      gam_fill_weight_factor = 0.2562
    ),
    race_types = list(
      "Sprint" = list(
        decay_lambda = 0.004712,
        sd_scale_factor = 1.0891,
        sd_min = 17,
        sd_max = 24,
        n_history_required = 22,
        gam_fill_weight_factor = 0.2572
      ),
      "Individual" = list(
        decay_lambda = 0.004116,
        sd_scale_factor = 0.6088,
        sd_min = 25,
        sd_max = 32,
        n_history_required = 21,
        gam_fill_weight_factor = 0.1939
      ),
      "Pursuit" = list(
        decay_lambda = 0.003678,
        sd_scale_factor = 1.0647,
        sd_min = 16,
        sd_max = 24,
        n_history_required = 19,
        gam_fill_weight_factor = 0.0574
      ),
      "Mass_Start" = list(
        decay_lambda = 0.001206,
        sd_scale_factor = 0.8452,
        sd_min = 22,
        sd_max = 33,
        n_history_required = 21,
        gam_fill_weight_factor = 0.0547
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
      decay_lambda = 0.005125,
      sd_scale_factor = 0.9165,
      sd_min = 18,
      sd_max = 25,
      n_history_required = 22,
      gam_fill_weight_factor = 0.2524
    ),
    race_types = list(
      "Downhill" = list(
        decay_lambda = 0.004104,
        sd_scale_factor = 0.7670,
        sd_min = 24,
        sd_max = 31,
        n_history_required = 15,
        gam_fill_weight_factor = 0.0654
      ),
      "Super_G" = list(
        decay_lambda = 0.003058,
        sd_scale_factor = 0.9951,
        sd_min = 22,
        sd_max = 30,
        n_history_required = 17,
        gam_fill_weight_factor = 0.2615
      ),
      "Giant_Slalom" = list(
        decay_lambda = 0.004392,
        sd_scale_factor = 1.0729,
        sd_min = 17,
        sd_max = 24,
        n_history_required = 22,
        gam_fill_weight_factor = 0.3427
      ),
      "Slalom" = list(
        decay_lambda = 0.003500,
        sd_scale_factor = 0.9000,
        sd_min = 17,
        sd_max = 25,
        n_history_required = 22,
        gam_fill_weight_factor = 0.2500
      ),
      "Combined" = list(
        decay_lambda = 0.002955,
        sd_scale_factor = 0.9590,
        sd_min = 20,
        sd_max = 28,
        n_history_required = 19,
        gam_fill_weight_factor = 0.1559
      )
    )
  ),

  "skijump" = list(
    default = list(
      decay_lambda = 0.005991,
      sd_scale_factor = 0.9508,
      sd_min = 19,
      sd_max = 30,
      n_history_required = 17,
      gam_fill_weight_factor = 0.1742
    ),
    race_types = list(
      "Large_Hill" = list(
        decay_lambda = 0.005000,
        sd_scale_factor = 1.1000,
        sd_min = 17,
        sd_max = 25,
        n_history_required = 22,
        gam_fill_weight_factor = 0.2500
      ),
      "Normal_Hill" = list(
        decay_lambda = 0.005409,
        sd_scale_factor = 0.9841,
        sd_min = 17,
        sd_max = 28,
        n_history_required = 15,
        gam_fill_weight_factor = 0.4346
      ),
      "Flying_Hill" = list(
        decay_lambda = 0.002750,
        sd_scale_factor = 0.6000,
        sd_min = 22,
        sd_max = 34,
        n_history_required = 10,
        gam_fill_weight_factor = 0.3500
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
