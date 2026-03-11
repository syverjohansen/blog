# sport_params.R
# Optimized simulation parameters by sport and race type
# Generated: 2026-03-10 23:37:08.028725
#
# Usage:
#   source('~/blog/daehl-e/content/post/shared/sport_params.R')
#   params <- get_sport_params('cross-country', 'Sprint_C')

SPORT_PARAMS <- list(

  "cross-country" = list(
    default = list(
      decay_lambda = 0.000778,
      sd_scale_factor = 0.9998,
      sd_min = 8,
      sd_max = 20,
      n_history_required = 18,
      gam_fill_weight_factor = 0.3653
    ),
    race_types = list(
      "Sprint_C" = list(
        decay_lambda = 0.002887,
        sd_scale_factor = 0.9756,
        sd_min = 8,
        sd_max = 20,
        n_history_required = 18,
        gam_fill_weight_factor = 0.1281
      ),
      "Sprint_F" = list(
        decay_lambda = 0.001228,
        sd_scale_factor = 0.8882,
        sd_min = 8,
        sd_max = 23,
        n_history_required = 18,
        gam_fill_weight_factor = 0.1234
      ),
      "Distance_C_Ind" = list(
        decay_lambda = 0.002358,
        sd_scale_factor = 0.9464,
        sd_min = 8,
        sd_max = 23,
        n_history_required = 18,
        gam_fill_weight_factor = 0.1124
      ),
      "Distance_C_Ms" = list(
        decay_lambda = 0.002737,
        sd_scale_factor = 0.9403,
        sd_min = 8,
        sd_max = 20,
        n_history_required = 18,
        gam_fill_weight_factor = 0.2700
      ),
      "Distance_F_Ind" = list(
        decay_lambda = 0.002316,
        sd_scale_factor = 0.8665,
        sd_min = 8,
        sd_max = 23,
        n_history_required = 18,
        gam_fill_weight_factor = 0.3546
      ),
      "Distance_F_Ms" = list(
        decay_lambda = 0.000907,
        sd_scale_factor = 0.9062,
        sd_min = 8,
        sd_max = 23,
        n_history_required = 18,
        gam_fill_weight_factor = 0.2051
      ),
      "Distance_Ms" = list(
        decay_lambda = 0.001019,
        sd_scale_factor = 0.9518,
        sd_min = 8,
        sd_max = 23,
        n_history_required = 18,
        gam_fill_weight_factor = 0.2128
      )
    )
  )

)

#' Get parameters for a sport/race-type combination
#'
#' @param sport Sport name
#' @param race_type Optional race type (uses default if NULL)
#' @return List of parameters
get_sport_params <- function(sport, race_type = NULL) {
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
