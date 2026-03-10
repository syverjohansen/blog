# sport_params.R
# Optimized simulation parameters by sport and race type
# Generated: 2026-03-09 20:43:48.366965
#
# Usage:
#   source('~/blog/daehl-e/content/post/shared/sport_params.R')
#   params <- get_sport_params('cross-country', 'Sprint_C')

SPORT_PARAMS <- list(

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
