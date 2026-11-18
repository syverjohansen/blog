# Validation shared by cross-country relay and team-sprint prediction writers.

validate_team_prediction_tables <- function(points_df, prob_df, member_count) {
  required_points <- c("Team", paste0("Leg ", seq_len(member_count)), "Expected Points")
  required_probabilities <- c(
    "Team", paste0("Leg ", seq_len(member_count)),
    "Win", "Podium", "Top 5", "Top 10"
  )
  missing_points <- setdiff(required_points, names(points_df))
  missing_probabilities <- setdiff(required_probabilities, names(prob_df))
  if (length(missing_points) > 0 || length(missing_probabilities) > 0) {
    stop(paste(
      "Prediction output is missing required columns:",
      paste(c(missing_points, missing_probabilities), collapse = ", ")
    ))
  }
  if (nrow(points_df) == 0 || nrow(prob_df) == 0) {
    stop("Prediction output must contain at least one team")
  }
  if (anyDuplicated(points_df$Team) || anyDuplicated(prob_df$Team)) {
    stop("Prediction output contains duplicate teams")
  }
  if (!setequal(points_df$Team, prob_df$Team)) {
    stop("Points and probability outputs contain different teams")
  }
  if (any(!is.finite(points_df[["Expected Points"]]))) {
    stop("Expected Points must be finite")
  }
  probability_columns <- c("Win", "Podium", "Top 5", "Top 10")
  probabilities <- as.matrix(prob_df[, probability_columns, drop = FALSE])
  if (any(!is.finite(probabilities)) || any(probabilities < 0 | probabilities > 1)) {
    stop("Probabilities must be finite values between 0 and 1")
  }
  if (any(probabilities[, 1] > probabilities[, 2] |
          probabilities[, 2] > probabilities[, 3] |
          probabilities[, 3] > probabilities[, 4])) {
    stop("Probabilities must be monotonic from Win through Top 10")
  }
  invisible(TRUE)
}
