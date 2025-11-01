# Ski Jumping Weekly Predictions Model Documentation

## Overview
This document explains how the ski jumping weekly predictions model works, based on the `weekly-picks2.R` script. The model predicts position probabilities (win, podium, top 5, top 10, top 30) for individual athletes and teams across multiple race types.

## Competition Types

### Individual Competitions
- **Men's Individual**: Individual male athletes competing
- **Ladies' Individual**: Individual female athletes competing

### Team Competitions  
- **Men's Team**: Teams of male athletes (nation-based)
- **Ladies' Team**: Teams of female athletes (nation-based)
- **Mixed Team**: Teams with both male and female athletes (nation-based)

## Data Sources

### Individual Data
- **Men**: `men_chrono_elevation.csv` - Historical individual men's results with elevation data
- **Ladies**: `ladies_chrono_elevation.csv` - Historical individual ladies' results with elevation data
- **Startlists**: `startlist_weekend_men.csv`, `startlist_weekend_ladies.csv`

### Team Data
- **Men's Teams**: `men_team_chrono.csv` - Historical men's team results  
- **Ladies' Teams**: `ladies_team_chrono.csv` - Historical ladies' team results
- **Mixed Teams**: `mixed_team_chrono.csv` - Historical mixed team results
- **Team Startlists**: `startlist_team_weekend_men.csv`, `startlist_team_weekend_ladies.csv`, `startlist_mixed_team_weekend_teams.csv`

## Points System
All ski jumping events use the same points system:
- **Top 30 get points**: 100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1

## ELO Features

### Individual Athletes
Hill-specific ELO ratings based on different hill sizes:
- `Normal_Elo_Pct` - Performance on normal hills (K90-K99)
- `Large_Elo_Pct` - Performance on large hills (K120-K130) 
- `Flying_Elo_Pct` - Performance on flying hills (K185+)
- `Elo_Pct` - Overall ELO rating
- `Prev_Points_Weighted` - Weighted average of recent points (last 5 races)

### Team Competitions
Average ELO ratings of team members:
- `Avg_Normal_Elo_Pct` - Team average normal hill ELO
- `Avg_Large_Elo_Pct` - Team average large hill ELO  
- `Avg_Flying_Elo_Pct` - Team average flying hill ELO
- `Avg_Elo_Pct` - Team average overall ELO

## Race Filtering

### Weekend Race Selection
```r
# Individual races
men_races <- next_weekend_races %>%
  filter(Sex == "M", !grepl("Team", RaceType, ignore.case = TRUE))

ladies_races <- next_weekend_races %>%
  filter(Sex == "L", !grepl("Team", RaceType, ignore.case = TRUE))

# Team races  
men_teams <- next_weekend_races %>%
  filter(grepl("Team", RaceType, ignore.case = TRUE) & Sex == "M")

ladies_teams <- next_weekend_races %>%
  filter(grepl("Team", RaceType, ignore.case = TRUE) & Sex == "L")

# Mixed team races
mixed_teams <- next_weekend_races %>%
  filter(Sex == "Mixed")
```

## Race Participation Probability

### Individual Athletes
Calculates probability based on historical participation in the specific race type over the last 5 years:

```r
get_race_probability <- function(chronos, participant, racetype, is_team = FALSE) {
  # Use 5 years of history or participant's first race date, whichever is later
  five_years_ago <- Sys.Date() - (5 * 365)
  start_date <- max(five_years_ago, first_race_date)
  
  # Count total races vs participant's races for this race type
  total_races <- count_distinct_races(chronos, racetype, start_date)
  participant_races <- count_participant_races(chronos, participant, racetype, start_date)
  
  probability <- min(1, participant_races / total_races)
  return(probability)
}
```

### Team Competitions
Teams typically have probability = 1.0 (assumed to participate if entered)

## Prediction Models

### Points Prediction Model
Uses Generalized Additive Models (GAM) with feature selection:

#### Individual Model
```r
explanatory_vars <- c("Prev_Points_Weighted", "Normal_Elo_Pct", "Large_Elo_Pct", 
                      "Flying_Elo_Pct", "Elo_Pct")

# Feature selection using regsubsets with BIC criterion
exhaustive_selection <- regsubsets(Points ~ ., data = race_df_75, method = "exhaustive")
best_bic_vars <- names(coef(exhaustive_selection, which.min(summary$bic)))

# GAM model with smooth terms
gam_formula <- as.formula(paste("Points ~", paste("s(", best_bic_vars[-1], ")", collapse=" + ")))
model <- gam(gam_formula, data = race_df_75)
```

#### Team Model
```r
explanatory_vars <- c("Avg_Normal_Elo_Pct", "Avg_Large_Elo_Pct", 
                      "Avg_Flying_Elo_Pct", "Avg_Elo_Pct")
```

### Position Probability Models
Creates separate GAM models for each position threshold (1, 3, 5, 10, 30):

```r
for(threshold in position_thresholds) {
  # Binary response: did athlete finish in top N?
  race_df$position_achieved <- as.numeric(race_df$Place <= threshold)
  
  # Feature selection for this threshold
  pos_selection <- regsubsets(position_achieved ~ ., data = race_df_75, 
                             family = binomial, method = "exhaustive")
  pos_best_vars <- names(coef(pos_selection, which.min(summary$bic)))
  
  # GAM model with binomial family
  pos_gam_formula <- as.formula(paste("position_achieved ~", 
                                     paste("s(", pos_best_vars[-1], ")", collapse=" + ")))
  position_model <- gam(pos_gam_formula, data = race_df_75, family = binomial)
  
  # Store model for prediction
  position_models[[paste0("threshold_", threshold)]] <- position_model
}
```

## Data Preprocessing

### ELO Percentage Calculation
Converts absolute ELO ratings to percentages by dividing by historical maximum:

```r
# Individual athletes
for(col in c("Normal_Elo", "Large_Elo", "Flying_Elo", "Elo")) {
  max_val <- max(race_df[[col]], na.rm = TRUE)
  result_df[[paste0(col, "_Pct")]] <- result_df[[col]] / max_val
}

# Team averages  
for(col in c("Avg_Normal_Elo", "Avg_Large_Elo", "Avg_Flying_Elo", "Avg_Elo")) {
  max_val <- max(race_df[[col]], na.rm = TRUE)
  result_df[[paste0(col, "_Pct")]] <- result_df[[col]] / max_val
}
```

### Missing Value Handling
```r
# Replace NAs with first quartile values
result_df <- result_df %>%
  mutate(across(ends_with("_Pct"), ~replace_na_with_quartile(.x)),
         Prev_Points_Weighted = replace_na(Prev_Points_Weighted, 0))
```

## Probability Normalization

### Multi-Step Process
1. **Race Participation Adjustment**: Multiply position probabilities by race participation probabilities
2. **Target Sum Normalization**: Scale probabilities to target sums (100% for win, 300% for podium, etc.)
3. **Individual Capping**: Cap individual probabilities at 100%
4. **Excess Redistribution**: Redistribute excess probability proportionally to other athletes

```r
normalize_position_probabilities <- function(predictions, race_prob_col, position_thresholds) {
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    
    # Step 1: Apply race participation probability
    normalized[[prob_col]] <- normalized[[prob_col]] * normalized[[race_prob_col]]
    
    # Step 2: Normalize to target sum
    current_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
    target_sum <- 100 * threshold
    scaling_factor <- target_sum / current_sum
    normalized[[prob_col]] <- normalized[[prob_col]] * scaling_factor
    
    # Step 3: Cap at 100%
    over_hundred <- which(normalized[[prob_col]] > 100)
    if(length(over_hundred) > 0) {
      excess <- sum(normalized[[prob_col]][over_hundred] - 100)
      normalized[[prob_col]][over_hundred] <- 100
      
      # Step 4: Redistribute excess
      under_hundred <- which(normalized[[prob_col]] < 100)
      if(length(under_hundred) > 0 && excess > 0) {
        under_sum <- sum(normalized[[prob_col]][under_hundred])
        redistrib_factor <- (under_sum + excess) / under_sum
        normalized[[prob_col]][under_hundred] <- normalized[[prob_col]][under_hundred] * redistrib_factor
      }
    }
  }
}
```

## Hill Size Classification
- **Normal Hills**: K90-K99 (most common World Cup venues)
- **Large Hills**: K120-K130 (major championships, Four Hills Tournament)
- **Flying Hills**: K185+ (ski flying competitions)

## Output Format

### Individual Results
- **Participant**: Skier name
- **ID**: FIS athlete ID  
- **Nation**: Country code
- **Race**: Race number within weekend
- **Participation**: Probability of racing (0-1)
- **Win**: Win probability percentage
- **Podium**: Top 3 probability percentage
- **Top5**: Top 5 probability percentage
- **Top10**: Top 10 probability percentage  
- **Top30**: Top 30 probability percentage

### Team Results
- **Nation**: Country/team name
- **Race**: Race number within weekend
- **Participation**: Probability of racing (typically 1.0)
- **Win**: Win probability percentage
- **Podium**: Top 3 probability percentage
- **Top5**: Top 5 probability percentage
- **Top10**: Top 10 probability percentage
- **Top30**: Top 30 probability percentage

## Key Differences from Alpine Skiing

1. **Hill-specific ELO**: Uses hill size categories (Normal/Large/Flying) instead of discipline-specific ELO
2. **Team competitions**: Includes sophisticated team prediction models using averaged athlete ELO ratings
3. **Mixed competitions**: Handles mixed-gender team events
4. **Unified points system**: Same points system across all event types
5. **Race type filtering**: Uses RaceType and HillSize instead of Distance for race categorization

## Model Validation
- Uses Brier score for model evaluation
- Applies period adjustments for trend changes
- Includes fallback models for insufficient data scenarios
- Logs detailed probability sum checks to ensure mathematical consistency