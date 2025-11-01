# Alpine Skiing Weekly Predictions Model Documentation

## Overview

The Alpine skiing weekly predictions system (`weekly-picks2.R`) is a comprehensive statistical modeling framework that generates both **point predictions** and **position probabilities** for weekend races. The system uses ELO ratings, historical performance data, and sophisticated statistical models to predict race outcomes.

## Key Differences from Cross-Country

Unlike cross-country skiing which has multiple points systems (World Cup, Stage, Tour de Ski) and complex optimization, Alpine skiing uses:
- **Single points system**: World Cup points only (100, 80, 60, 50, 45, 40, 36, 32, 29, 26...)
- **Individual races only**: No relay events
- **Discipline-specific ELO ratings**: Separate ratings for Downhill, Super G, Giant Slalom, Slalom, Combined, Tech, Speed

## Main Components

### 1. Race Probability Calculation

**Purpose**: Determines the probability that each athlete will participate in each race based on historical participation patterns.

**Method**:
```r
get_race_probability <- function(chronos, participant, discipline) {
  # Calculate date from 5 years ago
  five_years_ago <- Sys.Date() - (5 * 365)
  
  # Use participant's first race or 5 years ago, whichever is later
  start_date <- max(five_years_ago, participant_first_race)
  
  # Count all races in this discipline since start_date
  all_races <- chronos %>%
    filter(Date >= start_date, Distance == discipline) %>%
    distinct(Date, City)
  
  # Count participant's races in this discipline
  participant_races <- chronos %>%
    filter(Date >= start_date, Skier == participant, Distance == discipline) %>%
    distinct(Date, City)
  
  # Calculate probability (capped at 1.0)
  prob <- min(1, races_participated / total_races)
  return(prob)
}
```

**Key Points**:
- Uses 5-year lookback window
- Discipline-specific (Downhill, Super G, Giant Slalom, Slalom)
- Based on actual participation history, not startlist presence
- For Race1, uses FIS startlist if available (In_Startlist=TRUE → 1.0 probability)

### 2. Points Prediction Models

**Purpose**: Predicts World Cup points each athlete will score in each race.

**Model Architecture**:
1. **Feature Selection**: Uses `regsubsets()` with BIC criterion to select best variables
2. **Model Type**: Generalized Additive Models (GAM) with smooth terms
3. **Fallback Strategy**: Linear models if GAM fails

**Variables by Discipline**:
- **Speed Events** (Downhill, Super G): `Prev_Points_Weighted`, `Downhill_Elo_Pct`, `Super.G_Elo_Pct`, `Giant.Slalom_Elo_Pct`, `Speed_Elo_Pct`, `Elo_Pct`
- **Technical Events** (Slalom, Giant Slalom): `Prev_Points_Weighted`, `Super.G_Elo_Pct`, `Slalom_Elo_Pct`, `Giant.Slalom_Elo_Pct`, `Tech_Elo_Pct`, `Elo_Pct`
- **Combined Events**: `Prev_Points_Weighted`, `Combined_Elo_Pct`, `Tech_Elo_Pct`, `Speed_Elo_Pct`, `Elo_Pct`

**Model Formula Example**:
```r
# Feature selection
exhaustive_selection <- regsubsets(Points ~ Prev_Points_Weighted + Downhill_Elo_Pct + ..., 
                                   data = race_df_75, method = "exhaustive")
best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))

# GAM model with smooth terms
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))
model <- gam(gam_formula, data = race_df_75)
```

### 3. Position Probability Models

**Purpose**: Predicts the probability that each athlete finishes in top-1, top-3, top-5, top-10, and top-30 positions.

**Model Architecture**:
1. **Binary Classification**: Separate GAM model for each threshold using binomial family
2. **Same Variables**: Uses identical feature selection as points models
3. **Period Adjustments**: Accounts for seasonal performance variations

**Position Thresholds**: [1, 3, 5, 10, 30]

**Model Formula**:
```r
# Create binary outcome
race_df$position_achieved <- race_df$Place <= threshold

# Feature selection (same as points model)
pos_formula <- as.formula(paste("position_achieved ~", paste(position_feature_vars, collapse = " + ")))
pos_selection <- regsubsets(pos_formula, data = race_df, method = "exhaustive")

# GAM with binomial family
pos_gam_formula <- as.formula(paste("position_achieved ~", pos_smooth_terms))
position_model <- gam(pos_gam_formula, data = race_df, family = binomial, method = "REML")
```

### 4. Adjustment Mechanisms

**Period Adjustments**:
- Compares athlete's recent performance in current period vs. other periods
- Uses t-test to determine if period effect is statistically significant (p < 0.05)
- Applies period-specific correction to both points and position predictions

**Discipline Adjustments**:
- Similar to period adjustments but for technical vs. speed events
- Uses `Tech_Flag` to categorize disciplines

**Volatility Metrics**:
- `prediction_volatility`: Standard deviation of prediction errors over last 10 races
- `upside_potential`: 90th percentile of prediction errors
- `downside_risk`: 10th percentile of prediction errors
- `confidence_factor`: Based on number of recent races (max 10)

### 5. Position Probability Normalization

**Problem**: Raw position probabilities don't sum to mathematically correct totals.

**Solution**: Custom normalization function that ensures:
- Top-1 probabilities sum to 100% across all participants
- Top-3 probabilities sum to 300% across all participants  
- Top-5 probabilities sum to 500% across all participants
- etc.

**Process**:
1. **Race Probability Adjustment**: Multiply by race participation probability
2. **Scaling**: Calculate scaling factor = target_sum / current_sum
3. **Capping**: Ensure no individual probability exceeds 100%
4. **Redistribution**: Redistribute excess probability proportionally

```r
normalize_position_probabilities <- function(predictions, race_prob_col, position_thresholds) {
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    
    # Apply race probability adjustment
    normalized[[prob_col]] <- normalized[[prob_col]] * normalized[[race_prob_col]]
    
    # Calculate scaling factor
    current_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
    target_sum <- 100 * threshold
    scaling_factor <- target_sum / current_sum
    
    # Apply scaling
    normalized[[prob_col]] <- normalized[[prob_col]] * scaling_factor
    
    # Cap at 100% and redistribute excess
    # [redistribution logic...]
  }
}
```

### 6. Final Predictions Integration

**Points Predictions**:
- Base prediction from GAM model
- Plus period and discipline adjustments
- Multiplied by race participation probability
- Generates: Final_Prediction, Safe_Prediction, Upside_Prediction

**Position Predictions**:
- Separate probability for each threshold (1, 3, 5, 10, 30)
- Normalized to ensure mathematical consistency
- Expressed as percentages

**Output Structure**:
- **Points Excel**: Total expected points across all races with probability weighting
- **Position Excel**: Race-by-race position probabilities for each threshold
- **Top Contenders**: Summary of top 5 athletes for win/podium/top-5 in each race

## Model Training Data

**Historical Scope**: Last 10+ seasons of race results
**Training Set**: Athletes with ELO > 75th percentile (top performers only)
**Cross-Validation**: None explicitly implemented (uses recent historical data)

## Key Strengths

1. **Discipline Specificity**: Separate models and ELO ratings for each Alpine discipline
2. **Participation Modeling**: Realistic race probability based on historical patterns
3. **Mathematical Consistency**: Position probabilities sum correctly across all athletes
4. **Volatility Awareness**: Accounts for athlete consistency/inconsistency patterns
5. **Seasonal Effects**: Period and discipline adjustments for changing conditions

## Current Limitations

1. **No Weather/Conditions**: Models don't account for snow, weather, or course conditions
2. **No Injury Modeling**: Doesn't predict injury risk or recovery patterns
3. **Static Course Difficulty**: Doesn't adjust for venue-specific difficulty
4. **Limited Cross-Validation**: Model validation relies primarily on recent performance

## File Dependencies

- **Input Data**: `weekends.csv`, `{gender}_chrono.csv`, `startlist_weekend_{gender}.csv`
- **Output**: Excel files in `~/blog/daehl-e/content/post/alpine/drafts/weekly-picks/{date}/`
- **Libraries**: `dplyr`, `mgcv`, `leaps`, `openxlsx`, `slider`, `purrr`