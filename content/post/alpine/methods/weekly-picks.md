# Alpine Skiing Weekend Prediction Methodology

## Overview

The Alpine skiing weekend prediction system provides comprehensive forecasts for entire race weekends, combining sophisticated startlist assembly with advanced statistical modeling to predict both race participation probabilities and finishing position probabilities. This methodology operates on a weekend-by-weekend basis, analyzing multiple races occurring across a single competition weekend.

## Data Flow Architecture

### 1. Weekend Race Identification and Processing

**Python Orchestration Script (`startlist-scrape-weekend.py`)**
- **Next Weekend Detection**: Automatically identifies the next race weekend from current date using UTC timezone
- **Weekend Race Aggregation**: Processes all races scheduled for the identified weekend date
- **Multi-Race Coordination**: Handles multiple races per gender occurring on the same weekend
- **Season Boundary Handling**: Includes fallback logic for season-opening races when no current-season data exists

**Weekend Date Logic**
```python
# Find next race date from today (inclusive)
today_utc = datetime.now(timezone.utc).strftime('%Y-%m-%d')
next_weekend_date = find_next_race_date(weekends_df)
weekend_races = filter_races_by_date(weekends_df, next_weekend_date)
```

### 2. Enhanced Startlist Assembly

**Multi-Source Startlist Creation**
- **FIS Integration Priority**: Primary attempt to extract official FIS startlists from race URLs
- **Season Fallback System**: When FIS startlists unavailable, generates comprehensive season startlists from current-season athletes
- **Cross-Season Continuity**: For season-opening weekends, falls back to previous season athlete pool

**Startlist Merging Algorithm**
```python
# Merge startlists across weekend races
consolidated_df = merge_race_dataframes(race1_df, race2_df, probability_column)
# Ensure comprehensive athlete coverage
all_season_athletes = create_season_startlist(current_season_data)
```

**Advanced Athlete Matching**
- **Fuzzy Name Matching**: Handles name variations between FIS entries and internal ELO database
- **Character Normalization**: Processes international characters (ø→oe, ä→ae, etc.) for robust matching
- **Multiple Identity Resolution**: Links FIS codes, athlete IDs, and name variants

### 3. Weekend-Specific Race Probability Modeling

**Historical Participation Analysis**
- **Weekend Patterns**: Analyzes participation patterns across race weekends rather than individual races
- **5-Year Rolling Window**: Uses participant's career start date or 5 years ago (whichever is later) for participation rate calculation
- **Discipline-Specific Rates**: Separate participation probabilities for each alpine discipline within the weekend

**Enhanced Participation Logic**
```r
# Calculate weekend participation probability
get_race_probability <- function(chronos, participant, discipline) {
  start_date <- max(five_years_ago, participant_first_race_date)
  
  all_races <- chronos %>%
    filter(Date >= start_date, Distance == discipline) %>%
    distinct(Date, City)
  
  participant_races <- chronos %>%
    filter(Date >= start_date, Skier == participant, Distance == discipline) %>%
    distinct(Date, City)
  
  participation_rate <- min(1, nrow(participant_races) / nrow(all_races))
}
```

**FIS Startlist Integration**
- **Confirmed Participation**: Athletes on official FIS startlists receive 100% participation probability for that specific race
- **Calculated Participation**: Non-startlist athletes receive historical participation probability estimates
- **Dynamic Probability Columns**: Creates `Race1_Prob`, `Race2_Prob`, etc. for each race in the weekend

## Advanced Statistical Framework

### 1. Multi-Race Feature Engineering

**Weekend-Aware Metrics**
- **Cross-Race ELO Consistency**: Tracks discipline-specific ELO ratings across multiple weekend races
- **Weekend Performance Patterns**: Analyzes historical performance in multi-race weekends
- **Fatigue and Scheduling Effects**: Considers race-order impacts on performance

**Enhanced Temporal Features**
- **Period Progression**: Maps races within 4-season periods:
  - Period 1: Races 1-8 (October-November, Early season)
  - Period 2: Races 9-16 (December-January, Mid season)
  - Period 3: Races 17-24 (February, Late season)  
  - Period 4: Races 25+ (March, Final season)

### 2. Discipline-Adaptive Model Selection

**Dynamic Variable Selection by Event Type**

**Speed Events (Downhill/Super-G)**
```r
explanatory_vars <- c("Prev_Points_Weighted", 
                      "Downhill_Elo_Pct", "Super.G_Elo_Pct", 
                      "Giant.Slalom_Elo_Pct", "Speed_Elo_Pct", "Elo_Pct")
```

**Technical Events (Slalom/Giant Slalom)**
```r
explanatory_vars <- c("Prev_Points_Weighted", "Super.G_Elo_Pct",
                      "Slalom_Elo_Pct", "Giant.Slalom_Elo_Pct", 
                      "Tech_Elo_Pct", "Elo_Pct")
```

**Combined Events**
```r
explanatory_vars <- c("Prev_Points_Weighted", 
                      "Combined_Elo_Pct", "Tech_Elo_Pct", 
                      "Speed_Elo_Pct", "Elo_Pct")
```

### 3. Hierarchical Model Architecture

**Primary GAM Framework**
- **Exhaustive Feature Selection**: BIC-optimized variable selection using `regsubsets`
- **Smooth Non-Linear Terms**: Spline smoothers `s(variable)` capture complex relationships
- **Binomial Position Models**: Separate logistic GAMs for each position threshold [1, 3, 5, 10, 30]

**Robust Fallback Hierarchy**
1. **Full GAM**: Complete feature set with optimal smooth terms
2. **Reduced GAM**: k=3 basis functions for computational stability
3. **Linear Regression**: Linear terms only when GAM fails
4. **Simple ELO Model**: Ultimate fallback using discipline-specific ELO only

### 4. Advanced Position Probability Modeling

**Multi-Threshold Binary Classification**
```r
# Create position achievement indicators
race_df$position_achieved <- race_df$Place <= threshold

# Fit binomial GAM for each threshold
position_model <- gam(position_achieved ~ s(selected_variables),
                      data = race_df, 
                      family = binomial,
                      method = "REML")
```

**Model Performance Evaluation**
- **Brier Score Calculation**: Measures probabilistic prediction accuracy
- **Cross-Validation**: Ensures robust out-of-sample performance
- **Variable Importance Tracking**: Logs selected variables for each position threshold

## Sophisticated Statistical Adjustments

### 1. Multi-Dimensional Contextual Corrections

**Period Effect Analysis**
```r
period_p <- t.test(prior_period_curr, prior_period_other)$p.value
period_correction <- ifelse(period_p < 0.05,
                           mean(prob_diff[Period == Period], na.rm = TRUE),
                           0)
```

**Discipline Specialization Effects**
```r
discipline_p <- t.test(prior_tech_curr, prior_tech_other)$p.value  
discipline_correction <- ifelse(discipline_p < 0.05,
                               mean(prob_diff[Tech_Flag == Tech_Flag], na.rm = TRUE),
                               0)
```

### 2. Advanced Volatility and Risk Assessment

**Rolling Performance Windows (10-Race Lookback)**
```r
# Volatility metrics using slider package
recent_prediction_volatility <- slider::slide_dbl(
  Points - Initial_Prediction,
  sd,
  .before = 9,  # Current plus 9 previous races
  .complete = FALSE
)

recent_upside_potential <- slider::slide_dbl(
  Points - Initial_Prediction,
  ~quantile(.x, 0.9, na.rm = TRUE),
  .before = 9,
  .complete = FALSE
)
```

**Risk-Adjusted Prediction Scenarios**
- **Base Prediction**: Central tendency estimate
- **Safe Prediction**: Downside-adjusted estimate for conservative betting
- **Upside Prediction**: Optimistic scenario incorporating positive volatility
- **Confidence Weighting**: Scales adjustments by recent race sample size

### 3. Enhanced Monotonic Constraints and Normalization

**Weekend-Integrated Probability Constraints**
```
P(Win) ≤ P(Podium) ≤ P(Top-5) ≤ P(Top-10) ≤ P(Top-30)
```

**Multi-Race Normalization Framework**
```r
# Participation-adjusted normalization
adjusted_prob = base_position_prob * race_participation_prob

# Target sum maintenance after constraints
target_sum <- 100 * threshold  # 100% for win, 300% for podium, etc.
scaling_factor <- target_sum / current_sum
normalized_prob <- adjusted_prob * scaling_factor
```

**Excess Probability Redistribution**
```r
# Cap individual probabilities at 100%
excess <- sum(normalized_prob[over_hundred] - 100)
normalized_prob[over_hundred] <- 100

# Redistribute excess proportionally to under-100% athletes
if(under_sum > 0 && excess > 0) {
  redistrib_factor <- (under_sum + excess) / under_sum
  normalized_prob[under_hundred] <- normalized_prob[under_hundred] * redistrib_factor
}
```

## Weekend-Specific Implementation Architecture

### 1. Multi-Race Processing Coordination

**Race-by-Race Model Training**
- Each weekend race receives independent model training
- Discipline-specific feature selection per race
- Cross-race consistency validation

**Probability Column Management**
```r
# Dynamic probability column creation
all_prob_columns <- paste0("Race", 1:total_weekend_races, "_Prob")

# Comprehensive athlete coverage
consolidated_df <- merge_race_dataframes(race1_df, race2_df, prob_column)
all_season_athletes <- create_season_startlist(...)
final_startlist <- rbind(consolidated_df, missing_athletes)
```

### 2. Weekend Aggregation and Scoring

**Participation-Weighted Point Expectations**
```r
# Weekend total calculation
Total_Points = Race1_Points * Race1_Probability + Race2_Points * Race2_Probability

# Risk-adjusted scenarios
Total_Safe = Race1_Safe * Race1_Probability + Race2_Safe * Race2_Probability
Total_Upside = Race1_Upside * Race1_Probability + Race2_Upside * Race2_Probability
```

**Comprehensive Output Generation**
- **Individual Race Sheets**: Separate predictions for each weekend race
- **Weekend Summary**: Combined rankings across all races
- **Position Probability Matrices**: Win/Podium/Top-5 probabilities by race
- **Top Contenders Analysis**: Focused rankings for key betting markets

### 3. Weekend-Optimized File Organization

**Date-Based Directory Structure**
```r
weekend_folder <- format(next_weekend_date, "%Y%m%d")
dir_path <- paste0("~/blog/daehl-e/content/post/alpine/drafts/weekly-picks/", 
                   weekend_folder)
```

**Multi-Format Output Generation**
- **Excel Workbooks**: Multi-sheet files with race-specific tabs
- **CSV Exports**: Machine-readable data for downstream analysis
- **Formatted Summaries**: Human-readable top contender lists

## Key Methodological Innovations

### 1. Weekend-Aware Temporal Modeling
Recognizes that alpine skiing operates on weekend competition cycles rather than individual races

### 2. Multi-Source Startlist Intelligence  
Combines official FIS startlists with comprehensive season athlete pools for robust coverage

### 3. Discipline-Adaptive Feature Selection
Dynamically selects optimal predictor variables based on specific alpine disciplines

### 4. Hierarchical Robustness Framework
Ensures prediction generation even with limited data through systematic model fallback

### 5. Participation-Integrated Position Modeling
Seamlessly combines startlist uncertainty with performance prediction for realistic weekend forecasts

### 6. Advanced Risk Scenario Generation
Provides multiple prediction scenarios (safe/base/upside) for different decision-making contexts

### 7. Cross-Race Consistency Validation
Maintains logical probability relationships across multiple races within the same weekend

This methodology represents a comprehensive approach to weekend alpine skiing prediction that balances statistical sophistication with practical applicability for multi-race weekend forecasting and analysis.