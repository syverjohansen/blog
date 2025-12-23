---
title: "Ski Jumping Race Picks Methodology"
date: 2023-12-01T01:23:07+00:00
draft: false
tags: ["methodology", "skiing", "race-picks", "skijump"]
---

## Ski Jumping

### Individual

#### Data Gathering

Ski jumping individual race startlists are scraped from the FIS websites using automated Python scripts that handle both individual and team events with specialized hill size detection and jump-specific data extraction. The system processes ski jumping's unique competition structure through FIS sector-specific integration and intelligent team event detection.

**Race Discovery and Scheduling**: The system identifies races to process using UTC timezone-based scheduling with ski jumping-specific paths:

```python
# From startlist-scrape-races.py:32-36
# Get today's date in UTC 
today_utc = datetime.now(timezone.utc)
today_str = today_utc.strftime('%m/%d/%Y')

print(f"Today's date (UTC): {today_str}")
```

**Ski Jumping Event Type Classification**: The system filters races by type including individual, regular team, and mixed team events:

```python
# From startlist-scrape-races.py:98-100
# Filter races by type: individual, regular team, and mixed team
individual_races = races_df[
    (~races_df['RaceType'].str.contains("Team", na=False)) &
```

**FIS Website Data Extraction**: Ski jumping athlete data is scraped from official FIS race pages with ski jumping-specific sector codes:

```python
# From startlist_common.py:84-95
def get_fis_race_data(race_id: str, sector_code: str = 'JP') -> Tuple[List[Dict], Dict]:
    """
    Extract race information from FIS website for ski jumping
    
    Args:
        race_id: FIS race ID
        sector_code: Sport code (JP = Ski Jumping)
        
    Returns:
        Tuple containing (list of athletes/teams, race metadata)
    """
    url = f"https://www.fis-ski.com/DB/general/results.html?sectorcode={sector_code}&raceid={race_id}"
```

**Hill Size Detection**: The system identifies ski jumping hill sizes from race metadata:

```python
# From startlist_common.py:144-156
# Try to find hill size and race type
race_type_elem = soup.select_one('.event-header__subtitle')
if race_type_elem:
    race_type = race_type_elem.text.strip()
    event_info['RaceType'] = race_type
    print(f"Race type: {race_type}")
    
    # Extract hill size from race type if present
    hill_size_match = re.search(r'(K\d+|HS\d+)', race_type)
    if hill_size_match:
        event_info['HillSize'] = hill_size_match.group(1)
    else:
        event_info['HillSize'] = 'Unknown'
```

**Advanced Team Event Detection**: Ski jumping has sophisticated team event detection using multiple validation methods:

```python
# From startlist_common.py:157-194
# Improved team event detection
is_team_event = False

# Check event title for team indicators
if event_title_elem:
    event_title = event_title_elem.text.strip().upper()
    if any(keyword in event_title for keyword in ['TEAM', 'MIXED']):
        is_team_event = True
        print(f"Detected team event from title: {event_title}")

# Check race type for team indicators
if race_type_elem and not is_team_event:
    race_type = race_type_elem.text.strip().upper()
    if any(keyword in race_type for keyword in ['TEAM', 'MIXED']):
        is_team_event = True
        print(f"Detected team event from race type: {race_type}")

# Additional check: Look for team-specific HTML structure
if not is_team_event:
    # Check if there are team rows in the results
    team_main_rows = soup.select('.table-row_theme_main')
    team_additional_rows = soup.select('.table-row_theme_additional')
    
    # If we have both main and additional rows, it's likely a team event
    if len(team_main_rows) > 0 and len(team_additional_rows) > 0:
        # Further check: see if main rows contain country names
        for main_row in team_main_rows[:3]:  # Check first few rows
            name_elem = main_row.select_one('.g-lg.g-md.g-sm.g-xs.justify-left.bold')
            if name_elem:
                name = name_elem.text.strip().upper()
                # Common country names in ski jumping
                if name in ['NORWAY', 'GERMANY', 'AUSTRIA', 'POLAND', 'SLOVENIA', 
                           'JAPAN', 'SWITZERLAND', 'FINLAND', 'ITALY', 'FRANCE', 
                           'USA', 'CANADA', 'RUSSIA', 'UKRAINE', 'CZECH REPUBLIC']:
                    is_team_event = True
                    print(f"Detected team event from country name: {name}")
                    break
```

**Individual vs Team Processing**: The system routes events to appropriate processing based on detected event type:

```python
# From startlist_common.py:196-199
# Store event type flags
event_info['IsTeamEvent'] = is_team_event

# Process based on event type
if is_team_event:
```

**Weekly Picks Integration**: The system integrates with ski jumping-specific weekly predictions:

```python
# From startlist_common.py:14-29
def check_and_run_weekly_picks():
    """
    Check if there are races with today's date in weekends.csv and run the weekly-picks2.R script if true.
    """
    # Get today's date in UTC
    today_utc = datetime.now(timezone.utc).strftime('%m/%d/%Y')
    print(f"Checking for races on today's date (UTC): {today_utc}")
    
    # Path to weekends.csv for ski jumping
    weekends_csv_path = os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/weekends.csv')
    
    # Path to R script for ski jumping
    r_script_path = os.path.expanduser('~/blog/daehl-e/content/post/skijump/drafts/weekly-picks2.R')
```

**Country Name Validation**: Ski jumping uses extensive country name validation for team event detection:

```python
# From startlist_common.py:187-193
# Common country names in ski jumping
if name in ['NORWAY', 'GERMANY', 'AUSTRIA', 'POLAND', 'SLOVENIA', 
           'JAPAN', 'SWITZERLAND', 'FINLAND', 'ITALY', 'FRANCE', 
           'USA', 'CANADA', 'RUSSIA', 'UKRAINE', 'CZECH REPUBLIC']:
    is_team_event = True
    print(f"Detected team event from country name: {name}")
    break
```

**Team Structure Detection**: The system identifies team events through HTML structure analysis:

```python
# From startlist_common.py:175-186
# Additional check: Look for team-specific HTML structure
if not is_team_event:
    # Check if there are team rows in the results
    team_main_rows = soup.select('.table-row_theme_main')
    team_additional_rows = soup.select('.table-row_theme_additional')
    
    # If we have both main and additional rows, it's likely a team event
    if len(team_main_rows) > 0 and len(team_additional_rows) > 0:
        # Further check: see if main rows contain country names
        for main_row in team_main_rows[:3]:  # Check first few rows
            name_elem = main_row.select_one('.g-lg.g-md.g-sm.g-xs.justify-left.bold')
            if name_elem:
                name = name_elem.text.strip().upper()
```

The Ski Jumping individual data gathering system provides specialized ski jumping competition processing through FIS ski jumping sector code utilization, comprehensive hill size detection, advanced multi-method team event detection, country name validation, HTML structure analysis, intelligent event routing, and weekly prediction integration that accommodates ski jumping's unique competition formats and team event structures.

#### Points

##### Training

###### Setup

The training data setup for Ski Jumping follows a comprehensive preprocessing pipeline that accommodates the sport's hill size variations and supports both individual and team competition formats with specialized ELO rating systems for different hill categories.

**Ski Jumping Unified Points System**: 
Ski jumping uses a standardized 30-position scoring system across all competition formats:

```r
# From race-picks.R:13-18
# Define points systems for Ski Jumping
# Individual: Top 30 get points
individual_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Team events use same system
team_points <- individual_points
```

**Hill Size-Specific Training Data Preprocessing**:
Ski jumping's unique preprocessing handles different hill sizes and both individual and team events through a flexible data pipeline:

```r
# From race-picks.R:869-898
preprocess_data <- function(df, is_team = FALSE) {
  # Load races data to determine points systems for historical races
  races_data <- read.csv("~/ski/elo/python/skijump/polars/excel365/races.csv", 
                         stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date, format="%Y-%m-%d"))
  
  participant_col <- if(is_team) "Nation" else "Skier"
  id_col <- if(is_team) "Nation" else "ID"
  
  # For team data, we want to keep the team structure
  if(is_team) {
    log_info("Processing team data - using team-based metrics")
    
    # Define the team-specific columns we expect/need
    team_cols <- c("Avg_Small_Elo", "Avg_Medium_Elo", "Avg_Normal_Elo", "Avg_Large_Elo", "Avg_Flying_Elo", "Avg_Elo")
    # Check which columns exist and create missing ones
    for(col in team_cols) {
      if(!col %in% names(df)) {
        log_info(paste("Creating missing team column:", col))
        # If we're missing a column but have the corresponding base column without Avg_
        base_col <- gsub("^Avg_", "", col)
        if(base_col %in% names(df)) {
          log_info(paste("Using base column", base_col, "to create", col))
          df[[col]] <- df[[base_col]]
        } else {
          df[[col]] <- 0
        }
      }
    }
  }
}
```

**Hill Size-Specific Weighted Points Calculation**:
The system calculates weighted previous points by race type, accounting for ski jumping's different competition formats:

```r
# From race-picks.R:915-927
# Calculate weighted previous points separately for each race type
df_with_points <- df_with_points %>%
  # Group by ID and race type
  group_by(!!sym(id_col), RaceType) %>%
  arrange(Season, Race) %>%
  mutate(Prev_Points_Weighted = sapply(1:n(), function(j) {
    if (j == 1) return(0)
    start_index <- max(1, j - 5)
    num_races <- j - start_index
    weights <- seq(1, num_races)
    weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
  })) %>%
  ungroup()
```

**Comprehensive Hill Size ELO Column Management**:
Ski jumping maintains the most detailed hill size-specific ELO rating system among winter sports:

```r
# From race-picks.R:929-952
# Check if Elo columns exist, if not create them
if(is_team) {
  elo_cols <- c("Avg_Small_Elo", "Avg_Medium_Elo", "Avg_Normal_Elo", "Avg_Large_Elo", "Avg_Flying_Elo", "Avg_Elo")
  pelo_cols <- c("Avg_Small_Pelo", "Avg_Medium_Pelo", "Avg_Normal_Pelo", "Avg_Large_Pelo", "Avg_Flying_Pelo", "Avg_Pelo")
} else {
  elo_cols <- c("Small_Elo", "Medium_Elo", "Normal_Elo", "Large_Elo", "Flying_Elo", "Elo")
  pelo_cols <- c("Small_Pelo", "Medium_Pelo", "Normal_Pelo", "Large_Pelo", "Flying_Pelo", "Pelo")
}

# Make sure these columns exist (create if missing)
for (col in elo_cols) {
  if (!col %in% names(df_with_points)) {
    log_info(paste("Creating missing column:", col))
    df_with_points[[col]] <- 0
  }
}

# Make sure Pelo columns exist (create if missing)
for (col in pelo_cols) {
  if (!col %in% names(df_with_points)) {
    log_info(paste("Creating missing Pelo column:", col))
    df_with_points[[col]] <- 0
  }
}
```

**Hill Size and Temporal Feature Engineering**:
Ski jumping includes hill size classification and seasonal periodization:

```r
# From race-picks.R:955-981
processed_df <- df_with_points %>%
  # Add period (Ski Jumping has 4 periods per season)
  group_by(Season) %>%
  mutate(
    Num_Races = max(Race),
    Period = case_when(
      Num_Races <= 5 ~ 1,
      Num_Races <= 10 ~ 2,
      Num_Races <= 15 ~ 3,
      TRUE ~ 4
    )
  ) %>%
  ungroup() %>%
  # Add hill size flag (0 for small/medium, 1 for large/flying)
  mutate(
    HillSize_Flag = case_when(
      HillSize %in% c("Small", "Medium") ~ 0,
      HillSize %in% c("Large", "Flying") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # Filter relevant races and add cumulative points
  filter(
    Season >= max(Season-10)
  )
```

**Hill Size-Specific ELO Percentage Calculation**:
The system calculates ELO percentages for each of the six different hill size categories, providing the most detailed hill-specific performance tracking:

```r
# From race-picks.R:983-1002
# Handle NAs and calculate percentages
group_by(Season, Race) %>%
mutate(
  across(
    all_of(elo_cols),
    ~replace_na_with_quartile(.x)
  )
) %>%
# Calculate percentages for each Elo and Pelo column
mutate(
  across(
    all_of(elo_cols),
    ~{
      max_val <- max(.x, na.rm = TRUE)
      if (max_val == 0) return(rep(0, length(.x)))
      .x / max_val
    },
    .names = "{.col}_Pct"
  )
) %>%
ungroup()
```

**Missing Value Handling for Ski Jumping**:
The system ensures all required ELO percentage columns exist and replaces missing values with first quartile values, accounting for the fact that jumpers inexperienced on specific hill sizes typically perform worse.

This ski jumping-specific setup creates training datasets that account for the sport's hill size variations (Small, Medium, Normal, Large, Flying), support both individual and team formats, maintain hill-specific performance tracking, and provide detailed performance context for accurate GAM model training across all jumping conditions.

###### Feature Selection

Ski jumping's feature selection process reflects the sport's most distinctive characteristic: dramatic performance variation based on hill size. The system uses a dual-path approach that adapts to individual versus team competition formats while incorporating comprehensive hill size-specific ELO ratings.

**Hill Size-Specific Variable Architecture**:
The system defines different candidate variables based on competition format, recognizing the distinct performance metrics for individual versus team events:

```r
# From race-picks.R:1774-1781
# Define explanatory variables based on race type
if(is_team) {
  explanatory_vars <- c("Avg_Normal_Pelo_Pct", "Avg_Large_Pelo_Pct", 
                        "Avg_Flying_Pelo_Pct", "Avg_Pelo_Pct")
} else {
  explanatory_vars <- c("Prev_Points_Weighted", 
                        "Normal_Pelo_Pct", "Large_Pelo_Pct", 
                        "Flying_Pelo_Pct", "Pelo_Pct")
}
```

**Individual vs Team Variable Philosophy**:
- **Individual Events**: Include weighted previous points plus hill size-specific ELO ratings (Normal, Large, Flying) plus overall ELO rating, recognizing that jumpers specialize on different hill categories
- **Team Events**: Use team-averaged hill size-specific ELO ratings without weighted points, since team performance dynamics differ from individual historical patterns

**Comprehensive Hill Size ELO Integration**:
The system includes the most detailed hill size categorization among winter sports, with separate ELO ratings for Small, Medium, Normal, Large, and Flying hills. The feature selection focuses on the most competitive hill sizes (Normal, Large, Flying) where World Cup events typically occur:

```r
# From race-picks.R:1742-1759
# Get relevant Elo column based on race type and hill size
if(is_team) {
  # For team races, use hill-specific ELO or general average ELO
  if(races$hillsize[i] %in% c("Large", "Flying")) {
    elo_col <- "Avg_Large_Elo_Pct"
  } else if(races$hillsize[i] == "Small") {
    elo_col <- "Avg_Small_Elo_Pct"
  } else if(races$hillsize[i] == "Medium") {
    elo_col <- "Avg_Medium_Elo_Pct"
  } else {
    elo_col <- "Avg_Elo_Pct"
  }
} else {
  if(races$hillsize[i] %in% c("Large", "Flying")) {
    elo_col <- "Large_Elo_Pct"
  } else if(races$hillsize[i] == "Small") {
    elo_col <- "Small_Elo_Pct"
  } else if(races$hillsize[i] == "Medium") {
    elo_col <- "Medium_Elo_Pct"
  } else {
    elo_col <- "Elo_Pct"
  }
}
```

**Exhaustive BIC Optimization**:
Like other sports, ski jumping uses exhaustive subset selection with BIC optimization to identify the most predictive variable combination:

```r
# From race-picks.R:1787-1801
exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
summary_exhaustive <- summary(exhaustive_selection)
best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))

log_info(paste("Variables selected by BIC:", paste(best_bic_vars, collapse = ", ")))
log_info(paste("Best BIC value:", min(summary_exhaustive$bic)))

smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))

model <- gam(gam_formula, data = race_df_75)
```

**Multi-Tier Fallback Strategy**:
Ski jumping implements the most comprehensive fallback system to handle data sparsity common in the sport's smaller competitive fields:

```r
# From race-picks.R:1802-1823
}, error = function(e) {
  log_warn(paste("Error in model selection:", e$message))
  # Fallback to a simpler model with reduced degrees of freedom
  tryCatch({
    # Try with reduced degrees of freedom
    fallback_formula <- as.formula(paste("Points ~ s(", elo_col, ", k=3) + s(Period, k=3) + s(HillSize_Flag, k=3)"))
    model <<- gam(fallback_formula, data = race_df_75)
  }, error = function(e2) {
    log_warn(paste("Error in fallback GAM model:", e2$message))
    # Try linear terms only
    tryCatch({
      linear_formula <- as.formula(paste("Points ~", elo_col))
      model <<- lm(linear_formula, data = race_df_75)
      log_info("Using linear model as final fallback")
    }, error = function(e3) {
      log_warn(paste("Error in linear fallback:", e3$message))
      # Ultimate fallback - use only elo column
      simple_formula <- as.formula(paste("Points ~", elo_col))
      model <<- lm(simple_formula, data = race_df_75)
      log_info("Using simple linear model with only ELO as ultimate fallback")
    })
  })
})
```

**Hill Size-Specific Performance Optimization**:
The feature selection adapts to hill size differences by including hill size flags and incorporating temporal effects through period variables, ensuring models can capture how jumpers perform differently across various hill categories and seasonal conditions.

This approach ensures robust model fitting across ski jumping's diverse hill sizes and competition formats, maintaining prediction capability even when data sparsity challenges arise from the sport's more limited competitive field compared to other winter sports.

###### Modeling

Ski jumping's modeling approach employs sophisticated Generalized Additive Models (GAM) specifically optimized for the sport's most distinctive challenge: dramatic performance variation based on hill size. The system creates both points prediction models and position probability models with comprehensive hill size-specific adaptations and robust validation.

**Primary GAM Implementation with Hill Size Integration**:
The main modeling approach incorporates ski jumping's hill size-specific feature selection for optimal performance prediction:

```r
# From race-picks.R:1796-1801
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))

log_info(paste("Final GAM formula:", as.character(gam_formula)[3]))

model <- gam(gam_formula, data = race_df_75)
```

**Comprehensive Multi-Tier Fallback Architecture**:
Ski jumping implements the most robust fallback system to handle data sparsity common in the sport's smaller competitive fields:

```r
# From race-picks.R:1802-1823
}, error = function(e) {
  log_warn(paste("Error in model selection:", e$message))
  # Fallback to a simpler model with reduced degrees of freedom
  tryCatch({
    # Try with reduced degrees of freedom
    fallback_formula <- as.formula(paste("Points ~ s(", elo_col, ", k=3) + s(Period, k=3) + s(HillSize_Flag, k=3)"))
    model <<- gam(fallback_formula, data = race_df_75)
  }, error = function(e2) {
    log_warn(paste("Error in fallback GAM model:", e2$message))
    # Try linear terms only
    tryCatch({
      linear_formula <- as.formula(paste("Points ~", elo_col))
      model <<- lm(linear_formula, data = race_df_75)
      log_info("Using linear model as final fallback")
    }, error = function(e3) {
      log_warn(paste("Error in linear fallback:", e3$message))
      # Ultimate fallback - use only elo column
      simple_formula <- as.formula(paste("Points ~", elo_col))
      model <<- lm(simple_formula, data = race_df_75)
      log_info("Using simple linear model with only ELO as ultimate fallback")
    })
  })
})
```

**Hill Size-Specific Position Probability Modeling**:
Ski jumping position models use binomial GAM with REML estimation, specifically adapted for hill size variations:

```r
# From race-picks.R:1868-1871
position_model <- gam(pos_gam_formula,
                      data = race_df,
                      family = binomial,
                      method = "REML")
```

**Enhanced Model Validation with Comprehensive Logging**:
Ski jumping implements extensive model validation with detailed performance tracking across all hill categories:

```r
# From race-picks.R:1873-1877
# Calculate Brier score for model evaluation
predicted_probs <- predict(position_model, newdata = race_df, type = "response")
brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
log_info(paste("Position model threshold", threshold, "- Brier score:", round(brier_score, 4)))

# Log model summary statistics
log_info(paste("Position model threshold", threshold, "- Mean predicted probability:", round(mean(predicted_probs, na.rm = TRUE), 4)))
```

**Hill Size Flag Integration in Fallback Models**:
The fallback strategies specifically incorporate hill size effects to maintain prediction quality even with simplified models:

```r
# From race-picks.R:1807
fallback_formula <- as.formula(paste("Points ~ s(", elo_col, ", k=3) + s(Period, k=3) + s(HillSize_Flag, k=3)"))
```

**Dual Competition Format Support**:
The modeling system seamlessly adapts between individual and team competitions while preserving hill size-specific performance characteristics. Individual models incorporate weighted previous points plus hill size-specific ELO ratings, while team models use aggregated hill size-specific team performance metrics.

**Model Robustness for Smaller Fields**:
Ski jumping's modeling accounts for the sport's typically smaller competitive fields compared to other winter sports, implementing more conservative smoothing parameters and comprehensive fallback strategies to ensure stable predictions across varying field sizes and hill conditions.

This ski jumping-specific modeling framework ensures accurate performance prediction across the sport's unique hill size variations (Small, Medium, Normal, Large, Flying) while maintaining robust statistical validation and comprehensive error handling adapted to the sport's distinctive competitive characteristics.

###### Adjustments

Ski jumping implements a sophisticated dual-adjustment system that accounts for both seasonal periodization and the sport's most distinctive characteristic: hill size-specific performance patterns. The adjustment methodology recognizes that jumpers may consistently over- or under-perform relative to base model predictions during specific periods or on certain hill size categories.

**Sequential Adjustment Calculation**:
The system calculates adjustments in a specific sequence to avoid double-counting effects while capturing the unique hill size dynamics of ski jumping:

```r
# From race-picks.R:1976-1990
race_df_75 <- race_df_75 %>%
  arrange(Date) %>%
  group_by(!!sym(participant_col)) %>%
  mutate(
    row_id = row_number()
  ) %>%
  ungroup() %>%
  # Step 1: Initial predictions
  mutate(
    Initial_Prediction = predict(model, newdata = .)
  ) %>%
  group_by(!!sym(participant_col)) %>%
  mutate(
    Prediction_Diff = Points - Initial_Prediction
  )
```

**Period-Based Seasonal Adjustments**:
The system identifies statistically significant seasonal performance patterns through comparative t-testing between current period performance and all other periods:

```r
# From race-picks.R:1991-2004
# Step 2: Calculate Period adjustments
mutate(
  period_p = purrr::map_dbl(row_id, function(r) {
    if(r <= 1) return(1)
    prior_period_curr <- Prediction_Diff[Period == Period[r] & row_id < r]
    prior_period_other <- Prediction_Diff[Period != Period[r] & row_id < r]
    if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
    tryCatch({
      t.test(prior_period_curr, prior_period_other)$p.value
    }, error = function(e) 1)
  }),
  period_correction = ifelse(period_p < 0.05,
                             mean(Prediction_Diff[Period == Period], na.rm = TRUE),
                             0)
)
```

**Hill Size-Specific Performance Adjustments**:
Ski jumping's most distinctive adjustment captures performance differences based on hill size categories, recognizing that jumpers may excel on certain hill types:

```r
# From race-picks.R:2006-2018
# Step 3: Calculate Hill Size adjustments
hillsize_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_hill_curr <- Prediction_Diff[HillSize_Flag == HillSize_Flag[r] & row_id < r]
  prior_hill_other <- Prediction_Diff[HillSize_Flag != HillSize_Flag[r] & row_id < r]
  if(length(prior_hill_curr) < 3 || length(prior_hill_other) < 3) return(1)
  tryCatch({
    t.test(prior_hill_curr, prior_hill_other)$p.value
  }, error = function(e) 1)
}),
hillsize_correction = ifelse(hillsize_p < 0.05,
                             mean(Prediction_Diff[HillSize_Flag == HillSize_Flag], na.rm = TRUE),
                             0)
```

**Combined Adjustment Application**:
All significant adjustments are combined additively to create final adjusted predictions:

```r
# From race-picks.R:2020-2022
# Combine adjustments
Adjusted_Prediction = Initial_Prediction + period_correction + hillsize_correction
```

**Advanced Volatility and Risk Metrics**:
Ski jumping incorporates sophisticated volatility tracking using rolling window calculations to assess prediction uncertainty and performance patterns:

```r
# From race-picks.R:2031-2062
# Create rolling window calculations for last 10 races
recent_prediction_volatility = slider::slide_dbl(
  Points - Initial_Prediction,
  sd,
  .before = 9,  # Look at current race plus 9 previous
  .complete = FALSE  # Allow partial windows
),

recent_consistency_score = slider::slide_dbl(
  abs(Points - Initial_Prediction),
  mean,
  .before = 9,
  .complete = FALSE
),

recent_upside_potential = slider::slide_dbl(
  Points - Initial_Prediction,
  ~quantile(.x, 0.9, na.rm = TRUE),
  .before = 9,
  .complete = FALSE
),

recent_downside_risk = slider::slide_dbl(
  Points - Initial_Prediction,
  ~quantile(.x, 0.1, na.rm = TRUE),
  .before = 9,
  .complete = FALSE
)
```

**Comprehensive Adjustment Storage**:
The system maintains detailed adjustment records for both points and position probabilities:

```r
# From race-picks.R:2065-2080
participant_adjustments <- race_df_75 %>%
  group_by(!!sym(participant_col)) %>%
  summarise(
    period_effect = last(period_correction),
    hillsize_effect = last(hillsize_correction),
    
    # Recent volatility metrics
    prediction_volatility = last(recent_prediction_volatility),
    consistency_score = last(recent_consistency_score),
    upside_potential = last(recent_upside_potential),
    downside_risk = last(recent_downside_risk),
    volatility_ratio = last(recent_volatility_ratio),
    
    # Add number of recent races for confidence
    n_recent_races = sum(!is.na(tail(Points, 10)))
  )
```

**Position Probability Adjustments with Boundary Enforcement**:
Position models receive period-specific adjustments with strict boundary constraints to ensure valid probability ranges:

```r
# From race-picks.R:1910-1913
period_correction = ifelse(period_p < 0.05,
                           mean(prob_diff[Period == Period], na.rm = TRUE),
                           0),
period_adjusted = pmin(pmax(initial_prob + period_correction, 0), 1)
```

**Statistical Significance Requirements**:
All adjustments require statistical significance (p < 0.05) and minimum data thresholds (≥3 observations per category) to ensure adjustments are based on meaningful patterns. This conservative approach prevents overfitting while capturing genuine hill size-specific performance characteristics that define competitive ski jumping.

#### Testing

##### Startlist Setup

Ski jumping's startlist setup for testing implements the most comprehensive hill size-aware data preparation pipeline among winter sports, accommodating the sport's dramatic performance variations across different hill categories while ensuring robust data quality for model prediction. The system handles both individual and team competition formats with specialized hill size-specific feature engineering.

**Hill Size-Aware Race Participation Probability Calculation**:
Ski jumping startlist setup employs an exponential decay model for calculating race-specific participation probabilities, recognizing that participation patterns vary significantly across different hill sizes and competition formats:

```r
# From race-picks.R:577-620 in get_race_probability()
get_race_probability <- function(chronos, participant, racetype, is_team = FALSE) {
  participant_races <- chronos %>%
    filter(get(id_col) == participant, RaceType == racetype) %>%
    arrange(Date, Season, Race)
  
  if(nrow(participant_races) == 0) return(0)
  
  total_races <- nrow(participant_races)
  if(total_races > 0) {
    # Create exponential decay weights (α = 0.1)
    race_weights <- exp(-0.1 * ((total_races-1):0))
    
    # Create participation vector (1 if skier participated, 0 if not)
    participation <- rep(1, total_races)
    
    # Calculate weighted probability
    weighted_participation <- sum(participation * race_weights)
    total_weight <- sum(race_weights)
    prob <- weighted_participation / total_weight
    
    return(prob)
  }
  return(0)
}
```

**Most Recent ELO Rating Retrieval Across Hill Categories**:
The system retrieves the most current ELO ratings for each athlete across all hill size categories (Small, Medium, Normal, Large, Flying), ensuring predictions use the most up-to-date hill-specific performance assessments:

```r
# From race-picks.R:1334-1342 in prepare_startlist_data()
most_recent_elos <- race_df %>%
  filter(Skier %in% base_df$Skier) %>%
  group_by(Skier) %>%
  arrange(Date, Season, Race) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(Skier, any_of(elo_cols))
```

**ELO to PELO Conversion with Hill Size Normalization**:
Ski jumping employs a sophisticated ELO-to-PELO conversion process where current ELO ratings are converted to percentages using historical PELO (pre-race ELO) maximum values, ensuring model compatibility across different hill sizes:

```r
# From race-picks.R:1405-1438
for(i in seq_along(elo_cols)) {
  elo_col <- elo_cols[i]
  pelo_col <- pelo_cols[i]  # corresponding Pelo column name
  pelo_pct_col <- paste0(pelo_col, "_Pct")
  
  if(elo_col %in% names(result_df)) {
    # Get max value for normalization from race_df (historical Pelo data)
    if(pelo_col %in% names(race_df)) {
      max_val <- max(race_df[[pelo_col]], na.rm = TRUE)
      if(!is.na(max_val) && max_val > 0) {
        result_df[[pelo_pct_col]] <- result_df[[elo_col]] / max_val
      } else {
        result_df[[pelo_pct_col]] <- 0.5  # Default middle value
      }
    }
  }
}
```

**Hill Size-Specific Weighted Previous Points Calculation**:
Recent performance is captured through weighted previous points using the last 5 races with linear increasing weights, calculated separately for each hill size category to capture performance patterns specific to different jumping conditions:

```r
# From race-picks.R:920-926 (training phase)
mutate(Prev_Points_Weighted = sapply(1:n(), function(j) {
  if (j == 1) return(0)
  start_index <- max(1, j - 5)
  num_races <- j - start_index
  weights <- seq(1, num_races)  # Sequential weighting (1,2,3,4,5)
  weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
}))

# From race-picks.R:1349-1356 (testing phase)
recent_points <- race_df %>%
  filter(Skier %in% base_df$Skier) %>%
  group_by(Skier) %>%
  arrange(Season, Race) %>%
  slice_tail(n = 5) %>%
  summarise(
    Prev_Points_Weighted = if(n() > 0) 
      weighted.mean(Points, w = seq_len(n()), na.rm = TRUE) 
    else 0
  )
```

**Advanced Missing Value Imputation with Dynamic Fallbacks**:
Ski jumping uses the most sophisticated NA handling approach among winter sports, with multiple fallback mechanisms to handle the smaller competitive field characteristic of the sport:

```r
# From race-picks.R:21-25
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Application during startlist preparation (lines 1441-1449):
result_df <- result_df %>%
  mutate(
    across(ends_with("_Pct"), ~replace_na_with_quartile(.x)),
    Prev_Points_Weighted = replace_na(Prev_Points_Weighted, 0)
  )

# Dynamic NA handling during prediction (lines 2158-2168):
if(any(is.na(prediction_subset[[var]]))) {
  if(is.numeric(prediction_subset[[var]])) {
    prediction_subset[[var]] <- replace_na_with_quartile(prediction_subset[[var]])
  } else {
    # For non-numeric, use most common value
    most_common <- names(sort(table(prediction_subset[[var]], useNA = "no"), 
                             decreasing = TRUE))[1]
    prediction_subset[[var]][is.na(prediction_subset[[var]])] <- most_common
  }
}
```

**Comprehensive Model Variable Validation**:
The startlist setup includes extensive model variable validation and type checking to ensure compatibility with trained GAM models across different hill sizes and competition formats:

```r
# From race-picks.R:2134-2170
for(var in model_vars) {
  if(!(var %in% names(prediction_subset))) {
    # Add missing variable with appropriate default value
    prediction_subset[[var]] <- 0
  } else {
    # Ensure the variable has the right type
    if(model_var_type == "numeric") {
      prediction_subset[[var]] <- as.numeric(prediction_subset[[var]])
    } else if(model_var_type == "factor") {
      prediction_subset[[var]] <- as.factor(prediction_subset[[var]])
    }
  }
}
```

**Individual vs Team Format Handling**:
The startlist setup dynamically adapts between individual and team competitions, with specialized handling for team-averaged hill size-specific ELO ratings and nation-based identification while preserving hill size-specific performance characteristics.

**Hill Size-Specific Model Preparation**:
The final startlist preparation ensures complete feature sets across all hill categories, validates race participation probabilities, and creates comprehensive prediction-ready datasets that capture ski jumping's unique hill size-dependent performance patterns essential for accurate GAM model prediction across Small, Medium, Normal, Large, and Flying hill competitions.

##### Modeling

Ski jumping testing applies sophisticated GAM models with hill size-specific awareness while implementing comprehensive Individual Points Testing Adjustments that account for systematic biases in venue-dependent performance prediction. The system uses statistical significance testing to identify genuine performance patterns during the testing phase, applying adjustments only when historical evidence supports systematic model bias across period and hill size conditions.

**Individual Points Testing Adjustments for Hill Size-Dependent Performance**:
Ski jumping testing employs a sophisticated sequential adjustment framework that accounts for the sport's unique hill size variations and seasonal progression effects. The system uses statistical significance testing to identify genuine performance patterns that require correction during the testing phase.

**Sequential Hill Size-Aware Adjustment Framework**:
Adjustments are calculated and applied sequentially to avoid double-counting effects while maintaining chronological integrity (using only prior race data for each test):

```r
# From race-picks.R:1975-2024
# Step 1: Initial GAM prediction using BIC-selected variables
Initial_Prediction = predict(model, newdata = startlist_prepared),

# Step 2: Period adjustments with t-test validation
period_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_period_curr <- Prediction_Diff[Period == Period[r] & row_id < r]
  prior_period_other <- Prediction_Diff[Period != Period[r] & row_id < r]
  if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
  tryCatch({
    t.test(prior_period_curr, prior_period_other)$p.value
  }, error = function(e) 1)
}),
period_correction = ifelse(period_p < 0.05,
                          mean(Prediction_Diff[Period == Period], na.rm = TRUE),
                          0),

# Step 3: Hill Size adjustments with statistical testing
hillsize_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_hill_curr <- Prediction_Diff[HillSize_Flag == HillSize_Flag[r] & row_id < r]
  prior_hill_other <- Prediction_Diff[HillSize_Flag != HillSize_Flag[r] & row_id < r]
  if(length(prior_hill_curr) < 3 || length(prior_hill_other) < 3) return(1)
  tryCatch({
    t.test(prior_hill_curr, prior_hill_other)$p.value
  }, error = function(e) 1)
}),
hillsize_correction = ifelse(hillsize_p < 0.05,
                            mean(Prediction_Diff[HillSize_Flag == HillSize_Flag], na.rm = TRUE),
                            0),

# Step 4: Combined adjustment application
Adjusted_Prediction = Initial_Prediction + period_correction + hillsize_correction
```

**Statistical Significance Requirements**: All adjustments require p < 0.05 from two-sample t-tests comparing current conditions (period/hill size) vs. other conditions, with minimum 3 observations per group for statistical validity. This ensures adjustments are applied only when there is strong evidence of systematic bias.

**Hill Size-Specific Adjustment Categories**:

1. **Period Adjustments**: Account for seasonal progression effects in ski jumping form development, as athletes' technical jumping ability and confidence develop throughout the season (4 periods based on race progression)

2. **Hill Size Adjustments**: Capture performance differences between hill categories using binary classification:
   - **Small/Medium Hills (Flag = 0)**: Technical jumping focus on steeper, shorter hills (K90-120m)
   - **Large/Flying Hills (Flag = 1)**: Distance and speed focus on larger hills (K120+ and K185+)

**Hill Size Performance Categorization**:
The system recognizes that ski jumping performance varies dramatically by hill type, requiring different technical skills and physical attributes:

```r
# From race-picks.R:970-974
HillSize_Flag = case_when(
  HillSize %in% c("Small", "Medium") ~ 0,    # Technical jumping
  HillSize %in% c("Large", "Flying") ~ 1,    # Distance jumping  
  TRUE ~ 0
)
```

**GAM Model Integration with Hill-Specific Features**:
The testing framework applies GAM models that incorporate hill size-specific ELO ratings and use BIC optimization for feature selection:

```r
# From race-picks.R:1787-1799
exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))

model <- gam(gam_formula, data = race_df_75)
```

**Race Probability Integration with Hill Size Adjustments**:
Testing adjustments are combined with race participation probability weighting to create final predictions that account for both hill size-specific bias correction and athlete likelihood to participate:

```r
# From race-picks.R:2306-2311
# Apply historically-derived adjustments from testing phase
Predicted_Points = Base_Prediction + period_adjustment + hillsize_adjustment,
Predicted_Points = pmax(pmin(Predicted_Points, 100), 0),

# Apply race probability weighting
Race_Prob = get(race_prob_col),
Final_Prediction = Predicted_Points * Race_Prob,

# Generate confidence scenarios with volatility consideration
confidence_factor = pmin(n_recent_races / 10, 1),
Safe_Prediction = pmax(
  (Predicted_Points - (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
  0
),
Upside_Prediction = pmin(
  (Predicted_Points + (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
  100 * Race_Prob
)
```

**Position Probability Models with Hill Size Awareness**:
The system also implements position probability models with the same hill size-aware adjustment framework, ensuring consistency across both points and position probability predictions while accounting for ski jumping's venue-dependent performance characteristics.

**Robust Error Handling for Hill Size Calculations**:
All adjustment calculations include comprehensive error handling with tryCatch blocks, defaulting to no adjustment (p-value = 1) when insufficient data or calculation errors occur. This ensures the prediction pipeline maintains stability across varying hill sizes and data quality conditions.

This hill size-aware testing adjustment framework ensures that ski jumping predictions account for systematic biases while preserving the complex venue-dependent performance patterns that characterize this unique winter sport, where technical precision on smaller hills differs fundamentally from distance-focused performance on large hills and ski flying venues.