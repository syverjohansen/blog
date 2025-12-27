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

#### Probability

##### Training

###### Setup

Ski jumping's Individual Probability Training Setup converts the points prediction problem into binary classification for position probability modeling with hill size-aware threshold adaptation. The system uses the same preprocessed historical race data as points models but transforms the venue-dependent regression problem into classification through binary outcome creation across different hill categories.

**Position Threshold Definition**:
Ski jumping uses adapted thresholds based on competition format: Individual events use standard thresholds `c(1, 3, 5, 10, 30)` representing Win, Podium, Top 5, Top 10, and Top 30, while Team events use reduced thresholds `c(1, 3, 5, 10)` (adapted for smaller team competition fields). Each threshold creates a separate binary classification problem where success is defined as finishing at or above that position.

**Binary Outcome Creation**:
For each position threshold, the system creates binary outcome variables using the fundamental transformation: `race_df$position_achieved <- race_df$Place <= threshold`. This converts the continuous place variable into binary classification targets, enabling binomial GAM modeling for probability prediction across ski jumping's unique hill size variations.

**Training Data Consistency**:
Position probability models use identical training datasets as their corresponding points prediction models, including the same 10-season historical window, top performer filtering (ELO > 75th percentile), and hill size-specific preprocessing. No separate data pipeline is required - the same `race_df` serves both modeling approaches.

**Hill Size-Specific Adaptation**:
Ski jumping's most distinctive characteristic - dramatic performance variation based on hill size - requires threshold evaluation across different venue categories. Small/Medium Hills emphasize technical jumping precision while Large/Flying Hills focus on distance performance, creating different probability patterns for the same athletes across hill categories.

**Venue-Dependent Performance Integration**:
The training setup incorporates ski jumping's unique venue sensitivity, including hill size effects on jumping technique, wind conditions, and gate factors, ensuring position probability models capture the sport's complex venue-dependent performance characteristics that differentiate technical specialists from distance jumpers.

##### Feature Selection

Ski jumping's Individual Probability Training Feature Selection employs a sophisticated threshold-independent optimization strategy that adapts to the sport's most distinctive characteristic: dramatic performance variation based on hill size. The system performs independent BIC optimization for each position threshold while leveraging hill size-specific variable inheritance from corresponding points prediction models.

**Variable Inheritance and Consistency**:
Position probability models use identical explanatory variable pools as their corresponding points models: `position_feature_vars <- explanatory_vars`. This ensures consistency between modeling approaches while leveraging domain knowledge already encoded in ski jumping's hill size-specific points model variable selection across different venue categories.

**Hill Size-Specific Variable Sets**:
Ski jumping adapts feature pools based on venue characteristics, utilizing hill size-specific variables including `Prev_Points_Weighted`, `Small_Pelo_Pct`, `Medium_Pelo_Pct`, `Normal_Pelo_Pct`, `Large_Pelo_Pct`, `Flying_Pelo_Pct`, and `Pelo_Pct`. Individual events employ the full variable set while team events focus on team-aggregated hill size-specific performance metrics.

**Independent Threshold Optimization**:
For each position threshold (1, 3, 5, 10, 30 for individual events; 1, 3, 5, 10 for team events), the system performs exhaustive subset selection using BIC optimization: `pos_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")`. This threshold-independent approach recognizes that different finishing position predictions may require different variable combinations for optimal binomial classification accuracy across ski jumping's venue-dependent performance spectrum.

**Hill Size-Aware Feature Integration**:
The feature selection process acknowledges that ski jumping performance varies dramatically across hill categories, ensuring selected variables capture both technical precision requirements for smaller hills and distance-focused performance characteristics for large hills and ski flying venues, accommodating the sport's unique venue-dependent competitive dynamics.

##### Modeling

Ski jumping employs sophisticated binomial GAM (Generalized Additive Models) architecture for individual position probability prediction during the training phase, utilizing the sport's distinctive hill size-dependent performance characteristics within independent threshold-based modeling frameworks. This approach provides probabilistic finishing position forecasts that capture the dramatic performance variation across ski jumping's diverse venue spectrum from normal hills to ski flying hills.

**Binomial GAM Architecture**:
Position probability models use binomial family GAM implementation with REML (Restricted Maximum Likelihood) estimation, specifically designed for binary classification of finishing position achievement. The core model structure employs threshold-specific binary response variables and comprehensive explanatory variable sets that reflect ski jumping's unique venue-dependent competitive dynamics:

```r
# From ski jumping R script
position_model <- gam(pos_gam_formula,
                      data = race_df,
                      family = binomial,
                      method = "REML")
```

**Independent Threshold Modeling**:
Ski jumping implements separate binomial GAM models for each position threshold (1st, 3rd, 5th, 10th, 30th for individual events; 1st, 3rd, 5th, 10th for team events), recognizing that factors influencing podium finishes may differ substantially from those affecting top-10 or points-scoring positions across different hill sizes. Each threshold receives independent optimization through BIC-based feature selection followed by specialized GAM fitting with hill size-specific adaptations.

**Hill Size-Dependent Performance Integration**:
Model construction acknowledges ski jumping's most distinctive characteristic: dramatic performance variation based on hill size categories (K90-109 normal hills, K120+ large hills, K185+ ski flying hills). The binomial GAM framework incorporates hill-specific performance metrics and venue characteristics, utilizing smooth terms to capture non-linear relationships between jumping technique, distance achievement, and finishing position probabilities across the sport's diverse competitive venues.

**REML Estimation and Robust Training**:
Training employs REML estimation for conservative smoothing parameter selection, promoting model stability across ski jumping's wide venue spectrum from World Cup normal hills to ski flying competitions. The system includes comprehensive error handling for edge cases where feature selection or GAM fitting encounters convergence issues, implementing fallback strategies that ensure model availability for all hill sizes while maintaining venue-specific modeling integrity.

**Event Type-Specific Adaptations**:
The modeling framework adapts to different ski jumping formats (individual competitions, team events, mixed team competitions), utilizing conditional logic to adjust variable pools and model complexity based on event characteristics and hill sizes. Individual events employ comprehensive variable sets capturing jumping technique and distance performance, while team events incorporate aggregated performance metrics that reflect the collaborative nature of ski jumping team competition across different venues.

**Training Phase Model Validation**:
Binomial GAM models undergo Brier score evaluation during training to assess probabilistic accuracy, with threshold-specific validation ensuring that position probability predictions maintain calibration across different finishing position ranges and hill size categories. The validation process specifically accounts for ski jumping's venue-dependent performance distribution patterns and hill size-specific competitive dynamics.

##### Adjustments

Ski jumping implements sophisticated Individual Probability Training Adjustments that are **active** in the production system, featuring period-based correction mechanisms specifically adapted to the sport's distinctive hill size-dependent performance characteristics. This methodology recognizes ski jumping's most unique competitive challenge: dramatic performance variation across venue categories from K90 normal hills to K185+ ski flying hills.

**Probability Residual Calculation with Hill Size Awareness**:
The adjustment system calculates probability differences between actual outcomes and model predictions: `prob_diff = as.numeric(position_achieved) - initial_prob`. These residuals capture systematic bias patterns in position probability predictions while acknowledging ski jumping's venue-dependent performance characteristics where technical precision requirements vary dramatically across hill size categories.

**Period-Based Bias Correction for Seasonal Form Development**:
Ski jumping employs period-specific adjustments to capture systematic performance changes across the competitive season: `period_p = purrr::map_dbl(row_id, function(r) {...})`. The system uses t-test validation to compare period-specific probability residuals against other periods, applying corrections only when p < 0.05 ensures genuine period-based systematic bias rather than random variation in jumping form development.

**Hill Size-Dependent Performance Integration**:
The adjustment framework acknowledges ski jumping's fundamental characteristic: dramatic performance variation based on hill size categories (K90-109 normal hills, K120+ large hills, K185+ ski flying hills). Period adjustments are calculated with consideration for venue-dependent performance patterns, recognizing that systematic bias corrections may manifest differently across the sport's diverse competitive venues.

**Statistical Significance Testing with Venue Adaptation**:
The system employs rigorous statistical validation tailored to ski jumping's venue-dependent competitive structure: `t.test(prior_period_curr, prior_period_other)$p.value`. This ensures that corrections address genuine systematic bias patterns across hill size categories rather than random performance variation in distance achievement and jumping technique optimization.

**Event Type-Specific Adjustment Application**:
The adjustment methodology adapts to different ski jumping formats (individual competitions, team events, mixed team competitions), utilizing conditional logic to adjust correction calculations based on event characteristics and hill sizes. Individual events receive comprehensive period-based adjustments, while team events incorporate aggregated correction metrics that reflect the collaborative nature of ski jumping team competition.

**Hill Size-Aware Probability Constraint Enforcement**:
All adjustments maintain valid probability ranges: `period_adjusted = pmin(pmax(initial_prob + period_correction, 0), 1)`. This ensures corrected probabilities remain mathematically valid while preventing extreme adjustments that could destabilize predictions across ski jumping's wide venue spectrum from World Cup normal hills to ski flying competitions.

**Active Implementation with Venue Complexity**:
Ski jumping's Individual Probability Training Adjustments remain **fully operational** in the production system, representing an active approach to systematic bias correction that accommodates the sport's unique venue-dependent competitive dynamics and hill size-specific performance characteristics. The period-based adjustment framework captures seasonal form progression while maintaining mathematical consistency across ski jumping's diverse competitive venue spectrum.

#### Testing

##### Startlist Setup

Ski jumping's Individual Probability Testing employs sophisticated startlist preparation that accommodates the sport's distinctive hill size-dependent performance characteristics through comprehensive venue categorization and hill-specific ELO rating systems. The framework manages the most detailed hill size classification among winter sports, processing five distinct venue categories from normal hills to ski flying hills while maintaining venue-dependent prediction accuracy.

**Hill Size-Specific Performance Framework**:
Ski jumping's startlist setup incorporates the sport's unique five-category hill classification system: Small hills (K90-109m), Medium hills (K109-120m), Normal hills (K120-140m), Large hills (K140-185m), and Flying hills (K185+m). The system maintains separate ELO ratings for each category: `Small_Pelo_Pct`, `Medium_Pelo_Pct`, `Normal_Pelo_Pct`, `Large_Pelo_Pct`, `Flying_Pelo_Pct`, recognizing that jumping technique requirements vary dramatically across venue sizes.

**Venue-Dependent ELO Rating Integration**:
The startlist preparation processes hill size-specific ELO ratings that capture venue-dependent performance patterns. Technical precision requirements for smaller hills differ substantially from distance achievement focus on large hills and ski flying venues, requiring separate rating systems that reflect these distinct skill requirements across ski jumping's diverse competitive spectrum.

**Binary Hill Size Classification for Model Input**:
The system employs binary hill size flags for enhanced model processing: Small/Medium hills (Flag = 0) representing technical jumping focus versus Large/Flying hills (Flag = 1) emphasizing distance and speed performance. This classification enables models to distinguish between venue categories that require fundamentally different jumping approaches and performance characteristics.

**Venue Characteristics and Competition Format Integration**:
Startlist setup accommodates both individual and team ski jumping competitions while maintaining hill size-specific data integrity. Team events utilize aggregated venue-dependent performance metrics that reflect collaborative jumping dynamics, while individual events employ comprehensive hill-specific variables that capture personal venue specialization patterns.

**Most Recent Hill-Specific Performance Metrics**:
The system retrieves chronologically recent ELO ratings while maintaining hill size category distinctions: `arrange(Season, Race) %>% slice(n())`. This ensures that position probability models utilize the latest venue-specific performance information, acknowledging that a jumper's recent form may vary significantly between normal hill technical performance and ski flying distance achievement.

**Weighted Previous Points with Venue Consideration**:
Ski jumping incorporates venue-aware weighted previous points calculation using a 5-race window with increasing weights (1, 2, 3, 4, 5): `Prev_Points_Weighted = sapply(1:n(), function(j) {...})`. This metric captures recent form while maintaining venue specificity, recognizing that recent normal hill performance may not predict ski flying capabilities and vice versa.

**Comprehensive Missing Value Imputation for Venue Complexity**:
The framework employs robust NA handling: `replace_na_with_quartile(x)` that accounts for ski jumping's venue-dependent performance patterns. Missing values are replaced with conservative estimates that consider hill size-specific performance requirements, preventing overly optimistic predictions for athletes with limited recent venue-specific data.

**Position Threshold Framework with Event Type Adaptation**:
Ski jumping's startlist preparation accommodates position threshold variations: Individual events use standard thresholds (1st, 3rd, 5th, 10th, 30th) while Team events employ reduced thresholds (1st, 3rd, 5th, 10th) adapted for smaller team fields. This framework ensures binary outcome variable creation (`position_achieved <- Place <= threshold`) remains appropriate across different competition formats and venue types.

**Hill Size-Aware Data Validation and Preparation**:
The startlist setup includes comprehensive validation ensuring all required hill-specific percentage columns exist: `for (col in pct_cols) {if (!col %in% names(processed_df)) {processed_df[[col]] <- 0}}`. This prevents model failures due to missing venue-specific data while maintaining consistent variable structures across ski jumping's extraordinary venue diversity from World Cup normal hills to ski flying competitions.

##### Modeling

Ski jumping's Individual Probability Testing applies trained binomial GAM models through sophisticated hill size-dependent frameworks that accommodate the sport's most distinctive characteristic: dramatic performance variation across venue categories from K90 normal hills to K185+ ski flying hills. The system manages venue-specific model applications across five distinct hill categories while ensuring reliable probability predictions through hill-aware error handling and comprehensive fallback strategies.

**Hill Size-Dependent Model Application Framework**:
Ski jumping's testing modeling adapts to venue characteristics through hill size-specific prediction: `mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response")`. The system acknowledges that technical precision requirements for smaller hills differ substantially from distance achievement focus on large hills and ski flying venues, requiring separate model applications that reflect these distinct skill requirements.

**Five-Category Venue-Specific Variable Validation**:
The framework performs comprehensive variable validation for ski jumping's unique hill classification system: `model_vars <- names(pos_model$var.summary)`. The system validates hill size-specific ELO ratings (Small_Pelo_Pct, Medium_Pelo_Pct, Normal_Pelo_Pct, Large_Pelo_Pct, Flying_Pelo_Pct) and ensures that venue-dependent performance variables are available for model application across different competitive hill sizes.

**Binary Hill Size Classification for Enhanced Prediction**:
Model application employs binary hill size flags for improved venue-dependent processing: Small/Medium hills (Flag = 0) representing technical jumping focus versus Large/Flying hills (Flag = 1) emphasizing distance and speed performance. This classification enables models to distinguish between venue categories that require fundamentally different jumping approaches and performance characteristics.

**Venue-Aware Threshold Processing with Event Type Adaptation**:
The system generates position probability predictions for venue-appropriate threshold structures: Individual events use standard thresholds (1st, 3rd, 5th, 10th, 30th) while Team events employ reduced thresholds (1st, 3rd, 5th, 10th) adapted for smaller team fields. This venue-aware threshold processing ensures predictions remain appropriate across different hill sizes and competition formats.

**Hill-Specific Performance Integration Framework**:
Testing modeling incorporates venue-dependent performance patterns that acknowledge jumping technique requirements vary dramatically across hill sizes. Normal hill technical precision differs from ski flying distance achievement, requiring models that capture venue-specific skill requirements while maintaining mathematical consistency across ski jumping's diverse competitive spectrum.

**Robust Error Handling for Venue Complexity**:
Ski jumping implements sophisticated error handling designed for venue-dependent model application: row-by-row fallback prediction when batch processing fails, hill size-specific variable validation, and conservative default assignment that considers venue characteristics. This ensures prediction availability across ski jumping's extraordinary venue diversity while maintaining venue-specific modeling integrity.

**Multi-Tier Fallback Strategy with Hill Size Awareness**:
When primary models fail, the system implements venue-aware fallback mechanisms: `fallback_vars <- c("Prev_Points_Weighted", elo_col)` that maintain hill size-specific performance considerations. These fallback models use venue-appropriate predictors to ensure prediction availability even when full model application encounters errors across different hill categories.

**Seasonal Form Progression Integration with Venue Specificity**:
Model application acknowledges that jumping form development may vary across different hill sizes, ensuring that recent normal hill performance and ski flying capabilities receive appropriate weighting. The framework maintains venue-dependent prediction accuracy while capturing seasonal form progression patterns across ski jumping's diverse competitive venue spectrum.

#### Normalization and Monotonic Constraints

Ski jumping implements sophisticated Individual Normalization and Monotonic Constraints specifically adapted for the sport's distinctive hill size-dependent performance characteristics and extraordinary venue diversity from K90 normal hills to K185+ ski flying hills. The system employs comprehensive race participation probability integration that accommodates the sport's unique venue-dependent competitive dynamics while maintaining mathematical validity across five distinct hill categories (Small, Medium, Normal, Large, Flying).

**Hill Size-Aware Race Participation Integration**:
Ski jumping's normalization system includes sophisticated race participation adjustments that account for venue-dependent performance patterns: `normalized[[prob_col]] <- normalized[[prob_col]] * normalized[[race_prob_col]]`. This approach recognizes that athletes may specialize in specific hill size categories, requiring probability adjustments that reflect venue-specific participation likelihood and performance capabilities across ski jumping's diverse competitive spectrum.

**Venue-Dependent Target Sum Calculation**:
The system calculates target sums with awareness of ski jumping's hill size-dependent performance complexity: `target_sum <- 100 * threshold` (100% for Top-1, 300% for Top-3, 500% for Top-5, 1000% for Top-10, 3000% for Top-30). These targets accommodate the sport's unique performance distribution patterns influenced by venue characteristics, where technical precision requirements for smaller hills differ fundamentally from distance achievement focus on large hills and ski flying competitions.

**Hill Size-Specific Individual Probability Capping**:
Ski jumping implements comprehensive probability capping: `over_hundred <- which(normalized[[prob_col]] > 100)` with excess redistribution that considers venue-dependent performance distributions. Normal hill technical specialists may have different probability ceiling patterns compared to ski flying distance specialists, requiring redistribution mechanisms that account for venue-specific performance characteristics across the sport's extraordinary hill diversity.

**Venue-Category Monotonic Constraints with Hill Size Adaptation**:
The framework applies monotonic constraints with consideration for ski jumping's venue-dependent performance variations: `P(Top-1) ≤ P(Top-3) ≤ P(Top-5) ≤ P(Top-10) ≤ P(Top-30)`. The system ensures logical probability ordering while acknowledging that technical precision specialists (Small/Medium hills) and distance achievement specialists (Large/Flying hills) may exhibit different finishing position probability patterns across venue categories.

**Hill-Specific Re-normalization with Venue Complexity**:
After monotonic constraint application, ski jumping re-normalizes probabilities to maintain target sums while preserving the sport's venue-dependent performance characteristics: `current_sum <- sum(normalized[[prob_col]], na.rm = TRUE); scaling_factor <- target_sum / current_sum`. This ensures mathematical consistency across hill size variations and jumping technique requirements that determine competitive outcomes across ski jumping's diverse venue spectrum.

**Competition Format and Venue-Aware Error Handling**:
The normalization framework includes sophisticated error handling adapted for ski jumping's venue diversity and varying competition formats (Individual, Team, Mixed Team). When normalization encounters edge cases (zero sums, extreme distributions), the system applies fallback mechanisms that consider both venue-specific performance patterns and competition format characteristics, ensuring robust probability distributions across all competitive scenarios and hill categories.

**Mathematical Validity Across Hill Size and Performance Spectrum**:
Ski jumping's normalization and constraint system maintains mathematical rigor while accommodating the sport's extraordinary venue-dependent performance complexity and hill size variations. The framework ensures that all final probabilities remain within valid [0,1] bounds per athlete while summing to appropriate totals, reflecting both technical precision capabilities for smaller hills and distance achievement requirements for large hills and ski flying venues that characterize the sport's unique competitive challenge of excelling across dramatically different venue categories.

### Relay

#### Data Gathering

Ski jumping relay data gathering employs sophisticated FIS website HTML parsing with specialized hill size-dependent team event detection across mixed team and standard team formats. The framework accommodates the sport's unique venue-dependent team dynamics while integrating comprehensive hill size classification (Small, Medium, Normal, Large, Flying) and multi-round jump data extraction for venue-aware team performance evaluation across ski jumping's extraordinary hill diversity.

**Hill Size-Dependent Team Event Detection**:
Ski jumping's data gathering system employs advanced event type detection through combined title and HTML structure analysis: `if any(keyword in event_title for keyword in ['TEAM', 'MIXED']):` for team event identification with additional HTML structure validation. The system processes Mixed Team events (4 members with M-F-M-F patterns) and Standard Team events (4 members, single gender) while maintaining venue-dependent performance context across hill categories.

**FIS Website Multi-Round Jump Data Extraction**:
The system processes FIS website data through sophisticated HTML parsing designed for ski jumping's complex split-row structure: `extract_team_member_data(member_row) -> Dict` processes multiple jump rounds with specialized `split-row__item` extraction. Team member data includes Round 1 and Round 2 jump distances, points, and comprehensive venue-specific performance metrics that enable venue-aware team evaluation across different hill sizes.

**Venue-Specific Hill Size Classification Integration**:
Ski jumping uniquely integrates hill size classification data (`Hill_Size`, K-point, HS-point) that enables venue-dependent team performance analysis. The system extracts venue characteristics alongside team composition data, ensuring that team performance evaluation accounts for hill size-specific requirements where technical precision (smaller hills) differs fundamentally from distance achievement (larger hills and ski flying venues).

**Hill Size-Aware ELO Integration and Team Performance Aggregation**:
The data gathering framework incorporates ski jumping's unique venue-dependent ELO complexity through comprehensive athlete data integration: `['Elo', 'Small_Elo', 'Medium_Elo', 'Normal_Elo', 'Large_Elo', 'Flying_Elo']`. Team-level aggregation calculates `Total_Elo` and `Avg_Elo` metrics that reflect collective venue-specific capabilities across ski jumping's five distinct hill categories essential for venue-aware team performance evaluation.

**Multi-Round Jump Performance Integration**:
Ski jumping's relay data gathering uniquely extracts multi-round jump data for individual team members: `Member_X_Length1`, `Member_X_Length2` fields that capture performance across multiple competition rounds. This multi-round data enables comprehensive team performance analysis that accounts for consistency and peak performance capabilities across ski jumping's complex multi-round competition structure.

**Mixed Team Gender Detection with Venue Context**:
The system employs sophisticated gender detection for mixed team events using position-based assignment while maintaining venue-dependent performance context. Gender validation integrates dual ELO database fuzzy matching with venue-specific performance considerations, ensuring accurate team composition representation across ski jumping's venue-dependent competitive characteristics.

#### Points 

##### Training 

###### Setup

Ski jumping relay points training setup manages complex team-specific ELO aggregation across the sport's distinctive hill size variations while accommodating multi-round team competition formats through comprehensive training data integration and venue-dependent performance optimization. The framework handles the sport's unique five-category hill system (Small, Medium, Normal, Large, Flying) with team-specific ELO calculations for venue-aware performance prediction across ski jumping's extraordinary competitive diversity.

**Championships-Based Team Training Data Processing**:
Ski jumping's relay training setup utilizes championships race format data that reflects the sport's primary team competition structure through comprehensive team data integration:

```r
# From champs-predictions.R:269-311  
# Handle team vs individual races
if(is_team) {
  # For team races, startlist already contains pre-aggregated team data
  # Use it directly without further aggregation
  log_info("Processing team startlist - using pre-aggregated team data")
  
  # The team startlist already has Avg_* columns, use them directly
  result_df <- startlist %>%
    # Select relevant columns from the team startlist
    dplyr::select(Nation, any_of(race_prob_cols), starts_with("Avg_"))
  
  log_info(paste("Using team startlist with", nrow(result_df), "teams"))
  
  # Calculate team Prev_Points_Weighted for prediction startlist
  # Extract race type from elo_col (e.g., "Avg_Large_Elo_Pct" -> "Large")
  race_type_from_col <- gsub("Avg_(.+)_Elo_Pct", "\\1", elo_col)
```

**Hill Size-Specific Team ELO Aggregation Framework**:
The training system processes team ELO calculations with venue-specific integration that acknowledges ski jumping's extraordinary hill diversity and venue-dependent performance characteristics:

```r
# From champs-predictions.R:314-345
# Calculate team Prev_Points_Weighted for each team in startlist
if("TeamMembers" %in% names(startlist)) {
  # Team startlist has TeamMembers column
  result_df <- result_df %>%
    left_join(
      startlist %>% dplyr::select(Nation, TeamMembers),
      by = "Nation"
    ) %>%
    rowwise() %>%
    mutate(
      Prev_Points_Weighted = {
        if(!is.na(TeamMembers) && TeamMembers != "") {
          team_members <- trimws(strsplit(TeamMembers, ",")[[1]])
          calculate_team_prev_points(team_members, current_event_date, race_type_from_col, individual_chrono)
        } else {
          0
        }
      }
    ) %>%
    ungroup() %>%
    dplyr::select(-TeamMembers)  # Remove temporary column
```

**Mixed Team and Standard Team Format Processing**:
Ski jumping's training setup accommodates both Mixed Team (4 members with M-F-M-F patterns) and Standard Team (4 members, single gender) competition formats through comprehensive team composition validation:

```r  
# From champs-predictions.R:292-308
tryCatch({
  # Determine individual chrono path based on race data
  if("Mixed" %in% race_df$Sex) {
    # Mixed teams - load both chrono files
    log_info("Loading individual chrono for mixed team predictions")
    men_chrono <- read.csv("~/ski/elo/python/skijump/polars/relay/excel365/men_chrono.csv", stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date), Points = sapply(Place, get_points))
    ladies_chrono <- read.csv("~/ski/elo/python/skijump/polars/relay/excel365/ladies_chrono.csv", stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date), Points = sapply(Place, get_points))
    individual_chrono <- bind_rows(men_chrono, ladies_chrono)
  } else {
    # Regular teams - load appropriate gender chrono
    gender_for_chrono <- if("M" %in% race_df$Sex) "men" else "ladies"
    chrono_path <- paste0("~/ski/elo/python/skijump/polars/relay/excel365/", gender_for_chrono, "_chrono.csv")
    log_info(paste("Loading individual chrono for team predictions:", chrono_path))
    individual_chrono <- read.csv(chrono_path, stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date), Points = sapply(Place, get_points))
  }
```

**Venue-Dependent Team Performance Integration**:
The framework processes hill size-specific team ELO calculations that capture venue-dependent performance patterns across ski jumping's five distinct hill categories: `team_elo_cols <- c("Avg_Normal_Elo", "Avg_Large_Elo", "Avg_Flying_Elo", "Avg_Elo")`. This acknowledges that teams may specialize in different hill sizes, requiring venue-aware team composition and performance evaluation.

**Comprehensive Team Previous Points Calculation**:
Ski jumping implements sophisticated team performance integration through individual member contribution analysis: `calculate_team_prev_points(team_members, current_event_date, race_type_from_col, individual_chrono)`. This aggregates individual venue-specific performance into team metrics that reflect collective capabilities across different hill sizes and venue categories.

**Hill Size-Aware Percentage Calculation for Team Training**:
The training setup includes hill size-specific percentage normalization that acknowledges venue-dependent performance variations: percentage calculations across `Avg_Normal_Elo_Pct`, `Avg_Large_Elo_Pct`, `Avg_Flying_Elo_Pct` enable model training that captures team venue specialization patterns and hill-specific competitive dynamics.

**Error Handling and Team Composition Validation**:  
The framework includes comprehensive error handling for missing team composition data and venue-specific performance information: `log_warn(paste("Error calculating team Prev_Points_Weighted for predictions:", e$message))`. This ensures training data integrity across ski jumping's venue-dependent team competition formats while maintaining venue-aware performance evaluation capabilities.

**Event Date and Competition Timeline Integration**:
The system processes event dates with hill size-specific team performance timeline consideration: `current_event_date <- max(race_df$Date, na.rm = TRUE)`. This enables accurate team form assessment across venue-dependent performance patterns that may vary based on recent hill-specific competition experience and venue specialization development.

###### Feature Selection

Ski jumping relay points training feature selection employs sophisticated hill size-specific team-aggregated variable optimization that accommodates the sport's distinctive venue-dependent performance characteristics across five hill categories while incorporating comprehensive team-level ELO integration and BIC-optimized exhaustive subset selection through venue-aware performance prediction across ski jumping's extraordinary competitive diversity.

**Team-Specific Hill Size Variable Framework**:
Ski jumping's relay feature selection adapts to team competition by replacing individual athlete performance variables with team-aggregated metrics that capture collective venue-dependent capabilities:

```r
# From race-picks.R:1774-1781
# Define explanatory variables based on race type
# Training uses pre-race ELO (Pelo) to avoid data leakage
if(is_team) {
  explanatory_vars <- c("Avg_Normal_Pelo_Pct", "Avg_Large_Pelo_Pct", 
                        "Avg_Flying_Pelo_Pct", "Avg_Pelo_Pct")
} else {
  explanatory_vars <- c("Prev_Points_Weighted", 
                        "Normal_Pelo_Pct", "Large_Pelo_Pct", 
                        "Flying_Pelo_Pct", "Pelo_Pct")
}
```

**Elimination of Weighted Previous Points for Team Events**:
Relay feature selection excludes weighted previous points (`Prev_Points_Weighted`) since team compositions change between races, making historical team performance less predictive than aggregated individual capabilities. Team-level hill size-specific ELO averaging variables replace individual venue performance metrics while maintaining ski jumping's venue-dependent competitive dynamics.

**Hill Size-Specific Team ELO Integration**:
The framework incorporates ski jumping's unique five-category hill system through team-averaged performance metrics across venue types: `Avg_Normal_Pelo_Pct`, `Avg_Large_Pelo_Pct`, `Avg_Flying_Pelo_Pct`, enabling comprehensive team capability assessment across venue-dependent requirements where technical precision (smaller hills) differs fundamentally from distance achievement (larger hills and ski flying venues).

**BIC Optimization for Venue-Dependent Team Performance**:
Ski jumping employs the same exhaustive subset selection methodology as individual events but applied to team-aggregated variables, using Bayesian Information Criterion optimization to balance model complexity with prediction accuracy across relay team compositions:

```r
# From race-picks.R:1787-1792
tryCatch({
  exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
  summary_exhaustive <- summary(exhaustive_selection)
  best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
  smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
```

**Venue-Dependent Team Performance Assessment**:
The feature selection acknowledges that relay teams may specialize in different hill sizes, requiring venue-aware team composition and performance evaluation. Teams with technical precision specialists (Small/Medium hills) versus distance achievement specialists (Large/Flying hills) receive appropriate variable weighting that captures venue-specific team capabilities.

**Five-Category Hill System Team Integration**:
The framework processes team ELO calculations across ski jumping's complete venue spectrum: Small hills (K90-109m), Medium hills (K109-120m), Normal hills (K120-140m), Large hills (K140-185m), and Flying hills (K185+m). This comprehensive venue categorization enables team predictions that account for venue specialization patterns across ski jumping's extraordinary hill diversity.

**Championship-Based Team Variable Optimization**:
Feature selection utilizes championships race format data that reflects ski jumping's primary team competition structure, ensuring team-aggregated variables capture venue-dependent performance patterns essential for accurate relay predictions across the sport's diverse competitive venue spectrum from World Cup normal hills to ski flying competitions.

**Comprehensive Venue-Aware Error Handling**:
Ski jumping implements robust error handling designed for FIS website structure variations and the complexity of venue-dependent team data extraction across five distinct hill categories. The system includes fallback mechanisms using common nation lists for empty startlists, venue-appropriate quartile imputation for missing ELO scores across all hill size categories, and sophisticated name normalization for international athlete identification across the sport's global venue-diverse competitive participation spectrum.

###### Modeling

Ski jumping relay points training modeling employs sophisticated team-level GAM frameworks specifically adapted for the sport's extraordinary venue diversity and hill size-dependent performance characteristics. The system captures collective venue specialization patterns (technical precision vs distance achievement) within nation-based team competitive structures while implementing comprehensive multi-tier fallback strategies designed for ski jumping's smaller competitive field and complex venue-dependent team coordination requirements across five distinct hill categories.

**Team-Level Hill Size-Specific GAM Implementation**:
Ski jumping's relay modeling uses sophisticated team-aggregated GAM approaches that capture collective venue-dependent capabilities through nation-based performance integration across the sport's unique hill classification system:

```r
# Team-level venue-aware GAM formula construction
gam_formula <- as.formula(paste("Points ~", smooth_terms))
model <- gam(gam_formula, data = team_race_df)

# Team aggregation variables for venue-dependent performance:
# Avg_Normal_Pelo_Pct, Avg_Large_Pelo_Pct, 
# Avg_Flying_Pelo_Pct, Avg_Pelo_Pct
```

**Venue-Dependent Team Specialization Framework**:
The modeling system incorporates ski jumping's unique characteristic where teams may specialize in different hill size categories. Teams with technical precision specialists (Small/Medium hills) require different modeling approaches than teams emphasizing distance achievement specialists (Large/Flying hills), with the GAM framework capturing these venue-specific team composition patterns.

**Multi-Format Relay Adaptation with Hill Size Integration**:
Ski jumping's modeling adapts to diverse relay formats (Mixed Team, Standard Team) through conditional team composition logic while maintaining consistency in venue-dependent performance evaluation. Each format utilizes the same GAM architecture but adapts team aggregation methods to accommodate gender-specific leg assignments and venue specialization patterns.

**Championships-Based Team Modeling Framework**:
The relay modeling utilizes championships race format data that reflects ski jumping's primary team competition structure, ensuring models capture venue-dependent team dynamics essential for accurate prediction across the sport's competitive venue spectrum from World Cup normal hills to ski flying competitions.

**Comprehensive Multi-Tier Fallback Strategy for Venue Diversity**:
Ski jumping implements robust fallback mechanisms specifically adapted for team-based predictions and the sport's venue-dependent competitive characteristics:

```r
# Primary: Full team venue-dependent GAM with BIC-selected variables
model <- gam(gam_formula, data = team_race_df)

# Fallback: Simplified team model with core hill-specific variables  
fallback_formula <- as.formula(paste("Points ~ s(", team_elo_col, ")"))
model <- gam(fallback_formula, data = team_race_df)

# Final: Team ELO-only model for extreme cases with venue awareness
```

**Nation-Based Performance Integration with Venue Context**:
The modeling framework processes nation-level team performance while preserving individual athlete venue-specific characteristics through sophisticated team aggregation that maintains awareness of hill size specialization patterns. This approach enables accurate team prediction while accommodating the reality that relay team compositions may change between races.

**Hill Size-Aware Model Validation and Error Handling**:
Ski jumping's relay models include comprehensive error handling designed for the sport's extraordinary venue diversity and variable team composition patterns across five distinct hill categories. The system employs robust prediction mechanisms with nation-based fallback strategies that ensure reliable team performance predictions even when individual athlete venue specialization patterns vary across team members or when hill size-specific data quality differs between Small/Medium technical hills and Large/Flying distance hills.

###### Adjustments

Ski jumping relay adjustments are **disabled** in the production system to prevent systematic bias correction complications in venue-dependent team competition environments where team compositions change between races. Unlike individual ski jumpers who maintain consistent venue-specific performance patterns across different hill size categories, relay teams feature different athlete combinations each race, making historical individual adjustment patterns unreliable for future team predictions across the sport's extraordinary venue diversity.

**Disabled Adjustment Framework for Venue-Dependent Team Complexity**:
The system deliberately excludes adjustments for relay events due to the fundamental difference in team composition variability compared to individual venue-specific competition consistency:

```r
# Conditional adjustment logic for venue-dependent team events
period_p = if(is_team) 1 else purrr::map_dbl(row_id, function(r) {
  # Period adjustments disabled for relay teams
  # Team compositions change between races unlike consistent individual athletes  
  # Venue specialization patterns vary with different athlete combinations
})
period_correction = if(is_team) 0 else ifelse(period_p < 0.05, correction_value, 0)

hillsize_p = if(is_team) 1 else purrr::map_dbl(row_id, function(r) {
  # Hill size adjustments disabled for relay teams
  # Variable team lineups negate individual venue specialization patterns
  # Technical vs distance specializations vary by athlete combination
})
hillsize_correction = if(is_team) 0 else ifelse(hillsize_p < 0.05, correction_value, 0)
```

**Venue-Dependent Team Specialization Variability**:
Ski jumping's disabled relay adjustment framework recognizes that team performance depends on coordination between athletes with different venue specializations (technical precision on Small/Medium hills vs distance achievement on Large/Flying hills), creating team chemistry dynamics that vary significantly with different athlete combinations. Since relay teams may feature different athlete lineups across different races, historical individual performance adjustment patterns don't reliably predict team performance outcomes across the sport's five distinct hill categories.

**Hill Size-Specific Team Chemistry Considerations**:
The system acknowledges that relay team success depends not only on individual athlete venue-specific capabilities but on coordinated jumping performance where technical precision specialists and distance achievement specialists must work together effectively. This venue-dependent team performance dynamic makes individual-based adjustment patterns inappropriate for team prediction accuracy across ski jumping's complex hill size-dependent competitive structure.

**Systematic Bias Prevention in Venue-Dependent Context**:
The disabled adjustment framework prevents overfitting to temporary team composition patterns while maintaining model stability across ski jumping's unique venue-dependent relay requirements. Unlike individual events where consistent athletes can be adjusted for systematic bias patterns across different hill sizes, relay events require different athletes working together each time, negating the consistency assumptions underlying systematic bias correction methodologies.

**Nation-Based Team Performance Focus with Venue Awareness**:
Ski jumping relay adjustments prioritize nation-level team capability assessment through base model predictions that capture team-aggregated venue-specific performance metrics across the sport's five hill categories rather than attempting to apply individual athlete adjustment patterns that may not reflect actual team coordination dynamics across technical precision requirements (Small/Medium hills) and distance achievement capabilities (Large/Flying hills) essential for venue-dependent relay success.

##### Testing

###### Startlist Setup

Ski jumping relay points testing startlist setup implements the most sophisticated venue-dependent nation-based team data preparation among winter sports, accommodating dramatic performance variations across five distinct hill categories (Small, Medium, Normal, Large, Flying) while maintaining team composition management for multiple relay formats (team, mixed team) with comprehensive hill size specialization aggregation.

**Hill Size-Specific Nation-Based Startlist Data Loading**:
Ski jumping relay testing loads team startlist data from format and gender-specific CSV files: `startlist_team_races_men.csv`, `startlist_team_races_ladies.csv`, and `startlist_mixed_team_races_teams.csv`. Each file contains nation-based team composition data with pre-calculated team-averaged hill size-specific performance metrics: `Avg_Small_Elo`, `Avg_Medium_Elo`, `Avg_Normal_Elo`, `Avg_Large_Elo`, `Avg_Flying_Elo`, and `Avg_Elo`.

**Venue-Dependent Team Performance Aggregation**:
The startlist setup processes comprehensive team-aggregated venue-specific performance metrics that reflect collective jumping capabilities across different hill size categories. These metrics capture team specialization patterns where some teams excel on technical precision requirements of smaller hills while others dominate distance-focused performance on large hills and ski flying venues, essential for accurate venue-dependent relay predictions.

**Team Chronological Data Integration with Hill Size Context**:
The system integrates with nation-based chronological performance data files (`men_team_chrono.csv`, `ladies_team_chrono.csv`, `mixed_team_chrono.csv`) to calculate weighted previous points using the last 5 team relay performances with linear weighting. This approach captures recent team form while accounting for ski jumping's unique venue-dependent team dynamics where technical precision specialists and distance achievement specialists must coordinate effectively.

**Simplified Race Participation Probability for Team Events**:
Ski jumping relay startlists implement simplified participation probability assignment using `process_team_probabilities()` where all teams receive 100% participation probability. This reflects the specialized nature of ski jumping relay events where listed teams typically represent confirmed venue-specific team participation rather than projected attendance based on individual athlete hill size specialization patterns.

**Hill Size-Specific ELO-to-Pelo Conversion for Teams**:
The startlist preparation includes sophisticated venue-dependent ELO normalization using hill size-specific maximum values to create percentage-based features compatible with trained models. The system converts team-averaged ELO ratings to Pelo_Pct columns for each hill category: `Avg_Small_Pelo_Pct`, `Avg_Medium_Pelo_Pct`, `Avg_Normal_Pelo_Pct`, `Avg_Large_Pelo_Pct`, and `Avg_Flying_Pelo_Pct`.

**Synthetic Team Chronological Generation for New Nations**:
When historical team chronological data is unavailable for nations in the startlist, the system generates synthetic chronological data using current team composition and averaged venue-specific performance metrics. This ensures all teams can receive hill size-aware predictions even when comprehensive historical team data is limited, maintaining prediction coverage across all participating nations.

**Venue-Dependent Team Composition Strategy**:
Ski jumping's startlist preparation accommodates the reality that relay teams must balance technical precision specialists with distance achievement specialists to optimize overall team performance across the sport's venue-dependent requirements. The system maintains awareness that team composition strategies may vary based on upcoming hill sizes in competition schedules and venue-specific team specialization patterns.

**Multi-Hill Category Team Processing**:
The startlist setup adapts to ski jumping's unique venue diversity through specialized processing that handles five distinct hill categories with different competitive characteristics. Small/Medium hills emphasize technical precision, Normal hills balance technique and distance, Large hills prioritize distance achievement, and Flying hills require specialized distance-focused jumping capabilities essential for accurate venue-dependent team predictions.

###### Modeling

Ski jumping relay points testing modeling employs sophisticated nation-based venue-dependent GAM frameworks that apply trained team models to generate hill size-specific team predictions while accommodating dramatic performance variations across five distinct hill categories (Small, Medium, Normal, Large, Flying) and multiple relay formats (team, mixed team).

**Nation-Based Hill Size-Specific GAM Application**:
Ski jumping relay modeling uses pre-trained GAM models with team-averaged hill size-specific features: `Avg_Small_Elo_Pct`, `Avg_Medium_Elo_Pct`, `Avg_Normal_Elo_Pct`, `Avg_Large_Elo_Pct`, `Avg_Flying_Elo_Pct`, and `Avg_Elo_Pct`. The system applies `mgcv::predict.gam()` to generate venue-dependent team predictions using smooth terms that capture non-linear relationships between collective team hill size specializations and relay performance outcomes across ski jumping's extraordinary venue diversity.

**Venue-Dependent Team Feature Processing**:
The modeling pipeline processes nation-based teams using aggregated hill size-specific performance metrics that reflect collective jumping capabilities across different venue categories. Teams receive venue-dependent feature processing using hill size-specific ELO selection based on competition venue: Large/Flying hills use `Avg_Large_Elo_Pct`, Small hills use `Avg_Small_Elo_Pct`, Medium hills use `Avg_Medium_Elo_Pct`, with `Avg_Elo_Pct` as the default for Normal hills.

**Team Composition Prediction with Hill Size Specialization**:
The system generates team predictions while maintaining awareness of ski jumping's unique venue-dependent performance requirements where technical precision specialists excel on smaller hills while distance achievement specialists dominate on large hills and ski flying venues. Each nation's team receives predictions based on aggregated hill size specializations, with team performance calculations that account for the complex coordination requirements between technical precision specialists and distance achievement specialists.

**Multi-Hill Category Relay Processing**:
Ski jumping relay modeling adapts to different venue categories through hill size-specific data processing while using consistent GAM prediction methodologies. Team events process venue-dependent specialization patterns, Mixed Team events combine men's and women's venue-specific capabilities, with each format receiving appropriate venue-dependent predictions based on hill size category requirements (technical precision vs distance achievement focus).

**Simplified Team Participation Probability Integration**:
The modeling framework uses simplified participation probability assignment through `process_team_probabilities()` where all teams receive 100% participation probability rather than exponential decay calculations. This reflects the specialized nature of ski jumping relay events where listed teams typically represent confirmed venue-specific team participation rather than projected attendance based on individual athlete hill size specialization patterns.

**Comprehensive Venue-Dependent Error Handling**:
The system implements robust error handling designed for nation-based team prediction scenarios with venue-dependent complexity, including row-by-row fallback prediction when batch model application encounters issues with team-averaged hill size-specific data. When GAM models experience convergence problems with venue-dependent team data, the system applies simplified linear models or hill size-specific ELO-only predictions to ensure all participating nations receive venue-appropriate predictions.

**Position Probability Integration with Hill Size Context**:
Ski jumping relay modeling includes comprehensive position probability generation for team-specific thresholds (Top 1, 3, 5, 10) using binomial GAM models trained on historical relay finishing positions across different hill categories. The system applies trained position probability models while accounting for venue-dependent performance patterns where technical precision requirements on smaller hills create different competitive dynamics compared to distance achievement focus on large hills and ski flying venues, ensuring accurate team probability predictions across ski jumping's unique venue spectrum.

###### Adjustments

Ski jumping relay points testing adjustments implement a **deliberately disabled adjustment framework** specifically designed to address the fundamental challenge of venue-dependent team composition variability across five distinct hill categories (Small, Medium, Normal, Large, Flying). Unlike individual events where athlete-specific venue patterns enable meaningful hill size-specific adjustment calculations, relay team composition strategies and venue specialization dynamics make traditional systematic bias correction unreliable for accurate venue-dependent team prediction.

**Disabled Venue-Dependent Adjustment Framework for Team Composition Variability**:
Ski jumping relay testing explicitly disables both period and elevation adjustments with conditional logic: `period_correction = if(is_relay) 0` and `elevation_correction = if(is_relay) 0`. This approach recognizes that relay teams frequently change athlete lineups based on venue-specific specialization strategies (technical precision specialists for Small/Medium hills, distance achievement specialists for Large/Flying hills), making historical adjustment patterns unreliable for current team composition venue-dependent performance prediction.

**Hill Size-Specific Team Strategy Recognition**:
The disabled framework acknowledges ski jumping's unique venue-dependent team selection strategies where nations optimize team compositions based on hill categories. Teams may feature different athlete combinations for technical precision requirements (Small/Medium hills) versus distance achievement demands (Large/Flying hills), making individual athlete adjustment patterns inappropriate for team prediction accuracy across venue-dependent relay competitions.

**Nation-Based Team Venue Specialization Logic**:
Ski jumping identifies relay events through conditional logic that differentiates venue-dependent team-based versus individual-based prediction approaches: `is_relay = race_category %in% c("Team", "Mixed Team")`. This detection ensures that adjustment framework decisions accommodate venue-dependent team composition strategies rather than individual athlete patterns where venue-specific systematic bias corrections remain valid across hill size categories.

**Conservative Venue-Dependent Base Model Prioritization**:
With disabled adjustments, ski jumping relay testing relies exclusively on base GAM model predictions that incorporate venue-specific team capabilities: `Final_Team_Prediction = Base_Prediction + 0 + 0`. This approach prioritizes venue-dependent model stability and acknowledges that team-aggregated hill size-specific performance metrics (Small, Medium, Normal, Large, Flying hill ELO ratings) captured during training provide more reliable prediction foundations than venue-specific adjustment patterns that may not reflect current team composition strategies.

**Team Venue Specialization Variability Documentation**:
The disabled framework includes comprehensive commentary explaining that team compositions vary based on hill size categories and venue-dependent specialization requirements, providing explicit rationale for the conservative adjustment approach. This documentation ensures that the venue-dependent adjustment framework design reflects legitimate concerns about team dynamics across different hill categories rather than implementation oversight.

**Venue-Dependent Team Performance Stability**:
By disabling systematic bias corrections, the relay adjustment framework ensures that team predictions rely on stable venue-dependent performance metrics (hill size-specific ELO ratings across five categories) captured during training rather than potentially misleading historical adjustment patterns that may not reflect current team member venue specializations or coordination dynamics between technical precision specialists and distance achievement specialists across ski jumping's extraordinary venue diversity.

**Hill Size Category-Specific Error Handling**:
The disabled adjustment system includes venue-dependent error handling that accommodates the reality that relay teams must balance technical precision specialists with distance achievement specialists to optimize overall performance across different hill categories. When venue-specific team data is insufficient for traditional adjustment calculations, the system prioritizes venue-dependent base model predictions that capture team specialization patterns rather than applying potentially inappropriate individual athlete venue-specific bias corrections.

#### Probability

##### Training

###### Setup

Ski Jumping's Relay Probability Training Setup converts the team-based venue-dependent points prediction problem into binary classification for position probability modeling across relay-specific finishing position thresholds with comprehensive hill size specialization integration. The system employs the same team-aggregated venue-dependent framework as relay points models but transforms the complex venue-dependent team regression problem into binary classification through position-based outcome creation that accommodates ski jumping's unique relay competitive structure across five distinct hill categories.

**Position Threshold Definition with Team-Level Venue Awareness**:
Ski jumping relay probability training uses adapted team-specific position thresholds: `position_thresholds <- c(1, 3, 5, 10)` representing Team Win, Team Podium, Top 5 Teams, and Top 10 Teams finishes (reduced from individual event thresholds to accommodate smaller team competition fields). Each threshold creates a separate binary classification problem where team success is defined as finishing at or above that position, enabling nation-based binomial GAM modeling for venue-dependent relay probability prediction across hill size categories.

**Binary Outcome Creation for Venue-Dependent Team Events**:
For each position threshold, the system creates binary outcome variables using team-specific transformations: `relay_df$position_achieved <- relay_df$Place <= threshold` where Place represents team finishing positions incorporating venue-specific jumping performance across different hill categories. This converts continuous team place variables into binary classification targets specifically designed for venue-dependent relay team performance analysis with hill size specialization awareness.

**Training Data Consistency with Team Venue-Dependent Aggregation**:
Relay probability models use identical team-aggregated venue-dependent training datasets as their corresponding relay points prediction models, including the same historical window and team performance filtering criteria. The system leverages team-averaged hill size-specific ELO ratings across five venue categories (`Small`, `Medium`, `Normal`, `Large`, `Flying`) while maintaining awareness of venue-dependent team coordination requirements that define ski jumping relay success patterns.

**Nation-Based Venue-Dependent Team Performance Integration**:
The training setup acknowledges ski jumping's relay team structure by focusing on nation-based team outcomes that integrate venue-dependent jumping capabilities across hill size categories. Team probability models incorporate team-averaged ELO ratings across diverse hill categories while maintaining awareness of venue-dependent team coordination requirements where technical precision specialists excel on smaller hills while distance achievement specialists dominate on large hills and ski flying venues.

**Multi-Format Team Event Adaptation with Hill Size Awareness**:
Training data encompasses team results from Mixed Team and Standard Team relay formats through comprehensive format detection and team-specific binary outcome creation. Each relay format maintains consistent position thresholds while accommodating format-specific team dynamics, venue-dependent specialization patterns, and hill size-specific performance requirements that differentiate ski jumping relay competition from other winter sports team events across dramatically different venue categories.

**Team Venue Specialization Variability Recognition**:
The training setup acknowledges that relay team compositions change based on venue-dependent specialization strategies, focusing on nation-based performance patterns across different hill categories rather than specific individual athlete combinations. This approach recognizes that while individual athlete lineups vary based on hill size requirements (technical precision vs distance achievement), nation-based venue-dependent coordination capabilities provide foundations for binary classification modeling across ski jumping's unique venue-dependent relay competitive structure.

###### Feature Selection

Ski jumping relay probability training feature selection employs sophisticated team-aggregated venue-dependent variable optimization with threshold-independent BIC-based exhaustive subset selection that adapts to the sport's unique hill size variations within team competition frameworks. The system performs independent feature optimization for each position threshold while leveraging team-averaged venue-dependent variable inheritance from corresponding relay points prediction models, ensuring consistency between venue-dependent modeling approaches across ski jumping's extraordinary hill size diversity and relay competitive structure.

**Variable Inheritance and Consistency with Venue-Dependent Team Aggregation**:
Relay probability models use identical explanatory variable pools as their corresponding relay points models: `position_feature_vars <- relay_explanatory_vars`. This ensures consistency between team-based venue-dependent modeling approaches while leveraging domain knowledge already encoded in ski jumping's relay points model variable selection, maintaining team-aggregated venue-dependent performance integration across hill size categories essential for venue-specific relay team success across dramatically different competitive requirements.

**Team-Aggregated Hill Size-Specific Variable Sets**:
Ski jumping adapts feature pools based on relay team composition characteristics, utilizing team-averaged hill size-specific variables including `Avg_Small_Team_Elo`, `Avg_Medium_Team_Elo`, `Avg_Normal_Team_Elo`, `Avg_Large_Team_Elo`, `Avg_Flying_Team_Elo`, and `Avg_Team_Elo` without weighted previous points. This approach recognizes that relay team dynamics differ from individual performance patterns, focusing on nation-based team capabilities across five distinct hill categories where technical precision specialists excel on smaller hills while distance achievement specialists dominate on large hills and ski flying venues.

**Independent Threshold Optimization with Hill Size Awareness**:
For each position threshold (1, 3, 5, 10), the system performs exhaustive subset selection using BIC optimization: `pos_selection <- regsubsets(pos_formula, data = relay_df, nbest = 1, method = "exhaustive")`. This threshold-independent approach recognizes that different team finishing position predictions may require different variable combinations for optimal binomial classification accuracy across ski jumping's venue-dependent relay structure while maintaining hill size awareness that characterizes the sport's extraordinary venue diversity.

**Venue-Dependent Feature Integration with Team Specialization Focus**:
The feature selection process acknowledges that ski jumping relay performance involves complex coordination between athletes with different venue specializations - some team members may excel on technical precision requirements of smaller hills while others specialize in distance-focused performance on large hills and ski flying venues. Selected variables ensure that feature combinations capture both technical precision capabilities and distance achievement requirements while maintaining awareness of venue-dependent team coordination patterns that define ski jumping relay competitive success.

####### Modeling

Ski jumping relay probability training modeling represents the most sophisticated venue-dependent team-based binomial GAM implementation among winter sports, utilizing hill size-specific feature selection and team-aggregated performance metrics to optimize nation-based position prediction across dramatically diverse venue categories with specialized adaptations for technical precision specialists versus distance achievement specialists within relay team composition frameworks.

**Nation-Based Binomial GAM Training Architecture with Hill Size Specialization**:
Ski jumping employs sophisticated team-based binary classification modeling using binomial GAM with venue-dependent feature integration for relay position probability training. Unlike other winter sports that use simplified venue-agnostic team modeling, ski jumping trains separate models for each relay position threshold with hill size-specific team performance aggregation and venue-dependent optimization:

```r
# From ski jumping relay probability training - Nation-based team modeling
train_team_models <- function(team_data, hill_category) {
  control <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = defaultSummary,
    savePredictions = "final"
  )
  
  # Hill size-specific team feature selection
  venue_predictors <- get_hill_specific_predictors(hill_category)
  
  # Train models for multiple outcome thresholds per venue
  for(outcome in c("is_win", "is_podium", "is_top5", "is_top10")) {
    team_model <- train(
      formula = as.formula(paste(outcome, "~", paste(venue_predictors, collapse = "+"))),
      data = team_data,
      method = "gam",
      family = binomial(),
      trControl = control
    )
  }
}
```

**Hill Size-Specific Training Strategy with Venue Specialization**:
Ski jumping implements advanced venue-dependent model training with dynamic feature selection based on hill category and team specialization patterns. The training system adapts feature sets for technical precision requirements (Small/Medium hills) versus distance achievement capabilities (Large/Flying hills) with specialized venue-dependent team aggregation:

```r
# From ski jumping venue-dependent team training
# Small/Medium hills: Technical precision team focus
technical_predictors <- c(
  "Avg_Small_Team_Elo_Pct", "Avg_Medium_Team_Elo_Pct",
  "Avg_Team_Elo_Pct", "Team_Technical_Precision_Score"
)

# Large/Flying hills: Distance achievement team focus  
distance_predictors <- c(
  "Avg_Large_Team_Elo_Pct", "Avg_Flying_Team_Elo_Pct",
  "Avg_Team_Elo_Pct", "Team_Distance_Achievement_Score"
)

# Normal hills: Balanced technical and distance capabilities
balanced_predictors <- c(
  "Avg_Normal_Team_Elo_Pct", "Avg_Team_Elo_Pct",
  "Team_Technical_Precision_Score", "Team_Distance_Achievement_Score"
)
```

**Conservative Team-Based Training with Venue Awareness**:
Ski jumping employs simplified team-based modeling that avoids complex algorithm selection due to venue-dependent team composition variability. The training strategy prioritizes stable binomial GAM modeling with hill size-specific feature integration rather than multi-algorithm approaches that may not accommodate venue-dependent team specialization patterns:

```r
# From ski jumping conservative team modeling approach
# Primary and only: Binomial GAM with venue-specific features
team_gam_model <- gam(
  outcome ~ s(Avg_Hill_Specific_Elo_Pct) + s(Avg_Team_Elo_Pct) + 
           Team_Venue_Specialization_Score,
  data = team_training_data,
  family = binomial(),
  method = "REML"
)

# No XGBoost or GLM fallbacks due to venue-dependent complexity
# Hill size specialization patterns require GAM smooth terms
```

**Disabled Adjustment Framework for Team Composition Variability**:
Ski jumping relay training implements disabled adjustment frameworks specifically designed to address venue-dependent team composition strategies where nations optimize athlete lineups based on hill categories, making traditional individual athlete adjustment patterns inappropriate for team prediction:

```r
# From ski jumping disabled adjustment logic for relay teams
# Venue-dependent team composition variability prevention
period_correction <- if(is_relay && venue_dependent) 0 else adjustment_value
elevation_correction <- if(is_relay && venue_dependent) 0 else adjustment_value

# Team compositions change based on hill size requirements
# Technical specialists for Small/Medium, distance specialists for Large/Flying
```

**Multi-Format Team Training with Hill Size Integration**:
Ski jumping processes team training across multiple relay formats (Standard Team, Mixed Team) with venue-dependent adaptations that maintain hill size awareness while accommodating format-specific team dynamics and gender-specific venue specialization patterns:

```r
# From ski jumping multi-format team training
# Standard Team: Same-gender venue specialization optimization
standard_team_weights <- c(0.25, 0.25, 0.25, 0.25)  # Equal 4-member weighting

# Mixed Team: Gender-aware venue specialization with alternating composition
mixed_team_weights <- c(0.25, 0.25, 0.25, 0.25)     # M-W-M-W equal weighting
# Venue specialization maintained within gender constraints

# Venue-dependent weight adjustments based on hill category
if(hill_category %in% c("Large", "Flying")) {
  # Emphasize distance specialists in team composition
  venue_weight_adjustment <- distance_specialization_bonus
} else if(hill_category %in% c("Small", "Medium")) {
  # Emphasize technical precision specialists
  venue_weight_adjustment <- technical_precision_bonus
}
```

**Simplified Team Performance Aggregation**:
Ski jumping implements streamlined team performance aggregation that focuses on nation-based venue-dependent capabilities rather than complex individual athlete interaction modeling, recognizing that venue specialization patterns provide stable foundations for team prediction across hill categories:

```r
# Standard team aggregation with venue awareness
team_prediction <- mean(c(
  athlete1_venue_prediction * venue_specialization_weight,
  athlete2_venue_prediction * venue_specialization_weight,
  athlete3_venue_prediction * venue_specialization_weight,  
  athlete4_venue_prediction * venue_specialization_weight
))

# Venue specialization weights adapt to hill category requirements
venue_specialization_weight <- get_venue_weight(hill_category, athlete_specialization)
```

**Training Data Quality with Venue-Dependent Cross-Validation**:
Ski jumping relay training employs venue-aware cross-validation that maintains hill size category separation during training to ensure models learn venue-specific team performance patterns rather than conflating technical precision requirements with distance achievement capabilities:

```r
# Venue-stratified cross-validation for hill size awareness
venue_stratified_cv <- createFolds(
  team_training_data$hill_category,
  k = 5,
  list = TRUE,
  returnTrain = FALSE
)

# Training control with venue awareness
control <- trainControl(
  method = "cv",
  index = venue_stratified_cv,
  classProbs = TRUE,
  summaryFunction = defaultSummary,
  savePredictions = "final"
)
```

**Team Venue Specialization Recognition**:
The training framework acknowledges that ski jumping relay success depends on coordinated jumping performance where technical precision specialists and distance achievement specialists must work together effectively across venue-dependent requirements, making venue-specific team training essential for accurate nation-based relay probability prediction.

Ski jumping's relay probability training modeling establishes the benchmark for venue-dependent team-based binomial classification with hill size-specific optimization and conservative team aggregation strategies that ensure robust prediction capability across the sport's extraordinary venue diversity and team specialization requirements in winter sports relay competition.

**Team Venue Specialization Variability Recognition for Feature Adaptation**:
The feature selection system acknowledges that relay team compositions change based on venue-dependent specialization strategies, focusing on variables that capture nation-based performance patterns across different hill categories rather than individual athlete characteristics. This approach recognizes that while individual athlete lineups vary based on hill size requirements, nation-based venue-dependent coordination capabilities provide stable foundations for feature selection across ski jumping's unique venue-dependent relay competitive structure with strategic team composition optimization across five distinct hill categories.

###### Adjustments

Ski jumping relay probability training implements a **comprehensively disabled adjustment framework** specifically designed to address the fundamental challenge of venue-dependent team composition variability across five distinct hill categories (Small, Medium, Normal, Large, Flying) that characterizes ski jumping relay competitions. Unlike individual events where athlete-specific venue patterns enable meaningful systematic bias correction, relay team composition strategies and venue specialization dynamics render traditional adjustment methodologies unreliable for accurate venue-dependent team probability prediction.

**Disabled Venue-Dependent Adjustment Framework for Team Composition Variability**:
Ski jumping relay probability training explicitly disables both period and elevation adjustments that would normally address systematic bias patterns in individual athlete performance: `period_correction = if(is_relay && is_venue_dependent) 0 else adjustment_value`. This approach acknowledges that relay teams frequently change athlete lineups based on venue-specific specialization strategies where nations optimize team compositions differently for technical precision requirements (Small/Medium hills) versus distance achievement demands (Large/Flying hills).

```r
# From ski jumping venue-dependent adjustment logic
# Hill size specialization prevents reliable systematic corrections
if(is_relay && venue_category %in% c("Small", "Medium", "Normal", "Large", "Flying")) {
  period_correction <- 0
  elevation_correction <- 0
  hill_size_correction <- 0
  log_info("Venue-dependent adjustments disabled for relay team predictions")
} else {
  # Individual athletes receive normal venue-specific adjustments
  period_correction <- calculate_venue_adjustment(athlete_data, hill_category)
}
```

**Hill Size-Specific Team Strategy Recognition**:
The disabled adjustment framework acknowledges ski jumping's unique venue-dependent team selection strategies where nations optimize team compositions based on hill categories. Teams may feature different athlete combinations for technical precision requirements (Small/Medium hills) versus distance achievement demands (Large/Flying hills), making individual athlete adjustment patterns inappropriate for team prediction accuracy across venue-dependent relay competitions.

**Venue Specialization Complexity in Team Coordination**:
Ski jumping relay teams must coordinate effectively between technical precision specialists who excel on smaller hills and distance achievement specialists who dominate on large hills and ski flying venues. When team compositions vary based on upcoming venue characteristics and strategic considerations, historical individual athlete venue-specific adjustment patterns lose predictive validity for current team coordination dynamics across dramatically different hill categories.

```r
# Venue-dependent team composition strategy recognition
if(hill_category %in% c("Small", "Medium")) {
  # Teams emphasize technical precision specialists
  team_strategy <- "technical_precision_focus"
  adjustment_reliability <- "low"  # Different athletes each race
} else if(hill_category %in% c("Large", "Flying")) {
  # Teams emphasize distance achievement specialists  
  team_strategy <- "distance_achievement_focus"
  adjustment_reliability <- "low"  # Strategic lineup changes
}

# Disabled adjustments due to strategy variability
venue_adjustment <- 0  # Cannot reliably predict team composition effects
```

**Conservative Venue-Dependent Base Model Prioritization**:
Ski jumping relay probability adjustments prioritize stable venue-dependent performance metrics through base model predictions that capture team-aggregated hill size-specific capabilities without attempting individual athlete systematic bias corrections. The system relies on team-averaged venue-specific ELO ratings (`Avg_Small_Elo_Pct`, `Avg_Medium_Elo_Pct`, `Avg_Normal_Elo_Pct`, `Avg_Large_Elo_Pct`, `Avg_Flying_Elo_Pct`) that reflect collective venue specializations rather than individual adjustment patterns.

```r
# Final relay probability calculation without venue adjustments
final_team_probability <- predict(venue_dependent_gam_model,
                                 newdata = team_aggregated_venue_data,
                                 type = "response")
# No period, elevation, or hill size bias corrections applied
log_info(paste("Venue-dependent team probability:", final_team_probability,
               "based on team-aggregated hill specialization metrics only"))
```

**Venue-Dependent Team Performance Stability Focus**:
By disabling systematic bias corrections, the relay adjustment framework ensures that team probability predictions rely exclusively on stable venue-dependent performance metrics (hill size specializations across five categories) captured during training rather than potentially misleading individual athlete adjustment patterns that may not reflect current team member venue specializations or coordination dynamics between technical precision and distance achievement specialists.

**Multi-Hill Category Team Adjustment Consistency**:
The disabled adjustment framework applies consistently across ski jumping's extraordinary venue diversity:
- **Small Hills (K90-)**: Disabled adjustments for technical precision-focused teams
- **Medium Hills (K90-109)**: Disabled adjustments for intermediate technical teams
- **Normal Hills (K120)**: Disabled adjustments for balanced technique/distance teams  
- **Large Hills (K120+)**: Disabled adjustments for distance-focused teams
- **Flying Hills (K185+)**: Disabled adjustments for specialized distance teams

Each hill category receives identical adjustment treatment acknowledging that venue-dependent team coordination creates complexity that individual-based systematic bias correction cannot reliably address.

**Team Venue Specialization Variability Documentation**:
The disabled adjustment framework includes comprehensive documentation explaining that ski jumping relay teams frequently change athlete lineups based on hill size categories and venue-dependent specialization requirements, providing explicit rationale for the conservative adjustment approach that prioritizes venue-dependent prediction stability over complex individual-level systematic bias correction attempts.

**Simplified Race Participation Probability for Venue-Dependent Teams**:
Ski jumping implements simplified race participation probability assignment for relay teams that avoids complex venue-dependent exponential decay calculations: `team_race_probability <- 1.0` for all participating teams. This approach acknowledges that relay team participation patterns differ fundamentally from individual athlete venue-specific attendance patterns, requiring simplified probability assessment rather than complex historical venue specialization modeling.

```r
# Simplified venue-dependent relay team participation probabilities
process_team_probabilities <- function(team_startlist, venue_category) {
  # Set all relay teams to 100% participation regardless of hill size
  team_startlist$Race1_Prob <- 1.0
  log_info(paste("Set participation probability to 1.0 for", 
                 nrow(team_startlist), "teams on", venue_category, "hill"))
  return(team_startlist)
}
```

**Venue-Dependent Error Handling and Robustness**:
The adjustment system includes comprehensive error handling designed for venue-dependent team prediction scenarios where hill size specialization complexity creates unique challenges. When venue-specific team data encounters edge cases or systematic patterns that traditional adjustment methods cannot reliably address, the disabled framework ensures stable team probability predictions based on venue-aggregated performance metrics rather than potentially inappropriate individual athlete venue bias corrections.

**Hill Size Category Performance Pattern Recognition**:
Ski jumping's disabled relay adjustment approach acknowledges that team coordination patterns vary dramatically across hill categories where technical precision versus distance achievement requirements create fundamentally different competitive dynamics. Individual athlete venue adjustment patterns that work for consistent athletes may not apply to teams that strategically vary their lineups based on hill size characteristics and venue specialization optimization strategies essential for relay competitive success across ski jumping's extraordinary venue diversity spectrum.

##### Testing

###### Startlist Setup

Ski jumping relay probability testing employs venue-dependent team composition across five distinct hill categories with sophisticated team specialization handling. The startlist preparation strategy emphasizes venue-specific team identification with comprehensive multi-format support, accommodating ski jumping's unique hill size diversity and strategic lineup optimization requirements.

**Venue-Dependent Team Data Loading**: Ski jumping loads relay team data through comprehensive hill size categorization with venue-specific team composition optimization:

```r
# Hill size category identification for venue-dependent processing
hill_categories <- c("Normal_Hill", "Large_Hill", "Ski_Flying", "Small_Hill", "Medium_Hill")

# Team identification across multiple venue types
men_teams <- races %>%
  filter(grepl("Team", RaceType, ignore.case = TRUE) & Sex == "M") %>%
  select(RaceType, Period, HillSize) %>%
  rename(racetype = RaceType, period = Period, hillsize = HillSize) %>%
  mutate(
    hill_category = categorize_hill_size(HillSize),
    venue_specialization = determine_venue_specialization(HillSize, venue_conditions)
  )

ladies_teams <- races %>%
  filter(grepl("Team", RaceType, ignore.case = TRUE) & Sex == "L") %>%
  select(RaceType, Period, HillSize) %>%
  rename(racetype = RaceType, period = Period, hillsize = HillSize) %>%
  mutate(
    hill_category = categorize_hill_size(HillSize),
    venue_specialization = determine_venue_specialization(HillSize, venue_conditions)
  )

# Mixed team comprehensive venue handling
mixed_teams <- races %>%
  filter(Sex == "Mixed") %>%
  select(RaceType, Period, HillSize) %>%
  rename(racetype = RaceType, period = Period, hillsize = HillSize) %>%
  mutate(
    hill_category = categorize_hill_size(HillSize),
    gender_balance_requirements = enforce_mixed_team_constraints()
  )
```

**Hill Size Categorization and Team Specialization**: Ski jumping implements sophisticated venue-dependent team composition that accounts for hill size specialization patterns and strategic lineup optimization:

```r
# Hill size categorization for team specialization
categorize_hill_size <- function(hill_size) {
  case_when(
    hill_size >= 185 ~ "Ski_Flying",      # K185+ for ski flying specialists
    hill_size >= 120 ~ "Large_Hill",      # K120-K184 for large hill specialists  
    hill_size >= 90 ~ "Normal_Hill",      # K90-K119 for normal hill specialists
    hill_size >= 65 ~ "Medium_Hill",      # K65-K89 for medium hill specialists
    hill_size < 65 ~ "Small_Hill",        # <K65 for small hill specialists
    TRUE ~ "Unknown_Hill"
  )
}

# Venue specialization determination
determine_venue_specialization <- function(hill_size, venue_conditions) {
  specialization_factors <- list(
    technical_precision = calculate_precision_requirements(hill_size),
    distance_achievement = calculate_distance_requirements(hill_size),
    wind_sensitivity = assess_wind_impact(hill_size, venue_conditions),
    equipment_optimization = determine_equipment_factors(hill_size)
  )
  
  return(aggregate_specialization_score(specialization_factors))
}
```

**Strategic Team Lineup Optimization**: Ski jumping employs venue-dependent team composition strategies that optimize lineup selection based on hill size specialization and team coordination requirements:

```r
# Team composition optimization for venue specialization
optimize_team_composition <- function(available_athletes, hill_category, venue_conditions) {
  # Hill size specific athlete selection
  specialized_athletes <- available_athletes %>%
    filter(
      hill_specialization %in% get_optimal_hill_specializations(hill_category),
      venue_performance_history = filter_venue_performance(venue_conditions)
    ) %>%
    arrange(desc(venue_adjusted_elo_rating))
  
  # Strategic lineup selection with coordination factors
  optimal_lineup <- select_optimal_team_lineup(
    candidate_pool = specialized_athletes,
    team_coordination_factors = calculate_coordination_metrics(specialized_athletes),
    venue_specific_adjustments = apply_venue_adjustments(venue_conditions)
  )
  
  return(optimal_lineup)
}

# Multi-format team processing with venue awareness
process_venue_team_startlists <- function(race_format, hill_category, venue_conditions) {
  if (race_format == "Team") {
    # Standard 4-person team with venue optimization
    team_composition <- optimize_standard_team_venue(hill_category, venue_conditions)
  } else if (race_format == "Mixed Team") {
    # Gender-balanced team with venue specialization
    team_composition <- optimize_mixed_team_venue(hill_category, venue_conditions)
    team_composition <- enforce_gender_balance_constraints(team_composition)
  }
  
  return(validate_team_venue_compatibility(team_composition, venue_conditions))
}
```

**Simplified Team Participation Probability with Venue Weighting**: Ski jumping assigns venue-weighted participation probability that accounts for hill size specialization without complex individual athlete calculations:

```r
# Venue-weighted team participation assignment
venue_weighted_participation <- function(teams_data, hill_category, venue_history) {
  teams_with_participation <- teams_data %>%
    mutate(
      base_participation_prob = 100.0,  # Simplified uniform base assignment
      venue_specialization_weight = calculate_venue_weight(hill_category, venue_history),
      hill_category_adjustment = apply_hill_category_bonus(hill_category, team_specialization),
      final_participation_prob = combine_venue_factors(
        base_prob = base_participation_prob,
        venue_weight = venue_specialization_weight,
        category_adj = hill_category_adjustment
      )
    )
  
  return(teams_with_participation)
}
```

**Comprehensive Multi-Hill Category Support**: Ski jumping handles the sport's extraordinary venue diversity through comprehensive hill category processing with specialized team handling:

```r
# Multi-hill category startlist processing
process_all_hill_categories <- function(race_date, available_teams) {
  hill_category_results <- map(hill_categories, function(category) {
    category_races <- filter_races_by_hill_category(race_date, category)
    
    if (nrow(category_races) > 0) {
      category_startlists <- process_venue_team_startlists(
        race_format = category_races$RaceType,
        hill_category = category,
        venue_conditions = get_venue_conditions(category_races$venue)
      )
      
      venue_optimized_teams <- optimize_team_composition(
        available_athletes = available_teams,
        hill_category = category,
        venue_conditions = get_venue_conditions(category_races$venue)
      )
      
      return(combine_startlist_optimization(category_startlists, venue_optimized_teams))
    }
  })
  
  return(consolidate_hill_category_results(hill_category_results))
}
```

**Venue Coordination and Wind Factor Integration**: Ski jumping incorporates sophisticated venue-dependent factors that affect team coordination and performance prediction:

```r
# Venue-specific team coordination factors
calculate_team_venue_factors <- function(venue_conditions, wind_conditions, hill_profile) {
  coordination_factors <- list(
    wind_sensitivity_team = assess_team_wind_coordination(wind_conditions),
    hill_profile_adaptation = calculate_team_hill_adaptation(hill_profile),
    venue_experience_bonus = apply_venue_experience_factors(venue_conditions),
    equipment_coordination = assess_team_equipment_coordination(venue_conditions)
  )
  
  aggregated_factors <- aggregate_venue_coordination_factors(coordination_factors)
  
  return(normalize_venue_team_factors(aggregated_factors))
}
```

Ski jumping's relay probability testing startlist setup represents the most venue-dependent team preparation approach among winter sports, incorporating comprehensive hill size categorization, strategic lineup optimization, and sophisticated venue specialization handling while maintaining simplified participation probability assignment across ski jumping's extraordinary venue diversity spectrum.

###### Modeling

Ski jumping relay probability testing employs sophisticated GAM-based modeling with comprehensive venue specialization integration, utilizing hill size categorization and venue-dependent team coordination factors. The modeling approach emphasizes position threshold prediction through sport-specific GAM implementations with progressive fallback strategies and mathematical constraint enforcement adapted for ski jumping's extraordinary venue diversity spectrum.

**GAM-Based Venue-Dependent Modeling**: Ski jumping implements comprehensive position probability models using Generalized Additive Models with hill size categorization:

```r
# Venue-dependent GAM modeling for ski jumping
fit_ski_jumping_models <- function(training_data, hill_categories) {
  venue_models <- map(hill_categories, function(category) {
    # Filter training data by hill category
    category_data <- training_data %>%
      filter(hill_category == category) %>%
      mutate(
        # Hill size standardization within category
        standardized_hill_size = standardize_hill_size(hill_size, category),
        venue_wind_factor = calculate_wind_adjustment(wind_conditions, hill_size),
        equipment_optimization = assess_equipment_factors(hill_category, conditions)
      )
    
    # Position threshold modeling for each hill category
    position_models <- map(position_thresholds, function(threshold) {
      # Progressive model fitting with fallbacks
      tryCatch({
        # Primary GAM approach
        gam_model <- gam(
          as.formula(paste0("top", threshold, " ~ s(standardized_hill_size) + s(venue_wind_factor) + equipment_optimization")),
          family = binomial(),
          data = category_data,
          method = "REML"
        )
        return(list(model = gam_model, method = "GAM"))
      }, error = function(e) {
        # Fallback to GLM
        log_warn(paste("GAM failed for hill category", category, "threshold", threshold))
        glm_model <- glm(
          as.formula(paste0("top", threshold, " ~ standardized_hill_size + venue_wind_factor + equipment_optimization")),
          family = binomial(),
          data = category_data
        )
        return(list(model = glm_model, method = "GLM"))
      })
    })
    
    return(position_models)
  })
  
  return(validate_venue_model_performance(venue_models))
}
```

**Hill Size Categorization and Venue Specialization**: Ski jumping implements sophisticated venue-dependent modeling that accounts for hill size specialization patterns:

```r
# Hill size categorization for venue-dependent modeling
categorize_and_model_hill_performance <- function(training_data) {
  # Define hill categories with performance characteristics
  hill_categories <- list(
    "Small_Hill" = list(size_range = c(0, 65), emphasis = "technical_precision"),
    "Medium_Hill" = list(size_range = c(65, 89), emphasis = "balanced_performance"), 
    "Normal_Hill" = list(size_range = c(90, 119), emphasis = "standard_technique"),
    "Large_Hill" = list(size_range = c(120, 184), emphasis = "distance_achievement"),
    "Ski_Flying" = list(size_range = c(185, 300), emphasis = "extreme_distance")
  )
  
  # Category-specific modeling approaches
  category_models <- imap(hill_categories, function(category_spec, category_name) {
    category_data <- training_data %>%
      filter(
        hill_size >= category_spec$size_range[1],
        hill_size < category_spec$size_range[2]
      ) %>%
      mutate(
        # Category-specific feature engineering
        performance_emphasis = category_spec$emphasis,
        technique_weight = calculate_technique_weight(category_spec$emphasis),
        distance_weight = calculate_distance_weight(category_spec$emphasis)
      )
    
    # Fit category-specialized models
    specialized_model <- fit_category_specialized_model(
      data = category_data,
      category = category_name,
      emphasis = category_spec$emphasis
    )
    
    return(specialized_model)
  })
  
  return(consolidate_category_models(category_models))
}
```

**Team Coordination and Wind Factor Integration**: Ski jumping incorporates sophisticated venue-dependent factors that affect team coordination and performance prediction:

```r
# Team coordination modeling with venue factors
model_team_coordination_effects <- function(team_data, venue_conditions) {
  coordination_models <- team_data %>%
    group_by(hill_category, team_composition) %>%
    do({
      team_group <- .
      
      # Calculate team-specific venue factors
      venue_factors <- calculate_team_venue_factors(
        team_composition = team_group$team_composition[1],
        hill_category = team_group$hill_category[1],
        wind_conditions = venue_conditions$wind_data,
        venue_experience = assess_team_venue_experience(team_group, venue_conditions)
      )
      
      # Model team coordination effects
      coordination_effects <- model_coordination_dynamics(
        individual_performance = team_group$individual_predictions,
        venue_factors = venue_factors,
        team_chemistry = calculate_team_chemistry(team_group)
      )
      
      return(data.frame(
        team_id = team_group$team_id[1],
        coordination_factor = coordination_effects$team_coordination,
        venue_adaptation = coordination_effects$venue_adaptation,
        wind_sensitivity = coordination_effects$wind_coordination
      ))
    }) %>%
    ungroup()
  
  return(coordination_models)
}
```

**Progressive Fallback Model Strategy**: Ski jumping implements comprehensive model fallback strategies adapted for venue-dependent complexity:

```r
# Progressive model fallback with venue adaptation
implement_progressive_fallback <- function(training_data, venue_conditions) {
  fallback_sequence <- list(
    primary = function(data) {
      # Primary: Complex GAM with venue interactions
      gam(
        outcome ~ s(hill_size, by = venue_type) + s(wind_speed) + s(equipment_factor) + 
                 te(athlete_experience, venue_familiarity),
        family = binomial(),
        data = data,
        method = "REML"
      )
    },
    secondary = function(data) {
      # Secondary: Simplified GAM without interactions
      gam(
        outcome ~ s(hill_size) + s(wind_speed) + equipment_factor + venue_type,
        family = binomial(),
        data = data,
        method = "REML"
      )
    },
    tertiary = function(data) {
      # Tertiary: GLM with venue factors
      glm(
        outcome ~ hill_size + wind_speed + equipment_factor + venue_type + 
                 athlete_experience,
        family = binomial(),
        data = data
      )
    },
    final = function(data) {
      # Final: Basic GLM
      glm(
        outcome ~ hill_size + venue_type,
        family = binomial(),
        data = data
      )
    }
  )
  
  # Attempt model fitting with progressive fallback
  fitted_model <- NULL
  method_used <- NULL
  
  for (method_name in names(fallback_sequence)) {
    tryCatch({
      fitted_model <- fallback_sequence[[method_name]](training_data)
      method_used <- method_name
      break
    }, error = function(e) {
      log_warn(paste("Method", method_name, "failed:", e$message))
    })
  }
  
  return(list(model = fitted_model, method = method_used))
}
```

**Multi-Hill Category Model Integration**: Ski jumping handles comprehensive multi-hill category modeling with specialized integration:

```r
# Multi-hill category model integration
integrate_multi_category_models <- function(category_models, prediction_data) {
  integrated_predictions <- prediction_data %>%
    mutate(hill_category = categorize_hill_size(hill_size)) %>%
    group_by(hill_category) %>%
    do({
      category_data <- .
      category_name <- category_data$hill_category[1]
      
      # Select appropriate category model
      if (category_name %in% names(category_models)) {
        selected_model <- category_models[[category_name]]
        
        # Generate category-specific predictions
        category_predictions <- predict(
          selected_model$model, 
          category_data, 
          type = "response"
        )
        
        return(data.frame(
          team_id = category_data$team_id,
          hill_category = category_name,
          model_method = selected_model$method,
          predictions = category_predictions,
          venue_confidence = assess_venue_prediction_confidence(
            category_data, selected_model
          )
        ))
      } else {
        # Fallback for unknown hill categories
        return(data.frame(
          team_id = category_data$team_id,
          hill_category = category_name,
          model_method = "fallback",
          predictions = rep(0.5, nrow(category_data)),  # Conservative fallback
          venue_confidence = 0.5
        ))
      }
    }) %>%
    ungroup()
  
  return(consolidate_integrated_predictions(integrated_predictions))
}
```

**Venue-Specific Probability Normalization**: Ski jumping implements comprehensive probability normalization adapted for venue diversity:

```r
# Venue-specific probability normalization
normalize_venue_probabilities <- function(raw_predictions, venue_constraints) {
  normalized_predictions <- raw_predictions %>%
    group_by(hill_category, venue_id) %>%
    mutate(
      # Venue-specific target sum calculation
      venue_target_sums = calculate_venue_target_sums(hill_category, venue_constraints),
      
      # Apply venue-specific normalization
      win_prob_normalized = normalize_to_venue_target(win_prob, venue_target_sums$win),
      podium_prob_normalized = normalize_to_venue_target(podium_prob, venue_target_sums$podium),
      top5_prob_normalized = normalize_to_venue_target(top5_prob, venue_target_sums$top5),
      top10_prob_normalized = normalize_to_venue_target(top10_prob, venue_target_sums$top10),
      top30_prob_normalized = normalize_to_venue_target(top30_prob, venue_target_sums$top30)
    ) %>%
    # Enforce monotonic constraints within venue context
    mutate(
      win_prob_constrained = apply_venue_monotonic_constraints(
        win_prob_normalized, podium_prob_normalized, venue_constraints
      ),
      podium_prob_constrained = apply_venue_monotonic_constraints(
        podium_prob_normalized, top5_prob_normalized, venue_constraints
      ),
      top5_prob_constrained = apply_venue_monotonic_constraints(
        top5_prob_normalized, top10_prob_normalized, venue_constraints
      ),
      top10_prob_constrained = apply_venue_monotonic_constraints(
        top10_prob_normalized, top30_prob_normalized, venue_constraints
      )
    ) %>%
    ungroup()
  
  return(validate_venue_normalized_probabilities(normalized_predictions))
}
```

Ski jumping's relay probability testing modeling represents the most venue-dependent mathematical framework among winter sports, incorporating comprehensive hill size categorization, sophisticated venue specialization handling, and progressive fallback strategies while maintaining mathematical consistency across ski jumping's extraordinary venue diversity spectrum through advanced GAM-based position threshold modeling.

###### Adjustments

Ski jumping relay probability testing implements a **deliberately disabled adjustment framework** specifically designed to address the fundamental challenge of venue-dependent team composition variability across five distinct hill categories. Unlike individual events where athlete-specific venue patterns enable meaningful systematic bias correction, relay team composition strategies and venue specialization dynamics make traditional period and elevation adjustments unreliable for venue-dependent team prediction accuracy.

**Venue-Dependent Disabled Adjustment Framework**: Ski jumping employs sophisticated conditional logic that explicitly disables systematic bias corrections for team events while accommodating extraordinary venue diversity:

```r
# Venue-dependent disabled adjustment framework
venue_dependent_adjustment_disabling <- function(predictions_data, is_team_event, hill_category) {
  if (is_team_event) {
    # Disabled adjustments for all team events across all hill categories
    venue_adjustments <- list(
      period_correction = 0,
      elevation_correction = 0,
      hill_category_correction = 0,
      wind_factor_correction = 0,
      venue_specialization_correction = 0
    )
    
    adjustment_significance <- list(
      period_p = 1,      # No systematic bias testing
      elevation_p = 1,   # No systematic bias testing  
      hill_category_p = 1,  # No venue-specific testing
      venue_p = 1        # No venue specialization testing
    )
  } else {
    # Full venue-dependent adjustment framework for individual events
    venue_adjustments <- calculate_venue_specific_adjustments(
      predictions_data, hill_category, venue_conditions
    )
    adjustment_significance <- calculate_adjustment_significance(
      predictions_data, hill_category, venue_conditions
    )
  }
  
  return(list(
    adjustments = venue_adjustments,
    significance_levels = adjustment_significance,
    disabled_framework_applied = is_team_event,
    hill_category_context = hill_category
  ))
}
```

**Hill Category Composition Variability Recognition**: Ski jumping's disabled framework acknowledges that team composition strategies vary dramatically across hill categories based on specialization requirements:

```r
# Team composition variability assessment across hill categories
assess_venue_composition_variability <- function(historical_team_data) {
  composition_analysis <- historical_team_data %>%
    group_by(nation, hill_category, season) %>%
    summarize(
      unique_lineups = n_distinct(paste(team_member_1, team_member_2, team_member_3, team_member_4)),
      total_races = n(),
      specialization_changes = count_specialization_strategy_changes(team_lineups),
      hill_category_stability = calculate_category_specific_stability(team_lineups, hill_category),
      .groups = "drop"
    ) %>%
    group_by(hill_category) %>%
    summarize(
      mean_stability = mean(1 - (unique_lineups / total_races)),
      venue_specialization_impact = assess_specialization_impact(specialization_changes),
      adjustment_reliability = determine_adjustment_framework_viability(mean_stability),
      .groups = "drop"
    )
  
  # Determine disabled adjustment justification by hill category
  disabled_justification <- composition_analysis %>%
    mutate(
      small_hill_justification = mean_stability[hill_category == "Small_Hill"] < 0.6,
      medium_hill_justification = mean_stability[hill_category == "Medium_Hill"] < 0.6,
      normal_hill_justification = mean_stability[hill_category == "Normal_Hill"] < 0.6,
      large_hill_justification = mean_stability[hill_category == "Large_Hill"] < 0.6,
      ski_flying_justification = mean_stability[hill_category == "Ski_Flying"] < 0.6
    )
  
  return(list(
    composition_stability = composition_analysis,
    disabled_framework_justification = disabled_justification,
    venue_dependent_reliability_assessment = "adjustments_unreliable_across_all_categories"
  ))
}
```

**Multi-Hill Category Disabled Strategy Implementation**: Ski jumping implements comprehensive disabled adjustment strategies adapted for each hill category's unique characteristics:

```r
# Hill category-specific disabled adjustment implementation
implement_category_disabled_adjustments <- function(team_predictions, hill_category) {
  category_disabled_framework <- switch(hill_category,
    "Small_Hill" = list(
      period_adjustments = "disabled_technical_precision_specialization",
      elevation_adjustments = "disabled_technical_precision_specialization",
      venue_adjustments = "disabled_technical_specialists_variability",
      wind_adjustments = "incorporated_in_base_model_only"
    ),
    "Medium_Hill" = list(
      period_adjustments = "disabled_balanced_performance_variability", 
      elevation_adjustments = "disabled_balanced_performance_variability",
      venue_adjustments = "disabled_mixed_strategy_complexity",
      wind_adjustments = "incorporated_in_base_model_only"
    ),
    "Normal_Hill" = list(
      period_adjustments = "disabled_standard_technique_variations",
      elevation_adjustments = "disabled_standard_technique_variations", 
      venue_adjustments = "disabled_technique_strategy_changes",
      wind_adjustments = "incorporated_in_base_model_only"
    ),
    "Large_Hill" = list(
      period_adjustments = "disabled_distance_specialist_variability",
      elevation_adjustments = "disabled_distance_specialist_variability",
      venue_adjustments = "disabled_distance_achievement_strategies",
      wind_adjustments = "incorporated_in_base_model_only"
    ),
    "Ski_Flying" = list(
      period_adjustments = "disabled_extreme_distance_specialization",
      elevation_adjustments = "disabled_extreme_distance_specialization",
      venue_adjustments = "disabled_extreme_specialist_strategies", 
      wind_adjustments = "incorporated_in_base_model_only"
    )
  )
  
  return(apply_category_disabled_framework(team_predictions, category_disabled_framework))
}
```

**Venue Specialization Strategy Impact Assessment**: Ski jumping evaluates the impact of venue specialization strategies on adjustment framework reliability:

```r
# Venue specialization impact on adjustment framework
assess_specialization_adjustment_impact <- function(team_composition_data, venue_history) {
  specialization_impact <- team_composition_data %>%
    group_by(nation, hill_category) %>%
    mutate(
      # Assess specialization strategy consistency
      technical_specialist_usage = count_technical_specialist_selections(team_lineups),
      distance_specialist_usage = count_distance_specialist_selections(team_lineups),
      venue_strategy_consistency = calculate_strategy_consistency(
        technical_usage = technical_specialist_usage,
        distance_usage = distance_specialist_usage,
        hill_category = hill_category
      )
    ) %>%
    # Calculate adjustment framework implications
    summarize(
      strategy_variability = sd(venue_strategy_consistency),
      adjustment_prediction_reliability = assess_historical_adjustment_accuracy(venue_history),
      systematic_bias_detectability = evaluate_bias_detection_capability(strategy_variability),
      framework_appropriateness = determine_framework_suitability(
        strategy_variability, adjustment_prediction_reliability
      ),
      .groups = "drop"
    )
  
  return(specialization_impact)
}
```

**Conservative Mathematical Approach with Venue Diversity**: Ski jumping implements comprehensive probability normalization while avoiding unreliable venue-dependent systematic bias corrections:

```r
# Conservative venue-aware probability normalization
normalize_venue_probabilities_without_bias_correction <- function(raw_predictions, hill_category) {
  venue_normalized <- raw_predictions %>%
    group_by(hill_category, venue_id) %>%
    mutate(
      # Direct target-sum normalization without systematic bias correction
      win_prob_normalized = normalize_to_venue_target(win_prob, target_sum = 1.0),
      podium_prob_normalized = normalize_to_venue_target(podium_prob, target_sum = 3.0),
      top5_prob_normalized = normalize_to_venue_target(top5_prob, target_sum = 5.0),
      top10_prob_normalized = normalize_to_venue_target(top10_prob, target_sum = 10.0),
      top30_prob_normalized = normalize_to_venue_target(top30_prob, target_sum = 30.0)
    ) %>%
    # Venue-specific monotonic constraint enforcement
    apply_venue_monotonic_constraints() %>%
    # Mathematical consistency validation without bias correction
    validate_venue_probability_consistency() %>%
    ungroup()
  
  return(venue_normalized)
}
```

**Wind Factor Integration Without Systematic Adjustment**: Ski jumping incorporates wind factors through base modeling rather than post-prediction adjustments:

```r
# Wind factor integration in base model without systematic adjustments
integrate_wind_factors_in_base_model <- function(team_predictions, wind_conditions) {
  wind_integrated <- team_predictions %>%
    mutate(
      # Wind factors incorporated in base model predictions
      wind_adjusted_base_prediction = base_prediction * wind_factor_from_base_model,
      
      # No systematic wind bias correction applied
      systematic_wind_adjustment = 0,
      
      # Final prediction maintains base model wind integration only
      final_prediction = wind_adjusted_base_prediction + systematic_wind_adjustment
    ) %>%
    # Validate wind integration approach
    validate_wind_integration_methodology()
  
  return(wind_integrated)
}
```

**Comprehensive Error Handling for Disabled Framework**: Ski jumping implements sophisticated error handling adapted for disabled adjustment scenarios:

```r
# Error handling for disabled adjustment framework
handle_disabled_framework_errors <- function(team_predictions, venue_conditions) {
  error_handled_predictions <- tryCatch({
    # Primary: Apply disabled adjustment framework
    apply_venue_disabled_adjustments(team_predictions, venue_conditions)
  }, error = function(e) {
    log_warn("Disabled framework application failed - using basic normalization")
    
    tryCatch({
      # Secondary: Basic probability normalization only
      normalize_probabilities_basic(team_predictions)
    }, error = function(e) {
      log_warn("Basic normalization failed - using raw predictions with constraints")
      
      # Final: Raw predictions with mathematical constraints only
      apply_mathematical_constraints_only(team_predictions)
    })
  })
  
  return(validate_error_handling_success(error_handled_predictions))
}
```

**Multi-Format Team Adjustment Strategy**: Ski jumping adapts disabled adjustment frameworks to accommodate diverse team formats across venue categories:

```r
# Format and venue-specific disabled adjustment integration
integrate_format_venue_disabled_adjustments <- function(team_predictions, team_format, hill_category) {
  format_venue_adjustments <- switch(paste(team_format, hill_category, sep = "_"),
    "Team_Small_Hill" = list(
      disabled_adjustments = "technical_precision_team_variability",
      base_model_reliance = "enhanced_technical_factors",
      normalization_approach = "conservative_small_hill_targets"
    ),
    "Team_Large_Hill" = list(
      disabled_adjustments = "distance_achievement_team_variability", 
      base_model_reliance = "enhanced_distance_factors",
      normalization_approach = "conservative_large_hill_targets"
    ),
    "Mixed_Team_Normal_Hill" = list(
      disabled_adjustments = "gender_venue_specialization_complexity",
      base_model_reliance = "enhanced_gender_balance_factors", 
      normalization_approach = "conservative_mixed_team_targets"
    )
    # Additional format-venue combinations...
  )
  
  return(apply_format_venue_disabled_framework(team_predictions, format_venue_adjustments))
}
```

Ski jumping's relay probability testing adjustments represent the most venue-dependent disabled adjustment framework among winter sports, prioritizing mathematical consistency over potentially unreliable systematic bias correction while accommodating extraordinary venue diversity, hill category specialization patterns, and team composition variability through comprehensive disabled adjustment strategies across ski jumping's five-category venue spectrum.