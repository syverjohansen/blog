---
title: "Alpine Skiing Race Picks Methodology"
date: 2023-12-01T01:23:07+00:00
draft: True
tags: ["methodology", "skiing", "race-picks", "alpine"]
---

## Alpine Skiing

### Individual

#### Data Gathering

Alpine skiing individual race startlists are scraped from the FIS websites using automated Python scripts that run based on scheduled race dates. The system processes only individual events as alpine skiing does not have team competitions, with specialized handling for various alpine disciplines and race formats.

**Race Discovery and Scheduling**: The system identifies races to process using UTC timezone-based scheduling with sport-specific paths:

```python
# From startlist-scrape-races.py:32-36
# Get today's date in UTC 
today_utc = datetime.now(timezone.utc)
today_str = today_utc.strftime('%m/%d/%Y')

print(f"Today's date (UTC): {today_str}")
```

**Alpine-Specific Race Processing**: The system processes only individual alpine races, filtering out any non-individual event types:

```python
# From startlist-scrape-races.py:98-100
# Filter races by type: individual races only for alpine (no team events)
individual_races = races_df[
    (races_df['Sex'].isin(['M', 'L']))  # Individual men's or ladies' races
]
```

**FIS Website Data Extraction**: Alpine athlete data is scraped from official FIS race result pages with alpine-specific sector codes:

```python
# From startlist_common.py:84-95
def get_fis_race_data(race_id: str, sector_code: str = 'AL') -> Tuple[List[Dict], Dict]:
    """
    Extract race information from FIS website for alpine skiing
    
    Args:
        race_id: FIS race ID
        sector_code: Sport code (AL = Alpine)
        
    Returns:
        Tuple containing (list of athletes, race metadata)
    """
    url = f"https://www.fis-ski.com/DB/general/results.html?sectorcode={sector_code}&raceid={race_id}"
```

**Alpine Discipline Detection**: The system identifies specific alpine disciplines from race metadata:

```python
# From startlist_common.py:144-153
# Try to find discipline and race type
race_type_elem = soup.select_one('.event-header__subtitle')
if race_type_elem:
    race_type = race_type_elem.text.strip()
    event_info['RaceType'] = race_type
    print(f"Race type: {race_type}")
    
    # Extract discipline from race type
    discipline = determine_discipline_from_race_type(race_type)
    event_info['Discipline'] = discipline
```

**Individual Athlete Data Processing**: Alpine athletes are extracted with comprehensive performance and identification data:

```python
# From startlist_common.py:167-199
def extract_individual_results(soup: BeautifulSoup) -> List[Dict]:
    """Extract individual athlete results from FIS alpine skiing race page"""
    athletes = []
    
    try:
        # First try the newer format with .table-row class
        athlete_rows = soup.select('.table-row')
        print(f"Found {len(athlete_rows)} .table-row elements in extract_individual_results")
        
        if not athlete_rows:
            # If no .table-row found, try events-info-results format
            print("No .table-row found, trying events-info-results format...")
            events_results = extract_events_info_results(soup)
            if events_results:
                return events_results
            
            # If that also fails, try alternative format with result cards
            print("No events-info-results found, trying alternative format...")
            return extract_alternative_format_results(soup)
        
        # Process .table-row elements from main extract function
        for row in athlete_rows:
            # Get rank
            rank_elem = row.select_one('.g-lg-1.g-md-1.g-sm-1.g-xs-2.justify-right.pr-1.bold')
            if not rank_elem:
                continue  # Skip if no rank (might be header or footer)
            
            rank = rank_elem.text.strip()
```

**Weekly Picks Integration**: The system integrates with weekly predictions through alpine-specific R script execution:

```python
# From startlist_common.py:14-29
def check_and_run_weekly_picks():
    """
    Check if there are races with today's date in weekends.csv and run the weekly-picks-alpine.R script if true.
    """
    # Get today's date in UTC
    today_utc = datetime.now(timezone.utc).strftime('%m/%d/%Y')
    print(f"Checking for races on today's date (UTC): {today_utc}")
    
    # Path to weekends.csv for alpine skiing
    weekends_csv_path = os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/weekends.csv')
    
    # Path to R script for alpine skiing
    r_script_path = os.path.expanduser('~/blog/daehl-e/content/post/alpine/drafts/weekly-picks-alpine.R')
```

**Multi-Format Result Processing**: The system handles multiple FIS result page formats with intelligent fallback mechanisms:

```python
# From startlist_common.py:176-185
if not athlete_rows:
    # If no .table-row found, try events-info-results format
    print("No .table-row found, trying events-info-results format...")
    events_results = extract_events_info_results(soup)
    if events_results:
        return events_results
    
    # If that also fails, try alternative format with result cards
    print("No events-info-results found, trying alternative format...")
    return extract_alternative_format_results(soup)
```

The Alpine Skiing individual data gathering system provides specialized alpine discipline processing through FIS alpine sector code utilization, individual-only event filtering, comprehensive discipline detection, multi-format result extraction, weekly prediction integration, and robust fallback mechanisms that ensure reliable data collection across all alpine skiing race formats and FIS website variations.

**Evidence from `startlist-scrape-races.py` lines 15-96:**
The Python orchestration script processes races by reading `races.csv` and filtering for today's date. It uses UTC timezone with format strings `%m/%d/%Y` and fallback to `%m/%d/%y` if no matches found. The script identifies closest available date if exact match fails.

**Evidence from `startlist-scrape-races.py` lines 98-108:**
Individual races are filtered by checking `Sex` column for 'M' and 'L' values. Team events are excluded at this stage.

**Evidence from `startlist_common.py` lines 84-95:**
The `get_fis_race_data()` function scrapes FIS websites using race IDs and sector code 'AL' for Alpine. It constructs URLs like `https://www.fis-ski.com/DB/general/results.html?sectorcode=AL&raceid={race_id}`.

**Evidence from `startlist-scrape-races.py` lines 294-404:**
When FIS startlists are available (race_id exists), `create_race_startlist()` is called. When not available, `create_season_startlist()` creates comprehensive startlists using all current season athletes from chronological data.

**Evidence from `startlist-scrape-races.py` lines 322-342:**
Athlete matching uses exact name comparison first, then `fuzzy_match_name()` for spelling variations. The system falls back to season startlists when no ELO match is found.

**Evidence from `startlist-scrape-races.py` lines 365-371:**
ELO integration adds columns: `'Elo', 'Downhill_Elo', 'Super.G_Elo', 'Giant.Slalom_Elo', 'Slalom_Elo', 'Combined_Elo', 'Tech_Elo', 'Speed_Elo'` and calculates race-specific ELO using `get_race_specific_elo()`.

### Points

#### Training

##### Setup

The training data setup for Alpine skiing follows a comprehensive preprocessing pipeline that prepares historical race data for points prediction models.

**Historical Data Loading and Points Assignment**:
The system begins by loading historical race data from CSV files and assigning World Cup points to all historical results based on finishing positions. Points are calculated using discipline-specific point systems:

```r
# From race-picks.R:680-700
preprocess_data <- function(df) {
  # Load races data to determine points systems for historical races
  races_data <- read.csv("~/ski/elo/python/alpine/polars/excel365/races.csv", 
                         stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date, format="%Y-%m-%d"))
  
  # First calculate points using data with appropriate points system
  df_with_points <- df %>%
    # Add points based on the discipline if they don't already exist
    mutate(
      Points = if("Points" %in% names(df)) {
        Points
      } else {
        mapply(function(place, distance) {
          get_points(place, distance)
        }, Place, Distance)
      }
    ) %>%
    # Sort
    arrange(Season, Race, Place)
```

**Weighted Previous Points Calculation**:
For each athlete and discipline combination, the system calculates weighted previous points using a rolling 5-race window with linearly increasing weights (1, 2, 3, 4, 5 for the most recent race):

```r
# From race-picks.R:705-716
# Calculate weighted previous points separately for each discipline
df_with_points <- df_with_points %>%
  # Group by ID and discipline
  group_by(!!sym(id_col), Distance) %>%
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

**Pre-Race ELO Processing**:
The system uses "Pelo" (pre-race ELO) columns for training to avoid data leakage. These represent ELO ratings before each race occurred:

```r
# From race-picks.R:718-727
# For training, use Pelo columns (pre-race ELO) but name them as Elo_Pct for consistency
pelo_cols <- c("Downhill_Pelo", "Super.G_Pelo", "Giant.Slalom_Pelo", "Slalom_Pelo", "Combined_Pelo", "Tech_Pelo", "Speed_Pelo", "Pelo")

# Make sure Pelo columns exist (create if missing)
for (col in pelo_cols) {
  if (!col %in% names(df_with_points)) {
    log_info(paste("Creating missing Pelo column:", col))
    df_with_points[[col]] <- 0
  }
}
```

**Temporal and Contextual Feature Engineering**:
The system adds several contextual features to capture seasonal and discipline-specific patterns:

```r
# From race-picks.R:730-748
processed_df <- df_with_points %>%
  # Add period (Alpine has 4 periods per season)
  group_by(Season) %>%
  mutate(
    Num_Races = max(Race),
    Period = case_when(
      Num_Races <= 8 ~ 1,   # Early season (Oct-Nov)
      Num_Races <= 16 ~ 2,  # Mid season (Dec-Jan)
      Num_Races <= 24 ~ 3,  # Late season (Feb)
      TRUE ~ 4              # Final season (Mar)
    )
  ) %>%
  ungroup() %>%
  # Add discipline flags for alpine
  mutate(
    Tech_Flag = ifelse(Distance %in% c("Slalom", "Giant Slalom"), 1, 0),
    Speed_Flag = ifelse(Distance %in% c("Downhill", "Super G"), 1, 0),
    Combined_Flag = ifelse(Distance %in% c("Combined", "Alpine Combined"), 1, 0)
  )
```

**Data Filtering and ELO Percentage Calculation**:
Training data is filtered to focus on recent performance and competitive athletes, then ELO percentages are calculated within each race:

```r
# From race-picks.R:749-776
# Filter relevant races and add cumulative points
filter(
  Season >= max(Season-10)
) %>%
group_by(!!sym(id_col), Season) %>%
mutate(Cumulative_Points = cumsum(Points)) %>%
ungroup() %>%
# Calculate percentages for each Pelo column but name as Elo_Pct for consistency
mutate(
  across(
    all_of(pelo_cols),
    ~{
      max_val <- max(.x, na.rm = TRUE)
      if (max_val == 0) return(rep(0, length(.x)))
      .x / max_val
    },
    .names = "{str_replace(.col, 'Pelo', 'Elo')}_Pct"
  )
) %>%
ungroup()
```

**Missing Value Imputation**:
The system handles missing ELO values by replacing them with first quartile values, recognizing that lack of experience typically correlates with lower performance.

This comprehensive setup creates a robust training dataset with temporal context, discipline-specific features, weighted historical performance, and properly normalized ELO ratings for effective GAM model training.

##### Feature Selection

Alpine skiing's feature selection process reflects the sport's fundamental characteristic: different disciplines require entirely different skill sets and predictive variables. The system uses a sophisticated three-tier approach that adapts to discipline-specific requirements.

**Discipline-Specific Variable Sets**:
The system defines different candidate variables based on race discipline, recognizing that predictive factors vary dramatically between speed and technical events:

```r
# From race-picks.R:1106-1117
# Define explanatory variables based on discipline
if(races$discipline[i] %in% c("Downhill", "Super G")) {
  explanatory_vars <- c("Prev_Points_Weighted", 
                        "Downhill_Elo_Pct", "Super.G_Elo_Pct", "Giant.Slalom_Elo_Pct", "Speed_Elo_Pct", "Elo_Pct")
} else if(races$discipline[i] %in% c("Slalom", "Giant Slalom")) {
  explanatory_vars <- c("Prev_Points_Weighted", "Super.G_Elo_Pct",
                        "Slalom_Elo_Pct", "Giant.Slalom_Elo_Pct", "Tech_Elo_Pct", "Elo_Pct")
} else {
  explanatory_vars <- c("Prev_Points_Weighted", "Downhill_Elo_Pct", "Super.G_Elo_Pct", "Giant.Slalom_Elo_Pct", 
                        "Slalom_Elo_Pct", "Giant.Slalom_Elo_Pct", 
                        "Combined_Elo_Pct", "Tech_Elo_Pct", "Speed_Elo_Pct", "Elo_Pct")
}
```

**Exhaustive BIC Optimization**:
The system uses an exhaustive subset selection approach with Bayesian Information Criterion (BIC) optimization to identify the most predictive variable combination while preventing overfitting:

```r
# From race-picks.R:1120-1127
formula <- as.formula(paste(response_variable, "~", paste(explanatory_vars, collapse = " + ")))
tryCatch({
  exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
  summary_exhaustive <- summary(exhaustive_selection)
  best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
  smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
  gam_formula <- as.formula(paste("Points ~", smooth_terms))
  
  model <- gam(gam_formula, data = race_df_75)
```

**GAM Smooth Term Conversion**:
Selected variables are automatically converted to smooth spline terms for Generalized Additive Model (GAM) fitting, allowing the model to capture non-linear relationships between predictors and performance.

**Robust Fallback System**:
The feature selection includes multiple fallback levels if the primary selection fails:

```r
# From race-picks.R:1128-1141
}, error = function(e) {
  log_warn(paste("Error in model selection:", e$message))
  # Fallback to a simpler model with reduced degrees of freedom
  tryCatch({
    # Try with reduced degrees of freedom
    fallback_formula <- as.formula(paste("Points ~ s(", elo_col, ", k=3) + s(Period, k=3) + s(Tech_Flag, k=3)"))
    model <<- gam(fallback_formula, data = race_df_75)
  }, error = function(e2) {
    log_warn(paste("Error in fallback GAM model:", e2$message))
    # Try linear terms only
    tryCatch({
      linear_formula <- as.formula(paste("Points ~", elo_col))
      model <<- lm(linear_formula, data = race_df_75)
      log_info("Using linear model as final fallback")
```

This multi-layered approach ensures that models are built successfully even when data sparsity or collinearity issues prevent optimal feature selection, maintaining prediction capability across all alpine disciplines and race conditions.

##### Modeling

Alpine skiing's modeling approach employs sophisticated Generalized Additive Models (GAM) with comprehensive error handling and multiple evaluation strategies. The system creates both points prediction models and position probability models, each optimized for their specific prediction tasks.

**Primary GAM Implementation**:
The main modeling approach uses smoothed terms for optimal nonlinear relationship capture between predictors and alpine skiing performance:

```r
# From race-picks.R:1124-1127
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))

model <- gam(gam_formula, data = race_df_75)
```

**Hierarchical Fallback Architecture**:
Alpine skiing implements the most comprehensive fallback system to handle data sparsity and model fitting challenges across diverse alpine disciplines:

```r
# From race-picks.R:1128-1150
}, error = function(e) {
  log_warn(paste("Error in model selection:", e$message))
  # Fallback to a simpler model with reduced degrees of freedom
  tryCatch({
    # Try with reduced degrees of freedom
    fallback_formula <- as.formula(paste("Points ~ s(", elo_col, ", k=3) + s(Period, k=3) + s(Tech_Flag, k=3)"))
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

**Position Probability Modeling with REML Estimation**:
The system creates binomial GAM models for each finishing position threshold, using REML (Restricted Maximum Likelihood) estimation for optimal smoothing parameter selection:

```r
# From race-picks.R:1180-1183
position_model <- gam(pos_gam_formula,
                      data = race_df,
                      family = binomial,
                      method = "REML")
```

**Model Validation with Brier Scores**:
Position probability models are rigorously evaluated using Brier scores to assess probabilistic prediction accuracy:

```r
# From race-picks.R:1185-1188
# Calculate Brier score for model evaluation
predicted_probs <- predict(position_model, newdata = race_df, type = "response")
brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
log_info(paste("Brier score for threshold", threshold, ":", round(brier_score, 4)))
```

**Multi-Tier Position Model Fallback Strategy**:
Position models include comprehensive fallback mechanisms to ensure robust probability predictions:

```r
# From race-picks.R:1254-1273
}, error = function(e) {
  log_warn(paste("Error in position model for threshold", threshold, ":", e$message))
  
  # Create a simpler fallback model with just the elo column
  fallback_vars <- c("Prev_Points_Weighted", elo_col)
  fallback_vars <- fallback_vars[fallback_vars %in% names(race_df)]
  
  if(length(fallback_vars) > 0) {
    fallback_terms <- paste("s(", fallback_vars, ")", collapse=" + ")
    fallback_formula <- as.formula(paste("position_achieved ~", fallback_terms))
    
    position_models[[paste0("threshold_", threshold)]] <- gam(
      fallback_formula,
      data = race_df,
      family = binomial,
      method = "REML"
    )
  }
}
```

**Discipline-Specific Model Optimization**:
Alpine skiing's modeling accounts for the fundamental differences between speed disciplines (Downhill, Super-G) and technical disciplines (Giant Slalom, Slalom), ensuring that models capture the distinct performance characteristics and predictive patterns unique to each discipline category.

This comprehensive modeling framework ensures robust point predictions and accurate probability assessments across alpine skiing's diverse discipline spectrum, with automatic adaptation to data availability and model fitting challenges.

##### Adjustments

Alpine skiing implements a sophisticated dual-adjustment system that accounts for both seasonal periodization and discipline-specific performance patterns. The adjustment methodology recognizes that alpine skiers may consistently over- or under-perform relative to their base model predictions during specific periods or in certain discipline categories.

**Sequential Adjustment Calculation**:
The system calculates adjustments in a specific sequence to avoid double-counting effects and ensure accurate correction application:

```r
# From race-picks.R:1299-1346
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

**Period-Based Adjustment Analysis**:
The system identifies statistically significant seasonal performance patterns through comparative t-testing between current period performance and all other periods:

```r
# From race-picks.R:1314-1328
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

**Discipline-Specific Adjustment Calculation**:
Alpine skiing's most distinctive adjustment captures performance differences between technical disciplines (Giant Slalom, Slalom) versus speed disciplines (Downhill, Super-G):

```r
# From race-picks.R:1329-1341
# Step 3: Calculate Discipline adjustments
discipline_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_tech_curr <- Prediction_Diff[Tech_Flag == Tech_Flag[r] & row_id < r]
  prior_tech_other <- Prediction_Diff[Tech_Flag != Tech_Flag[r] & row_id < r]
  if(length(prior_tech_curr) < 3 || length(prior_tech_other) < 3) return(1)
  tryCatch({
    t.test(prior_tech_curr, prior_tech_other)$p.value
  }, error = function(e) 1)
}),
discipline_correction = ifelse(discipline_p < 0.05,
                               mean(Prediction_Diff[Tech_Flag == Tech_Flag], na.rm = TRUE),
                               0)
```

**Combined Adjustment Application**:
All significant adjustments are combined additively to create final adjusted predictions:

```r
# From race-picks.R:1343-1345
# Combine adjustments
Adjusted_Prediction = Initial_Prediction + period_correction + discipline_correction
```

**Advanced Volatility and Risk Metrics**:
Alpine skiing incorporates sophisticated volatility tracking using rolling window calculations to assess prediction uncertainty and performance patterns:

```r
# From race-picks.R:1354-1385
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

**Statistical Significance Requirements**:
All adjustments require statistical significance (p < 0.05) and minimum data thresholds (≥3 observations per category) to ensure adjustments are based on meaningful patterns rather than random variations. This conservative approach prevents overfitting to short-term performance fluctuations while capturing genuine athlete-specific performance characteristics across alpine skiing's diverse discipline spectrum.

#### Testing

##### Startlist Setup

Alpine skiing's startlist setup for testing employs a comprehensive data preparation pipeline that integrates multiple data sources to create prediction-ready datasets. The system handles the complexities of alpine's multi-discipline structure while ensuring robust data quality for model prediction.

**Race Participation Probability Calculation**:
Alpine startlist setup begins by calculating race-specific participation probabilities for each skier-discipline combination. The system uses an exponential decay model that weights recent race participation more heavily:

```r
# From race-picks.R:444-484 in get_race_probability()
get_race_probability <- function(chronos, participant, discipline) {
  participant_races <- chronos %>%
    filter(Skier == participant, Distance == discipline) %>%
    arrange(Date, Season, Race)
  
  if(nrow(participant_races) == 0) return(0)
  
  total_races <- nrow(participant_races)
  if(total_races > 0) {
    # Exponential decay weights (α = 0.1)
    race_weights <- exp(-0.1 * ((total_races-1):0))
    participation <- rep(1, total_races)
    
    weighted_participation <- sum(participation * race_weights)
    total_weight <- sum(race_weights)
    prob <- weighted_participation / total_weight
    return(prob)
  }
  return(0)
}
```

**Most Recent ELO Rating Retrieval**:
The system retrieves the most current ELO ratings for each athlete by taking the chronologically latest entry, ensuring predictions use the most up-to-date performance assessments:

```r
# From race-picks.R:586-608 in prepare_startlist_data()
most_recent_elos <- race_df %>%
  filter(Skier %in% base_df$Skier) %>%
  group_by(Skier) %>%
  arrange(Date, Season, Race) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(Skier, any_of(elo_cols))
```

**ELO Score Normalization to Percentages**:
All ELO ratings are converted to percentage form by normalizing against the maximum observed value, creating standardized 0-1 scale features for consistent model input:

```r
# From race-picks.R:631-649
for(col in elo_columns_to_process) {
  pct_col <- paste0(col, "_Pct")
  if(col %in% names(result_df)) {
    if(col %in% names(race_df)) {
      max_val <- max(race_df[[col]], na.rm = TRUE)
      if(!is.na(max_val) && max_val > 0) {
        result_df[[pct_col]] <- result_df[[col]] / max_val
      } else {
        result_df[[pct_col]] <- 0.5  # Default middle value
      }
    }
  } else {
    result_df[[pct_col]] <- 0.5  # Default if missing
  }
}
```

**Weighted Previous Points Calculation**:
Recent performance is captured through weighted previous points using the last 5 races with linear increasing weights that emphasize more recent results:

```r
# From race-picks.R:599-608
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

**Comprehensive Missing Value Imputation**:
Alpine skiing uses a sophisticated NA handling strategy that preserves data quality while ensuring complete datasets for model prediction:

```r
# From race-picks.R:17-22
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Application during startlist preparation:
result_df <- result_df %>%
  mutate(
    across(ends_with("_Pct"), ~replace_na_with_quartile(.x)),
    Prev_Points_Weighted = replace_na(Prev_Points_Weighted, 0)
  )
```

**Data Integration and Final Preparation**:
The startlist setup concludes by combining all data sources through left joins, ensuring every athlete has complete feature sets for prediction while preserving race probability information from the original startlist. This creates a comprehensive, prediction-ready dataset that captures alpine skiing's discipline-specific performance patterns and maintains data quality standards essential for accurate GAM model prediction.

##### Modeling

Alpine skiing's testing modeling employs sophisticated GAM application with comprehensive error handling and adjustment integration. The system applies trained models to prepared startlist data while maintaining robust fallback mechanisms and statistical validation.

**Primary GAM Model Application**:
The system applies discipline-specific trained GAM models to the prepared startlist data, using the same feature sets that were optimized during training. Speed events (Downhill/Super-G) and technical events (Slalom/Giant Slalom) use different variable combinations:

```r
# From race-picks.R:1610-1612
startlist_prepared <- prepare_startlist_data(startlist, race_df, elo_col)
Base_Prediction = predict(model, newdata = startlist_prepared)
```

**Comprehensive Four-Tier Fallback System**:
Alpine implements the most robust fallback hierarchy to ensure predictions are always generated, even when primary models encounter issues:

```r
# From race-picks.R:1128-1150
# Level 1: Primary GAM with BIC-selected variables
model <- gam(gam_formula, data = race_df_75)

# Level 2: Reduced complexity GAM  
fallback_formula <- as.formula(paste("Points ~ s(", elo_col, ", k=3) + s(Period, k=3) + s(Tech_Flag, k=3)"))
model <- gam(fallback_formula, data = race_df_75)

# Level 3: Linear regression
linear_formula <- as.formula(paste("Points ~", elo_col))
model <- lm(linear_formula, data = race_df_75)

# Level 4: Simple linear model (ultimate fallback)
simple_formula <- as.formula(paste("Points ~", elo_col))
model <- lm(simple_formula, data = race_df_75)
```

**Robust Prediction with Error Handling**:
The prediction process includes extensive error handling with row-by-row fallback mechanisms to handle individual athlete data issues:

```r
# From race-picks.R:1493-1517
base_predictions <- tryCatch({
  mgcv::predict.gam(model, newdata = prediction_subset, type = "response")
}, error = function(e) {
  # Row-by-row fallback prediction
  result <- numeric(nrow(prediction_subset))
  for(j in 1:nrow(prediction_subset)) {
    single_row <- prediction_subset[j,, drop = FALSE]
    result[j] <- tryCatch({
      mgcv::predict.gam(model, newdata = single_row, type = "response")
    }, error = function(e2) {
      threshold/100  # Default value based on threshold
    })
  }
  return(result)
})
```

**Historical Adjustment Integration**:
Testing predictions are enhanced by applying historically-derived adjustments that capture systematic performance patterns:

```r
# From race-picks.R:1610-1656
race_dfs[[i]] <- startlist_prepared %>%
  mutate(
    Base_Prediction = predict(model, newdata = .),
    # Apply historical period and discipline adjustments
    Predicted_Points = Base_Prediction + period_adjustment + discipline_adjustment,
    # Apply race probability weighting
    Final_Prediction = Predicted_Points * Race_Prob,
    # Generate confidence scenarios using volatility
    Safe_Prediction = (Predicted_Points - volatility_adjustment) * Race_Prob,
    Upside_Prediction = (Predicted_Points + volatility_adjustment) * Race_Prob
  )
```

**Position Probability Model Application**:
Separate GAM models for each position threshold (1, 3, 5, 10, 30) use binomial regression with comprehensive validation:

```r
# From race-picks.R:1185-1188
predicted_probs <- predict(position_model, newdata = race_df, type = "response")
brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
log_info(paste("Brier score for threshold", threshold, ":", round(brier_score, 4)))
```

**Confidence Factor Integration**:
The system incorporates confidence factors based on recent race history and volatility assessments to adjust prediction certainty and generate multiple prediction scenarios (safe, base, upside) that reflect prediction uncertainty.

### Probability

#### Training

##### Setup

Alpine skiing's Individual Probability Training Setup converts the points prediction problem into binary classification for position probability modeling across five finishing position thresholds. The system uses the same preprocessed historical race data as points models but transforms the regression problem into classification through binary outcome creation.

**Position Threshold Definition**:
Alpine skiing uses standard individual event thresholds: `position_thresholds <- c(1, 3, 5, 10, 30)` representing Win, Podium, Top 5, Top 10, and Top 30 finishes. Each threshold creates a separate binary classification problem where success is defined as finishing at or above that position.

**Binary Outcome Creation**:
For each position threshold, the system creates binary outcome variables using the fundamental transformation: `race_df$position_achieved <- race_df$Place <= threshold`. This converts the continuous place variable into binary classification targets, enabling binomial GAM modeling for probability prediction.

**Training Data Consistency**:
Position probability models use identical training datasets as their corresponding points prediction models, including the same 10-season historical window, top performer filtering (ELO > 75th percentile), and discipline-specific preprocessing. No separate data pipeline is required - the same `race_df` serves both modeling approaches.

**Discipline-Specific Adaptation**:
Alpine skiing's unique multi-discipline structure requires threshold evaluation across different event types. Technical disciplines (Giant Slalom, Slalom) and speed disciplines (Downhill, Super-G) maintain the same position thresholds but may exhibit different probability patterns due to varying field sizes and competitive dynamics.

##### Feature Selection

Alpine skiing's Individual Probability Training Feature Selection employs a sophisticated threshold-independent optimization strategy that adapts to the sport's unique multi-discipline structure. The system performs independent BIC optimization for each position threshold while leveraging discipline-specific variable inheritance from the corresponding points prediction models.

**Variable Inheritance and Consistency**:
Position probability models use identical explanatory variable pools as their corresponding points models: `position_feature_vars <- explanatory_vars`. This ensures consistency between modeling approaches while leveraging domain knowledge already encoded in discipline-specific points model variable selection.

**Discipline-Specific Variable Sets**:
Alpine skiing adapts feature pools based on race discipline characteristics. Speed events (Downhill, Super-G) utilize variables including `Prev_Points_Weighted`, `Downhill_Elo_Pct`, `Super.G_Elo_Pct`, `Speed_Elo_Pct`, and `Elo_Pct`. Technical events (Giant Slalom, Slalom) employ `Tech_Elo_Pct`, `Slalom_Elo_Pct`, `Giant.Slalom_Elo_Pct` alongside weighted previous points. Combined events utilize the complete variable set across all disciplines.

**Independent Threshold Optimization**:
For each position threshold (1, 3, 5, 10, 30), the system performs exhaustive subset selection using BIC optimization: `pos_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")`. This threshold-independent approach recognizes that different finishing position predictions may require different variable combinations for optimal binomial classification accuracy.

**GAM Model Construction**:
Selected variables are converted to smooth terms for binomial GAM modeling: `pos_smooth_terms <- paste("s(", pos_best_bic_vars[-1], ")", collapse=" + ")`, enabling non-linear relationships between alpine performance predictors and position probability outcomes. This approach accommodates alpine skiing's complex performance patterns across different discipline types.

##### Modeling

Alpine skiing's Individual Probability Training Modeling employs sophisticated binomial GAM architecture specifically optimized for position probability prediction across the sport's multi-discipline structure. The system creates independent models for each position threshold while maintaining discipline-specific performance characteristics and implementing robust fallback strategies for model stability.

**Binomial GAM Architecture**:
Alpine skiing uses GAM models with binomial family for position probability prediction: `position_model <- gam(pos_gam_formula, data = race_df, family = binomial, method = "REML")`. This approach provides natural probability bounds (0-1) and proper handling of binary outcomes, with REML estimation ensuring conservative smoothing parameters and better generalization to new data.

**Independent Threshold Modeling**:
Each position threshold (1, 3, 5, 10, 30) receives its own independently fitted model, allowing optimal variable combinations specific to each finishing position category. This threshold-independence recognizes that predicting wins may require different predictors than predicting top-30 finishes, especially across alpine's diverse discipline spectrum.

**Model Validation and Quality Assurance**:
Alpine skiing implements comprehensive model evaluation using Brier scores for probabilistic accuracy assessment: `brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)`. This metric penalizes both overconfident and underconfident probability predictions, ensuring well-calibrated probability estimates across technical and speed disciplines.

**Robust Fallback Strategy**:
Alpine skiing implements a multi-tier fallback system to handle model fitting failures, progressing from full BIC-optimized models to simplified GAM with essential variables, ensuring reliable probability predictions even when data sparsity occurs in specific discipline/athlete combinations.

##### Adjustments

Alpine skiing implements sophisticated Individual Probability Training Adjustments that are currently **disabled** in the production system, though the comprehensive framework exists for systematic bias correction in finishing position predictions. The methodology was designed to identify and correct systematic performance patterns through statistical analysis of probability residuals across periods and disciplines.

**Probability Residual Calculation**:
The adjustment system calculates probability differences between actual outcomes and model predictions for each athlete: `prob_diff = as.numeric(position_achieved) - initial_prob`. This residual represents the systematic bias in position probability predictions, capturing whether the model consistently over- or under-predicts an athlete's finishing position probabilities across different competitive contexts.

**Statistical Significance Testing**:
Alpine employs t-test validation to ensure only statistically meaningful adjustments are applied. For each athlete and position threshold, the system compares probability residuals from the current period against residuals from all other periods using: `t.test(prior_period_curr, prior_period_other)$p.value`. Adjustments are only applied when p < 0.05, ensuring corrections address genuine systematic bias rather than random variation.

**Period-Based Bias Correction**:
When statistical significance is established, the system calculates period-specific corrections: `period_correction = ifelse(period_p < 0.05, mean(prob_diff[Period == Period], na.rm = TRUE), 0)`. These corrections capture systematic performance changes across different periods of the alpine season, from early season technical preparation through peak World Cup competition periods.

**Discipline-Specific Framework**:
The adjustment methodology was designed to accommodate alpine skiing's unique discipline structure, with separate adjustment calculations for speed events (Downhill, Super-G) versus technical events (Slalom, Giant Slalom). This recognizes that systematic biases may manifest differently across alpine's diverse skill requirements.

**Probability Constraint Enforcement**:
All adjustments are bounded within valid probability ranges: `period_adjusted = pmin(pmax(initial_prob + period_correction, 0), 1)`. This ensures that corrected probabilities remain mathematically valid while preventing extreme adjustments that could destabilize the prediction framework.

**Current Status**:
The Individual Probability Training Adjustments are commented out in the production system (lines 1198-1249 in race-picks.R) with the annotation "removed to improve model stability and prediction accuracy." The system currently relies on base GAM predictions without individual-specific probability adjustments, prioritizing model stability over potential accuracy gains from systematic bias correction.

#### Testing

##### Startlist Setup

Alpine skiing's Individual Probability Testing employs sophisticated startlist preparation that builds upon the points prediction framework while incorporating position probability-specific enhancements. The system preserves race participation probabilities, manages discipline-specific ELO ratings, and integrates with the comprehensive normalization framework to ensure accurate position probability predictions across alpine's diverse discipline spectrum.

**Base Participant Data Framework**:
Position probability testing begins with core participant information extraction: `base_df <- startlist %>% select(Skier, ID, Nation, Sex, all_of(race_prob_cols))`. This foundation preserves essential athlete identification while maintaining race participation probability columns (`Race1_Prob`, `Race2_Prob`, etc.) that are critical for subsequent probability normalization and adjustment processes.

**Race Participation Probability Preservation**:
Alpine's startlist setup specifically preserves race probability columns through dynamic detection: `race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)`. These probability values, calculated using the exponential decay model for discipline-specific participation patterns, are maintained throughout the position probability prediction process for accurate normalization.

**Discipline-Specific ELO Rating Integration**:
The system incorporates alpine's comprehensive ELO rating structure: `elo_cols <- c("Downhill_Elo", "Super.G_Elo", "Giant.Slalom_Elo", "Slalom_Elo", "Combined_Elo", "Tech_Elo", "Speed_Elo", "Elo")`. These ratings are normalized to percentage values relative to the maximum rating in each race, providing model input that reflects relative performance capabilities across speed versus technical disciplines.

**Most Recent Performance Metrics**:
Alpine maintains consistency with points prediction methodology by retrieving the most recent ELO ratings chronologically: `arrange(Season, Race) %>% slice(n())`. This ensures that position probability models utilize the latest performance information while maintaining the same temporal data handling as points predictions for methodological consistency.

**Weighted Previous Points Integration**:
The system incorporates discipline-specific weighted previous points using a 5-race window with increasing weights (1, 2, 3, 4, 5): `Prev_Points_Weighted = sapply(1:n(), function(j) {...})`. This metric captures recent form while maintaining discipline specificity, acknowledging that a slalom specialist's recent technical performance differs from downhill recent speed performance.

**Comprehensive Missing Value Imputation**:
Alpine employs robust NA handling through first quartile replacement: `replace_na_with_quartile <- function(x) {ifelse(is.na(x), quantile(x, 0.25, na.rm = TRUE), x)}`. This conservative approach ensures that athletes with limited recent data receive reasonable baseline performance estimates rather than defaulting to zero values.

**Data Validation and Preparation**:
The startlist setup includes comprehensive validation ensuring all required ELO percentage columns exist: `for (col in pct_cols) {if (!col %in% names(processed_df)) {processed_df[[col]] <- 0}}`. This prevents model failures due to missing data while maintaining consistent variable structures across different race scenarios and discipline combinations.

**Position Threshold Framework Integration**:
Alpine's startlist preparation accommodates the standard position threshold structure (1st, 3rd, 5th, 10th, 30th) while ensuring that subsequent binary outcome variables (`position_achieved <- Place <= threshold`) can be properly created during model application. The framework maintains consistency between training and testing data structures for reliable probability predictions.

##### Modeling

Alpine skiing's Individual Probability Testing applies trained binomial GAM models to startlist data through sophisticated variable validation, robust prediction mechanisms, and comprehensive fallback strategies. The system accommodates alpine's discipline-specific performance characteristics while ensuring reliable probability predictions across speed and technical events through multi-tier error handling and model application frameworks.

**Variable Validation and Data Preparation**:
Alpine's testing modeling begins with comprehensive variable validation to ensure model compatibility: `model_vars <- names(pos_model$var.summary)`. The system explicitly checks for each required variable in the prepared startlist data and adds default values for missing variables: `for(var in model_vars) {if(!(var %in% names(prediction_subset))) {prediction_subset[[var]] <- 0}}`. This prevents model application failures due to missing discipline-specific ELO ratings or other required features.

**Robust GAM Model Application**:
The framework employs explicit GAM prediction with comprehensive error handling: `mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response")`. This approach uses direct mgcv package calls to avoid method dispatch issues while generating probabilities for each position threshold (1st, 3rd, 5th, 10th, 30th) across alpine's diverse discipline spectrum.

**Row-by-Row Fallback Mechanism**:
When batch prediction fails, alpine implements sophisticated row-by-row prediction recovery: `for(j in 1:nrow(prediction_subset)) {single_row <- prediction_subset[j,, drop = FALSE]; result[j] <- tryCatch({mgcv::predict.gam(pos_model, newdata = single_row, type = "response")}, error = function(e2) {threshold/100})}`. This ensures that individual prediction failures don't compromise the entire race prediction process.

**Multi-Tier Fallback Strategy**:
Alpine maintains the most comprehensive fallback system among winter sports, progressing from full BIC-optimized models to simplified GAM with essential variables: `fallback_vars <- c("Prev_Points_Weighted", elo_col)`. When primary models fail, the system creates simpler models using core predictors: `position_models[[paste0("threshold_", threshold)]] <- gam(fallback_formula, data = race_df, family = binomial, method = "REML")`.

**Discipline-Specific Model Integration**:
The testing framework accommodates alpine's discipline-specific ELO ratings (Downhill, Super-G, Giant Slalom, Slalom) while ensuring that model predictions reflect appropriate discipline specializations. Speed event models utilize different variable combinations compared to technical event models, acknowledging that downhill specialists and slalom experts require distinct predictive approaches.

**Threshold-Specific Probability Generation**:
Alpine generates independent probability predictions for each position threshold through separate model applications: `prob_col <- paste0("prob_top", threshold)`. This threshold-independent approach ensures that factors influencing podium predictions (top-3) may differ from those affecting points-scoring predictions (top-30) across alpine's discipline-diverse competitive spectrum.

**Default Value Assignment with Conservative Estimation**:
When all prediction mechanisms fail, alpine assigns conservative default values based on threshold-specific probability estimates: `threshold/100`. This ensures that the system always provides reasonable probability estimates even in extreme edge cases, maintaining prediction availability across all competitive scenarios while prioritizing model stability over potentially unreliable predictions.

##### Adjustments

Alpine skiing's Individual Probability Testing Adjustments are **disabled** in the production system to maintain model stability and prevent overfitting in testing scenarios. Unlike the Individual Points Testing Adjustments which account for systematic biases in points prediction, position probability adjustments have been disabled to prioritize prediction reliability and mathematical consistency.

The system recognizes that probability predictions already incorporate sufficient complexity through the binomial GAM modeling framework with discipline-specific feature selection, comprehensive normalization procedures, and monotonic constraint enforcement. Rather than risk introducing additional systematic bias through sequential adjustment layers, Alpine probability testing maintains base GAM predictions while ensuring mathematically valid probability distributions.

**Framework Design (Currently Disabled)**:
The disabled adjustment system was designed to mirror points prediction adjustments, using probability residuals to identify systematic patterns: `prob_diff = as.numeric(position_achieved) - initial_prob`. Period-specific and discipline-specific adjustments would be calculated using t-test validation (p < 0.05) and applied with probability bounds enforcement (`pmin(pmax(adjusted_prob, 0), 1)`).

**Current Implementation**:
Position probability testing uses base GAM predictions directly: `position_preds[[prob_col]] <- position_preds[[paste0(prob_col, "_base")]]`. This approach ensures stable, well-calibrated probability predictions while relying on the comprehensive normalization and monotonic constraint framework to maintain mathematical validity across alpine skiing's diverse discipline spectrum.

#### Normalization and Monotonic Constraints

**Evidence from `race-picks.R` lines 96-251:**
The `normalize_position_probabilities()` function implements:

**Race Participation Adjustment (lines 116-128):**
Base probabilities are scaled by race participation: `normalized[[prob_col]] <- normalized[[prob_col]] * normalized[[race_prob_col]]`

**Normalization to Target Sums (lines 130-182):**
- Calculates target sums: `target_sum <- 100 * threshold` (100% for Top-1, 300% for Top-3, etc.)
- Applies scaling factor: `scaling_factor <- target_sum / current_sum`
- Caps individual probabilities at 100% and redistributes excess proportionally (lines 142-172)

**Monotonic Constraints (lines 194-236):**
For each athlete, ensures `P(Top-1) ≤ P(Top-3) ≤ P(Top-5) ≤ P(Top-10) ≤ P(Top-30)` by adjusting probabilities row-wise: `if(probs[j] < probs[j-1]) { probs[j] <- probs[j-1] }` (lines 207-210).

**Re-normalization (lines 218-236):**
After monotonic adjustment, probabilities are re-normalized to maintain target sums while preserving constraint ordering.