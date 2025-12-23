---
title: "Alpine Skiing Race Picks Methodology"
date: 2023-12-01T01:23:07+00:00
draft: false
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

**Evidence from `race-picks.R` lines 1174-1178:**
Position probability training uses same training data as points models but creates binary outcome variables: `race_df$position_achieved <- race_df$Place <= threshold` for each threshold in `position_thresholds <- c(1, 3, 5, 10, 30)`.

##### Feature Selection

**Evidence from `race-picks.R` lines 1170-1188:**
Position models use identical feature selection as points models: `position_feature_vars <- explanatory_vars`. The same `regsubsets()` with BIC optimization is applied: `pos_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")`.

##### Modeling

**Evidence from `race-picks.R` lines 1190-1198:**
Position models use GAM with binomial family: `gam(pos_gam_formula, data = race_df, family = binomial, method = "REML")`. Brier scores are calculated for model evaluation: `brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)`.

##### Adjustments

**Evidence from `race-picks.R` lines 1210-1254:**
Period adjustments for position models follow same methodology as points models. T-tests compare probability differences across periods, with corrections applied when p < 0.05. Adjustments stored per participant per threshold.

#### Testing

##### Startlist Setup

Testing uses same startlist setup as points prediction with addition of position-specific probability columns for each threshold.

##### Modeling

Position probability prediction applies trained binomial GAM models to startlist data, generating probabilities for each threshold.

##### Adjustments

Period and discipline adjustments are applied to raw position probabilities before normalization.

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