---
title: "Biathlon Race Picks Methodology"
date: 2023-12-01T01:23:07+00:00
draft: false
tags: ["methodology", "skiing", "race-picks", "biathlon"]
---

## Biathlon

### Individual

#### Data Gathering

Biathlon individual race startlists are scraped from the Biathlon World website using specialized JSON-based data extraction that handles both individual and relay events. The system processes biathlon's unique data structure through IBU-specific website integration and comprehensive event type detection.

**Race Discovery and Scheduling**: The system identifies races to process using UTC timezone-based scheduling with biathlon-specific paths:

```python
# From startlist-scrape-races.py:32-36
# Get today's date in UTC 
today_utc = datetime.now(timezone.utc)
today_str = today_utc.strftime('%m/%d/%Y')

print(f"Today's date (UTC): {today_str}")
```

**Biathlon Event Type Detection**: The system distinguishes between individual and relay events using race type classification:

```python
# From startlist-scrape-races.py:98-100
# Check for team/relay events
relay_races = races_df[races_df['RaceType'].isin(['Relay', 'Mixed Relay', 'Single Mixed Relay'])]
print(f"Found {len(relay_races)} relay/team events")
```

**Biathlon World Website Integration**: Unlike other sports, biathlon uses specialized JSON extraction from the Biathlon World website:

```python
# From startlist_common.py:89-128
def get_biathlon_startlist(url: str) -> List[Dict]:
    """Gets athlete data from Biathlon World website"""
    try:
        # Make request to the URL
        headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
        }
        response = requests.get(url, headers=headers)
        response.raise_for_status()
        
        # Parse the HTML content
        soup = BeautifulSoup(response.text, 'html.parser')
        
        # Extract the JSON data that contains athlete information
        # This is typically found in a script tag containing the initial state
        athletes = []
        
        # Look for the script tag with JSON data
        scripts = soup.find_all('script')
        json_data = None
        
        for script in scripts:
            if script.string and '"Results":[' in script.string:
                # Extract JSON string from the script
                match = re.search(r'"Results":\[(.*?)\]', script.string, re.DOTALL)
                if match:
                    json_str = '{"Results":[' + match.group(1) + ']}'
                    try:
                        json_data = json.loads(json_str)
                        break
                    except json.JSONDecodeError:
                        # Try to fix truncated JSON by finding the last complete object
                        json_str = '{"Results":[' + match.group(1).rsplit('},', 1)[0] + '}]}'
                        json_data = json.loads(json_str)
                        break
```

**Individual Athlete Data Processing**: Biathlon athletes are extracted with IBU-specific identification and nation data:

```python
# From startlist_common.py:129-143
# Process athlete data
for athlete in json_data.get('Results', []):
    if not athlete.get('IsTeam', False):  # Skip team entries in the results
        athlete_data = {
            'Name': athlete.get('Name', ''),
            'FamilyName': athlete.get('FamilyName', ''),
            'GivenName': athlete.get('GivenName', ''),
            'Nation': athlete.get('Nat', ''),
            'IBUId': athlete.get('IBUId', ''),
            'Bib': athlete.get('Bib', ''),
            'StartOrder': athlete.get('StartOrder', '')
        }
        athletes.append(athlete_data)
        print(f"Added athlete: {athlete_data['Name']} ({athlete_data['Nation']})")
```

**Relay Team Data Processing**: Biathlon has specialized relay team extraction with member organization:

```python
# From startlist_common.py:151-199
def get_biathlon_relay_teams(url: str) -> List[Dict]:
    """Gets relay team data from Biathlon World website"""
    try:
        # Extract the JSON data that contains team and athlete information
        teams = {}
        
        # Look for the script tag with JSON data
        scripts = soup.find_all('script')
        json_data = None
        
        # First pass to collect team entries
        team_entries = {}
        for entry in json_data.get('Results', []):
            if entry.get('IsTeam', False):  # This is a team entry
                team_nat = entry.get('Nat', '')
                team_entries[team_nat] = {
                    'team_name': entry.get('Name', ''),
                    'nation': team_nat,
                    'team_rank': entry.get('Rank', ''),
```

**JSON Error Handling and Recovery**: The system handles truncated JSON data from the Biathlon World website:

```python
# From startlist_common.py:118-123
try:
    json_data = json.loads(json_str)
    break
except json.JSONDecodeError:
    # Try to fix truncated JSON by finding the last complete object
    json_str = '{"Results":[' + match.group(1).rsplit('},', 1)[0] + '}]}'
    json_data = json.loads(json_str)
    break
```

**Weekly Picks Integration**: The system integrates with biathlon-specific weekly predictions:

```python
# From startlist_common.py:13-32
def check_and_run_weekly_picks():
    """
    Check if there are races with today's date in weekends.csv and run the weekly-picks2.R script if true.
    """
    # Get today's date in UTC
    today_utc = datetime.now(timezone.utc).strftime('%m/%d/%Y')
    print(f"Checking for races on today's date (UTC): {today_utc}")
    
    # Path to weekends.csv
    weekends_csv_path = os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/weekends.csv')
    
    # Path to R script
    r_script_path = os.path.expanduser('~/blog/daehl-e/content/post/biathlon/drafts/weekly-picks2.R')
```

**Mixed Relay and Team Event Handling**: The system specifically identifies and processes various biathlon relay formats:

```python
# From startlist-scrape-races.py:98-100
# Check for team/relay events
relay_races = races_df[races_df['RaceType'].isin(['Relay', 'Mixed Relay', 'Single Mixed Relay'])]
print(f"Found {len(relay_races)} relay/team events")
```

The Biathlon individual data gathering system provides specialized IBU website integration through JSON-based data extraction, comprehensive relay and individual event handling, IBU ID tracking, robust JSON error recovery mechanisms, biathlon-specific team organization, and seamless integration with biathlon prediction workflows that accommodate the sport's unique data structure and event formats.

#### Points

##### Training

###### Setup

The training data setup for Biathlon follows a comprehensive preprocessing pipeline that prepares historical race data for points prediction models, with specific accommodations for biathlon's unique race types and elevation effects.

**Historical Data Loading and Points Assignment**:
The system loads historical race data and assigns World Cup points based on race type. Biathlon uses different point systems for regular races versus mass start events:

```r
# From race-picks.R:1456-1470
df_with_points <- df %>%
  # Add points based on the race type if they don't already exist
  mutate(
    Points = if("Points" %in% names(df)) {
      Points
    } else {
      mapply(function(place, RaceType) {
        get_points(place, RaceType)
      }, Place, RaceType)
    }
  ) %>%
  # Sort
  arrange(Season, Race, Place)
```

**Race Type-Specific Weighted Points Calculation**:
Unlike other sports that group by discipline, biathlon calculates weighted previous points by race type (Sprint, Pursuit, Individual, Mass Start), recognizing the interconnected nature of biathlon events:

```r
# From race-picks.R:1471-1483
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

**Biathlon-Specific ELO Column Management**:
The system handles biathlon's race type-specific ELO ratings, differentiating between individual and relay processing:

```r
# From race-picks.R:1485-1498
# Check if Pelo columns exist, if not create them
if(is_relay) {
  pelo_cols <- c("Avg_Sprint_Pelo", "Avg_Individual_Pelo", "Avg_MassStart_Pelo", "Avg_Pursuit_Pelo", "Avg_Pelo")
} else {
  pelo_cols <- c("Sprint_Pelo", "Individual_Pelo", "MassStart_Pelo", "Pursuit_Pelo", "Pelo")
}

# Make sure these columns exist (create if missing)
for (col in pelo_cols) {
  if (!col %in% names(df_with_points)) {
    log_info(paste("Creating missing column:", col))
    df_with_points[[col]] <- 0
  }
}
```

**Elevation and Temporal Feature Engineering**:
Biathlon's training setup includes elevation effects (venues above/below 1300m) and seasonal periodization:

```r
# From race-picks.R:1501-1521
processed_df <- df_with_points %>%
  # Add period (biathlon has 4 periods per season)
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
  # Add elevation flag (0 if below 1300m, 1 if above)
  mutate(
    Elevation_Flag = ifelse(Elevation >= 1300, 1, 0)
  ) %>%
  # Filter relevant races and add cumulative points
  dplyr::filter(
    Season >= max(Season-10)
  )
```

**ELO Percentage Calculation with Biathlon-Specific Structure**:
The system calculates ELO percentages using biathlon's race type-specific ELO ratings within each race context:

```r
# From race-picks.R:1525-1544
# Handle NAs and calculate percentages
group_by(Season, Race) %>%
mutate(
  across(
    all_of(pelo_cols),
    ~replace_na_with_quartile(.x)
  )
) %>%
# Calculate percentages for each Pelo column
mutate(
  across(
    all_of(pelo_cols),
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

**Missing Value Handling for Biathlon**:
The system ensures all required ELO percentage columns exist and replaces missing values with first quartile values, accounting for the fact that inexperienced biathletes typically perform worse.

This biathlon-specific setup creates training datasets that account for the sport's unique race type dependencies (pursuit races following sprints), elevation effects on performance, and the interconnected nature of biathlon competition formats, providing robust foundation data for GAM model training.

###### Feature Selection

Biathlon's feature selection process reflects the sport's interconnected race format structure and the need to differentiate between individual and relay event prediction requirements. The system uses a dual-path approach that adapts to competition format while maintaining consistency in optimization methodology.

**Race Format-Specific Variable Sets**:
The system defines different candidate variables based on whether predictions target individual or relay events, recognizing the different performance metrics available:

```r
# From race-picks.R:1862-1872
# Define explanatory variables based on race type
if(is_relay) {
  explanatory_vars <- c("Avg_Sprint_Pelo_Pct", "Avg_Individual_Pelo_Pct", 
                        "Avg_MassStart_Pelo_Pct", "Avg_Pursuit_Pelo_Pct", 
                        "Avg_Pelo_Pct")
} else {
  explanatory_vars <- c("Prev_Points_Weighted", 
                        "Sprint_Pelo_Pct", "Individual_Pelo_Pct", 
                        "MassStart_Pelo_Pct", "Pursuit_Pelo_Pct", 
                        "Pelo_Pct")
}
```

**Exhaustive BIC Optimization**:
Like other sports, biathlon uses exhaustive subset selection with BIC optimization to identify the most predictive variable combination:

```r
# From race-picks.R:1875-1882
formula <- as.formula(paste(response_variable, "~", paste(explanatory_vars, collapse = " + ")))
tryCatch({
  exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
  summary_exhaustive <- summary(exhaustive_selection)
  best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
  smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
  gam_formula <- as.formula(paste("Points ~", smooth_terms))
  
  model <- gam(gam_formula, data = race_df_75)
```

**Race Type-Specific ELO Integration**:
Individual events use race type-specific ELO ratings (Sprint, Individual, Pursuit, Mass Start) plus weighted previous points, recognizing that biathlon events have different tactical and physical demands. Relay events use team-averaged ELO ratings without weighted points since team performance dynamics differ from individual competition.

**Simplified Fallback Strategy**:
Biathlon uses a streamlined fallback approach compared to other sports:

```r
# From race-picks.R:1883-1888
}, error = function(e) {
  log_warn(paste("Error in model selection:", e$message))
  # Fallback to a simpler model
  fallback_formula <- as.formula(paste("Points ~ s(", pelo_col, ")"))
  model <<- gam(fallback_formula, data = race_df_75)
})
```

This approach ensures robust model fitting while accommodating biathlon's unique race interconnectedness and elevation effects on both skiing and shooting performance.

###### Modeling

Biathlon's modeling approach employs Generalized Additive Models (GAM) optimized for the sport's unique characteristics including interconnected race formats, environmental factors, and distinct individual versus relay competition requirements. The system creates both points prediction models and position probability models with sport-specific adaptations.

**Primary GAM Implementation**:
The main modeling approach uses smoothed terms for capturing nonlinear relationships between biathlon performance predictors and race outcomes:

```r
# From race-picks.R:1879-1882
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))

model <- gam(gam_formula, data = race_df_75)
```

**Streamlined Fallback Strategy**:
Biathlon implements a focused fallback approach compared to other sports, reflecting the sport's smaller competitive field and specialized nature:

```r
# From race-picks.R:1883-1888
}, error = function(e) {
  log_warn(paste("Error in model selection:", e$message))
  # Fallback to a simpler model
  fallback_formula <- as.formula(paste("Points ~ s(", pelo_col, ")"))
  model <<- gam(fallback_formula, data = race_df_75)
})
```

**Position Probability Modeling with REML**:
Biathlon position models use binomial GAM with REML estimation for optimal smoothing parameter selection, accounting for the sport's specific finishing position dynamics:

```r
# From race-picks.R:2040-2043
position_model <- gam(pos_gam_formula,
                      data = race_df,
                      family = binomial,
                      method = "REML")
```

**Brier Score Validation**:
Position probability models are evaluated using Brier scores to assess prediction accuracy for each finishing position threshold:

```r
# From race-picks.R:2045-2048
# Calculate Brier score for model evaluation
predicted_probs <- predict(position_model, newdata = race_df, type = "response")
brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
log_info(paste("Brier score for threshold", threshold, ":", round(brier_score, 4)))
```

**Conditional Adjustment Calculations**:
Biathlon models incorporate sophisticated conditional adjustment systems that differentiate between individual and relay events:

```r
# From race-picks.R:1921-1929
period_p = if(is_relay) 1 else purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_period_curr <- Prediction_Diff[Period == Period[r] & row_id < r]
  prior_period_other <- Prediction_Diff[Period != Period[r] & row_id < r]
  if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
  tryCatch({
    t.test(prior_period_curr, prior_period_other)$p.value
  }, error = function(e) 1)
})
```

**Race Format-Specific Model Architecture**:
The modeling system adapts to biathlon's interconnected race structure, where sprint results affect pursuit starting positions and different race types require different tactical approaches. Individual models use race type-specific ELO ratings and weighted previous points, while relay models rely on team-aggregated performance metrics.

**Robust Prediction Methods**:
Biathlon implements sophisticated prediction error handling with explicit mgcv package calls and row-by-row fallback strategies to ensure reliable probability predictions across varying race conditions and field sizes.

This biathlon-specific modeling framework ensures accurate performance prediction while accounting for the sport's unique dual-discipline nature (skiing + shooting), environmental effects, and the complex tactical relationships between different race formats.

###### Adjustments

Biathlon implements a sophisticated conditional adjustment system that differentiates between individual and relay events, recognizing that relay team compositions change between races while individual athletes maintain consistent performance patterns. The system accounts for both seasonal periodization and elevation-specific performance effects.

**Conditional Adjustment Architecture for Individual vs Relay Events**:
The system applies different adjustment strategies based on competition format, recognizing the unique characteristics of each event type:

```r
# From race-picks.R:1921-1932
# Period adjustments disabled for relay teams - team compositions change between races
period_p = if(is_relay) 1 else purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_period_curr <- Prediction_Diff[Period == Period[r] & row_id < r]
  prior_period_other <- Prediction_Diff[Period != Period[r] & row_id < r]
  if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
  tryCatch({
    t.test(prior_period_curr, prior_period_other)$p.value
  }, error = function(e) 1)
}),
period_correction = if(is_relay) 0 else ifelse(period_p < 0.05,
                           mean(Prediction_Diff[Period == Period], na.rm = TRUE),
                           0)
```

**Elevation-Specific Performance Adjustments**:
Biathlon's unique elevation adjustment system recognizes that altitude affects both skiing endurance and shooting accuracy differently for individual athletes:

```r
# From race-picks.R:1948-1959
elevation_p = if(is_relay) 1 else purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_elev_curr <- Prediction_Diff[Elevation_Flag == Elevation_Flag[r] & row_id < r]
  prior_elev_other <- Prediction_Diff[Elevation_Flag != Elevation_Flag[r] & row_id < r]
  if(length(prior_elev_curr) < 3 || length(prior_elev_other) < 3) return(1)
  tryCatch({
    t.test(prior_elev_curr, prior_elev_other)$p.value
  }, error = function(e) 1)
}),
elevation_correction = if(is_relay) 0 else ifelse(elevation_p < 0.05,
                              mean(Prediction_Diff[Elevation_Flag == Elevation_Flag], na.rm = TRUE),
                              0)
```

**Combined Adjustment Application for Points Predictions**:
Individual events receive combined period and elevation adjustments, while relay events use base model predictions without adjustments:

```r
# From race-picks.R:1961-1963
# Combine adjustments
Adjusted_Prediction = Initial_Prediction + period_correction + elevation_correction
```

**Position Probability Adjustments with Bounds Enforcement**:
Biathlon implements probability-specific adjustments with strict boundary constraints to ensure valid probability ranges:

```r
# From race-picks.R:2039-2042
period_correction = if(is_relay) 0 else ifelse(period_p < 0.05,
                           mean(prob_diff[Period == Period], na.rm = TRUE),
                           0),
period_adjusted = pmin(pmax(initial_prob + period_correction, 0), 1)
```

**Comprehensive Adjustment Storage and Tracking**:
The system maintains detailed adjustment records for both points and position probabilities, enabling analysis of adjustment effectiveness:

```r
# From race-picks.R:2110-2122
# Get final adjustments for each participant
skier_adjustments <- race_df_75 %>%
  group_by(!!sym(participant_col)) %>%
  summarise(
    period_effect = last(period_correction),
    elevation_effect = last(elevation_correction),
    
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

**Relay Event Considerations**:
The system deliberately excludes adjustments for relay events due to the fundamental difference in team composition variability. Since relay teams can change members between races, historical individual performance patterns don't reliably predict team performance, making adjustments inappropriate for this competition format.

This biathlon-specific adjustment framework ensures accurate performance prediction while appropriately handling the sport's unique dual-discipline challenges, environmental factors, and the distinct requirements of individual versus relay competition formats.

#### Testing

##### Startlist Setup

Biathlon's startlist setup for testing implements a sophisticated data preparation pipeline that accommodates the sport's unique dual-discipline requirements and handles both individual and relay competition formats. The system ensures robust data quality while preparing prediction-ready datasets that capture biathlon's complex performance patterns.

**Race Participation Probability Calculation with Race Type Specificity**:
Biathlon startlist setup begins by calculating race-specific participation probabilities for each athlete-race type combination. The system recognizes that participation patterns differ significantly between Sprint, Individual, Pursuit, Mass Start, and Relay events:

```r
# From race-picks.R:214-256 in get_race_probability()
get_race_probability <- function(chronos, participant, racetype, is_relay = FALSE) {
  id_col <- if(is_relay) "Nation" else "Skier"
  
  participant_races <- chronos %>%
    filter(!!sym(id_col) == participant & RaceType == racetype) %>%
    arrange(desc(Date))
  
  if(nrow(participant_races) > 0) {
    total_races <- nrow(participant_races)
    recent_races <- min(10, total_races) # Consider last 10 races
    
    # Use exponential decay: more recent races count more
    decay_rate <- 0.3
    weights <- exp(-decay_rate * (0:(recent_races-1)))
    prob <- sum(weights) / (sum(weights) + 1) # Normalize to [0,1]
    
    return(prob)
  }
  return(0)
}
```

**Most Recent PELO Rating Retrieval**:
The system retrieves the most current PELO (biathlon-specific ELO) ratings for each athlete by taking the chronologically latest entry, ensuring predictions use the most up-to-date performance assessments across all race types:

```r
# From race-picks.R:921-948 in prepare_startlist_data()
most_recent_elos <- race_df %>%
  filter(Skier %in% base_df$Skier) %>%
  group_by(Skier) %>%
  arrange(Date, Season, Race) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(Skier, any_of(elo_cols))
```

**PELO Score Normalization to Percentages**:
All PELO ratings are converted to percentage form by normalizing against the maximum observed value, creating standardized 0-1 scale features for consistent model input across different race types:

```r
# From race-picks.R:951-984
for(i in seq_along(pelo_cols)) {
  pelo_pct_col <- paste0(pelo_cols[i], "_Pct")
  elo_col <- elo_cols[i]
  if(elo_col %in% names(result_df)) {
    if(elo_col %in% names(race_df)) {
      max_val <- max(race_df[[elo_col]], na.rm = TRUE)
      if(!is.na(max_val) && max_val > 0) {
        result_df[[pelo_pct_col]] <- result_df[[elo_col]] / max_val
      } else {
        result_df[[pelo_pct_col]] <- 0.5
      }
    }
  } else if(!pelo_pct_col %in% names(result_df)) {
    result_df[[pelo_pct_col]] <- 0.5  # Default to middle value
  }
}
```

**Race Type-Specific Weighted Previous Points Calculation**:
Recent performance is captured through weighted previous points calculated separately for each race type, recognizing that biathlon performance patterns vary significantly across different event formats:

```r
# From race-picks.R:1471-1483
df_with_points <- df_with_points %>%
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

**Comprehensive Missing Value Imputation Strategy**:
Biathlon uses an advanced NA handling approach that preserves data quality while accommodating the sport's complex performance requirements:

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

**Individual vs Relay Format Handling**:
The startlist setup adapts to both individual and relay formats, with appropriate data structure modifications for team-based competitions. Relay events use nation-based identification and team-specific performance metrics while maintaining the same robust data quality standards.

**Model Preparation and Validation**:
The final startlist preparation includes comprehensive validation of race participation probabilities, ensuring all required model variables exist with appropriate defaults, and creating complete prediction-ready datasets that capture biathlon's unique dual-discipline performance patterns across all competition formats.

##### Modeling

Biathlon's testing modeling employs sophisticated GAM application with dual-discipline awareness and comprehensive error handling. The system applies race-type-specific trained models while maintaining robust fallback mechanisms and integrating historically-derived adjustments that capture biathlon's unique performance patterns.

**Race Type-Specific GAM Model Application**:
The system applies trained GAM models that were optimized for each race type using BIC feature selection, recognizing that biathlon performance varies significantly across Sprint, Individual, Pursuit, Mass Start, and Relay formats:

```r
# From race-picks.R:1875-1888
exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))

model <- gam(gam_formula, data = race_df_75)

# Model application to startlist
Base_Prediction = predict(model, newdata = startlist_prepared)
```

**Three-Tier Fallback System for Robust Prediction**:
Biathlon implements comprehensive fallback mechanisms to handle both points and position probability prediction failures:

```r
# From race-picks.R:1883-1888 (Points models)
}, error = function(e) {
  # Primary fallback: Simplified GAM
  fallback_formula <- as.formula(paste("Points ~ s(", pelo_col, ")"))
  model <<- gam(fallback_formula, data = race_df_75)
})

# From race-picks.R:2056-2101 (Position models)  
}, error = function(e) {
  # Secondary fallback: Basic predictors only
  fallback_vars <- c("Prev_Points_Weighted", pelo_col)
  fallback_terms <- paste("s(", fallback_vars, ")", collapse=" + ")
  fallback_formula <- as.formula(paste("position_achieved ~", fallback_terms))
  
  # Last resort: Single variable model
  fallback_formula <- as.formula(paste("position_achieved ~ s(", pelo_col, ")"))
  position_models[[paste0("threshold_", threshold)]] <- gam(
    fallback_formula, data = race_df, family = binomial, method = "REML"
  )
})
```

**Advanced Prediction Error Handling with Row-by-Row Fallback**:
The system includes sophisticated error recovery that attempts multiple prediction strategies:

```r
# From race-picks.R:2252-2275
base_predictions <- tryCatch({
  mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response")
}, error = function(e) {
  # Row-by-row prediction fallback
  result <- numeric(nrow(prediction_subset))
  for(j in 1:nrow(prediction_subset)) {
    single_row <- prediction_subset[j,, drop = FALSE]
    result[j] <- tryCatch({
      mgcv::predict.gam(pos_model, newdata = single_row, type = "response")
    }, error = function(e2) {
      threshold/100  # Default threshold-based value
    })
  }
  return(result)
})
```

**Dual-Discipline Adjustment Integration**:
Testing predictions incorporate historically-derived adjustments that capture biathlon's unique dual-discipline performance patterns:

```r
# From race-picks.R:2384-2407
race_dfs[[i]] <- startlist_prepared %>%
  mutate(
    Base_Prediction = predict(model, newdata = .),
    # Apply period and elevation adjustments
    Predicted_Points = Base_Prediction + period_adjustment + elevation_adjustment,
    Predicted_Points = pmax(pmin(Predicted_Points, 100), 0),
    
    # Apply race probability weighting
    Final_Prediction = Predicted_Points * Race_Prob,
    
    # Generate volatility-based confidence scenarios
    confidence_factor = pmin(n_recent_races / 10, 1),
    Safe_Prediction = pmax(
      (Predicted_Points - (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
      0
    ),
    Upside_Prediction = pmin(
      (Predicted_Points + (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
      100 * Race_Prob
    )
  )
```

**Comprehensive Model Validation and Confidence Assessment**:
The system incorporates multiple validation metrics and confidence measures specific to biathlon's dual-discipline requirements:

```r
# From race-picks.R:1999-2002
predicted_probs <- predict(position_model, newdata = race_df, type = "response")
brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)

# From race-picks.R:2104-2141
recent_prediction_volatility = slider::slide_dbl(
  Points - Initial_Prediction, sd, .before = 9, .complete = FALSE
),
recent_upside_potential = slider::slide_dbl(
  Points - Initial_Prediction, ~quantile(.x, 0.9, na.rm = TRUE),
  .before = 9, .complete = FALSE
)
```

**Individual vs Relay Format Adaptation**:
The testing modeling dynamically adapts between individual and relay predictions, with appropriate handling for team-based competitions while maintaining the same statistical rigor and dual-discipline awareness that characterizes biathlon performance prediction.