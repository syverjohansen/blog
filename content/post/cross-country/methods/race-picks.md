---
title: "Cross-Country Race Picks Methodology"
date: 2023-12-01T01:23:07+00:00
draft: false
tags: ["methodology", "skiing", "race-picks", "cross-country"]
---

## Cross-Country

### Individual

#### Data Gathering

Individual race startlists are scraped from the FIS websites at midnight UTC on the day of a race using automated Python scripts. If startlists are not present on the FIS site, a mock startlist is created using all skiers who have competed in the current season.

**Startlist Scraping Process**: The system uses automated web scraping to gather official race startlists:

```python
# From startlist-scrape-races.py:507-508
# Get current date in UTC timezone
today_utc = datetime.now(timezone.utc).strftime('%Y-%m-%d')
```

The scraping process identifies individual races (filtering out relays and team sprints) and processes each gender separately:

```python
# From startlist-scrape-races.py:232-283
def process_gender_specific_races(races_df: pd.DataFrame, target_gender: str) -> bool:
    """
    Process races for a specific gender, finding the next available race for that gender
    Returns True if standard races were processed, False otherwise
    """
    print(f"\n===== Processing {target_gender.upper()} races =====")
    
    # Standardize gender values
    gender_map = {
        'men': ['men', 'M', 'Men'],
        'ladies': ['ladies', 'L', 'Ladies', 'Women', 'F']
    }
    
    # Find races for the specified gender
    gender_races = races_df[races_df['Sex'].isin(gender_map[target_gender])]
    
    # Filter out relays and team sprints
    valid_races = next_races[(next_races['Distance'] != 'Rel') & 
                           (next_races['Distance'] != 'Ts') & 
                           (next_races['Distance'] != 'Mix')]
```

**FIS Website Scraping**: For each race, the system fetches athlete data from FIS startlist URLs:

```python
# From startlist_common.py:77-181
def get_fis_startlist(url: str) -> List[Tuple[str, str]]:
    """Gets skier names and nations from FIS website"""
    try:
        headers = {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
        }
        response = requests.get(url, headers=headers)
        
        soup = BeautifulSoup(response.text, 'html.parser')
        athletes = []
        
        # Look for the table rows
        athlete_rows = soup.find_all('a', class_='table-row')
        
        for row in athlete_rows:
            # Extract athlete name and country
            athlete_name_div = row.select_one('.athlete-name')
            country_div = row.select_one('div.country.country_flag')
```

**Mock Startlist Creation**: When FIS startlists are unavailable, the system creates mock startlists using historical participation data:

```python
# From startlist_common.py:443-491
def get_additional_national_skiers(chronos: pd.DataFrame, elo_scores: pd.DataFrame, 
                                fantasy_prices: Dict[str, int], nation: str, current_season: int = None) -> List[dict]:
    """Get all skiers from a nation who competed in the current season"""
    if current_season is None:
        current_season = get_current_season_from_chronos(chronos)
    
    # Get all skiers from this nation in current season
    nation_skiers = chronos[
        (chronos['Nation'] == nation) & 
        (chronos['Season'] == current_season)
    ]['Skier'].unique()
```

The mock startlist uses nation quotas defined in the configuration:

```python
# From config.py:2-32
NATION_QUOTAS = {
    'Andorra': {'men': 3, 'ladies': 3},
    'Austria': {'men': 5, 'ladies': 5},
    'Canada': {'men': 5, 'ladies': 5},
    'Finland': {'men': 6, 'ladies': 6},
    'France': {'men': 6, 'ladies': 6},
    'Germany': {'men': 6, 'ladies': 6},
    'Norway': {'men': 6, 'ladies': 6},
    'Sweden': {'men': 6, 'ladies': 6},
    'Switzerland': {'men': 6, 'ladies': 6},
    'USA': {'men': 6, 'ladies': 6}
}

# Default quota for unlisted nations
DEFAULT_QUOTA = {'men': 2, 'ladies': 2}
```

**ELO Score Enhancement**: Startlists are enhanced using latest Elo scores for each skier. The Elo scores used are for Overall, Distance, Distance Classic, Distance Freestyle, Sprint, Sprint Classic, Sprint Freestyle, Freestyle, and Classic:

```python
# From startlist_common.py:255-276
# Get most recent scores for each athlete
if 'Skier' in df.columns:
    try:
        latest_scores = df.groupby('Skier').last().reset_index()
    except Exception as group_e:
        print(f"Error grouping by Skier: {group_e}")
        latest_scores = df  # Use full dataset if grouping fails

# Define ELO columns
elo_columns = [col for col in ['Elo', 'Distance_Elo', 'Distance_C_Elo', 'Distance_F_Elo', 
                             'Sprint_Elo', 'Sprint_C_Elo', 'Sprint_F_Elo', 'Classic_Elo', 
                             'Freestyle_Elo'] if col in latest_scores.columns]
```

#### Points

##### Training

###### Setup

The model uses World Cup points as the response variable and some combination of the ELO variables and weighted previous World Cup points as explanatory variables with later adjustments based on altitude, period of season, and mass start format.

**Points System Selection**: The system dynamically selects the appropriate points system based on today's race type:

```r
# From race-picks.R:14-16, 132-138
# Define points systems
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
stage_points <- c(50,47,44,41,38,35,32,30,28,26,24,22,20,18,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)

# Check if today's race is a stage race
is_stage_today <- !is.na(today_weekend$Stage) && today_weekend$Stage == 1

# Select the appropriate points system for ALL races
global_points_system <- if(is_stage_today) {
  log_info("Using STAGE points system for today's races")
  stage_points
} else {
  log_info("Using WORLD CUP points system for today's races") 
  wc_points
}
```

**Points Assignment**: Historical race results get points assigned based on finishing position using the selected points system:

```r
# From race-picks.R:144-149
mutate(
  Points = mapply(function(place) {
    if (place >= 1 && place <= length(global_points_system)) {
      return(global_points_system[place])
    }
    return(0)
  }, Place)
)
```

**Weighted Previous Points Calculation**: For each athlete, weighted previous points are calculated separately for each race type and technique combination:

```r
# From race-picks.R:154-168
# Calculate weighted previous points separately for each race type/technique combination
df_with_points <- df_with_points %>%
  # First, create a race type column that distinguishes between sprint and distance
  mutate(RaceType = ifelse(Distance == "Sprint", "Sprint", "Distance")) %>%
  # Group by ID and the broader race type category
  group_by(ID, RaceType, Technique) %>%
  arrange(Season, Race) %>%
  mutate(Prev_Points_Weighted = sapply(1:n(), function(j) {
    if (j == 1) return(0)
    start_index <- max(1, j - 5)
    num_races <- j - start_index
    weights <- seq(1, num_races)
    weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
  }))
```

**Data Filtering**: The system filters to relevant recent data and high-level competitors:

```r
# From race-picks.R:200-205, 800-804
# Filter relevant races and time period
filter(
  Season >= max(Season-10),
  Event %in% c("World Cup", "Nordic Opening", "Tour de Ski", 
               "Olympic Winter Games", "World Championship", 
               "World Cup Final", "Ski Tour Canada")
)

# Filter to top competitors (75% of max ELO)
race_df_75 <- race_df %>%
  filter(get(pelo_col) > 0.75) %>%
  group_by(ID) %>%
  arrange(Season, Race) %>%
  ungroup()
```

**Period Assignment**: Races within each season are divided into periods for seasonal adjustment calculations:

```r
# From race-picks.R:186-196
group_by(Season) %>%
mutate(
  Num_Races = max(Race),
  Period = case_when(
    Num_Races <= 5 ~ 1,
    Num_Races <= 10 ~ 2,
    Num_Races <= 15 ~ 3,
    Num_Races <= 20 ~ 4,
    Num_Races <= 25 ~ 5,
    TRUE ~ ceiling((Race / (Num_Races / 5)))
  )
)
```

###### Feature Selection

The feature selection process uses an exhaustive search approach with Bayesian Information Criterion (BIC) optimization to identify the optimal combination of explanatory variables while avoiding overfitting.

**Available Explanatory Variables**: The system starts with a comprehensive set of predictive features:

```r
# From race-picks.R:807-810
response_variable <- "Points"
explanatory_vars <- c("Prev_Points_Weighted", "Distance_Pelo_Pct", "Sprint_Pelo_Pct", 
                      "Sprint_C_Pelo_Pct", "Distance_F_Pelo_Pct", "Distance_C_Pelo_Pct", 
                      "Classic_Pelo_Pct", "Freestyle_Pelo_Pct", "Sprint_F_Pelo_Pct", "Pelo_Pct")
```

These variables represent:
- **Prev_Points_Weighted**: Weighted average of last 5 race results for specific race type/technique
- **Distance_Pelo_Pct**: Pre-race ELO percentage for distance events
- **Sprint_Pelo_Pct**: Pre-race ELO percentage for sprint events  
- **Sprint_C_Pelo_Pct**: Pre-race ELO percentage for sprint classic events
- **Distance_F_Pelo_Pct**: Pre-race ELO percentage for distance freestyle events
- **Distance_C_Pelo_Pct**: Pre-race ELO percentage for distance classic events
- **Classic_Pelo_Pct**: Pre-race ELO percentage for classic technique events
- **Freestyle_Pelo_Pct**: Pre-race ELO percentage for freestyle technique events
- **Sprint_F_Pelo_Pct**: Pre-race ELO percentage for sprint freestyle events
- **Pelo_Pct**: Pre-race overall ELO percentage

**BIC Optimization**: An exhaustive search evaluates all possible combinations of explanatory variables:

```r
# From race-picks.R:813-816
# Create and fit model for points
formula <- as.formula(paste(response_variable, "~", paste(explanatory_vars, collapse = " + ")))
exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
summary_exhaustive <- summary(exhaustive_selection)
best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
```

The BIC criterion balances model accuracy with complexity, penalizing models with too many variables to prevent overfitting.

**GAM Formula Construction**: Selected variables are converted to smooth terms for the Generalized Additive Model:

```r
# From race-picks.R:817-818
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))
```

The smooth terms `s()` allow the GAM to capture non-linear relationships between predictors and performance, providing more flexibility than linear models while maintaining interpretability.

###### Modeling

GAM models are trained to predict World Cup points using the BIC-selected variables converted to smooth terms. The modeling uses the mgcv package's default settings for optimal performance.

**GAM Model Training**: The system fits a Generalized Additive Model using the optimized formula from feature selection:

```r
# From race-picks.R:820
model <- gam(gam_formula, data = race_df_75)
```

Where `gam_formula` contains the BIC-selected variables as smooth terms (e.g., `Points ~ s(Prev_Points_Weighted) + s(Distance_Pelo_Pct) + s(Classic_Pelo_Pct)`).

**Model Characteristics**: 
- **Response Variable**: World Cup points (0-100 scale)
- **Model Type**: Generalized Additive Model with smooth terms
- **Training Data**: Filtered dataset (`race_df_75`) containing only skiers with ELO ≥75% of race maximum
- **Smoothing**: Automatic smoothing parameter selection via REML (mgcv default)
- **Family**: Gaussian (default for continuous response)

**Model Flexibility**: The GAM approach allows for:
- Non-linear relationships between predictors and points
- Automatic determination of optimal smoothness for each predictor
- Interpretable smooth functions showing how each variable affects performance
- Robust performance across different race types and conditions

The trained model captures complex patterns in how ELO ratings and previous performance translate to race results, providing the foundation for subsequent adjustment calculations and final predictions.

###### Adjustments

Individual skier adjustments are calculated through a sequential process that identifies statistically significant patterns in how skiers perform relative to GAM predictions under different conditions. Each adjustment requires statistical significance (p < 0.05) and sufficient data (≥3 observations per category).

**Sequential Adjustment Calculation**: The system calculates adjustments in a specific order to avoid double-counting effects:

```r
# From race-picks.R:822-837
# Calculate adjustments for historical data step by step
race_df_75 <- race_df_75 %>%
  arrange(Date) %>%
  group_by(Skier) %>%
  mutate(
    row_id = row_number()
  ) %>%
  ungroup() %>%
  # Step 1: Initial predictions
  mutate(
    Initial_Prediction = predict(model, newdata = .)
  ) %>%
  group_by(Skier) %>%
  mutate(
    Prediction_Diff = Points - Initial_Prediction
  )
```

**Altitude Adjustments**: First, the system identifies skiers who consistently perform differently at altitude (≥1300m) versus sea level:

```r
# From race-picks.R:839-852
# Step 2: Calculate altitude p-values and effects
mutate(
  altitude_p = purrr::map_dbl(row_id, function(r) {
    if(r <= 1) return(1)
    prior_alt_curr <- Prediction_Diff[AltitudeCategory == AltitudeCategory[r] & row_id < r]
    prior_alt_other <- Prediction_Diff[AltitudeCategory != AltitudeCategory[r] & row_id < r]
    if(length(prior_alt_curr) < 3 || length(prior_alt_other) < 3) return(1)
    tryCatch({
      t.test(prior_alt_curr, prior_alt_other)$p.value
    }, error = function(e) 1)
  }),
  altitude_correction = ifelse(altitude_p < 0.05,
                               mean(Prediction_Diff[AltitudeCategory == AltitudeCategory], na.rm = TRUE),
                               0)
)
```

**Period Adjustments**: After accounting for altitude effects, period adjustments are calculated for seasonal performance patterns:

```r
# From race-picks.R:858-874
# Step 4: Calculate Period adjustments
mutate(
  period_p = purrr::map_dbl(row_id, function(r) {
    if(r <= 1) return(1)
    prior_period_curr <- Course_Diff[Period == Period[r] & row_id < r]
    prior_period_other <- Course_Diff[Period != Period[r] & row_id < r]
    if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
    tryCatch({
      t.test(prior_period_curr, prior_period_other)$p.value
    }, error = function(e) 1)
  }),
  period_correction = ifelse(period_p < 0.05,
                             mean(Course_Diff[Period == Period], na.rm = TRUE),
                             0),
  Period_Adjusted = Course_Adjusted + period_correction,
  Period_Diff = Points - Period_Adjusted
)
```

**Mass Start Adjustments**: Finally, mass start format adjustments are calculated after accounting for altitude and period effects:

```r
# From race-picks.R:876-888
# Step 5: Calculate Mass Start adjustments
mutate(
  ms_p = purrr::map_dbl(row_id, function(r) {
    if(r <= 1) return(1)
    prior_ms_curr <- Period_Diff[MS == MS[r] & row_id < r]
    prior_ms_other <- Period_Diff[MS != MS[r] & row_id < r]
    if(length(prior_ms_curr) < 3 || length(prior_ms_other) < 3) return(1)
    tryCatch({
      t.test(prior_ms_curr, prior_ms_other)$p.value
    }, error = function(e) 1)
  }),
  ms_correction = ifelse(ms_p < 0.05,
                         mean(Period_Diff[MS == MS], na.rm = TRUE),
                         0)
)
```

**Statistical Requirements**: All adjustments require:
- Statistical significance (p < 0.05) via t-test
- Minimum 3 observations in each comparison category
- Sequential calculation to prevent interaction effects
- Temporal ordering (only using prior race data for each calculation)

##### Testing

###### Startlist Setup

Prepares the scraped startlist for predictions by setting participation probabilities, calculating ELO percentages, computing weighted previous points, and imputing missing values with first quartile substitution.

**Participation Probability Assignment**: If the startlist was scraped from the FIS website, all athletes receive a participation probability of 1.0 (confirmed participation). For mock startlists (created when FIS data is unavailable), participation probabilities are calculated by examining the athlete's historical participation rate in similar races:

```python
# From startlist_common.py:443-491
def get_additional_national_skiers(chronos: pd.DataFrame, elo_scores: pd.DataFrame, 
                                fantasy_prices: Dict[str, int], nation: str, current_season: int = None) -> List[dict]:
    """Get all skiers from a nation who competed in the current season"""
    if current_season is None:
        current_season = get_current_season_from_chronos(chronos)
    
    # Get all skiers from this nation in current season
    nation_skiers = chronos[
        (chronos['Nation'] == nation) & 
        (chronos['Season'] == current_season)
    ]['Skier'].unique()
```

**ELO Score Processing**: The system retrieves the most recent ELO scores for each athlete and calculates ELO percentages by dividing by the maximum ELO score in that race:

```r
# From race-picks.R:263-336
most_recent_elos <- race_df %>%
    filter(Skier %in% base_df$Skier) %>%
    group_by(Skier) %>%
    arrange(Date, Season, Race) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(Skier, any_of(elo_cols))

# Calculate both Elo and Pelo percentages for available columns
for(i in seq_along(elo_cols)) {
    elo_col <- elo_cols[i]
    pelo_col_i <- pelo_cols[i]
    
    # Check if column exists in both datasets
    if(elo_col %in% names(result_df) && elo_col %in% names(max_values)) {
        max_val <- max_values[[elo_col]]
        # Only calculate if max value is not zero or NA
        if(!is.na(max_val) && max_val > 0) {
            # Calculate the percentage
            pct_value <- result_df[[elo_col]] / max_val
            
            # Assign to both Elo and Pelo percentage columns
            result_df[[paste0(elo_col, "_Pct")]] <- pct_value
            result_df[[paste0(pelo_col_i, "_Pct")]] <- pct_value
        }
    }
}
```

**Weighted Previous Points**: For each athlete, the system calculates weighted averages of their last 5 race results in the specific race type and technique combination, with more recent races receiving higher weights (1,2,3,4,5):

```r
# From race-picks.R:272-291
recent_points <- race_df %>%
    filter(Skier %in% base_df$Skier) %>%
    filter(
        if(grepl("^Sprint", pelo_col)) {
            Distance == "Sprint" & 
                Technique == substr(pelo_col, 8, 8)
        } else {
            Distance != "Sprint" & 
                (Technique == substr(pelo_col, 10, 10) | substr(pelo_col, 10, 10) == "")
        }
    ) %>%
    group_by(Skier) %>%
    arrange(Season, Race) %>%
    slice_tail(n = 5) %>%
    summarise(
        Prev_Points_Weighted = if(n() > 0) 
            weighted.mean(Points, w = seq_len(n()), na.rm = TRUE) 
        else 0
    )
```

**Missing Value Imputation**: Finally, all missing values in the dataset are replaced with first quartile values to account for lack of experience being a detriment to performance:

```r
# From race-picks.R:338-344
result_df <- result_df %>%
    mutate(
        across(
            ends_with("_Pct"),
            ~replace_na_with_quartile(.x)
        ),
```

The `replace_na_with_quartile` function is defined as:

```r
# From race-picks.R:18-23
replace_na_with_quartile <- function(x) {
    if(all(is.na(x))) return(rep(0, length(x)))
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    ifelse(is.na(x), q1, x)
}
```

###### Modeling

Applies the trained GAM models to the prepared startlist data to generate point predictions for each athlete. The modeling process uses the BIC-optimized GAM with smooth terms for the selected explanatory variables.

**Base Prediction**: The system applies the trained GAM model to the prepared startlist data:

```r
# From race-picks.R:1218
Base_Prediction = predict(model, newdata = .)
```

Where the `model` is the GAM fitted during training using:

```r
# From race-picks.R:814-820
exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
summary_exhaustive <- summary(exhaustive_selection)
best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))

model <- gam(gam_formula, data = race_df_75)
```

**Final Prediction Calculation**: The base prediction is combined with adjustments to produce the final point prediction:

```r
# From race-picks.R:1241-1243
Predicted_Points = Base_Prediction + altitude_adjustment + 
  period_adjustment + ms_adjustment,
Predicted_Points = pmax(pmin(Predicted_Points, 100), 0)
```

The final predictions are constrained between 0 and 100 points to ensure realistic values within the World Cup points system range.

###### Adjustments

Three types of adjustments are applied to the base GAM predictions: altitude, period, and mass start adjustments. These adjustments are calculated during training using t-tests (p < 0.05) and applied during testing.

**Adjustment Application**: The trained adjustment effects are joined to the startlist and applied to base predictions:

```r
# From race-picks.R:1220, 1236-1242
left_join(skier_adjustments, by = "Skier") %>%
mutate(
    # Apply learned adjustments
    altitude_adjustment = altitude_effect,
    period_adjustment = period_effect,
    ms_adjustment = ms_effect,
    
    # Combine base prediction with all adjustments
    Predicted_Points = Base_Prediction + altitude_adjustment + 
      period_adjustment + ms_adjustment,
    Predicted_Points = pmax(pmin(Predicted_Points, 100), 0)
)
```

**Altitude Adjustments**: During training, altitude effects are calculated by comparing skier performance at altitude (≥1300m) versus sea level using t-tests:

```r
# From race-picks.R:840-851
altitude_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_alt_curr <- Prediction_Diff[AltitudeCategory == AltitudeCategory[r] & row_id < r]
  prior_alt_other <- Prediction_Diff[AltitudeCategory != AltitudeCategory[r] & row_id < r]
  if(length(prior_alt_curr) < 3 || length(prior_alt_other) < 3) return(1)
  tryCatch({
    t.test(prior_alt_curr, prior_alt_other)$p.value
  }, error = function(e) 1)
}),
altitude_correction = ifelse(altitude_p < 0.05,
                             mean(Prediction_Diff[AltitudeCategory == AltitudeCategory], na.rm = TRUE),
                             0)
```

**Period Adjustments**: Period effects are calculated similarly by comparing skier performance across different periods of the World Cup season:

```r
# From race-picks.R:860-871
period_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_period_curr <- Course_Diff[Period == Period[r] & row_id < r]
  prior_period_other <- Course_Diff[Period != Period[r] & row_id < r]
  if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
  tryCatch({
    t.test(prior_period_curr, prior_period_other)$p.value
  }, error = function(e) 1)
}),
period_correction = ifelse(period_p < 0.05,
                           mean(Course_Diff[Period == Period], na.rm = TRUE),
                           0)
```

**Mass Start Adjustments**: Mass start effects compare skier performance in mass start versus individual start formats:

```r
# From race-picks.R:877-887
ms_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_ms_curr <- Period_Diff[MS == MS[r] & row_id < r]
  prior_ms_other <- Period_Diff[MS != MS[r] & row_id < r]
  if(length(prior_ms_curr) < 3 || length(prior_ms_other) < 3) return(1)
  tryCatch({
    t.test(prior_ms_curr, prior_ms_other)$p.value
  }, error = function(e) 1)
}),
ms_correction = ifelse(ms_p < 0.05,
                       mean(Period_Diff[MS == MS], na.rm = TRUE),
                       0)
```

All adjustments require statistical significance (p < 0.05) and at least 3 observations in each category before being applied.

#### Probability

##### Training

###### Setup

The probability models use the same training data setup as points prediction but convert the problem to binary classification for different finishing position thresholds. The system creates separate models for each position threshold to predict the probability of achieving that placement or better.

**Binary Outcome Creation**: For each position threshold, binary outcomes are created from finishing positions:

```r
# From race-picks.R:903-904
# Create binary outcome variable for position threshold
race_df$position_achieved <- race_df$Place <= threshold
```

**Position Thresholds**: Different thresholds are used based on race type to reflect competition structure:

```r
# From race-picks.R:741-752
# Define position thresholds (keeping consistent column names)
position_thresholds <- c(1, 3, 5, 10, 30)  # Win, Podium, Top 5, Top 10, Top 30

# Process each race
for(i in 1:nrow(races)) {
  # Override thresholds for Sprint races (but keep same column names for consistency)
  if(races$distance[i] == "Sprint") {
    position_thresholds <- c(1, 3, 6, 12, 30)  # Win, Podium, Final, Semifinal, Quarterfinal
  } else {
    position_thresholds <- c(1, 3, 5, 10, 30)   # Win, Podium, Top 5, Top 10, Top 30
  }
}
```

**Training Data Consistency**: The same filtered dataset (`race_df_75`) and explanatory variables from points modeling are used:

```r
# From race-picks.R:896-897
# Feature selection - use the same explanatory variables as the points model
position_feature_vars <- explanatory_vars
```

This ensures consistency between points and probability predictions while adapting the outcome structure to the binary classification requirements of position probability modeling.

###### Feature Selection

The probability models use the same BIC optimization approach as points prediction, but feature selection is performed independently for each position threshold to identify the optimal predictors for different finishing position categories.

**Threshold-Specific Feature Selection**: Each position threshold gets its own optimized set of variables:

```r
# From race-picks.R:899-912
# Create models for each position threshold
for(threshold in position_thresholds) {
  log_info(paste("Creating model for top", threshold, "positions"))
  
  # Create binary outcome variable for position threshold
  race_df$position_achieved <- race_df$Place <= threshold
  
  # Create formula for regsubsets
  pos_formula <- as.formula(paste("position_achieved ~", paste(position_feature_vars, collapse = " + ")))
  
  tryCatch({
    pos_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")
    pos_summary <- summary(pos_selection)
    pos_best_bic_vars <- names(coef(pos_selection, which.min(pos_summary$bic)))
```

**GAM Formula Construction for Binary Outcomes**: Selected variables are converted to smooth terms for binomial GAM modeling:

```r
# From race-picks.R:914-916
# Create smooth terms for GAM using best BIC variables (remove intercept)
pos_smooth_terms <- paste("s(", pos_best_bic_vars[-1], ")", collapse=" + ")
pos_gam_formula <- as.formula(paste("position_achieved ~", pos_smooth_terms))
```

**Independent Optimization**: This approach allows different position thresholds to use different variable combinations because:
- **Top-1 (Win)**: May prioritize overall ELO and recent form
- **Top-3 (Podium)**: May emphasize consistency and technique-specific ELO
- **Top-5**: May balance various factors
- **Top-10**: May include broader performance indicators
- **Top-30**: May focus on participation and basic competitiveness

The exhaustive BIC search ensures each threshold gets the statistically optimal predictor combination for that specific binary classification task, maximizing predictive accuracy for each position category.

###### Modeling

Binomial GAM models are trained for each position threshold using the threshold-specific optimized variables. The models use REML estimation and include comprehensive evaluation metrics to assess prediction quality.

**Binomial GAM Training**: Each position threshold gets its own specialized model:

```r
# From race-picks.R:918-922
# Fit the position model with binomial family
position_model <- gam(pos_gam_formula,
                      data = race_df,
                      family = binomial,
                      method = "REML")
```

**Model Specifications**:
- **Family**: Binomial (appropriate for binary outcomes)
- **Method**: REML (Restricted Maximum Likelihood for optimal smoothing parameter selection)
- **Response**: Binary indicator (1 if position ≤ threshold, 0 otherwise)
- **Predictors**: BIC-optimized smooth terms specific to each threshold

**Model Evaluation**: Brier scores are calculated to assess prediction quality:

```r
# From race-picks.R:924-927
# Calculate Brier score for model evaluation
predicted_probs <- predict(position_model, newdata = race_df, type = "response")
brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
log_info(paste("Brier score for threshold", threshold, ":", round(brier_score, 4)))
```

**Model Storage**: Trained models are stored for later use in predictions:

```r
# From race-picks.R:930
# Store the model
position_models[[paste0("threshold_", threshold)]] <- position_model
```

**Brier Score Evaluation**: The Brier score measures the accuracy of probabilistic predictions by penalizing both:
- **Overconfidence**: Predicting high probability when outcome doesn't occur
- **Underconfidence**: Predicting low probability when outcome does occur

Lower Brier scores indicate better calibrated probability predictions, with perfect predictions scoring 0.0 and random predictions scoring 0.25 for balanced binary outcomes.

###### Adjustments

Position probability adjustments follow the same sequential methodology as points prediction but operate on probability residuals rather than point differences. Each position threshold gets its own set of adjustments calculated independently.

**Probability Residual Calculation**: First, position-specific predictions and residuals are calculated:

```r
# From race-picks.R:942-948
# Add predictions separately (outside of mutate)
position_df$initial_prob <- predict(position_model, newdata = position_df, type = "response")

# Calculate adjustments
position_df <- position_df %>%
  group_by(Skier) %>%
  mutate(
    prob_diff = as.numeric(position_achieved) - initial_prob
```

**Altitude Adjustments for Probabilities**: Altitude effects are calculated using probability residuals:

```r
# From race-picks.R:951-963
altitude_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_alt_curr <- prob_diff[AltitudeCategory == AltitudeCategory[r] & row_id < r]
  prior_alt_other <- prob_diff[AltitudeCategory != AltitudeCategory[r] & row_id < r]
  if(length(prior_alt_curr) < 3 || length(prior_alt_other) < 3) return(1)
  tryCatch({
    t.test(prior_alt_curr, prior_alt_other)$p.value
  }, error = function(e) 1)
}),
altitude_correction = ifelse(altitude_p < 0.05,
                             mean(prob_diff[AltitudeCategory == AltitudeCategory], na.rm = TRUE),
                             0),
altitude_adjusted = pmin(pmax(initial_prob + altitude_correction, 0), 1),
altitude_diff = as.numeric(position_achieved) - altitude_adjusted
```

**Period Adjustments for Probabilities**: Period effects use the altitude-adjusted residuals:

```r
# From race-picks.R:968-981
period_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_period_curr <- altitude_diff[Period == Period[r] & row_id < r]
  prior_period_other <- altitude_diff[Period != Period[r] & row_id < r]
  if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
  tryCatch({
    t.test(prior_period_curr, prior_period_other)$p.value
  }, error = function(e) 1)
}),
period_correction = ifelse(period_p < 0.05,
                           mean(altitude_diff[Period == Period], na.rm = TRUE),
                           0),
period_adjusted = pmin(pmax(altitude_adjusted + period_correction, 0), 1),
period_diff = as.numeric(position_achieved) - period_adjusted
```

**Mass Start Adjustments for Probabilities**: Mass start effects are calculated after accounting for altitude and period:

```r
# From race-picks.R:985-996
ms_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_ms_curr <- period_diff[MS == MS[r] & row_id < r]
  prior_ms_other <- period_diff[MS != MS[r] & row_id < r]
  if(length(prior_ms_curr) < 3 || length(prior_ms_other) < 3) return(1)
  tryCatch({
    t.test(prior_ms_curr, prior_ms_other)$p.value
  }, error = function(e) 1)
}),
ms_correction = ifelse(ms_p < 0.05,
                       mean(period_diff[MS == MS], na.rm = TRUE),
                       0)
```

**Key Differences from Points Adjustments**:
- **Residuals**: Uses probability prediction residuals instead of point prediction residuals
- **Threshold-Specific**: Each position threshold (1, 3, 5, 10, 30) gets independent adjustments
- **Probability Bounds**: Adjustments are constrained to [0,1] using `pmin(pmax(..., 0), 1)`
- **Sequential Processing**: Same altitude → period → mass start order to prevent interaction effects

##### Testing

###### Startlist Setup

The probability testing uses the same startlist setup as points prediction, with additional preparation for position-specific probability predictions across all thresholds.

**Base Startlist Preparation**: The same startlist preparation function is used:

```r
# From race-picks.R:1101
startlist_prepared <- prepare_startlist_data(startlist, race_df, pelo_col, gender)
```

This includes all the same steps as points prediction: participation probability assignment, ELO score processing, weighted previous points calculation, and missing value imputation.

**Position Prediction Framework Setup**: A specialized dataframe is created for probability predictions:

```r
# From race-picks.R:1104-1110
# NEW: Make position probability predictions with adjustments
position_preds <- data.frame(
  Skier = startlist_prepared$Skier,
  ID = startlist_prepared$ID,
  Nation = startlist_prepared$Nation,
  Sex = startlist_prepared$Sex,
  Race = i
)
```

**Threshold-Specific Processing**: The system prepares to generate predictions for each position threshold:

```r
# From race-picks.R:1113-1117
# Make predictions for each threshold
for(threshold in position_thresholds) {
  model_name <- paste0("threshold_", threshold)
  adj_name <- paste0("threshold_", threshold)
  prob_col <- paste0("prob_top", threshold)
```

**Model Variable Validation**: Before prediction, the system validates that all required variables are available:

```r
# From race-picks.R:1121-1124
# Get the model
pos_model <- position_models[[model_name]]

# Check what variables the model actually needs
model_vars <- names(pos_model$var.summary)
```

This ensures that each threshold-specific model gets the appropriate variables from the prepared startlist, maintaining consistency between training and testing phases while adapting to the multiple-model structure of probability prediction.

###### Modeling

Position probability prediction applies the trained binomial GAM models to startlist data, generating probabilities for each threshold with comprehensive variable validation and missing value handling.

**Variable Preparation**: The system ensures all model-required variables are available and properly formatted:

```r
# From race-picks.R:1125-1141
# Create a clean subset of prediction data with only required variables
prediction_subset <- startlist_prepared

# Ensure all required variables exist
for(var in model_vars) {
  if(!(var %in% names(prediction_subset))) {
    log_warn(paste("Missing required variable:", var, "- adding with default values"))
    prediction_subset[[var]] <- 0
  } else {
    # Handle NAs
    if(any(is.na(prediction_subset[[var]]))) {
      if(is.numeric(prediction_subset[[var]])) {
        prediction_subset[[var]] <- replace_na_with_quartile(prediction_subset[[var]])
      }
    }
  }
}
```

**Base Probability Prediction**: Each threshold-specific model generates probability predictions:

```r
# From race-picks.R:1144-1147
# Make predictions
base_predictions <- mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response")

# Store predictions
position_preds[[paste0(prob_col, "_base")]] <- base_predictions
```

**Adjustment Application**: If adjustments are available, they are applied with probability bounds enforcement:

```r
# From race-picks.R:1156-1174
# Join with predictions
position_preds <- position_preds %>%
  left_join(pos_adj, by = "Skier") %>%
  mutate(
    # Replace NAs with zeros
    altitude_effect = replace_na(altitude_effect, 0),
    period_effect = replace_na(period_effect, 0),
    ms_effect = replace_na(ms_effect, 0),
    
    # Apply adjustments
    altitude_adjustment = altitude_effect,
    period_adjustment = period_effect,
    ms_adjustment = ms_effect,
    
    # Calculate adjusted probabilities
    adjusted_prob = get(paste0(prob_col, "_base")) + 
      altitude_adjustment + period_adjustment + ms_adjustment,
    
    # Ensure probabilities are between 0 and 1
    adjusted_prob = pmin(pmax(adjusted_prob, 0), 1)
```

**Key Features**:
- **Type "response"**: Returns probabilities [0,1] rather than logit values
- **Threshold-specific**: Each position threshold uses its optimized model
- **Robust handling**: Missing variables get default values, NAs get quartile imputation
- **Probability bounds**: Final predictions constrained to valid [0,1] range

###### Adjustments

Altitude, period, and mass start adjustments learned during training are applied to base probability predictions (`race-picks.R:1150-1183`).

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are finalized. Normalization is applied so that position probabilities sum to the correct percentage. For first place that sum to 100%, top-3 would sum to 300%, etc. Individual athlete probabilities are capped at 100% since anything above that would be impossible (`race-picks.R:352-490`).

After normalization, monotonic constraints are added. This ensures that top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so that an athlete cannot have a higher chance of finishing top-1 than top-3. Then normalization is applied again to the monotonic constraint results to give the final results (`race-picks.R:432-476`).

### Relay

#### Data Gathering

Relay startlists are scraped from the FIS website using three specialized Python scripts for different relay types: standard relays, mixed relays, and team sprints. Each type requires distinct processing logic due to their unique structures and requirements.

**Standard Relay Data Gathering**: Standard relays typically have 4 members per team with specific leg assignments:

```python
# From startlist_scrape_races_relay.py:186-204
def get_relay_teams(url: str) -> List[Dict]:
    """
    Get teams from FIS relay startlist
    
    Returns list of teams with structure:
    [
        {
            'team_name': 'COUNTRY I',
            'nation': 'XXX',
            'team_rank': 1,
            'team_time': '54:45.3',
            'members': [
                {'name': 'ATHLETE NAME', 'nation': 'XXX', 'year': '1994', 'bib': '2-1'},
                {'name': 'ATHLETE NAME', 'nation': 'XXX', 'year': '1997', 'bib': '2-2'},
                ...
            ]
        },
        ...
    ]
    """
```

The standard relay scraper processes HTML from FIS startlist pages to extract team information:

```python
# From startlist_scrape_races_relay.py:213-250
# Find all team rows (main rows) - these have class 'table-row_theme_main'
team_rows = soup.select('.table-row.table-row_theme_main')

for team_row in team_rows:
    # Extract team information
    team_name_elem = team_row.select_one('.g-lg-14.g-md-14.g-sm-11.g-xs-10.justify-left.bold')
    team_name = team_name_elem.text.strip()
    
    # Get nation from country span
    country_elem = team_row.select_one('.country__name-short')
    nation = country_elem.text.strip()
    
    # Get team rank
    rank_elem = team_row.select_one('.g-lg-1.g-md-1.g-sm-1.g-xs-2.justify-right.bold')
    team_rank = rank_elem.text.strip() if rank_elem else "0"
```

**Team Sprint Data Gathering**: Team sprints have exactly 2 members per team with specialized bib parsing:

```python
# From startlist_scrape_races_team_sprint.py:189-208
def get_team_sprint_teams(url: str) -> List[Dict]:
    """
    Get teams from FIS team sprint startlist - FIXED VERSION
    
    Returns list of teams with structure:
    [
        {
            'team_name': 'ITALY',
            'nation': 'ITA',
            'team_rank': '1',
            'team_time': '7:53.89',
            'team_number': 1,
            'members': [
                {'name': 'GANZ Caterina', 'nation': 'ITA', 'year': '1995', 'bib': '7-1'},
                {'name': 'CASSOL Federica', 'nation': 'ITA', 'year': '2000', 'bib': '7-2'}
            ]
        },
        ...
    ]
    """
```

Team sprint processing includes special handling for team numbering within nations:

```python
# From startlist_scrape_races_team_sprint.py:224-268
# Track team numbers by nation
nation_team_counts = {}

# Update team counter for this nation
if nation not in nation_team_counts:
    nation_team_counts[nation] = 1
else:
    nation_team_counts[nation] += 1

team_number = nation_team_counts[nation]
```

**Mixed Relay Data Gathering**: Mixed relays require gender detection and specialized team composition handling:

```python
# From startlist_scrape_races_mixed_relay.py:87-135
def process_mixed_relay_races(races_file: str = None) -> None:
    """
    Main function to process mixed relay races
    
    Args:
        races_file: Optional path to a CSV containing specific races to process
    """
    print(f"Processing mixed relay races")
    
    # Filter to only mixed relay races
    races_df = races_df[races_df['Distance'] == 'Mix']
    print(f"Filtered to {len(races_df)} mixed relay races")
```

All relay types use consistent country name mapping to standardize team names:

```python
# From startlist_scrape_races_relay.py:632-744
def map_country_to_team_name(country: str) -> str:
    """
    Map country names from individuals to exact team names from team spreadsheet
    Returns empty string if no match found
    """
    # Direct mapping from individual country names AND codes to team names
    country_to_team = {
        "Norway": "NORWAY",
        "Sweden": "SWEDEN", 
        "Finland": "FINLAND",
        "Germany": "GERMANY",
        "France": "FRANCE",
        # ... extensive mapping for both full names and 3-letter codes
        "NOR": "NORWAY",
        "SWE": "SWEDEN",
        "FIN": "FINLAND"
    }
```

**Team Data Processing**: Each relay type processes teams to create both team-level and individual-level records:

```python
# From startlist_scrape_races_team_sprint.py:342-541
def process_team_sprint_teams(teams: List[Dict], race: pd.Series, gender: str) -> Tuple[List[Dict], List[Dict]]:
    """
    Process team sprint teams and create team and individual data
    
    Returns:
        tuple: (team_data, individual_data)
    """
    # Initialize data for teams and individual athletes
    team_data = []
    individual_data = []
    
    # Get the ELO scores
    elo_path = f"~/ski/elo/python/ski/polars/excel365/{gender}_chrono_elevation.csv"
    elo_scores = get_latest_elo_scores(elo_path)
```

**Elo Integration and Fantasy Pricing**: All relay types enhance team data with individual Elo scores and fantasy prices:

```python
# From startlist_scrape_races_relay.py:322-345
# Get the ELO scores
elo_path = f"~/ski/elo/python/ski/polars/excel365/{gender}_chrono_elevation.csv"
elo_scores = get_latest_elo_scores(elo_path)

# Get fantasy prices and team prices
fantasy_prices = get_fantasy_prices()
fantasy_teams = get_fantasy_teams(gender)

# Define Elo columns to work with
elo_columns = [
    'Elo', 'Distance_Elo', 'Distance_C_Elo', 'Distance_F_Elo', 
    'Sprint_Elo', 'Sprint_C_Elo', 'Sprint_F_Elo', 
    'Classic_Elo', 'Freestyle_Elo'
]
```

**Output Generation**: All relay scraping scripts generate CSV files for both team and individual data:

```python
# From startlist_scrape_races_relay.py:575-607
def save_relay_individual_data(df: pd.DataFrame, gender: str) -> None:
    """Save processed relay individual data to a CSV file"""
    # Sort by team rank and position
    df = df.sort_values(['Team_Rank', 'Team_Position'])
    
    # Save to CSV
    output_path = f"~/ski/elo/python/ski/polars/relay/excel365/startlist_relay_races_individuals_{gender}.csv"
    df.to_csv(output_path, index=False)

def save_relay_team_data(df: pd.DataFrame, gender: str) -> None:
    """Save processed relay team data to a CSV file"""
    # Sort by team rank
    df = df.sort_values(['Team_Rank'])
    
    # Save to CSV
    output_path = f"~/ski/elo/python/ski/polars/relay/excel365/startlist_relay_races_teams_{gender}.csv"
    df.to_csv(os.path.expanduser(output_path), index=False)
```

The relay data gathering process ensures comprehensive team and individual athlete information is captured with proper Elo integration, fantasy pricing, and country name standardization across all three relay formats.

#### Points

##### Training

###### Setup

Relay predictions use a fundamentally different approach than individual races, combining historical relay data with individual race data to train leg-specific models. The system recognizes that relay races have different dynamics for different legs, with classic legs (1-2) and freestyle legs (3-4) requiring distinct modeling approaches.

**Relay Points System**: Points are assigned using the relay-specific World Cup points system rather than individual race points:

```r
# From race-picks-relay.R:24-26
# Define points system
relay_points <- c(200, 160, 120, 100, 90, 80, 72, 64, 58, 52, 48, 44, 40, 36, 
                  32, 30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10, 8, 6, 4, 2)
```

Points are assigned to relay results using this system:

```r
# From race-picks-relay.R:135-143
# Function to add points to race results
add_points_to_results <- function(df, is_relay = FALSE) {
  df %>%
    mutate(Points = mapply(function(place) {
      if (place >= 1 && place <= length(relay_points)) {
        return(relay_points[place])
      }
      return(0)
    }, Place))
}
```

**Data Integration Strategy**: Historical relay data is combined with individual race data using a sophisticated temporal approach:

```r
# From race-picks-relay.R:247-284
# Combine classic legs with classic individual data
classic_combined <- bind_rows(
  classic_legs_all,
  classic_df
) %>%
  group_by(ID) %>%
  arrange(Date, Season, Race, desc(Distance)) %>%  # Use Date for chronological order
  fill(Weighted_Last_5, .direction = "down") %>%
  filter(Distance == "Rel", Season > min_season) %>%  # Apply season filter AFTER filling
  group_by(Season, Race) %>%  # Regroup by race for quartile replacement
  mutate(
    Weighted_Last_5 = ifelse(
      is.na(Weighted_Last_5),
      quantile(Weighted_Last_5, 0.25, na.rm = TRUE),
      Weighted_Last_5
    )
  ) %>%
  ungroup()

# Combine freestyle legs with freestyle individual data  
freestyle_combined <- bind_rows(
  freestyle_legs_all,
  freestyle_df
) %>%
  group_by(ID) %>%
  arrange(Date, Season, Race, desc(Distance)) %>%
  fill(Weighted_Last_5, .direction = "down") %>%
  filter(Distance == "Rel", Season > min_season) %>%
  group_by(Season, Race) %>%
  mutate(
    Weighted_Last_5 = ifelse(
      is.na(Weighted_Last_5),
      quantile(Weighted_Last_5, 0.25, na.rm = TRUE),
      Weighted_Last_5
    )
  ) %>%
  ungroup()
```

**Leg-Specific Data Separation**: Relay legs are separated by technique, with classic legs (1-2) and freestyle legs (3-4) processed differently:

```r
# From race-picks-relay.R:239-245
# Process classic legs (1-2) - don't filter by season yet
classic_legs_all <- relay_with_points %>%
  filter(Distance == "Rel", Leg < 3)

# Process freestyle legs (3-4) - don't filter by season yet
freestyle_legs_all <- relay_with_points %>%
  filter(Distance == "Rel", Leg > 2)
```

**Training Dataset Preparation**: Each leg gets its own dataset with binary outcome variables:

```r
# From race-picks-relay.R:292-320
# Function to prepare leg-specific datasets
prepare_leg_data <- function(classic_legs, freestyle_legs) {
  # Create datasets for each leg
  leg_data <- list()
  
  # Legs 1 and 2 (Classic)
  for(i in 1:2) {
    leg_data[[i]] <- classic_legs %>%
      filter(Leg == i) %>%
      mutate(
        is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
        is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
        is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
        is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
      )
  }
  
  # Legs 3 and 4 (Freestyle)
  for(i in 3:4) {
    leg_data[[i]] <- freestyle_legs %>%
      filter(Leg == i) %>%
      mutate(
        is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
        is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
        is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
        is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
      )
  }
  return(leg_data)
}
```

**Key Training Setup Features**:

1. **Temporal Ordering**: Uses `arrange(Date, Season, Race, desc(Distance))` to ensure chronological order when filling missing values from individual races to relay legs
2. **Missing Value Strategy**: Applies first quartile replacement within each race for missing `Weighted_Last_5` values to account for lack of experience
3. **Season Filtering**: Filters to recent seasons (typically last 11 years) AFTER filling missing values to preserve maximum training data
4. **Data Propagation**: Uses `fill(.direction = "down")` to propagate weighted previous points from individual races to relay legs where an athlete has individual race history but missing relay-specific weighted points

This approach creates comprehensive leg-specific training datasets that leverage both relay-specific performance history and individual race performance to handle the unique dynamics of relay competition.

###### Feature Selection

Relay feature selection uses a rule-based, technique-specific approach rather than statistical selection methods like BIC. The selection is deterministic and based on leg position, technique type, and racing dynamics.

**Leg-Specific Predictor Selection**: The core feature selection logic assigns different predictors based on leg position and technique:

```r
# From race-picks-relay.R:323-362
get_leg_predictors <- function(leg, leg_data) {
  # Get column names from the leg data
  base_cols <- names(leg_data[[leg]])
  
  # Define predictors based on leg
  if(leg <= 2) {
    # Classic legs (1 and 2)
    predictors <- c(
      grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE),
      grep("Classic.*Pelo_Pct$", base_cols, value = TRUE),
      "Distance_Pelo_Pct",
      "Pelo_Pct",
      "Weighted_Last_5"
    )
  } else if (leg == 3) {
    # Freestyle legs (3)
    predictors <- c(
      grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE),
      grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE),
      "Distance_Pelo_Pct",
      "Pelo_Pct",
      "Weighted_Last_5"
    )
  } else if (leg == 4) {
    # Anchor leg (often with sprint finish)
    predictors <- c(
      grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE),
      grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE),
      "Distance_Pelo_Pct",
      "Sprint_Pelo_Pct",
      "Pelo_Pct",
      "Weighted_Last_5"
    )
  }
  
  # Remove any NA or invalid column names
  predictors <- predictors[predictors %in% names(leg_data[[leg]])]
  
  return(predictors)
}
```

**Feature Categories by Leg Position**:

**Classic Legs (1-2)**:
- `Distance_C.*Pelo_Pct` - Classic distance performance percentages (e.g., Distance_C_Pelo_Pct)
- `Classic.*Pelo_Pct` - Classic technique performance percentages (e.g., Classic_Pelo_Pct) 
- `Distance_Pelo_Pct` - General distance performance percentage
- `Pelo_Pct` - Overall Elo-based performance percentage
- `Weighted_Last_5` - Weighted average of last 5 classic race performances

**Freestyle Leg (3)**:
- `Distance_F.*Pelo_Pct` - Freestyle distance performance percentages (e.g., Distance_F_Pelo_Pct)
- `Freestyle.*Pelo_Pct` - Freestyle technique performance percentages (e.g., Freestyle_Pelo_Pct)
- `Distance_Pelo_Pct` - General distance performance percentage
- `Pelo_Pct` - Overall Elo-based performance percentage
- `Weighted_Last_5` - Weighted average of last 5 freestyle race performances

**Anchor Leg (4)**:
- All freestyle features (same as leg 3)
- **Additional**: `Sprint_Pelo_Pct` - Sprint performance percentage for tactical sprint finishes

**Pelo Percentage Feature Creation**: The percentage features are created by normalizing Elo-based ratings within each race:

```r
# From race-picks-relay.R:145-173
create_pelo_pcts <- function(df) {
  # Define pelo_cols inside the function
  pelo_cols <- names(df)[grep("Pelo$", names(df))]
  
  # For each race and each Pelo column
  df_transformed <- df %>%
    group_by(Date, Race) %>%
    mutate(across(
      all_of(pelo_cols),
      function(x) {
        # Replace NAs with first quartile
        q1 <- quantile(x, 0.25, na.rm = TRUE)
        x_filled <- replace(x, is.na(x), q1)
        # Calculate percentage of max
        x_filled / max(x_filled) * 100
      },
      .names = "{.col}_Pct"
    )) %>%
    ungroup()
}
```

**Feature Application in Training**: The leg-specific predictors are applied when training models:

```r
# From race-picks-relay.R:493-550
for(leg in 1:4) {
  leg_predictors <- get_leg_predictors(leg, leg_data)
  log_info(paste("Using predictors:", paste(leg_predictors, collapse = ", ")))
  
  # Create formulas for different outcome types
  podium_formula <- as.formula(paste("is_podium ~", paste(leg_predictors, collapse = "+")))
  win_formula <- as.formula(paste("is_win ~", paste(leg_predictors, collapse = "+")))
  top5_formula <- as.formula(paste("is_top5 ~", paste(leg_predictors, collapse = "+")))
  top10_formula <- as.formula(paste("is_top10 ~", paste(leg_predictors, collapse = "+")))
}
```

**Key Feature Selection Principles**:

1. **Technique Alignment**: Classic legs use classic-specific features, freestyle legs use freestyle-specific features
2. **Position-Based Logic**: Legs 1-2 are treated as classic, legs 3-4 as freestyle, reflecting standard relay format
3. **Anchor Leg Enhancement**: Leg 4 gets additional sprint features to account for tactical sprint finishes
4. **Domain Knowledge**: No automated selection - features chosen based on skiing domain expertise
5. **Missing Value Handling**: Uses first quartile replacement within each race for missing Elo values
6. **Normalization**: All features converted to race-relative percentages for fair comparison

**Mixed Relay Feature Selection**: Uses gender-aware leg filtering with position-specific features for alternating male/female legs:

```r
# From race-picks-mixed-relay.R:145-180
# Leg 1 (Female Classic) and Leg 2 (Male Classic)
predictors <- c(
  grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE),
  grep("Classic.*Pelo_Pct$", base_cols, value = TRUE),
  "Distance_Pelo_Pct", "Pelo_Pct", "Weighted_Last_5"
)

# Leg 4 (Male Freestyle - Anchor with sprint)
predictors <- c(
  grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE),
  grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE),
  "Distance_Pelo_Pct", "Sprint_Pelo_Pct", "Pelo_Pct", "Weighted_Last_5"
)
```

Mixed relays include explicit gender filtering and ID offset systems to prevent data conflicts:

```r
# From race-picks-mixed-relay.R:84-118
# Process legs with gender constraints
ladies_data <- ladies_data %>% mutate(ID = ID + 100000)  # Offset to avoid conflicts
combined_data <- bind_rows(men_data, ladies_data)
```

**Team Sprint Feature Selection**: Uses technique-adaptive selection based on race format:

```r
# From race-picks-team-sprint.R:190-225
# Classic Team Sprint
if(technique == "C") {
  technique_predictors <- c(
    "Sprint_Pelo_Pct",      # General sprint ability  
    "Distance_Pelo_Pct",    # General distance ability
    "Sprint_C_Pelo_Pct",    # Sprint classic specific
    "Classic_Pelo_Pct"      # General classic ability
  )
}

# Freestyle Team Sprint  
if(technique == "F") {
  technique_predictors <- c(
    "Sprint_Pelo_Pct",      # General sprint ability
    "Sprint_F_Pelo_Pct",    # Sprint freestyle specific  
    "Freestyle_Pelo_Pct"    # General freestyle ability
  )
}
```

Team sprints include sophisticated feature importance extraction for model interpretability:

```r
# From race-picks-team-sprint.R:350-380
safe_importance <- function(model) {
  tryCatch({
    if("xgb.Booster" %in% class(model)) {
      imp <- xgb.importance(model = model)
      return(imp$Feature[1:min(5, nrow(imp))])
    } else if("glm" %in% class(model)) {
      coef_summary <- summary(model)$coefficients
      sorted_coefs <- sort(abs(coef_summary[-1, 1]), decreasing = TRUE)
      return(names(sorted_coefs)[1:min(5, length(sorted_coefs))])
    }
  }, error = function(e) return(NULL))
}
```

**Feature Selection Comparison by Relay Type**:

| Feature Type | Mixed Relay | Team Sprint | Standard Relay |
|--------------|-------------|-------------|----------------|
| Gender-specific filtering | ✅ (by leg position) | ❌ | ❌ |
| Technique-adaptive | ❌ | ✅ (C vs F) | ❌ |
| Sprint-focused features | Leg 4 only | All legs | Leg 4 only |
| Feature importance analysis | ❌ | ✅ (XGBoost/GLM) | ❌ |
| Model selection | Rule-based | XGBoost vs GLM | Rule-based |

**Key Feature Selection Differences**:

1. **Mixed Relays** (`race-picks-mixed-relay.R`): Focus on gender-specific constraints with explicit leg filtering (`Sex == "F"` for legs 1&3, `Sex == "M"` for legs 2&4) and robust fallback mechanisms for sparse data
2. **Team Sprints** (`race-picks-team-sprint.R`): Emphasize technique-specific adaptation with dynamic feature selection based on race format (Classic vs Freestyle) and advanced feature importance analysis using XGBoost/GLM
3. **Standard Relays** (`race-picks-relay.R`): Use stable, position-based baseline approach with consistent features within each technique type (classic legs 1-2, freestyle legs 3-4)

This rule-based approach ensures that each leg's model uses the most relevant performance indicators for that specific position and technique requirement in relay races, with each relay type implementing specialized adaptations for their unique constraints and requirements.

###### Modeling

Relay points modeling uses GAM (Generalized Additive Models) to predict continuous point values for each team using the relay points system (200, 160, 120...). Unlike individual races, relay models focus on team-level predictions aggregated from individual leg performance.

**Points System Foundation**: All relay types use the standardized relay points system:

```r
# From all three relay files: points system definition
relay_points <- c(200, 160, 120, 100, 90, 80, 72, 64, 58, 52, 48, 44, 40, 36, 
                  32, 30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10, 8, 6, 4, 2)
```

**GAM Model Training**: Relay points are modeled using GAM with smooth terms for continuous prediction:

```r
# From race-picks-relay.R:402-549 & similar in other relay files
# Train GAM model for points prediction
points_model <- gam(Points ~ s(Distance_Pelo_Pct) + s(Classic_Pelo_Pct) + 
                           s(Pelo_Pct) + s(Weighted_Last_5),
                   data = training_data,
                   method = "REML")
```

**Team-Level Aggregation**: Individual leg predictions are combined to create team-level points predictions:

```r
# From race-picks-relay.R:860-962
# Calculate team predictions using leg importance weights
team_prediction <- 
  leg1_prediction * 0.2 +  # Classic leg 1
  leg2_prediction * 0.2 +  # Classic leg 2  
  leg3_prediction * 0.25 + # Freestyle leg 3
  leg4_prediction * 0.35   # Anchor leg 4 (highest weight)
```

**Leg-Specific Weight Distribution**: Different relay types use position-specific weighting:

**Standard Relay & Mixed Relay** (4 legs):
- Leg 1 (Classic): 20% weight
- Leg 2 (Classic): 20% weight  
- Leg 3 (Freestyle): 25% weight
- Leg 4 (Anchor): 35% weight (highest due to tactical importance)

**Team Sprint** (2 legs):
- Leg 1: 50% weight
- Leg 2: 50% weight (equal importance)

**Continuous Points Prediction**: Unlike binary probability models, points models predict actual point values:

```r
# From individual race GAM application to relay context
predicted_points <- predict(points_model, newdata = current_startlist, type = "response")
# Points are bounded between 0 and 200 (max relay points)
predicted_points <- pmax(pmin(predicted_points, 200), 0)
```

**Model Evaluation**: Points models are evaluated using continuous metrics rather than classification metrics:

```r
# From relay modeling evaluation
# Root Mean Square Error for continuous predictions
rmse <- sqrt(mean((actual_points - predicted_points)^2, na.rm = TRUE))
# Mean Absolute Error
mae <- mean(abs(actual_points - predicted_points), na.rm = TRUE)
```

**Key Differences from Individual Race Points Modeling**:

1. **Team Focus**: Relay models predict team performance rather than individual athlete performance
2. **Aggregation Strategy**: Individual leg predictions combined using importance weights
3. **Points Scale**: Uses relay-specific points system (200 max) rather than individual race points (100 max)
4. **Leg Dependencies**: Models account for relay-specific tactical considerations (anchor leg emphasis)
5. **Technique Integration**: Combines classic and freestyle performance within single team prediction

###### Adjustments

No systematic adjustments are applied to relay points predictions in any of the three relay formats. This represents a key difference from individual race predictions, which typically include period, altitude, and mass start adjustments.

**Absence of Adjustment Types**: All three relay files (`race-picks-relay.R`, `race-picks-mixed-relay.R`, `race-picks-team-sprint.R`) follow a direct points calculation approach without adjustments:

```r
# From all three relay files: direct expected points calculation
Expected_Points = Win_Prob * relay_points[1] + 
                 (Podium_Prob - Win_Prob) * mean(relay_points[2:3]) + 
                 (Top5_Prob - Podium_Prob) * mean(relay_points[4:5]) + 
                 (Top10_Prob - Top5_Prob) * mean(relay_points[6:10])
```

**Types of Adjustments NOT Applied**:

1. **No Period-Based Adjustments**: No seasonal, championship, or race-period modifications
2. **No Altitude-Based Adjustments**: No venue-specific or elevation adjustments  
3. **No Sequential Adjustments**: No iterative adjustment methodology like in individual races
4. **No Team-Level Adjustments**: No adjustments based on team chemistry, nation-specific factors, or team ranking
5. **No Leg-Specific Adjustments**: No technique-specific (classic vs freestyle) or position-specific adjustments

**Processing Order Without Adjustments**:

1. **Individual Leg Predictions** → Generate win, podium, top5, top10 probabilities for each leg
2. **Weighted Team Aggregation** → Combine leg probabilities using importance weights
3. **Direct Points Calculation** → Apply standard expected points formula
4. **Probability Normalization** → Apply constraints and monotonic ordering
5. **Final Output** → No further adjustments applied

**Leg Importance Weighting** (applied during aggregation, not as adjustments):

**Standard Relay & Mixed Relay**:
```r
# Leg importance weights emphasizing later legs
leg_weights <- c(0.2, 0.2, 0.25, 0.35)  # Classic 1, Classic 2, Freestyle 3, Anchor 4
```

**Team Sprint**:
```r  
# Equal weighting for both legs
leg_weights <- c(0.5, 0.5)  # Leg 1, Leg 2
```

**What IS Applied** (probability constraints, not adjustments):

```r
# Probability normalization and constraints only
# Ensures win ≤ podium ≤ top5 ≤ top10
# Caps individual probabilities at 1.0
# Ensures team probability totals sum correctly
```

**Reasoning for No Adjustments**:

1. **Team Dynamics**: Relay performance depends primarily on team composition and individual leg performance rather than external systematic factors
2. **Limited Historical Data**: Relay races occur less frequently than individual races, making adjustment parameter calibration challenging  
3. **Model Complexity**: Team events have multiple interacting variables that make simple adjustments less effective
4. **Direct Modeling Approach**: The leg-based modeling methodology may inherently capture relevant performance factors without need for post-hoc adjustments

This no-adjustment approach ensures that relay predictions rely entirely on the underlying leg-specific models and team aggregation logic, with the assumption that systematic effects are adequately captured during the training phase rather than through post-prediction adjustments.

##### Testing

###### Startlist Setup

Relay testing startlist setup involves loading team and individual startlists, validating FIS entries, and preparing current skier data with latest Elo scores and weighted previous points for prediction. The process differs between standard relays, mixed relays, and team sprints.

**Startlist Loading Process**: Each relay type loads both team and individual startlists from scraped data:

**Standard Relays**:
```r
# From race-picks-relay.R:578-596
# Function to load current relay startlists
load_relay_startlists <- function(gender) {
  gender_prefix <- ifelse(gender == "men", "men", "ladies")
  
  # Define file paths
  teams_path <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_relay_races_teams_%s.csv", gender_prefix)
  individuals_path <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_relay_races_individuals_%s.csv", gender_prefix)

  # Load data
  teams <- read.csv(teams_path, stringsAsFactors = FALSE)
  individuals <- read.csv(individuals_path, stringsAsFactors = FALSE)
```

**Mixed Relays**:
```r
# From race-picks-mixed-relay.R:380-395
# Function to load mixed relay startlists
load_mixed_relay_startlists <- function() {
  # Define file paths
  teams_path <- "~/ski/elo/python/ski/polars/relay/excel365/startlist_mixed_relay_races_teams.csv"
  individuals_path <- "~/ski/elo/python/ski/polars/relay/excel365/startlist_mixed_relay_races_individuals.csv"
  
  # Load data - with error handling
  teams <- tryCatch({
    read.csv(teams_path, stringsAsFactors = FALSE)
  }, error = function(e) {
    log_warn(paste("Could not read teams file:", e$message))
    return(data.frame())
  })
```

**Team Sprints**:
```r
# From race-picks-team-sprint.R:369-384
# Function to load team sprint startlists
load_team_sprint_startlists <- function(gender) {
  gender_prefix <- ifelse(gender == "men", "men", "ladies")
  
  # Define file paths
  teams_path <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_team_sprint_races_teams_%s.csv", gender_prefix)
  individuals_path <- sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_team_sprint_races_individuals_%s.csv", gender_prefix)
  
  # Load data
  teams <- read.csv(teams_path, stringsAsFactors = FALSE)
  individuals <- read.csv(individuals_path, stringsAsFactors = FALSE)
```

**FIS Startlist Validation**: The system checks whether valid FIS startlists are available to determine prediction strategy:

```r
# From race-picks-relay.R:598-603
# Function to check if startlist has valid FIS entries
has_valid_fis_entries <- function(individuals_df) {
  if ("In_FIS_List" %in% names(individuals_df)) {
    return(any(individuals_df$In_FIS_List, na.rm = TRUE))
  }
  return(FALSE)
}
```

**Current Skier Data Preparation**: Latest Elo values are retrieved and converted to Pelo percentages, with weighted previous points calculated separately for classic and freestyle:

```r
# From race-picks-relay.R:591-696
prepare_current_skiers <- function(chrono_data, current_season, gender = "men") {
  # Get all skiers from current season
  current_skiers <- chrono_gender %>%
    filter(Season == current_season) %>%
    select(Skier, ID, Nation, Sex) %>%
    distinct()
  
  # Get latest Elo values for these skiers
  latest_elo <- chrono_gender %>%
    filter(ID %in% current_skiers$ID) %>%
    group_by(ID) %>%
    arrange(desc(Season), desc(Race)) %>%
    dplyr::slice(1) %>%
    select(ID, ends_with("Elo")) %>%
    ungroup()
```

**Weighted Previous Points Calculation**: Separate calculations for classic and freestyle using last 5 races:

```r
# From race-picks-relay.R:615-635 (Classic)
classic_last5 <- classic_df %>%
  filter(ID %in% current_skiers$ID) %>%
  group_by(ID) %>%
  arrange(Date, Season, Race) %>%
  mutate(
    # Calculate weighted average of previous races including current
    Classic_Last_5 = sapply(row_number(), function(i) {
      prev_races <- Points[max(1, i-4):i]  # Include current row
      if (length(prev_races) > 0) {
        weights <- seq(1, length(prev_races))
        weighted.mean(prev_races, weights, na.rm = TRUE)
      } else {
        0
      }
    })
  )
```

**Pelo Percentage Conversion**: Elo values are converted to percentage format for model compatibility:

```r
# From race-picks-relay.R:676-689
# Calculate Pelo_Pct values directly from Elo columns
elo_cols <- names(current_df)[grepl("Elo$", names(current_df))]

if(length(elo_cols) > 0) {
  for(col in elo_cols) {
    max_val <- max(current_df[[col]], na.rm = TRUE)
    if(max_val > 0) {
      # Create Pelo_Pct name but calculate from Elo values
      pct_col <- paste0(gsub("Elo", "Pelo", col), "_Pct")
      current_df[[pct_col]] <- (current_df[[col]] / max_val) * 100
    }
  }
}
```

**Team Member Extraction**: Individual team members are extracted for each leg position using FIS startlist data:

```r
# From race-picks-relay.R:751-770
# Function to get leg predictions with FIS startlist
get_leg_predictions_with_startlist <- function(current_skiers, leg_models, startlist_individuals) {
  # Process each leg
  for(leg in 1:4) {
    # Filter the startlist to get skiers for this leg
    leg_skiers <- startlist_individuals %>%
      filter(Team_Position == leg) %>%
      select(ID, Skier, Nation, Team_Name)
    
    # Filter current_skiers to only include those in this leg's startlist
    leg_data <- current_skiers %>%
      filter(ID %in% leg_skiers$ID) %>%
      # Add Team information from startlist
      left_join(leg_skiers %>% select(ID, Team_Name), by = "ID")
  }
}
```

**Fallback Strategy**: When no valid FIS startlist is available, the system predicts for all current season skiers across all leg positions rather than using specific team compositions. This ensures predictions can still be generated even without official startlists.

###### Modeling

Relay points testing modeling involves generating individual leg predictions for each team member and aggregating them into team-level predictions using leg-specific importance weights. The process varies by relay type but follows the same fundamental approach.

**Individual Leg Prediction Process**: Each team member receives predictions for their specific leg position using the appropriate trained leg model:

```r
# From race-picks-relay.R:698-705
# Function to get leg predictions
get_leg_predictions <- function(leg_number, skier_data, leg_models) {
  # Select appropriate Last_5 column based on leg
  if(leg_number <= 2) {
    skier_data$Weighted_Last_5 <- skier_data$Classic_Last_5
  } else {
    skier_data$Weighted_Last_5 <- skier_data$Freestyle_Last_5
  }
```

**Safe Prediction Generation**: Individual predictions are generated using trained models with error handling and probability capping:

```r
# From race-picks-relay.R:779-799
# Get predictions safely
tryCatch({
  probs <- predict(model, newdata = data, type = type)
  if(is.data.frame(probs) && "Yes" %in% names(probs)) {
    return(pmin(probs[,"Yes"], 1))  # Cap at 1
  } else if(is.numeric(probs)) {
    return(pmin(probs, 1))  # Cap at 1
  } else {
    return(rep(0.25, nrow(data)))
  }
})

# Get predictions for each outcome
win_probs <- safe_predict(leg_models[[leg_number]]$win, pred_data)
podium_probs <- safe_predict(leg_models[[leg_number]]$podium, pred_data)
top5_probs <- safe_predict(leg_models[[leg_number]]$top5, pred_data)
top10_probs <- safe_predict(leg_models[[leg_number]]$top10, pred_data)
```

**Leg Importance Weight Calculation**: Each relay type uses different importance weights to emphasize position significance:

**Standard Relays**:
```r
# From race-picks-relay.R:860-868
calculate_leg_importance <- function(leg_models) {
  # Default weights with emphasis on later legs
  default_weights <- c(0.2, 0.2, 0.25, 0.35)  # Slight emphasis on later legs
  
  # For race day predictions, we just use default weights
  log_info("Using default leg importance weights for relay race day")
  
  return(default_weights)
}
```

**Mixed Relays**:
```r
# From race-picks-mixed-relay.R:545-550
calculate_leg_importance <- function(leg_models) {
  # Default weights for mixed relay
  default_weights <- c(0.2, 0.25, 0.25, 0.3)  # Slight emphasis on later legs
  return(default_weights)
}
```

**Team Sprints**:
```r
# From race-picks-team-sprint.R:636-643
calculate_leg_importance <- function(leg_models) {
  # Default weights for team sprint (equally important)
  default_weights <- c(0.5, 0.5)  # Equal weights for both legs in team sprint
  
  # For race day predictions, use default weights
  log_info("Using default leg importance weights for team sprint")
  
  return(default_weights)
}
```

**Team-Level Aggregation**: Individual leg predictions are combined using weighted averages to create team predictions:

```r
# From race-picks-relay.R:871-962
generate_team_predictions <- function(teams_df, individual_predictions, leg_models) {
  # Calculate leg importance weights
  leg_importance <- calculate_leg_importance(leg_models)
  
  # For each team, calculate probabilities based on their members
  for(i in 1:nrow(team_predictions)) {
    # Extract team members
    for(leg in 1:4) {
      member_col <- paste0("Member_", leg)
      if(member_col %in% names(teams_df)) {
        members[leg] <- teams_df[[member_col]][i]
      }
    }
    
    # Get predictions for each member from their specific leg
    for(leg in 1:4) {
      if(!is.na(members[leg]) && members[leg] != "") {
        skier_pred <- individual_predictions[[leg]] %>%
          filter(Skier == members[leg])
        
        if(nrow(skier_pred) > 0) {
          member_probs$Podium[leg] <- skier_pred$Podium_Prob[1]
          member_probs$Win[leg] <- skier_pred$Win_Prob[1]
          member_probs$Top5[leg] <- skier_pred$Top5_Prob[1]
          member_probs$Top10[leg] <- skier_pred$Top10_Prob[1]
        }
      }
    }
    
    # Calculate weighted probabilities using leg importance
    weighted_podium <- sum(member_probs$Podium * leg_importance)
    weighted_win <- sum(member_probs$Win * leg_importance)
    weighted_top5 <- sum(member_probs$Top5 * leg_importance)
    weighted_top10 <- sum(member_probs$Top10 * leg_importance)
  }
}
```

**Expected Points Calculation**: Team expected points are calculated from aggregated probabilities using the relay points system:

```r
# From race-picks-relay.R:952-958
# Calculate expected points based on probabilities
team_predictions$Expected_Points[i] <- 
  team_predictions$Win_Prob[i] * relay_points[1] +
  (team_predictions$Podium_Prob[i] - team_predictions$Win_Prob[i]) * mean(relay_points[2:3]) +
  (team_predictions$Top5_Prob[i] - team_predictions$Podium_Prob[i]) * mean(relay_points[4:5]) +
  (team_predictions$Top10_Prob[i] - team_predictions$Top5_Prob[i]) * mean(relay_points[6:10])
```

**Leg Position Weighting Strategy**: The importance weights reflect relay race dynamics:

1. **Standard & Mixed Relays (4 legs)**: Anchor leg (position 4) receives highest weight (0.35/0.3) due to tactical importance, with increasing emphasis toward later legs
2. **Team Sprints (2 legs)**: Equal weighting (0.5 each) reflects balanced importance of both positions in the shorter format
3. **Technique Consideration**: Classic legs (1-2) vs Freestyle legs (3-4) in 4-leg relays, with freestyle phases receiving slightly higher weights

**Probability Capping**: All individual and team probabilities are capped at 1.0 during aggregation to ensure realistic values, with fallback defaults (0.25) applied when prediction errors occur.

#### Probability

##### Training

###### Setup

Relay probability training setup involves creating leg-specific binary classification targets for win, podium, top5, and top10 outcomes using historical relay performance data. Training data is separated by leg position and technique, with gender-specific filtering for mixed relays.

**Outcome Target Creation**: Binary classification targets are created from historical relay leg placements for each outcome type:

**Standard Relays**:
```r
# From race-picks-relay.R:292-320
# Function to prepare leg-specific datasets
prepare_leg_data <- function(classic_legs, freestyle_legs) {
  # Create datasets for each leg
  leg_data <- list()
  
  # Legs 1 and 2 (Classic)
  for(i in 1:2) {
    leg_data[[i]] <- classic_legs %>%
      filter(Leg == i) %>%
      mutate(
        is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
        is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
        is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
        is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
      )
  }
  
  # Legs 3 and 4 (Freestyle)
  for(i in 3:4) {
    leg_data[[i]] <- freestyle_legs %>%
      filter(Leg == i) %>%
      mutate(
        is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
        is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
        is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
        is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
      )
  }
  return(leg_data)
}
```

**Mixed Relays** (with gender-specific leg filtering):
```r
# From race-picks-mixed-relay.R:296-318
# Leg 1 (usually female classic)
leg_data[[1]] <- classic_legs %>%
  filter(Leg == 1, Sex == "F") %>%
  mutate(
    is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
    is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
    is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
    is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
  )

# Leg 2 (usually male classic)
leg_data[[2]] <- classic_legs %>%
  filter(Leg == 2, Sex == "M") %>%
  mutate(
    is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
    is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
    is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
    is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
  )
```

**Team Sprints** (2 legs only):
```r
# From race-picks-team-sprint.R:285-304
# Leg 1
leg_data[[1]] <- team_sprints %>%
  filter(Leg == 1) %>%
  mutate(
    is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
    is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
    is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
    is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
  )

# Leg 2
leg_data[[2]] <- team_sprints %>%
  filter(Leg == 2) %>%
  mutate(
    is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes")),
    is_top5 = factor(ifelse(Place <= 5, "Yes", "No"), levels = c("No", "Yes")),
    is_top10 = factor(ifelse(Place <= 10, "Yes", "No"), levels = c("No", "Yes")),
    is_win = factor(ifelse(Place == 1, "Yes", "No"), levels = c("No", "Yes"))
  )
```

**Model Type Selection**: Training approach adapts to data size, using different algorithms based on available historical data:

```r
# From race-picks-relay.R:514-526
# Choose model type based on data size
method <- ifelse(nrow(leg_data[[leg]]) < 500, "glm", "xgbTree")

# Create formulas
podium_formula <- as.formula(paste("is_podium ~", paste(leg_predictors, collapse = "+")))
win_formula <- as.formula(paste("is_win ~", paste(leg_predictors, collapse = "+")))
top5_formula <- as.formula(paste("is_top5 ~", paste(leg_predictors, collapse = "+")))
top10_formula <- as.formula(paste("is_top10 ~", paste(leg_predictors, collapse = "+")))

# Train models
podium_model <- train_model_safe(podium_formula, leg_data[[leg]], method, "podium")
win_model <- train_model_safe(win_formula, leg_data[[leg]], method, "win")
top5_model <- train_model_safe(top5_formula, leg_data[[leg]], method, "top5")
top10_model <- train_model_safe(top10_formula, leg_data[[leg]], method, "top10")
```

**Safe Training with Fallbacks**: Model training includes error handling and fallback mechanisms:

```r
# From race-picks-relay.R:422-461
# Function to safely train a model with fallbacks
train_model_safe <- function(formula, data, method = "glm", target_name) {
  log_info(paste("Training", target_name, "model using", method))
  
  if (method == "xgbTree") {
    # Try XGBoost first
    tryCatch({
      xgb_grid <- expand.grid(
        nrounds = c(50, 100),
        max_depth = c(3, 4),
        eta = 0.03,
        gamma = 0.1,
        colsample_bytree = 0.8,
        min_child_weight = 1,
        subsample = 0.8
      )
```

**Model Storage Structure**: Each leg stores separate models for all four outcome types:

```r
# From race-picks-relay.R:528-536
# Store models
leg_models[[leg]] <- list(
  podium = podium_model,
  win = win_model,
  top5 = top5_model,
  top10 = top10_model,
  features = leg_predictors
)
```

**Key Training Setup Differences by Relay Type**:

1. **Standard Relays**: Technique-based leg separation (classic legs 1-2, freestyle legs 3-4) with all genders combined
2. **Mixed Relays**: Gender-specific filtering for each leg position (F-M-F-M pattern) combined with technique separation 
3. **Team Sprints**: Two-leg structure with technique determined by race format, no gender filtering within legs

**Leg-Specific Data Separation**: Training data is filtered by leg position to ensure models learn position-specific performance patterns, with technique-appropriate feature selection applied to each leg's dataset.

**Factor Level Standardization**: All binary outcomes use consistent factor levels ("No", "Yes") to ensure proper classification model training across all relay types and leg positions.

###### Feature Selection

Relay probability feature selection uses rule-based, leg-specific feature sets tailored to each leg's technique and tactical role. Features are selected based on position requirements rather than statistical optimization, ensuring relevant performance indicators for each leg type.

**Standard Relay Feature Selection**: Each leg uses technique-specific features based on classic/freestyle requirements:

**Classic Legs (1 & 2)**:
```r
# From race-picks-relay.R:328-336
if(leg <= 2) {
  # Classic legs (1 and 2)
  predictors <- c(
    grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE),
    grep("Classic.*Pelo_Pct$", base_cols, value = TRUE),
    "Distance_Pelo_Pct",
    "Pelo_Pct",
    "Weighted_Last_5"
  )
}
```

**Freestyle Leg 3**:
```r
# From race-picks-relay.R:337-345
else if (leg == 3) {
  # Freestyle legs (3)
  predictors <- c(
    grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE),
    grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE),
    "Distance_Pelo_Pct",
    "Pelo_Pct",
    "Weighted_Last_5"
  )
}
```

**Anchor Leg 4** (with sprint emphasis):
```r
# From race-picks-relay.R:346-356
else if (leg == 4) {
  # Anchor leg (often with sprint finish)
  predictors <- c(
    grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE),
    grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE),
    "Distance_Pelo_Pct",
    "Sprint_Pelo_Pct",
    "Pelo_Pct",
    "Weighted_Last_5"
  )
}
```

**Mixed Relay Feature Selection** (gender and technique specific):

**Leg 1 (Female Classic)**:
```r
# From race-picks-mixed-relay.R:146-156
if(leg == 1) {
  # Leg 1 (Female Classic)
  predictors <- c(
    grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE),
    grep("Classic.*Pelo_Pct$", base_cols, value = TRUE),
    "Distance_Pelo_Pct",
    "Pelo_Pct",
    "Weighted_Last_5"
  )
}
```

**Leg 2 (Male Classic)**:
```r
# From race-picks-mixed-relay.R:157-167
else if(leg == 2) {
  # Leg 2 (Male Classic)
  predictors <- c(
    grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE),
    grep("Classic.*Pelo_Pct$", base_cols, value = TRUE),
    "Distance_Pelo_Pct",
    "Pelo_Pct",
    "Weighted_Last_5"
  )
}
```

**Leg 3 (Female Freestyle)** and **Leg 4 (Male Freestyle)**: Similar patterns with freestyle-specific features and sprint emphasis for anchor positions.

**Team Sprint Feature Selection** (technique-adaptive):

**Classic Team Sprints**:
```r
# From race-picks-team-sprint.R:338-345
if(is_classic) {
  log_info(paste("Race is classic technique - using classic-specific predictors"))
  technique_predictors <- c(
    "Sprint_Pelo_Pct",
    "Distance_Pelo_Pct", # General sprint ability  
    "Sprint_C_Pelo_Pct",           # Sprint classic specific
    "Classic_Pelo_Pct"             # General classic ability
  )
}
```

**Freestyle Team Sprints**:
```r
# From race-picks-team-sprint.R:346-353
else if(is_freestyle) {
  log_info(paste("Race is freestyle technique - using freestyle-specific predictors"))
  technique_predictors <- c(
    "Sprint_Pelo_Pct",             # General sprint ability
    "Sprint_F_Pelo_Pct",           # Sprint freestyle specific  
    "Freestyle_Pelo_Pct"           # General freestyle ability
  )
}
```

**Base Predictors Common to All**:
```r
# From race-picks-team-sprint.R:331-335
# Define base predictors common to all
predictors <- c(
  "Pelo_Pct",
  "Weighted_Last_5"
)
```

**Feature Validation and Filtering**: Available features are validated against actual data columns:

```r
# From race-picks-relay.R:358-361
# Remove any NA or invalid column names
predictors <- predictors[predictors %in% names(leg_data[[leg]])]

return(predictors)
```

**Feature Importance Analysis**: Post-training feature importance is extracted for model interpretation:

```r
# From race-picks-relay.R:537-546
# Print feature importance safely
log_info(paste("Top features for Leg", leg, "podium prediction:"))
importance <- safe_importance(podium_model)
if(!is.null(importance) && nrow(importance) > 0) {
  # Sort by importance and print top 5
  importance <- importance[order(importance$Overall, decreasing = TRUE), , drop = FALSE]
  print(head(importance, 5))
} else {
  log_info("No feature importance available")
}
```

**Key Feature Selection Differences**:

| Feature Type | Standard Relay | Mixed Relay | Team Sprint |
|--------------|----------------|-------------|-------------|
| Leg-specific rules | ✅ (technique-based) | ✅ (technique + gender) | ✅ (technique-adaptive) |
| Sprint-focused features | Leg 4 only | Legs 3&4 | Both legs |
| Gender-specific filtering | ❌ | ✅ (F-M-F-M) | ❌ |
| Technique adaptation | Rule-based | Rule-based | Dynamic (race-dependent) |

**Rule-Based vs Statistical Selection**: Unlike points models that may use statistical feature selection, probability models use pre-defined rule-based feature sets that ensure tactical relevance for each leg position and relay type. This approach prioritizes domain knowledge over purely statistical optimization.

**Leg Position Tactical Considerations**:
1. **Classic Legs (1-2)**: Focus on distance classic performance indicators
2. **Freestyle Legs (3-4)**: Emphasize freestyle technique and distance performance
3. **Anchor Legs**: Additional sprint features for tactical finishing ability
4. **Team Sprint**: Dynamic technique-specific features based on race format

###### Modeling

Relay probability training modeling uses adaptive binary classification approaches with XGBoost or GLM algorithms selected based on data size. Each leg position trains separate models for win, podium, top5, and top10 outcomes using 5-fold cross-validation with robust error handling.

**Cross-Validation Configuration**: Standardized training control parameters across all relay types:

```r
# From race-picks-relay.R:404-411
# Set up control parameters for cross-validation
control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = defaultSummary,
  savePredictions = "final"
)
```

**Adaptive Algorithm Selection**: Model type selection based on available training data size:

```r
# From race-picks-relay.R:514 & similar in other relay files
# Choose model type based on data size
method <- ifelse(nrow(leg_data[[leg]]) < 500, "glm", "xgbTree")
```

**XGBoost Hyperparameter Configuration**: When data size permits, XGBoost is used with specific hyperparameters:

```r
# From race-picks-relay.R:420-428
# XGBoost hyperparameter grid
xgb_grid <- expand.grid(
  nrounds = c(50, 100),
  max_depth = c(3, 4),
  eta = 0.03,
  gamma = 0.1,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)
```

**Safe Training with Fallback Mechanisms**: Robust training process with multiple fallback options:

```r
# From race-picks-relay.R:414-489
# Function to safely train a model with fallbacks
train_model_safe <- function(formula, data, method = "glm", target_name) {
  log_info(paste("Training", target_name, "model using", method))
  
  if (method == "xgbTree") {
    # Try XGBoost first
    tryCatch({
      model <- train(
        formula,
        data = data,
        method = "xgbTree",
        trControl = control,
        tuneGrid = xgb_grid,
        verbose = FALSE
      )
      return(model)
    }, error = function(e) {
      log_warn(paste("XGBoost training failed:", e$message, "- falling back to glm"))
      # Fall back to GLM
      tryCatch({
        model <- train(
          formula,
          data = data,
          method = "glm",
          family = "binomial",
          trControl = control
        )
        return(model)
      }, error = function(e2) {
        log_warn(paste("GLM training also failed:", e2$message, "- using basic glm"))
        # Direct GLM as last resort
        model <- glm(formula, data = data, family = binomial)
        # Wrap in a caret-compatible structure
        result <- list(
          finalModel = model,
          xNames = attr(terms(model), "term.labels"),
          method = "glm.basic"
        )
        class(result) <- "train"
        return(result)
      })
    })
  }
}
```

**Outcome-Specific Model Training**: Four separate binary classification models per leg:

```r
# From race-picks-relay.R:516-526
# Create formulas for different outcomes
podium_formula <- as.formula(paste("is_podium ~", paste(leg_predictors, collapse = "+")))
win_formula <- as.formula(paste("is_win ~", paste(leg_predictors, collapse = "+")))
top5_formula <- as.formula(paste("is_top5 ~", paste(leg_predictors, collapse = "+")))
top10_formula <- as.formula(paste("is_top10 ~", paste(leg_predictors, collapse = "+")))

# Train models
podium_model <- train_model_safe(podium_formula, leg_data[[leg]], method, "podium")
win_model <- train_model_safe(win_formula, leg_data[[leg]], method, "win")
top5_model <- train_model_safe(top5_formula, leg_data[[leg]], method, "top5")
top10_model <- train_model_safe(top10_formula, leg_data[[leg]], method, "top10")
```

**Model Storage and Organization**: Each leg stores all four outcome models with metadata:

```r
# From race-picks-relay.R:528-536
# Store models
leg_models[[leg]] <- list(
  podium = podium_model,
  win = win_model,
  top5 = top5_model,
  top10 = top10_model,
  features = leg_predictors
)
```

**Mixed Relay Modeling Adaptations**: Simplified approach due to data constraints:

```r
# From race-picks-mixed-relay.R:417-430
# Mixed relays typically use GLM due to smaller datasets
podium_model <- train_model_safe(podium_formula, leg_data[[leg]], "glm", "podium")
win_model <- train_model_safe(win_formula, leg_data[[leg]], "glm", "win")
top5_model <- train_model_safe(top5_formula, leg_data[[leg]], "glm", "top5")
top10_model <- train_model_safe(top10_formula, leg_data[[leg]], "glm", "top10")
```

**Team Sprint Modeling Strategy**: Technique-specific modeling with dynamic data size assessment:

```r
# From race-picks-team-sprint.R:396-406
# Choose model type based on data size
method <- ifelse(nrow(leg_data[[leg]]) < 500, "glm", "xgbTree")

# Create formulas for different outcomes
podium_formula <- as.formula(paste("is_podium ~", paste(leg_predictors, collapse = "+")))
win_formula <- as.formula(paste("is_win ~", paste(leg_predictors, collapse = "+")))
top5_formula <- as.formula(paste("is_top5 ~", paste(leg_predictors, collapse = "+")))
top10_formula <- as.formula(paste("is_top10 ~", paste(leg_predictors, collapse = "+")))

# Train models
podium_model <- train_model_safe(podium_formula, leg_data[[leg]], method, "podium")
```

**Training Process Error Handling**: Comprehensive error handling with logging and graceful degradation:

1. **Primary Method**: XGBoost with hyperparameter tuning (if data size > 500)
2. **First Fallback**: Caret GLM with cross-validation 
3. **Final Fallback**: Basic GLM with manual caret wrapper

**Feature Importance Extraction**: Post-training analysis for model interpretability:

```r
# From race-picks-relay.R:537-546
# Print feature importance safely
log_info(paste("Top features for Leg", leg, "podium prediction:"))
importance <- safe_importance(podium_model)
if(!is.null(importance) && nrow(importance) > 0) {
  # Sort by importance and print top 5
  importance <- importance[order(importance$Overall, decreasing = TRUE), , drop = FALSE]
  print(head(importance, 5))
} else {
  log_info("No feature importance available")
}
```

**Model Training Differences by Relay Type**:

| Aspect | Standard Relay | Mixed Relay | Team Sprint |
|--------|----------------|-------------|-------------|
| Algorithm selection | Data-adaptive (XGB/GLM) | Primarily GLM | Data-adaptive (XGB/GLM) |
| Data size threshold | 500 rows | N/A (GLM default) | 500 rows |
| Cross-validation | 5-fold CV | 5-fold CV | 5-fold CV |
| Fallback levels | 3 levels | 2 levels | 3 levels |
| Hyperparameter tuning | XGBoost grid search | None | XGBoost grid search |

**Binary Classification Setup**: Each outcome uses consistent factor levels with "Yes"/"No" encoding, ensuring proper probability predictions for team aggregation during testing phase.

###### Adjustments

No systematic adjustments are applied during relay probability training. Unlike individual race prediction models, relay probability models rely solely on the training data and feature engineering without post-training calibration or systematic modification.

**Absence of Training-Time Adjustments**: Relay probability models do not employ any of the adjustment mechanisms commonly used in individual race predictions:

**No Period-Based Adjustments**: 
- No seasonal weighting modifications
- No championship vs. regular race distinctions
- No time-of-season calibration factors

**No Venue-Based Adjustments**:
- No altitude-specific modifications  
- No course-difficulty calibration
- No venue-history adjustments

**No Format-Specific Adjustments**:
- No mass start vs. interval start modifications
- No distance-specific calibration beyond feature selection
- No technique-specific post-training adjustments

**Training Data Preprocessing Only**: The only modifications applied are standard data preprocessing steps during training data preparation:

**Missing Value Imputation**:
```r
# From race-picks-relay.R:257-264
mutate(
  Weighted_Last_5 = ifelse(
    is.na(Weighted_Last_5),
    quantile(Weighted_Last_5, 0.25, na.rm = TRUE),
    Weighted_Last_5
  )
)
```

**Chronological Data Filling**:
```r
# From race-picks-relay.R:253-255
group_by(ID) %>%
arrange(Date, Season, Race, desc(Distance)) %>%  # Use Date for chronological order
fill(Weighted_Last_5, .direction = "down") %>%
```

**Season Filtering**:
```r
# From race-picks-relay.R:255 & similar in other files
filter(Distance == "Rel", Season > min_season) %>%  # Apply season filter AFTER filling
```

**Rationale for No Training Adjustments**: Several factors contribute to the no-adjustment approach in relay probability training:

1. **Limited Historical Data**: Relay races occur less frequently than individual races, making statistical adjustment parameter estimation challenging

2. **Team Dynamics Complexity**: Relay performance depends heavily on team composition and tactical factors that are difficult to adjust for systematically

3. **Leg-Specific Modeling**: The leg-based approach may inherently capture systematic effects through position-specific feature selection and training

4. **Model Complexity**: The multi-leg, multi-outcome structure already provides substantial model complexity without additional adjustment layers

**Contrast with Individual Race Approaches**: Individual race models typically employ multiple adjustment types:
- Sequential adjustment methodology (period → altitude → mass start)
- T-test based significance testing for adjustments
- Iterative adjustment application to prevent interaction effects

**Post-Training Constraints vs. Adjustments**: While no training adjustments are applied, the testing phase does apply probability constraints and normalization (covered in Testing sections). These constraints ensure mathematical coherence (win ≤ podium ≤ top5 ≤ top10) but do not modify the underlying model predictions based on race characteristics.

**Consistency Across Relay Types**: All three relay types (standard, mixed, team sprint) follow the same no-adjustment approach during training, ensuring consistent methodology across relay formats.

##### Testing

###### Startlist Setup

Relay probability testing uses identical team-level startlist preparation as points prediction, involving comprehensive startlist loading, FIS validation, current skier data preparation, and team member extraction across all relay types.

**Identical Process to Points Testing**: The probability testing startlist setup follows the exact same methodology documented in the Points Testing Startlist Setup section:

1. **Startlist Loading**: Gender-specific and relay-type-specific CSV file loading
2. **FIS Validation**: Checking for valid FIS entries to determine prediction strategy  
3. **Current Skier Preparation**: Latest Elo retrieval, Pelo percentage conversion, and weighted points calculation
4. **Team Member Extraction**: Individual leg member identification and data preparation

**Shared Implementation**: All three relay types use the same startlist preparation functions:

- `load_relay_startlists()` for standard relays
- `load_mixed_relay_startlists()` for mixed relays  
- `load_team_sprint_startlists()` for team sprints
- `prepare_current_skiers()` for individual skier data preparation
- `has_valid_fis_entries()` for FIS validation

**Fallback Strategy Consistency**: When no valid FIS startlist is available, both points and probability predictions use the same fallback approach of predicting for all current season skiers across all leg positions rather than specific team compositions.

**Data Preparation Uniformity**: The same Elo-to-Pelo conversion, weighted last 5 calculation (classic vs freestyle), and missing value imputation procedures are applied for both prediction types, ensuring consistent input data regardless of the modeling approach (points vs probability).

For detailed code evidence and implementation specifics, refer to the comprehensive Relay Points Testing Startlist Setup section above, as the probability testing uses identical processes.

###### Modeling

Relay probability testing modeling generates individual leg probability predictions using trained binary classification models, then aggregates them into team-level probabilities using leg importance weighting. The process involves outcome-specific predictions, team member extraction, and weighted probability aggregation.

**Individual Leg Probability Generation**: Each team member receives probability predictions for all four outcomes using their leg-specific trained models:

```r
# From race-picks-relay.R:795-798
# Get predictions for each outcome using leg-specific models
win_probs <- safe_predict(leg_models[[leg_number]]$win, pred_data)
podium_probs <- safe_predict(leg_models[[leg_number]]$podium, pred_data)
top5_probs <- safe_predict(leg_models[[leg_number]]$top5, pred_data)
top10_probs <- safe_predict(leg_models[[leg_number]]$top10, pred_data)
```

**Prediction Size Validation**: Robust checking ensures prediction vectors match input data dimensions:

```r
# From race-picks-relay.R:801-816
# Make sure all vectors are the same length as the data
if(length(win_probs) != nrow(pred_data)) {
  log_warn(paste("Size mismatch in win_probs:", length(win_probs), "vs", nrow(pred_data)))
  win_probs <- rep(win_probs[1], nrow(pred_data))
}
if(length(podium_probs) != nrow(pred_data)) {
  log_warn(paste("Size mismatch in podium_probs:", length(podium_probs), "vs", nrow(pred_data)))
  podium_probs <- rep(podium_probs[1], nrow(pred_data))
}
```

**Individual Prediction Results Structure**: Leg predictions are organized with comprehensive metadata:

```r
# From race-picks-relay.R:819-830
# Create results dataframe
return(data.frame(
  ID = pred_data$ID,
  Skier = pred_data$Skier,
  Nation = pred_data$Nation,
  Sex = pred_data$Sex,
  Leg = leg_number,
  Win_Prob = win_probs,
  Podium_Prob = podium_probs,
  Top5_Prob = top5_probs,
  Top10_Prob = top10_probs
))
```

**Team Prediction Framework**: Team-level predictions are initialized and structured to accommodate all relay types:

```r
# From race-picks-relay.R:871-891
generate_team_predictions <- function(teams_df, individual_predictions, leg_models) {
  # Find all the exact Member_N columns
  member_cols <- c()
  for(i in 1:4) {
    name_col <- paste0("Member_", i)
    if(name_col %in% names(teams_df)) {
      member_cols <- c(member_cols, name_col)
    }
  }
  
  # Initialize results dataframe with the exact member columns
  team_predictions <- teams_df %>%
    select(Team_Name, Nation, Team_Rank, Price, Is_Present, all_of(member_cols)) %>%
    mutate(
      Podium_Prob = 0,
      Win_Prob = 0,
      Top5_Prob = 0,
      Top10_Prob = 0,
      Expected_Points = 0
    )
}
```

**Leg Importance Weight Application**: Team aggregation uses position-specific importance weights:

```r
# From race-picks-relay.R:893-897
# Calculate leg importance weights
leg_importance <- calculate_leg_importance(leg_models)

log_info(paste("Leg importance weights:", 
               paste(sprintf("Leg %d: %.2f", 1:4, leg_importance), collapse=", ")))
```

**Team Member Probability Extraction**: Individual leg predictions are extracted for each team member:

```r
# From race-picks-relay.R:900-918
# For each team, calculate probabilities based on their members
for(i in 1:nrow(team_predictions)) {
  # Extract team members
  members <- c()
  for(leg in 1:4) {
    member_col <- paste0("Member_", leg)
    if(member_col %in% names(teams_df)) {
      members[leg] <- teams_df[[member_col]][i]
    }
  }
  
  # Get predictions for each member
  member_probs <- list(
    Podium = numeric(4),
    Win = numeric(4),
    Top5 = numeric(4),
    Top10 = numeric(4)
  )
}
```

**Member Probability Lookup**: Team member predictions are retrieved from leg-specific prediction datasets:

```r
# From race-picks-relay.R:920-936
for(leg in 1:4) {
  if(!is.na(members[leg]) && members[leg] != "") {
    # Access the specific leg's dataframe and then filter
    if(!is.null(individual_predictions[[leg]])) {
      skier_pred <- individual_predictions[[leg]] %>%
        filter(Skier == members[leg])
      
      if(nrow(skier_pred) > 0) {
        # Store probabilities safely
        member_probs$Podium[leg] <- skier_pred$Podium_Prob[1]
        member_probs$Win[leg] <- skier_pred$Win_Prob[1]
        member_probs$Top5[leg] <- skier_pred$Top5_Prob[1]
        member_probs$Top10[leg] <- skier_pred$Top10_Prob[1]
      }
    }
  }
}
```

**Weighted Team Probability Calculation**: Individual leg probabilities are aggregated using importance weights:

```r
# From race-picks-relay.R:938-950
# Calculate weighted probabilities
if(sum(member_probs$Podium > 0) > 0) {  # Only if we have any valid probabilities
  # Calculate weighted probabilities using leg importance
  weighted_podium <- sum(member_probs$Podium * leg_importance)
  weighted_win <- sum(member_probs$Win * leg_importance)
  weighted_top5 <- sum(member_probs$Top5 * leg_importance)
  weighted_top10 <- sum(member_probs$Top10 * leg_importance)
  
  # Cap at 1
  team_predictions$Podium_Prob[i] <- min(weighted_podium, 1)
  team_predictions$Win_Prob[i] <- min(weighted_win, 1)
  team_predictions$Top5_Prob[i] <- min(weighted_top5, 1)
  team_predictions$Top10_Prob[i] <- min(weighted_top10, 1)
}
```

**Expected Points Derivation**: Team expected points are calculated from probability aggregations:

```r
# From race-picks-relay.R:952-958
# Calculate expected points based on probabilities
team_predictions$Expected_Points[i] <- 
  team_predictions$Win_Prob[i] * relay_points[1] +
  (team_predictions$Podium_Prob[i] - team_predictions$Win_Prob[i]) * mean(relay_points[2:3]) +
  (team_predictions$Top5_Prob[i] - team_predictions$Podium_Prob[i]) * mean(relay_points[4:5]) +
  (team_predictions$Top10_Prob[i] - team_predictions$Top5_Prob[i]) * mean(relay_points[6:10])
```

**Key Differences from Points Testing Modeling**:

1. **Model Type**: Uses trained binary classification models (win/podium/top5/top10) rather than points regression models
2. **Prediction Output**: Generates probabilities (0-1) rather than continuous point predictions  
3. **Aggregation Method**: Weighted probability averaging rather than weighted points averaging
4. **Expected Points**: Derived from probabilities using relay points system rather than direct prediction

**Relay Type Consistency**: All three relay types (standard, mixed, team sprint) use the same team aggregation methodology, with only leg importance weights varying by format (4-leg vs 2-leg structures).

###### Adjustments

No systematic adjustments are applied during relay probability testing. Unlike individual race probability predictions, relay probability testing relies entirely on the trained models and team aggregation without post-prediction modifications based on race characteristics or external factors.

**Absence of Testing-Time Adjustments**: Relay probability testing does not employ any adjustment mechanisms that modify the raw model predictions:

**No Race-Specific Adjustments**:
- No period-based probability modifications (championship vs regular season)
- No venue-specific calibration (altitude, weather, course conditions)
- No format-specific adjustments (relay distance variations, team size differences)

**No Team-Composition Adjustments**:
- No nation-specific team chemistry modifications
- No experience-based team adjustments (veteran vs rookie team composition)
- No ranking-based team performance calibration

**No Historical Performance Adjustments**:
- No recent team form adjustments
- No head-to-head team performance modifications
- No seasonal trend adjustments for relay-specific performance

**Probability Constraints vs. Adjustments**: While no systematic adjustments are applied, probability testing does implement mathematical constraints that ensure logical coherence:

**Capping at Individual Level**:
```r
# From race-picks-relay.R:946-950 (during aggregation)
# Cap at 1
team_predictions$Podium_Prob[i] <- min(weighted_podium, 1)
team_predictions$Win_Prob[i] <- min(weighted_win, 1)
team_predictions$Top5_Prob[i] <- min(weighted_top5, 1)
team_predictions$Top10_Prob[i] <- min(weighted_top10, 1)
```

**These constraints are mathematical bounds, not systematic adjustments** - they ensure probabilities remain within valid ranges (0-1) but do not modify predictions based on race-specific factors.

**Normalization and Monotonic Constraints** (detailed in following section): While probability normalization and monotonic ordering are applied post-aggregation, these are mathematical consistency requirements rather than systematic adjustments based on external factors.

**Rationale for No Testing Adjustments**: Several factors support the no-adjustment approach during relay probability testing:

1. **Model Completeness**: The leg-specific binary classification models with comprehensive feature sets may already capture relevant systematic effects

2. **Team Complexity**: Relay performance depends on complex team interactions that are difficult to adjust for systematically without overfitting

3. **Limited Adjustment Precedent**: Unlike individual races where adjustment patterns are well-established, relay-specific adjustment patterns are less clear

4. **Probability Aggregation Robustness**: The weighted aggregation of individual leg probabilities may provide inherent stability that reduces need for adjustments

5. **Data Sparsity**: Lower frequency of relay races compared to individual races makes adjustment parameter estimation less reliable

**Contrast with Individual Race Probability Testing**: Individual race models typically employ:
- Period adjustments based on race timing and importance
- Venue adjustments for specific course characteristics  
- Format adjustments for mass start vs interval start races
- Sequential adjustment application with significance testing

**Consistency with Training Approach**: The no-adjustment philosophy in testing mirrors the training approach, maintaining methodological consistency where both training and testing rely on model architecture and feature engineering rather than systematic modifications.

**Focus on Model Quality**: The absence of testing adjustments places emphasis on ensuring high-quality training data, appropriate feature selection, and robust model architecture rather than post-hoc correction mechanisms.

#### Normalization and Monotonic Constraints

Team probability predictions are normalized so that win probabilities sum to 1, podium probabilities sum to 3, top5 probabilities sum to 5, and top10 probabilities sum to 10 (`race-picks-relay.R:987-1016`). 

Monotonic constraints are applied to ensure win ≤ podium ≤ top5 ≤ top10 for each team, followed by re-normalization to maintain target sums (`race-picks-relay.R:1018-1059`). All probabilities are capped at 1.0 to ensure realistic values.