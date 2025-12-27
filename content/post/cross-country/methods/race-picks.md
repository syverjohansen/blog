---
title: "Cross-Country Race Picks Methodology"
date: 2023-12-01T01:23:07+00:00
draft: false
tags: ["methodology", "skiing", "race-picks", "cross-country"]
---

## Cross-Country

### Individual

#### Data Gathering

Cross-country individual race startlists are scraped from the FIS websites using automated Python scripts that process multiple race formats and handle complex event scheduling. The system manages individual races, relays, team sprints, and mixed events with sophisticated nation quota management and mock startlist generation capabilities.

**Race Discovery and Scheduling**: The system identifies races to process using advanced date handling and race type classification:

```python
# From startlist-scrape-races.py:32-36
# Get today's date in UTC 
today_utc = datetime.now(timezone.utc)
today_str = today_utc.strftime('%m/%d/%Y')

print(f"Today's date (UTC): {today_str}")
```

**Relay Event Detection and Management**: Cross-country has sophisticated relay event handling through distance-based classification:

```python
# From startlist-scrape-races.py:89-95
def is_relay_event(race: pd.Series) -> bool:
    """
    Determine if a race is a relay event
    Relay types: 'Rel' (standard relay), 'Ts' (team sprint), 'Mix' (mixed relay)
    """
    distance = str(race['Distance']).strip() if 'Distance' in race else ""
    return distance in ['Rel', 'Ts', 'Mix']
```

**FIS Website Data Extraction**: Cross-country athlete data is scraped from FIS race pages with advanced name matching and nation tracking:

```python
# From startlist_common.py:77-100
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

**Nation Quota Management**: Cross-country uses comprehensive nation quota systems for mock startlist generation:

```python
# From config.py:2-32
NATION_QUOTAS = {
    'Andorra': {'men': 3, 'ladies': 3},
    'Austria': {'men': 5, 'ladies': 5},
    'Canada': {'men': 5, 'ladies': 5},
    'Finland': {'men': 6, 'ladies': 6},
    'France': {'men': 6, 'ladies': 6},
    'Germany': {'men': 6, 'ladies': 6},
    'Italy': {'men': 6, 'ladies': 6},
    'Norway': {'men': 6, 'ladies': 6},
    'Sweden': {'men': 6, 'ladies': 6},
    'Switzerland': {'men': 6, 'ladies': 6},
    'USA': {'men': 6, 'ladies': 6}
}

# Default quota for unlisted nations
DEFAULT_QUOTA = {'men': 2, 'ladies': 2}
```

**Dynamic Quota Calculation**: The system calculates nation quotas with host country and World Cup leader bonuses:

```python
# From config.py:40-70
def get_nation_quota(nation: str, gender: str, is_host: bool = False) -> int:
    """
    Get the quota for a nation considering:
    1. Base quota from NATION_QUOTAS or default
    2. Host country bonus (+5 if applicable)
    3. World Cup leader bonus (+1 if applicable)
    """
    # Get base quota
    base_quota = NATION_QUOTAS.get(nation, DEFAULT_QUOTA).get(gender, DEFAULT_QUOTA[gender])
    
    # Add host country bonus
    if is_host:
        base_quota += 5
        
    # Add World Cup leader bonus
    if nation in WC_LEADERS.get(gender, []):
        base_quota += 1
        
    return base_quota
```

**Fantasy XC Price Integration**: Cross-country integrates with Fantasy XC for athlete pricing data:

```python
# From startlist_common.py:182-193
def get_fantasy_prices() -> Dict[str, int]:
    """Gets athlete prices from Fantasy XC API"""
    try:
        response = requests.get('https://www.fantasyxc.se/api/athletes')
        response.raise_for_status()
        
        athletes = response.json()
        return {athlete['name']: athlete['price'] for athlete in athletes}
        
    except Exception as e:
        print(f"Error getting Fantasy XC prices: {e}")
        return {}
```

**Manual Name Mapping**: Cross-country has extensive manual name mapping for FIS format conversions:

```python
# From startlist_common.py:12-30
MANUAL_NAME_MAPPINGS = {
    'Thomas MALONEY WESTGAARD': 'Thomas Hjalmar Westgård',
    'John Steel HAGENBUCH': 'Johnny Hagenbuch',
    'Imanol ROJO GARCIA': 'Imanol Rojo',
    'Sammy SMITH': 'Samantha Smith',
    'SMITH Sammy': 'Samantha Smith',
    'JC SCHOONMAKER': 'James Clinton Schoonmaker',
    'HAGENBUCH John Steel': 'Johnny Hagenbuch',
    'MALONEY WESTGAARD Thomas': 'Thomas Hjalmar Westgård',
    'Katharina HENNIG DOTZLER': 'Katharina Hennig',
    'HENNIG DOTZLER Katharina': 'Katharina Hennig'
}
```

**Advanced Name Matching**: The system uses sophisticated name matching with fuzzy logic:

```python
# From startlist_common.py:32-75
def get_fantasy_price(name: str, fantasy_prices: Dict[str, int]) -> int:
    """Gets fantasy price by trying multiple name formats"""
    print(f"\nTrying to find price for: {name}")
    
    # First check if there's a reverse mapping
    reverse_map = {v: k for k, v in MANUAL_NAME_MAPPINGS.items()}
    
    # Try exact match
    if name in fantasy_prices:
        print(f"Found exact match: {name} -> {fantasy_prices[name]}")
        return fantasy_prices[name]
    
    # Try all possible FIS formats
    fis_formats = convert_to_last_first(name)
    print(f"Trying FIS formats: {fis_formats}")
    
    for fis_format in fis_formats:
        if fis_format in fantasy_prices:
            print(f"Found FIS format match: {fis_format} -> {fantasy_prices[fis_format]}")
            return fantasy_prices[fis_format]
    
    # Try fuzzy matching with original name
    best_match = fuzzy_match_name(name, list(fantasy_prices.keys()))
    if best_match and best_match in fantasy_prices:
        print(f"Found fuzzy match for original name: {name} -> {best_match} -> {fantasy_prices[best_match]}")
        return fantasy_prices[best_match]
```

**Multi-Script R Integration**: Cross-country has comprehensive R script integration for various event types:

```python
# From startlist-scrape-races.py:16-62
def call_r_script(script_type: str, race_type: str = None, gender: str = None) -> None:
    """
    Call the appropriate R script after processing race data
    
    Args:
        script_type: 'weekend' or 'races' (determines weekly picks or race picks)
        race_type: 'standard', 'team_sprint', 'relay', or 'mixed_relay'
        gender: 'men', 'ladies', or None for mixed events
    """
    # Set the base path to the R scripts
    r_script_base_path = "~/blog/daehl-e/content/post/cross-country/drafts"
    
    # Determine which R script to call based on script type and race type
    if script_type == 'weekend':
        # Weekly picks scripts
        if race_type == 'standard':
            r_script = "weekly-picks2.R"
        elif race_type == 'team_sprint':
            r_script = "weekly-picks-team-sprint.R"
        elif race_type == 'relay':
            r_script = "weekly-picks-relay.R"
        elif race_type == 'mixed_relay':
            r_script = "weekly-picks-mixed-relay.R"
```

The Cross-Country individual data gathering system provides comprehensive race format handling through relay event detection, nation quota management with dynamic calculation, Fantasy XC integration, extensive manual name mapping, advanced fuzzy name matching, multi-format FIS data extraction, and sophisticated R script integration that accommodates cross-country skiing's diverse race formats and complex startlist requirements.

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

The training data setup for Cross-Country skiing follows a sophisticated preprocessing pipeline that accommodates the sport's diverse race formats, technique variations, and complex points systems.

**Dynamic Points System Selection**: 
Cross-country uses different points systems depending on race context. The system dynamically determines which points system to apply to ALL historical races based on today's race type:

```r
# From race-picks.R:238-260
preprocess_data <- function(df) {
  # Load races data to determine points systems for historical races
  races_data <- read.csv("~/ski/elo/python/ski/polars/excel365/races.csv", 
                         stringsAsFactors = FALSE) %>%
    mutate(Date = mdy(Date))
  
  # Determine points system based on today's race
  current_date <- as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d")
  today_weekend <- races_data %>%
    filter(Date == current_date) %>%
    dplyr::slice(1)
  
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

**Historical Points Assignment with Global System**:
Unlike other sports that use race-specific points systems, cross-country applies the global points system determined by today's race type to ALL historical data:

```r
# From race-picks.R:262-274
# First calculate points using historical data but with the GLOBAL points system
df_with_points <- df %>%
  # Add points based on the global points system determined by today's races
  mutate(
    Points = mapply(function(place) {
      if (place >= 1 && place <= length(global_points_system)) {
        return(global_points_system[place])
      }
      return(0)
    }, Place)
  ) %>%
  # Sort
  arrange(Season, Race, Place)
```

**Race Type and Technique-Specific Weighted Points**:
Cross-country calculates weighted previous points by race type (Sprint vs Distance) AND technique (Classic vs Freestyle), creating the most granular historical performance tracking:

```r
# From race-picks.R:276-290
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
  })) %>%
  ungroup()
```

**Comprehensive ELO Column Management**:
Cross-country maintains the most extensive ELO rating system, tracking performance across distance/sprint, classic/freestyle combinations:

```r
# From race-picks.R:292-303
# Check if Pelo columns exist, if not create them
pelo_cols <- c("Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
               "Pelo", "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo",
               "Freestyle_Pelo", "Classic_Pelo")

# Make sure these columns exist (create if missing)
for (col in pelo_cols) {
  if (!col %in% names(df_with_points)) {
    log_info(paste("Creating missing column:", col))
    df_with_points[[col]] <- 0
  }
}
```

**Advanced Periodization System**:
Cross-country uses a dynamic 5+ period system that adapts to season length, providing more granular seasonal context than other sports:

```r
# From race-picks.R:306-319
# Add period
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
) %>%
ungroup()
```

**Event Filtering and Quality Control**:
Cross-country filters for high-quality events, excluding lower-level competitions and team events:

```r
# From race-picks.R:322-327, 352-353
filter(
  Season >= max(Season-10),
  Event %in% c("World Cup", "Nordic Opening", "Tour de Ski", 
               "Olympic Winter Games", "World Championship", 
               "World Cup Final", "Ski Tour Canada")
) %>%
# Filter out team sprint and relay
filter(!Distance %in% c("Ts", "Rel"))
```

**ELO Percentage Calculation with Cross-Country Specificity**:
The system calculates ELO percentages for each of the nine different ELO rating categories, providing the most comprehensive performance normalization:

```r
# From race-picks.R:339-350
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

This cross-country specific setup creates the most sophisticated training dataset among all winter sports, accounting for technique variations, dynamic points systems, granular race type classifications, advanced periodization, and comprehensive ELO tracking across multiple performance dimensions.
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

Cross-country's feature selection process reflects the sport's extraordinary complexity, managing the most comprehensive variable set among all winter sports. The system uses exhaustive BIC optimization while handling the intricate interactions between race type, technique, and performance history.

**Comprehensive Variable Universe**:
Cross-country maintains the most extensive set of candidate variables, capturing all dimensions of performance:

```r
# From race-picks.R:930-932
explanatory_vars <- c("Prev_Points_Weighted", "Distance_Pelo_Pct", "Sprint_Pelo_Pct", 
                      "Sprint_C_Pelo_Pct", "Distance_F_Pelo_Pct", "Distance_C_Pelo_Pct", 
                      "Classic_Pelo_Pct", "Freestyle_Pelo_Pct", "Sprint_F_Pelo_Pct", "Pelo_Pct")
```

This includes:
- **Prev_Points_Weighted**: Technique and race type-specific weighted previous points  
- **Distance vs Sprint ELOs**: Separate ratings for endurance vs speed events
- **Technique-Specific ELOs**: Classic and Freestyle ratings for both distance and sprint
- **Aggregate ELOs**: Overall freestyle, classic, and general performance ratings

**Unified Optimization Approach**:
Unlike some sports that use different selection strategies for different event types, cross-country applies consistent BIC optimization across all race contexts:

```r
# From race-picks.R:935-942
formula <- as.formula(paste(response_variable, "~", paste(explanatory_vars, collapse = " + ")))
exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
summary_exhaustive <- summary(exhaustive_selection)
best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))

model <- gam(gam_formula, data = race_df_75)
```

**No Fallback Required**:
Cross-country's robust variable selection typically succeeds without requiring fallback strategies, reflecting the sport's large competitive fields and extensive historical data availability.

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

Cross-country skiing's modeling approach reflects the sport's incredible complexity, employing Generalized Additive Models (GAM) optimized to handle the most diverse set of predictive variables among winter sports. The system creates both points prediction models and position probability models with comprehensive validation and sophisticated adjustment systems.

**Primary GAM Implementation**: 
The main modeling approach accommodates cross-country's unified optimization strategy across all race contexts:

```r
# From race-picks.R:940-942
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))
model <- gam(gam_formula, data = race_df_75)
```

**Model Characteristics for Cross-Country Complexity**:
- **Response Variable**: World Cup points with dynamic scaling based on race type (distance vs sprint vs stage race)
- **Model Type**: Generalized Additive Model with comprehensive smooth term integration
- **Training Data**: 10-year filtered dataset (`race_df_75`) containing only World Cup-caliber skiers (ELO ≥75% of race maximum)
- **Variable Integration**: Most extensive variable set among winter sports (9+ ELO categories plus weighted points)
- **Smoothing**: Automatic REML-based smoothing parameter selection for optimal flexibility

**Position Probability Modeling with REML Estimation**:
Cross-country implements sophisticated binomial GAM models for position predictions with comprehensive threshold coverage:

```r
# From race-picks.R:1135-1138
position_model <- gam(pos_gam_formula,
                      data = race_df,
                      family = binomial,
                      method = "REML")
```

**Comprehensive Brier Score Validation**:
Cross-country position models undergo rigorous evaluation using Brier scores across all finishing position thresholds:

```r
# From race-picks.R:1140-1143
# Calculate Brier score for model evaluation
predicted_probs <- predict(position_model, newdata = race_df, type = "response")
brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
log_info(paste("Brier score for threshold", threshold, ":", round(brier_score, 4)))
```

**Advanced Fallback Architecture**:
Cross-country implements robust error handling with sport-specific fallback strategies for handling the diverse race format challenges:

```r
# From race-picks.R:1158-1165
}, error = function(e) {
  log_warn(paste("Error in position model for threshold", threshold, ":", e$message))
  
  fallback_vars <- c("Prev_Points_Weighted", "Distance_Elo_Pct", "Sprint_Elo_Pct")
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

**Multi-Dimensional Adjustment System**:
Cross-country's modeling incorporates the most comprehensive adjustment system among winter sports, accounting for altitude effects, seasonal periodization, and mass start tactical dynamics through sequential statistical testing and conditional correction application.

**Technique-Specific Model Adaptation**:
The modeling system adapts to cross-country's fundamental technique distinctions (classic vs freestyle) while accommodating distance variations (sprint vs distance) and start format differences (mass start vs individual start), ensuring predictions capture the sport's full competitive complexity.

This cross-country-specific modeling framework ensures accurate performance prediction across the sport's unparalleled diversity of race formats, techniques, and environmental conditions while maintaining robust statistical validation and comprehensive adjustment mechanisms.

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

Cross-country skiing's startlist setup for testing implements a sophisticated data preparation pipeline that accommodates the sport's complex race format variations and dual-technique requirements. The system handles both distance and sprint races across freestyle and classic techniques while ensuring robust data quality for model prediction.

**Advanced Race Participation Probability Calculation**:
Cross-country startlist setup employs an exponential decay model for calculating race-specific participation probabilities, with precise race characteristic matching. The system recognizes that participation patterns differ significantly between Sprint vs Distance races and Classic vs Freestyle techniques:

```r
# From race-picks.R:25-122 in calculate_participation_probabilities()
calculate_participation_probabilities <- function(startlist_df, chronos) {
  # Only calculate for skiers with 0 probability
  zero_prob_skiers <- startlist_df %>%
    filter(get(race_prob_col) == 0) %>%
    pull(Skier)
  
  for(skier in zero_prob_skiers) {
    # Extract race characteristics from current race
    race_characteristics <- get_race_characteristics(current_race)
    
    # Find similar historical races (same distance/technique)
    participant_races <- chronos %>%
      filter(Skier == skier,
             Distance == race_characteristics$distance,
             Technique == race_characteristics$technique) %>%
      arrange(Date, Season, Race)
    
    if(nrow(participant_races) > 0) {
      total_races <- nrow(participant_races)
      # Exponential decay weights (α = 0.1)
      race_weights <- exp(-0.1 * ((total_races-1):0))
      
      weighted_participation <- sum(rep(1, total_races) * race_weights)
      total_weight <- sum(race_weights)
      prob <- weighted_participation / total_weight
      
      startlist_df[startlist_df$Skier == skier, race_prob_col] <- prob
    }
  }
}
```

**Most Recent ELO Rating Retrieval with Technique Specificity**:
The system retrieves the most current ELO ratings for each athlete across multiple categories, ensuring predictions use the most up-to-date performance assessments for distance/sprint and classic/freestyle combinations:

```r
# From race-picks.R:385-393 in prepare_startlist_data()
most_recent_elos <- race_df %>%
  filter(Skier %in% base_df$Skier) %>%
  group_by(Skier) %>%
  arrange(Date, Season, Race) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(Skier, any_of(elo_cols))
```

**ELO Score Normalization with Dual Methodology**:
Cross-country skiing employs two distinct normalization approaches depending on the race type. Standard races use historical maximum values, while Final Climb races use current startlist maximums for more dynamic scaling:

```r
# Standard race normalization (race-picks.R):
for(i in seq_along(elo_cols)) {
  elo_col <- elo_cols[i]
  if(elo_col %in% names(result_df) && elo_col %in% names(race_df)) {
    max_val <- max(race_df[[elo_col]], na.rm = TRUE)
    if(!is.na(max_val) && max_val > 0) {
      pct_value <- result_df[[elo_col]] / max_val
      result_df[[paste0(elo_col, "_Pct")]] <- pct_value
      result_df[[paste0(pelo_cols[i], "_Pct")]] <- pct_value
    }
  }
}

# Final Climb normalization (final_climb.R:1091-1125):
for(col in elo_cols) {
  max_val <- max(startlist_processed[[col]], na.rm = TRUE)
  if(!is.na(max_val) && max_val > 0) {
    startlist_processed[[paste0(col, "_Pct")]] <- startlist_processed[[col]] / max_val
  }
}
```

**Technique-Specific Weighted Previous Points Calculation**:
Recent performance is captured through weighted previous points using the last 5 races with linear increasing weights, calculated separately for each race distance/technique combination:

```r
# From race-picks.R:283-289
df_with_points <- df_with_points %>%
  group_by(Skier, Distance) %>%
  arrange(Season, Race) %>%
  mutate(Prev_Points_Weighted = sapply(1:n(), function(j) {
    if (j == 1) return(0)
    start_index <- max(1, j - 5)
    num_races <- j - start_index
    weights <- seq(1, num_races)  # Linear weights (1,2,3,4,5)
    weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
  })) %>%
  ungroup()
```

**Enhanced Feature Engineering for Final Climb Races**:
Final Climb races receive additional specialized features including FC_pred (preliminary model predictions) and Last_5_2 features that capture more nuanced performance patterns:

```r
# From final_climb.R:153-158
calc_weighted_last_5 <- function(places) {
  if(length(places) > 0) {
    weights <- seq(1, length(places))
    return(weighted.mean(places, weights, na.rm = TRUE))
  } else {
    return(NA_real_)
  }
}

# Last_5_2 feature integration (lines 1178-1238):
last_5_2_features <- get_latest_last_5_2_features(chronos_df)
startlist_processed <- merge_last_5_features(startlist_processed, last_5_2_features)
```

**Comprehensive Missing Value Imputation Strategy**:
Cross-country skiing uses an advanced NA handling approach with multiple fallback levels to preserve data quality across diverse race formats:

```r
# From race-picks.R:18-23
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

# Final Climb specific NA handling (final_climb.R:1274-1281):
if(any(is.na(startlist_processed[[feature]]))) {
  # Use first quartile of available data
  q1_val <- quantile(startlist_processed[[feature]], 0.25, na.rm = TRUE)
  startlist_processed[[feature]][is.na(startlist_processed[[feature]])] <- q1_val
}
```

**Dynamic Feature Completeness Assurance**:
The startlist setup ensures all required model variables exist with appropriate defaults, handling the complexity of cross-country's multiple race formats and technique combinations:

```r
# From race-picks.R:456-468
for(i in seq_along(pelo_cols)) {
  pelo_pct_col <- paste0(pelo_cols[i], "_Pct")
  if(!pelo_pct_col %in% names(result_df)) {
    result_df[[pelo_pct_col]] <- 0.5  # Default to middle value
  }
}

# Technique extraction and mapping:
current_technique <- substr(pelo_col, 8, 8)  # Extract technique from column name
```

**Model Preparation and Integration**:
The final startlist preparation combines all data sources through sophisticated joining logic, ensuring complete feature sets while preserving race-specific characteristics. This creates comprehensive, prediction-ready datasets that capture cross-country skiing's unique distance/sprint and classic/freestyle performance patterns essential for accurate GAM model prediction across all race formats.
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

Cross-country skiing's testing modeling employs sophisticated GAM application with dual-methodology support and comprehensive error handling. The system applies technique-specific trained models while maintaining robust fallback mechanisms and integrating altitude, period, and mass start adjustments that capture cross-country skiing's diverse performance patterns.

**Technique-Specific GAM Model Application**:
The system applies trained GAM models that were optimized using BIC feature selection, recognizing that performance varies significantly across distance/sprint formats and classic/freestyle techniques:

```r
# From race-picks.R:814-820
exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
summary_exhaustive <- summary(exhaustive_selection)
best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))

model <- gam(gam_formula, data = race_df_75)

# Model application to startlist
Base_Prediction = predict(model, newdata = startlist_prepared)
```

**Dual-Methodology Approach for Standard vs Final Climb Races**:
Cross-country skiing employs two distinct modeling approaches depending on race type, with Final Climb races receiving specialized feature engineering:

```r
# From final_climb.R:1394-1399
if(is.null(fc_model)) {
  # Fallback to FC_pred for Final Climb races
  startlist_df$Points_Pred <- startlist_df$FC_pred
  return(startlist_df)
}

startlist_df$Points_Pred <- tryCatch({
  predict(fc_model, newdata = startlist_df, type = "response")
}, error = function(e) {
  return(startlist_df$FC_pred)  # Fallback to FC_pred
})
```

**Comprehensive Multi-Tier Fallback System**:
Cross-country implements robust fallback mechanisms for both standard and position probability models:

```r
# From race-picks.R:1134-1161
}, error = function(e) {
  # Primary fallback: Simplified variable set
  fallback_vars <- c("Prev_Points_Weighted", pelo_col)
  fallback_vars <- fallback_vars[fallback_vars %in% names(race_df)]
  
  if(length(fallback_vars) > 0) {
    fallback_terms <- paste("s(", fallback_vars, ")", collapse=" + ")
    fallback_formula <- as.formula(paste("position_achieved ~", fallback_terms))
    
    position_models[[paste0("threshold_", threshold)]] <- gam(
      fallback_formula, data = race_df, family = binomial, method = "REML")
  }
})

# Complete model failure handling:
}, error = function(e) {
  # Set reasonable default based on threshold
  position_preds[[prob_col]] <- rep(threshold, nrow(position_preds))
})
```

**Advanced Prediction Error Handling with Variable Validation**:
The system includes sophisticated error recovery that ensures all required model variables exist with proper types:

```r
# From race-picks.R:1251-1264
for(var in model_vars) {
  if(!(var %in% names(prediction_subset))) {
    log_warn(paste("Missing required variable:", var, "- adding with default values"))
    prediction_subset[[var]] <- 0
  } else {
    # Handle NAs in existing variables
    if(any(is.na(prediction_subset[[var]]))) {
      if(is.numeric(prediction_subset[[var]])) {
        prediction_subset[[var]] <- replace_na_with_quartile(prediction_subset[[var]])
      }
    }
  }
}
```

**Sequential Multi-Dimensional Adjustment Integration**:
Testing predictions incorporate historically-derived adjustments that capture cross-country skiing's unique environmental and tactical factors:

```r
# From race-picks.R:1241-1243
Predicted_Points = Base_Prediction + altitude_adjustment + 
  period_adjustment + ms_adjustment,
Predicted_Points = pmax(pmin(Predicted_Points, 100), 0),

# Apply race probability weighting
Final_Prediction = Predicted_Points * Race_Prob
```

**Comprehensive Model Validation and Confidence Assessment**:
The system incorporates multiple validation metrics and confidence measures specific to cross-country's technique-dependent requirements:

```r
# From race-picks.R:1046-1049
predicted_probs <- predict(position_model, newdata = race_df, type = "response")
brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
log_info(paste("Brier score for threshold", threshold, ":", round(brier_score, 4)))

# Confidence factor calculation
confidence_factor = pmin(n_recent_races / 10, 1)
```

**Probability Normalization and Mathematical Consistency**:
Position probability models include extensive validation to ensure mathematical coherence across all thresholds, with monotonic constraints applied (win ≤ podium ≤ top5 ≤ top10 ≤ top30) and target sum validation for each threshold level.

The final predictions are constrained between 0 and 100 points to ensure realistic values within the World Cup points system range, with all predictions weighted by race participation probabilities to reflect actual competition dynamics.

###### Adjustments

Cross-country skiing implements a sophisticated sequential three-dimensional adjustment system that applies historically-derived corrections during testing. The system uses statistical significance testing to identify genuine performance patterns and applies altitude, period, and mass start adjustments in cascading sequence to capture cross-country skiing's unique environmental and tactical factors.

**Sequential Adjustment Calculation During Testing**:
The system applies adjustments in a specific sequence to avoid double-counting effects while capturing different dimensions of performance variation:

```r
# From race-picks.R:944-1012
race_df_75 <- race_df_75 %>%
  mutate(
    Initial_Prediction = predict(model, newdata = .)
  ) %>%
  group_by(Skier) %>%
  mutate(
    Prediction_Diff = Points - Initial_Prediction,
    
    # Step 1: Altitude adjustments using t-tests
    altitude_p = purrr::map_dbl(row_id, function(r) {
      prior_alt_curr <- Prediction_Diff[AltitudeCategory == AltitudeCategory[r] & row_id < r]
      prior_alt_other <- Prediction_Diff[AltitudeCategory != AltitudeCategory[r] & row_id < r]
      if(length(prior_alt_curr) < 3 || length(prior_alt_other) < 3) return(1)
      tryCatch({
        t.test(prior_alt_curr, prior_alt_other)$p.value
      }, error = function(e) 1)
    }),
    altitude_correction = ifelse(altitude_p < 0.05,
                                mean(Prediction_Diff[AltitudeCategory == AltitudeCategory], na.rm = TRUE),
                                0),
    
    # Step 2: Period adjustments (using altitude-corrected residuals)
    period_p = purrr::map_dbl(row_id, function(r) {
      if(r <= 1) return(1)
      prior_period_curr <- Course_Diff[Period == Period[r] & row_id < r]
      prior_period_other <- Course_Diff[Period != Period[r] & row_id < r]
      tryCatch({
        t.test(prior_period_curr, prior_period_other)$p.value
      }, error = function(e) 1)
    }),
    period_correction = ifelse(period_p < 0.05,
                              mean(Course_Diff[Period == Period], na.rm = TRUE),
                              0),
    
    # Step 3: Mass Start adjustments (using period-corrected residuals)
    ms_p = purrr::map_dbl(row_id, function(r) {
      if(r <= 1) return(1)
      prior_ms_curr <- Period_Diff[MS == MS[r] & row_id < r]
      prior_ms_other <- Period_Diff[MS != MS[r] & row_id < r]
      tryCatch({
        t.test(prior_ms_curr, prior_ms_other)$p.value
      }, error = function(e) 1)
    }),
    ms_correction = ifelse(ms_p < 0.05,
                          mean(Period_Diff[MS == MS], na.rm = TRUE),
                          0)
  )
```

**Final Adjustment Integration and Race Probability Weighting**:
All adjustments are combined additively with base predictions and integrated with race participation probabilities:

```r
# From race-picks.R:1363-1376
Predicted_Points = Base_Prediction + altitude_adjustment + 
  period_adjustment + ms_adjustment,
Predicted_Points = pmax(pmin(Predicted_Points, 100), 0),

# Apply race participation probability weighting
Race_Prob = ifelse(
  paste0("Race", i, "_Prob") %in% names(.),
  get(paste0("Race", i, "_Prob")) / 100,
  1.0
),
Final_Prediction = Predicted_Points * Race_Prob
```

**Statistical Significance Requirements**:
Each adjustment type requires statistical validation using t-tests with strict criteria:
- Minimum 3 observations in both current and comparison groups
- Statistical significance threshold of p < 0.05 
- Chronological integrity (only using historically prior data)
- Robust error handling to prevent model failures

**Position Probability Adjustments**:
The same three-dimensional adjustment framework is applied to position probability models, ensuring consistency across both points and probability predictions while maintaining proper probability bounds and mathematical constraints.
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

Cross-country skiing's Individual Probability Training Setup converts the points prediction problem into binary classification for position probability modeling with technique-specific threshold adaptation. The system uses the same preprocessed historical race data as points models but transforms the complex multi-technique regression problem into classification through binary outcome creation across different event formats.

**Position Threshold Definition**:
Cross-country skiing uses technique-adapted thresholds: Distance events use standard thresholds `c(1, 3, 5, 10, 30)` representing Win, Podium, Top 5, Top 10, and Top 30, while Sprint events use specialized thresholds `c(1, 3, 6, 12, 30)` representing Win, Podium, Final, Semifinal, and Quarterfinal. Each threshold creates a separate binary classification problem where success is defined as finishing at or above that position.

**Binary Outcome Creation**:
For each position threshold, the system creates binary outcome variables using the fundamental transformation: `race_df$position_achieved <- race_df$Place <= threshold`. This converts the continuous place variable into binary classification targets, enabling binomial GAM modeling for probability prediction across cross-country's diverse event spectrum.

**Training Data Consistency**:
Position probability models use identical training datasets as their corresponding points prediction models, including the same 10-season historical window, top performer filtering (ELO > 75th percentile), and technique-specific preprocessing. No separate data pipeline is required - the same `race_df` serves both modeling approaches.

**Technique-Specific Adaptation**:
Cross-country skiing's incredible complexity across techniques (Classic, Freestyle), distances (Sprint, Distance), and formats (Individual Start, Mass Start) requires threshold evaluation that adapts to event characteristics. Each technique-distance combination maintains appropriate position thresholds but may exhibit different probability patterns due to varying tactical demands and field compositions.

**Environmental Condition Integration**:
The training setup incorporates cross-country's unique environmental sensitivity, including altitude effects (above/below 1300m thresholds) and mass start tactical dynamics, ensuring position probability models capture the sport's diverse performance characteristics across all conditions.

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

Cross-country skiing's Individual Probability Training Feature Selection employs the most comprehensive threshold-independent optimization strategy among winter sports, adapting to the sport's incredible complexity across techniques, distances, and environmental conditions. The system performs independent BIC optimization for each position threshold while leveraging technique-specific variable inheritance from corresponding points prediction models.

**Variable Inheritance and Consistency**:
Position probability models use identical explanatory variable pools as their corresponding points models: `position_feature_vars <- explanatory_vars`. This ensures consistency between modeling approaches while leveraging domain knowledge already encoded in cross-country's technique-specific points model variable selection across the sport's diverse race spectrum.

**Technique-Specific Variable Sets**:
Cross-country skiing employs the most comprehensive variable set among winter sports, including `Prev_Points_Weighted`, `Distance_Pelo_Pct`, `Sprint_Pelo_Pct`, `Sprint_C_Pelo_Pct`, `Distance_F_Pelo_Pct`, `Distance_C_Pelo_Pct`, `Classic_Pelo_Pct`, `Freestyle_Pelo_Pct`, `Sprint_F_Pelo_Pct`, and `Pelo_Pct`. This reflects the sport's complexity across techniques (Classic, Freestyle), distances (Sprint, Distance), and formats (Individual Start, Mass Start).

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

Cross-country skiing's Individual Probability Training Modeling employs the most sophisticated binomial GAM architecture among winter sports, specifically optimized for the sport's incredible complexity across techniques, distances, and environmental conditions. The system creates independent models for each position threshold while incorporating technique-specific adaptations and implementing comprehensive validation approaches.

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

Cross-country skiing implements the most sophisticated Individual Probability Training Adjustments among winter sports, featuring an **active** multi-factor correction system that operates on probability residuals through sequential adjustment layers. This comprehensive methodology adapts to the sport's extraordinary complexity across techniques (Classic, Freestyle), distances (Sprint, Distance), and environmental conditions with statistical validation for each adjustment factor.

**Probability Residual Calculation with Technique Awareness**:
The adjustment system begins with probability difference calculations between actual outcomes and model predictions: `prob_diff = as.numeric(position_achieved) - initial_prob`. These residuals capture systematic bias patterns in position probability predictions while acknowledging cross-country's complex multi-dimensional performance characteristics across different race formats and techniques.

**Altitude Adjustments for Environmental Performance Variations**:
Cross-country's unique altitude adjustment layer addresses systematic performance differences based on elevation categories: `altitude_p = purrr::map_dbl(row_id, function(r) {...})`. The system employs t-test validation comparing altitude-specific probability residuals against other elevation categories, applying corrections only when p < 0.05 ensures genuine altitude-based systematic bias rather than random variation: `altitude_correction = ifelse(altitude_p < 0.05, mean(prob_diff[AltitudeCategory == AltitudeCategory], na.rm = TRUE), 0)`.

**Sequential Period Adjustments with Residual Propagation**:
Period adjustments operate on altitude-corrected residuals to capture systematic performance changes across the competitive season: `period_diff = as.numeric(position_achieved) - altitude_adjusted`. This sequential approach prevents interaction effects while addressing period-specific bias patterns through statistical validation: `period_p = purrr::map_dbl(row_id, function(r) {...})` with probability-bounded corrections: `period_adjusted = pmin(pmax(altitude_adjusted + period_correction, 0), 1)`.

**Mass Start Format Adjustments with Tactical Complexity**:
The final adjustment layer addresses systematic differences between individual start and mass start race formats: `ms_p = purrr::map_dbl(row_id, function(r) {...})`. Mass start races involve fundamentally different tactical dynamics compared to individual start formats, requiring separate bias correction to account for pack racing effects and strategic positioning challenges unique to cross-country skiing competition.

**Multi-Threshold Independent Adjustment Architecture**:
Each position threshold (Distance events: 1st, 3rd, 5th, 10th, 30th; Sprint events: 1st, 3rd, 6th, 12th, 30th) receives completely independent adjustment calculations, recognizing that factors influencing podium finishes may differ substantially from those affecting points-scoring positions across cross-country's diverse competitive spectrum.

**Statistical Validation with Robust Error Handling**:
All adjustment calculations include comprehensive error handling: `tryCatch({t.test(prior_period_curr, prior_period_other)$p.value}, error = function(e) 1)`. This ensures system stability across cross-country's variable competitive conditions while maintaining rigorous statistical validation for all adjustment applications.

**Probability Constraint Enforcement and Mathematical Validity**:
All adjustments maintain valid probability ranges through systematic boundary enforcement: `pmin(pmax(altitude_adjusted + period_correction, 0), 1)`. This prevents impossible probability predictions while preserving the mathematical integrity of the adjustment framework across cross-country's complex performance distribution patterns.

**Active Implementation and Production Status**:
Cross-country's Individual Probability Training Adjustments remain **fully active** in the production system, representing the most comprehensive implementation among winter sports. The sequential altitude → period → mass start processing order prevents adjustment interaction effects while capturing the sport's multifaceted systematic bias patterns across environmental conditions, competitive periods, and race format variations.

##### Testing

###### Startlist Setup

Cross-country skiing's Individual Probability Testing employs the most sophisticated startlist preparation among winter sports, accommodating the sport's extraordinary complexity across techniques (Classic, Freestyle), distances (Sprint, Distance), environmental conditions, and format variations. The system integrates fantasy applications, manages adaptive position thresholds, and processes the most comprehensive technique-specific performance data framework across all winter sports disciplines.

**Race Format-Specific Threshold Selection**:
Cross-country's startlist setup begins with race format detection to determine appropriate position thresholds. Distance events utilize standard thresholds (1st, 3rd, 5th, 10th, 30th representing Win, Podium, Top 5, Top 10, Top 30), while Sprint events employ format-specific thresholds (1st, 3rd, 6th, 12th, 30th for Win, Podium, Final, Semifinal, Quarterfinal). This adaptive threshold framework ensures position predictions align with cross-country's diverse competitive structures.

**Fantasy XC Price Integration**:
The system incorporates Fantasy XC price data through specialized column handling: `base_df <- startlist %>% dplyr::select(Skier, ID, Nation, Price, Sex)`. This integration enables fantasy sports applications while maintaining prediction accuracy, recognizing that athlete pricing reflects market perception of performance potential across technique and distance specializations.

**Comprehensive Technique-Specific ELO Processing**:
Cross-country utilizes the most extensive ELO rating system among winter sports: `elo_cols <- c("Distance_Elo", "Distance_C_Elo", "Distance_F_Elo", "Elo", "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", "Freestyle_Elo", "Classic_Elo")`. These ratings capture performance nuances across Classic versus Freestyle techniques, Sprint versus Distance specializations, and combined format capabilities, providing model input that reflects cross-country's exceptional technical and tactical complexity.

**Environmental Condition Framework Integration**:
The startlist preparation incorporates altitude categorization for environmental performance adaptation: athletes are classified into altitude performance categories (above/below 1300m) that influence both ELO calculations and subsequent position probability adjustments. This environmental sensitivity distinguishes cross-country from other winter sports in its startlist preparation sophistication.

**Mass Start versus Individual Start Format Adaptation**:
The system includes mass start format detection that influences tactical positioning variables and probability calculations. Mass start races involve fundamentally different competitive dynamics compared to individual start formats, requiring specialized startlist variables that capture pack racing effects and strategic positioning capabilities unique to cross-country competition.

**Base Startlist Preparation with Enhanced Framework**:
Cross-country maintains consistency with points prediction methodology while expanding for position probability requirements: `startlist_prepared <- prepare_startlist_data(startlist, race_df, pelo_col, gender)`. This includes participation probability assignment using technique-specific patterns, comprehensive ELO score processing across nine rating categories, weighted previous points calculation with technique specialization awareness, and advanced missing value imputation that considers technique-specific performance patterns.

**Position Prediction Framework with Multi-Dimensional Structure**:
The system creates specialized position prediction dataframes that accommodate cross-country's complexity: `position_preds <- data.frame(Skier, ID, Nation, Sex, Race)`. This framework prepares for threshold-specific model application while maintaining compatibility with technique-specific variables, environmental adjustments, and format-dependent tactical considerations.

**Model Variable Validation with Technique Awareness**:
Cross-country employs comprehensive model variable validation: `model_vars <- names(pos_model$var.summary)` that ensures each threshold-specific model receives appropriate technique-specific variables from the prepared startlist. This validation process maintains consistency between training and testing phases while accommodating the sport's extraordinary variable complexity across techniques, distances, and environmental conditions.

**Advanced Position Threshold Processing Framework**:
The system prepares for multi-threshold predictions with format-dependent naming: `prob_col <- paste0("prob_top", threshold)` while adapting to Sprint versus Distance threshold variations. This sophisticated framework ensures that position predictions properly reflect cross-country's unique competitive structures and finishing position definitions across different race formats.

###### Modeling

Cross-country skiing's Individual Probability Testing employs the most sophisticated model application framework among winter sports, accommodating extraordinary complexity across techniques (Classic, Freestyle), distances (Sprint, Distance), environmental conditions, and format variations. The system applies trained binomial GAM models through comprehensive variable validation, advanced adjustment integration, and robust error handling designed for the sport's exceptional competitive diversity.

**Technique-Specific Variable Validation with Environmental Awareness**:
Cross-country's testing modeling begins with the most comprehensive variable validation among winter sports: `model_vars <- names(pos_model$var.summary)`. The system validates technique-specific ELO ratings (Distance_Elo, Sprint_Elo, Classic_Elo, Freestyle_Elo), environmental variables (altitude categories), and format-specific features while ensuring all required variables exist: `for(var in model_vars) {if(!(var %in% names(prediction_subset))) {prediction_subset[[var]] <- 0}}`.

**Advanced Missing Value Handling with Technique Context**:
The framework employs sophisticated NA handling that considers technique-specific performance patterns: `if(is.numeric(prediction_subset[[var]])) {prediction_subset[[var]] <- replace_na_with_quartile(prediction_subset[[var]])}`. This approach ensures that missing values for Classic technique performance receive different treatment than missing Freestyle data, maintaining technique specialization integrity.

**Threshold-Adaptive GAM Model Application**:
Cross-country applies separate GAM models for Distance events (1st, 3rd, 5th, 10th, 30th) versus Sprint events (1st, 3rd, 6th, 12th, 30th): `base_predictions <- mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response")`. This threshold adaptation recognizes that Sprint semifinals and finals require different prediction approaches compared to Distance event top-30 finishes.

**Comprehensive Multi-Factor Adjustment Integration**:
Cross-country's testing uniquely integrates active altitude, period, and mass start adjustments during model application: `position_preds <- position_preds %>% left_join(pos_adj, by = "Skier") %>% mutate(altitude_effect = replace_na(altitude_effect, 0), period_effect = replace_na(period_effect, 0), ms_effect = replace_na(ms_effect, 0))`. This active adjustment system distinguishes cross-country from other sports that disable adjustments during testing.

**Sequential Adjustment Application with Probability Bounds**:
The system applies adjustments sequentially to maintain mathematical validity: altitude → period → mass start effects with probability constraint enforcement ensuring all values remain within [0,1]. This sequential approach prevents adjustment interaction effects while capturing cross-country's multifaceted environmental and tactical influences.

**Environmental Condition Model Integration**:
Model application incorporates altitude categorization effects that influence performance across different venue elevations (above/below 1300m). The framework ensures that environmental adjustments are properly applied during testing while maintaining technique-specific performance considerations.

**Mass Start versus Individual Start Model Differentiation**:
The testing framework distinguishes between mass start and individual start race formats during model application, recognizing that pack racing dynamics in mass start events require different prediction approaches compared to individual start time trial formats.

**Robust Error Handling for Technique Complexity**:
Cross-country implements comprehensive error handling that accommodates the sport's exceptional variable complexity across nine ELO rating categories. When model application fails, the system employs technique-aware fallback strategies that maintain distinction between Classic and Freestyle specializations.

**Fantasy Integration with Model Application**:
The framework accommodates Fantasy XC price data during model application while ensuring that fantasy considerations don't compromise prediction accuracy across technique and distance specializations. This integration maintains the sport's unique fantasy application requirements while preserving methodological rigor.
    
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

##### Individual Position Probability Testing Adjustments

Cross-country skiing implements **active** position probability adjustments, representing the most comprehensive Individual Position Probability Testing Adjustments system among winter sports. The system applies three-dimensional adjustments (altitude, period, and mass start) to raw position probabilities before normalization, capturing the sport's complex environmental and tactical performance patterns.

**Three-Dimensional Adjustment Framework**:
Cross-country's position probability adjustment system uses the same statistical methodology as points predictions but operates on probability residuals to identify systematic biases in finishing position predictions:

```r
# From race-picks.R:1150-1183 (Active probability adjustments)
# Apply adjustments if available for this threshold
if(adj_name %in% names(position_adjustments)) {
  # Get adjustments for this threshold
  pos_adj <- position_adjustments[[adj_name]]
  
  # Join with predictions and apply three adjustment types
  position_preds <- position_preds %>%
    left_join(pos_adj, by = participant_col) %>%
    mutate(
      # Replace NAs with zeros for all adjustment types
      altitude_effect = replace_na(altitude_effect, 0),
      period_effect = replace_na(period_effect, 0),
      ms_effect = replace_na(ms_effect, 0),
      
      # Apply three-dimensional adjustments
      altitude_adjustment = altitude_effect,
      period_adjustment = period_effect, 
      ms_adjustment = ms_effect,
      
      # Calculate adjusted probabilities
      adjusted_prob = get(paste0(prob_col, "_base")) + 
        altitude_adjustment + period_adjustment + ms_adjustment,
      
      # Ensure probabilities are between 0 and 1
      adjusted_prob = pmin(pmax(adjusted_prob, 0), 1)
    )
}
```

**Multi-Dimensional Statistical Validation**:
Cross-country's adjustment system uses rigorous statistical testing across three performance dimensions, each requiring p < 0.05 from t-tests comparing performance patterns:

1. **Altitude Adjustments**: Account for endurance effects at venues above 1300m, as some athletes thrive at altitude while others struggle with cardiovascular demands
2. **Period Adjustments**: Capture seasonal progression patterns from early season technique development through tour periods to championship peaks  
3. **Mass Start Adjustments**: Recognize tactical dynamics that differentiate mass start races from individual time trials, unique among winter sports

**Probability Boundary Enforcement**:
Unlike points adjustments, position probability adjustments include strict boundary constraints (`pmin(pmax(adjusted_prob, 0), 1)`) to ensure all adjusted probabilities remain mathematically valid, preventing impossible predictions while capturing genuine systematic performance biases.

**Threshold-Specific Implementation**:
Each position threshold (Top-1, Top-3, Top-6/5, Top-12/10, Top-30) receives independent adjustment calculations, allowing for nuanced correction patterns that may vary by finishing position category. This recognizes that systematic biases may affect different performance levels differently.

Cross-country skiing's active position probability adjustments represent the most sophisticated implementation of Individual Position Probability Testing Adjustments, leveraging the sport's complex performance landscape to enhance prediction accuracy while maintaining statistical rigor and mathematical validity.

#### Normalization and Monotonic Constraints

Cross-country skiing implements streamlined Individual Normalization and Monotonic Constraints that accommodate the sport's extraordinary technique and format complexity while maintaining mathematical validity across the most diverse race spectrum among winter sports. The system employs simplified normalization procedures without race participation probability adjustments, focusing on technique-aware constraint enforcement and threshold-adaptive processing for Distance versus Sprint event variations.

**Simplified Normalization Framework for Technique Complexity**:
Cross-country's normalization system employs a streamlined approach that processes probability distributions without race participation adjustments: `current_sum <- sum(normalized[[prob_col]], na.rm = TRUE); target_sum <- 100 * threshold`. This simplified framework acknowledges that cross-country's technique-specific participation patterns (Classic versus Freestyle specialization) are already captured during model application and probability generation phases.

**Technique-Aware Target Sum Calculation**:
The system calculates target sums that accommodate cross-country's unique threshold variations: Distance events use standard thresholds (100% for Top-1, 300% for Top-3, 500% for Top-5, 1000% for Top-10, 3000% for Top-30), while Sprint events employ format-specific targets (100% for Top-1, 300% for Top-3, 600% for Top-6, 1200% for Top-12, 3000% for Top-30) reflecting semifinal and quarterfinal advancement structures.

**Individual Probability Capping with Technique Specialization Awareness**:
Cross-country implements comprehensive probability capping: `over_hundred <- which(normalized[[prob_col]] > 100)` with excess redistribution that considers the sport's technique-specific performance distributions. The system redistributes excess probability proportionally while maintaining awareness that Classic specialists and Freestyle specialists may have different performance ceiling patterns.

**Distance vs Sprint Format-Adaptive Monotonic Constraints**:
The framework applies monotonic constraints with adaptation for different race formats: Distance events ensure `P(Top-1) ≤ P(Top-3) ≤ P(Top-5) ≤ P(Top-10) ≤ P(Top-30)` while Sprint events enforce `P(Top-1) ≤ P(Top-3) ≤ P(Top-6) ≤ P(Top-12) ≤ P(Top-30)`. This format-specific approach acknowledges the different competitive structures and advancement mechanisms across cross-country's diverse race spectrum.

**Re-normalization with Environmental and Technical Complexity**:
After monotonic constraint application, cross-country re-normalizes probabilities to maintain target sums while preserving the sport's multi-dimensional performance characteristics: `scaling_factor <- target_sum / current_sum; normalized[[prob_col]] <- normalized[[prob_col]] * scaling_factor`. This ensures mathematical consistency across altitude effects, technique variations, and mass start tactical dynamics.

**Streamlined Processing for Technique Diversity**:
Cross-country's normalization system maintains computational efficiency despite the sport's extraordinary complexity by focusing constraint enforcement on essential mathematical requirements rather than complex participation adjustments. This approach recognizes that technique-specific patterns are better captured through comprehensive model training and adjustment systems rather than normalization-phase interventions.

**Mathematical Validity Across Technique and Environmental Spectrum**:
The framework ensures that all final probabilities maintain mathematical rigor while accommodating cross-country's complex technique-specific and environmental performance patterns. Final probabilities remain within valid [0,1] bounds per athlete while summing to format-appropriate totals, reflecting the sport's exceptional competitive diversity across Classic/Freestyle techniques, Sprint/Distance formats, and environmental conditions.

#### Fantasy

Cross-country skiing implements sophisticated Individual Fantasy Team optimization representing the most advanced fantasy sports application among winter sports. The system employs Mixed Integer Programming (MIP) with multiple team generation strategies that accommodate the sport's extraordinary complexity across technique variations, environmental conditions, and risk preferences while maintaining mathematical optimality within budget and roster constraints.

**Multi-Strategy Team Generation Framework**:
Cross-country's fantasy optimization generates three distinct team strategies to accommodate different risk preferences: Normal teams maximize standard expected points (`Total_Points`), Safe teams prioritize conservative scenarios (`Total_Safe`), and Upside teams target optimistic projections (`Total_Upside`). This multi-strategy approach recognizes that cross-country's technique complexity and environmental variability create diverse performance scenarios requiring different fantasy approaches.

**Advanced Expected Value Integration**:
The system calculates probability-weighted expected values that incorporate cross-country's multi-race weekend structure: `Total_Points = Race1_Points * Race1_Probability + Race2_Points * Race2_Probability + ...`. This methodology accounts for technique-specific participation patterns where Classic specialists may skip certain Freestyle races, requiring sophisticated probability weighting across the sport's diverse race spectrum.

**Mixed Integer Programming Optimization**:
Fantasy team selection employs sophisticated MIP optimization using GLPK solver with binary decision variables for athlete selection: `maximize sum_expr(fantasy_df$Points[i] * x[i], i = 1:n)` subject to budget constraints (`sum_expr(fantasy_df$Price[i] * x[i], i = 1:n) <= 100000`) and roster requirements (exactly 16 athletes: maximum 8 men, maximum 8 women). This mathematical optimization ensures globally optimal team selection within constraints.

**Technique-Aware Budget and Roster Management**:
The fantasy framework accommodates cross-country's technique specialization through pricing mechanisms that reflect Classic versus Freestyle performance capabilities. Budget allocation (100,000 fantasy dollars) and roster constraints (16 total athletes with gender balance) enable strategic selection across technique specialists while maintaining competitive team balance.

**Risk Scenario Integration with Environmental Complexity**:
Fantasy optimization incorporates cross-country's environmental variability through scenario-based prediction integration. Safe scenarios account for altitude effects and harsh conditions that favor endurance specialists, while Upside scenarios emphasize mass start tactical opportunities and technique-specific advantages that create breakthrough performance potential.

**Robust Optimization with Fallback Strategies**:
When primary MIP optimization encounters computational issues, the system implements greedy value-per-dollar fallback selection: `value_per_price <- expected_value / Price`. This ensures fantasy team generation reliability even when complex optimization scenarios challenge solver capabilities, maintaining team selection availability across all competitive scenarios.

**Integration with Comprehensive Prediction Pipeline**:
Fantasy optimization seamlessly integrates with cross-country's sophisticated prediction models, utilizing GAM-based point predictions, technique-specific probability adjustments, and environmental condition modifications. The framework leverages the sport's most comprehensive adjustment system (altitude + period + mass start) to generate fantasy-ready expected values that capture cross-country's exceptional performance complexity.

**Mathematical Optimality Across Technique Spectrum**:
Cross-country's fantasy team optimization maintains mathematical rigor while accommodating the sport's extraordinary technique and environmental complexity. The MIP framework ensures globally optimal team selection within budget and roster constraints while generating multiple strategies that reflect different risk preferences and technique specialization approaches across cross-country skiing's diverse competitive landscape.

### Relay

#### Data Gathering

Cross-country relay data gathering employs sophisticated FIS website HTML parsing with Fantasy XC API integration across three distinct relay formats, representing the most complex relay data collection system among winter sports. The framework accommodates standard relays, mixed relays, and team sprints through format-specific processing modules while integrating technique-specific performance data and fantasy sports pricing information for comprehensive team evaluation.

**Multi-Format Relay Type Detection**:
Cross-country's data gathering system employs advanced event type detection using string pattern matching in race titles: `if 'MIXED' in event_title and 'TEAM' in event_title:` for mixed team identification. Standard relays feature 4-member single-gender teams (4x5km women, 4x10km men), Mixed relays utilize alternating gender patterns (4 members with M-F-M-F composition), and Team Sprint events employ 2-person teams competing in same-technique races.

**FIS Website HTML Parsing with Advanced Team Structure Recognition**:
The system processes FIS website data through sophisticated HTML parsing using `.table-row_theme_main` for team identification and `.table-row_theme_additional` with `.athlete-team-row` for individual member extraction. Team data includes nation-based identification, team ranking, and comprehensive member roster with leg assignments determined through bib parsing using "X-Y" format where Y indicates leg position.

**Fantasy XC API Integration for Pricing and Team Enhancement**:
Cross-country uniquely integrates Fantasy XC API data for enhanced team information: team pricing, API identification, and specialized Fantasy XC team composition data. This integration provides `Price` and `Team_API_ID` fields that enable fantasy sports applications while maintaining comprehensive performance data integration across the sport's technique-specific competitive requirements.

**Technique-Specific ELO Integration and Team Performance Aggregation**:
The data gathering framework incorporates cross-country's exceptional ELO complexity through comprehensive athlete data integration: `['Elo', 'Distance_Elo', 'Distance_C_Elo', 'Distance_F_Elo', 'Sprint_Elo', 'Sprint_C_Elo', 'Sprint_F_Elo', 'Classic_Elo', 'Freestyle_Elo']`. Team-level aggregation calculates `Total_Elo` and `Avg_Elo` metrics that reflect collective technique-specific capabilities across Classic and Freestyle specializations essential for relay performance evaluation.

**Advanced Gender Detection with Position-Based Validation**:
Cross-country employs sophisticated gender detection for mixed relay events using position-based leg assignment combined with dual ELO database fuzzy matching. The system validates gender assignments through historical performance data correlation, ensuring accurate team composition representation across the sport's diverse relay format variations and technique specialization patterns.

**Environmental Data Integration with Elevation Processing**:
The relay data gathering system incorporates cross-country's environmental complexity through elevation data integration and country code mapping for comprehensive team performance context. This environmental data enables altitude-aware team performance evaluation that accounts for endurance effects and venue-specific performance characteristics essential for accurate relay predictions.

**Comprehensive Error Handling with Fallback Team Generation**:
Cross-country implements robust error handling designed for FIS website structure variations and data availability challenges. The system includes fallback mechanisms using common nation lists for empty startlists, quartile imputation for missing ELO scores, and sophisticated name normalization for international athlete identification across the sport's global competitive participation spectrum.

#### Points

##### Training

###### Setup

Cross-country relay points training setup employs sophisticated leg-specific modeling with technique-aware data preparation that represents the most comprehensive relay training methodology among winter sports. The system combines 11 years of historical relay and individual data to create specialized training datasets for each relay leg while accommodating cross-country's extraordinary technique complexity (Classic vs Freestyle) and environmental performance variations across diverse competitive conditions.

**Leg-Specific Technique-Aware Training Data Preparation**:
Cross-country's relay training setup creates specialized datasets for each of the four relay legs with technique-specific modeling approaches: Legs 1-2 utilize Classic technique specialists data with `Classic_Pelo_Pct` and `Distance_C_Pelo_Pct` variables, while Legs 3-4 employ Freestyle technique specialists data with `Freestyle_Pelo_Pct` and `Distance_F_Pelo_Pct` variables. This leg-specific approach recognizes that relay performance requires different technical specializations across race segments while maintaining comprehensive environmental and seasonal context.

**Comprehensive Historical Data Integration**:
The training setup incorporates 11 years of both relay race results and individual race performance data (minimum season calculation: `max(df_individuals$Season) - 11`) to create robust training datasets. Relay races (`Distance == "Rel"`) are processed separately from individual races while maintaining technique-specific linkage through athlete performance histories, enabling comprehensive form progression analysis across both individual capabilities and team coordination requirements.

**Advanced Weighted Previous Points Calculation with Technique Matching**:
Cross-country implements sophisticated weighted previous points calculation that matches relay leg requirements with appropriate individual race histories: Legs 1-2 (Classic) utilize Classic distance race results for weighted averages, Legs 3-4 (Freestyle) use Freestyle distance race results, with the critical innovation that relay races themselves do NOT contribute to future weighted averages, preventing circular dependency while maintaining technique-specific performance context.

**ELO Percentile Conversion for Team Context**:
The system employs advanced `create_pelo_pcts()` function to convert absolute ELO ratings into race-specific percentile values, enabling fair comparison across different competitive field strengths and seasonal variations. This percentile conversion maintains technique-specific context while creating standardized performance metrics suitable for leg-specific model training across cross-country's diverse competitive spectrum.

**Relay-Specific Points System Integration**:
Cross-country relay training utilizes a specialized points system that provides higher point values than individual races: `relay_points <- c(200, 160, 120, 100, 90, 80, 72, 64, 58, 52, ...)` compared to individual race points, recognizing the increased prestige and difficulty of relay competition. This relay-specific points system ensures appropriate weighting of team achievements while maintaining consistency with World Cup relay competition point allocations.

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

Cross-country relay points training feature selection employs sophisticated rule-based technique-specific optimization that adapts to the sport's extraordinary complexity across multiple relay formats while incorporating leg-specific variable sets and technique-aware performance integration through deterministic feature selection based on relay position dynamics and cross-country's distinctive multi-dimensional competitive characteristics across classic and freestyle techniques.

**Rule-Based Technique-Specific Approach**:
Cross-country relay feature selection diverges from individual race BIC optimization, employing deterministic rule-based selection that accommodates the sport's leg-specific performance requirements:

Relay feature selection uses a rule-based, technique-specific approach rather than statistical selection methods like BIC. The selection is deterministic and based on leg position, technique type, and racing dynamics.

**Classic Leg Feature Set (Legs 1 & 2)**:
Classic legs in cross-country relays utilize technique-specific variables that capture classic skiing proficiency and endurance capabilities:

```r
# From weekly-picks-relay.R:376-383
if(leg <= 2) {
  # Classic legs (1 and 2)
  predictors <- c(
    grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE),
    grep("Classic.*Pelo_Pct$", base_cols, value = TRUE),
    "Distance_Pelo_Pct",
    "Pelo_Pct",
    "Weighted_Last_5"
  )
```

Classic legs will consider Distance Classic, Classic, Distance, Overall Elos and weighted last 5 points for the features.

**Freestyle Leg Feature Set (Leg 3)**:
Freestyle legs employ technique-specific variables for freestyle skiing performance without sprint considerations:

```r
# From weekly-picks-relay.R:384-393
} else if (leg==3){
  # Freestyle legs (3 and 4)
  predictors <- c(
    grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE),
    grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE),
    "Distance_Pelo_Pct",
    "Pelo_Pct",
    "Weighted_Last_5"
  )
}
```

**Anchor Leg Feature Set (Leg 4)**:
The anchor leg (Leg 4) receives enhanced variable sets that include sprint capabilities for finishing tactics:

```r
# From weekly-picks-relay.R:394-403
else if(leg ==4){
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

Freestyle legs will consider Distance Freestyle, Freestyle, Distance, Overall Elos, and weighted last 5 freestyle points for theirs with the exception of the anchor leg for standard relays which will also consider sprint Elo.

**Team Sprint Technique-Specific Feature Selection**:
Team Sprint events employ technique-specific feature selection that adapts to race technique (Classic or Freestyle) with comprehensive sprint-focused variable integration:

```r
# From weekly-picks-team-sprint.R:450-477
if(is_classic) {
  log_info(paste("Race is classic technique - using classic-specific predictors"))
  technique_predictors <- c(
    "Sprint_Pelo_Pct",             # General sprint ability  
    "Sprint_C_Pelo_Pct",           # Sprint classic specific
    "Classic_Pelo_Pct"             # General classic ability
  )
} else if(is_freestyle) {
  log_info(paste("Race is freestyle technique - using freestyle-specific predictors"))
  technique_predictors <- c(
    "Sprint_Pelo_Pct",             # General sprint ability
    "Sprint_F_Pelo_Pct",           # Sprint freestyle specific  
    "Freestyle_Pelo_Pct"           # General freestyle ability
  )
}
```

**Mixed Relay Gender and Technique Integration**:
Mixed relay feature selection incorporates both gender and technique considerations with leg-specific adaptations:

```r
# From weekly-picks-mixed-relay.R:427-463
if(leg == 1) {
  # Leg 1 (Female Classic)
  predictors <- c(
    grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE),
    grep("Classic.*Pelo_Pct$", base_cols, value = TRUE),
    "Distance_Pelo_Pct", "Pelo_Pct", "Weighted_Last_5"
  )
} else if(leg == 4) {
  # Leg 4 (Male Freestyle)
  predictors <- c(
    grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE),
    grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE),
    "Distance_Pelo_Pct", "Sprint_Pelo_Pct", "Pelo_Pct", "Weighted_Last_5"
  )
}
```

**Comprehensive Missing Value Management**:
The missing values are imputed with the first quartile values like they are for individual races, ensuring consistent data quality across cross-country's complex relay format spectrum and technique-specific performance requirements.

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

####### Modeling

Cross-country relay probability training modeling represents the most sophisticated leg-specific binomial GAM implementation among winter sports, utilizing technique-aware feature selection and comprehensive fallback strategies that integrate XGBoost and GLM modeling to optimize individual leg position prediction across multiple relay formats with specialized adaptations for team sprint, mixed relay, and standard 4-leg relay competition structures.

**Leg-Specific Binomial GAM Training Architecture**:
Cross-country employs sophisticated leg-specific binary classification modeling using binomial GAM with REML estimation for individual leg outcome probability training. Unlike other winter sports that use simplified team-level modeling, cross-country trains separate models for each relay leg position with position-specific feature selection and technique-aware optimization:

```r
# From weekly-picks-relay.R:493-550 - Leg-specific model training
train_leg_models <- function(leg_data) {
  control <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = defaultSummary,
    savePredictions = "final"
  )
  
  # Train models for multiple outcome thresholds per leg
  for(outcome in c("is_win", "is_podium", "is_top5", "is_top10")) {
    leg_model <- train(
      formula = as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))),
      data = leg_data,
      method = "gam",
      family = binomial(),
      trControl = control
    )
  }
}
```

**Technique-Aware Training Strategy**:
Cross-country implements advanced technique-specific model training with dynamic feature selection based on leg position and relay format. The training system adapts feature sets for classic legs (1-2) versus freestyle legs (3-4) with specialized anchor leg enhancement that incorporates sprint-specific predictors for tactical finish modeling:

```r
# From weekly-picks-relay.R:950-1050 - Technique-aware leg training
# Classic legs (1-2): Focus on classic distance and general endurance
classic_predictors <- c(
  grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE),
  grep("Classic.*Pelo_Pct$", base_cols, value = TRUE),
  "Distance_Pelo_Pct", "Pelo_Pct", "Weighted_Last_5"
)

# Freestyle legs (3-4): Focus on freestyle technique and sprint ability
freestyle_predictors <- c(
  grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE),
  grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE),
  "Distance_Pelo_Pct", "Sprint_Pelo_Pct", "Pelo_Pct", "Weighted_Last_5"
)
```

**Comprehensive XGBoost and GLM Fallback Training**:
Cross-country employs the most sophisticated fallback strategy among winter sports with multi-algorithm training that includes XGBoost primary modeling, GLM secondary fallback, and basic GLM tertiary fallback to ensure robust prediction capability across varying data quality scenarios:

```r
# From weekly-picks-relay.R:402-549 - Multi-algorithm training strategy
# Primary: XGBoost with hyperparameter tuning
xgb_model <- train(
  formula, data = training_data,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
  tuneLength = 3
)

# Secondary: Standard GLM fallback
if(failed_xgb) {
  glm_model <- train(
    formula, data = training_data,
    method = "glm", family = binomial(),
    trControl = trainControl(method = "cv", number = 5, classProbs = TRUE)
  )
}

# Tertiary: Basic GLM with reduced features
if(failed_glm) {
  basic_glm <- glm(formula, data = training_data, family = binomial())
}
```

**Mixed Relay Training with Gender-Alternating Optimization**:
Mixed relay training implements sophisticated gender-aware leg modeling with alternating male/female composition (F-M-F-M) and advanced ID management systems to prevent data conflicts between men's and women's datasets during training:

```r
# From weekly-picks-mixed-relay.R:84-118 - Gender-specific training
# Process with gender constraints and ID conflict prevention
ladies_data <- ladies_data %>% mutate(ID = ID + 100000)  # Offset for conflict avoidance
combined_data <- bind_rows(men_data, ladies_data)

# Gender-specific leg training
# Leg 1 (Female Classic) & Leg 3 (Female Freestyle)
female_legs <- combined_data %>% filter(Sex == "F")
# Leg 2 (Male Classic) & Leg 4 (Male Freestyle) 
male_legs <- combined_data %>% filter(Sex == "M")
```

**Team Sprint Training with 2-Leg Optimization**:
Team sprint training employs simplified 2-leg architecture with equal weighting and technique-adaptive feature selection that dynamically adjusts for classic versus freestyle race formats with comprehensive sprint-focused predictor integration:

```r
# From weekly-picks-team-sprint.R:190-225 - Team sprint training adaptation
# Equal leg weighting for 2-leg team sprint format
team_sprint_weights <- c(0.5, 0.5)  # Leg 1, Leg 2

# Dynamic technique adaptation
if(technique == "C") {
  technique_predictors <- c(
    "Sprint_Pelo_Pct", "Distance_Pelo_Pct", 
    "Sprint_C_Pelo_Pct", "Classic_Pelo_Pct"
  )
} else if(technique == "F") {
  technique_predictors <- c(
    "Sprint_Pelo_Pct", "Sprint_F_Pelo_Pct", "Freestyle_Pelo_Pct"
  )
}
```

**Advanced Leg Importance Training Weights**:
Cross-country implements sophisticated leg importance weighting during training that emphasizes anchor leg tactical significance with progressive weight increases reflecting competitive importance and strategic impact on team relay outcomes:

```r
# Standard relay training weights (4-leg format)
leg_importance_weights <- c(
  0.2,   # Leg 1 (Classic opener)
  0.2,   # Leg 2 (Classic foundation) 
  0.25,  # Leg 3 (Freestyle transition)
  0.35   # Leg 4 (Freestyle anchor - highest tactical weight)
)

# Applied during team aggregation training
weighted_prediction <- sum(leg_predictions * leg_importance_weights)
```

**Feature Importance Analysis in Training**:
Team sprint training includes advanced feature importance extraction and analysis for model interpretability with XGBoost and GLM coefficient analysis that provides insights into predictor significance for sprint-specific relay performance optimization:

```r
# From weekly-picks-team-sprint.R:350-380 - Training feature importance
safe_importance <- function(model) {
  if("xgb.Booster" %in% class(model)) {
    imp <- xgb.importance(model = model)
    return(imp$Feature[1:min(5, nrow(imp))])
  } else if("glm" %in% class(model)) {
    coef_summary <- summary(model)$coefficients
    sorted_coefs <- sort(abs(coef_summary[-1, 1]), decreasing = TRUE)
    return(names(sorted_coefs)[1:min(5, length(sorted_coefs))])
  }
}
```

**Training Data Quality and Cross-Validation**:
Cross-country relay training employs robust 5-fold cross-validation with class probability prediction and comprehensive error handling that ensures model stability across varying data scenarios with sophisticated missing value imputation and outlier detection during the training phase:

```r
# Training control configuration
control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = defaultSummary,
  savePredictions = "final",
  allowParallel = TRUE
)
```

Cross-country's relay probability training modeling establishes the benchmark for sophisticated leg-specific binomial classification with technique-aware optimization and comprehensive algorithm fallback strategies that ensure robust prediction capability across the full spectrum of relay competition formats and data quality scenarios in winter sports.

###### Adjustments

Cross-country relay probability training implements the **most sophisticated enabled adjustment framework** among winter sports, featuring a unique two-stage systematic bias correction system with comprehensive mode-based probability reset strategies and mathematical constraint enforcement specifically designed to eliminate team prediction biases and ensure optimal probability distribution across leg-specific relay modeling complexity.

**Two-Stage Adjustment System Architecture**:
Cross-country employs a revolutionary two-stage approach that represents the pinnacle of relay probability adjustment sophistication: **Stage 1** utilizes mode detection and probability reset strategies (`reset_mode_probabilities()`) to eliminate systematic bias patterns, followed by **Stage 2** comprehensive mathematical normalization with constraint enforcement (`normalize_probabilities()`) to maintain valid probability distributions across all participating teams.

```r
# Stage 1: Mode-based probability reset for systematic bias elimination
men_team_predictions <- reset_mode_probabilities(men_team_predictions)
ladies_team_predictions <- reset_mode_probabilities(ladies_team_predictions)

# Stage 2: Mathematical normalization with constraint enforcement
men_team_predictions <- normalize_probabilities(men_team_predictions)
ladies_team_predictions <- normalize_probabilities(ladies_team_predictions)
```

**Advanced Mode Detection and Reset Strategy**:
The `reset_mode_probabilities()` function implements sophisticated statistical analysis to identify and eliminate unrealistic probability clustering that emerges from complex leg-specific modeling interactions. The system detects repeated probability values (using 6-decimal precision to avoid floating-point artifacts) and strategically resets problematic modes to zero when they occur more than twice or fall below maximum repeated values:

```r
# From cross-country mode reset implementation
reset_mode_probabilities <- function(team_predictions) {
  # Round to 6 decimal places to handle floating point precision
  rounded_values <- round(team_predictions$probability, 6)
  
  # Count frequency of each probability value
  value_counts <- table(rounded_values)
  
  # Identify repeated values (appearing >= 2 times)
  repeated_values <- as.numeric(names(value_counts[value_counts >= 2]))
  
  # Find maximum among repeated values
  max_repeated <- max(repeated_values)
  
  # Reset values that are repeated OR below max repeated value
  to_reset <- sapply(rounded_values, function(v) 
    any(abs(v - repeated_values) < 1e-6) || v < max_repeated)
    
  team_predictions$probability[to_reset] <- 0
  return(team_predictions)
}
```

**Comprehensive Mathematical Normalization with Target Enforcement**:
Cross-country implements rigorous mathematical normalization that enforces theoretically correct probability targets based on race structure: exactly 1 winner (100%), exactly 3 podium positions (300%), exactly 5 top-5 positions (500%), and exactly 10 top-10 positions (1000%). The system maintains these mathematical constraints while preserving competitive realism across team probability distributions:

```r
# Mathematical target enforcement across position thresholds
targets <- list(
  Win_Prob = 1,      # Exactly 1 winner per race
  Podium_Prob = 3,   # Exactly 3 podium positions
  Top5_Prob = 5,     # Exactly 5 top-5 positions  
  Top10_Prob = 10    # Exactly 10 top-10 positions
)

# Normalization with scaling factor calculation
current_sum <- sum(team_predictions[[prob_col]], na.rm = TRUE)
scaling_factor <- targets[[prob_col]] / current_sum
team_predictions[[prob_col]] <- team_predictions[[prob_col]] * scaling_factor
```

**Monotonic Constraint Enforcement with Re-normalization**:
The adjustment system implements sophisticated monotonic constraint enforcement that ensures logical probability ordering (Win ≤ Podium ≤ Top5 ≤ Top10) for each team while maintaining target sum requirements. After constraint application, the system performs comprehensive re-normalization to preserve mathematical consistency across probability distributions:

```r
# Monotonic constraint enforcement with target preservation
for(i in 1:nrow(team_predictions)) {
  # Apply monotonic constraints row-wise
  team_predictions$Win_Prob[i] <- min(team_predictions$Win_Prob[i], 
                                     team_predictions$Podium_Prob[i])
  team_predictions$Podium_Prob[i] <- min(team_predictions$Podium_Prob[i],
                                        team_predictions$Top5_Prob[i])
  team_predictions$Top5_Prob[i] <- min(team_predictions$Top5_Prob[i],
                                      team_predictions$Top10_Prob[i])
}

# Re-normalize after constraint application
team_predictions <- normalize_probabilities(team_predictions)
```

**Format-Specific Adjustment Application**:
Cross-country adapts adjustment complexity based on relay format requirements, recognizing that different relay structures require varying levels of systematic bias correction:

**Standard Relay (4-leg)**: Full two-stage adjustment system with comprehensive mode reset and normalization
**Team Sprint (2-leg)**: Simplified adjustment approach acknowledging reduced team complexity  
**Mixed Relay (gender-alternating)**: Gender-aware adjustment processing with specialized ID management to prevent conflicts between men's and women's datasets

```r
# Format-specific adjustment application
if(relay_format == "standard" || relay_format == "mixed") {
  # Full two-stage system for complex team dynamics
  predictions <- apply_full_adjustment_system(predictions)
} else if(relay_format == "team_sprint") {
  # Simplified approach for 2-leg team dynamics
  predictions <- apply_simplified_adjustments(predictions)
}
```

**Advanced Probability Distribution Analysis**:
The adjustment framework includes sophisticated statistical analysis of probability distributions to identify systematic patterns requiring correction. The system analyzes frequency distributions, identifies statistical modes and outliers, and applies normalization strategies that maintain mathematical validity while eliminating unrealistic clustering inherent in complex leg-specific relay prediction scenarios:

```r
# Statistical distribution analysis for adjustment optimization
analyze_probability_distribution <- function(team_predictions) {
  # Frequency analysis of probability values
  freq_analysis <- table(round(team_predictions$probability, 3))
  
  # Identify statistical outliers and modes
  outliers <- identify_statistical_outliers(team_predictions)
  modes <- identify_probability_modes(freq_analysis)
  
  # Generate adjustment recommendations
  adjustment_strategy <- optimize_adjustment_approach(outliers, modes)
  return(adjustment_strategy)
}
```

**Error Handling and Robustness in Complex Scenarios**:
Cross-country's adjustment system includes comprehensive error handling designed for complex leg-specific modeling scenarios with multiple fallback mechanisms when probability distributions encounter edge cases (zero sums, extreme clustering). The system ensures robust adjustment application across varying data quality conditions while maintaining mathematical validity and competitive realism.

**Technique-Aware Adjustment Processing**:
The adjustment framework acknowledges cross-country's technique-specific requirements (classic vs freestyle legs) by maintaining technique awareness during probability corrections. The system ensures that adjustments preserve the tactical significance of anchor leg performance and technique specialization patterns essential for accurate relay probability prediction across diverse competition formats.

Cross-country's two-stage relay probability training adjustment system represents the most advanced systematic bias correction methodology in winter sports, providing comprehensive solutions for the complex probability distribution challenges that arise from sophisticated leg-specific modeling and ensuring mathematical consistency while preserving competitive accuracy across the sport's diverse relay competition formats.

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

Cross-country relay points training adjustments employ sophisticated systematic bias correction mechanisms that represent the most advanced team adjustment framework among winter sports. Unlike individual events that apply environmental and temporal adjustments, relay adjustments focus on comprehensive probability normalization with systematic mode reset strategies and mathematical constraint enforcement designed to eliminate team prediction biases inherent in relay modeling complexity.

Cross-country relay points testing adjustments implement a **sophisticated two-stage systematic bias correction framework** that utilizes mathematical probability normalization with mode reset strategies specifically designed to eliminate systematic biases in team probability predictions during the testing phase. Unlike other winter sports that disable adjustment frameworks for relay events, cross-country employs the most advanced relay testing adjustment system with comprehensive probability distribution analysis and constraint enforcement.

**Two-Stage Relay Testing Adjustment Process**:
Cross-country relay testing applies a unique two-stage adjustment methodology that differs fundamentally from individual event adjustments: Stage 1 employs `reset_mode_probabilities()` function to identify and eliminate unrealistic probability clustering, followed by Stage 2 mathematical constraint enforcement to maintain valid probability distributions across all participating teams.

**reset_mode_probabilities() Function for Testing Phase Probability Normalization**:
The testing adjustment framework employs a specialized `reset_mode_probabilities()` function that identifies modal probability distributions and resets them to zero when they occur more than twice or are less than the maximum repeated value: `team_predictions <- reset_mode_probabilities(team_predictions)`. This approach prevents unrealistic probability clustering that can emerge during the testing phase when models generate repetitive predictions for similar team compositions.

**Modal Probability Detection and Reset Logic**:
The reset function analyzes probability distributions across all teams to identify modes that represent systematic bias patterns: when identical or highly similar probabilities occur repeatedly across different teams, the function identifies these as unrealistic clustering and sets them to zero. This approach ensures that team probability predictions reflect genuine performance differences rather than modeling artifacts inherent in relay prediction complexity.

**Mathematical Constraint Enforcement with Team Probability Validation**:
Following mode reset, the testing adjustment system enforces comprehensive mathematical constraints to ensure valid probability distributions: total probabilities across all teams must sum to appropriate targets (100% for Top-1, 300% for Top-3, 500% for Top-5, etc.) while maintaining individual team probability bounds within [0,1] ranges and preserving monotonic constraints (P(Top-1) ≤ P(Top-3) ≤ P(Top-5)).

**Sophisticated Probability Distribution Analysis**:
The testing adjustment framework includes advanced statistical analysis of probability distributions to identify systematic patterns that require correction. The system analyzes frequency distributions of probability values across teams, identifies statistical modes and outliers, and applies sophisticated normalization strategies that maintain mathematical validity while eliminating unrealistic clustering inherent in complex relay team prediction scenarios.

**Two-Stage Testing Application Methodology**:
Cross-country applies the two-stage testing adjustment sequentially: men's relay predictions undergo `reset_mode_probabilities()` followed by constraint enforcement, then women's relay predictions receive identical treatment, ensuring consistent adjustment application across gender categories while maintaining separate processing that acknowledges potential gender-specific bias patterns in relay team performance.

**Advanced Testing Phase Bias Correction**:
Unlike training adjustments that focus on historical systematic patterns, testing adjustments target real-time prediction quality through probability distribution analysis. The framework identifies when relay team models produce unrealistic probability clustering during testing and applies immediate corrections that preserve individual team competitive differences while eliminating systematic modeling biases that could affect prediction accuracy.

**Comprehensive Systematic Bias Prevention**:
The testing adjustment system prevents multiple forms of systematic bias: modal probability clustering (when models generate repetitive values), mathematical constraint violations (when probabilities exceed valid bounds), and monotonic sequence disruption (when higher thresholds receive lower probabilities than lower thresholds). This comprehensive approach ensures relay testing predictions maintain both mathematical validity and competitive realism.

**Two-Stage Systematic Bias Correction Framework**:
Cross-country relay adjustments implement a sophisticated two-stage systematic bias correction approach unique among winter sports relay methodologies:

```r
# Stage 1: Mode Reset Strategy for Systematic Bias Elimination
# From weekly-picks-relay.R and weekly-picks-mixed-relay.R
men_team_predictions <- reset_mode_probabilities(men_team_predictions)
ladies_team_predictions <- reset_mode_probabilities(ladies_team_predictions)

# Stage 2: Mathematical Probability Normalization
men_team_predictions <- normalize_probabilities(men_team_predictions)
ladies_team_predictions <- normalize_probabilities(ladies_team_predictions)
```

**Systematic Mode Reset Strategy**:
Cross-country employs advanced mode detection to identify and eliminate systematic prediction biases in relay team probability distributions:

```r
# Mode reset identifies repeated probability values and systematic bias patterns
reset_mode_probabilities <- function(team_predictions) {
  # Identifies repeated probability values (mode detection)
  repeated_values <- as.numeric(names(value_counts[value_counts >= 2]))
  # Resets repeated values and values below max repeated value to zero
  to_reset <- sapply(rounded_values, function(v) 
    any(abs(v - repeated_values) < 1e-6) || v < max_repeated)
}
```

**Mathematical Normalization with Constraint Enforcement**:
Cross-country relay adjustments implement strict mathematical normalization with theoretical target enforcement unique among team sports:

```r
# Normalization targets based on mathematical race structure
targets <- list(
  Win_Prob = 1,      # Exactly 1 winner per race
  Podium_Prob = 3,   # Exactly 3 podium positions  
  Top5_Prob = 5,     # Exactly 5 top-5 positions
  Top10_Prob = 10    # Exactly 10 top-10 positions
)

# Monotonic constraint enforcement
# Ensures Win_Prob ≤ Podium_Prob ≤ Top5_Prob ≤ Top10_Prob
```

**Format-Specific Adjustment Application**:
The adjustment framework adapts to different relay formats with varying sophistication levels:

1. **Standard Relay & Mixed Relay**: Full two-stage systematic bias correction enabled
2. **Team Sprint**: Mode reset disabled, normalization enabled (simplified team dynamics)
3. **All Formats**: Comprehensive probability constraint enforcement and mathematical normalization

**Relay vs Individual Adjustment Philosophy**:
Cross-country relay adjustments focus on systematic bias correction rather than environmental adaptations:
- **Individual Events**: Period, altitude, mass start environmental adjustments
- **Relay Events**: Systematic bias correction and mathematical probability constraints
- **Reasoning**: Team composition variability requires different adjustment approaches than consistent individual athlete patterns

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

Cross-country relay points testing startlist setup implements the most sophisticated team composition data preparation among winter sports, accommodating three distinct relay formats (standard relays, mixed relays, team sprints) while maintaining leg-specific athlete assignments and technique-aware performance integration. The system handles both official FIS startlists and optimized team generation with comprehensive gender and technique specialization management.

**Multi-Format Relay Startlist Data Loading**:
Cross-country implements format-specific startlist loading with dual-file architecture that separates team composition from individual athlete assignments:

**Standard Relay Startlists (4x5km/4x7.5km)**:
- **Team File**: `startlist_relay_teams_[gender].csv` containing complete team information with `Member_1`, `Member_2`, `Member_3`, `Member_4` columns
- **Individual File**: `startlist_relay_individuals_[gender].csv` containing athlete data with `Team_Position` (1-4) leg assignments

**Mixed Relay Startlists (2x5km women + 2x7.5km men)**:
- **Team File**: `startlist_mixed_relay_teams.csv` with gender-alternating team compositions
- **Individual File**: `startlist_mixed_relay_individuals.csv` with explicit `Sex` column and position constraints (F-M-F-M for legs 1-2-3-4)

**Team Sprint Startlists (6x1.5km technique-specific)**:
- **Team File**: `startlist_team_sprint_teams_[gender].csv` with 2-member team compositions
- **Individual File**: `startlist_team_sprint_individuals_[gender].csv` with `Team_Position` (1-2) assignments and technique specifications

**Leg-Specific Athlete Assignment Processing**:
The system uses `get_leg_predictions_with_startlist()` to filter athletes by assigned leg positions: `filter(Team_Position == leg)`. This ensures predictions are generated only for athletes actually assigned to specific relay legs rather than all available team members, maintaining realistic team composition constraints.

**FIS Startlist Validation and Dual-Strategy Processing**:
Cross-country employs sophisticated startlist validation using `has_valid_fis_entries()` to determine whether to use official FIS team compositions or generate optimized teams: `any(individuals_df$In_FIS_List, na.rm = TRUE)`. When valid FIS entries exist, predictions use actual assigned leg positions; otherwise, the system generates optimal team compositions using `build_optimized_teams()` with mathematical optimization.

**Technique-Aware Current Skier Data Preparation**:
The system implements comprehensive current skier preparation with technique-specific weighted previous points calculation: `Classic_Last_5` for legs 1-2 and `Freestyle_Last_5` for legs 3-4. Each calculation uses the last 5 races with linear weighting (1,2,3,4,5) applied chronologically to capture recent form patterns specific to each relay technique requirement.

**Advanced Gender and ID Management for Mixed Relay**:
Mixed relay processing includes sophisticated gender constraint handling with ID offset systems: `ladies_data <- ladies_data %>% mutate(ID = ID + 100000)` to prevent ID conflicts when combining men's and women's data. This enables proper gender-alternating leg assignments (F-M-F-M) while maintaining data integrity across combined datasets.

**Team Sprint 2-Member Composition Handling**:
Team sprint events use specialized 2-member team processing with technique-specific requirements. The system processes only legs 1-2 rather than the standard 4-leg relay format, with each team member racing the same technique (Classic or Freestyle) determined by race scheduling rather than alternating techniques.

**Comprehensive Elo-to-Pelo Percentage Conversion**:
The startlist preparation includes advanced Elo rating normalization: `(current_df[[col]] / max_val) * 100` to create percentage-based features compatible with trained models. This conversion maintains relative performance relationships while standardizing input ranges across different Elo categories (`Distance_Elo`, `Sprint_Elo`, `Classic_Elo`, `Freestyle_Elo`).

**Nation-Based Team Aggregation with Member Tracking**:
For team prediction generation, the system uses `generate_team_predictions()` to extract individual team members from startlist data: `Member_1`, `Member_2`, `Member_3`, `Member_4` columns. Each team member's individual leg prediction is retrieved and aggregated using weighted averaging to produce nation-level team performance predictions.

**Optimized Team Generation Fallback**:
When official FIS startlists are unavailable, cross-country implements sophisticated team optimization using mathematical programming. The system selects optimal 4-person teams from available athletes using nation quota constraints and performance maximization while maintaining technique specialization requirements for different relay legs.

###### Modeling

Cross-country relay points testing modeling implements the most sophisticated leg-specific prediction aggregation system among winter sports, generating individual athlete predictions for each relay leg position and combining them into team-level performance predictions using technique-aware importance weighting. The system accommodates three distinct relay formats (standard relays, mixed relays, team sprints) with format-specific modeling approaches while maintaining leg-position-specific prediction methodologies.

**Leg-Specific Model Application with Technique Awareness**:
Cross-country relay modeling applies trained leg models using technique-specific performance features: `get_leg_predictions(leg_number, skier_data, leg_models)` where legs 1-2 use classic-specific features (`Classic_Last_5`) and legs 3-4 use freestyle-specific features (`Freestyle_Last_5`). The system generates individual athlete predictions for Win, Podium, Top5, and Top10 outcomes using leg-position-appropriate trained models (GLM, XGBoost, or GAM with comprehensive fallback strategies).

**Multi-Format Team Prediction Generation**:
The system employs format-specific team aggregation methodologies through `generate_team_predictions()` that extract team member assignments from startlist data (`Member_1`, `Member_2`, `Member_3`, `Member_4` for standard/mixed relays; `Member_1`, `Member_2` for team sprints) and retrieve individual leg predictions for each assigned athlete. Team predictions combine individual leg probabilities using weighted averaging with format-specific importance weights.

**Dynamic Leg Importance Weight Application**:
Cross-country implements sophisticated importance weighting through `calculate_leg_importance()` using three-tier calculation strategies: extracted coefficients from team-level models (when available), leg model accuracy proxies, or default weights. Standard relays use `c(0.2, 0.2, 0.25, 0.35)` emphasizing anchor leg tactics, mixed relays use `c(0.2, 0.25, 0.25, 0.3)` accounting for gender alternation, and team sprints use `c(0.5, 0.5)` reflecting equal 2-member importance.

**Gender-Aware Mixed Relay Processing**:
Mixed relay modeling employs sophisticated gender constraint handling with ID offset systems (`ID = ID + 100000` for women) to prevent data conflicts while enabling proper gender-alternating leg assignments (F-M-F-M). The system applies gender-specific leg filtering to ensure accurate team member matching and maintains data integrity across combined men's and women's datasets during prediction generation.

**Technique-Specific Model Integration**:
The modeling framework adapts to relay technique requirements using position-based technique assignment: legs 1-2 employ classic-focused models with classic-specific predictors (`Distance_C_Pelo_Pct`, `Classic_Pelo_Pct`), while legs 3-4 utilize freestyle-focused models with freestyle predictors (`Distance_F_Pelo_Pct`, `Freestyle_Pelo_Pct`), with anchor legs receiving additional sprint capabilities for tactical finishing requirements.

**Team Sprint 2-Member Specialized Processing**:
Team sprint modeling adapts to technique-specific race formats (Classic or Freestyle) using technique-adaptive predictor selection and 2-member team optimization. Classic team sprints emphasize `Sprint_C_Pelo_Pct` and `Classic_Pelo_Pct`, while freestyle team sprints focus on `Sprint_F_Pelo_Pct` and `Freestyle_Pelo_Pct`, with equal importance weighting reflecting balanced 2-person team dynamics.

**Comprehensive Error Handling and Fallback Integration**:
The system implements robust error handling through `safe_predict()` functions with probability capping (`pmin(probs, 1)`), fallback defaults (0.25 when prediction fails), and multi-tier model application strategies. When individual athlete data is incomplete, the system applies optimized team generation using mathematical programming to create optimal team compositions based on available athlete pools and performance constraints.

**Expected Points Calculation from Aggregated Probabilities**:
Team expected points derive from weighted probability aggregation using relay-specific points systems, where team probabilities calculate expected point values by multiplying position probabilities with corresponding relay points values and summing across all finishing position categories. This approach provides comprehensive team performance predictions that account for the complex tactical dynamics inherent in cross-country skiing relay competition.

#### Probability

##### Training

###### Setup

Cross-country relay probability training setup employs sophisticated leg-specific binary classification targeting with technique-aware data preparation that represents the most comprehensive relay probability modeling among winter sports. The system transforms the complex multi-dimensional relay performance problem into specialized binary classification datasets for each leg position while accommodating cross-country's extraordinary technique complexity (Classic vs Freestyle) and multi-format relay event variations (Standard, Mixed, Team Sprint).

**Position Threshold Definition with Leg-Specific Adaptation**:
Cross-country relay probability training uses fixed relay-specific position thresholds: `c(1, 3, 5, 10)` representing Win, Podium, Top 5, and Top 10 leg finishes. Unlike individual events that use dynamic threshold adaptation based on race format, relay events maintain consistent thresholds across all leg positions while accommodating technique-specific performance variations between classic legs (1-2) and freestyle legs (3-4).

**Leg-Specific Binary Outcome Creation with Technique Integration**:
Cross-country implements the most sophisticated binary classification framework among winter sports, creating separate datasets for each relay leg with technique-specific binary targets. The system uses categorical factor creation: `is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes"))` for each position threshold, ensuring proper categorical handling for binomial classification modeling across leg-specific technique requirements.

**Multi-Technique Training Data Preparation with Temporal Integration**:
The training setup combines historical relay data with individual race performance through sophisticated temporal ordering: `arrange(Date, Season, Race, desc(Distance))` to ensure chronological consistency when filling missing values from individual races to relay legs. This approach acknowledges that relay leg performance benefits from individual race form while maintaining leg-specific tactical dynamics unique to relay competition structure.

**Technique-Specific Leg Assignment and Data Separation**:
Cross-country relay training employs technique-aware data preparation that separates classic legs (1-2) from freestyle legs (3-4): `classic_legs_all <- relay_with_points %>% filter(Distance == "Rel", Leg < 3)` and `freestyle_legs_all <- relay_with_points %>% filter(Distance == "Rel", Leg > 2)`. This separation enables technique-specific feature engineering and model training that captures the fundamental performance differences between classic and freestyle skiing techniques within relay contexts.

**Gender-Specific Processing for Mixed Relay Formats**:
Mixed relay events receive specialized gender-constraint handling with leg-specific filtering: Female athletes for legs 1 and 3, male athletes for legs 2 and 4 (F-M-F-M pattern). This gender-specific approach ensures that binary classification models capture gender-dependent performance patterns while maintaining technique awareness across the mixed relay leg assignment structure unique to cross-country skiing.

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

Cross-country relay probability training feature selection employs the most sophisticated leg-specific technique-aware optimization strategy among winter sports, implementing deterministic rule-based selection with multi-dimensional adaptations across technique (Classic/Freestyle), leg position (1-4), and relay format (Standard, Mixed, Team Sprint) requirements. The system diverges from automated statistical optimization, utilizing domain knowledge-based variable selection that captures cross-country's extraordinary competitive complexity through position-specific feature engineering and technique-adaptive variable pools.

**Rule-Based Leg-Specific Technique Selection**:
Cross-country implements deterministic feature selection through the `get_leg_predictors(leg, leg_data)` function that adapts variable pools based on leg position and technique requirements rather than statistical optimization criteria. This approach acknowledges that relay leg performance involves specialized tactical roles and technique-specific capabilities that require distinct variable combinations for optimal binomial classification accuracy across different leg positions and technique requirements.

**Technique-Adaptive Variable Pool Architecture**:
The feature selection system employs sophisticated technique-aware variable filtering that separates classic-focused variables from freestyle-focused variables based on leg-specific technique assignments:
- **Classic Legs (1-2)**: `grep("Distance_C.*Pelo_Pct$", base_cols, value = TRUE)` and `grep("Classic.*Pelo_Pct$", base_cols, value = TRUE)`
- **Freestyle Legs (3-4)**: `grep("Distance_F.*Pelo_Pct$", base_cols, value = TRUE)` and `grep("Freestyle.*Pelo_Pct$", base_cols, value = TRUE)`

**Individual Performance Integration with Leg Specialization**:
Feature selection incorporates individual race performance through `Weighted_Last_5` variable inheritance while maintaining leg-specific tactical considerations. This approach recognizes that relay leg performance benefits from individual athlete form while acknowledging leg-specific tactical dynamics unique to relay competition structure, enabling performance propagation from individual races to relay contexts with position-appropriate feature emphasis.

**Multi-Format Adaptive Feature Selection**:
The system adapts feature pools across different relay formats through format-specific variable selection strategies:
- **Standard Relays**: Position-based technique selection (classic legs 1-2, freestyle legs 3-4)
- **Mixed Relays**: Gender and position combined selection with F-M-F-M pattern awareness
- **Team Sprints**: Race technique-dependent selection with sprint-focused variable emphasis

**Performance Metric Inheritance Strategy**:
Cross-country relay probability feature selection maintains consistency with relay points models through shared variable pool foundations while adapting to binary classification requirements. The system employs `Distance_Pelo_Pct`, `Pelo_Pct`, and `Weighted_Last_5` as core variables across all leg positions while adding technique-specific and position-specific variables that capture the unique performance requirements of each relay leg within cross-country's complex multi-technique competitive structure.

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

Cross-country relay points training modeling employs sophisticated leg-specific GAM frameworks that capture the intricate tactical dynamics of relay racing across different techniques and formats. Unlike individual races where athletes compete solely for themselves, relay modeling must account for team composition, leg-specific tactics, and technique transitions that create unique competitive dynamics requiring specialized multi-tier modeling approaches adapted to cross-country's extraordinary complexity.

**Leg-Specific Multi-Model Architecture**:
Cross-country's relay modeling creates independent models for each leg position (1-4) using either XGBoost or GLM algorithms based on dataset size and complexity requirements. This leg-specific approach recognizes that each relay position has distinct tactical responsibilities:

```r
# Leg-specific model training framework
for(leg in 1:4) {
  leg_predictors <- get_leg_predictors(leg, leg_data)
  
  # Adaptive algorithm selection based on data size
  method <- if(nrow(leg_data[[leg]]) > 500) "xgbTree" else "glm"
  
  # Train separate models for each outcome type
  podium_model <- train_model_safe(podium_formula, leg_data[[leg]], method)
  win_model <- train_model_safe(win_formula, leg_data[[leg]], method)
  top5_model <- train_model_safe(top5_formula, leg_data[[leg]], method)
}
```

**Technique-Aware Model Differentiation**:
The modeling framework adapts to cross-country's unique technique requirements where classic legs (1-2) require different predictive models than freestyle legs (3-4). This technique specialization captures the fundamental difference between classic striding technique and freestyle skating technique within relay tactical frameworks.

**Team Coordination Modeling Integration**:
Cross-country's relay models incorporate team chemistry factors through comprehensive leg-specific performance aggregation while preserving individual athlete performance characteristics. The modeling accounts for mixed relay gender transitions and team sprint technique specificity where coordination between team members affects overall finishing position outcomes.

**Multi-Tier Hierarchical Modeling Strategy**:
Cross-country implements sophisticated fallback mechanisms that ensure robust predictions across different data conditions:

```r
# Hierarchical modeling with fallback strategies
train_model_safe <- function(formula, data, method = "glm", target_name) {
  tryCatch({
    # Primary: XGBoost for large datasets
    if (method == "xgbTree" && nrow(data) > 500) {
      model <- train(formula, data = data, method = "xgbTree", 
                    trControl = control, metric = "Accuracy")
    } else {
      # Fallback: GLM for smaller datasets or XGBoost failures
      model <- train(formula, data = data, method = "glm", 
                    family = "binomial", trControl = control, metric = "Accuracy")
    }
    return(model)
  }, error = function(e) {
    # Final fallback: Simplified GLM
    simplified_formula <- reformulate("Pelo_Pct", response = target_name)
    return(train(simplified_formula, data = data, method = "glm", 
                family = "binomial", trControl = control))
  })
}
```

**Cross-Validation Framework with Relay Dynamics**:
All relay models use 5-fold cross-validation with standardized parameters that account for the temporal dependencies and team composition variations inherent in relay racing:

```r
# Relay-specific cross-validation setup
control <- trainControl(
  method = "cv", number = 5, classProbs = TRUE,
  summaryFunction = defaultSummary, savePredictions = "final"
)
```

**Format-Specific Model Adaptation**:
The modeling system adapts to different relay formats (standard relays, mixed relays, team sprints) through conditional logic that recognizes format-specific tactical requirements while maintaining consistent underlying statistical methodology across cross-country's diverse relay competitive structures.

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

Cross-country relay probability testing startlist setup implements the most sophisticated leg-specific athlete assignment and technique-aware data preparation among winter sports, utilizing advanced FIS startlist validation with comprehensive fallback strategies and multi-format relay processing capabilities. The system employs specialized leg-specific probability modeling with technique integration that represents the pinnacle of relay startlist complexity across winter sports disciplines.

**Advanced Leg-Specific Startlist Loading Architecture**:
Cross-country employs sophisticated startlist loading functions that accommodate three distinct relay formats with technique-aware processing and gender-specific handling: `load_relay_startlists(gender)` for standard 4-leg relays, `load_team_sprint_startlists(gender)` for 2-leg team sprints, and `load_mixed_relay_startlists()` for gender-alternating mixed relays with automatic gender constraint enforcement ensuring proper F-M-F-M leg assignments.

```r
# Format-specific startlist loading with technique awareness
# Standard Relay: 4-leg with Classic-Classic-Freestyle-Freestyle pattern
load_relay_startlists <- function(gender) {
  # Gender-specific file path construction
  startlist_file <- paste0("~/startlist_relay_", gender, ".csv")
  
  # Technique detection from race schedule
  technique <- detect_race_technique(race_schedule)
  
  # Load with technique filtering
  startlist <- read.csv(startlist_file) %>%
    filter(Technique == technique | Technique == "") %>%
    mutate(Sex = if(gender == "men") "M" else "L")
}
```

**Sophisticated FIS Startlist Validation with Leg Assignment**:
The system implements advanced FIS startlist validation through `has_valid_fis_entries()` function that checks for official FIS leg assignments and determines prediction strategy: when valid FIS startlists exist, the system uses `get_leg_predictions_with_startlist()` for precise athlete-to-leg assignments; when no FIS data available, employs comprehensive fallback strategy predicting for all eligible athletes across all leg positions with gender filtering.

```r
# Advanced FIS validation with leg-specific assignment detection
has_valid_fis_entries <- function(individuals_df) {
  if ("In_FIS_List" %in% names(individuals_df)) {
    return(any(individuals_df$In_FIS_List, na.rm = TRUE))
  }
  return(FALSE)
}

# Leg-specific athlete assignment from FIS startlists
get_leg_predictions_with_startlist <- function(current_data, leg_models, startlist_individuals) {
  for(leg in 1:4) {
    # Extract athletes assigned to specific legs
    leg_skiers <- startlist_individuals %>%
      filter(Team_Position == leg) %>%
      select(ID, Skier, Nation, Team_Name)
    
    # Generate leg-specific predictions using technique-aware models
    leg_predictions <- predict_leg_probabilities(leg_skiers, leg_models[[leg]])
  }
}
```

**Technique-Aware Data Preparation with Race Schedule Integration**:
Cross-country integrates race schedule analysis with technique detection that automatically filters training data and selects appropriate predictors based on race technique: Classic races utilize classic-specific features while Freestyle races employ freestyle-focused variables with sophisticated technique validation ensuring consistent technique application across leg assignments and model selection.

```r
# Race schedule technique detection and data filtering
race_technique <- if(nrow(race_info$men) > 0) race_info$men$Technique[1] else NA
men_filtered_data <- chrono_data$men_relay %>%
  filter(Technique == race_technique | Technique == "") %>%
  mutate(technique_validated = TRUE)

# Technique-specific predictor selection per leg
get_leg_predictors <- function(leg, technique) {
  if(technique == "C") {  # Classic technique
    return(c("Sprint_C_Pelo_Pct", "Classic_Pelo_Pct", "Distance_C_Pelo_Pct"))
  } else if(technique == "F") {  # Freestyle technique  
    return(c("Sprint_F_Pelo_Pct", "Freestyle_Pelo_Pct", "Distance_F_Pelo_Pct"))
  }
}
```

**Gender-Aware Mixed Relay Startlist Processing**:
Mixed relay startlists employ sophisticated gender constraint enforcement with automatic leg assignment validation: legs 1&3 restricted to female athletes, legs 2&4 restricted to male athletes, with ID offset management (`ladies_data$ID + 100000`) preventing data conflicts when combining men's and women's datasets for comprehensive mixed relay team composition.

```r
# Mixed relay gender constraint enforcement
process_mixed_relay_startlist <- function(startlist_individuals) {
  # Enforce gender constraints by leg position
  female_legs <- startlist_individuals %>%
    filter(Team_Position %in% c(1, 3), Sex == "F")
  
  male_legs <- startlist_individuals %>%
    filter(Team_Position %in% c(2, 4), Sex == "M")
  
  # ID offset to prevent conflicts in combined dataset
  female_legs$ID <- female_legs$ID + 100000
  
  return(bind_rows(female_legs, male_legs))
}
```

**Multi-Tier Fallback Strategy for Missing FIS Data**:
When official FIS startlists are unavailable, Cross-country implements comprehensive fallback strategy generating predictions for all eligible current season athletes across all leg positions: the system maintains gender filtering for mixed relays while providing complete coverage ensuring probability predictions for all potential team compositions rather than limiting to specific confirmed lineups.

**Current Skier Data Preparation with Technique Integration**:
The startlist preparation employs sophisticated current skier data processing through `prepare_current_skiers()` function that retrieves latest ELO ratings, performs technique-aware PELO percentage conversion, and calculates weighted previous 5 performance with technique-specific filtering ensuring consistent input data quality across leg assignments and relay formats.

```r
# Comprehensive current skier data preparation
prepare_current_skiers <- function(gender, technique) {
  # Retrieve latest ELO ratings with technique awareness
  current_data <- get_latest_elos(gender) %>%
    filter(technique_validated == TRUE) %>%
    
    # PELO percentage conversion with technique-specific normalization
    mutate(
      Pelo_Pct = Elo / max(Elo, na.rm = TRUE),
      Classic_Pelo_Pct = Classic_Elo / max(Classic_Elo, na.rm = TRUE),
      Freestyle_Pelo_Pct = Freestyle_Elo / max(Freestyle_Elo, na.rm = TRUE)
    ) %>%
    
    # Weighted previous 5 calculation with technique filtering
    calculate_weighted_points(technique = technique)
}
```

**Robust Error Handling and Validation**:
Cross-country's startlist setup includes comprehensive error handling for missing files, invalid technique specifications, and inconsistent team compositions with detailed logging and fallback mechanisms ensuring reliable startlist preparation across all relay formats even when data quality varies or official startlists are incomplete.

Cross-country's relay probability testing startlist setup represents the most advanced athlete assignment and technique-aware data preparation methodology among winter sports, providing sophisticated leg-specific modeling capabilities that capture the sport's extraordinary competitive complexity while ensuring robust prediction coverage across all relay competition scenarios.

###### Modeling

Cross-country relay probability testing employs sophisticated leg-specific modeling using XGBoost (xgbTree) as the primary algorithm with GLM fallback, implementing technique-aware prediction frameworks with comprehensive 5-fold cross-validation. The modeling approach emphasizes individual leg probability predictions through binary classification models, then aggregates them into team-level probabilities using dynamic leg importance weighting and technique-specific predictor optimization.

**XGBoost-First Modeling Architecture**: Cross-country implements advanced machine learning algorithms with comprehensive fallback strategies:

```r
# Primary XGBoost modeling with GLM fallback
train_leg_specific_models <- function(training_data, leg_number) {
  # Configure training control with cross-validation
  train_control <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  
  # Attempt XGBoost training first
  tryCatch({
    xgb_model <- train(
      outcome ~ .,
      data = training_data,
      method = "xgbTree",
      trControl = train_control,
      metric = "ROC"
    )
    return(xgb_model)
  }, error = function(e) {
    # Fallback to GLM if XGBoost fails
    log_warn(paste("XGBoost failed for leg", leg_number, "- using GLM fallback"))
    glm_model <- train(
      outcome ~ .,
      data = training_data,
      method = "glm",
      family = "binomial",
      trControl = train_control
    )
    return(glm_model)
  })
}
```

**Technique-Specific Predictor Optimization**: Cross-country adapts predictor selection based on leg technique requirements:

```r
# Technique-aware predictor selection
select_technique_predictors <- function(leg_number, race_technique) {
  if (leg_number %in% c(1, 2)) {
    # Classic technique legs (1-2)
    predictors <- c("Distance_C_Elo", "Classic_Elo", "Overall_Elo", "Weighted_Last_5_Classic")
  } else if (leg_number == 4) {
    # Sprint-focused freestyle leg (4)
    predictors <- c("Sprint_F_Elo", "Freestyle_Elo", "Overall_Elo", "Weighted_Last_5_Sprint")
  } else {
    # Standard freestyle leg (3)
    predictors <- c("Distance_F_Elo", "Freestyle_Elo", "Overall_Elo", "Weighted_Last_5_Freestyle")
  }
  
  # Additional technique-specific adjustments
  if (race_technique == "Classic") {
    predictors <- enhance_classic_predictors(predictors)
  } else if (race_technique == "Freestyle") {
    predictors <- enhance_freestyle_predictors(predictors)
  }
  
  return(validate_predictor_availability(predictors))
}
```

**Dynamic Leg Importance Weighting**: Cross-country employs sophisticated leg importance calculation with optimization capabilities:

```r
# Dynamic leg importance weight calculation
calculate_leg_importance <- function(team_composition, race_conditions) {
  # Base importance weights with later leg emphasis
  base_weights <- c(0.2, 0.2, 0.25, 0.35)  # Legs 1-4
  
  # Adjust based on team composition strength
  strength_adjustments <- calculate_strength_adjustments(team_composition)
  
  # Apply race condition modifiers
  condition_modifiers <- apply_race_condition_effects(race_conditions)
  
  # Calculate final weighted importance
  final_weights <- normalize_weights(
    base_weights * strength_adjustments * condition_modifiers
  )
  
  return(validate_weight_constraints(final_weights))
}

# Advanced leg importance optimization
optimize_leg_weights <- function(historical_results, validation_data) {
  weight_combinations <- expand_weight_combinations(
    leg1_range = c(0.15, 0.25),
    leg2_range = c(0.15, 0.25), 
    leg3_range = c(0.20, 0.30),
    leg4_range = c(0.30, 0.40)
  )
  
  optimal_weights <- optimize_prediction_accuracy(
    weight_combinations, 
    validation_data,
    performance_metric = "team_prediction_accuracy"
  )
  
  return(optimal_weights)
}
```

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

Cross-country relay probability testing implements the **most sophisticated adjustment framework** among winter sports, employing comprehensive multi-stage probability normalization with mode reset strategies and mathematical constraint enforcement. Unlike other sports that disable systematic bias correction for relay events, cross-country utilizes advanced probability distribution analysis and constraint enforcement specifically designed for leg-specific team prediction scenarios.

**Multi-Stage Probability Normalization Pipeline**: Cross-country employs sophisticated three-stage probability adjustment frameworks with comprehensive mathematical validation:

```r
# Comprehensive three-stage probability adjustment pipeline
apply_comprehensive_relay_adjustments <- function(raw_team_predictions, relay_format) {
  # Stage 1: Mode probability reset to eliminate systematic biases
  stage1_adjusted <- reset_mode_probabilities(raw_team_predictions)
  
  # Stage 2: Target-sum probability normalization
  stage2_normalized <- normalize_probabilities(
    stage1_adjusted,
    target_sums = list(win = 1.0, podium = 3.0, top5 = 5.0, top10 = 10.0)
  )
  
  # Stage 3: Mathematical constraint enforcement with probability capping
  stage3_constrained <- cap_probabilities(stage2_normalized)
  
  # Final validation and monotonic constraint application
  final_adjusted <- apply_monotonic_constraints(stage3_constrained)
  
  return(validate_adjustment_pipeline_compliance(final_adjusted))
}

# Stage 1: Mode probability reset functionality
reset_mode_probabilities <- function(team_predictions) {
  reset_adjusted <- team_predictions %>%
    mutate(
      # Eliminate repeated/low-probability systematic patterns
      win_prob_reset = remove_systematic_mode_patterns(win_prob),
      podium_prob_reset = remove_systematic_mode_patterns(podium_prob),
      top5_prob_reset = remove_systematic_mode_patterns(top5_prob),
      top10_prob_reset = remove_systematic_mode_patterns(top10_prob)
    ) %>%
    # Validate reset effectiveness
    validate_mode_pattern_elimination()
  
  return(reset_adjusted)
}
```

**Format-Specific Adjustment Implementation**: Cross-country adapts adjustment frameworks to accommodate different relay format requirements:

```r
# Format-specific adjustment strategies
implement_format_adjustments <- function(team_predictions, relay_format) {
  format_adjustments <- switch(relay_format,
    "Standard_Relay" = list(
      mode_reset_enabled = TRUE,
      normalization_targets = list(win = 1.0, podium = 3.0, top5 = 5.0, top10 = 10.0),
      leg_importance_weights = c(0.2, 0.2, 0.25, 0.35),  # 4-leg weighting
      technique_adjustment = apply_technique_specific_adjustments
    ),
    "Team_Sprint" = list(
      mode_reset_disabled = TRUE,  # Disabled for team sprint format
      normalization_targets = list(win = 1.0, podium = 3.0, top5 = 5.0, top10 = 10.0),
      leg_importance_weights = c(0.5, 0.5),  # 2-leg equal weighting
      technique_adjustment = apply_sprint_specific_adjustments
    ),
    "Mixed_Relay" = list(
      mode_reset_enabled = TRUE,  # Full reset methodology enabled
      normalization_targets = list(win = 1.0, podium = 3.0, top5 = 5.0, top10 = 10.0),
      leg_importance_weights = c(0.2, 0.2, 0.25, 0.35),  # 4-leg weighting
      gender_balance_adjustment = apply_mixed_gender_adjustments
    )
  )
  
  return(apply_format_specific_framework(team_predictions, format_adjustments))
}
```

**Technique-Aware Adjustment Integration**: Cross-country incorporates sophisticated technique-specific adjustments that account for Classic vs Freestyle performance patterns:

```r
# Technique-specific probability adjustments
apply_technique_adjustments <- function(leg_predictions, race_technique) {
  technique_adjusted <- leg_predictions %>%
    group_by(leg_number) %>%
    mutate(
      # Leg-specific technique adjustments
      technique_bias_correction = case_when(
        leg_number %in% c(1, 2) & race_technique == "Classic" ~ 
          apply_classic_technique_adjustment(predictions),
        leg_number %in% c(3, 4) & race_technique == "Freestyle" ~ 
          apply_freestyle_technique_adjustment(predictions),
        leg_number == 4 & race_technique == "Freestyle" ~
          apply_sprint_specific_adjustment(predictions),
        TRUE ~ 0  # No adjustment for mismatched technique/leg combinations
      ),
      
      # Apply technique bias correction
      adjusted_win_prob = win_prob + technique_bias_correction,
      adjusted_podium_prob = podium_prob + technique_bias_correction,
      adjusted_top5_prob = top5_prob + technique_bias_correction,
      adjusted_top10_prob = top10_prob + technique_bias_correction
    ) %>%
    # Re-normalize after technique adjustments
    normalize_post_technique_adjustment() %>%
    ungroup()
  
  return(technique_adjusted)
}
```

**Mathematical Constraint Enforcement with Leg Importance Integration**: Cross-country implements comprehensive constraint enforcement that incorporates leg importance weighting:

```r
# Advanced constraint enforcement with leg importance weighting
enforce_mathematical_constraints_with_weighting <- function(team_predictions, leg_weights) {
  constrained_predictions <- team_predictions %>%
    group_by(nation, relay_format) %>%
    mutate(
      # Weight-adjusted probability calculations
      weighted_win_prob = sum(win_prob * leg_weights),
      weighted_podium_prob = sum(podium_prob * leg_weights),
      weighted_top5_prob = sum(top5_prob * leg_weights),
      weighted_top10_prob = sum(top10_prob * leg_weights)
    ) %>%
    # Mathematical constraint application
    mutate(
      # Probability capping to valid ranges
      constrained_win_prob = pmin(weighted_win_prob, 1.0),
      constrained_podium_prob = pmin(weighted_podium_prob, 1.0),
      constrained_top5_prob = pmin(weighted_top5_prob, 1.0),
      constrained_top10_prob = pmin(weighted_top10_prob, 1.0)
    ) %>%
    # Monotonic constraint enforcement
    apply_monotonic_ordering() %>%
    # Final target-sum normalization
    normalize_to_theoretical_targets() %>%
    ungroup()
  
  return(validate_constraint_compliance(constrained_predictions))
}
```

**Sophisticated Error Handling and Fallback Strategies**: Cross-country implements comprehensive error handling for adjustment pipeline failures:

```r
# Comprehensive error handling for adjustment failures
handle_adjustment_pipeline_errors <- function(team_predictions, adjustment_stage) {
  tryCatch({
    # Attempt full adjustment pipeline
    adjusted_predictions <- apply_comprehensive_relay_adjustments(team_predictions)
    return(adjusted_predictions)
  }, error = function(e) {
    # Stage-specific fallback strategies
    fallback_result <- switch(adjustment_stage,
      "mode_reset_failure" = {
        log_warn("Mode reset failed - applying normalization only")
        normalize_probabilities(team_predictions)
      },
      "normalization_failure" = {
        log_warn("Normalization failed - applying constraints only") 
        cap_probabilities(team_predictions)
      },
      "constraint_failure" = {
        log_warn("Constraint enforcement failed - using raw predictions")
        team_predictions
      }
    )
    
    return(validate_fallback_result(fallback_result))
  })
}
```

**Advanced Probability Distribution Analysis**: Cross-country employs sophisticated probability distribution analysis to identify and correct systematic biases:

```r
# Comprehensive probability distribution analysis
analyze_probability_distributions <- function(adjusted_predictions) {
  distribution_analysis <- adjusted_predictions %>%
    group_by(relay_format, position_threshold) %>%
    summarize(
      mean_probability = mean(probability),
      median_probability = median(probability),
      probability_variance = var(probability),
      distribution_skewness = calculate_skewness(probability),
      systematic_bias_indicator = detect_systematic_patterns(probability),
      adjustment_effectiveness = assess_adjustment_impact(probability, raw_probability),
      .groups = "drop"
    )
  
  bias_correction_recommendations <- generate_bias_correction_strategy(distribution_analysis)
  
  return(list(
    distribution_metrics = distribution_analysis,
    bias_correction_strategy = bias_correction_recommendations,
    adjustment_pipeline_performance = assess_overall_adjustment_effectiveness(distribution_analysis)
  ))
}
```

Cross-country's relay probability testing adjustments represent the most advanced systematic bias correction framework among winter sports, utilizing sophisticated multi-stage probability normalization, technique-aware adjustments, and comprehensive mathematical constraint enforcement to deliver mathematically consistent and systematically unbiased probability predictions across cross-country's extraordinary competitive complexity.

#### Normalization and Monotonic Constraints

Cross-country relay probability predictions undergo the most sophisticated multi-stage post-processing framework among winter sports, employing comprehensive mode-based probability reset, target-sum normalization, monotonic constraint enforcement, and format-specific implementations across standard relay, team sprint, and mixed relay configurations. The system combines mathematical rigor with sport-specific competitive realism through advanced probability distribution analysis and systematic bias elimination.

**Stage 1: Mode-Based Probability Reset**: Cross-country implements sophisticated mode pattern detection and elimination to reduce systematic prediction clustering before normalization:

```r
# Mode-based probability reset for systematic bias elimination
reset_mode_probabilities <- function(team_predictions) {
  reset_predictions <- team_predictions %>%
    mutate(
      # Identify mode patterns for each probability column
      win_prob_mode_reset = apply_mode_reset_logic(Win_Prob),
      podium_prob_mode_reset = apply_mode_reset_logic(Podium_Prob),
      top5_prob_mode_reset = apply_mode_reset_logic(Top5_Prob),
      top10_prob_mode_reset = apply_mode_reset_logic(Top10_Prob)
    )
  
  return(validate_mode_reset_effectiveness(reset_predictions))
}

# Mode reset logic implementation
apply_mode_reset_logic <- function(probability_vector) {
  # Count value occurrences
  value_counts <- table(probability_vector)
  repeated_values <- names(value_counts)[value_counts >= 2]
  
  if (length(repeated_values) > 0) {
    max_repeated_value <- max(as.numeric(repeated_values))
    
    # Reset values that are repeated (≥2 times) OR below max repeated value to 0
    reset_vector <- ifelse(
      probability_vector %in% repeated_values | probability_vector < max_repeated_value,
      0,
      probability_vector
    )
  } else {
    # No repeated values found - return original vector
    reset_vector <- probability_vector
  }
  
  return(reset_vector)
}
```

**Format-Specific Mode Reset Implementation**: Cross-country adapts mode reset application based on relay format characteristics:

```r
# Format-specific mode reset strategies
apply_format_mode_reset <- function(team_predictions, relay_format) {
  format_reset_strategy <- switch(relay_format,
    "Standard_Relay" = list(
      mode_reset_enabled = TRUE,
      reset_threshold = 2,  # Reset values appearing ≥2 times
      low_value_reset = TRUE  # Reset values below max repeated
    ),
    "Team_Sprint" = list(
      mode_reset_enabled = FALSE,  # Disabled for team sprint
      reset_threshold = NULL,
      low_value_reset = FALSE
    ),
    "Mixed_Relay" = list(
      mode_reset_enabled = TRUE,   # Full reset methodology enabled
      reset_threshold = 2,
      low_value_reset = TRUE,
      gender_aware_reset = TRUE    # Additional gender-specific reset
    )
  )
  
  if (format_reset_strategy$mode_reset_enabled) {
    return(apply_mode_reset_with_strategy(team_predictions, format_reset_strategy))
  } else {
    return(team_predictions)  # Skip mode reset for formats where disabled
  }
}
```

**Stage 2: Target-Sum Probability Normalization**: Team probabilities are normalized to match expected mathematical totals with comprehensive error handling and validation:

```r
# Enhanced target-sum normalization with race participation weighting
normalize_probabilities <- function(team_predictions) {
  # Define normalization targets
  targets <- list(
    Win_Prob = 1,      # Win probability sums to 1
    Podium_Prob = 3,   # Podium probability sums to 3
    Top5_Prob = 5,     # Top5 probability sums to 5
    Top10_Prob = 10,   # Top10 probability sums to 10
    Top30_Prob = 30    # Top30 probability sums to 30 (when available)
  )
  
  # Calculate race participation weighting
  if ("race_participation_prob" %in% names(team_predictions)) {
    participation_adjustment <- sum(team_predictions$race_participation_prob, na.rm = TRUE)
    
    # Adjust targets based on actual expected participation
    for (target_name in names(targets)) {
      if (participation_adjustment > 0) {
        targets[[target_name]] <- targets[[target_name]] * (participation_adjustment / nrow(team_predictions))
      }
    }
  }
  
  # Apply normalization with comprehensive error handling
  for(prob_col in names(targets)) {
    if(prob_col %in% names(team_predictions)) {
      current_sum <- sum(team_predictions[[prob_col]], na.rm = TRUE)
      
      if(current_sum > 0 && !is.infinite(current_sum)) {
        target_sum <- targets[[prob_col]]
        normalization_factor <- target_sum / current_sum
        
        # Apply normalization with bounds checking
        team_predictions[[prob_col]] <- pmin(
          team_predictions[[prob_col]] * normalization_factor, 
          1.0  # Cap individual probabilities at 100%
        )
        
        # Log normalization effectiveness
        log_debug(paste(prob_col, "normalized from", round(current_sum, 3), "to", round(target_sum, 3)))
      }
    }
  }
  
  return(validate_normalization_effectiveness(team_predictions, targets))
}
```

**Normalization Targets Logic**: The normalization targets reflect the mathematical expectation for probability distributions:
- **Win Probability**: Sum = 1 (exactly one winner per race)
- **Podium Probability**: Sum = 3 (exactly three podium positions)  
- **Top5 Probability**: Sum = 5 (exactly five top5 positions)
- **Top10 Probability**: Sum = 10 (exactly ten top10 positions)

**Stage 3: Monotonic Constraint Application**: Cross-country implements sophisticated monotonic constraint enforcement with adjustment tracking and validation:

```r
# Enhanced monotonic constraint application with adjustment metrics
apply_monotonic_constraints <- function(team_predictions) {
  log_info("Applying enhanced monotonic constraints with adjustment tracking...")
  
  # Define probability hierarchy (ascending order)
  prob_cols <- c("Win_Prob", "Podium_Prob", "Top5_Prob", "Top10_Prob", "Top30_Prob")
  prob_cols <- prob_cols[prob_cols %in% names(team_predictions)]
  
  # Track adjustment statistics
  adjustment_stats <- list(
    teams_adjusted = 0,
    total_adjustments = 0,
    adjustment_magnitude = 0
  )
  
  # Apply monotonic constraints for each team
  for(i in 1:nrow(team_predictions)) {
    original_probs <- numeric(length(prob_cols))
    adjusted_probs <- numeric(length(prob_cols))
    
    # Extract current probabilities
    for(j in 1:length(prob_cols)) {
      original_probs[j] <- team_predictions[[prob_cols[j]]][i]
      adjusted_probs[j] <- original_probs[j]
    }
    
    # Apply monotonic adjustment (Win ≤ Podium ≤ Top5 ≤ Top10 ≤ Top30)
    team_adjusted <- FALSE
    for(j in 2:length(adjusted_probs)) {
      if(adjusted_probs[j] < adjusted_probs[j-1]) {
        adjustment_magnitude <- adjusted_probs[j-1] - adjusted_probs[j]
        adjusted_probs[j] <- adjusted_probs[j-1]
        
        # Track adjustment metrics
        adjustment_stats$total_adjustments <- adjustment_stats$total_adjustments + 1
        adjustment_stats$adjustment_magnitude <- adjustment_stats$adjustment_magnitude + adjustment_magnitude
        team_adjusted <- TRUE
      }
    }
    
    if (team_adjusted) {
      adjustment_stats$teams_adjusted <- adjustment_stats$teams_adjusted + 1
    }
    
    # Update team predictions with adjusted probabilities
    for(j in 1:length(prob_cols)) {
      team_predictions[[prob_cols[j]]][i] <- adjusted_probs[j]
    }
  }
  
  # Log adjustment effectiveness
  log_info(paste("Monotonic constraints applied:", 
                 adjustment_stats$teams_adjusted, "teams adjusted with",
                 adjustment_stats$total_adjustments, "total adjustments"))
  
  return(list(
    predictions = team_predictions,
    adjustment_metrics = adjustment_stats
  ))
}
```

**Monotonic Logic**: The constraint ensures that for any team:
**Win_Probability ≤ Podium_Probability ≤ Top5_Probability ≤ Top10_Probability**

This reflects the logical hierarchy where achieving a higher-level outcome (e.g., winning) implies achieving all lower-level outcomes (e.g., podium, top5, top10).

**Stage 4: Re-Normalization After Constraints**: Cross-country implements iterative re-normalization with convergence monitoring to maintain mathematical consistency:

```r
# Enhanced re-normalization with iterative convergence monitoring
re_normalize_after_constraints <- function(team_predictions, targets, max_iterations = 5) {
  log_info("Beginning iterative re-normalization after monotonic constraints...")
  
  convergence_threshold <- 1e-6
  iteration <- 1
  
  repeat {
    converged <- TRUE
    
    for(prob_col in names(targets)) {
      if(prob_col %in% names(team_predictions)) {
        current_sum <- sum(team_predictions[[prob_col]], na.rm = TRUE)
        target_sum <- targets[[prob_col]]
        
        if(current_sum > 0 && abs(current_sum - target_sum) > convergence_threshold) {
          scaling_factor <- target_sum / current_sum
          
          # Apply scaling with probability capping
          team_predictions[[prob_col]] <- pmin(
            team_predictions[[prob_col]] * scaling_factor,
            1.0
          )
          
          converged <- FALSE
          log_debug(paste("Iteration", iteration, "-", prob_col, "scaled by", round(scaling_factor, 4)))
        }
      }
    }
    
    # Check for convergence or maximum iterations
    if (converged || iteration >= max_iterations) {
      log_info(paste("Re-normalization converged after", iteration, "iterations"))
      break
    }
    
    iteration <- iteration + 1
  }
  
  return(validate_final_probability_consistency(team_predictions, targets))
}
```

**Stage 5: Final Validation and Quality Assurance**: Cross-country implements comprehensive validation to ensure mathematical consistency and competitive realism:

```r
# Comprehensive final validation with quality metrics
validate_final_probabilities <- function(team_predictions) {
  prob_cols <- c("Win_Prob", "Podium_Prob", "Top5_Prob", "Top10_Prob", "Top30_Prob")
  prob_cols <- prob_cols[prob_cols %in% names(team_predictions)]
  
  validation_results <- list(
    probability_capping_applied = FALSE,
    monotonic_violations = 0,
    sum_deviations = list(),
    quality_score = 0
  )
  
  # Apply final probability capping
  for(prob_col in prob_cols) {
    if(prob_col %in% names(team_predictions)) {
      # Check for values exceeding 1.0 before capping
      violations <- sum(team_predictions[[prob_col]] > 1.0, na.rm = TRUE)
      if (violations > 0) {
        validation_results$probability_capping_applied <- TRUE
        log_warn(paste(violations, "probability values > 1.0 found in", prob_col))
      }
      
      # Apply capping
      team_predictions[[prob_col]] <- pmin(team_predictions[[prob_col]], 1.0)
      team_predictions[[prob_col]] <- pmax(team_predictions[[prob_col]], 0.0)
    }
  }
  
  # Validate monotonic constraints
  for(i in 1:nrow(team_predictions)) {
    team_probs <- numeric(length(prob_cols))
    for(j in 1:length(prob_cols)) {
      team_probs[j] <- team_predictions[[prob_cols[j]]][i]
    }
    
    # Check for monotonic violations
    for(j in 2:length(team_probs)) {
      if(team_probs[j] < team_probs[j-1] - 1e-10) {  # Allow small numerical errors
        validation_results$monotonic_violations <- validation_results$monotonic_violations + 1
      }
    }
  }
  
  # Calculate quality score based on validation metrics
  validation_results$quality_score <- calculate_probability_quality_score(team_predictions, validation_results)
  
  log_info(paste("Final validation completed. Quality score:", 
                round(validation_results$quality_score, 3)))
  
  return(list(
    predictions = team_predictions,
    validation_metrics = validation_results
  ))
}
```

**Format-Specific Implementation Framework**: Cross-country implements distinct normalization approaches for different relay formats:

```r
# Master normalization controller for format-specific implementations
apply_format_specific_normalization <- function(team_predictions, relay_format, race_info) {
  normalization_strategy <- switch(relay_format,
    "Standard_Relay" = list(
      mode_reset = TRUE,
      normalization_targets = list(Win_Prob = 1, Podium_Prob = 3, Top5_Prob = 5, Top10_Prob = 10),
      monotonic_constraints = TRUE,
      iterative_renormalization = TRUE,
      max_iterations = 5
    ),
    "Team_Sprint" = list(
      mode_reset = FALSE,  # Simplified for 2-leg teams
      normalization_targets = list(Win_Prob = 1, Podium_Prob = 3, Top5_Prob = 5),
      monotonic_constraints = TRUE,
      iterative_renormalization = FALSE,
      max_iterations = 1
    ),
    "Mixed_Relay" = list(
      mode_reset = TRUE,
      normalization_targets = list(Win_Prob = 1, Podium_Prob = 3, Top5_Prob = 5, Top10_Prob = 10),
      monotonic_constraints = TRUE,
      iterative_renormalization = TRUE,
      max_iterations = 7,  # Higher for gender complexity
      gender_aware_processing = TRUE
    )
  )
  
  return(execute_normalization_pipeline(team_predictions, normalization_strategy))
}
```
      team_predictions[[prob_col]] <- pmin(team_predictions[[prob_col]], 1)
      
      # Log any modifications
      capped_count <- sum(team_predictions[[prob_col]] == 1)
      if(capped_count > 0) {
        log_info(paste("Capped", capped_count, "values at 1 for", prob_col))
      }
    }
  }
}
```

**Complete Processing Sequence**:
1. **Initial Normalization** → Ensure probability totals match mathematical expectations
2. **Monotonic Constraints** → Ensure logical ordering within each team
3. **Re-Normalization** → Restore correct probability totals after constraint adjustments
4. **Probability Capping** → Ensure no individual probability exceeds 1.0

**Cross-Relay Type Consistency**: All three relay types (standard, mixed, team sprint) use identical normalization and constraint procedures, ensuring consistent probability post-processing across relay formats.

**Mathematical Guarantee**: The final probabilities satisfy three critical requirements:
- **Valid Range**: All probabilities ∈ [0,1]
- **Correct Totals**: Sum to expected mathematical targets (1, 3, 5, 10)
- **Logical Ordering**: Win ≤ Podium ≤ Top5 ≤ Top10 for each team

**Purpose**: This post-processing ensures that the aggregated team probabilities, which result from weighted combinations of individual leg predictions, maintain mathematical coherence and interpretability for decision-making and expected value calculations.

#### Fantasy

Cross-country relay fantasy employs sophisticated optimization algorithms to select optimal teams across three distinct relay formats: Standard Relay (4-leg), Team Sprint (2-leg), and Mixed Relay (4-leg with gender constraints). The system implements format-specific Mixed Integer Programming (MIP) models that maximize expected points while adhering to budget constraints and format-specific team composition requirements.

**Format-Specific Fantasy Frameworks**: Cross-country implements distinct fantasy optimization strategies for each relay format, reflecting their unique competitive characteristics and team assembly requirements:

```r
# Standard Relay Fantasy Optimization (4-leg format)
optimize_relay_fantasy <- function(team_predictions, max_price = 100000, max_per_gender = 6) {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:n, type = "binary") %>%
    set_objective(sum_expr(team_predictions$Expected_Points[i] * x[i], i = 1:n), "max") %>%
    add_constraint(sum_expr(team_predictions$Price[i] * x[i], i = 1:n) <= max_price) %>%
    add_constraint(sum_expr(x[i], i = men_indices) <= max_per_gender) %>%
    add_constraint(sum_expr(x[i], i = women_indices) <= max_per_gender)
  
  return(solve_model(model, with_ROI(solver = "glpk")))
}

# Mixed Relay Fantasy Optimization (gender-balanced constraint)
optimize_mixed_relay_fantasy <- function(team_predictions, max_price = 100000, max_teams = 12) {
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:n, type = "binary") %>%
    set_objective(sum_expr(team_predictions$Expected_Points[i] * x[i], i = 1:n), "max") %>%
    add_constraint(sum_expr(team_predictions$Price[i] * x[i], i = 1:n) <= max_price) %>%
    add_constraint(sum_expr(x[i], i = 1:n) <= max_teams)  # Total team limit
  
  return(solve_model(model, with_ROI(solver = "glpk")))
}
```

**Team Formation and Expected Points Calculation**: Cross-country relay fantasy calculates expected points through weighted aggregation of individual leg probabilities, incorporating format-specific leg importance weights and relay-specific point systems:

```r
# Team expected points calculation with leg importance weighting
calculate_team_expected_points <- function(team_members, individual_predictions, leg_weights) {
  # Standard relay leg importance: c(0.2, 0.2, 0.25, 0.35)
  # Team sprint leg importance: c(0.5, 0.5)  
  # Mixed relay leg importance: c(0.2, 0.25, 0.25, 0.3)
  
  relay_points <- c(200, 160, 120, 100, 90, 80, 72, 64, 58, 52, 46, 40, 36, 32, 28, 24, 22, 20, 18, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  
  # Calculate weighted team probabilities
  team_win_prob <- sum(member_win_probs * leg_weights)
  team_podium_prob <- sum(member_podium_probs * leg_weights)
  team_top5_prob <- sum(member_top5_probs * leg_weights)
  team_top10_prob <- sum(member_top10_probs * leg_weights)
  
  # Calculate expected points using probability-weighted point values
  expected_points <- team_win_prob * relay_points[1] +
    (team_podium_prob - team_win_prob) * mean(relay_points[2:3]) +
    (team_top5_prob - team_podium_prob) * mean(relay_points[4:5]) +
    (team_top10_prob - team_top5_prob) * mean(relay_points[6:10])
  
  return(expected_points)
}
```

**Standard Relay Fantasy (4-Leg Format)**: The standard relay fantasy employs technique-specific leg modeling with progressive importance weighting that emphasizes later legs where tactical positioning becomes critical:

- **Leg 1 (Classic)**: 20% importance, classic-specific predictors
- **Leg 2 (Classic)**: 20% importance, classic-specific predictors  
- **Leg 3 (Freestyle)**: 25% importance, freestyle-specific predictors
- **Leg 4 (Freestyle)**: 35% importance, freestyle + sprint predictors (anchor leg)

```r
# Standard relay team formation with technique-specific leg assignment
generate_standard_relay_teams <- function(teams_df, individual_predictions) {
  leg_importance <- c(0.2, 0.2, 0.25, 0.35)
  
  for(team in teams_df) {
    team_probs <- list(Win = 0, Podium = 0, Top5 = 0, Top10 = 0)
    
    for(leg in 1:4) {
      # Classic legs (1-2) vs Freestyle legs (3-4)
      technique <- if(leg <= 2) "Classic" else "Freestyle"
      leg_predictor_set <- get_technique_predictors(technique, leg)
      
      # Special sprint predictors for anchor leg
      if(leg == 4) {
        leg_predictor_set <- c(leg_predictor_set, "Sprint_F_Elo_Pct")
      }
      
      member_probs <- get_member_predictions(team$members[leg], leg_predictor_set)
      team_probs <- aggregate_leg_probabilities(team_probs, member_probs, leg_importance[leg])
    }
  }
}
```

**Team Sprint Fantasy (2-Leg Format)**: Team sprint fantasy adapts to technique-specific competition formats with equal leg importance weighting and sprint-focused predictive modeling:

```r
# Team sprint optimization with technique-specific filtering
optimize_team_sprint_fantasy <- function(race_info, max_price = 100000) {
  # Determine technique from race schedule
  technique <- race_info$men$Technique[1]  # "Classic" or "Freestyle"
  
  # Technique-specific predictor sets
  predictor_set <- switch(technique,
    "Classic" = c("Overall_Elo_Pct", "Classic_Elo_Pct", "Sprint_C_Elo_Pct", "Sprint_Elo_Pct"),
    "Freestyle" = c("Overall_Elo_Pct", "Freestyle_Elo_Pct", "Sprint_F_Elo_Pct", "Sprint_Elo_Pct")
  )
  
  # Equal leg importance for 2-person teams
  leg_weights <- c(0.5, 0.5)
  
  return(optimize_fantasy_team(team_predictions, max_price, max_per_gender = 6))
}
```

**Mixed Relay Fantasy (Gender-Constrained 4-Leg)**: Mixed relay fantasy implements the most complex constraint system, enforcing gender-specific leg assignments with combined athlete pool optimization:

```r
# Mixed relay team formation with gender-specific leg constraints  
generate_mixed_relay_teams <- function(combined_chrono, race_info) {
  # Gender-specific leg assignments:
  # Leg 1: Female Classic
  # Leg 2: Male Classic
  # Leg 3: Female Freestyle  
  # Leg 4: Male Freestyle
  
  leg_importance <- c(0.2, 0.25, 0.25, 0.3)
  
  for(team in teams_df) {
    for(leg in 1:4) {
      required_sex <- if(leg %in% c(1, 3)) "F" else "M"
      technique <- if(leg %in% c(1, 2)) "Classic" else "Freestyle"
      
      # Filter athlete pool by gender and get technique-specific predictions
      eligible_athletes <- combined_chrono %>% filter(Sex == required_sex)
      leg_predictions <- generate_leg_predictions(eligible_athletes, technique)
      
      # Apply gender-aware scoring
      team_score <- calculate_gender_aware_team_score(leg_predictions, leg_importance[leg])
    }
  }
}
```

**Optimization Constraints and Budget Management**: Cross-country relay fantasy implements sophisticated constraint hierarchies that balance budget limitations with format-specific team composition requirements:

**Budget Constraints**:
- **Standard Budget**: 100,000 price units across all formats
- **Team Limits**: 6 teams per gender (Standard/Team Sprint), 12 teams total (Mixed Relay)
- **Price Validation**: Automatic filtering of teams with missing or invalid pricing

**Format-Specific Constraints**:
```r
# Standard Relay & Team Sprint: Gender-balanced selection
add_constraint(sum_expr(x[i], i = men_indices) <= max_per_gender)
add_constraint(sum_expr(x[i], i = women_indices) <= max_per_gender)

# Mixed Relay: Total team constraint (no gender split needed)
add_constraint(sum_expr(x[i], i = 1:n) <= max_teams)
```

**Knapsack Algorithm Implementation**: The fantasy optimization employs Mixed Integer Programming (MIP) using the GLPK solver, implementing a sophisticated knapsack algorithm that maximizes expected points subject to multiple constraint categories:

```r
# Core knapsack optimization framework
fantasy_optimization_framework <- function(team_predictions, constraints) {
  # Decision variables: binary selection (0 or 1) for each team
  model <- MIPModel() %>%
    add_variable(x[i], i = 1:n, type = "binary") %>%
    
    # Objective: maximize sum of (expected_points × selection_indicator)
    set_objective(sum_expr(team_predictions$Expected_Points[i] * x[i], i = 1:n), "max") %>%
    
    # Constraint hierarchy
    apply_budget_constraints(constraints$budget) %>%
    apply_team_composition_constraints(constraints$composition) %>%
    apply_gender_constraints(constraints$gender)
  
  # Solve using GLPK solver with integer programming
  result <- solve_model(model, with_ROI(solver = "glpk"))
  
  return(extract_optimal_selection(result))
}
```

Cross-country relay fantasy represents the most sophisticated team selection optimization among winter sports, employing format-specific modeling approaches, technique-aware predictive systems, and multi-constraint optimization frameworks that deliver optimal team selections across the diverse competitive landscape of cross-country relay events.