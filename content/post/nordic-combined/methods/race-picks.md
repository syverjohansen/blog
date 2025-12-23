---
title: "Nordic Combined Race Picks Methodology"
date: 2023-12-01T01:23:07+00:00
draft: false
tags: ["methodology", "skiing", "race-picks", "nordic-combined"]
---

## Nordic Combined

### Individual

#### Data Gathering

Nordic Combined individual race startlists are scraped from the FIS websites using automated Python scripts that run based on scheduled race dates. The system processes races by type (individual, team events) and handles both FIS startlists and fallback scenarios when official startlists are unavailable.

**Race Discovery and Scheduling**: The system identifies races to process using UTC timezone-based scheduling:

```python
# From startlist-scrape-races.py:32-36
# Get today's date in UTC 
today_utc = datetime.now(timezone.utc)
today_str = today_utc.strftime('%m/%d/%Y')

print(f"Today's date (UTC): {today_str}")
```

**Race Type Classification**: Races are categorized by format to ensure appropriate processing:

```python
# From startlist-scrape-races.py:98-112
# Filter races by type: individual, team, team sprint, and mixed team
# Mixed team events have RaceType="Team" and Sex="Mixed"
individual_races = races_df[~races_df['RaceType'].str.contains("Team", na=False)]
team_races = races_df[(races_df['RaceType'].str.contains("Team", na=False)) & 
                      (~races_df['RaceType'].str.contains("Sprint", na=False)) &
                      (races_df['Sex'] != 'Mixed')]
team_sprint_races = races_df[races_df['RaceType'].str.contains("Team Sprint", na=False)]
mixed_team_races = races_df[(races_df['RaceType'].str.contains("Team", na=False)) & 
                             (~races_df['RaceType'].str.contains("Sprint", na=False)) &
                             (races_df['Sex'] == 'Mixed')]

print(f"Found {len(individual_races)} individual races")
print(f"Found {len(team_races)} team races")
print(f"Found {len(team_sprint_races)} team sprint races")
print(f"Found {len(mixed_team_races)} mixed team races")
```

**FIS Website Data Extraction**: Individual athlete data is scraped from official FIS race result pages:

```python
# From startlist_common.py:187-302
def extract_individual_results(soup: BeautifulSoup) -> List[Dict]:
    """Extract individual athlete results from FIS race page"""
    athletes = []
    
    try:
        # Find all athlete rows
        athlete_rows = soup.select('.table-row')
        
        for row in athlete_rows:
            try:
                # Extract athlete data
                
                # Get rank
                rank_elem = row.select_one('.g-lg-1.g-md-1.g-sm-1.g-xs-2.justify-right.pr-1.bold')
                if not rank_elem:
                    continue  # Skip if no rank (might be header or footer)
                
                rank = rank_elem.text.strip()
                
                # Get bib
                bib_elem = row.select_one('.g-lg-1.g-md-1.g-sm-1.justify-center.hidden-sm-down')
                bib = bib_elem.text.strip() if bib_elem else ""
                
                # Get FIS code
                fis_code_elem = row.select_one('.g-lg-1.g-md-2.g-sm-2.hidden-xs.justify-right.gray.pr-1, .g-lg-2.g-md-2.g-sm-3.hidden-xs.justify-right.gray.pr-1')
                fis_code = fis_code_elem.text.strip() if fis_code_elem else ""
                
                # Get name
                name_elem = row.select_one('.g-lg.g-md.g-sm.g-xs.justify-left.bold, .g-lg-8.g-md-8.g-sm-5.g-xs-9.justify-left.bold')
                name = name_elem.text.strip() if name_elem else ""
                
                # Get nation
                nation_elem = row.select_one('.country__name-short')
                nation = nation_elem.text.strip() if nation_elem else ""
                
                # Get jump distance
                jump_elem = row.select_one('.g-lg.g-md.g-sm.justify-right.bold.hidden-xs')
                jump = jump_elem.text.strip() if jump_elem else ""
```

**Nordic Combined Specific Data**: The system captures Nordic Combined-specific performance metrics including both jumping and cross-country components:

```python
# From startlist_common.py:275-288
athlete_data = {
    'Rank': rank,
    'Bib': bib,
    'FisCode': fis_code,
    'Name': name,
    'Year': year,
    'Nation': nation,
    'Jump': jump,           # Jump distance
    'Points': points,       # Jump points
    'JumpRank': rank_jump,  # Jump portion ranking
    'Time': time,           # Cross-country time
    'TimeDiff': time_diff,  # Time difference from winner
    'ID': athlete_id
}
```

**ELO Score Integration**: Historical performance data is matched with current startlist athletes using first quartile imputation:

```python
# From startlist_common.py:456-589
def get_latest_elo_scores(file_path: str) -> pd.DataFrame:
    """Gets most recent ELO scores for each athlete with quartile imputation"""
    try:
        # Get most recent scores for each athlete
        if 'Skier' in df.columns:
            try:
                latest_scores = df.groupby('Skier').last().reset_index()
            except Exception as group_e:
                print(f"Error grouping by Skier: {group_e}")
                latest_scores = df  # Use full dataset if grouping fails
        
        # Define ELO columns
        elo_columns = [col for col in ['Elo', 'Individual_Elo', 'Sprint_Elo', 'MassStart_Elo', 'IndividualCompact_Elo'] 
                      if col in latest_scores.columns]
        
        # Calculate first quartile for each ELO column
        q1_values = {}
        for col in elo_columns:
            if col in latest_scores.columns:
                try:
                    q1_values[col] = latest_scores[col].astype(float).quantile(0.25)
                    print(f"First quartile value for {col}: {q1_values[col]}")
                except Exception as q1_e:
                    print(f"Error calculating quartile for {col}: {q1_e}")
                    q1_values[col] = 1000  # Default value if calculation fails
        
        # Replace NAs with first quartile values
        for col in elo_columns:
            if col in latest_scores.columns:
                latest_scores[col] = latest_scores[col].fillna(q1_values.get(col, 1000))
```

**Fuzzy Name Matching**: Robust athlete name matching handles international name variations common in Nordic Combined:

```python
# From startlist_common.py:591-627
def normalize_name(name: str) -> str:
    """Normalizes name for better fuzzy matching"""
    normalized = name.lower()
    char_map = {
        'ø': 'oe', 'ö': 'oe', 'ó': 'o',
        'ä': 'ae', 'á': 'a', 'å': 'aa',
        'é': 'e', 'è': 'e',
        'ü': 'ue',
        'ý': 'y',
        'æ': 'ae'
    }
    for char, replacement in char_map.items():
        normalized = normalized.replace(char, replacement)
    return normalized

def fuzzy_match_name(name: str, name_list: List[str], threshold: int = 80) -> str:
    """Finds best matching name using normalized comparison"""
    from thefuzz import fuzz
    
    best_score = 0
    best_match = ''
    normalized_name = normalize_name(name)
    
    # Try matching full name
    for candidate in name_list:
        normalized_candidate = normalize_name(candidate)
        score = fuzz.ratio(normalized_name, normalized_candidate)
        
        # Also try matching tokens in any order
        token_score = fuzz.token_sort_ratio(normalized_name, normalized_candidate)
        score = max(score, token_score)
        
        if score > best_score and score >= threshold:
            best_score = score
            best_match = candidate
    
    return best_match
```

**Race-Specific ELO Selection**: ELO scores are prioritized based on Nordic Combined race type:

```python
# From startlist_common.py:629-652
def get_race_specific_elo(elo_data: Dict, race_type: str) -> float:
    """Get the most relevant ELO score based on race type"""
    # Define priority order for each race type
    priority_elo = {
        'Individual': ['Individual_Elo', 'Elo'],
        'Individual Compact': ['IndividualCompact_Elo', 'Individual_Elo', 'Elo'],
        'Sprint': ['Sprint_Elo', 'Elo'],
        'Mass Start': ['MassStart_Elo', 'Elo'],
        'Team': ['Elo'],  # Team events use overall ELO
        'Team Sprint': ['Sprint_Elo', 'Elo']  # Team sprint uses sprint ELO if available
    }
    
    # Get the priority list for this race type (default to just 'Elo' if race type not found)
    priority_cols = priority_elo.get(race_type, ['Elo'])
    
    # Try each column in priority order
    for col in priority_cols:
        if col in elo_data and elo_data[col] is not None:
            try:
                return float(elo_data[col])
            except (TypeError, ValueError):
                continue
    
    return 0.0  # Default if no matching ELO found
```

**Mock Startlist Creation**: When FIS startlists are unavailable, comprehensive season-based startlists are generated:

```python
# From startlist-scrape-races.py:239-347
def create_season_startlist(elo_path: str, race_info: pd.Series, gender: str, 
                           host_nation: str, prob_column: str) -> Optional[pd.DataFrame]:
    """Creates DataFrame with all skiers from current season when no startlist is available"""
    try:
        # Get chronological data to find current season skiers
        chrono_path = f"~/ski/elo/python/nordic-combined/polars/excel365/{gender}_chrono.csv"
        try:
            # First try to read chronological data
            chrono_df = pd.read_csv(chrono_path)
            
            # Get current season
            current_season = chrono_df['Season'].max()
            print(f"Using most recent season {current_season} for all skiers list")
            
            # Filter to current season
            current_season_df = chrono_df[chrono_df['Season'] == current_season]
            
            # Get unique skiers from current season
            current_skiers_df = current_season_df[['Skier', 'ID', 'Nation']].drop_duplicates()
            
            print(f"Found {len(current_skiers_df)} unique skiers from current season {current_season}")
```

**Multiple Race Processing**: The system handles multiple races per day with comprehensive probability tracking:

```python
# From startlist-scrape-races.py:507-514
# Get total number of races for this gender on the race date
total_gender_races = len(races_df)
print(f"Total {gender} races: {total_gender_races}")

# Create enough probability columns for all races
all_prob_columns = [f'Race{i+1}_Prob' for i in range(total_gender_races)]
print(f"All probability columns: {all_prob_columns}")
```

**Data Consolidation**: Multiple race startlists are merged while preserving athlete uniqueness and race participation tracking:

```python
# From startlist-scrape-races.py:471-501
def merge_race_dataframes(df1: pd.DataFrame, df2: pd.DataFrame, prob_column: str) -> pd.DataFrame:
    """Merge two race dataframes, preserving unique athletes and combining probability columns"""
    # For skiers that exist in both dataframes, update the probability column
    common_skiers = set(df2['Skier']) & existing_skiers
    for skier in common_skiers:
        # Find the row index in result for this skier
        idx = result[result['Skier'] == skier].index[0]
        
        # Get probability from df2
        prob_value = df2[df2['Skier'] == skier][prob_column].values[0]
        
        # Update the probability in result
        result.loc[idx, prob_column] = prob_value
    
    # For skiers that only exist in df2, add them to result
    new_skiers = set(df2['Skier']) - existing_skiers
    new_rows = df2[df2['Skier'].isin(new_skiers)]
    
    # Append the new rows to result
    result = pd.concat([result, new_rows], ignore_index=True)
```

The Nordic Combined individual data gathering system provides comprehensive startlist creation with FIS integration, Nordic Combined-specific performance metrics (jumping and cross-country), robust name matching, ELO score integration with race-type prioritization, and fallback mechanisms to ensure complete athlete coverage for prediction modeling.

#### Points

##### Training

###### Setup

The training data setup for Nordic Combined follows a sophisticated preprocessing pipeline that handles the sport's dual-discipline nature (ski jumping + cross-country) and supports both individual and team competition formats.

**Nordic Combined Unified Points System**: 
Unlike other sports with discipline variations, Nordic Combined uses a single standardized 40-position scoring system across all race formats:

```r
# From race-picks.R:14-20
# Define points systems for Nordic Combined
# Individual: Top 50 get points
individual_points <- c(100, 90, 80, 70, 60, 55, 52, 49, 46, 43, 40, 38, 36, 34, 32, 30, 28, 26, 24, 22, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Sprint and Mass Start events typically use same system
sprint_points <- individual_points
mass_start_points <- individual_points
```

**Dual-Discipline Training Data Preprocessing**:
Nordic Combined's unique preprocessing handles both individual and team events through a flexible data pipeline:

```r
# From race-picks.R:996-1024
preprocess_data <- function(df, is_team = FALSE) {
  # Load races data to determine points systems for historical races
  races_data <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/races.csv", 
                         stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date, format="%Y-%m-%d"))
  
  participant_col <- if(is_team) "Nation" else "Skier"
  id_col <- if(is_team) "Nation" else "ID"
  
  # For team data, we want to keep the team structure
  if(is_team) {
    log_info("Processing team data - using team-based metrics")
    
    # Define the team-specific columns we expect/need
    team_cols <- c("Avg_Sprint_Elo", "Avg_Individual_Elo", "Avg_MassStart_Elo", "Avg_IndividualCompact_Elo", "Avg_Elo")
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

**Race Type-Specific Weighted Points Calculation**:
The system calculates weighted previous points by Nordic Combined race type, accounting for the sport's varied event formats:

```r
# From race-picks.R:1042-1054
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

**Nordic Combined ELO Column Management**:
The system handles Nordic Combined's race type-specific ELO ratings with individual and team processing paths:

```r
# From race-picks.R:1056-1069
# For training, use Pelo columns (pre-race ELO) but name them as Elo_Pct for consistency
if(is_team) {
  pelo_cols <- c("Avg_Sprint_Pelo", "Avg_Individual_Pelo", "Avg_MassStart_Pelo", "Avg_IndividualCompact_Pelo", "Avg_Pelo")
} else {
  pelo_cols <- c("Sprint_Pelo", "Individual_Pelo", "MassStart_Pelo", "IndividualCompact_Pelo", "Pelo")
}

# Make sure Pelo columns exist (create if missing)
for (col in pelo_cols) {
  if (!col %in% names(df_with_points)) {
    log_info(paste("Creating missing Pelo column:", col))
    df_with_points[[col]] <- 0
  }
}
```

**Elevation and Temporal Feature Engineering**:
Nordic Combined includes elevation effects (venues above/below 1300m) and seasonal periodization:

```r
# From race-picks.R:1072-1094
processed_df <- df_with_points %>%
  # Add period (Nordic Combined has 4 periods per season)
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
  filter(
    Season >= max(Season-10)
  )
```

This Nordic Combined setup creates training datasets that account for the sport's dual-discipline nature (ski jumping + cross-country), support both individual and team formats, maintain race type-specific performance tracking, and provide elevation-adjusted performance context for accurate GAM model training.

**Points Calculation Function**: Race positions are mapped to points using a unified function across all race types:

```r
# From race-picks.R:122-131
# Function to get points based on place for Nordic Combined
get_points <- function(place, RaceType) {
  # All Nordic Combined events use the same points system
  points_list <- individual_points
  
  if (place >= 1 && place <= length(points_list)) {
    return(points_list[place])
  }
  return(0)
}
```

**Training Data Loading and Preprocessing**: The system loads chronological race data with automatic points generation when missing:

```r
# From race-picks.R:147-153
men_chrono <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/men_chrono_elevation.csv", 
                       stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

men_startlist <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/startlist_races_men.csv", 
                          stringsAsFactors = FALSE)
men_startlist$Sex = "M"
```

**Historical Points Integration**: The training setup automatically generates points columns when missing from historical data:

```r
# From race-picks.R:469-482
# Check if Points column exists, if not create it
if(!"Points" %in% names(men_chrono)) {
  log_info("Creating Points column in men's chrono based on Place")
  men_chrono$Points <- mapply(function(place, RaceType) {
    get_points(place, RaceType)
  }, men_chrono$Place, men_chrono$RaceType)
}

if(!"Points" %in% names(ladies_chrono)) {
  log_info("Creating Points column in ladies' chrono based on Place")
  ladies_chrono$Points <- mapply(function(place, RaceType) {
    get_points(place, RaceType)
  }, ladies_chrono$Place, ladies_chrono$RaceType)
}
```

**Missing Value Imputation Strategy**: The system uses first quartile imputation for missing ELO values, providing conservative estimates:

```r
# From race-picks.R:22-27
# Function to replace NAs with first quartile value
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}
```

**Team Performance Aggregation**: For team events, individual athlete performance is aggregated to team-level metrics with average ELO calculations:

```r
# From race-picks.R:487-505
men_team_chrono <- men_chrono %>%
  filter(RaceType == "Team") %>%
  # Group by race to get team results
  group_by(Date, City, Country, Nation, RaceType, Season, Race, Event, Elevation) %>%
  summarize(
    # Team rank is the first Place value within each group
    Place = first(Place),
    Points = first(Points),
    # Calculate average Elo values for the team
    Avg_Elo = mean(Elo, na.rm = TRUE),
    Avg_Individual_Elo = mean(Individual_Elo, na.rm = TRUE),
    Avg_Sprint_Elo = mean(Sprint_Elo, na.rm = TRUE),
    Avg_MassStart_Elo = mean(MassStart_Elo, na.rm = TRUE),
    Avg_IndividualCompact_Elo = mean(IndividualCompact_Elo, na.rm = TRUE),
    # Additional metadata
    MassStart = first(MassStart),
    Sex = "M",
    .groups = "drop"
  )
```

The Nordic Combined points training setup provides a comprehensive foundation with unified scoring across race formats, automatic data preprocessing, conservative missing value imputation, and sophisticated team performance aggregation for multi-athlete events.

###### Feature Selection

Nordic Combined's feature selection process reflects the sport's dual-discipline nature and the need to accommodate both individual and team competition formats. The system uses a sophisticated dual-path approach with exhaustive BIC optimization adapted to each competition structure.

**Dual-Format Variable Architecture**: 
The system defines completely different variable sets for individual versus team events, recognizing the distinct performance metrics and predictive requirements:

```r
# From race-picks.R:1861-1871
# Define explanatory variables based on race type
if(is_team) {
  explanatory_vars <- c("Avg_Sprint_Elo_Pct", "Avg_Individual_Elo_Pct", 
                        "Avg_MassStart_Elo_Pct", "Avg_IndividualCompact_Elo_Pct", 
                        "Avg_Elo_Pct")
} else {
  explanatory_vars <- c("Prev_Points_Weighted", 
                        "Sprint_Elo_Pct", "Individual_Elo_Pct", 
                        "MassStart_Elo_Pct", "IndividualCompact_Elo_Pct", 
                        "Elo_Pct")#, "Period", "Elevation_Flag")
}
```

**Individual vs Team Variable Philosophy**: 
- **Individual Events**: Include weighted previous points plus race type-specific ELO ratings (Sprint, Individual, Mass Start, Individual Compact) reflecting the different tactical approaches of Nordic Combined events
- **Team Events**: Use team-averaged ELO ratings without weighted points, recognizing that team performance emerges from collective strength rather than individual historical performance

**Exhaustive Subset Selection**: The system uses the leaps package to perform exhaustive feature selection with BIC optimization:

```r
# From race-picks.R:1874-1882
# Create and fit model for points
formula <- as.formula(paste(response_variable, "~", paste(explanatory_vars, collapse = " + ")))
tryCatch({
  exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
  summary_exhaustive <- summary(exhaustive_selection)
  best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
  smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
  gam_formula <- as.formula(paste("Points ~", smooth_terms))
  
  model <- gam(gam_formula, data = race_df_75)
```

**Weighted Historical Points**: Individual races incorporate recent performance history through exponentially weighted previous points:

```r
# From race-picks.R:1047-1053
Prev_Points_Weighted = sapply(1:n(), function(j) {
  if (j == 1) return(0)
  start_index <- max(1, j - 5)
  num_races <- j - start_index
  num_races <- j - start_index
  weights <- seq(1, num_races)
  weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
})
```

**Race-Type Specific ELO Selection**: The system dynamically selects the most relevant ELO metric based on race format:

```r
# From race-picks.R:1834-1849
# Get relevant Elo column based on race type
if(is_team) {
  # For team races, use general average ELO
  elo_col <- "Avg_Elo_Pct"
} else {
  if(races$racetype[i] == "Sprint") {
    elo_col <- "Sprint_Elo_Pct"
  } else if(races$racetype[i] == "Individual") {
    elo_col <- "Individual_Elo_Pct"
  } else if(races$racetype[i] == "MassStart") {
    elo_col <- "MassStart_Elo_Pct"
  } else if(races$racetype[i] == "Individual Compact") {
    elo_col <- "IndividualCompact_Elo_Pct"
  } else {
    elo_col <- "Elo_Pct"
  }
}
```

**Elite Athlete Filtering**: Feature selection operates on a filtered dataset of top performers to improve model precision:

```r
# From race-picks.R:1851-1856
# Filter for top performers and add previous points
race_df_75 <- race_df %>%
  filter(get(elo_col) > 0.75) %>%
  group_by(!!sym(participant_col)) %>%
  arrange(Season, Race) %>%
  ungroup()
```

**Feature Engineering for Team Metrics**: Team performance features are derived from individual athlete aggregation:

```r
# From race-picks.R:1010-1024
# Define the team-specific columns we expect/need
team_cols <- c("Avg_Sprint_Elo", "Avg_Individual_Elo", "Avg_MassStart_Elo", "Avg_IndividualCompact_Elo", "Avg_Elo")
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
```

**Normalization and Percentage Conversion**: All ELO features are converted to percentages for consistent scaling across race types:

```r
# From race-picks.R:1104-1116
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
)
```

The Nordic Combined feature selection system provides adaptive variable selection that optimizes for different race formats while incorporating Nordic Combined-specific performance metrics, historical points weighting, and elite athlete filtering to maximize prediction accuracy across the sport's diverse competition formats.

###### Modeling

Nordic Combined points prediction employs a sophisticated multi-tier modeling approach that combines Generalized Additive Models (GAMs) with Bayesian Information Criterion optimization and hierarchical fallback strategies. The system creates both points prediction models and position probability models for comprehensive race outcome forecasting.

**Primary GAM Modeling**: The main modeling approach uses smoothed terms for optimal nonlinear relationship capture:

```r
# From race-picks.R:1875-1882
tryCatch({
  exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
  summary_exhaustive <- summary(exhaustive_selection)
  best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
  smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
  gam_formula <- as.formula(paste("Points ~", smooth_terms))
  
  model <- gam(gam_formula, data = race_df_75)
```

**Hierarchical Fallback Strategy**: The system implements robust error handling with progressively simpler models:

```r
# From race-picks.R:1883-1905
}, error = function(e) {
  log_warn(paste("Error in model selection:", e$message))
  # Fallback to a simpler model with reduced degrees of freedom
  tryCatch({
    # Try with reduced degrees of freedom
    fallback_formula <- as.formula(paste("Points ~ s(", elo_col, ", k=3) + s(Period, k=3) + s(Elevation_Flag, k=3)"))
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

**Position Probability Modeling**: The system creates binomial GAM models for each position threshold with comprehensive error handling:

```r
# From race-picks.R:1914-1951
# Create models for each position threshold
for(threshold in position_thresholds) {
  log_info(paste("Creating model for top", threshold, "positions"))
  
  # Create binary outcome variable for position threshold
  race_df$position_achieved <- race_df$Place <= threshold
  
  # Create formula for regsubsets using the same explanatory variables as the points model
  pos_formula <- as.formula(paste("position_achieved ~", paste(position_feature_vars, collapse = " + ")))
  
  # Use regsubsets to select best features for this position threshold
  tryCatch({
    pos_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")
    pos_summary <- summary(pos_selection)
    pos_best_bic_vars <- names(coef(pos_selection, which.min(pos_summary$bic)))
    
    # Create smooth terms for GAM using best BIC variables (remove intercept)
    pos_smooth_terms <- paste("s(", pos_best_bic_vars[-1], ")", collapse=" + ")
    pos_gam_formula <- as.formula(paste("position_achieved ~", pos_smooth_terms))
    
    # Fit the position model with binomial family
    position_model <- gam(pos_gam_formula,
                          data = race_df,
                          family = binomial,
                          method = "REML")
    
    # Calculate Brier score for model evaluation
    predicted_probs <- predict(position_model, newdata = race_df, type = "response")
    brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
    log_info(paste("Brier score for threshold", threshold, ":", round(brier_score, 4)))
    
    # Log selected variables
    log_info(paste("Variables selected for", threshold, "position model:", 
                   paste(pos_best_bic_vars[-1], collapse=", ")))
    
    # Store the model
    position_models[[paste0("threshold_", threshold)]] <- position_model
```

**Model Validation with Brier Scores**: Position models are evaluated using Brier scores for probabilistic accuracy assessment:

```r
# From race-picks.R:1940-1943
# Calculate Brier score for model evaluation
predicted_probs <- predict(position_model, newdata = race_df, type = "response")
brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
log_info(paste("Brier score for threshold", threshold, ":", round(brier_score, 4)))
```

**Adaptive Prediction Methods**: The system implements robust prediction strategies with multiple fallback approaches:

```r
# From race-picks.R:2243-2266
# Make predictions with explicit try-catch
base_predictions <- tryCatch({
  # Debug output
  log_info(paste("Attempting prediction for threshold", threshold, "with", nrow(prediction_subset), "rows"))
  
  # Explicit call to mgcv::predict.gam to avoid method dispatch issues
  mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response")
}, error = function(e) {
  log_warn(paste("Prediction call failed:", e$message))
  
  # Try alternative prediction approach with one row at a time
  log_info("Trying row-by-row prediction as fallback")
  result <- numeric(nrow(prediction_subset))
  
  for(j in 1:nrow(prediction_subset)) {
    single_row <- prediction_subset[j,, drop = FALSE]
    result[j] <- tryCatch({
      mgcv::predict.gam(pos_model, newdata = single_row, type = "response")
    }, error = function(e2) {
      log_warn(paste("Failed on row", j, ":", e2$message))
      threshold/100  # Default value based on threshold
    })
  }
  return(result)
})
```

**Volatility and Risk Modeling**: The system incorporates sophisticated risk metrics using rolling window calculations:

```r
# From race-picks.R:2095-2132
# Calculate volatility metrics using recent races
race_df_75 <- race_df_75 %>%
  group_by(!!sym(participant_col)) %>%
  arrange(Date) %>%  # Ensure chronological order
  mutate(
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
  ) %>%
  mutate(
    recent_volatility_ratio = recent_upside_potential / abs(recent_downside_risk)
  ) %>%
  ungroup()
```

**Confidence-Weighted Scoring Scenarios**: The system generates multiple prediction scenarios based on recent race history:

```r
# From race-picks.R:2384-2399
# Different scoring scenarios - adjusted by race probability
confidence_factor = pmin(n_recent_races / 10, 1),
scaled_upside_potential = upside_potential * (Predicted_Points/100),
scaled_downside_potential = downside_risk * (Predicted_Points/100),

# Safe prediction (downside)
Safe_Prediction = pmax(
  (Predicted_Points - (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
  0
),

# Upside prediction
Upside_Prediction = pmin(
  (Predicted_Points + (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
  100 * Race_Prob  # Cap at 100 * probability
)
```

The Nordic Combined modeling system provides robust prediction capabilities through hierarchical GAM modeling, comprehensive position probability estimation, sophisticated volatility analysis, and adaptive fallback strategies that ensure reliable predictions across all race scenarios and data quality conditions.

###### Adjustments

Nordic Combined points prediction incorporates sophisticated adjustment mechanisms that account for systematic biases in period-specific and elevation-specific performance. The system uses statistical significance testing to identify when adjustments are warranted and applies them through both points prediction and position probability models.

**Period-Based Adjustments**: The system identifies and corrects for seasonal performance patterns using t-test significance analysis:

```r
# From race-picks.R:2062-2074
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
                             0),
```

**Elevation-Based Adjustments**: High-altitude race performance is systematically adjusted based on historical elevation effects:

```r
# From race-picks.R:2076-2088
# Step 3: Calculate Elevation adjustments
elevation_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_elev_curr <- Prediction_Diff[Elevation_Flag == Elevation_Flag[r] & row_id < r]
  prior_elev_other <- Prediction_Diff[Elevation_Flag != Elevation_Flag[r] & row_id < r]
  if(length(prior_elev_curr) < 3 || length(prior_elev_other) < 3) return(1)
  tryCatch({
    t.test(prior_elev_curr, prior_elev_other)$p.value
  }, error = function(e) 1)
}),
elevation_correction = ifelse(elevation_p < 0.05,
                              mean(Prediction_Diff[Elevation_Flag == Elevation_Flag], na.rm = TRUE),
                              0),
```

**Combined Adjustment Application**: Period and elevation corrections are combined to produce adjusted predictions:

```r
# From race-picks.R:2090-2092
# Combine adjustments
Adjusted_Prediction = Initial_Prediction + period_correction + elevation_correction
```

**Position Probability Adjustments**: Position models receive independent period-based adjustments using the same statistical framework:

```r
# From race-picks.R:1970-1983
# Calculate period adjustments
period_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_period_curr <- prob_diff[Period == Period[r] & row_id < r]
  prior_period_other <- prob_diff[Period != Period[r] & row_id < r]
  if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
  tryCatch({
    t.test(prior_period_curr, prior_period_other)$p.value
  }, error = function(e) 1)
}),
period_correction = ifelse(period_p < 0.05,
                           mean(prob_diff[Period == Period], na.rm = TRUE),
                           0),
period_adjusted = pmin(pmax(initial_prob + period_correction, 0), 1)
```

**Final Adjustment Integration**: Adjustments are applied to startlist predictions with comprehensive error handling:

```r
# From race-picks.R:2357-2377
left_join(participant_adjustments, by = participant_col) %>%
mutate(
  # Regular adjustments
  period_effect = replace_na(period_effect, 0),
  elevation_effect = replace_na(elevation_effect, 0),
  
  # Volatility metrics
  prediction_volatility = replace_na(prediction_volatility, 0),
  consistency_score = replace_na(consistency_score, 0),
  upside_potential = replace_na(upside_potential, 0),
  downside_risk = replace_na(downside_risk, 0),
  volatility_ratio = replace_na(volatility_ratio, 1),
  n_recent_races = replace_na(n_recent_races, 0),
  
  # Using existing adjustment approach
  period_adjustment = period_effect,
  elevation_adjustment = elevation_effect,
  
  # Base prediction and adjustments
  Predicted_Points = Base_Prediction + period_adjustment + elevation_adjustment,
  Predicted_Points = pmax(pmin(Predicted_Points, 100), 0),
```

**Race Probability Integration**: All adjustments are scaled by race participation probability:

```r
# From race-picks.R:2379-2381
# Apply race probability to predictions
Race_Prob = get(race_prob_col),
Final_Prediction = Predicted_Points * Race_Prob,
```

**Confidence-Weighted Risk Scenarios**: The system generates multiple adjusted prediction scenarios based on recent race confidence:

```r
# From race-picks.R:2383-2398
# Different scoring scenarios - adjusted by race probability
confidence_factor = pmin(n_recent_races / 10, 1),
scaled_upside_potential = upside_potential * (Predicted_Points/100),
scaled_downside_potential = downside_risk * (Predicted_Points/100),

# Safe prediction (downside)
Safe_Prediction = pmax(
  (Predicted_Points - (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
  0
),

# Upside prediction
Upside_Prediction = pmin(
  (Predicted_Points + (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
  100 * Race_Prob  # Cap at 100 * probability
)
```

**Adjustment Summary Generation**: The system captures final adjustment effects for each athlete:

```r
# From race-picks.R:2134-2150
# Get final adjustments for each participant
participant_adjustments <- race_df_75 %>%
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

The Nordic Combined adjustment system provides statistically-grounded corrections for systematic biases while maintaining prediction bounds and integrating race participation probabilities. The multi-layered approach ensures that both points predictions and position probabilities benefit from historical performance pattern recognition across different racing conditions.

##### Testing

###### Startlist Setup

Nordic Combined's startlist setup for testing implements a sophisticated dual-format data preparation pipeline that accommodates both individual and team competition formats while handling the sport's unique dual-discipline requirements. The system ensures robust data quality through advanced race probability preservation and comprehensive feature engineering that captures the complex interactions between ski jumping and cross-country skiing performance.

**Advanced Race Participation Probability Calculation with Race Type Specificity**:
Nordic Combined startlist setup employs an exponential decay model for calculating race-specific participation probabilities, recognizing that participation patterns differ significantly between Sprint, Individual, Mass Start, and Individual Compact events:

```r
# From race-picks.R:227-270 in get_race_probability()
get_race_probability <- function(chronos, participant, racetype, is_team = FALSE) {
  participant_races <- chronos %>%
    filter(get(id_col) == participant, RaceType == racetype) %>%
    arrange(Date, Season, Race)
  
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
}
```

**Dynamic Race Probability Column Detection and Preservation**: The testing setup identifies and preserves all race participation probability columns essential for multi-race prediction weighting:

```r
# From race-picks.R:1299-1301
race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
log_info(paste("Race probability columns found:", paste(race_prob_cols, collapse=", ")))
```

**Most Recent ELO Rating Retrieval Across Multiple Event Types**:
The system retrieves the most current ELO ratings for each athlete across all Nordic Combined event categories, ensuring predictions use the most up-to-date performance assessments for Sprint, Individual, Mass Start, and Individual Compact formats:

```r
# From race-picks.R:1396-1402
most_recent_elos <- race_df %>%
  filter(Skier %in% base_df$Skier) %>%
  group_by(Skier) %>%
  arrange(Date, Season, Race) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(Skier, any_of(elo_cols))
```

**ELO Score Normalization Using Historical PELO References**:
Nordic Combined employs a sophisticated normalization approach where current ELO ratings are converted to percentages using historical PELO (pre-race ELO) maximum values, ensuring consistency between training and testing phases:

```r
# From race-picks.R:1468-1502
for(i in seq_along(elo_columns_to_process)) {
  elo_col <- elo_columns_to_process[i]
  pelo_col <- pelo_columns_to_process[i]  # corresponding Pelo column name
  elo_pct_col <- paste0(str_replace(elo_col, "_Elo", "_Elo"), "_Pct")
  
  if(elo_col %in% names(result_df)) {
    # Get max value for normalization from race_df (historical Pelo data)
    if(pelo_col %in% names(race_df)) {
      max_val <- max(race_df[[pelo_col]], na.rm = TRUE)
      if(!is.na(max_val) && max_val > 0) {
        result_df[[elo_pct_col]] <- result_df[[elo_col]] / max_val
      }
    }
  }
}
```

**Event Type-Specific Weighted Previous Points Calculation**:
Recent performance is captured through weighted previous points calculated for each Nordic Combined event format, recognizing the distinct performance patterns across Sprint, Individual, Mass Start, and Individual Compact races:

```r
# From race-picks.R:1047-1054 (training phase)
mutate(Prev_Points_Weighted = sapply(1:n(), function(j) {
  if (j == 1) return(0)
  start_index <- max(1, j - 5)
  num_races <- j - start_index
  weights <- seq(1, num_races)  # Linear weights (1,2,3,4,5)
  weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
}))

# From race-picks.R:1407-1417 (testing phase)
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

**Comprehensive Missing Value Imputation Strategy**:
Nordic Combined uses an advanced NA handling approach that preserves data quality while accommodating the sport's complex dual-discipline performance requirements:

```r
# From race-picks.R:23-27
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Application during startlist preparation (lines 1504-1512):
result_df <- result_df %>%
  mutate(
    across(ends_with("_Pct"), ~replace_na_with_quartile(.x)),
    Prev_Points_Weighted = replace_na(Prev_Points_Weighted, 0)
  )
```

**Individual vs Team Format Adaptation**:
The startlist setup dynamically adapts between individual and team competitions, with specialized handling for team-averaged ELO ratings and nation-based identification while maintaining the same robust data quality standards.

**Model Compatibility Assurance**:
The final startlist preparation ensures all required model variables exist with appropriate defaults, creates complete prediction-ready datasets that capture Nordic Combined's unique dual-discipline performance patterns, and maintains consistency between training and testing data structures essential for accurate GAM model prediction across all competition formats.

# Combine all data
result_df <- base_df %>%
  left_join(most_recent_elos, by = "Skier") %>%
  left_join(recent_points, by = "Skier")
```

**Team Race Startlist Preparation**: Team events receive specialized handling with aggregated ELO metrics and team-specific probability columns:

```r
# From race-picks.R:1308-1342
# For teams, handle differently since startlist already has team average Elos
# Get all columns that might be needed for the model
elo_cols <- c("Avg_Sprint_Elo", "Avg_Individual_Elo", "Avg_MassStart_Elo", "Avg_IndividualCompact_Elo", "Avg_Elo")
pelo_cols <- c("Avg_Sprint_Pelo", "Avg_Individual_Pelo", "Avg_MassStart_Pelo", "Avg_IndividualCompact_Pelo", "Avg_Pelo")

# Select needed columns from startlist
result_cols <- c("Nation", elo_cols, race_prob_cols)
result_df <- startlist %>%
  select(any_of(result_cols))

# Get recent points from historical data if available
if("Points" %in% names(race_df)) {
  recent_points <- race_df %>%
    filter(Nation %in% result_df$Nation) %>%
    group_by(Nation) %>%
    arrange(Season, Race) %>%
    slice_tail(n = 5) %>%
    summarise(
      Prev_Points_Weighted = if(n() > 0) 
        weighted.mean(Points, w = seq_len(n()), na.rm = TRUE) 
      else 0
    )
  
  # Add previous points if they exist
  if(nrow(recent_points) > 0) {
    result_df <- result_df %>%
      left_join(recent_points, by = "Nation") %>%
      mutate(Prev_Points_Weighted = replace_na(Prev_Points_Weighted, 0))
  } else {
    result_df$Prev_Points_Weighted <- 0
  }
}
```

**Critical ELO-to-Percentage Conversion**: The system converts current ELO ratings to percentage scales using historical maximum values for model compatibility:

```r
# From race-picks.R:1352-1385
# CRITICAL: Convert Elo columns to Elo_Pct columns for model prediction
# Models are trained on Pelo data (named as Elo_Pct) but we predict using Elo data from startlist
for(i in seq_along(elo_cols)) {
  elo_col <- elo_cols[i]
  pelo_col <- pelo_cols[i]  # corresponding Pelo column name
  elo_pct_col <- paste0(str_replace(elo_col, "_Elo", "_Elo"), "_Pct")
  
  if(elo_col %in% names(result_df)) {
    # Get max value for normalization from race_df (historical Pelo data)
    if(pelo_col %in% names(race_df)) {
      max_val <- max(race_df[[pelo_col]], na.rm = TRUE)
      if(!is.na(max_val) && max_val > 0) {
        log_info(paste("Converting", elo_col, "to", elo_pct_col, "using max Pelo =", max_val))
        result_df[[elo_pct_col]] <- result_df[[elo_col]] / max_val
      } else {
        log_warn(paste("No valid max value for", pelo_col, "using default"))
        result_df[[elo_pct_col]] <- 0.5
      }
    } else {
      # Fallback: normalize within current Elo data
      max_val <- max(result_df[[elo_col]], na.rm = TRUE)
      if(!is.na(max_val) && max_val > 0) {
        log_info(paste("Converting", elo_col, "to", elo_pct_col, "using internal max =", max_val))
        result_df[[elo_pct_col]] <- result_df[[elo_col]] / max_val
      } else {
        log_warn(paste("No valid data for", elo_col, "using default"))
        result_df[[elo_pct_col]] <- 0.5
      }
    }
  } else {
    # If Elo column doesn't exist, create default Elo_Pct
    log_info(paste("Creating default", elo_pct_col, "(missing", elo_col, ")"))
    result_df[[elo_pct_col]] <- 0.5
  }
}
```

**Missing Value Imputation and Validation**: The system applies first quartile imputation and validates race probability columns:

```r
# From race-picks.R:1504-1524
# Replace NAs with first quartile
result_df <- result_df %>%
  mutate(
    across(
      ends_with("_Pct"),
      ~replace_na_with_quartile(.x)
    ),
    Prev_Points_Weighted = replace_na(Prev_Points_Weighted, 0)
  )

# Check race probability columns in result
for(col in race_prob_cols) {
  if(!is.null(result_df[[col]])) {
    log_info(paste("Race probability column", col, "summary:"))
    log_info(paste("  Mean:", mean(result_df[[col]], na.rm = TRUE)))
    log_info(paste("  Sum:", sum(result_df[[col]], na.rm = TRUE)))
    log_info(paste("  Max:", max(result_df[[col]], na.rm = TRUE)))
  } else {
    log_warn(paste("Race probability column", col, "is missing from result_df!"))
  }
}
```

**Model Variable Compatibility Checking**: The testing setup ensures all required model variables are present with appropriate types:

```r
# From race-picks.R:2200-2240
# Check what variables the model actually needs
model_vars <- names(pos_model$var.summary)
log_info(paste("Model for threshold", threshold, "requires variables:", paste(model_vars, collapse=", ")))

# Create a clean subset of prediction data with only required variables
prediction_subset <- startlist_prepared

# Explicitly check for each variable
for(var in model_vars) {
  if(!(var %in% names(prediction_subset))) {
    log_warn(paste("Missing required variable:", var, "- adding with default values"))
    # Add missing variable with appropriate default value
    prediction_subset[[var]] <- 0
  } else {
    # Ensure the variable has the right type
    model_var_type <- class(pos_model$var.summary[[var]])
    data_var_type <- class(prediction_subset[[var]])
    
    if(!identical(model_var_type, data_var_type)) {
      log_warn(paste("Variable type mismatch for", var, ":", 
                     "model expects", model_var_type, "but got", data_var_type))
      # Convert to correct type
      if(model_var_type == "numeric") {
        prediction_subset[[var]] <- as.numeric(prediction_subset[[var]])
      } else if(model_var_type == "factor") {
        prediction_subset[[var]] <- as.factor(prediction_subset[[var]])
      }
    }
    
    # Handle NAs
    if(any(is.na(prediction_subset[[var]]))) {
      log_info(paste("Replacing NAs in", var))
      if(is.numeric(prediction_subset[[var]])) {
        prediction_subset[[var]] <- replace_na_with_quartile(prediction_subset[[var]])
      } else {
        # For non-numeric, use most common value
        most_common <- names(sort(table(prediction_subset[[var]], useNA = "no"), decreasing = TRUE))[1]
        prediction_subset[[var]][is.na(prediction_subset[[var]])] <- most_common
      }
    }
  }
}
```

**Race Probability Column Validation**: The system validates and restores race probability columns with fallback mechanisms:

```r
# From race-picks.R:2155-2167
# Ensure race probability column exists
if(!(race_prob_col %in% names(startlist_prepared))) {
  log_warn(paste("Race probability column missing:", race_prob_col))
  if(race_prob_col %in% names(startlist)) {
    # Copy from original startlist if available
    log_info("Copying from original startlist")
    startlist_prepared[[race_prob_col]] <- startlist[match(startlist_prepared[[participant_col]], startlist[[participant_col]]), race_prob_col]
  } else {
    # Default to 1 for first race, 0 for others if not available
    log_info("Setting default probabilities")
    startlist_prepared[[race_prob_col]] <- if(i == 1) 1 else 0
  }
}
```

**Testing Data Structure Creation**: The system creates structured testing datasets for position probability validation:

```r
# From race-picks.R:2169-2186
# NEW CODE: Make position probability predictions with adjustments
position_preds <- data.frame(startlist_prepared[[participant_col]])
names(position_preds)[1] <- participant_col

# Add ID, Nation, Sex for individual races
if(!is_team) {
  position_preds$ID <- startlist_prepared$ID
  position_preds$Nation <- startlist_prepared$Nation
  position_preds$Sex <- startlist_prepared$Sex
}

# Add race number
position_preds$Race <- i

# Add race probability column for later normalization
if(race_prob_col %in% names(startlist_prepared)) {
  position_preds[[race_prob_col]] <- startlist_prepared[[race_prob_col]]
}
```

The Nordic Combined testing startlist setup provides comprehensive data preparation that ensures model compatibility through dynamic feature engineering, robust validation procedures, and adaptive normalization strategies that maintain prediction integrity across individual and team race formats.

###### Modeling

Nordic Combined's testing modeling employs sophisticated dual-format GAM application with comprehensive error handling and dual-discipline awareness. The system applies event type-specific trained models while maintaining robust four-tier fallback mechanisms and integrating historically-derived period and elevation adjustments that capture Nordic Combined's unique dual-discipline performance patterns.

**Event Type-Specific GAM Model Application**:
The system applies trained GAM models that were optimized for each event format using BIC feature selection, recognizing that performance varies significantly across Sprint, Individual, Mass Start, and Individual Compact races:

```r
# From race-picks.R:1873-1905
exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
gam_formula <- as.formula(paste("Points ~", smooth_terms))

model <- gam(gam_formula, data = race_df_75)

# Model application to startlist
Base_Prediction = predict(model, newdata = startlist_prepared)
```

**Comprehensive Four-Tier Fallback Hierarchy**:
Nordic Combined implements the most robust fallback system to handle both individual and team prediction failures:

```r
# From race-picks.R:1883-1905
tryCatch({
  # Level 1: Primary GAM with BIC-selected variables
  model <- gam(gam_formula, data = race_df_75)
}, error = function(e) {
  tryCatch({
    # Level 2: Simplified GAM with reduced complexity
    fallback_formula <- as.formula(paste("Points ~ s(", elo_col, ", k=3) + s(Period, k=3) + s(Elevation_Flag, k=3)"))
    model <<- gam(fallback_formula, data = race_df_75)
  }, error = function(e2) {
    tryCatch({
      # Level 3: Linear regression
      linear_formula <- as.formula(paste("Points ~", elo_col))
      model <<- lm(linear_formula, data = race_df_75)
    }, error = function(e3) {
      # Level 4: Simple linear model (ultimate fallback)
      simple_formula <- as.formula(paste("Points ~", elo_col))
      model <<- lm(simple_formula, data = race_df_75)
    })
  })
})
```

**Advanced Prediction Error Handling with Row-by-Row Fallback**:
The system includes sophisticated error recovery that attempts multiple prediction strategies for both points and position probability models:

```r
# From race-picks.R:2242-2266
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

**Individual Points Testing Adjustments for Dual-Discipline Performance**:
Nordic Combined testing applies sophisticated historically-derived adjustments that account for systematic biases in dual-discipline prediction across both period and elevation conditions. The system uses statistical significance testing to identify genuine performance patterns during the testing phase, applying adjustments only when historical evidence supports systematic model bias.

**Sequential Testing Adjustment Framework**:
Adjustments are calculated and applied sequentially to avoid double-counting effects while maintaining chronological integrity (using only prior race data for each test):

```r
# From race-picks.R:1874-1903
# Step 1: Initial GAM prediction
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

# Step 3: Elevation adjustments with statistical testing  
elevation_p = purrr::map_dbl(row_id, function(r) {
  if(r <= 1) return(1)
  prior_elev_curr <- Prediction_Diff[Elevation_Flag == Elevation_Flag[r] & row_id < r]
  prior_elev_other <- Prediction_Diff[Elevation_Flag != Elevation_Flag[r] & row_id < r]
  if(length(prior_elev_curr) < 3 || length(prior_elev_other) < 3) return(1)
  tryCatch({
    t.test(prior_elev_curr, prior_elev_other)$p.value
  }, error = function(e) 1)
}),
elevation_correction = ifelse(elevation_p < 0.05,
                             mean(Prediction_Diff[Elevation_Flag == Elevation_Flag], na.rm = TRUE),
                             0),

# Step 4: Combined adjustment application
Adjusted_Prediction = Initial_Prediction + period_correction + elevation_correction
```

**Statistical Significance Requirements**: All adjustments require p < 0.05 from two-sample t-tests comparing current conditions (period/elevation) vs. other conditions, with minimum 3 observations per group for statistical validity. This ensures adjustments are applied only when there is strong evidence of systematic bias.

**Dual-Discipline Adjustment Categories**:

1. **Period Adjustments**: Account for seasonal progression effects across both ski jumping and cross-country skiing phases, as athletes' dual-discipline form develops differently throughout the Nordic Combined season (4 periods based on race progression)

2. **Elevation Adjustments**: Capture altitude effects on both jumping performance (air density affects ski jumping technique and distance) and cross-country endurance (altitude training effects), using 1300m threshold to distinguish high-altitude venues

**Race Probability Integration with Adjustment Framework**:
Testing adjustments are combined with race participation probability weighting to create final predictions that account for both historical bias correction and athlete likelihood to participate:

```r
# From race-picks.R:2352-2377
race_dfs[[i]] <- startlist_prepared %>%
  mutate(
    Base_Prediction = predict(model, newdata = .),
    
    # Apply historically-derived adjustments from testing phase
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

**Robust Error Handling for Adjustment Calculations**:
All adjustment calculations include comprehensive error handling with tryCatch blocks, defaulting to no adjustment (p-value = 1) when insufficient data or calculation errors occur, ensuring the prediction pipeline maintains stability across varying data quality conditions.

This dual-discipline testing adjustment framework ensures that Nordic Combined predictions account for systematic biases while preserving the complex interactions between ski jumping technique and cross-country skiing endurance that characterize this unique winter sport.

**ELO-to-PELO Conversion with Historical Reference Normalization**:
Nordic Combined employs sophisticated normalization using historical PELO maximum values to ensure consistency between training and testing phases:

```r
# From race-picks.R:1468-1502
for(i in seq_along(elo_columns_to_process)) {
  elo_col <- elo_columns_to_process[i]
  pelo_col <- pelo_columns_to_process[i]
  elo_pct_col <- paste0(str_replace(elo_col, "_Elo", "_Elo"), "_Pct")
  
  # Get max value for normalization from historical training data
  if(pelo_col %in% names(race_df)) {
    max_val <- max(race_df[[pelo_col]], na.rm = TRUE)
    result_df[[elo_pct_col]] <- result_df[[elo_col]] / max_val
  }
}
```

**Comprehensive Model Validation and Confidence Assessment**:
The system incorporates multiple validation metrics and confidence measures specific to Nordic Combined's dual-discipline requirements:

```r
# From race-picks.R:1940-1943
predicted_probs <- predict(position_model, newdata = race_df, type = "response")
brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)

# Confidence factor based on recent race participation
confidence_factor = pmin(n_recent_races / 10, 1)
```

**Individual vs Team Format Adaptation**:
The testing modeling dynamically adapts between individual and team competitions, with appropriate handling for team-averaged performance metrics while maintaining the same statistical rigor and dual-discipline awareness that characterizes Nordic Combined performance prediction across all event formats.

**Position Probability Prediction Pipeline**: The testing system creates structured position probability predictions for each threshold with comprehensive model compatibility checking:

```r
# From race-picks.R:2169-2186
# NEW CODE: Make position probability predictions with adjustments
position_preds <- data.frame(startlist_prepared[[participant_col]])
names(position_preds)[1] <- participant_col

# Add ID, Nation, Sex for individual races
if(!is_team) {
  position_preds$ID <- startlist_prepared$ID
  position_preds$Nation <- startlist_prepared$Nation
  position_preds$Sex <- startlist_prepared$Sex
}

# Add race number
position_preds$Race <- i

# Add race probability column for later normalization
if(race_prob_col %in% names(startlist_prepared)) {
  position_preds[[race_prob_col]] <- startlist_prepared[[race_prob_col]]
}
```

**Model Variable Compatibility Verification**: The system validates and adapts prediction data to ensure compatibility with trained position models:

```r
# From race-picks.R:2200-2241
# Check what variables the model actually needs
model_vars <- names(pos_model$var.summary)
log_info(paste("Model for threshold", threshold, "requires variables:", paste(model_vars, collapse=", ")))

# Create a clean subset of prediction data with only required variables
prediction_subset <- startlist_prepared

# Explicitly check for each variable
for(var in model_vars) {
  if(!(var %in% names(prediction_subset))) {
    log_warn(paste("Missing required variable:", var, "- adding with default values"))
    # Add missing variable with appropriate default value
    prediction_subset[[var]] <- 0
  } else {
    # Ensure the variable has the right type
    model_var_type <- class(pos_model$var.summary[[var]])
    data_var_type <- class(prediction_subset[[var]])
    
    if(!identical(model_var_type, data_var_type)) {
      log_warn(paste("Variable type mismatch for", var, ":", 
                     "model expects", model_var_type, "but got", data_var_type))
      # Convert to correct type
      if(model_var_type == "numeric") {
        prediction_subset[[var]] <- as.numeric(prediction_subset[[var]])
      } else if(model_var_type == "factor") {
        prediction_subset[[var]] <- as.factor(prediction_subset[[var]])
      }
    }
  }
}
```

**Adaptive Position Probability Prediction**: The testing system implements robust prediction strategies with comprehensive error handling and row-by-row fallback mechanisms:

```r
# From race-picks.R:2243-2266
# Make predictions with explicit try-catch
base_predictions <- tryCatch({
  # Debug output
  log_info(paste("Attempting prediction for threshold", threshold, "with", nrow(prediction_subset), "rows"))
  
  # Explicit call to mgcv::predict.gam to avoid method dispatch issues
  mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response")
}, error = function(e) {
  log_warn(paste("Prediction call failed:", e$message))
  
  # Try alternative prediction approach with one row at a time
  log_info("Trying row-by-row prediction as fallback")
  result <- numeric(nrow(prediction_subset))
  
  for(j in 1:nrow(prediction_subset)) {
    single_row <- prediction_subset[j,, drop = FALSE]
    result[j] <- tryCatch({
      mgcv::predict.gam(pos_model, newdata = single_row, type = "response")
    }, error = function(e2) {
      log_warn(paste("Failed on row", j, ":", e2$message))
      threshold/100  # Default value based on threshold
    })
  }
  return(result)
})
```

**Position Probability Adjustment Application**: The system applies statistical adjustments to position probabilities using period-specific corrections:

```r
# From race-picks.R:2272-2302
# Apply adjustments if available
if(adj_name %in% names(position_adjustments)) {
  # Get adjustments
  pos_adj <- position_adjustments[[adj_name]]
  
  # Join with predictions
  position_preds <- position_preds %>%
    left_join(pos_adj, by = participant_col) %>%
    mutate(
      # Replace NAs with zeros
      period_effect = replace_na(period_effect, 0),
      
      # Apply adjustments
      period_adjustment = period_effect,
      
      # Calculate adjusted probabilities
      adjusted_prob = get(paste0(prob_col, "_base")) + period_adjustment,
      
      # Ensure probabilities are between 0 and 1
      adjusted_prob = pmin(pmax(adjusted_prob, 0), 1)
    )
  
  # Use adjusted probability as final
  position_preds[[prob_col]] <- position_preds$adjusted_prob
} else {
  # Use base prediction if no adjustments
  position_preds[[prob_col]] <- position_preds[[paste0(prob_col, "_base")]]
}

# Convert to percentage and round
position_preds[[prob_col]] <- round(position_preds[[prob_col]] * 100, 1)
```

**Position Probability Normalization**: The testing system normalizes position probabilities to ensure mathematical consistency across all thresholds:

```r
# From race-picks.R:2324-2336
# Normalize position probabilities to ensure they sum to the correct totals
position_preds <- normalize_position_probabilities(position_preds, race_prob_col, position_thresholds)

# Add verification logging for each threshold
log_info(sprintf("Race %d position probability sums after normalization:", i))
for(threshold in position_thresholds) {
  prob_col <- paste0("prob_top", threshold)
  if(prob_col %in% names(position_preds)) {
    sum_val <- sum(position_preds[[prob_col]], na.rm = TRUE)
    log_info(sprintf("  %s: %.2f%% (should be %d%%)", 
                     prob_col, sum_val, 100 * threshold))
  }
}
```

**Points Prediction with Volatility Integration**: The testing system generates comprehensive points predictions incorporating volatility metrics and confidence-weighted scenarios:

```r
# From race-picks.R:2352-2399
# Prepare startlist points predictions (original functionality)
race_dfs[[i]] <- startlist_prepared %>%
  mutate(
    Base_Prediction = predict(model, newdata = .),
  ) %>%
  left_join(participant_adjustments, by = participant_col) %>%
  mutate(
    # Regular adjustments
    period_effect = replace_na(period_effect, 0),
    elevation_effect = replace_na(elevation_effect, 0),
    
    # Volatility metrics
    prediction_volatility = replace_na(prediction_volatility, 0),
    consistency_score = replace_na(consistency_score, 0),
    upside_potential = replace_na(upside_potential, 0),
    downside_risk = replace_na(downside_risk, 0),
    volatility_ratio = replace_na(volatility_ratio, 1),
    n_recent_races = replace_na(n_recent_races, 0),
    
    # Base prediction and adjustments
    Predicted_Points = Base_Prediction + period_adjustment + elevation_adjustment,
    Predicted_Points = pmax(pmin(Predicted_Points, 100), 0),
    
    # Apply race probability to predictions
    Race_Prob = get(race_prob_col),
    Final_Prediction = Predicted_Points * Race_Prob,
    
    # Different scoring scenarios - adjusted by race probability
    confidence_factor = pmin(n_recent_races / 10, 1),
    scaled_upside_potential = upside_potential * (Predicted_Points/100),
    scaled_downside_potential = downside_risk * (Predicted_Points/100),
    
    # Safe prediction (downside)
    Safe_Prediction = pmax(
      (Predicted_Points - (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
      0
    ),
    
    # Upside prediction
    Upside_Prediction = pmin(
      (Predicted_Points + (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
      100 * Race_Prob  # Cap at 100 * probability
    )
  )
```

The Nordic Combined testing modeling system provides comprehensive dual-prediction architecture that generates both position probabilities and points forecasts with sophisticated adjustment integration, robust error handling, and mathematical consistency validation across all race scenarios and athlete participation patterns.

###### Adjustments

Nordic Combined points testing implements comprehensive adjustment validation and verification systems that ensure prediction accuracy through statistical correction integration, probability normalization validation, and multi-scenario confidence weighting. The testing adjustment phase validates that all corrections are properly applied and maintains mathematical consistency across prediction scenarios.

**Adjustment Integration Validation**: The testing system validates that all calculated adjustments are properly integrated into final predictions:

```r
# From race-picks.R:2357-2377
race_dfs[[i]] <- startlist_prepared %>%
  mutate(
    Base_Prediction = predict(model, newdata = .),
  ) %>%
  left_join(participant_adjustments, by = participant_col) %>%
  mutate(
    # Regular adjustments
    period_effect = replace_na(period_effect, 0),
    elevation_effect = replace_na(elevation_effect, 0),
    
    # Volatility metrics
    prediction_volatility = replace_na(prediction_volatility, 0),
    consistency_score = replace_na(consistency_score, 0),
    upside_potential = replace_na(upside_potential, 0),
    downside_risk = replace_na(downside_risk, 0),
    volatility_ratio = replace_na(volatility_ratio, 1),
    n_recent_races = replace_na(n_recent_races, 0),
    
    # Using existing adjustment approach
    period_adjustment = period_effect,
    elevation_adjustment = elevation_effect,
    
    # Base prediction and adjustments
    Predicted_Points = Base_Prediction + period_adjustment + elevation_adjustment,
    Predicted_Points = pmax(pmin(Predicted_Points, 100), 0)
  )
```

**Position Probability Normalization Verification**: The system validates that position probabilities sum to mathematically correct totals after adjustment application:

```r
# From race-picks.R:2324-2336
# Normalize position probabilities to ensure they sum to the correct totals
position_preds <- normalize_position_probabilities(position_preds, race_prob_col, position_thresholds)

# Add verification logging for each threshold
log_info(sprintf("Race %d position probability sums after normalization:", i))
for(threshold in position_thresholds) {
  prob_col <- paste0("prob_top", threshold)
  if(prob_col %in% names(position_preds)) {
    sum_val <- sum(position_preds[[prob_col]], na.rm = TRUE)
    log_info(sprintf("  %s: %.2f%% (should be %d%%)", 
                     prob_col, sum_val, 100 * threshold))
  }
}
```

**Race Probability Conservation Monitoring**: The testing system monitors race probability preservation throughout the adjustment process:

```r
# From race-picks.R:2341-2350
# Check if probabilities are getting lost
log_info(paste("Race", i, "probability check:"))
prob_summary <- startlist_prepared %>%
  group_by(if(!is_team) Nation else NULL) %>%
  summarise(
    mean_prob = mean(get(race_prob_col), na.rm = TRUE),
    sum_prob = sum(get(race_prob_col), na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(sum_prob))
```

**Multi-Scenario Confidence Weighting**: The testing system applies confidence-weighted adjustments across different prediction scenarios:

```r
# From race-picks.R:2383-2398
# Different scoring scenarios - adjusted by race probability
confidence_factor = pmin(n_recent_races / 10, 1),
scaled_upside_potential = upside_potential * (Predicted_Points/100),
scaled_downside_potential = downside_risk * (Predicted_Points/100),

# Safe prediction (downside)
Safe_Prediction = pmax(
  (Predicted_Points - (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
  0
),

# Upside prediction
Upside_Prediction = pmin(
  (Predicted_Points + (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
  100 * Race_Prob  # Cap at 100 * probability
)
```

**Final Adjustment Summary Generation**: The testing system creates comprehensive adjustment summaries for each race prediction:

```r
# From race-picks.R:1147-1159, 1195-1218
final_predictions <- race_dfs[[1]] %>%
  rename(
    Race1_Base = Base_Prediction,
    Race1_Period = period_adjustment,
    Race1_Elevation = elevation_adjustment,
    Race1_Points = Final_Prediction,
    Race1_Safe = Safe_Prediction,
    Race1_Upside = Upside_Prediction,
    Race1_Volatility = prediction_volatility,
    Race1_Ratio = volatility_ratio,
    Race1_Confidence = confidence_factor,
    Race1_Probability = Race1_Prob
  )

# For subsequent races
final_predictions <- final_predictions %>%
  left_join(
    race_dfs[[i]] %>%
      rename(
        !!paste0("Race", i, "_Base") := Base_Prediction,
        !!paste0("Race", i, "_Period") := period_adjustment,
        !!paste0("Race", i, "_Elevation") := elevation_adjustment,
        !!paste0("Race", i, "_Points") := Final_Prediction,
        !!paste0("Race", i, "_Safe") := Safe_Prediction,
        !!paste0("Race", i, "_Upside") := Upside_Prediction,
        !!paste0("Race", i, "_Volatility") := prediction_volatility,
        !!paste0("Race", i, "_Ratio") := volatility_ratio,
        !!paste0("Race", i, "_Confidence") := confidence_factor,
        !!paste0("Race", i, "_Probability") := !!sym(paste0("Race", i, "_Prob"))
      )
  )
```

**Adjustment Output Validation**: The testing system validates that all adjustment components are properly captured in the final output:

```r
# From race-picks.R:2400-2405
select(all_of(participant_col), 
       if(!is_team) "Nation" else NULL,
       Base_Prediction, period_adjustment, elevation_adjustment,
       prediction_volatility, volatility_ratio, confidence_factor,
       Final_Prediction, Safe_Prediction, Upside_Prediction,
       race_prob_col)
```

The Nordic Combined testing adjustments system ensures comprehensive validation of all statistical corrections, maintains mathematical consistency across prediction scenarios, and provides detailed adjustment tracking that enables robust performance evaluation and model refinement across all race formats and competition conditions.

#### Probability

##### Training

###### Setup

Nordic Combined probability training establishes comprehensive race participation prediction systems through exponential decay algorithms, FIS startlist integration, and multi-format race probability calculation. The setup phase prepares specialized data structures that capture historical participation patterns and generate dynamic probability assignments for individual and team events.

**Core Probability Calculation Framework**: The system implements exponential decay algorithms to weight recent participation patterns more heavily than historical data:

```r
# From race-picks.R:228-270
# Function to get race probability for a skier/team using exponential decay
get_race_probability <- function(chronos, participant, racetype, is_team = FALSE) {
  log_debug(paste("Calculating exponential decay probability for participant:", participant))
  
  # For team, participant is a Nation
  id_col <- if(is_team) "Nation" else "Skier"
  
  # Get participant's race history for this race type
  participant_races <- chronos %>%
    filter(
      get(id_col) == participant,
      RaceType == racetype
    ) %>%
    arrange(Date, Season, Race)
  
  if(nrow(participant_races) == 0) {
    log_debug(paste("No race history found for participant:", participant, "in race type:", racetype))
    return(0)
  }
  
  # Calculate exponential decay probability
  total_races <- nrow(participant_races)
  
  if(total_races > 0) {
    # Create exponential decay weights (α = 0.1)
    race_weights <- exp(-0.1 * ((total_races-1):0))
    
    # Create participation vector (1 if skier participated, 0 if not)
    # Since they're all in the chronos data, they all participated
    participation <- rep(1, total_races)
    
    # Calculate weighted probability
    weighted_participation <- sum(participation * race_weights)
    total_weight <- sum(race_weights)
    prob <- weighted_participation / total_weight
    
    log_debug(paste("Exponential decay probability for", participant, ":", round(prob, 3), 
                    "based on", total_races, "races"))
    
    return(prob)
  }
  
  return(0)
}
```

**Data Source Loading and Validation**: The system loads comprehensive historical data and startlist information for probability calculation:

```r
# From race-picks.R:134-165
calculate_race_probabilities <- function() {
  log_info("Calculating race participation probabilities")
  
  # Initialize empty data frames
  men_startlist <- data.frame()
  ladies_startlist <- data.frame()
  
  men_chrono <- data.frame()
  ladies_chrono <- data.frame()
  
  # Only load data for race types that are happening
  if(nrow(men_races) > 0) {
    log_info("Reading men's chronological data and startlist")
    men_chrono <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/men_chrono_elevation.csv", 
                           stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
    
    men_startlist <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/startlist_races_men.csv", 
                              stringsAsFactors = FALSE)
    men_startlist$Sex = "M"
  }
  
  if(nrow(ladies_races) > 0) {
    log_info("Reading ladies' chronological data and startlist")
    ladies_chrono <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/ladies_chrono_elevation.csv", 
                              stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
    
    ladies_startlist <- read.csv("~/ski/elo/python/nordic-combined/polars/excel365/startlist_races_ladies.csv", 
                                 stringsAsFactors = FALSE)
    ladies_startlist$Sex = "L"
  }
}
```

**FIS Startlist Integration Logic**: The system implements sophisticated logic to handle official FIS startlists when available, preserving confirmed participation while calculating probabilities for other races:

```r
# From race-picks.R:273-316
process_individual_probabilities <- function(startlist, chronos, races, gender) {
  # Check if we have FIS startlist (at least one skier has In_Startlist=True)
  has_fis_startlist <- FALSE
  if("In_Startlist" %in% names(startlist)) {
    has_fis_startlist <- any(startlist$In_Startlist, na.rm = TRUE)
  }
  
  # Handle Race1_Prob based on FIS startlist existence
  if(has_fis_startlist) {
    log_info("FIS startlist exists (at least one In_Startlist=True), keeping existing Race1_Prob values")
    # Keep existing Race1_Prob if it exists, otherwise set based on In_Startlist
    if(!"Race1_Prob" %in% names(startlist)) {
      startlist$Race1_Prob <- ifelse(startlist$In_Startlist, 1, 0)
    }
  } else {
    log_info("No FIS startlist (all In_Startlist=False), will calculate Race1_Prob")
    # Race1_Prob will be calculated like other races below
  }
  
  # Process each race
  for(i in 1:nrow(races)) {
    race_prob_col <- paste0("Race", i, "_Prob")
    
    # Skip Race1_Prob if we have a FIS startlist and it's already set
    if(race_prob_col == "Race1_Prob" && has_fis_startlist && race_prob_col %in% names(startlist)) {
      log_info("Using existing Race1_Prob from FIS startlist, skipping calculation")
      next
    }
    
    # Create the column if it doesn't exist
    if(!(race_prob_col %in% names(startlist))) {
      startlist[[race_prob_col]] <- NA_real_
    }
    
    # Process probabilities for each skier
    for(j in 1:nrow(startlist)) {
      skier <- startlist$Skier[j]
      startlist[j, race_prob_col] <- 
        get_race_probability(chronos, skier, races$racetype[i], is_team = FALSE)
    }
  }
  
  return(startlist)
}
```

**Team Event Probability Handling**: For team events, the system implements simplified probability assignment recognizing the different participation dynamics:

```r
# From race-picks.R:318-336
# Process team race probabilities - set all to 1 for simplicity
process_team_probabilities <- function(startlist, races) {
  log_info("Processing team race probabilities - setting all to 1")
  log_info(paste("Input races dataframe has", nrow(races), "rows"))
  
  # We should treat this as a single race type regardless of how many rows are in races
  # So we'll just set Race1_Prob for all teams
  race_prob_col <- "Race1_Prob"
  
  # Create the column if it doesn't exist
  if(!(race_prob_col %in% names(startlist))) {
    startlist[[race_prob_col]] <- 1  # Set probability to 1 for all teams
    log_info(paste("Set", race_prob_col, "to 1 for all", nrow(startlist), "teams"))
  } else {
    log_info(paste(race_prob_col, "already exists in startlist"))
  }
  
  return(startlist)
}
```

**Multi-Format Team Startlist Loading**: The system handles diverse team event formats with specialized loading procedures:

```r
# From race-picks.R:175-225
# For team events
if(nrow(men_teams) > 0) {
  men_team_file <- "~/ski/elo/python/nordic-combined/polars/relay/excel365/startlist_team_races_men.csv"
  if(file.exists(men_team_file)) {
    men_team_startlist <- read.csv(men_team_file, stringsAsFactors = FALSE)
    log_info(paste("Loaded men's team startlist with", nrow(men_team_startlist), "teams"))
  } else {
    log_warn("Men's team startlist file not found")
  }
}

# Team sprint events
if(nrow(men_team_sprint) > 0) {
  men_team_sprint_file <- "~/ski/elo/python/nordic-combined/polars/relay/excel365/startlist_team_sprint_races_men.csv"
  if(file.exists(men_team_sprint_file)) {
    men_team_sprint_startlist <- read.csv(men_team_sprint_file, stringsAsFactors = FALSE)
    log_info(paste("Loaded men's team sprint startlist with", nrow(men_team_sprint_startlist), "teams"))
  } else {
    log_warn("Men's team sprint startlist file not found")
  }
}

# Mixed team events
if(nrow(mixed_teams) > 0) {
  mixed_team_file <- "~/ski/elo/python/nordic-combined/polars/relay/excel365/startlist_mixed_team_races_teams.csv"
  if(file.exists(mixed_team_file)) {
    mixed_team_startlist <- read.csv(mixed_team_file, stringsAsFactors = FALSE)
    log_info(paste("Loaded mixed team startlist with", nrow(mixed_team_startlist), "teams"))
  } else {
    log_warn("Mixed team startlist file not found")
  }
}
```

**Gender-Specific Processing Pipeline**: The system implements parallel processing for men's and ladies' events with unified probability calculation:

```r
# From race-picks.R:338-348
# Process each gender and teams
if(nrow(men_races) > 0 && nrow(men_startlist) > 0) {
  log_info("Processing men's race probabilities")
  men_startlist_with_probs <- process_individual_probabilities(men_startlist, men_chrono, men_races, "M")
} else {
  men_startlist_with_probs <- men_startlist
}

if(nrow(ladies_races) > 0 && nrow(ladies_startlist) > 0) {
  log_info("Processing ladies' race probabilities")
  ladies_startlist_with_probs <- process_individual_probabilities(ladies_startlist, ladies_chrono, ladies_races, "L")
} else {
  ladies_startlist_with_probs <- ladies_startlist
}
```

The Nordic Combined probability training setup provides comprehensive participation prediction through exponential decay weighting, intelligent FIS startlist integration, specialized team event handling, and robust multi-format processing that captures the dynamic participation patterns across all Nordic Combined competition formats.

###### Feature Selection

Nordic Combined probability training employs sophisticated feature selection that leverages the established points prediction variables while implementing threshold-specific optimization for position probability models. The feature selection process uses exhaustive subset selection with Bayesian Information Criterion optimization to identify optimal variable combinations for each position probability threshold.

**Unified Feature Base Inheritance**: The probability training system inherits the proven feature set from the points prediction model to maintain consistency across prediction types:

```r
# From race-picks.R:1911-1912
# Feature selection - use the same explanatory variables as the points model
position_feature_vars <- explanatory_vars
```

**Core Feature Variable Definitions**: The system uses comprehensive feature sets that adapt based on event type (individual vs. team) and include specialized Nordic Combined performance metrics:

```r
# From race-picks.R:1860-1871
if(is_team) {
  explanatory_vars <- c("Prev_Points_Weighted", 
                        "Avg_Sprint_Elo_Pct", "Avg_Individual_Elo_Pct", 
                        "Avg_MassStart_Elo_Pct", "Avg_IndividualCompact_Elo_Pct", 
                        "Avg_Elo_Pct")
} else {
  explanatory_vars <- c("Prev_Points_Weighted", 
                        "Sprint_Elo_Pct", "Individual_Elo_Pct", 
                        "MassStart_Elo_Pct", "IndividualCompact_Elo_Pct", 
                        "Elo_Pct")
}
```

**Binary Outcome Variable Generation**: For each position threshold, the system creates binary outcome variables that capture whether athletes achieved specific position targets:

```r
# From race-picks.R:1918-1919
# Create binary outcome variable for position threshold
race_df$position_achieved <- race_df$Place <= threshold
```

**Threshold-Specific Feature Optimization**: The system implements exhaustive subset selection for each position threshold using BIC optimization to identify the most predictive variable combinations:

```r
# From race-picks.R:1921-1928
# Create formula for regsubsets using the same explanatory variables as the points model
pos_formula <- as.formula(paste("position_achieved ~", paste(position_feature_vars, collapse = " + ")))

# Use regsubsets to select best features for this position threshold
tryCatch({
  pos_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")
  pos_summary <- summary(pos_selection)
  pos_best_bic_vars <- names(coef(pos_selection, which.min(pos_summary$bic)))
```

**GAM-Compatible Smooth Term Generation**: Selected variables are transformed into smooth terms for Generalized Additive Model implementation with binomial family distribution:

```r
# From race-picks.R:1930-1933
# Create smooth terms for GAM using best BIC variables (remove intercept)
pos_smooth_terms <- paste("s(", pos_best_bic_vars[-1], ")", collapse=" + ")
pos_gam_formula <- as.formula(paste("position_achieved ~", pos_smooth_terms))
```

**Feature Selection Validation and Logging**: The system provides comprehensive logging of selected variables for each threshold to ensure transparency and model interpretability:

```r
# From race-picks.R:1945-1947
# Log selected variables
log_info(paste("Variables selected for", threshold, "position model:", 
               paste(pos_best_bic_vars[-1], collapse=", ")))
```

**Nordic Combined-Specific ELO Feature Integration**: The feature selection incorporates race-type specific ELO ratings that capture Nordic Combined performance nuances:

- **Sprint_Elo_Pct**: Performance rating for sprint-format Nordic Combined events
- **Individual_Elo_Pct**: Rating for traditional individual Nordic Combined competitions  
- **MassStart_Ct**: Performance metric for mass start format events
- **IndividualCompact_Elo_Pct**: Rating for compact hill individual events
- **Elo_Pct**: Overall Nordic Combined performance rating

**Weighted Historical Points Integration**: The system incorporates weighted historical performance through the `Prev_Points_Weighted` variable, providing continuity with recent competitive results.

**Team-Specific Aggregated Features**: For team events, the feature selection uses nation-level aggregated ELO ratings:

- **Avg_Sprint_Elo_Pct**: Team average sprint ELO performance
- **Avg_Individual_Elo_Pct**: Team average individual event rating
- **Avg_MassStart_Elo_Pct**: Team average mass start performance
- **Avg_IndividualCompact_Elo_Pct**: Team average compact hill rating
- **Avg_Elo_Pct**: Overall team performance average

**Exhaustive Search Methodology**: The feature selection employs comprehensive exhaustive subset selection that evaluates all possible variable combinations within each threshold, ensuring optimal predictive power while maintaining model parsimony through BIC criterion.

The Nordic Combined probability training feature selection provides systematic optimization of predictor variables across position probability thresholds while maintaining consistency with points prediction methodology and incorporating sport-specific performance metrics that capture the unique demands of Nordic Combined competition across all event formats.

###### Modeling

Nordic Combined probability training employs sophisticated Generalized Additive Models with binomial family distributions to generate position probability predictions across multiple achievement thresholds. The modeling framework implements comprehensive validation through Brier score evaluation, hierarchical fallback strategies, and specialized period adjustment integration for robust probability estimation.

**Primary GAM Binomial Modeling**: The system implements GAM models with binomial family distributions specifically designed for binary position achievement outcomes:

```r
# From race-picks.R:1934-1938
# Fit the position model with binomial family
position_model <- gam(pos_gam_formula,
                      data = race_df,
                      family = binomial,
                      method = "REML")
```

**Multi-Threshold Position Model Architecture**: The system creates dedicated models for each position threshold using position-specific binary outcomes and optimized feature sets:

```r
# From race-picks.R:1914-1933
# Create models for each position threshold
for(threshold in position_thresholds) {
  log_info(paste("Creating model for top", threshold, "positions"))
  
  # Create binary outcome variable for position threshold
  race_df$position_achieved <- race_df$Place <= threshold
  
  # Create formula for regsubsets using the same explanatory variables as the points model
  pos_formula <- as.formula(paste("position_achieved ~", paste(position_feature_vars, collapse = " + ")))
  
  # Use regsubsets to select best features for this position threshold
  tryCatch({
    pos_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")
    pos_summary <- summary(pos_selection)
    pos_best_bic_vars <- names(coef(pos_selection, which.min(pos_summary$bic)))
    
    # Create smooth terms for GAM using best BIC variables (remove intercept)
    pos_smooth_terms <- paste("s(", pos_best_bic_vars[-1], ")", collapse=" + ")
    pos_gam_formula <- as.formula(paste("position_achieved ~", pos_smooth_terms))
```

**Brier Score Model Validation**: The system implements comprehensive probabilistic accuracy assessment using Brier scores for each position threshold model:

```r
# From race-picks.R:1940-1943
# Calculate Brier score for model evaluation
predicted_probs <- predict(position_model, newdata = race_df, type = "response")
brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
log_info(paste("Brier score for threshold", threshold, ":", round(brier_score, 4)))
```

**Model Storage and Organization**: The system maintains organized model storage for efficient retrieval during prediction phases:

```r
# From race-picks.R:1949-1950
# Store the model
position_models[[paste0("threshold_", threshold)]] <- position_model
```

**Initial Probability Prediction Generation**: The system generates baseline probability predictions that serve as the foundation for subsequent adjustment calculations:

```r
# From race-picks.R:1961-1962
# Add predictions separately (outside of mutate)
position_df$initial_prob <- predict(position_model, newdata = position_df, type = "response")
```

**Hierarchical Fallback Strategy Implementation**: The system implements robust error handling with progressively simpler models to ensure prediction continuity:

```r
# From race-picks.R:2000-2013
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
```

**Ultimate Fallback Model Creation**: For extreme error scenarios, the system provides minimal viable models using only core ELO performance metrics:

```r
# From race-picks.R:2024-2031
# Last resort fallback - just use the elo column
fallback_formula <- as.formula(paste("position_achieved ~ s(", elo_col, ")"))
position_models[[paste0("threshold_", threshold)]] <- gam(
  fallback_formula,
  data = race_df,
  family = binomial,
  method = "REML"
)
```

**Fallback Adjustment Structure Creation**: The system generates appropriate adjustment structures for fallback models to maintain prediction pipeline consistency:

```r
# From race-picks.R:2015-2019, 2033-2037
# Create empty adjustments object since we can't calculate them for the fallback model
empty_adjustments <- data.frame(x = unique(race_df[[participant_col]]))
names(empty_adjustments)[1] <- participant_col
empty_adjustments$period_effect <- 0
position_adjustments[[paste0("threshold_", threshold)]] <- empty_adjustments
```

**Comprehensive Model Logging and Validation**: The system provides detailed logging for model creation success and variable selection transparency:

```r
# From race-picks.R:1945-1947, 2021-2022, 2039-2040
# Log selected variables
log_info(paste("Variables selected for", threshold, "position model:", 
               paste(pos_best_bic_vars[-1], collapse=", ")))

log_info(paste("Created fallback model for threshold", threshold, 
               "using variables:", paste(fallback_vars, collapse=", ")))

log_info(paste("Created last-resort fallback model for threshold", threshold, 
               "using only", elo_col))
```

**Position Achievement Binary Encoding**: The modeling system uses precise binary encoding for position achievement that captures whether athletes finished within specific thresholds:

- **Threshold 1**: Top 1 position (win probability)
- **Threshold 3**: Top 3 positions (podium probability) 
- **Threshold 5**: Top 5 positions (strong finish probability)
- **Threshold 10**: Top 10 positions (solid performance probability)
- **Threshold 30**: Top 30 positions (points-scoring probability)

**REML Optimization**: All GAM models use Restricted Maximum Likelihood (REML) optimization for robust parameter estimation that accounts for smoothing parameter uncertainty.

**Probabilistic Output Generation**: The binomial family GAM models generate probability outputs in the [0,1] range using logistic link functions, providing intuitive position achievement probabilities for each threshold.

The Nordic Combined probability training modeling provides comprehensive position probability prediction through sophisticated GAM implementations, robust validation metrics, intelligent fallback strategies, and threshold-specific optimization that ensures reliable probabilistic forecasting across all Nordic Combined competition scenarios and data quality conditions.

###### Adjustments

Nordic Combined probability training implements sophisticated statistical adjustment mechanisms that identify and correct for systematic biases in position probability predictions through period-specific analysis and probability difference calculations. The adjustment system uses t-test significance testing to determine when corrections are warranted and applies bounded probability adjustments for each position threshold.

**Chronological Data Preparation for Adjustment Analysis**: The system prepares historical data with proper chronological ordering and participant-specific row identification for temporal bias detection:

```r
# From race-picks.R:1952-1959
# Calculate adjustments for period for this threshold
position_df <- race_df %>%
  arrange(Date) %>%
  group_by(!!sym(participant_col)) %>%
  mutate(
    row_id = row_number()
  ) %>%
  ungroup()
```

**Initial Probability Prediction Integration**: The system generates baseline probability predictions for all historical data to establish the foundation for bias detection:

```r
# From race-picks.R:1961-1962
# Add predictions separately (outside of mutate)
position_df$initial_prob <- predict(position_model, newdata = position_df, type = "response")
```

**Probability Difference Calculation and Period Analysis**: The system calculates prediction differences between actual outcomes and model predictions, then performs period-specific statistical testing:

```r
# From race-picks.R:1964-1983
# Continue with period adjustment calculations
position_df <- position_df %>%
  group_by(!!sym(participant_col)) %>%
  mutate(
    prob_diff = as.numeric(position_achieved) - initial_prob,
    
    # Calculate period adjustments
    period_p = purrr::map_dbl(row_id, function(r) {
      if(r <= 1) return(1)
      prior_period_curr <- prob_diff[Period == Period[r] & row_id < r]
      prior_period_other <- prob_diff[Period != Period[r] & row_id < r]
      if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
      tryCatch({
        t.test(prior_period_curr, prior_period_other)$p.value
      }, error = function(e) 1)
    }),
    period_correction = ifelse(period_p < 0.05,
                               mean(prob_diff[Period == Period], na.rm = TRUE),
                               0),
    period_adjusted = pmin(pmax(initial_prob + period_correction, 0), 1)
  ) %>%
  ungroup()
```

**Statistical Significance Testing Protocol**: The adjustment system implements rigorous t-test protocols that compare period-specific performance differences against other periods to identify systematic biases:

- **Minimum Sample Requirement**: Requires at least 3 observations in both current and other periods before conducting statistical tests
- **Significance Threshold**: Uses p < 0.05 as the threshold for applying period corrections
- **Error Handling**: Comprehensive error handling with fallback to no adjustment (p = 1) when tests fail

**Period Correction Calculation and Application**: When statistical significance is detected, the system calculates and applies mean-based corrections while maintaining probability bounds:

```r
# From race-picks.R:1980-1983
period_correction = ifelse(period_p < 0.05,
                          mean(prob_diff[Period == Period], na.rm = TRUE),
                          0),
period_adjusted = pmin(pmax(initial_prob + period_correction, 0), 1)
```

**Participant-Specific Adjustment Summarization**: The system extracts final adjustment values for each participant to enable application during testing phases:

```r
# From race-picks.R:1987-1992
# Get final adjustments for each participant
participant_pos_adjustments <- position_df %>%
  group_by(!!sym(participant_col)) %>%
  summarise(
    period_effect = last(period_correction)
  )
```

**Threshold-Specific Adjustment Storage**: Each position threshold maintains its own adjustment structure for independent bias correction:

```r
# From race-picks.R:1994-1995
# Store adjustments for this threshold
position_adjustments[[paste0("threshold_", threshold)]] <- participant_pos_adjustments
```

**Fallback Adjustment Structure Creation**: For models that fail primary training, the system creates null adjustment structures to maintain pipeline consistency:

```r
# From race-picks.R:2015-2019, 2033-2037
# Create empty adjustments object since we can't calculate them for the fallback model
empty_adjustments <- data.frame(x = unique(race_df[[participant_col]]))
names(empty_adjustments)[1] <- participant_col
empty_adjustments$period_effect <- 0
position_adjustments[[paste0("threshold_", threshold)]] <- empty_adjustments
```

**Probability Boundary Enforcement**: All adjusted probabilities are constrained to the valid [0,1] range using minimum and maximum functions to ensure mathematical consistency.

**Temporal Bias Detection Methodology**: The adjustment system specifically targets temporal biases that may arise from:

- **Seasonal Performance Variations**: Differences in athlete performance across competition periods
- **Training Cycle Effects**: Systematic changes in competitive readiness between periods
- **Equipment Evolution**: Impact of technological or equipment changes on performance patterns
- **Rule Modifications**: Effects of competitive rule changes between seasons

**Threshold-Independent Adjustment Calculation**: Each position threshold (top 1, 3, 5, 10, 30) receives independent adjustment calculation, recognizing that biases may affect different achievement levels differently.

**Robust Statistical Foundation**: The adjustment methodology employs established statistical testing (t-tests) with appropriate sample size requirements and error handling to ensure reliable bias detection while avoiding false adjustments due to random variation.

The Nordic Combined probability training adjustments provide statistically-grounded correction mechanisms that identify and address systematic biases in position probability predictions through rigorous period-specific analysis, bounded probability adjustments, and comprehensive error handling that maintains prediction accuracy across varying competitive conditions and temporal effects.

##### Testing

###### Startlist Setup

Nordic Combined probability testing implements comprehensive startlist preparation that preserves race participation probabilities while ensuring model compatibility through dynamic column detection, probability validation, and robust fallback mechanisms. The testing setup maintains probability data integrity throughout the prediction pipeline while adapting to various startlist configurations and race scenarios.

**Dynamic Race Probability Column Detection**: The testing system automatically identifies and preserves all race participation probability columns using pattern matching:

```r
# From race-picks.R:1299-1301, 1782-1784
# Dynamically get race probability columns - important to preserve these!
race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
log_info(paste("Race probability columns found:", paste(race_prob_cols, collapse=", ")))

# Debug: Show race probability columns in startlist
race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
log_info(paste("Race probability columns in startlist:", paste(race_prob_cols, collapse=", ")))
```

**Race-Specific Probability Column Validation**: For each race, the system validates that corresponding probability columns exist and contain meaningful data:

```r
# From race-picks.R:1792-1811
# Get race probability column name for this race
race_prob_col <- paste0("Race", i, "_Prob")

# Debug: Check if this race probability column exists in startlist
if(race_prob_col %in% names(startlist)) {
  log_info(paste("Race probability column", race_prob_col, "exists in startlist"))
  # Show some stats
  log_info(paste("  Mean:", mean(startlist[[race_prob_col]], na.rm = TRUE)))
  log_info(paste("  Sum:", sum(startlist[[race_prob_col]], na.rm = TRUE)))
} else {
  log_warn(paste("Race probability column", race_prob_col, "NOT FOUND in startlist!"))
}
```

**Startlist Data Preparation with Probability Preservation**: The system prepares startlist data while maintaining all probability columns for both individual and team events:

```r
# From race-picks.R:1314-1316, 1388-1390
# For teams - select needed columns from startlist
result_cols <- c("Nation", elo_cols, race_prob_cols)
result_df <- startlist %>%
  select(any_of(result_cols))

# For individual races, use the original approach
base_df <- startlist %>%
  select(Skier, ID, Nation, Sex, all_of(race_prob_cols))
```

**Probability Column Recovery and Fallback**: The system implements robust recovery mechanisms when probability columns are missing from processed startlists:

```r
# From race-picks.R:2155-2167
# Ensure race probability column exists
if(!(race_prob_col %in% names(startlist_prepared))) {
  log_warn(paste("Race probability column missing:", race_prob_col))
  if(race_prob_col %in% names(startlist)) {
    # Copy from original startlist if available
    log_info("Copying from original startlist")
    startlist_prepared[[race_prob_col]] <- startlist[match(startlist_prepared[[participant_col]], startlist[[participant_col]]), race_prob_col]
  } else {
    # Default to 1 for first race, 0 for others if not available
    log_info("Setting default probabilities")
    startlist_prepared[[race_prob_col]] <- if(i == 1) 1 else 0
  }
}
```

**Position Prediction Data Structure Creation**: The system creates structured datasets for position probability predictions while preserving essential participant information:

```r
# From race-picks.R:2169-2186
# NEW CODE: Make position probability predictions with adjustments
position_preds <- data.frame(startlist_prepared[[participant_col]])
names(position_preds)[1] <- participant_col

# Add ID, Nation, Sex for individual races
if(!is_team) {
  position_preds$ID <- startlist_prepared$ID
  position_preds$Nation <- startlist_prepared$Nation
  position_preds$Sex <- startlist_prepared$Sex
}

# Add race number
position_preds$Race <- i

# Add race probability column for later normalization
if(race_prob_col %in% names(startlist_prepared)) {
  position_preds[[race_prob_col]] <- startlist_prepared[[race_prob_col]]
}
```

**Comprehensive Probability Column Validation**: The system performs detailed validation of probability data quality and distribution:

```r
# From race-picks.R:1514-1524
# Check race probability columns in result
for(col in race_prob_cols) {
  if(!is.null(result_df[[col]])) {
    log_info(paste("Race probability column", col, "summary:"))
    log_info(paste("  Mean:", mean(result_df[[col]], na.rm = TRUE)))
    log_info(paste("  Sum:", sum(result_df[[col]], na.rm = TRUE)))
    log_info(paste("  Max:", max(result_df[[col]], na.rm = TRUE)))
  } else {
    log_warn(paste("Race probability column", col, "is missing from result_df!"))
  }
}
```

**Race Probability Conservation Monitoring**: The system continuously monitors probability preservation throughout the data preparation process:

```r
# From race-picks.R:2341-2350
# Check if probabilities are getting lost
log_info(paste("Race", i, "probability check:"))
prob_summary <- startlist_prepared %>%
  group_by(if(!is_team) Nation else NULL) %>%
  summarise(
    mean_prob = mean(get(race_prob_col), na.rm = TRUE),
    sum_prob = sum(get(race_prob_col), na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(sum_prob))
```

**Final Output Probability Column Verification**: The system ensures that probability columns are properly included in the final output structures:

```r
# From race-picks.R:2405, 2407-2415
race_prob_col)

# Extra check to ensure race probability column exists and is properly named
if(!race_prob_col %in% names(race_dfs[[i]])) {
  log_warn(paste("Race probability column", race_prob_col, "not in final race_dfs!"))
  # Try to fix
  if("Race_Prob" %in% names(race_dfs[[i]])) {
    log_info("Renaming Race_Prob column to correct race probability column name")
    race_dfs[[i]][[race_prob_col]] <- race_dfs[[i]][["Race_Prob"]]
  }
}
```

**Multi-Race Probability Column Management**: For events with multiple races, the system manages distinct probability columns for each race:

```r
# From race-picks.R:1175-1190
# Get probability column name
race_prob_col <- paste0("Race", i, "_Prob")

# Check if Race{i}_Prob exists in this race_df
if(!race_prob_col %in% names(race_dfs[[i]])) {
  log_warn(paste("Race probability column", race_prob_col, "not found in race_dfs[[", i, "]]"))
  
  # Try to fix by getting from startlist
  if(race_prob_col %in% names(startlist)) {
    log_info("Copying probability from original startlist")
    race_dfs[[i]][[race_prob_col]] <- startlist[match(race_dfs[[i]][[participant_col]], startlist[[participant_col]]), race_prob_col]
  } else {
    log_warn("Cannot find probability in startlist either, using default value")
    race_dfs[[i]][[race_prob_col]] <- 0
  }
}
```

**Probability Data Integrity Features**:

- **Pattern-Based Detection**: Uses regex pattern `^Race\\d+_Prob$` to identify all race probability columns
- **Cross-Reference Validation**: Compares probability columns between original and processed startlists
- **Statistical Summary Generation**: Provides mean, sum, and maximum values for probability validation
- **Graceful Degradation**: Implements intelligent defaults (1.0 for first race, 0.0 for subsequent races) when data is missing
- **Multi-Format Support**: Handles both individual athlete and team probability structures

The Nordic Combined probability testing startlist setup provides comprehensive probability data management through dynamic detection, robust validation, intelligent recovery mechanisms, and continuous monitoring that ensures race participation probabilities are preserved and accurately integrated throughout the position probability prediction pipeline.

###### Modeling

Nordic Combined probability testing employs sophisticated multi-threshold position modeling that generates probability predictions across achievement levels through trained GAM models, comprehensive variable validation, adaptive adjustment integration, and mathematical normalization. The testing modeling system ensures robust prediction generation with fallback mechanisms and probability constraint enforcement.

**Threshold-Specific Model Retrieval and Validation**: The system retrieves trained models for each position threshold and validates model-data compatibility:

```r
# From race-picks.R:2188-2201
# Make predictions for each threshold
for(threshold in position_thresholds) {
  model_name <- paste0("threshold_", threshold)
  adj_name <- paste0("threshold_", threshold)
  prob_col <- paste0("prob_top", threshold)
  
  if(model_name %in% names(position_models)) {
    tryCatch({
      # Get the model
      pos_model <- position_models[[model_name]]
      
      # Check what variables the model actually needs
      model_vars <- names(pos_model$var.summary)
      log_info(paste("Model for threshold", threshold, "requires variables:", paste(model_vars, collapse=", ")))
```

**Comprehensive Variable Compatibility Verification**: The system performs detailed validation and type conversion to ensure prediction data compatibility with trained models:

```r
# From race-picks.R:2203-2240
# Create a clean subset of prediction data with only required variables
prediction_subset <- startlist_prepared

# Explicitly check for each variable
for(var in model_vars) {
  if(!(var %in% names(prediction_subset))) {
    log_warn(paste("Missing required variable:", var, "- adding with default values"))
    # Add missing variable with appropriate default value
    prediction_subset[[var]] <- 0
  } else {
    # Ensure the variable has the right type
    model_var_type <- class(pos_model$var.summary[[var]])
    data_var_type <- class(prediction_subset[[var]])
    
    if(!identical(model_var_type, data_var_type)) {
      log_warn(paste("Variable type mismatch for", var, ":", 
                     "model expects", model_var_type, "but got", data_var_type))
      # Convert to correct type
      if(model_var_type == "numeric") {
        prediction_subset[[var]] <- as.numeric(prediction_subset[[var]])
      } else if(model_var_type == "factor") {
        prediction_subset[[var]] <- as.factor(prediction_subset[[var]])
      }
    }
    
    # Handle NAs
    if(any(is.na(prediction_subset[[var]]))) {
      log_info(paste("Replacing NAs in", var))
      if(is.numeric(prediction_subset[[var]])) {
        prediction_subset[[var]] <- replace_na_with_quartile(prediction_subset[[var]])
      } else {
        # For non-numeric, use most common value
        most_common <- names(sort(table(prediction_subset[[var]], useNA = "no"), decreasing = TRUE))[1]
        prediction_subset[[var]][is.na(prediction_subset[[var]])] <- most_common
      }
    }
  }
}
```

**Adaptive Prediction Generation with Hierarchical Fallbacks**: The system implements robust prediction strategies with comprehensive error handling and row-by-row fallback mechanisms:

```r
# From race-picks.R:2242-2266
# Make predictions with explicit try-catch
base_predictions <- tryCatch({
  # Debug output
  log_info(paste("Attempting prediction for threshold", threshold, "with", nrow(prediction_subset), "rows"))
  
  # Explicit call to mgcv::predict.gam to avoid method dispatch issues
  mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response")
}, error = function(e) {
  log_warn(paste("Prediction call failed:", e$message))
  
  # Try alternative prediction approach with one row at a time
  log_info("Trying row-by-row prediction as fallback")
  result <- numeric(nrow(prediction_subset))
  
  for(j in 1:nrow(prediction_subset)) {
    single_row <- prediction_subset[j,, drop = FALSE]
    result[j] <- tryCatch({
      mgcv::predict.gam(pos_model, newdata = single_row, type = "response")
    }, error = function(e2) {
      log_warn(paste("Failed on row", j, ":", e2$message))
      threshold/100  # Default value based on threshold
    })
  }
  return(result)
})
```

**Statistical Adjustment Integration**: The system applies training-derived adjustments to position probabilities with boundary enforcement:

```r
# From race-picks.R:2271-2298
# Apply adjustments if available
if(adj_name %in% names(position_adjustments)) {
  # Get adjustments
  pos_adj <- position_adjustments[[adj_name]]
  
  # Join with predictions
  position_preds <- position_preds %>%
    left_join(pos_adj, by = participant_col) %>%
    mutate(
      # Replace NAs with zeros
      period_effect = replace_na(period_effect, 0),
      
      # Apply adjustments
      period_adjustment = period_effect,
      
      # Calculate adjusted probabilities
      adjusted_prob = get(paste0(prob_col, "_base")) + period_adjustment,
      
      # Ensure probabilities are between 0 and 1
      adjusted_prob = pmin(pmax(adjusted_prob, 0), 1)
    )
  
  # Use adjusted probability as final
  position_preds[[prob_col]] <- position_preds$adjusted_prob
  
  # Clean up temporary columns
  position_preds <- position_preds %>%
    select(-period_effect, -period_adjustment, -adjusted_prob)
} else {
  # Use base prediction if no adjustments
  position_preds[[prob_col]] <- position_preds[[paste0(prob_col, "_base")]]
}
```

**Probability Scaling and Percentage Conversion**: The system converts probabilities to percentage format with appropriate rounding:

```r
# From race-picks.R:2304-2311
# Clean up base prediction column
position_preds <- position_preds %>%
  select(-paste0(prob_col, "_base"))

# Convert to percentage and round
position_preds[[prob_col]] <- round(position_preds[[prob_col]] * 100, 1)

log_info(paste("Made predictions with adjustments for position threshold", threshold))
```

**Comprehensive Probability Normalization**: The system applies mathematical normalization to ensure probability consistency across thresholds:

```r
# From race-picks.R:641-684
normalize_position_probabilities <- function(predictions, race_prob_col, position_thresholds) {
  # Make a copy to avoid modifying the original data frame
  normalized <- predictions
  
  # For each threshold, adjust and normalize probabilities
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    
    # First, adjust by race participation probability
    if(race_prob_col %in% names(normalized)) {
      # Apply race probability adjustment
      normalized[[prob_col]] <- normalized[[prob_col]] * normalized[[race_prob_col]]
    }
    
    # Calculate the current sum
    current_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
    
    # Target sum should be 100 * threshold (e.g., 100% for top 1, 300% for top 3)
    target_sum <- 100 * threshold
    
    # Normalize only if current sum is not zero to avoid division by zero
    if(current_sum > 0) {
      # Apply scaling factor to adjust the probabilities
      scaling_factor <- target_sum / current_sum
      normalized[[prob_col]] <- normalized[[prob_col]] * scaling_factor
    }
  }
}
```

**Probability Sum Verification and Logging**: The system provides comprehensive validation of normalized probability totals:

```r
# From race-picks.R:2324-2336
# Normalize position probabilities to ensure they sum to the correct totals
position_preds <- normalize_position_probabilities(position_preds, race_prob_col, position_thresholds)

# Add verification logging for each threshold
log_info(sprintf("Race %d position probability sums after normalization:", i))
for(threshold in position_thresholds) {
  prob_col <- paste0("prob_top", threshold)
  if(prob_col %in% names(position_preds)) {
    sum_val <- sum(position_preds[[prob_col]], na.rm = TRUE)
    log_info(sprintf("  %s: %.2f%% (should be %d%%)", 
                     prob_col, sum_val, 100 * threshold))
  }
}
```

**Ultimate Fallback Prediction Strategy**: For complete modeling failures, the system provides threshold-based default predictions:

```r
# From race-picks.R:2313-2321
}, error = function(e) {
  log_warn(paste("Complete failure for threshold", threshold, ":", e$message))
  # Set a reasonable default based on threshold (1% for top1, 3% for top3, etc.)
  position_preds[[prob_col]] <- rep(threshold, nrow(position_preds))
}) 
} else {
  log_warn(paste("No model found for threshold", threshold))
  position_preds[[prob_col]] <- NA
}
```

**Multi-Threshold Achievement Probability Architecture**: The system generates predictions across five critical position achievement levels:

- **Top 1 (prob_top1)**: Win probability for race victory
- **Top 3 (prob_top3)**: Podium finish probability 
- **Top 5 (prob_top5)**: Strong performance probability
- **Top 10 (prob_top10)**: Solid competitive result probability
- **Top 30 (prob_top30)**: Points-scoring achievement probability

**Race Participation Probability Integration**: All position probabilities are scaled by race participation probabilities to reflect realistic achievement expectations based on startlist inclusion likelihood.

**Mathematical Constraint Enforcement**: The system maintains probability boundaries [0,1] before percentage conversion and ensures normalized sums match mathematical expectations (100% for top 1, 300% for top 3, etc.).

The Nordic Combined probability testing modeling provides comprehensive position achievement prediction through sophisticated GAM-based threshold modeling, robust variable validation, intelligent adjustment integration, mathematical normalization, and adaptive fallback strategies that ensure reliable probability generation across all Nordic Combined competition scenarios and data quality conditions.

###### Adjustments

Nordic Combined probability testing implements sophisticated multi-layered adjustment mechanisms that integrate training-derived statistical corrections, race participation probability scaling, mathematical normalization, and monotonic constraint enforcement. The adjustment system ensures probability consistency while maintaining logical ordering relationships across position achievement thresholds.

**Training-Derived Statistical Adjustment Integration**: The system applies period-specific adjustments calculated during the training phase to correct for systematic biases:

```r
# From race-picks.R:2271-2291
# Apply adjustments if available
if(adj_name %in% names(position_adjustments)) {
  # Get adjustments
  pos_adj <- position_adjustments[[adj_name]]
  
  # Join with predictions
  position_preds <- position_preds %>%
    left_join(pos_adj, by = participant_col) %>%
    mutate(
      # Replace NAs with zeros
      period_effect = replace_na(period_effect, 0),
      
      # Apply adjustments
      period_adjustment = period_effect,
      
      # Calculate adjusted probabilities
      adjusted_prob = get(paste0(prob_col, "_base")) + period_adjustment,
      
      # Ensure probabilities are between 0 and 1
      adjusted_prob = pmin(pmax(adjusted_prob, 0), 1)
    )
}
```

**Probability Boundary Enforcement**: The system maintains strict probability boundaries to ensure mathematical validity before further processing:

```r
# From race-picks.R:2289-2302
# Ensure probabilities are between 0 and 1
adjusted_prob = pmin(pmax(adjusted_prob, 0), 1)

# Use adjusted probability as final
position_preds[[prob_col]] <- position_preds$adjusted_prob

# Clean up temporary columns
position_preds <- position_preds %>%
  select(-period_effect, -period_adjustment, -adjusted_prob)
} else {
  # Use base prediction if no adjustments
  position_preds[[prob_col]] <- position_preds[[paste0(prob_col, "_base")]]
}
```

**Race Participation Probability Scaling**: The normalization process applies race participation probabilities as multiplicative factors to reflect realistic achievement expectations:

```r
# From race-picks.R:660-672
# First, adjust by race participation probability
if(race_prob_col %in% names(normalized)) {
  # Log sum before race probability adjustment
  sum_before_race_adj <- sum(normalized[[prob_col]], na.rm = TRUE)
  
  # Apply race probability adjustment
  normalized[[prob_col]] <- normalized[[prob_col]] * normalized[[race_prob_col]]
  
  # Log sum after race probability adjustment
  sum_after_race_adj <- sum(normalized[[prob_col]], na.rm = TRUE)
  log_info(sprintf("  %s after race prob adjustment: %.2f%% (scaling by race participation)", 
                   prob_col, sum_after_race_adj))
}
```

**Mathematical Normalization with Target Sum Enforcement**: The system normalizes probabilities to ensure they sum to mathematically correct totals for each threshold:

```r
# From race-picks.R:674-684
# Calculate the current sum
current_sum <- sum(normalized[[prob_col]], na.rm = TRUE)

# Target sum should be 100 * threshold (e.g., 100% for top 1, 300% for top 3)
target_sum <- 100 * threshold

# Normalize only if current sum is not zero to avoid division by zero
if(current_sum > 0) {
  # Apply scaling factor to adjust the probabilities
  scaling_factor <- target_sum / current_sum
  normalized[[prob_col]] <- normalized[[prob_col]] * scaling_factor
}
```

**Probability Capping and Redistribution**: The system implements sophisticated probability capping with excess redistribution to maintain realistic individual probability limits:

```r
# From race-picks.R:686-716
# Cap individual probabilities at 100%
over_hundred <- which(normalized[[prob_col]] > 100)
if(length(over_hundred) > 0) {
  log_info(sprintf("  Capping %d participants with >100%% probability for %s", 
                   length(over_hundred), prob_col))
  
  # Calculate excess probability that needs to be redistributed
  excess <- sum(normalized[[prob_col]][over_hundred] - 100)
  
  # Cap values at 100%
  normalized[[prob_col]][over_hundred] <- 100
  
  # Redistribute the excess to other participants proportionally
  under_hundred <- which(normalized[[prob_col]] < 100)
  if(length(under_hundred) > 0 && excess > 0) {
    # Get current sum of under-100 probabilities
    under_sum <- sum(normalized[[prob_col]][under_hundred])
    
    # Calculate scaling factor for redistribution
    if(under_sum > 0) {
      redistrib_factor <- (under_sum + excess) / under_sum
      normalized[[prob_col]][under_hundred] <- normalized[[prob_col]][under_hundred] * redistrib_factor
    }
  }
}
```

**Zero-Sum Fallback Mechanism**: For edge cases where probability sums are zero, the system provides even distribution fallbacks:

```r
# From race-picks.R:721-726
} else {
  # If sum is zero, distribute evenly among all participants
  # This is a fallback that should rarely be needed
  log_warn(paste("Zero sum for", prob_col, "- distributing evenly"))
  normalized[[prob_col]] <- target_sum / nrow(normalized)
}
```

**Final Sum Validation and Tolerance Checking**: The system validates final probability sums against target values with reasonable tolerance for rounding differences:

```r
# From race-picks.R:728-733
# Final check to ensure we're close to the target sum
final_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
if(abs(final_sum - target_sum) > 1) {  # Allow for small rounding differences
  log_warn(sprintf("  %s sum after capping: %.2f%% (target: %.2f%%)", 
                   prob_col, final_sum, target_sum))
}
```

**Monotonic Constraint Enforcement**: The system ensures logical ordering relationships between different position achievement thresholds:

```r
# From race-picks.R:736-739
# APPLY MONOTONIC CONSTRAINTS: Ensure Win <= Podium <= Top5 <= Top10 <= Top30
log_info("Applying monotonic constraints...")

# Get available probability columns in ascending order
```

**Comprehensive Adjustment Logging and Validation**: The system provides detailed logging throughout the adjustment process to ensure transparency and debugging capability:

```r
# From race-picks.R:2311, 719-720
log_info(paste("Made predictions with adjustments for position threshold", threshold))

log_info(sprintf("  %s normalization: applied scaling factor of %.4f", 
                 prob_col, scaling_factor))
```

**Adjustment Processing Features**:

- **Statistical Bias Correction**: Applies training-derived period adjustments to address systematic temporal biases
- **Boundary Enforcement**: Maintains probability values within valid [0,1] range throughout processing
- **Race Context Integration**: Scales probabilities by race participation likelihood for realistic expectations
- **Mathematical Consistency**: Ensures probability sums match theoretical expectations (100% for top 1, 300% for top 3, etc.)
- **Redistribution Logic**: Implements sophisticated excess probability redistribution when individual caps are exceeded
- **Tolerance Management**: Allows reasonable rounding tolerances while flagging significant deviations
- **Monotonic Ordering**: Enforces logical relationships where higher position thresholds have equal or greater probabilities

**Multi-Level Adjustment Architecture**: The adjustment system operates at multiple levels:

1. **Individual Prediction Adjustments**: Period-specific corrections applied to base model predictions
2. **Race Participation Scaling**: Multiplication by participation probabilities for realistic context
3. **Mathematical Normalization**: Scaling to achieve target probability sums across participants
4. **Boundary Enforcement**: Individual probability capping with intelligent redistribution
5. **Constraint Validation**: Monotonic ordering enforcement across position thresholds

The Nordic Combined probability testing adjustments provide comprehensive probability refinement through multi-layered statistical corrections, mathematical normalization, intelligent redistribution mechanisms, and logical constraint enforcement that ensures both mathematical validity and practical interpretability of position achievement probabilities across all Nordic Combined competition scenarios.

### Relay

#### Data Gathering

Nordic Combined relay events (team, team sprint, and mixed team) are processed through specialized scraping mechanisms that handle team-based competition structures and multi-athlete team compositions. The system processes three distinct relay formats with automated delegation to format-specific Python modules.

**Race Type Classification and Delegation**: The main scraping process identifies relay events and delegates processing to specialized modules based on race format:

```python
# From startlist-scrape-races.py:98-112
# Filter races by type: individual, team, team sprint, and mixed team
# Mixed team events have RaceType="Team" and Sex="Mixed"
individual_races = races_df[~races_df['RaceType'].str.contains("Team", na=False)]
team_races = races_df[(races_df['RaceType'].str.contains("Team", na=False)) & 
                      (~races_df['RaceType'].str.contains("Sprint", na=False)) &
                      (races_df['Sex'] != 'Mixed')]
team_sprint_races = races_df[races_df['RaceType'].str.contains("Team Sprint", na=False)]
mixed_team_races = races_df[(races_df['RaceType'].str.contains("Team", na=False)) & 
                             (~races_df['RaceType'].str.contains("Sprint", na=False)) &
                             (races_df['Sex'] == 'Mixed')]

print(f"Found {len(individual_races)} individual races")
print(f"Found {len(team_races)} team races")
print(f"Found {len(team_sprint_races)} team sprint races")
print(f"Found {len(mixed_team_races)} mixed team races")
```

**Team Race Processing**: Regular team events are processed through dedicated team scripts with subprocess delegation:

```python
# From startlist-scrape-races.py:118-138
# Process team races if available
if not team_races.empty:
    # Save team races to temporary file for team script to process
    temp_file = "/tmp/races_team.csv"
    team_races.to_csv(temp_file, index=False)
    
    try:
        # Set environment variable to skip running race picks in team script
        env = os.environ.copy()
        env["SKIP_RACE_PICKS"] = "1"
        
        # Call the team race script
        script_path = os.path.expanduser("~/ski/elo/python/nordic-combined/polars/relay/startlist_scrape_races_team.py")
        subprocess.run(
            [sys.executable, script_path, temp_file],
            check=True,
            env=env
        )
        print("Successfully processed team races")
    except subprocess.CalledProcessError as e:
        print(f"Error calling team race script: {e}")
```

**Team Sprint Processing**: Team sprint events receive specialized handling through dedicated sprint modules:

```python
# From startlist-scrape-races.py:140-160
# Process team sprint races if available
if not team_sprint_races.empty:
    # Save team sprint races to temporary file for team sprint script to process
    temp_file = "/tmp/races_team_sprint.csv"
    team_sprint_races.to_csv(temp_file, index=False)
    
    try:
        # Set environment variable to skip running race picks in team sprint script
        env = os.environ.copy()
        env["SKIP_RACE_PICKS"] = "1"
        
        # Call the team sprint race script
        script_path = os.path.expanduser("~/ski/elo/python/nordic-combined/polars/relay/startlist_scrape_races_team_sprint.py")
        subprocess.run(
            [sys.executable, script_path, temp_file],
            check=True,
            env=env
        )
        print("Successfully processed team sprint races")
    except subprocess.CalledProcessError as e:
        print(f"Error calling team sprint race script: {e}")
```

**Mixed Team Event Handling**: Mixed team events require specialized gender-aware processing:

```python
# From startlist-scrape-races.py:162-182
# Process mixed team races if available
if not mixed_team_races.empty:
    # Save mixed team races to temporary file for mixed team script to process
    temp_file = "/tmp/races_mixed_team.csv"
    mixed_team_races.to_csv(temp_file, index=False)
    
    try:
        # Set environment variable to skip running race picks in mixed team script
        env = os.environ.copy()
        env["SKIP_RACE_PICKS"] = "1"
        
        # Call the mixed team race script
        script_path = os.path.expanduser("~/ski/elo/python/nordic-combined/polars/relay/startlist_scrape_races_mixed_team.py")
        subprocess.run(
            [sys.executable, script_path, temp_file],
            check=True,
            env=env
        )
        print("Successfully processed mixed team races")
    except subprocess.CalledProcessError as e:
        print(f"Error calling mixed team race script: {e}")
```

**Team Event Type Detection**: The system uses intelligent HTML parsing to identify specific team event characteristics:

```python
# From relay/startlist_common.py:84-128
def determine_event_type(soup: BeautifulSoup) -> Tuple[bool, bool, bool]:
    """
    Determine the type of Nordic Combined event from the HTML content.
    
    Returns:
        Tuple of (is_team_event, is_team_sprint, is_mixed_team)
    """
    is_team_event = False
    is_team_sprint = False
    is_mixed_team = False
    
    # Check event title
    event_title_elem = soup.select_one('.event-header__name')
    if event_title_elem:
        event_title = event_title_elem.text.strip().upper()
        
        if 'MIXED' in event_title and 'TEAM' in event_title:
            is_mixed_team = True
            is_team_event = True
            if 'SPRINT' in event_title:
                is_team_sprint = True
        elif 'TEAM' in event_title:
            is_team_event = True
            if 'SPRINT' in event_title:
                is_team_sprint = True
    
    return is_team_event, is_team_sprint, is_mixed_team
```

**Team Data Extraction**: Team results are extracted with detailed member information and jumping/cross-country performance data:

```python
# From relay/startlist_common.py:366-437
def extract_team_results(soup: BeautifulSoup, is_mixed_team: bool = False) -> List[Dict]:
    """Extract team results from Nordic Combined FIS race page"""
    teams = []
    
    try:
        # Find all team rows (main rows, not the team members)
        team_rows = soup.select('.table-row_theme_main')
        
        for team_row in team_rows:
            # Get rank
            rank_elem = team_row.select_one('.g-lg-1.g-md-1.g-sm-1.g-xs-2.justify-right.bold.pr-1')
            rank = rank_elem.text.strip()
            
            # Get team name (country name)
            name_elem = team_row.select_one('.g-lg-8.g-md-8.g-sm-5.g-xs-9.justify-left.bold')
            team_name = name_elem.text.strip() if name_elem else ""
            
            # Get nation code
            nation_elem = team_row.select_one('.country__name-short')
            nation = nation_elem.text.strip() if nation_elem else ""
            
            # Create team data dictionary
            team_data = {
                'Rank': rank,
                'TeamName': team_name,
                'Nation': nation,
                'Points': points,
                'Time': time,
                'TimeDiff': time_diff,
                'Members': [],
                'IsMixedTeam': is_mixed_team
            }
```

**Mixed Team Gender Assignment**: Mixed team events require sophisticated gender determination for team members:

```python
# From relay/startlist_common.py:341-365
def determine_member_gender_mixed_team(member: Dict, position: int, bib: str = "") -> str:
    """
    Determine gender for a mixed team member based on position and bib information.
    
    Returns:
        'M' for male, 'F' for female
    """
    # Check if bib has a pattern like "1-1", "1-2", etc.
    bib_match = re.search(r'(\d+)-(\d+)', bib) if isinstance(bib, str) else None
    
    if bib_match:
        # For bib format like "1-1", "1-2", etc.
        # In mixed team, typically legs 1 and 3 are male, 2 and 4 are female
        leg_num = int(bib_match.group(2))  # Get the second number (leg)
        return 'M' if leg_num in [1, 3] else 'F'
    else:
        # Fallback to position-based assumption if bib format not recognized
        # Positions 1 and 3 are typically male
        return 'M' if position in [1, 3] else 'F'
```

**Data Processing Integration**: The relay data gathering system integrates with the main R processing pipeline through specialized file outputs:

```python
# From race-picks.R:175-225
# For team events
if(nrow(men_teams) > 0) {
  men_team_file <- "~/ski/elo/python/nordic-combined/polars/relay/excel365/startlist_team_races_men.csv"
  if(file.exists(men_team_file)) {
    men_team_startlist <- read.csv(men_team_file, stringsAsFactors = FALSE)
    log_info(paste("Loaded men's team startlist with", nrow(men_team_startlist), "teams"))
  } else {
    log_warn("Men's team startlist file not found")
  }
}

# Mixed team events
if(nrow(mixed_teams) > 0) {
  mixed_team_file <- "~/ski/elo/python/nordic-combined/polars/relay/excel365/startlist_mixed_team_races_teams.csv"
  if(file.exists(mixed_team_file)) {
    mixed_team_startlist <- read.csv(mixed_team_file, stringsAsFactors = FALSE)
    log_info(paste("Loaded mixed team startlist with", nrow(mixed_team_startlist), "teams"))
  } else {
    log_warn("Mixed team startlist file not found")
  }
}
```

The Nordic Combined relay data gathering system provides comprehensive team event processing through format-specific scraping modules, intelligent event type detection, detailed team composition extraction, gender-aware mixed team handling, and seamless integration with the main prediction pipeline through standardized CSV outputs for all relay event formats.

#### Points

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

#### Probability

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

#### Normalization and Monotonic Constraints