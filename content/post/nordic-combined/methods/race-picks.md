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

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

###### Adjustments

#### Probability

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

###### Adjustments

### Relay

#### Data Gathering

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