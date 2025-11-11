# Biathlon Championships Predictions - Implementation Scope

Based on the Nordic Combined champs-predictions.R file, here's what the biathlon version should include:

## Core Structure (1519 lines in Nordic Combined)

### 1. Library Imports & Setup (Lines 1-32)
- Standard R libraries (dplyr, tidyr, openxlsx, arrow, mgcv, leaps, logger, purrr, lubridate, slider)
- Biathlon-specific points systems (regular_points, mass_start_points)
- Logging configuration to `~/ski/elo/python/biathlon/polars/excel365/champs-predictions/`

### 2. Points Systems & Helper Functions (Lines 13-117)
- **Biathlon Points**: Regular (40 positions) and Mass Start (30 positions) - ✅ DONE
- `replace_na_with_quartile()` function for missing value imputation
- `get_points()` function with race type logic for Mass Start vs other races - ✅ DONE
- `enforce_probability_constraints()` - ensures Win ≤ Podium ≤ Top5 ≤ Top10
- `normalize_position_probabilities()` - complex normalization with race participation scaling

### 3. Data Reading & Race Filtering (Lines 34-99)
- Read weekends.csv and filter for Championships (Championship == 1)
- Separate races by type:
  - **Individual races**: Men/Ladies for Individual, Sprint, Pursuit, Mass Start
  - **Relay races**: Men/Ladies for Relay, Mixed Relay, Single Mixed Relay
- Race dataframes with original race numbering for Excel sheet naming

### 4. Data Preprocessing Function (Lines 411-528)
- `preprocess_data()` - handles historical race data
- Points calculation using appropriate points system
- Weighted previous points calculation (last 5 races, recent weighted higher)
- ELO/PELO column creation and percentage normalization
- Race type filtering (exclude Offseason)
- Season period assignment (4 periods per season)

### 5. Startlist Data Preparation (Lines 229-409)
- `prepare_startlist_data()` - prepares prediction data
- Handles both individual and team startlists
- Team Prev_Points_Weighted calculation from individual member data
- ELO percentage column creation (Elo_Pct, Pelo_Pct)
- Race probability column preservation
- Team vs individual data path logic

### 6. Individual Championships Processing (Lines 530-1004)
- `process_gender_championships()` - main individual prediction function
- GAM model creation using regsubsets for feature selection
- Position probability models for thresholds (1, 3, 5, 10, 30)
- Training on PELO (pre-race), prediction on ELO (post-race)
- Period adjustments using t-tests
- Brier score evaluation for model quality
- Excel output generation with race-by-race sheets

### 7. Team Championships Processing (Lines 1030-1348)
- `process_team_championships()` - handles relay predictions
- Team-specific ELO columns (Avg_Individual_Elo, etc.)
- Team position models (fewer thresholds: 1, 3, 5, 10)
- Team startlist handling for different relay types
- Team summary and race-by-race Excel outputs

### 8. Race Probability Calculation (Lines 1350-1486)
- `calculate_championships_race_probabilities()` - calculates participation probabilities
- Historical participation analysis (5-year lookback)
- **Biathlon-specific**: 6-person quota constraint per nation per race (vs 3 for Nordic Combined)
- Base probability scaling and normalization
- Updates startlist files with race probabilities

### 9. Team Member Points Calculation (Lines 1006-1028)
- `calculate_team_prev_points()` - calculates team Prev_Points_Weighted
- Individual member analysis from chrono data
- Weighted average of last 5 individual races
- **Biathlon-specific**: Include Individual, Sprint, Pursuit, Mass Start (exclude Offseason)

## Biathlon-Specific Adaptations Needed

### 1. Points Systems
- ✅ Already adapted: regular_points (40 positions), mass_start_points (30 positions)
- ✅ get_points() function with Mass Start logic

### 2. File Paths
- Change all paths from `nordic-combined` to `biathlon`
- Output directory: `~/blog/daehl-e/content/post/biathlon/drafts/champs-predictions/`

### 3. Race Types
- **Individual**: Individual, Sprint, Pursuit, Mass Start (vs Individual, IndividualCompact, Mass Start)
- **Relay**: Use "Relay" in RaceType (vs "Team")
- **Mixed**: Mixed Relay (2M+2L), Single Mixed Relay (1M+1L)

### 4. ELO Column Names
- ✅ Use MassStart_Elo not Mass_Start_Elo
- Individual_Elo, Sprint_Elo, Pursuit_Elo, MassStart_Elo

### 5. Quota System
- **6-person quota** per nation (vs 3 for Nordic Combined)
- Update quota constraints in race probability calculation

### 6. Relay Processing
- **4 separate relay types**: men_relay, ladies_relay, mixed_relay, single_mixed_relay
- Team composition logic for mixed relays (2M+2L vs 1M+1L)
- Different chrono file paths for each relay type

### 7. Output Files
- **6 Excel workbooks**: men, ladies, men_relay, ladies_relay, mixed_relay, single_mixed_relay
- Race-by-race sheets with proper naming

## Implementation Priority

1. **High Priority** (Core functionality):
   - Data preprocessing function with biathlon race types
   - Individual championships processing (men/ladies)
   - Race probability calculation with 6-person quota
   - Basic Excel output generation

2. **Medium Priority** (Relay functionality):
   - Relay championships processing for all 4 types
   - Team member points calculation
   - Mixed relay logic (2M+2L, 1M+1L)

3. **Low Priority** (Advanced features):
   - Period adjustments and t-test analysis
   - Advanced GAM model diagnostics
   - Brier score calculations

## Key Functions to Implement

1. `preprocess_data()` - adapt for biathlon race types
2. `prepare_startlist_data()` - handle biathlon ELO columns
3. `process_gender_championships()` - individual predictions
4. `process_relay_championships()` - relay predictions (new function)
5. `calculate_championships_race_probabilities()` - 6-person quota
6. `calculate_team_prev_points()` - biathlon race type filtering

The file should be approximately **1500+ lines** to match the complexity and functionality of the Nordic Combined version.