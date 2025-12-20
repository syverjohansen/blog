# Biathlon Weekly Prediction Methodology

## Overview

The biathlon weekly prediction system delivers comprehensive weekend forecasts across multiple race formats, uniquely handling the sport's complex multi-day competition structure. This methodology extends beyond single-race predictions to model entire weekend scenarios, incorporating both individual and team events while maintaining the sophisticated PELO (Penalty-adjusted ELO) shooting accuracy integration that distinguishes biathlon from other winter sports.

## Data Flow Architecture

### 1. Weekend Race Processing and Multi-Format Coordination

**Python Weekend Orchestration Script (`startlist-scrape-weekend.py`)**
- **Weekend-Centric Processing**: Identifies and processes all races scheduled for a specific weekend date
- **Multi-Race Coordination**: Handles up to 2 individual races plus multiple relay formats simultaneously
- **Race Format Classification**: Automatically separates individual, relay, mixed relay, and single mixed relay events
- **Priority-Based Selection**: Prioritizes earliest race dates when multiple options exist within a weekend

**Weekend Race Selection Logic**
```python
# Weekend race identification and prioritization
next_weekend_races = weekends_df.filter(Date == next_weekend_date)
sorted_races = valid_races.sort_values('Race_Date')  # Earliest first
races_to_process = sorted_races.head(2)  # Maximum 2 individual races per weekend
```

**Specialized Relay Weekend Processing**
- **Standard Relay Weekend**: Gender-specific 4x7.5km relay processing with team ELO aggregation
- **Mixed Relay Weekend**: Combined gender team analysis for 2+2 format competitions
- **Single Mixed Relay Weekend**: Alternating format processing with individual-team hybrid metrics
- **Skip Control Environment**: Prevents duplicate R script execution through environment variable `SKIP_WEEKLY_PICKS`

### 2. Enhanced Multi-Race Startlist Assembly

**Comprehensive Athlete Pool Creation**
```python
# Season-wide athlete inclusion for complete coverage
all_season_skiers_df = create_season_startlist(
    elo_path=elo_path,
    race_info=races_df.iloc[0],
    gender=gender,
    host_nation=host_nation,
    prob_column="temp_prob"
)

# Merge with race-specific startlists
consolidated_df = merge_race_dataframes(consolidated_df, race_df, prob_column)
```

**Advanced Startlist Integration**
- **URL-Based Startlist Processing**: IBU official startlist scraping with fuzzy name matching
- **Season Fallback Mechanism**: Complete athlete pool from chronological data when startlists unavailable
- **Probability Column Management**: Race-specific participation probabilities (`Race1_Prob`, `Race2_Prob`)
- **Host Nation Integration**: Home advantage modeling through nation-specific flags

### 3. Weekend-Specific Probability Framework

**Multi-Race Probability Architecture**
- **Race-Specific Columns**: Dynamic probability columns for each weekend race
- **Participation Modeling**: 100% probability for confirmed startlist entries, 0% for season athletes not entered
- **Cross-Race Athlete Tracking**: Maintains athlete records across multiple weekend events
- **Consolidated Startlist Generation**: Single comprehensive file containing all weekend race probabilities

**Weekend Race Probability Assignment**
```python
# Race probability assignment based on startlist presence
if url and not pd.isna(url):
    row_data[prob_column] = 1.0  # In startlist = 100% for this race
else:
    row_data[prob_column] = 0.0  # Not in startlist = 0% probability
```

## Statistical Modeling Framework

### 1. Weekend-Adapted Feature Engineering

**R Statistical Processing (`weekly-picks2.R`)**
- **Current Date Filtering**: Processes only races scheduled for the current UTC date
- **Weekend Race Structure**: Handles multiple individual races plus relay events within unified framework
- **Enhanced Logging**: Comprehensive process logging for weekend-specific operations
- **Race Classification**: Separate processing tracks for men's, ladies', and mixed events

**Weekend Race Variables**
```r
# Weekend race configuration
men_races <- next_weekend_races %>%
  filter(Sex == "M", !RaceType %in% c("Relay", "Mixed Relay", "Single Mixed Relay"))

ladies_races <- next_weekend_races %>%
  filter(Sex == "L", !RaceType %in% c("Relay", "Mixed Relay", "Single Mixed Relay"))

mixed_races <- next_weekend_races %>%
  filter(Sex == "Mixed")
```

### 2. Multi-Event Participation Integration

**Weekend Participation Modeling**
- **Cross-Race Participation**: Athletes may compete in multiple events within the same weekend
- **Format-Specific Rates**: Different participation patterns for Sprint, Individual, Pursuit, Mass Start
- **Relay Team Selection**: Nation-based team participation separate from individual event entry
- **Historical Weekend Analysis**: 5-year rolling windows for weekend-specific participation rates

**Enhanced Feature Set for Weekend Predictions**
```r
# Weekend-specific explanatory variables
explanatory_vars <- c("Prev_Points_Weighted", 
                      "Sprint_Pelo_Pct", "Individual_Pelo_Pct", 
                      "MassStart_Pelo_Pct", "Pursuit_Pelo_Pct", 
                      "Pelo_Pct", "Period", "Elevation_Flag")
```

### 3. Weekend-Optimized PELO Integration

**Multi-Race PELO Framework**
- **Weekend Form Assessment**: PELO ratings aggregated across weekend format variations
- **Shooting Condition Adaptation**: Venue-specific range condition adjustments for weekend events
- **Cross-Format PELO Application**: Shooting accuracy metrics adapted for different weekend race types
- **Weekend-Specific Normalization**: PELO percentiles calculated within weekend competition context

## Advanced Weekend-Specific Adjustments

### 1. Multi-Day Competition Modeling

**Weekend Race Interaction Effects**
- **Fatigue Modeling**: Performance degradation across multiple weekend events
- **Strategic Pacing**: Athletes' race selection and intensity distribution over weekend
- **Recovery Windows**: Time gaps between weekend events affecting performance
- **Equipment Optimization**: Venue-specific equipment choices across weekend formats

**Weekend Context Variables**
```r
# Weekend-specific race context
next_weekend_date <- min(next_races$Date, na.rm = TRUE)
weekend_race_count <- sum(next_weekend_races$Sex %in% c("M", "L"))
host_nation <- sorted_races$Country[1]
```

### 2. Enhanced Relay Weekend Processing

**Weekend Relay Coordination**
- **Team Selection Dynamics**: Nation-specific team composition strategies for weekend events
- **Individual-Team Performance Correlation**: Weekend individual results affecting relay selection probability
- **Mixed Format Integration**: Coordination between standard and mixed relay weekend events
- **Team ELO Weekend Adjustment**: Weekend-specific team chemistry and preparation factors

**Weekend Team Processing Architecture**
```python
# Weekend relay processing with skip control
def process_relay_and_team_races(relay_races: pd.DataFrame) -> None:
    for race_type in race_types:
        env = os.environ.copy()
        env["SKIP_WEEKLY_PICKS"] = "1"  # Prevent duplicate R script execution
        
        subprocess.run([script_path, temp_file], env=env)
```

### 3. Weekend Output Generation and Integration

**Comprehensive Weekend Results**
- **Multi-Event Spreadsheets**: Individual race predictions plus relay team forecasts
- **Weekend Summary Analysis**: Aggregated performance expectations across all weekend events
- **Cross-Event Probability Matrices**: Unified position probabilities accounting for multi-event participation
- **Weekend Performance Tracking**: Historical weekend success rates for model validation

**Weekend-Specific File Architecture**
```r
# Weekend output structure
output_path <- "~/ski/elo/python/biathlon/polars/excel365/startlist_weekend_{gender}.csv"
log_file <- "~/ski/elo/python/biathlon/polars/excel365/weekly-predictions/weekly_picks_processing.log"
```

## Key Methodological Innovations for Weekend Predictions

### 1. Multi-Race Probability Integration
Handles complex weekend scenarios where athletes compete in multiple events with varying participation probabilities

### 2. Weekend-Optimized PELO Application
Adapts shooting accuracy metrics for the unique challenges of multi-day competition formats

### 3. Enhanced Team-Individual Coordination
Seamlessly integrates individual weekend performance with relay team selection and performance modeling

### 4. Dynamic Race Priority Management
Intelligently prioritizes race processing when multiple events occur within the same weekend timeframe

### 5. Comprehensive Season Athlete Pool
Ensures complete coverage by including all active season athletes with appropriate zero probabilities for non-participants

### 6. Weekend-Aware Relay Processing
Coordinates multiple relay formats (standard, mixed, single mixed) within unified weekend prediction framework

### 7. Advanced Weekend Context Integration
Incorporates host nation effects, elevation adjustments, and period effects specific to weekend competition dynamics

### 8. Cross-Format Performance Modeling
Bridges individual and team event predictions within cohesive weekend forecasting methodology

This weekend prediction methodology represents the most sophisticated approach to multi-day biathlon competition forecasting, uniquely addressing the sport's complex weekend structure while maintaining the shooting accuracy integration and statistical rigor that characterizes elite biathlon performance analysis.