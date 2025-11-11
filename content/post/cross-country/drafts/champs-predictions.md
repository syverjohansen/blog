# Cross-Country Championships Predictions Implementation Plan

## Overview

This document outlines the implementation plan for cross-country (XC) championships prediction system, building on the successful biathlon and nordic-combined implementations. Cross-country presents unique challenges with its 9 different ELO/PELO rating systems based on technique and distance combinations.

**Status: Phase 2 Completed - Unified Startlist Generation Working**  
**Target Implementation: Complete unified system**  
**Last Updated: 2025-01-06**

## Key Cross-Country Differences from Biathlon

### 1. ELO/PELO Rating Systems (9 total)
Unlike biathlon's 5 systems, cross-country uses 9 different rating systems:

**Distance Races (Classic Technique):**
- `Distance_Classic_Elo/Pelo` - Long distance classic races
- `Mid_Classic_Elo/Pelo` - Mid distance classic races  
- `Short_Classic_Elo/Pelo` - Short distance classic races

**Distance Races (Freestyle Technique):**
- `Distance_Freestyle_Elo/Pelo` - Long distance freestyle races
- `Mid_Freestyle_Elo/Pelo` - Mid distance freestyle races
- `Short_Freestyle_Elo/Pelo` - Short distance freestyle races

**Sprint Races:**
- `Sprint_Classic_Elo/Pelo` - Sprint classic technique
- `Sprint_Freestyle_Elo/Pelo` - Sprint freestyle technique
- `Overall_Elo/Pelo` - Overall rating across all race types

### 2. Race Type Determination Logic
```python
# Distance races: Distance != "Sprint" AND Distance != "0"  
# Sprint races: Distance == "Sprint"
# Technique determined by Technique column (Classic/Freestyle)
```

### 3. Team Selection Complexity
Cross-country uses complex team selection logic from `race-picks.R`, not simple ELO-based ranking like biathlon. Team selection considers:
- Multiple technique specializations
- Distance preferences 
- Recent form and training patterns
- Strategic team composition

### 4. Championship Quota
- **4-person quota per nation** (same as biathlon)
- Must handle both classic and freestyle specialists

## Implementation Components

### Phase 1: Configuration Updates

#### 1.1 Config.py Updates
**File:** `~/ski/elo/python/ski/polars/config.py`

**Required Changes:**
- Add `CHAMPS_ATHLETES_MEN_XC` and `CHAMPS_ATHLETES_LADIES_XC` dictionaries
- Include 4-person quota validation for cross-country
- Add cross-country specific athlete roster management

```python
# Cross-country Championships Athletes (4 per nation)
CHAMPS_ATHLETES_MEN_XC = {
    'NOR': ['Johannes Hoesflot Klaebo', 'Paal Golberg', 'Didrik Toenseth', 'Emil Iversen'],
    'SWE': ['Calle Halfvarsson', 'Jens Burman', 'Oskar Svensson', 'William Poromaa'],
    # ... additional nations
}

CHAMPS_ATHLETES_LADIES_XC = {
    'NOR': ['Therese Johaug', 'Tiril Udnes Weng', 'Helene Marie Fossesholm', 'Anne Kjersti Kalva'],
    'SWE': ['Frida Karlsson', 'Ebba Andersson', 'Maja Dahlqvist', 'Jonna Sundling'],
    # ... additional nations  
}
```

**Status:** ❌ **Not Started**

#### 1.2 Directory Structure Setup
**Target Structure:**
```
~/ski/elo/python/ski/polars/excel365/champs-predictions/
├── champs_picks_processing.log
└── (Excel outputs will be generated here)

~/ski/elo/python/ski/polars/relay/excel365/
├── startlist_champs_men_relay.csv
├── startlist_champs_ladies_relay.csv  
├── startlist_champs_mixed_relay.csv
└── startlist_champs_team_sprint.csv
```

**Status:** ❌ **Not Started**

### Phase 2: Unified Startlist Generation

#### 2.1 Create startlist-scrape-champs.py
**File:** `~/ski/elo/python/ski/polars/startlist-scrape-champs.py`

**Architecture Overview:**
Based on analysis of existing cross-country scrape patterns and biathlon championships implementation, the unified scraper will follow this design:

**Race Type Classification from weekends.csv:**
```python
# Individual Distance races: Distance in ["10", "15", "20", "30", "50"] + Technique in ["C", "F", "P"]
# Sprint races: Distance == "Sprint" + Technique in ["C", "F"]  
# Team Sprint: Distance == "Ts" + Sex in ["M", "L"]
# Relay: Distance == "Rel" + Sex in ["M", "L"]
# Mixed Relay: Distance == "Mix" + Sex == "Mixed"
```

**Key Design Principles:**
1. **Single Entry Point**: One script handles all race types unlike current separate scripts
2. **Config-Based Athletes**: Use `CHAMPS_ATHLETES_*_XC` instead of FIS startlist scraping
3. **No Race Probability Calculation**: Generate startlists only; probabilities calculated in R
4. **ELO Matching Priority**: 9-column system with technique-specific prioritization
5. **Quartile Imputation**: Fallback for athletes not in historical data
6. **Consistent Output Structure**: Match existing file naming and column conventions

**Core Functions Required:**
```python
def process_championships() -> None
    """Main entry point - reads weekends.csv, filters Championship==1, routes by race type"""

def create_individual_championships_startlist(gender: str, races_df: pd.DataFrame, race_type: str) -> None
    """Handle individual distance and sprint races - use config athletes with 4-person quota"""

def create_team_sprint_championships_startlist(gender: str, races_df: pd.DataFrame) -> None  
    """Create 2-person team startlists for team sprint events"""

def create_relay_championships_startlist(gender: str, races_df: pd.DataFrame) -> None
    """Create 4-person relay team startlists using top athletes by ELO"""

def create_mixed_relay_championships_startlist(races_df: pd.DataFrame) -> None
    """Create mixed gender relay teams (2M+2L) using combined ELO data"""

def get_race_specific_elo_priority(distance: str, technique: str) -> List[str]
    """Return prioritized ELO column list based on race characteristics"""
    
def apply_4_person_quota_selection(nation_athletes: List[str], elo_scores: pd.DataFrame, 
                                  race_distance: str, race_technique: str) -> List[str]
    """Select best 4 athletes per nation for individual races using race-picks methodology"""
```

**ELO Column Prioritization Logic:**
```python
# Sprint Classic: ['Sprint_C_Elo', 'Sprint_Elo', 'Classic_Elo', 'Elo']
# Sprint Freestyle: ['Sprint_F_Elo', 'Sprint_Elo', 'Freestyle_Elo', 'Elo']  
# Distance Classic: ['Distance_C_Elo', 'Distance_Elo', 'Classic_Elo', 'Elo']
# Distance Freestyle: ['Distance_F_Elo', 'Distance_Elo', 'Freestyle_Elo', 'Elo']
# Pursuit: ['Distance_Elo', 'Elo'] (technique-neutral)
```

**Data Flow Architecture:**
```python
1. Read weekends.csv -> filter Championship == 1
2. Classify races by (Distance, Sex, Technique) combinations
3. For each race type:
   - Load appropriate ELO data (men/ladies/both)
   - Get config athletes for qualifying nations  
   - Apply race-specific athlete selection (4-person quota for individuals)
   - Match athletes with ELO scores using prioritized columns
   - Apply quartile imputation for missing athletes
   - Generate output CSV in appropriate directory
4. No R script calling (handled separately by champs-predictions.R)
```

**Output File Structure (Updated to Match Weekend Scraper Pattern):**
```
Individual Events (Consolidated like weekend scraper):
~/ski/elo/python/ski/polars/excel365/startlist_champs_men.csv
~/ski/elo/python/ski/polars/excel365/startlist_champs_ladies.csv
  - Single file per gender containing all championship athletes
  - Multiple race probability columns: Race1_Prob, Race2_Prob, etc.
  - Each column represents participation in a specific championship race
  - Athletes have 1.0 probability for races they're selected for, 0.0 otherwise

Team Events (Dual output like existing relay scrapers):
~/ski/elo/python/ski/polars/relay/excel365/startlist_champs_relay_teams_{gender}.csv
~/ski/elo/python/ski/polars/relay/excel365/startlist_champs_relay_individuals_{gender}.csv
~/ski/elo/python/ski/polars/relay/excel365/startlist_champs_team_sprint_teams_{gender}.csv  
~/ski/elo/python/ski/polars/relay/excel365/startlist_champs_team_sprint_individuals_{gender}.csv
~/ski/elo/python/ski/polars/relay/excel365/startlist_champs_mixed_relay_teams.csv
~/ski/elo/python/ski/polars/relay/excel365/startlist_champs_mixed_relay_individuals.csv
  - Team files: aggregated team data with total/average ELOs
  - Individual files: individual athlete records with team affiliations
```

**Status:** ✅ **Completed** - Implementation matches existing cross-country scraper patterns

#### 2.2 Enhanced Team Selection Logic  
**Challenge:** Cross-country uses race-picks.R methodology for individual race selection, which is more sophisticated than simple ELO ranking.

**Current Cross-Country Selection Patterns (from existing codebase):**
```python
# Individual races: Apply race-picks.R algorithm that considers:
# 1. Technique specialization (classic vs freestyle performance differentials)
# 2. Distance specialization (sprint vs distance performance patterns) 
# 3. Recent form factors and seasonal trends
# 4. Strategic team composition for maximum medal potential

# Team events: Use ELO-based selection (similar to biathlon pattern)
# - Relay teams: Top 4 athletes by race-relevant ELO
# - Team Sprint: Top 2 athletes by sprint-relevant ELO  
# - Mixed events: Top athletes by ELO from each gender
```

**Implementation Strategy for Championships:**
```python
# ✅ Phase 2.2a: Simplified ELO Selection (COMPLETED)
def apply_4_person_quota_selection(nation_athletes: List[str], elo_scores: pd.DataFrame, 
                                  race_distance: str, race_technique: str) -> List[str]:
    """Select athletes using prioritized ELO columns - implemented and working"""
    # 1. Get race-specific ELO priority list (9-column system)
    # 2. Sort nation's athletes by highest relevant ELO
    # 3. Return top 4 athletes for the specific race
    
# 🔄 Phase 2.2b: Enhanced Selection (Future Enhancement) 
def select_championship_athletes_advanced(nation: str, gender: str, race_distance: str,
                                        race_technique: str, max_athletes: int = 4) -> List[str]:
    """Select athletes using race-picks.R methodology"""  
    # 1. Calculate technique differential scores
    # 2. Apply distance specialization factors
    # 3. Include recent performance trends
    # 4. Optimize for team medal probability
```

**4-Person Quota Implementation (ARCHITECTURE UPDATED):**
```python
# ✅ Python startlist generation: Include ALL championship athletes with 0.0 probabilities
# ✅ R script quota handling: Use sophisticated probability calculations (like biathlon)
# ✅ Consolidated output: All athletes with Race1_Prob, Race2_Prob columns for R processing

def create_consolidated_individual_championships_startlist(gender: str, races_df: pd.DataFrame):
    """Implemented - creates weekend-scraper-style output following biathlon pattern"""
    # 1. ✅ Load all championship athletes for gender
    # 2. ✅ Create consolidated dataframe with all athletes  
    # 3. ✅ Initialize all Race*_Prob columns to 0.0 (no quota selection in Python)
    # 4. ✅ R script will calculate base probabilities from historical data
    # 5. ✅ R script will apply 4-person quota constraint with fractional probabilities
    # 6. ✅ Output single CSV per gender matching biathlon architecture
```

**Integration with Existing Architecture (COMPLETED):**
- **✅ Phase 2.2a**: ELO prioritization implemented using 9-column cross-country system
- **📋 Phase 2.2b**: Integration with race-picks.R algorithms (future enhancement)
- **📋 Phase 2.2c**: Cross-race quota optimization (future advanced feature)

**Key Implementation Details:**
```python
# ✅ All championship athletes included (no Python quota selection):
# - All configured athletes from CHAMPS_ATHLETES_*_XC included in startlist
# - All Race*_Prob columns initialized to 0.0 for R script processing

# ✅ Output matches biathlon architecture exactly:
# - startlist_champs_men.csv with Race1_Prob, Race2_Prob columns (all 0.0)
# - startlist_champs_ladies.csv with Race1_Prob, Race2_Prob columns (all 0.0)
# - R script will calculate sophisticated probabilities and apply quota constraints

# ✅ Race-specific ELO prioritization available for R script:
# - Sprint Classic: ['Sprint_C_Elo', 'Sprint_Elo', 'Classic_Elo', 'Elo']
# - Distance Freestyle: ['Distance_F_Elo', 'Distance_Elo', 'Freestyle_Elo', 'Elo']
# - Pursuit: ['Distance_Elo', 'Elo'] (technique-neutral)
```

**Status:** ✅ **Phase 2.2a Completed** - ELO-based selection working, ready for R script integration

### Phase 3: Relay Data Processing

#### 3.1 Create relay_chrono.py
**File:** `~/ski/elo/python/ski/polars/relay/relay_chrono.py`

**Enhancements over Biathlon Version:**
- Handle 9 ELO/PELO columns instead of 5
- Technique-specific aggregations for team events
- Support for Team Sprint events (unique to cross-country)
- Enhanced quartile imputation for broader rating system

**Key Processing Functions:**
```python
def process_team_sprint(combined_df)     # Team Sprint events
def process_mixed_relay(combined_df)     # 2M+2L Mixed Relay  
def process_relay(df, sex)              # Standard M/L Relays
def get_xc_elo_pelo_columns(df)         # 9 XC rating columns
def calculate_technique_aggregates(df)   # Technique-specific metrics
```

**Status:** ❌ **Not Started**

#### 3.2 Technique-Specific Processing
**Key Requirement:** Process relay teams with awareness of technique specializations.

**Implementation Details:**
- Classic specialists contribute more to classic-technique relays
- Freestyle specialists contribute more to freestyle-technique relays
- Mixed technique teams require balanced representation
- Quartile imputation applied per technique category

**Status:** ❌ **Not Started**

### Phase 4: Unified R Prediction Script

#### 4.1 Create champs-predictions.R  
**File:** `~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions.R`

**Core Architecture:** Based on successful biathlon implementation with cross-country adaptations.

**Key Components:**

##### 4.1.1 Race Type Processing
```r
# Individual races (Distance != "Sprint" AND Distance != "0")
men_races <- champs_races %>%
  filter(Sex == "M", Distance != "Sprint", Distance != "0") 

# Sprint races (Distance == "Sprint") 
men_sprints <- champs_races %>%
  filter(Sex == "M", Distance == "Sprint")

# Relay races (RaceType contains "Relay")
men_relays <- champs_races %>%
  filter(Sex == "M", grepl("Relay", RaceType, ignore.case = TRUE))

# Team Sprint races (RaceType contains "Team Sprint")  
team_sprints <- champs_races %>%
  filter(grepl("Team Sprint", RaceType, ignore.case = TRUE))

# Mixed Relay races (Sex == "Mixed")
mixed_relays <- champs_races %>%
  filter(Sex == "Mixed")
```

##### 4.1.2 ELO/PELO Prediction Mapping
**Critical Fix:** Train on PELO, predict on ELO (same pattern as biathlon).

```r
# 9-column mapping for cross-country
elo_cols <- c("Distance_Classic_Elo", "Mid_Classic_Elo", "Short_Classic_Elo",
             "Distance_Freestyle_Elo", "Mid_Freestyle_Elo", "Short_Freestyle_Elo", 
             "Sprint_Classic_Elo", "Sprint_Freestyle_Elo", "Overall_Elo")

pelo_cols <- c("Distance_Classic_Pelo", "Mid_Classic_Pelo", "Short_Classic_Pelo",
              "Distance_Freestyle_Pelo", "Mid_Freestyle_Pelo", "Short_Freestyle_Pelo",
              "Sprint_Classic_Pelo", "Sprint_Freestyle_Pelo", "Overall_Pelo")

# Mapping for prediction phase
elo_to_pelo_map <- setNames(pelo_cols, elo_cols)
```

##### 4.1.3 Prev_Points_Weighted Calculations  
**Individual Races:** Filter by specific race type (Distance/Sprint + Technique)
**Relays:** Filter RaceType != "Offseason" (broader inclusion)
**Team Sprints:** Use team member average weighted points

**Status:** ❌ **Not Started**

#### 4.2 GAM Model Adaptations
**Enhanced Feature Selection:** Handle 9 rating systems with regsubsets.

**Model Architecture:**
```r
# Technique-specific model selection
classic_features <- c("Distance_Classic_Pelo", "Mid_Classic_Pelo", "Short_Classic_Pelo")
freestyle_features <- c("Distance_Freestyle_Pelo", "Mid_Freestyle_Pelo", "Short_Freestyle_Pelo") 
sprint_features <- c("Sprint_Classic_Pelo", "Sprint_Freestyle_Pelo")

# Combined model with technique awareness
full_model <- gam(Position ~ s(selected_features) + Technique + Distance, data = train_data)
```

**Status:** ❌ **Not Started**

#### 4.3 Team Selection Integration
**Complexity:** Integrate race-picks.R team selection methodology.

**Planned Approach:**
1. **Phase 4.3a:** Implement simplified ELO-based team selection
2. **Phase 4.3b:** Enhance with technique-specific preferences  
3. **Phase 4.3c:** Full race-picks.R methodology integration

**Status:** ❌ **Not Started**

### Phase 5: Excel Output Generation

#### 5.1 Multi-Workbook Structure
**Individual Events:**
- `XC_Champs_Men_Individual.xlsx`
- `XC_Champs_Ladies_Individual.xlsx` 
- `XC_Champs_Men_Sprint.xlsx`
- `XC_Champs_Ladies_Sprint.xlsx`

**Team Events:**
- `XC_Champs_Team_Sprint.xlsx` 
- `XC_Champs_Men_Relay.xlsx`
- `XC_Champs_Ladies_Relay.xlsx`
- `XC_Champs_Mixed_Relay.xlsx`

**Status:** ❌ **Not Started**

#### 5.2 Probability Constraints
**4-person quota:** Each nation limited to 4 participants across individual events.
**Team quotas:** Nations must have sufficient athletes for team events.

**Implementation:**
```r
# Ensure 4-person constraint across all individual events
nation_quotas <- aggregate_individual_selections(men_individual, ladies_individual, 
                                               men_sprint, ladies_sprint)
validate_nation_quotas(nation_quotas, max_per_nation = 4)
```

**Status:** ❌ **Not Started**

## Testing Strategy

### Unit Testing
- [ ] Config.py athlete validation
- [ ] ELO/PELO column mapping accuracy  
- [ ] Quartile imputation for all 9 systems
- [ ] Team selection algorithm validation

### Integration Testing  
- [ ] End-to-end startlist generation
- [ ] R script data processing pipeline
- [ ] Excel output validation
- [ ] Quota constraint enforcement

### Performance Testing
- [ ] Processing time benchmarks
- [ ] Memory usage optimization
- [ ] Large dataset handling

## Risk Mitigation

### Technical Risks
1. **9-system complexity:** Start with simplified models, iterate to full complexity
2. **Team selection accuracy:** Implement in phases (ELO → technique-aware → full race-picks.R)
3. **Data integration:** Extensive testing with biathlon patterns as baseline

### Implementation Risks  
1. **Timeline pressure:** Prioritize core functionality over advanced features
2. **Quality assurance:** Validate against biathlon implementation patterns
3. **Debugging complexity:** Implement comprehensive logging throughout

## Success Criteria

### Phase 1 Success Metrics
- [ ] Config.py properly validates 4-person quotas
- [ ] Directory structure supports all race types
- [ ] Athlete rosters accurately configured

### Phase 2 Success Metrics  
- [ ] Unified startlist generation handles all 5 race types
- [ ] ELO matching with quartile fallbacks works consistently
- [ ] Team selection produces realistic team compositions

### Phase 3 Success Metrics
- [ ] Relay processing handles 9 ELO/PELO systems
- [ ] Technique-specific aggregations work correctly
- [ ] Team Sprint processing implemented successfully

### Phase 4 Success Metrics
- [ ] GAM models train and predict successfully
- [ ] Probability constraints enforced properly
- [ ] Excel outputs generated in correct format

### Final Success Metrics
- [ ] Complete championships prediction system operational
- [ ] All race types properly supported
- [ ] Output quality matches biathlon implementation standards
- [ ] Performance meets acceptable benchmarks

## Timeline Estimates

**Phase 1 (Config):** 2-3 days  
**Phase 2 (Startlists):** 4-5 days  
**Phase 3 (Relay Processing):** 3-4 days  
**Phase 4 (R Predictions):** 5-7 days  
**Phase 5 (Excel Outputs):** 2-3 days  

**Total Estimated Duration:** 16-22 days

## Notes and Considerations

### Technical Debt
- Simplified team selection in initial implementation
- Gradual enhancement toward full race-picks.R complexity
- Performance optimization as secondary priority

### Future Enhancements
- Advanced technique-specific team selection
- Historical performance trend analysis  
- Integration with live race data feeds
- Enhanced visualization and reporting

### Dependencies
- Successful biathlon implementation as foundation
- Access to complete cross-country ELO/PELO data
- race-picks.R methodology documentation
- Testing data for validation

---

**Document Status:** Living document - updated throughout implementation  
**Next Review Date:** Upon Phase 1 completion  
**Implementation Lead:** Cross-country championships team  
**Last Modified:** 2025-01-06