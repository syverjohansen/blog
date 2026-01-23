# Winter Sports Methodology Documentation Status

## Recently Completed Work (Current Session)

### ## Relay Normalization and Monotonic Constraints (COMPLETED)

**Objective**: Document comprehensive relay probability normalization methodology across winter sports
**Status**:  COMPLETED

**Files Enhanced/Created**:
1. **Cross-Country Relay Normalization** (`/content/post/cross-country/methods/race-picks.md` lines 3968-4334)
   - Enhanced existing section with 5-stage sophisticated framework
   - Stage 1: Mode-based probability reset with format-specific strategies
   - Stage 2: Target-sum normalization with race participation weighting  
   - Stage 3: Enhanced monotonic constraint application with tracking
   - Stage 4: Iterative re-normalization with convergence monitoring
   - Stage 5: Final validation and quality assurance

2. **Existing Sections Confirmed Complete**:
   - **Biathlon Relay Normalization** (lines 1717-1941) - Conservative mathematical consistency
   - **Nordic Combined Relay Normalization** (lines 4321-4344) - Unified team treatment with dual-discipline integration
   - **Ski Jumping** - Has normalization section in Individual methodology

3. **Main Methodology Summary** (`/content/post/methods/race-picks.md` lines 1515-1527)
   - Cross-Country: Multi-stage sophisticated framework
   - Nordic Combined: Unified team treatment
   - Biathlon: Conservative mathematical consistency  
   - Ski Jumping: Venue-dependent normalization
   - Mathematical universality principles

**Key Methodology Insights**:
- **Target-sum normalization**: Win=1, Podium=3, Top5=5, Top10=10, Top30=30
- **Monotonic constraints**: Win d Podium d Top5 d Top10 d Top30
- **Cross-Country most sophisticated**: 5-stage processing with mode reset, iterative convergence
- **Format-specific strategies**: Standard Relay, Team Sprint, Mixed Relay variants

### ## Cross-Country Relay Fantasy (COMPLETED)

**Objective**: Document Cross-Country relay fantasy optimization across three formats
**Status**:  COMPLETED

**Files Enhanced**:
1. **Cross-Country Specific Section** (`/content/post/cross-country/methods/race-picks.md` lines 4337-4518)
   - Format-specific fantasy frameworks for Standard Relay, Team Sprint, Mixed Relay
   - Team formation and expected points calculation with leg importance weighting
   - Mixed Integer Programming (MIP) optimization using GLPK solver
   - Budget constraints (100,000 units) and format-specific team limits

2. **Main Methodology Section** (`/content/post/methods/race-picks.md` lines 1529-1539)
   - Cross-format integration and competitive intelligence
   - Mathematical foundation for expected points calculation
   - Knapsack algorithm implementation with multi-constraint optimization

**Key Fantasy Framework Components**:
- **Standard Relay (4-leg)**: Leg importance 20%, 20%, 25%, 35% (anchor leg emphasis)
- **Team Sprint (2-leg)**: Equal leg weighting 50%, 50%, technique-specific (Classic/Freestyle)
- **Mixed Relay (4-leg)**: Gender constraints (F-Classic, M-Classic, F-Freestyle, M-Freestyle)
- **Budget System**: 100,000 price units, 6 teams per gender (Standard/Team Sprint), 12 teams total (Mixed)
- **Expected Points**: Probability-weighted using relay points (200, 160, 120, 100, 90, 80...)

## File Structure and Organization

### Core Methodology Files:
1. **`/content/post/methods/race-picks.md`** - Main cross-sport methodology summary
2. **`/content/post/cross-country/methods/race-picks.md`** - Cross-country specific detailed methodology  
3. **`/content/post/biathlon/methods/race-picks.md`** - Biathlon specific methodology
4. **`/content/post/nordic-combined/methods/race-picks.md`** - Nordic Combined methodology
5. **`/content/post/skijump/methods/race-picks.md`** - Ski jumping methodology

### Source R Scripts Analyzed:
- **Fantasy Scripts**: 
  - `/drafts/weekly-picks-relay.R` (Standard Relay)
  - `/drafts/weekly-picks-team-sprint.R` (Team Sprint)  
  - `/drafts/weekly-picks-mixed-relay.R` (Mixed Relay)
- **Normalization Scripts**: Various relay prediction scripts across sports

### Section Structure Pattern:
Each sport methodology follows consistent structure:
- **### Individual** (with subsections: Data Gathering, Points, Probability, Normalization)
- **### Relay** (with subsections: Data Gathering, Points, Probability, Normalization, Fantasy)

## Key Methodological Framework Insights

### Cross-Country Relay Complexity:
- **Most sophisticated** winter sports relay methodology
- **Three distinct formats** requiring format-specific optimization
- **Technique-aware processing** (Classic vs Freestyle)
- **Gender-constrained optimization** for Mixed Relay
- **Progressive leg importance** reflecting tactical considerations

### Normalization Mathematical Principles:
- **Probability bounds**: All probabilities  [0,1]  
- **Target sums**: Match available positions (1 winner, 3 podium, etc.)
- **Monotonic constraints**: Logical ordering across probability categories
- **Format-specific adaptations** while maintaining mathematical consistency

### Fantasy Optimization Framework:
- **Mixed Integer Programming** with GLPK solver
- **Multi-constraint knapsack** algorithm
- **Binary decision variables** for team selection
- **Expected value maximization** using probability-weighted point systems

## Working Approach and Best Practices

### Documentation Methodology:
1. **Read source R scripts** thoroughly to understand implementation
2. **Use Task tool** for complex analysis and file structure research  
3. **TodoWrite/TodoRead** for systematic progress tracking
4. **MultiEdit for complex enhancements**, Edit for simple changes
5. **Maintain consistent technical depth** with comprehensive code examples

### File Management:
- **Always read files first** before editing to understand context
- **Use line offsets** for large files to focus on relevant sections
- **Maintain existing section structure** and formatting consistency
- **Add new sections at appropriate hierarchy levels**

### Content Standards:
- **Technical accuracy** with specific R code examples
- **Sport-specific differentiation** rather than assuming uniformity  
- **Mathematical rigor** with proper constraint definitions
- **Competitive realism** reflecting actual sport characteristics

## Future Work Considerations

### Potential Next Areas:
1. **Individual Fantasy** methodologies (if not already documented)
2. **Real-time prediction updates** and live scoring methodologies
3. **Cross-sport comparative analysis** of prediction accuracy
4. **Advanced model validation** and backtesting frameworks

### Methodology Expansion:
- **Alpine/Ski Jumping relay** fantasy (if relay formats exist)
- **Advanced constraint systems** for fantasy optimization
- **Multi-weekend strategy optimization** for fantasy leagues
- **Risk management frameworks** for prediction confidence intervals

## Technical Notes

### R Package Dependencies:
- **Optimization**: `ompr`, `ompr.roi`, `ROI.plugin.glpk` for MIP solving
- **Data Processing**: `dplyr`, `tidyr`, `purrr` for data manipulation
- **Modeling**: `mgcv` (GAM), `xgboost`, `caret` for prediction models
- **File I/O**: `openxlsx`, `arrow`, `readr` for data import/export

### Mathematical Framework Consistency:
- **Relay point systems** vary by sport but follow consistent hierarchies
- **Probability aggregation** uses weighted sums with leg importance factors
- **Constraint hierarchies** balance budget, composition, and format requirements
- **Validation frameworks** ensure mathematical and competitive consistency

This documentation represents the most comprehensive relay methodology framework for winter sports prediction and fantasy optimization, with Cross-Country serving as the flagship implementation demonstrating the full complexity and sophistication possible in multi-format relay prediction systems.

## Current Blog Post Development Framework

### Primary Blog Post Structure:
**Main Blog Post**: `~/blog/daehl-e/content/post/methods/race-picks.md`
- Central methodology blog post synthesizing race pick approaches across all winter sports
- Draws content and insights from sport-specific detailed methodology notes

### Supporting Methodology Notes (Source Files):
**Sport-Specific Detail Files** serve as comprehensive notes for the main blog post:
1. `~/blog/daehl-e/content/post/alpine/methods/race-picks.md` - Alpine skiing methodology notes
2. `~/blog/daehl-e/content/post/biathlon/methods/race-picks.md` - Biathlon methodology notes  
3. `~/blog/daehl-e/content/post/cross-country/methods/race-picks.md` - Cross-country skiing methodology notes
4. `~/blog/daehl-e/content/post/nordic-combined/methods/race-picks.md` - Nordic Combined methodology notes
5. `~/blog/daehl-e/content/post/skijump/methods/race-picks.md` - Ski jumping methodology notes

### Content Development Approach:
- **Sport-specific files**: Detailed technical documentation with comprehensive R code examples, mathematical frameworks, and sport-specific nuances
- **Main blog post**: Synthesized methodology overview drawing key insights, comparative analysis, and unified framework from the detailed notes
- **Content flow**: Sport notes → Main blog post (not vice versa)

## Current Work: Main Blog Post Section-by-Section Development

### Objective:
Edit `~/blog/daehl-e/content/post/methods/race-picks.md` one section at a time to inform readers how race-picks are made.

### Content Structure:
**Sports** → **Formats** → **Main Sections** → **Subsections**

**Sports**: Alpine, Biathlon, Cross-Country, Nordic-Combined, Ski Jumping

**Formats**: 
- Alpine: Individual only
- All other sports: Individual and Relay

**Main Sections**: 
1. Data Gathering
2. Points
3. Probability  
4. Normalization and Monotonic Constraints
5. Fantasy Team (Cross-Country only)

**Points and Probability Subsections**:
- **Training**: Setup, Feature Selection, Modeling, Adjustments
- **Testing**: Startlist Setup, Modeling, Adjustments

### Content Guidelines:
- **Brevity over comprehensiveness** - don't want too much information per section
- **Avoid being too wordy** - concise explanations
- **Focus on methodology** - how race-picks are made, not exhaustive technical detail
- **Reader-friendly** - informative but accessible

### Current Progress:
**Completed**:
- ✅ Alpine → Individual (complete: Data Gathering, Points, Probability, Normalization and Monotonic Constraints)
- ✅ Biathlon → Individual (complete: Data Gathering, Points, Probability, Normalization and Monotonic Constraints) 
- ✅ Biathlon → Relay (complete: Data Gathering, Points, Probability, Normalization and Monotonic Constraints)
- 🔄 Cross-Country → Individual → Fantasy (just completed)

**Currently working on**: Cross-Country → Individual → Probability

---

## Winter Olympics Championship Prediction Blog Posts (2026-01-20)

### Objective
Create two comprehensive blog posts for Winter Olympics predictions:
1. **Team Predictions Post**: Country-by-country breakdown showing athletes competing and performance predictions
2. **Calendar Post**: Race-by-race breakdown with participation, winning, and podium probabilities

### Data Flow Pipeline

**Stage 1: Python Startlist Scraping**
```
~/ski/elo/python/{sport}/polars/startlist-scrape-champs.py
```
Where `{sport}` = alpine, biathlon, ski (cross-country), nordic-combined, skijump

**Stage 2: R Probability Predictions**
```
~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions.R
```
Where `{sport}` = alpine, biathlon, cross-country, nordic-combined, skijump

**Outputs** (stored in `~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions/{YYYYMMDD}/`):
- `{gender}.xlsx` - Summary stats per athlete/team
- `{gender}_position_probabilities.xlsx` - Race-by-race detailed predictions

### Current Status by Sport

| Sport | Python Scraper | R Script | Individual | Relay | Latest Output |
|-------|---------------|----------|------------|-------|---------------|
| Alpine | ✅ Complete | ✅ Complete | ✅ Active | N/A | 20251104 |
| Biathlon | ✅ Complete | ⚠️ Partial | ✅ Active | ⚠️ Commented out | 20251105 |
| Cross-Country | ✅ Complete | ✅ Complete | ✅ Active | ✅ Active | 20260120 |
| Nordic Combined | ✅ Complete | ⚠️ Partial | ⚠️ Commented out | ✅ Active | 20251105 |
| Ski Jumping | ✅ Complete | ✅ Complete | ✅ Active | ✅ Active | 20251104 |

### Known Issues to Troubleshoot

1. **Biathlon champs-predictions.R** (~lines 1677-1708): Relay predictions are commented out/deactivated in main execution
2. **Nordic Combined champs-predictions.R** (~lines 1485-1493): Individual predictions are commented out; only team predictions active
3. **Output dates are old**: Most outputs from November 2025; need to rerun with current data for Olympics
4. **Missing ladies team outputs**: Nordic Combined missing ladies teams (may be intentional - check Olympic program)

### Workflow Understanding

**Python Scrapers Generate:**
- Athlete startlists with ELO columns (discipline-specific)
- Team compositions for relays (based on top N athletes by ELO)
- Initial Race_Prob columns set to 0.0

**R Scripts Calculate:**
- Participation probabilities (quota-constrained: typically 4 athletes/nation)
- Position probabilities at 5 thresholds: Top1, Top3, Top5, Top10, Top30
- GAM-based modeling with feature selection (regsubsets → gam)
- Normalization ensuring probabilities sum correctly

### Planned Blog Post Structure

**Post 1: Team Predictions**
- Introduction explaining methodology
- Country sections (major nations first: Norway, Sweden, France, Germany, USA, etc.)
- Per country: athletes selected, strength by discipline, medal predictions
- Tables showing top medal contenders aggregated from probability data

**Post 2: Calendar/Race-by-Race**
- Race schedule with dates/locations
- Per race: favorites (top win probabilities), podium contenders, dark horses
- Relay predictions with team compositions
- Interactive elements (sortable tables if possible)

### Next Steps

1. **Troubleshoot Issues**: Fix commented-out sections in biathlon and nordic-combined R scripts
2. **Rerun Pipeline**: Execute Python scrapers then R scripts for all sports with current data
3. **Verify Outputs**: Ensure all xlsx files generated correctly in 20260120 folders
4. **Design Post Templates**: Create markdown templates for both post types
5. **Data Integration**: Write scripts to pull data from xlsx into markdown format
6. **Content Writing**: Draft narrative content for each section

### Key Files Reference

**Python Scrapers:**
- `~/ski/elo/python/alpine/polars/startlist-scrape-champs.py`
- `~/ski/elo/python/biathlon/polars/startlist-scrape-champs.py`
- `~/ski/elo/python/ski/polars/startlist-scrape-champs.py` (cross-country)
- `~/ski/elo/python/nordic-combined/polars/startlist-scrape-champs.py`
- `~/ski/elo/python/skijump/polars/startlist-scrape-champs.py`

**R Prediction Scripts:**
- `~/blog/daehl-e/content/post/alpine/drafts/champs-predictions.R`
- `~/blog/daehl-e/content/post/biathlon/drafts/champs-predictions.R`
- `~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions.R`
- `~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions.R`
- `~/blog/daehl-e/content/post/skijump/drafts/champs-predictions.R`

**Current Output Locations:**
- Alpine: `~/blog/daehl-e/content/post/alpine/drafts/champs-predictions/20251104/`
- Biathlon: `~/blog/daehl-e/content/post/biathlon/drafts/champs-predictions/20251105/`
- Cross-Country: `~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions/20260120/`
- Nordic Combined: `~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions/20251105/`
- Ski Jumping: `~/blog/daehl-e/content/post/skijump/drafts/champs-predictions/20251104/`

### Session Resume Instructions

If connection is lost, read this file and:
1. User wants to create 2 blog posts: Team Predictions + Calendar/Race-by-Race
2. First troubleshoot issues with biathlon relay and nordic combined individual predictions
3. Then rerun full pipeline to generate fresh predictions
4. Finally help structure and write the blog posts

---

## Cross-Country Team Selection Logic (Explored 2026-01-20)

### Source Files
- **Python Scraper**: `~/ski/elo/python/ski/polars/startlist-scrape-champs.py`
- **Config File**: `~/ski/elo/python/ski/polars/config.py` (contains `CHAMPS_ATHLETES_MEN_XC` and `CHAMPS_ATHLETES_LADIES_XC`)

### Team Selection Algorithm

#### 1. Relay Teams (4-person)
**Function**: `create_relay_championships_startlist()` (lines 420-529)

**Selection Logic**:
1. Filter nations from config with **≥4 athletes** configured
2. For each nation:
   - Get all configured athletes
   - Match each athlete to ELO scores (exact match or fuzzy match)
   - Get ELO priority based on race technique using `get_race_specific_elo_priority('Rel', technique)`
   - For technique 'C' (Classic): prioritize `Classic_Elo > Distance_C_Elo > Sprint_C_Elo > Elo`
   - For technique 'F' (Freestyle): prioritize `Freestyle_Elo > Distance_F_Elo > Sprint_F_Elo > Elo`
3. **Sort athletes by best available ELO (highest first)**
4. **Select top 4 athletes** for the relay team

**Output**: Teams CSV + Individuals CSV with all 9 ELO columns aggregated

#### 2. Team Sprint (2-person)
**Function**: `create_team_sprint_championships_startlist()` (lines 302-418)

**Selection Logic**:
1. Filter nations from config with **≥2 athletes** configured
2. For each nation:
   - Get all configured athletes
   - Use ELO priority for Team Sprint: `get_race_specific_elo_priority('Ts', technique)`
   - Same technique prioritization as relay
3. **Sort athletes by sprint-relevant ELO (highest first)**
4. **Select top 2 athletes** for the team sprint

#### 3. Mixed Relay (2M + 2L)
**Function**: `create_mixed_relay_championships_startlist()` (lines 531-661)

**Selection Logic**:
1. Find nations with athletes in **both** men's and ladies' configs
2. Filter for nations with **≥2 men AND ≥2 ladies**
3. For each qualifying nation:
   - Get top 2 men by overall ELO
   - Get top 2 ladies by overall ELO
4. Team composition: 2 men + 2 ladies (order: men first, then ladies)

### ELO Columns Used (9 total)
```
Elo, Distance_Elo, Distance_F_Elo, Distance_C_Elo,
Sprint_Elo, Sprint_C_Elo, Sprint_F_Elo, Classic_Elo, Freestyle_Elo
```

### Key Points
- **Athletes come from config.py** - manually curated per nation
- **ELO-based ranking** determines team composition
- **Technique-specific** ELO prioritization (Classic vs Freestyle events)
- **No quota enforcement in Python** - all athletes included, R handles quotas for individual events
- Team outputs include both aggregated team ELO (Total_*, Avg_*) and individual member ELO

### Current Progress
- ✅ Explored cross-country Python scraper team selection logic
- ✅ Documented relay, team sprint, and mixed relay selection algorithms
- ✅ Reviewed R script champs-predictions.R structure

---

## Cross-Country champs-predictions.R Structure (Explored 2026-01-20)

### File Location
`~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions.R`

### Overall Structure (~3555 lines)
```
PART 1: INDIVIDUAL RACES - TRAIN SETUP (lines 22-232)
  - Load chronological data
  - Add world cup points
  - Calculate weighted prev_points by discipline
  - Calculate ELO/PELO percentage columns
  - Quartile imputation for missing values

PART 2: INDIVIDUAL RACES - TRAIN EXECUTION (lines 234-430)
  - Race type classification (Sprint_C, Sprint_F, Distance_C_Ind, etc.)
  - Feature selection using regsubsets()
  - GAM model training for each race type + threshold

PART 3: INDIVIDUAL RACES - TEST SETUP (lines 431-504)
  - Create PELO percentage columns for startlist

PART 4: INDIVIDUAL RACES - TEST EXECUTION (lines 505-628)
  - Apply trained models to championship startlist
  - Calculate position probabilities

process_individual_races() - line 629
  - Orchestrates individual race prediction pipeline

process_relay_races() - line 1181
  - Reads relay chronological data (separate from individual)
  - Leg-specific feature selection (Legs 1-2 Classic, Legs 3-4 Freestyle)
  - Trains GAM models per leg + threshold
  - Calculates team probabilities by combining leg probabilities
  - Leg importance weights from historical data
  - Hierarchy enforcement + normalization

process_ts_races() - line 2313
  - Team Sprint processing (2 legs instead of 4)
  - Technique-specific models (Classic vs Freestyle team sprints)
  - Same GAM approach as relay

MAIN EXECUTION (lines 3502-3555)
  - process_individual_races()
  - process_relay_races()
  - process_ts_races()
```

### Key Relay Processing Logic

**Leg-Specific Variables** (`get_relay_explanatory_vars()` at line 1459):
- Leg 1: Classic with sprint options (`Sprint_C_Pelo_pct`, `Classic_Pelo_pct`)
- Leg 2: Classic distance only (`Distance_C_Pelo_pct`, `Classic_Pelo_pct`)
- Leg 3: Freestyle distance only (`Distance_F_Pelo_pct`, `Freestyle_Pelo_pct`)
- Leg 4: Freestyle with sprint options (`Sprint_F_Pelo_pct`, `Freestyle_Pelo_pct`)

**Weighted Previous Points** (`calculate_relay_weighted_prev_points()` at line 1251):
- Relay results do NOT contribute to future averages (avoids circular dependency)
- Legs 1-2: Use Distance Classic history
- Legs 3-4: Use Distance Freestyle history

**Position Thresholds**: 1 (win), 3 (podium), 5 (top5), 10 (top10)

**Team Probability Calculation**:
1. Get individual leg probabilities from GAM models
2. Weight by leg importance (learned from historical data)
3. Combine across 4 legs: `team_prob = weighted.mean(leg_probs, leg_importance)`
4. Normalize so probabilities sum to expected totals

### Key Team Sprint Processing Logic

**Technique-Specific Models** (`get_ts_explanatory_vars()` at line 2591):
- Classic team sprint: `Sprint_C_Pelo_pct`, `Classic_Pelo_pct`
- Freestyle team sprint: `Sprint_F_Pelo_pct`, `Freestyle_Pelo_pct`

**2-Leg Structure**: Team sprints only have 2 legs (vs 4 for relay)

### Output Files (to `champs-predictions/{YYYYMMDD}/`)
- `team_sprint_optimization.xlsx` - Team composition optimization
- `team_sprint_all_threshold_predictions.xlsx` - All threshold predictions
- `team_sprint_final_predictions.xlsx` - Hierarchy-enforced normalized predictions

### Current Status
- ✅ Individual processing: Complete
- ✅ Relay processing: Complete
- ✅ Team Sprint processing: Complete
- ⚠️ Missing: Mixed Relay processing (not yet implemented in R script)

### Next Steps
- Check if mixed relay is needed for championships
- Review output xlsx files for accuracy
- Integrate outputs into blog post templates

---

## Troubleshooting Log (2026-01-20)

### Issue 1: `prev_points_weighted` column doesn't exist (FIXED)
**Error**:
```
Error in `select()`:
! Can't select columns that don't exist.
✖ Column `prev_points_weighted` doesn't exist.
```

**Location**: `champs-predictions.R` line 708 in `process_individual_races()`

**Cause**: Code assumed startlist from Python scraper had `prev_points_weighted` column

**Fix**: Changed `select(-prev_points_weighted)` to `select(-any_of("prev_points_weighted"))`

**Status**: ✅ Fixed

### Issue 2: Ladies startlist not loaded / PELO_pct columns missing (FIXED)
**Error**:
```
WARN Error predicting for threshold 3 : object 'Distance_Pelo_pct' not found
WARN Error predicting for threshold 5 : object 'Distance_Pelo_pct' not found
```

**Location**: `champs-predictions.R` lines 455-456 and 494

**Cause**: Ladies startlist loading and PELO_pct column creation were commented out

**Fix**: Uncommented:
- Line 455-456: `ladies_startlist <- read.csv(...)`
- Line 494: `ladies_startlist <- create_test_pelo_pct_columns(ladies_startlist)`
- Lines 499-501: Logging for ladies PELO_pct range

**Status**: ✅ Fixed

### Issue 3: `individual_races` not defined for start probability calculation (FIXED)
**Error**: All `Race_Prob` columns were 0 because race probability calculation code was silently failing

**Location**: `champs-predictions.R` line 882 - code references `individual_races` but it wasn't defined

**Cause**: The race probability calculation code at lines 875-909 uses `individual_races`, but that variable was only defined inside `process_individual_races()` function, not in the global setup section where the probability code runs.

**Fix**: Added `individual_races` definition before line 877:
```r
individual_races <- champs_races %>%
  filter(!Distance %in% c("Rel", "Ts"), Sex %in% c("M", "L")) %>%
  mutate(race_type = mapply(determine_race_type, Distance, Technique, MS))
```

**Status**: ✅ Fixed (but superseded by Issue 4)

### Issue 4: Step 3 ran AFTER position predictions instead of BEFORE (FIXED)
**Error**: All position probabilities (Win, Podium, etc.) were 0 because Race_Prob columns didn't exist when predictions were made

**Symptoms**:
- Start probabilities calculated correctly (Mean: 0.257, etc.)
- But normalization showed `Win: 0 → 0` for all races
- Warning: "Start probability column RaceX_Prob not found in startlist"

**Cause**: Code structure issue - inside `process_individual_races()`:
1. Prediction for-loop (lines 650-765) ran FIRST
2. Step 3 (lines 767-1002) calculated Race_Prob columns AFTER
3. So when predictions tried to multiply by start_prob, the columns didn't exist yet

**Fix**: Restructured `process_individual_races()` to run Step 3 BEFORE the prediction loop:
1. Moved Step 3 logic (with local function `get_base_race_probability_local`) to run right after the early return check
2. Used `<<-` for global assignment to modify startlists from within function scope
3. Removed the old duplicate Step 3 code that ran too late (lines 920-1155)

**Status**: ✅ Fixed

### Issue 5: Start probabilities above 1.0 after quota scaling (FIXED)
**Error**: Some athletes had Race_Prob values > 1.0 (e.g., 2.0, 3.5)

**Cause**: Quota scaling formula `scaling_factor = 4 / current_sum` can produce large multipliers when `current_sum` is small. Example:
- Nation has 2 athletes with base probs 0.2 each → `current_sum = 0.4`
- `scaling_factor = 4 / 0.4 = 10`
- Each athlete gets `0.2 * 10 = 2.0` → above 1!

**Fix**: Added `pmin(scaled_probs, 1.0)` to cap individual probabilities at 1.0 after scaling:
```r
scaled_probs <- nation_probs * scaling_factor
scaled_probs <- pmin(scaled_probs, 1.0)  # Cap at 1.0
men_startlist[nation_mask, race_col] <<- scaled_probs
```

Applied to both men's and ladies' quota constraint loops (lines 752-755 and 779-782).

**Status**: ✅ Fixed

---

## Methodology Improvements to Adopt Across Sports

### Weighted Participation Probability (Implemented in Cross-Country 2026-01-20)

**What**: Replace flat participation rate with exponential decay weighted participation for calculating start/race probabilities.

**Why**: Recent race participation is more predictive of future participation than older races. An athlete who raced 3 months ago is more likely to race again than one who last raced 2 years ago.

**Implementation**:
```r
# Time window: later of 5 years ago OR athlete's first race date
five_years_ago <- Sys.Date() - (5 * 365)
athlete_first_race <- chronos %>%
  filter(Skier == participant) %>%
  summarise(first_date = min(Date, na.rm = TRUE)) %>%
  pull(first_date)
cutoff_date <- max(five_years_ago, athlete_first_race)

# Exponential decay weighting (alpha = 0.1)
# Most recent race gets weight 1.0, older races get exponentially less weight
race_weights <- exp(-0.1 * ((n_races - 1):0))

# Calculate weighted participation probability
weighted_participation <- sum(participation * race_weights)
total_weight <- sum(race_weights)
prob <- weighted_participation / total_weight
```

**Key Detail**: The cutoff date uses `max(five_years_ago, athlete_first_race)` so newer athletes are only compared against races since they started competing, not penalized for races before their career began.

**Reference Implementation**:
- **Alpine** (original): `~/blog/daehl-e/content/post/alpine/drafts/race-picks.R` lines 448-488
- **Cross-Country** (updated): `~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions.R` lines 651-706

**Sports to Update**:
| Sport | Script | Status |
|-------|--------|--------|
| Alpine | `race-picks.R` | ✅ Already uses weighted approach |
| Cross-Country | `champs-predictions.R` | ✅ Updated 2026-01-20 |
| Biathlon | `champs-predictions.R` | ⏳ Needs update |
| Nordic Combined | `champs-predictions.R` | ⏳ Needs update |
| Ski Jumping | `champs-predictions.R` | ⏳ Needs update |

**Key Changes Required**:
1. Find the `get_base_race_probability` or equivalent function
2. Replace flat `races_done / total_races` calculation with exponential decay weighting
3. Ensure the function gets all races of the type sorted by date
4. Apply weights where most recent race index gets highest weight

---

## Championship Prediction Blog Posts (2026-01-22)

### Overview

Create two championship prediction blog posts for the 2026 Winter Olympics:

1. **Nations Post**: Country-by-country breakdown showing athletes and performance predictions
2. **Race-by-Race Post**: Calendar view with participation, winning, and podium probabilities per race

### Post Location
```
~/blog/daehl-e/content/post/champs-predictions/2026/
├── nations.md          # Country-by-country predictions
└── race-by-race.md     # Race calendar predictions
```

### Data Flow Pipeline

```
┌─────────────────────────────────────────────────────────────────────────┐
│ STAGE 1: Python Scraper                                                  │
│ ~/ski/elo/python/{sport}/polars/startlist-scrape-champs.py              │
│ → Creates startlists with ELO columns                                    │
│ → Outputs: excel365/startlist_champs_{gender}.csv                       │
└─────────────────────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────┐
│ STAGE 2: R Prediction Script                                             │
│ ~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions.R         │
│ → Calculates position probabilities (Win, Podium, Top5, Top10, Top30)   │
│ → Optimizes relay/team sprint rosters                                    │
│ → Outputs: drafts/champs-predictions/{YYYYMMDD}/*.xlsx                  │
└─────────────────────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────┐
│ STAGE 3: Excel → JSON Conversion                                         │
│ ~/blog/daehl-e/static/python/excel_to_hugo_multiple_sheets.py           │
│ → Converts Excel sheets to JSON format for Hugo datatables              │
│ → Outputs: data/{sport}/drafts/champs-predictions/{YYYYMMDD}/*.json    │
└─────────────────────────────────────────────────────────────────────────┘
                                    ↓
┌─────────────────────────────────────────────────────────────────────────┐
│ STAGE 4: Hugo Blog Post                                                  │
│ Uses shortcode: {{< {sport}/datatable "path/to/json" >}}                │
│ → Renders interactive datatables from JSON                              │
└─────────────────────────────────────────────────────────────────────────┘
```

### Excel Output Structure Requirements

The R scripts need to generate Excel files with specific naming conventions:

**For Nations Post** (aggregated by country):
- `nations_summary.xlsx` - Overall medal predictions by nation
- `{gender}_by_nation.xlsx` - Athletes grouped by nation with probabilities

**For Race-by-Race Post** (per race):
- `{gender}.xlsx` - Summary points/stats per athlete
- `{gender}_position_probabilities.xlsx` - Multiple sheets, one per race
  - Sheet names like: "Men Race 1", "Ladies Race 2", etc.

### JSON Conversion Details

From `excel_to_hugo_multiple_sheets.py`:
- Single-sheet Excel → `filename.json`
- Multi-sheet Excel → `filename_SheetName.json` (one file per sheet)

JSON structure:
```json
{
  "sheet_name": "Men Race 1",
  "headers": ["Skier", "Nation", "Win_Prob", "Podium_Prob", ...],
  "rows": [["Johannes Klaebo", "NOR", "15.2%", "42.1%", ...], ...]
}
```

### Datatable Shortcode Usage

In markdown posts:
```markdown
## Cross-Country

### Men

#### Individual Races

##### Skiathlon

{{< cross-country/datatable "cross-country/drafts/champs-predictions/20260122/men_position_probabilities_Skiathlon" >}}
```

### Data Directory Structure

```
~/blog/daehl-e/data/{sport}/drafts/champs-predictions/{YYYYMMDD}/
├── men.json
├── men_position_probabilities_Race_1.json
├── men_position_probabilities_Race_2.json
├── ladies.json
├── ladies_position_probabilities_Race_1.json
├── nations_summary.json
└── ...
```

### Implementation Plan

#### Phase 1: R Script Excel Output Enhancement
For each sport's `champs-predictions.R`:

1. **Individual Races Output**:
   - `{gender}.xlsx` - Summary with columns: Skier, Nation, Total_Expected_Points, Win_Count, Podium_Count, etc.
   - `{gender}_position_probabilities.xlsx` - Multi-sheet with per-race probabilities

2. **Relay/Team Output**:
   - `{gender}_relay.xlsx` - Team compositions and probabilities
   - `{gender}_team_sprint.xlsx` - Team sprint compositions

3. **Nations Summary Output**:
   - `nations_summary.xlsx` - Aggregated predictions by country

#### Phase 2: Excel → JSON Conversion
Run conversion script:
```bash
~/blog/daehl-e/static/python/convert_all_excel_to_json.sh \
  ~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions/{YYYYMMDD} \
  ~/blog/daehl-e/data/{sport}/drafts/champs-predictions/{YYYYMMDD}
```

#### Phase 3: Blog Post Creation
Create markdown files with datatable shortcodes referencing the JSON files.

### Current Understanding of Relay Selection

#### Python Scraper (startlist-scrape-champs.py)
- Selects top 4 athletes per nation by ELO (technique-prioritized)
- Does NOT assign athletes to specific legs
- Outputs to `relay/excel365/startlist_champs_relay_*.csv`

#### R Script (champs-predictions.R)
- Reads general individual startlist (ALL athletes per nation)
- Brute-force tries all permutations of 4 athletes across 4 legs
- Leg-specific models: Legs 1-2 use Classic features, Legs 3-4 use Freestyle features
- Selects permutation with highest team probability

#### Team Sprint
- Same approach but 2 athletes, 2 legs
- Single technique per race (either all Classic OR all Freestyle)
- Uses sprint-specific PELO variables

### Relay Selection Improvements (To Discuss)

Potential improvements to relay team selection:
1. **Python-side**: Pre-filter athletes by technique specialty
2. **R-side**: Consider limiting permutations to top N athletes per technique
3. **Leg assignment**: Explicit technique-based pre-assignment before optimization

### Next Steps

1. ⏳ Review/fix relay selection in Python scraper (technique-aware pre-selection)
2. ⏳ Review/fix R script relay loading (use relay-specific startlists?)
3. ⏳ Ensure R scripts output correct Excel format for conversion
4. ⏳ Run full pipeline: Python → R → Excel → JSON
5. ⏳ Create blog post templates with datatable shortcodes
6. ⏳ Write narrative content for nations and race-by-race posts