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
| Ski Jumping | `champs-predictions.R` | ✅ Complete (2026-01-27) |

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
3. ✅ Ensure R scripts output correct Excel format for conversion
4. ⏳ Run full pipeline: Python → R → Excel → JSON
5. ✅ Create blog post templates with datatable shortcodes
6. ⏳ Write narrative content for nations and race-by-race posts

---

## Championship Predictions Pipeline Updates (2026-01-25)

### Overview of Changes

Updated the cross-country champs-predictions.R and champs_script.sh to create a complete automated pipeline for championship prediction blog posts.

### Key Changes

#### 1. Output Directory Structure Changed (YYYYMMDD → YYYY)

**Before**: `~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions/20260120/`
**After**: `~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions/2026/`

**Rationale**: Only one championship per year, so year-based directories are cleaner.

**Files Updated**:
- `champs-predictions.R` line ~1098: `current_year <- format(Sys.Date(), "%Y")`

#### 2. Blog Post Structure (One Post Per Sport)

**Before**: Two separate posts (Nations + Race-by-Race)
**After**: One post per sport with two sections

**Post Location**: `~/blog/daehl-e/content/post/champs-predictions/2026/{sport}.md`

**Structure**:
```markdown
# 2026 Winter Olympics - {Sport} Predictions

## Calendar
### Individual Races
#### Men
##### {Race Name}
{{< sport/datatable "..." >}}
#### Ladies
...
### Relay
### Team Sprint

## Nation
### Summary
### Men
#### {Nation}
...
### Ladies
#### {Nation}
...
```

#### 3. Simplified Excel Column Output

**Before**: 24+ columns including all intermediate calculations
```
Skier, Nation, ID, raw_win_prob, raw_podium_prob, raw_top5_prob, raw_top10_prob,
raw_top30_prob, Race1_Prob, start_prob, win_prob, podium_prob, top5_prob,
top10_prob, top30_prob, pre_norm_win, pre_norm_podium, pre_norm_top5,
pre_norm_top10, pre_norm_top30, pre_hierarchy_win, pre_hierarchy_podium,
pre_hierarchy_top5, pre_hierarchy_top10, pre_hierarchy_top30
```

**After**: 9 columns with user-friendly names
```
Skier, Nation, ID, Start, Win, Podium, Top5, Top-10, Top-30
```

**Files Updated**:
- `champs-predictions.R` lines ~1108-1126: Added select/rename before storing results

#### 4. Nations Excel Split by Gender

**Before**: Nations counted overall (4+ athletes total)
**After**: Nations counted per gender (4+ athletes per gender)

**Sheet Structure**:
- `{Nation} Men` - Nations with 4+ male athletes
- `{Nation} Ladies` - Nations with 4+ female athletes
- `Other Men` - Combined sheet for nations with <4 male athletes
- `Other Ladies` - Combined sheet for nations with <4 female athletes
- `Summary` - Aggregated totals by nation and gender

#### 5. Dynamic Blog Post Generation (champs_script.sh)

**Before**: Created placeholder posts with `[Position probability tables for each event]`
**After**: Dynamically generates full post content from JSON files

**How It Works**:
1. Scans `data/{sport}/drafts/champs-predictions/{year}/` for JSON files
2. Parses filenames to extract race names and nation names
3. Generates appropriate markdown sections with datatable shortcodes
4. Automatically handles varying numbers of nations per sport

**Key Patterns**:
- `men_position_probabilities_*.json` → Individual Men races
- `ladies_position_probabilities_*.json` → Individual Ladies races
- `relay_final_predictions_*.json` → Relay predictions
- `team_sprint_final_predictions_*.json` → Team Sprint predictions
- `nations_individual_*_Men.json` → Men's nation sheets
- `nations_individual_*_Ladies.json` → Ladies' nation sheets
- `nations_individual_Summary.json` → Summary sheet

#### 6. 3-Phase Normalization (All Race Types)

Updated normalization for individual, relay, and team sprint races to use consistent 3-phase approach:

**Phase 1**: Normalize with capping at 1.0 and redistribution
- Scale probabilities to target sum
- Cap any probability > 1.0
- Redistribute excess to uncapped athletes

**Phase 2**: Monotonic constraints
- Ensure: Win ≤ Podium ≤ Top5 ≤ Top10 ≤ Top30
- Adjust violations by averaging adjacent values

**Phase 3**: Re-normalize after monotonic adjustment
- Re-scale to target sums after constraint adjustments

**Target Sums**:
- Individual: Win=1, Podium=3, Top5=5, Top10=10, Top30=30
- Relay/Team Sprint: Win=1, Podium=3, Top5=5, Top10=10

### Pipeline Execution

```bash
# Step 1: Run R script to generate Excel files
cd ~/blog/daehl-e/content/post/cross-country/drafts
Rscript champs-predictions.R

# Step 2: Convert Excel to JSON and generate blog posts
cd ~/blog/daehl-e
./champs_script.sh 2026
```

### Files Modified

| File | Changes |
|------|---------|
| `champs-predictions.R` | Output dir YYYY, simplified columns, gender-split nations, 3-phase normalization |
| `champs_script.sh` | Dynamic post generation from JSON files |

### Sports to Update

These changes were made to cross-country. Other sports need similar updates:

| Sport | Status |
|-------|--------|
| Cross-Country | ✅ Complete |
| Alpine | ✅ Complete (2026-01-26) |
| Biathlon | ✅ Complete (2026-01-26) |
| Nordic Combined | ✅ Complete (2026-01-27) |
| Ski Jumping | ✅ Complete (2026-01-27) |

---

## Win-Optimized Team Selection & Nations Relay/TS Files (2026-01-25)

### Overview

Added win-optimized team selection alongside podium optimization, and created nations breakdown files for relay and team sprint events.

### Changes Made

#### 1. Win-Optimized Relay Team Selection

**New Functions Added**:
```r
# Generic function that takes threshold parameter
calculate_team_prob_for_threshold(team_athletes, relay_models, leg_importance, threshold = 3)

# Wrappers for specific optimizations
calculate_team_podium_prob()  # threshold = 3
calculate_team_win_prob()      # threshold = 1
```

**Updated `optimize_country_team()`**:
- Now tracks both `best_podium_team` and `best_win_team`
- Logs when win-optimized team differs from podium-optimized team
- Returns both optimization results

**New Output Files**:
- `relay_team_optimization_podium.xlsx` - Teams optimized for podium probability
- `relay_team_optimization_win.xlsx` - Teams optimized for win probability

#### 2. Win-Optimized Team Sprint Selection

Same pattern as relay:
```r
calculate_ts_team_prob_for_threshold(team_athletes, ts_models, leg_importance, race_technique, threshold = 3)
calculate_ts_team_podium_prob()
calculate_ts_team_win_prob()
```

**New Output Files**:
- `team_sprint_optimization_podium.xlsx`
- `team_sprint_optimization_win.xlsx`

#### 3. Nations Relay Excel Files (Podium & Win Optimized)

**Files**:
- `nations_relay_podium.xlsx` - Teams optimized for podium probability
- `nations_relay_win.xlsx` - Teams optimized for win probability

**Structure** (each file):
- One sheet per nation (e.g., "Norway Men", "Norway Ladies")
- Summary sheet with team probabilities by nation/gender

**Columns** (clean names without underscores):
- Athlete, ID, Nation, Leg
- Leg Win, Leg Podium, Leg Top5, Leg Top-10
- Team Win, Team Podium, Team Top5, Team Top-10

#### 4. Nations Team Sprint Excel Files (Podium & Win Optimized)

**Files**:
- `nations_ts_podium.xlsx` - Teams optimized for podium probability
- `nations_ts_win.xlsx` - Teams optimized for win probability

Same structure as relay but for team sprint (2 legs instead of 4).

#### 5. Clean Column Names (No Underscores)

All Excel files that go into the blog post now use clean column names:
- `Leg_Win_Prob` → `Leg Win`
- `Team_Podium_Prob` → `Team Podium`
- `Leg_Top-10` → `Leg Top-10`
- etc.

#### 6. Updated champs_script.sh

Blog post ## Nation section structure:
```markdown
### Individual
- Summary
- Men by nation
- Ladies by nation

### Relay Nations
#### Podium Optimized
- Summary
- Men by nation
- Ladies by nation
#### Win Optimized
- Summary
- Men by nation
- Ladies by nation

### Team Sprint Nations
#### Podium Optimized
...
#### Win Optimized
...
```

### Key Insight: Win vs Podium Optimization

The best team for winning may differ from the best team for podium:
- **Win optimization**: May favor putting the absolute best athlete on anchor leg (Leg 4) to maximize probability of 1st place
- **Podium optimization**: May spread strength more evenly to maximize probability of finishing in top 3

The script now logs when teams differ:
```
Best podium probability for Norway: 0.4521
Best win probability for Norway: 0.2134
  Note: Win-optimized team differs from podium-optimized team
```

### Files Modified

| File | Changes |
|------|---------|
| `champs-predictions.R` | Win optimization, separate podium/win nation files, clean column names |
| `champs_script.sh` | Podium/Win sections for Relay and Team Sprint Nations |

### New Excel Output Files

| File | Description |
|------|-------------|
| `relay_team_optimization_podium.xlsx` | Podium-optimized relay teams |
| `relay_team_optimization_win.xlsx` | Win-optimized relay teams |
| `team_sprint_optimization_podium.xlsx` | Podium-optimized team sprint teams |
| `team_sprint_optimization_win.xlsx` | Win-optimized team sprint teams |
| `nations_relay_podium.xlsx` | Nation-by-nation relay (podium optimized) |
| `nations_relay_win.xlsx` | Nation-by-nation relay (win optimized) |
| `nations_ts_podium.xlsx` | Nation-by-nation team sprint (podium optimized) |
| `nations_ts_win.xlsx` | Nation-by-nation team sprint (win optimized) |

---

## Bug Fix: Leg Probabilities Not Following Athletes in Win vs Podium Teams (2026-01-25)

### Issue

In `nations_ts_win.xlsx` and `nations_ts_podium.xlsx` (and relay equivalents), the leg probabilities (Leg Win, Leg Podium, etc.) had the same values for the same leg positions even though athletes were on different legs between the two files.

**Example**:
- `nations_ts_podium` (Norway): Leg 1: Klæbo (Leg Win: 0.261), Leg 2: Amundsen (Leg Win: 0.4344)
- `nations_ts_win` (Norway): Leg 1: Amundsen (Leg Win: 0.261), Leg 2: Klæbo (Leg Win: 0.4344)

The leg probabilities stayed with the leg positions instead of following the athletes.

### Root Cause

The `generate_ts_all_threshold_predictions()` and `generate_all_threshold_predictions()` functions only calculated predictions for ONE team arrangement (the podium-optimized team). When `format_ts_nations_data()` was called with `opt_type = "win"`, it correctly selected the win-optimized team from `optimization_results`, but the predictions came from `all_predictions` which were calculated only for the podium-optimized team arrangement.

### Fix Applied

Updated both relay and team sprint prediction generation to calculate predictions for BOTH team arrangements:

**`generate_all_threshold_predictions()` (relay)**:
```r
for (country in names(optimization_results)) {
  result <- optimization_results[[country]]
  podium_team <- result$podium_team
  win_team <- result$win_team

  # Calculate all threshold probabilities for BOTH team arrangements
  podium_predictions <- calculate_team_all_thresholds(podium_team, relay_models, leg_importance)
  win_predictions <- calculate_team_all_thresholds(win_team, relay_models, leg_importance)

  all_predictions[[country]] <- list(
    podium_team = podium_team,
    win_team = win_team,
    podium_predictions = podium_predictions,
    win_predictions = win_predictions,
    # For backward compatibility
    team = podium_team,
    predictions = podium_predictions
  )
}
```

**`format_relay_nations_data()` and `format_ts_nations_data()`**:
```r
# Select team AND predictions based on optimization type
if (opt_type == "win") {
  team <- country_data$win_team
  predictions <- country_data$win_predictions
} else {
  team <- country_data$podium_team
  predictions <- country_data$podium_predictions
}
```

### Files Modified

| File | Lines | Changes |
|------|-------|---------|
| `champs-predictions.R` | ~2285-2311 | `generate_all_threshold_predictions()` now generates predictions for both podium and win teams |
| `champs-predictions.R` | ~2550-2594 | `format_relay_nations_data()` now uses correct predictions based on opt_type |
| `champs-predictions.R` | ~3690-3716 | `generate_ts_all_threshold_predictions()` now generates predictions for both teams |
| `champs-predictions.R` | ~3948-3992 | `format_ts_nations_data()` now uses correct predictions based on opt_type |

### Result

Now when athletes are on different legs between podium-optimized and win-optimized teams, the leg probabilities correctly reflect each athlete's predicted performance on their assigned leg.

---

## Blog Post Structure Updates (2026-01-25)

### Changes Made

#### 1. Nations Individual Column Order

Updated the column order in `nations_individual.xlsx` to be more user-friendly:

**Before**: Race, Athlete, ID, Start, Win, Podium, Top5, Top-10, Top-30
**After**: Athlete, ID, Race, Start, Win, Podium, Top5, Top-10, Top-30

**File Modified**: `champs-predictions.R` lines ~1206-1218 (`select_and_rename_cols` function)

#### 2. Nations Section Structure in Blog Post

Restructured the Nations section so that Relay and Team Sprint data appears under each nation rather than as separate top-level sections.

**Before**:
```markdown
## Nation
### Summary
### Men
#### Norway
#### Sweden
...
### Ladies
...
### Relay Nations
#### Podium Optimized
##### Men
###### Norway
...
### Team Sprint Nations
...
```

**After**:
```markdown
## Nation
### Summary
### Men
#### Norway
##### Individual
##### Relay
###### Podium Optimized
###### Win Optimized
##### Team Sprint
###### Podium Optimized
###### Win Optimized
#### Sweden
...
### Ladies
...
```

**File Modified**: `champs_script.sh` - Replaced the nation section generation logic with new structure that groups all event types under each nation.

#### 3. Calendar Individual Files

Confirmed the Calendar section already correctly uses:
- `men_position_probabilities.xlsx` (multi-sheet, one per race)
- `ladies_position_probabilities.xlsx` (multi-sheet, one per race)

The Python Excel-to-JSON converter creates one JSON file per sheet (e.g., `men_position_probabilities_10_F.json`, `men_position_probabilities_Sprint_C.json`), which the script then finds and displays.

### Files Modified

| File | Changes |
|------|---------|
| `champs-predictions.R` | Column order: Athlete, ID, Race (was Race, Athlete, ID) |
| `champs_script.sh` | Nations restructured: Relay/TS under each nation |

---

## Baseball-Reference Style Tables (2026-01-26)

### Overview

Replaced DataTables-based table styling with a custom Baseball-Reference inspired design that is more mobile-friendly and uses native CSS sticky columns.

### Changes Made

#### 1. New `datatable2.html` Shortcode

Created a new shortcode for each sport that uses:
- Native CSS `position: sticky` for frozen first column (no jQuery/DataTables)
- Full-width containers with horizontal scroll
- Scroll shadow indicators (CSS gradients) to show scrollable content
- Vanilla JavaScript for sorting, search, and pagination
- Responsive design with `-webkit-overflow-scrolling: touch`

**Files Created**:
- `layouts/shortcodes/datatable2.html` (base version)
- `layouts/shortcodes/{sport}/datatable2.html` for alpine, biathlon, cross-country, nordic-combined, skijump

#### 2. Updated Table Partials (All 5 Sports)

Updated all table partials with Baseball-Reference styling:

**Files Updated** (for each sport: alpine, biathlon, cross-country, nordic-combined, skijump):
- `layouts/partials/{sport}/all-table.html` - All-time Elo records
- `layouts/partials/{sport}/ranks-table.html` - All-time rankings
- `layouts/partials/{sport}/skier-table.html` - Individual skier race history
- `layouts/partials/{sport}/small-table.html` - Current Elo tables

**Key Styling Features**:
- Sticky first column (Rank) and second column (Skier name with link)
- Gradient header with hover effect
- Alternating row colors with hover highlight
- Sort indicators (▲/▼) on column headers
- Scroll shadow indicators on left/right edges
- Pagination with page info
- Search/filter input
- Entries per page selector (10/25/50/All)

#### 3. Removed Redundant Place Column

Fixed duplicate Rank/Place columns in `all-table.html` and `small-table.html`:

**Issue**: Both tables showed a "Rank" column (dynamically calculated) AND a "Place" column from JSON data.

**Fix**: Added `key === 'Place'` to the skip conditions when iterating over JSON keys:
```javascript
// all-table.html
if (key === 'ID' || key === 'Place' || key.endsWith('_Date')) return;

// small-table.html
if (key === 'ID' || key === 'Place') return;
```

#### 4. Updated champs_script.sh

- Changed `draft: true` to `draft: false`
- Changed all `{sport}/datatable` references to `{sport}/datatable2`

### Files Modified Summary

| File | Changes |
|------|---------|
| `layouts/shortcodes/datatable2.html` | New Baseball-Reference style shortcode |
| `layouts/shortcodes/{sport}/datatable2.html` | Sport-specific versions (5 files) |
| `layouts/partials/{sport}/all-table.html` | New styling, removed Place column (5 files) |
| `layouts/partials/{sport}/ranks-table.html` | New styling (5 files) |
| `layouts/partials/{sport}/skier-table.html` | New styling (5 files) |
| `layouts/partials/{sport}/small-table.html` | New styling, removed Place column (5 files) |
| `champs_script.sh` | draft: false, datatable2 shortcode |

---

## Fix: Position Probabilities Exceeding Start Probability (2026-01-26)

### Issue

Position probabilities (Win, Podium, Top5, etc.) could exceed the participation/start probability after normalization. For example:
- Start probability: 0.30 (30% chance of participating)
- Win probability after normalization: 0.40 (40% chance of winning)

This is logically impossible - you can't win a race you don't start.

### Root Cause

The normalization process applied scaling AFTER multiplying by start probability:

1. Position probs multiplied by start_prob: `win_prob = raw_win_prob * start_prob` ✓
2. Normalization scales up to hit target sums (e.g., win_prob sums to 1.0)
3. This scaling can push position probs ABOVE start_prob ✗

### Fix Applied

Updated Step 4 (Normalization and Monotonic Constraints) in `champs-predictions.R`:

**PHASE 2 Update** - Added start_prob as ceiling in monotonic constraints:
```r
# For each row, ensure probabilities are monotonically non-decreasing
# AND all position probabilities are capped at start_prob
for (row_i in 1:nrow(race_results)) {
  start_ceiling <- race_results$start_prob[row_i]

  probs <- c(win_prob, podium_prob, top5_prob, top10_prob, top30_prob)

  # First, cap all position probabilities at start_prob
  probs <- pmin(probs, start_ceiling)

  # Then enforce: each probability >= previous one
  for (j in 2:length(probs)) {
    if (probs[j] < probs[j-1]) {
      probs[j] <- probs[j-1]
    }
  }

  # Final cap at start_prob (in case monotonic adjustment pushed values up)
  probs <- pmin(probs, start_ceiling)

  # Update row...
}
```

**NEW PHASE 4** - Final cap after re-normalization:
```r
# PHASE 4: Final cap at start_prob (position probs can never exceed participation prob)
log_info("  Applying final start_prob ceiling...")
violations_fixed <- 0
for (row_i in 1:nrow(race_results)) {
  start_ceiling <- race_results$start_prob[row_i]
  for (col in prob_cols) {
    if (race_results[[col]][row_i] > start_ceiling) {
      race_results[[col]][row_i] <- start_ceiling
      violations_fixed <- violations_fixed + 1
    }
  }
}
if (violations_fixed > 0) {
  log_info(sprintf("    Fixed %d cases where position prob exceeded start_prob", violations_fixed))
}
```

### Constraint Chain

The monotonic constraint chain is now:
```
win <= podium <= top5 <= top10 <= top30 <= start
```

### Note on Probability Sums

After this fix, probability sums may be slightly lower than targets (e.g., win_prob sum < 1.0) if many athletes have low start probabilities. This is actually correct behavior - it represents uncertainty about participation affecting outcome distributions.

### Files Modified

| File | Lines | Changes |
|------|-------|---------|
| `champs-predictions.R` | ~1023-1059 | PHASE 2: Added start_prob ceiling to monotonic constraints |
| `champs-predictions.R` | ~1078-1095 | NEW PHASE 4: Final cap at start_prob after re-normalization |

---

## Alpine champs-predictions.R Pipeline Update (2026-01-26)

### Objective

Update Alpine `champs-predictions.R` with the same pipeline improvements made to Cross-Country, ensuring consistent output format for the championship prediction blog posts.

### Current State Analysis

**File**: `~/blog/daehl-e/content/post/alpine/drafts/champs-predictions.R` (~1300 lines after updates)

**Status: UPDATED (2026-01-26)**

**What Alpine Now Has**:
- ✅ 4-phase normalization with capping, redistribution, monotonic constraints, and start_prob ceiling
- ✅ Output directory uses `YYYY` format
- ✅ Simplified column names (Skier, Nation, Start, Win, Podium, Top5, Top-10, Top-30)
- ✅ Nations breakdown Excel file (`nations_individual.xlsx`)
- ✅ Exponential decay weighted participation probability
- ✅ Race probability calculation with 4-person quota

**Note**: Alpine is individual-only (no relay/team sprint), so relay-related changes don't apply.

### Changes Required

#### 1. Output Directory: YYYYMMDD → YYYY
**Location**: Line 870
```r
# BEFORE
champs_date <- format(Sys.Date(), "%Y%m%d")

# AFTER
champs_date <- format(Sys.Date(), "%Y")
```

#### 2. Simplified Excel Column Output
**Location**: Lines 886-912 (race sheet creation)

Change from 24+ columns to 9 clean columns:
- Skier, Nation, ID, Start, Win, Podium, Top5, Top-10, Top-30

Need to:
- Add ID column from startlist
- Add Start probability column
- Rename columns to remove underscores

#### 3. Nations Excel File (NEW)
**Add**: After race-by-race Excel creation

Create `nations_individual.xlsx` with:
- One sheet per nation with 4+ athletes (per gender)
- "Other Men" / "Other Ladies" sheets for nations with <4 athletes
- "Summary" sheet with aggregated totals
- Columns: Athlete, ID, Race, Start, Win, Podium, Top5, Top-10, Top-30

#### 4. 4-Phase Normalization with start_prob Ceiling
**Location**: Replace `normalize_position_probabilities()` function (lines 98-206)

Implement:
- **Phase 1**: Scale to target sum, cap at 100%, redistribute excess
- **Phase 2**: Monotonic constraints + cap at start_prob
- **Phase 3**: Re-normalize after constraint adjustments
- **Phase 4**: Final cap at start_prob

#### 5. Weighted Participation Probability (Exponential Decay)
**Location**: Replace `get_base_race_probability()` function (lines 932-966)

Implement exponential decay weighting:
```r
# Get races sorted by date
races_sorted <- chronos %>% filter(...) %>% arrange(Date)

# Exponential decay (alpha = 0.1)
n_races <- nrow(races_sorted)
race_weights <- exp(-0.1 * ((n_races - 1):0))

# Weighted participation
weighted_participation <- sum(participation * race_weights)
prob <- weighted_participation / sum(race_weights)
```

### Implementation Steps

1. ✅ **Step 1**: Change output directory format (YYYYMMDD → YYYY) - Line 931
2. ✅ **Step 2**: Update `get_base_race_probability()` with exponential decay - Lines 988-1037
3. ✅ **Step 3**: Replace normalization with 4-phase approach - Lines 98-206
4. ✅ **Step 4**: Update Excel column output (simplified names) - Lines 948-985
5. ✅ **Step 5**: Add nations breakdown Excel generation - Lines 1168-1297
6. ⏳ **Step 6**: Test by running the script
7. ⏳ **Step 7**: Update champs_script.sh if needed for Alpine

### Files to Modify

| File | Changes |
|------|---------|
| `~/blog/daehl-e/content/post/alpine/drafts/champs-predictions.R` | All changes above |
| `~/blog/daehl-e/champs_script.sh` | Verify Alpine section works with new output |

### Reference Implementation

Use Cross-Country `champs-predictions.R` as reference:
- 4-phase normalization: lines ~1000-1095
- Weighted participation: lines ~651-706
- Nations Excel generation: lines ~1150-1280
- Simplified columns: lines ~1108-1126

---

## Biathlon champs-predictions.R Pipeline Update (2026-01-26)

### Objective

Update Biathlon `champs-predictions.R` with the same pipeline improvements made to Cross-Country and Alpine, ensuring consistent output format for the championship prediction blog posts.

### Current State Analysis

**File**: `~/blog/daehl-e/content/post/biathlon/drafts/champs-predictions.R` (~1987 lines after updates)

**Status: UPDATED (2026-01-26)**

**What Biathlon Now Has**:
- ✅ 4-phase normalization with capping, redistribution, monotonic constraints, and start_prob ceiling
- ✅ Output directory uses `YYYY` format (2 locations updated)
- ✅ Simplified column names for individual races (Skier, Nation, Start, Win, Podium, Top5, Top-10, Top-30)
- ✅ Simplified column names for relay races (Team/Nation, Leg assignments, Start, Win, Podium, Top5, Top-10)
- ✅ Nations breakdown Excel file (`nations_individual.xlsx`)
- ✅ Exponential decay weighted participation probability
- ✅ Race probability calculation with 4-person quota

**Note**: Biathlon has individual races (Individual, Sprint, Pursuit, Mass Start) AND 4 relay types (Men, Ladies, Mixed, Single Mixed).

### Changes Made

#### 1. Output Directory: YYYYMMDD → YYYY
**Location**: 2 occurrences (used replace_all)
```r
# Create output directory (use year only since there's one championship per year)
champs_date <- format(Sys.Date(), "%Y")
dir_path <- paste0("~/blog/daehl-e/content/post/biathlon/drafts/champs-predictions/", champs_date)
```

#### 2. Exponential Decay Weighted Participation (get_base_race_probability)
**Location**: Lines ~1050-1084

Key changes:
- Calculate cutoff date as max of 5 years ago or athlete's first race
- Sort races by date within the time window
- Apply exponential decay weights: `race_weights <- exp(-0.1 * ((n_races - 1):0))`
- Calculate weighted participation rate

#### 3. 4-Phase Normalization with start_prob Ceiling
**Location**: Lines ~138-244 (normalize_position_probabilities function)

Phases implemented:
- **Phase 1**: Scale to target sum, cap at 100%, redistribute excess
- **Phase 2**: Monotonic constraints + cap at start_prob
- **Phase 3**: Re-normalize after constraint adjustments
- **Phase 4**: Final cap at start_prob

Removed separate `enforce_probability_constraints()` calls that were duplicating Phase 2 logic.

#### 4. Simplified Excel Column Output
**Individual races** (summary and position_probabilities):
- Columns: Skier, Nation, Start, Win, Podium, Top5, Top-10, Top-30

**Relay races** (4 relay types):
- Summary: Team, Nation, Start, Win, Podium, Top5, Top-10
- Race sheets: Team, Leg1, Leg2, Leg3, Leg4, Nation, Start, Win, Podium, Top5, Top-10

#### 5. Nations Breakdown Excel File
**Location**: Lines ~1802-1987 (end of main execution)

Creates `nations_individual.xlsx` with:
- Per-nation sheets for nations with 4+ athletes (per gender): "{Nation} Men", "{Nation} Ladies"
- "Other Men" / "Other Ladies" sheets for nations with <4 athletes
- "Summary" sheet with aggregated totals by nation and gender
- Columns: Athlete, Race, Start, Win, Podium, Top5, Top-10, Top-30 (plus Nation in Other sheets)

**Special handling**: Code checks if individual results exist before generating (some sections commented out in main execution).

### Implementation Steps Completed

1. ✅ **Step 1**: Change output directory format (YYYYMMDD → YYYY) - 2 locations with replace_all
2. ✅ **Step 2**: Update `get_base_race_probability()` with exponential decay
3. ✅ **Step 3**: Replace normalization with 4-phase approach (including start_prob ceiling)
4. ✅ **Step 4**: Update Excel column output with simplified names (individual + relay)
5. ✅ **Step 5**: Add nations breakdown Excel generation
6. ⏳ **Step 6**: Test by running the script
7. ✅ **Step 7**: Update next-prompts.md with completion status

### Files Modified

| File | Changes |
|------|---------|
| `~/blog/daehl-e/content/post/biathlon/drafts/champs-predictions.R` | All changes above |
| `~/blog/daehl-e/next-prompts.md` | Marked Biathlon as complete |

---

## Nordic Combined champs-predictions.R Pipeline Update (2026-01-27)

### Objective

Update Nordic Combined `champs-predictions.R` with the same pipeline improvements made to Cross-Country, Alpine, and Biathlon, ensuring consistent output format for the championship prediction blog posts.

### Current State Analysis

**File**: `~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions.R` (~1800 lines after updates)

**Status: UPDATED (2026-01-27)**

**What Nordic Combined Now Has**:
- ✅ 4-phase normalization with capping, redistribution, monotonic constraints, and start_prob ceiling
- ✅ Output directory uses `YYYY` format (2 locations updated)
- ✅ Simplified column names for individual races (Skier, Nation, Start, Win, Podium, Top5, Top-10, Top-30)
- ✅ Simplified column names for team races (Nation, Team, Start, Win, Podium, Top5, Top-10)
- ✅ TeamMembers column in team output
- ✅ Nations breakdown Excel file (`nations_individual.xlsx`)
- ✅ Exponential decay weighted participation probability

**Note**: Nordic Combined has individual races (commented out in main execution) AND team events (men, ladies, mixed). Uses 3-person quota per nation (not 4).

### Changes Made

#### 1. Output Directory: YYYYMMDD → YYYY
**Location**: 2 occurrences (used replace_all)
```r
# Create output directory (use year only since there's one championship per year)
champs_date <- format(Sys.Date(), "%Y")
dir_path <- paste0("~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions/", champs_date)
```

#### 2. Exponential Decay Weighted Participation (get_base_race_probability)

Key changes:
- Calculate cutoff date as max of 5 years ago or athlete's first race
- Sort races by date within the time window
- Apply exponential decay weights: `race_weights <- exp(-0.1 * ((n_races - 1):0))`
- Calculate weighted participation rate

#### 3. 4-Phase Normalization with start_prob Ceiling (normalize_position_probabilities)

Phases implemented:
- **Phase 1**: Scale to target sum, cap at 100%, redistribute excess
- **Phase 2**: Monotonic constraints + cap at start_prob (with NA handling)
- **Phase 3**: Re-normalize after constraint adjustments
- **Phase 4**: Final cap at start_prob

#### 4. Simplified Excel Column Output
**Individual races**:
- Columns: Skier, Nation, Start, Win, Podium, Top5, Top-10, Top-30

**Team races**:
- Columns: Nation, Team, Start, Win, Podium, Top5, Top-10

#### 5. TeamMembers Column in Team Output
Added TeamMembers from startlist to position_preds, renamed to "Team" in output.

#### 6. Nations Breakdown Excel File
Creates `nations_individual.xlsx` with:
- Per-nation sheets for nations with 3+ athletes (per gender)
- "Other Men" / "Other Ladies" sheets for nations with <3 athletes
- "Summary" sheet with aggregated totals by nation and gender
- **Note**: Uses 3-athlete threshold (Nordic Combined quota) instead of 4

### Implementation Steps Completed

1. ✅ **Step 1**: Change output directory format (YYYYMMDD → YYYY) - 2 locations with replace_all
2. ✅ **Step 2**: Update `get_base_race_probability()` with exponential decay
3. ✅ **Step 3**: Replace normalization with 4-phase approach (including start_prob ceiling)
4. ✅ **Step 4**: Update Excel column output with simplified names (individual + team)
5. ✅ **Step 5**: Add TeamMembers column to team output
6. ✅ **Step 6**: Add nations breakdown Excel generation
7. ⏳ **Step 7**: Test by running the script
8. ✅ **Step 8**: Update next-prompts.md with completion status

### Files Modified

| File | Changes |
|------|---------|
| `~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions.R` | All changes above |
| `~/blog/daehl-e/next-prompts.md` | Marked Nordic Combined as complete |

---

## Team Sprint vs Team Separation (2026-01-27)

### Overview

Nordic Combined can have both Team Sprint (2-person teams) and regular Team (4-person teams) events at championships. Updated all three codebases to handle these as separate event types.

### Changes Made

#### 1. Python Startlist Scraper (`startlist-scrape-champs.py`)

**File**: `~/ski/elo/python/nordic-combined/polars/startlist-scrape-champs.py`

Separated Team Sprint from regular Team races:
```python
# Separate Team Sprint (2-person) from regular Team (4-person) events
team_sprint_races = team_races[team_races['RaceType'].str.contains('Sprint', case=False, na=False)]
regular_team_races = team_races[~team_races['RaceType'].str.contains('Sprint', case=False, na=False)]
```

Fixed qualifying threshold bug (was hardcoded to 3):
```python
# Determine minimum athletes needed based on race type
if 'Sprint' in race_type:
    min_athletes = 2  # Team Sprint needs 2 athletes
else:
    min_athletes = 4  # Regular Team needs 4 athletes

qualifying_nations = []
for nation in all_nations:
    athletes = get_champs_athletes(nation, gender)
    if len(athletes) >= min_athletes:
        qualifying_nations.append(nation)
```

**Output Files**:
- `startlist_champs_men_team_sprint.csv` - 2-person team sprint startlist
- `startlist_champs_men_teams.csv` - 4-person regular team startlist
- (Same pattern for ladies and mixed)

#### 2. R Prediction Script (`champs-predictions.R`)

**File**: `~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions.R`

Race filtering separation:
```r
# Create race dataframes for Team Sprint (2-person teams)
men_team_sprint <- champs_races_with_race_num %>%
  filter(grepl("Team Sprint", RaceType, ignore.case = TRUE) & Sex == "M")

# Create race dataframes for regular Team events (4-person teams)
# Team but NOT Team Sprint
men_teams <- champs_races_with_race_num %>%
  filter(grepl("Team", RaceType, ignore.case = TRUE) &
         !grepl("Sprint", RaceType, ignore.case = TRUE) &
         Sex == "M")
```

Updated function signature:
```r
process_team_championships <- function(gender, races, event_type = "teams")
```

Output file naming:
```r
summary_file <- file.path(dir_path, paste0(gender, "_", event_type, ".xlsx"))
race_file <- file.path(dir_path, paste0(gender, "_", event_type, "_position_probabilities.xlsx"))
```

Main execution calls both:
```r
# Process Team Sprint Championships (2-person teams)
if(nrow(men_team_sprint) > 0) {
  men_team_sprint_results <- process_team_championships("men", men_team_sprint, "team_sprint")
}

# Process regular Team Championships (4-person teams)
if(nrow(men_teams) > 0) {
  men_team_results <- process_team_championships("men", men_teams, "teams")
}
```

#### 3. Shell Script (`champs_script.sh`)

**File**: `~/blog/daehl-e/champs_script.sh`

Added separate Team Sprint and Team sections:
```bash
# Add Team Sprint section if files exist (2-person teams)
men_ts=$(find "$json_dir" -name "team_sprint_final_predictions_Men_*.json" -type f 2>/dev/null | head -1)
# Nordic Combined style naming for team sprint
if [[ -z "$men_ts" ]]; then
    men_ts=$(find "$json_dir" -name "men_team_sprint_position_probabilities*.json" -type f 2>/dev/null | head -1)
fi

# Add regular Team section if files exist (4-person teams)
men_team=$(find "$json_dir" -name "men_teams_position_probabilities*.json" -type f 2>/dev/null | head -1)
```

### Bug Fixed

**Issue**: USA (with 2 male athletes configured) wasn't appearing in Team Sprint startlist

**Root Cause**: The qualifying threshold was hardcoded to `>= 3` athletes regardless of event type

**Fix**: Made minimum athletes dynamic based on race type:
- Team Sprint: 2 athletes minimum
- Regular Team: 4 athletes minimum
- Mixed Team Sprint: 1 per gender minimum
- Regular Mixed Team: 2 per gender minimum

### Files Modified

| File | Changes |
|------|---------|
| `~/ski/elo/python/nordic-combined/polars/startlist-scrape-champs.py` | Separate Team Sprint from Team, dynamic qualifying threshold |
| `~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions.R` | Separate race filtering, event_type parameter, output file naming |
| `~/blog/daehl-e/champs_script.sh` | Separate Team Sprint and Team sections in blog post |

### Testing Status

⏳ **Pending**: Run full pipeline to verify Team Sprint vs Team separation works correctly:
1. `python startlist-scrape-champs.py` (generate startlists)
2. `Rscript champs-predictions.R` (generate predictions)
3. `./champs_script.sh 2026` (generate blog post)

---

## Duplicate Race Type Sheet Naming Fix (2026-01-27)

### Issue

When a championship has multiple races of the same type (e.g., two "Individual" races), the R script was creating sheets with identical names ("Men Individual"), causing the second sheet to overwrite the first. This resulted in only one sheet in the Excel file and no individual race tables appearing in the blog post.

### Root Cause

The sheet naming logic used only the race type without accounting for duplicates:
```r
sheet_name <- paste(ifelse(gender == "men", "Men", "Ladies"), race_type)
```

When both races had `RaceType = "Individual"`, both sheets were named "Men Individual".

### Fix Applied

**File**: `~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions.R`

Added a counter to track duplicate race types and append a number suffix when needed:

```r
# Track race type counts to handle duplicates (e.g., two "Individual" races)
race_type_counts <- list()

for(race_num in unique_races) {
  # ... existing code ...

  # Track how many times we've seen this race type to handle duplicates
  race_type_key <- paste(gender_prefix, race_type)
  if (is.null(race_type_counts[[race_type_key]])) {
    race_type_counts[[race_type_key]] <- 1
  } else {
    race_type_counts[[race_type_key]] <- race_type_counts[[race_type_key]] + 1
  }

  # Add number suffix if this race type appears multiple times
  total_of_type <- sum(champs_races_with_race_num$Sex == ifelse(gender == "men", "M", "L") &
                       champs_races_with_race_num$RaceType == race_type)

  if (total_of_type > 1) {
    sheet_name <- paste(gender_prefix, race_type, race_type_counts[[race_type_key]])
  } else {
    sheet_name <- paste(gender_prefix, race_type)
  }
}
```

### Result

- Two "Individual" races → "Men Individual 1", "Men Individual 2"
- Single "Sprint" race → "Men Sprint" (no suffix needed)
- JSON files created: `men_position_probabilities_Men_Individual_1.json`, `men_position_probabilities_Men_Individual_2.json`

### Sports That May Need This Fix

| Sport | Status | Notes |
|-------|--------|-------|
| Nordic Combined | ✅ Fixed | Has two Individual races |
| Ski Jumping | ✅ Fixed | Applied same fix |
| Cross-Country | ⏳ Check | Likely has unique race types |
| Alpine | ⏳ Check | Multiple DH/SG/GS/SL races possible |
| Biathlon | ⏳ Check | Multiple Sprint/Individual races possible |

---

## Ski Jumping champs-predictions.R Pipeline Update (2026-01-27)

### Objective

Update Ski Jumping `champs-predictions.R` with the same pipeline improvements made to Cross-Country, Alpine, Biathlon, and Nordic Combined, ensuring consistent output format for the championship prediction blog posts.

### Current State Analysis

**File**: `~/blog/daehl-e/content/post/skijump/drafts/champs-predictions.R` (~1900 lines after updates)

**Status: UPDATED (2026-01-27)**

**What Ski Jumping Now Has**:
- ✅ 4-phase normalization with capping, redistribution, monotonic constraints, and start_prob ceiling
- ✅ Output directory uses `YYYY` format (2 locations updated)
- ✅ Simplified column names for individual races (Skier, Nation, Start, Win, Podium, Top5, Top-10, Top-30)
- ✅ Nations breakdown Excel file (`nations_individual.xlsx`)
- ✅ Exponential decay weighted participation probability
- ✅ Duplicate race type sheet naming fix (handles multiple races of same type)

**Note**: Ski Jumping has individual races AND team events (men, ladies, mixed).

### Changes Made

#### 1. Output Directory: YYYYMMDD → YYYY
**Location**: 2 occurrences (used replace_all)
```r
# Create output directory (use year only since there's one championship per year)
champs_date <- format(Sys.Date(), "%Y")
dir_path <- paste0("~/blog/daehl-e/content/post/skijump/drafts/champs-predictions/", champs_date)
```

#### 2. Exponential Decay Weighted Participation (get_base_race_probability)

Key changes:
- Calculate cutoff date as max of 5 years ago or athlete's first race
- Sort races by date within the time window
- Apply exponential decay weights: `race_weights <- exp(-0.1 * ((n_races - 1):0))`
- Calculate weighted participation rate

#### 3. 4-Phase Normalization with start_prob Ceiling (normalize_position_probabilities)

Phases implemented:
- **Phase 1**: Scale to target sum, cap at 100%, redistribute excess
- **Phase 2**: Monotonic constraints + cap at start_prob
- **Phase 3**: Re-normalize after constraint adjustments
- **Phase 4**: Final cap at start_prob

#### 4. Simplified Excel Column Output
**Individual races**:
- Columns: Skier, Nation, Start, Win, Podium, Top5, Top-10, Top-30

#### 5. Duplicate Race Type Sheet Naming Fix
Added counter to track duplicate race types and append number suffix when needed.

#### 6. Nations Breakdown Excel File
Creates `nations_individual.xlsx` with:
- Per-nation sheets for nations with 4+ athletes (per gender)
- "Other Men" / "Other Ladies" sheets for nations with <4 athletes
- "Summary" sheet with aggregated totals by nation and gender

### Implementation Steps Completed

1. ✅ **Step 1**: Change output directory format (YYYYMMDD → YYYY) - 2 locations with replace_all
2. ✅ **Step 2**: Update `get_base_race_probability()` with exponential decay
3. ✅ **Step 3**: Replace normalization with 4-phase approach (including start_prob ceiling)
4. ✅ **Step 4**: Update Excel column output with simplified names
5. ✅ **Step 5**: Add duplicate race type sheet naming fix
6. ✅ **Step 6**: Add nations breakdown Excel generation
7. ⏳ **Step 7**: Test by running the script

### Files Modified

| File | Changes |
|------|---------|
| `~/blog/daehl-e/content/post/skijump/drafts/champs-predictions.R` | All changes above |
| `~/blog/daehl-e/next-prompts.md` | Marked Ski Jumping as complete |

---

## All Sports Pipeline Updates Complete (2026-01-27)

All five winter sports have been updated with the championship prediction pipeline improvements:

| Sport | Output Dir | Exp Decay | 4-Phase Norm | Simplified Cols | Nations Excel | Dup Race Fix |
|-------|------------|-----------|--------------|-----------------|---------------|--------------|
| Alpine | ✅ YYYY | ✅ | ✅ | ✅ | ✅ | ⏳ Check |
| Biathlon | ✅ YYYY | ✅ | ✅ | ✅ | ✅ | ⏳ Check |
| Cross-Country | ✅ YYYY | ✅ | ✅ | ✅ | ✅ | ⏳ Check |
| Nordic Combined | ✅ YYYY | ✅ | ✅ | ✅ | ✅ | ✅ |
| Ski Jumping | ✅ YYYY | ✅ | ✅ | ✅ | ✅ | ✅ |

### Next Steps

1. Test all sports by running their R scripts
2. Run `champs_script.sh 2026` to generate blog posts
3. Verify blog post tables display correctly
4. Apply duplicate race type fix to other sports if needed

---