# Winter Sports Prediction System

## Current Status (2026-02-27)

**IMPORTANT**: Update this file (`next-prompts.md`) whenever changes are made to the codebase. This ensures session continuity and accurate status tracking.

### Pipeline
```
Python Scraper → R Predictions → Excel → JSON → Hugo Blog Post
```

### Sport-by-Sport Status

| Sport | champs-predictions | race-picks | Status |
|-------|-------------------|------------|--------|
| Alpine | Ready | Ready | Production |
| Biathlon | Ready | Ready | Production |
| Cross-Country | Ready (Simulation) | **Ready** | `race-picks-simulation.R` with adjustments |
| Nordic Combined | Ready | Ready | Production |
| Ski Jumping | Ready | Ready | Production |

### Active Work: Simulation Scripts - ALL COMPLETE + PIPELINE INTEGRATED
- [x] `race-picks-simulation.R` - Individual/Relay/TS/Mixed (with condition adjustments)
- [x] `weekly-picks-simulation.R` - Fantasy predictions with knapsack optimization (DONE 2026-02-27)
- [x] `tds-picks-simulation.R` - Tour de Ski overall (DONE 2026-02-27)
- [x] `final_climb-simulation.R` - Final Climb race-day (DONE 2026-02-27)
- [x] **Pipeline Integration** - Added to predict_script.sh (DONE 2026-02-28)

### Pipeline Integration (2026-02-28)

**Files Modified:**
- `predict_script.sh` - Added R script calls for cross-country
- `predict_script_ubuntu.sh` - Added R script calls for cross-country

**Output File Names (Production Conventions):**
```
race-picks/YYYYMMDD/
├── men_position_probabilities.xlsx
├── ladies_position_probabilities.xlsx
├── men_relay_position_probabilities.xlsx
├── ladies_relay_position_probabilities.xlsx
├── men_team_sprint_position_probabilities.xlsx
├── ladies_team_sprint_position_probabilities.xlsx
├── mixed_relay_position_probabilities.xlsx
├── fantasy_team.xlsx
└── fantasy_position_probabilities.xlsx
```

**When Scripts Run:**
- `weekly-picks-simulation.R` - Runs during regular weekend events
- `race-picks-simulation.R` - Runs during race events (races.csv match)
- `tds-picks-simulation.R` - Runs on first day of Tour de Ski (Period 2)
- `final_climb-simulation.R` - Runs on Final Climb day (Val Di Fiemme + Distance + Freestyle)
- Fantasy files processed during weekend flow (even if no races in races.csv)

**All Simulation Scripts Now Integrated** ✓

---

## Simulation Script: Completed Features

### Hybrid Approach (Production Models + Simulation)
Combines the best of both approaches:
- **From Production**: Sophisticated leg-specific GAM models predicting team podium probability
- **From Simulation**: Natural field-size handling, no normalization artifacts

**How it works:**
1. Train leg-specific binomial GAMs predicting `is_podium = (Place <= 3)`
2. For each team member, predict P(team podium | athlete on this leg)
3. Convert weighted probabilities to logit scores for simulation
4. Monte Carlo simulation ranks teams naturally

### Variance Control Parameters (Calibrated 2026-02-05)
```r
# Individual races
DECAY_LAMBDA <- 0.002     # Exponential decay rate (0.002 = 50% weight after 1 year)
SD_SCALE_FACTOR <- 0.77   # Multiply all SDs (lower = favorites win more)
SD_MIN <- 4               # Minimum SD
SD_MAX <- 16              # Maximum SD

# Relay (4 legs)
RELAY_SCORE_SD_MIN <- 0.5
RELAY_SCORE_SD_MAX <- 1.15

# Team Sprint (2 legs)
TS_SCORE_SD_MIN <- 0.45
TS_SCORE_SD_MAX <- 0.8
```

### Exponential Decay for Historical Weighting
- Uses date-based exponential decay instead of race-count weighting
- Formula: `weight = exp(-lambda * days_ago)`
- DECAY_LAMBDA = 0.002 means 50% weight after ~1 year
- Higher lambda = faster decay, more emphasis on recent races
- Applied to:
  - Individual race prev_points_weighted calculation
  - Relay prev_points_weighted (technique-specific: classic for legs 1-2, freestyle for legs 3-4)
  - Team sprint prev_points_weighted (technique-specific: C or F based on race)
- Added to individual race calibration grid search

### Technique-Specific Team Sprint (2026-02-06)
Team sprint now uses technique-specific features and models (matching champs-predictions.R):
- **Classic team sprint (C)**: Uses `Sprint_C_Pelo_pct`, `Classic_Pelo_pct`, `Distance_C_Pelo_pct`
- **Freestyle team sprint (F)**: Uses `Sprint_F_Pelo_pct`, `Freestyle_Pelo_pct`, `Distance_F_Pelo_pct`
- Separate models trained for each technique at the championship
- `prev_points_weighted` uses technique-specific sprint history

### Podium-Optimized Team Selection
- Team selection optimizes for podium probability (threshold=3)
- Matches champs-predictions.R approach (no separate win-optimized team)
- Uses leg-specific binomial models to predict P(team podium | athlete on leg X)

### Calibration System
Three independent calibration processes using Brier score on historical data (2018+):

1. **Individual Race Calibration** (`RUN_CALIBRATION <- TRUE`)
   - Grid search over DECAY_LAMBDA, SD_SCALE_FACTOR, SD_MIN, SD_MAX
   - Tests predictions against actual race results

2. **Relay Calibration** (`RUN_RELAY_CALIBRATION <- TRUE`)
   - Grid search over RELAY_SCORE_SD_MIN, RELAY_SCORE_SD_MAX
   - Uses 4-leg relay events

3. **Team Sprint Calibration** (`RUN_TEAM_SPRINT_CALIBRATION <- TRUE`)
   - Grid search over TS_SCORE_SD_MIN, TS_SCORE_SD_MAX
   - Uses 2-leg team sprint events

**Usage:** Set the appropriate flag to TRUE, run the script, copy recommended values to the config section, set flag back to FALSE.

---

## Key File Locations

### R Prediction Scripts
```
~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions.R            # Production
~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions-simulation.R  # Simulation
```

### Excel Outputs
```
~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions/2026/
~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions-simulation/2026/
```

---

## Technical Reference

### Key Code Sections (champs-predictions-simulation.R)
- **Lines 27-50**: Configuration parameters (variance control, decay lambda, calibration flags)
- **Lines 189-228**: `get_relay_explanatory_vars()` - technique-specific feature selection
- **Lines 232-430**: `calculate_leg_importance_from_models()` - model deviance approach
- **Lines 967-1177**: `train_relay_leg_models_for_simulation()` - leg-specific GAMs with technique support
- **Lines 2034-2055**: `get_race_prev_points()` - technique-specific prev_points
- **Lines 2213-2290**: `calculate_team_points()` - technique-aware team scoring
- **Lines 2300-2405**: `select_relay_team()` - podium-optimized team selection with technique
- **Lines 2410-2550**: `build_team_distribution_hybrid()` - hybrid scoring with technique

### Feature Selection (matches champs-predictions.R)

**4-Leg Relay:**
- Leg 1: `prev_points_weighted, Pelo_pct, Distance_Pelo_pct, Distance_C_Pelo_pct, Sprint_Pelo_pct, Sprint_C_Pelo_pct, Classic_Pelo_pct`
- Leg 2: `prev_points_weighted, Pelo_pct, Distance_Pelo_pct, Distance_C_Pelo_pct, Classic_Pelo_pct`
- Leg 3: `prev_points_weighted, Pelo_pct, Distance_Pelo_pct, Distance_F_Pelo_pct, Freestyle_Pelo_pct`
- Leg 4: `prev_points_weighted, Pelo_pct, Distance_Pelo_pct, Distance_F_Pelo_pct, Sprint_Pelo_pct, Sprint_F_Pelo_pct, Freestyle_Pelo_pct`

**Team Sprint (technique-specific):**
- Classic (C): `prev_points_weighted, Pelo_pct, Sprint_Pelo_pct, Sprint_C_Pelo_pct, Classic_Pelo_pct, Distance_Pelo_pct, Distance_C_Pelo_pct`
- Freestyle (F): `prev_points_weighted, Pelo_pct, Sprint_Pelo_pct, Sprint_F_Pelo_pct, Freestyle_Pelo_pct, Distance_Pelo_pct, Distance_F_Pelo_pct`

### Leg Importance Calculation
- Matches production `champs-predictions.R` methodology
- Trains leg-specific binomial GAMs predicting team podium (Place <= 3)
- Importance = deviance explained by each leg's model
- Higher deviance = athlete quality on that leg better predicts team success

### Tuning Tips
If top athletes' win probabilities seem too low:
- **Individual races**: Lower SD_SCALE_FACTOR (e.g., 0.77 → 0.6)
- **Relay/Team Sprint**: Lower SCORE_SD_MAX (e.g., 1.0 → 0.7)

If predictions are too deterministic (favorites always win):
- Increase the relevant SD parameters

---

## Running the Pipeline

### Production Predictions
```bash
cd ~/blog/daehl-e/content/post/{sport}/drafts
Rscript champs-predictions.R

cd ~/blog/daehl-e
./champs_script.sh 2026
```

### Simulation Predictions
```bash
cd ~/blog/daehl-e/content/post/cross-country/drafts
Rscript champs-predictions-simulation.R
```

### Running Calibration
```bash
# Edit champs-predictions-simulation.R, set ONE of:
# RUN_CALIBRATION <- TRUE
# RUN_RELAY_CALIBRATION <- TRUE
# RUN_TEAM_SPRINT_CALIBRATION <- TRUE

Rscript champs-predictions-simulation.R

# Script will output recommended values and stop
# Copy values to config section, set flag back to FALSE
```

---

## Recent Changes (2026-02-06)

### Technique-Specific Team Sprint
- Updated `get_relay_explanatory_vars()` to accept `technique` parameter for team sprint
- Updated `train_relay_leg_models_for_simulation()` to train separate models per technique
- Team sprint models now stored as `men_ts_leg_models[["C"]]` and `men_ts_leg_models[["F"]]`
- Updated `calculate_team_points()` to use technique-specific `prev_points_weighted`
- Updated `select_relay_team()` to accept technique parameter
- Updated `build_team_distribution_hybrid()` to accept technique parameter
- Updated `get_race_prev_points()` to handle "Sprint", "Sprint_C", "Sprint_F" race types

### Dual-Optimized Team Selection (Updated 2026-02-06)
- Team selection now optimizes for both podium (threshold=3) and win (threshold=1)
- Matches champs-predictions.R approach with both optimization types
- Separate leg-specific models trained for podium and win outcomes
- Outputs both `nations_relay_podium.xlsx`/`nations_ts_podium.xlsx` and `nations_relay_win.xlsx`/`nations_ts_win.xlsx`

---

## COMPLETED: Simulation Moved to Production (2026-02-06)

### Summary
The cross-country simulation script (`champs-predictions-simulation.R`) has been fully integrated into the production pipeline. All tasks completed:

### Completed Tasks

#### Task 1: Positive Coefficient Constraint ✓
Added `filter_positive_coefficients()` helper function that iteratively removes features with negative coefficients. Applied to:
- `train_points_gam()` for individual race models
- `train_relay_leg_models_for_simulation()` for relay/team sprint leg models
- `calculate_leg_importance_from_models()` for leg importance calculation

#### Task 2: Excel Output Format ✓
Updated Excel output to match production format:
- `relay_final_predictions.xlsx` with sheets "Men All Thresholds Final", "Ladies All Thresholds Final"
- `team_sprint_final_predictions.xlsx` with same sheet structure
- `nations_relay_podium.xlsx` with per-nation sheets "{Nation} Men", "{Nation} Ladies"
- `nations_ts_podium.xlsx` with same per-nation structure
- `nations_relay_win.xlsx` - win-optimized team rosters (added 2026-02-06)
- `nations_ts_win.xlsx` - win-optimized team rosters (added 2026-02-06)

Columns: Country, Leg, Athlete, Nation, ID, Leg Win, Leg Podium, Leg Top5, Leg Top-10, Team Win, Team Podium, Team Top5, Team Top-10

#### Win-Optimized Team Selection (Added 2026-02-06)
- `train_relay_leg_models_for_simulation()` now trains both `podium_model` and `win_model` for each leg
- `calculate_team_points()` accepts `opt_type` parameter ("podium" or "win") to use appropriate model
- `select_relay_team()` returns both `podium_team` and `win_team` with their respective leg probabilities
- Team distributions store both team rosters for Excel output

#### Task 3: Output Directory ✓
Changed output from `champs-predictions-simulation/` to `champs-predictions/`

#### Task 4: Pipeline Integration ✓
Updated `run_champs_predictions.sh` to run `champs-predictions-simulation.R` for cross-country specifically (other sports still use `champs-predictions.R`)

#### Task 5: Methodology Documentation ✓
Updated `~/blog/daehl-e/content/post/methods/champs-predictions.md` with:
- Monte Carlo simulation approach explanation
- Exponential decay for historical weighting
- Positive coefficient constraint in feature selection
- Hybrid approach for relay/team sprint
- Technique-specific team sprint models
- Calibration system documentation

### Running the Pipeline

```bash
# Run all sports predictions (cross-country uses simulation)
cd ~/blog/daehl-e
./run_champs_predictions.sh

# Convert Excel to JSON and generate blog posts
./champs_script.sh 2026
```

### Files Modified
- `content/post/cross-country/drafts/champs-predictions-simulation.R` - Production-ready simulation script
- `run_champs_predictions.sh` - Updated to run simulation for cross-country
- `content/post/methods/champs-predictions.md` - Updated methodology documentation

---

## Recent Changes (2026-02-11)

### race-picks.R Bug Fix
Fixed error `object 'race_prob_col' not found` in `normalize_position_probabilities`:
- Added `race_prob_col = NULL` as optional parameter to function (line 556)
- Updated function call at line 1433 to pass `race_prob_col`
- Updated condition at line 652 to check `!is.null(race_prob_col)` before use

### weekly-picks2.R Changes (Cross-Country)

#### Fantasy Output Simplified (lines 1097-1141)
- **Old method** (commented out): MIP optimization with budget constraints (100,000 budget, price-based selection)
- **New method**: Simply takes top 8 men + top 8 ladies by `Total_Points`, ignoring prices
- The `optimize_weekly_team()` function is preserved for future use

#### Race Probability Logic Updated (lines 326-340, 391-400)
Priority system for race probability:
1. **If FIS startlist exists** (any `In_FIS_List == TRUE`): Use FIS list (athletes with `In_FIS_List == TRUE` get prob=1)
2. **If NO FIS startlist** (all `In_FIS_List == FALSE`): Athletes with `In_Config == TRUE` get prob=1

Old logic is preserved as comments for when the fantasy game changes back.

### daily_cleanup.sh Review
Current script only does git gc and removes `.log` files older than 7 days. Potential future improvements discussed:
- Clean up `*pred*.csv` files older than 7 days in `~/ski/elo`
- Clean up old dated `race-picks/` directories older than 30 days
- Clean up R/Python temp files (`.Rhistory`, `__pycache__`)
- Ubuntu system cleanup (`apt autoremove`, `journalctl --vacuum-time`)

Not implemented yet - revisit if disk space becomes an issue again.

---

## COMPLETED: Centralized .env Configuration (2026-02-27)

### Goal
Create a single `.env` file to toggle TEST_MODE across the entire pipeline:
```
Python Scraper → Startlist Generation → R Predictions → Excel Output
```

### Implementation

#### 1. Created `.env` file at `~/ski/elo/.env`
```bash
# Winter Sports Prediction Pipeline Configuration
TEST_MODE=false
```

#### 2. Created Python config module at `~/ski/elo/python/pipeline_config.py`
```python
from pipeline_config import TEST_MODE, get_races_file, get_weekends_file

# Check mode
if TEST_MODE:
    print("Running in TEST mode")

# Get file paths (automatically switches to test files when TEST_MODE=true)
races_file = get_races_file("ski")      # → races.csv or test_races.csv
weekends_file = get_weekends_file("ski") # → weekends.csv or test_weekends.csv
```

#### 3. Updated Python startlist scripts
Updated to import from `pipeline_config`:
- `~/ski/elo/python/ski/polars/startlist-scrape-weekend.py`
- `~/ski/elo/python/ski/polars/startlist-scrape-races.py`
- `~/ski/elo/python/ski/polars/relay/startlist_scrape_weekend_relay.py`
- (Pattern for other sports/relay scripts)

#### 4. Updated R scripts to read from `.env`
All cross-country R scripts now load TEST_MODE from .env:
```r
# ===== TEST MODE (loaded from .env) =====
load_env <- function(env_path = "~/ski/elo/.env") {
  env_file <- path.expand(env_path)
  if (file.exists(env_file)) {
    lines <- readLines(env_file, warn = FALSE)
    for (line in lines) {
      line <- trimws(line)
      if (nchar(line) > 0 && !startsWith(line, "#") && grepl("=", line)) {
        parts <- strsplit(line, "=", fixed = TRUE)[[1]]
        key <- trimws(parts[1])
        value <- trimws(paste(parts[-1], collapse = "="))
        value <- gsub("^[\"']|[\"']$", "", value)
        do.call(Sys.setenv, setNames(list(value), key))  # Dynamically set env var
      }
    }
  }
}
load_env()
TEST_MODE <- tolower(Sys.getenv("TEST_MODE", "false")) == "true"
```

**Important**: Must use `do.call(Sys.setenv, setNames(list(value), key))` instead of `Sys.setenv(key = value)` - the latter literally creates an env var named "key" instead of using the variable's value.

**Updated R scripts:**
- `race-picks.R`
- `race-picks-relay.R`
- `race-picks-team-sprint.R`
- `race-picks-mixed-relay.R`
- `race-picks-simulation.R`
- `weekly-picks2.R`
- `weekly-picks-relay.R`
- `weekly-picks-team-sprint.R`
- `weekly-picks-mixed-relay.R`

### Usage
To switch to test mode:
```bash
# Edit ~/ski/elo/.env
TEST_MODE=true

# Now all scripts will use test_races.csv and test_weekends.csv
```

To switch back to production:
```bash
TEST_MODE=false
```

### Files Created/Modified
- **Created**: `~/ski/elo/.env`
- **Created**: `~/ski/elo/python/pipeline_config.py`
- **Modified**: 3 Python startlist scripts (cross-country)
- **Modified**: 9 R prediction scripts (cross-country)

### Extending to Other Sports
Apply the same pattern to other sports by updating their Python scripts to:
1. Add `sys.path.insert(0, os.path.expanduser('~/ski/elo/python'))`
2. `from pipeline_config import TEST_MODE, get_races_file, get_weekends_file`
3. Replace hardcoded paths with `get_races_file('sport_name')` or `get_weekends_file('sport_name')`

---

## Session Resume Instructions

If starting a new session:
1. Read this file to understand current status
2. **Current task**: Simulation scripts are integrated into the pipeline
3. **Completed**: All simulation scripts + pipeline integration
4. **Update this file** with any changes made

### Current Session Status (2026-03-01)

**Completed this session (Biathlon Simulation Scripts)**:

1. **biathlon/drafts/race-picks-simulation.R** (~565 lines):
   - Monte Carlo simulation for Biathlon individual races
   - Race type-specific distributions (Sprint, Individual, Pursuit, Mass Start)
   - Uses biathlon points systems (regular_points, mass_start_points)
   - Filters out relay races (Relay, Mixed Relay, Single Mixed Relay)
   - Output: `men_position_probabilities.xlsx`, `ladies_position_probabilities.xlsx`

2. **biathlon/drafts/champs-predictions-simulation.R** (~725 lines):
   - Monte Carlo simulation for World Championships
   - Race start probability calculation per athlete/race type
   - 4-person quota constraint per nation
   - Nations Excel file with per-nation sheets
   - Output: `men_position_probabilities.xlsx`, `ladies_position_probabilities.xlsx`, `men.xlsx`, `ladies.xlsx`, `nations_individual.xlsx`

3. **Pipeline Integration**:
   - `predict_script.sh` - Added biathlon race-picks-simulation.R call (lines 421-432)
   - `run_champs_predictions.sh` - Updated to use champs-predictions-simulation.R for biathlon (line 93)

**Next up: Nordic Combined and Ski Jumping simulation scripts**

---

### Previous Session Status (2026-02-28)

**Completed this session (Pipeline Integration + Output Format Fixes)**:

1. **race-picks-simulation.R** - Updated output:
   - Output directory: `race-picks/YYYYMMDD` (dated folder)
   - File names: `men_position_probabilities.xlsx`, `ladies_position_probabilities.xlsx`
   - Team events: `men_relay_position_probabilities.xlsx`, `men_team_sprint_position_probabilities.xlsx`, etc.
   - Mixed relay: `mixed_relay_position_probabilities.xlsx`

2. **weekly-picks-simulation.R** - Updated output:
   - Output directory: `race-picks/YYYYMMDD` (fantasy files go to race-picks per user request)
   - File names: `fantasy_team.xlsx`, `fantasy_position_probabilities.xlsx`

3. **predict_script.sh** and **predict_script_ubuntu.sh** - Added R script calls:
   - After weekend scrapers: calls `weekly-picks-simulation.R` for cross-country
   - After race scrapers: calls `race-picks-simulation.R` for cross-country
   - Added fantasy file processing: processes `fantasy*.xlsx` from race-picks during weekend events

4. **race-picks-simulation.R** - Output format fixes to match race-picks.R:
   - Added sprint-specific thresholds: `c(1, 3, 6, 12, 30)` for sprint races
   - Sprint columns: `Skier, ID, Nation, Win, Podium, Final, Semifinal, Quarterfinal`
   - Distance columns: `Skier, ID, Nation, Win, Podium, Top5, Top10, Top30`
   - Changed `Name` to `Skier` (matching race-picks.R)
   - Removed hyphens from column names (`Top10` not `Top-10`)
   - Sheet names now: `"Men Race 1"`, `"Ladies Race 1"` (matching race-picks.R)
   - Team events: `Top10` instead of `Top-10`

5. **tds-picks-simulation.R** - Output format fixes to match tds-picks.R:
   - Removed `_simulation` suffix from file names
   - Files: `men.xlsx`, `ladies.xlsx`, `men_position_probabilities.xlsx`, `ladies_position_probabilities.xlsx`
   - Sheet names: `"Men Tour de Ski"`, `"Ladies Tour de Ski"`
   - Added to pipeline: runs on first day of Tour de Ski (Period 2)

6. **final_climb-simulation.R** - Output format fixes to match final_climb.R:
   - Removed `_simulation` suffix from file names
   - Files: `men.xlsx`, `ladies.xlsx`, `men_position_probabilities.xlsx`, `ladies_position_probabilities.xlsx`
   - Sheet names: `"Men Race 1"`, `"Ladies Race 1"`
   - Added to pipeline: runs when Val Di Fiemme + Distance + Freestyle detected

7. **Alpine race-picks-simulation.R** - Created new file (~450 lines):
   - Monte Carlo simulation for Alpine skiing individual races
   - Discipline-specific distributions (Downhill, Super G, Giant Slalom, Slalom, Combined)
   - Speed vs Tech grouping for historical performance
   - Output: `men_position_probabilities.xlsx`, `ladies_position_probabilities.xlsx`
   - Integrated into pipeline (predict_script.sh)

**ALL SIMULATION SCRIPTS NOW INTEGRATED** - Cross-country and Alpine simulation scripts run automatically via master_automation.

### Previous Session (2026-02-27)

**Simulation Scripts Created**:

1. **race-picks-simulation.R** - Individual/Relay/TS/Mixed predictions
2. **weekly-picks-simulation.R** - Weekend fantasy predictions with knapsack optimization
3. **tds-picks-simulation.R** - Tour de Ski overall predictions (~450 lines)
4. **final_climb-simulation.R** - Final Climb race-day predictions (~400 lines)

**ALL SIMULATION SCRIPTS COMPLETE** - All four cross-country simulation scripts are now ready for testing.

---

## COMPLETED: Cross-Country race-picks-simulation.R (2026-02-27)

### Goal
Create a single unified `race-picks-simulation.R` script that:
1. Uses Monte Carlo simulation (like `champs-predictions-simulation.R`)
2. Consolidates all race types: individual, relay, team sprint, mixed relay
3. Replaces the 4 separate scripts (~6,300 lines total) with one unified script

### Current Scripts Being Replaced
| Script | Lines | Purpose |
|--------|-------|---------|
| `race-picks.R` | 2,010 | Individual races (distance, sprint) |
| `race-picks-relay.R` | 1,454 | 4-leg relay |
| `race-picks-team-sprint.R` | 1,504 | 2-leg team sprint |
| `race-picks-mixed-relay.R` | 1,387 | 4-leg mixed relay |

### Reference: champs-predictions-simulation.R (3,543 lines)
Key sections to replicate:
- **Lines 23-50**: Configuration (variance control, decay lambda, calibration flags)
- **Lines 95-150**: `filter_positive_coefficients()` - iterative feature selection
- **Lines 150-300**: Data loading and preprocessing
- **Lines 300-500**: `calculate_leg_importance_from_models()` - model deviance approach
- **Lines 500-800**: `train_points_gam()` - GAM model training with positive coefficients
- **Lines 800-1200**: `train_relay_leg_models_for_simulation()` - leg-specific models
- **Lines 1200-1600**: `run_individual_simulation()` - Monte Carlo for individuals
- **Lines 1600-2000**: `run_relay_simulation_hybrid()` - Monte Carlo for relays
- **Lines 2000-2500**: Team selection and optimization
- **Lines 2500-3543**: Excel output formatting

### Key Differences from champs-predictions
| Aspect | champs-predictions | race-picks |
|--------|-------------------|------------|
| Field | Championship entries (known) | World Cup startlists (daily) |
| Date handling | Fixed championship dates | Today's UTC date |
| Participation | All entered athletes race | FIS startlist or probability-based |
| Output | Per-championship Excel | Per-race-day Excel |
| TEST_MODE | Not needed | Uses `test_races.csv` when enabled |

### Implementation Plan (COMPLETED 2026-02-27)

#### Phase 1: Core Structure ✓
- [x] Create `race-picks-simulation.R` with configuration section
- [x] Add TEST_MODE support (already in other race-picks scripts)
- [x] Load races.csv and filter for today's date
- [x] Determine which race types are scheduled today (individual/relay/ts/mixed)

#### Phase 2: Individual Race Simulation ✓
- [x] Port `filter_positive_coefficients()` from champs-predictions-simulation.R
- [x] Port `train_points_gam()` with exponential decay weighting
- [x] Port `simulate_race_positions()` with variance control parameters
- [x] Port `build_athlete_distribution()` with history + GAM samples

#### Phase 3: Relay/Team Sprint Simulation ✓
- [x] Port `train_relay_leg_models_for_simulation()` (handles both 4-leg and 2-leg)
- [x] Port `calculate_leg_importance_from_models()`
- [x] Port `simulate_team_race()` for Monte Carlo team ranking
- [x] Port team selection with dual optimization (podium + win)
- [x] Handle technique-specific features (C/F for team sprint)

#### Phase 4: Mixed Relay Support ✓
- [x] Extend relay logic for mixed gender teams
- [x] Handle leg gender assignments (Ladies legs 1-2, Men legs 3-4)
- [x] Combine gender-specific leg models for mixed team simulation

#### Phase 5: Output and Integration ✓
- [x] Format Excel output with per-race files
- [x] User-friendly column names (Win, Podium, Top5, Top-10, Top-30)
- [x] Separate files for individual/relay/team sprint/mixed relay
- [x] Console summary of top predictions

#### Phase 6: Calibration (Uses Pre-calibrated Values)
- [x] Calibration flags present (RUN_CALIBRATION, RUN_RELAY_CALIBRATION, RUN_TEAM_SPRINT_CALIBRATION)
- [x] Uses pre-calibrated parameters from champs-predictions-simulation.R
- [ ] Optional: Port full calibration grid search if needed

### Variance Control Parameters (from champs-predictions-simulation.R)
```r
# Individual races
DECAY_LAMBDA <- 0.002      # Exponential decay (50% weight after ~1 year)
SD_SCALE_FACTOR <- 0.77    # Multiply all SDs (lower = favorites win more)
SD_MIN <- 4                # Minimum SD
SD_MAX <- 16               # Maximum SD

# Relay (4 legs)
RELAY_SCORE_SD_MIN <- 0.5
RELAY_SCORE_SD_MAX <- 1.15

# Team Sprint (2 legs)
TS_SCORE_SD_MIN <- 0.45
TS_SCORE_SD_MAX <- 0.8
```

### Files
- **New**: `content/post/cross-country/drafts/race-picks-simulation.R`
- **Reference**: `content/post/cross-country/drafts/champs-predictions-simulation.R`
- **Data**: `~/ski/elo/python/ski/polars/excel365/races.csv` (or `test_races.csv`)

---

## FUTURE: Extend Simulation to Other Sports

### Championship Predictions
| Sport | Status |
|-------|--------|
| Alpine | Pending |
| Biathlon | Pending |
| Cross-Country | **DONE** |
| Nordic Combined | Pending |
| Ski Jumping | Pending |

### Championship Predictions (Simulation)
| Sport | Status |
|-------|--------|
| Cross-Country | **DONE** |
| Alpine | **DONE** (2026-03-01) |
| Biathlon | **DONE** (2026-03-01) |
| Nordic Combined | Pending |
| Ski Jumping | Pending |

### Race Picks
| Sport | Status |
|-------|--------|
| Cross-Country | **DONE** |
| Alpine | **DONE** (2026-03-01) |
| Biathlon | **DONE** (2026-03-01) |
| Nordic Combined | Pending |
| Ski Jumping | Pending |

### Alpine Simulation (2026-03-01)
- Created `alpine/drafts/race-picks-simulation.R`
- Created `alpine/drafts/champs-predictions-simulation.R`
- Features:
  - Monte Carlo simulation (10,000 iterations)
  - Discipline-specific distributions (Downhill, Super G, Giant Slalom, Slalom, Combined)
  - Speed vs Tech grouping for historical performance
  - Exponential decay weighting (DECAY_LAMBDA = 0.002)
  - 4-person quota constraint per nation (champs)
- race-picks Output: `men_position_probabilities.xlsx`, `ladies_position_probabilities.xlsx`
- champs-predictions Output: `men_position_probabilities.xlsx`, `ladies_position_probabilities.xlsx`, `men.xlsx`, `ladies.xlsx`, `nations_individual.xlsx`
  - Sheet names: `"1. Downhill - Feb 07"` format
- Integrated into `predict_script.sh` and `predict_script_ubuntu.sh`

### Biathlon Simulation (2026-03-01)
- Created `biathlon/drafts/race-picks-simulation.R`
- Created `biathlon/drafts/champs-predictions-simulation.R`
- Features:
  - Monte Carlo simulation (10,000 iterations)
  - Race type-specific distributions (Sprint, Individual, Pursuit, Mass Start)
  - Biathlon points systems: regular_points (90, 75, 65...) and mass_start_points
  - RaceType column filtering (excludes relay races)
  - Exponential decay weighting (DECAY_LAMBDA = 0.002)
  - 4-person quota constraint per nation (champs)
- race-picks Output: `men_position_probabilities.xlsx`, `ladies_position_probabilities.xlsx`
- champs-predictions Output: `men_position_probabilities.xlsx`, `ladies_position_probabilities.xlsx`, `men.xlsx`, `ladies.xlsx`, `nations_individual.xlsx`
  - Sheet names: `"1. Sprint - Feb 07"` format
- Integrated into `predict_script.sh` and `run_champs_predictions.sh`

### Sport-Specific Considerations
- **Alpine**: Speed vs Technical discipline distinction
- **Biathlon**: Shooting accuracy, pursuit/mass start effects
- **Nordic Combined**: Two-phase competition, gap-start
- **Ski Jumping**: Hill size distinctions, wind factors

---

## COMPLETED: Align Leg Importance Calculation Across Scripts (2026-02-13)

### Problem
The `race-picks-relay.R` and `race-picks-team-sprint.R` scripts use simple hardcoded default weights for leg importance, while `weekly-picks-relay.R`, `weekly-picks-team-sprint.R`, and `champs-predictions-simulation.R` use more sophisticated model-based approaches.

### Current Implementations

| Script | Leg Importance Method |
|--------|----------------------|
| `weekly-picks-relay.R` | `calculate_leg_importance()` - 3 options with fallbacks |
| `weekly-picks-team-sprint.R` | `calculate_leg_importance()` - 3 options with fallbacks |
| `race-picks-relay.R` | Hardcoded `c(0.2, 0.2, 0.25, 0.35)` |
| `race-picks-team-sprint.R` | Likely hardcoded defaults |
| `champs-predictions-simulation.R` | `calculate_leg_importance_from_models()` - model deviance approach |

### `calculate_leg_importance()` in weekly-picks-*.R (lines 1150-1211)
```r
calculate_leg_importance <- function(leg_models) {
  # Option 1: Use team model coefficients if available (from trained team_podium model)
  # Option 2: Use individual leg model accuracy as proxy for importance
  # Option 3: Default weights c(0.2, 0.2, 0.25, 0.35) for relay, c(0.5, 0.5) for team sprint
}
```

### `calculate_leg_importance_from_models()` in champs-predictions-simulation.R (lines 288-462)
More sophisticated approach:
1. Calculate `prev_points_weighted` with exponential decay from individual race history
2. Create `Pelo_pct` columns normalized per race
3. Train leg-specific GLM models predicting `is_top3 = (Place <= 3)`
4. Calculate importance based on model deviance (pseudo-R²)
5. Higher deviance = athlete quality on that leg better predicts team success

### Tasks (COMPLETED)

#### Task 1: Update race-picks-relay.R ✓
**Location**: `~/blog/daehl-e/content/post/cross-country/drafts/race-picks-relay.R`

Replaced the simple `calculate_leg_importance()` function (line 870) with the full 3-option version from `weekly-picks-relay.R`:
- Option 1: Team model coefficients via `varImp()`
- Option 2: Individual leg model accuracy
- Option 3: Default weights `c(0.2, 0.2, 0.25, 0.35)`

#### Task 2: Update race-picks-team-sprint.R ✓
**Location**: `~/blog/daehl-e/content/post/cross-country/drafts/race-picks-team-sprint.R`

Applied the same pattern for 2-leg team sprint:
- Added `safe_team_importance()` helper function (line 850)
- Added full 3-option `calculate_leg_importance()` function (line 882)
- Default weights for team sprint: `c(0.5, 0.5)`

#### Task 2b: Update race-picks-mixed-relay.R ✓
**Location**: `~/blog/daehl-e/content/post/cross-country/drafts/race-picks-mixed-relay.R`

Applied the same pattern for 4-leg mixed relay:
- Added `safe_team_importance()` helper function (line 863)
- Added full 3-option `calculate_leg_importance()` function (line 893)
- Default weights for mixed relay: `c(0.2, 0.25, 0.25, 0.3)`

#### Task 3: Verify Consistency ✓
Verified that:
1. Both race-picks-*.R scripts now have the same 3-option fallback logic as weekly-picks-*.R
2. Relay uses 4-leg weights, team sprint uses 2-leg weights
3. Log messages indicate which option was used at runtime

### Files to Modify
- `~/blog/daehl-e/content/post/cross-country/drafts/race-picks-relay.R`
- `~/blog/daehl-e/content/post/cross-country/drafts/race-picks-team-sprint.R`

### Reference Files
- `~/blog/daehl-e/content/post/cross-country/drafts/weekly-picks-relay.R` (lines 1150-1211)
- `~/blog/daehl-e/content/post/cross-country/drafts/weekly-picks-team-sprint.R`
- `~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions-simulation.R` (lines 288-462)

---

## Recent Changes (2026-02-13)

### Fantasy Output Updated (weekly-picks*.R)
Changed fantasy prediction output from top 8 men + 8 ladies to **top 20 men + top 20 ladies** (or max available):

**Files modified:**
- `weekly-picks2.R` - Lines 1104-1124: Now outputs top 20 men + top 20 ladies by `Total_Points`
- `weekly-picks-relay.R`:
  - `save_fantasy_results()` (lines 1816-1841): Filters by Gender, takes top 20 each, sorted by `Expected_Points`
  - Line 2102: Commented out `optimize_fantasy_team()` knapsack call, now passes `list(team = combined_teams)` directly
- `weekly-picks-team-sprint.R`:
  - `save_fantasy_results()` (lines 1768-1800): Filters by Gender, takes top 20 each, sorted by `Expected_Points`
  - Line 2039: Commented out `optimize_fantasy_team()` knapsack call, now passes `list(team = combined_teams)` directly
- `weekly-picks-mixed-relay.R`:
  - `save_fantasy_results()` (lines 1853-1871): Takes top 20 teams overall (mixed relay teams are mixed gender)
  - Line 1973: Commented out `optimize_fantasy_team()` knapsack call, now passes `list(team = team_predictions)` directly

### Relay Scraper Fix (startlist_scrape_races_relay.py)
Fixed sponsor names being appended to athlete names (e.g., "SVAHN Linn Fischer" instead of "SVAHN Linn"):
- Changed selector from `.g-lg-14.g-md-14.g-sm-11.g-xs-10.justify-left.bold` to `.athlete-name`
- The sponsor is in a separate `.constructor-name` sibling div

**Files modified:**
- `~/ski/elo/python/ski/polars/relay/startlist_scrape_races_relay.py` (line 275)
- `~/ski/elo/python/ski/polars/relay/startlist_scrape_races_mixed_relay.py` (line 280)

### Request Headers Added (startlist_scrape_races_relay.py)
Added User-Agent header to avoid 403 Forbidden errors from FIS website:
```python
headers = {
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36'
}
response = requests.get(url, headers=headers)
```

### User-Friendly Column Names in Excel Output
Updated all fantasy and race-picks output files to use user-friendly column names instead of underscored variable names:

**Column Renames Applied:**
- `Team_Name` → `Team`
- `Expected_Points` → `Expected Points`
- `Member_1`, `Member_2`, etc. → `Leg 1`, `Leg 2`, etc.
- `Win_Prob` → `Win`
- `Podium_Prob` → `Podium`
- `Top5_Prob` → `Top 5`
- `Top10_Prob` → `Top 10`

**Files modified:**
- `weekly-picks-relay.R` - `save_fantasy_results()` (lines 1843-1854)
- `weekly-picks-team-sprint.R` - `save_fantasy_results()` (lines 1795-1806)
- `weekly-picks-mixed-relay.R` - `save_fantasy_results()` (lines 1870-1881)
- `race-picks-relay.R` - `save_prediction_results()` (lines 1207-1248)
- `race-picks-team-sprint.R` - `save_prediction_results()` (lines 1244-1285)
- `race-picks-mixed-relay.R` - `save_prediction_results()` (lines 1207-1260)

---

## COMPLETED: TEST_MODE for Sandbox/QA Testing (2026-02-26)

### Overview
Added `TEST_MODE` flag to all race-picks and weekly-picks R scripts across all sports. When enabled, scripts read from `test_races.csv` or `test_weekends.csv` instead of production files, allowing EDA and testing without modifying production data.

### Usage
1. Set `TEST_MODE <- TRUE` at the top of any script
2. Create your test file (e.g., `test_races.csv`) in the same directory as the production file
3. Run the script - it will use the test file instead
4. Set `TEST_MODE <- FALSE` when done testing

### Test File Locations
All test files have been created (copied from production files on 2026-02-26):

| Sport | test_races.csv | test_weekends.csv |
|-------|----------------|-------------------|
| Alpine | `~/ski/elo/python/alpine/polars/excel365/test_races.csv` ✓ | `~/ski/elo/python/alpine/polars/excel365/test_weekends.csv` ✓ |
| Biathlon | `~/ski/elo/python/biathlon/polars/excel365/test_races.csv` ✓ | `~/ski/elo/python/biathlon/polars/excel365/test_weekends.csv` ✓ |
| Cross-Country | `~/ski/elo/python/ski/polars/excel365/test_races.csv` ✓ | `~/ski/elo/python/ski/polars/excel365/test_weekends.csv` ✓ |
| Nordic Combined | `~/ski/elo/python/nordic-combined/polars/excel365/test_races.csv` ✓ | `~/ski/elo/python/nordic-combined/polars/excel365/test_weekends.csv` ✓ |
| Ski Jump | `~/ski/elo/python/skijump/polars/excel365/test_races.csv` ✓ | `~/ski/elo/python/skijump/polars/excel365/test_weekends.csv` ✓ |

Edit these test files to set up your test scenarios (e.g., change dates to today's date to trigger predictions).

### Files Modified (16 total)

**Alpine:**
- `content/post/alpine/drafts/race-picks.R`
- `content/post/alpine/drafts/weekly-picks2.R`

**Biathlon:**
- `content/post/biathlon/drafts/race-picks.R`
- `content/post/biathlon/drafts/weekly-picks2.R`

**Cross-Country:**
- `content/post/cross-country/drafts/race-picks.R`
- `content/post/cross-country/drafts/race-picks-relay.R`
- `content/post/cross-country/drafts/race-picks-team-sprint.R`
- `content/post/cross-country/drafts/race-picks-mixed-relay.R`
- `content/post/cross-country/drafts/weekly-picks2.R`
- `content/post/cross-country/drafts/weekly-picks-relay.R`
- `content/post/cross-country/drafts/weekly-picks-team-sprint.R`
- `content/post/cross-country/drafts/weekly-picks-mixed-relay.R`

**Nordic Combined:**
- `content/post/nordic-combined/drafts/race-picks.R`
- `content/post/nordic-combined/drafts/weekly-picks2.R`

**Ski Jump:**
- `content/post/skijump/drafts/race-picks.R`
- `content/post/skijump/drafts/weekly-picks2.R`

### Implementation Pattern
```r
# ===== TEST MODE =====
# Set to TRUE to use test_races.csv for EDA/sandbox testing
TEST_MODE <- FALSE

# ... later in the script ...
races_file <- if(TEST_MODE) {
  "~/ski/elo/python/{sport}/polars/excel365/test_races.csv"
} else {
  "~/ski/elo/python/{sport}/polars/excel365/races.csv"
}
races <- read.csv(races_file, stringsAsFactors = FALSE)
log_info(paste("Reading races from:", races_file))
```

---

## PENDING: Simulation Scripts for Cross-Country (2026-02-27)

### Overview
Create simulation versions of the remaining cross-country prediction scripts using the Monte Carlo approach from `race-picks-simulation.R` and `champs-predictions-simulation.R`.

### 1. race-picks.R Adjustments Analysis

**Current Adjustments in race-picks.R (lines 1075-1125):**

The production `race-picks.R` applies three personalized adjustments using t-tests on historical residuals:

#### Altitude/Elevation Adjustment
```r
# Creates altitude category: high (>=1300m) vs low (<1300m)
AltitudeCategory = ifelse(Elevation >= 1300, 1, 0)

# For each skier, compares performance at high vs low altitude
# If t-test p < 0.05, applies correction:
altitude_correction = mean(Prediction_Diff[AltitudeCategory == AltitudeCategory])
```

#### Period Adjustment (Season Phase)
```r
# Compares performance in same vs different periods of the season
# If t-test p < 0.05, applies correction:
period_correction = mean(Course_Diff[Period == Period])
```

#### Mass Start (MS) Adjustment
```r
# Compares performance in mass start vs interval start races
# If t-test p < 0.05, applies correction:
ms_correction = mean(Period_Diff[MS == MS])
```

**Can These Be Applied to race-picks-simulation.R?**

YES, using the adjustments approach (preferred over separating prev_points_weighted by MS):

| Adjustment | Production Approach | Simulation Approach |
|------------|--------------------|--------------------|
| Altitude | T-test on residuals, apply if p<0.05 | Add altitude_effect to mean in distribution building |
| Period | T-test on residuals, apply if p<0.05 | Add period_effect to mean in distribution building |
| MS | T-test on residuals, apply if p<0.05 | Add ms_effect to mean in distribution building |

**Why Adjustments vs Separating prev_points_weighted by MS:**
- Many skiers haven't competed in both mass start AND individual races → small sample sizes
- Adjustments approach only applies when statistically significant (p < 0.05)
- Base prediction uses ALL matching races for robust sample size
- Skiers without enough data in one format simply get no adjustment (0 effect)

**Implementation in race-picks-simulation.R (COMPLETED 2026-02-27):**

Adjustments are calculated INSIDE `build_athlete_distribution()` using the same decay-weighted approach as the distribution mean. This ensures adjustments are relative to what's actually used for predictions.

**Key insight**: The adjustment must be `condition_mean - history_mean` (both using decay weights), NOT relative to GAM residuals. The distribution is built from historical performance, so adjustments should be relative to that baseline.

**Implementation Details:**
```r
# Inside build_athlete_distribution():
history_weighted_mean <- weighted.mean(history_points, history_weights, na.rm = TRUE)

# For each condition (altitude/period/MS):
# 1. Split historical races by condition
# 2. Run t-test between groups
# 3. If p < 0.05:
#    high_wmean <- weighted.mean(high_points, high_weights)
#    adjustment <- high_wmean - history_weighted_mean  # (if racing at high altitude)

# Apply total adjustment
adjusted_mean <- weighted_mean + altitude_adj + period_adj + ms_adj
```

**Helper Functions Added:**
- `add_period_column()` - Calculates season phase (1-5) based on race number
- `add_altitude_category()` - Creates binary high/low altitude category (≥1300m)

**Parameters:**
- `min_samples_for_adjustment = 3` - Minimum races in each condition group for t-test
- `p < 0.05` threshold for statistical significance

---

### 2. weekly-picks-simulation.R Plan

**Purpose**: Weekly fantasy predictions using Monte Carlo simulation instead of GAM predictions.

**Current weekly-picks2.R Approach:**
- Uses GAM models with regsubsets feature selection
- Applies same altitude/period/MS adjustments as race-picks.R
- Fantasy output: Top 20 men + top 20 ladies by Total_Points
- Output: `fantasy_team.xlsx`

**Simulation Approach:**
1. Build athlete distributions for each race in the weekend
2. Run Monte Carlo simulation (N_SIMULATIONS = 10000)
3. Calculate expected points from simulation results
4. Apply race participation probabilities
5. Sum across races for Total_Points
6. Output top 20 men + top 20 ladies

**Key Differences from race-picks-simulation.R:**
- Uses `weekends.csv` instead of `races.csv`
- Handles multiple races per weekend
- Probability-weighted expected points across races
- Fantasy-specific output format

**Files to Create:**
- `content/post/cross-country/drafts/weekly-picks-simulation.R`

---

### 3. tds-picks-simulation.R Plan

**Purpose**: Tour de Ski overall predictions using Monte Carlo simulation.

**Current tds-picks.R Approach:**
- Uses rolling weighted features (Last_5 columns by technique/distance)
- XGBoost and GAM hybrid modeling
- Special handling for Final Climb race
- TdS-specific points system (300, 285, 270...)
- Stage race scoring

**Simulation Approach:**
1. Build athlete distributions for each TdS stage
2. Run Monte Carlo simulation for each stage
3. Sum stage points for overall TdS prediction
4. Special handling for Final Climb (uses Distance_F features heavily)
5. Account for accumulated fatigue (optional: increase variance in later stages)

**Key Features from tds-picks.R to Port:**
- `process_dataframe_for_points()` - rolling weighted features
- `impute_features()` - first quartile imputation
- `create_percentage_columns()` - Elo/Pelo percentages
- `fc_feature_selection()` - Final Climb specific features
- Stage race points system

**Files to Create:**
- `content/post/cross-country/drafts/tds-picks-simulation.R`

---

### 4. final_climb-simulation.R Plan

**Purpose**: Tour de Ski Final Climb race-day predictions using Monte Carlo simulation.

**Current final_climb.R Approach:**
- Filters training data to `Final_Climb == 1` only
- Key features: `Distance_F_Pelo_Pct`, `Distance_F_Last_5`
- Uses fc_points (same as tds_points: 300, 285, 270...)
- GAM model trained on Final Climb historical data only

**Simulation Approach:**
1. Load Final Climb startlist from `startlist_races_*.csv`
2. Build distributions using Distance_F specific features
3. Run Monte Carlo simulation
4. Output Final Climb predictions with probabilities

**Key Considerations:**
- Small training dataset (only one Final Climb per year since 2007)
- Heavy reliance on Distance_F_Pelo_Pct as predictor
- May need wider variance due to unique nature of race

**Files to Create:**
- `content/post/cross-country/drafts/final_climb-simulation.R`

---

### Implementation Priority

| Script | Priority | Complexity | Dependencies | Status |
|--------|----------|------------|--------------|--------|
| Add adjustments to race-picks-simulation.R | 1 | Medium | None | **DONE** (2026-02-27) |
| weekly-picks-simulation.R | 2 | Medium | race-picks-simulation.R | **DONE** (2026-02-27) |
| tds-picks-simulation.R | 3 | Medium | weekly-picks-simulation.R | **DONE** (2026-02-27) |
| final_climb-simulation.R | 4 | Medium | tds-picks-simulation.R | **DONE** (2026-02-27) |

---

### Shared Functions to Port

These functions from the production scripts should be adapted for simulation:

```r
# From race-picks.R / weekly-picks2.R:
calculate_skier_adjustments()      # T-test based altitude/period/MS adjustments
prepare_startlist_data()           # Startlist preparation with Elo percentages
normalize_position_probabilities() # Probability normalization with caps

# From tds-picks.R:
process_dataframe_for_points()     # Rolling weighted features
impute_features()                  # First quartile imputation by Season/Race
create_percentage_columns()        # Elo/Pelo percentage calculations

# From final_climb.R:
fc_feature_selection()             # Final Climb specific feature selection
filter_zero_probability_athletes() # Filter by race participation probability
```

---

### Variance Control Parameters (Proposed)

```r
# Individual races (from race-picks-simulation.R)
DECAY_LAMBDA <- 0.002
SD_SCALE_FACTOR <- 0.77
SD_MIN <- 4
SD_MAX <- 16

# TdS Stage races (may need calibration)
TDS_SD_SCALE_FACTOR <- 0.8    # Slightly higher variance for stage races
TDS_SD_MIN <- 3
TDS_SD_MAX <- 14

# Final Climb (unique race, may need higher variance)
FC_SD_SCALE_FACTOR <- 0.85
FC_SD_MIN <- 5
FC_SD_MAX <- 18
```

---

### File Locations Summary

**Scripts to Create:**
```
content/post/cross-country/drafts/weekly-picks-simulation.R
content/post/cross-country/drafts/tds-picks-simulation.R
content/post/cross-country/drafts/final_climb-simulation.R
```

**Reference Scripts:**
```
content/post/cross-country/drafts/race-picks-simulation.R      # Monte Carlo base
content/post/cross-country/drafts/champs-predictions-simulation.R # Hybrid approach
content/post/cross-country/drafts/race-picks.R                 # Adjustments
content/post/cross-country/drafts/weekly-picks2.R              # Fantasy logic
content/post/cross-country/drafts/tds-picks.R                  # TdS features
content/post/cross-country/drafts/final_climb.R                # FC features
```
