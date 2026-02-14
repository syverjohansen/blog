# 2026 Winter Olympics Championship Predictions

## Current Status (2026-02-06)

### Project Overview
Creating championship prediction blog posts for the 2026 Winter Olympics with:
- Race-by-race predictions with participation, winning, and podium probabilities
- Nations breakdown showing athletes and expected medal counts

### Pipeline
```
Python Scraper → R Predictions → Excel → JSON → Hugo Blog Post
```

**IMPORTANT**: Update this file (`next-prompts.md`) whenever changes are made to the codebase. This ensures session continuity and accurate status tracking.

---

## Sport-by-Sport Status

| Sport | Production Script | Simulation Script | Status |
|-------|-------------------|-------------------|--------|
| Alpine | Ready | N/A | Ready |
| Biathlon | Ready | N/A | Ready |
| Cross-Country | Ready | Ready (Hybrid) | Ready |
| Nordic Combined | Ready | N/A | Ready |
| Ski Jumping | Ready | N/A | Ready |

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

## Session Resume Instructions

If starting a new session:
1. Read this file to understand current status
2. **Cross-country simulation is now production-ready**
3. Run `./run_champs_predictions.sh` followed by `./champs_script.sh 2026` to generate predictions
4. The original `champs-predictions.R` is preserved but no longer used for cross-country
5. **weekly-picks2.R**: Fantasy output now uses simple top-8 selection (old MIP method commented out)
6. **weekly-picks2.R**: Race probability uses FIS startlist when available, falls back to In_Config
7. **Update this file** with any changes made

---

## FUTURE: Extend Simulation Approach to Other Scripts

### Overview
Apply the Monte Carlo simulation methodology (developed for cross-country champs-predictions-simulation.R) to other prediction scripts across all sports.

### Scripts to Convert

#### Championship Predictions (champs-predictions.R)
| Sport | Location | Status |
|-------|----------|--------|
| Alpine | `content/post/alpine/drafts/champs-predictions.R` | Pending |
| Biathlon | `content/post/biathlon/drafts/champs-predictions.R` | Pending |
| Cross-Country | `content/post/cross-country/drafts/champs-predictions-simulation.R` | **DONE** |
| Nordic Combined | `content/post/nordic-combined/drafts/champs-predictions.R` | Pending |
| Ski Jumping | `content/post/skijump/drafts/champs-predictions.R` | Pending |

#### Race Picks (Weekly World Cup Predictions)
| Sport | Script | Location | Status |
|-------|--------|----------|--------|
| Cross-Country | race-picks.R | `content/post/cross-country/drafts/` | Pending |
| Cross-Country | weekly-picks2.R | `content/post/cross-country/drafts/` | Pending |
| Alpine | race-picks.R (or equivalent) | `content/post/alpine/drafts/` | Pending |
| Biathlon | race-picks.R (or equivalent) | `content/post/biathlon/drafts/` | Pending |
| Nordic Combined | race-picks.R (or equivalent) | `content/post/nordic-combined/drafts/` | Pending |
| Ski Jumping | race-picks.R (or equivalent) | `content/post/skijump/drafts/` | Pending |

### Key Features to Implement (per script)

1. **Monte Carlo Simulation**
   - Build athlete distributions from GAM predictions + historical variance
   - Run 10,000 simulations per race
   - Natural probability normalization (no post-hoc adjustments)

2. **Exponential Decay Weighting**
   - Apply to prev_points_weighted calculation
   - Use DECAY_LAMBDA = 0.002 (or sport-specific calibrated value)
   - Consistent between training and testing

3. **Positive Coefficient Constraint**
   - Filter features with negative coefficients during selection
   - Ensures all predictors positively correlate with success

4. **Calibration System**
   - Grid search over variance parameters (SD_SCALE_FACTOR, SD_MIN, SD_MAX)
   - Minimize Brier score on historical data
   - Sport-specific and event-specific calibration

5. **Team Event Handling** (where applicable)
   - Dual optimization (podium + win)
   - Leg-specific models
   - Technique-specific features (for cross-country)

### Sport-Specific Considerations

**Alpine:**
- Speed vs Technical discipline distinction
- Downhill/Super-G vs Slalom/Giant Slalom feature sets

**Biathlon:**
- Shooting accuracy as additional feature
- Pursuit/Mass Start starting position effects

**Nordic Combined:**
- Two-phase competition (jumping + skiing)
- Gap-start considerations

**Ski Jumping:**
- Hill size distinctions (Normal/Large/Flying)
- Wind/conditions factors if available

### Implementation Order (Suggested)
1. Cross-country race-picks.R / weekly-picks2.R (closest to existing simulation)
2. Biathlon champs-predictions.R (similar structure to cross-country)
3. Alpine champs-predictions.R
4. Nordic Combined champs-predictions.R
5. Ski Jumping champs-predictions.R
6. Remaining race-picks scripts

### Methodology Documentation
For each converted script, update:
- `content/post/methods/champs-predictions.md` (for championship scripts)
- `content/post/methods/race-picks.md` (for weekly picks scripts)
- Create Reddit-style explanation post (like `reddit-post.md`)
