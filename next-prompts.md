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

## Session Resume Instructions

If starting a new session:
1. Read this file to understand current status
2. **Cross-country simulation is now production-ready**
3. Run `./run_champs_predictions.sh` followed by `./champs_script.sh 2026` to generate predictions
4. The original `champs-predictions.R` is preserved but no longer used for cross-country
5. **Update this file** with any changes made
