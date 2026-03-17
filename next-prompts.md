# Winter Sports Prediction System

## Current Status (2026-03-14)

**IMPORTANT**: Update this file (`next-prompts.md`) whenever changes are made to the codebase.

### Immediate Focus

- Parameter optimization is now being rerun event by event instead of full-sport/full-day sweeps.
- New helper added in `content/post/optimization/param-optimizer.R`:
  - `optimize_event_and_update_sport_params(sport, gender, race_type = NULL)`
- Intended workflow:
  - run one sport/gender `default` first
  - then run one race type at a time
  - each event run merges into the latest saved sport result and regenerates `shared/sport_params.R`
- Current cross-country men status:
  - `default` optimized
  - `Sprint_C` optimized
  - `Sprint_F` optimized
  - pending:
    - `Distance_C_Ind`
    - `Distance_C_Ms`
    - `Distance_F_Ind`
    - `Distance_F_Ms`
    - `Distance_Ms`
    - `Relay`
    - `Mixed_Relay`
    - `Team_Sprint`
- Current cross-country women status:
  - pending:
    - `default`
    - `Sprint_C`
    - `Sprint_F`
    - `Distance_C_Ind`
    - `Distance_C_Ms`
    - `Distance_F_Ind`
    - `Distance_F_Ms`
    - `Distance_Ms`
    - `Relay`
    - `Mixed_Relay`
    - `Team_Sprint`
- Current other-sport event-by-event status:
  - Alpine:
    - pending event-by-event fill
  - Biathlon:
    - pending event-by-event fill
  - Nordic Combined:
    - pending event-by-event fill
  - Ski Jumping:
    - pending event-by-event fill
- Current optimization debugging status:
  - fixed missing helper export in race-type optimization path
  - fixed integer-format `%d` crash during fine/random search
  - added sequential fallback if parallel worker setup is unavailable
  - cross-country duplicate race-picks post sections fixed by removing the extra weekend-side `race-picks` registration in `predict_script.sh` and `predict_script_ubuntu.sh`

### Pipeline
```
Python Scraper → R Predictions → Excel → JSON → Hugo Blog Post
```

### Sport-by-Sport Status

| Sport | race-picks-simulation.R | champs-predictions-simulation.R | GAM Support | Pipeline Integrated |
|-------|------------------------|--------------------------------|-------------|---------------------|
| Alpine | ✓ | ✓ | ✓ Added | ✓ |
| Biathlon | ✓ | ✓ | ✓ Added | ✓ |
| Cross-Country | ✓ | ✓ | ✓ (original) | ✓ |
| Nordic Combined | ✓ | ✓ | ✓ Added | ✓ |
| Ski Jumping | ✓ | ✓ | ✓ Added | ✓ |

**ALL SPORTS NOW USE MONTE CARLO SIMULATION** - Legacy GAM-based scripts are still in place but simulation scripts are the primary prediction method.

---

## COMPLETED TASKS

### Task 1: Code Review of Simulation Scripts (COMPLETED 2026-03-02)

**Issues Found:**

1. **Column Naming**: Simulation scripts use `Top-5`, `Top-10`, `Top-30` (with hyphens). User confirmed this is preferred.

2. **TEST_MODE Loading Fixed**:
   - Alpine race-picks-simulation.R - Fixed (was hardcoded)
   - Alpine champs-predictions-simulation.R - Fixed (was hardcoded)
   - Cross-Country champs-predictions-simulation.R - Fixed (was missing entirely)

3. **GAM Support for Athletes with Insufficient History**:
   - **Problem**: Scripts fell back to `mean=5, sd=16` for athletes with no history (poor estimates)
   - **Solution**: Added GAM-based fill following Cross-Country's exemplar pattern
   - **How it works**: For athletes with < 10 races, GAM predicts expected points based on ELO features, then fills missing history slots with synthetic points (weighted at 0.25x)

**GAM Support Added To:**
- ✅ Alpine race-picks-simulation.R
- ✅ Alpine champs-predictions-simulation.R
- ✅ Biathlon race-picks-simulation.R
- ✅ Nordic Combined race-picks-simulation.R
- ✅ Nordic Combined champs-predictions-simulation.R
- ✅ Ski Jumping race-picks-simulation.R
- ✅ Ski Jumping champs-predictions-simulation.R

**GAM Support Complete** - All simulation scripts now have GAM-based fill for athletes with insufficient history.

---

## ACTIVE TASKS

### Task 2: Create Testing Harness (COMPLETED 2026-03-03)

Created comprehensive testing infrastructure for the prediction pipeline.

**What was done:**

1. **Expanded test_races.csv Files** - Created curated test files for all 5 sports:
   | Sport | Races | Coverage |
   |-------|-------|----------|
   | Alpine | 28 | All 4 disciplines, Periods 1-5, M/L, Championships |
   | Biathlon | 37 | All 7 race types, Periods 1-5, M/L/Mixed, Multiple elevations, Championships |
   | Cross-Country | 44 | All distances/techniques, MS/Pursuit/Stage/Final_Climb flags, Championships |
   | Nordic Combined | 30 | Individual/IndividualCompact/Mass Start/Team Sprint, M/L/Mixed, Championships |
   | Ski Jumping | 40 | Large/Normal/Flying/Team Large/Team Normal, Hill sizes 90-240, Championships |

2. **Updated Startlist Scrapers** - Added TEST_MODE support via `pipeline_config`:
   - ✅ alpine/polars/startlist-scrape-races.py
   - ✅ biathlon/polars/startlist-scrape-races.py
   - ✅ nordic-combined/polars/startlist-scrape-races.py
   - ✅ skijump/polars/startlist-scrape-races.py
   - (cross-country already had TEST_MODE support)

3. **Created Test Harness** - `~/ski/elo/python/test_harness.py`:
   ```bash
   # Usage examples:
   python test_harness.py                    # Test all sports
   python test_harness.py alpine             # Test specific sport
   python test_harness.py --scrape-only      # Only run scraping phase
   python test_harness.py --simulate-only    # Only run simulation phase
   python test_harness.py -v                 # Verbose output
   ```

**Test File Locations:**
```
~/ski/elo/python/alpine/polars/excel365/test_races.csv
~/ski/elo/python/biathlon/polars/excel365/test_races.csv
~/ski/elo/python/ski/polars/excel365/test_races.csv
~/ski/elo/python/nordic-combined/polars/excel365/test_races.csv
~/ski/elo/python/skijump/polars/excel365/test_races.csv
```

**Note:** Run test harness from within the venv to avoid numpy path conflicts.

### Task 2.5: Shared R Library & Enhanced Logging (COMPLETED 2026-03-08)

**Goal:** Extract common functions into a shared library and add comprehensive logging to enable full pipeline visibility.

**Why:**
- ~60-70% code duplication across 5 sport scripts (~2,500+ lines)
- Current logging is moderate - missing timing, distribution stats, data quality checks
- Future goal: sport/event-specific parameter tuning (easier with shared config)

**Approach (Safe Rollout):**
1. Keep existing production scripts intact (no changes to working code)
2. Create shared library alongside existing scripts
3. Create v2 scripts that use shared library
4. Test v2 thoroughly before any production switchover
5. Gradual rollout - one sport at a time

**Files to Create:**
```
~/blog/daehl-e/content/post/shared/race-picks-common.R          # Shared functions + logging
~/blog/daehl-e/content/post/shared/logging-utils.R              # Logging utilities
~/blog/daehl-e/content/post/cross-country/drafts/race-picks-simulation-v2.R  # Test v2
```

**Common Functions to Extract:**
| Function | Lines | Purpose |
|----------|-------|---------|
| `load_env()` | ~20 | Load .env configuration |
| `replace_na_with_quartile()` | ~5 | Fill NAs for feature prep |
| `filter_positive_coefficients()` | ~25 | Remove negative GAM coefficients |
| `calculate_percentage_columns()` | ~15 | Normalize Pelo columns |
| `calculate_weighted_prev_points()` | ~50 | Decay-weighted history |
| `train_points_gam()` | ~70 | Train GAM model |
| `build_athlete_distribution()` | ~140 | Build performance distribution |
| `simulate_race_positions()` | ~100 | Monte Carlo simulation |
| `preprocess_chrono()` | ~12 | Preprocessing pipeline |

**Enhanced Logging to Add:**
- Timing: Start/end timestamps, duration per phase
- Distribution stats: Mean/SD/min/max for athlete distributions
- Data quality: Row counts, NA percentages, outlier detection
- Progress: "Processing athlete 50/120..."
- Validation: Sanity checks (probabilities sum to ~1, etc.)

**Also:** Add logging to existing production scripts (non-breaking changes)

**Progress Update (2026-03-08):**
- ✅ Shared logging utilities created in `content/post/shared/logging-utils.R`
- ✅ Cross-country race-picks script integrated with enhanced logging
- ✅ Enhanced logging now added to all remaining race-picks simulation scripts:
  - `content/post/alpine/drafts/race-picks-simulation.R`
  - `content/post/biathlon/drafts/race-picks-simulation.R`
  - `content/post/nordic-combined/drafts/race-picks-simulation.R`
  - `content/post/skijump/drafts/race-picks-simulation.R`
- Logging coverage now includes:
  - Phase timing
  - Config logging
  - Data quality summaries for chrono/startlist inputs
  - Per-race and per-team progress logging
  - Distribution summary logging
  - Probability validation checks
  - Output file/save summaries
- ✅ Enhanced logging now also added to championship simulation scripts:
  - `content/post/alpine/drafts/champs-predictions-simulation.R`
  - `content/post/biathlon/drafts/champs-predictions-simulation.R`
  - `content/post/nordic-combined/drafts/champs-predictions-simulation.R`
  - `content/post/skijump/drafts/champs-predictions-simulation.R`

**Remaining for Task 2.5:**
- Create/test `race-picks-simulation-v2.R` shared-library rollout script
- Decide whether to migrate common simulation functions out of production scripts or keep logging-only integration for now
- Decide whether to add the same tracer-athlete / tracer-team debug flow used in cross-country to the other sports

**Parameterization for Future Tuning:**
```r
# Each sport will define its config
sport_config <- list(
  sport = "alpine",
  decay_lambda = 0.002,
  sd_scale_factor = 0.77,
  event_params = list(
    "Downhill" = list(decay_lambda = 0.0015),
    "Slalom" = list(decay_lambda = 0.0025)
  )
)
```

### Task 2.6: Parameter Optimization Framework (COMPLETED 2026-03-09)

**Goal:** Create a testing/calibration framework to optimize simulation parameters per sport and race type.

**Why:**
- Current parameters (DECAY_LAMBDA=0.002, SD_SCALE_FACTOR=0.77, etc.) are uniform across all sports
- Different sports likely have different optimal parameters due to varying consistency/variance
- Within sports, different race types may need different tuning (e.g., Sprint vs Distance, Slalom vs Downhill)

**Parameters to Optimize:**
| Parameter | Range | Description |
|-----------|-------|-------------|
| DECAY_LAMBDA | 0.0005 - 0.005 | Exponential decay for historical weighting |
| SD_SCALE_FACTOR | 0.5 - 1.0 | Variance shrinkage (lower = favorites win more) |
| SD_MIN | 2 - 8 | Minimum standard deviation |
| SD_MAX | 10 - 25 | Maximum standard deviation |
| N_HISTORY_REQUIRED | 5 - 20 | Number of historical races per athlete |
| GAM_FILL_WEIGHT_FACTOR | 0.1 - 0.5 | Weight for GAM-filled slots |

**Files to Create:**
```
~/blog/daehl-e/content/post/
  shared/
    sport_params.R              # OUTPUT: Optimized parameters + get_sport_params() helper
  optimization/
    param-optimizer.R           # Main orchestration script
    backtest-engine.R           # Run simulation against historical races
    scoring-metrics.R           # Brier score, log loss, calibration error
    param-grid.R                # Parameter ranges and race type mappings
```

**Scoring Metrics:**
- **Primary: Brier Score** - proper scoring rule, lower is better
- **Secondary:** Log Loss, Calibration Error (ECE)
- **Composite:** 0.5*Brier + 0.3*LogLoss + 0.2*ECE

**Optimization Approach:**
1. **Phase 1: Coarse Grid Search** - ~500 combinations, 200 sims each, identify top 10%
2. **Phase 2: Random Search Refinement** - 50 samples in promising regions, 500 sims each
3. **Phase 3: Final Validation** - 10,000 sims on top candidates, validate on holdout season

**Calibration Data:**
- **Training:** Seasons 2018-2025 (~7 years)
- **Validation:** Season 2025-2026 (holdout)
- **Minimum:** 30 races per race type (fall back to defaults if insufficient)

**Implementation Steps:**
1. [ ] Create `scoring-metrics.R` - Brier score, log loss, calibration error functions
2. [ ] Create `backtest-engine.R` - Run simulation against historical races
3. [ ] Create `param-grid.R` - Parameter ranges and race type mappings
4. [ ] Create `param-optimizer.R` - Grid search, random search, results aggregation
5. [ ] Generate `sport_params.R` - Optimized parameters with `get_sport_params()` helper
6. [ ] Integrate with simulation scripts - Load params from config

**Execution Order:**
1. Cross-Country (most race types, most data)
2. Biathlon (similar complexity)
3. Alpine (4 disciplines)
4. Ski Jumping (individual + team)
5. Nordic Combined (smallest dataset)

**Output Example:**
```r
SPORT_PARAMS <- list(
  "cross-country" = list(
    default = list(decay_lambda = 0.002, sd_scale_factor = 0.77, ...),
    race_types = list(
      "Sprint_C" = list(decay_lambda = 0.003, sd_scale_factor = 0.72),
      "Distance_C_Ms" = list(...),
      ...
    )
  ),
  ...
)

get_sport_params <- function(sport, race_type = NULL) {
  # Returns parameter list, merging defaults with race-type overrides
}
```

### Task 2.7: Optimization Fixes & Data Filtering (COMPLETED 2026-03-11)

**Issues Fixed:**

1. **Offseason Row Filtering** - All race-picks-simulation.R scripts now filter out:
   - `Place == 0` (offseason ELO reset rows)
   - `Distance == "0"` (tour overall standings - cross-country only)
   - Fixed Tour de Ski case mismatch bug (`"Tour De Ski"` vs `"Tour de Ski"`)

2. **Race Type Filter for Skiathlons** - Added `Distance_Ms` race type:
   - Skiathlons (`Technique == "P"`) were not covered by any race type
   - Now included in `Distance_Ms`: `Distance != Sprint/Rel/Ts & MS == 1`
   - Matches how simulation handles skiathlons

3. **SD Parameter Range Testing** - Found optimal ranges through testing:
   - sd_min and sd_max converge but maintain ~8 point gap (differentiation matters)
   - Updated coarse grid: sd_min = 10-24, sd_max = 16-30
   - Tested fixed_sd approach but found per-athlete differentiation still valuable

4. **Optimizer Improvements:**
   - Default now runs both genders: `genders = c("men", "ladies")`
   - Added `test_sd_ranges()` for quick ~5-10 min parameter testing
   - Added `caffeinate` instructions to HOW_TO_RUN.md (prevents Mac sleep)
   - Allow `sd_min == sd_max` (equivalent to fixed_sd)

**Files Modified:**
- `cross-country/drafts/race-picks-simulation.R` - Fixed offseason/tour filtering
- `alpine/drafts/race-picks-simulation.R` - Added offseason filtering
- `biathlon/drafts/race-picks-simulation.R` - Added offseason filtering
- `nordic-combined/drafts/race-picks-simulation.R` - Added offseason filtering
- `skijump/drafts/race-picks-simulation.R` - Added offseason filtering
- `optimization/param-grid.R` - Updated sd_min/sd_max ranges
- `optimization/param-optimizer.R` - Added test_sd_ranges(), both genders default
- `optimization/backtest-engine.R` - Fixed calibration race filtering
- `HOW_TO_RUN.md` - Added caffeinate instructions, both genders examples

### Task 3: Update Race Picks Methodology Documentation (PENDING)

Archive old methodology page and create new Monte Carlo documentation.

---

## Technical Reference

### Simulation Parameters (Optimized Ranges)
```r
# Individual races
N_SIMULATIONS <- 10000
DECAY_LAMBDA <- 0.001 - 0.004  # Optimized per race type (~0.002 typical)
SD_SCALE_FACTOR <- 0.8 - 1.0   # Optimized per race type (~0.9 typical)
SD_MIN <- 10 - 24              # Higher than original (was 4)
SD_MAX <- 16 - 30              # Higher than original (was 16)

# GAM fill for insufficient history
N_HISTORY_REQUIRED <- 10 - 18
GAM_FILL_WEIGHT_FACTOR <- 0.1 - 0.4

# Team events
TEAM_SD_SCALE_FACTOR <- 0.8
TEAM_SD_MIN <- 3
TEAM_SD_MAX <- 12

# Position thresholds
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)  # Win, Podium, Top-5, Top-10, Top-30
```

### Parameter Interpretation
| Parameter | Meaning |
|-----------|---------|
| decay_lambda | Higher = recent form matters more (sprints ~0.003, mass start ~0.001) |
| sd_scale_factor | Multiplier on calculated SD (~1.0 means minimal scaling) |
| sd_min/sd_max | Bounds on athlete uncertainty (higher = more upsets) |
| gam_fill_weight_factor | Weight for GAM-predicted history (lower = less trust in GAM) |

### GAM Feature Sets by Sport

| Sport | Features Used |
|-------|--------------|
| Alpine | prev_points_weighted, Pelo_pct, {Discipline}_Pelo_pct, Speed_Pelo_pct, Tech_Pelo_pct |
| Biathlon | prev_points_weighted, Pelo_pct, Sprint_Pelo_pct, Individual_Pelo_pct, Pursuit_Pelo_pct, MassStart_Pelo_pct |
| Cross-Country | prev_points_weighted, Pelo_pct, Distance_Pelo_pct, Sprint_Pelo_pct, {Technique}_Pelo_pct |
| Nordic Combined | prev_points_weighted, Pelo_pct |
| Ski Jumping | prev_points_weighted, Pelo_pct, Large_Pelo_pct, Normal_Pelo_pct, Flying_Pelo_pct |

### TEST_MODE Configuration
All simulation scripts read TEST_MODE from `~/ski/elo/.env`:
```bash
# ~/ski/elo/.env
TEST_MODE=false  # Set to true for testing
```

---

## Key File Locations

### R Simulation Scripts
```
~/blog/daehl-e/content/post/{sport}/drafts/race-picks-simulation.R
~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions-simulation.R
```

### Data Sources
```
~/ski/elo/python/{sport}/polars/excel365/races.csv
~/ski/elo/python/{sport}/polars/excel365/weekends.csv
~/ski/elo/python/{sport}/polars/excel365/{gender}_chrono_elevation.csv
~/ski/elo/python/{sport}/polars/excel365/startlist_*.csv
```

---

## Session Resume Instructions

If starting a new session:
1. Read this file to understand current status
2. Check the ACTIVE TASKS section for current work
3. **Current:** Running parameter optimization for cross-country (men + ladies)
4. **Next:** Run optimization for other sports, then Task 3 - Update methodology documentation
5. Update this file with any changes made

**To Run Optimization:**
```bash
# Single sport, both genders (~6 hours)
caffeinate -s Rscript -e 'source("~/blog/daehl-e/content/post/optimization/param-optimizer.R"); run_full_optimization(sports = c("cross-country"))'

# Quick parameter test (~5-10 min)
caffeinate -s Rscript -e 'source("~/blog/daehl-e/content/post/optimization/param-optimizer.R"); test_sd_ranges("cross-country", "men")'

# Check progress
tail -f ~/blog/daehl-e/content/post/optimization/logs/optimization_cross-country*.log
```

**Task 2.5 Progress (COMPLETED):**
- [x] Create `shared/logging-utils.R` - Logging utilities (DONE)
- [x] Create `shared/race-picks-common.R` - Common functions (DONE)
- [x] Add logging to cross-country production script (DONE 2026-03-07)
- [x] Fix GAM/distribution filter consistency for Distance_Ms (DONE 2026-03-03)
- [x] Remove dead MS adjustment code (DONE 2026-03-03)
- [x] Add tracer athlete/team logging (DONE 2026-03-03)
- [x] Add logging to other sports (alpine, biathlon, nordic-combined, skijump) (DONE 2026-03-08)
- [ ] Create `cross-country/drafts/race-picks-simulation-v2.R` (deferred to after param optimization)
- [ ] Test v2 against production with test harness (deferred to after param optimization)

**Task 2.6 Progress (COMPLETED 2026-03-09):**
- [x] Create `scoring-metrics.R` - Brier score, log loss, calibration error functions
- [x] Create `backtest-engine.R` - Backtesting engine for historical race evaluation
- [x] Create `param-grid.R` - Parameter ranges and race type mappings for all 5 sports
- [x] Create `param-optimizer.R` - Grid search, random search, parallel optimization
- [x] Generate `sport_params.R` - Template with defaults and `get_sport_params()` helper
- [x] Integrate with simulation scripts - All 5 sports now load params from sport_params.R

**Task 2.7 Progress (COMPLETED 2026-03-11):**
- [x] Fix offseason row filtering in all 5 race-picks-simulation.R scripts
- [x] Fix Tour de Ski summary row filtering (case mismatch bug)
- [x] Add Distance_Ms race type for skiathlons
- [x] Update sd_min/sd_max ranges based on testing (10-24 / 16-30)
- [x] Add test_sd_ranges() for quick parameter testing
- [x] Update run_full_optimization() to include both genders by default
- [x] Add caffeinate instructions to HOW_TO_RUN.md
- [x] Allow sd_min == sd_max in validation (equivalent to fixed_sd)
- [x] Run full optimization for cross-country (men + ladies) - COMPLETED
- [x] Update race-picks-simulation.R to use race-type-specific params
- [x] Update weekly-picks-simulation.R to use race-type-specific params
- [x] Add participation probability calculation to weekly-picks-simulation.R
- [ ] Run full optimization for other sports (alpine, biathlon, nordic-combined, skijump)

**Files Created:**
```
~/blog/daehl-e/content/post/
  shared/
    sport_params.R              # Optimized parameters + get_sport_params() helper
  optimization/
    scoring-metrics.R           # Brier score, log loss, calibration error
    backtest-engine.R           # Run simulation against historical races
    param-grid.R                # Parameter ranges and race type mappings
    param-optimizer.R           # Main orchestration script
```

**Integration:**
All 5 race-picks-simulation.R scripts updated to source sport_params.R and load parameters via `get_sport_params()`. Fallback to hardcoded defaults if sport_params.R unavailable.

**To Run Optimization:**
```r
source("~/blog/daehl-e/content/post/optimization/param-optimizer.R")
# Optimize one sport
results <- optimize_sport("cross-country", verbose = TRUE)
# Optimize all sports and generate updated sport_params.R
run_full_optimization()
```

**Cross-Country Logging Complete (2026-03-07):**
Added comprehensive logging to `cross-country/drafts/race-picks-simulation.R`:
- `log_data_quality` after loading chrono and startlist data
- `log_race_start` at beginning of each race (individual, relay, team sprint, mixed relay)
- `log_distribution_stats` after building athlete/team distributions
- `log_progress` during athlete/team processing loops
- `log_race_results` and `validate_probabilities` after simulations
- `log_gam_training` for GAM models with deviance explained
- `log_tracer_data_load` with race type breakdown and performance trends
- Final summary with race counts and files saved

**Bug Fix (2026-03-03): GAM/Distribution Filter Consistency**
- Problem: GAM trained on `Technique == 'P'` but distribution built from `MS == 1` history
- Solution: Changed GAM filter for `Distance_Ms` to `Distance != 'Sprint' & MS == 1`
- Also removed dead MS adjustment code (never fired with type-specific filters)
- Race type filters now cleanly separate mass start from individual races

**Feature (2026-03-03): Tracer Athlete/Team Logging**
- Added end-to-end tracer logging to follow one athlete/team through entire pipeline
- Uses first ID from startlist (individuals) or first Nation (teams)
- Logs at each stage:
  - **DATA LOAD**: Historical races found, most recent result, date range
  - **GAM PREDICTION**: Race type, key features, prediction, residual SD
  - **DISTRIBUTION BUILDING**: Filtered races, adjustments, final mean/SD
  - **SIMULATION RESULTS**: Predicted rank, win/podium probabilities
- For teams: Logs team composition (members per leg), team mean/SD, simulation results
- Tracer functions added to `shared/logging-utils.R`:
  - `init_tracer()` - Initialize tracer from startlist
  - `is_tracer()` / `is_tracer_team()` - Check if athlete/team is tracer
  - `log_tracer_gam()` - Log GAM prediction details
  - `log_tracer_distribution()` - Log distribution building
  - `log_tracer_simulation()` - Log simulation results
  - `log_tracer_team()` - Log team composition and results
