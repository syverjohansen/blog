# Winter Sports Prediction System

## Current Status (2026-03-02)

**IMPORTANT**: Update this file (`next-prompts.md`) whenever changes are made to the codebase.

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

### Task 2.5: Shared R Library & Enhanced Logging (IN PROGRESS)

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

### Task 3: Update Race Picks Methodology Documentation (PENDING)

Archive old methodology page and create new Monte Carlo documentation.

---

## Technical Reference

### Simulation Parameters (All Sports)
```r
# Individual races
N_SIMULATIONS <- 10000
DECAY_LAMBDA <- 0.002     # 50% weight after ~1 year
SD_SCALE_FACTOR <- 0.77   # Lower = favorites win more
SD_MIN <- 4
SD_MAX <- 16

# GAM fill for insufficient history (NEW)
N_HISTORY_REQUIRED <- 10
GAM_FILL_WEIGHT_FACTOR <- 0.25

# Team events
TEAM_SD_SCALE_FACTOR <- 0.8
TEAM_SD_MIN <- 3
TEAM_SD_MAX <- 12

# Position thresholds
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)  # Win, Podium, Top-5, Top-10, Top-30
```

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
3. **Current:** Task 2.5 - Creating shared R library with enhanced logging
4. Approach: Keep production scripts intact, create v2 alongside, test before switchover
5. Update this file with any changes made

**Task 2.5 Progress:**
- [x] Create `shared/logging-utils.R` - Logging utilities (DONE)
- [x] Create `shared/race-picks-common.R` - Common functions (DONE)
- [x] Add logging to cross-country production script (DONE)
- [x] Fix GAM/distribution filter consistency for Distance_Ms (DONE 2026-03-03)
- [x] Remove dead MS adjustment code (DONE 2026-03-03)
- [x] Add tracer athlete/team logging (DONE 2026-03-03)
- [ ] Add logging to other sports (alpine, biathlon, nordic-combined, skijump)
- [ ] Create `cross-country/drafts/race-picks-simulation-v2.R`
- [ ] Test v2 against production with test harness

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
