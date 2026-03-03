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

### Task 1.5: Complete GAM Support (COMPLETED 2026-03-02)

All simulation scripts now have GAM support for athletes with insufficient history.

### Task 2: Create Testing Harness (PENDING)

Create a testing harness using test_races.csv and test_weekends.csv files.

**Goals:**
- Test all race types for each sport
- Verify correct output file generation
- Enable detailed logging at each step
- Validate data integrity throughout the pipeline

**Test File Locations:**
```
~/ski/elo/python/alpine/polars/excel365/test_races.csv
~/ski/elo/python/biathlon/polars/excel365/test_races.csv
~/ski/elo/python/ski/polars/excel365/test_races.csv
~/ski/elo/python/nordic-combined/polars/excel365/test_races.csv
~/ski/elo/python/skijump/polars/excel365/test_races.csv
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
3. Task 1.5 (GAM support) is complete - all scripts have GAM-based fill
4. Next task: Create testing harness (Task 2)
5. Update this file with any changes made
