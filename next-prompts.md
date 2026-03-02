# Winter Sports Prediction System

## Current Status (2026-03-01)

**IMPORTANT**: Update this file (`next-prompts.md`) whenever changes are made to the codebase.

### Pipeline
```
Python Scraper → R Predictions → Excel → JSON → Hugo Blog Post
```

### Sport-by-Sport Status

| Sport | race-picks-simulation.R | champs-predictions-simulation.R | Pipeline Integrated |
|-------|------------------------|--------------------------------|---------------------|
| Alpine | ✓ | ✓ | ✓ |
| Biathlon | ✓ | ✓ | ✓ |
| Cross-Country | ✓ | ✓ | ✓ |
| Nordic Combined | ✓ | ✓ | ✓ |
| Ski Jumping | ✓ | ✓ | ✓ |

**ALL SPORTS NOW USE MONTE CARLO SIMULATION** - Legacy GAM-based scripts are still in place but simulation scripts are the primary prediction method.

---

## ACTIVE TASKS

### Task 1: Code Review of Simulation Scripts
Review all simulation scripts for:
- **Efficiency**: Vectorized operations, minimal redundant calculations
- **Readability**: Clear variable names, logical organization, well-commented sections
- **LLM-friendliness**: Structure that's easy for Claude to parse and understand
- **Correctness**: Proper implementation of Monte Carlo simulation
- **Output consistency**: Verify output matches non-simulation counterparts (file names, directories, columns, worksheets)

**Scripts to Review:**
```
# Race Picks (daily predictions)
content/post/alpine/drafts/race-picks-simulation.R
content/post/biathlon/drafts/race-picks-simulation.R
content/post/cross-country/drafts/race-picks-simulation.R
content/post/nordic-combined/drafts/race-picks-simulation.R
content/post/skijump/drafts/race-picks-simulation.R

# Championships Predictions
content/post/alpine/drafts/champs-predictions-simulation.R
content/post/biathlon/drafts/champs-predictions-simulation.R
content/post/cross-country/drafts/champs-predictions-simulation.R
content/post/nordic-combined/drafts/champs-predictions-simulation.R
content/post/skijump/drafts/champs-predictions-simulation.R

# Cross-Country Special Events
content/post/cross-country/drafts/weekly-picks-simulation.R
content/post/cross-country/drafts/tds-picks-simulation.R
content/post/cross-country/drafts/final_climb-simulation.R
```

**Non-Simulation Counterparts (for output comparison):**
```
content/post/{sport}/drafts/race-picks.R
content/post/{sport}/drafts/champs-predictions.R
content/post/cross-country/drafts/weekly-picks2.R
content/post/cross-country/drafts/tds-picks.R
content/post/cross-country/drafts/final_climb.R
```

### Task 2: Create Testing Harness
Create a testing harness using test_races.csv and test_weekends.csv files:

**Goals:**
- Test all race types for each sport
- Verify correct output file generation
- Enable detailed logging at each step
- Validate data integrity throughout the pipeline
- Document what's happening at each stage

**Test File Locations:**
```
~/ski/elo/python/alpine/polars/excel365/test_races.csv
~/ski/elo/python/alpine/polars/excel365/test_weekends.csv
~/ski/elo/python/biathlon/polars/excel365/test_races.csv
~/ski/elo/python/biathlon/polars/excel365/test_weekends.csv
~/ski/elo/python/ski/polars/excel365/test_races.csv
~/ski/elo/python/ski/polars/excel365/test_weekends.csv
~/ski/elo/python/nordic-combined/polars/excel365/test_races.csv
~/ski/elo/python/nordic-combined/polars/excel365/test_weekends.csv
~/ski/elo/python/skijump/polars/excel365/test_races.csv
~/ski/elo/python/skijump/polars/excel365/test_weekends.csv
```

**Test Scenarios by Sport:**

| Sport | Individual Types | Team Types |
|-------|-----------------|------------|
| Alpine | Downhill, Super G, Giant Slalom, Slalom, Combined | - |
| Biathlon | Sprint, Individual, Pursuit, Mass Start | Relay, Mixed Relay, Single Mixed Relay |
| Cross-Country | Distance, Sprint (C/F variants) | Relay, Team Sprint, Mixed Relay |
| Nordic Combined | Individual, IndividualCompact, Mass Start, Sprint | Team, Team Sprint, Mixed Team |
| Ski Jumping | Large, Normal, Flying | Team Large, Team Normal, Mixed Team |

**Logging Requirements:**
- Log entry/exit of each major function
- Log data dimensions at each transformation step
- Log parameter values used
- Log intermediate results for debugging
- Log output file paths and sizes

### Task 3: Update Race Picks Methodology Documentation
Archive old methodology page and create new Monte Carlo documentation.

**Current File:**
```
content/post/methods/race-picks.md
```

**Tasks:**
1. Archive existing page to `content/post/methods/race-picks-legacy.md`
2. Create new `content/post/methods/race-picks.md` explaining Monte Carlo approach
3. Cover:
   - Monte Carlo simulation basics (10,000 iterations)
   - Exponential decay weighting for historical performance
   - Variance control parameters
   - Position probability calculation
   - Sport-specific considerations (hill size, discipline, technique, etc.)
   - Team event handling

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

# Team events
TEAM_SD_SCALE_FACTOR <- 0.8
TEAM_SD_MIN <- 3
TEAM_SD_MAX <- 12

# Position thresholds
POSITION_THRESHOLDS <- c(1, 3, 5, 10, 30)  # Win, Podium, Top-5, Top-10, Top-30
```

### Points Systems by Sport
```r
# Alpine (top 30)
alpine_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16,
                   15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Biathlon (top 30)
biathlon_regular <- c(90, 75, 65, 60, 55, 50, 46, 43, 40, 37, 34, 32, 30, 28, 26,
                      24, 22, 20, 18, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6)
biathlon_mass_start <- c(90, 80, 70, 60, 55, 50, 46, 43, 40, 37, 34, 32, 30, 28, 26,
                         24, 22, 20, 18, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6)

# Cross-Country (top 30)
xc_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16,
               15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Nordic Combined (top 40)
nc_points <- c(100, 90, 80, 70, 60, 55, 52, 49, 46, 43, 40, 38, 36, 34, 32,
               30, 28, 26, 24, 22, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11,
               10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

# Ski Jumping (top 30)
sj_points <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16,
               15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
```

### Expected Output Structure

**race-picks (daily):**
```
~/blog/daehl-e/content/post/{sport}/drafts/race-picks/YYYYMMDD/
├── men_position_probabilities.xlsx          # Individual races
├── ladies_position_probabilities.xlsx
├── men_team_position_probabilities.xlsx     # Team events (where applicable)
├── ladies_team_position_probabilities.xlsx
├── mixed_team_position_probabilities.xlsx
└── (sport-specific relay/team files)
```

**champs-predictions:**
```
~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions/YYYY/
├── men_position_probabilities.xlsx
├── ladies_position_probabilities.xlsx
├── men.xlsx                                 # Summary
├── ladies.xlsx
├── nations_individual.xlsx                  # Per-nation breakdown
└── (team files for team sports)
```

### TEST_MODE Configuration
All simulation scripts read TEST_MODE from `~/ski/elo/.env`:
```bash
# ~/ski/elo/.env
TEST_MODE=false  # Set to true for testing
```

When `TEST_MODE=true`, scripts use `test_races.csv` and `test_weekends.csv` instead of production files.

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

### Pipeline Scripts
```
~/blog/daehl-e/predict_script.sh           # Daily predictions
~/blog/daehl-e/run_champs_predictions.sh   # Championships predictions
```

---

## Session Resume Instructions

If starting a new session:
1. Read this file to understand current status
2. Check the ACTIVE TASKS section for current work
3. Update this file with any changes made
