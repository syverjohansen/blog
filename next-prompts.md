# 2026 Winter Olympics Championship Predictions

## Current Status (2026-01-29)

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

| Sport | Date Ordering | ID Column | Percentages | Relay Support | Status |
|-------|---------------|-----------|-------------|---------------|--------|
| Alpine | ✅ Complete | ✅ Complete | ✅ Complete | N/A | ✅ Ready |
| Biathlon | ✅ Complete | ✅ Complete | ✅ Complete | ✅ Complete | ✅ Ready |
| Cross-Country | ✅ Complete | ✅ Complete | ✅ Complete | ✅ Complete | ✅ Ready |
| Nordic Combined | ✅ Complete | ✅ Complete | ✅ Complete | ✅ Complete | ✅ Ready |
| Ski Jumping | ✅ Complete | ✅ Complete | ✅ Complete | ✅ Complete | ✅ Ready |

### Features Implemented (All Sports)
- 5-phase normalization (scale → monotonic → re-scale → cap at start_prob → final monotonic)
- Exponential decay weighted participation probability
- Nations Excel with Summary sheet and per-nation breakdown
- Clean column names (no underscores)

### Standard Output Format (Target for All Sports)
- **Columns**: Skier, ID, Nation, Start, Win, Podium, Top5, Top-10, Top-30
- **Values**: Percentages (0-100 scale), rounded to 1 decimal
- **Sheet names**: `"N. RaceType - Mon DD"` format (e.g., "1. Sprint - Feb 12")
- **Nations Summary**: Expected medal counts (divide percentage sums by 100)

---

## Recent Changes

### Ski Jumping Full Update (2026-01-29)

**Changes Made:**
1. **Chronological ordering**: Added `arrange(Date)` before assigning `OriginalRaceNum`
2. **Race date in dataframes**: Added `race_date` column to all race dataframes (men_races, ladies_races, teams)
3. **ID in prepare_startlist_data**: Added ID to base_df select
4. **ID in position_preds**: Added `position_preds$ID <- startlist_prepared$ID`
5. **Start percentage fix**: Multiplied start_prob by 100 in Excel output
6. **ID in Excel output**: Added ID as second column (Skier, ID, Nation, ...)
7. **Sheet naming**: Format `"1. Normal - Feb 20"` with numeric prefix and date
8. **Nations Race column**: Extracts just race type from sheet name
9. **Nations select_and_rename_cols**: Added ID as second column
10. **Nations Summary**: Changed to expected medal counts (sum/100) instead of averages

**Files Modified:**
- `~/blog/daehl-e/content/post/skijump/drafts/champs-predictions.R`

### Nordic Combined Full Update (2026-01-29)

**Changes Made:**
1. **Chronological ordering**: Added `Race_Date` parsing and `arrange(Race_Date)` for all race dataframes
2. **ID in prepare_startlist_data**: Added ID to base_df select
3. **ID in position_preds**: Added `position_preds$ID <- startlist_prepared$ID`
4. **Converted to percentages**: Multiplied all probabilities by 100 in Excel output
5. **Sheet naming**: Format `"1. Individual - Feb 20"` with numeric prefix and date
6. **Nations Race column**: Extracts just race type from sheet name
7. **Nations select_and_rename_cols**: Added ID as second column

**Files Modified:**
- `~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions.R`

### Alpine ID Column (2026-01-29) ✅ Tested

**Changes Made:**
1. **ID in prepare_startlist_data**: Added ID to base_df select (line 337)
2. **ID in position_preds**: Added `position_preds$ID <- startlist_prepared$ID`
3. **ID in Excel output**: Added ID as second column (Skier, ID, Nation, ...)
4. **ID in nations**: Updated `select_and_rename_cols` to include ID

**Files Modified:**
- `~/blog/daehl-e/content/post/alpine/drafts/champs-predictions.R`

### Cross-Country Calendar Date Ordering, ID Column, and Percentages (2026-01-29)

**Changes Made:**
1. **Chronological ordering**: Added `Race_Date` parsing and `arrange(Race_Date)` before assigning `OriginalRaceNum`
2. **Race metadata storage**: Changed `results_list` to store metadata (gender, distance, technique, race_date, race_num) alongside data
3. **Sheet naming**: Format `"1. 10 C - Feb 22"` with numeric prefix and date
4. **ID column**: Added ID as second column (Skier, ID, Nation, ...)
5. **Converted to percentages**: Multiplied all probabilities by 100
6. **Nations Race column**: Extracts just race type from sheet name
7. **Nations Summary**: Divides by 100 to show expected medal counts

**Files Modified:**
- `~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions.R`

### Biathlon Calendar Date Ordering and ID Column (2026-01-29)

**Changes Made:**
1. **Chronological ordering**: Added `arrange(Race_Date)` before assigning `OriginalRaceNum`
2. **Race_Date in dataframes**: Added `race_date` column to all race dataframes
3. **Sheet naming**: Format `"1. Sprint - Feb 12"` with numeric prefix and date
4. **Nations Race column**: Shows just race type (e.g., "Sprint"), not full sheet name
5. **ID column**: Added ID as second column in all Excel outputs (Skier, ID, Nation, ...)
6. **Removed old JSON files**: Deleted `Ladies_Individual`, `Men_Sprint`, etc. from data folder

**Files Modified:**
- `~/blog/daehl-e/content/post/biathlon/drafts/champs-predictions.R`

### Alpine Calendar Date Ordering (2026-01-28)

**Changes Made:**
1. Parse `Race_Date` from weekends.csv
2. Order races chronologically with `arrange(Race_Date)`
3. Sheet naming: `"1. Downhill - Feb 07"` format
4. Nations Race column shows just discipline
5. Blog post formatting in champs_script.sh

**Files Modified:**
- `~/blog/daehl-e/content/post/alpine/drafts/champs-predictions.R`
- `~/blog/daehl-e/champs_script.sh`

---

## Key File Locations

### R Prediction Scripts
```
~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions.R
```
Where `{sport}` = alpine, biathlon, cross-country, nordic-combined, skijump

### Excel Outputs
```
~/blog/daehl-e/content/post/{sport}/drafts/champs-predictions/2026/
```

### JSON Data (for Hugo)
```
~/blog/daehl-e/data/{sport}/drafts/champs-predictions/2026/
```

### Blog Posts
```
~/blog/daehl-e/content/post/champs-predictions/2026/{sport}.md
```

---

## Excel Output Structure

### Individual Position Probabilities (`{gender}_position_probabilities.xlsx`)
- One sheet per race: `"1. Sprint - Feb 12"`, `"2. Individual - Feb 14"`, etc.
- Columns: Skier, ID, Nation, Start, Win, Podium, Top5, Top-10, Top-30
- Values: Percentages (0-100 scale)

### Nations Individual (`nations_individual.xlsx`)
- One sheet per nation with 4+ athletes (e.g., "Norway Men", "France Ladies")
- "Other Men" / "Other Ladies" sheets for nations with <4 athletes
- "Summary" sheet with expected medal counts by nation
- Columns: Athlete, ID, Race, [Nation if Other], Start, Win, Podium, Top5, Top-10, Top-30

### Relay Outputs (`{gender}_relay_position_probabilities.xlsx`)
- Columns: Nation, Team, Start, Win, Podium, Top5, Top-10

---

## Technical Reference

### 5-Phase Normalization
1. **Phase 1**: Scale to target sum, cap at 100%, redistribute excess
2. **Phase 2**: Monotonic constraints + cap at start_prob
3. **Phase 3**: Re-normalize after constraint adjustments
4. **Phase 4**: Final cap at start_prob
5. **Phase 5**: Final monotonic constraint enforcement

### Target Sums (as percentages)
- Individual: Win=100, Podium=300, Top5=500, Top10=1000, Top30=3000
- Relay: Win=100, Podium=300, Top5=500, Top10=1000

### Monotonic Constraint Chain
```
Win ≤ Podium ≤ Top5 ≤ Top10 ≤ Top30 ≤ Start
```

### Exponential Decay Participation
```r
# Time window: later of 5 years ago OR athlete's first race
# Exponential decay (alpha = 0.1) - recent races weighted more heavily
race_weights <- exp(-0.1 * ((n_races - 1):0))
weighted_participation <- sum(participation * race_weights) / sum(race_weights)
```

---

## Running the Pipeline

### 1. Generate Excel predictions
```bash
cd ~/blog/daehl-e/content/post/{sport}/drafts
Rscript champs-predictions.R
```

### 2. Convert Excel to JSON and generate blog posts
```bash
cd ~/blog/daehl-e
./champs_script.sh 2026
```

---

## Session Resume Instructions

If starting a new session:
1. Read this file to understand current status
2. Check the Sport-by-Sport Status table for what needs work
3. Apply changes following the Cross-Country or Biathlon patterns documented above
4. Run the R scripts to regenerate Excel files
5. Run champs_script.sh to update JSON and blog posts
6. **Update this file** with any changes made
