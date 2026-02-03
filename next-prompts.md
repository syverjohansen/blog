# 2026 Winter Olympics Championship Predictions

## Current Status (2026-02-02)

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
- 7-phase normalization (scale → monotonic → re-scale → cap → final monotonic → start_prob cap → log)
- Exponential decay weighted participation probability
- Nations Excel with Summary sheet and per-nation breakdown
- Clean column names (no underscores)
- Sport-specific Elo columns in skier tables

---

## Pending Tasks

### Apply Two-Phase Normalization to race-picks.R

**Priority:** Medium

**Description:** The two-phase normalization fix applied to champs-predictions.R should also be applied to all race-picks.R files. This ensures:
1. Over-predicted athletes are scaled down fairly before any capping
2. Truly dominant athletes get capped at 100% with excess redistributed
3. Probabilities always sum to the correct target

**Files to Update:**

Individual race files:
- `content/post/alpine/drafts/race-picks.R`
- `content/post/biathlon/drafts/race-picks.R`
- `content/post/cross-country/drafts/race-picks.R`
- `content/post/nordic-combined/drafts/race-picks.R`
- `content/post/skijump/drafts/race-picks.R`

Cross-country relay/team files:
- `content/post/cross-country/drafts/race-picks-relay.R`
- `content/post/cross-country/drafts/race-picks-mixed-relay.R`
- `content/post/cross-country/drafts/race-picks-team-sprint.R`

**Implementation:**
1. Add `normalize_with_cap()` helper function (same as in champs-predictions.R)
2. Update Phase 1 normalization to use two-phase approach
3. Update Phase 3 re-normalization to use two-phase approach

**Reference:** See "Two-Phase Normalization Fix (2026-02-03)" section below for the algorithm.

### Standard Output Format (Target for All Sports)
- **Columns**: Skier, ID, Nation, Start, Win, Podium, Top5, Top-10, Top-30
- **Values**: Percentages (0-100 scale), rounded to 1 decimal
- **Sheet names**: `"N. RaceType - Mon DD"` format (e.g., "1. Sprint Classic - Feb 12")
- **Nations Summary**: Expected medal counts (divide percentage sums by 100)

### Cross-Country Race Naming
- "P" technique = Skiathlon (e.g., "20km Skiathlon")
- "C" technique = Classic (e.g., "10km Classic", "Sprint Classic")
- "F" technique = Freestyle (e.g., "10km Freestyle", "Sprint Freestyle")

---

## Completed Task: Race-Picks Methodology Verification (2026-01-31)

### Task Description
Verified the accuracy of `~/blog/daehl-e/content/post/methods/race-picks.md` methodology documentation and applied corrections based on actual R and Python implementation.

### Files to Review

**Documentation File:**
- `~/blog/daehl-e/content/post/methods/race-picks.md` (1123 lines covering all sports)

**R Source Files (Individual Race Picks):**
- `~/blog/daehl-e/content/post/alpine/drafts/race-picks.R`
- `~/blog/daehl-e/content/post/biathlon/drafts/race-picks.R`
- `~/blog/daehl-e/content/post/cross-country/drafts/race-picks.R`
- `~/blog/daehl-e/content/post/nordic-combined/drafts/race-picks.R`
- `~/blog/daehl-e/content/post/skijump/drafts/race-picks.R`

**R Source Files (Cross-Country Relays):**
- `~/blog/daehl-e/content/post/cross-country/drafts/race-picks-relay.R`
- `~/blog/daehl-e/content/post/cross-country/drafts/race-picks-mixed-relay.R`
- `~/blog/daehl-e/content/post/cross-country/drafts/race-picks-team-sprint.R`

**Python Startlist Scraper Files (Individual):**
- `~/ski/elo/python/alpine/polars/startlist-scrape-race*.py`
- `~/ski/elo/python/biathlon/polars/startlist-scrape-race*.py`
- `~/ski/elo/python/cross-country/polars/startlist-scrape-race*.py`
- `~/ski/elo/python/nordic-combined/polars/startlist-scrape-race*.py`
- `~/ski/elo/python/skijump/polars/startlist-scrape-race*.py`

**Python Startlist Scraper Files (Relays):**
- `~/ski/elo/python/biathlon/polars/relay/`
- `~/ski/elo/python/cross-country/polars/relay/`
- `~/ski/elo/python/nordic-combined/polars/relay/`
- `~/ski/elo/python/skijump/polars/relay/`

### Verification Process (Sport by Sport)

For each sport, verify:

1. **Conceptual Accuracy:**
   - Is the overall approach described correctly?
   - Are the model types (GAM with binomial family) accurately described?
   - Is the feature selection process (BIC) correctly explained?
   - Is the exponential decay participation probability described accurately?

2. **Detailed Accuracy:**
   - Are the input data sources and columns correct?
   - Are the specific features/variables mentioned accurate?
   - Is the normalization process (7-phase) correctly documented?
   - Are sport-specific differences (event types, Elo columns) accurate?
   - Are relay/team variations correctly described?

3. **Cross-Reference Python Scrapers:**
   - What data do the scrapers collect?
   - How does this feed into the R prediction scripts?
   - Are there any data transformations in the scrapers that affect predictions?

### Sports Checklist (COMPLETED 2026-01-31)

- [x] Alpine (Individual only - no team events)
- [x] Biathlon (Individual + Relay)
- [x] Cross-Country (Individual + Relay + Mixed Relay + Team Sprint)
- [x] Nordic Combined (Individual + Team)
- [x] Ski Jumping (Individual + Team + Mixed Team)

### Verification Results Summary

#### Issues Found and FIXED in race-picks.md:

**1. ALL SPORTS - Normalization Section Incomplete**
Documentation describes ~3 phases but code uses 7 phases:
1. Scale to target sum
2. Cap at 100%, redistribute excess
3. Monotonic constraints
4. Re-normalize
5. Cap at 100% again
6. Final monotonic constraint (recently added)
7. Final cap at start_prob (recently added)

**2. Exponential Decay α Value WRONG (4 sports)**
- **Documentation says α = 0.3** for Biathlon, Nordic Combined, Ski Jumping
- **Code uses α = 0.1** for ALL sports
- Only Alpine documentation correctly states α = 0.1

**3. Cross-Country - Period Definition WRONG**
- Documentation describes semantic periods: pre-TdS, TdS, post-TdS, championships, post-championships
- Code uses simple race count buckets: 1-5, 6-10, 11-15, 16-20, 21-25+

**4. Nordic Combined - Multiple Errors**
- Documentation says "Pursuit" Elo, code uses "Sprint_Elo" (no Pursuit in NC)
- Documentation claims NO 75% Elo filter, but code DOES filter by 75%
- Documentation says "IBU site" but NC uses FIS

**5. Ski Jumping - Multiple Errors**
- Elo columns incomplete: missing Small_Elo, Medium_Elo from documentation
- Copy-paste error: lists Nordic Combined disciplines for weighted points
- Claims elevation adjustment but code uses hillsize_adjustment instead

#### Verified as Correct:
- Overall approach (GAM with binomial family, BIC feature selection)
- Alpine: All major claims verified correct
- Biathlon: Elo columns, elevation threshold (1300m), relay methodology
- Cross-Country: Sprint thresholds (1,3,6,12,30), Elo columns, altitude threshold, relay leg structure
- Relay: XGBoost for n>500, GLM for smaller datasets (cross-country)
- Data sources: FIS (alpine, XC, NC, SJ), IBU (biathlon)

### Weekly Recap Improvements (2026-02-02)

**Column Naming Improvements:**
Updated ALL 5 sports' race-recap2.R files to use user-friendly column names:

1. **Elo Change Section:**
   - `Current_Elo` → `Current Elo`
   - `Previous_Week_Elo` → `Previous Elo`
   - `Elo_Change` → `Change`

2. **Magic Numbers Section:**
   - `Current_Place` → `Rank`
   - `Magic_Number` → `Magic #`

**Files Updated:**
- `content/post/alpine/drafts/race-recap2.R`
- `content/post/biathlon/drafts/race-recap2.R`
- `content/post/cross-country/drafts/race-recap2.R`
- `content/post/nordic-combined/drafts/race-recap2.R`
- `content/post/skijump/drafts/race-recap2.R`

**Methodology Documentation:**
Created `content/post/methods/race-recap.md` documenting:
- Elo change tracking methodology
- Monte Carlo season simulation (formula, noise model, iterations)
- Magic number calculation with mathematical definition
- Points remaining calculation per sport
- Technical notes on date handling and edge cases

**Shell Script Update:**
Updated `weekly-recap.sh` to include methodology link in generated posts:
```markdown
*For details on how these predictions are generated, see the [Weekly Recap Methodology](/post/methods/race-recap/).*
```

### Start Probability Testing (2026-02-01) - HISTORICAL

Start probability multiplication and capping was commented out in all 5 champs-predictions.R files for testing purposes. All changes marked with `# NOTE:` comments in the code. This was an experimental change to evaluate the impact of start_prob on predictions.

### Methodology Page Dates (2026-02-01)

Updated all methodology page dates to 2020-01-01 to hide from post feed:
- elo-calculations.md
- ranks.md
- champs-predictions.md
- race-picks.md

### Methodology Documentation Improvements (2026-02-01)

1. **ranks.md** - Converted to use datatable shortcodes for better readability
   - Created JSON data files in `data/methods/ranks/`
   - Tables: base_points, race_modifiers, alpine_points, biathlon_points, crosscountry_points, nordic_combined_points, skijump_points, output_columns

2. **elo-calculations.md** - Fixed and improved
   - Fixed formula: End Elo = Elo x 0.85 + 1300 x 0.15 (not "Pre-race Elo")
   - Added example race showing how Elo changes
   - Added season discount example
   - Created JSON data files in `data/methods/elo/`

3. **ranks-table.html fixes** - Fixed sport-specific columns
   - Alpine: Removed Tour de Ski column (doesn't exist for alpine)
   - Biathlon: Removed Tour de Ski column (doesn't exist for biathlon)
   - Nordic Combined: Removed Tour de Ski column (doesn't exist for NC)
   - Ski Jumping: Replaced Tour de Ski with 4 Hills + Ski Flying WC + World Cup columns

### Elo Calculations Documentation (2026-02-01)

Created `content/post/methods/elo-calculations.md` documenting:
- Core algorithm: Base Elo 1300, multi-player Elo formula E = 1/(1 + 10^((Rj - Ri)/400))
- Dynamic K-factor: max_racers/current_season_racers, capped 1-5
- Season discount: 0.85 regression toward base
- Sport-specific K adjustments:
  - Alpine Combined: K × 0.8
  - Biathlon Relay/Mixed Relay: K / 4, Single Mixed Relay: K / 2
  - Cross-Country Relay: K / 4, Team Sprint: K / 2
  - Nordic Combined Team: K / 4, Team Sprint: K / 2
  - Ski Jumping Team: K / 4
- Pipeline: scrape.py → elo.py → chrono.py (website) or elo_predict.py → chrono_predict.py (predictions)
- Discipline-specific Elos for each sport

### Completed Tasks (2026-02-01)

1. **Ranks Methodology Documentation** - DONE
   - Created `content/post/methods/ranks.md` documenting:
     - Points system: Only top 3 finishes earn points
     - Base points: Olympics (80/40/20), WSC (40/20/10), WC (8/4/4), Standings (80/40/20)
     - Race type modifiers: Individual (1.0×), Team Sprint/SMR (0.5×), Relay/Team (0.25×)
     - Sport-specific events: Tour de Ski, 4 Hills Tournament, Ski Flying WC

2. **Methodology Links in Blog Posts** - DONE
   - `predict_script.sh`: Added race-picks methodology link to weekly-picks, TdS, and race-picks posts
   - `champs_script.sh`: Added champs-predictions methodology link to championship posts
   - Elo pages: Added elo-calculations methodology link to all 10 elo/all-elo pages
   - Ranks pages: Added ranks methodology link to all 5 ranks pages

### Championship Predictions Documentation (2026-01-31, updated 2026-02-02)

Created `content/post/methods/champs-predictions.md` documenting:
- Config-based athlete selection (vs scraped startlists)
- Probability-only predictions (no points)
- 5-phase iterative constrained normalization process (updated 2026-02-02)
- Sport-specific thresholds:
  - Individual: 1, 3, 5, 10, 30
  - Relay/Team: 1, 3, 5, 10 (biathlon, XC, NC) or 1, 3, 5 (ski jumping)
- All sports: α = 0.1 exponential decay, 75% Elo filter

---

## Recent Changes

### Two-Phase Normalization Fix (2026-02-03)

**Issue:** The original iterative constrained normalization had a flaw: if multiple athletes all had raw predictions above 100% (e.g., 5 athletes at 120% each), the algorithm would cap them all at 100% immediately, resulting in a 500% total instead of the target 100%.

**Problem Example:**
- 5 athletes each have raw Win probability of 120% (model over-predicting)
- Old algorithm: Cap all 5 at 100% → Total = 500% (wrong!)
- The issue: When everyone is above cap, there's no one to redistribute excess to

**Solution:** Implemented two-phase `normalize_with_cap()`:
1. **Phase A**: Scale proportionally to target sum FIRST (no capping)
2. **Phase B**: THEN cap at 100% and redistribute excess iteratively

**Algorithm:**
```r
normalize_with_cap <- function(probs, target_sum, max_prob = 100) {
  # Phase A: Scale proportionally first
  current_sum <- sum(probs, na.rm = TRUE)
  if (current_sum > 0) {
    probs <- probs * (target_sum / current_sum)
  }

  # Phase B: Cap and redistribute iteratively
  repeat {
    above_cap <- probs > max_prob
    if (!any(above_cap)) break

    probs[above_cap] <- max_prob
    remaining_target <- target_sum - sum(above_cap) * max_prob
    uncapped_sum <- sum(probs[!above_cap])

    probs[!above_cap] <- probs[!above_cap] * (remaining_target / uncapped_sum)
  }
  return(probs)
}
```

**Why this works:**
- After Phase A, total = target (100% for Win, 300% for Podium, etc.)
- Mathematically, at most `target/100` athletes can exceed 100% after Phase A
- For Win (100%): At most 1 athlete can exceed 100%
- For Podium (300%): At most 3 can exceed 100%
- This guarantees Phase B always has room to redistribute

**Example with 5 athletes at 120% (target = 100%):**
- Phase A: Scale 600% → 100%, each gets 20%
- Phase B: No one above 100%, done
- Result: [20, 20, 20, 20, 20] = 100% ✓

**Files Updated (all 5 sports):**
- `content/post/alpine/drafts/champs-predictions.R`
- `content/post/biathlon/drafts/champs-predictions.R`
- `content/post/cross-country/drafts/champs-predictions.R`
- `content/post/nordic-combined/drafts/champs-predictions.R`
- `content/post/skijump/drafts/champs-predictions.R`

### run_champs_predictions.sh Restructure (2026-02-02)

**Issue:** The shell script was using bash associative arrays (`declare -A`) which failed silently, causing cross-country to run in the wrong directory.

**Debug Output Showed:**
```
>>> RUNNING SCRAPER for alpine <<<
Working directory: /Users/syverjohansen/ski/elo/python/skijump/polars  # WRONG!
```

**Fix:** Replaced associative array with a `case` statement function and restructured to run each sport completely before moving to the next:

```bash
get_elo_folder() {
    case "$1" in
        alpine) echo "alpine" ;;
        biathlon) echo "biathlon" ;;
        cross-country) echo "ski" ;;
        nordic-combined) echo "nordic-combined" ;;
        skijump) echo "skijump" ;;
        *) echo "" ;;
    esac
}
```

**New Structure (per sport):**
1. Step 1: Run elo_predict_script.sh
2. Step 2: Run chrono_predict.py
3. Step 3: Run startlist-scrape-champs.py
4. Step 4: Run champs-predictions.R

**Files Modified:**
- `~/blog/daehl-e/run_champs_predictions.sh`

### Cross-Country Startlist Scraper chrono_pred Fix (2026-02-02)

**Issue:** Team sprint and mixed relay were using `_chrono.csv` (historical data) instead of `_chrono_pred.csv` (prediction data) for startlist generation.

**Files Fixed:**
- `~/ski/elo/python/ski/polars/startlist-scrape-champs.py`
  - Line 308: Team sprint `_chrono.csv` → `_chrono_pred.csv`
  - Lines 538-539: Mixed relay `_chrono.csv` → `_chrono_pred.csv`

**Audit Results (all sports/relay scrapers verified):**
All other startlist scrapers (races, weekend, champs) across all sports and relay directories correctly use `_chrono_pred.csv`.

### Champs-Predictions Methodology Documentation Update (2026-02-02)

Updated `content/post/methods/champs-predictions.md` to document the new iterative constrained normalization approach in the Normalization section. The documentation now describes the 5-phase process with the key insight that athletes at 100% cap are "locked" and excluded from further scaling.

### Magic Number Calculation Bug Fix (2026-02-02)

**Issue:** Cross-country magic number calculation was missing Skiathlon races (technique "P"). The `calculate_remaining_races()` function only handled techniques "", "C", and "F" but not "P".

**Impact:** Magic numbers were undercounted by 100 points per Skiathlon race remaining. For Feb 2, 2026, this meant 800 points calculated instead of 900 points (missing Falun 20P on 03/01).

**Fix:** Added Skiathlon handling in `content/post/cross-country/drafts/race-recap2.R`:
```r
is_distance & technique_clean == "P" & is_world_cup ~ "WC_Distance",  # Skiathlon
is_distance & technique_clean == "P" & is_stage_race ~ "Stage_Distance",  # Skiathlon
```

**Audit Results (all sports checked):**
- Alpine: OK - All disciplines mapped correctly
- Biathlon: OK - All race types mapped correctly
- Cross-Country: FIXED - Skiathlon was missing
- Nordic Combined: OK - All race types mapped correctly
- Ski Jumping: OK - All race types mapped correctly

### Biathlon Champs-Predictions ID Column Fix (2026-02-02)

**Issue:** Biathlon champs-predictions.R was missing the ID column in `prepare_startlist_data()`, causing "Column `ID` doesn't exist" error.

**Fix:** Added ID to the select statement at line 577:
```r
base_df <- startlist %>%
  dplyr::select(Skier, ID, Nation, Price, all_of(race_prob_cols), any_of(elo_cols))
```

### Final Monotonic Constraint in Race Picks (2026-01-31)

**Issue:** After the re-normalization step, small probability inversions could occur (e.g., Top-30 slightly lower than Top-10), which undermines user trust in predictions regardless of how small the difference.

**Fix:** Added a final monotonic constraint check after the last re-normalization step to ensure probabilities always satisfy: Win ≤ Podium ≤ Top5 ≤ Top10 ≤ Top30.

**Files Updated (8 files):**

Individual race files:
- `content/post/alpine/drafts/race-picks.R`
- `content/post/biathlon/drafts/race-picks.R`
- `content/post/cross-country/drafts/race-picks.R`
- `content/post/nordic-combined/drafts/race-picks.R`
- `content/post/skijump/drafts/race-picks.R`

Cross-country relay/team files:
- `content/post/cross-country/drafts/race-picks-relay.R`
- `content/post/cross-country/drafts/race-picks-mixed-relay.R`
- `content/post/cross-country/drafts/race-picks-team-sprint.R`

**Code Pattern Added (after re-normalization block):**
```r
# FINAL MONOTONIC CONSTRAINT CHECK after re-normalization
# This ensures no inversions were introduced by the re-normalization step
log_info("Applying final monotonic constraints after re-normalization...")
for(i in 1:nrow(normalized)) {
  probs <- numeric(length(prob_cols))
  for(j in 1:length(prob_cols)) {
    probs[j] <- normalized[[prob_cols[j]]][i]
  }

  # Apply monotonic adjustment: each probability should be >= previous one
  for(j in 2:length(probs)) {
    if(probs[j] < probs[j-1]) {
      probs[j] <- probs[j-1]  # Set to previous value
    }
  }

  # Update the normalized dataframe
  for(j in 1:length(prob_cols)) {
    normalized[[prob_cols[j]]][i] <- probs[j]
  }
}

# FINAL CAP AT START_PROB: No probability should exceed participation probability
if(race_prob_col %in% names(normalized)) {
  log_info("Applying final cap at start probability...")
  for(prob_col in prob_cols) {
    if(prob_col %in% names(normalized)) {
      # Cap each probability at the participant's start probability (converted to percentage)
      start_probs <- normalized[[race_prob_col]] * 100
      normalized[[prob_col]] <- pmin(normalized[[prob_col]], start_probs)
    }
  }
}
```

**Note:** For relay/team files, teams are confirmed participants (start_prob = 1.0), so the existing cap at 1.0 serves as the start_prob cap.

---

### Column Display Fixes (2026-01-31)

#### small-table.html - Clean Column Names
**Issue:** Columns displayed as "Downhill_Pelo", "Super G_Pelo" instead of "Downhill", "Super G".

**Fix:** Updated `titleMap` and added `formatColumnTitle()` function in all 5 sports.

**Sport-specific titleMaps:**
- **Alpine:** Downhill, Super G, Giant Slalom, Slalom, Combined, Tech, Speed
- **Biathlon:** Overall, Sprint, Pursuit, Individual, Mass Start
- **Cross-Country:** Overall, Distance, Sprint, Distance Classic/Freestyle, Sprint Classic/Freestyle, Classic, Freestyle
- **Nordic Combined:** Overall, Individual, Individual Compact, Sprint, Mass Start
- **Ski Jump:** Overall, Small, Medium, Normal, Large, Flying

#### skier-table.html - Sport-Specific Elo Columns
**Issue:** All sports were using cross-country column definitions, causing missing columns for other sports.

**Fix:** Updated `eloColumns`, `pctColumns`, and `titleMap` for each sport with correct discipline-specific columns.

**Sport-specific eloColumns:**
- **Alpine:** Elo, Downhill_Elo, Super G_Elo, Giant Slalom_Elo, Slalom_Elo, Combined_Elo, Tech_Elo, Speed_Elo
- **Biathlon:** Elo, Individual_Elo, Sprint_Elo, Pursuit_Elo, MassStart_Elo
- **Cross-Country:** (unchanged - already correct)
- **Nordic Combined:** Elo, Individual_Elo, IndividualCompact_Elo, Sprint_Elo, MassStart_Elo
- **Ski Jump:** Elo, Small_Elo, Medium_Elo, Normal_Elo, Large_Elo, Flying_Elo

**Files Updated:**
- `layouts/partials/{sport}/small-table.html` (all 5 sports)
- `layouts/partials/{sport}/skier-table.html` (all 5 sports)

---

### Mobile Display Updates (2026-01-30)

**Files Updated (all 5 sports: alpine, biathlon, cross-country, nordic-combined, skijump):**

#### ranks-table.html
- Hide middle columns on mobile (show only Rank, Skier, Nation, Total)
- Remove sticky from Skier and Nation columns on mobile
- Keep only Rank column sticky

#### small-table.html (Elo pages) - REVISED APPROACH
- Keep all Elo columns visible for horizontal scrolling
- Truncate skier name with ellipsis (max-width: 80px)
- Both Rank and Skier columns stay sticky on mobile
- Hide graph sections on mobile via `.graph-section { display: none; }`
- Wrapped graph partials in `<div class="graph-section">`

#### all-table.html (All-Time Elo pages) - REVISED APPROACH
- Keep all Elo columns visible for horizontal scrolling
- Truncate skier name with ellipsis (max-width: 80px)
- Both Rank and Skier columns stay sticky on mobile
- Hide graph sections on mobile
- Wrapped graph partials in `<div class="graph-section">`

#### skier-table.html (Individual skier pages) - REVISED APPROACH
- Keep all columns visible for horizontal scrolling
- Truncate city name with ellipsis (max-width: 80px)
- Both Date and City columns stay sticky on mobile
- Consistent approach across all 5 sports

#### radar.html (Skier performance radar charts)
- Filter out disciplines with zero values (skier hasn't competed)
- Only show axes for disciplines with actual data
- Show message if fewer than 3 disciplines have data

**CSS Pattern for Truncation (small-table.html, all-table.html):**
```css
@media (max-width: 600px) {
    .bbref-table-wrapper { font-size: 13px; }
    .bbref-table th, .bbref-table td { padding: 5px 6px; }
    .bbref-controls { flex-direction: column; align-items: stretch; }
    .bbref-controls input[type="text"] { width: 100%; }
    /* Truncate skier name with ellipsis on mobile */
    .bbref-table td.sticky-col-2 {
        max-width: 80px;
        overflow: hidden;
        text-overflow: ellipsis;
    }
    .bbref-table th.sticky-col-2 { max-width: 80px; }
    /* Adjust sticky positions for mobile */
    .bbref-table .sticky-col { left: 0; }
    .bbref-table .sticky-col-2 { left: 40px; }
    /* Hide graphs */
    .graph-section { display: none; }
}
```

---

### Ski Jumping Team Predictions Fix (2026-01-30)

**Issue:** Team predictions for ski jumping were being generated in Excel files but not appearing on the site.

**Root Cause:** The `champs_script.sh` looked for files named `men_teams_position_probabilities*.json` (plural "teams" + "position_probabilities"), but ski jumping generates files named `men_team.json` and `mixed_team.json` (singular "team", no suffix).

**Fix:** Updated `champs_script.sh` to also check for ski jumping style naming:
```bash
# Ski jumping style naming (men_team.json, mixed_team.json)
if [[ -z "$men_team" ]]; then
    men_team=$(find "$json_dir" -name "men_team.json" -type f 2>/dev/null | head -1)
fi
if [[ -z "$ladies_team" ]]; then
    ladies_team=$(find "$json_dir" -name "ladies_team.json" -type f 2>/dev/null | head -1)
fi
if [[ -z "$mixed_team" ]]; then
    mixed_team=$(find "$json_dir" -name "mixed_team.json" -type f 2>/dev/null | head -1)
fi
```

**Files Modified:**
- `~/blog/daehl-e/champs_script.sh`

---

### Cross-Country Race Name Expansion (2026-01-29)

**Changes Made:**
1. Added `expand_race_name()` helper function to convert abbreviated race names to full names
2. Technique expansion: "P" → "Skiathlon", "C" → "Classic", "F" → "Freestyle"
3. Distance format: "10" → "10km", "20" → "20km", etc.
4. Examples: "20 P" → "20km Skiathlon", "Sprint C" → "Sprint Classic", "10 F" → "10km Freestyle"

**Files Modified:**
- `~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions.R`

### Nordic Combined Percentage Fix (2026-01-29)

**Issue:** Values were 100x too high (e.g., 10000 instead of 100 for Start)
**Fix:** Removed extra `* 100` multiplication in Excel output - values were already percentages from normalization

**Files Modified:**
- `~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions.R`

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

### Hugo Layout Partials (Tables & Charts)
```
~/blog/daehl-e/layouts/partials/{sport}/ranks-table.html     # All-time rankings
~/blog/daehl-e/layouts/partials/{sport}/small-table.html     # Current Elo table
~/blog/daehl-e/layouts/partials/{sport}/all-table.html       # All-time Elo table
~/blog/daehl-e/layouts/partials/{sport}/skier-table.html     # Individual skier data
~/blog/daehl-e/layouts/partials/{sport}/radar.html           # Performance radar chart
~/blog/daehl-e/layouts/partials/{sport}/men-graph.html       # Men's Elo graph
~/blog/daehl-e/layouts/partials/{sport}/ladies-graph.html    # Ladies' Elo graph
~/blog/daehl-e/layouts/partials/{sport}/men-graph-all.html   # Men's all-time Elo graph
~/blog/daehl-e/layouts/partials/{sport}/ladies-graph-all.html # Ladies' all-time Elo graph
```
Where `{sport}` = alpine, biathlon, cross-country, nordic-combined, skijump

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

### 7-Phase Normalization
1. **Phase 1**: Scale to target sum, cap at 100%, redistribute excess
2. **Phase 2**: Monotonic constraints (Win ≤ Podium ≤ Top5 ≤ Top10 ≤ Top30)
3. **Phase 3**: Re-normalize after constraint adjustments
4. **Phase 4**: Cap at 100% again
5. **Phase 5**: Final monotonic constraint enforcement (ensures no inversions from re-normalization)
6. **Phase 6**: Final cap at start_prob (no probability can exceed participation probability)
7. **Phase 7**: Log final sums for verification

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
