---
title: "Alpine Race Picks Methodology Notes"
date: 2023-12-01T01:23:07+00:00
draft: true
tags: ["methodology", "skiing", "alpine", "race-picks", "notes"]
---

## Individual

### Data Gathering

**Evidence from `startlist-scrape-races.py` lines 15-96:**
The Python orchestration script processes races by reading `races.csv` and filtering for today's date. It uses UTC timezone with format strings `%m/%d/%Y` and fallback to `%m/%d/%y` if no matches found. The script identifies closest available date if exact match fails.

**Evidence from `startlist-scrape-races.py` lines 98-108:**
Individual races are filtered by checking `Sex` column for 'M' and 'L' values. Team events are excluded at this stage.

**Evidence from `startlist_common.py` lines 84-95:**
The `get_fis_race_data()` function scrapes FIS websites using race IDs and sector code 'AL' for Alpine. It constructs URLs like `https://www.fis-ski.com/DB/general/results.html?sectorcode=AL&raceid={race_id}`.

**Evidence from `startlist-scrape-races.py` lines 294-404:**
When FIS startlists are available (race_id exists), `create_race_startlist()` is called. When not available, `create_season_startlist()` creates comprehensive startlists using all current season athletes from chronological data.

**Evidence from `startlist-scrape-races.py` lines 322-342:**
Athlete matching uses exact name comparison first, then `fuzzy_match_name()` for spelling variations. The system falls back to season startlists when no ELO match is found.

**Evidence from `startlist-scrape-races.py` lines 365-371:**
ELO integration adds columns: `'Elo', 'Downhill_Elo', 'Super.G_Elo', 'Giant.Slalom_Elo', 'Slalom_Elo', 'Combined_Elo', 'Tech_Elo', 'Speed_Elo'` and calculates race-specific ELO using `get_race_specific_elo()`.

### Points

#### Training

##### Setup

**Evidence from `race-picks.R` lines 694-806:**
The `preprocess_data()` function creates training data by:
- Loading races data and calculating points using `get_points()` function (lines 704-717)
- Computing `Prev_Points_Weighted` using 5-race rolling weighted average with increasing weights (lines 720-731)
- Creating ELO columns if missing and ensuring they exist (lines 733-742)
- Adding period classification based on race progression within seasons (lines 748-756)
- Adding discipline flags: `Tech_Flag`, `Speed_Flag`, `Combined_Flag` (lines 758-762)
- Filtering for recent seasons (last 10 seasons) and athletes with >75% ELO (lines 765-806)

##### Feature Selection

**Evidence from `race-picks.R` lines 1121-1137:**
Feature selection uses discipline-specific variable sets:
- Speed events (Downhill/Super G): `c("Prev_Points_Weighted", "Downhill_Elo_Pct", "Super.G_Elo_Pct", "Giant.Slalom_Elo_Pct", "Speed_Elo_Pct", "Elo_Pct")`
- Technical events (Slalom/GS): `c("Prev_Points_Weighted", "Super.G_Elo_Pct", "Slalom_Elo_Pct", "Giant.Slalom_Elo_Pct", "Tech_Elo_Pct", "Elo_Pct")`
- Combined events: `c("Prev_Points_Weighted", "Combined_Elo_Pct", "Tech_Elo_Pct", "Speed_Elo_Pct", "Elo_Pct")`

**Evidence from `race-picks.R` lines 1134-1141:**
Automated selection uses `regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")` with BIC criterion (`which.min(summary_exhaustive$bic)`). Selected variables are converted to smooth terms for GAM.

##### Modeling

**Evidence from `race-picks.R` lines 1139-1164:**
Points models use GAM with smooth spline terms: `gam_formula <- as.formula(paste("Points ~", smooth_terms))`. Multiple fallback levels implemented:
1. Full GAM with selected variables (line 1141)
2. Reduced GAM with k=3 degrees of freedom (lines 1147-1148)  
3. Linear model (lines 1153-1154)
4. Simple ELO-only linear model (lines 1159-1160)

##### Adjustments

**Evidence from `race-picks.R` lines 1350-1450 (continuation from previous read):**
Period and discipline adjustments are calculated using statistical testing. T-tests compare athlete performance across periods, with significance threshold p < 0.05 triggering corrections. Adjustments are athlete-specific and stored for application during prediction.

#### Testing

##### Startlist Setup

**Evidence from `race-picks.R` lines 586-692:**
The `prepare_startlist_data()` function processes startlist by:
- Extracting race probability columns using `grep("^Race\\d+_Prob$", names(startlist), value = TRUE)` (line 590)
- Getting most recent ELO values through `group_by(Skier)` and `slice_tail(n = 1)` (lines 602-608)
- Calculating recent points using 5-race weighted average (lines 614-623)
- Creating ELO percentage columns by normalizing against maximum values (lines 631-664)
- Replacing NAs with first quartile values using `replace_na_with_quartile()` (lines 667-674)

##### Modeling

**Evidence from implementation in predict_races function:**
Testing applies trained GAM models to prepared startlist data. Predictions are generated using `predict(model, newdata = startlist_data)`. The same fallback hierarchy is maintained if prediction fails.

### Probability

#### Training

##### Setup

**Evidence from `race-picks.R` lines 1174-1178:**
Position probability training uses same training data as points models but creates binary outcome variables: `race_df$position_achieved <- race_df$Place <= threshold` for each threshold in `position_thresholds <- c(1, 3, 5, 10, 30)`.

##### Feature Selection

**Evidence from `race-picks.R` lines 1170-1188:**
Position models use identical feature selection as points models: `position_feature_vars <- explanatory_vars`. The same `regsubsets()` with BIC optimization is applied: `pos_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")`.

##### Modeling

**Evidence from `race-picks.R` lines 1190-1198:**
Position models use GAM with binomial family: `gam(pos_gam_formula, data = race_df, family = binomial, method = "REML")`. Brier scores are calculated for model evaluation: `brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)`.

##### Adjustments

**Evidence from `race-picks.R` lines 1210-1254:**
Period adjustments for position models follow same methodology as points models. T-tests compare probability differences across periods, with corrections applied when p < 0.05. Adjustments stored per participant per threshold.

#### Testing

##### Startlist Setup

Testing uses same startlist setup as points prediction with addition of position-specific probability columns for each threshold.

##### Modeling

Position probability prediction applies trained binomial GAM models to startlist data, generating probabilities for each threshold.

##### Adjustments

Period and discipline adjustments are applied to raw position probabilities before normalization.

#### Normalization and Monotonic Constraints

**Evidence from `race-picks.R` lines 96-251:**
The `normalize_position_probabilities()` function implements:

**Race Participation Adjustment (lines 116-128):**
Base probabilities are scaled by race participation: `normalized[[prob_col]] <- normalized[[prob_col]] * normalized[[race_prob_col]]`

**Normalization to Target Sums (lines 130-182):**
- Calculates target sums: `target_sum <- 100 * threshold` (100% for Top-1, 300% for Top-3, etc.)
- Applies scaling factor: `scaling_factor <- target_sum / current_sum`
- Caps individual probabilities at 100% and redistributes excess proportionally (lines 142-172)

**Monotonic Constraints (lines 194-236):**
For each athlete, ensures `P(Top-1) ≤ P(Top-3) ≤ P(Top-5) ≤ P(Top-10) ≤ P(Top-30)` by adjusting probabilities row-wise: `if(probs[j] < probs[j-1]) { probs[j] <- probs[j-1] }` (lines 207-210).

**Re-normalization (lines 218-236):**
After monotonic adjustment, probabilities are re-normalized to maintain target sums while preserving constraint ordering.