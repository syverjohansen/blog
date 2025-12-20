---
title: "Biathlon Race Picks Methodology Notes"
date: 2023-12-01T01:23:07+00:00
draft: true
tags: ["methodology", "skiing", "biathlon", "race-picks", "notes"]
---

## Individual

### Data Gathering

**Evidence from `startlist-scrape-races.py` lines 15-50:**
The Python script processes races by reading `~/ski/elo/python/biathlon/polars/excel365/races.csv` and filtering for today's date using UTC timezone with format `%m/%d/%Y` and fallback to `%m/%d/%y`. Date comparison: `today_races = races_df[races_df['Date'] == today_str]`.

**Evidence from `startlist-scrape-races.py` lines 102-112:**
Individual races are separated from relay events by filtering `RaceType` column: `individual_races = races_df[~races_df['RaceType'].isin(['Relay', 'Mixed Relay', 'Single Mixed Relay'])]` and `relay_races = races_df[races_df['RaceType'].isin(['Relay', 'Mixed Relay', 'Single Mixed Relay'])]`.

**Evidence from `startlist_common.py` lines 89-149:**
The `get_biathlon_startlist()` function scrapes IBU website using BeautifulSoup to extract JSON data from script tags containing `"Results":[` pattern. Athlete data extracted: `'Name', 'FamilyName', 'GivenName', 'Nation', 'IBUId', 'Bib', 'StartOrder'`.

**Evidence from `startlist-scrape-races.py` lines 248-355:**
When startlists are unavailable, `create_season_startlist()` uses chronological data from `~/ski/elo/python/biathlon/polars/excel365/{gender}_chrono.csv`. Current season athletes identified: `current_season = chrono_df['Season'].max()` and `current_season_df = chrono_df[chrono_df['Season'] == current_season]`.

**Evidence from `startlist-scrape-races.py` lines 388-444:**
ELO integration adds race-type specific columns: `['Elo', 'Individual_Elo', 'Sprint_Elo', 'Pursuit_Elo', 'MassStart_Elo']` using `get_race_specific_elo()` with priority mapping: Individual→Individual_Elo, Sprint→Sprint_Elo, Pursuit→Pursuit_Elo→Sprint_Elo, Mass Start→MassStart_Elo→Pursuit_Elo.

**Evidence from `startlist_common.py` lines 314-329:**
Missing values replaced with first quartile: `q1_values[col] = latest_scores[col].astype(float).quantile(0.25)` and `latest_scores[col] = latest_scores[col].fillna(q1_values.get(col, 1000))`.

### Points

#### Training

##### Setup

**Evidence from `race-picks.R` lines 13-15:**
Two points systems defined: `regular_points <- c(90,75,65,55,50,45,41,37,34,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)` (40 finishers) and `mass_start_points <- c(90,75,65,55,50,45,41,37,34,31,30,29,28,27,26,25,24,23,22,21,20,18,16,14,12,10,8,6,4,2)` (30 finishers).

**Evidence from biathlon `preprocess_data()` function:**
Training data filtering includes last 10 seasons: `filter(Season >= max(Season, na.rm = TRUE) - 9)` and top performers: `filter(get(pelo_col) > 0.75 * max(get(pelo_col), na.rm = TRUE))`. Weighted previous points calculated using 5-race rolling average with increasing weights for recent races.

**Evidence from chronological data loading:**
Period classification divides seasons into 4 periods based on race progression. Elevation flags created for races ≥1300m altitude using `Elevation_Flag = ifelse(Elevation >= 1300, 1, 0)`.

##### Feature Selection

**Evidence from biathlon feature selection logic:**
Race-type specific variable sets include:
- Sprint races: `c("Prev_Points_Weighted", "Sprint_Pelo_Pct", "Pelo_Pct")`
- Individual races: `c("Prev_Points_Weighted", "Individual_Pelo_Pct", "Pelo_Pct")`
- Pursuit races: `c("Prev_Points_Weighted", "Pursuit_Pelo_Pct", "Sprint_Pelo_Pct", "Pelo_Pct")`
- Mass Start races: `c("Prev_Points_Weighted", "MassStart_Pelo_Pct", "Pursuit_Pelo_Pct", "Pelo_Pct")`

**Evidence from feature selection implementation:**
Uses `regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")` with BIC optimization: `best_bic_vars <- names(coef(selection, which.min(summary_exhaustive$bic)))`.

##### Modeling

**Evidence from GAM modeling:**
Points models use `gam(gam_formula, data = race_df, method = "REML")` with smooth terms: `smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")`. Fallback hierarchy includes: full GAM → reduced complexity GAM → linear regression → PELO-only model.

##### Adjustments

**Evidence from adjustment calculations:**
Period and elevation adjustments use t-tests: `period_p = purrr::map_dbl(row_id, function(r) { t.test(prior_period_curr, prior_period_other)$p.value })` with significance threshold p < 0.05. Corrections calculated as `period_correction = ifelse(period_p < 0.05, mean(Prediction_Diff[Period == Period]), 0)`.

#### Testing

##### Startlist Setup

**Evidence from startlist preparation:**
Race probability columns extracted using `race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)`. PELO percentages calculated by normalizing against maximum: `startlist_with_pct[[elo_pct_col]] <- 100 * startlist_with_pct[[elo_col]] / max(startlist_with_pct[[elo_col]], na.rm = TRUE)`. Missing values replaced with quartile imputation.

##### Modeling

**Evidence from prediction application:**
Trained GAM models applied to prepared startlist using `predict(model, newdata = startlist_data)`. Period and elevation adjustments added: `Adjusted_Prediction = Initial_Prediction + period_correction + elevation_correction`. Final predictions weighted by race participation probability.

##### Adjustments

**Evidence from `race-picks.R` lines 2314-2344:**
Testing phase applies learned adjustments to startlist predictions: `startlist_prepared %>% left_join(skier_adjustments, by = participant_col) %>% mutate(period_adjustment = period_effect, elevation_adjustment = elevation_effect, Predicted_Points = Base_Prediction + period_adjustment + elevation_adjustment)`. Final predictions capped between 0-100 points.

### Probability

#### Training

##### Setup

**Evidence from position threshold creation:**
Binary outcome variables created for thresholds `c(1, 3, 5, 10, 30)`: `race_df$position_achieved <- race_df$Place <= threshold`. Same training data and filtering as points models.

##### Feature Selection

**Evidence from position model features:**
Uses identical feature selection as points models with same race-type specific variables and BIC optimization through `regsubsets()`.

##### Modeling

**Evidence from binomial GAM:**
Position models use `gam(pos_gam_formula, data = race_df, family = binomial, method = "REML")` with smooth terms for selected variables. Brier score evaluation: `brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)`.

##### Adjustments

**Evidence from position adjustments:**
Period adjustments calculated using probability residuals: `prob_diff = as.numeric(position_achieved) - initial_prob`. T-test methodology identical to points models with p < 0.05 threshold for applying corrections.

#### Testing

##### Startlist Setup

Testing uses same startlist setup as points prediction with addition of position-specific probability columns for each threshold.

##### Modeling

Position probability prediction applies trained binomial GAM models to startlist data, generating probabilities for each threshold.

##### Adjustments

**Evidence from `race-picks.R` lines 2233-2273:**
Testing applies position-specific adjustments: `position_preds %>% left_join(pos_adj, by = participant_col) %>% mutate(period_adjustment = period_effect, adjusted_prob = get(paste0(prob_col, "_base")) + period_adjustment, adjusted_prob = pmin(pmax(adjusted_prob, 0), 1))`. Probabilities constrained between 0 and 1.

#### Normalization and Monotonic Constraints

**Evidence from normalization implementation:**
Race participation adjustment: probabilities scaled by participation probability. Normalization ensures target sums (100% for Top-1, 300% for Top-3, etc.) with individual caps at 100%. Monotonic constraints enforce P(Top-1) ≤ P(Top-3) ≤ P(Top-5) ≤ P(Top-10) ≤ P(Top-30) followed by re-normalization to maintain target sums.

## Relay

### Data Gathering

**Evidence from `startlist_scrape_races_relay.py` lines 15-51:**
Regular relay races filtered by `RaceType == 'Relay'` with gender separation using `Sex` column ('M' for men, 'L' for ladies). Script processes team startlists from IBU website.

**Evidence from `startlist_scrape_races_mixed_relay.py` lines 15-55:**
Mixed relay races identified by `RaceType == 'Mixed Relay'` without gender separation since teams contain both male and female athletes.

**Evidence from `startlist_common.py` lines 151-229:**
The `get_biathlon_relay_teams()` function extracts team data using same JSON parsing as individual races. Team entries identified by `entry.get('IsTeam', False)` and individual leg athletes by `entry.get('Leg') is not None`.

**Evidence from mixed relay team processing lines 137-352:**
Team members processed with gender detection through ELO database matching. Function attempts exact match in both men's and women's databases, uses fuzzy matching as fallback, then determines gender based on match scores: `if men_match_score >= women_match_score: use men's database`.

### Points

#### Training

##### Setup

**Evidence from `race-picks.R` lines 1838-1867:**
Relay training uses team-aggregated ELO features: `explanatory_vars <- c("Avg_Sprint_Pelo_Pct", "Avg_Individual_Pelo_Pct", "Avg_MassStart_Pelo_Pct", "Avg_Pursuit_Pelo_Pct", "Avg_Pelo_Pct")`. Team data created by averaging individual member ELO scores across relay positions.

##### Feature Selection

**Evidence from relay feature selection:**
Uses `regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")` with BIC optimization: `best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))`. Team-level averaging variables replace individual PELO percentages.

##### Modeling

**Evidence from GAM modeling:**
Team points models use same GAM approach as individual: `gam(gam_formula, data = race_df_75)` with smooth terms: `smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")`. Fallback hierarchy maintained for team modeling.

##### Adjustments

**Evidence from team adjustments:**
Period and elevation adjustments calculated using same t-test methodology as individuals but applied to team (nation) level performance patterns. Team-specific correction factors stored and applied during prediction.

#### Testing

##### Startlist Setup

**Evidence from `race-picks.R` lines 1600-1670:**
Relay startlist setup uses nation as participant: `participant_col <- "Nation"`. Chronological data paths set by relay type: men's relay uses `"~/ski/elo/python/biathlon/polars/relay/excel365/men_relay_chrono.csv"`, ladies' relay uses corresponding ladies file.

##### Modeling

**Evidence from relay prediction application:**
Trained team GAM models applied to relay startlists using same prediction pipeline as individuals but with team-aggregated features. Nation-based predictions generated with team ELO averages as input variables.

### Probability

#### Training

##### Setup

**Evidence from `race-picks.R` lines 1923-1962:**
Relay position probability training uses same binary outcome approach: `race_df$position_achieved <- race_df$Place <= threshold` for thresholds `c(1, 3, 5, 10, 30)`. Team finishing positions used instead of individual athlete positions.

##### Feature Selection

**Evidence from relay position features:**
Uses identical feature selection as relay points models: `position_feature_vars <- explanatory_vars` with team-aggregated PELO variables. BIC optimization through `regsubsets()` for each position threshold.

##### Modeling

**Evidence from team binomial GAM:**
Team position models use `gam(pos_gam_formula, data = race_df, family = binomial, method = "REML")` with team-aggregated smooth terms. Brier score evaluation applied to team-level predictions.

##### Adjustments

**Evidence from team position adjustments:**
Period adjustments calculated using team probability residuals: `prob_diff = as.numeric(position_achieved) - initial_prob` at nation level. T-test methodology applied to team performance patterns across periods.

#### Testing

##### Startlist Setup

Testing uses same relay startlist setup as points prediction with addition of team position-specific probability columns for each threshold.

##### Modeling

Team position probability prediction applies trained binomial GAM models to relay startlist data, generating probabilities for each threshold at nation level.

#### Normalization and Monotonic Constraints

**Evidence from `race-picks.R` lines 1040-1194:**
Relay normalization uses same function as individuals: `normalize_position_probabilities()` with race participation adjustment and target sum enforcement. Monotonic constraints applied: `for(j in 2:length(probs)) { if(probs[j] < probs[j-1]) { probs[j] <- probs[j-1] }}`. Re-normalization after constraints maintains target sums (100% for Top-1, 300% for Top-3, etc.).