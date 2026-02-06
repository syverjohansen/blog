---
title: "Championship Predictions Methodology"
date: 2020-01-01T01:00:00+00:00
draft: false
tags: ["methodology", "skiing", "championships", "olympics", "world-championships"]
---

## Overview

Championship predictions use the same core methodology as race-picks but with key differences:

1. **Probability only** - No points predictions are made for championships
2. **Config-based athletes** - Athletes are manually configured rather than scraped from startlists
3. **Championship filter** - Races marked with `Championship == 1` in the schedule are processed

The prediction pipeline is: Config Athletes → Elo Matching → Probability Training → Normalization → Output

## Alpine

### Individual

#### Data Gathering

Championship athletes are configured in `config.py` by nation rather than scraped from FIS. Each nation has a list of qualified athletes for World Championships or Olympics. The race schedule is read from `weekends.csv` and filtered for races where `Championship == 1`.

Athletes are matched with their most recent Elo scores for Overall, Downhill, Super-G, Giant Slalom, Slalom, Speed, and Technical disciplines.

#### Probability

##### Training

###### Setup

Historical race data is preprocessed to create binary classification outcomes for each position threshold. The training data is filtered to the last 10 seasons and to skiers with an Elo percentage of 75% or greater.

Position thresholds are Win (1), Podium (3), Top 5, Top 10, and Top 30. Each threshold creates a separate binary classification problem.

Elo scores are converted to percentages of the maximum for each race. Weighted previous points use the last 5 discipline-specific results with linear weights (most recent = 5, oldest = 1).

###### Feature Selection

Feature selection uses BIC optimization via `regsubsets` for each position threshold independently. Speed events (Downhill, Super-G) use speed-related Elo columns. Technical events (Slalom, Giant Slalom) use technical-related Elo columns.

###### Modeling

Binomial GAM models with REML estimation are trained for each position threshold. Brier scores validate probabilistic accuracy. Fallback models use simplified formulas if the full model fails.

###### Adjustments

Period adjustments are calculated during training based on performance differences across World Cup stages. Adjustments require statistical significance (p < 0.05) and minimum sample sizes.

##### Testing

###### Startlist Setup

Athletes from the config are matched with current Elo scores. Elo percentages are calculated by dividing by the startlist maximum. Missing values are imputed with first quartile values.

Race participation probabilities use exponential decay (α = 0.1) based on recent race participation. Configured championship athletes receive 1.0 probability.

###### Modeling

The trained binomial GAM models are applied to generate probability predictions for each position threshold.

###### Adjustments

Period adjustments from training are applied to predictions where statistically significant.

#### Normalization and Monotonic Constraints

A 5-phase normalization process is applied using **two-phase constrained normalization**:

1. **Phase 1 - Two-Phase Constrained Normalization**:
   - **Phase A**: Scale all probabilities proportionally to target sum (100% for win, 300% for podium, etc.)
   - **Phase B**: Cap athletes above 100% and redistribute excess iteratively
   - Process iterates until no athletes exceed cap

2. **Phase 2 - Monotonic Constraints**: Ensures Win ≤ Podium ≤ Top 5 ≤ Top 10 ≤ Top 30

3. **Phase 3 - Re-normalize (Two-Phase Constrained)**:
   - Same two-phase approach as Phase 1
   - Adjusts for any sum changes from monotonic constraints

4. **Phase 4 - Final Participation Cap**: No probability can exceed athlete's participation probability

5. **Phase 5 - Final Monotonic Enforcement**: Second pass to ensure no inversions from re-normalization

**Why Two-Phase Normalization?**

The key insight is that raw model predictions may be over-inflated (e.g., multiple athletes at 120% win probability). The two-phase approach handles this correctly:

1. **Phase A** scales everyone proportionally first, bringing the total to the target
2. **Phase B** then caps any athletes still above 100% and redistributes excess

This ensures:
- Over-predicted athletes are scaled down fairly before capping
- Truly dominant athletes (above 100% after scaling) get capped and excess redistributed
- Probabilities always sum to the correct target

**Mathematical Guarantee:** After Phase A scaling, at most `target/100` athletes can exceed 100%. For Win (target=100%), at most 1 athlete can exceed 100%. For Podium (target=300%), at most 3 can exceed 100%. This guarantees Phase B always has room to redistribute excess.

## Biathlon

### Individual

#### Data Gathering

Championship athletes are configured in `config.py` by nation. Race schedule filtered from `weekends.csv` for `Championship == 1`. Athletes matched with Elo scores for Overall, Sprint, Pursuit, Individual, and Mass Start.

#### Probability

##### Training

Training data filtered to last 10 seasons and 75% Elo threshold. Position thresholds: Win, Podium, Top 5, Top 10, Top 30. BIC feature selection per threshold. Binomial GAM models with REML. Period and elevation (≥1300m) adjustments calculated.

##### Testing

Elo percentages from startlist maximum. Exponential decay (α = 0.1) for participation probability. Trained models applied with adjustments.

#### Normalization and Monotonic Constraints

Same 5-phase process as Alpine.

### Relay

#### Data Gathering

Relay teams configured by nation in `config.py` with team member lists. Team Elo scores calculated as averages of individual members.

#### Probability

##### Training

Position thresholds reduced to Win, Podium, Top 5, Top 10 for relay events. BIC feature selection on team average Elos. Binomial GAM models trained per threshold.

##### Testing

Team average Elos calculated from configured rosters. Models applied to generate team probability predictions.

#### Normalization and Monotonic Constraints

Same 5-phase process with relay-specific thresholds.

## Cross-Country

Cross-country skiing predictions use a **Monte Carlo simulation approach** that differs from other sports. This approach naturally handles varying field sizes without normalization artifacts and produces more calibrated probability distributions.

### Individual

#### Data Gathering

Championship athletes configured by nation. Race schedule filtered for `Championship == 1`. Athletes matched with Elo scores for Overall, Distance, Distance Classic, Distance Freestyle, Sprint, Sprint Classic, Sprint Freestyle, Classic, and Freestyle.

#### Probability

##### Training

Training data filtered to last 10 seasons.

Position thresholds: Win, Podium, Top 5, Top 10, Top 30 for all individual events.

**Feature Selection** uses BIC optimization via `regsubsets` with a **positive coefficient constraint**. Features that receive negative coefficients in the model are excluded, as all features should be positive indicators of performance. This prevents counterintuitive situations where higher Elo predicts lower probability.

**Exponential Decay Weighting**: Historical races are weighted using exponential decay based on date:
- Formula: `weight = exp(-DECAY_LAMBDA * days_ago)`
- DECAY_LAMBDA = 0.002 gives approximately 50% weight after 1 year
- More recent races have greater influence on predictions

##### Testing

For each athlete, a performance distribution is built by:

1. **Historical Performance**: Recent discipline-specific race results weighted by exponential decay
2. **GAM Prediction**: Expected points from the trained model
3. **Variance Estimation**: Standard deviation based on historical variability, scaled and bounded

**Monte Carlo Simulation**: 10,000 race simulations are run where each athlete samples from their distribution. Position is determined by ranking all samples. Position probabilities are the proportion of simulations where each athlete achieved each threshold.

**Variance Control Parameters** (calibrated via Brier score grid search):
- SD_SCALE_FACTOR: Scales athlete variance (lower = favorites win more)
- SD_MIN/SD_MAX: Bounds on variance to prevent extreme values

#### Normalization

Simulation naturally produces correctly-summed probabilities (exactly 1 winner, 3 podium spots, etc.). No post-hoc normalization is required, avoiding artifacts from iterative capping/redistribution.

### Relay

#### Data Gathering

Relay teams configured by nation with leg assignments. Classic legs (1-2) and Freestyle legs (3-4) use appropriate technique-specific Elos.

#### Team Selection

**Podium-Optimized Selection**: Teams are selected to maximize podium probability (threshold = 3) using leg-specific models. The selection algorithm:
1. Trains leg-specific binomial GAMs predicting P(team podium | athlete on leg X)
2. Uses combinatorial optimization to find the 4-athlete lineup maximizing team podium probability
3. Considers athlete availability and leg-appropriate technique

#### Probability

##### Hybrid Approach (GAM + Simulation)

Cross-country relay uses a hybrid approach combining production models with simulation:

1. **Leg-Specific GAM Models**: Binomial GAMs are trained for each leg position predicting P(team podium | athlete on this leg)
2. **Leg Importance Calculation**: Model deviance explained determines relative importance of each leg position
3. **Team Distribution**: Athlete predictions are combined with importance weights to create team score distributions
4. **Monte Carlo Simulation**: Teams are ranked across 10,000 simulations

**Leg Features** (matching technique requirements):
- Legs 1-2 (Classic): `prev_points_weighted, Pelo_pct, Distance_Pelo_pct, Distance_C_Pelo_pct, Classic_Pelo_pct, Sprint_Pelo_pct, Sprint_C_Pelo_pct`
- Legs 3-4 (Freestyle): `prev_points_weighted, Pelo_pct, Distance_Pelo_pct, Distance_F_Pelo_pct, Freestyle_Pelo_pct, Sprint_Pelo_pct, Sprint_F_Pelo_pct`

##### Variance Control

Relay-specific variance parameters (calibrated separately from individual races):
- RELAY_SCORE_SD_MIN/MAX: Bounds on team score variance

#### Output

Team probabilities for Win, Podium, Top 5, Top 10 from simulation. Leg-level probabilities are derived by scaling the leg model predictions by team-level threshold ratios.

### Team Sprint

#### Data Gathering

Team sprint pairs configured by nation. Each team has two athletes alternating legs.

#### Technique-Specific Models

Unlike 4-leg relay, team sprint uses **technique-specific features and models**:
- **Classic Team Sprint (C)**: Uses `Sprint_C_Pelo_pct, Classic_Pelo_pct, Distance_C_Pelo_pct`
- **Freestyle Team Sprint (F)**: Uses `Sprint_F_Pelo_pct, Freestyle_Pelo_pct, Distance_F_Pelo_pct`

Separate models are trained for each technique at the championship.

#### Probability

Same hybrid approach as relay with 2 legs instead of 4. Variance parameters (TS_SCORE_SD_MIN/MAX) are calibrated separately.

#### Calibration System

Three independent calibration processes using Brier score on historical data (2018+):

1. **Individual Race Calibration**: Grid search over DECAY_LAMBDA, SD_SCALE_FACTOR, SD_MIN, SD_MAX
2. **Relay Calibration**: Grid search over RELAY_SCORE_SD_MIN, RELAY_SCORE_SD_MAX
3. **Team Sprint Calibration**: Grid search over TS_SCORE_SD_MIN, TS_SCORE_SD_MAX

## Nordic Combined

### Individual

#### Data Gathering

Championship athletes configured by nation. Athletes matched with Elo scores for Overall, Individual, Individual Compact, Sprint, and Mass Start.

#### Probability

##### Training

Training data filtered to last 10 seasons and 75% Elo threshold. Position thresholds: Win, Podium, Top 5, Top 10, Top 30. BIC feature selection per threshold. Binomial GAM models with REML. Period and elevation adjustments calculated.

##### Testing

Elo percentages from startlist maximum. Exponential decay (α = 0.1) for participation. Models applied with adjustments.

#### Normalization and Monotonic Constraints

Same 5-phase process as Alpine.

### Team

#### Data Gathering

Team members configured by nation. Team Elo scores calculated as member averages.

#### Probability

##### Training

Position thresholds: Win, Podium, Top 5, Top 10. BIC feature selection on team average Elos. Binomial GAM models per threshold.

##### Testing

Team average Elos from configured rosters. Models applied for team predictions.

#### Normalization and Monotonic Constraints

Same 5-phase process with team thresholds.

## Ski Jumping

### Individual

#### Data Gathering

Championship athletes configured by nation. Athletes matched with Elo scores for Overall, Small Hill, Medium Hill, Normal Hill, Large Hill, and Flying.

#### Probability

##### Training

Training data filtered to last 10 seasons and 75% Elo threshold. Position thresholds: Win, Podium, Top 5, Top 10, Top 30. BIC feature selection per threshold using hill-size appropriate Elos. Binomial GAM models with REML. Period and hill size adjustments calculated.

##### Testing

Elo percentages from startlist maximum. Exponential decay (α = 0.1) for participation. Models applied with adjustments.

#### Normalization and Monotonic Constraints

Same 5-phase process as Alpine.

### Team

#### Data Gathering

Team members (typically 4 jumpers) configured by nation. Team Elo scores calculated as member averages.

#### Probability

##### Training

Position thresholds: Win, Podium, Top 5 (smaller team fields). BIC feature selection on team average Elos. Binomial GAM models per threshold.

##### Testing

Team average Elos from configured rosters. Models applied for team predictions.

#### Normalization and Monotonic Constraints

Same 5-phase process with team-specific thresholds.

### Mixed Team

#### Data Gathering

Mixed team members (2 men, 2 women) configured by nation. Team Elo scores calculated as averages across both genders.

#### Probability

Same methodology as team events with mixed team thresholds.
