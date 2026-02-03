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

### Individual

#### Data Gathering

Championship athletes configured by nation. Race schedule filtered for `Championship == 1`. Athletes matched with Elo scores for Overall, Distance, Distance Classic, Distance Freestyle, Sprint, Sprint Classic, Sprint Freestyle, Classic, and Freestyle.

#### Probability

##### Training

Training data filtered to last 10 seasons and 75% Elo threshold.

Position thresholds: Win, Podium, Top 5, Top 10, Top 30 for all individual events.

BIC feature selection uses discipline-appropriate Elo columns. Binomial GAM models with REML. Altitude (≥1300m), period, and mass start adjustments calculated.

##### Testing

Elo percentages from startlist maximum. Exponential decay (α = 0.1) for participation. Models applied with adjustments.

#### Normalization and Monotonic Constraints

Same 5-phase process as Alpine.

### Relay

#### Data Gathering

Relay teams configured by nation with leg assignments. Classic legs (1-2) and Freestyle legs (3-4) use appropriate technique-specific Elos.

#### Probability

##### Training

Position thresholds: Win, Podium, Top 5, Top 10. Leg-specific models account for classic vs freestyle and anchor leg importance.

##### Testing

Team Elos calculated from configured rosters with leg-appropriate weightings.

#### Normalization and Monotonic Constraints

Same 5-phase process with relay thresholds.

### Team Sprint

#### Data Gathering

Team sprint pairs configured by nation. Each team has two athletes alternating legs.

#### Probability

Same methodology as relay with team sprint-specific thresholds.

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
