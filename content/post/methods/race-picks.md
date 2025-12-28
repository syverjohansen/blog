---
title: "Race Picks Methodology"
date: 2023-12-01T01:23:07+00:00
draft: false
tags: ["methodology", "skiing", "race-picks"]
---

## BLUF

## Alpine

### Individual

#### Data Gathering

Each day at midnight UTC, an automated Python scrape is performed on the FIS website to identify upcoming World Cup/World Championship/Olympic races.  When upcoming races are found, the startlist is scraped and saved.  Alpine skiing data collection focuses on individual events only, as team events are too new and infrequent to generate predictions for.  

When official startlists aren't available (common due to weather delays and last-minute changes, FIS laziness), the system generates comprehensive mock startlists using all athletes who competed in the current season in order to maintain prediction capabilities.

In addition to the startlist roster, each skier is matched with historical skier data to get their most recent Elo scores for Overall, Downhill, Super-G, Giant Slalom, Slalom, Speed (Downhill/Super-G), and Technical (Giant Slalom/Slalom).

#### Points

The below sections describe the process for training and testing the models used to predict World Cup points for the upcoming race.

##### Training

###### Setup

The basis for our training dataset is to predict World Cup points for an upcoming race using historical Elo data and race performance data.  For this reason, we need to setup a dataframe that contains a column for Points, all the Elo data, and information about recent race performance.

The points column is created by mapping place to World Cup points for that place, so 1st place is 100, 2nd is 80, etc.

The Elo data is transformed so that the skier's elo prior to the race is turned into a percentage of the maximum elo for the participants in that race. This is so it is easier to predict results based on who is on the race vs an arbitrary elo score that is often subject to inflation throughout the course of the season.

Additionally, a column for weighted previous points is created that takes the last 5 points for a specific discipline (Downhill, Super-G, Giant Slalom, and Slalom). So in a row for a Slalom result, the weighted previous points will have that skiers last five results weighted so the most recent has a weight of 5, the one before 4, etc.  

Lastly, the training data is filtered to the last 10 seasons and missing values are imputed with the first quartile value for the given race.


###### Feature Selection

To decide what variables to use in the model, we filter down to discipline specific features to choose from.  For Downhill/Super-G, we use pre-race Elo percentages for Overall, Downhill, Super-G, Giant Slalom, and Speed as well as the weighted previous points for the given discipline.  For Slalom/Giant Slalom it is Overall, Super-G, Giant Slalom, Slalom, Technical and weighted previous points.  For combined, all available features are used.  

Then using an exhaustive approach, the explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 

###### Modeling

The models used to predict points are Generalized Additive Models (GAM) that capture non-linear relationships between Elo scores/prior performances and World Cup points.

In the case that there is insufficient data, the following fallback measures are in store:

  1. Full GAM with all BIC-selected variables and flexible smoothing
  2. Simplified GAM with reduced complexity and discipline-specific terms
  3. Linear regression if GAM approaches encounter fitting issues
  4. Simple ELO-only model as the ultimate fallback


###### Adjustments

While the modeling captures the majority of the accuracy in points predictions, additional adjustments are applied to individual skiers depending on their trends and how they typically overperform/underperform the predictions.  

There are two post-model adjustments applied to skiers for the points prediction.  The first is World Cup period and the second is differences between technical disciplines.  The period adjustment is for the different stages of the World Cup season as some skiers perform better earlier or later in the season than others.  The technical adjustment is if skiers perform better or worse in a specific discipline than they're predicted for, they will get an adjustment.  The determination if an adjustment is needed Actual Points - Predicted Points with a p-value of p < 0.05.


##### Testing

###### Startlist Setup

After the model training is performed, the startlist must be setup in a way that has all the features from the model as well as race participation probability if no startlist data was present on the FIS site.

Race participation probabilities use exponential decay weighting that gives more importance to recent participation patterns. For confirmed FIS startlists, athletes receive 1.0 probability. For mock startlists, the system calculates probabilities using historical participation rates with exponential decay (α = 0.1), where recent races count more heavily than older ones.

ELO score processing retrieves the most current performance ratings for each skier and normalizes them to percentages by dividing by the startlist's maximum values. This creates standardized 0-1 scale features across all discipline-specific ELO categories (Downhill, Super-G, Giant Slalom, Slalom, Technical, Speed, Overall).

Weighted previous points capture recent form using the last 5 races with linear increasing weights (1,2,3,4,5), giving more emphasis to the most recent performances.

Missing value imputation uses first quartile replacement for numeric variables and mode replacement for categorical variables, ensuring complete datasets while accounting for the fact that inexperienced athletes typically perform worse than established competitors.

###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

The adjustments for each skier found during training are applied the points prediction to get a final result.

#### Probability

##### Training

###### Setup

For probability predictions, the places from the training dataframe are converted into binary classification for position probability modeling across five finishing position thresholds. The system uses the same preprocessed historical race data as points models but transforms the regression problem into classification through binary outcome creation.

Position thresholds are defined as Win, Podium, Top 5, Top 10, and Top 30 finishes. Each threshold creates a separate binary classification problem where success is defined as finishing at or better than that position. This enables binomial GAM modeling while maintaining consistency with discipline-specific performance characteristics across technical and speed events. 

Elo percentages, weighted previous points, missing value imputation, and data filtering are all performed the same way as the points predictions.

###### Feature Selection

Position probability feature selection employs threshold-independent optimization with discipline-specific adaptation. The system uses the same explanatory variable pools as the points model while performing independent BIC optimization for each position threshold (1, 3, 5, 10, 30). Speed events use variables including Downhill, Super-G, and Speed elos, while technical events employ Slalom, Giant Slalom, and Tech elos. Selected variables are converted to smooth terms for binomial GAM modeling.

###### Modeling

The models for position probabilities are binomial GAM models that use the independent threshold-based features from the feature selection and adapt to the specific discipline performance characteristics.  The binomial GAM models are for each position threshold (1st, 3rd, 5th, 10th, and 30th).  Each model uses binomial family GAM implementation with REML estimation for conservtive smoothing parameter selection, promoting stability and capturing non-linear relationships between Elo ratings, weighted previous points, and finishing position probabilities.  Model validation employs Brier score evlauation to assess probablistic accuracy across the different position thresholds and discipline types.

###### Adjustments

Adjustments for probability predictions are disabled as testing showed larger inaccuracies after adjustments were performed.

##### Testing

###### Startlist Setup

After the model training is performed, the startlist must be setup in a way that has all the features from the model as well as race participation probability if no startlist data was present on the FIS site.

Race participation probabilities use exponential decay weighting that gives more importance to recent participation patterns. For confirmed FIS startlists, athletes receive 1.0 probability. For mock startlists, the system calculates probabilities using historical participation rates with exponential decay (α = 0.1), where recent races count more heavily than older ones.

ELO score processing retrieves the most current performance ratings for each skier and normalizes them to percentages by dividing by the startlist's maximum values. This creates standardized 0-1 scale features across all discipline-specific ELO categories (Downhill, Super-G, Giant Slalom, Slalom, Technical, Speed, Overall).

Weighted previous points capture recent form using the last 5 races with linear increasing weights (1,2,3,4,5), giving more emphasis to the most recent performances.

Missing value imputation uses first quartile replacement for numeric variables and mode replacement for categorical variables, ensuring complete datasets while accounting for the fact that inexperienced athletes typically perform worse than established competitors.

###### Modeling

The binomial GAM models with the chosen features from training are applied to the startlist data using direct mgcv package calls to generate threshold-independent probability predictions (1st, 3rd, 5th, 10th, and 30th). If needed, fallback measures are performed to handle prediction failures.


###### Adjustments

Adjustments for probability predictions are disabled as testing showed larger inaccuracies after adjustments were performed.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, normalization and monotonic constraints are applied.

Normalization is first applied so that the position probabilities all add up to the correct percentage.  For first place that sum to 100%, top-3 would sum to 300%, etc.  Individual athlete probabilities are capped at 100% since anything above that would be impossible.  

After normalization, monotonic constraints are added.  This ensures that top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so that an athlete cannot have a higher chance of finishing top-1 than top-3.  Then normalization is applied again to the monotonic constraint results to give the final results.

At this time, normalization and monotonic constraints are not applied to points predictions.

## Biathlon

### Individual

#### Data Gathering

Each day at midnight UTC, an automated Python scrape is performed on the IBU website to identify upcoming World Cup/World Championship/Olympic races.  When upcoming races are found, the startlist is scraped and saved.  Alpine skiing data collection focuses on individual events only, as team events are too new and infrequent to generate predictions for.  

When official startlists aren't available (common due to weather delays and last-minute changes, IBU laziness), the system generates comprehensive mock startlists using all athletes who competed in the current season in order to maintain prediction capabilities.

In addition to the startlist roster, each skier is matched with historical skier data to get their most recent Elo scores for Overall, Sprint, Pursuit, Individual, and Mass Start.


#### Points

##### Training

###### Setup

The basis for our training dataset is to predict World Cup points for an upcoming race using historical Elo data and race performance data.  For this reason, we need to setup a dataframe that contains a column for Points, all the Elo data, and information about recent race performance.

The points column is created by mapping place to World Cup points for that place, so 1st place is 100, 2nd is 80, etc.

The Elo data is transformed so that the skier's elo prior to the race is turned into a percentage of the maximum elo for the participants in that race. This is so it is easier to predict results based on who is on the race vs an arbitrary elo score that is often subject to inflation throughout the course of the season.

Additionally, a column for weighted previous points is created that takes the last 5 points for a specific discipline (Sprint, Pursuit, Individual, Mass Start). So in a row for a Sprint result, the weighted previous points will have that skiers last five results weighted so the most recent has a weight of 5, the one before 4, etc. 

At this time, there is no factoring in sprint results for pursuit predictions. 

Lastly, the training data is filtered to the last 10 seasons, skiers who have an Elo percentage of 75% or greater, and missing values are imputed with the first quartile value for the given race.

###### Feature Selection

To decide what variables to use in the model, we use all the Elo percent columns as well as the weighted previous points.

Then using an exhaustive approach, the explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 

###### Modeling

The models used to predict points are Generalized Additive Models (GAM) that capture non-linear relationships between Elo scores/prior performances and World Cup points.

In the case that there is insufficient data, the following fallback measures are in store:

  1. Full GAM with all BIC-selected variables and flexible smoothing
  2. Simplified GAM with reduced complexity and discipline-specific terms
  3. Linear regression if GAM approaches encounter fitting issues
  4. Simple ELO-only model as the ultimate fallback 

###### Adjustments

While the modeling captures the majority of the accuracy in points predictions, additional adjustments are applied to individual skiers depending on their trends and how they typically overperform/underperform the predictions.  

There are two post-model adjustments applied to skiers for the points prediction.  The first is World Cup period and the second is for elevation.  The period adjustment is for the different stages (1-4) of the World Cup season as some skiers perform better earlier or later in the season than others.  The elevation adjustment is if skiers perform better or worse at altitude (≥1300) than they're predicted for, they will get an adjustment.  The determination if an adjustment is needed Actual Points - Predicted Points with a p-value of p < 0.05.

##### Testing

###### Startlist Setup

After the model training is performed, the startlist must be setup in a way that has all the features from the model as well as race participation probability if no startlist data was present on the IBU site.

Race participation probabilities use exponential decay weighting that gives more importance to recent participation patterns. For confirmed IBU startlists, athletes receive 1.0 probability. For mock startlists, the system calculates probabilities using historical participation rates with exponential decay (α = 0.3), where recent races count more heavily than older ones.

Elo score processing retrieves the most current performance ratings for each skier and normalizes them to percentages by dividing by the startlist's maximum values. This creates standardized 0-1 scale features across all discipline-specific ELO categories (Overall, Sprint, Pursuit, Individual, Mass Start).

Weighted previous points capture recent form using the last 5 races with linear increasing weights (1,2,3,4,5), giving more emphasis to the most recent performances.

Missing value imputation uses first quartile replacement for numeric variables and mode replacement for categorical variables, ensuring complete datasets while accounting for the fact that inexperienced athletes typically perform worse than established competitors.

###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

The adjustments for each skier found during training are applied the points prediction to get a final result.

#### Probability

##### Training

###### Setup

For probability predictions, the places from the training dataframe are converted into binary classification for position probability modeling across five finishing position thresholds. The system uses the same preprocessed historical race data as points models but transforms the regression problem into classification through binary outcome creation.

Position thresholds are defined as Win, Podium, Top 5, Top 10, and Top 30 finishes. Each threshold creates a separate binary classification problem where success is defined as finishing at or better than that position. This enables binomial GAM modeling while maintaining consistency with discipline-specific performance characteristics. 

Elo percentages, weighted previous points, missing value imputation, and data filtering are all performed the same way as the points predictions.

###### Feature Selection

Position probability feature selection employs threshold-independent optimization with discipline-specific adaptation. The system uses the same explanatory variable pools as the points model while performing an exhaustive independent BIC optimization for each position threshold (1, 3, 5, 10, 30).  Selected variables are converted to smooth terms for binomial GAM modeling.

###### Modeling

Biathlon employs sophisticated binomial GAM (Generalized Additive Models) architecture for position probability prediction, utilizing independent threshold-based modeling frameworks that incorporate the sport's unique dual-discipline characteristics (shooting + skiing). The system implements separate binomial GAM models for each position threshold (1st, 3rd, 5th, 10th, 30th), recognizing that factors influencing podium finishes may differ substantially from those affecting top-10 or points-scoring positions across biathlon's diverse event formats. Each model uses binomial family GAM implementation with REML estimation for conservative smoothing parameter selection, promoting stability across biathlon's complex race format spectrum. The framework incorporates shooting technique and skiing endurance variables through smooth terms that capture non-linear relationships between dual-discipline performance components and finishing position probabilities, with event type-specific adaptations for individual versus relay competitions. Model validation employs Brier score evaluation to assess probabilistic accuracy across different position thresholds and event format combinations.

###### Adjustments

Biathlon implements sophisticated Individual Probability Training Adjustments that are currently **disabled** in the production system to prevent "double-dipping" and overfitting concerns. The comprehensive framework exists for systematic bias correction with conditional logic designed to accommodate biathlon's unique dual-discipline characteristics (skiing + shooting) and race format complexity. The system calculates probability differences between actual outcomes and model predictions (`prob_diff = as.numeric(position_achieved) - initial_prob`) with sophisticated conditional logic differentiating between individual and relay events (`period_p = if(is_relay) 1 else purrr::map_dbl(row_id, function(r) {...})`). Statistical validation employs t-test validation with robust error handling (`tryCatch({t.test(prior_period_curr, prior_period_other)$p.value}, error = function(e) 1)`) and period-specific corrections for individual events while maintaining zero adjustments for relay events. All adjustments are bounded within valid probability ranges to ensure mathematical validity. The framework is disabled in production with the annotation "Remove double-dipping" to prevent applying adjustments multiple times, prioritizing model stability over potential accuracy gains in biathlon's complex dual-discipline performance environment.

##### Testing

###### Startlist Setup

Biathlon's Individual Probability Testing employs sophisticated startlist preparation with conditional race format logic that accommodates both individual and relay events through dual-discipline performance frameworks. The system manages race format detection (`participant_col <- if(is_relay) "Nation" else "Skier"`), race format-specific ELO ratings (`Sprint_Elo`, `Individual_Elo`, `Pursuit_Elo`, `MassStart_Elo`), and nation-based relay team processing (`base_df <- startlist %>% select(Nation, all_of(race_prob_cols))`). The framework acknowledges biathlon's unique dual-discipline structure where shooting accuracy influences skiing strategy, maintains chronological ELO rating retrieval with dual-discipline context, and employs robust error handling with row-by-row fallback strategies. Position threshold integration accommodates standard thresholds (1st, 3rd, 5th, 10th, 30th) while ensuring compatibility with different race formats and the complex interactions between skiing speed and shooting precision that characterize biathlon's competitive challenges.

###### Modeling

Biathlon's Individual Probability Testing applies trained binomial GAM models through conditional race format logic accommodating both individual athlete predictions and nation-based relay team predictions. The system employs sophisticated error handling for dual-discipline complexity (skiing + shooting) with race format-specific model applications (`mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response")`). The framework differentiates between individual and relay prediction failures, integrates shooting accuracy considerations with skiing endurance requirements, and implements multi-tier fallback strategies maintaining dual-discipline performance considerations to ensure prediction availability across biathlon's varying competitive conditions and field sizes.

###### Adjustments

Biathlon implements **conditional** Individual Probability Testing Adjustments that are active for individual events but disabled for relay events to accommodate the sport's dual-discipline competitive structure. The system applies period-based corrections (`period_p = if(is_relay) 1 else purrr::map_dbl(row_id, function(r) {...})`) with statistical validation (p < 0.05) designed for systematic bias patterns across skiing and shooting performance interactions. Individual athletes receive comprehensive probability residual corrections while relay events use base model predictions to prevent overfitting from changing team compositions between races.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, normalization and monotonic constraints are applied.

Normalization is first applied so that the position probabilities all add up to the correct percentage.  For first place that sum to 100%, top-3 would sum to 300%, etc.  Individual athlete probabilities are capped at 100% since anything above that would be impossible.  

After normalization, monotonic constraints are added.  This ensures that top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so that an athlete cannot have a higher chance of finishing top-1 than top-3.  Then normalization is applied again to the monotonic constraint results to give the final results.

### Relay

#### Data Gathering

Biathlon relay data gathering employs sophisticated IBU JSON extraction with specialized dual-discipline team composition management across three relay formats (Standard, Mixed, Single Mixed). The system utilizes advanced JSON extraction from embedded script tags (`entry.get('IsTeam', False)`) for precise team/individual separation while accommodating biathlon's unique M-F-M-F leg patterns with comprehensive gender detection using position-based assignment and dual ELO database fuzzy matching that validates team composition across dual-discipline competitive requirements (skiing + shooting).

#### Points

##### Training

###### Setup

Biathlon relay points training setup employs sophisticated dual-discipline team aggregation that accommodates the sport's unique combination of skiing endurance and shooting precision within relay competition formats while managing IBU-specific race format variations and comprehensive team-level ELO integration. The framework handles biathlon's distinctive competitive structure through team performance aggregation and race format-aware training data preparation across the sport's interconnected relay event spectrum.

**IBU-Specific Team Training Data Integration**:
Biathlon's training setup utilizes IBU JSON-based data sources that reflect the sport's specialized dual-discipline competitive structure through comprehensive team composition and performance integration:

```r
# From race-picks.R:2030-2042
# Individual Probability Testing Adjustments (Conditional)
if(race_type %in% c("Relay", "Mixed Relay", "Single Mixed Relay")) {
  is_relay <- TRUE
  period_p <- 1  # Skip adjustments for relay races
  period_correction <- 0
  altitude_p <- 1  # Skip altitude adjustments
  altitude_correction <- 0
  log_info("Skipping adjustments for relay race type")
} else {
  is_relay <- FALSE
  # Apply normal adjustment calculations for individual races
  period_p <- purrr::map_dbl(row_id, function(r) {
    # ... adjustment logic for individual races
```

**Dual-Discipline Team Performance Aggregation**:
The system combines biathlon's unique skiing and shooting requirements through team-level ELO integration that acknowledges both endurance performance and precision shooting capabilities across relay team compositions:

Similar to the Individual, World Cup points are used as response variable. Explanatory variables are set up as the average prior to the race Elo scores for each discipline for the skiers on each team. Then those averages are converted to the percentage of the maximum value for the Elo type for the given race.

**Team-Specific ELO Calculation with Dual-Discipline Integration**:
The framework processes team ELO calculations that capture dual-discipline performance patterns across biathlon's specialized competitive structure: `Avg_Sprint_Elo`, `Avg_Individual_Elo`, `Avg_Pursuit_Elo`, `Avg_Mass_Start_Elo`. This acknowledges that relay teams require coordination between athletes with varying skiing endurance and shooting precision capabilities.

**Race Format-Aware Team Training Data Management**:
Biathlon's training setup accommodates Standard Relays (4x6km women, 4x7.5km men), Mixed Relays (2x6km women + 2x7.5km men with W-M-W-M patterns), and Single Mixed Relays through comprehensive format detection and dual-discipline team performance integration.

**Comprehensive Team Performance Timeline Integration**:
Finally, the same filtering is performed for last 10 seasons of results and average Elo score being at least 75% of the maximum Elo score, ensuring training data quality across biathlon's dual-discipline relay competitive requirements and IBU-specific performance standards.

###### Feature Selection

Biathlon relay points training feature selection employs sophisticated dual-discipline team-aggregated variable optimization that accommodates the sport's unique combination of skiing endurance and shooting precision requirements within relay competition environments. The system replaces individual athlete performance variables with team-aggregated metrics (`Avg_Sprint_Pelo_Pct`, `Avg_Individual_Pelo_Pct`, `Avg_MassStart_Pelo_Pct`, `Avg_Pursuit_Pelo_Pct`) while excluding weighted previous points since team compositions change between races. BIC optimization ensures optimal variable selection for team performance prediction across biathlon's interconnected race format structure.

###### Modeling

Relay points models use the same GAM approach as individuals.

###### Adjustments

No adjustments are made for relay races.

##### Testing

###### Startlist Setup

Biathlon relay points testing startlist setup implements sophisticated nation-based team data preparation that transforms individual athlete startlists into team-aggregated prediction datasets while preserving biathlon's dual-discipline competitive requirements and multi-format relay event handling capabilities. The system handles three distinct relay formats (Standard Relay, Mixed Relay, Single Mixed Relay) with nation-based identification and simplified participation probability assignment where all teams receive 100% participation probability rather than exponential decay calculations used for individual events.

###### Modeling

Biathlon relay points testing modeling employs sophisticated nation-based GAM frameworks that apply trained team models to generate dual-discipline team predictions while accommodating the sport's unique combination of skiing endurance and shooting precision requirements across multiple relay formats (Standard Relay, Mixed Relay, Single Mixed Relay). The system uses team-averaged dual-discipline features with disabled historical adjustments, processing teams as aggregated entities rather than individual athlete combinations to account for changing team composition dynamics.

#### Probability

##### Training

###### Setup

Biathlon's Relay Probability Training Setup converts the team-based points prediction problem into binary classification for position probability modeling across relay-specific finishing position thresholds with nation-based team composition awareness. The system employs the same team-aggregated dual-discipline framework as relay points models but transforms the complex team regression problem into binary classification through position-based outcome creation.

**Position Threshold Definition with Team-Level Focus**: Biathlon relay probability training uses standard relay position thresholds `c(1, 3, 5, 10, 30)` representing Team Win, Team Podium, Top 5 Teams, Top 10 Teams, and Top 30 Teams finishes, creating separate binary classification problems for each threshold with nation-based binomial GAM modeling for relay probability prediction.

**Binary Outcome Creation for Team Events**: The system creates binary outcome variables using team-specific transformations `relay_df$position_achieved <- relay_df$Place <= threshold` where Place represents team finishing positions rather than individual athlete placements, converting continuous team place variables into binary classification targets specifically designed for relay team performance analysis.

**Nation-Based Team Performance Integration**: Training setup acknowledges biathlon's relay team structure by focusing on nation-based team outcomes with team-averaged ELO ratings across multiple race formats while maintaining awareness of dual-discipline team coordination requirements that define biathlon relay success patterns.  

###### Feature Selection

Biathlon's Relay Probability Training Feature Selection employs sophisticated threshold-independent optimization strategy with automated BIC-based exhaustive subset selection that adapts to the sport's unique dual-discipline characteristics and nation-based team competition structure. The system performs independent feature optimization for each position threshold while leveraging team-aggregated variable inheritance from corresponding relay points prediction models.

**Variable Inheritance and Team Aggregation**: Relay probability models use identical explanatory variable pools as their corresponding relay points models, ensuring consistency between team-based modeling approaches while leveraging domain knowledge already encoded in biathlon's relay points model variable selection with team-aggregated dual-discipline performance integration.

**Team-Aggregated Dual-Discipline Variable Sets**: Biathlon adapts feature pools based on relay team composition characteristics, utilizing team-averaged variables including `Avg_Sprint_Pelo_Pct`, `Avg_Individual_Pelo_Pct`, `Avg_MassStart_Pelo_Pct`, `Avg_Pursuit_Pelo_Pct`, and `Avg_Pelo_Pct` without weighted previous points, focusing on nation-based team capabilities rather than individual athlete historical performance metrics.

**Automated Exhaustive BIC Selection for Team Events**: Biathlon implements the most sophisticated automated feature selection among winter sports relay probability models through comprehensive BIC optimization, ensuring optimal variable combinations for each position threshold while maintaining awareness of team-aggregated dual-discipline performance characteristics.

###### Modeling

Team position models use GAM with REML with team-aggregated smooth terms.  Brier score evlauation applied to team-level predictions.

###### Adjustments

**Biathlon relay adjustments are disabled** in the production system to prevent systematic bias correction complications in team competition environments where team compositions change between races. Unlike individual biathlon competitions where consistent athlete performance patterns (shooting accuracy under specific conditions, skiing speed on certain terrains) can be statistically validated across multiple races, relay teams feature different athlete combinations each race, making historical adjustment patterns unreliable for future team predictions.

The system recognizes that biathlon relay performance depends on team chemistry, tactical coordination, and leg-specific role execution that varies significantly with different athlete combinations. Each leg requires different skills - opening legs demand consistent shooting under pressure, middle legs need tactical positioning abilities, and anchor legs require clutch performance under intense competition pressure. These role-specific requirements change with different athlete lineups.

Moreover, biathlon relay tactics evolve dynamically based on shooting performance and race situations. Teams may employ conservative shooting strategies when leading (prioritizing accuracy over speed) or aggressive approaches when trailing (accepting higher penalty risk for faster lap times). The disabled adjustment framework prevents overfitting to temporary team composition patterns while maintaining model stability across biathlon's unique combination of shooting precision and skiing endurance in team environments.

##### Testing

###### Startlist Setup

Biathlon relay probability testing employs nation-based team aggregation with simplified participation probability assignment, supporting multi-format relay processing across standard Relay, Mixed Relay, and Single Mixed Relay configurations. The startlist preparation strategy emphasizes team-aggregated performance metrics with comprehensive IBU-specific data integration.

**Nation-Based Team Data Loading**: Biathlon loads relay team data through multi-format startlist processing with nation-centric team identification that consolidates team performance across four distinct relay formats (Relay, Mixed Relay, Single Mixed Relay, and Team events) using IBU-specific JSON data extraction methodologies.

**Team-Aggregated Performance Metrics**: The system implements sophisticated team performance aggregation that combines individual athlete ELO ratings into nation-level team metrics, incorporating shooting accuracy factors, skiing speed components, and relay-specific coordination elements into unified team performance predictions.

**Simplified Uniform Participation Probability**: Biathlon assigns uniform 100% participation probability across all qualified teams, avoiding complex individual athlete probability calculations while maintaining mathematical consistency through comprehensive probability normalization and monotonic constraint enforcement.

**Multi-Format Relay Processing**: The methodology handles comprehensive relay format diversity with nation-based team composition validation, including gender constraint enforcement for mixed relay formats (Female-Male-Female-Male patterns) and standardized 4-person team composition for standard relay events.

**IBU-Specific Integration**: The startlist preparation incorporates specialized IBU data structures with JSON-based athlete extraction, nation quota management, and team member organization that accounts for biathlon's unique dual-discipline performance requirements while maintaining simplified team-level probability assignment across all relay formats.

###### Modeling

Biathlon relay probability testing employs sophisticated Generalized Additive Models (GAM) with binomial family distributions and comprehensive individual-to-team aggregation frameworks. The modeling approach emphasizes position threshold prediction through GAM implementations with backward stepwise feature selection, Brier score validation, and mathematical constraint enforcement to ensure logical probability relationships across multiple relay formats.

The framework utilizes GAM-based position threshold modeling for comprehensive coverage (win, podium, top 5, top 10, top 30) with individual athlete probability aggregation that converts athlete-level predictions into team outcomes through sophisticated coordination modeling. Team probability calculations incorporate shooting correlation, skiing correlation, and relay exchange efficiency factors specific to biathlon's dual-discipline requirements.

Mathematical constraint enforcement ensures probability normalization to theoretical target sums and monotonic constraint compliance (Win ≤ Podium ≤ Top 5 ≤ Top 10 ≤ Top 30). The system includes period-specific temporal adjustments and multi-format relay adaptations that account for standard relay endurance coordination, mixed relay gender-alternating tactics, and single mixed relay simplified coordination dynamics while maintaining comprehensive Brier score validation across all modeling components.

###### Adjustments

Biathlon relay probability testing implements a **deliberately disabled adjustment framework** specifically designed to address the fundamental challenge of changing team compositions between relay events. Unlike individual competitions where athlete-specific historical patterns enable meaningful systematic bias correction, relay team composition variability makes traditional period and elevation adjustments unreliable for team prediction accuracy and mathematical consistency.

**Conditional Relay Adjustment Disabling**: Biathlon employs sophisticated conditional logic that explicitly disables systematic bias corrections (period adjustments = 0, elevation adjustments = 0) for relay events while maintaining full adjustment capabilities for individual competitions. The framework recognizes team composition volatility assessment patterns where mean stability <0.7 justifies disabled adjustments.

**Team Composition Volatility Recognition**: The disabled framework acknowledges that relay teams frequently change athlete lineups between competitions based on form, tactics, and availability, making historical individual-based adjustment patterns inappropriate for current team composition prediction scenarios.

**Mathematical Robustness Through Conservative Approach**: Biathlon's disabled adjustment framework prioritizes prediction reliability over systematic bias correction, utilizing probability normalization without period/elevation corrections, multi-format relay adaptations, and comprehensive mathematical consistency validation while avoiding potentially unreliable systematic bias correction in team environments.

**Conservative Base Model Prioritization**: With disabled adjustments, biathlon relay testing relies exclusively on base GAM model predictions, prioritizing model stability and acknowledging that team-aggregated performance metrics captured during training provide more reliable prediction foundations than individual athlete adjustment patterns that may no longer apply to current team compositions.

#### Normalization and Monotonic Constraints

Same as individual.

## Cross-Country

### Individual

#### Data Gathering

Cross-country skiing presented the most complex data gathering challenge due to the sport's incredible diversity of race formats and the need for sophisticated mock startlist generation. The sport includes distance races, sprints, relays, team sprints, mixed relays, and even special events like Final Climb competitions, each requiring different handling approaches.

The key innovation for cross-country was developing a comprehensive nation quota system for mock startlists. When official FIS startlists aren't available, I can't simply include all athletes who've competed recently - that would create startlists with hundreds of participants. Instead, I built a quota system that mirrors real FIS World Cup qualification rules, allocating spots by nation based on their skiing strength and current World Cup standings, plus bonus allocations for host countries and World Cup leaders.

Name matching became particularly complex in cross-country because of the integration with Fantasy XC, a popular prediction game that uses slightly different name formats. I developed sophisticated fuzzy matching algorithms that can connect FIS names (often in "LASTNAME Firstname" format) with Fantasy names, manual mappings for common edge cases, and multiple fallback strategies when exact matches aren't found.

The sport's technical/classical technique distinction required building dual rating systems. Cross-country races can be freestyle (skating), classical (traditional), or mixed format, and athlete performance varies dramatically between techniques. I maintain separate ELO ratings for classical and freestyle events within each race distance category, plus aggregate ratings for athletes' overall distance and sprint capabilities. This granular approach captures that some athletes excel in classical distance races but struggle with freestyle sprints. 

#### Points

##### Training

###### Setup

Cross-country skiing presents the most complex training data challenge among winter sports because of its incredible diversity. The sport combines multiple race distances (sprint versus distance), two completely different techniques (classic versus freestyle), various start formats (mass start versus individual start), and different point systems (World Cup versus stage races like Tour de Ski).

The key insight is that these factors interact in meaningful ways. A skier who excels in freestyle sprints might struggle with classic distance races. Someone who thrives in mass start chaos might perform differently in time trial-style individual starts. The training data setup captures these nuances by calculating weighted previous points based on both race type AND technique combinations.

This creates the most granular performance tracking system among all winter sports. When predicting a freestyle sprint, the model looks primarily at previous freestyle sprint results, giving recent performances more weight. For classic distance races, it focuses on classic distance history. This technique-specific approach is crucial because classic and freestyle skiing are almost different sports entirely.

The system also handles cross-country's unique points complexity. Tour de Ski stage races use different point scales than regular World Cup races, and the system dynamically selects which point system to apply to ALL historical data based on what type of race is being predicted today. This ensures consistent scoring across the training dataset.

Environmental factors matter enormously in cross-country. Altitude affects endurance performance, mass start formats create different tactical dynamics than individual starts, and certain periods of the season see different competitive dynamics (early season versus Tour de Ski versus World Championships). The system tracks all these contextual factors and filters to high-quality events (World Cup, Olympics, World Championships) from the past 10 years, focusing on athletes competitive enough to score points.

###### Feature Selection

Cross-country skiing requires the most comprehensive feature selection among winter sports due to the sport's incredible diversity. The system considers an extensive set of variables: weighted previous points and ELO percentages for Overall, Distance, Distance Freestyle, Distance Classic, Sprint, Sprint Freestyle, Sprint Classic, Freestyle, and Classic performance. This captures how athletes might excel in freestyle sprints but struggle with classic distance races, or vice versa.

Unlike other sports that adapt their variable sets to race conditions, cross-country uses a unified optimization approach across all race contexts. Using an exhaustive search approach, explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables. The chosen variables are then converted to smooth terms for the Generalized Additive Model (GAM) to capture non-linear relationships.

###### Modeling

Cross-country skiing employs the most sophisticated modeling approach among winter sports due to the sport's incredible complexity. The system uses Generalized Additive Models that must handle the most diverse set of predictive variables across multiple race formats, techniques, and environmental conditions.

The modeling reflects cross-country's unique challenges: athletes who excel in freestyle sprints might struggle with classic distance races, someone who thrives in mass start chaos might perform differently in time trial-style individual starts, and altitude affects endurance performance significantly. The GAM models capture these complex non-linear relationships through comprehensive smooth term integration.

Cross-country implements both points prediction and position probability models with extensive validation. The models use dynamic point scaling based on race type (distance vs sprint vs stage races like Tour de Ski), accommodating the sport's varying scoring systems. Position models use binomial GAM with REML estimation and comprehensive Brier score validation across all finishing position thresholds.

The system includes the most comprehensive adjustment mechanism among winter sports, accounting for altitude effects, seasonal periodization, and mass start tactical dynamics through sequential statistical testing. This multi-dimensional approach ensures predictions capture the full complexity of cross-country competition while maintaining robust statistical validation.

###### Adjustments

Cross-country skiing implements the most comprehensive adjustment system among winter sports, reflecting the sport's incredible complexity across race formats, techniques, and environmental conditions. The system uses a sophisticated sequential approach to avoid double-counting effects while capturing three critical performance dimensions.

First, altitude adjustments account for the significant endurance effects of racing at venues above 1300 meters. Some athletes thrive at altitude due to superior fitness and adaptation, while others struggle with the cardiovascular demands. The system identifies these patterns through rigorous statistical testing.

Second, seasonal period adjustments capture how athletes perform during different phases of the World Cup season - early season when technique development matters most, tour periods when consistency is key, and championship periods when peak fitness determines outcomes. Some skiers are "early season specialists" while others peak for major events.

Third, mass start format adjustments recognize the tactical dynamics that differentiate mass start races from individual time trials. Some athletes excel in the pack dynamics and tactical positioning of mass starts, while others prefer the pure time trial format of individual starts. This adjustment is unique to cross-country among winter sports, reflecting the sport's diverse race format spectrum.

##### Testing

###### Startlist Setup

Cross-country skiing's startlist setup employs advanced probability modeling with precise race characteristic matching and dual-methodology normalization. The system handles both distance and sprint races across freestyle and classic techniques while ensuring robust data quality.

Race participation probabilities use exponential decay weighting with technique-specific matching. ELO score processing employs dual normalization approaches - standard races use historical maximums while Final Climb races use current startlist maximums. Weighted previous points are calculated separately for each distance/technique combination, and missing values are imputed using first quartile replacement.

###### Modeling

Cross-country's testing modeling applies technique-specific GAM models with comprehensive Individual Points Testing Adjustments that account for systematic biases in the most complex winter sport prediction challenge. The system implements statistical significance testing to identify genuine performance patterns across multiple race dimensions during the testing phase.

**Individual Points Testing Adjustments for Multi-Dimensional Performance**: Cross-country testing employs the most sophisticated sequential adjustment framework among winter sports, accounting for the sport's incredible diversity across techniques, distances, and environmental conditions. The system uses rigorous statistical testing (p < 0.05 threshold with t-tests) to identify genuine patterns requiring correction.

**Sequential Testing Adjustment Framework**: Adjustments are calculated and applied sequentially across three dimensions:
1. **Altitude Adjustments**: Account for endurance effects at venues above 1300m, as some athletes thrive at altitude while others struggle with cardiovascular demands
2. **Period Adjustments**: Capture seasonal progression patterns from early season technique development through tour periods to championship peaks
3. **Mass Start Adjustments**: Recognize tactical dynamics that differentiate mass start races from individual time trials, unique among winter sports

Each adjustment requires minimum 3 observations per category and uses comprehensive error handling with tryCatch blocks. The three-dimensional adjustment system avoids double-counting through sequential application and integrates with race participation probability weighting to balance historical bias correction with technique-specific performance prediction requirements across cross-country's diverse race spectrum.

###### Adjustments

Cross-country skiing implements **active** Individual Probability Testing Adjustments representing the most comprehensive three-dimensional adjustment system among winter sports. The system applies sequential corrections for altitude effects (above/below 1300m venues), period-based seasonal progression patterns, and mass start tactical dynamics (`altitude_effect + period_effect + ms_effect`) with probability bounds enforcement ensuring all adjusted probabilities remain between 0 and 1 while capturing systematic performance biases across cross-country's complex competitive landscape.

#### Probability

##### Training

###### Setup

Cross-country skiing's Individual Probability Training Setup converts the points prediction problem into binary classification for position probability modeling with technique-specific threshold adaptation. The system uses the same preprocessed historical race data as points models but transforms the complex multi-technique regression problem into classification through binary outcome creation across different event formats.

Position thresholds adapt to event type: Distance events use standard thresholds `c(1, 3, 5, 10, 30)` representing Win, Podium, Top 5, Top 10, and Top 30, while Sprint events use specialized thresholds `c(1, 3, 6, 12, 30)` representing Win, Podium, Final, Semifinal, and Quarterfinal. Each threshold creates a separate binary classification problem using the transformation: `race_df$position_achieved <- race_df$Place <= threshold`. This enables binomial GAM modeling while accommodating cross-country's incredible complexity across techniques, distances, and environmental conditions.  

###### Feature Selection

Cross-country skiing's Individual Probability Training Feature Selection employs the most comprehensive threshold-independent optimization among winter sports, adapting to the sport's incredible complexity across techniques, distances, and environmental conditions. The system uses identical explanatory variable pools as points models (`position_feature_vars <- explanatory_vars`) while performing independent BIC optimization for each position threshold (Distance events: 1, 3, 5, 10, 30; Sprint events: 1, 3, 6, 12, 30). The comprehensive variable set includes `Distance_Pelo_Pct`, `Sprint_Pelo_Pct`, `Sprint_C_Pelo_Pct`, `Distance_F_Pelo_Pct`, `Distance_C_Pelo_Pct`, `Classic_Pelo_Pct`, `Freestyle_Pelo_Pct`, `Sprint_F_Pelo_Pct`, reflecting the sport's complexity across techniques (Classic, Freestyle), distances (Sprint, Distance), and formats (Individual Start, Mass Start).  

###### Modeling

Cross-country skiing employs the most sophisticated binomial GAM (Generalized Additive Models) architecture among winter sports for position probability prediction, utilizing independent threshold-based modeling frameworks that incorporate the sport's extraordinary complexity across techniques, distances, and environmental conditions. The system implements separate binomial GAM models for each position threshold (Distance events: 1st, 3rd, 5th, 10th, 30th; Sprint events: 1st, 3rd, 6th, 12th, 30th), recognizing that factors influencing success vary dramatically across classic versus freestyle techniques, sprint versus distance formats, and individual start versus mass start competitions. Each model uses binomial family GAM implementation with REML estimation for conservative smoothing parameter selection, promoting stability across cross-country's exceptionally diverse competitive spectrum. The framework incorporates technique-specific and distance-specific variables through sophisticated smooth terms that capture non-linear relationships between Classic/Freestyle performance, sprint/distance capabilities, and finishing position probabilities across different race formats. Model validation employs comprehensive Brier score evaluation to assess probabilistic accuracy across different position thresholds, techniques, and distance categories.  

###### Adjustments

Cross-country skiing implements the most sophisticated Individual Probability Training Adjustments among winter sports, featuring an **active** multi-factor correction system that operates on probability residuals through sequential adjustment layers. The comprehensive methodology adapts to the sport's extraordinary complexity across techniques (Classic, Freestyle), distances (Sprint, Distance), and environmental conditions with statistical validation for each adjustment factor. The system begins with probability residual calculations (`prob_diff = as.numeric(position_achieved) - initial_prob`) and employs sequential altitude → period → mass start processing to capture environmental performance variations, competitive season changes, and tactical race format differences. Each adjustment layer uses t-test validation (p < 0.05) with comprehensive error handling (`tryCatch({t.test(prior_period_curr, prior_period_other)$p.value}, error = function(e) 1)`) and maintains valid probability ranges through systematic boundary enforcement (`pmin(pmax(altitude_adjusted + period_correction, 0), 1)`). The active implementation represents the most comprehensive adjustment system among winter sports, capturing cross-country's multifaceted systematic bias patterns across environmental conditions, competitive periods, and race format variations.

##### Testing

###### Startlist Setup

Cross-country skiing's Individual Probability Testing employs the most sophisticated startlist preparation among winter sports, accommodating extraordinary complexity across techniques (Classic, Freestyle), distances (Sprint, Distance), environmental conditions, and format variations. The system features race format-specific threshold selection (Distance: 1st, 3rd, 5th, 10th, 30th; Sprint: 1st, 3rd, 6th, 12th, 30th), Fantasy XC price integration (`base_df <- startlist %>% dplyr::select(Skier, ID, Nation, Price, Sex)`), and the most extensive ELO rating system (`Distance_Elo`, `Distance_C_Elo`, `Distance_F_Elo`, `Sprint_Elo`, `Sprint_C_Elo`, `Sprint_F_Elo`, `Freestyle_Elo`, `Classic_Elo`). The framework incorporates altitude categorization for environmental adaptation (above/below 1300m), mass start versus individual start format detection, and comprehensive model variable validation with technique awareness. Position threshold processing adapts to format-dependent naming while maintaining consistency across cross-country's extraordinary variable complexity and competitive structure variations.

###### Modeling

Cross-country skiing's Individual Probability Testing employs the most sophisticated model application framework among winter sports, accommodating extraordinary complexity across techniques, distances, environmental conditions, and format variations. The system applies trained binomial GAM models through comprehensive variable validation, active multi-factor adjustment integration (`altitude_effect`, `period_effect`, `ms_effect`), and threshold-adaptive processing for Distance versus Sprint events. The framework incorporates environmental condition integration, mass start versus individual start model differentiation, and technique-aware error handling while maintaining fantasy integration requirements and sequential adjustment application with probability bounds enforcement across cross-country's exceptional competitive diversity.

###### Adjustments

Cross-country skiing implements **active** Individual Probability Testing Adjustments representing the most comprehensive three-dimensional adjustment system among winter sports. The system applies sequential corrections for altitude effects (above/below 1300m venues), period-based seasonal progression patterns, and mass start tactical dynamics (`altitude_effect + period_effect + ms_effect`) with probability bounds enforcement ensuring all adjusted probabilities remain between 0 and 1 while capturing systematic performance biases across cross-country's complex competitive landscape.

#### Normalization and Monotonic Constraints

Cross-country skiing implements streamlined Individual Normalization and Monotonic Constraints that accommodate the sport's extraordinary technique and format complexity while maintaining mathematical validity across the most diverse race spectrum among winter sports. The system employs simplified normalization procedures without race participation probability adjustments, focusing on technique-aware constraint enforcement and threshold-adaptive processing for Distance versus Sprint event variations with format-specific target sum calculations (Distance events: 100%/300%/500%/1000%/3000%, Sprint events: 100%/300%/600%/1200%/3000%) that reflect semifinal and quarterfinal advancement structures.

#### Fantasy

Cross-country skiing implements sophisticated Individual Fantasy Team optimization representing the most advanced fantasy sports application among winter sports. The system employs Mixed Integer Programming with multiple team generation strategies (Normal/Safe/Upside) that accommodate technique complexity and environmental variability through probability-weighted expected value calculations (`Total_Points = Race1_Points * Race1_Probability + Race2_Points * Race2_Probability`). Fantasy optimization uses mathematical optimization (GLPK solver) with budget constraints (100,000 fantasy dollars) and roster requirements (16 athletes: max 8 men, max 8 women) while integrating comprehensive prediction models and technique-specific adjustments to generate globally optimal team selections across cross-country's diverse competitive landscape.

### Relay

#### Data Gathering

Cross-country relay data gathering employs sophisticated FIS website HTML parsing with Fantasy XC API integration across three distinct relay formats (Standard, Mixed, Team Sprint). The system utilizes advanced event type detection through string pattern matching (`'MIXED' in event_title and 'TEAM' in event_title`) while integrating technique-specific ELO ratings across nine performance categories and Fantasy XC pricing data for comprehensive team evaluation that accommodates the sport's extraordinary technique complexity (Classic/Freestyle) and environmental variability.  

#### Points

##### Training

###### Setup

Cross-country relay points training setup employs sophisticated multi-dimensional data integration that accommodates the sport's extraordinary technique complexity (Classic/Freestyle) across three distinct relay formats while incorporating comprehensive individual athlete performance aggregation. The framework manages relay-specific training data preparation through technique-aware leg assignment and environmental condition integration that reflects cross-country's most complex competitive structure among winter sports.

**Technique-Specific Relay Format Management**:
Cross-country's training setup accommodates Standard Relays (4x5km women, 4x7.5km men with Classic-Classic-Freestyle-Freestyle leg assignments), Mixed Relays (2x5km women + 2x7.5km men with F-M-F-M patterns), and Team Sprints (6x1.5km with single technique per race) through comprehensive format detection and leg-specific data integration:

```r
# From weekly-picks-relay.R:61-76
# Filter to only relay races
relay_races <- tomorrow_races %>%
  filter(Distance == "Rel")

# Split into men and ladies races
men_races <- relay_races %>%
  filter(Sex == "M")

ladies_races <- relay_races %>%
  filter(Sex == "L")

log_info(paste("Found", nrow(men_races), "men's relay races and", 
               nrow(ladies_races), "ladies' relay races"))
```

**Historical Relay Performance Integration with Individual Athlete Aggregation**:
The system combines relay team performance with individual athlete capabilities through comprehensive data integration that acknowledges both team dynamics and individual technique-specific performance patterns:

```r
# From champs-predictions.R:25-64
# Read chronological data
log_info("Reading chronological data files")

men_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/men_chrono_elevation.csv", 
                      stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

ladies_chrono <- read.csv("~/ski/elo/python/ski/polars/excel365/ladies_chrono_elevation.csv", 
                         stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date))

log_info(paste("Loaded", nrow(men_chrono), "men's chronological records"))
log_info(paste("Loaded", nrow(ladies_chrono), "ladies' chronological records"))
```

**Technique-Specific Weighted Previous Points Calculation**:
Cross-country implements the most sophisticated technique-aware points aggregation among winter sports, calculating separate weighted averages for Classic and Freestyle performance based on leg assignment requirements:

```r
# From champs-predictions.R:69-100
calculate_weighted_prev_points <- function(chrono_data) {
  chrono_data %>%
    arrange(ID, Date) %>%
    group_by(ID) %>%
    mutate(
      prev_points_weighted = sapply(1:n(), function(i) {
        if (i == 1) return(0)  # First race has no previous races
        
        current_distance <- Distance[i]
        current_technique <- Technique[i]
        
        # Get all races up to (but not including) current race
        prev_distances <- Distance[1:(i-1)]
        prev_techniques <- Technique[1:(i-1)]
        prev_points_values <- points[1:(i-1)]
        
        # Filter for matching race type
        if (current_distance == "Sprint" && current_technique == "C") {
          # Previous Sprint Classic races
          matching <- prev_distances == "Sprint" & prev_techniques == "C"
        } else if (current_distance == "Sprint" && current_technique == "F") {
          # Previous Sprint Freestyle races  
          matching <- prev_distances == "Sprint" & prev_techniques == "F"
        }
```

**Mixed Relay Gender Assignment with Technique Integration**:
The training setup accommodates cross-country's unique mixed relay format with alternating gender assignments (Female legs 1&3, Male legs 2&4) while maintaining technique-specific performance tracking:

Mixed relays set the female legs for 1 and 3, while the mens legs for 2 and 4. Team Sprints filter individual races down to the technique that will be used in the race, ensuring technique-specific training data integrity across cross-country's diverse relay format spectrum.

**Environmental Condition and Altitude Integration**:
The framework incorporates cross-country's unique environmental sensitivity through altitude categorization and venue-specific performance adjustments that acknowledge the sport's extraordinary sensitivity to environmental conditions compared to other winter sports.

**Comprehensive Training Data Filtering and Timeline Management**:
Cross-country employs extensive historical data preservation (11+ years) while maintaining technique-specific data integrity and environmental condition tracking that enables robust model training across the sport's complex competitive landscape.

###### Feature Selection

Cross-country relay points training feature selection employs sophisticated rule-based technique-specific optimization that adapts to the sport's extraordinary complexity across multiple relay formats. The system uses deterministic feature selection based on leg position and technique type rather than BIC optimization. Classic legs utilize technique-specific variables (`Distance_C_Pelo_Pct`, `Classic_Pelo_Pct`, `Distance_Pelo_Pct`) while freestyle legs employ freestyle-focused variables with anchor legs receiving additional sprint capabilities. Team Sprint events use technique-adaptive selection, and Mixed Relays incorporate gender-aware leg filtering with position-specific features for alternating male/female legs.


###### Modeling

Cross-country relay modeling employs sophisticated leg-specific GAM frameworks that capture the intricate tactical dynamics of relay racing across different techniques. Unlike individual races where athletes compete solely for themselves, relay performance depends on team composition, leg-specific tactics, and technique transitions that create unique competitive dynamics.

The system implements comprehensive leg position-specific modeling that recognizes the different roles each leg plays in relay strategy. Classic legs (typically 1st and 2nd in standard relays) require strong pacing and tactical positioning skills, while freestyle legs (3rd and 4th) demand speed and finishing capabilities. The anchor leg receives special consideration since finishing tactics differ significantly from earlier legs.

Relay models incorporate team chemistry factors through aggregated ELO ratings while preserving individual athlete performance characteristics. The modeling accounts for mixed relay gender transitions (female legs 1 and 3, male legs 2 and 4 in mixed relays) and team sprint technique specificity where both team members race the same technique.

The GAM framework uses leg-specific feature sets with technique awareness, accommodating the complex interactions between individual athlete capabilities and team tactical requirements. Multi-tier fallback strategies ensure robust predictions even when individual athlete data is sparse, utilizing team-level performance aggregation and historical relay-specific performance patterns. 

###### Adjustments

Cross-country relay adjustments employ the **most sophisticated systematic bias correction framework** in winter sports, utilizing a two-stage adjustment process specifically designed for team prediction environments. The system implements advanced mode reset probability corrections followed by mathematical normalization to address systematic prediction biases while preserving team chemistry dynamics.

**Two-Stage Systematic Bias Correction Framework**: Cross-country relay employs sophisticated statistical bias correction through mode probability reset followed by mathematical normalization. The system identifies prediction probability distributions that deviate from expected patterns and applies `reset_mode_probabilities()` corrections that eliminate systematic biases while preserving relative team performance rankings.

**Mode Reset Strategy**: The framework detects when team prediction probabilities exhibit statistical modes that don't align with historical team performance patterns, applying reset corrections that distribute probabilities more evenly while maintaining mathematical validity. This captures complex relay dynamics where team chemistry effects create systematic prediction patterns.

**Mathematical Probability Normalization**: Following mode reset corrections, the system applies comprehensive probability normalization to ensure team prediction distributions sum correctly across all threshold categories (1st, 3rd, 5th, 10th, 30th) while maintaining constraint enforcement that preserves logical ordering and prevents impossible probability assignments.

The active adjustment framework represents the most advanced bias correction methodology among winter sports relay events, acknowledging that cross-country relay team performance involves complex tactical coordination that benefits from sophisticated statistical correction while maintaining prediction reliability across diverse team compositions and race formats.

##### Testing

###### Startlist Setup

Cross-country relay points testing startlist setup implements the most sophisticated team composition data preparation among winter sports, accommodating three distinct relay formats (standard relays, mixed relays, team sprints) while maintaining leg-specific athlete assignments and technique-aware performance integration. The system handles both official FIS startlists and optimized team generation with comprehensive gender and technique specialization management through dual-file architecture that separates team composition from individual athlete assignments. Multi-format relay startlist data loading employs format-specific processing with leg-specific athlete assignment using `get_leg_predictions_with_startlist()` to filter athletes by assigned leg positions, ensuring predictions are generated only for athletes actually assigned to specific relay legs rather than all available team members. Advanced gender and ID management for mixed relay includes sophisticated gender constraint handling with ID offset systems to prevent conflicts when combining men's and women's data, enabling proper gender-alternating leg assignments (F-M-F-M) while maintaining data integrity across combined datasets.

###### Modeling

Cross-country relay points testing modeling implements the most sophisticated leg-specific prediction aggregation system among winter sports, generating individual athlete predictions for each relay leg position and combining them into team-level performance predictions using technique-aware importance weighting. The system accommodates three distinct relay formats (standard relays, mixed relays, team sprints) with leg-specific model application, multi-format team aggregation methodologies, and dynamic leg importance weight application. Technique-specific model integration adapts to relay requirements using position-based technique assignment where legs 1-2 employ classic-focused models while legs 3-4 utilize freestyle-focused models, with gender-aware mixed relay processing using ID offset systems to prevent data conflicts while enabling proper gender-alternating leg assignments.

#### Probability

##### Training

###### Setup

Cross-country relay probability training setup employs sophisticated leg-specific binary classification targeting with technique-aware data preparation that represents the most comprehensive relay probability modeling among winter sports. The system transforms the complex multi-dimensional relay performance problem into specialized binary classification datasets for each leg position while accommodating cross-country's extraordinary technique complexity (Classic vs Freestyle) and multi-format relay event variations.

**Position Threshold Definition with Leg-Specific Adaptation**: Cross-country relay probability training uses fixed relay-specific position thresholds `c(1, 3, 5, 10)` representing Win, Podium, Top 5, and Top 10 leg finishes, maintaining consistent thresholds across all leg positions while accommodating technique-specific performance variations between classic legs (1-2) and freestyle legs (3-4).

**Leg-Specific Binary Outcome Creation with Technique Integration**: Cross-country implements the most sophisticated binary classification framework among winter sports, creating separate datasets for each relay leg with technique-specific binary targets using categorical factor creation: `is_podium = factor(ifelse(Place <= 3, "Yes", "No"), levels = c("No", "Yes"))` for proper categorical handling across leg-specific technique requirements.

**Technique-Specific Leg Assignment and Data Separation**: Cross-country relay training employs technique-aware data preparation separating classic legs (1-2) from freestyle legs (3-4) with sophisticated temporal ordering to ensure chronological consistency when filling missing values from individual races to relay legs, enabling technique-specific feature engineering that captures fundamental performance differences between classic and freestyle skiing techniques within relay contexts.

###### Feature Selection

Cross-country relay probability training feature selection employs the most sophisticated leg-specific technique-aware optimization strategy among winter sports, implementing deterministic rule-based selection with multi-dimensional adaptations across technique (Classic/Freestyle), leg position (1-4), and relay format (Standard, Mixed, Team Sprint) requirements. The system diverges from automated statistical optimization, utilizing domain knowledge-based variable selection that captures cross-country's extraordinary competitive complexity.

**Rule-Based Leg-Specific Technique Selection**: Cross-country implements deterministic feature selection through the `get_leg_predictors(leg, leg_data)` function that adapts variable pools based on leg position and technique requirements rather than statistical optimization criteria, acknowledging that relay leg performance involves specialized tactical roles and technique-specific capabilities.

**Technique-Adaptive Variable Pool Architecture**: The feature selection system employs sophisticated technique-aware variable filtering that separates classic-focused variables from freestyle-focused variables based on leg-specific technique assignments, with classic legs (1-2) using distance classic and classic-specific variables while freestyle legs (3-4) utilize distance freestyle and freestyle-specific variables.

**Multi-Format Adaptive Feature Selection**: The system adapts feature pools across different relay formats through format-specific variable selection strategies including standard relays (position-based technique selection), mixed relays (gender and position combined selection), and team sprints (race technique-dependent selection with sprint-focused variable emphasis).

###### Modeling

Cross-country relay employs sophisticated binomial GAM (Generalized Additive Models) architecture for team position probability prediction, utilizing independent threshold-based modeling frameworks that incorporate the sport's complex relay team dynamics and leg-specific tactical interactions. The system implements separate binomial GAM models for each position threshold (1st, 3rd, 5th, 10th, 30th), recognizing that factors influencing team podium finishes may differ substantially from those affecting top-10 or points-scoring positions in relay competitions. Each model uses binomial family GAM implementation with REML estimation for conservative smoothing parameter selection, promoting stability across cross-country's diverse relay formats (standard, mixed, team sprint). The framework acknowledges that relay performance depends on team chemistry, leg-specific role execution, and tactical coordination that varies significantly with different athlete combinations, incorporating team-aggregated performance metrics and relay format-specific variables through smooth terms that capture non-linear relationships between collective team capabilities and team finishing position probabilities. Model validation employs comprehensive Brier score evaluation to assess probabilistic accuracy across different team position thresholds and relay format combinations while accounting for team composition variability.

###### Adjustments

Cross-country relay implements **disabled** Individual Probability Training Adjustments to prevent systematic bias correction complications in team competition environments where team compositions change between races. Unlike individual events where consistent athlete performance patterns can be statistically validated across multiple competitions, relay teams feature different athlete combinations each race, making historical adjustment patterns unreliable for future team predictions. The disabled framework prevents overfitting to temporary team composition patterns and maintains model stability by avoiding systematic bias correction assumptions that don't apply to dynamic team environments. The system prioritizes model consistency over potential accuracy gains from adjustments that cannot account for changing team chemistry and tactical variations inherent in relay competitions.

##### Testing

###### Startlist Setup

Cross-country relay probability testing employs sophisticated nation-level startlist preparation with leg-specific probability modeling and team-based threshold integration. The system accommodates the most complex relay structure among winter sports through relay format detection (standard, mixed, team sprint) and comprehensive team composition management with leg-specific ELO integration.

The framework incorporates position threshold adaptation for relay competitions (1st, 3rd, 5th, 10th, 30th) while maintaining team-level data structures and nation-based identification systems. Team probability columns are dynamically detected and preserved throughout the prediction pipeline, ensuring mathematical consistency across different relay formats.

Leg-specific attribute management preserves individual athlete performance characteristics while enabling team-level aggregation for probability modeling. Missing value imputation employs team-specific first quartile replacement with comprehensive fallback mechanisms designed for relay competition requirements.

###### Modeling

Cross-country relay probability testing employs sophisticated leg-specific modeling using XGBoost (xgbTree) as the primary algorithm with GLM fallback, implementing technique-aware prediction frameworks with comprehensive 5-fold cross-validation. The modeling approach emphasizes individual leg probability predictions through binary classification models, then aggregates them into team-level probabilities using dynamic leg importance weighting and technique-specific predictor optimization.

The framework utilizes XGBoost-first modeling architecture with comprehensive fallback strategies, technique-specific predictor optimization based on leg technique requirements (Classic vs Freestyle), and dynamic leg importance weighting with later leg emphasis (default: [0.2, 0.2, 0.25, 0.35]). Team probability calculations incorporate leg-specific technique adaptations with sophisticated aggregation methodologies.

Mathematical modeling includes comprehensive 5-fold cross-validation with class probabilities enabled, technique-aware predictor selection (Distance_C_Elo/Classic_Elo for legs 1-2, Distance_F_Elo/Freestyle_Elo for leg 3, Sprint_F_Elo for leg 4), and advanced leg importance optimization with team composition strength adjustments. The system accommodates multiple relay formats (standard relay, team sprint, mixed relay) while maintaining sophisticated individual-to-team aggregation frameworks across cross-country's extraordinary competitive complexity.

###### Adjustments

Cross-country relay probability testing implements the **most sophisticated adjustment framework** among winter sports, employing comprehensive multi-stage probability normalization with mode reset strategies, technique-aware adjustments, and format-specific implementations. Unlike other sports that disable systematic bias correction for relay events, cross-country utilizes advanced probability distribution analysis and constraint enforcement specifically designed for leg-specific team prediction scenarios.

**Multi-Stage Probability Normalization Pipeline**: Cross-country employs sophisticated three-stage probability adjustment frameworks (mode reset → normalization → constraint enforcement) with format-specific implementations across standard relay, team sprint, and mixed relay events. The system combines `reset_mode_probabilities()` functionality with comprehensive mathematical constraint enforcement and technique-aware bias correction.

**Format-Specific Adjustment Implementation**: Cross-country adapts adjustment frameworks to accommodate different relay format requirements: Standard relay (mode reset enabled, 4-leg weighting), Team Sprint (mode reset disabled, 2-leg equal weighting), and Mixed Relay (full reset methodology enabled, gender-balance adjustments). Each format receives specialized adjustment strategies optimized for competitive requirements.

**Technique-Aware Adjustment Integration**: The system incorporates sophisticated technique-specific adjustments that account for Classic vs Freestyle performance patterns across leg assignments, with leg-specific technique bias corrections and specialized sprint-focused adjustments for final legs in freestyle segments.

**Advanced Probability Distribution Analysis**: Cross-country employs comprehensive probability distribution analysis to identify and correct systematic biases through sophisticated error handling, fallback strategies, and mathematical consistency validation specifically adapted for leg-specific team prediction scenarios.

#### Normalization and Monotonic Constraints

#### Setup

#### Predictions

## Nordic-Combined

### Individual

#### Data Gathering

Nordic Combined represents perhaps the most technically complex data gathering challenge because it's essentially two sports combined into one competition. Athletes compete in both ski jumping and cross-country skiing, with jumping results determining starting positions and time delays for the cross-country portion. This dual-sport nature required building systems that could capture both jumping and skiing performance data from FIS race results.

The complexity multiplies when considering Nordic Combined's various event formats. Beyond individual competitions, the sport features team events, team sprints, and mixed team competitions, each with different team composition rules and scoring systems. I developed specialized processing pipelines that can automatically detect event types and route data appropriately - individual athlete data goes to individual prediction models, while team data gets aggregated at the nation level for team predictions.

Data extraction for Nordic Combined required understanding the unique result formats used by FIS for combined events. Results pages include both jumping scores (distance, points, jump rank) and cross-country times (finish time, time behind leader), plus the complex calculations that convert jumping results into cross-country start time penalties. The system parses all these components to build comprehensive athlete performance profiles.

The challenge extended to mock startlist generation for Nordic Combined. Unlike pure cross-country or pure jumping events, creating realistic Nordic Combined startlists requires understanding which athletes actually compete in combined events (many top cross-country skiers never do Nordic Combined, and vice versa). I built historical participation tracking that identifies the actual Nordic Combined athlete pool, ensuring mock startlists reflect realistic competitive fields rather than including athletes who would never actually participate.

#### Points

##### Training

###### Setup

Nordic Combined training data setup handles one of the most unique challenges in winter sports: predicting performance in a sport that's actually two sports combined. Athletes must excel at both ski jumping and cross-country skiing, with jumping performance directly affecting cross-country starting positions through a complex time penalty system.

The training approach recognizes that Nordic Combined athletes are specialists in this specific dual-discipline format. Unlike pure ski jumpers or cross-country skiers, Nordic Combined athletes optimize their training for the unique demands of competing in both disciplines on the same day. This specialization means the system uses Nordic Combined-specific ELO ratings rather than trying to combine separate jumping and skiing ratings.

The sport uses a standardized 40-position point system across all race formats (Individual, Sprint, Mass Start), which simplifies the training data setup compared to sports with varying point scales. However, the complexity comes from handling both individual and team events within the same framework. Team events require aggregating individual athlete performance to team-level metrics while preserving the dual-discipline performance characteristics.

Training data spans 10 years and includes both elevation effects (venues above 1300m affect both jumping and skiing performance differently) and seasonal periodization. The system tracks race type-specific performance since Sprint Nordic Combined events have different tactical dynamics than longer Individual races, where jumping performance has more time to be overcome or extended during the skiing portion.

The filtering focuses on athletes with ELO ratings above 75% of the race leader, but Nordic Combined's smaller competitive field means this includes a broader range of athletes compared to more popular sports. This ensures sufficient training data while maintaining focus on World Cup-level competition.

###### Feature Selection

Nordic Combined feature selection handles one of the most unique challenges in winter sports: adapting to both individual and team competition formats while accounting for the dual-discipline nature of the sport (ski jumping + cross-country). The system uses different variable sets for individual versus team events. Individual competitions use weighted previous points plus race type-specific ELO ratings (Sprint, Individual, Mass Start, Individual Compact), while team events rely on team-averaged ELO ratings without historical points since team dynamics differ from individual performance patterns.

Using exhaustive subset selection with BIC optimization, the system identifies the most predictive combination while preventing overfitting. The selected variables are converted to smooth terms for GAM modeling, with comprehensive fallback strategies to handle data sparsity common in Nordic Combined's smaller competitive field.

###### Modeling

Nordic Combined modeling addresses one of the most unique challenges in winter sports: accurately predicting performance in a sport that's actually two sports combined (ski jumping + cross-country). Athletes must excel at both disciplines, with jumping performance directly affecting cross-country starting positions through complex time penalty calculations.

The system uses sophisticated Generalized Additive Models that capture the dual-discipline performance characteristics while adapting to both individual and team competition formats. Nordic Combined athletes are specialists in this specific combined format, requiring models that understand how jumping and skiing performance interact within the same competition day.

The modeling implements comprehensive multi-tier fallback strategies with progressively simpler approaches: full GAM with BIC-selected variables, simplified GAM with reduced complexity, linear regression, and ultimate ELO-only fallback. This ensures robust predictions even with the smaller competitive field typical of Nordic Combined.

Position probability models use binomial GAM with REML estimation and are validated using Brier scores. The system incorporates both elevation effects (venues above 1300m affect jumping and skiing differently) and seasonal periodization, recognizing that Sprint Nordic Combined events have different tactical dynamics than longer Individual races where jumping performance has more time to be overcome during the skiing portion.

###### Adjustments

Nordic Combined uses a sophisticated three-dimensional adjustment system that captures the sport's unique combination of ski jumping and cross-country skiing components. Since athletes must excel at both disciplines, their performance patterns can be more complex than sports requiring only one skill set.

The system identifies seasonal performance trends, recognizing that some athletes peak during early season competitions while others perform best during major championships. More importantly, it captures elevation effects - venues above 1300 meters affect both the jumping and skiing portions differently, as altitude impacts both ski jumping performance and cardiovascular demands during cross-country skiing.

Position probability adjustments receive special treatment with strict boundary enforcement to ensure all probabilities remain between 0 and 1. This prevents the statistical models from producing impossible predictions while capturing genuine performance patterns. The system also incorporates advanced volatility tracking to identify athletes prone to breakthrough performances or unexpected struggles, which is particularly valuable in Nordic Combined where the multi-discipline format can create surprising results.

##### Testing

###### Startlist Setup

Nordic Combined's startlist setup implements a sophisticated dual-format pipeline accommodating both individual and team competition formats with advanced race probability preservation and comprehensive feature engineering. The system handles event type-specific data preparation while capturing the complex interactions between ski jumping and cross-country skiing performance.

Race participation probabilities use exponential decay modeling with race type specificity across Sprint, Individual, Mass Start, and Individual Compact events. ELO-to-PELO conversion employs historical reference normalization for model consistency, and event type-specific weighted previous points capture distinct performance patterns across different Nordic Combined formats.

###### Modeling

Nordic Combined's testing modeling applies event type-specific GAM models with comprehensive Individual Points Testing Adjustments that account for systematic biases in dual-discipline performance prediction. The system uses trained models optimized for each event format while implementing statistical significance testing to identify genuine performance patterns during the testing phase.

**Individual Points Testing Adjustments for Dual-Discipline Performance**: Nordic Combined testing employs a sophisticated sequential adjustment framework that accounts for the sport's unique combination of ski jumping and cross-country skiing components. The system uses rigorous statistical testing (p < 0.05 threshold with t-tests) to identify genuine patterns requiring correction across both disciplines.

**Sequential Testing Adjustment Framework**: Adjustments are calculated and applied sequentially across dual-discipline conditions:
1. **Period Adjustments**: Account for seasonal progression effects across both ski jumping and cross-country skiing phases, as athletes' dual-discipline form develops differently throughout the season
2. **Elevation Adjustments**: Capture altitude effects on both jumping performance (air density affects technique and distance) and cross-country endurance (altitude training effects)

Each adjustment requires minimum 3 observations per category and uses comprehensive error handling with tryCatch blocks. The dual-discipline adjustment system avoids double-counting through sequential application and integrates with race participation probability weighting to balance historical bias correction with the complex interactions between ski jumping technique and cross-country skiing endurance that characterize this unique winter sport.

###### Adjustments

Nordic Combined applies sophisticated three-dimensional adjustments that capture the sport's unique combination of ski jumping and cross-country skiing components, including seasonal performance trends and elevation effects that impact both disciplines differently.

#### Probability

##### Training

###### Setup

Nordic Combined's Individual Probability Training Setup converts the points prediction problem into binary classification for position probability modeling across five finishing position thresholds. The system uses the same preprocessed historical race data as points models but transforms the dual-discipline regression problem (ski jumping + cross-country skiing) into classification through binary outcome creation.

Position thresholds are defined as `c(1, 3, 5, 10, 30)` representing Win, Podium, Top 5, Top 10, and Top 30 finishes. Each threshold creates a separate binary classification problem where success is defined as finishing at or above that position using the transformation: `race_df$position_achieved <- race_df$Place <= threshold`. This enables binomial GAM modeling while incorporating Nordic Combined's unique dual-discipline performance characteristics and event format variations.

###### Feature Selection

Nordic Combined's Individual Probability Training Feature Selection employs threshold-independent optimization with dual-discipline adaptation. The system uses identical explanatory variable pools as points models (`position_feature_vars <- explanatory_vars`) while performing independent BIC optimization for each position threshold (1, 3, 5, 10, 30). The feature selection incorporates both ski jumping and cross-country skiing performance elements, utilizing variables that capture the dual-discipline nature including event type-specific ELO ratings (Sprint, Individual, Mass Start, Individual Compact) and weighted previous points. The process acknowledges that Nordic Combined performance involves complex interactions between jumping technique and cross-country endurance, ensuring selected variables capture both components while maintaining the sport's unique competitive structure.

###### Modeling

Nordic Combined employs sophisticated binomial GAM (Generalized Additive Models) architecture for position probability prediction, utilizing independent threshold-based modeling frameworks that incorporate the sport's unique dual-discipline characteristics (ski jumping + cross-country skiing). The system implements separate binomial GAM models for each position threshold (1st, 3rd, 5th, 10th, 30th), recognizing that factors influencing podium finishes may differ substantially from those affecting top-10 or points-scoring positions in dual-discipline competition. Each model uses binomial family GAM implementation with REML estimation for conservative smoothing parameter selection, promoting stability across Nordic Combined's diverse event spectrum. The framework acknowledges that ski jumping results directly influence cross-country starting positions through time compensation systems, incorporating both jumping technique variables and cross-country endurance metrics through smooth terms that capture non-linear relationships between dual-discipline performance components and finishing position probabilities. The modeling framework adapts to different Nordic Combined formats (Individual Compact, Mass Start, Team events), utilizing conditional logic to adjust variable pools and model complexity based on event characteristics, with comprehensive validation employing Brier score evaluation to assess probabilistic accuracy across different finishing position ranges and event format combinations.

###### Adjustments

Nordic Combined implements sophisticated Individual Probability Training Adjustments that are **active** in the production system, featuring period-based correction mechanisms designed to address systematic bias patterns across the sport's unique dual-discipline competitive structure (ski jumping + cross-country skiing). The methodology acknowledges that Nordic Combined performance involves complex interactions between jumping results and cross-country starting positions through time compensation systems. The system calculates probability residuals (`prob_diff = as.numeric(position_achieved) - initial_prob`) and employs period-specific adjustments with t-test validation (`t.test(prior_period_curr, prior_period_other)$p.value`) to capture systematic performance changes across competitive phases, applying corrections only when p < 0.05 ensures genuine period-based systematic bias. The framework adapts to different Nordic Combined formats with conditional logic for event characteristics, maintains valid probability ranges (`period_adjusted = pmin(pmax(initial_prob + period_correction, 0), 1)`), and remains fully operational representing an active approach to systematic bias correction that accommodates the sport's unique dual-discipline competitive dynamics.

##### Testing

###### Startlist Setup

Nordic Combined's Individual Probability Testing employs sophisticated startlist preparation that accommodates the sport's unique dual-discipline characteristics (ski jumping + cross-country skiing) through event type-specific ELO rating integration and time compensation framework management. The system incorporates dual-discipline event type integration for Sprint, Individual, Mass Start, and Individual Compact formats while maintaining separate ELO tracking for jumping and cross-country components. The framework handles Nordic Combined's complex competitive structure where ski jumping results directly influence cross-country starting positions, requiring integrated data preparation across both discipline components with dynamic race probability column detection and comprehensive probability data integrity validation throughout the prediction pipeline.

###### Modeling

Nordic Combined's Individual Probability Testing applies trained binomial GAM models through sophisticated dual-discipline integration accommodating the sport's unique competitive structure where ski jumping results directly influence cross-country starting positions through time compensation systems. The framework manages event type-specific model applications (Sprint, Individual, Mass Start, Individual Compact) with comprehensive variable validation for dual-discipline requirements and time compensation-aware prediction (`mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response"`). The system ensures reliable probability predictions through multi-threshold position modeling and comprehensive fallback strategies while maintaining dual-discipline performance integration across Nordic Combined's diverse competitive formats.

###### Adjustments

Nordic Combined implements **disabled** Individual Probability Testing Adjustments for position probabilities while maintaining active points adjustments. The position probability adjustment framework exists but is currently disabled in production to prioritize model stability over potential accuracy gains, recognizing that the sport's dual-discipline nature (ski jumping + cross-country) and time compensation interactions create complex performance patterns that may be better captured through robust base models rather than adjustment corrections.

#### Normalization and Monotonic Constraints

Nordic Combined implements comprehensive Individual Normalization and Monotonic Constraints specifically adapted for the sport's unique dual-discipline competitive structure (ski jumping + cross-country skiing) and time compensation system. The system employs full race participation probability integration that accommodates diverse event type variations (Sprint, Individual, Mass Start, Individual Compact) with time compensation-aware target sum calculations (100%/300%/500%/1000%/3000%) that account for jumping distance achievements converted to cross-country starting position advantages, ensuring mathematical validity across dual-discipline performance interactions while maintaining logical probability ordering for athletes with varying jumping versus skiing specializations.

### Relay

#### Data Gathering

Nordic Combined relay data gathering employs sophisticated FIS website HTML parsing with specialized dual-discipline team event detection across multiple team formats (Mixed Team, Team Sprint, Standard Team). The system utilizes multi-level event type analysis (`determine_event_type() -> Tuple[bool, bool, bool]`) with jump performance data extraction including individual team member distances and points for time compensation analysis, while accommodating the sport's unique dual-discipline competitive structure where jumping performance directly influences cross-country starting positions through time compensation systems.

#### Points

##### Training

###### Setup

Nordic Combined relay points training setup employs sophisticated dual-discipline team aggregation mechanisms that accommodate the sport's unique combination of ski jumping and cross-country skiing within relay competition formats while managing time compensation-aware training data integration and multi-format team event processing. The framework handles Nordic Combined's distinctive dual-discipline complexity through team-specific ELO calculation and comprehensive individual athlete performance aggregation across the sport's specialized competitive structure.

**Dual-Discipline Team Performance Integration**:
Nordic Combined's training setup adapts dual-discipline prediction frameworks to team competitions where coordination between ski jumping and cross-country skiing specialists becomes critical through comprehensive team composition analysis:

```r
# From race-picks.R:1490-1515
# Filter base dataset for race type 
race_df <- df %>%
  filter(RaceType == race_type)

log_info(paste("Filtered data dimensions:", nrow(race_df), "x", ncol(race_df)))

# Define ELO columns for training (Pelo) and prediction (Elo)
training_elo_col <- paste0("Avg_", hill_size, "_Pelo_Pct")  # Pre-race ELO for training
prediction_elo_col <- paste0("Avg_", hill_size, "_Elo_Pct")  # Post-race ELO for prediction

# Use team-specific explanatory variables for training (pre-race data)
explanatory_vars <- c("Prev_Points_Weighted",
                      "Avg_Normal_Pelo_Pct", "Avg_Large_Pelo_Pct", 
                      "Avg_Flying_Pelo_Pct", "Avg_Pelo_Pct")
```

**Time Compensation-Aware Training Data Integration**:
The system processes Nordic Combined's unique time compensation system where jumping performance directly influences cross-country starting positions, requiring specialized training data preparation that accommodates dual-discipline performance interactions:

Nordic Combined relay training setup adapts the dual-discipline prediction framework to team-based competitions where coordination between ski jumping and cross-country skiing specialists becomes critical. Unlike individual events where athletes optimize their personal dual-discipline performance, relay teams must balance individual strengths with tactical team coordination across both jumping and skiing phases.

**Team-Specific ELO Aggregation with Dual-Discipline Integration**:
The framework aggregates individual athlete Nordic Combined-specific ELO ratings to team-level metrics while preserving the dual-discipline performance characteristics that define the sport: `team_elo_cols <- c("Avg_Normal_Elo", "Avg_Large_Elo", "Avg_Flying_Elo", "Avg_Elo")`. Team composition analysis considers how jumping specialists and skiing specialists complement each other within relay tactical frameworks.

**Multi-Format Team Event Processing**:
Training data encompasses team results from multiple Nordic Combined relay formats (Mixed Team, Team Sprint, Standard Team), using the same 40-position point system as individual events but adapting to team-based performance dynamics and dual-discipline coordination requirements.

**Comprehensive Team Performance Timeline Management**:
The system filters to competitive team events from the past 10 years, focusing on teams where aggregated individual athlete capabilities indicate World Cup-level competition quality while maintaining dual-discipline performance integrity across Nordic Combined's specialized competitive structure and time compensation-aware tactical dynamics.

###### Feature Selection

Nordic Combined relay feature selection employs sophisticated team-aggregated dual-discipline variable optimization with BIC-based exhaustive subset selection that adapts to the sport's unique combination of ski jumping and cross-country skiing within team competition frameworks. The system focuses exclusively on team-averaged ELO variables without weighted previous points, recognizing that relay team compositions change between races unlike consistent individual athlete participation patterns.

The feature selection utilizes team-aggregated dual-discipline variables including `Avg_Normal_Team_Elo`, `Avg_Large_Team_Elo`, `Avg_Flying_Team_Elo`, and `Avg_Team_Elo` that capture collective jumping capabilities across different hill categories, while incorporating cross-country endurance metrics through team aggregation mechanisms. This acknowledges that relay team performance depends on strategic coordination between athletes with different dual-discipline specializations - some team members may excel at jumping components while others focus on cross-country skiing elements.

Using exhaustive BIC optimization with `regsubsets()`, the system identifies the most predictive combination of team-averaged Nordic Combined ELO ratings across event formats (Sprint, Individual, Mass Start, Individual Compact). The selected variables are converted to smooth terms for team-level GAM modeling with comprehensive fallback strategies designed to handle Nordic Combined's smaller competitive field and the complex interactions between jumping and skiing components that define this dual-discipline sport.

###### Modeling

Nordic Combined relay modeling employs sophisticated team-level GAM frameworks that capture the unique challenge of coordinating dual-discipline specialists (ski jumping + cross-country skiing) within relay tactical structures. Unlike individual events where athletes compete solely with their own dual-discipline capabilities, relay teams must optimize coordination between team members with different jumping and skiing strengths.

The system uses team-aggregated dual-discipline ELO ratings while incorporating relay-specific tactical considerations unique to Nordic Combined competitions. Team models account for how jumping performance affects subsequent cross-country strategy at the team level, where strong team jumping can provide tactical advantages for skiing phases.

The GAM framework includes comprehensive multi-tier fallback strategies designed for Nordic Combined's smaller competitive field, ensuring robust team predictions even when individual athlete data varies in quality across team members. Model validation employs team-level performance tracking with appropriate adjustments for the dual-discipline team coordination requirements.

###### Adjustments

**Nordic Combined relay adjustments are disabled** in the production system to prevent systematic bias correction complications in dual-discipline team competition environments where team compositions change between races. Unlike individual Nordic Combined athletes who maintain consistent dual-discipline performance patterns across jumping and skiing components, relay teams feature different athlete combinations each race with varying jumping and skiing specializations, making historical adjustment patterns unreliable for future team predictions.

The system recognizes that Nordic Combined relay team performance depends on complex coordination between athletes with different dual-discipline specializations - some team members may excel at ski jumping while others specialize in cross-country skiing. This creates team chemistry dynamics that vary significantly with different athlete combinations, as the optimal team strategy depends on how jumping specialists coordinate with skiing specialists across the time compensation system.

Furthermore, Nordic Combined relay tactics involve sophisticated dual-discipline coordination where strong team jumping performance can provide tactical advantages for subsequent skiing phases, but these advantages vary depending on the specific athletes involved. The disabled adjustment framework prevents overfitting to temporary team composition patterns while maintaining model stability across Nordic Combined's unique dual-discipline relay coordination requirements, acknowledging that systematic bias correction assumptions don't apply to constantly changing team chemistry dynamics between jumping and skiing specialists.

##### Testing

###### Startlist Setup

Nordic Combined relay points testing startlist setup implements sophisticated nation-based dual-discipline team data preparation that aggregates individual athlete jumping and cross-country skiing capabilities into team-level performance metrics while preserving the sport's unique time compensation system requirements across multiple relay formats (team, team sprint, mixed team). The system loads team startlist data from format and gender-specific CSV files with pre-calculated team-averaged dual-discipline performance metrics including `Avg_Elo`, `Avg_Individual_Elo`, `Avg_Sprint_Elo`, `Avg_MassStart_Elo`, and `Avg_IndividualCompact_Elo` representing collective team capabilities across Nordic Combined's diverse event formats. Team chronological data integration with time compensation awareness uses nation-based chronological performance data files to calculate weighted previous points using the last 5 team relay performances while accounting for Nordic Combined's unique dual-discipline team dynamics where jumping performance directly influences cross-country starting positions. Race participation probability assignment for team events implements simplified participation probability assignment where all teams receive 100% participation probability, reflecting the specialized nature of Nordic Combined relay events where listed teams typically represent confirmed dual-discipline team participation rather than projected attendance based on individual athlete patterns.

###### Modeling

Nordic Combined relay points testing modeling employs sophisticated nation-based dual-discipline GAM frameworks that apply trained team models to generate coordinated jumping and cross-country skiing predictions while accommodating the sport's unique time compensation system and multi-format relay event requirements (team, team sprint, mixed team). The system uses team-averaged dual-discipline features including `Avg_Sprint_Elo_Pct`, `Avg_Individual_Elo_Pct`, `Avg_MassStart_Elo_Pct`, `Avg_IndividualCompact_Elo_Pct`, and `Avg_Elo_Pct` with time compensation-aware team prediction generation that accounts for how jumping distance achievements translate to cross-country starting advantages. Multi-format relay processing adapts to different relay formats through format-specific data processing while maintaining comprehensive error handling for dual-discipline team scenarios and position probability integration with time compensation context.

#### Probability

##### Training

###### Setup

Nordic Combined's Relay Probability Training Setup converts the team-based dual-discipline points prediction problem into binary classification for position probability modeling across relay-specific finishing position thresholds with comprehensive time compensation integration. The system employs the same team-aggregated dual-discipline framework as relay points models but transforms the complex dual-discipline team regression problem (ski jumping + cross-country skiing) into binary classification through position-based outcome creation.

**Position Threshold Definition with Team-Level Dual-Discipline Focus**: Nordic Combined relay probability training uses standard relay position thresholds `c(1, 3, 5, 10, 30)` representing Team Win, Team Podium, Top 5 Teams, Top 10 Teams, and Top 30 Teams finishes, creating separate binary classification problems for nation-based binomial GAM modeling with time compensation awareness.

**Binary Outcome Creation for Dual-Discipline Team Events**: The system creates binary outcome variables using team-specific transformations where Place represents team finishing positions incorporating both jumping distance achievements and cross-country skiing performance, converting continuous team place variables into binary classification targets designed for dual-discipline relay team performance analysis with time compensation integration.

**Nation-Based Dual-Discipline Team Performance Integration**: Training setup acknowledges Nordic Combined's relay team structure by focusing on nation-based team outcomes that integrate both jumping and cross-country skiing components, incorporating team-averaged ELO ratings while maintaining awareness of dual-discipline team coordination requirements and time compensation dynamics that define Nordic Combined relay success patterns.

###### Feature Selection

Nordic Combined relay probability training feature selection employs sophisticated team-aggregated dual-discipline variable optimization with threshold-independent BIC-based exhaustive subset selection that adapts to the sport's unique combination of ski jumping and cross-country skiing within team competition frameworks. The system performs independent feature optimization for each position threshold while leveraging team-averaged variable inheritance from corresponding relay points prediction models.

**Variable Inheritance and Dual-Discipline Team Aggregation**: Relay probability models use identical explanatory variable pools as their corresponding relay points models, ensuring consistency between team-based dual-discipline modeling approaches while leveraging domain knowledge already encoded in Nordic Combined's relay points model variable selection with team-aggregated dual-discipline performance integration.

**Team-Aggregated Dual-Discipline Variable Sets with Time Compensation**: Nordic Combined adapts feature pools based on relay team composition characteristics, utilizing team-averaged variables including `Avg_Normal_Team_Elo`, `Avg_Large_Team_Elo`, `Avg_Flying_Team_Elo`, and `Avg_Team_Elo` without weighted previous points, focusing on nation-based team capabilities that integrate both jumping distance achievements and cross-country skiing endurance while accounting for time compensation effects.

**Dual-Discipline Feature Integration with Team Coordination Focus**: The feature selection process acknowledges that Nordic Combined relay performance involves complex coordination between athletes with different dual-discipline specializations, ensuring selected variables capture both jumping distance capabilities and cross-country endurance requirements while maintaining awareness of time compensation tactical patterns.

###### Modeling

Nordic Combined relay employs sophisticated binomial GAM (Generalized Additive Models) architecture for team position probability prediction, utilizing independent threshold-based modeling frameworks that incorporate the sport's unique dual-discipline team coordination characteristics (ski jumping + cross-country skiing). The system implements separate binomial GAM models for each position threshold (1st, 3rd, 5th, 10th, 30th), recognizing that factors influencing team podium finishes may differ substantially from those affecting top-10 or points-scoring positions in dual-discipline relay competitions. Each model uses binomial family GAM implementation with REML estimation for conservative smoothing parameter selection, promoting stability across Nordic Combined's specialized relay competitive environment. The framework acknowledges that relay team performance depends on coordination between athletes with different dual-discipline specializations, incorporating team-aggregated performance metrics through smooth terms that capture non-linear relationships between collective dual-discipline capabilities and team finishing position probabilities. Model validation employs comprehensive Brier score evaluation to assess probabilistic accuracy across different team position thresholds while accounting for team composition variability and dual-discipline coordination requirements.

###### Adjustments

Nordic Combined relay implements **disabled** probability training adjustments to prevent systematic bias correction complications in dual-discipline team competition environments where team compositions change between races. Unlike individual Nordic Combined athletes who maintain consistent dual-discipline performance patterns, relay teams feature different athlete combinations each race with varying jumping and skiing specializations, making historical adjustment patterns unreliable for future team predictions. The disabled framework prevents overfitting to temporary team composition patterns while maintaining model stability across Nordic Combined's unique dual-discipline relay coordination requirements.

##### Testing

###### Startlist Setup

Nordic Combined relay probability testing employs sophisticated nation-level startlist preparation with dual-discipline team composition management and comprehensive team-based threshold integration. The system accommodates Nordic Combined's unique relay structure through team composition analysis and dual-discipline team aggregation with position probability modeling for team competitions (1st, 3rd, 5th, 10th, 30th).

The framework incorporates team-level data structures with nation-based identification systems while preserving dual-discipline individual athlete performance characteristics through team aggregation. Team probability columns are dynamically detected and preserved throughout the prediction pipeline, ensuring mathematical consistency across Nordic Combined's specialized relay competitive requirements.

###### Modeling

Nordic Combined relay probability testing employs sophisticated GAM-based modeling with comprehensive dual-discipline integration, utilizing elevation-adjusted predictors and best subset feature selection with BIC criterion. The modeling approach emphasizes simplified team probability assignment through mathematical frameworks that accommodate Nordic Combined's unique time compensation system and dual-discipline competitive structure.

The framework utilizes GAM-based dual-discipline modeling with elevation-adjusted predictors, simplified team probability assignment avoiding complex individual aggregation, and comprehensive time compensation integration that reflects dual-discipline performance interactions. Team probability calculations incorporate jumping and skiing performance aggregation through time compensation methodologies with venue-specific factors.

Mathematical modeling includes best subset feature selection with BIC criterion for dual-discipline predictor optimization, exponential decay historical weighting (α = 0.1) for performance history integration, and comprehensive model validation with Brier score assessment adapted for dual-discipline performance prediction. The system accommodates multiple relay formats (Team, Team Sprint, Mixed Team) while maintaining simplified participation probability assignment (uniform 1.0) and comprehensive time compensation integration across Nordic Combined's unique competitive landscape.

###### Adjustments

Nordic Combined relay probability testing implements a **sophisticated unified adjustment framework** that treats relay events identically to individual competitions, employing comprehensive systematic bias correction with dual-discipline awareness. Unlike other winter sports that disable adjustment mechanisms for team events, Nordic Combined utilizes fully active period and elevation corrections specifically adapted for dual-discipline team prediction scenarios with time compensation integration.

**Unified Individual-Team Adjustment Strategy**: Nordic Combined applies identical systematic bias correction frameworks to both individual and team events with comprehensive conditional logic, dual-discipline team adjustment integration, and time compensation-aware adjustment frameworks that account for jumping-skiing performance interactions.

**Dual-Discipline Team Adjustment Integration**: The framework incorporates sophisticated dual-discipline adjustments with jumping component period adjustment, cross-country component period adjustment, and time compensation correction integration that reflects venue-specific dual-discipline performance interactions.

**Elevation Adjustment with Dual-Discipline Awareness**: Nordic Combined implements comprehensive elevation adjustments that account for both jumping and skiing performance impacts through jumping-specific elevation effects, skiing-specific elevation effects, and dual-discipline elevation interaction modeling with comprehensive significance testing.

**Multi-Level Fallback System and Mathematical Consistency**: The system includes comprehensive fallback strategies adapted for team events (full dual-discipline → simplified dual-discipline → single discipline → basic correction) while maintaining mathematical consistency enforcement with dual-discipline validation across Nordic Combined's unique competitive landscape.

#### Normalization and Monotonic Constraints

## Ski Jumping

### Individual

#### Data Gathering

Ski jumping required developing the most sophisticated event detection system of all the winter sports because determining whether a competition is individual or team-based isn't always straightforward from the race data alone. While other sports have clear indicators in their race classifications, ski jumping events can be ambiguously labeled, requiring multiple verification methods to ensure accurate categorization.

The breakthrough was developing a multi-layered detection system that examines event titles, race type descriptions, HTML page structure, and even the actual competitor names to determine event format. For team events, the system looks for country names rather than individual athlete names in the results, plus specific HTML structures that indicate team versus individual competitions. This redundant verification prevents individual athletes from being incorrectly processed as teams and vice versa.

Hill size detection became crucial for ski jumping predictions because athlete performance varies significantly between different hill sizes. A ski jumper who excels on large hills (K120+) might struggle on smaller normal hills (K90-K99), and this needed to be captured in the data. I built pattern matching systems that extract hill size information from race descriptions, handling various formats (K120, HS140, etc.) and ensuring this critical context is preserved for prediction models.

The sport's unique scoring system also required special handling. Ski jumping uses a complex points calculation based on distance, style points from judges, wind conditions, and gate factors. Unlike other sports where place-based points are straightforward, ski jumping results needed parsing of the actual jumping performance metrics to understand not just who won, but why they won and how different conditions affected the outcome. This detailed performance tracking enables more nuanced predictions that account for conditions and hill characteristics.

#### Points

##### Training

###### Setup

Ski jumping training data setup reflects the sport's most distinctive characteristic: performance varies dramatically based on hill size. A jumper who dominates on Flying Hills (the largest hills, 200+ meters) might struggle on Normal Hills (90-99 meters), and someone who excels on smaller hills might be overwhelmed by the massive Flying Hills. This isn't just a matter of distance - the timing, technique, and mental approach required for different hill sizes are fundamentally different.

The system addresses this by maintaining separate ELO ratings for each hill category: Small, Medium, Normal, Large, and Flying Hills. This creates the most detailed hill-specific performance tracking among all winter sports. When building training data, weighted previous points are calculated based on race type, recognizing that individual competitions have different dynamics than team events.

Ski jumping uses a standardized 30-position point system across all formats, which simplifies the scoring compared to sports with multiple point scales. However, the complexity lies in the hill size variations and the fact that jumping conditions (wind, gate factors, snow conditions) can dramatically affect performance in ways that other winter sports don't experience.

The training data spans 10 years and focuses on World Cup-level competition by filtering athletes with ELO ratings above 75% of the race leader. Ski jumping's smaller competitive field compared to cross-country or alpine skiing means this threshold captures a good range of competitive athletes while ensuring sufficient data for robust model training.

The system also handles both individual and team competitions within the same framework. Team events require aggregating individual jumper performance to team-level metrics while preserving the hill size-specific performance characteristics. This dual approach ensures the models can predict both individual jumper performance and team competition outcomes.

###### Feature Selection

Ski jumping's feature selection reflects the sport's most distinctive characteristic: dramatic performance variation based on hill size. A jumper who dominates on Flying Hills (200+ meters) might struggle on Normal Hills (90-99 meters), and vice versa. The system adapts to both individual and team formats while incorporating comprehensive hill size-specific ELO ratings.

For individual events, we consider weighted previous points plus hill size-specific ELO ratings (Normal, Large, Flying) plus overall ELO. For team events, we use team-averaged hill size-specific ELO ratings without weighted points. The feature selection focuses on the most competitive hill sizes where World Cup events typically occur, with exhaustive BIC optimization to identify the most predictive combination. Multi-tier fallback strategies handle data sparsity common in ski jumping's smaller competitive field.

###### Modeling

Ski jumping modeling is specifically optimized for the sport's most distinctive challenge: dramatic performance variation based on hill size. A jumper who dominates Flying Hills (200+ meter jumps) might struggle on Normal Hills (90-99 meters) due to completely different timing, technique, and mental approaches required for different hill categories.

The system uses Generalized Additive Models with comprehensive hill size-specific adaptations. The models seamlessly adapt between individual and team competitions while preserving hill size-specific performance characteristics. Individual models incorporate weighted previous points plus hill size-specific ELO ratings, while team models use aggregated hill size-specific team performance metrics.

Ski jumping implements the most robust fallback system among winter sports to handle data sparsity common in smaller competitive fields: full GAM with hill size integration, simplified GAM with reduced complexity including hill size flags, linear regression fallback, and ultimate ELO-only model. The fallback strategies specifically incorporate hill size effects to maintain prediction quality even with simplified models.

Position probability models use binomial GAM with REML estimation and undergo extensive validation with detailed performance tracking across all hill categories. The system includes enhanced model validation with comprehensive logging and Brier score assessment to ensure accurate probability predictions across the sport's unique hill size variations.

###### Adjustments

Ski jumping implements the most sophisticated dual-adjustment system in winter sports, recognizing that performance patterns vary not just by season but dramatically by hill size. This reflects ski jumping's fundamental challenge: a jumper who excels on Flying Hills (200+ meters) may struggle on Normal Hills (90-99 meters) due to completely different technical and mental requirements.

The system captures seasonal periodization effects, identifying athletes who consistently over- or under-perform during specific parts of the World Cup season. Some jumpers excel in early season conditions when hills are challenging, while others peak during major championship periods when conditions are optimal.

The hill size-specific adjustments are unique to ski jumping, using statistical testing to identify patterns where jumpers consistently perform differently on certain hill categories. This accounts for technical specialists who prefer smaller, more precise hills versus power jumpers who thrive on massive Flying Hills. The system calculates these adjustments sequentially to avoid double-counting effects while ensuring all significant performance patterns are captured.

Advanced volatility tracking analyzes each jumper's recent consistency and potential for both upside and downside performance. This helps identify athletes prone to breakthrough results or surprise disappointments, which is particularly valuable in ski jumping where mental factors and confidence can dramatically impact performance on different hill sizes.

##### Testing

###### Startlist Setup

Ski jumping's startlist setup implements the most comprehensive hill size-aware data preparation pipeline among winter sports, accommodating dramatic performance variations across different hill categories while ensuring robust data quality. The system handles both individual and team competition formats with specialized hill size-specific feature engineering.

Race participation probabilities use exponential decay modeling with hill size awareness, and ELO-to-PELO conversion employs sophisticated normalization using historical maximum values. Hill size-specific weighted previous points are calculated separately for each hill category, and the system includes advanced missing value imputation with dynamic fallback mechanisms.

###### Modeling

Ski jumping's testing modeling applies hill size-specific GAM models with comprehensive Individual Points Testing Adjustments that account for systematic biases in venue-dependent performance prediction. The system uses trained models optimized for different hill categories while implementing statistical significance testing to identify genuine performance patterns during the testing phase.

**Individual Points Testing Adjustments for Hill Size-Dependent Performance**: Ski jumping testing employs a sophisticated sequential adjustment framework that accounts for the sport's unique hill size variations and seasonal progression effects. The system uses rigorous statistical testing (p < 0.05 threshold with t-tests) to identify genuine patterns requiring correction across venue-dependent performance characteristics.

**Sequential Testing Adjustment Framework**: Adjustments are calculated and applied sequentially across hill size and seasonal dimensions:
1. **Period Adjustments**: Account for seasonal progression effects in ski jumping form development, as athletes' technical jumping ability and confidence develop throughout the season
2. **Hill Size Adjustments**: Capture performance differences between hill categories using binary classification (Small/Medium Hills for technical jumping vs. Large/Flying Hills for distance jumping)

Each adjustment requires minimum 3 observations per category and uses comprehensive error handling with tryCatch blocks. The hill size-aware adjustment system avoids double-counting through sequential application and integrates with race participation probability weighting to balance historical bias correction with the complex venue-dependent performance patterns that characterize ski jumping, where technical precision on smaller hills differs fundamentally from distance-focused performance on large hills and ski flying venues.

###### Adjustments

Ski jumping implements the most sophisticated dual-adjustment system in winter sports, recognizing that performance patterns vary not just by season but dramatically by hill size, capturing both seasonal periodization effects and hill size-specific performance characteristics unique to the sport.

#### Probability

##### Training

###### Setup

Ski jumping's Individual Probability Training Setup converts the points prediction problem into binary classification for position probability modeling with hill size-aware threshold adaptation. The system uses the same preprocessed historical race data as points models but transforms the venue-dependent regression problem into classification through binary outcome creation across different hill categories.

Position thresholds adapt to competition format: Individual events use standard thresholds `c(1, 3, 5, 10, 30)` representing Win, Podium, Top 5, Top 10, and Top 30, while Team events use reduced thresholds `c(1, 3, 5, 10)` (adapted for smaller team fields). Each threshold creates a separate binary classification problem using the transformation: `race_df$position_achieved <- race_df$Place <= threshold`. This enables binomial GAM modeling while incorporating ski jumping's unique hill size-dependent performance characteristics.

###### Feature Selection

Ski jumping's Individual Probability Training Feature Selection employs threshold-independent optimization with hill size-aware adaptation. The system uses identical explanatory variable pools as points models (`position_feature_vars <- explanatory_vars`) while performing independent BIC optimization for each position threshold (Individual events: 1, 3, 5, 10, 30; Team events: 1, 3, 5, 10). The feature selection utilizes hill size-specific variables including `Small_Pelo_Pct`, `Medium_Pelo_Pct`, `Normal_Pelo_Pct`, `Large_Pelo_Pct`, `Flying_Pelo_Pct`, and `Prev_Points_Weighted`. The process acknowledges that ski jumping performance varies dramatically across hill categories, ensuring selected variables capture both technical precision requirements for smaller hills and distance-focused performance characteristics for large hills and ski flying venues.

###### Modeling

Ski jumping employs sophisticated binomial GAM (Generalized Additive Models) architecture for position probability prediction, utilizing independent threshold-based modeling frameworks that incorporate the sport's distinctive hill size-dependent performance characteristics. The system implements separate binomial GAM models for each position threshold (Individual events: 1st, 3rd, 5th, 10th, 30th; Team events: 1st, 3rd, 5th, 10th), recognizing that factors influencing podium finishes may differ substantially from those affecting top-10 or points-scoring positions across different hill sizes. Each model uses binomial family GAM implementation with REML estimation for conservative smoothing parameter selection, promoting stability across ski jumping's wide venue spectrum from World Cup normal hills to ski flying competitions. The framework acknowledges ski jumping's most distinctive characteristic: dramatic performance variation based on hill size categories (K90-109 normal hills, K120+ large hills, K185+ ski flying hills), incorporating hill-specific performance metrics and venue characteristics through smooth terms that capture non-linear relationships between jumping technique, distance achievement, and finishing position probabilities across diverse competitive venues. Model validation employs Brier score evaluation to assess probabilistic accuracy across different position thresholds and hill size categories, with comprehensive validation ensuring predictions maintain calibration across ski jumping's venue-dependent performance distribution patterns.

###### Adjustments

Ski jumping implements sophisticated Individual Probability Training Adjustments that are **active** in the production system, featuring period-based correction mechanisms specifically adapted to the sport's distinctive hill size-dependent performance characteristics. The methodology recognizes ski jumping's most unique competitive challenge: dramatic performance variation across venue categories from K90 normal hills to K185+ ski flying hills. The system calculates probability residuals (`prob_diff = as.numeric(position_achieved) - initial_prob`) and employs period-specific adjustments with t-test validation (`period_p = purrr::map_dbl(row_id, function(r) {...})`) to capture systematic performance changes across the competitive season, applying corrections only when p < 0.05 ensures genuine period-based systematic bias rather than random variation in jumping form development. Period adjustments are calculated with consideration for venue-dependent performance patterns, maintaining valid probability ranges (`period_adjusted = pmin(pmax(initial_prob + period_correction, 0), 1)`), and adapting to different ski jumping formats with conditional logic for event characteristics and hill sizes. The active implementation remains fully operational, representing systematic bias correction that accommodates ski jumping's unique venue-dependent competitive dynamics and seasonal form progression while maintaining mathematical consistency across the sport's diverse competitive venue spectrum.

##### Testing

###### Startlist Setup

Ski jumping's Individual Probability Testing employs sophisticated startlist preparation that accommodates the sport's distinctive hill size-dependent performance characteristics through comprehensive venue categorization and hill-specific ELO rating systems. The framework manages the most detailed hill size classification among winter sports with five distinct venue categories: Small hills (K90-109m), Medium hills (K109-120m), Normal hills (K120-140m), Large hills (K140-185m), and Flying hills (K185+m). The system maintains separate ELO ratings for each category (`Small_Pelo_Pct`, `Medium_Pelo_Pct`, `Normal_Pelo_Pct`, `Large_Pelo_Pct`, `Flying_Pelo_Pct`) with binary hill size classification for model input (Small/Medium vs Large/Flying) and venue-aware weighted previous points calculation. Position threshold adaptation accommodates Individual events (1st, 3rd, 5th, 10th, 30th) versus Team events (1st, 3rd, 5th, 10th) while maintaining hill size-specific data validation and venue-dependent prediction accuracy across ski jumping's extraordinary venue diversity.

###### Modeling

Ski jumping's Individual Probability Testing applies trained binomial GAM models through sophisticated hill size-dependent frameworks accommodating dramatic performance variation across venue categories from K90 normal hills to K185+ ski flying hills. The system manages venue-specific model applications across five distinct hill categories with comprehensive variable validation for hill size-specific requirements (`Small_Pelo_Pct`, `Medium_Pelo_Pct`, `Normal_Pelo_Pct`, `Large_Pelo_Pct`, `Flying_Pelo_Pct`) and venue-aware threshold processing for different competition formats. The framework implements robust error handling designed for venue-dependent model application, multi-tier fallback strategies with hill size awareness, and seasonal form progression integration while maintaining venue-dependent prediction accuracy across ski jumping's diverse competitive venue spectrum.

###### Adjustments

Ski jumping implements **active** Individual Probability Testing Adjustments with sophisticated dual-factor correction mechanisms specifically adapted for the sport's distinctive hill size-dependent performance characteristics and seasonal form progression patterns. The methodology recognizes ski jumping's most unique competitive challenge: dramatic performance variation across venue categories from K90 normal hills to K185+ ski flying hills, where technical precision requirements differ fundamentally from distance-focused performance demands.

The system calculates probability residuals (`prob_diff = as.numeric(position_achieved) - initial_prob`) and employs sequential dual-factor adjustments with rigorous t-test validation to capture systematic performance changes across both competitive periods and venue-dependent characteristics. Period adjustments account for seasonal form development in jumping technique and confidence progression, while hill size adjustments capture performance differences between technical precision requirements (Small/Medium hills) versus distance achievement capabilities (Large/Flying hills).

The active implementation maintains fully operational dual-factor systematic bias correction, with sequential adjustment application ensuring no double-counting effects while preserving valid probability ranges through comprehensive boundary enforcement. The framework adapts to different ski jumping formats with conditional logic for venue characteristics and competition types, representing the most sophisticated venue-dependent adjustment system among winter sports that accommodates ski jumping's extraordinary performance variation across diverse competitive venues.

#### Normalization and Monotonic Constraints

Ski jumping implements sophisticated Individual Normalization and Monotonic Constraints specifically adapted for the sport's distinctive hill size-dependent performance characteristics and extraordinary venue diversity from K90 normal hills to K185+ ski flying hills. The system employs comprehensive race participation probability integration accommodating venue-dependent competitive dynamics across five distinct hill categories (Small, Medium, Normal, Large, Flying) with hill size-aware target sum calculations (100%/300%/500%/1000%/3000%) that account for venue characteristics where technical precision requirements differ fundamentally from distance achievement focus, ensuring mathematical validity while maintaining logical probability ordering for athletes with varying venue specializations across ski jumping's diverse competitive spectrum.

### Relay

#### Data Gathering

Ski jumping relay data gathering employs sophisticated FIS website HTML parsing with specialized hill size-dependent team event detection across mixed team and standard team formats. The system utilizes combined title and HTML structure analysis for team event identification while integrating comprehensive hill size classification (Small, Medium, Normal, Large, Flying) and multi-round jump data extraction (`Member_X_Length1`, `Member_X_Length2`) for venue-aware team performance evaluation across the sport's extraordinary venue diversity and hill-dependent performance characteristics.

#### Points

##### Training

###### Setup

Ski jumping relay points training setup manages complex team-specific ELO aggregation across the sport's distinctive hill size variations while accommodating multi-round team competition formats through comprehensive training data integration and venue-dependent performance optimization. The framework handles the sport's unique five-category hill system (Small, Medium, Normal, Large, Flying) with team-specific ELO calculations for venue-aware performance prediction across ski jumping's extraordinary competitive diversity.

**Championships-Based Team Training Data Processing**:
Ski jumping's relay training setup utilizes championships race format data that reflects the sport's primary team competition structure through comprehensive team data integration and venue-dependent performance aggregation:

```r
# From champs-predictions.R:269-311  
# Handle team vs individual races
if(is_team) {
  # For team races, startlist already contains pre-aggregated team data
  # Use it directly without further aggregation
  log_info("Processing team startlist - using pre-aggregated team data")
  
  # The team startlist already has Avg_* columns, use them directly
  result_df <- startlist %>%
    # Select relevant columns from the team startlist
    dplyr::select(Nation, any_of(race_prob_cols), starts_with("Avg_"))
  
  log_info(paste("Using team startlist with", nrow(result_df), "teams"))
  
  # Calculate team Prev_Points_Weighted for prediction startlist
  # Extract race type from elo_col (e.g., "Avg_Large_Elo_Pct" -> "Large")
  race_type_from_col <- gsub("Avg_(.+)_Elo_Pct", "\\1", elo_col)
```

**Hill Size-Specific Team ELO Aggregation Framework**:
The training system processes team ELO calculations with venue-specific integration that acknowledges ski jumping's extraordinary hill diversity and venue-dependent performance characteristics:

```r
# From champs-predictions.R:314-345
# Calculate team Prev_Points_Weighted for each team in startlist
if("TeamMembers" %in% names(startlist)) {
  # Team startlist has TeamMembers column
  result_df <- result_df %>%
    left_join(
      startlist %>% dplyr::select(Nation, TeamMembers),
      by = "Nation"
    ) %>%
    rowwise() %>%
    mutate(
      Prev_Points_Weighted = {
        if(!is.na(TeamMembers) && TeamMembers != "") {
          team_members <- trimws(strsplit(TeamMembers, ",")[[1]])
          calculate_team_prev_points(team_members, current_event_date, race_type_from_col, individual_chrono)
        } else {
          0
        }
      }
    ) %>%
    ungroup() %>%
    dplyr::select(-TeamMembers)  # Remove temporary column
```

**Mixed Team and Standard Team Format Processing**:
Ski jumping's training setup accommodates both Mixed Team (4 members with M-F-M-F patterns) and Standard Team (4 members, single gender) competition formats through comprehensive team composition validation and hill size-aware performance integration.

**Venue-Dependent Team Performance Integration**:
The framework processes hill size-specific team ELO calculations that capture venue-dependent performance patterns across ski jumping's five distinct hill categories: `team_elo_cols <- c("Avg_Normal_Elo", "Avg_Large_Elo", "Avg_Flying_Elo", "Avg_Elo")`. This acknowledges that teams may specialize in different hill sizes, requiring venue-aware team composition and performance evaluation.

**Event Date and Competition Timeline Integration**:
The system processes event dates with hill size-specific team performance timeline consideration: `current_event_date <- max(race_df$Date, na.rm = TRUE)`. This enables accurate team form assessment across venue-dependent performance patterns that may vary based on recent hill-specific competition experience and venue specialization development.

###### Feature Selection

Ski jumping relay feature selection employs sophisticated team-aggregated hill size-specific variable optimization with BIC-based exhaustive subset selection that accommodates the sport's extraordinary venue diversity across five distinct hill categories (Small, Medium, Normal, Large, Flying). The system utilizes team-averaged ELO variables without weighted previous points, recognizing that relay team compositions change between races while venue-dependent performance patterns require hill size-specific feature adaptation.

The feature selection incorporates comprehensive hill size-specific team variables including `Avg_Small_Team_Elo`, `Avg_Medium_Team_Elo`, `Avg_Normal_Team_Elo`, `Avg_Large_Team_Elo`, `Avg_Flying_Team_Elo`, and `Avg_Team_Elo` that capture collective jumping capabilities across different venue categories. This acknowledges that relay teams may specialize in specific hill sizes, with some teams excelling on technical precision requirements of smaller hills while others dominate distance-focused performance on large hills and ski flying venues.

Using exhaustive BIC optimization with `regsubsets()`, the system identifies the most predictive combination of hill size-specific team-averaged ELO ratings while preventing overfitting through comprehensive model selection criteria. The selected variables are converted to smooth terms for team-level GAM modeling with robust fallback strategies designed to handle venue-dependent data sparsity patterns and the complex hill size-specific performance interactions that characterize ski jumping's unique competitive venue spectrum.

###### Modeling

Ski jumping relay points training modeling employs sophisticated team-level GAM frameworks specifically adapted for the sport's extraordinary venue diversity and hill size-dependent performance characteristics. The system captures collective venue specialization patterns (technical precision vs distance achievement) within nation-based team competitive structures while implementing comprehensive multi-tier fallback strategies designed for ski jumping's smaller competitive field and complex venue-dependent team coordination requirements across five distinct hill categories.

###### Adjustments

**Ski jumping relay adjustments are disabled** in the production system to prevent systematic bias correction complications in team competition environments where team compositions change between races and venue-dependent specializations create complex performance patterns. Unlike individual ski jumping competitions where consistent athlete performance patterns across different hill sizes can be statistically validated, relay teams feature different athlete combinations each race with varying venue specializations, making historical adjustment patterns unreliable for future team predictions.

The system recognizes that ski jumping relay team performance depends on coordination between athletes with different hill size specializations - some team members may excel on technical precision requirements of smaller hills while others specialize in distance-focused performance on large hills and ski flying venues. This creates venue-dependent team chemistry dynamics that vary significantly with different athlete combinations and hill size categories.

Additionally, ski jumping relay tactics involve sophisticated venue-dependent coordination where team composition strategy must account for upcoming hill sizes in competition schedules. Teams may optimize lineups differently for technical precision hills versus distance-focused venues, but these tactical advantages vary depending on the specific athletes involved and their venue specializations. The disabled adjustment framework prevents overfitting to temporary team composition patterns while maintaining model stability across ski jumping's unique venue-dependent relay coordination requirements, acknowledging that systematic bias correction assumptions don't apply to constantly changing team chemistry dynamics across five distinct hill categories.

##### Testing

###### Startlist Setup

Ski jumping relay probability testing employs venue-dependent team composition across five distinct hill categories with sophisticated venue specialization handling. The startlist preparation represents the most venue-dependent approach among winter sports, emphasizing strategic team optimization based on hill size specialization patterns while maintaining simplified participation probability assignment across ski jumping's extraordinary venue diversity spectrum.

**Venue-Dependent Team Composition**: Ski jumping implements comprehensive hill size categorization (Small Hill <K65, Medium Hill K65-K89, Normal Hill K90-K119, Large Hill K120-K184, Ski Flying K185+) with venue-specific team identification that accounts for technical precision versus distance achievement requirements across different competitive environments.

**Strategic Lineup Optimization**: The system employs venue-dependent team composition strategies that optimize lineup selection based on hill size specialization and team coordination requirements, incorporating wind sensitivity factors, equipment optimization considerations, and venue experience bonuses into team probability testing scenarios.

**Multi-Format Team Processing**: Ski jumping handles comprehensive relay format diversity including standard Team events, Mixed Team events with gender balance constraints, and venue-specific team composition validation that accounts for hill category specialization patterns and strategic coordination optimization.

**Simplified Venue-Weighted Participation**: The methodology assigns venue-weighted participation probability (base 100% with hill category adjustments) that accounts for team specialization without complex individual athlete calculations, maintaining mathematical consistency while incorporating venue specialization factors.

**Comprehensive Hill Category Integration**: The startlist preparation incorporates sophisticated multi-hill category processing with specialized team handling that consolidates venue-dependent results across ski jumping's extraordinary venue diversity, ensuring team probability predictions account for hill size specialization patterns essential for relay competitive success.

###### Modeling

#### Probability

##### Training

###### Setup

Ski Jumping's Relay Probability Training Setup converts the team-based venue-dependent points prediction problem into binary classification for position probability modeling across relay-specific finishing position thresholds with comprehensive hill size specialization integration. The system employs the same team-aggregated venue-dependent framework as relay points models but transforms the complex venue-dependent team regression problem into binary classification through position-based outcome creation that accommodates ski jumping's unique relay competitive structure across five distinct hill categories.

**Position Threshold Definition with Team-Level Venue Awareness**: Ski jumping relay probability training uses adapted team-specific position thresholds `c(1, 3, 5, 10)` representing Team Win, Team Podium, Top 5 Teams, and Top 10 Teams finishes (reduced from individual event thresholds to accommodate smaller team competition fields), creating separate binary classification problems for nation-based binomial GAM modeling for venue-dependent relay probability prediction across hill size categories.

**Binary Outcome Creation for Venue-Dependent Team Events**: The system creates binary outcome variables using team-specific transformations where Place represents team finishing positions incorporating venue-specific jumping performance across different hill categories, converting continuous team place variables into binary classification targets specifically designed for venue-dependent relay team performance analysis with hill size specialization awareness.

**Nation-Based Venue-Dependent Team Performance Integration**: Training setup acknowledges ski jumping's relay team structure by focusing on nation-based team outcomes that integrate venue-dependent jumping capabilities across hill size categories, incorporating team-averaged ELO ratings while maintaining awareness of venue-dependent team coordination requirements where technical precision specialists excel on smaller hills while distance achievement specialists dominate on large hills and ski flying venues.

###### Feature Selection

Ski jumping relay probability training feature selection employs sophisticated team-aggregated venue-dependent variable optimization with threshold-independent BIC-based exhaustive subset selection that adapts to the sport's unique hill size variations within team competition frameworks. The system performs independent feature optimization for each position threshold while leveraging team-averaged venue-dependent variable inheritance from corresponding relay points prediction models.

**Variable Inheritance and Venue-Dependent Team Aggregation**: Relay probability models use identical explanatory variable pools as their corresponding relay points models, ensuring consistency between team-based venue-dependent modeling approaches while leveraging domain knowledge already encoded in ski jumping's relay points model variable selection with team-aggregated venue-dependent performance integration across hill size categories.

**Team-Aggregated Hill Size-Specific Variable Sets**: Ski jumping adapts feature pools based on relay team composition characteristics, utilizing team-averaged hill size-specific variables including `Avg_Small_Team_Elo`, `Avg_Medium_Team_Elo`, `Avg_Normal_Team_Elo`, `Avg_Large_Team_Elo`, `Avg_Flying_Team_Elo`, and `Avg_Team_Elo` without weighted previous points, focusing on nation-based team capabilities across five distinct hill categories where technical precision and distance achievement specialists coordinate effectively.

**Venue-Dependent Feature Integration with Team Specialization Focus**: The feature selection process acknowledges that ski jumping relay performance involves complex coordination between athletes with different venue specializations across dramatically different hill categories, ensuring selected variables capture both technical precision capabilities and distance achievement requirements while maintaining awareness of venue-dependent team coordination patterns across ski jumping's extraordinary competitive diversity.

###### Modeling

Ski jumping relay employs sophisticated binomial GAM (Generalized Additive Models) architecture for team position probability prediction, utilizing independent threshold-based modeling frameworks that incorporate the sport's unique venue-dependent team coordination characteristics across five distinct hill categories (Small, Medium, Normal, Large, Flying). The system implements separate binomial GAM models for each position threshold while accommodating venue-dependent performance variations that characterize ski jumping's extraordinary competitive diversity.

###### Adjustments

**Ski jumping relay probability adjustments are disabled** to maintain consistent methodology with points training adjustments. The system maintains disabled adjustments for both points and probability training phases to ensure model stability and prevent systematic bias correction complications in team competition environments where venue-dependent composition strategies vary between races and hill size categories.

##### Testing

###### Startlist Setup

Ski jumping relay points testing startlist setup implements the most sophisticated venue-dependent nation-based team data preparation among winter sports, accommodating dramatic performance variations across five distinct hill categories (Small, Medium, Normal, Large, Flying) while maintaining team composition management for multiple relay formats (team, mixed team) with comprehensive hill size specialization aggregation. The system loads team startlist data from format and gender-specific CSV files with pre-calculated team-averaged hill size-specific performance metrics including `Avg_Small_Elo`, `Avg_Medium_Elo`, `Avg_Normal_Elo`, `Avg_Large_Elo`, `Avg_Flying_Elo`, and `Avg_Elo` that reflect collective jumping capabilities across different hill size categories. Team chronological data integration with hill size context uses nation-based chronological performance data files to calculate weighted previous points using the last 5 team relay performances while accounting for ski jumping's unique venue-dependent team dynamics where technical precision specialists and distance achievement specialists must coordinate effectively. Simplified race participation probability assignment for team events provides all teams with 100% participation probability, reflecting the specialized nature of ski jumping relay events where listed teams typically represent confirmed venue-specific team participation rather than projected attendance based on individual athlete hill size specialization patterns.

###### Modeling

Ski jumping relay probability testing employs sophisticated GAM-based modeling with comprehensive venue specialization integration, utilizing hill size categorization and venue-dependent team coordination factors. The modeling approach emphasizes position threshold prediction through GAM implementations with progressive fallback strategies and mathematical constraint enforcement adapted for ski jumping's extraordinary venue diversity spectrum.

The framework utilizes GAM-based venue-dependent modeling with hill size categorization across five distinct categories (Small <K65, Medium K65-K89, Normal K90-K119, Large K120-K184, Ski Flying K185+), progressive fallback model strategies with comprehensive venue adaptation, and multi-hill category model integration with specialized venue handling. Team probability calculations incorporate venue-dependent team coordination factors, wind factor integration, and equipment optimization considerations.

Mathematical modeling includes comprehensive hill size categorization with venue specialization patterns, progressive fallback strategies (GAM → simplified GAM → GLM → basic GLM), and venue-specific probability normalization adapted for venue diversity. The system accommodates multiple hill categories and relay formats while maintaining simplified venue-weighted participation probability assignment and comprehensive venue-dependent constraint enforcement across ski jumping's extraordinary competitive landscape.

###### Adjustments

Ski jumping relay probability testing implements a **deliberately disabled adjustment framework** specifically designed to address the fundamental challenge of venue-dependent team composition variability across five distinct hill categories. Unlike individual events where athlete-specific venue patterns enable meaningful systematic bias correction, relay team composition strategies and venue specialization dynamics make traditional period and elevation adjustments unreliable for venue-dependent team prediction accuracy.

**Venue-Dependent Disabled Adjustment Framework**: Ski jumping employs sophisticated conditional logic that explicitly disables systematic bias corrections (period corrections = 0, elevation corrections = 0, hill category corrections = 0) for team events while accommodating extraordinary venue diversity across five hill categories with comprehensive venue composition variability assessment.

**Hill Category Composition Variability Recognition**: The disabled framework acknowledges that team composition strategies vary dramatically across hill categories based on specialization requirements, with composition analysis showing mean stability <0.6 across all categories, justifying disabled adjustments for technical precision specialists (Small/Medium hills) and distance achievement specialists (Large/Flying hills).

**Multi-Hill Category Disabled Strategy Implementation**: The system implements comprehensive disabled adjustment strategies adapted for each hill category's unique characteristics, with hill category-specific disabled frameworks that account for venue specialization patterns, wind factor integration through base modeling only, and venue-dependent conservative mathematical approaches.

**Conservative Mathematical Approach with Venue Diversity**: Ski jumping prioritizes mathematical consistency over potentially unreliable systematic bias correction while accommodating venue diversity through comprehensive probability normalization without bias correction, multi-format team adjustment strategies, and venue-dependent disabled frameworks across ski jumping's five-category venue spectrum.

#### Normalization and Monotonic Constraints

## Relay Probability Training Modeling Across Winter Sports

Relay probability training modeling represents one of the most sophisticated and diverse methodological frameworks in winter sports prediction, with each sport developing unique approaches that reflect their specific competitive structures, performance requirements, and team dynamics. The analysis of biathlon, cross-country skiing, Nordic combined, and ski jumping reveals fundamental differences in how each sport approaches leg-specific versus team-level modeling, feature selection strategies, and adjustment frameworks for relay competition prediction.

### Sport-Specific Modeling Philosophy Comparison

**Biathlon**: Implements **disabled adjustment frameworks** due to changing team compositions and dual-discipline complexity (shooting + skiing). Uses nation-based binomial GAM implementation with conservative systematic bias prevention that acknowledges team composition variability makes traditional individual athlete adjustment patterns inappropriate for relay prediction accuracy.

**Cross-Country**: Employs the **most sophisticated leg-specific binomial GAM** implementation among winter sports, utilizing technique-aware feature selection (classic vs freestyle) and comprehensive XGBoost/GLM fallback strategies. Implements advanced two-stage systematic bias correction with `reset_mode_probabilities()` function and mathematical constraint enforcement specifically designed for team probability normalization.

**Nordic Combined**: Utilizes **dual-discipline team aggregation** that integrates both ski jumping and cross-country skiing performance metrics with time compensation awareness. Implements disabled probability training adjustments to prevent systematic bias correction complications in dual-discipline team environments while maintaining comprehensive position threshold modeling (1st, 3rd, 5th, 10th, 30th).

**Ski Jumping**: Features **venue-dependent team-based modeling** across five distinct hill categories (Small, Medium, Normal, Large, Flying) with disabled adjustment frameworks specifically designed for venue-dependent team composition variability. Employs hill size-specific feature selection that adapts to technical precision versus distance achievement specialization patterns within relay team coordination requirements.

### Technical Architecture Comparison

| Aspect | Biathlon | Cross-Country | Nordic Combined | Ski Jumping |
|--------|----------|---------------|-----------------|-------------|
| **Primary Approach** | Nation-based GAM | Leg-specific GAM | Dual-discipline GAM | Venue-dependent GAM |
| **Feature Selection** | Disabled-aware | Technique-specific | Dual-discipline | Hill size-specific |
| **Modeling Strategy** | Conservative | XGBoost + GLM fallbacks | Binomial GAM | Venue-adapted GAM |
| **Adjustment Framework** | Disabled | Two-stage correction | Disabled | Disabled |
| **Team Aggregation** | Nation-based | Leg importance weighting | Dual-discipline averaging | Venue specialization |
| **Position Thresholds** | Standard relay | Standard relay | Extended (1,3,5,10,30) | Reduced (1,3,5,10) |

### Sophisticated Feature Engineering Approaches

**Cross-Country's Leg-Specific Optimization**: Implements the most advanced feature selection with technique-aware adaptation for classic legs (1-2) versus freestyle legs (3-4), utilizing comprehensive sprint-focused predictors for anchor leg tactical modeling and gender-aware filtering for mixed relay formats with advanced ID management systems.

**Ski Jumping's Venue-Dependent Specialization**: Features sophisticated hill size-specific variable sets that adapt to technical precision requirements (Small/Medium hills) versus distance achievement capabilities (Large/Flying hills), with venue-dependent team aggregation that balances technical precision specialists with distance achievement specialists.

**Nordic Combined's Dual-Discipline Integration**: Employs comprehensive team-averaged variables spanning multiple event formats (`Individual_Elo`, `Sprint_Elo`, `MassStart_Elo`, `IndividualCompact_Elo`) while maintaining time compensation awareness where jumping performance directly influences cross-country starting positions and final relay outcomes.

**Biathlon's Conservative Nation-Based Approach**: Utilizes stable, team-aggregated performance metrics that focus on nation-level capability assessment without attempting complex individual athlete interaction modeling, acknowledging that team composition variability requires different prediction approaches than consistent individual athlete patterns.

### Advanced Algorithmic Frameworks

**Cross-Country's Multi-Algorithm Strategy**: 
- Primary: XGBoost with hyperparameter tuning and 5-fold cross-validation
- Secondary: Standard GLM fallback with class probability prediction  
- Tertiary: Basic GLM with reduced features for robustness
- Most sophisticated fallback system among winter sports

**Other Sports' Conservative Approaches**:
- **Biathlon**: Binomial GAM only, disabled adjustments for stability
- **Nordic Combined**: Binomial GAM with dual-discipline focus, disabled adjustments
- **Ski Jumping**: Venue-dependent GAM with hill size specialization, disabled adjustments

### Adjustment Framework Philosophy Differences

**Enabled Adjustments (Cross-Country Only)**:
- Two-stage systematic bias correction with mode reset strategies
- Mathematical probability normalization with target enforcement
- Comprehensive constraint validation and monotonic ordering
- Advanced probability distribution analysis and clustering prevention

**Disabled Adjustments (Biathlon, Nordic Combined, Ski Jumping)**:
- **Biathlon**: Team composition variability prevents reliable systematic bias patterns
- **Nordic Combined**: Dual-discipline complexity with time compensation effects
- **Ski Jumping**: Venue-dependent team strategy variations across hill categories
- Common reasoning: Team dynamics differ fundamentally from individual athlete patterns

### Team Aggregation and Weighting Strategies

**Cross-Country Leg Importance Weighting**:
- Standard Relay: 20%, 20%, 25%, 35% (emphasizing anchor leg tactical importance)
- Team Sprint: 50%, 50% (equal importance for 2-leg format)
- Mixed Relay: Gender-alternating composition with specialized processing

**Other Sports' Simplified Approaches**:
- **Biathlon**: Nation-based team averaging without leg-specific weights
- **Nordic Combined**: Dual-discipline team averaging with format adaptation
- **Ski Jumping**: Venue-dependent team averaging with hill size specialization

### Methodological Innovation Summary

**Cross-Country** establishes the **most sophisticated relay probability training methodology** with leg-specific optimization, comprehensive algorithmic fallbacks, and advanced systematic bias correction that represents the pinnacle of winter sports relay modeling complexity and technical innovation.

**Ski Jumping** contributes unique **venue-dependent modeling frameworks** that accommodate extraordinary competitive diversity across five hill categories, providing essential insights for sports with dramatic venue-dependent performance variations.

**Nordic Combined** offers comprehensive **dual-discipline integration approaches** that balance fundamentally different sport components (jumping + skiing) with time compensation awareness, essential for complex multi-discipline relay competition understanding.

**Biathlon** provides **conservative stability-focused methodologies** that prioritize prediction reliability over complex optimization, offering valuable approaches for scenarios where team composition variability challenges traditional modeling assumptions.

### Cross-Sport Learning Opportunities

1. **Feature Selection Innovation**: Cross-country's technique-aware optimization could inform other sports with similar discipline variations
2. **Venue Adaptation**: Ski jumping's hill size-specific approaches could enhance other sports with significant venue effects  
3. **Conservative Stability**: Biathlon's disabled adjustment philosophy could improve reliability in complex team scenarios
4. **Dual-Discipline Integration**: Nordic combined's coordination approaches could benefit other multi-component sports

The comprehensive analysis of relay probability training modeling across winter sports reveals that successful approaches must balance sport-specific requirements with methodological rigor, with each sport's unique characteristics driving distinct but equally valid modeling philosophies that collectively advance the field of team-based sports prediction methodology.

## Relay Probability Training Adjustments Across Winter Sports

Relay probability training adjustments represent the most diverse and philosophically divergent aspect of winter sports prediction methodology, with sports implementing fundamentally different approaches that range from comprehensively disabled frameworks to sophisticated two-stage bias correction systems. The analysis reveals a clear philosophical divide: **Cross-Country** stands alone with enabled adjustments while **Biathlon**, **Nordic Combined**, and **Ski Jumping** all implement disabled adjustment frameworks, each for sport-specific but related reasons centered on team composition variability challenges.

### Adjustment Framework Philosophy Spectrum

**Enabled Adjustments (Cross-Country Only)**:
Cross-country implements the most sophisticated adjustment system in winter sports with a revolutionary **two-stage bias correction framework** featuring mode-based probability reset strategies (`reset_mode_probabilities()`) followed by comprehensive mathematical normalization with constraint enforcement (`normalize_probabilities()`). This system addresses the complex probability distribution challenges arising from sophisticated leg-specific modeling across multiple relay formats.

**Disabled Adjustments (All Other Sports)**:
Biathlon, Nordic Combined, and Ski Jumping all implement **comprehensively disabled adjustment frameworks** but for different sport-specific reasons that collectively highlight the fundamental challenge of team composition variability in relay prediction methodology.

### Sport-Specific Adjustment Rationale Analysis

| Sport | Framework | Primary Rationale | Secondary Factors |
|-------|-----------|------------------|-------------------|
| **Cross-Country** | **Enabled** (Two-stage) | Leg-specific modeling complexity requires bias correction | Technique awareness, anchor leg tactical significance |
| **Biathlon** | **Disabled** | Team composition changes + dual-discipline complexity | Shooting precision + skiing endurance coordination variability |
| **Nordic Combined** | **Disabled** | Dual-discipline coordination + time compensation effects | Jumping specialists vs skiing specialists team strategies |
| **Ski Jumping** | **Disabled** | Venue-dependent team composition across 5 hill categories | Technical precision vs distance achievement specialist coordination |

### Cross-Country's Revolutionary Two-Stage System

**Stage 1: Mode Detection and Reset (`reset_mode_probabilities`)**
- Identifies probability values appearing ≥2 times (6-decimal precision)
- Calculates maximum among repeated values
- Strategically resets repeated values OR values below maximum to zero
- Eliminates "background noise" probabilities that artificially inflate probability mass

**Stage 2: Mathematical Normalization (`normalize_probabilities`)**
- Enforces theoretically correct targets: 100% (Win), 300% (Podium), 500% (Top5), 1000% (Top10)
- Applies monotonic constraint enforcement (Win ≤ Podium ≤ Top5 ≤ Top10)
- Implements comprehensive re-normalization after constraint application
- Maintains mathematical validity while preserving competitive realism

### Disabled Adjustment Framework Commonalities

**Universal Rationale Themes**:
1. **Team Composition Variability**: All disabled sports acknowledge that relay teams change athlete lineups between races
2. **Individual vs Team Pattern Mismatch**: Historical individual adjustment patterns don't reliably predict team performance
3. **Sport-Specific Complexity**: Each sport has unique characteristics that compound team coordination challenges
4. **Conservative Stability Priority**: Disabled frameworks prioritize prediction stability over potential accuracy gains

**Common Implementation Pattern**:
```r
# Universal disabled adjustment logic across sports
period_correction <- if(is_relay) 0 else individual_adjustment
elevation_correction <- if(is_relay) 0 else individual_adjustment  
final_team_probability <- base_team_prediction + 0 + 0
```

### Sport-Specific Complexity Factors

**Biathlon's Dual-Discipline Challenge**:
- Skiing endurance + shooting precision coordination
- Team members with different dual-discipline specialization patterns
- Individual shooting accuracy patterns don't aggregate reliably to team performance
- `period_correction = if(is_relay) 0 else period_adjustment_value`

**Nordic Combined's Time Compensation Complexity**:
- Jumping performance determines cross-country starting positions
- Jumping specialists vs skiing specialists require different tactical approaches
- Time compensation strategies vary significantly with different team combinations
- `period_correction = if(is_relay && is_dual_discipline) 0 else adjustment_value`

**Ski Jumping's Venue-Dependent Specialization**:
- Five distinct hill categories (Small, Medium, Normal, Large, Flying)
- Technical precision specialists (smaller hills) vs distance specialists (larger hills)
- Strategic team composition optimization based on venue characteristics
- `period_correction = if(is_relay && is_venue_dependent) 0 else adjustment_value`

### Simplified Race Participation Probability Convergence

All four sports converge on **simplified race participation probability** for relay teams:
- **Standard approach**: `team_race_probability <- 1.0` for all participating teams
- **Rationale**: Team participation patterns differ from individual athlete attendance
- **Alternative to**: Complex exponential decay calculations used for individuals
- **Benefit**: Avoids unreliable historical team participation modeling

### Mathematical Normalization Preservation

Despite disabled individual-level adjustments, all sports maintain:
- **Probability normalization**: Target sum enforcement across position thresholds
- **Monotonic constraints**: Logical probability ordering (Win ≤ Podium ≤ Top5 ≤ Top10)
- **Mathematical validity**: Probability bounds [0,1] and theoretical consistency
- **Competitive realism**: Preservation of realistic probability distributions

### Cross-Sport Methodological Insights

**Why Cross-Country Enables Adjustments**:
1. **Leg-specific modeling sophistication** creates unique probability distribution challenges
2. **Multiple relay formats** (Standard, Team Sprint, Mixed) require format-specific corrections
3. **Technique awareness** (classic vs freestyle) adds modeling complexity requiring bias correction
4. **Advanced fallback systems** (XGBoost + GLM) create prediction interactions needing adjustment

**Why Other Sports Disable Adjustments**:
1. **Team composition variability** makes individual adjustment patterns unreliable
2. **Sport-specific complexity** (dual-discipline, venue-dependent, etc.) compounds prediction challenges
3. **Conservative stability priority** acknowledges limitations of team-based systematic bias correction
4. **Base model reliance** on team-aggregated metrics provides sufficient prediction foundation

### Future Development Opportunities

**Cross-Country Innovation Potential**:
- Mode detection algorithms could enhance other complex modeling scenarios
- Two-stage normalization approaches could improve probability calibration generally
- Format-specific adjustment strategies could benefit other multi-format sports

**Disabled Framework Insights**:
- Conservative approaches provide valuable stability lessons for complex prediction scenarios
- Team composition variability recognition could improve other team sport methodologies
- Sport-specific complexity documentation enhances methodological transparency

### Adjustment Framework Evolution

The analysis reveals that **relay probability training adjustments** represent an evolving frontier in sports prediction methodology where **Cross-Country's sophisticated enabled approach** establishes the upper bound of adjustment complexity while **other sports' disabled frameworks** provide essential insights into the limitations and challenges of systematic bias correction in team-based prediction contexts.

**Key Lesson**: Successful adjustment frameworks must balance mathematical sophistication with practical recognition of team dynamics limitations, with each sport's approach reflecting optimal solutions for their specific competitive characteristics and modeling complexity requirements that collectively advance understanding of team-based sports prediction adjustment methodology across winter sports disciplines.

## Relay Normalization and Monotonic Constraints

Winter sports relay probability predictions require sophisticated post-processing to ensure mathematical consistency and competitive realism. Each sport implements distinct normalization approaches that reflect their unique competitive characteristics while maintaining unified mathematical principles: target-sum normalization (ensuring probability sums match available positions), monotonic constraint enforcement (Win ≤ Podium ≤ Top5 ≤ Top10 ≤ Top30), and sport-specific error handling frameworks.

**Cross-Country: Multi-Stage Sophisticated Framework**: Cross-country implements the most advanced normalization system with five distinct stages: (1) Mode-based probability reset for systematic bias elimination, (2) Target-sum normalization with race participation weighting, (3) Enhanced monotonic constraint application with adjustment tracking, (4) Iterative re-normalization with convergence monitoring, and (5) Comprehensive final validation with quality assurance metrics. The framework accommodates three relay formats (Standard Relay, Team Sprint, Mixed Relay) with format-specific strategies including mode reset configuration, normalization targets, and gender-aware processing for Mixed Relay events.

**Nordic Combined: Unified Team Treatment**: Nordic Combined applies the same mathematical rigor to relay teams as individual athletes while incorporating dual-discipline complexity (ski jumping + cross-country skiing). The system uses target-sum normalization with race participation probability scaling that accounts for complex team assembly requirements across Team competitions, Mixed Team events, and Team Sprint formats. Monotonic constraints ensure logical probability ordering while acknowledging that teams with jumping specialists versus cross-country specialists exhibit different finishing position patterns across diverse event formats.

**Biathlon: Conservative Mathematical Consistency**: Biathlon employs comprehensive target-sum normalization with team-level probability capping while maintaining the sport's characteristic conservative approach. The framework ensures theoretical probability sums align with mathematical expectations (Win=1, Podium=3, Top5=5, Top10=10, Top30=30) through systematic normalization factors followed by monotonic constraint enforcement. Error handling includes bounds checking, convergence monitoring, and team composition validation across Relay and Mixed Relay formats.

**Ski Jumping: Venue-Dependent Normalization**: Ski jumping implements target-sum normalization adapted for the sport's unique venue-dependent competitive structure across five hill categories (Normal, Large, Flying, Ski Flying, Team Large). The system applies monotonic constraints with hill-specific considerations while maintaining mathematical validity across varying team sizes and competition formats. Probability distributions are normalized to reflect venue-dependent performance patterns while ensuring logical finishing position hierarchies.

**Mathematical Universality Across Sports**: Despite sport-specific implementation variations, all winter sports relay normalization systems maintain core mathematical principles: (1) Individual team probabilities bounded within [0,1], (2) Probability sums matching available positions (1 winner, 3 podium, etc.), (3) Monotonic ordering ensuring Win ≤ Podium ≤ Top5 ≤ Top10 ≤ Top30, and (4) Comprehensive error handling for edge cases. These unified mathematical foundations ensure prediction reliability across the diverse competitive landscapes of winter sports relay competitions while accommodating each sport's unique competitive characteristics and team composition requirements.

## Cross-Country Relay Fantasy

Cross-country skiing represents the pinnacle of relay fantasy complexity in winter sports, implementing sophisticated optimization algorithms across three distinct relay formats: Standard Relay (4-leg), Team Sprint (2-leg), and Mixed Relay (4-leg with gender constraints). The system employs format-specific Mixed Integer Programming (MIP) models that maximize expected points while managing budget constraints, gender balance requirements, and technique-specific team composition considerations unique to cross-country's diverse competitive landscape.

**Format-Specific Optimization Architecture**: Cross-country relay fantasy implements the most sophisticated constraint management system among winter sports, employing distinct optimization strategies for each relay format. Standard Relay fantasy utilizes technique-progressive leg importance weighting (20%, 20%, 25%, 35%) that emphasizes the freestyle anchor leg where tactical positioning becomes critical. Team Sprint fantasy adapts to technique-specific competitions (Classic vs Freestyle) with equal leg weighting (50%, 50%) and sprint-focused predictive modeling. Mixed Relay fantasy enforces the most complex constraint system, implementing gender-specific leg assignments (Female Classic, Male Classic, Female Freestyle, Male Freestyle) with combined athlete pool optimization across 12-team selection limits.

**Mathematical Foundation and Expected Points Calculation**: The fantasy framework calculates team expected points through weighted aggregation of individual leg probabilities using relay-specific point systems (200, 160, 120, 100, 90, 80...) and format-specific leg importance weights. Expected points employ probability-weighted calculations: Expected Points = Win_Prob × 200 + (Podium_Prob - Win_Prob) × mean(160,120) + (Top5_Prob - Podium_Prob) × mean(100,90,80,72) + (Top10_Prob - Top5_Prob) × mean(remaining top-10 values). This mathematical approach ensures that team selection optimizes realistic point expectations based on probabilistic performance projections rather than deterministic predictions.

**Knapsack Algorithm Implementation**: Cross-country relay fantasy employs sophisticated Mixed Integer Programming using the GLPK solver, implementing a multi-constraint knapsack algorithm that maximizes expected points subject to budget limitations (100,000 price units), team composition requirements (6 teams per gender for Standard/Team Sprint, 12 teams total for Mixed Relay), and format-specific constraints. The optimization framework utilizes binary decision variables for team selection, comprehensive constraint hierarchies for budget and composition management, and advanced solver capabilities that handle the combinatorial complexity inherent in multi-format relay team selection optimization.

**Cross-Format Integration and Competitive Intelligence**: The fantasy system provides comprehensive team selection capabilities that accommodate cross-country's extraordinary relay format diversity while maintaining consistent optimization principles. Standard Relay selections optimize across technique transitions and leg importance progression, Team Sprint selections adapt to technique-specific competition requirements with sprint performance emphasis, and Mixed Relay selections manage the most complex gender and technique constraint combinations in winter sports. This integrated approach delivers optimal team selections that reflect the unique competitive characteristics of each relay format while maximizing expected fantasy point returns across cross-country's diverse relay competitive landscape.








