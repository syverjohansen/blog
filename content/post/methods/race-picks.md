---
title: "Race Picks Methodology"
date: 2020-01-01T01:00:00+00:00
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

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, a 7-phase normalization and monotonic constraint process is applied to position probabilities:

1. **Scale to target sum**: Probabilities are scaled so they sum to the correct total (100% for top-1, 300% for top-3, etc.)

2. **Cap at 100% and redistribute**: Individual athlete probabilities are capped at 100% since anything above would be impossible. Excess probability is redistributed proportionally to athletes below 100%.

3. **Monotonic constraints**: Ensures top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so an athlete cannot have a higher chance of finishing top-1 than top-3.

4. **Re-normalize**: After monotonic adjustment, probabilities are re-scaled to maintain target sums.

5. **Cap at 100% again**: Any values pushed above 100% by re-normalization are capped.

6. **Final monotonic constraint**: A second pass ensures no inversions were introduced by re-normalization.

7. **Final cap at start probability**: No position probability can exceed the athlete's participation probability. An athlete with 80% chance of starting cannot have more than 80% chance of any finishing position.

At this time, normalization and monotonic constraints are not applied to points predictions.

## Biathlon

### Individual

#### Data Gathering

Each day at midnight UTC, an automated Python scrape is performed on the IBU website to identify upcoming World Cup/World Championship/Olympic races.  When upcoming races are found, the startlist is scraped and saved.  

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

Race participation probabilities use exponential decay weighting that gives more importance to recent participation patterns. For confirmed IBU startlists, athletes receive 1.0 probability. For mock startlists, the system calculates probabilities using historical participation rates with exponential decay (α = 0.1), where recent races count more heavily than older ones.

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

The models for position probabilities are binomial GAM models that use the independent threshold-based features from the feature selection and adapt to the specific discipline performance characteristics.  The binomial GAM models are for each position threshold (1st, 3rd, 5th, 10th, and 30th).  Each model uses binomial family GAM implementation with REML estimation for conservtive smoothing parameter selection, promoting stability and capturing non-linear relationships between Elo ratings, weighted previous points, and finishing position probabilities.  Model validation employs Brier score evaluation to assess probablistic accuracy across the different position thresholds and discipline types.

###### Adjustments

Adjustments for position probabilities are currently disabled in the production system to prevent "double-dipping" and overfitting concerns.

##### Testing

###### Startlist Setup

After the model training is performed, the startlist must be setup in a way that has all the features from the model as well as race participation probability if no startlist data was present on the IBU site.

Race participation probabilities use exponential decay weighting that gives more importance to recent participation patterns. For confirmed IBU startlists, athletes receive 1.0 probability. For mock startlists, the system calculates probabilities using historical participation rates with exponential decay (α = 0.1), where recent races count more heavily than older ones.

ELO score processing retrieves the most current performance ratings for each skier and normalizes them to percentages by dividing by the startlist's maximum values. This creates standardized 0-1 scale features across all discipline-specific ELO categories (Overall, Sprint, Pursuit, Individual, and Mass Start).

Weighted previous points capture recent form using the last 5 races with linear increasing weights (1,2,3,4,5), giving more emphasis to the most recent performances.

Missing value imputation uses first quartile replacement for numeric variables and mode replacement for categorical variables, ensuring complete datasets while accounting for the fact that inexperienced athletes typically perform worse than established competitors.

###### Modeling

The binomial GAM models with the chosen features from training are applied to the startlist data using direct mgcv package calls to generate threshold-independent probability predictions (1st, 3rd, 5th, 10th, and 30th). If needed, fallback measures are performed to handle prediction failures.

###### Adjustments

Adjustments for position probabilities are currently disabled in the production system to prevent "double-dipping" and overfitting concerns.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, a 7-phase normalization and monotonic constraint process is applied to position probabilities:

1. **Scale to target sum**: Probabilities are scaled so they sum to the correct total (100% for top-1, 300% for top-3, etc.)

2. **Cap at 100% and redistribute**: Individual athlete probabilities are capped at 100% since anything above would be impossible. Excess probability is redistributed proportionally to athletes below 100%.

3. **Monotonic constraints**: Ensures top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so an athlete cannot have a higher chance of finishing top-1 than top-3.

4. **Re-normalize**: After monotonic adjustment, probabilities are re-scaled to maintain target sums.

5. **Cap at 100% again**: Any values pushed above 100% by re-normalization are capped.

6. **Final monotonic constraint**: A second pass ensures no inversions were introduced by re-normalization.

7. **Final cap at start probability**: No position probability can exceed the athlete's participation probability. An athlete with 80% chance of starting cannot have more than 80% chance of any finishing position.

At this time, normalization and monotonic constraints are not applied to points predictions.

### Relay

#### Data Gathering

Similar to the individual data gathering, a web scraper is employed at midnight UTC daily to see if there are any upcoming races for that day.  If there is a relay race, it checks to see if the race is a normal relay, a mixed relay, or a single mixed relay.  

For the relays, the skiers and their legs are extracted.  The names are matched with skiers from local databases and latest Elo scores are paired with them to go in the final startlist data frame. For mixed relays, sex assignments are obtained through fuzzy matching names from the startlist to historical results.

In addition to individual elo assignment, average elos for the team are created by summing the Elos of the skiers scraped and dividing by the number of legs.  


#### Points

##### Training

###### Setup

The goal of the training setup for relays is to create a dataframe that measures historical success of relay teams based on the Elo scores.

To accomplish this, the points column is created by assigning place of the team to World Cup points.  Then the average Elo scores for each team are made by averaging each skiers Elo score prior to the race and then taking the percentage of the maximum Elo score.  Since we're most interested in the top-end of the races and want to account for any trends in relay results, the results are filtered to Elo percentages greater than 75% and for the last 10 years.  

At this time, weighted average World Cup points are not included in the model, however, that is something that may change in the future.


###### Feature Selection

To decide what variables to use in the model, we use all the Elo percent columns.

Then using an exhaustive approach, the explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 

###### Modeling

The models used to predict points are Generalized Additive Models (GAM) that capture non-linear relationships between Elo scores and World Cup points.

In the case that there is insufficient data, the following fallback measures are in store:

  1. Full GAM with all BIC-selected variables and flexible smoothing
  2. Simplified GAM with reduced complexity and discipline-specific terms
  3. Linear regression if GAM approaches encounter fitting issues
  4. Simple ELO-only model as the ultimate fallback 

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

##### Testing

###### Startlist Setup

Because the average team elos for the team are already created during the data gathering phase and weighted point averages are not used as features, the only thing done during startlist setup is calculating the Average Elo percentage for each of the teams.  Participation probability is not needed for relay as predictions are not made if there is no startlist scraped.

###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

#### Probability

##### Training

###### Setup

For probability predictions, the places from the points training dataframe are converted into binary classification for position probability modeling across five finishing position thresholds. The system uses the same preprocessed historical race data as points models but transforms the regression problem into classification through binary outcome creation.

Position thresholds are defined as Win, Podium, Top 5, Top 10, and Top 30 finishes. Each threshold creates a separate binary classification problem where success is defined as finishing at or better than that position. This enables binomial GAM modeling while maintaining consistency with discipline-specific performance characteristics. 

Elo percentages, weighted previous points, missing value imputation, and data filtering are all performed the same way as the points predictions

At this time, weighted average World Cup points are not included in the model, however, that is something that may change in the future.

###### Feature Selection

To decide what variables to use in the model, we use all the Elo percent columns.

Then using an exhaustive approach, the explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 


###### Modeling

The models for position probabilities are binomial GAM models that use the independent threshold-based features from the feature selection and adapt to the specific discipline performance characteristics.  The binomial GAM models are for each position threshold (1st, 3rd, 5th, 10th, and 30th).  Each model uses binomial family GAM implementation with REML estimation for conservtive smoothing parameter selection, promoting stability and capturing non-linear relationships between Elo ratings and finishing position probabilities.  Model validation employs Brier score evaluation to assess probablistic accuracy across the different position thresholds and discipline types.

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

##### Testing

###### Startlist Setup

Because the average team elos for the team are already created during the data gathering phase and weighted point averages are not used as features, the only thing done during startlist setup is calculating the Average Elo percentage for each of the teams.  Participation probability is not needed for relay as predictions are not made if there is no startlist scraped.

###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, a 7-phase normalization and monotonic constraint process is applied to position probabilities:

1. **Scale to target sum**: Probabilities are scaled so they sum to the correct total (100% for top-1, 300% for top-3, etc.)

2. **Cap at 100% and redistribute**: Individual athlete probabilities are capped at 100% since anything above would be impossible. Excess probability is redistributed proportionally to athletes below 100%.

3. **Monotonic constraints**: Ensures top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so an athlete cannot have a higher chance of finishing top-1 than top-3.

4. **Re-normalize**: After monotonic adjustment, probabilities are re-scaled to maintain target sums.

5. **Cap at 100% again**: Any values pushed above 100% by re-normalization are capped.

6. **Final monotonic constraint**: A second pass ensures no inversions were introduced by re-normalization.

7. **Final cap at start probability**: No position probability can exceed the athlete's participation probability. An athlete with 80% chance of starting cannot have more than 80% chance of any finishing position.

At this time, normalization and monotonic constraints are not applied to points predictions.

## Cross-Country

### Individual

#### Data Gathering

Each day at midnight UTC, an automated Python scrape is performed on the FIS website to identify upcoming World Cup/World Championship/Olympic races.  When upcoming races are found, the startlist is scraped and saved.

When official startlists aren't available (common due to weather delays and last-minute changes, FIS laziness), the system generates comprehensive mock startlists using all athletes who competed in the current season in order to maintain prediction capabilities.  Moreover, FIS quota data for the nations are registered to be used later for participation probability.

For integration with Noah Hoffman's Fantasy XC, the prices for each of the athletes on the startlist are entered. 

In addition to the startlist roster, each skier is matched with historical skier data to get their most recent Elo scores for Overall, Distance, Distance Freestyle, Distance Classic, Sprint, Sprint Freestyle, Sprint Classic, Freestyle, and Classic.  


#### Points

##### Training

###### Setup

The basis for our training dataset is to predict World Cup points for an upcoming race using historical Elo data and recent race performance data.  To this, we need to establish a dataframe that has columns for points, Elo data, and information about recent race performance.

To create the points column, the places of the skiers in the historical dataframe are mapped to points.  Since we predict for many different types of events that use different points systems (relay, World Cup, stage, Tour de Ski), the mappings are done based on which one we are predicting.  For example, if we are predicting a stage race, we the points column will be using the stage points system.  

To handle Elo data, the historical elo data is calculated so that we use the percentage of the maximum pre-race elo for a given race.  So if there is a race on January, 1, 2018, we find the maximum pre-race elos for each type and then divide each athletes score by that amount.  

For recent race performances, we use weighted average for the given discipline.  For example, if we are predicting a Distance Classic race, we filter the historic dataframe down to Distance Classic races, and then we take the weighted average of a given skier's points over their most recent 5 performances with the most recent being given a weight of 5 and the 5th most recent given a weight of 1.  

After the points, weighted points, and elo columns are created, we filter down to the last 10 seasons and for skiers who are at least 75% of the maximum Elo.  Missing data values are imputed with first quartile values.



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

Cross-country uses three types of adjustments for the initial predictions: altitude, seasonal period, and mass start.  Adjustments add or subtract points depending on if an athlete has a history of statitically significant difference (p<0.05) in their results versus the predicted value given the presence of one of the three defined factors.

First, altitude adjustments are for athletes who show significantly different results at altitude (1300m) vs not.

Second, seasonal period adjustments capture how athletes perform during different phases of the World Cup season.  The five periods are determined by race count within each season: races 1-5 (I), races 6-10 (II), races 11-15 (III), races 16-20 (IV), and races 21+ (V). 

Third, mass start versus individual start in distance races.

All adjustments require statistical significance, a minimum of 3 observations in the comparison category, sequential calculation to prevent interaction effects, and temporal ordering (only using prior race data for each calculation).

##### Testing

###### Startlist Setup

The objective of the startlist setup is to transform the scraped startlist into a dataframe that can use the selected features to predict points.  Elo percentages are created, weighted point averages are added, and race participation probability is performed if a mock startlist is being used.

To do this Elo percentages are created by dividing the Elo columns divided by the maximum value in that column.  Weighted point averages are calculated by finding the previous races of the technique and distance for the race we're predicting and calculating the weighted average (5 for most recent, 1 for 5th most recent). Furthermore, missing values are imputed with first quartile values. 

Lastly, if a mock startlist is being used, race participation probabilities are calculated using exponential decay weighting with technique-specific matching meaning skiers who have competed in more races for a given discipline recently, they will have a higher participation probability.


###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

Points adjustments from the training data are applied to the athletes who qualify for them.

#### Probability

##### Training

###### Setup

The training dataset for probability predictions is nearly identical to the points one.  The only difference is creating binary position thresholds to identify when a skier in the training set met the threshold.  First we filter down to the type of race we are predicting and then apply the the threshold.  For distance races the thresholds are top-1, 3, 5, 10, and 30.  For sprint races, we use top-1, 3, 6, 12, and 30 to reflect winning, podium, final, semi-final, and quarterfinal.
 

###### Feature Selection

Position probability feature selection employs threshold-independent optimization with discipline-specific adaptation. The system uses the same explanatory variable pools as the points model while performing an exhaustive independent BIC optimization for each position threshold.  Selected variables are converted to smooth terms for binomial GAM modeling.

###### Modeling

The binomial GAM models with the chosen features from training are applied to the startlist data using direct mgcv package calls to generate threshold-independent probability predictions (1st, 3rd, 5th, 10th, and 30th). If needed, fallback measures are performed to handle prediction failures.  

###### Adjustments

Adjustments are performed for altitude, period, and mass start for the position probablilities in the same way that they are for points.  

##### Testing

###### Startlist Setup

The startlist for probability predictions is the exact same one as used for the points predictions.

###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

Points adjustments from the training data are applied to the athletes who qualify for them.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, a 7-phase normalization and monotonic constraint process is applied to position probabilities:

1. **Scale to target sum**: Probabilities are scaled so they sum to the correct total (100% for top-1, 300% for top-3, etc.)

2. **Cap at 100% and redistribute**: Individual athlete probabilities are capped at 100% since anything above would be impossible. Excess probability is redistributed proportionally to athletes below 100%.

3. **Monotonic constraints**: Ensures top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so an athlete cannot have a higher chance of finishing top-1 than top-3.

4. **Re-normalize**: After monotonic adjustment, probabilities are re-scaled to maintain target sums.

5. **Cap at 100% again**: Any values pushed above 100% by re-normalization are capped.

6. **Final monotonic constraint**: A second pass ensures no inversions were introduced by re-normalization.

7. **Final cap at start probability**: No position probability can exceed the athlete's participation probability. An athlete with 80% chance of starting cannot have more than 80% chance of any finishing position.

At this time, normalization and monotonic constraints are not applied to points predictions.

#### Fantasy

Cross-country has the unique aspect of using the results to optimize a fantasy team for Noah Hoffman's Fantasy XC.  For individual races, one must optimize am 8-man/8-woman roster for points while remaining in the 100,000 budget.  To do this Mixed Integer Programming (MIP) is employed using a GLPK with the points output to optimize the budget.  

### Relay

#### Data Gathering

Similar to the individual data gathering, a web scraper is employed at midnight UTC daily to see if there are any upcoming races for that day.  If there is a relay race, it checks to see if the race is a normal relay, a mixed relay, or a single mixed relay.  

For the relays, the skiers and their legs are extracted.  The names are matched with skiers from local databases and latest Elo scores are paired with them to go in the final startlist data frame. For mixed relays, sex assignments are obtained through fuzzy matching names from the startlist to historical results.

In addition to individual elo assignment, average elos for the team are created by summing the Elos of the skiers scraped and dividing by the number of legs.  

Like individual data gathering, integration with the Fantasy XC API is performed to gather prices for the teams.

#### Points

##### Training

###### Setup

Training data setup for cross-country relays and team sprint is perhaps the most sophisticated of all prediction types.  In contrast to biathlon, nordic combined and ski jumping, cross-country does not use average elos on the team as a predictor, but rather trains on the individuals who make up the team to created an expected value for probability from which expected points are extracted.  If no startlist is present, a team is made which optimizes for podium probability.

To prepare the startlist for feature section, each skier gets its own individual pre-race elo percentage columns and weighted last five.  The weighted last five for relays is distance classic races for legs 1 and 2, and distance freestyle races for legs 3 and 4.  For team sprints weighted last 5 is sprint races in the technique for which the race takes place.  Lastly, the dataset adds relay points based on place and puts binary position thresholds for places 1, 3, 5, and 10.  

Missing data is imputted with first quartile values and the seasons are filtered to the last 11 years.  

###### Feature Selection

For relays, feature selection is broken into classic legs (1 & 2), freestyle chase leg (3), and anchor leg (4).  The available explanatory variables for the classic legs are pre-race elos for Distance Classic, Classic, Overall, and weighted last 5 distance classic races.  For the freestyle leg it is Distance Freestyle, Freestyle, Overall, and weighted last 5 distance freestyle races.  Then for anchor it is Distance Freestyle, Freestyle, Sprint, Overall, and Weighted Last 5 for freestyle. 

For team sprints it is dependent on the technique of the race.  For classic races it is Elos for Sprint, Sprint Classic, and Classic, while for freestyle it is Sprint, Sprint Freestyle and Freestyle.


###### Modeling

Modeling for cross-country relays differs from the other sports in that trains seperate models for each relay leg position with position-specific feature selection.  Using the features chosen from the section above, the leg models are trained using a bionomial GAM with REML estimation for winning, podium, top-5, and top-10. 

Then to estimate final results, leg importance is trained where certain legs are given more importance to final results than others.  Points are then predicted by multiplying the predicted points for each leg by the importance of the leg.

Additionally, cross-country relays have XGBoost and GLM fallbacks if the binomial GAM fails.


###### Adjustments

No adjustments are applied to relay races.

##### Testing

###### Startlist Setup

The startlist dataframe is loaded in and Elo percentages and weighted previous points are made for each skier on the startlist depending on the type of relay and the technique the skier will be performing.  

If there is no startlist scraped, the optimal 4-person team for a podium position for each nation is created from available skiers.

###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

No adjustments are made for relay races.

#### Probability

##### Training

###### Setup

The same dataset is used for training probability predictions as points predictions.  However, binary classification for position thresholds for top-1, 3, 5, 10, and 30 are added.  Furthermore, models are selected based on the amount of training data.  Training datasets with fewer than 500 observations use GLM, while larger ones use an xgbTree.  

###### Feature Selection

For relays, feature selection is broken into classic legs (1 & 2), freestyle chase leg (3), and anchor leg (4).  The available explanatory variables for the classic legs are pre-race elos for Distance Classic, Classic, Overall, and weighted last 5 distance classic races.  For the freestyle leg it is Distance Freestyle, Freestyle, Overall, and weighted last 5 distance freestyle races.  Then for anchor it is Distance Freestyle, Freestyle, Sprint, Overall, and Weighted Last 5 for freestyle. 

For team sprints it is dependent on the technique of the race.  For classic races it is Elos for Sprint, Sprint Classic, and Classic, while for freestyle it is Sprint, Sprint Freestyle and Freestyle.


###### Modeling

Unlike other models, the position probability models for cross-country use XGBoost for large datasets (n>500), and a glm for smaller ones.  Moreover, 5-fold cross-validation with standardized parameters are used to account for the temporal dependencies and team composition variations within each team.  Formulas are created for win, podium, top-5, and top-10 and stored to be used for the testing.  

###### Adjustments

No adjustments are applied for relay position probability.

##### Testing

###### Startlist Setup

The startlist dataframe is loaded in and Elo percentages and weighted previous points are made for each skier on the startlist depending on the type of relay and the technique the skier will be performing.  

If there is no startlist scraped, the optimal 4-person team for a podium position for each nation is created from available skiers.

###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

No adjustments are made for relay races.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, a 7-phase normalization and monotonic constraint process is applied to position probabilities:

1. **Scale to target sum**: Probabilities are scaled so they sum to the correct total (100% for top-1, 300% for top-3, etc.)

2. **Cap at 100% and redistribute**: Individual athlete probabilities are capped at 100% since anything above would be impossible. Excess probability is redistributed proportionally to athletes below 100%.

3. **Monotonic constraints**: Ensures top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so an athlete cannot have a higher chance of finishing top-1 than top-3.

4. **Re-normalize**: After monotonic adjustment, probabilities are re-scaled to maintain target sums.

5. **Cap at 100% again**: Any values pushed above 100% by re-normalization are capped.

6. **Final monotonic constraint**: A second pass ensures no inversions were introduced by re-normalization.

7. **Final cap at start probability**: No position probability can exceed the athlete's participation probability. An athlete with 80% chance of starting cannot have more than 80% chance of any finishing position.

At this time, normalization and monotonic constraints are not applied to points predictions.

#### Fantasy

Cross-country has the unique aspect of using the results to optimize a fantasy team for Noah Hoffman's Fantasy XC.  For relay races and team sprints, one must optimize am 6-mens team/6-womens team roster for points while remaining in the 100,000 budget.  For mixed relays, it is 8 teams.  To do this Mixed Integer Programming (MIP) is employed using a GLPK with the points output to optimize the budget.  


## Nordic Combined

### Individual

#### Data Gathering

Each day at midnight UTC, an automated Python scrape is performed on the FIS website to identify upcoming World Cup/World Championship/Olympic races.  When upcoming races are found, the startlist is scraped and saved.

When official startlists aren't available (common due to weather delays and last-minute changes, FIS laziness), the system generates comprehensive mock startlists using all athletes who competed in the current season in order to maintain prediction capabilities.

In addition to the startlist roster, each skier is matched with historical skier data to get their most recent Elo scores for Overall, Individual, Individual Compact, Sprint, and Mass Start.


#### Points

##### Training

###### Setup

The basis for our training dataset is to predict World Cup points for an upcoming race using historical Elo data and race performance data.  For this reason, we need to setup a dataframe that contains a column for Points, all the Elo data, and information about recent race performance.

The points column is created by mapping place to World Cup points for that place, so 1st place is 100, 2nd is 80, etc.

The Elo data is transformed so that the skier's elo prior to the race is turned into a percentage of the maximum elo for the participants in that race. This is so it is easier to predict results based on who is on the race vs an arbitrary elo score that is often subject to inflation throughout the course of the season.

Additionally, a column for weighted previous points is created that takes the last 5 points for a specific discipline (Sprint, Individual, Individual Compact, and Mass Start). So in a row for a Sprint result, the weighted previous points will have that skiers last five results weighted so the most recent has a weight of 5, the one before 4, etc.

Lastly, the training data is filtered to the last 10 seasons and to skiers who have an Elo percentage of 75% or greater. Missing values are imputed with the first quartile value for the given race.

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

After the model training is performed, the startlist must be setup in a way that has all the features from the model as well as race participation probability if no startlist data was present on the FIS site.

Race participation probabilities use exponential decay weighting that gives more importance to recent participation patterns. For confirmed FIS startlists, athletes receive 1.0 probability. For mock startlists, the system calculates probabilities using historical participation rates with exponential decay (α = 0.1), where recent races count more heavily than older ones.

Elo score processing retrieves the most current performance ratings for each skier and normalizes them to percentages by dividing by the startlist's maximum values. This creates standardized 0-1 scale features across all discipline-specific ELO categories (Overall, Individual, Individual Compact, and Mass Start).

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

The models for position probabilities are binomial GAM models that use the independent threshold-based features from the feature selection and adapt to the specific discipline performance characteristics.  The binomial GAM models are for each position threshold (1st, 3rd, 5th, 10th, and 30th).  Each model uses binomial family GAM implementation with REML estimation for conservtive smoothing parameter selection, promoting stability and capturing non-linear relationships between Elo ratings, weighted previous points, and finishing position probabilities.  Model validation employs Brier score evaluation to assess probablistic accuracy across the different position thresholds and discipline types.

###### Adjustments

Adjustments for position probabilities are currently disabled in the production system to prevent "double-dipping" and overfitting concerns.

##### Testing

###### Startlist Setup

After the model training is performed, the startlist must be setup in a way that has all the features from the model as well as race participation probability if no startlist data was present on the FIS site.

Race participation probabilities use exponential decay weighting that gives more importance to recent participation patterns. For confirmed FIS startlists, athletes receive 1.0 probability. For mock startlists, the system calculates probabilities using historical participation rates with exponential decay (α = 0.1), where recent races count more heavily than older ones.

ELO score processing retrieves the most current performance ratings for each skier and normalizes them to percentages by dividing by the startlist's maximum values. This creates standardized 0-1 scale features across all discipline-specific ELO categories (Overall, Individual, Individual Compact, Sprint, and Mass Start).

Weighted previous points capture recent form using the last 5 races with linear increasing weights (1,2,3,4,5), giving more emphasis to the most recent performances.

Missing value imputation uses first quartile replacement for numeric variables and mode replacement for categorical variables, ensuring complete datasets while accounting for the fact that inexperienced athletes typically perform worse than established competitors.

###### Modeling

The binomial GAM models with the chosen features from training are applied to the startlist data using direct mgcv package calls to generate threshold-independent probability predictions (1st, 3rd, 5th, 10th, and 30th). If needed, fallback measures are performed to handle prediction failures.

###### Adjustments

Adjustments for position probabilities are currently disabled in the production system to prevent "double-dipping" and overfitting concerns.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, a 7-phase normalization and monotonic constraint process is applied to position probabilities:

1. **Scale to target sum**: Probabilities are scaled so they sum to the correct total (100% for top-1, 300% for top-3, etc.)

2. **Cap at 100% and redistribute**: Individual athlete probabilities are capped at 100% since anything above would be impossible. Excess probability is redistributed proportionally to athletes below 100%.

3. **Monotonic constraints**: Ensures top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so an athlete cannot have a higher chance of finishing top-1 than top-3.

4. **Re-normalize**: After monotonic adjustment, probabilities are re-scaled to maintain target sums.

5. **Cap at 100% again**: Any values pushed above 100% by re-normalization are capped.

6. **Final monotonic constraint**: A second pass ensures no inversions were introduced by re-normalization.

7. **Final cap at start probability**: No position probability can exceed the athlete's participation probability. An athlete with 80% chance of starting cannot have more than 80% chance of any finishing position.

At this time, normalization and monotonic constraints are not applied to points predictions.

### Relay

#### Data Gathering

Similar to the individual data gathering, a web scraper is employed at midnight UTC daily to see if there are any upcoming races for that day.  If there is a relay race, it checks to see if the race is a normal relay, a mixed relay, or a single mixed relay.  

For the relays, the skiers and their legs are extracted.  The names are matched with skiers from local databases and latest Elo scores are paired with them to go in the final startlist data frame. For mixed relays, sex assignments are obtained through fuzzy matching names from the startlist to historical results.

In addition to individual elo assignment, average elos for the team are created by summing the Elos of the skiers scraped and dividing by the number of legs.  


#### Points

##### Training

###### Setup

The goal of the training setup for relays is to create a dataframe that measures historical success of relay teams based on the Elo scores.

To accomplish this, the points column is created by assigning place of the team to World Cup points.  Then the average Elo scores for each team are made by averaging each skiers Elo score prior to the race and then taking the percentage of the maximum Elo score.  Since we're most interested in the top-end of the races and want to account for any trends in relay results, the results are filtered to Elo percentages greater than 75% and for the last 10 years.  

At this time, weighted average World Cup points are not included in the model, however, that is something that may change in the future.


###### Feature Selection

To decide what variables to use in the model, we use all the Elo percent columns.

Then using an exhaustive approach, the explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 

###### Modeling

The models used to predict points are Generalized Additive Models (GAM) that capture non-linear relationships between Elo scores and World Cup points.

In the case that there is insufficient data, the following fallback measures are in store:

  1. Full GAM with all BIC-selected variables and flexible smoothing
  2. Simplified GAM with reduced complexity and discipline-specific terms
  3. Linear regression if GAM approaches encounter fitting issues
  4. Simple ELO-only model as the ultimate fallback 

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

##### Testing

###### Startlist Setup

Because the average team elos for the team are already created during the data gathering phase and weighted point averages are not used as features, the only thing done during startlist setup is calculating the Average Elo percentage for each of the teams.  Participation probability is not needed for relay as predictions are not made if there is no startlist scraped.

###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

#### Probability

##### Training

###### Setup

For probability predictions, the places from the points training dataframe are converted into binary classification for position probability modeling across five finishing position thresholds. The system uses the same preprocessed historical race data as points models but transforms the regression problem into classification through binary outcome creation.

Position thresholds are defined as Win, Podium, Top 5, Top 10, and Top 30 finishes. Each threshold creates a separate binary classification problem where success is defined as finishing at or better than that position. This enables binomial GAM modeling while maintaining consistency with discipline-specific performance characteristics. 

Elo percentages, weighted previous points, missing value imputation, and data filtering are all performed the same way as the points predictions

At this time, weighted average World Cup points are not included in the model, however, that is something that may change in the future.

###### Feature Selection

To decide what variables to use in the model, we use all the Elo percent columns.

Then using an exhaustive approach, the explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 


###### Modeling

The models for position probabilities are binomial GAM models that use the independent threshold-based features from the feature selection and adapt to the specific discipline performance characteristics.  The binomial GAM models are for each position threshold (1st, 3rd, 5th, 10th, and 30th).  Each model uses binomial family GAM implementation with REML estimation for conservtive smoothing parameter selection, promoting stability and capturing non-linear relationships between Elo ratings and finishing position probabilities.  Model validation employs Brier score evaluation to assess probablistic accuracy across the different position thresholds and discipline types.

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

##### Testing

###### Startlist Setup

Because the average team elos for the team are already created during the data gathering phase and weighted point averages are not used as features, the only thing done during startlist setup is calculating the Average Elo percentage for each of the teams.  Participation probability is not needed for relay as predictions are not made if there is no startlist scraped.

###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, a 7-phase normalization and monotonic constraint process is applied to position probabilities:

1. **Scale to target sum**: Probabilities are scaled so they sum to the correct total (100% for top-1, 300% for top-3, etc.)

2. **Cap at 100% and redistribute**: Individual athlete probabilities are capped at 100% since anything above would be impossible. Excess probability is redistributed proportionally to athletes below 100%.

3. **Monotonic constraints**: Ensures top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so an athlete cannot have a higher chance of finishing top-1 than top-3.

4. **Re-normalize**: After monotonic adjustment, probabilities are re-scaled to maintain target sums.

5. **Cap at 100% again**: Any values pushed above 100% by re-normalization are capped.

6. **Final monotonic constraint**: A second pass ensures no inversions were introduced by re-normalization.

7. **Final cap at start probability**: No position probability can exceed the athlete's participation probability. An athlete with 80% chance of starting cannot have more than 80% chance of any finishing position.

At this time, normalization and monotonic constraints are not applied to points predictions.

## Ski Jumping

### Individual

#### Data Gathering

Each day at midnight UTC, an automated Python scrape is performed on the FIS website to identify upcoming World Cup/World Championship/Olympic races.  When upcoming races are found, the startlist is scraped and saved.  

When official startlists aren't available (common due to weather delays and last-minute changes, FIS laziness), the system generates comprehensive mock startlists using all athletes who competed in the current season in order to maintain prediction capabilities.

In addition to the startlist roster, each skier is matched with historical skier data to get their most recent Elo scores for Overall, Small Hill, Medium Hill, Normal Hill, Large Hill, and Flying.


#### Points

##### Training

###### Setup

The basis for our training dataset is to predict World Cup points for an upcoming race using historical Elo data and race performance data.  For this reason, we need to setup a dataframe that contains a column for Points, all the Elo data, and information about recent race performance.

The points column is created by mapping place to World Cup points for that place, so 1st place is 100, 2nd is 80, etc.

The Elo data is transformed so that the skier's elo prior to the race is turned into a percentage of the maximum elo for the participants in that race. This is so it is easier to predict results based on who is on the race vs an arbitrary elo score that is often subject to inflation throughout the course of the season.

Additionally, a column for weighted previous points is created that takes the last 5 points for a specific hill size (Small Hill, Medium Hill, Normal Hill, Large Hill, and Flying). So in a row for a Large Hill result, the weighted previous points will have that skiers last five results weighted so the most recent has a weight of 5, the one before 4, etc.

Lastly, the training data is filtered to the last 10 seasons and missing values are imputed with the first quartile value for the given race. Unlike the other sports, the data is not filtered down to skiers who have an Elo percentage of 75% or greater.

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

There are two post-model adjustments applied to skiers for the points prediction.  The first is World Cup period and the second is for hill size.  The period adjustment is for the different stages (1-4) of the World Cup season as some skiers perform better earlier or later in the season than others.  The hill size adjustment is if skiers perform better or worse on different hill sizes than they're predicted for, they will get an adjustment.  The determination if an adjustment is needed Actual Points - Predicted Points with a p-value of p < 0.05.

##### Testing

###### Startlist Setup

After the model training is performed, the startlist must be setup in a way that has all the features from the model as well as race participation probability if no startlist data was present on the FIS site.

Race participation probabilities use exponential decay weighting that gives more importance to recent participation patterns. For confirmed FIS startlists, athletes receive 1.0 probability. For mock startlists, the system calculates probabilities using historical participation rates with exponential decay (α = 0.1), where recent races count more heavily than older ones.

Elo score processing retrieves the most current performance ratings for each skier and normalizes them to percentages by dividing by the startlist's maximum values. This creates standardized 0-1 scale features across all discipline-specific ELO categories (Overall, Small Hill, Medium Hill, Normal Hill, Large Hill, and Flying).

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

The models for position probabilities are binomial GAM models that use the independent threshold-based features from the feature selection and adapt to the specific discipline performance characteristics.  The binomial GAM models are for each position threshold (1st, 3rd, 5th, 10th, and 30th).  Each model uses binomial family GAM implementation with REML estimation for conservtive smoothing parameter selection, promoting stability and capturing non-linear relationships between Elo ratings, weighted previous points, and finishing position probabilities.  Model validation employs Brier score evaluation to assess probablistic accuracy across the different position thresholds and discipline types.

###### Adjustments

Adjustments for position probabilities are currently disabled in the production system to prevent "double-dipping" and overfitting concerns.

##### Testing

###### Startlist Setup

After the model training is performed, the startlist must be setup in a way that has all the features from the model as well as race participation probability if no startlist data was present on the FIS site.

Race participation probabilities use exponential decay weighting that gives more importance to recent participation patterns. For confirmed FIS startlists, athletes receive 1.0 probability. For mock startlists, the system calculates probabilities using historical participation rates with exponential decay (α = 0.1), where recent races count more heavily than older ones.

ELO score processing retrieves the most current performance ratings for each skier and normalizes them to percentages by dividing by the startlist's maximum values. This creates standardized 0-1 scale features across all discipline-specific ELO categories (Overall, Small Hill, Medium Hill, Normal Hill, Large Hill, and Flying).

Weighted previous points capture recent form using the last 5 races with linear increasing weights (1,2,3,4,5), giving more emphasis to the most recent performances.

Missing value imputation uses first quartile replacement for numeric variables and mode replacement for categorical variables, ensuring complete datasets while accounting for the fact that inexperienced athletes typically perform worse than established competitors.

###### Modeling

The binomial GAM models with the chosen features from training are applied to the startlist data using direct mgcv package calls to generate threshold-independent probability predictions (1st, 3rd, 5th, 10th, and 30th). If needed, fallback measures are performed to handle prediction failures.

###### Adjustments

Adjustments for position probabilities are currently disabled in the production system to prevent "double-dipping" and overfitting concerns.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, a 7-phase normalization and monotonic constraint process is applied to position probabilities:

1. **Scale to target sum**: Probabilities are scaled so they sum to the correct total (100% for top-1, 300% for top-3, etc.)

2. **Cap at 100% and redistribute**: Individual athlete probabilities are capped at 100% since anything above would be impossible. Excess probability is redistributed proportionally to athletes below 100%.

3. **Monotonic constraints**: Ensures top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so an athlete cannot have a higher chance of finishing top-1 than top-3.

4. **Re-normalize**: After monotonic adjustment, probabilities are re-scaled to maintain target sums.

5. **Cap at 100% again**: Any values pushed above 100% by re-normalization are capped.

6. **Final monotonic constraint**: A second pass ensures no inversions were introduced by re-normalization.

7. **Final cap at start probability**: No position probability can exceed the athlete's participation probability. An athlete with 80% chance of starting cannot have more than 80% chance of any finishing position.

At this time, normalization and monotonic constraints are not applied to points predictions.

### Relay

#### Data Gathering

Similar to the individual data gathering, a web scraper is employed at midnight UTC daily to see if there are any upcoming races for that day.  If there is a relay race, it checks to see if the race is a normal relay, a mixed relay, or a single mixed relay.  

For the relays, the skiers and their legs are extracted.  The names are matched with skiers from local databases and latest Elo scores are paired with them to go in the final startlist data frame. For mixed relays, sex assignments are obtained through fuzzy matching names from the startlist to historical results.

In addition to individual elo assignment, average elos for the team are created by summing the Elos of the skiers scraped and dividing by the number of legs.  


#### Points

##### Training

###### Setup

The goal of the training setup for relays is to create a dataframe that measures historical success of relay teams based on the Elo scores.

To accomplish this, the points column is created by assigning place of the team to World Cup points.  Then the average Elo scores for each team are made by averaging each skiers Elo score prior to the race and then taking the percentage of the maximum Elo score.  Since we're most interested in the top-end of the races and want to account for any trends in relay results, the results are filtered to Elo percentages greater than 75% and for the last 10 years.  

At this time, weighted average World Cup points are not included in the model, however, that is something that may change in the future.


###### Feature Selection

To decide what variables to use in the model, we use all the Elo percent columns.

Then using an exhaustive approach, the explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 

###### Modeling

The models used to predict points are Generalized Additive Models (GAM) that capture non-linear relationships between Elo scores and World Cup points.

In the case that there is insufficient data, the following fallback measures are in store:

  1. Full GAM with all BIC-selected variables and flexible smoothing
  2. Simplified GAM with reduced complexity and discipline-specific terms
  3. Linear regression if GAM approaches encounter fitting issues
  4. Simple ELO-only model as the ultimate fallback 

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

##### Testing

###### Startlist Setup

Because the average team elos for the team are already created during the data gathering phase and weighted point averages are not used as features, the only thing done during startlist setup is calculating the Average Elo percentage for each of the teams.  Participation probability is not needed for relay as predictions are not made if there is no startlist scraped.

###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

#### Probability

##### Training

###### Setup

For probability predictions, the places from the points training dataframe are converted into binary classification for position probability modeling across five finishing position thresholds. The system uses the same preprocessed historical race data as points models but transforms the regression problem into classification through binary outcome creation.

Position thresholds are defined as Win, Podium, Top 5, Top 10, and Top 30 finishes. Each threshold creates a separate binary classification problem where success is defined as finishing at or better than that position. This enables binomial GAM modeling while maintaining consistency with discipline-specific performance characteristics. 

Elo percentages, weighted previous points, missing value imputation, and data filtering are all performed the same way as the points predictions

At this time, weighted average World Cup points are not included in the model, however, that is something that may change in the future.

###### Feature Selection

To decide what variables to use in the model, we use all the Elo percent columns.

Then using an exhaustive approach, the explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 


###### Modeling

The models for position probabilities are binomial GAM models that use the independent threshold-based features from the feature selection and adapt to the specific discipline performance characteristics.  The binomial GAM models are for each position threshold (1st, 3rd, 5th, 10th, and 30th).  Each model uses binomial family GAM implementation with REML estimation for conservtive smoothing parameter selection, promoting stability and capturing non-linear relationships between Elo ratings and finishing position probabilities.  Model validation employs Brier score evaluation to assess probablistic accuracy across the different position thresholds and discipline types.

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

##### Testing

###### Startlist Setup

Because the average team elos for the team are already created during the data gathering phase and weighted point averages are not used as features, the only thing done during startlist setup is calculating the Average Elo percentage for each of the teams.  Participation probability is not needed for relay as predictions are not made if there is no startlist scraped.

###### Modeling

The models from the training phase are applied to the startlist data to provide the initial points predictions.

###### Adjustments

No adjustments are made for relay races as lineups change between races and sample sizes of relays are too small to identify trends for adjustments.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, a 7-phase normalization and monotonic constraint process is applied to position probabilities:

1. **Scale to target sum**: Probabilities are scaled so they sum to the correct total (100% for top-1, 300% for top-3, etc.)

2. **Cap at 100% and redistribute**: Individual athlete probabilities are capped at 100% since anything above would be impossible. Excess probability is redistributed proportionally to athletes below 100%.

3. **Monotonic constraints**: Ensures top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so an athlete cannot have a higher chance of finishing top-1 than top-3.

4. **Re-normalize**: After monotonic adjustment, probabilities are re-scaled to maintain target sums.

5. **Cap at 100% again**: Any values pushed above 100% by re-normalization are capped.

6. **Final monotonic constraint**: A second pass ensures no inversions were introduced by re-normalization.

7. **Final cap at start probability**: No position probability can exceed the athlete's participation probability. An athlete with 80% chance of starting cannot have more than 80% chance of any finishing position.

At this time, normalization and monotonic constraints are not applied to points predictions.








