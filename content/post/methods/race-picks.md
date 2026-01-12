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

The models for position probabilities are binomial GAM models that use the independent threshold-based features from the feature selection and adapt to the specific discipline performance characteristics.  The binomial GAM models are for each position threshold (1st, 3rd, 5th, 10th, and 30th).  Each model uses binomial family GAM implementation with REML estimation for conservtive smoothing parameter selection, promoting stability and capturing non-linear relationships between Elo ratings, weighted previous points, and finishing position probabilities.  Model validation employs Brier score evaluation to assess probablistic accuracy across the different position thresholds and discipline types.

###### Adjustments

Adjustments for position probabilities are currently disabled in the production system to prevent "double-dipping" and overfitting concerns.

##### Testing

###### Startlist Setup

After the model training is performed, the startlist must be setup in a way that has all the features from the model as well as race participation probability if no startlist data was present on the IBU site.

Race participation probabilities use exponential decay weighting that gives more importance to recent participation patterns. For confirmed IBU startlists, athletes receive 1.0 probability. For mock startlists, the system calculates probabilities using historical participation rates with exponential decay (α = 0.3), where recent races count more heavily than older ones.

ELO score processing retrieves the most current performance ratings for each skier and normalizes them to percentages by dividing by the startlist's maximum values. This creates standardized 0-1 scale features across all discipline-specific ELO categories (Overall, Sprint, Pursuit, Individual, and Mass Start).

Weighted previous points capture recent form using the last 5 races with linear increasing weights (1,2,3,4,5), giving more emphasis to the most recent performances.

Missing value imputation uses first quartile replacement for numeric variables and mode replacement for categorical variables, ensuring complete datasets while accounting for the fact that inexperienced athletes typically perform worse than established competitors.

###### Modeling

The binomial GAM models with the chosen features from training are applied to the startlist data using direct mgcv package calls to generate threshold-independent probability predictions (1st, 3rd, 5th, 10th, and 30th). If needed, fallback measures are performed to handle prediction failures.

###### Adjustments

Adjustments for position probabilities are currently disabled in the production system to prevent "double-dipping" and overfitting concerns.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, normalization and monotonic constraints are applied.

Normalization is first applied so that the position probabilities all add up to the correct percentage.  For first place that sum to 100%, top-3 would sum to 300%, etc.  Individual athlete probabilities are capped at 100% since anything above that would be impossible.  

After normalization, monotonic constraints are added.  This ensures that top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so that an athlete cannot have a higher chance of finishing top-1 than top-3.  Then normalization is applied again to the monotonic constraint results to give the final results.

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

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, normalization and monotonic constraints are applied.

Normalization is first applied so that the position probabilities all add up to the correct percentage.  For first place that sum to 100%, top-3 would sum to 300%, etc.  Individual athlete probabilities are capped at 100% since anything above that would be impossible.  

After normalization, monotonic constraints are added.  This ensures that top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so that an athlete cannot have a higher chance of finishing top-1 than top-3.  Then normalization is applied again to the monotonic constraint results to give the final results.

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

Second, seasonal period adjustments capture how athletes perform during different phases of the World Cup season.  The five periods of the World Cup season are pre-Tour de Ski (I), Tour de Ski (II), between Tour de Ski and championships (or February 15 if no champs) (III), championships (IV), and post-championships (V). 

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

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, normalization and monotonic constraints are applied.

Normalization is first applied so that the position probabilities all add up to the correct percentage.  For first place that sum to 100%, top-3 would sum to 300%, etc.  Individual athlete probabilities are capped at 100% since anything above that would be impossible.  

After normalization, monotonic constraints are added.  This ensures that top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so that an athlete cannot have a higher chance of finishing top-1 than top-3.  Then normalization is applied again to the monotonic constraint results to give the final results.

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

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, normalization and monotonic constraints are applied.

Normalization is first applied so that the position probabilities all add up to the correct percentage.  For first place that sum to 100%, top-3 would sum to 300%, etc.  Individual athlete probabilities are capped at 100% since anything above that would be impossible.  

After normalization, monotonic constraints are added.  This ensures that top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so that an athlete cannot have a higher chance of finishing top-1 than top-3.  Then normalization is applied again to the monotonic constraint results to give the final results.

At this time, normalization and monotonic constraints are not applied to points predictions.

#### Fantasy

Cross-country has the unique aspect of using the results to optimize a fantasy team for Noah Hoffman's Fantasy XC.  For relay races and team sprints, one must optimize am 6-mens team/6-womens team roster for points while remaining in the 100,000 budget.  For mixed relays, it is 8 teams.  To do this Mixed Integer Programming (MIP) is employed using a GLPK with the points output to optimize the budget.  


## Nordic-Combined

### Individual

#### Data Gathering

Each day at midnight UTC, an automated Python scrape is performed on the FIS website to identify upcoming World Cup/World Championship/Olympic races.  When upcoming races are found, the startlist is scraped and saved.  

When official startlists aren't available (common due to weather delays and last-minute changes, FIS laziness), the system generates comprehensive mock startlists using all athletes who competed in the current season in order to maintain prediction capabilities.

In addition to the startlist roster, each skier is matched with historical skier data to get their most recent Elo scores for Overall, Sprint, Pursuit, Individual, and Mass Start.


#### Points

##### Training

###### Setup

The basis for our training dataset is to predict World Cup points for an upcoming race using historical Elo data and race performance data.  For this reason, we need to setup a dataframe that contains a column for Points, all the Elo data, and information about recent race performance.

The points column is created by mapping place to World Cup points for that place, so 1st place is 100, 2nd is 80, etc.

The Elo data is transformed so that the skier's elo prior to the race is turned into a percentage of the maximum elo for the participants in that race. This is so it is easier to predict results based on who is on the race vs an arbitrary elo score that is often subject to inflation throughout the course of the season.

Additionally, a column for weighted previous points is created that takes the last 5 points for a specific discipline (Sprint, Individual, Individual Compact, and Mass Start). So in a row for a Sprint result, the weighted previous points will have that skiers last five results weighted so the most recent has a weight of 5, the one before 4, etc. 

Lastly, the training data is filtered to the last 10 seasons and missing values are imputed with the first quartile value for the given race. However, unlike the other sports the data is not filtered down to skiers who have an Elo percentage of 75% or greater, and missing values are imputed with the first quartile value for the given race.

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








