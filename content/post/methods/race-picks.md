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

Startlists are scraped from the FIS websites at midnight UTC on the day of a race. For alpine, only individual events are scraped.  If startlists are not present on the FIS site, a mock startlist is created using all skiers who have competed in the current season.

Startlists are further enhanced using latest Elo scores for each of the skiers on the startlist.  The Elo scores used are for Overall, each of the disciplines, Downhill, Super-G, Giant Slalom, Slalom, Combined) as well as categorical for Speed (Downhill/Super-G) and Technical (Giant Slalom/Slalom).

#### Points

##### Training

###### Setup

The model we intend to create will use World Cup points as the response variable and some combination of the Elo variables and weighted previous World Cup points as the explanatory variables with later adjustments based on period and discipline.

To do this, we calculate previous World Cup points by assigning a points column based on the place of the skier in a given race.  Then we also create an explanatory variable for weighted previous World Cup points where for the given discipline, we calculate how the skier did in their last five races and give a weighted average where the most recent race gets a weight of 5 the one before a 4, etc.

Next, to account for possible changes in the sport in what makes a good skier and to focus more on the accurately predicting the top end of the results sheet, we filter down to the last 10 years of results and only include skiers with an Elo score that is 75% or more of the top Elo score in a given race.

###### Feature Selection

To decide what variables to use in the model, we filter down to discipline specific features to choose from.  For Downhill/Super-G, we use pre-race Elo percentages for Overall, Downhill, Super-G, and Speed as well as the weighted previous points for the given discipline.  For Slalom/Giant Slalom it is Overall, Giant Slalom, Slalom, Technical and weighted previous points.  For combined, all available features are used.  

Then using an exhaustive approach, the explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 

###### Modeling

GAM models are trained to predict World Cup points using the variables selected above.  If the model fails due to failures with feature selection, GAM fitting, too sparse of data or collinearity, there exist the following fallback levels:

  1. Full GAM with all selected variables and flexible smoothing
  2. Simplified GAM with reduced complexity if the full model fails
  3. Linear regression if GAM approaches don't work
  4. Simple ELO-only model as a final fallback


###### Adjustments

For individual skiers, adjustments to predicted points are done based on their historical success in certain periods of the World Cup season (up to 4 periods).  For example if a skier typically overperforms in a certain period, they will get an adjustment based on how well they usually perform in a period versus how they are predicted to perform (p < 0.05).  Moreover, there are discipline-specific adjustments for skiers who consistently over/underperform in Technical vs Speed events.  When these significant patterns are detected (0.05), adjustments are applied to account for individual athlete quirks.

##### Testing

###### Startlist Setup

Prepares the scraped startlist for predictions.  First it creates a race participation probability for each athlete.  If the startlist was grabbed from the FIS site, the probability will be set to 1 (confirmed participation).  Otherwise, if the startlist includes all skiers from the current season, the probability is calculated by examining either all races in the given discipline since the skier started competing or the last five years (whichever period is shorter), then calculating the proportion of available races the skier actually competed in.

After participation probabilities have been added, the most recent Elo scores are grabbed for each skier and Elo percents are calculated by dividing the scores by the maximum value in the race.  Additionally, the weighted point averages of the last five race results in the specific discipline are calculated for each skier.

Finally with all the explanatory variables prepared for the model, missing values (NAs) are imputed with the first quartile value for the given variable to account for lack of experience being a detriment.

###### Modeling

Applies the trained GAM models to the prepared startlist data to generate point predictions for each athlete.

#### Probability

##### Training

###### Setup

Uses the same training data setup as for points, but instead of using points as the response variable, binary outcomes based on finishing positions are used.  Place thresholds are created for top-1, 3, 5, 10, and 30 where the value is 1 if the skier achieved that finishing position or better in the race, and 0 otherwise. 

###### Feature Selection

Uses the same BIC optimization method as the points model, but feature selection is performed separately for each position threshold (top-1, 3, 5, 10, and 30) to identify the best predictors for each finishing position category.

###### Modeling

For these position thresholds (top-1, 3, 5, etc.), GAM models are trained to predict their probabilities.  Because they are 1/0 outcomes, the models use binomial regression.  Model accuracy is measured using Brier scores, which evaluate how close the predicted probabilities are to what actually happened.  These Brier scores penalize overconfident and underconfident probability predictions.

###### Adjustments

No adjustments for position probabilities.

##### Testing

###### Startlist Setup

Testing uses same startlist setup as points prediction with addition of position-specific probability columns for each threshold.

###### Modeling

Position probability prediction applies trained binomial GAM models to startlist data, generating probabilities for each threshold.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, normalization and monotonic constraints are applied.

Normalization is first applied so that the position probabilities all add up to the correct percentage.  For first place that sum to 100%, top-3 would sum to 300%, etc.  Individual athlete probabilities are capped at 100% since anything above that would be impossible.  

After normalization, monotonic constraints are added.  This ensures that top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so that an athlete cannot have a higher chance of finishing top-1 than top-3.  Then normalization is applied again to the monotonic constraint results to give the final results.

## Biathlon

### Individual

#### Data Gathering

Startlists are scraped from the IBU website at midnight UTC on the day of a race and individual races are filtered from relay races.  If a startlist is not present, a mock startlist is created by using the biathletes who have competed in the current season.    

Startlists are further enhanced using latest Elo scores for each of the skiers on the startlist.  The Elo scores used are for Overall, and each of the disciplines: Sprint, Pursuit, Individual, and Mass Start.  Missing data is imputed with first quartile data.

#### Points

##### Training

###### Setup

The model we intend to create will use World Cup points as the response variable and some combination of the Elo variables and weighted previous World Cup points as the explanatory variables with later adjustments based on period of the season and altitude (≥1300m).

To do this, we calculate previous World Cup points by assigning a points column based on the place of the skier in a given race.  Then we also create an explanatory variable for weighted previous World Cup points where for the given discipline, we calculate how the skier did in their last five races and give a weighted average where the most recent race gets a weight of 5 the one before a 4, etc.

Next, to account for possible changes in the sport in what makes a good skier and to focus more on the accurately predicting the top end of the results sheet, we filter down to the last 10 years of results and only include skiers with an Elo score that is 75% or more of the top Elo score in a given race.

###### Feature Selection

The explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 

###### Modeling

GAM models (using REML method) are trained to predict World Cup points using the BIC-selected variables converted to smooth terms.  If the model fails due to feature selection errors, GAM fitting issues, or data sparsity, the system falls back to a simplified GAM using only the Overall Elo percentage.  

###### Adjustments

For individual biathletes, adjustments to predicted points are done based on their historical success in certain periods of the World Cup season (up to 4 periods) and at altitude levels.  For example if a skier typically overperforms in a certain period, they will get an adjustment based on how well they usually perform in a period versus how they are predicted to perform (p < 0.05).  Moreover, if a skier skies much better at altitude than not (p < 0.05), they would get a positive adjustment (negative if much worse).  Same if they performed much better when not at altitude compared to altitude. 

##### Testing

###### Startlist Setup

Prepares the scraped startlist for predictions.  First it creates a race participation probability for each athlete.  If the startlist was grabbed from the IBU site, the probability will be set to 1 (confirmed participation).  Otherwise, if the startlist includes all skiers from the current season, the probability is calculated by examining either all races in the given discipline since the skier started competing or the last five years (whichever period is shorter), then calculating the proportion of available races the skier actually competed in.

After participation probabilities have been added, the most recent Elo scores are grabbed for each skier and Elo percents are calculated by dividing the scores by the maximum value in the race.  Additionally, the weighted point averages of the last five race results in the specific discipline are calculated for each skier.

Finally with all the explanatory variables prepared for the model, missing values (NAs) are imputed with the first quartile value for the given variable to account for lack of experience being a detriment.

###### Modeling

Applies the trained GAM models (fitted using REML method) to the prepared startlist data to generate point predictions for each athlete.

###### Adjustments

Period and altitude adjustments learned during training are applied to base predictions.

#### Probability

##### Training

###### Setup

Uses the same training data setup as for points, but instead of using points as the response variable, binary outcomes based on finishing positions are used.  Place thresholds are created for top-1, 3, 5, 10, and 30 where the value is 1 if the biathlete achieved that finishing position or better in the race, and 0 otherwise. 

###### Feature Selection

Uses the same BIC optimization method as the points model, but feature selection is performed separately for each position threshold (top-1, 3, 5, 10, and 30) to identify the best predictors for each finishing position category.

###### Modeling

For these position thresholds (top-1, 3, 5, etc.), GAM models (using REML) are trained to predict their probabilities.  Because they are 1/0 outcomes, the models use binomial regression.  Model accuracy is measured using Brier scores, which evaluate how close the predicted probabilities are to what actually happened.  These Brier scores penalize overconfident and underconfident probability predictions.

###### Adjustments

No adjustments are made for position probabilities.

##### Testing

###### Startlist Setup

Testing uses same startlist setup as points prediction with addition of position-specific probability columns for each threshold.

###### Modeling

Position probability prediction applies trained binomial GAM models to startlist data, generating probabilities for each threshold.

###### Adjustments

No adjustments are made for position probabilities.

#### Normalization and Monotonic Constraints

After modeling is complete, points and position probabilities are multiplied by race participation probabilities.  For example, a skier with an estimated World Cup points of 80 with a participation probability of 80% would get a final estimation of 64 points.  After this, normalization and monotonic constraints are applied.

Normalization is first applied so that the position probabilities all add up to the correct percentage.  For first place that sum to 100%, top-3 would sum to 300%, etc.  Individual athlete probabilities are capped at 100% since anything above that would be impossible.  

After normalization, monotonic constraints are added.  This ensures that top-1 ≤ top-3 ≤ top-5 ≤ top-10 ≤ top-30, so that an athlete cannot have a higher chance of finishing top-1 than top-3.  Then normalization is applied again to the monotonic constraint results to give the final results.

### Relay

#### Data Gathering

Startlists for relays are also gathered from the IBU website at midnight UTC on the day of the race.  There are three types of relays: Relay, Mixed Relay, and Single Mixed Relay.  Historical data exists for each relay type that is filtered separately from the scrape.  

Similar to the individuals predictions, Elo scores for the skiers are matched with the skiers in the relay as well as their leg information.  For Mixed Relays, gender detection is performed by matching athletes against both men's and women's Elo databases.  Furthermore, the average elo score for the team in each of the Elo types is calculated.

#### Points

##### Training

###### Setup

Similar to the Individual, World Cup points are used as response variable.  Explanatory variables are set up as the average prior to the race Elo scores for each discipline for the skiers on each team.  Then those averages are converted to the percentage of the maximum value for the Elo type for the given race.  Finally, the same filtering is performed for last 10 seasons of results and average Elo score being at least 75% of the maximum Elo score.

###### Feature Selection

Same as individual, except team-level Elo averaging variables replace individual Elo variables and weighted average points are not used.

###### Modeling

Relay points models use the same GAM approach as individuals.

###### Adjustments

No adjustments are made for relay races.

##### Testing

###### Startlist Setup

Relay startlist setup uses nation as the participant.  Otherwise the same as individual.

###### Modeling

Trained relay GAM models applied to relay startlists using the same prediction pipeline as individuals, but with team-aggregated features.

#### Probability

##### Training

###### Setup

Relay position probability training uses the same binary outcome approach for thresholds (top-1, 3, 5, 10, 30).  

###### Feature Selection

Identifical feature selection as relay points models with team-aggregated Elo variables and performs BIC optmization for each position threshold.

###### Modeling

Team position models use GAM with REML with team-aggregated smooth terms.  Brier score evlauation applied to team-level predictions.

###### Adjustments

No adjustments for relays.

##### Testing

###### Startlist Setup

Uses same relay startlist setup as points prediction with addition of team position-specific probability columns for each threshold.

###### Modeling

Team position probability prediction applies trained binomial GAM models to relay startlist data, generating probabilities for each threshold at the nation level.

###### Adjustments

No adjustments for relays.

#### Normalization and Monotonic Constraints

Same as individual.

## Cross-Country

### Individual

#### Data Gathering

Individual race startlists are scraped from the FIS websites at midnight UTC on the day of a race.  If startlists are not present on the FIS site, a mock startlist is created using all skiers who have competed in the current season where it constrains the number of skiers from each nation by using FIS quotas.

Startlists are enriched using the latest Elo scores for each skier.  The Elo scores used are for Overall, Distance, Distance Classic, Distance Freestyle. Sprint, Sprint Classic, Sprint Freestyle, Freestyle, and Classic. 

#### Points

##### Training

###### Setup

The model uses World Cup points as the reponse variable and some combination of the Elo variables and weighted previous World Cup points as explanatory variables with later adjustments based on altitude, period of season, and whether the race is a mass start or not.

Previous World Cup points are calculated by assigning a points column based on the place of the skier in a given race using either World Cup or stage race points, depending on whether we are predicting a stage race or not.  Weighted previous World Cup points are calculated where for the given race type and technique combination, the skier's last five races get a weighted average where the most recent race gets a weight of 5, the one before a 4, etc.  Furthermore, elo percentages for skiers prior to the races are calculated by dividing the Elo score by the maximum Elo score in that race.

To focus on accurately predicting the top end of the results sheet, the system filters down to the last 10 years of results and only includes skiers with an Elo score that is 75% or more of the top Elo score in a given race.  Periods are also assigned to the training data so that it can be used for adjustments later.

###### Feature Selection

The explanatory variables are: weighted previous points, and Elo percentages for: Overall, Distance, Distance Freestyle, Distance Classic, Sprint, Sprint Freestyle, Sprint Classic, Freestyle, Classic.  

Using an exhaustive search approach, explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The chosem variables are smoothed for the Generalized Additive Model (GAM).

###### Modeling

GAN models are trained to predict World Cup points using the selected variables.  GAM is chosen to allow the capture of non-linear relationships between predictors and performance.

###### Adjustments

For individual races, adjustments to predicted points are performed based on the historical success of individual skiers at altitude, periods of the World Cup season, and mass start format races (n≥3)e.  For example if a skier typically overperforms at altitude (≥1300m), they will get a positive adjustment.  Similarly, adjustments are made for World Cup period performance of the skier and mass start vs individual start formats using the same statitical significance threshold (p < 0.05).

##### Testing

###### Startlist Setup

If the startlist was scraped from the FIS website, all athletes receive a participation probability of 100% (confirmed participation).  For mock startlists (created when FIS data is unavailable), participation probabilities are calculated by examining the athlete's historical participation rate in similar races.  Then it performs Elo score processing where it finds the most recent Elo scores for each athlete and calculates their percentage of the max for the racers in the startlist.  Additionally, weighted previous points are calculated for each skier given the race type and technique.  Finally, missing values are imputed with the first quartile from their columns to account for lack of experience being a detriment to performance.


###### Modeling

Applies the trained GAM models to the prepared startlist data to generate point predictions for each athlete.

###### Adjustments

Applies the adjustments from training for altitude, period, and mass start effects.

#### Probability

##### Training

###### Setup

The probability models use the same training data setup as the points prediction but convert the problem to binary classification for different finishing position thresholds.  For sprint races it is top-1, 3, 6, 12, and 30 in order to imitate winning, podium, finals, semifinals, and quarterfinals.  For the other races it is the standard 1, 3, 5, 10, and 30.  The same filtered dataset for the last 10 years and Elo percentage above 75% is used.  

###### Feature Selection

THe probability models use the same BIC optimization as the points prediction, but with feature selection performed independently for each position threshold to identify the optimal predictors for different finishing position categories.  Once the variables are selected, they are converted to smooth terms for binomial GAM modeling.  

###### Modeling

Binomisl GAM models are trained for each position threshold using the optmized variables from the feature selection.  The models use REML estimation and use Brier scores to assess prediction quality.  

###### Adjustments

Position probability adjustments follow the same sequential methodology as points prediction, but operate on probability residuals rather than point differences.  Each position threshold gets independent adjustments and the adjustments are constrained to [0, 1].

##### Testing

###### Startlist Setup

Probability testing uses the same startlist setup as points prediction with preparing for position-specific probability predictions for the thresholds rather than for predicting points.

###### Modeling

Position probability predictions apply the trained binomial GAM models to the startlist data and generates probabilities for each threshold.

###### Adjustments

If adjustments are available, they are applied with probability bounds enforcement.

### Relay

#### Data Gathering

Relay startlists are scraped from the FIS website using three scripts for different relay types: standard single gender relay, mixed relays, and team sprints.  The members of each team and their legs are scraped and the startlists are setup to gather both individual team member elos and aggregate team elos.  

#### Points

##### Training

###### Setup

Relay predictions differ from individual races in that they combined both historical relay race results as well as the individual attributes of the skiers that race them.  Like the individual races, a points column is assigned based on historical places.  Then for the legs, Elo data is matched from the most recent Elo scores as well as their previous points weighted average for classic if they're legs 1 or 2, or freestyle of they're legs 3 or 4.  The training data is then filtered down the last 11 years to preserve maximum training data.

Mixed relays set the female legs for 1 and 3, while the mens legs for 2 and 4.  Team Sprints will filter individual races down to the the technique that will be used in the race.

###### Feature Selection

Relay feature selection uses a rule-based, technique-specific approach instead of BIC.  The selection is deterministic and based on leg position and technique type.  Classic legs will consider Distance Classic, Classic, Distance, Overall Elos and weighted last 5 points for the features.  Freestyle legs will consider Distance Freestyle, Freestyle, Distance, Overall Elos, and weighted last 5 freestyle points for theirs with the exception of the anchor leg for standard relays which will also consider sprint Elo.  The missing values are imputed with the first quartile values like they are for individual races.


###### Modeling

Relay modeling uses leg-specific 

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

#### Probability

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

###### Adjustments

#### Normalization and Monotonic Constraints

#### Setup

#### Predictions

## Nordic-Combined

### Individual

#### Data Gathering

#### Points

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

###### Adjustments

#### Probability

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

###### Adjustments

### Relay

#### Data Gathering

#### Points

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

#### Probability

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

#### Normalization and Monotonic Constraints

## Ski Jumping

### Individual

#### Data Gathering

#### Points

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

###### Adjustments

#### Probability

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

###### Adjustments

### Relay

#### Data Gathering

#### Points

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

#### Probability

##### Training

###### Setup

###### Feature Selection

###### Modeling

###### Adjustments

##### Testing

###### Startlist Setup

###### Modeling

#### Normalization and Monotonic Constraints












