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

Alpine skiing presents unique challenges in data collection because the sport consists of distinctly different disciplines with varying skill requirements. My system scrapes race startlists from FIS websites each morning, focusing exclusively on individual events since alpine skiing doesn't have team competitions.

The key innovation here is handling the unpredictable nature of FIS website formats. Unlike other winter sports that have more standardized data structures, alpine skiing results pages can vary significantly depending on the event type, venue, and timing. I've built multiple parsing approaches that automatically detect and adapt to different page layouts - from standard table formats to specialized event-specific displays.

When official startlists aren't available (which happens more often in alpine than other sports due to last-minute changes and weather delays), the system generates comprehensive mock startlists using all athletes who've competed in the current season. This ensures predictions can still be made even when official information is delayed.

The system tracks performance across alpine's diverse discipline spectrum: Downhill, Super-G, Giant Slalom, Slalom, and Combined events. Each discipline demands different skills - speed events versus technical events require fundamentally different athlete capabilities. To capture this complexity, I maintain separate ELO ratings for each discipline plus aggregate categories for Speed events (Downhill/Super-G) and Technical events (Giant Slalom/Slalom), allowing the prediction models to understand that a downhill specialist might struggle in slalom conditions.

#### Points

##### Training

###### Setup

Alpine skiing's training data setup is designed around the sport's unique characteristic: completely different disciplines that demand distinct skill sets. A downhill specialist and a slalom expert are almost different athletes entirely, so the system needs to understand these variations when building prediction models.

The foundation starts with historical race results going back 10 years. For each race result, I assign World Cup points based on finishing position, then calculate what I call "weighted previous points" - essentially looking at how a skier performed in their last 5 races in that specific discipline. Recent races get more weight (the most recent gets weight 5, then 4, 3, 2, 1), because current form matters more than results from months ago.

The clever part is using pre-race ELO ratings instead of current ones. This prevents "data leakage" - the model can't accidentally use information from after a race to predict that same race. These pre-race ELO ratings are calculated separately for each discipline (Downhill, Super-G, Giant Slalom, Slalom) plus broader categories (Speed events, Technical events), giving the model nuanced understanding of different skill sets.

I also add contextual information like which period of the season it is (early season in October versus peak season in February affects performance), and whether races are on technical slopes versus speed courses. The system focuses on competitive races by filtering to only include skiers who had ELO ratings within 75% of the top performer in each race - this eliminates amateur skiers and focuses on predicting the business end of World Cup races.

###### Feature Selection

To decide what variables to use in the model, we filter down to discipline specific features to choose from.  For Downhill/Super-G, we use pre-race Elo percentages for Overall, Downhill, Super-G, and Speed as well as the weighted previous points for the given discipline.  For Slalom/Giant Slalom it is Overall, Giant Slalom, Slalom, Technical and weighted previous points.  For combined, all available features are used.  

Then using an exhaustive approach, the explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables.  The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 

###### Modeling

Alpine skiing uses sophisticated Generalized Additive Models (GAM) that capture non-linear relationships between skiing performance and race results. The models adapt to alpine's fundamental challenge: speed disciplines (Downhill, Super-G) versus technical disciplines (Giant Slalom, Slalom) require completely different predictive approaches.

The system creates both points prediction models and position probability models. Points models predict how many World Cup points a skier will earn, while position models predict the probability of finishing in top-1, top-3, top-5, etc. The position models use binomial regression with REML estimation and are validated using Brier scores to ensure accurate probability predictions.

Alpine implements the most comprehensive fallback system among winter sports to handle the challenges of predicting across diverse disciplines:

  1. Full GAM with all BIC-selected variables and flexible smoothing
  2. Simplified GAM with reduced complexity and discipline-specific terms
  3. Linear regression if GAM approaches encounter fitting issues
  4. Simple ELO-only model as the ultimate fallback

This multi-tier approach ensures reliable predictions even when data sparsity occurs in specific discipline/athlete combinations.


###### Adjustments

Alpine skiing implements the most sophisticated adjustment system in winter sports, recognizing that skiers often have consistent patterns in how they perform relative to their base skill level. The system captures two critical alpine-specific patterns: seasonal performance variations and the fundamental difference between technical disciplines (Giant Slalom, Slalom) versus speed disciplines (Downhill, Super-G).

Seasonal adjustments account for skiers who consistently over- or under-perform during specific periods of the World Cup season. Some athletes excel early in the season when conditions are challenging, while others peak during major championship periods. The system uses statistical testing (p < 0.05) to identify these patterns and applies corrections accordingly.

The discipline-specific adjustments are unique to alpine skiing, capturing that some skiers are "technical specialists" who excel in slalom and giant slalom but struggle in downhill and super-G, while others are "speed specialists" with the opposite pattern. This reflects the fundamentally different skill sets required for technical precision versus high-speed racing.

The system also incorporates advanced volatility tracking, analyzing each skier's recent consistency and potential for both upside and downside performance. This helps identify skiers who are prone to breakthrough results or surprise disappointments, enhancing prediction accuracy across alpine's diverse discipline spectrum.

##### Testing

###### Startlist Setup

Alpine skiing's startlist setup transforms raw race data into prediction-ready datasets through sophisticated probability modeling and comprehensive feature engineering. The system integrates multiple data sources while handling the complexities of alpine's multi-discipline structure.

Race participation probabilities use exponential decay weighting that gives more importance to recent participation patterns. For confirmed FIS startlists, athletes receive 1.0 probability. For mock startlists, the system calculates probabilities using historical participation rates with exponential decay (α = 0.1), where recent races count more heavily than older ones.

ELO score processing retrieves the most current performance ratings for each skier and normalizes them to percentages by dividing by historical maximum values. This creates standardized 0-1 scale features across all discipline-specific ELO categories (Downhill, Super-G, Giant Slalom, Slalom, Technical, Speed, Overall).

Weighted previous points capture recent form using the last 5 races with linear increasing weights (1,2,3,4,5), giving more emphasis to the most recent performances. This helps identify athletes in peak form versus those struggling with current conditions.

Missing value imputation uses first quartile replacement for numeric variables and mode replacement for categorical variables, ensuring complete datasets while accounting for the fact that inexperienced athletes typically perform worse than established competitors.

###### Modeling

Alpine skiing's testing modeling applies sophisticated GAM models with comprehensive error handling and Individual Points Testing Adjustments that account for systematic biases in discipline-specific performance prediction. The system uses discipline-specific trained models while implementing statistical significance testing to identify genuine performance patterns during the testing phase.

**Individual Points Testing Adjustments for Discipline-Specific Performance**: Alpine testing employs a sophisticated sequential adjustment framework that accounts for the sport's unique multi-discipline structure and seasonal progression effects. The system uses rigorous statistical testing (p < 0.05 threshold with t-tests) to identify genuine patterns that require correction during the testing phase.

**Sequential Testing Adjustment Framework**: Adjustments are calculated and applied sequentially to avoid double-counting effects while maintaining chronological integrity:
1. **Period Adjustments**: Account for seasonal performance patterns as athletes' technical and speed capabilities develop throughout the alpine season
2. **Discipline Adjustments**: Capture the fundamental differences between technical disciplines (Giant Slalom, Slalom) and speed disciplines (Downhill, Super-G)

Each adjustment requires minimum 3 observations per category and uses comprehensive error handling with tryCatch blocks. The system combines adjustments additively while enforcing bounds (0-100 points) and integrating with race participation probability weighting for final predictions that balance historical bias correction with athlete participation likelihood.

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

Biathlon required a completely different approach to data collection because the IBU (International Biathlon Union) uses a modern web application structure that's fundamentally different from FIS websites. While most winter sports serve traditional HTML pages, the IBU website loads athlete data dynamically through JavaScript, requiring specialized extraction techniques.

The breakthrough came when I discovered that biathlon startlist data is embedded as JSON within the webpage's JavaScript code. This actually provides richer, more structured data than traditional HTML scraping - including detailed athlete information like IBU IDs, family names, given names, and precise startlist positions. The challenge was building robust parsing that could handle incomplete or truncated JSON data when network issues occur.

Biathlon's event structure is more complex than other sports, featuring individual races (Sprint, Pursuit, Individual, Mass Start) alongside various relay formats (standard relays, mixed relays, single mixed relays). The system automatically categorizes these event types and routes them to appropriate processing pipelines, ensuring that individual athlete data isn't contaminated with team performance metrics.

The sport's unique scoring system also required special handling. Unlike alpine skiing where disciplines are clearly distinct, biathlon events are interconnected - pursuit races use sprint results for starting positions, creating dependencies that needed to be tracked. I maintain separate ELO ratings for each race format while understanding their relationships, allowing the system to predict how a strong sprinter might perform in a subsequent pursuit race.

#### Points

##### Training

###### Setup

Biathlon training data setup reflects the sport's unique interconnected race structure. Unlike alpine skiing where each race stands alone, biathlon events flow into each other - sprint results determine pursuit starting positions, and athletes who excel in short sprints might struggle in longer individual races.

The system builds historical training data by recognizing these race type relationships. When calculating weighted previous points, I group by race type (Sprint, Pursuit, Individual, Mass Start) rather than treating all races equally. This captures that a biathlete's sprint form is more predictive of future sprint performance than their individual race results.

Biathlon also has environmental factors that significantly impact performance. Altitude affects both skiing endurance and shooting accuracy, so I include venue elevation data (venues above 1300 meters get flagged). The system tracks seasonal patterns too - some athletes struggle early in the season when conditions are harsh, while others peak during major championship periods.

The training data uses different point systems depending on race format. Regular races award points to top 40 finishers, while mass start races use a compressed scale for the top 30. Like other sports, I use pre-race ELO ratings to avoid data leakage, but biathlon's ELO system tracks race-type-specific performance: Sprint ELO, Individual ELO, Pursuit ELO, and Mass Start ELO, recognizing that excellence in one format doesn't guarantee success in another.

Given biathlon's smaller field sizes compared to alpine or cross-country, the system filters to the last 10 years and includes athletes with ELO ratings above 75% of the race leader, ensuring the models focus on World Cup-caliber competition while having enough data for robust training.

###### Feature Selection

Biathlon's feature selection reflects the sport's interconnected race format structure. The system adapts to whether we're predicting individual or relay events, since these require different performance metrics. For individual events, we consider weighted previous points plus race type-specific ELO ratings (Sprint, Pursuit, Individual, Mass Start). For relay events, we use team-averaged ELO ratings without weighted points.

Using an exhaustive search approach, the explanatory variables are selected by finding the combination that yields the lowest Bayesian Information Criterion (BIC), which balances model accuracy with simplicity to avoid overfitting by penalizing models with too many variables. The selected variables are then converted to smooth terms for Generalized Additive Models (GAM), which allows the model to capture non-linear relationships between predictors and performance. 

###### Modeling

Biathlon modeling reflects the sport's unique interconnected race structure, where sprint results directly affect pursuit starting positions and different race types require different tactical approaches. The system uses Generalized Additive Models optimized for biathlon's dual-discipline nature (skiing + shooting) and environmental factors like altitude.

The models adapt to whether we're predicting individual or relay events. Individual models incorporate race type-specific ELO ratings and weighted previous points, while relay models use team-aggregated performance metrics since team dynamics differ from individual competition patterns.

Biathlon implements a streamlined but effective fallback approach that reflects the sport's specialized nature and smaller competitive field compared to other winter sports. If the primary GAM model encounters fitting issues, the system falls back to a simplified model using only the most relevant ELO rating for that race type.

Position probability models use binomial GAM with REML estimation and are validated using Brier scores. The system includes sophisticated conditional adjustment calculations that differentiate between individual and relay events, ensuring appropriate treatment of each competition format.  

###### Adjustments

Biathlon implements a sophisticated conditional adjustment system that recognizes the fundamental difference between individual and relay events. For individual competitions, the system tracks both seasonal performance patterns and altitude effects, since biathlon's dual-discipline nature (skiing + shooting) means altitude affects both cardiovascular performance and shooting accuracy differently for each athlete.

The seasonal adjustments capture patterns where certain athletes consistently excel or struggle during specific periods of the biathlon season - early season when conditions are harsh, mid-season during tour events, or late season during championship periods. The system uses rigorous statistical testing (p < 0.05) to identify genuine patterns rather than random variations.

The altitude adjustments are unique to biathlon among winter sports, recognizing that venues above 1300 meters affect both skiing endurance and shooting precision. Some athletes thrive at altitude due to superior fitness, while others struggle with the breathing challenges and shooting stability at elevation.

Critically, the system excludes adjustments for relay events since relay teams can change composition between races. Unlike individual athletes who maintain consistent performance patterns, relay performance depends on team chemistry and tactical decisions that vary with different lineups. 

##### Testing

###### Startlist Setup

Biathlon's startlist setup accommodates the sport's unique dual-discipline requirements and diverse competition formats through sophisticated race-type-specific probability modeling and comprehensive feature engineering. The system handles both individual and relay formats while capturing biathlon's complex performance patterns.

Race participation probabilities are calculated separately for each race type (Sprint, Individual, Pursuit, Mass Start, Relay) using exponential decay weighting (α = 0.3) that considers the last 10 races. This recognizes that participation patterns differ significantly across biathlon's diverse event formats. Confirmed IBU startlists receive 1.0 probability, while mock startlists use historical participation rates with stronger recent weighting.

PELO (biathlon-specific ELO) score processing retrieves the most current performance ratings and normalizes them to percentages using historical maximum values. This ensures consistent model input across all race types while maintaining the sport's unique performance tracking requirements.

Race type-specific weighted previous points are calculated separately for each competition format, using the last 5 races with linear increasing weights (1,2,3,4,5). This captures how athletes perform differently across Sprint versus Distance events, recognizing that biathlon specialists often excel in specific formats.

The system adapts dynamically between individual and relay formats, using appropriate identification systems (Skier vs Nation) and performance metrics. Missing value imputation uses first quartile replacement with comprehensive fallback mechanisms, ensuring robust data quality across biathlon's complex competition structure.

###### Modeling

Biathlon's testing modeling applies race-type-specific GAM models with Individual Points Testing Adjustments that account for systematic biases in dual-discipline performance prediction. The system uses trained models optimized for each competition format while implementing statistical significance testing to identify genuine performance patterns during the testing phase.

**Individual Points Testing Adjustments for Dual-Discipline Performance**: Biathlon testing employs a sophisticated conditional adjustment framework that differentiates between individual and relay events. The system uses rigorous statistical testing (p < 0.05 threshold with t-tests) to identify genuine patterns while recognizing that relay teams change composition between races.

**Sequential Testing Adjustment Framework**: Adjustments are calculated and applied sequentially for individual events only:
1. **Period Adjustments**: Account for seasonal performance patterns in biathlon's interconnected race structure (Sprint, Pursuit, Individual, Mass Start)
2. **Elevation Adjustments**: Capture altitude effects on both skiing endurance and shooting accuracy (1300m threshold)

The system deliberately excludes adjustments for relay events due to changing team compositions. All adjustments require minimum 3 observations per category and use comprehensive error handling. Combined adjustments are integrated with race participation probability weighting to balance historical bias correction with dual-discipline performance prediction requirements.

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












