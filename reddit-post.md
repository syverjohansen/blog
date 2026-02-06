To predict results at the Olympics, I have developed a simulation model to predict thousands of possible outcomes using the skiers' recent results and Elo scores.

**What is a Monte Carlo Simulation?**

Rather than predicting a single outcome like you would from binomial regression or linear regression, a Monte Carlo simulation runs many trials using created distributions for each skier to account for variance, then tallies up the results.

**What Data Goes Into the Model?**

*Elo Ratings:* Similar to chess, these ratings are calculated from World Cup/World Championship/Olympic results dating back to 1924 and are updated after each race. The Elo scores considered are specific to Overall, Distance, Sprint, Distance Classic, Distance Freestyle, Sprint Classic, Sprint Freestyle, Classic, and Freestyle - then converted to percentages of the maximum in the field. Athletes who have not competed in a top-level race have inferred Elo scores based on how they have performed against athletes in lesser formats who have competed in such races.

*Previous Points:* How many World Cup points an athlete has scored recently in races of the same distance type (sprint vs distance) and technique. These scores are subject to exponential decay - a race from last month gets ~94% weight, one from a year ago gets ~48%, and two years ago ~23%.

**How Are Individual Race Predictions Made?**

The model trains on historical data (specifically the last 10 years) and builds a Generalized Additive Model (GAM) that learns: given Elo ratings and recent results, what is the probability of finishing top-1, top-3, etc.? For feature selection, not all Elo types are relevant, so only the most relevant ones are chosen via lowest BIC score.

From these models and each athlete's own placements in recent races, we build a distribution for each athlete. This accounts for variance in their results, with more weight given to recent performances. From these distributions, a random performance is drawn for each skier, results are ranked, and outcomes are counted. The simulation is run 10,000 times.

**How Are Relay Teams Decided?**

Relay prediction is more complex because we have to select the optimal relay team for each nation and weigh the importance of each leg. Since the relay has a specific structure - legs 1-2 are classic and legs 3-4 are freestyle, with the anchor leg often valuing sprinting ability - we train separate models for each leg position using historical relay data.

Not all legs are weighted equally either; we measure how much each leg explains the overall relay result. Then for each nation, the model tries every possible combination of 4 athletes from the Olympic roster to find the lineup that maximizes podium probability. Once teams are selected, the Monte Carlo simulation runs 10,000 times using each leg's athlete Elo scores and recent results, weighted by leg importance.

Team sprint works the same way.

**How Are We Making Sure the Probabilities Are Accurate?**

Modeling percentages is tricky - you can either fall into a trap of: 1) selling your picks too hard ("so-and-so has a 100% chance of winning"), or 2) hedging too hard. The ideal scenario is to calibrate your model so that if the race was actually run 10,000 times, it captures reality most accurately. This requires setting the athlete distributions correctly.

The tunable parameters of the model are:
- *Decay Lambda:* How much weight to give recent vs older races
- *SD Scale Factor:* How much variability athletes have in simulation
- *Bounds on variance:* To prevent extreme predictions

These parameters were calibrated using historical races from 2018 onwards by finding values that minimize Brier score - a measure of how wrong probability predictions are. Separate calibration was done for individual races, relays, and team sprints.

**Output**

For each athlete in every race:
- Probabilities for win, podium, top-5, top-10, and top-30

For relays and team sprints:
- Nation-level probabilities
- Optimal team compositions (both podium-optimized and win-optimized)
- Per-leg athlete assignments
