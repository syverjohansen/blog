---
layout: post
title: "2024-25 Preview Part 2: World Cup Standings"
date: 2024-10-16
author: "Syver Johansen"
---

## World Cup Standings Predictions

### Data Setup

Elo data calculated from race data going back to 1920.  Race data includes World Cups, World Championships, and Olympic Winter Games as well as a handful of Holmenkollen races prior to the World Cup era that began in 1982.  A separate post about how Elo is calculated will be made.

Once Elo data was gathered, it was compiled into a df, saved into a Feather file, and read into RStudio where the bulk of analysis for this project was done.

With the elo data loaded, I had to set up my data and figure out what I needed for predictions.  For this post, I was mainly curious about two things: 1) World Cup Points predictions for the upcoming season and 2) Probability of a skier winning the overall, podiuming, etc.

To set up the data, I created a column that measured how many points a skier got in a given season.  I did this by filtering out Olympics, World Championships, and relay races and assigning points to World Cup races based on the kind of race (regular, stage, or Tour de Ski final).  Then, because every season has a unique number of races meaning a unique number of points, I standardized it by at seasons end creating a "Pct_of_Max_Points" column that showed the proportion of points a skier had to the most possible points for that given season.  

Next I did some imputting by replacing all NAs with the first quartile for that given season.  For example if the first quartile for a season's Distance Elo was 1200, all those who had NA in that column got 1200 entered there.  

Finally, I filtered the dataframe to only be season's end, since I don't care about individual race results, and I created lag variables of previous year's Elo results.  This was since the rows currently corresponded to the current status of points and elo, and I wanted to measure how their end of previous season's elo scores could predict the next season's.  I also created a lag variable of Pct_of_Max_Points.  Lastly, I only wanted recent seasons since the landscape of good cross country skiers has changed into more "hybrid" skiers succeeding at the top.  Also, I wanted to filter out any skiers who did not race 10 or more times in a given season.  This is because there were certain outliers like Sindre Skar and Justyna Kowalczyk who were not high volume racers during the season either due to being selective or not getting the nod from their national team, but still carried a very high elo from earlier in their career.  Obviously this disrupted predictions.


### Feature Selection

Next came getting the variables that best explained predicting Pct_of_Max_Points.  The method for this was a simple exhaustive search with a linear model and optimizing for adjusted R2 and BIC, with a preference for BIC should the models differ.  The model was trained on the 2019-2023 seasons, with the 2024 season being the test data set.  After running the script and looking at the top features, I chose Prev_Distance, Prev_Sprint, and Prev_Pct_of_Max_Points as my explanatory variables.  These were chosen as they were near the top model for R2 and BIC (for men and women), and had mutually exclusive and completely exhaustive parts, meaning a lack of multicollinearity.  

### Model Selection

Since since the dataset was relatively small, I was able to be picky about what type of model to use.  So out of curiosity, I benchmarked 12 different regression model types and evaluted them using different metrics.

The 12 different regression model types were

1. Linear
2. Log transformation of Pct_of_Max_Points
3. Square root transformation of Pct_of_Max_Points
4. Weighted regression (weighing residuals higher for higher performing skiers, to more accurately capture the top end)
5. Quantile regression
6. Polynomial regression (of the 3rd order, decided upon with evaluation)
7. Tobit regression
8. Spline regression
9. KNN regression (with finding optimal K value prior to)
10. GAM regression
11. XGBoost
12. Random Forest

and I measured the models with 4 metrics in 2 different ways

1. Custom R2 (R2, but weighing the residuals higher for the better skiers)
2. RMSE
3. MSE
4. MAE

and the two ways were

1. Taking the top 30 predicted and using the metrics on those
2. Taking the top 30 actual and using the metrics on those

The models went through a for loop of seasons from 2019 through 2024 where the year would be the test data and the other years were the training data, then the final score was calculated by averaging out the metric for that given season.

While the models bounced around depending on the metrics and how it was filtered, the GAM models were the only ones that were always ranked 1 to 3 no matter what.  The margins were slim, but its performance as well as its favorable residual plots let me to choosing it as the model for the season's predictions.


### Season Points Predictions


#### Men


{{< datatable "season-prediction.men-predictions" >}}


#### Ladies

{{< datatable "season-prediction.ladies-predictions" >}}


## World Cup Odds Predictions

For this one I tried a number of approaches and only one came out with results that made any amount of sense -- the old school binomial model.

#### Men


{{< datatable "season-prediction.men_placement-odds" >}}


#### Ladies

{{< datatable "season-prediction.ladies_placement-odds" >}}