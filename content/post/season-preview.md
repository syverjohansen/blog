 ---
layout:     post
title:      "2024-25 FIS Cross-Country Skiing Season Preview"
subtitle:   "Predicting End of Season Standings"
date:       2024-10-17
author:     "Syver Johansen"
---



# 2024-25 Season Preview

## Introduction

It's near Thanksgiving time and you know what that means.  The 2024-25 FIS Cross Country season is here! In this post, I will give my predictions for the upcoming World Cup season based on historical relationships between Elo scores and World Cup points.  

The scope of this post is just the individual World Cup season.  The World Championships will have a post of their own when late February comes around.

## Schedule

The season is pretty milquetoast in terms of travel.  With no races outside of Western Europe and the entire Tour de Ski taking place in only two locations (both inside Italy), there is nothing too special with the schedule.  The one location that did not fit this criteria, Nove Mesto, was recently moved to Cogne, Italy due to funding issues.  Three of the four sites that have not been on the annual rotation have all hosted recently as well.  Cogne last hosted in 2019, Engadin 2021, and Tallinn in 2023.  Les Rousses is the sole new venue for the season (were supposed to host in 2022, but cancelled due to COVID).  And of course the big change of the season is no Holmenkollen 50km


In terms of what's on the schedule, this is the breakdown:

* Total races: 30 
* Sprint: 11
	* Classic: 5
	* Freestyle: 6 
* Distance: 19
	* Classic: 9
	* Freestyle: 8
	* Skiathlon: 2
	* Individual: 9
	* Mass Start: 9
	* Pursuit: 1
* Classic: 14
* Freestyle: 14

Nothing too crazy here, but a couple things to note.  There are two skiathlons in the World Cup season, in the past there has been 1 at most.  Here is how it compares to last year

* Total races: 33
* Sprint: 13
    * Classic: 6
    * Freestyle: 7
* Distance: 20
    * Classic: 10
    * Freestyle: 9
    * Skiathlon: 1
    * Individual: 9
    * Mass Start: 10
    * Pursuit: 1
* Classic: 16
* Freestyle: 16

And for percentages of overall races between this season, last season, and the last 5 seasons on average.

{{< datatable "season-prediction.race-proportions" >}}

## Off-season news:
* Russia: To not many people's surprise, Russian athletes will not be competing in the 2024-25 season.  If anyone knows a good place to find race results for their circuit, please email me. 
* Therese Johaug: After 2 years off, a wedding, and a baby, Therese Johaug is back.  It's unknown how much racing she will do outside of the World Championship in Trondheim, but given her success at the Norwegian Championships in March last year, she looks to be in top form.
* Petter Northug: First retiring in 2018, Petter Northug returned to skiing last season with some success in the Scandinavian Cup and Norwegian National Championships.  He is unlikely to get any starts on the World Cup, but his presence in the sport always generates more entertainment. 
* Major Retirements: Maurice Manificat (FRA), Scott Patterson (USA), Sjur Røthe (NOR), Anna Svendsen


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


## Breakthrough Potential

By breakthrough I mean people who score more than 50% of the Pct_of_Max_Points in a given season.  Since 2002, this has only been done 41 times for men and _ times for women, and they are listed here.  I used the same criteria that went into earlier analyis, while also putting in age and the number of times a skier had raced in their career (exp).  Then I used a Random Forest method to test the importance of each possible predictor.  From the most important features, I predicted the likelihood that a given skier breaks 50% this season.  Here are the results.

#### Men


{{< datatable "season-prediction.men_breakthrough-predictions-2025-final" >}}


#### Ladies

{{< datatable "season-prediction.ladies_breakthrough-predictions-2025-final" >}}

## Age and Season Predictions

While all these previous season factors definitely play a role in how a skier will perform in the upcoming season, I wanted to measure how age has an effect on the the performance of a skier in the upcoming season.  The data came from season end results, only including skiers who had raced 100 times or more in their career.  Here is what I found.  Unsurprisingly, as most studies would tell you, peak performance comes around age 27 and age 28, with sprint peaking a bit younger.  I also measured in three ways.  One as increase in total elo score, one as a percent of elo score, and one as what percent of the gap was closed between an individual skier and the top skier in the field.  Here are the results of the three.

#### Men


{{< datatable "season-prediction.men_age_table" >}}


#### Ladies

{{< datatable "season-prediction.ladies_age_table" >}}


### Therese Johaug

Therese Johaug is by far the most interesting case of the season.  She hasn't raced in two years, and no one knows how much she will race.  However, let's assume that she races as much as she did in 2022 and that her performance deteriorates like someone who normally would from ages 34 to 36, here is what we get:

{{< datatable "season-prediction.johaug" >}}


### So what would it take for Klaebo to lose?

Now clearly Klaebo is the best skier in the world.  He has the highest Elo scores in everything except for distance freestyle, and even his performance there is improving, and will likely improve as he ages.  The only way he loses the World Cup if his form fails to hold like it has for the past 5 years, if another skier improves a ton, or if he has to miss a lot of races like he did last year due to illness or injury.  To do answer this question, I pulled from my race predictions work and ran simulations his predicted race score for different types of races.  So using the schedule from this year.

{{< datatable "season-prediction.klaebo" >}}




