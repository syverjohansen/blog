---
layout: post
title: "2024-25 Preview Part 3: Age Analysis"
date: 2024-10-17
author: "Syver Johansen"
---

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