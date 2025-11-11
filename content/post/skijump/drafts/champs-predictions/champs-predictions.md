In champs-predictions, we are having some issues with the relay one right now.  Here is what I want the file to do for relay that's currently missing.

1) Rewrite the team chrono python to calculate averages for the pelo and elo team values and call those columns Avg_{RaceType}_Elo or Pelo.  If there are members without values, you would substitute their value with the first quartile of values for skiers in that event.  We also will not be using Small or Medium hill elos/pelos, so please get rid of those.  So the average would end up being total elo/TeamSize for that given row.  

2) Train the model.  The model should be trained by using as possible explanatory variables, Avg_Normal_Pelo, Avg_Large_Pelo, Avg_Flying_Pelo, Avg, previous 5 races points 

3) Like you did for mens and ladies, for the predictions you should use the Elo version of the Pelo values to predict.

Now I have a question about prev points weighted for the individual.  When doing the training it should be last 5 for that RaceType not including the current row.  In the prediction it should include the current row.  Is that how it's working?  Repeat back to me what you think I'm saying.

I think I've figured out how to get prev points weighted as an explanatory variable for the team events. There are files for men_chrono.csv and ladies_chrono.csv in polars/relay/excel365. These have team results.  What you do is take the date for the training event in question, filter to Normal or Large depending on what type of team event it is, and then take the previous 5 points totals for that individual, add them to their teams total and then average out by number of legs.  

For example if there was a Team Large event on 2020-01-01, you would filter the chrono file down to RaceType==Large, and take the weighted average of the five events prior to 2020-01-01.

Of course for this one you would also need to add get points to the chronos after they're read in.  For mixed, you can do the same, but would need to read in both men_chrono.csv and ladies_chrono.csv and be able to differentiate between men and ladies to get their prev points.  

Repeat back to me how you understand what I said and let me know what the plan is to implement it.


Ok now that we have completed skijump, let's go back to alpine and do the same fix for pelo and elo as well as prev last 5 weighted that we did for skijump.  Please tell me what you think this means before proceeding.