In race-picks.R for ski jumping, the work flow goes like this.

First run_integrated_predictions_workflow is called.  This is the main workflow function that really calls all the follow on functions for the excel files.

## Run Integrated Predictions Workflow

The first thing that is called within the function is calculate_race_probabilities that configures the startlists for the races happening tomorrow and calculates the probability that each skier races based on the FIS startlist and their history.  After those probabilities are created, the team chronos are made, if there are team races in the create_team_chronos function.

After the team chronos function, we move onto predict races.  This is where the meat of the predictions lie. 

### Calculate Race Probabilities

In calculate race probabilities, the elo datasets are read in, and the startlists are also read in.  If team events are happening, their startlists are read in.  Within the function there is a function called get_race_probability which calculates the odds of someone racing.  There is also a function called process_individual_probabilities that is called to calls get_race_probability for each skier in the startlist.  There is also a process_team_probabilities function that calculates the same for the teams.  After those are processed, the probabilities are saved to the startlist that they were made from and returned back to the variable in run integrated predictions workflow.  

#### Get Race Probability

In this function, it calculates the odds that a skier races in a given race based on their history.  It calculates this for both team and individual events.  How it does this is by finding when the skier's first ever race was and also the data from five years ago.  Then from the later date of the skiers first race date and five years ago, it uses that as the date to find the percentage of events that skier has jumped in during that time.  

#### Process Individual Probability

In this function, if there is already a startlist made from successful scraping of the FIS website, then it assigns 1 to Race1_Prob, since that skier is in the startlist.  If there has been no succesfully scraped startlist, then we have to generate the probabilities using Get Race Probability.  That startlist is returned in the end.

#### Process Team Probability

This one just sets everything to 1 for all teams in the startlist for all team results.

### Create Team Chronos

This function reads in the elevation enhanced elo files for relay and creates a Points column based on World Cup points for each of the races before saving the csv to a new file.  

### Predict Races

In predict races, there are separate predictions for individual and teams.

#### Individuals



#### Teams

