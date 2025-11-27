
How weekly-picks.md would work in an ideal world



1) Gathering the startlist
Individuals: Creates a mock startlist for the weekend-scrape python file that scrapes the FIS startlist using the FIS link for the first race of the weekend and then inputting the other skiers that have raced this season into the startlist.  Also puts in the latest elo scores of each of the skiers in the appropriate columns.  Outputs to a csv file to be referenced by the R file.

Teams: Same as individuals, but separates the skiers into each leg (1-4) and creates columns for average elos for each of the teams. 

Mixed Team: Same as mixed team, but make sure to reference the correct chrono files.

Quality Assurance tests
1) Properly reads startlist (multiple styles)
2) Properly gets all skiers from current season
3) Only runs if there is a weekend race that day UTC.
4) Works even if the startlist isn't ready
5) Works with a config file
6) Have enough Race_{i} prob columns for as many races there are that weekend 
7) Puts in correct elo values into all the columns (avg elo for teams)

2) Setting up training data
Individuals: Reads in the chrono elevation for the specific gender.  Modifies it by adding a Points column using the World Cup points structure where place one is 100, etc.  Imputs NA values in the Pelo columns to be replaced by the first quartile for that column.  Creats a Prev_Points_weighted column that calculates the weighted average last 5 races of that specific race points where the most recent gets a weight of 5 and the least recent gets 1.  Gets a zero if a skier does not have any previous races.

Teams: Reads in the chrono elevation (relay) for the specific gender.  Does the same modifications as the individual one, but in the rows for team events, references the skiers last 5 individual events of that hill size (not team).  

Mixed team: Same thing as Team, but uses all of 



## Executive Summary

The purpose of weekly picks is to predict the results for the upcoming weekend of events for Ski Jumping.  The steps to accomplish this are:

1) Gather and generate the startlists using posted information on FIS and historical trends.

2) Train models using historical Elo and WC points data to predict World Cup points for the events and the probability of skiers and teams getting top 1, 3, 5, 10, and 30 in each event.

3) Using those models to predict the outcome of the upcoming weekend.

### Scraping the data

Scraping the data is done in different ways depending on if the race is an individual race, team event, or mixed team event.  They all have the overarching startlist_common.py file though for the scraping.  The mother file for the event that gets called is startlist-scrape-weekend.py

#### Startlist Scrape Weekend

This file operates by importing the utility functions from startlist_common.py and immediately calls process_weekend_races().

##### Process Weekend Races

Process weekend races reads in the weekends.csv file that lists the race information and FIS startlist/results URL for the races this weekend.  It first does this by calling find_next_race_date() to get the next race date on the calendar.  It then calls filter_races_by_date() to get just those events to work with in the future of the file.  If there are events for the weekend starting on today, it further filters down into the three events types we have:

1) Individual
2) Regular team (Men/Ladies)
3) Mixed team

The individual events get run through process_individual_races, regular team gets processed through startlist_scrape_weekend_team.py, and startlist_scrape_weekend_mixed_team.py for the mixed one.   

###### Find Next Race Date

This function uses the current date in UTC time and finds the next race on the calendar in the Date column of the weekends.csv file.  

###### Filter Races by Date

Returns the races with the next race date on the calendar.

###### Process Individual Races

Further breaks it down into men's races and ladies' races to be processed in process_gender_races

####### Process Gender Races

Iterates through each of the races for the given gender in the weekend and creates prob columns for the probability that a skier races in a given race.  Includes all racers from the given season in the startlist so that they can be plausibly added.  Calls create_season_startlist and create_weekend_startlist.  

######## Create Season Startlist

Gets a df of all the skiers who have participated in this season with their given elo scores.  

######## Create Weekend Startlist

Scrapes the startlist for the upcoming weekend and gets their latest elo scores.  

###### Startlist Scrape Weekend Team

###### Startlist Scrape Weekend Mixed Team

