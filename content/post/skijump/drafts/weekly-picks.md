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

