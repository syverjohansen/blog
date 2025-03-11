---
layout:     post
title:      "Scraping Ski Data for Statistical Analysis"
subtitle:   "How to get your data"
date:       2024-06-13
author:     "Syver Johansen"
---
<!--image:      "/img/scrape-blog/libraries.png"-->


# Goal: Getting the Data Needed to Perform Statistical Analysis

The first step to perform any meaningful statistical analysis is to get the data needed.  Because I haven’t recorded ski results since the beginning of time, I need to use an external source.  The seemingly obvious way to do this is to use the FIS website and search for World Cup races.  While they undoubtedly have the highest volume of data and are the official arbiter of the results, their records management has something to be desired.  Because our analysis will want to make sure that each individual skier is unique, names cannot be the ultimate form of identification since multiple people can have the same name.  Instead, we need a unique ID for each skier.  While FIS does have a FIS code, I discovered examples of people (especially women who got married) who have a FIS code for one name/spelling, and another FIS code for another name/spelling.  On top of this, biathlon data is not stored on FIS, which would mean I would need a script to scrape from an IBU site.  

Fortunately, I stumbled across [firstskisport.com](https://firstskisport.com) (formerly skisport365.com).  I am not sure who maintains the site as it just says “By the fans – for the fans”.  A simple whois query on the site didn’t bring up anything meaningful other than their servers were based out of Norway.  However, email I had previously sent to the site telling them of a dead link got a reply from Tomas Skogheim Walsvik, Founder and Head of Development.  Assuming he is the main guy who runs the site, thank you Tomas for all of your hard work.

[Firstskisport.com](https://firstskisport.com) is a super well organized site that has results for alpine, biathlon, cross-country, Nordic Combined, ski jumping and speed skating dating back to the earliest records.  Also, the names in the records have been normalized for each sport, so there is not the same issues as the FIS site in terms of multiple names for one person.  Due to this, there is also only one id per person, a necessity for any follow-on analysis.  

Now that we have the data source we want it is time to web scrape.


## What is Web Scraping?
Web scraping is an automated method to get information off websites.  Several programming languages allow web scraping, however, Python is the most common.  Within Python there are several methods to web scrape.  To name a few, there are libraries such as `BeautifulSoup`, `lxml`, or `polars` that enable web scraping.  There are frameworks like scrapy that are generally used for web crawling.  There are tools like `Selenium` that are better for website interaction.  APIs, browser extensions, and online services are other ways to perform web scraping.  Maybe at some point I will give a tutorial or performance tests with `lxml` or `scrapy`, but for the purpose of this tutorial, we will be using `BeautifulSoup` as it is the most common and easy to learn.

## What is `BeautifulSoup`?
`BeautifulSoup` is an HTML/XML parser.  HTML/XML are mark-up languages that are used to structure and organize websites.  So, by using `BeautifulSoup`, one can comb through the contents of a web site and pick out the information that they want.

## Libraries

Before we do any meaningful coding, let’s import some libraries.

<!-- ![screenshot](/img/scrape-blog/libraries.png) -->

```python
#Importing libraries
import ssl
ssl._create_default_https_context = ssl._create_unverified_context
from bs4 import BeautifulSoup
from urllib.request import urlopen
from datetime import datetime
import re
import pandas as pd
import time
```

Going through the imports we have

- **import ssl**
  - Provides access to the SSL/TLS protocols, which allows us to verify our certificates when accessing an HTTPS site. We don’t need SSL certificate verification for what we’re doing, so we will skip it. Use caution when doing this because it makes the connection less secure and exposes you to man-in-the-middle attacks.
  
- **ssl._create_default_https_context = ssl._create_unverified_context**
  - Disables the verification of HTTPS certificates.
  
- **from bs4 import BeautifulSoup**
  - Imports the `BeautifulSoup` class from `bs4` to be used for parsing HTML and XML documents.
  
- **from urllib.request import urlopen**
  - This will be used to open and read URLs.
  
- **from datetime import datetime**
  - For working with dates and times within the context of data types.
  
- **import re**
  - For when we use regular expressions.
  
- **import pandas as pd**
  - Importing `pandas`, which is a must for most Python projects using dataframes. We will try other ones in different blogs, but pandas will be the default.
  
- **import time**
  - For timing how long our program takes.

## Getting `BeautifulSoup` to work

We start out by defining the website we want to use and opening the url.

```python
#Getting the year page
worldcup_page0 = "https://firstskisport.com/cross-country/calendar.php?y=1920"

#Opening the webpage
worldcup_page0 = urlopen(worldcup_page0, timeout=10)
```

Unfortunately, this page only get us the list of races that happened in a given season.  We want to go one layer deeper and get the results for each race.  For this we need to be able to get the urls for each race in this season, and access the link.  Here is where we get to actually use BeautifulSoup for the first time. 

```python
#BeautifulSoup the HTML for the page
worldcup_soup0 = BeautifulSoup(worldcup_page0, 'html.parser')
print(worldcup_soup0)
```

In the snippet that was printed, you can see we’re dealing with A LOT of structure here.  So we need to break it down further and find the what’s important to us, which is the links to the individual races.  Scrolling down we see this.  Looking back at the urls on the firstskisport page, this matches the end of the url for the 50km at Holmenkollen. 

```
<td><img align="center" class="flag" src="../img/flag/NOR.png" title="NOR"/> <a href="results.php?id=1808" title="Results">Holmenkollen</a></td>
```

To extract the URL, we use the `find_all()` method on the `worldcup_soup0` object. This method finds all occurrences of the `<a>` tag where the `title` attribute is set to 'results'. Since each race link occurs twice in the HTML, we only want every other occurrence. We achieve this by iterating over the results in a for loop.

```
['https://firstskisport.com/cross-country/results.php?id=1808']
```

We can confirm these links match the races in the season.  Just one race, which makes it simple.  You can try a different year if you don’t believe this one works.

## `get_results()` Function

Now we need to actually get the results.  Since `worldcup_page1` is a list, we will need to use a for loop to go through the results and extract what’s in there.  But first let’s see what we want to extract from the race.

If we are to build a dataframe, we want to have as much information as we can about the race, and the racers.  From the results page we can get the following:  

- **Date:** 19.Feb 2020

- **City:** Holmenkollen

- **Country:** Norway

- **Event:** Pre World Cup

- **Distance:** 50km

- **Mass Stert:** No

- **Technique:** Not listed (which means classic for anything pre 1985)

- **Place:** 1, 2, 3

- **Name:** Thorleif Haug, Johan Grøttumsbråten, Torkel Persson

- **Nationality:** Norway, Norway, Sweden

- **Time:** 4:35:30.0, +05:12.0, +08:27.0

- **ID:** 10000, 879, 881 

Maybe in further analysis, I will look at finishing time as a predictor for a result, but for now we will omit it.  Here is how we can get the above information.

```python
#Function to get results from the worldcup_page
def get_results(worldcup_page):
        #BeautifulSouping the webpage
    worldcup_page = urlopen(worldcup_page)
    worldcup_soup = BeautifulSoup(worldcup_page, 'html.parser')

    #Getting the race distance/technique and the city
    race_city = worldcup_soup.body.find('h1').text

    #Getting the date of the race and the country it takes place
    date_country = worldcup_soup.body.find('h2').text.split(", ")
    event = date_country[1]
    #Start by getting the event and date extracted.  The try and except are because of a weird exception in 1980.
    date = str(date_country[2]).split(" ")[1:3]
    try:
        year = date[1]
        date = " ".join(date)
        #Now we need to get the date into yyyymmdd format
        date_obj = datetime.strptime(date, "%d.%b %Y")
        date = date_obj.strftime("%Y%m%d")
    
    #There is a women's race in 1980 that does not have a date 
    except:
        year = "1980"
        date = "19800101"

    print(date)
    #Now we get the country
    country = str(date_country[3])

    #Now the race and the city
    race_city = str(race_city).split("- ")
    race = race_city[0]
    city = race_city[1]

    #Now lets isolate the distance
    #Getting the mass start is optional
    distance = race.split(" ")[0]
    if(("Mass" in race) and ("Start" in distance)):
        ms=1
    else:
        ms=0

    #Start with relays.  Since for now we are only concerned with individual, we will not return
    if(distance.startswith("4x") or distance.startswith("3x")):
        distance = "Rel"

    #Now team sprints.  Also will not return
    if(distance=="Team"):
        distance = "Ts"

    #Now duathlon.  This won't be a thing until much later
    if(distance=="Duathlon"):
        technique = "P"
        #We want to make a list of all the important parts that will be
        #inputted into the Excel.  This table value will be returned later
        table = [date, city, country, event, distance, 1, technique]

    #Now sprints
    elif(distance=="Sprint"):
        technique = race.split(" ")
        technique = technique[1][0]
        table = [date, city, country, event, distance, ms, technique]
    
    elif(distance=="Stage"):
        technique = "N/A"
        table = [date, city, country, event, distance, ms, technique]
    #And finally distance.  Here we will ask if the year is greater than 1985 and make sure it is not a relay
    #1985 is when freestyle came into play
    elif(int(year) > 1985 and distance!="Rel"):
        technique = race.split() #Empty splits all blank space.
        technique = technique[2][0]
        table = [date, city, country, event, distance, ms, technique]
    else:
        table = [date, city, country, event, distance, ms, "N/A"]

    #Now we get the skiers by looking at the the table
    body = worldcup_soup.body.find_all('table', {'class':'tablesorter sortTabell'})[0].find_all('td')
    #We want to get: places, skier, nation, ski_ids
    #Need to go through the body and extract this data
    #Skier's info comes up every seventh iteration
    places = []
    skier = []
    nation = []
    ski_ids = []
    for b in range(len(body)):
        if(b%7==0):
            #Making sure the skier didn't drop off, get dsq, etc.
            #This may be something to measure, but generally these events
            #would be bad to include in elo since they are so major and seemingly random
            #A randomness check could be done at a later time.
            if(str(body[b].text)!="DNF" and str(body[b].text)!="DSQ" and str(body[b].text)!="DNS"
                and str(body[b].text)!="DNQ" and str(body[b].text)!="OOT"):
                places.append(body[b].text) 
                ski_id = str(body[b+2])
                id_match = re.search(r'id=(\d+)', ski_id)
                ski_id = id_match.group(1)
                ski_ids.append(ski_id)
                skier_name = str(body[b+2])
                skier_match = re.search(r'title="([^"]+)"', skier_name)
                skier.append(skier_match.group(1))
                nation.append(body[b+4].text.strip())
    skiers = [places, skier, nation, ski_ids]
    #Using Pandas for creating a dataframe
    ski_df = pd.DataFrame(skiers).T
    ski_df.columns=['Place', 'Name', 'Nation', 'ID']                      

    #And now the table.  We are using a dictionary for this
    table_df = {'Date': [table[0]]*len(ski_df),
               'City': [table[1]]*len(ski_df),
               'Country': [table[2]]*len(ski_df),
               'Event': [table[3]]*len(ski_df),
               'Distance': [table[4]]*len(ski_df),
               'MS': [table[5]]*len(ski_df),
               'Technique': [table[6]]*len(ski_df)}

    #Concatinating table_df with ski_df
    ski_df = pd.concat([pd.DataFrame(table_df), ski_df], axis=1)

    #Correcting the column order
    column_order = ['Date', 'City', 'Country','Event', 'Distance', 'MS', 'Technique',
                   'Place', 'Name', 'Nation', 'ID']
    ski_df = ski_df[column_order]
    return ski_df     
    






```

Lots to unpack here.  

1. **Format**: First this is a `for` loop that goes through the list of result pages for a given year.  We start out by opening the first race and `BeautifulSoup`-ing it.  

2. **Race/City and Event/Date/Country**: Then we have to parse the html.  We can see in the page source that the race (50 km) and the city (Holmenkollen) are tied together in `h1`, so we extract that text.  Next we see that same is true for the event (Pre World Cup), date (19.Feb 1920), and the country (Norway) in `h2`, so we do the same.  However, the date is not in a format we want.  We want it in yyyymmdd, so we use the `datetime` library and `strptime`/`strftime` functions to do that.  Separating out the race and the city is pretty simple and just requires a `split` in the string.

3. **Mass Start**: The mass start part is optional, but you will need to adjust the lists later if you decide to leave it out.

4. **Distance/Technique**: To get distance, we just use simple `if` statements. To get technique, if the year is after 1985 we can `split` the race further to gather whether it’s freestyle or classic.  Otherwise it is just marked as N/A.

5. **Results**: Now we want to get the actual results of the race, so we use `BeautifulSoup` again to find the table with all of the results.  The fields we want as columns for our df are places, skier, nation, and ski_ids.  
We start out by going through the body of the HTML.  We can see that in the table, every 7th instance is a new skier.  So, now we can for loop through the body and stop at every 7th instance.  I’ve decided to leave out DNF, DNS, DNQ, DSQ, and OOTs since they would significantly affect one’s elo rating for what is likely a random occurrence.  Maybe at a later time, I will measure how random they are and if they can be adjusted for correctly.

6. **Places, IDs, Name, and Nation** For each instance, we can grab the places, ids, name, and nation.  Because the id and the name are within a jumble of text, I used a regex search to grab the string.  Then I built the skiers list, which is just a list of lists.

7. **Building the dataframe** Finally, to build the dataframe, I took the skiers list, made it a pandas dataframe, and transposed it so that each list in skiers was its own column.  
To add the race information to the ski_df, I turn each value into a `dict` where the key will be its own column and the values to each key will be the same value repeated as many times as the number of rows in ski_df.
Then I concatenate the ski_df with table_df along columns (`axis=1`), and reorder the columns to fit how I want it.  

8. **`return`**: We also return the ski_df at the bottom 

## Calling the Function

```python
start_time = time.time()
#0 is men, 1 is women
for a in range(0,2):
    if(a==0):
        sheet = "Men"
    else:
        sheet = "Ladies"
#Start with an empty dataframe with the columns we want
    ski_df = pd.DataFrame(columns = ['Date', 'City', 'Country','Event', 'Distance', 'MS', 'Technique',
                   'Place', 'Name', 'Nation', 'ID'])
    for b in range(1920, 2025):
        print(b)
        #Getting the year page
        if(a==0):
            worldcup_page0 = "https://firstskisport.com/cross-country/calendar.php?y=" +str(b)
        else:
            worldcup_page0 = "https://firstskisport.com/cross-country/calendar.php?y=" +str(b) + "&g=w"
        #Opening the webpage
        worldcup_page0 = urlopen(worldcup_page0, timeout=10)   
        #BeautifulSoup the HTML for the page
        worldcup_soup0 = BeautifulSoup(worldcup_page0, 'html.parser')

        #Getting the links to each race in a season
        worldcup_page1 = []
        title_results_count = 0
        for c in worldcup_soup0.find_all('a', {'title':'Results'}, href = True):
            if(title_results_count%2==0):
                worldcup_page1.append('https://firstskisport.com/cross-country/'+str(c['href']))
            title_results_count+=1

        #Going through each race in a season and getting the result.
        for c in range(len(worldcup_page1)):
            result = get_results(worldcup_page1[c])
            result['Race'] = c+1
            result['Season'] = b
            ski_df = pd.concat([ski_df, result], axis=0)
           
        
        
        
            
            
    #Saving it to a specific excel worksheet
    if(a==0):
        ski_df['Sex'] = "M"
        men_df = ski_df
        with pd.ExcelWriter("/Users/syverjohansen/ski/elo/python/ski/excel365/all_demo.xlsx", 
                            engine='xlsxwriter') as writer:
            ski_df.to_excel(writer, sheet_name=sheet, index=False)
    else:
        ski_df['Sex'] = "L"
        ladies_df = ski_df
        with pd.ExcelWriter("/Users/syverjohansen/ski/elo/python/ski/excel365/all_demo.xlsx", 
                            engine='openpyxl', mode='a') as writer:
            ski_df.to_excel(writer, sheet_name=sheet, index=False)
#Combine men and ladies dfs and write to a pickle


print(time.time() - start_time)
```

1. We start the timer with `start_time = time.time()`

2. Then we want to use both sexes, so we create a `for` loop with `range(0,2)`.

3. We create an empty dataframe, `ski_df` that has all the columns that we want.

4. Now we create an inner `for` loop that goes through all the years of results.

5. `if(a==0)` is to differentiate between the sexes.  Men and ladies have different endings to their results in firstskisport.

6. Grab the links to the seasons as explained earlier in the blog

7. Grab the results by calling the function

8. Create a `race` column and a `season` column.  `race` will be the race number for a given season and `season` will give the season.

9. Use the returned result and concatenate to the df we already.

10. Write to an excel file.


The output should look something like this.  Note the run time.

```
20240120
20240121
20240127
20240128
20240209
20240210
20240211
20240213
20240217
20240218
20240301
20240302
20240303
20240309
20240312
20240315
20240316
20240317
2473.1086418628693
```


All done!

    











