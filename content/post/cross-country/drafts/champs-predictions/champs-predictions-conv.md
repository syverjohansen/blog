Ok now it's time for cross-country.  This one is a little different.  Let's go through the differences and then come up with a plan in a .md that you can follow closely to implement.

1) The python files are found in ~/ski/elo/python/ski/polars and ~/ski/elo/python/ski/polars/relay.  The R files ar found in ~/blog/daehl-e/content/post/cross-country/drafts

2) The race types for cross country championships actually find in combination of Distance and Technique.  So there is Distance_C, Distance_F, Distance (which is skiathlon), Sprint_C, Sprint_F, Ts_F, Ts_C, and Rel.

3) Cross country has a different scrape setup than the other sports.  There is the config.py file which does need to be configured to have championship like the others where the quota is 4.

4) There is a more layered approach for scrapes in cross-country.  You can check it out, but there are not only startlist_common, startlist-scrape-races.py, and startlist-scrape-weekend.py, but there is also startlist_scrape_races/weekend_relay.py and startlist_scrape_races_weekend_team_sprint.py.  While championships right now do not have mixed, we should probably include them in case they do in the future.  There are scrapes for this as well.

5) In terms of implementing the R script, this one will be a bit tricker as well.  While we should follow closely the implementation of race-picks.R and the other champs-predictions.R (specifically biathlon), there is a lot more to it in terms of team selection and the like in race-picks-mixed-relay.R and race-picks-team-sprint.R, race-picks-relay.R. 

These are the big five that we should work through.  After we agree on what it should be like, please create a md that will go in champs-predictions.md in ~/blog/daehl-e/content/post/drafts/cross-country/champs-predictions.  But first it's important to have a conversation for this.


Ok let's go through your points.

1. For race types, team sprints are just called Ts in the Distance column and have F or C in the technique column.  In order to determine if something is a sprint race or a distance race, sprint races will have Sprint as the distance and distance races will have anything not equal to Sprint or 0 as their Distance.  The elo/pelo columns are Elo/Pelo, Distance, Distance_C, Distance_F, Sprint, Sprint_F, Sprint_C, Classic, and Freestyle

2. File structure is right.  Important to note that the excels and csvs will be in excel365 files in the python ones.  Examples being weekends.csv

3. Yes there is more complex scraping for normal races/weekends, but I want to have the championship scrape be in one file.  

A1. Yes

A2. Separetely, but there should be a Distance one that has poitns for C/F/NA techniques (to include skiathlon and to train it for skiathlon).

B1. 2
B2. 4
B3. 2M and 2L

C1. Great
C2. No.  Just like the other champs-predictions.R, there should just be one workbook and a sheet for each race.  Take a look at how biathlons one does it.
C3.  See C2.

D1. Yes it does.  There are 9 different Elp/Pelos.  There is Elo/Pelo, Distance_Elo/Pelo, Sprint_C, Sprint_F, Sprint, Distance_C, Distance_F, Distance, Freestyle, and Classic
D2.  Yes there are in the chrono files

E1. No you will be able to read race-picks.R or weekly-picks2.R to see how I want team selection done.
E2. Again see race-picks.R and weekly-picks2.R
E3. Yes team sprints will have techniques.  So we use prev points based on their previous points in that specific technique.  Relay is first 2 legs are classic and last 2 legs are freestyle.  SO there will be 2 prev points based on distance classic and distance freestyle.  

F1. One champs-predictions.R that does everything that those multiple R files would do.
F2. See


Ok let's go through the file starting with key differences
**Distance Races (Classic Technique):**
- `Distance_Classic_Elo/Pelo` - Long distance classic races
- `Mid_Classic_Elo/Pelo` - Mid distance classic races  
- `Short_Classic_Elo/Pelo` - Short distance classic races

**Distance Races (Freestyle Technique):**
- `Distance_Freestyle_Elo/Pelo` - Long distance freestyle races
- `Mid_Freestyle_Elo/Pelo` - Mid distance freestyle races
- `Short_Freestyle_Elo/Pelo` - Short distance freestyle races

**Sprint Races:**
- `Sprint_Classic_Elo/Pelo` - Sprint classic technique
- `Sprint_Freestyle_Elo/Pelo` - Sprint freestyle technique
- `Overall_Elo/Pelo` - Overall rating across all race types

No this is not right.
The Elos/Pelos are 
Elo: Elo, Distance_Elo, Distance_F_Elo, Distance_C_Elo, Sprint_Elo, Sprint_C_Elo, Sprint_F_Elo, Classic_Elo, Freestyle_Elo
Pelo: Pelo, Distance_Pelo, Distance_F_Pelo, Distance_C_Pelo, Sprint_Pelo, Sprint_C_Pelo, Sprint_F_Pelo, Classic_Pelo, Freestyle_Pelo

### 4. Championship Quota
- **4-person quota per nation** (same as biathlon)
- Must handle both classic and freestyle specialists
Start probability will be determined the same way as race-picks.R and weekly-picks2.R

Now for phase 1, it looks correct.  Let's start by making the config file first and then discuss for the next part of making the scrape.


Andorra:
Irineu Esteve Alti

Argentina:
Mateo Lorenzo Sauma
Franco Dal Farra

Armenia:
Mikayel Mikayelyan

Australia:
Lars Young Vik
Hugo Hinckfuss
Seve De Campo

Austria:
Mika Vermeulen
Benjamin Moser
Michael Föttinger

Belgium:
Samuel Maes

Bolivia:
Timo Juhani Gronlund

Bosnia&Herzegovina:
Srdjan Lalovic

Brazil:
Guilherme Pereira Santos

Bulgaria:
Mario Matikanov
Daniel Peshkov

Canada:
Antoine Cyr
Olivier Leveille
Xavier McKeever
Graham Ritchie

Chile:
Sigurd Herrera

China:
Qiang Wang
Minglin Li

Colombia:
Samuel Jaramillo

Croatia:
Marko Skender

Czechia:
Michal Novak
Ondrej Cerny
Adam Fellner
Jiri Tuz
Ludek Seller

Denmark:
Magnus Tobiassen

Estonia:
Alvar Johannes Alev
Karl Sebastian Dremljuga
Hendrik Peterson

Finland:
Iivo Niskanen
Lauri Vuorinen
Ristomatti Hakola
Joni Mäki
Perttu Hyvärinen
Arsi Ruuskanen
Ville Ahonen

France:
Hugo Lapalus
Richard Jouve
Jules Chappaz
Lucas Chanavat
Mathis Desloges
Remi Bourdin
Jules Lapierre

Germany:
Friedrich Moch
Jan Stölben
Florian Notz
Elias Keck
Janosch Brugger

Great Britain:
Andrew Musgrave
Joe Davies
Andrew Young

Greece:
Panagiotis Papasis

Haiti:
Theo Mallett

Hungary:
Daniel Szollos
Adam Konya

Iceland:
Einar Arni Gislason

India:
Shubam Parihar

Iran:
Seyed Ahmad Reza Seyd

Ireland:
Thomas Hjalmar Westgård
Dylan Longridge

Italy:
Federico Pellegrino
Davide Graz
Elia Barp
Michael Hellweger
Giovanni Ticco

Japan:
Naoto Baba
Ryo Hirose
Haruki Yamashita

Kazakhstan:
Svyatoslav Matassov
Vitaliy Pukhkalo
Sultan Bazarbekov

Kyrgyzstan:
Artur Saparbekov

Latvia:
Raimo Vigants
Jekabs Skolnieks
Sandijs Suhanovs

Liechtenstein:
Micha Büchel

Lithuania:
Modestas Vaiciulis
Matas Grazys

Mexico:
Allan Corona

Mongolia:
Khuslen Ariunjargal

Montenegro:
Aleksandar Grbovic

Nigeria:
Samuel Uduigowme Ikpefan

North Macedonia:
Darko Damjanovski

Norway:
Johannes Høsflot Klæbo
Harald Østberg Amundsen
Martin Løwstrøm Nyenget
Erik Valnes
Simen Hegstad Krüger
Andreas Fjorden Ree
Even Northug

Poland:
Dominik Bury
Maciej Starega
Piotr Jarecki

Portugal:
Jose Cabeca

Romania:
Gabriel Cojocaru
Ionut Alexandru Costea

Saudi Arabia:
Rakan Alireza

Serbia:
Rejhan Smrkovic

Slovakia:
Michal Adamov
Denis Tilesch

Slovenia:
Miha Simenc
Nejc Stern
Valeriy Gontar

South Africa:
Matthew Smith

South Korea:
Joon-Seo Lee

Spain:
Jaume Pueyo
Marc Colell Pantebre
Bernat Selles Gasch

Sweden:
Edvin Anger
William Poromaa
Calle Halfvarsson
Jens Burman
Emil Danielsson
Gustaf Berglund
Oskar Svensson

Switzerland:
Valerio Grond
Janik Riebli
Jonas Baumann
Jason Rüesch
Beda Klee

Taiwan:
Chieh-Han Lee

Thailand:
Tanathip Bunrit

Turkey:
Abdullah Yilmaz

Ukraine:
Ruslan Denysenko
Andriy Dotsenko

USA:
Gus Schumacher
Ben Ogden
James Clinton Schoonmaker
Kevin Bolger
Zak Ketterson
Jack Young
Zanden McMullen


And for ladies:

Andorra:
Gina del Rio

Argentina:
Agustina Groetzner
Nahiara Diaz Gonzalez

Armenia:
Katya Galstyan

Australia:
Rosie Fordham
Phoebe Cridland
Ellen Søhol Lie
Tuva Bygrave

Austria:
Teresa Stadlober
Magdalena Scherz
Lisa Achleitner
Katharina Brudermann

Brazil:
Eduarda Ribera
Bruna Moura

Bulgaria:
Kalina Nedyalkova

Canada:
Liliane Gagnon
Kathrine Stewart-Jones
Sonjaa Schmidt
Katherine Weaver
Alison Mackie
Olivia Bouffard-Nesbitt

China:
Bayani Jialin
Lingshuang Chen
Dinigeer Yilamujiang

Croatia:
Ema Sobol
Leona Garac

Czechia:
Katerina Janatova
Tereza Beranova
Barbora Antosova
Anna Marie Jaklova
Anna Milerska
Barbora Havlickova

Estonia:
Mariel Merlii Pulles
Kaidy Kaasiku
Keidy Kaasiku
Teiloora Ojaste

Finland:
Jasmi Joensuu
Kerttu Niskanen
Krista Pärmäkoski
Johanna Matintalo
Jasmin Kähärä
Katri Lylynperä
Anne Kyllönen
Amanda Saari

France:
Flora Dolci
Delphine Claudel
Lena Quintin
Melissa Gal
France Pignot
Juliette Ducordeau

Germany:
Katharina Hennig
Coletta Rydzek
Laura Gimmler
Pia Fink
Sofie Krehl
Katherine Sauerbrey
Helen Hoffmann
Anna-Maria Dietze

Greece:
Konstantina Charalampidou
Maria Dimitra Tsiarka

Hungary:
Evelin Vivien Laczko
Larissza Vanda Bere

Iceland:
Kristrun Gudnadottir

Iran:
Atefah Salehi

Italy:
Caterina Ganz
Federica Cassol
Nicole Monsorno
Anna Comarella
Maria Gismondi
Cristina Pittin

Japan:
Masae Tsuchiya
Chika Honda
Chika Kobayashi

Kazakhstan:
Yelizaveta Tolmachyova
Laura Kinybaeyeva
Darya Ryazhko
Angelina Shuryga

Latvia:
Patricijia Eiduka
Adriana Suminska
Linda Kaparkaleja

Liechtenstein:
Nina Riedener

Lithuania:
Egle Savickaite
Ieva Dainyte

Mexico:
Karla Schleske

Mongolia:
Ariunbold Tumur

Norway:
Heidi Weng
Astrid Øyre Slind
Kristine Stavås Skistad
Lotta Udnes Weng
Kristin Austgulen Fosnæs
Julie Myhre
Mathilde Myhrvold
Nora Sanness

Poland:
Izabela Marcisz
Monika Skinder
Aleksandra Kolodziej
Andzelika Szyszka

Romania:
Delia Ioana Reit

Serbia:
Anja Ilic

Slovakia:
Maria Danielova

Slovenia:
Anja Mandeljc
Eva Urevc

South Korea:
Eui Jin Lee
Da-Som Han

Sweden:
Jonna Sundling
Frida Karlsson
Ebba Andersson
Linn Svahn
Maja Dahlqvist
Emma Ribom
Johanna Hagström
Moa Ilar

Switzerland:
Nadine Fähndrich
Anja Weber
Alina Meier
Nadja Kälin
Marina Kälin
Lea Fischer

Taiwan:
Sophia Tsu Velicer

Turkey:
Rabia Akyol

Ukraine:
Yelizaveta Nopriienko
Sofiia Shkatula
Anastasiia Nikon

USA:
Jessie Diggins
Julia Kern
Rosie Brennan
Sophia Laukli
Kate Oldham
Kendall Kramer
Luci Anderson
Sammy Smith


Yes let's move onto phase 2.  Take a look at all the other scrape files in the directory and the relay directory.  Then take a look at the champs scrape in the biathlon polars directory.  Update champs-predictions.md with how you envision the scrape will entail.


Not quite.  For Mixed Relay, Distance should be Rel, and the Sex will be Mixed.  

Take another look at the relay scrape for ski.  I'm pretty sure it doesn't create a relay with top 4 athletes by Elo does it?  It should mimic what all the scrape files do, but put it in a champs scrape.



Ok let's do champs-predictions.R right now for individual athletes.  Right now I'm only running it for men.  Here is supposed to be the workflow.

1) Read in men_chrono_elevation.csv or ladies_chrono_elevation.csv depending on the gender you're predicting for.  Filter to last 10 seasons (so current year - 10).  Also filter out races where City == Tour de Ski

2) Put in a column for Period in the dataframe using the same logic as is used in race-picks.R

3) get_points using the same points schedule as race-picks.R (100,95, etc.)

4) Create the elo_pct and pelo_pct columns.  These are elo score of the skier divided by the top elo score of a person in that race.

5) Create prev_points_weighted.  This filters by race type (Distance, Distance Classic, etc.) where we weigh heavier the more recent races.  Can use the biathlon one as an example, but the points in the current row should not be counted.

6) Using the startlist, calculate the probability that the individual races in a given race.  Same strategy used in biathlon

7) Do the win, podium, etc odds probability training using same method as biathlon.  Remember it should be trained on pelo_pcts

8) Do the predictions for #7 using the startlist probablities.  Remember this will be using elo_pct equivalents of the features selected from #7.  However the elo values will have to be assigned to the pelo_pct columns so that it recognizes the correct features.  Also calculate prev points weighted using last five for the given discipline.  

Let's do these one at a time.  Don't forget to consult weekly-picks2.R and the biathlon champs-predictions R file.

Ok there's going to be 4 parts for each of individual, relay, team sprint, mixed relay

They are
1) Train setup
2) Train execution
3) Test setup
4) Test execution

Let's start with individual setup.  Let's start by reading in the chrono files for men and ladies (men_chrono_elevation.csv and ladies_chrono_elevation.csv).  Let's just start there for now.

Ok now we create a points column and assign points based on world cup points and place. Feel free to consult race-picks.R for this one

Now it's time to make a weighted prev points column.  Here is how it works.  It takes the points column and takes the weighted average of the last five races for that given discipline.  Here is how it should go about it.  If distance == Sprint and technique is C, the most recent race (tail of 1) gets a weight of 5, the one before that for that skier gets a weight of 4, etc.  Else if distance == Sprint and technique is F, else if distance != Sprint and technique is C, else if distance != Sprint and technique is F, else if distance != Sprint (because this would cover pursuits).  


Now let's move onto relay.  Let's do the training portion to start.  Let's start it simple by reading in men_chrono.csv and ladies_chrono.csv from ~/ski/elo/python/ski/polars/relay/excel365


Let's make some changes to the individual one.  The first change to make is I want to split Distance C and Distance F into Distance C Ind, Distance C Ms, Distance F Ind and Distance F Ms.  This is for interval (individual) start and mass start.  The chrono files have a column called MS.  One MS is 1 it is a mass start.  When it is 0 it is not.  So there should be prev points that account for that, and training for both mass start and individual.  

The second change I want to make is that the potential explanatory variables for the different race types should be technique dependent.  For example if we're predicting a Distance F Ind race, we should not be using anything classic as an explanatory variable.  

Ok I commented out the process_individual_results run and want to focus on relay now.

So for relay there is two parts.  The first part is selecting the team.  The second part is picking the winner.  Here is how I propose picking the best team for each nation.  

Before we can get to training, however, we need to set up the chrono correctly.  Let's start out by filtering out Distance of Ts and 0.  Then we will add the points in the same manner as individual.  Let's stop after that.

Ok now we are going to do prev results.  This will be a little bit tricker.  For the most part it will be the same (Sprint C, Sprint F, Distance C, Distance F, Distance), however, there will be a slight difference for the Rel races.  For athletes with leg of 1 or 2, it will just take the most recent value from a Distance C prev points.  For 3 or 4 it will do the same but for Distance F.

Ok now let's filter for last 20 seasons.  After we do that let's calculate elo and pelo_pcts

Ok now I think we have set up all the necessary columns

That's correct.  So my design of it will go something like this.  

1) Pick the best features to podium (top-3 for each leg)
2) Get the importance of each leg (open to suggestions on this)
3) Using the importance of each leg and the features, use the startlist for the Olympics to select the roster that best maximizes that country to podium.
4) Generate predictions for top-1, 3, 5, and 10 and save to xlsx.


Ok this is part of the test execution part of relay.  So before we put in step 3, we have to do the test setup.  The dataframe that has to be read in is ~/ski/elo/python/ski/polars/excel365/startlist_champs_men.csv and startlist_champs_ladies.csv.  Then we have to setup the dataframe.  First use quartile imputation to fill in the NAs in the elo columns. Then the dataframe has to make Pelo_pct columns for predictions.  So it should take the Elo columns and do the row value / max of that column and call it Pelo_pct, Distance_C_Pelo_pct, etc depending on the column.  Then after that we need to get prev_points_weighted_classic and prev_points_weighted_freestyle.  To do this you do the same as you we did earlier by reading the chrono, filtering for Distance_C and Distance_F races and getting the weighted points from the last 5 races for that given distance and technique. 
























