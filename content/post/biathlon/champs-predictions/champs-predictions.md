Ok let's move onto champs-predictions for biathlon.  Here are some differences between biathlon and the others

1) There are two mixed relay races.  There is Mixed Relay and Single Mixed Relay.  Mixed Relay is four athletes, Single Mixed Relay is two athletes.

Beyond that it is pretty much the same.  I want a config.py file like nordic-combined, a scrape like nordic-combined, relay-chrono.py for biathlon should be like nordic-combined where you take the average pelos and elos from the total divided by number of legs.

In the R file it will have the same predictions.  It is important to remember how prev_points_weighted is calculated for both individual races and team races, and how training will use pelo, but prediction will use elo, however we have to make the pelo ones equal to elo value in the test.  Just follow nordic-combined as a model for all of this.  Also important to note how the races get saved in the excel file.  Should be a sheet for each race.

1. Individual race types are Individiual, Sprint, Pursuit, and Mass Start.
2. Yes men's teams and ladies' teams with 4 athletes each
3. There are no team sprints in biathlon.  Just Relay, Single Mixed Relay, and Mixed Relay.  Team events have Relay in RaceType instead of Team
4. Quotas are 6 athletes.  For relays it should take the athletes with the highest elo
5. Yes exactly
6. Great question.  Exactly correct you filter by RaceType != "Offseason"
7. Sprint, Pursuit, Individual, Mass Start
8. Exactly
9. Always one man one woman for sigle mixed and two men/two ladies for mixed.  Will take highest elo for single mixed of each gender and highest two elos for each gender in mixed.
10. Different startlist files.  There is a single_mixed_relay_chrono and a mixed_relay_chrono
11. Separate sheets yes.  But it should follow nordic combined where the different relays get their own workbooks.     


Ok let's make the CHAMPS_ATHLETES_MEN and CHAMPS_ATHLETES_LADIES in the config now.

For men we have

Austria: Simon Eder
David Komatz
Felix Leitner
Fabian Müllauer

Belgium: Florent Claude
Thierry Langer
Marek Mackels
Cesar Beauvais

Bulgaria:
Blagoy Todev
Vladimir Iliev
Konstantin Vasilev
Vasil Zashev

Canada:
Adam Runnalls
Gavin Johnston
Daniel Gilfillan
Jasper Fleming

China:
Cang Gu

Croatia:
Matija Legovic
Kresimir Crnkovic

Czechia:
Vitezslav Hornig
Michal Krcmar
Tomas Mikyska
Jonas Marecek
Adam Vaclavik

Denmark:
Rasmus Schiellerup

Estonia:
Rene Zahkna
Kristo Siimer
Jakob Kulbin
Karl Rasmus Tiislar

Finland:
Tero Seppälä
Olli Hiidensalo
Otto Invenius
Arttu Heikkinen
Jimi Klemettinen

France:
Eric Perrot
Quentin Fillon Maillet
Emilien Jacquelin
Fabien Claude
Emilien Claude
Antonin Guigonnat

Germany:
Philipp Nawrath
Justus Strelow
Philipp Horn
Roman Rees
Johannes Kühn

Itay:
Tommaso Giacomel
Lukas Hofer
Didier Bionaz
Patrick Braunhofer
Daniele Cappellari

Japan:
Mikito Tachizaki

Kazakhstan:
Nikita Akimov
Vladislav Kireyev

Latvia:
Andrejs Rastorgujevs
Rihards Lozbers
Renars Birkentals
Matiss Meirans

Lithuania:
Vytautas Strolia
Maksim Fomin
Nikita Cigak
Tomas Kaukenas

Moldova:
Pavel Magazeev
Maksim Makarov

Norway:
Sturla Holm Lægreid
Vetle Sjåstad Christiansen
Endre Strømsheim
Johannes Dale-Skjevdal
Vebjørn Sørum
Martin Uldal

Poland:
Jan Gunka
Konrad Badacz
Fabian Suchodolski
Andrzej Nedza-Kubiniec

Romania:
Dmitrii Shamaev
George Buta
George Coltea
Cornel Puchianu

Slovakia:
Simon Adamov
Jakub Borgula

Slovenia:
Jakov Fak
Miha Dovzan
Anton Vidmar
Lovro Planko
Matic Repnik

South Korea:
Timofei Lapshin

Sweden:
Sebastian Samuelsson
Martin Ponsiluoma
Jesper Nelin
Victor Brandt
Emil Nykvist
Malte Stefansson


Switzerland:
Niklas Hartweg
Joscha Burkhalter
Sebastian Stalder
Jeremy Finello
James Pacal

Ukraine:
Dmytro Pidruchnyi
Vitalii Mandzyn
Anton Dudchenko
Artem Tyshchenko
Denys Nasyko

USA:
Campbell Wright
Jake Brown
Sean Doherty
Maxime Germain

Now onto ladies

Australia: Darcie Morton

Austria:
Lisa Theresa Hauser
Anna Gandler
Tamara Steiner
Lea Rothschopf
Anna Andexer

Belgium:
Lotte Lie
Maya Cloetens
Marisa Emonts
Rieke de Maeyer

Brazil:
Gaia Brunello

Bulgaria:
Milena Todorova
Lora Hristova
Valentina Dimitrova
Stefani Yolova

Canada:
Emma Lunder
Nadia Moser
Pascale Paradis
Benita Peiffer


China:
Fanqi Meng
Jialin Tang

Croatia:
Anika Kozica

Czechia:
Marketa Davidova
Tereza Vobornikova
Jessica Jislova
Katerina Pavlu
Lucie Charvatova

Estonia:
Tuuli Tomingas
Susan Külm
Regina Ermits
Violetta Konopljova

Finland:
Suvi Minkkinen
Venla Lehtonen
Sonja Leinamo
Inka Hämäläinen
Noora Kaisa Keränen

France:
Lou Jeanmonnot
Julia Simon
Oceane Michelon
Justine Braisaz-Bouchet
Jeanne Richard
Gilonne Guigonnat

Germany:
Franziska Preuss
Selina Grotian
Vanessa Voigt
Julia Tannheimer
Sophia Schneider
Julia Kink

Italy:
Dorothea Wierer
Michela Carrara
Samuela Comola
Hannah Auchentaller
Beatrice Trabucchi

Kazakhstan:
Milana Geneva

Latvia:
Baiba Bendika
Estere Volfa
Elza Bleidele
Sanita Bulina

Lithuania:
Sara Urumova
Lidiia Zhurauskaite
Judita Traubaite
Natalija Kocergina

Moldova:
Alina Stremous
Aliona Makarova

Norway:
Maren Kirkeeide
Ingrid Landmark Tandrevold
Ida Lien
Karoline Offigstad Knotten
Marthe Kråkstad Johansen

Poland:
Natalia Sidorowicz
Anna Nedza-Kubiniec
Joanna Jakiela
Anna Maka

Romania:
Adelina Rimbeu
Anastasia Tolmacheva
Elena Chirkova
Andreea Mezdrea

Slovakia:
Paulina Batovska Fialkova
Anastasiya Kuzmina
Ema Kapustova
Maria Remenova

Slovenia:
Anamarija Lampic
Polona Klemencic
Klara Vindisar
Lena Repinc

South Korea:
Ekaterina Avvakumova

Sweden:
Elvira Öberg
Hanna Öberg
Anna Magnusson
Ella Halvarsson
Sara Andersson
Anna-Karin Heijdenberg

Switzerland:
Aita Gasparin
Amy Baserga
Lena Häcki-Gross
Elisa Gasparin
Lea Meier

Ukraine:
Yuliia Dzhima
Khrystyna Dmytrenko
Olena Horodna
Liliia Steblyna
Anastasiya Merkushyna

USA:
Deedra Irwin
Lucinda Anderson
Tara Geraghty-Moats
Chloe Levins

Ok I have some questions
1) Does it get trained on Pelo_Pct and then predictions used the Elo_Pct values (but assigned to Pelo_Pct variables since that's how feature section was done)
2) For Prev_Weighted_Points how does it do weights? Do the training use the current row points?
3) For getting Prev_Weighted_Points for relays, does it filter out offseason?  For individual does it filter for that current racetype?

So one thing with the 4 person quota.  It should be set in a similar fashion to how biathlon does for start probabilities for individual races.  Take a look at how biathlon does it.  Let's do that before moving on


















