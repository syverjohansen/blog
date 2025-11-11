Ok now let's move onto nordic-combined.  Nordic combined should have the same exact data pipeline as skijump, however, it should be using nordic-combined RaceTypes.  These would be IndividualCompact, Individual, and Mass Start (although you can confirm in weekends.csv).  Team logic should be same except there is no Team Large and Team Small.  Just Team and Team Sprint.  Team Sprint should be trained separately from Team, but both should use all the RaceType for previous points.  For filtering for RaceTypes in the weighted prev points, use Mass Start, IndividualCompact, and Individual...this gets rid of the Offseason rows which should not be taken into account.

Don't forget about the python aspect of it.  config.py, and the scrapes.  


Ok let's configure the config.  For CHAMPS_ATHLETES_MEN we have 
Austria: Johannes Lamparter
Stefan Rettenegger
Franz-Josef Rehrl

China: Jialei Wang
Haibin Fan
Jiawen Zhao

Czechia: Jan Vytrval
Jiri Konvalinka

Estonia: Kristjan Ilves

Finland: Ilkka Herola
Eero Hirvonen
Otto Niittykoski


France:
Laurent Muhlethaler
Matteo Baud
Gael Blondeau

Germany: Vinzenz Geiger
Julian Schmid
Johannes Rydzek

Italy: Aaron Kostner
Raffaele Buzzi
Alessandro Pittin

Japan: Ryota Yamamoto
Akito Watabe
Sora Yachi

Kazakhstan: Ali Askar
Chingiz Rakparov

Latvia: Karlis Svede

Norway: Jens Lurås Oftebro
Jørgen Graabak
Espen Bjørnstad

Poland: Kacper Jarzabek
Pawel Szyndlar
Andrzej Waliczek

Slovenia: Vid Vrhovnik
Matic Garbajs
Gašper Brecl

Switzerland: Pascal Müller

Ukraine: Ruslan Shumanskyi
Oleksandr Shovkoplias
Dmytro Mazurchuk

USA: Ben Loomis
Niklas Malacinski
Henry Johnstone

