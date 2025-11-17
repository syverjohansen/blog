import pandas as pd
import polars as pl
print(pl.__version__)
import json
import os


L_chrono = pl.read_ipc(os.path.expanduser('~/ski/elo/python/ski/polars/excel365/ladies_chrono.feather'))


M_chrono = pl.read_ipc(os.path.expanduser('~/ski/elo/python/ski/polars/excel365/men_chrono.feather'))

#L_chrono = L_chrono.filter(pl.col("City")!="Summer")
#M_chrono = M_chrono.filter(pl.col("City")!="Summer")

codes = {
"Algeria":"alg", "Andorra":"and", "Argentina":"arg", "Armenia":"arm", "Australia":"aus", "Austria":"aut", "Belarus":"blr", "Belgium":"bel", "Bermuda":"ber", "Bolivia":"bol", "Bosnia&Herzegovina":"bih", "Brazil":"bra", "Bulgaria":"bul",
"Cameroon":"cmr", "Canada":"can", "Chile":"chi", "China":"chn", "Colombia":"col", "Costa Rica":"crc", "Croatia":"cro", "Czechia":"cze", "Denmark":"den",
"Ecuador":"ecu", "Estonia":"est", "Ethiopia":"eth", "Fiji":"fij", "Finland":"fin", "France":"fra",
"Germany":"ger", "Great Britain":"gbr", "Greece":"gre", "Guatemala":"gua", "Haiti":"hai", "Honduras":"hon", "Hungary":"hun", 
"Iceland":"isl", "India":"ind", "Iran":"iri", "Ireland":"irl", "Israel":"isr", "Italy":"ita", "Japan":"jpn", 
"Kazakhstan":"kaz", "Kenya":"ken", "Kyrgyzstan":"kgz", "Latvia":"lat", "Lebanon":"lbn", "Liechtenstein":"lie", "Lithuania":"ltu", "Luxembourg":"lux",
"Mexico":"mex", "Moldova":"mda", "Mongolia":"mgl", "Montenegro":"mne", "Morocco":"mar", "Nepal":"nep", "Netherlands":"ned", "New Zealand":"nzl", "Nigeria":"ngr", "North Korea":"prk", "North Macedonia":"mkd", "Norway":"nor",
"Pakistan":"pak", "Peru":"per", "Poland":"pol", "Portugal":"por", "Romania":"rou", "Russia":"rus",
"San Marino":"smr", "Serbia":"srb", "Slovakia":"svk", "Slovenia":"slo", "South Africa":"rsa", "South Korea":"kor", "Soviet":"urs", "Spain":"esp", "Sweden":"swe", "Switzerland":"sui", "Taiwan":"tpe", "Thailand":"tha", "Togo":"tog", "Tonga":"tga", "Trinidad & Tobago":"tto", "Turkey":"tur",
"USA":"usa", "Ukraine":"ukr", "Venezuela":"ven", None:"unknown"
    }

def get_country_code(nation):
    codes = {
"Algeria":"alg", "Andorra":"and", "Argentina":"arg", "Armenia":"arm", "Australia":"aus", "Austria":"aut", "Belarus":"blr", "Belgium":"bel", "Bermuda":"ber", "Bolivia":"bol", "Bosnia&Herzegovina":"bih", "Brazil":"bra", "Bulgaria":"bul",
"Cameroon":"cmr", "Canada":"can", "Chile":"chi", "China":"chn", "Colombia":"col", "Costa Rica":"crc", "Croatia":"cro", "Czechia":"cze", "Denmark":"den",
"Ecuador":"ecu", "Estonia":"est", "Ethiopia":"eth", "Fiji":"fij", "Finland":"fin", "France":"fra",
"Germany":"ger", "Great Britain":"gbr", "Greece":"gre", "Guatemala":"gua", "Haiti":"hai", "Honduras":"hon", "Hungary":"hun", 
"Iceland":"isl", "India":"ind", "Iran":"iri", "Ireland":"irl", "Israel":"isr", "Italy":"ita", "Japan":"jpn", 
"Kazakhstan":"kaz", "Kenya":"ken", "Kyrgyzstan":"kgz", "Latvia":"lat", "Lebanon":"lbn", "Liechtenstein":"lie", "Lithuania":"ltu", "Luxembourg":"lux",
"Mexico":"mex", "Moldova":"mda", "Mongolia":"mgl", "Montenegro":"mne", "Morocco":"mar", "Nepal":"nep", "Netherlands":"ned", "New Zealand":"nzl", "Nigeria":"ngr", "North Korea":"prk", "North Macedonia":"mkd", "Norway":"nor",
"Pakistan":"pak", "Peru":"per", "Poland":"pol", "Portugal":"por", "Romania":"rou", "Russia":"rus",
"San Marino":"smr", "Serbia":"srb", "Slovakia":"svk", "Slovenia":"slo", "South Africa":"rsa", "South Korea":"kor", "Soviet":"urs", "Spain":"esp", "Sweden":"swe", "Switzerland":"sui", "Taiwan":"tpe", "Thailand":"tha", "Togo":"tog", "Tonga":"tga", "Trinidad & Tobago":"tto", "Turkey":"tur",
"USA":"usa", "Ukraine":"ukr", "Venezuela":"ven"
    }
    return codes.get(nation, "Unknown")  # Return 'Unknown' if the nation isn't found

for a in range(2):
	if a == 0:
		sex = 'M'
		df = M_chrono
	else:
		sex = 'L'
		df = L_chrono

	df = (
    df.group_by("ID")
    .agg([
        pl.col("Skier").last().alias("Skier"),
        pl.col("Sex").last().alias("Sex"),     # Get the last Name for each ID
        pl.col("Nation").last().alias("Nation")  # Get the last Nation for each ID
    ])
	)

	print(df)
	df = df.with_columns(
		pl.col("Nation").replace_strict(codes, default="unknown")
	)

	df = df.with_columns(
	    (pl.lit("/img/flags/") + pl.col("Nation") + pl.lit(".svg")).alias("Image")
	)
	df = df.rename({col: col.lower() for col in df.columns})
	
	print(df)
	file_path = os.path.expanduser(f"~/blog/daehl-e/static/python/excel365/{sex}/skier_info.json")
	json_data = df.to_dicts()
	with open(file_path, "w") as json_file:
	    json.dump(json_data, json_file, indent=2)

	print(json.dumps(json_data, indent=2))

