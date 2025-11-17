import pandas as pd
import polars as pl
print(pl.__version__)
import json
import os


L_chrono = pl.read_ipc(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/ladies_chrono.feather'))


M_chrono = pl.read_ipc(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/men_chrono.feather'))

#L_chrono = L_chrono.filter(pl.col("City")!="Summer")
#M_chrono = M_chrono.filter(pl.col("City")!="Summer")

codes = {
    "Albania": "alb", "Algeria": "alg", "Andorra": "and", "Argentina": "arg", "Armenia": "arm", 
    "Australia": "aus", "Austria": "aut", "Azerbaijan": "aze", "Belarus": "blr", "Belgium": "bel", 
    "Bermuda": "ber", "Bolivia": "bol", "Bosnia&Herzegovina": "bih", "Brazil": "bra", "Bulgaria": "bul",
    "Cameroon": "cmr", "Canada": "can", "Cayman Islands": "cay", "Chile": "chi", "China": "chn", 
    "Colombia": "col", "Costa Rica": "crc", "Croatia": "cro", "Cyprus": "cyp", "Czechia": "cze", 
    "Denmark": "den", "East Timor": "tls", "Ecuador": "ecu", "Egypt": "egy", "Eritrea": "eri", 
    "Estonia": "est", "Eswatini": "swz", "Ethiopia": "eth", "Fiji": "fij", "Finland": "fin", 
    "FIS": "fis", "France": "fra", "Georgia": "geo", "Germany": "ger", "Ghana": "gha", 
    "Great Britain": "gbr", "Greece": "gre", "Greenland": "grl", "Grenada": "grn", "Guam": "gum", 
    "Guatemala": "gua", "Haiti": "hai", "Honduras": "hon", "Hong Kong": "hkg", "Hungary": "hun", 
    "Iceland": "isl", "India": "ind", "Iran": "iri", "Ireland": "irl", "Israel": "isr", 
    "Italy": "ita", "Jamaica": "jam", "Japan": "jpn", "Kazakhstan": "kaz", "Kenya": "ken", 
    "Kosovo": "kos", "Kuwait": "kuw", "Kyrgyzstan": "kgz", "Latvia": "lat", "Lebanon": "lbn", 
    "Liechtenstein": "lie", "Lithuania": "ltu", "Luxembourg": "lux", "Madagascar": "mad", 
    "Malaysia": "mas", "Malta": "mlt", "Mexico": "mex", "Moldova": "mda", "Monaco": "mon", 
    "Mongolia": "mgl", "Montenegro": "mne", "Morocco": "mar", "Nepal": "nep", "Netherlands": "ned", 
    "New Zealand": "nzl", "Nigeria": "ngr", "North Korea": "prk", "North Macedonia": "mkd", 
    "Norway": "nor", "Pakistan": "pak", "Peru": "per", "Philippines": "phi", "Poland": "pol", 
    "Portugal": "por", "Puerto Rico": "pur", "Romania": "rou", "Russia": "rus", "San Marino": "smr", 
    "Saudi Arabia": "ksa", "Senegal": "sen", "Serbia": "srb", "Singapore": "sgp", "Slovakia": "svk", 
    "Slovenia": "slo", "South Africa": "rsa", "South Korea": "kor", "Soviet": "urs", "Spain": "esp", 
    "Sweden": "swe", "Switzerland": "sui", "Taiwan": "tpe", "Tajikistan": "tjk", "Thailand": "tha", 
    "Togo": "tog", "Tonga": "tga", "Trinidad & Tobago": "tto", "Turkey": "tur", "Ukraine": "ukr", 
    "Uruguay": "uru", "US Virgin Islands": "isv", "USA": "usa", "Uzbekistan": "uzb", 
    "Venezuela": "ven", "Yugoslavia": "yug", "Zimbabwe": "zim"
}

def get_country_code(nation):
    codes = {
        "Albania": "alb", "Algeria": "alg", "Andorra": "and", "Argentina": "arg", "Armenia": "arm", 
        "Australia": "aus", "Austria": "aut", "Azerbaijan": "aze", "Belarus": "blr", "Belgium": "bel", 
        "Bermuda": "ber", "Bolivia": "bol", "Bosnia&Herzegovina": "bih", "Brazil": "bra", "Bulgaria": "bul",
        "Cameroon": "cmr", "Canada": "can", "Cayman Islands": "cay", "Chile": "chi", "China": "chn", 
        "Colombia": "col", "Costa Rica": "crc", "Croatia": "cro", "Cyprus": "cyp", "Czechia": "cze", 
        "Denmark": "den", "East Timor": "tls", "Ecuador": "ecu", "Egypt": "egy", "Eritrea": "eri", 
        "Estonia": "est", "Eswatini": "swz", "Ethiopia": "eth", "Fiji": "fij", "Finland": "fin", 
        "FIS": "fis", "France": "fra", "Georgia": "geo", "Germany": "ger", "Ghana": "gha", 
        "Great Britain": "gbr", "Greece": "gre", "Greenland": "grl", "Grenada": "grn", "Guam": "gum", 
        "Guatemala": "gua", "Haiti": "hai", "Honduras": "hon", "Hong Kong": "hkg", "Hungary": "hun", 
        "Iceland": "isl", "India": "ind", "Iran": "iri", "Ireland": "irl", "Israel": "isr", 
        "Italy": "ita", "Jamaica": "jam", "Japan": "jpn", "Kazakhstan": "kaz", "Kenya": "ken", 
        "Kosovo": "kos", "Kuwait": "kuw", "Kyrgyzstan": "kgz", "Latvia": "lat", "Lebanon": "lbn", 
        "Liechtenstein": "lie", "Lithuania": "ltu", "Luxembourg": "lux", "Madagascar": "mad", 
        "Malaysia": "mas", "Malta": "mlt", "Mexico": "mex", "Moldova": "mda", "Monaco": "mon", 
        "Mongolia": "mgl", "Montenegro": "mne", "Morocco": "mar", "Nepal": "nep", "Netherlands": "ned", 
        "New Zealand": "nzl", "Nigeria": "ngr", "North Korea": "prk", "North Macedonia": "mkd", 
        "Norway": "nor", "Pakistan": "pak", "Peru": "per", "Philippines": "phi", "Poland": "pol", 
        "Portugal": "por", "Puerto Rico": "pur", "Romania": "rou", "Russia": "rus", "San Marino": "smr", 
        "Saudi Arabia": "ksa", "Senegal": "sen", "Serbia": "srb", "Singapore": "sgp", "Slovakia": "svk", 
        "Slovenia": "slo", "South Africa": "rsa", "South Korea": "kor", "Soviet": "urs", "Spain": "esp", 
        "Sweden": "swe", "Switzerland": "sui", "Taiwan": "tpe", "Tajikistan": "tjk", "Thailand": "tha", 
        "Togo": "tog", "Tonga": "tga", "Trinidad & Tobago": "tto", "Turkey": "tur", "Ukraine": "ukr", 
        "Uruguay": "uru", "US Virgin Islands": "isv", "USA": "usa", "Uzbekistan": "uzb", 
        "Venezuela": "ven", "Yugoslavia": "yug", "Zimbabwe": "zim"
    }
    return codes.get(nation)  # Return None if the nation isn't found

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
	df = df.with_columns(pl.col("ID").cast(pl.Int32))   
	print(df)
	df = df.with_columns(
		pl.col("Nation").replace_strict(codes)
	)

	df = df.with_columns(
	    (pl.lit("/img/flags/") + pl.col("Nation") + pl.lit(".svg")).alias("Image")
	)
	df = df.rename({col: col.lower() for col in df.columns})
	
	print(df)
	file_path = f"/Users/syverjohansen/blog/daehl-e/static/python/skijump/excel365/{sex}/skier_info.json"
	json_data = df.to_dicts()
	with open(file_path, "w") as json_file:
	    json.dump(json_data, json_file, indent=2)

	print(json.dumps(json_data, indent=2))