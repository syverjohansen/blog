import pandas as pd
import polars as pl
import json
import os


L_chrono = pl.read_ipc(os.path.expanduser('~/ski/elo/python/ski/polars/excel365/ladies_chrono.feather'))


M_chrono = pl.read_ipc(os.path.expanduser('~/ski/elo/python/ski/polars/excel365/men_chrono.feather'))

L_chrono = L_chrono.filter(pl.col("City")!="Summer")
M_chrono = M_chrono.filter(pl.col("City")!="Summer")

L_chrono = L_chrono.with_columns(pl.col("Date").str.strptime(pl.Date, "%Y-%m-%d"))
M_chrono = M_chrono.with_columns(pl.col("Date").str.strptime(pl.Date, "%Y-%m-%d"))

def chop_decimals(df):
    df = df.with_columns(pl.col("Age").cast(pl.Float64))
    columns = ["Age", "Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
           "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", "Classic_Elo", "Freestyle_Elo"]
    for col in columns:
        df = df.with_columns(pl.col(col).round(2))
    return df

output_dir_L = '/Users/syverjohansen/blog/daehl-e/content/skiers/l'
output_dir_M = '/Users/syverjohansen/blog/daehl-e/content/skiers/m'

L_chrono = chop_decimals(L_chrono)
M_chrono = chop_decimals(M_chrono)


unique_ladies = L_chrono.select("ID").unique()
unique_men = M_chrono.select("ID").unique()

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
    return codes[nation]

def process_chrono(df, ID, sex):
    # Filter the DataFrame to keep only the rows with the maximum date
    df= df.filter(pl.col("ID")==ID)
    
    # Sort the filtered DataFrame by the "Elo" column in descending order
    df = df.sort(["Season", "Race"])

    
    # Select the desired columns
    df = df.select(["Exp", "Date", "City", "Country", "Event", "Distance", "MS", "Technique", "Place", "Season", "Race", "Age", "Elo","Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
    "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", "Classic_Elo", "Freestyle_Elo", "ID"])

    file_path = f"/Users/syverjohansen/blog/daehl-e/static/python/excel365/{sex}/{ID}.json"
    # Save the result to a JSON file
    json_str = df.write_json(file_path)
    #with open(file_path, 'w') as f:
    #    f.write(json_str)

for id_value in unique_ladies["ID"]:
    process_chrono(L_chrono, id_value, "l")


for id_value in unique_men["ID"]:
    process_chrono(M_chrono, id_value, "m")

for row in unique_ladies.iter_rows(named=True):
    skier_id = row['ID']
    skier  = L_chrono.filter(pl.col("ID")==skier_id)[-1, "Skier"]
    nation = L_chrono.filter(pl.col("ID")==skier_id)[-1, "Nation"]
    nation = get_country_code(nation)
    # Create Markdown content for each skier
    markdown_content = f"""---
title: Skier Profile for {skier}
sex: "L"
id: "{skier_id}"
image: "/img/flags/{nation}.svg" 
---

# Skier Profile

This page is for: {skier}.
    """

    # Save the Markdown file
    with open(os.path.join(output_dir_L, f'{skier_id}.md'), 'w') as file:
        file.write(markdown_content)

    print(f"Generated Markdown for skier ID {skier}")




print("All files have been processed.")


for row in unique_men.iter_rows(named=True):
    skier_id = row['ID']
    skier  = M_chrono.filter(pl.col("ID")==skier_id)[-1, "Skier"]
    nation = M_chrono.filter(pl.col("ID")==skier_id)[-1, "Nation"]
    nation = get_country_code(nation)
    # Create Markdown content for each skier
    markdown_content = f"""---
title: Skier Profile for {skier}
sex: "M"
id: "{skier_id}"
image: "/img/flags/{nation}.svg" 
---

# Skier Profile

This page is for: {skier}.
    """

    # Save the Markdown file
    with open(os.path.join(output_dir_M, f'{skier_id}.md'), 'w') as file:
        file.write(markdown_content)

    print(f"Generated Markdown for skier ID {skier}")




print("All files have been processed.")

#Convert to excel files for each in /Users/syverjohansen/blog/daehl-e/static/python/excel365