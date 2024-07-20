import pandas as pd
import polars as pl
import json
import os


L_chrono = pl.read_ipc('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/ladies_chrono.feather')


M_chrono = pl.read_ipc('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/men_chrono.feather')

L_chrono = L_chrono.filter(pl.col("City")!="Summer")
M_chrono = M_chrono.filter(pl.col("City")!="Summer")
output_dir_L = '/Users/syverjohansen/blog/daehl-e/content/skiers/L'
output_dir_M = '/Users/syverjohansen/blog/daehl-e/content/skiers/M'

unique_ladies = L_chrono.select("ID").unique()
unique_men = M_chrono.select("ID").unique()


def process_chrono(df, ID, sex):
    # Filter the DataFrame to keep only the rows with the maximum date
    df= df.filter(pl.col("ID")==ID)
    
    # Sort the filtered DataFrame by the "Elo" column in descending order
    df = df.sort(["Season", "Race"])

    
    # Select the desired columns
    df = df.select(["Exp", "Date", "City", "Country", "Event", "Distance", "MS", "Technique", "Place", "Season", "Race", "Age", "Elo","Distance_Elo", "Distance_C_Pelo", "Distance_F_Pelo",
    "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", "Classic_Elo", "Freestyle_Elo", "ID"])

    file_path = f"/Users/syverjohansen/blog/daehl-e/static/python/excel365/{sex}/{ID}.json"
    # Save the result to a JSON file
    json_str = df.write_json(file_path)
    #with open(file_path, 'w') as f:
    #    f.write(json_str)

for id_value in unique_ladies["ID"]:
    process_chrono(L_chrono, id_value, "L")


for id_value in unique_men["ID"]:
    process_chrono(M_chrono, id_value, "M")

for row in unique_ladies.iter_rows(named=True):
    skier_id = row['ID']
    skier  = L_chrono.filter(pl.col("ID")==skier_id)[-1, "Skier"]
    
    # Create Markdown content for each skier
    markdown_content = f"""---
title: Skier Profile for {skier}
sex: "L"
id: "{skier_id}"
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
    
    # Create Markdown content for each skier
    markdown_content = f"""---
title: Skier Profile for {skier}
sex: "M"
id: "{skier_id}"
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


