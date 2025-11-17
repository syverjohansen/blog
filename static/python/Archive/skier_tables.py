import pandas as pd
import polars as pl
import json
import os
import math


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


def get_season(df, season, min_season):
    if(min_season==True):
        max_elo = 1300
        season_df = df.filter(pl.col("Season")==season)
        preseason = season_df.filter(pl.col("City")=="Summer")
        preseason.with_columns([
            pl.lit(1300).alias("Elo"),
            pl.lit(1300).alias("Distance_Elo"),
            pl.lit(1300).alias("Distance_C_Elo"),
            pl.lit(1300).alias("Distance_F_Elo"),
            pl.lit(1300).alias("Sprint_Elo"),
            pl.lit(1300).alias("Sprint_C_Elo"),
            pl.lit(1300).alias("Sprint_F_Elo"),
            pl.lit(1300).alias("Classic_Elo"),
            pl.lit(1300).alias("Freestyle_Elo")
        ])
        season_df = season_df.filter(pl.col("City")!="Summer")
        season_df = pl.concat([preseason, season_df])
        season_df = season_df.sort("Race", "Elo", descending=[False, True])

        pct_df = season_df

        columns = ["Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
               "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", "Classic_Elo", "Freestyle_Elo"]

        # Create cumulative max and percentage columns for each specified column
        for col in columns:
            pct_df = pct_df.with_columns([
            pl.col(col).cum_max().alias(f"{col}_Cumulative_Max")])

            pct_df = pct_df.with_columns([
            (100*pl.col(col) / pl.col(f"{col}_Cumulative_Max")).alias(f"{col}_Pct")
            ])
        pct_df = pct_df.filter(pl.col("City")!="Summer")
        return pct_df
    else:
        preseason = df.filter((pl.col("City")=="Summer") & (pl.col("Season")==(season-1)))
        season_df = df.filter(pl.col("Season")==season)
        season_df = season_df.filter(pl.col("City")!="Summer")
        unique_ids = season_df.select("ID").unique()

        preseason = preseason.filter(pl.col("ID").is_in(unique_ids["ID"]))
        season_df = pl.concat([preseason, season_df])
        season_df = season_df.sort("Race", "Elo", descending=[False, True])
        pct_df = season_df
        columns = ["Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
               "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", "Classic_Elo", "Freestyle_Elo"]

        # Create cumulative max and percentage columns for each specified column
        for col in columns:
            pct_df = pct_df.with_columns([
            pl.col(col).cum_max().alias(f"{col}_Cumulative_Max")])

            pct_df = pct_df.with_columns([
            (100*pl.col(col) / pl.col(f"{col}_Cumulative_Max")).alias(f"{col}_Pct")
            ])
        pct_df = pct_df.filter(pl.col("City")!="Summer")
        return pct_df


def build_pct_df(df, seasons, min_season):
    pct_df = pl.DataFrame()
    for season in seasons["Season"]:
        print(season)
        if(season==min_season):
            season_df = get_season(df, season, True)
        else:
            season_df = get_season(df, season, False)
        pct_df = pl.concat([pct_df, season_df])
    return pct_df


def chop_decimals(df):
    df = df.with_columns(pl.col("Age").cast(pl.Float64))
    columns = ["Age","Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
           "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", "Classic_Elo", "Freestyle_Elo", "Elo_Pct","Distance_Elo_Pct", "Distance_C_Elo_Pct", "Distance_F_Elo_Pct",
    "Sprint_Elo_Pct", "Sprint_C_Elo_Pct", "Sprint_F_Elo_Pct", "Classic_Elo_Pct", "Freestyle_Elo_Pct"]
    for col in columns:
        df = df.with_columns(pl.col(col).round(2))
    return df


def process_season(df, ID, sex, skier_data_dict):
    # Filter the DataFrame to keep only the rows with the given ID
    df = df.filter(pl.col("ID")==ID)
    
    # Sort the filtered DataFrame
    df = df.sort(["Season", "Race"])
    
    # Select the desired columns
    df = df.select(["Exp", "Date", "City", "Country", "Event", "Distance", "MS", "Technique", "Place", "Season", "Race", "Age","Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo",
           "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", "Classic_Elo", "Freestyle_Elo", "Elo_Pct","Distance_Elo_Pct", "Distance_C_Elo_Pct", "Distance_F_Elo_Pct",
    "Sprint_Elo_Pct", "Sprint_C_Elo_Pct", "Sprint_F_Elo_Pct", "Classic_Elo_Pct", "Freestyle_Elo_Pct", "ID"])

    # Convert to dict for JSON serialization
    skier_data_dict[str(ID)] = df.to_dict(as_series=False)


def main():
    # Load data
    base_path = os.path.expanduser('~/ski/elo/python/ski/polars/excel365')
    output_dir_base = os.path.expanduser('~/blog/daehl-e/static/python/cross-country/excel365')
    # Make sure base output directory exists
    os.makedirs(output_dir_base, exist_ok=True)
    
    # Create subdirectories
    for gender in ['L', 'M']:
        os.makedirs(os.path.join(output_dir_base, gender), exist_ok=True)
    
    L_chrono = pl.read_ipc(os.path.join(base_path, 'ladies_chrono.feather'))
    M_chrono = pl.read_ipc(os.path.join(base_path, 'men_chrono.feather'))
    
    # Process Date columns
    L_chrono = L_chrono.with_columns(
        pl.col("Date").str.strptime(pl.Date, "%Y-%m-%d", strict=False)
    )
    
    M_chrono = M_chrono.with_columns(
        pl.col("Date").str.strptime(pl.Date, "%Y-%m-%d", strict=False)
    )
    
    # Build percentile DataFrames
    ladies_seasons = L_chrono.select("Season").unique()
    ladies_seasons = ladies_seasons.sort("Season", descending=False)
    min_ladies = min(ladies_seasons["Season"])
    ladies_season_df = build_pct_df(L_chrono, ladies_seasons, min_ladies)
    ladies_season_df = chop_decimals(ladies_season_df)
    
    men_seasons = M_chrono.select("Season").unique()
    men_seasons = men_seasons.sort("Season", descending=False)
    min_men = min(men_seasons["Season"])
    men_season_df = build_pct_df(M_chrono, men_seasons, min_men)
    men_season_df = chop_decimals(men_season_df)
    
    # Get unique IDs
    unique_ladies = ladies_season_df.select("ID").unique()
    unique_men = men_season_df.select("ID").unique()
    
    # Create ranges for grouping IDs (e.g., 1-1000, 1001-2000, etc.)
    GROUP_SIZE = 1000
    
    # Process ladies data by groups
    ladies_groups = {}
    for id_value in unique_ladies["ID"]:
        group_key = str(math.floor(id_value / GROUP_SIZE) * GROUP_SIZE)
        if group_key not in ladies_groups:
            ladies_groups[group_key] = {}
        process_season(ladies_season_df, id_value, "l", ladies_groups[group_key])
    
    # Save ladies data by groups
    for group_key, group_data in ladies_groups.items():
        group_end = int(group_key) + GROUP_SIZE - 1
        file_path = os.path.join(output_dir_base, "L", f"skiers_{group_key}_{group_end}.json")
        with open(file_path, 'w') as f:
            json.dump(group_data, f)
    
    # Process men data by groups
    men_groups = {}
    for id_value in unique_men["ID"]:
        group_key = str(math.floor(id_value / GROUP_SIZE) * GROUP_SIZE)
        if group_key not in men_groups:
            men_groups[group_key] = {}
        process_season(men_season_df, id_value, "m", men_groups[group_key])
    
    # Save men data by groups
    for group_key, group_data in men_groups.items():
        group_end = int(group_key) + GROUP_SIZE - 1
        file_path = os.path.join(output_dir_base, "M", f"skiers_{group_key}_{group_end}.json")
        with open(file_path, 'w') as f:
            json.dump(group_data, f)
    
    # Create lookup tables for skier info
    ladies_info = []
    for id_value in unique_ladies["ID"]:
        skier_name = L_chrono.filter(pl.col("ID")==id_value)[-1, "Skier"]
        nation = L_chrono.filter(pl.col("ID")==id_value)[-1, "Nation"]
        nation_code = get_country_code(nation)
        ladies_info.append({
            "id": int(id_value),
            "skier": skier_name,
            "nation": nation,
            "nation_code": nation_code,
            "group": str(math.floor(id_value / GROUP_SIZE) * GROUP_SIZE)
        })
    
    men_info = []
    for id_value in unique_men["ID"]:
        skier_name = M_chrono.filter(pl.col("ID")==id_value)[-1, "Skier"]
        nation = M_chrono.filter(pl.col("ID")==id_value)[-1, "Nation"]
        nation_code = get_country_code(nation)
        men_info.append({
            "id": int(id_value),
            "skier": skier_name,
            "nation": nation,
            "nation_code": nation_code,
            "group": str(math.floor(id_value / GROUP_SIZE) * GROUP_SIZE)
        })
    
    # Save lookup tables
    with open(os.path.join(output_dir_base, "L_skiers_lookup.json"), 'w') as f:
        json.dump(ladies_info, f)
    
    with open(os.path.join(output_dir_base, "M_skiers_lookup.json"), 'w') as f:
        json.dump(men_info, f)
    
    # Create ID to name maps for compatibility with existing code
    ladies_id_map = {str(info["id"]): info["skier"] for info in ladies_info}
    men_id_map = {str(info["id"]): info["skier"] for info in men_info}
    
    with open(os.path.join(output_dir_base, "L_all_ids.json"), 'w') as f:
        json.dump(ladies_id_map, f)
    
    with open(os.path.join(output_dir_base, "M_all_ids.json"), 'w') as f:
        json.dump(men_id_map, f)

    # Create ID to name maps for current active skiers
    latest_ladies_season = max(ladies_seasons["Season"])
    latest_men_season = max(men_seasons["Season"])
    
    current_ladies = ladies_season_df.filter(pl.col("Season") == latest_ladies_season).select("ID").unique()
    current_men = men_season_df.filter(pl.col("Season") == latest_men_season).select("ID").unique()
    
    current_ladies_map = {str(id_value): ladies_id_map[str(id_value)] 
                          for id_value in current_ladies["ID"]}
    current_men_map = {str(id_value): men_id_map[str(id_value)] 
                       for id_value in current_men["ID"]}
    
    with open(os.path.join(output_dir_base, "L_current_ids.json"), 'w') as f:
        json.dump(current_ladies_map, f)
    
    with open(os.path.join(output_dir_base, "M_current_ids.json"), 'w') as f:
        json.dump(current_men_map, f)

    print("All files have been processed and grouped successfully.")


if __name__ == "__main__":
    main()