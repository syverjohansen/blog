import pandas as pd
import polars as pl
print(pl.__version__)
import json
import os


L_chrono = pl.read_ipc(os.path.expanduser('~/ski/elo/python/ski/polars/excel365/ladies_chrono.feather'))


M_chrono = pl.read_ipc(os.path.expanduser('~/ski/elo/python/ski/polars/excel365/men_chrono.feather'))

L_chrono = L_chrono.filter(pl.col("City")!="Summer")
M_chrono = M_chrono.filter(pl.col("City")!="Summer")

L_chrono = L_chrono.with_columns(
    pl.col("Date").str.strptime(pl.Date, "%Y-%m-%d")
)


M_chrono = M_chrono.with_columns(
    pl.col("Date").str.strptime(pl.Date, "%Y-%m-%d")
)





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
    columns = ["Age", "Elo_Pct","Distance_Elo_Pct", "Distance_C_Elo_Pct", "Distance_F_Elo_Pct",
    "Sprint_Elo_Pct", "Sprint_C_Elo_Pct", "Sprint_F_Elo_Pct", "Classic_Elo_Pct", "Freestyle_Elo_Pct"]
    for col in columns:

        df = df.with_columns(pl.col(col).round(2))
    return df


def process_season(df, ID, sex):
    # Filter the DataFrame to keep only the rows with the maximum date
    df= df.filter(pl.col("ID")==ID)
    
    # Sort the filtered DataFrame by the "Elo" column in descending order
    df = df.sort(["Season", "Race"])

    
    # Select the desired columns
    df = df.select(["Exp", "Date", "City", "Country", "Event", "Distance", "MS", "Technique", "Place", "Season", "Race", "Age", "Elo_Pct","Distance_Elo_Pct", "Distance_C_Elo_Pct", "Distance_F_Elo_Pct",
    "Sprint_Elo_Pct", "Sprint_C_Elo_Pct", "Sprint_F_Elo_Pct", "Classic_Elo_Pct", "Freestyle_Elo_Pct", "ID"])

    file_path = f"/Users/syverjohansen/blog/daehl-e/static/python/excel365/{sex}/{ID}_pct.json"
    # Save the result to a JSON file
    json_str = df.write_json(file_path)


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


unique_ladies = ladies_season_df.select("ID").unique()
unique_men = men_season_df.select("ID").unique()

for id_value in unique_ladies["ID"]:
    process_season(ladies_season_df, id_value, "l")


for id_value in unique_men["ID"]:
    process_season(men_season_df, id_value, "m")