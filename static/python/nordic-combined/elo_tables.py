import pandas as pd
import json
import os

# Load Ladies (L) nordic combined data
L = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/L.feather')
L_Individual = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/L_Individual.feather')
L_Individual_Compact = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/L_Individual_Compact.feather')
L_Mass_Start = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/L_Mass_Start.feather')
L_Sprint = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/L_Sprint.feather')
L_chrono = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/ladies_chrono.feather')

# Load Men's (M) nordic combined data
M = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/M.feather')
M_Individual = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/M_Individual.feather')
M_Individual_Compact = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/M_Individual_Compact.feather')
M_Mass_Start = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/M_Mass_Start.feather')
M_Sprint = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/M_Sprint.feather')
M_chrono = pd.read_feather('/Users/syverjohansen/ski/elo/python/nordic-combined/polars/excel365/men_chrono.feather')

# List of DataFrames
dfs = [L, L_Individual, L_Individual_Compact, L_Mass_Start, L_Sprint,
       M, M_Individual, M_Individual_Compact, M_Mass_Start, M_Sprint]

dfs_str = ["L", "L_Individual", "L_Individual_Compact", "L_Mass_Start", "L_Sprint",
           "M", "M_Individual", "M_Individual_Compact", "M_Mass_Start", "M_Sprint"]

chronos = [L_chrono, M_chrono]
chronos_str = ["L_chrono", "M_chrono"]

def process_chrono(df, file_path):
    # Filter the DataFrame to keep only the rows with the maximum date
    df_current = df.loc[df['Date'] == max(df['Date'])]
    
    # Sort the filtered DataFrame by the "Elo" column in descending order
    df_current = df_current.sort_values(by="Elo", ascending=False)
    
    # Add a "Place" column with values ranging from 1 to the length of the DataFrame
    df_current['Place'] = range(1, len(df_current) + 1)
    
    # Select the desired columns for nordic combined chronological data
    df_current = df_current[["Place", "Skier", "Nation", "Age", "Pelo", "Individual_Pelo", "IndividualCompact_Pelo", 
                             "MassStart_Pelo", "Sprint_Pelo", "ID"]]
    print(df_current)
    
    # Save the result to a JSON file
    df_current.to_json(file_path, orient='records', lines=False)

# Function to process each DataFrame
def process_df(df, file_path):
    # Filter the DataFrame to keep only the rows with the maximum date
    df_current = df.loc[df['Date'] == max(df['Date'])]
    
    # Sort the filtered DataFrame by the "Elo" column in descending order
    df_current = df_current.sort_values(by="Elo", ascending=False)
    
    # Add a "Place" column with values ranging from 1 to the length of the DataFrame
    df_current['Place'] = range(1, len(df_current) + 1)
    
    # Select the desired columns
    df_current = df_current[["Place", "Skier", "Nation", "Age", "Pelo", "ID"]]
    print(df_current)
    
    # Save the result to a JSON file
    df_current.to_json(file_path, orient='records', lines=False)

# Process each DataFrame and save the results to JSON files
for i, df in enumerate(chronos):	
    print(i)
    file_path = f"/Users/syverjohansen/blog/daehl-e/static/python/nordic-combined/excel365/{chronos_str[i]}_current.json"
    process_chrono(df, file_path)

# Uncomment below to process individual category DataFrames
# for i, df in enumerate(dfs):
#     print(i)
#     file_path = f"/Users/syverjohansen/blog/daehl-e/static/python/nordic-combined/excel365/{dfs_str[i]}_current.json"
#     process_df(df, file_path)

print("All nordic combined files have been processed.")