import pandas as pd
import json
import os

# Load Ladies (L) alpine skiing data
L = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/L.feather'))
L_Downhill = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/L_Downhill.feather'))
L_SuperG = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/L_SuperG.feather'))
L_GS = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/L_GS.feather'))
L_SL = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/L_SL.feather'))
L_Combined = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/L_Combined.feather'))
L_Tech = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/L_Tech.feather'))
L_Speed = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/L_Speed.feather'))
L_chrono = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/ladies_chrono.feather'))

# Load Men's (M) alpine skiing data
M = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/M.feather'))
M_Downhill = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/M_Downhill.feather'))
M_SuperG = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/M_SuperG.feather'))
M_GS = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/M_GS.feather'))
M_SL = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/M_SL.feather'))
M_Combined = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/M_Combined.feather'))
M_Tech = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/M_Tech.feather'))
M_Speed = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/M_Speed.feather'))
M_chrono = pd.read_feather(os.path.expanduser('~/ski/elo/python/alpine/polars/excel365/men_chrono.feather'))

# List of DataFrames
dfs = [L, L_Downhill, L_SuperG, L_GS, L_SL, L_Combined, L_Tech, L_Speed,
       M, M_Downhill, M_SuperG, M_GS, M_SL, M_Combined, M_Tech, M_Speed]

dfs_str = ["L", "L_Downhill", "L_SuperG", "L_GS", "L_SL", "L_Combined", "L_Tech", "L_Speed",
           "M", "M_Downhill", "M_SuperG", "M_GS", "M_SL", "M_Combined", "M_Tech", "M_Speed"]

chronos = [L_chrono, M_chrono]
chronos_str = ["L_chrono", "M_chrono"]

def process_chrono(df, file_path):
    # Filter the DataFrame to keep only the rows with the maximum date
    df_current = df.loc[df['Date'] == max(df['Date'])]
    
    # Sort the filtered DataFrame by the "Elo" column in descending order
    df_current = df_current.sort_values(by="Elo", ascending=False)
    
    # Add a "Place" column with values ranging from 1 to the length of the DataFrame
    df_current['Place'] = range(1, len(df_current) + 1)
    
    # Format birthday as readable date string for frontend age calculation
    def format_birthday(birthday_str):
        if pd.isna(birthday_str) or birthday_str == '' or str(birthday_str) == '1700-01-01T00:00:00.000000':
            return None
        try:
            # Handle different birthday formats and convert to YYYY-MM-DD
            if 'T' in str(birthday_str):
                return str(birthday_str).split('T')[0]
            else:
                return str(birthday_str)
        except:
            return None
    
    df_current['Birthday_Formatted'] = df_current['Birthday'].apply(format_birthday)
    
    # Select the desired columns for alpine skiing chronological data, using formatted birthday
    df_current = df_current[["Place", "Skier", "Nation", "Birthday_Formatted", "Pelo", "Downhill_Pelo", "Super G_Pelo", 
                             "Giant Slalom_Pelo", "Slalom_Pelo", "Combined_Pelo", "Tech_Pelo", "Speed_Pelo", "ID"]]
    
    # Rename Birthday_Formatted to Birthday for JSON output
    df_current = df_current.rename(columns={'Birthday_Formatted': 'Birthday'})
    
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
    file_path = f"/Users/syverjohansen/blog/daehl-e/static/python/alpine/excel365/{chronos_str[i]}_current.json"
    process_chrono(df, file_path)

# Uncomment below to process individual category DataFrames
# for i, df in enumerate(dfs):
#     print(i)
#     file_path = f"/Users/syverjohansen/blog/daehl-e/static/python/alpine/excel365/{dfs_str[i]}_current.json"
#     process_df(df, file_path)

print("All alpine skiing files have been processed.")