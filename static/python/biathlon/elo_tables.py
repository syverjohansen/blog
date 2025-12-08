import pandas as pd
import json
import os
from datetime import datetime

# Load Ladies (L) biathlon data
L = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/L.csv'))
L_Sprint = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/L_Sprint.csv'))
L_Pursuit = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/L_Pursuit.csv'))
L_Individual = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/L_Individual.csv'))
L_Mass_Start = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/L_Mass_Start.csv'))
L_chrono = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/ladies_chrono.csv'))

# Load Men's (M) biathlon data
M = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/M.csv'))
M_Sprint = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/M_Sprint.csv'))
M_Pursuit = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/M_Pursuit.csv'))
M_Individual = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/M_Individual.csv'))
M_Mass_Start = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/M_Mass_Start.csv'))
M_chrono = pd.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/men_chrono.csv'))

# List of DataFrames
dfs = [L, L_Sprint, L_Pursuit, L_Individual, L_Mass_Start,
       M, M_Sprint, M_Pursuit, M_Individual, M_Mass_Start]

dfs_str = ["L", "L_Sprint", "L_Pursuit", "L_Individual", "L_Mass_Start",
           "M", "M_Sprint", "M_Pursuit", "M_Individual", "M_Mass_Start"]

chronos = [L_chrono, M_chrono]
chronos_str = ["L_chrono", "M_chrono"]

# Load current IDs
def load_current_ids():
    with open(os.path.expanduser('~/blog/daehl-e/static/python/biathlon/excel365/M_current_ids.json'), 'r') as f:
        m_current_ids = set(json.load(f).keys())
    with open(os.path.expanduser('~/blog/daehl-e/static/python/biathlon/excel365/L_current_ids.json'), 'r') as f:
        l_current_ids = set(json.load(f).keys())
    return m_current_ids, l_current_ids

def process_chrono(df, file_path, current_ids):
    # Filter the DataFrame to keep only the rows with the maximum date
    df_current = df.loc[df['Date'] == max(df['Date'])]
    
    # Filter to include only current skiers based on IDs
    df_current = df_current[df_current['ID'].astype(str).isin(current_ids)]
    
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
    
    # Select the desired columns for biathlon chronological data, using formatted birthday
    df_current = df_current[["Place", "Skier", "Nation", "Birthday_Formatted", "Pelo", "Sprint_Pelo", "Pursuit_Pelo", 
                             "Individual_Pelo", "MassStart_Pelo", "ID"]]
    
    # Rename Birthday_Formatted to Birthday for JSON output
    df_current = df_current.rename(columns={'Birthday_Formatted': 'Birthday'})
    
    print(df_current)
    
    # Save the result to a JSON file
    df_current.to_json(file_path, orient='records', lines=False)

# Function to process each DataFrame
def process_df(df, file_path, current_ids):
    # Filter the DataFrame to keep only the rows with the maximum date
    df_current = df.loc[df['Date'] == max(df['Date'])]
    
    # Filter to include only current skiers based on IDs
    df_current = df_current[df_current['ID'].astype(str).isin(current_ids)]
    
    # Sort the filtered DataFrame by the "Elo" column in descending order
    df_current = df_current.sort_values(by="Elo", ascending=False)
    
    # Add a "Place" column with values ranging from 1 to the length of the DataFrame
    df_current['Place'] = range(1, len(df_current) + 1)
    
    # Select the desired columns
    df_current = df_current[["Place", "Skier", "Nation", "Age", "Pelo", "ID"]]
    print(df_current)
    
    # Save the result to a JSON file
    df_current.to_json(file_path, orient='records', lines=False)

# Load current IDs
m_current_ids, l_current_ids = load_current_ids()

# Process each DataFrame and save the results to JSON files
for i, df in enumerate(chronos):    
    print(i)
    file_path = os.path.expanduser(f"~/blog/daehl-e/static/python/biathlon/excel365/{chronos_str[i]}_current.json")
    
    # Use appropriate current_ids based on gender
    current_ids = l_current_ids if 'L_' in chronos_str[i] else m_current_ids
    process_chrono(df, file_path, current_ids)

# Uncomment below to process individual category DataFrames
# for i, df in enumerate(dfs):
#     print(i)
#     file_path = os.path.expanduser("~/blog/daehl-e/static/python/biathlon/excel365/{dfs_str[i]}_current.json")
#     process_df(df, file_path)

print("All biathlon files have been processed.")