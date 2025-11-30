import pandas as pd
import json
import os

# Load Ladies (L) ski jumping data
L = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/L.feather'))
L_Small = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/L_Small.feather'))
L_Medium = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/L_Medium.feather'))
L_Normal = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/L_Normal.feather'))
L_Large = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/L_Large.feather'))
L_Flying = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/L_Flying.feather'))
L_chrono = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/ladies_chrono.feather'))

# Load Men's (M) ski jumping data
M = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/M.feather'))
M_Small = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/M_Small.feather'))
M_Medium = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/M_Medium.feather'))
M_Normal = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/M_Normal.feather'))
M_Large = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/M_Large.feather'))
M_Flying = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/M_Flying.feather'))
M_chrono = pd.read_feather(os.path.expanduser('~/ski/elo/python/skijump/polars/excel365/men_chrono.feather'))

# List of DataFrames
dfs = [L, L_Small, L_Medium, L_Normal, L_Large, L_Flying,
       M, M_Small, M_Medium, M_Normal, M_Large, M_Flying]

dfs_str = ["L", "L_Small", "L_Medium", "L_Normal", "L_Large", "L_Flying",
           "M", "M_Small", "M_Medium", "M_Normal", "M_Large", "M_Flying"]

chronos = [L_chrono, M_chrono]
chronos_str = ["L_chrono", "M_chrono"]

# Load current IDs
def load_current_ids():
    with open(os.path.expanduser('~/blog/daehl-e/static/python/skijump/excel365/M_current_ids.json'), 'r') as f:
        m_current_ids = set(json.load(f).keys())
    with open(os.path.expanduser('~/blog/daehl-e/static/python/skijump/excel365/L_current_ids.json'), 'r') as f:
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
    
    # Select the desired columns for ski jumping chronological data, using formatted birthday
    df_current = df_current[["Place", "Skier", "Nation", "Birthday_Formatted", "Pelo", "Small_Pelo", "Medium_Pelo", "Normal_Pelo",
                             "Large_Pelo", "Flying_Pelo", "ID"]]
    
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

# Process each DataFrame and save the results to JSON files
m_current_ids, l_current_ids = load_current_ids()

for i, df in enumerate(chronos):    
    print(i)
    file_path = os.path.expanduser(f"~/blog/daehl-e/static/python/skijump/excel365/{chronos_str[i]}_current.json")
    
    # Determine which current_ids to use based on the dataframe name
    if 'M_' in chronos_str[i] or chronos_str[i] == 'M_chrono':
        current_ids = m_current_ids
    else:
        current_ids = l_current_ids
    
    process_chrono(df, file_path, current_ids)

# Uncomment below to process individual category DataFrames
# for i, df in enumerate(dfs):
#     print(i)
#     file_path = os.path.expanduser("~/blog/daehl-e/static/python/skijump/excel365/{dfs_str[i]}_current.json")
#     
#     # Determine which current_ids to use based on the dataframe name
#     if 'M_' in dfs_str[i] or dfs_str[i].startswith('M'):
#         current_ids = m_current_ids
#     else:
#         current_ids = l_current_ids
#     
#     process_df(df, file_path, current_ids)

print("All ski jumping files have been processed.")