import pandas as pd
import json
import os

L = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/L.feather')
L_Distance = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/L_Distance.feather')
L_Distance_C = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/L_Distance_C.feather')
L_Distance_F = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/L_Distance_F.feather')
L_Sprint = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/L_Sprint.feather')
L_Sprint_C = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/L_Sprint_C.feather')
L_Sprint_F = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/L_Sprint_F.feather')
L_C = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/L_C.feather')
L_F = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/L_F.feather')
L_chrono = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/ladies_chrono.feather')

M = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/M.feather')
M_Distance = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/M_Distance.feather')
M_Distance_C = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/M_Distance_C.feather')
M_Distance_F = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/M_Distance_F.feather')
M_Sprint = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/M_Sprint.feather')
M_Sprint_C = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/M_Sprint_C.feather')
M_Sprint_F = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/M_Sprint_F.feather')
M_C = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/M_C.feather')
M_F = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/M_F.feather')
M_chrono = pd.read_feather('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/men_chrono.feather')


# List of DataFrames
dfs = [L, L_Distance, L_Distance_C, L_Distance_F, L_Sprint, L_Sprint_C, L_Sprint_F,
       L_C, L_F, M, M_Distance, M_Distance_F, M_Distance_C, M_Sprint, M_Sprint_C, M_Sprint_F,
       M_C, M_F]

dfs_str = ["L", "L_Distance", "L_Distance_C", "L_Distance_F", "L_Sprint", "L_Sprint_C", "L_Sprint_F",
       "L_C", "L_F", "M", "M_Distance", "M_Distance_F", "M_Distance_C", "M_Sprint", "M_Sprint_C", "M_Sprint_F",
       "M_C", "M_F"]

chronos = [L_chrono, M_chrono]
chronos_str = ["L_chrono", "M_chrono"]



def process_chrono(df, file_path):
    # Filter the DataFrame to keep only the rows with the maximum date
    df_current = df.loc[df['Date'] == max(df['Date'])]
    
    # Sort the filtered DataFrame by the "Elo" column in descending order
    df_current = df_current.sort_values(by="Elo", ascending=False)
    
    # Add a "Place" column with values ranging from 1 to the length of the DataFrame
    df_current['Place'] = range(1, len(df_current) + 1)
    
    # Select the desired columns
    df_current = df_current[["Place", "Skier", "Nation", "Age", "Pelo","Distance_Pelo", "Distance_C_Pelo", "Distance_F_Pelo",
    "Sprint_Pelo", "Sprint_C_Pelo", "Sprint_F_Pelo", "Classic_Pelo", "Freestyle_Pelo", "ID"]]
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
    df_current = df_current[["Place", "Skier", "Nation", "Age", "Pelo" , "ID"]]
    print(df_current)
    
    # Save the result to a JSON file
    df_current.to_json(file_path, orient='records', lines=False)

# Process each DataFrame and save the results to JSON files
#for i, df in enumerate(dfs):
for i, df in enumerate(chronos):	
    print(i)
    file_path = f"/Users/syverjohansen/blog/daehl-e/static/python/cross-country/excel365/{chronos_str[i]}_current.json"
    #process_df(df, file_path)
    process_chrono(df, file_path)



'''L.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/L.csv')
L_Distance.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Distance.csv')
L_Distance_C.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Distance_C.csv')
L_Distance_F.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Distance_F.csv')
L_Sprint.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Sprint.csv')
L_Sprint_C.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Sprint_C.csv')
L_Sprint_F.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Sprint_F.csv')
L_C.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_C.csv')
L_F.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_F.csv')

M.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/M.csv')
M_Distance.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Distance.csv')
M_Distance_C.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Distance_C.csv')
M_Distance_F.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Distance_F.csv')
M_Sprint.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Sprint.csv')
M_Sprint_C.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Sprint_C.csv')
M_Sprint_F.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Sprint_F.csv')
M_C.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_C.csv')
M_F.to_csv('/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_F.csv')'''


'''L.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/L.json", 
	orient = 'records', lines=False)
L_Distance.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Distance.json", 
	orient = 'records', lines=False)
L_Distance_C.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Distance_C.json",
	orient= 'records', lines=False)
L_Distance_F.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Distance_F.json",
	orient = 'records', lines=False)
L_Sprint.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Sprint.json",
	orient = 'records', lines=False)
L_Sprint_C.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Sprint_C.json",
	orient = 'records', lines=False)
L_Sprint_F.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_Sprint_F.json",
	orient = 'records', lines=False)
L_C.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_C.json",
	orient = 'records', lines=False)
L_F.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/L_F.json",
	orient = 'records', lines=False)





M.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/M.json", 
	orient = 'records', lines=False)
M_Distance.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Distance.json", 
	orient = 'records', lines=False)
M_Distance_C.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Distance_C.json",
	orient = 'records', lines=False)
M_Distance_F.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Distance_F.json",
	orient = 'records', lines=False)
M_Sprint.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Sprint.json",
	orient = 'records', lines=False)
M_Sprint_C.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Sprint_C.json",
	orient = 'records', lines=False)
M_Sprint_F.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_Sprint_F.json",
	orient = 'records', lines=False)
M_C.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_C.json",
	orient = 'records', lines=False)
M_F.to_json("/Users/syverjohansen/blog/daehl-e/static/python/excel365/M_F.json",
	orient = 'records', lines=False)'''



# Ensure the output directory exists


print("All files have been processed.")