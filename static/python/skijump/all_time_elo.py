import pandas as pd
import json
import os
import numpy as np

# Load feather files
L_chrono = pd.read_feather('/Users/syverjohansen/ski/elo/python/skijump/polars/excel365/ladies_chrono.feather')
M_chrono = pd.read_feather('/Users/syverjohansen/ski/elo/python/skijump/polars/excel365/men_chrono.feather')

chronos = [L_chrono, M_chrono]
chronos_str = ["L_chrono", "M_chrono"]

def process_all_time_records(df, file_path):
    # First, replace any infinite values with NaN
    df = df.replace([np.inf, -np.inf], np.nan)
    
    # Define column mapping to simpler names
    column_mapping = {
        'Elo': 'Overall',
        'Small_Elo': 'Small',
        'Medium_Elo': 'Medium',
        'Normal_Elo': 'Normal',
        'Large_Elo': 'Large',
        'Flying_Elo': 'Flying'
    }
    
    # Group by ID and find maximum values for each category
    all_time_records = df.groupby('ID').agg({
        'Skier': 'last',    # Get the most recent name
        'Nation': 'last',   # Get the most recent nation
        'Elo': 'max',
        'Small_Elo': 'max',
        'Medium_Elo': 'max',
        'Normal_Elo': 'max',
        'Large_Elo': 'max',
        'Flying_Elo': 'max'
    }).reset_index()
    
    # Sort by overall Elo, handling NaN values
    all_time_records = all_time_records.sort_values(
        by="Elo", 
        ascending=False,
        na_position='last'
    )
    
    # Add place column
    all_time_records['Place'] = range(1, len(all_time_records) + 1)
    
    # Create a dictionary to store dates for each column
    date_columns = {}
    
    # Find dates for maximum values before renaming columns
    for old_col, new_col in column_mapping.items():
        if old_col == 'ID':
            continue
            
        date_column = f"{new_col}_Date"
        dates = []
        
        for id_val in all_time_records['ID']:
            id_data = df[df['ID'] == id_val]
            max_value = all_time_records.loc[all_time_records['ID'] == id_val, old_col].iloc[0]
            
            if pd.notna(max_value):
                date_mask = (id_data[old_col] == max_value) & (id_data[old_col].notna())
                if date_mask.any():
                    max_date = id_data[date_mask]['Date'].iloc[0]
                    dates.append(max_date)
                else:
                    dates.append(None)
            else:
                dates.append(None)
        
        date_columns[date_column] = dates
    
    # Rename columns to simpler names
    all_time_records = all_time_records.rename(columns=column_mapping)
    
    # Reorder columns
    all_time_records = all_time_records[[
        "Place", "Skier", "Nation", "Overall", "Small", 
        "Medium", "Normal", "Large", 
        "Flying", "ID"
    ]]
    
    # Add date columns
    for date_col, dates in date_columns.items():
        all_time_records[date_col] = dates
    
    # Print debug for Maja
    #print("\nMaja Dahlqvist's final record:")
    #maja_records = all_time_records[all_time_records['Skier'] == 'Maja Dahlqvist']
    pd.set_option('display.max_columns', None)
    pd.set_option('display.width', None)
    #print(maja_records)
    
    # Save to JSON
    all_time_records.to_json(file_path, orient='records', lines=False)

# Process each DataFrame
for i, df in enumerate(chronos):
    print(f"Processing {chronos_str[i]}")
    file_path = f"/Users/syverjohansen/blog/daehl-e/static/python/skijump/excel365/{chronos_str[i]}_all_time.json"
    process_all_time_records(df, file_path)

print("All files have been processed.")