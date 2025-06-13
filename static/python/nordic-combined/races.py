#!/usr/bin/env python
import pandas as pd
import os
import json
from pathlib import Path
import sys

# Define paths
CSV_DIR = Path(os.path.expanduser("~/ski/elo/python/nordic-combined/polars/relay/excel365"))
OUTPUT_BASE_DIR = Path("excel365")

def process_csv(gender, csv_path):
    """Process CSV file and create race JSON files."""
    print(f"Processing {gender} data from {csv_path}")
    
    # Make sure the gender directory exists
    gender_dir = OUTPUT_BASE_DIR / gender
    os.makedirs(gender_dir, exist_ok=True)
    
    # Read CSV file
    try:
        df = pd.read_csv(csv_path)
        print(f"Successfully loaded CSV with {len(df)} rows")
    except Exception as e:
        print(f"Error reading CSV: {e}")
        return

    # Create dictionary to track last offseason Pelo for each skier
    last_offseason_pelo = {}
    
    # Get all seasons
    seasons = sorted(df['Season'].unique())
    print(f"Found {len(seasons)} unique seasons")
    
    # Initialize seasons_data array to store season summaries
    seasons_data = []
    
    # Process each season
    for season in seasons:
        season_str = str(int(season))
        season_dir = gender_dir / season_str
        os.makedirs(season_dir, exist_ok=True)
        
        # Get all races for this season
        season_df = df[df['Season'] == season]
        races = season_df['Race'].unique()
        
        # Create race summary data for the season
        race_summaries = []
        winner_info = None  # Will be populated when processing Race 0
        
        for race_num in sorted(races):
            race_data = season_df[season_df['Race'] == race_num].copy()  # Use copy to avoid SettingWithCopyWarning

            # Special handling for offseason (Race == 0)
            if race_num == 0:
                # Sort by Elo in descending order and assign new places
                race_data = race_data.sort_values('Elo', ascending=False)
                race_data['Place'] = range(1, len(race_data) + 1)
                
                # Get winner info from the sorted Race 0 data
                winner = race_data.iloc[0]  # First place after sorting
                winner_info = {
                    "name": str(winner['Skier']),
                    "nation": str(winner['Nation'])
                }
                
                # Update previous_elo for each skier
                for _, row in race_data.iterrows():
                    skier_id = row['ID']
                    current_pelo = row['Pelo']
                    # Store current Pelo for next time
                    previous_pelo = last_offseason_pelo.get(skier_id, 1300.0)  # Default to 1300 if no previous
                    last_offseason_pelo[skier_id] = current_pelo
                    # Update row's Pelo and Elo
                    race_data.loc[race_data['ID'] == skier_id, 'Pelo'] = previous_pelo
                    race_data.loc[race_data['ID'] == skier_id, 'Elo'] = current_pelo
            else:
                # For regular races, sort by original Place
                race_data = race_data.sort_values('Place')
            
            if len(race_data) == 0:
                continue
                
            # Get race info from first row
            first_row = race_data.iloc[0]
            
            # Get podium (top 3)
            podium = []
            for i in range(1, 4):
                podium_skier = race_data[race_data['Place'] == i]
                if len(podium_skier) > 0:
                    podium.append({
                        "name": str(podium_skier.iloc[0]['Skier']),
                        "nation": str(podium_skier.iloc[0]['Nation'])
                    })
                else:
                    podium.append(None)
            
            # Create race summary
            summary = {
                "race_number": int(race_num),
                "date": str(first_row['Date']),
                "city": str(first_row['City']),
                "country": str(first_row['Country']),
                "distance": str(first_row['Distance']),
                "racetype": str(first_row['RaceType']),
                "podium": podium
            }
            race_summaries.append(summary)
            
            # Create detailed race results
            race_results = []
            for _, row in race_data.iterrows():
                result = {
                    "place": int(row['Place']),
                    "skier": str(row['Skier']),
                    "nation": str(row['Nation']),
                    "id": int(row['ID']),
                    "previous_elo": float(row['Pelo']),
                    "new_elo": float(row['Elo']),
                    "change": float(row['Elo'] - row['Pelo'])
                }
                race_results.append(result)
            
            # Save race results
            race_file = season_dir / f"race_{race_num}.json"
            with open(race_file, 'w') as f:
                json.dump(race_results, f)
            
            print(f"Created race data: {race_file}")
        
        # Save season race summary
        season_file = season_dir / "summary.json"
        with open(season_file, 'w') as f:
            json.dump(race_summaries, f)
        
        print(f"Created season summary: {season_file}")
        
        # Add this season's data to our seasons array
        seasons_data.append({
            "season": int(season),
            "race_count": len([r for r in race_summaries if r['race_number'] != 0]),  # Don't count Race 0
            "winner": winner_info
        })
    
    # Save seasons data after processing all seasons
    seasons_output = gender_dir / f"seasons.json"
    with open(seasons_output, 'w') as f:
        json.dump(seasons_data, f)
    
    print(f"Created {gender} seasons index at {seasons_output}")

def main():
    # Check if CSV files exist
    ladies_csv = CSV_DIR / "ladies_chrono.csv"
    men_csv = CSV_DIR / "men_chrono.csv"
    
    if not ladies_csv.exists():
        print(f"Error: Ladies CSV file not found at {ladies_csv}")
        sys.exit(1)
    
    if not men_csv.exists():
        print(f"Error: Men's CSV file not found at {men_csv}")
        sys.exit(1)
    
    # Process ladies data
    process_csv('L', ladies_csv)
    
    # Process men's data
    process_csv('M', men_csv)
    
    print("All race data processing complete")

if __name__ == "__main__":
    main()