import pandas as pd
import json
import os
import numpy as np

# Read the CSV files for Biathlon
men_df = pd.read_csv('/Users/syverjohansen/ski/elo/python/biathlon/polars/excel365/men_chrono.csv')
ladies_df = pd.read_csv('/Users/syverjohansen/ski/elo/python/biathlon/polars/excel365/ladies_chrono.csv')

def clean_value(value):
    """Clean a value for JSON serialization"""
    if pd.isna(value) or value is None or value == 'NaN':
        return None
    if isinstance(value, (np.integer, int)):
        return int(value)
    if isinstance(value, (np.floating, float)):
        if np.isnan(value):
            return None
        return float(value)
    return str(value)

def process_biathlon_head2head_data(df, gender):
    """Process chronological data for biathlon head-to-head comparisons"""
    
    print(f"Processing Biathlon {gender} data...")
    print(f"Original data shape: {df.shape}")
    
    # Filter out Race 0 (offseason)
    df_filtered = df[df['Race'] != 0].copy()
    print(f"After filtering Race != 0: {df_filtered.shape}")
    
    # Ensure all necessary columns exist and have proper data types
    required_columns = ['ID', 'Skier', 'Season', 'Race', 'Date', 'City', 'Country', 
                       'RaceType', 'Place', 'Elo']
    
    # Check if all required columns exist
    missing_columns = [col for col in required_columns if col not in df_filtered.columns]
    if missing_columns:
        print(f"Warning: Missing columns for {gender}: {missing_columns}")
        print(f"Available columns: {list(df_filtered.columns)}")
        return {}
    
    # Convert data types and handle NaN values
    df_filtered['ID'] = df_filtered['ID'].astype(str)
    df_filtered['Season'] = df_filtered['Season'].astype(str)
    df_filtered['Race'] = df_filtered['Race'].astype(str)
    df_filtered['Place'] = pd.to_numeric(df_filtered['Place'], errors='coerce')
    df_filtered['Elo'] = pd.to_numeric(df_filtered['Elo'], errors='coerce')
    
    # Handle NaN values in string columns
    df_filtered['City'] = df_filtered['City'].fillna('Unknown')
    df_filtered['Country'] = df_filtered['Country'].fillna('Unknown')
    df_filtered['RaceType'] = df_filtered['RaceType'].fillna('Unknown')
    
    # Sort by date to ensure chronological order
    df_filtered['Date'] = pd.to_datetime(df_filtered['Date'], errors='coerce')
    df_filtered = df_filtered.sort_values(['Date', 'Season', 'Race'])
    
    # Convert back to string for JSON serialization
    df_filtered['Date'] = df_filtered['Date'].dt.strftime('%Y-%m-%d')
    df_filtered['Date'] = df_filtered['Date'].fillna('Unknown')
    
    # Group by skier ID and create structured data
    skiers_data = {}
    
    print(f"Processing {df_filtered['ID'].nunique()} unique skiers...")
    
    for skier_id, skier_group in df_filtered.groupby('ID'):
        skier_races = []
        
        for _, race in skier_group.iterrows():
            race_data = {
                'season': clean_value(race['Season']),
                'race': clean_value(race['Race']),
                'date': clean_value(race['Date']),
                'city': clean_value(race['City']),
                'country': clean_value(race['Country']),
                'event': clean_value(race['RaceType']),  # In biathlon, Distance column contains events
                'place': clean_value(race['Place']),
                'elo': clean_value(race['Elo']),
                'skier_name': clean_value(race['Skier'])
            }
            skier_races.append(race_data)
            
        skiers_data[str(skier_id)] = skier_races
        
        # Debug: print info for first few skiers
        if len(skiers_data) <= 5:
            print(f"Skier {skier_id} ({clean_value(race['Skier'])}): {len(skier_races)} races")
    
    print(f"Total skiers processed: {len(skiers_data)}")
    return skiers_data

def create_biathlon_race_lookup(df, gender):
    """Create a lookup table for biathlon race information"""
    df_filtered = df[df['Race'] != 0].copy()
    
    # Handle NaN values
    df_filtered['City'] = df_filtered['City'].fillna('Unknown')
    df_filtered['Country'] = df_filtered['Country'].fillna('Unknown')
    df_filtered['RaceType'] = df_filtered['RaceType'].fillna('Unknown')
    
    # Get unique races
    races = df_filtered[['Season', 'Race', 'Date', 'City', 'Country', 'RaceType']].drop_duplicates()
    races['Date'] = pd.to_datetime(races['Date'], errors='coerce')
    races = races.sort_values(['Date', 'Season', 'Race'])
    races['Date'] = races['Date'].dt.strftime('%Y-%m-%d')
    races['Date'] = races['Date'].fillna('Unknown')
    
    race_lookup = {}
    for _, race in races.iterrows():
        key = f"{race['Season']}_{race['Race']}"
        race_lookup[key] = {
            'season': clean_value(race['Season']),
            'race': clean_value(race['Race']),
            'date': clean_value(race['Date']),
            'city': clean_value(race['City']),
            'country': clean_value(race['Country']),
            'event': clean_value(race['RaceType'])  # Event instead of distance for biathlon
        }
    
    return race_lookup

# Process both datasets
print("Starting Biathlon data processing...")

men_data = process_biathlon_head2head_data(men_df, 'M')
men_races = create_biathlon_race_lookup(men_df, 'M')

ladies_data = process_biathlon_head2head_data(ladies_df, 'L')
ladies_races = create_biathlon_race_lookup(ladies_df, 'L')

# Create output directory if it doesn't exist
output_dir = '/Users/syverjohansen/blog/daehl-e/static/python/biathlon/excel365'
head2head_dir = f"{output_dir}/head2head"
os.makedirs(head2head_dir, exist_ok=True)

# Custom JSON encoder to handle any remaining NaN values
class NaNEncoder(json.JSONEncoder):
    def encode(self, obj):
        # Replace any NaN values that might have slipped through
        if isinstance(obj, float) and np.isnan(obj):
            return 'null'
        return super().encode(obj)
    
    def iterencode(self, obj, _one_shot=False):
        # Handle NaN in nested structures
        if isinstance(obj, dict):
            obj = {k: None if (isinstance(v, float) and np.isnan(v)) else v for k, v in obj.items()}
        elif isinstance(obj, list):
            obj = [None if (isinstance(item, float) and np.isnan(item)) else item for item in obj]
        return super().iterencode(obj, _one_shot)

# Save the processed data with NaN handling
if men_data:
    print(f"Saving men's data: {len(men_data)} skiers")
    with open(f"{head2head_dir}/M_head2head_data.json", 'w') as f:
        json.dump(men_data, f, indent=2, cls=NaNEncoder)
    
    with open(f"{head2head_dir}/M_race_lookup.json", 'w') as f:
        json.dump(men_races, f, indent=2, cls=NaNEncoder)
        
    # Print sample data for verification
    sample_skier_id = list(men_data.keys())[0] if men_data else None
    if sample_skier_id and men_data[sample_skier_id]:
        print(f"Sample men's data for skier {sample_skier_id}:")
        print(f"  Number of races: {len(men_data[sample_skier_id])}")
        print(f"  First race: {men_data[sample_skier_id][0]}")

if ladies_data:
    print(f"Saving ladies' data: {len(ladies_data)} skiers")
    with open(f"{head2head_dir}/L_head2head_data.json", 'w') as f:
        json.dump(ladies_data, f, indent=2, cls=NaNEncoder)
    
    with open(f"{head2head_dir}/L_race_lookup.json", 'w') as f:
        json.dump(ladies_races, f, indent=2, cls=NaNEncoder)
        
    # Print sample data for verification
    sample_skier_id = list(ladies_data.keys())[0] if ladies_data else None
    if sample_skier_id and ladies_data[sample_skier_id]:
        print(f"Sample ladies' data for skier {sample_skier_id}:")
        print(f"  Number of races: {len(ladies_data[sample_skier_id])}")
        print(f"  First race: {ladies_data[sample_skier_id][0]}")

print("Biathlon head-to-head data processing complete!")
print(f"Files saved to: {head2head_dir}/")