import pandas as pd
import numpy as np
import os
from pathlib import Path
import logging
from datetime import datetime, timezone

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

def calculate_points(row):
    """Calculate points based on event type and placement"""
    event = row['Event']
    distance = row['RaceType']
    place = row['Place']
    
    # Only points for top 3 places
    if place > 3:
        return 0
        
    # Handle Standings separately
    if event == "Standings":
        points = {1: 80, 2: 40, 3: 20}
        return points.get(place, 0)
    
    # Define points structure
    points_structure = {
        'Olympic Winter Games': {
            'individual': {1: 80, 2: 40, 3: 20},
            'relay': {1: 20, 2: 10, 3: 5},
            'single_mixed_relay': {1: 40, 2: 20, 3: 10}
        },
        'World Championship': {
            'individual': {1: 40, 2: 20, 3: 10},
            'relay': {1: 10, 2: 5, 3: 2.5},
            'single_mixed_relay': {1: 20, 2: 10, 3: 5}
        },
        'World Cup': {
            'individual': {1: 8, 2: 4, 3: 4},
            'relay': {1: 2, 2: 1, 3: 1},
            'single_mixed_relay': {1: 4, 2: 2, 3: 2}
        }
    }
    
    # Determine race type
    if distance in ["Relay", "Mixed Relay"]:
        race_type = 'relay'
    elif distance == "Single Mixed Relay":
        race_type = 'single_mixed_relay'
    else:
        race_type = 'individual'
    
    # Determine event type
    if event == "Olympic Winter Games":
        event_type = 'Olympic Winter Games'
    elif event == "World Championship":
        event_type = 'World Championship'
    else:
        # Everything else that's not Standings is World Cup
        event_type = 'World Cup'
    
    return points_structure[event_type][race_type].get(place, 0)

def process_rankings(gender='M'):
    """Process rankings for given gender"""
    try:
        # Read scraped data
        base_path = os.path.expanduser("~/ski/ranks/biathlon/excel365")
        input_file = Path(base_path) / f"{'men' if gender == 'M' else 'ladies'}_scrape_update.csv"
        
        df = pd.read_csv(input_file)
        logging.info(f"Read {len(df)} records for {gender}")
        
        # Convert date columns to datetime
        df['Date'] = pd.to_datetime(df['Date'])
        df['Birthday'] = pd.to_datetime(df['Birthday'])
        
        # Check if we should exclude current season standings
        races_file = os.path.expanduser("~/ski/elo/python/biathlon/polars/excel365/races.csv")
        exclude_current_standings = False
        
        if os.path.exists(races_file):
            races_df = pd.read_csv(races_file)
            races_df['Date'] = pd.to_datetime(races_df['Date'])
            
            current_date = datetime.now(timezone.utc).date()
            min_race_date = races_df['Date'].min().date()
            max_race_date = races_df['Date'].max().date()
            
            # If current date is within race season, exclude current season standings
            if min_race_date <= current_date <= max_race_date:
                exclude_current_standings = True
                max_season = df['Season'].max()
                logging.info(f"Excluding current season ({max_season}) standings - season in progress")
        
        # Filter out current season standings if needed
        if exclude_current_standings:
            max_season = df['Season'].max()
            df = df[~((df['Event'] == 'Standings') & (df['Season'] == max_season))]
            logging.info(f"Filtered out {max_season} season standings")
        
        # Calculate points for each row
        df['Points'] = df.apply(calculate_points, axis=1)
        
        # Group by skier and calculate totals
        results = []
        for skier_id, skier_data in df.groupby('ID'):
            total_points = skier_data['Points'].sum()
            if total_points > 0:  # Only include skiers with points
                latest_record = skier_data.iloc[-1]
                
                # Calculate From/To values
                seasons = skier_data['Season'].unique()
                from_season = int(min(seasons))
                to_season = int(max(seasons))
                
                # Calculate current age (today's date - birthday)
                today = pd.Timestamp.now()
                birthday = pd.to_datetime(latest_record['Birthday'])
                if pd.notna(birthday):
                    age = int((today - birthday).days / 365.25)  # Convert to years and round down
                else:
                    age = None  # Set to None if birthday is missing
                
                result = {
                    'Skier': latest_record['Skier'],
                    'Nation': latest_record['Nation'],
                    'ID': latest_record['ID'],
                    'Olympics': float(skier_data[skier_data['Event'] == 'Olympic Winter Games']['Points'].sum()),
                    'WSC': float(skier_data[skier_data['Event'] == 'World Championship']['Points'].sum()),
                    'WC': float(skier_data[
                        (skier_data['Event'] != 'Olympic Winter Games') & 
                        (skier_data['Event'] != 'World Championship') & 
                        (skier_data['Event'] != 'Standings')
                    ]['Points'].sum()),
                    'Table': float(skier_data[skier_data['Event'] == 'Standings']['Points'].sum()),
                    'Total': float(total_points),
                    'From': from_season,
                    'To': to_season,
                    'Age': age
                }
                results.append(result)
                
        # Create final DataFrame and sort by total points
        results_df = pd.DataFrame(results)
        results_df = results_df.sort_values('Total', ascending=False).reset_index(drop=True)
        
        # Save results
        output_dir = Path(os.path.expanduser("~/blog/daehl-e/static/python/biathlon/excel365")) / gender
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Save as both CSV and JSON for flexibility
        csv_file = output_dir / "ranks.csv"
        json_file = output_dir / "ranks.json"
        
        results_df.to_csv(csv_file, index=False)
        results_df.to_json(json_file, orient='records')
        
        logging.info(f"Processed {len(results_df)} skiers with points for {gender}")
        logging.info(f"Saved results to {output_dir}")
        
    except Exception as e:
        logging.error(f"Error processing {gender} rankings: {e}")
        raise

def main():
    """Main execution function"""
    try:
        # Process both genders
        process_rankings('M')
        process_rankings('L')
        
    except Exception as e:
        logging.error(f"Error in main execution: {e}")
        raise

if __name__ == "__main__":
    main()