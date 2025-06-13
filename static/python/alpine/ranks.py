import pandas as pd
import numpy as np
import os
from pathlib import Path
import logging

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

def calculate_points(row):
    """Calculate points based on event type and placement"""
    event = row['Event']
    place = row['Place']
    
    # Only points for top 3 places
    if place > 3:
        return 0
        
    # Handle Standings separately
    if event == "Standings":
        points = {1: 80, 2: 40, 3: 20}
        return points.get(place, 0)
    
    # Define points structure - only individual events in alpine
    points_structure = {
        'Olympic Winter Games': {
            'individual': {1: 80, 2: 40, 3: 20}
        },
        'WSC': {
            'individual': {1: 40, 2: 20, 3: 10}
        },
        'World Cup': {
            'individual': {1: 8, 2: 4, 3: 4}
        }
    }
    
    # All alpine events are individual
    race_type = 'individual'
    
    # Determine event type
    if event == "Olympic Winter Games":
        event_type = 'Olympic Winter Games'
    elif event == "WSC":
        event_type = 'WSC'
    else:
        # Everything else that's not Standings is World Cup
        event_type = 'World Cup'
    
    return points_structure[event_type][race_type].get(place, 0)

def process_rankings(gender='M'):
    """Process rankings for given gender"""
    try:
        # Read scraped data
        base_path = os.path.expanduser("~/ski/ranks/alpine/excel365")
        input_file = Path(base_path) / f"{'men' if gender == 'M' else 'ladies'}_scrape_update.csv"
        
        df = pd.read_csv(input_file)
        logging.info(f"Read {len(df)} records for {gender}")
        
        # Convert date columns to datetime
        df['Date'] = pd.to_datetime(df['Date'])
        df['Birthday'] = pd.to_datetime(df['Birthday'])
        
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
                    'WSC': float(skier_data[skier_data['Event'] == 'WSC']['Points'].sum()),
                    'WC': float(skier_data[
                        (skier_data['Event'] != 'Olympic Winter Games') & 
                        (skier_data['Event'] != 'WSC') & 
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
        output_dir = Path(os.path.expanduser("~/blog/daehl-e/static/python/alpine/excel365")) / gender
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