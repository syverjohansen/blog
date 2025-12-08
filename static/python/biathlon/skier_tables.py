import pandas as pd
import polars as pl
import json
import os
import math
from datetime import datetime

# Load your data
print("Loading data...")
L_chrono = pl.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/ladies_chrono.csv'))
M_chrono = pl.read_csv(os.path.expanduser('~/ski/elo/python/biathlon/polars/excel365/men_chrono.csv'))

# Check for null dates and analyze patterns
def analyze_null_dates(df, label):
    """Analyze patterns in null dates"""
    print(f"\n=== Analyzing null dates in {label} dataset ===")
    
    # Total rows
    total_rows = len(df)
    null_dates = df.filter(pl.col("Date").is_null())
    null_count = len(null_dates)
    
    print(f"Total rows: {total_rows}")
    print(f"Null dates: {null_count} ({null_count/total_rows*100:.2f}%)")
    
    # Check if City == "Summer" is related to null dates
    if "City" in df.columns:
        summer_rows = df.filter(pl.col("City") == "Summer")
        summer_count = len(summer_rows)
        if summer_count > 0:
            summer_null_count = summer_rows.filter(pl.col("Date").is_null()).height
            print(f"Summer rows: {summer_count}")
            print(f"Summer rows with null dates: {summer_null_count} ({summer_null_count/summer_count*100:.2f}%)")
            
            # Check if most null dates are from Summer rows
            if null_count > 0:
                summer_null_pct = summer_null_count / null_count * 100
                print(f"Percentage of null dates that are Summer rows: {summer_null_pct:.2f}%")

# Run analysis
analyze_null_dates(L_chrono, "Ladies")
analyze_null_dates(M_chrono, "Men")

# Function to create fake dates for Summer rows to avoid nulls
def fix_null_dates(df):
    """Create synthetic dates for null values"""
    
    # First, identify rows with null dates
    null_date_rows = df.filter(pl.col("Date").is_null())
    print(f"Found {len(null_date_rows)} rows with null dates")
    
    # For each row with a null date, assign a date based on Season
    df = df.with_columns([
        pl.when(pl.col("Date").is_null())
        .then(
            pl.concat_str([
                pl.col("Season").cast(pl.Utf8),
                pl.lit("-06-15")  # Middle of summer as default
            ])
            .str.strptime(pl.Date, "%Y-%m-%d")
        )
        .otherwise(pl.col("Date"))
        .alias("Date")
    ])
    
    return df

# Apply the fix to both datasets
print("\nFixing null dates in Ladies dataset...")
L_chrono = fix_null_dates(L_chrono)

print("Fixing null dates in Men dataset...")
M_chrono = fix_null_dates(M_chrono)

# Verify the fix worked
null_ladies = L_chrono.filter(pl.col("Date").is_null()).height
null_men = M_chrono.filter(pl.col("Date").is_null()).height

print(f"\nAfter fixes: Ladies null dates: {null_ladies}, Men null dates: {null_men}")

# Function to calculate Elo percentages correctly
def calculate_elo_percentages(df):
    """
    Calculate Elo percentages based on maximum Elo values per race.
    For each race, find the skier with the highest Elo in each category,
    then calculate each skier's percentage relative to that maximum.
    """
    # Elo columns to process for biathlon
    elo_columns = [
        "Elo", "Sprint_Elo", "Pursuit_Elo", "Individual_Elo", "MassStart_Elo"
    ]
    
    # Cast all Elo columns to Float64 at the beginning to handle string columns
    cast_expressions = [pl.col(col).cast(pl.Float64) for col in elo_columns if col in df.columns]
    if cast_expressions:
        df = df.with_columns(cast_expressions)
    
    # Group by Season and Race to find maximum Elo values per race
    grouped_df = df.group_by(["Season", "Race"])
    
    # Process each Elo column to add its percentage column
    for col in elo_columns:
        pct_col = f"{col}_Pct"
        
        # Calculate the maximum Elo for each race
        max_elos = grouped_df.agg(
            pl.col(col).max().alias(f"max_{col}")
        )
        
        # Join the max values back to the original dataframe
        df = df.join(
            max_elos,
            on=["Season", "Race"]
        )
        
        # Calculate percentages
        df = df.with_columns(
            (pl.col(col) / pl.col(f"max_{col}") * 100).alias(pct_col)
        )
        
        # Handle division by zero or null values
        df = df.with_columns(
            pl.when(pl.col(pct_col).is_null() | pl.col(pct_col).is_infinite())
            .then(0)
            .otherwise(pl.col(pct_col))
            .alias(pct_col)
        )
        
        # Drop the temporary max column
        df = df.drop(f"max_{col}")
    
    return df

# Process balanced data with the date fixes
def process_balanced_data():
    # Create output directories
    output_base = os.path.expanduser('~/blog/daehl-e/static/python/biathlon/excel365')
    os.makedirs(os.path.join(output_base, 'L'), exist_ok=True)
    os.makedirs(os.path.join(output_base, 'M'), exist_ok=True)

    # Define a function to determine the group for an ID
    def get_group_key(id_value):
        # Convert to int if it's a string or other type
        if isinstance(id_value, str):
            id_value = int(id_value)
        elif hasattr(id_value, '__iter__') and not isinstance(id_value, str):
            # Handle case where id_value might be a list/array with one element
            id_value = int(id_value[0]) if len(id_value) > 0 else 0
        else:
            id_value = int(id_value)
            
        if id_value < 500:
            return f"0_to_499_{math.floor(id_value / 100) * 100}"
        elif id_value < 1000:
            return f"500_to_999_{math.floor((id_value - 500) / 100) * 100 + 500}"
        else:
            return f"1000_plus_{math.floor(id_value / 500) * 500}"
    
    # Calculate Elo percentages with the corrected function
    print("Calculating Elo percentages for ladies...")
    L_chrono_with_pct = calculate_elo_percentages(L_chrono)
    
    print("Calculating Elo percentages for men...")
    M_chrono_with_pct = calculate_elo_percentages(M_chrono)
    
    # Process ladies data
    print("Processing ladies data...")
    unique_ladies = L_chrono_with_pct.select("ID").unique()
    ladies_groups = {}
    
    # Get count of skiers per ID range to analyze distribution
    id_counts = {}
    
    for id_value in unique_ladies["ID"]:
        group_key = get_group_key(id_value)
        
        if group_key not in ladies_groups:
            ladies_groups[group_key] = {}
            id_counts[group_key] = 0
        
        id_counts[group_key] += 1
        
        # Get skier data
        skier_df = L_chrono_with_pct.filter(pl.col("ID") == id_value)
        skier_df = skier_df.sort(["Date", "Season", "Race"])
        
        # Convert to format suitable for JSON
        skier_data = {}
        for col in skier_df.columns:
            # Handle all columns, converting datetime objects to strings
            skier_data[col] = []
            for value in skier_df[col].to_list():
                if value is None:
                    skier_data[col].append(None)
                elif hasattr(value, 'strftime'):
                    # It's a datetime object
                    skier_data[col].append(value.strftime('%Y-%m-%d'))
                elif hasattr(value, 'isoformat'):
                    # It's a date object
                    skier_data[col].append(value.isoformat())
                else:
                    # Everything else (strings, numbers, etc.)
                    skier_data[col].append(value)

        # Add to group
        ladies_groups[group_key][str(id_value)] = skier_data
    
    # Print distribution analysis
    print("Ladies ID distribution:")
    for group, count in id_counts.items():
        print(f"  {group}: {count} skiers")
    
    # Save ladies groups
    for group_key, group_data in ladies_groups.items():
        group_path = os.path.join(output_base, 'L', f"skiers_{group_key}.json")
        print(f"Saving ladies group {group_key} to {group_path}")
        try:
            with open(group_path, 'w') as f:
                json.dump(group_data, f)
            # Get file size
            file_size = os.path.getsize(group_path) / (1024 * 1024)  # Size in MB
            print(f"  File size: {file_size:.2f} MB")
        except Exception as e:
            print(f"  ERROR saving group {group_key}: {str(e)}")

    # Process men's data
    print("Processing men's data...")
    unique_men = M_chrono_with_pct.select("ID").unique()
    men_groups = {}
    
    # Get count of skiers per ID range to analyze distribution
    id_counts = {}
    
    for id_value in unique_men["ID"]:
        group_key = get_group_key(id_value)
        
        if group_key not in men_groups:
            men_groups[group_key] = {}
            id_counts[group_key] = 0
        
        id_counts[group_key] += 1
        
        # Get skier data
        skier_df = M_chrono_with_pct.filter(pl.col("ID") == id_value)
        skier_df = skier_df.sort(["Date", "Season", "Race"])
        
        # Convert to format suitable for JSON
        skier_data = {}
        for col in skier_df.columns:
            # Handle all columns, converting datetime objects to strings
            skier_data[col] = []
            for value in skier_df[col].to_list():
                if value is None:
                    skier_data[col].append(None)
                elif hasattr(value, 'strftime'):
                    # It's a datetime object
                    skier_data[col].append(value.strftime('%Y-%m-%d'))
                elif hasattr(value, 'isoformat'):
                    # It's a date object
                    skier_data[col].append(value.isoformat())
                else:
                    # Everything else (strings, numbers, etc.)
                    skier_data[col].append(value)

        # Add to group
        men_groups[group_key][str(id_value)] = skier_data
    
    # Print distribution analysis
    print("Men's ID distribution:")
    for group, count in id_counts.items():
        print(f"  {group}: {count} skiers")
    
    # Save men's groups
    for group_key, group_data in men_groups.items():
        group_path = os.path.join(output_base, 'M', f"skiers_{group_key}.json")
        print(f"Saving men's group {group_key} to {group_path}")
        try:
            with open(group_path, 'w') as f:
                json.dump(group_data, f)
            # Get file size
            file_size = os.path.getsize(group_path) / (1024 * 1024)  # Size in MB
            print(f"  File size: {file_size:.2f} MB")
        except Exception as e:
            print(f"  ERROR saving group {group_key}: {str(e)}")

    # Create lookup tables with the new grouping scheme
    print("Creating lookup tables...")
    ladies_lookup = []
    
    # Dictionary to store ID to skier name mappings as we process each skier
    ladies_id_map = {}
    
    for id_value in unique_ladies["ID"]:
        try:
            # Get the most recent row for this skier
            skier_rows = L_chrono_with_pct.filter(pl.col("ID")==id_value).sort("Date", descending=True)
            
            if len(skier_rows) == 0:
                print(f"No data found for ladies ID {id_value}")
                continue
                
            skier_row = skier_rows[0]
            
            # Make sure we extract single values, not Series
            skier_name = skier_row["Skier"]
            if hasattr(skier_name, "__iter__") and not isinstance(skier_name, str):
                skier_name = skier_name[0] if len(skier_name) > 0 else f"Unknown-{id_value}"
                
            nation = skier_row["Nation"]
            if hasattr(nation, "__iter__") and not isinstance(nation, str):
                nation = nation[0] if len(nation) > 0 else "Unknown"
                
            nation_code = safe_get_country_code(nation)
            group_key = get_group_key(id_value)
            
            # Add to lookup table
            ladies_lookup.append({
                "id": int(id_value),
                "skier": skier_name,
                "nation": nation,
                "nation_code": nation_code,
                "group": group_key
            })
            
            # Also add to the ID to name map
            ladies_id_map[str(int(id_value))] = skier_name
            
        except Exception as e:
            print(f"Error processing ladies lookup for ID {id_value}: {str(e)}")

    men_lookup = []
    
    # Dictionary to store ID to skier name mappings as we process each skier
    men_id_map = {}
    
    for id_value in unique_men["ID"]:
        try:
            # Get the most recent row for this skier
            skier_rows = M_chrono_with_pct.filter(pl.col("ID")==id_value).sort("Date", descending=True)
            
            if len(skier_rows) == 0:
                print(f"No data found for men's ID {id_value}")
                continue
                
            skier_row = skier_rows[0]
            
            # Make sure we extract single values, not Series
            skier_name = skier_row["Skier"]
            if hasattr(skier_name, "__iter__") and not isinstance(skier_name, str):
                skier_name = skier_name[0] if len(skier_name) > 0 else f"Unknown-{id_value}"
                
            nation = skier_row["Nation"]
            if hasattr(nation, "__iter__") and not isinstance(nation, str):
                nation = nation[0] if len(nation) > 0 else "Unknown"
                
            nation_code = safe_get_country_code(nation)
            group_key = get_group_key(id_value)
            
            # Add to lookup table
            men_lookup.append({
                "id": int(id_value),
                "skier": skier_name,
                "nation": nation,
                "nation_code": nation_code,
                "group": group_key
            })
            
            # Also add to the ID to name map
            men_id_map[str(int(id_value))] = skier_name
            
        except Exception as e:
            print(f"Error processing men's lookup for ID {id_value}: {str(e)}")

    # Save lookup tables
    with open(os.path.join(output_base, "L_skiers_lookup.json"), 'w') as f:
        json.dump(ladies_lookup, f)

    with open(os.path.join(output_base, "M_skiers_lookup.json"), 'w') as f:
        json.dump(men_lookup, f)

    # Save ID to name maps (we created these as we processed the skiers)
    with open(os.path.join(output_base, "L_all_ids.json"), 'w') as f:
        json.dump(ladies_id_map, f)

    with open(os.path.join(output_base, "M_all_ids.json"), 'w') as f:
        json.dump(men_id_map, f)

    # Create current skier lists
    latest_ladies_season = L_chrono_with_pct["Season"].max()
    latest_men_season = M_chrono_with_pct["Season"].max()

    current_ladies = L_chrono_with_pct.filter(pl.col("Season") == latest_ladies_season).select("ID").unique()
    current_men = M_chrono_with_pct.filter(pl.col("Season") == latest_men_season).select("ID").unique()

    # Create current ID maps using only IDs that exist in the main map
    current_ladies_map = {}
    for id_value in current_ladies["ID"]:
        id_str = str(int(id_value))
        if id_str in ladies_id_map:
            current_ladies_map[id_str] = ladies_id_map[id_str]
        else:
            print(f"Warning: Ladies ID {id_str} is in current season but not in ID map")
    
    current_men_map = {}
    for id_value in current_men["ID"]:
        id_str = str(int(id_value))
        if id_str in men_id_map:
            current_men_map[id_str] = men_id_map[id_str]
        else:
            print(f"Warning: Men's ID {id_str} is in current season but not in ID map")

    with open(os.path.join(output_base, "L_current_ids.json"), 'w') as f:
        json.dump(current_ladies_map, f)

    with open(os.path.join(output_base, "M_current_ids.json"), 'w') as f:
        json.dump(current_men_map, f)

    print("All biathlon files have been processed and grouped successfully.")

# Complete country code mapping for biathlon
def safe_get_country_code(nation):
    codes = {
        "Albania": "alb", "Algeria": "alg", "Andorra": "and", "Argentina": "arg", "Armenia": "arm", 
        "Australia": "aus", "Austria": "aut", "Azerbaijan": "aze", "Belarus": "blr", "Belgium": "bel", 
        "Bermuda": "ber", "Bolivia": "bol", "Bosnia&Herzegovina": "bih", "Brazil": "bra", "Bulgaria": "bul",
        "Cameroon": "cmr", "Canada": "can", "Cayman Islands": "cay", "Chile": "chi", "China": "chn", 
        "Colombia": "col", "Costa Rica": "crc", "Croatia": "cro", "Cyprus": "cyp", "Czechia": "cze", 
        "Denmark": "den", "East Timor": "tls", "Ecuador": "ecu", "Egypt": "egy", "Eritrea": "eri", 
        "Estonia": "est", "Eswatini": "swz", "Ethiopia": "eth", "Fiji": "fij", "Finland": "fin", 
        "FIS": "fis", "France": "fra", "Georgia": "geo", "Germany": "ger", "Ghana": "gha", 
        "Great Britain": "gbr", "Greece": "gre", "Greenland": "grl", "Grenada": "grn", "Guam": "gum", 
        "Guatemala": "gua", "Haiti": "hai", "Honduras": "hon", "Hong Kong": "hkg", "Hungary": "hun", 
        "Iceland": "isl", "India": "ind", "Iran": "iri", "Ireland": "irl", "Israel": "isr", 
        "Italy": "ita", "Jamaica": "jam", "Japan": "jpn", "Kazakhstan": "kaz", "Kenya": "ken", 
        "Kosovo": "kos", "Kuwait": "kuw", "Kyrgyzstan": "kgz", "Latvia": "lat", "Lebanon": "lbn", 
        "Liechtenstein": "lie", "Lithuania": "ltu", "Luxembourg": "lux", "Madagascar": "mad", 
        "Malaysia": "mas", "Malta": "mlt", "Mexico": "mex", "Moldova": "mda", "Monaco": "mon", 
        "Mongolia": "mgl", "Montenegro": "mne", "Morocco": "mar", "Nepal": "nep", "Netherlands": "ned", 
        "New Zealand": "nzl", "Nigeria": "ngr", "North Korea": "prk", "North Macedonia": "mkd", 
        "Norway": "nor", "Pakistan": "pak", "Peru": "per", "Philippines": "phi", "Poland": "pol", 
        "Portugal": "por", "Puerto Rico": "pur", "Romania": "rou", "Russia": "rus", "San Marino": "smr", 
        "Saudi Arabia": "ksa", "Senegal": "sen", "Serbia": "srb", "Singapore": "sgp", "Slovakia": "svk", 
        "Slovenia": "slo", "South Africa": "rsa", "South Korea": "kor", "Soviet": "urs", "Spain": "esp", 
        "Sweden": "swe", "Switzerland": "sui", "Taiwan": "tpe", "Tajikistan": "tjk", "Thailand": "tha", 
        "Togo": "tog", "Tonga": "tga", "Trinidad & Tobago": "tto", "Turkey": "tur", "Ukraine": "ukr", 
        "Uruguay": "uru", "US Virgin Islands": "isv", "USA": "usa", "Uzbekistan": "uzb", 
        "Venezuela": "ven", "Yugoslavia": "yug", "Zimbabwe": "zim"
    }
    
    # Handle Series objects
    if hasattr(nation, "__iter__") and not isinstance(nation, str):
        if len(nation) > 0:
            nation = nation[0]
        else:
            return "unknown"
    
    return codes.get(nation, "unknown")

# Run the balanced processing function
if __name__ == "__main__":
    process_balanced_data()