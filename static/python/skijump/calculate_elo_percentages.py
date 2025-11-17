import os
import json

# Base directory of your JSON files
output_base = os.path.expanduser('~/blog/daehl-e/static/python/cross-country/excel365')

# Function to calculate Elo percentages for a single group file
def calculate_percentages_for_group(gender, group_key):
    group_path = os.path.join(output_base, gender, f"skiers_{group_key}.json")
    
    try:
        print(f"Processing {gender}/skiers_{group_key}.json...")
        
        # Load the group data
        with open(group_path, 'r') as f:
            group_data = json.load(f)
        
        # For each skier in the group
        for skier_id, skier_data in group_data.items():
            # Get the Elo columns
            elo_columns = [
                "Elo", "Distance_Elo", "Distance_C_Elo", "Distance_F_Elo", 
                "Sprint_Elo", "Sprint_C_Elo", "Sprint_F_Elo", 
                "Classic_Elo", "Freestyle_Elo"
            ]
            
            # For each Elo column, calculate the percentage
            for col in elo_columns:
                if col in skier_data:
                    # Create new column name
                    pct_col = f"{col}_Pct"
                    
                    # Get values as float
                    values = [float(v) if v is not None else 0 for v in skier_data[col]]
                    
                    # Find max value
                    max_val = max(values) if values else 0
                    
                    # Calculate percentages
                    if max_val > 0:
                        percentages = [round((v / max_val) * 100, 2) if v > 0 else 0 for v in values]
                    else:
                        percentages = [0] * len(values)
                    
                    # Add to skier data
                    skier_data[pct_col] = percentages
        
        # Save updated group data
        with open(group_path, 'w') as f:
            json.dump(group_data, f)
        
        print(f"Updated {gender}/skiers_{group_key}.json with percentages")
        return True
        
    except Exception as e:
        print(f"Error processing {gender}/skiers_{group_key}.json: {str(e)}")
        return False

# Process all group files for both genders
def process_all_groups():
    success_count = 0
    error_count = 0
    
    # Get all group files for ladies
    try:
        l_files = [f for f in os.listdir(os.path.join(output_base, 'L')) if f.startswith('skiers_') and f.endswith('.json')]
        
        for filename in l_files:
            group_key = filename.replace('skiers_', '').replace('.json', '')
            if calculate_percentages_for_group('L', group_key):
                success_count += 1
            else:
                error_count += 1
    except Exception as e:
        print(f"Error processing ladies groups: {str(e)}")
        error_count += 1
    
    # Get all group files for men
    try:
        m_files = [f for f in os.listdir(os.path.join(output_base, 'M')) if f.startswith('skiers_') and f.endswith('.json')]
        
        for filename in m_files:
            group_key = filename.replace('skiers_', '').replace('.json', '')
            if calculate_percentages_for_group('M', group_key):
                success_count += 1
            else:
                error_count += 1
    except Exception as e:
        print(f"Error processing men's groups: {str(e)}")
        error_count += 1
    
    print(f"\nPercentage calculation completed!")
    print(f"Successfully processed: {success_count} files")
    print(f"Errors encountered: {error_count} files")
    
    if error_count == 0:
        print("All files processed successfully!")
    else:
        print("Some errors occurred. Check the logs above for details.")

# Run the script
if __name__ == "__main__":
    process_all_groups()