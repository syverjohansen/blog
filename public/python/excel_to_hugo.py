import pandas as pd
import json
import sys

def excel_to_hugo_json(excel_file, output_file):
    # Read Excel file
    df = pd.read_excel(excel_file)
    
    # Create the data structure
    data = {
        "headers": df.columns.tolist(),
        "rows": df.values.tolist()
    }
    
    # Write to JSON file
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=2, ensure_ascii=False)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python excel_to_hugo.py input.xlsx output.json")
        sys.exit(1)
        
    excel_to_hugo_json(sys.argv[1], sys.argv[2])