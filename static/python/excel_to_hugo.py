import pandas as pd
import json
import sys
import numpy as np

def excel_to_hugo_json(excel_file, output_file):
    df = pd.read_excel(excel_file)
    
    def convert_value(val):
        if pd.isna(val):
            return ""
        elif isinstance(val, np.integer):
            return int(val)
        elif isinstance(val, np.floating):
            return float(val)
        elif isinstance(val, np.ndarray):
            return val.tolist()
        return str(val)
    
    data = {
        "headers": df.columns.tolist(),
        "rows": [[convert_value(cell) for cell in row] for row in df.values.tolist()]
    }
    
    with open(output_file, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=2, ensure_ascii=False)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python excel_to_hugo.py input.xlsx output.json")
        sys.exit(1)
        
    excel_to_hugo_json(sys.argv[1], sys.argv[2])