import pandas as pd
import json
import sys
import os
import numpy as np

def excel_sheets_to_json(excel_file, output_dir):
    os.makedirs(output_dir, exist_ok=True)
    excel = pd.ExcelFile(excel_file)
    
    def clean_percentage(val):
        if pd.isna(val) or val == "NA%":
            return "0.0%"
        return str(val)

    def clean_numeric(val):
        if pd.isna(val) or val == np.inf or val == -np.inf:
            return 0.0
        return val
    
    def clean_string(val):
        if pd.isna(val):
            return ""
        return str(val)

    def convert_to_native(obj):
        if isinstance(obj, np.integer):
            return int(obj)
        elif isinstance(obj, np.floating):
            return float(obj)
        elif isinstance(obj, np.ndarray):
            return obj.tolist()
        elif pd.isna(obj):
            return ""
        return obj
    
    for sheet_name in excel.sheet_names:
        df = pd.read_excel(excel_file, sheet_name=sheet_name)
        
        # Clean all string columns
        string_cols = df.select_dtypes(include=['object']).columns
        for col in string_cols:
            df[col] = df[col].apply(clean_string)
            
        percentage_cols = [col for col in df.columns if str(col).endswith('_Prob')]
        for col in percentage_cols:
            df[col] = df[col].apply(clean_percentage)

        numeric_cols = [col for col in df.columns if col.endswith(('_Decimal', '_American'))]
        for col in numeric_cols:
            df[col] = df[col].apply(clean_numeric)
        
        data = {
            "sheet_name": sheet_name,
            "headers": df.columns.tolist(),
            "rows": [[convert_to_native(cell) for cell in row] for row in df.values.tolist()]
        }
        
        safe_sheet_name = "".join(c if c.isalnum() else "_" for c in sheet_name)
        output_file = os.path.join(output_dir, f"{safe_sheet_name}.json")
        
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2, ensure_ascii=False)
        
        print(f"Created JSON file for worksheet '{sheet_name}': {output_file}")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python excel_to_json.py input.xlsx output_directory")
        sys.exit(1)
    
    excel_sheets_to_json(sys.argv[1], sys.argv[2])