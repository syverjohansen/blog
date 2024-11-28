import pandas as pd
import json
import sys
import os
import numpy as np

def excel_sheets_to_json(excel_file, output_dir):
    """
    Convert all worksheets in an Excel file to separate JSON files,
    cleaning data to handle NA%, NaN, and other special values.
    
    Args:
        excel_file (str): Path to the Excel file
        output_dir (str): Directory where JSON files will be saved
    """
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Read all sheets from Excel file
    excel = pd.ExcelFile(excel_file)
    
    # Function to clean percentage strings
    def clean_percentage(val):
        if pd.isna(val) or val == "NA%":
            return "0.0%"
        return str(val)

    # Function to clean numeric values
    def clean_numeric(val):
        if pd.isna(val) or val == np.inf or val == -np.inf:
            return 0.0
        return val

    # Convert numpy types to native Python types
    def convert_to_native(obj):
        if isinstance(obj, np.integer):
            return int(obj)
        elif isinstance(obj, np.floating):
            return float(obj)
        elif isinstance(obj, np.ndarray):
            return obj.tolist()
        return obj
    
    # Process each worksheet
    for sheet_name in excel.sheet_names:
        # Read the worksheet
        df = pd.read_excel(excel_file, sheet_name=sheet_name)
        
        # Clean percentage columns (those ending with _Prob)
        percentage_cols = [col for col in df.columns if str(col).endswith('_Prob')]
        for col in percentage_cols:
            df[col] = df[col].apply(clean_percentage)

        # Clean numeric columns (those ending with _Decimal or _American)
        numeric_cols = [col for col in df.columns if col.endswith(('_Decimal', '_American'))]
        for col in numeric_cols:
            df[col] = df[col].apply(clean_numeric)
        
        # Create the data structure
        data = {
            "sheet_name": sheet_name,
            "headers": df.columns.tolist(),
            "rows": [[convert_to_native(cell) for cell in row] for row in df.values.tolist()]
        }
        
        # Create output filename based on sheet name
        # Replace spaces and special characters with underscores
        safe_sheet_name = "".join(c if c.isalnum() else "_" for c in sheet_name)
        output_file = os.path.join(output_dir, f"{safe_sheet_name}.json")
        
        # Write to JSON file
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2, ensure_ascii=False)
        
        print(f"Created JSON file for worksheet '{sheet_name}': {output_file}")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python excel_to_json.py input.xlsx output_directory")
        sys.exit(1)
    
    excel_sheets_to_json(sys.argv[1], sys.argv[2])