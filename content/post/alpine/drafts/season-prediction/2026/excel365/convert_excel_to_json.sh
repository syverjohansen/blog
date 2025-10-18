#!/bin/bash

# Script to convert all Excel files in current directory to JSON for Hugo
# Run this from the excel365 directory

echo "Converting Excel files to JSON for Hugo..."

# Create the output directory if it doesn't exist
mkdir -p /Users/syverjohansen/blog/daehl-e/data/alpine/season-prediction/2026

# Convert all Excel files in current directory
for file in *.xlsx; do 
    if [ -f "$file" ]; then
        echo "Converting $file..."
        python /Users/syverjohansen/blog/daehl-e/static/python/excel_to_hugo_multiple_sheets.py "$file" /Users/syverjohansen/blog/daehl-e/data/alpine/season-prediction/2026
        
        if [ $? -eq 0 ]; then
            echo "✓ Successfully converted $file"
        else
            echo "✗ Error converting $file"
        fi
    fi
done

echo "Conversion complete!"
echo "JSON files saved to: /Users/syverjohansen/blog/daehl-e/data/alpine/season-prediction/2026"