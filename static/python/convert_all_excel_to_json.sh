#!/bin/bash

# Script to recursively convert all Excel files to JSON for Hugo
# Usage: ./convert_all_excel_to_json.sh <input_directory> <output_directory>

if [ $# -ne 2 ]; then
    echo "Usage: $0 <input_directory> <output_directory>"
    echo "Example: $0 ~/blog/daehl-e/content/post/alpine/drafts/season-prediction/2026 ~/blog/daehl-e/data/alpine/season-prediction/2026"
    exit 1
fi

INPUT_DIR="$1"
OUTPUT_DIR="$2"

# Expand tilde if present
INPUT_DIR="${INPUT_DIR/#\~/$HOME}"
OUTPUT_DIR="${OUTPUT_DIR/#\~/$HOME}"

echo "Converting Excel files to JSON for Hugo..."
echo "Input directory: $INPUT_DIR"
echo "Output directory: $OUTPUT_DIR"

# Check if input directory exists
if [ ! -d "$INPUT_DIR" ]; then
    echo "Error: Input directory '$INPUT_DIR' does not exist"
    exit 1
fi

# Create the output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

if [ $? -ne 0 ]; then
    echo "Error: Could not create output directory '$OUTPUT_DIR'"
    exit 1
fi

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Count total Excel files first
total_files=$(find "$INPUT_DIR" -name "*.xlsx" -type f | wc -l)
echo "Found $total_files Excel files to convert..."

if [ "$total_files" -eq 0 ]; then
    echo "No Excel files found in '$INPUT_DIR'"
    exit 0
fi

# Initialize counters
converted=0
errors=0

# Find and convert all Excel files recursively
find "$INPUT_DIR" -name "*.xlsx" -type f | while read -r file; do
    # Get relative path from input directory
    rel_path="${file#$INPUT_DIR/}"
    
    echo "Converting: $rel_path"
    
    # Convert the file - all files go to the main output directory
    python "$SCRIPT_DIR/excel_to_hugo_multiple_sheets.py" "$file" "$OUTPUT_DIR"
    
    if [ $? -eq 0 ]; then
        echo "✓ Successfully converted $rel_path"
        ((converted++))
    else
        echo "✗ Error converting $rel_path"
        ((errors++))
    fi
done

echo ""
echo "Conversion complete!"
echo "Successfully converted: $converted files"
echo "Errors: $errors files"
echo "JSON files saved to: $OUTPUT_DIR"

# List the created JSON files
echo ""
echo "Created JSON files:"
find "$OUTPUT_DIR" -name "*.json" -type f | sort