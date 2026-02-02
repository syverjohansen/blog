#!/bin/bash

# Set variables - explicitly use GMT/UTC time
TODAY=$(date -u +%Y%m%d)  # -u flag ensures UTC/GMT time
YEAR=$(date -u +%Y)
MONTH=$(date -u +%m)
DAY=$(date -u +%d)
FORMATTED_DATE="${YEAR}-${MONTH}-${DAY}"

# File path for races.csv
RACES_CSV="~/ski/elo/python/ski/polars/excel365/races.csv"

# Date 7 days ago in MM/DD/YYYY format for comparison
SEVEN_DAYS_AGO=$(date -u -d "7 days ago" +%m/%d/%Y)

BLOG_DIR=~/blog/daehl-e
R_SCRIPT="${BLOG_DIR}/content/post/cross-country/drafts/weekly-recap/race-recap2.R"
PYTHON_SCRIPT="${BLOG_DIR}/static/python/excel_to_hugo.py"
OUTPUT_DIR="${BLOG_DIR}/content/post/cross-country/drafts/weekly-recap/${TODAY}"
POST_DIR="${BLOG_DIR}/content/post/weekly-recap"
DATA_DIR="${BLOG_DIR}/data/cross-country/weekly-recap/${TODAY}"

echo "====== Weekly Recap Generator ======"
echo "Date: ${FORMATTED_DATE}"
echo "Processing for date: ${TODAY}"

# Check if there were any races in the last 7 days
echo "Checking for races in the last 7 days..."
RECENT_RACES=$(awk -F, -v date="$SEVEN_DAYS_AGO" 'BEGIN {count=0} 
{
  split($0, fields, ",");
  race_date = fields[1];
  # Compare dates - this compares MM/DD/YYYY format strings
  if (race_date >= date) {
    count++;
  }
}
END {print count}' "$RACES_CSV")

# Expand the tilde in the path
EXPANDED_CSV="${RACES_CSV/#\~/$HOME}"

# Check if there were any races in the last 7 days
echo "Checking for races in the last 7 days..."
if [ -f "$EXPANDED_CSV" ]; then
  # Use awk to check for dates in the last 7 days
  RECENT_RACES=$(awk -F, -v date="$SEVEN_DAYS_AGO" '
  BEGIN {count=0} 
  NR>1 {  # Skip header row
    if ($1 >= date) {
      count++;
    }
  }
  END {print count}' "$EXPANDED_CSV")
  
  echo "Found $RECENT_RACES races in the last 7 days."
  
  if [ "$RECENT_RACES" -eq 0 ]; then
    echo "No races found in the last 7 days. Exiting."
    exit 0
  fi
else
  echo "Error: Races CSV file not found at $EXPANDED_CSV"
  exit 1
fi

# Create necessary directories
echo "Creating directories..."
mkdir -p "${OUTPUT_DIR}"
mkdir -p "${POST_DIR}"
mkdir -p "${DATA_DIR}"

# Run R script
echo "Running R script..."
Rscript "${R_SCRIPT}"

# Check if R script executed successfully
if [ $? -ne 0 ]; then
  echo "Error: R script execution failed!"
  exit 1
fi

# Process all Excel files and convert to JSON
echo "Converting Excel files to JSON..."
if [ -d "${OUTPUT_DIR}" ]; then
  for excel_file in "${OUTPUT_DIR}"/*.xlsx; do
    if [ -f "$excel_file" ]; then
      filename=$(basename "$excel_file" .xlsx)
      json_file="${DATA_DIR}/${filename}.json"
      echo "Converting $excel_file to $json_file"
      python "${PYTHON_SCRIPT}" "$excel_file" "$json_file"
      
      if [ $? -ne 0 ]; then
        echo "Error: Failed to convert $excel_file to JSON!"
      fi
    fi
  done
else
  echo "Error: Output directory does not exist: ${OUTPUT_DIR}"
  exit 1
fi

# Create the Markdown post
echo "Creating Markdown post..."
POST_FILE="${POST_DIR}/${TODAY}.md"

cat > "${POST_FILE}" << EOF
---
layout:     post
title:      "Weekly recap for ${FORMATTED_DATE}"
date:       ${FORMATTED_DATE}
author:     "Syver Johansen"
---

*For details on how these predictions are generated, see the [Weekly Recap Methodology](/post/methods/race-recap/).*

## Cross Country Skiing

EOF

# Add datatable shortcodes for each JSON file
if [ -d "${DATA_DIR}" ]; then
  for json_file in "${DATA_DIR}"/*.json; do
    if [ -f "$json_file" ]; then
      filename=$(basename "$json_file" .json)
      shortcode_path="weekly-recap/cross-country/${TODAY}/${filename}"
      
      # Add section heading based on filename
      if [[ "$filename" == *"men"* && "$filename" == *"weekend"* ]]; then
        echo -e "### Men's Weekend Results\n" >> "${POST_FILE}"
      elif [[ "$filename" == *"ladies"* && "$filename" == *"weekend"* ]]; then
        echo -e "### Ladies' Weekend Results\n" >> "${POST_FILE}"
      elif [[ "$filename" == *"men"* && "$filename" == *"race1"* ]]; then
        echo -e "### Men's Race 1 Results\n" >> "${POST_FILE}"
      elif [[ "$filename" == *"ladies"* && "$filename" == *"race1"* ]]; then
        echo -e "### Ladies' Race 1 Results\n" >> "${POST_FILE}"
      elif [[ "$filename" == *"men"* && "$filename" == *"race2"* ]]; then
        echo -e "### Men's Race 2 Results\n" >> "${POST_FILE}"
      elif [[ "$filename" == *"ladies"* && "$filename" == *"race2"* ]]; then
        echo -e "### Ladies' Race 2 Results\n" >> "${POST_FILE}"
      elif [[ "$filename" == *"men"* && "$filename" == *"standings"* ]]; then
        echo -e "### Men's Standings Predictions\n" >> "${POST_FILE}"
      elif [[ "$filename" == *"womens"* && "$filename" == *"standings"* ]]; then
        echo -e "### Women's Standings Predictions\n" >> "${POST_FILE}"
      else
        echo -e "### ${filename}\n" >> "${POST_FILE}"
      fi
      
      # Add the shortcode
      echo "{{< cross-country/datatable \"${shortcode_path}\" >}}" >> "${POST_FILE}"
      echo -e "\n" >> "${POST_FILE}"
    fi
  done
else
  echo "Warning: No JSON files found in ${DATA_DIR}"
fi

echo "Done! Post created at: ${POST_FILE}"