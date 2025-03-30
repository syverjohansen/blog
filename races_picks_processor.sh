#!/bin/bash

# Set up error handling
set -e  # Exit on error
set -o pipefail  # Exit if any command in a pipe fails

# Ensure script runs in UTC timezone
export TZ=UTC

# Define log file with UTC date
LOG_FILE="$HOME/blog/daehl-e/logs/race-picks-processor-$(date +%Y-%m-%d).log"
mkdir -p "$(dirname "$LOG_FILE")"  # Make sure log directory exists

# Function to log messages with UTC timestamp
log() {
    echo "[$(date +"%Y-%m-%d %H:%M:%S UTC")] $1" | tee -a "$LOG_FILE"
}

# Function to run a command and log its output
run_command() {
    log "Running: $1"
    if eval "$1" >> "$LOG_FILE" 2>&1; then
        log "✓ Successfully completed: $1"
    else
        log "✗ Failed: $1"
        return 1
    fi
}

# Get today's date in YYYYMMDD format for folder matching
TODAY_DATE=$(date -u +"%Y%m%d")
# Get date in YYYY-MM-DD format for the markdown file
FORMATTED_DATE=$(date -u +"%Y-%m-%d")

# Define the directories
DRAFTS_DIR="$HOME/blog/daehl-e/content/post/cross-country/drafts/race-picks"
OUTPUT_DIR="$HOME/blog/daehl-e/content/post/race-picks"
JSON_OUTPUT_DIR="$HOME/blog/daehl-e/data/cross-country/race-picks"
EXCEL_SCRIPT="$HOME/blog/daehl-e/static/python/excel_to_hugo_multiple_sheets.py"

log "===== STARTING RACE PICKS PROCESSING ====="
log "Looking for folder with today's date: $TODAY_DATE"

# Check if folder with today's date exists
if [ -d "$DRAFTS_DIR/$TODAY_DATE" ]; then
    log "Found folder for today: $DRAFTS_DIR/$TODAY_DATE"
    
    # Process Excel files in the folder
    # Create output directory for JSON files
    JSON_TODAY_DIR="$JSON_OUTPUT_DIR/$TODAY_DATE"
    mkdir -p "$JSON_TODAY_DIR"
    
    # Find all Excel files in the directory
    for EXCEL_FILE in "$DRAFTS_DIR/$TODAY_DATE"/*.xlsx; do
        if [ -f "$EXCEL_FILE" ]; then
            # Get filename without path
            FILE_NAME=$(basename "$EXCEL_FILE")
            log "Processing Excel file: $FILE_NAME"
            
            # Output the JSON files to the data directory
            run_command "python3 $EXCEL_SCRIPT \"$EXCEL_FILE\" \"$JSON_TODAY_DIR\""
        fi
    done
    
    # Create the markdown file
    MD_FILE="$OUTPUT_DIR/$TODAY_DATE.md"
    
    log "Creating markdown file: $MD_FILE"
    
    # Create frontmatter
    cat > "$MD_FILE" << EOF
---
layout:     post
title:      "Race Picks for $FORMATTED_DATE"
date:       $FORMATTED_DATE
author:     "Syver Johansen"
summary:    " "
---

## Cross-Country Skiing

EOF
    
    # Find all JSON files and add them to the markdown file
    for JSON_FILE in "$JSON_TODAY_DIR"/*.json; do
        if [ -f "$JSON_FILE" ]; then
            # Get the base name without extension
            BASE_NAME=$(basename "$JSON_FILE" .json)
            
            # Prepare the title based on the full filename
            # Handle files like "mixed_relay_probabilities" or "ladies_team_sprint_points"
            # We want to keep all words and capitalize each one
            
            # Replace underscores with spaces and capitalize first letter of each word
            TITLE=$(echo "$BASE_NAME" | tr '_' ' ' | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2)} 1')
            
            # Special case for handling "Sheet_1" or similar default sheet names
            if [[ "$TITLE" == *"Sheet "* ]]; then
                # Use the Excel filename instead if the sheet name is just a default one
                EXCEL_NAME=$(basename "$(find "$DRAFTS_DIR/$TODAY_DATE" -name "*.xlsx" | head -1)" .xlsx)
                TITLE=$(echo "$EXCEL_NAME" | tr '_' ' ' | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2)} 1')
            fi
            
            # Add subsection with datatable shortcode without debugging info
            cat >> "$MD_FILE" << EOF
### $TITLE

{{< cross-country/datatable "cross-country/race-picks/$TODAY_DATE/$BASE_NAME" >}}

EOF
            
            log "Added section for $BASE_NAME with title: $TITLE"
        fi
    done
    
    log "Markdown file created successfully: $MD_FILE"
else
    log "No folder found for today's date ($TODAY_DATE)"
fi

log "===== RACE PICKS PROCESSING COMPLETED ====="