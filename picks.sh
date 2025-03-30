#!/bin/bash

# Set up error handling
set -e  # Exit on error
set -o pipefail  # Exit if any command in a pipe fails

# Ensure script runs in UTC timezone
export TZ=UTC

# Define log file with UTC date
LOG_FILE="$HOME/ski/elo/logs/picks-$(date +%Y-%m-%d).log"
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

# Step 1: Check for races today
log "===== CHECKING FOR RACES TODAY ====="

# Get current date in the format used in the CSV files (MM/DD/YY)
CURRENT_DATE=$(date -u +"%m/%d/%y")
# Also normalize by removing leading zeros to handle cases like "3/28/25" vs "03/28/25"
CURRENT_DATE_NORMALIZED=$(echo "$CURRENT_DATE" | sed 's/^0//')
log "Current date (UTC): $CURRENT_DATE (normalized: $CURRENT_DATE_NORMALIZED)"

# Check weekends.csv for races today
WEEKEND_RACES=$(python3 -c "
import pandas as pd
import sys
import re
try:
    df = pd.read_csv('$HOME/ski/elo/python/ski/polars/excel365/weekends.csv')
    
    # Handle both formats (with or without leading zeros in month/day)
    today_date = '$CURRENT_DATE'
    today_date_alt = '$CURRENT_DATE_NORMALIZED'
    
    # Check for races with either date format
    today_races = df[df['Date'].str.strip().isin([today_date, today_date_alt])]
    
    if not today_races.empty:
        print('RACES_TODAY')
    else:
        print('NO_RACES')
except Exception as e:
    print(f'ERROR: {e}')
    sys.exit(1)
")

if [ "$WEEKEND_RACES" = "RACES_TODAY" ]; then
    log "Found weekend races scheduled for today in weekends.csv"
    run_command "cd ~/ski/elo/python/ski/polars && python3 startlist-scrape-weekend.py"
elif [ "$WEEKEND_RACES" = "NO_RACES" ]; then
    log "No weekend races scheduled for today in weekends.csv"
else
    log "Error checking weekends.csv: $WEEKEND_RACES"
fi

# Check races.csv for races today
SINGLE_RACES=$(python3 -c "
import pandas as pd
import sys
import re
try:
    df = pd.read_csv('$HOME/ski/elo/python/ski/polars/excel365/races.csv')
    
    # Handle both formats (with or without leading zeros in month/day)
    today_date = '$CURRENT_DATE'
    today_date_alt = '$CURRENT_DATE_NORMALIZED'
    
    # Check for races with either date format
    today_races = df[df['Date'].str.strip().isin([today_date, today_date_alt])]
    
    if not today_races.empty:
        print('RACES_TODAY')
    else:
        print('NO_RACES')
except Exception as e:
    print(f'ERROR: {e}')
    sys.exit(1)
")

if [ "$SINGLE_RACES" = "RACES_TODAY" ]; then
    log "Found single races scheduled for today in races.csv"
    run_command "cd ~/ski/elo/python/ski/polars && python startlist-scrape-races.py"
elif [ "$SINGLE_RACES" = "NO_RACES" ]; then
    log "No single races scheduled for today in races.csv"
else
    log "Error checking races.csv: $SINGLE_RACES"
fi

log "===== PICKS PROCESSING COMPLETED ====="