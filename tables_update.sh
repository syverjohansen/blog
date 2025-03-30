#!/bin/bash

# Set up error handling
set -e  # Exit on error
set -o pipefail  # Exit if any command in a pipe fails

# Ensure script runs in UTC timezone
export TZ=UTC

# Define log file with UTC date
LOG_FILE="$HOME/blog/daehl-e/logs/cross-country-update-$(date +%Y-%m-%d).log"
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

# Base directory for cross-country Python scripts
BASE_DIR="$HOME/blog/daehl-e/static/python/cross-country"

log "===== STARTING CROSS-COUNTRY DATA PROCESSING ====="

# Run Python scripts in the specified order
run_command "cd $BASE_DIR && python3 current_ids.py"
run_command "cd $BASE_DIR && python3 all_ids.py"
run_command "cd $BASE_DIR && python3 all_time_elo.py"
run_command "cd $BASE_DIR && python3 races.py"
run_command "cd $BASE_DIR && python3 ranks.py"
run_command "cd $BASE_DIR && python3 elo_tables.py"
run_command "cd $BASE_DIR && python3 skier_info.py"
run_command "cd $BASE_DIR && python3 skier_tables.py"

log "===== CROSS-COUNTRY DATA PROCESSING COMPLETED ====="