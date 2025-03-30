#!/bin/bash

# Set up error handling
set -e  # Exit on error
set -o pipefail  # Exit if any command in a pipe fails

# Ensure script runs in UTC timezone
export TZ=UTC

# Define log file with UTC date
LOG_FILE="$HOME/ski/elo/logs/daily-run-$(date +%Y-%m-%d).log"
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

# Step 1: Regular ski data processing
log "===== STARTING REGULAR SKI DATA PROCESSING ====="
run_command "cd ~/ski/elo/python/ski/polars && python3 update_scrape.py"
run_command "cd ~/ski/elo/python/ski/polars && python3 standings_scrape.py"
run_command "cd ~/ski/elo/python/ski/polars && bash elo_script.sh"
run_command "cd ~/ski/elo/python/ski/polars && python3 chrono.py" 
run_command "cd ~/ski/elo/python/ski/polars && python3 elevation_chrono_merge.py"

# Step 2: Relay ski data processing
log "===== STARTING RELAY SKI DATA PROCESSING ====="
run_command "cd ~/ski/elo/python/ski/polars/relay && python3 update_scrape.py"
run_command "cd ~/ski/elo/python/ski/polars/relay && bash elo_script.sh"
run_command "cd ~/ski/elo/python/ski/polars/relay && python3 chrono.py"



log "===== ELO UPDATE COMPLETED ====="

log "===== STARTING RANK SKI DATA PROCESSING ====="
run_command "cd ~/ski/ranks/ski && python3 rank_scrape.py"


log "===== RANK UPDATE COMPLETED ====="
