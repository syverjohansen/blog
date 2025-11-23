#!/bin/bash

# Ski Statistics Update Script for daehl-e.com (Ubuntu Version)
# This script checks for races from yesterday and updates all necessary files

# Set timezone to GMT
export TZ=GMT

# Get yesterday's date in MM/DD/YYYY format (GNU date syntax)
YESTERDAY=$(date -d "yesterday" +"%m/%d/%Y")

# Verify we got a date
if [[ -z "$YESTERDAY" ]]; then
    echo "Error: Could not determine yesterday's date"
    exit 1
fi

echo "=== Ski Statistics Update Script ==="
echo "Date: $(date)"
echo "Checking for races on: $YESTERDAY"
echo "======================================"

# Define sports arrays
# Sports for checking races (matches directory structure in ~/ski/elo/python/)
RACE_SPORTS=("alpine" "biathlon" "nordic-combined" "skijump" "ski")

# Sports for hugo site updates (matches directory structure in ~/blog/daehl-e/static/python/)
HUGO_SPORTS=("alpine" "biathlon" "cross-country" "nordic-combined" "skijump")

# Files to update in Hugo site
HUGO_FILES=("current_ids.py" "all_ids.py" "all_time_elo.py" "races.py" "ranks.py" "elo_tables.py" "skier_info.py" "skier_tables.py" "head2head.py")

# Function to check if there were races yesterday for a sport
check_races_yesterday() {
    local sport=$1
    local races_file="$HOME/ski/elo/python/$sport/polars/excel365/races.csv"
    
    if [[ ! -f "$races_file" ]]; then
        echo "Warning: Races file not found for $sport: $races_file"
        return 1
    fi
    
    echo "  Checking $races_file for date $YESTERDAY"
    
    # Check if yesterday's date exists in the races.csv file
    # Handle both comma-separated and space-separated formats
    if grep -q "$YESTERDAY" "$races_file"; then
        echo "✓ Races found for $sport on $YESTERDAY"
        return 0
    else
        echo "- No races found for $sport on $YESTERDAY"
        # Debug: show what dates are in the file
        echo "  Available dates in file:"
        head -n 20 "$races_file" | grep -o '[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]' | sort | uniq | head -n 10
        return 1
    fi
}

# Function to run elo processing scripts for a sport
run_elo_processing() {
    local sport=$1
    local base_dir="$HOME/ski/elo/python/$sport/polars"
    
    echo "  Running elo processing for $sport..."
    
    # Run scripts in main polars directory
    if [[ -d "$base_dir" ]]; then
        cd "$base_dir" || return 1
        
        echo "    Running update_scrape.py..."
        python3 update_scrape.py
        
        echo "    Running elo_script.sh..."
        if [[ -f "elo_script.sh" ]]; then
            bash elo_script.sh
        fi
        
        echo "    Running chrono.py..."
        python3 chrono.py
        
        # Skip elevation_chrono_merge.py for alpine (doesn't exist)
        if [[ "$sport" != "alpine" ]]; then
            echo "    Running elevation_chrono_merge.py..."
            python3 elevation_chrono.merge.py
        fi
    fi
    
    # Run scripts in relay directory (skip for alpine)
    if [[ "$sport" != "alpine" ]] && [[ -d "$base_dir/relay" ]]; then
        cd "$base_dir/relay" || return 1
        
        echo "    Running relay scripts for $sport..."
        
        echo "      Running update_scrape.py..."
        python3 update_scrape.py
        
        echo "      Running elo_script.sh..."
        if [[ -f "elo_script.sh" ]]; then
            bash elo_script.sh
        fi
        
        echo "      Running chrono.py..."
        python3 chrono.py
        python3 relay_chrono.py
        
        # Skip elevation_chrono_merge.py for alpine (doesn't exist)  
        if [[ "$sport" != "alpine" ]]; then
            echo "      Running elevation_chrono.merge.py..."
            python3 elevation_chrono.merge.py
        fi
    fi
}

# Function to run rank scraping for a sport
run_rank_scraping() {
    local sport=$1
    local rank_dir="$HOME/ski/ranks/$sport"
    
    echo "  Running rank scraping for $sport..."
    
    if [[ -d "$rank_dir" ]]; then
        cd "$rank_dir" || return 1
        echo "    Running rank_scrape.py..."
        python3 rank_scrape.py
    else
        echo "    Warning: Rank directory not found: $rank_dir"
    fi
}

# Function to update Hugo site files for a sport
update_hugo_files() {
    local sport=$1
    local hugo_dir="$HOME/blog/daehl-e/static/python/$sport"
    
    echo "  Updating Hugo files for $sport..."
    
    if [[ -d "$hugo_dir" ]]; then
        cd "$hugo_dir" || return 1
        
        for file in "${HUGO_FILES[@]}"; do
            if [[ -f "$file" ]]; then
                echo "    Running $file..."
                python3 "$file"
            else
                echo "    Warning: File not found: $hugo_dir/$file"
            fi
        done
    else
        echo "    Warning: Hugo directory not found: $hugo_dir"
    fi
}

# Main processing loop
SPORTS_WITH_RACES=()

echo ""
echo "=== CHECKING FOR RACES ==="
for sport in "${RACE_SPORTS[@]}"; do
    if check_races_yesterday "$sport"; then
        SPORTS_WITH_RACES+=("$sport")
    fi
done

if [[ ${#SPORTS_WITH_RACES[@]} -eq 0 ]]; then
    echo ""
    echo "No races found for yesterday ($YESTERDAY). Exiting."
    exit 0
fi

echo ""
echo "=== PROCESSING SPORTS WITH RACES ==="
echo "Sports to process: ${SPORTS_WITH_RACES[*]}"

# Process each sport that had races
for sport in "${SPORTS_WITH_RACES[@]}"; do
    echo ""
    echo "--- Processing $sport ---"
    
    # Run elo processing
    run_elo_processing "$sport"
    
    # Run rank scraping
    run_rank_scraping "$sport"
done

echo ""
echo "=== UPDATING HUGO SITE ==="

# Update Hugo files for all sports (not just those with races)
for sport in "${HUGO_SPORTS[@]}"; do
    echo ""
    echo "--- Updating Hugo files for $sport ---"
    update_hugo_files "$sport"
done

echo ""
echo "=== SCRIPT COMPLETED ==="
echo "Processed sports with races: ${SPORTS_WITH_RACES[*]}"
echo "Updated Hugo files for all sports: ${HUGO_SPORTS[*]}"
echo "Completion time: $(date)"