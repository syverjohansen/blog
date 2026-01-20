#!/bin/bash

# Annual Season Processing Script for Nordic Sports (macOS Version)
# This script runs on May 1st to perform complete season data processing
# including scraping, elo calculations, chronological processing, and elevation merging

echo "=== Annual Season Processing Script ==="
echo "Date: $(date)"
echo "Processing complete season data for all sports"
echo "==============================================="

# Define sports arrays
# Sports for processing (matches directory structure in ~/ski/elo/python/)
SPORTS=("alpine" "biathlon" "nordic-combined" "skijump" "ski")

# Hugo sports (matches directory structure in ~/blog/daehl-e/static/python/)
HUGO_SPORTS=("alpine" "biathlon" "cross-country" "nordic-combined" "skijump")

# Files to update in Hugo site
HUGO_FILES=("current_ids.py" "all_ids.py" "all_time_elo.py" "races.py" "ranks.py" "elo_tables.py" "skier_info.py" "skier_tables.py" "head2head.py")

# Function to run complete season processing for a sport
run_season_processing() {
    local sport=$1
    local base_dir="$HOME/ski/elo/python/$sport/polars"
    
    echo "  Running complete season processing for $sport..."
    
    # Process main polars directory
    if [[ -d "$base_dir" ]]; then
        cd "$base_dir" || return 1
        
        echo "    Running scrape.py (full season data scraping)..."
        if [[ -f "scrape.py" ]]; then
            python3 scrape.py
        else
            echo "    Warning: scrape.py not found for $sport"
        fi

        echo "    Running all_scrape.py (all competitions data scraping)..."
        if [[ -f "all_scrape.py" ]]; then
            python3 all_scrape.py
        else
            echo "    Warning: all_scrape.py not found for $sport"
        fi

        echo "    Running elo_script.sh (complete elo calculations)..."
        if [[ -f "elo_script.sh" ]]; then
            bash elo_script.sh
        else
            echo "    Warning: elo_script.sh not found for $sport"
        fi
        
        echo "    Running chrono.py (chronological processing)..."
        if [[ -f "chrono.py" ]]; then
            python3 chrono.py
        else
            echo "    Warning: chrono.py not found for $sport"
        fi
        
        # Skip elevation_chrono_merge.py for alpine (doesn't exist)
        if [[ "$sport" != "alpine" ]]; then
            echo "    Running elevation_chrono_merge.py..."
            if [[ -f "elevation_chrono_merge.py" ]]; then
                python3 elevation_chrono_merge.py
            else
                echo "    Warning: elevation_chrono_merge.py not found for $sport"
            fi
        fi
    else
        echo "    Warning: Base directory not found: $base_dir"
    fi
    
    # Process relay directory (skip for alpine)
    if [[ "$sport" != "alpine" ]] && [[ -d "$base_dir/relay" ]]; then
        cd "$base_dir/relay" || return 1
        
        echo "    Running relay season processing for $sport..."
        
        echo "      Running scrape.py (relay data scraping)..."
        if [[ -f "scrape.py" ]]; then
            python3 scrape.py
        else
            echo "      Warning: relay scrape.py not found for $sport"
        fi

        echo "      Running all_scrape.py (relay all competitions data scraping)..."
        if [[ -f "all_scrape.py" ]]; then
            python3 all_scrape.py
        else
            echo "      Warning: relay all_scrape.py not found for $sport"
        fi

        echo "      Running elo_script.sh (relay elo calculations)..."
        if [[ -f "elo_script.sh" ]]; then
            bash elo_script.sh
        else
            echo "      Warning: relay elo_script.sh not found for $sport"
        fi
        
        echo "      Running chrono.py (relay chronological processing)..."
        if [[ -f "chrono.py" ]]; then
            python3 chrono.py
        else
            echo "      Warning: relay chrono.py not found for $sport"
        fi
        
        echo "      Running relay_chrono.py..."
        if [[ -f "relay_chrono.py" ]]; then
            python3 relay_chrono.py
        else
            echo "      Warning: relay_chrono.py not found for $sport"
        fi
        
        echo "      Running elevation_chrono_merge.py (relay elevation merging)..."
        if [[ -f "elevation_chrono_merge.py" ]]; then
            python3 elevation_chrono_merge.py
        else
            echo "      Warning: relay elevation_chrono_merge.py not found for $sport"
        fi
    fi
}

# Function to run rank scraping for a sport
run_rank_scraping() {
    local sport=$1
    local rank_dir="$HOME/ski/ranks/$sport"
    
    echo "  Running complete rank scraping for $sport..."
    
    if [[ -d "$rank_dir" ]]; then
        cd "$rank_dir" || return 1
        echo "    Running rank_scrape.py..."
        if [[ -f "rank_scrape.py" ]]; then
            python3 rank_scrape.py
        else
            echo "    Warning: rank_scrape.py not found for $sport"
        fi
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

# Main processing
echo ""
echo "=== PROCESSING ALL SPORTS FOR COMPLETE SEASON ==="
echo "Sports to process: ${SPORTS[*]}"

# Process each sport completely
for sport in "${SPORTS[@]}"; do
    echo ""
    echo "--- Processing $sport (Complete Season) ---"
    
    # Run complete season processing
    run_season_processing "$sport"
    
    # Run rank scraping
    run_rank_scraping "$sport"
done

echo ""
echo "=== UPDATING HUGO SITE ==="

# Update Hugo files for all sports
for sport in "${HUGO_SPORTS[@]}"; do
    echo ""
    echo "--- Updating Hugo files for $sport ---"
    update_hugo_files "$sport"
done

echo ""
echo "=== ANNUAL SEASON SCRIPT COMPLETED ==="
echo "Processed complete season data for: ${SPORTS[*]}"
echo "Updated Hugo files for all sports: ${HUGO_SPORTS[*]}"
echo "Completion time: $(date)"