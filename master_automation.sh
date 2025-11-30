#!/bin/bash

# Master Automation Script for Nordic Numbers Blog
# This script runs daily at midnight UTC and coordinates all blog automation
# It determines which scripts to run based on season dates and day of week

# Set timezone to UTC for all operations
export TZ=UTC

# Get current date information in UTC
TODAY_UTC=$(date -u '+%Y-%m-%d')
TODAY_MMDDYYYY=$(date -u '+%m/%d/%Y')
TODAY_DAY_OF_WEEK=$(date -u '+%u')  # 1=Monday, 7=Sunday
YESTERDAY_MMDDYYYY=$(date -u -v-1d '+%m/%d/%Y')

# Base directories
BLOG_DIR="$HOME/blog/daehl-e"
SKI_DIR="$HOME/ski/elo/python"
SCRIPT_DIR="$BLOG_DIR"
LOG_DIR="$BLOG_DIR/logs"

# Create logs directory if it doesn't exist
mkdir -p "$LOG_DIR"

# Log file for this run
LOG_FILE="$LOG_DIR/master-automation-$(date -u '+%Y%m%d').log"

# Logging function
log_message() {
    local timestamp=$(date -u '+%Y-%m-%d %H:%M:%S UTC')
    local message="[$timestamp] $1"
    
    # Output to both stdout and log file
    echo "$message"
    echo "$message" >> "$LOG_FILE"
}

# Function to get season dates for a sport
get_season_dates() {
    local sport_dir="$1"
    local races_csv="$SKI_DIR/$sport_dir/polars/excel365/races.csv"
    
    if [[ ! -f "$races_csv" ]]; then
        log_message "Warning: Races CSV not found for $sport_dir: $races_csv"
        return 1
    fi
    
    # Find the earliest and latest dates from the Date column
    # Convert MM/DD/YYYY to YYYY-MM-DD for proper sorting, then convert back
    local first_date=$(tail -n +2 "$races_csv" 2>/dev/null | awk -F',' '{
        split($1, d, "/")
        printf "%04d-%02d-%02d %s\n", d[3], d[1], d[2], $1
    }' | sort | head -1 | awk '{print $2}')
    
    local last_date=$(tail -n +2 "$races_csv" 2>/dev/null | awk -F',' '{
        split($1, d, "/")
        printf "%04d-%02d-%02d %s\n", d[3], d[1], d[2], $1
    }' | sort | tail -1 | awk '{print $2}')
    
    local dates=""
    if [[ -n "$first_date" && -n "$last_date" ]]; then
        dates="$first_date|$last_date"
    fi
    
    if [[ -n "$dates" ]]; then
        echo "$dates"
        return 0
    else
        return 1
    fi
}

# Function to convert MM/DD/YYYY to YYYY-MM-DD for date comparison
convert_date_format() {
    local mmddyyyy="$1"
    if [[ "$mmddyyyy" =~ ^([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})$ ]]; then
        local month="${BASH_REMATCH[1]}"
        local day="${BASH_REMATCH[2]}"
        local year="${BASH_REMATCH[3]}"
        
        # Add leading zeros if needed (remove leading zeros first to avoid octal interpretation)
        month=$(printf "%02d" "$((10#$month))")
        day=$(printf "%02d" "$((10#$day))")
        
        echo "$year-$month-$day"
        return 0
    else
        return 1
    fi
}

# Function to check if current date is within season
is_in_season() {
    local first_race_date="$1"
    local last_race_date="$2"
    
    # Convert dates to YYYY-MM-DD format for comparison
    local first_date_iso=$(convert_date_format "$first_race_date")
    local last_date_iso=$(convert_date_format "$last_race_date")
    
    if [[ -z "$first_date_iso" || -z "$last_date_iso" ]]; then
        log_message "Error: Could not convert season dates for comparison"
        return 1
    fi
    
    # Add one day to last race date for score_scrape.sh (needs to run day after last race)
    # Simple date arithmetic for YYYY-MM-DD format
    local year="${last_date_iso%-*-*}"
    local month="${last_date_iso#*-}"
    month="${month%-*}"
    local day="${last_date_iso##*-}"
    
    # Add one day
    day=$((day + 1))
    
    # Handle month overflow (simplified - just add some buffer days)
    if [[ $day -gt 31 ]]; then
        day=1
        month=$((month + 1))
        if [[ $month -gt 12 ]]; then
            month=1
            year=$((year + 1))
        fi
    fi
    
    # Format with leading zeros
    local last_date_plus_one=$(printf "%04d-%02d-%02d" "$year" "$month" "$day")
    
    log_message "Season check: $first_date_iso <= $TODAY_UTC <= $last_date_plus_one"
    
    # Check if today is within the extended season range
    # Use lexicographic comparison for YYYY-MM-DD format (works reliably)
    if [[ "$TODAY_UTC" > "$first_date_iso" || "$TODAY_UTC" = "$first_date_iso" ]] && \
       [[ "$TODAY_UTC" < "$last_date_plus_one" || "$TODAY_UTC" = "$last_date_plus_one" ]]; then
        return 0
    else
        return 1
    fi
}

# Function to get overall season dates (earliest first race to latest last race)
get_overall_season() {
    local sports=("alpine" "biathlon" "nordic-combined" "ski" "skijump")
    local overall_first=""
    local overall_last=""
    
    log_message "Determining overall season dates..." >&2
    
    for sport in "${sports[@]}"; do
        local dates=$(get_season_dates "$sport")
        if [[ $? -eq 0 && -n "$dates" ]]; then
            IFS='|' read -r first_date last_date <<< "$dates"
            log_message "  $sport: $first_date to $last_date" >&2
            
            # Convert to comparable format
            local first_iso=$(convert_date_format "$first_date")
            local last_iso=$(convert_date_format "$last_date")
            
            if [[ -n "$first_iso" && -n "$last_iso" ]]; then
                # Update overall first date (earliest)
                if [[ -z "$overall_first" || "$first_iso" < "$overall_first" ]]; then
                    overall_first="$first_iso"
                fi
                
                # Update overall last date (latest)
                if [[ -z "$overall_last" || "$last_iso" > "$overall_last" ]]; then
                    overall_last="$last_iso"
                fi
            fi
        else
            log_message "  $sport: No valid season dates found" >&2
        fi
    done
    
    if [[ -n "$overall_first" && -n "$overall_last" ]]; then
        log_message "Overall season: $overall_first to $overall_last" >&2
        echo "$overall_first|$overall_last"
        return 0
    else
        log_message "Error: Could not determine overall season dates" >&2
        return 1
    fi
}

# Function to check if any sport has races today
has_races_today() {
    local sports=("alpine" "biathlon" "nordic-combined" "ski" "skijump")
    
    for sport in "${sports[@]}"; do
        local races_csv="$SKI_DIR/$sport/polars/excel365/races.csv"
        if [[ -f "$races_csv" ]]; then
            # Check if today's date exists in the races CSV (comma-separated)
            if awk -F',' -v date="$TODAY_MMDDYYYY" '
                NR>1 && $1 == date {found=1; exit}
                END { exit !found }
            ' "$races_csv" 2>/dev/null; then
                log_message "Found races today for $sport"
                return 0
            fi
        fi
    done
    
    return 1
}

# Function to run a script with error handling
run_script() {
    local script_name="$1"
    local script_path="$SCRIPT_DIR/$script_name"
    
    if [[ ! -f "$script_path" ]]; then
        log_message "Error: Script not found: $script_path"
        return 1
    fi
    
    if [[ ! -x "$script_path" ]]; then
        log_message "Making script executable: $script_path"
        chmod +x "$script_path"
    fi
    
    log_message "Starting $script_name..."
    
    # Run script and capture output
    local start_time=$(date -u '+%Y-%m-%d %H:%M:%S UTC')
    local output_file="$LOG_DIR/${script_name%.sh}-$(date -u '+%Y%m%d-%H%M%S').log"
    
    if bash "$script_path" 2>&1 | tee "$output_file"; then
        local end_time=$(date -u '+%Y-%m-%d %H:%M:%S UTC')
        log_message "✓ $script_name completed successfully ($start_time to $end_time)"
        log_message "  Output logged to: $output_file"
        return 0
    else
        local exit_code=$?
        local end_time=$(date -u '+%Y-%m-%d %H:%M:%S UTC')
        log_message "✗ $script_name failed with exit code $exit_code ($start_time to $end_time)"
        log_message "  Error output in: $output_file"
        return $exit_code
    fi
}

# Main execution starts here
log_message "======================================="
log_message "Master Automation Script Starting"
log_message "======================================="
log_message "Date: $TODAY_UTC ($TODAY_MMDDYYYY)"
log_message "Day of week: $TODAY_DAY_OF_WEEK (1=Monday, 7=Sunday)"
log_message "======================================="

# Pull latest changes from both repositories before processing
log_message "======================================="
log_message "Pulling latest changes before processing"
log_message "======================================="

# Pull latest changes from blog repository
cd "$BLOG_DIR"
log_message "Pulling latest changes from blog repository..."
if git pull origin main >> "$LOG_FILE" 2>&1; then
    log_message "✓ Successfully pulled latest blog repository changes"
else
    log_message "⚠️  Warning: Failed to pull from blog repository origin main"
fi

# Pull latest changes from ski repository
cd "$SKI_DIR/.." # Go to ~/ski directory (parent of elo/python)
log_message "Pulling latest changes from ski repository..."
if git pull origin main >> "$LOG_FILE" 2>&1; then
    log_message "✓ Successfully pulled latest ski repository changes"
else
    log_message "⚠️  Warning: Failed to pull from ski repository origin main"
fi

log_message "======================================="

# Get overall season dates
overall_season=$(get_overall_season)
if [[ $? -ne 0 || -z "$overall_season" ]]; then
    log_message "Error: Could not determine season dates. Exiting."
    exit 1
fi

IFS='|' read -r season_start season_end <<< "$overall_season"
log_message "Season period: $season_start to $season_end"

# Check if we're in season  
# Convert YYYY-MM-DD to MM/DD/YYYY format
season_start_mmdd="${season_start#*-}"
season_start_mmdd="${season_start_mmdd%-*}/${season_start_mmdd#*-}/${season_start%-*-*}"
season_end_mmdd="${season_end#*-}"  
season_end_mmdd="${season_end_mmdd%-*}/${season_end_mmdd#*-}/${season_end%-*-*}"

if ! is_in_season "$season_start_mmdd" "$season_end_mmdd"; then
    log_message "Currently outside of racing season. No scripts will be executed."
    log_message "Next season expected to start around: $season_start"
    exit 0
fi

log_message "✓ Currently in racing season"

# Initialize execution flags
run_score_scrape=false
run_recap_script=false  
run_predict_script=false
run_season_script=false

# Decision logic for which scripts to run

# 1. predict_script.sh - Run if there are races today
if has_races_today; then
    log_message "✓ Races found for today - will run predict_script.sh"
    run_predict_script=true
else
    log_message "- No races found for today - skipping predict_script.sh"
fi

# 2. score_scrape.sh - Run every day during season (processes yesterday's races if any)
log_message "✓ In season - will run score_scrape.sh (checks yesterday's races)"
run_score_scrape=true

# 3. recap_script.sh - Run on Mondays during season
if [[ "$TODAY_DAY_OF_WEEK" -eq 1 ]]; then
    log_message "✓ Monday detected - will run recap_script.sh"
    run_recap_script=true
else
    log_message "- Not Monday - skipping recap_script.sh"
fi

# 4. season_script.sh - Run on May 1st every year for complete season processing
if [[ "$TODAY_UTC" == *"-05-01" ]]; then
    log_message "✓ May 1st detected - will run season_script.sh (annual season processing)"
    run_season_script=true
else
    log_message "- Not May 1st - skipping season_script.sh"
fi

# Execute the scripts in order
log_message "======================================="
log_message "Execution Summary:"
log_message "  score_scrape.sh: $([ "$run_score_scrape" = true ] && echo "YES" || echo "NO")"
log_message "  predict_script.sh: $([ "$run_predict_script" = true ] && echo "YES" || echo "NO")"  
log_message "  recap_script.sh: $([ "$run_recap_script" = true ] && echo "YES" || echo "NO")"
log_message "  season_script.sh: $([ "$run_season_script" = true ] && echo "YES" || echo "NO")"
log_message "======================================="

script_results=""

# Run score_scrape.sh first (processes yesterday's results and updates Elo ratings)
if [[ "$run_score_scrape" = true ]]; then
    if run_script "score_scrape.sh"; then
        script_results="$script_results score_scrape:SUCCESS"
    else
        script_results="$script_results score_scrape:FAILED"
    fi
fi

# Run predict_script.sh second (for today's races, using updated Elo ratings)
if [[ "$run_predict_script" = true ]]; then
    if run_script "predict_script.sh"; then
        script_results="$script_results predict_script:SUCCESS"
    else
        script_results="$script_results predict_script:FAILED"
    fi
fi

# Run recap_script.sh (weekly analysis)
if [[ "$run_recap_script" = true ]]; then
    if run_script "recap_script.sh"; then
        script_results="$script_results recap_script:SUCCESS"
    else
        script_results="$script_results recap_script:FAILED"
    fi
fi

# Run season_script.sh (annual complete season processing on May 1st)
if [[ "$run_season_script" = true ]]; then
    if run_script "season_script.sh"; then
        script_results="$script_results season_script:SUCCESS"
    else
        script_results="$script_results season_script:FAILED"
    fi
fi

# Git automation - commit and push changes
if [[ "$script_results" != *"FAILED"* ]]; then
    log_message "======================================="
    log_message "Committing and pushing changes to git"
    log_message "======================================="
    
    # Change to blog directory for git operations
    cd "$BLOG_DIR"
    
    # Check if there are any changes to commit
    if ! git diff --quiet || ! git diff --cached --quiet || [[ -n "$(git ls-files --others --exclude-standard)" ]]; then
        log_message "Changes detected, proceeding with git operations..."
        
        # Add all changes
        log_message "Adding all changes to git..."
        if git add . >> "$LOG_FILE" 2>&1; then
            log_message "✓ Successfully added changes to git"
        else
            log_message "✗ Failed to add changes to git"
            script_results="$script_results git_add:FAILED"
        fi
        
        # Create commit message based on successful scripts
        successful_scripts=""
        [[ "$script_results" == *"predict_script:SUCCESS"* ]] && successful_scripts="$successful_scripts predict_script"
        [[ "$script_results" == *"score_scrape:SUCCESS"* ]] && successful_scripts="$successful_scripts score_scrape"
        [[ "$script_results" == *"recap_script:SUCCESS"* ]] && successful_scripts="$successful_scripts recap_script"
        [[ "$script_results" == *"season_script:SUCCESS"* ]] && successful_scripts="$successful_scripts season_script"
        
        commit_message="Master automation successfully ran and did the following:$successful_scripts"
        
        # Commit changes
        log_message "Committing changes with message: $commit_message"
        if git commit -m "$commit_message" >> "$LOG_FILE" 2>&1; then
            log_message "✓ Successfully committed changes"
            script_results="$script_results git_commit:SUCCESS"
        else
            log_message "✗ Failed to commit changes"
            script_results="$script_results git_commit:FAILED"
        fi
        
        # Pull latest changes from origin
        log_message "Pulling latest changes from origin main..."
        if git pull origin main >> "$LOG_FILE" 2>&1; then
            log_message "✓ Successfully pulled from origin main"
            script_results="$script_results git_pull:SUCCESS"
        else
            log_message "✗ Failed to pull from origin main"
            script_results="$script_results git_pull:FAILED"
        fi
        
        # Push changes to origin
        log_message "Pushing changes to origin main..."
        if git push origin main >> "$LOG_FILE" 2>&1; then
            log_message "✓ Successfully pushed to origin main"
            script_results="$script_results git_push:SUCCESS"
        else
            log_message "✗ Failed to push to origin main"
            script_results="$script_results git_push:FAILED"
        fi
    else
        log_message "No changes detected, skipping git operations"
    fi
else
    log_message "Scripts failed, skipping git operations"
fi

# Final summary
log_message "======================================="
log_message "Master Automation Script Completed"
log_message "======================================="
log_message "Execution results:$script_results"
log_message "Total execution time: $(date -u '+%Y-%m-%d %H:%M:%S UTC')"
log_message "Log file: $LOG_FILE"
log_message "======================================="

# Exit with error code if any script failed
if [[ "$script_results" == *"FAILED"* ]]; then
    log_message "⚠️  Some operations failed. Check individual log files for details."
    exit 1
else
    log_message "✓ All operations completed successfully"
    exit 0
fi