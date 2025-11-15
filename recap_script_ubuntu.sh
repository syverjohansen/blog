#!/bin/bash

# Set timezone to GMT/UTC for all operations (Ubuntu Version)
export TZ=UTC

# Get date calculations in UTC (GNU date syntax)
TODAY_UTC=$(date -u '+%Y-%m-%d')
TODAY_YYYYMMDD=$(date -u '+%Y%m%d')
TODAY_MMDDYYYY=$(date -u '+%m/%d/%Y')
YESTERDAY_UTC=$(date -u -d "yesterday" '+%Y-%m-%d')
YESTERDAY_MMDDYYYY=$(date -u -d "yesterday" '+%m/%d/%Y')
SEVEN_DAYS_AGO_UTC=$(date -u -d "7 days ago" '+%Y-%m-%d')
SEVEN_DAYS_AGO_MMDDYYYY=$(date -u -d "7 days ago" '+%m/%d/%Y')

# Base directories
BLOG_DIR="$HOME/blog/daehl-e"
SKI_DIR="$HOME/ski/elo/python"
CONTENT_DIR="$BLOG_DIR/content/post"

# Function to get content sport name from directory sport name
get_content_sport() {
    case "$1" in
        "alpine") echo "alpine" ;;
        "biathlon") echo "biathlon" ;;
        "nordic-combined") echo "nordic-combined" ;;
        "ski") echo "cross-country" ;;
        "skijump") echo "skijump" ;;
        *) echo "$1" ;;
    esac
}

# Enhanced logging function
log_message() {
    local timestamp=$(date -u '+%Y-%m-%d %H:%M:%S UTC')
    local message="[$timestamp] $1"
    
    # Output to stderr to avoid interfering with function returns
    printf "%s\n" "$message" >&2
    
    # Optional: Write to a dedicated log file
    local log_file="$SKI_DIR/alpine/polars/excel365/weekly_recap_script.log"
    echo "$message" >> "$log_file" 2>/dev/null || true
}

# Function to check if there were races in the last 7 days
check_races_last_7_days() {
    local csv_file="$1"
    
    if [[ ! -f "$csv_file" ]]; then
        log_message "CSV file not found: $csv_file"
        return 1
    fi
    
    log_message "Checking for races in last 7 days in $csv_file"
    
    # Create array of dates from 7 days ago to yesterday (inclusive) - GNU date syntax
    local check_dates=()
    for i in {7..1}; do
        local check_date=$(date -u -d "$i days ago" '+%m/%d/%Y')
        check_dates+=("$check_date")
    done
    
    log_message "Checking dates: ${check_dates[*]}"
    
    # Use awk to check if any of the dates exist in the Date column
    local dates_pattern=$(printf "|%s" "${check_dates[@]}")
    dates_pattern=${dates_pattern:1} # Remove leading |
    
    awk -F',' -v dates="$dates_pattern" '
        BEGIN {
            split(dates, date_array, "|")
            for(i in date_array) {
                target_dates[date_array[i]] = 1
            }
        }
        NR==1 {
            for(i=1; i<=NF; i++) {
                gsub(/^[ \t]+|[ \t]+$/, "", $i) # Strip whitespace
                if($i == "Date") {
                    date_col = i
                    break
                }
            }
        }
        NR>1 && date_col {
            gsub(/^[ \t]+|[ \t]+$/, "", $date_col) # Strip whitespace from date value
            if($date_col in target_dates) {
                found=1
                exit
            }
        }
        END { exit !found }
    ' "$csv_file" 2>/dev/null
}

# Function to process sport recaps
process_sport_recap() {
    local sport_dir="$1"
    local content_sport=$(get_content_sport "$sport_dir")
    
    log_message "Processing sport recap: $sport_dir -> $content_sport"
    
    local excel_dir="$SKI_DIR/$sport_dir/polars/excel365"
    local polars_dir="$SKI_DIR/$sport_dir/polars"
    local races_csv="$excel_dir/races.csv"
    local standings_script="$polars_dir/standings_scrape.py"
    local recap_script="$CONTENT_DIR/$content_sport/drafts/race-recap2.R"
    
    log_message "Checking files for $sport_dir:"
    log_message "  Standings script: $standings_script (exists: $([ -f "$standings_script" ] && echo "yes" || echo "no"))"
    log_message "  Races CSV: $races_csv (exists: $([ -f "$races_csv" ] && echo "yes" || echo "no"))"
    log_message "  Recap R script: $recap_script (exists: $([ -f "$recap_script" ] && echo "yes" || echo "no"))"
    
    # Step 1: Run standings_scrape.py
    if [[ -f "$standings_script" ]]; then
        log_message "Running standings_scrape.py for $sport_dir"
        cd "$polars_dir" && python standings_scrape.py
        local standings_exit_code=$?
        if [[ $standings_exit_code -eq 0 ]]; then
            log_message "✓ Successfully ran standings_scrape.py for $sport_dir"
        else
            log_message "✗ Error running standings_scrape.py for $sport_dir (exit code: $standings_exit_code)"
            return 1
        fi
    else
        log_message "✗ standings_scrape.py not found for $sport_dir"
        return 1
    fi
    
    # Step 2: Check for races in last 7 days
    if ! check_races_last_7_days "$races_csv"; then
        log_message "✗ No races found in last 7 days for $sport_dir"
        return 1
    fi
    
    log_message "✓ Races found in last 7 days for $sport_dir"
    
    # Step 3: Run race-recap2.R
    if [[ -f "$recap_script" ]]; then
        log_message "Running race-recap2.R for $sport_dir"
        cd "$(dirname "$recap_script")" && Rscript race-recap2.R
        local recap_exit_code=$?
        if [[ $recap_exit_code -eq 0 ]]; then
            log_message "✓ Successfully ran race-recap2.R for $sport_dir"
        else
            log_message "✗ Error running race-recap2.R for $sport_dir (exit code: $recap_exit_code)"
            return 1
        fi
    else
        log_message "✗ race-recap2.R not found for $sport_dir"
        return 1
    fi
    
    # Step 4: Process Excel files to JSON
    local recap_source_dir="$CONTENT_DIR/$content_sport/drafts/weekly-recap/$TODAY_YYYYMMDD"
    local recap_output_dir="$BLOG_DIR/data/$content_sport/drafts/weekly-recap/$TODAY_YYYYMMDD"
    
    if [[ -d "$recap_source_dir" ]]; then
        log_message "Processing weekly recap Excel files in $recap_source_dir"
        
        # Create output directory in data folder
        mkdir -p "$recap_output_dir"
        
        # Expected files for each gender
        local genders=("men" "ladies")
        local file_types=("elo_change" "standings_predictions" "magic_numbers")
        
        local files_processed=0
        local files_expected=$((${#genders[@]} * ${#file_types[@]}))
        
        for gender in "${genders[@]}"; do
            for file_type in "${file_types[@]}"; do
                local excel_file="$recap_source_dir/${gender}_${file_type}.xlsx"
                
                if [[ -f "$excel_file" ]]; then
                    log_message "Processing Excel file: $(basename "$excel_file")"
                    python "$BLOG_DIR/static/python/excel_to_hugo_multiple_sheets.py" "$excel_file" "$recap_output_dir"
                    if [[ $? -eq 0 ]]; then
                        log_message "✓ Successfully processed $(basename "$excel_file")"
                        ((files_processed++))
                    else
                        log_message "✗ Error processing $(basename "$excel_file")"
                    fi
                else
                    log_message "Expected file not found: $(basename "$excel_file")"
                fi
            done
        done
        
        log_message "Processed $files_processed out of $files_expected expected files for $sport_dir"
        
        if [[ $files_processed -gt 0 ]]; then
            # Return sport info if we processed any files
            echo "$content_sport:weekly-recap:true"
            return 0
        else
            log_message "✗ No files were successfully processed for $sport_dir"
            return 1
        fi
    else
        log_message "✗ Weekly recap source directory not found: $recap_source_dir"
        return 1
    fi
}

# Function to create sport section content for Hugo post
create_sport_section() {
    local sport="$1"
    local post_file="$2"
    
    # Capitalize sport name for display
    local sport_display=$(echo "$sport" | sed 's/-/ /g' | sed 's/\b\w/\u&/g')
    
    log_message "Creating section for sport: $sport_display"
    
    cat >> "$post_file" << EOF
## $sport_display

EOF
    
    local recap_dir="$BLOG_DIR/data/$sport/drafts/weekly-recap/$TODAY_YYYYMMDD"
    
    if [[ -d "$recap_dir" ]]; then
        local genders=("men" "ladies")
        local file_types=("elo_change" "standings_predictions" "magic_numbers")
        local file_type_displays=("Elo Change" "Standings Predictions" "Magic Numbers")
        
        # Process each gender
        for gender in "${genders[@]}"; do
            local gender_display=$(echo "$gender" | sed 's/\b\w/\u&/g')
            
            # Check if any files exist for this gender
            local has_gender_content=false
            for file_type in "${file_types[@]}"; do
                local json_file="$recap_dir/${gender}_${file_type}.json"
                if [[ -f "$json_file" ]]; then
                    has_gender_content=true
                    break
                fi
            done
            
            if [[ "$has_gender_content" == "true" ]]; then
                log_message "Adding section for $gender_display in $sport_display"
                
                cat >> "$post_file" << EOF
### $gender_display

EOF
                
                # Add each file type as subsection
                for i in "${!file_types[@]}"; do
                    local file_type="${file_types[$i]}"
                    local file_type_display="${file_type_displays[$i]}"
                    local json_file="$recap_dir/${gender}_${file_type}.json"
                    
                    if [[ -f "$json_file" ]]; then
                        log_message "Adding $file_type_display section for $gender_display in $sport_display"
                        
                        cat >> "$post_file" << EOF
#### $file_type_display

{{< $sport/datatable "$sport/drafts/weekly-recap/$TODAY_YYYYMMDD/$(basename "$json_file" .json)" >}}

EOF
                    else
                        log_message "JSON file not found: $json_file"
                    fi
                done
            else
                log_message "No content found for $gender_display in $sport_display"
            fi
        done
    else
        log_message "Warning: Recap directory not found: $recap_dir"
    fi
}

# Main execution
log_message "======================================="
log_message "Starting weekly recap script"
log_message "======================================="
log_message "Date information (UTC):"
log_message "  Today: $TODAY_UTC ($TODAY_MMDDYYYY)"
log_message "  Yesterday: $YESTERDAY_UTC ($YESTERDAY_MMDDYYYY)"
log_message "  7 days ago: $SEVEN_DAYS_AGO_UTC ($SEVEN_DAYS_AGO_MMDDYYYY)"
log_message "  Today YYYYMMDD: $TODAY_YYYYMMDD"
log_message "======================================="

# Check base directories
log_message "Checking base directories:"
log_message "  BLOG_DIR: $BLOG_DIR (exists: $([ -d "$BLOG_DIR" ] && echo "yes" || echo "no"))"
log_message "  SKI_DIR: $SKI_DIR (exists: $([ -d "$SKI_DIR" ] && echo "yes" || echo "no"))"
log_message "  CONTENT_DIR: $CONTENT_DIR (exists: $([ -d "$CONTENT_DIR" ] && echo "yes" || echo "no"))"

# Array to store sports with recaps
sports_with_recaps=""

# Process each sport
log_message "======================================="
log_message "Processing sports for weekly recaps"
log_message "======================================="

for sport_dir in alpine biathlon nordic-combined ski skijump; do
    log_message "--- Starting $sport_dir processing ---"
    result=$(process_sport_recap "$sport_dir")
    
    if [[ $? -eq 0 && -n "$result" ]]; then
        log_message "✓ $sport_dir returned recap data: $result"
        
        if [[ -n "$sports_with_recaps" ]]; then
            sports_with_recaps="$sports_with_recaps|$result"
        else
            sports_with_recaps="$result"
        fi
        log_message "Added recap: $result"
    else
        log_message "✗ $sport_dir returned no recap data or failed processing"
    fi
    log_message "--- Completed $sport_dir processing ---"
    log_message ""
done

log_message "======================================="
log_message "Sport processing complete"
log_message "Sports with recaps: $sports_with_recaps"
log_message "======================================="

# Create Hugo post if there are recaps
if [[ -n "$sports_with_recaps" ]]; then
    log_message "Creating Hugo post for weekly recaps"
    log_message "Sports with recaps to process: '$sports_with_recaps'"
    
    recap_post_dir="$CONTENT_DIR/weekly-recap"
    mkdir -p "$recap_post_dir"
    recap_post_file="$recap_post_dir/$TODAY_YYYYMMDD.md"
    
    log_message "Recap post directory: $recap_post_dir (exists: $([ -d "$recap_post_dir" ] && echo "yes" || echo "no"))"
    log_message "Recap post file: $recap_post_file"
    
    title="Weekly Recap for week of $SEVEN_DAYS_AGO_MMDDYYYY to $YESTERDAY_MMDDYYYY"
    
    cat > "$recap_post_file" << EOF
---
title: "$title"
date: $(date -u -Iseconds)
draft: false
tags: ["recap", "skiing", "weekly-recap", "standings", "elo"]
---

# $title

EOF
    
    log_message "Created weekly recap post header"
    
    # Add sections for each sport with recaps
    IFS='|' read -ra recap_array <<< "$sports_with_recaps"
    for recap_info in "${recap_array[@]}"; do
        IFS=':' read -r sport recap_type has_data <<< "$recap_info"
        log_message "Adding section for recap sport: $sport"
        create_sport_section "$sport" "$recap_post_file"
    done
    
    log_message "✓ Created weekly recap post: $recap_post_file"
else
    log_message "No recaps found - no races in the last 7 days for any sport"
fi

log_message "======================================="
log_message "Weekly recap script completed successfully"
log_message "======================================="