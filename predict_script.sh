#!/bin/bash

# Set timezone to GMT for all operations
export TZ=GMT

# Get today's date in different formats
TODAY_MMDDYYYY=$(date '+%m/%d/%Y')
TODAY_YYYYMMDD=$(date '+%Y%m%d')
TODAY_DISPLAY=$(date '+%B %d, %Y')

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

# Enhanced logging function that forces output to appear
log_message() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S GMT')
    local message="[$timestamp] $1"
    
    # Output only to stderr to avoid interfering with function returns
    printf "%s\n" "$message" >&2
    
    # Optional: Write to a dedicated log file as well
    local log_file="$SKI_DIR/alpine/polars/excel365/predict_script.log"
    echo "$message" >> "$log_file" 2>/dev/null || true
}

# Function to check if date exists in CSV file
check_date_in_csv() {
    local csv_file="$1"
    local target_date="$2"
    
    if [[ ! -f "$csv_file" ]]; then
        log_message "CSV file not found: $csv_file"
        return 1
    fi
    
    log_message "Checking for date $target_date in $csv_file"
    
    # Use awk to check if the date exists in the Date column
    awk -F',' -v date="$target_date" '
        NR==1 {
            for(i=1; i<=NF; i++) {
                if($i == "Date" || $i == " Date" || $i == "Date ") {
                    date_col = i
                    break
                }
            }
        }
        NR>1 && date_col && $date_col == date { found=1; exit }
        END { exit !found }
    ' "$csv_file" 2>/dev/null
}

# Function to process sport data generation (Python/R scripts + Excel to JSON)
process_sport_data() {
    local sport_dir="$1"
    local content_sport=$(get_content_sport "$sport_dir")
    
    log_message "Processing sport data: $sport_dir -> $content_sport"
    
    local excel_dir="$SKI_DIR/$sport_dir/polars/excel365"
    local polars_dir="$SKI_DIR/$sport_dir/polars"
    local weekends_csv="$excel_dir/weekends.csv"
    local races_csv="$excel_dir/races.csv"
    
    log_message "Checking directories for $sport_dir:"
    log_message "  Excel dir: $excel_dir (exists: $([ -d "$excel_dir" ] && echo "yes" || echo "no"))"
    log_message "  Polars dir: $polars_dir (exists: $([ -d "$polars_dir" ] && echo "yes" || echo "no"))"
    log_message "  Weekends CSV: $weekends_csv (exists: $([ -f "$weekends_csv" ] && echo "yes" || echo "no"))"
    log_message "  Races CSV: $races_csv (exists: $([ -f "$races_csv" ] && echo "yes" || echo "no"))"
    
    local data_generated=""
    
    # Check for weekend races
    if check_date_in_csv "$weekends_csv" "$TODAY_MMDDYYYY"; then
        log_message "✓ Weekend races found for $sport_dir on $TODAY_MMDDYYYY"
        
        # Run weekend scrape script
        if [[ -f "$polars_dir/startlist-scrape-weekend.py" ]]; then
            log_message "Running startlist-scrape-weekend.py for $sport_dir"
            cd "$polars_dir" && source ~/blog/venv/bin/activate && python startlist-scrape-weekend.py >/dev/null 2>&1
            log_message "Completed startlist-scrape-weekend.py for $sport_dir"
        else
            log_message "Warning: startlist-scrape-weekend.py not found for $sport_dir"
        fi
        
        # Process weekend predictions
        local weekend_source_dir="$CONTENT_DIR/$content_sport/drafts/weekly-picks/$TODAY_YYYYMMDD"
        local weekend_output_dir="$BLOG_DIR/data/$content_sport/drafts/weekly-picks/$TODAY_YYYYMMDD"
        
        # Create weekend source directory if it doesn't exist
        mkdir -p "$weekend_source_dir"
        log_message "Created weekend source directory: $weekend_source_dir"
        
        if [[ -d "$weekend_source_dir" ]]; then
            log_message "Processing weekend Excel files in $weekend_source_dir"
            
            # Create output directory in data folder
            mkdir -p "$weekend_output_dir"
            
            # Count Excel files
            weekend_excel_count=$(find "$weekend_source_dir" -name "*.xlsx" | wc -l)
            log_message "Found $weekend_excel_count weekend Excel files to process"
            
            # Process each Excel file in the weekend source directory
            for excel_file in "$weekend_source_dir"/*.xlsx; do
                if [[ -f "$excel_file" ]]; then
                    log_message "Processing weekend Excel file: $(basename "$excel_file")"
                    source ~/blog/venv/bin/activate && python "$BLOG_DIR/static/python/excel_to_hugo_multiple_sheets.py" "$excel_file" "$weekend_output_dir" >/dev/null 2>&1
                    if [[ $? -eq 0 ]]; then
                        log_message "✓ Successfully processed weekend $(basename "$excel_file")"
                    else
                        log_message "✗ Error processing weekend $(basename "$excel_file")"
                    fi
                fi
            done
            
            # Add to data generated
            if [[ -n "$data_generated" ]]; then
                data_generated="$data_generated|$content_sport:weekly-picks:true"
            else
                data_generated="$content_sport:weekly-picks:true"
            fi
        else
            log_message "Weekend source directory not found: $weekend_source_dir"
        fi
    else
        log_message "✗ No weekend races found for $sport_dir on $TODAY_MMDDYYYY"
    fi
    
    # Check for regular races
    if check_date_in_csv "$races_csv" "$TODAY_MMDDYYYY"; then
        log_message "✓ Regular races found for $sport_dir on $TODAY_MMDDYYYY"
        
        # Run race scrape script
        if [[ -f "$polars_dir/startlist-scrape-races.py" ]]; then
            log_message "Running startlist-scrape-races.py for $sport_dir"
            cd "$polars_dir" && source ~/blog/venv/bin/activate && python startlist-scrape-races.py >/dev/null 2>&1
            log_message "Completed startlist-scrape-races.py for $sport_dir"
        else
            log_message "Warning: startlist-scrape-races.py not found for $sport_dir"
        fi
        
        # Process race predictions
        local race_source_dir="$CONTENT_DIR/$content_sport/drafts/race-picks/$TODAY_YYYYMMDD"
        local race_output_dir="$BLOG_DIR/data/$content_sport/drafts/race-picks/$TODAY_YYYYMMDD"
        
        # Create race source directory if it doesn't exist
        mkdir -p "$race_source_dir"
        log_message "Created race source directory: $race_source_dir"
        
        if [[ -d "$race_source_dir" ]]; then
            log_message "Processing race Excel files in $race_source_dir"
            
            # Create output directory in data folder
            mkdir -p "$race_output_dir"
            
            # Count Excel files
            race_excel_count=$(find "$race_source_dir" -name "*.xlsx" | wc -l)
            log_message "Found $race_excel_count race Excel files to process"
            
            # Process each Excel file in the race source directory
            for excel_file in "$race_source_dir"/*.xlsx; do
                if [[ -f "$excel_file" ]]; then
                    log_message "Processing race Excel file: $(basename "$excel_file")"
                    source ~/blog/venv/bin/activate && python "$BLOG_DIR/static/python/excel_to_hugo_multiple_sheets.py" "$excel_file" "$race_output_dir" >/dev/null 2>&1
                    if [[ $? -eq 0 ]]; then
                        log_message "✓ Successfully processed race $(basename "$excel_file")"
                    else
                        log_message "✗ Error processing race $(basename "$excel_file")"
                    fi
                fi
            done
            
            # Add to data generated
            if [[ -n "$data_generated" ]]; then
                data_generated="$data_generated|$content_sport:race-picks:true"
            else
                data_generated="$content_sport:race-picks:true"
            fi
        else
            log_message "Race source directory not found: $race_source_dir"
        fi
    else
        log_message "✗ No regular races found for $sport_dir on $TODAY_MMDDYYYY"
    fi
    
    # Return all data generated for this sport
    echo "$data_generated"
}

# Function to create sport section content for Hugo post
create_sport_section() {
    local sport="$1"
    local pred_type="$2"
    local post_file="$3"
    
    # Capitalize sport name for display
    local sport_display=$(echo "$sport" | sed 's/-/ /g' | sed 's/\b\w/\u&/g')
    
    log_message "Creating section for sport: $sport_display"
    
    cat >> "$post_file" << EOF
## $sport_display

EOF
    
    local prediction_dir="$BLOG_DIR/data/$sport/drafts/$pred_type/$TODAY_YYYYMMDD"
    
    if [[ -d "$prediction_dir" ]]; then
        # Define the processing order and categories
        local individual_categories=("men" "ladies" "mixed")
        local team_categories=()
        local fantasy_categories=()
        
        # Debug: List all files in the directory
        log_message "Files found in $prediction_dir:"
        for file in "$prediction_dir"/*.json; do
            if [[ -f "$file" ]]; then
                log_message "  - $(basename "$file")"
            fi
        done
        
        # Collect all JSON files to categorize them
        local all_files=($(find "$prediction_dir" -name "*.json" | sort))
        log_message "Total JSON files found: ${#all_files[@]}"
        
        # Categorize team/relay files based on sport
        case "$sport" in
            "biathlon")
                team_categories=("single_mixed_relay" "mixed_relay" "men_relay" "ladies_relay")
                log_message "Biathlon team categories: ${team_categories[*]}"
                ;;
            "cross-country")
                team_categories=("mixed_relay" "men_relay" "ladies_relay" "men_team_sprint" "ladies_team_sprint")
                fantasy_categories=("fantasy_mixed_relay_team" "fantasy_relay_team" "fantasy_team_sprint_team" "fantasy_team")
                log_message "Cross-country team categories: ${team_categories[*]}"
                log_message "Cross-country fantasy categories: ${fantasy_categories[*]}"
                ;;
            "nordic-combined")
                team_categories=("men_team" "ladies_team" "men_team_sprint" "ladies_team_sprint" "Mixed_team")
                log_message "Nordic-combined team categories: ${team_categories[*]}"
                ;;
            "skijump")
                team_categories=("men_team" "ladies_team" "Mixed_team")
                log_message "Skijump team categories: ${team_categories[*]}"
                ;;
        esac
        
        # Function to process a category
        process_category() {
            local category="$1"
            local category_display="$2"
            
            local has_category_content=false
            
            # Check if we have files for this category
            local points_file="$prediction_dir/${category}.json"
            local prob_base="$prediction_dir/${category}_position_probabilities"
            
            log_message "Checking category '$category':"
            log_message "  Points file: $points_file (exists: $([ -f "$points_file" ] && echo "yes" || echo "no"))"
            log_message "  Prob base: $prob_base"
            
            # Check for probability files
            local prob_files=($(ls "${prob_base}"*.json 2>/dev/null | sort))
            log_message "  Probability files found: ${#prob_files[@]}"
            for pf in "${prob_files[@]}"; do
                log_message "    - $(basename "$pf")"
            done
            
            if [[ -f "$points_file" ]] || [[ ${#prob_files[@]} -gt 0 ]]; then
                has_category_content=true
                log_message "  → Adding section for $category_display"
                
                cat >> "$post_file" << EOF
### $category_display

EOF
            else
                log_message "  → No content found for $category"
                return
            fi
            
            # Add Points section if points file exists
            if [[ -f "$points_file" ]]; then
                log_message "Adding Points section for $category in $sport"
                
                cat >> "$post_file" << EOF
#### Points

{{< $sport/datatable "$sport/drafts/$pred_type/$TODAY_YYYYMMDD/$(basename "$points_file" .json)" >}}

EOF
            fi
            
            # Add Position Probabilities section if probability files exist
            if [[ ${#prob_files[@]} -gt 0 ]]; then
                log_message "Adding Position Probabilities section for $category in $sport"
                
                cat >> "$post_file" << EOF
#### Position Probabilities

EOF
                
                # Add each race
                local race_num=1
                for prob_file in "${prob_files[@]}"; do
                    local filename=$(basename "$prob_file" .json)
                    
                    # Extract race number or sheet name from filename
                    local race_label="Race $race_num"
                    if [[ "$filename" =~ _([^_]+)_([0-9]+)$ ]]; then
                        race_label="Race ${BASH_REMATCH[2]}"
                    elif [[ "$filename" =~ _Race_([0-9]+)$ ]]; then
                        race_label="Race ${BASH_REMATCH[1]}"
                    fi
                    
                    log_message "Adding $race_label for $category in $sport"
                    
                    cat >> "$post_file" << EOF
##### $race_label

{{< $sport/datatable "$sport/drafts/$pred_type/$TODAY_YYYYMMDD/$filename" >}}

EOF
                    ((race_num++))
                done
            fi
        }
        
        # Process individual categories (Men, Ladies, Mixed)
        for category in "${individual_categories[@]}"; do
            local category_display=$(echo "$category" | sed 's/\b\w/\u&/g')
            process_category "$category" "$category_display"
        done
        
        # Process team/relay categories
        for category in "${team_categories[@]}"; do
            # Create display name for team categories
            local category_display=$(echo "$category" | sed 's/_/ /g' | sed 's/\b\w/\u&/g')
            process_category "$category" "$category_display"
        done
        
        # Process fantasy categories (for cross-country)
        if [[ ${#fantasy_categories[@]} -gt 0 ]]; then
            cat >> "$post_file" << EOF
### Fantasy

EOF
            for category in "${fantasy_categories[@]}"; do
                # Create display name for fantasy categories
                local category_display=$(echo "$category" | sed 's/fantasy_//g' | sed 's/_/ /g' | sed 's/\b\w/\u&/g')
                
                local points_file="$prediction_dir/${category}.json"
                
                if [[ -f "$points_file" ]]; then
                    log_message "Adding Fantasy section for $category in $sport"
                    
                    cat >> "$post_file" << EOF
#### $category_display

{{< $sport/datatable "$sport/drafts/$pred_type/$TODAY_YYYYMMDD/$(basename "$points_file" .json)" >}}

EOF
                fi
            done
        fi
        
    else
        log_message "Warning: Prediction directory not found: $prediction_dir"
    fi
}

# Main execution
log_message "======================================="
log_message "Starting prediction script for $TODAY_DISPLAY"
log_message "======================================="
log_message "Date formats:"
log_message "  MM/DD/YYYY: $TODAY_MMDDYYYY"
log_message "  YYYYMMDD: $TODAY_YYYYMMDD"
log_message "  Display: $TODAY_DISPLAY"
log_message "======================================="

# Check base directories
log_message "Checking base directories:"
log_message "  BLOG_DIR: $BLOG_DIR (exists: $([ -d "$BLOG_DIR" ] && echo "yes" || echo "no"))"
log_message "  SKI_DIR: $SKI_DIR (exists: $([ -d "$SKI_DIR" ] && echo "yes" || echo "no"))"
log_message "  CONTENT_DIR: $CONTENT_DIR (exists: $([ -d "$CONTENT_DIR" ] && echo "yes" || echo "no"))"

# Array to store sports with predictions
sports_with_predictions=""

# Process each sport
log_message "======================================="
log_message "Processing sports for predictions"
log_message "======================================="

for sport_dir in alpine biathlon nordic-combined ski skijump; do
    log_message "--- Starting $sport_dir data generation ---"
    result=$(process_sport_data "$sport_dir")
    
    if [[ -n "$result" ]]; then
        log_message "✓ $sport_dir returned data generation result: $result"
        
        # Handle multiple predictions from a single sport (separated by |)
        IFS='|' read -ra sport_predictions <<< "$result"
        for single_prediction in "${sport_predictions[@]}"; do
            if [[ -n "$single_prediction" ]]; then
                if [[ -n "$sports_with_predictions" ]]; then
                    sports_with_predictions="$sports_with_predictions|$single_prediction"
                else
                    sports_with_predictions="$single_prediction"
                fi
                log_message "Added prediction: $single_prediction"
            fi
        done
    else
        log_message "✗ $sport_dir returned no data generation result"
    fi
    log_message "--- Completed $sport_dir processing ---"
    log_message ""
done

log_message "======================================="
log_message "Data generation phase complete"
log_message "Sports with generated data: $sports_with_predictions"
log_message "======================================="

# Wait briefly to ensure all data files are fully written
log_message "Waiting 5 seconds for data files to be fully written..."
sleep 5

# Phase 2: Create Hugo posts if there are predictions
if [[ -n "$sports_with_predictions" ]]; then
    log_message "======================================="
    log_message "Starting Hugo post creation phase"
    log_message "======================================="
    log_message "Creating Hugo posts for predictions"
    log_message "Raw sports_with_predictions: '$sports_with_predictions'"
    
    # Convert string back to array-like processing
    IFS='|' read -ra prediction_array <<< "$sports_with_predictions"
    
    log_message "Found ${#prediction_array[@]} sport(s) with predictions"
    log_message "Prediction array contents:"
    for i in "${!prediction_array[@]}"; do
        log_message "  [$i]: '${prediction_array[$i]}'"
    done
    
    # Separate predictions by type
    weekend_predictions=""
    race_predictions=""
    
    for prediction in "${prediction_array[@]}"; do
        IFS=':' read -r sport pred_type is_weekend_flag <<< "$prediction"
        log_message "Processing prediction: sport='$sport', type='$pred_type', weekend='$is_weekend_flag'"
        
        if [[ "$pred_type" == "weekly-picks" ]]; then
            if [[ -n "$weekend_predictions" ]]; then
                weekend_predictions="$weekend_predictions|$prediction"
            else
                weekend_predictions="$prediction"
            fi
            log_message "Added to weekend predictions: $prediction"
        elif [[ "$pred_type" == "race-picks" ]]; then
            if [[ -n "$race_predictions" ]]; then
                race_predictions="$race_predictions|$prediction"
            else
                race_predictions="$prediction"
            fi
            log_message "Added to race predictions: $prediction"
        else
            log_message "WARNING: Unknown prediction type '$pred_type' for prediction '$prediction'"
        fi
    done
    
    log_message "Final weekend predictions: '$weekend_predictions'"
    log_message "Final race predictions: '$race_predictions'"
    
    # Create weekly picks post if there are weekend predictions
    if [[ -n "$weekend_predictions" ]]; then
        log_message "Creating weekly picks post"
        log_message "Weekend predictions to process: '$weekend_predictions'"
        
        weekly_post_dir="$CONTENT_DIR/weekly-picks"
        mkdir -p "$weekly_post_dir"
        weekly_post_file="$weekly_post_dir/$TODAY_YYYYMMDD.md"
        
        log_message "Weekly post directory: $weekly_post_dir (exists: $([ -d "$weekly_post_dir" ] && echo "yes" || echo "no"))"
        log_message "Weekly post file: $weekly_post_file"
        
        title="Weekend Picks for $TODAY_MMDDYYYY"
        
        cat > "$weekly_post_file" << EOF
---
title: "$title"
date: $(date -Iseconds)
draft: false
tags: ["predictions", "skiing", "weekend-picks"]
---

# $title

EOF
        
        log_message "Created weekly post header"
        
        # Add sections for each sport with weekend predictions
        IFS='|' read -ra weekend_array <<< "$weekend_predictions"
        for prediction in "${weekend_array[@]}"; do
            IFS=':' read -r sport pred_type is_weekend_flag <<< "$prediction"
            log_message "Adding section for weekend sport: $sport"
            create_sport_section "$sport" "$pred_type" "$weekly_post_file"
        done
        
        log_message "✓ Created weekly picks post: $weekly_post_file"
    fi
    
    # Create race picks post if there are race predictions
    if [[ -n "$race_predictions" ]]; then
        log_message "Creating race picks post"
        log_message "Race predictions to process: '$race_predictions'"
        
        race_post_dir="$CONTENT_DIR/race-picks"
        mkdir -p "$race_post_dir"
        race_post_file="$race_post_dir/$TODAY_YYYYMMDD.md"
        
        log_message "Race post directory: $race_post_dir (exists: $([ -d "$race_post_dir" ] && echo "yes" || echo "no"))"
        log_message "Race post file: $race_post_file"
        
        title="Race Picks for $TODAY_MMDDYYYY"
        
        cat > "$race_post_file" << EOF
---
title: "$title"
date: $(date -Iseconds)
draft: false
tags: ["predictions", "skiing", "race-picks"]
---

# $title

EOF
        
        log_message "Created race post header"
        
        # Add sections for each sport with race predictions
        IFS='|' read -ra race_array <<< "$race_predictions"
        for prediction in "${race_array[@]}"; do
            IFS=':' read -r sport pred_type is_weekend_flag <<< "$prediction"
            log_message "Adding section for race sport: $sport"
            create_sport_section "$sport" "$pred_type" "$race_post_file"
        done
        
        log_message "✓ Created race picks post: $race_post_file"
    fi
else
    log_message "No predictions found for today"
fi

log_message "======================================="
log_message "Prediction script completed successfully"
log_message "======================================="