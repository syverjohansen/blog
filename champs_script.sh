#!/bin/bash

# Championship Predictions Pipeline Script
# Converts Excel outputs from R champs-predictions scripts to JSON for Hugo datatables
#
# Usage: ./champs_script.sh [YYYY]
#   If year is provided, uses that year
#   If no year provided, uses current year

# Set timezone to GMT for all operations
export TZ=GMT

# Get year - use argument if provided, otherwise use current year
if [[ -n "$1" ]]; then
    CURRENT_YEAR="$1"
else
    CURRENT_YEAR=$(date '+%Y')
fi

# Base directories
BLOG_DIR="$HOME/blog/daehl-e"
CONTENT_DIR="$BLOG_DIR/content/post"
DATA_DIR="$BLOG_DIR/data"
PYTHON_DIR="$BLOG_DIR/static/python"

# Sports to process
SPORTS=("alpine" "biathlon" "cross-country" "nordic-combined" "skijump")

# Logging function
log_message() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S GMT')
    echo "[$timestamp] $1"
}

log_message "======================================="
log_message "Championship Predictions Pipeline"
log_message "Year: $CURRENT_YEAR"
log_message "======================================="

# Phase 1: Convert Excel files to JSON for each sport
log_message ""
log_message "=== Phase 1: Excel to JSON Conversion ==="

for sport in "${SPORTS[@]}"; do
    # Source directory (where R outputs Excel files)
    source_dir="$CONTENT_DIR/$sport/drafts/champs-predictions/$CURRENT_YEAR"

    # Output directory for JSON (in data folder)
    output_dir="$DATA_DIR/$sport/drafts/champs-predictions/$CURRENT_YEAR"

    log_message ""
    log_message "Processing $sport..."
    log_message "  Source: $source_dir"
    log_message "  Output: $output_dir"

    # Check if source directory exists
    if [[ ! -d "$source_dir" ]]; then
        log_message "  SKIP: Source directory not found"
        continue
    fi

    # Count Excel files
    excel_count=$(find "$source_dir" -name "*.xlsx" -type f 2>/dev/null | wc -l | tr -d ' ')

    if [[ "$excel_count" -eq 0 ]]; then
        log_message "  SKIP: No Excel files found"
        continue
    fi

    log_message "  Found $excel_count Excel file(s)"

    # Create output directory
    mkdir -p "$output_dir"

    # Convert each Excel file
    for excel_file in "$source_dir"/*.xlsx; do
        if [[ -f "$excel_file" ]]; then
            filename=$(basename "$excel_file")
            log_message "  Converting: $filename"

            source ~/blog/venv/bin/activate && python "$PYTHON_DIR/excel_to_hugo_multiple_sheets.py" "$excel_file" "$output_dir" 2>/dev/null

            if [[ $? -eq 0 ]]; then
                log_message "    OK"
            else
                log_message "    ERROR: Conversion failed"
            fi
        fi
    done
done

# Phase 2: List generated JSON files
log_message ""
log_message "=== Phase 2: Generated JSON Files ==="

for sport in "${SPORTS[@]}"; do
    output_dir="$DATA_DIR/$sport/drafts/champs-predictions/$CURRENT_YEAR"

    if [[ -d "$output_dir" ]]; then
        json_count=$(find "$output_dir" -name "*.json" -type f 2>/dev/null | wc -l | tr -d ' ')

        if [[ "$json_count" -gt 0 ]]; then
            log_message ""
            log_message "$sport ($json_count files):"
            find "$output_dir" -name "*.json" -type f | sort | while read -r f; do
                echo "  - $(basename "$f")"
            done
        fi
    fi
done

# Phase 3: Create blog post directory structure with dynamic content
log_message ""
log_message "=== Phase 3: Blog Post Generation ==="

post_dir="$CONTENT_DIR/champs-predictions/$CURRENT_YEAR"
mkdir -p "$post_dir"
log_message "Created post directory: $post_dir"

# Generate posts dynamically based on JSON files
for sport in "${SPORTS[@]}"; do
    # Get display name for sport
    case "$sport" in
        "alpine") sport_display="Alpine Skiing" ;;
        "biathlon") sport_display="Biathlon" ;;
        "cross-country") sport_display="Cross-Country Skiing" ;;
        "nordic-combined") sport_display="Nordic Combined" ;;
        "skijump") sport_display="Ski Jumping" ;;
        *) sport_display="$sport" ;;
    esac

    json_dir="$DATA_DIR/$sport/drafts/champs-predictions/$CURRENT_YEAR"
    post_file="$post_dir/$sport.md"

    # Check if JSON directory exists
    if [[ ! -d "$json_dir" ]]; then
        log_message "Skipping $sport - no JSON files found"
        continue
    fi

    log_message "Generating $sport.md from JSON files..."

    # Start building the post content
    post_content="---
title: \"$CURRENT_YEAR Winter Olympics - $sport_display Predictions\"
date: $(date -Iseconds)
draft: false
tags: [\"predictions\", \"olympics\", \"$CURRENT_YEAR\", \"$sport\"]
---

# $CURRENT_YEAR Winter Olympics - $sport_display Predictions

*See [Championship Predictions Methodology](/post/methods/champs-predictions/) for details on how these predictions are generated.*

## Calendar

### Individual Races
"

    # Add Men's individual races
    men_individual=$(find "$json_dir" -name "men_position_probabilities_*.json" -type f 2>/dev/null | sort)
    if [[ -n "$men_individual" ]]; then
        post_content+="
#### Men
"
        while IFS= read -r json_file; do
            filename=$(basename "$json_file" .json)
            # Extract race name from filename (e.g., men_position_probabilities_1__Downhill___Feb_07 -> 1. Downhill (Feb 07))
            race_name=$(echo "$filename" | sed 's/men_position_probabilities_//' | tr '_' ' ' | sed 's/  */ /g' | sed 's/^\([0-9]*\) /\1. /' | sed 's/ \([A-Z][a-z][a-z] [0-9][0-9]\)$/ (\1)/')
            post_content+="
##### $race_name

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        done <<< "$men_individual"
    fi

    # Add Ladies' individual races
    ladies_individual=$(find "$json_dir" -name "ladies_position_probabilities_*.json" -type f 2>/dev/null | sort)
    if [[ -n "$ladies_individual" ]]; then
        post_content+="
#### Ladies
"
        while IFS= read -r json_file; do
            filename=$(basename "$json_file" .json)
            # Extract race name from filename (e.g., ladies_position_probabilities_1__Downhill___Feb_08 -> 1. Downhill (Feb 08))
            race_name=$(echo "$filename" | sed 's/ladies_position_probabilities_//' | tr '_' ' ' | sed 's/  */ /g' | sed 's/^\([0-9]*\) /\1. /' | sed 's/ \([A-Z][a-z][a-z] [0-9][0-9]\)$/ (\1)/')
            post_content+="
##### $race_name

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        done <<< "$ladies_individual"
    fi

    # Add Relay section if files exist
    # Check for cross-country style naming (relay_final_predictions_*) OR biathlon style (men_relay_position_probabilities)
    men_relay=$(find "$json_dir" -name "relay_final_predictions_Men_*.json" -type f 2>/dev/null | head -1)
    ladies_relay=$(find "$json_dir" -name "relay_final_predictions_Ladies_*.json" -type f 2>/dev/null | head -1)
    # Biathlon style naming
    if [[ -z "$men_relay" ]]; then
        men_relay=$(find "$json_dir" -name "men_relay_position_probabilities*.json" -type f 2>/dev/null | head -1)
    fi
    if [[ -z "$ladies_relay" ]]; then
        ladies_relay=$(find "$json_dir" -name "ladies_relay_position_probabilities*.json" -type f 2>/dev/null | head -1)
    fi
    # Mixed relay (biathlon only)
    mixed_relay=$(find "$json_dir" -name "mixed_relay_position_probabilities*.json" -type f 2>/dev/null | head -1)
    single_mixed_relay=$(find "$json_dir" -name "single_mixed_relay_position_probabilities*.json" -type f 2>/dev/null | head -1)

    if [[ -n "$men_relay" ]] || [[ -n "$ladies_relay" ]] || [[ -n "$mixed_relay" ]] || [[ -n "$single_mixed_relay" ]]; then
        post_content+="
### Relay
"
        if [[ -n "$men_relay" ]]; then
            filename=$(basename "$men_relay" .json)
            post_content+="
#### Men

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
        if [[ -n "$ladies_relay" ]]; then
            filename=$(basename "$ladies_relay" .json)
            post_content+="
#### Ladies

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
        if [[ -n "$mixed_relay" ]]; then
            filename=$(basename "$mixed_relay" .json)
            post_content+="
#### Mixed

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
        if [[ -n "$single_mixed_relay" ]]; then
            filename=$(basename "$single_mixed_relay" .json)
            post_content+="
#### Single Mixed

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
    fi

    # Add Team Sprint section if files exist (2-person teams)
    # Check for cross-country style (team_sprint_final_predictions_*) OR nordic-combined style (men_team_sprint_position_probabilities)
    men_ts=$(find "$json_dir" -name "team_sprint_final_predictions_Men_*.json" -type f 2>/dev/null | head -1)
    ladies_ts=$(find "$json_dir" -name "team_sprint_final_predictions_Ladies_*.json" -type f 2>/dev/null | head -1)
    # Nordic Combined style naming for team sprint
    if [[ -z "$men_ts" ]]; then
        men_ts=$(find "$json_dir" -name "men_team_sprint_position_probabilities*.json" -type f 2>/dev/null | head -1)
    fi
    if [[ -z "$ladies_ts" ]]; then
        ladies_ts=$(find "$json_dir" -name "ladies_team_sprint_position_probabilities*.json" -type f 2>/dev/null | head -1)
    fi
    # Mixed team sprint
    mixed_ts=$(find "$json_dir" -name "mixed_team_sprint_position_probabilities*.json" -type f 2>/dev/null | head -1)

    if [[ -n "$men_ts" ]] || [[ -n "$ladies_ts" ]] || [[ -n "$mixed_ts" ]]; then
        post_content+="
### Team Sprint
"
        if [[ -n "$men_ts" ]]; then
            filename=$(basename "$men_ts" .json)
            post_content+="
#### Men

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
        if [[ -n "$ladies_ts" ]]; then
            filename=$(basename "$ladies_ts" .json)
            post_content+="
#### Ladies

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
        if [[ -n "$mixed_ts" ]]; then
            filename=$(basename "$mixed_ts" .json)
            post_content+="
#### Mixed

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
    fi

    # Add regular Team section if files exist (4-person teams)
    # Nordic Combined style naming (men_teams_position_probabilities)
    men_team=$(find "$json_dir" -name "men_teams_position_probabilities*.json" -type f 2>/dev/null | head -1)
    ladies_team=$(find "$json_dir" -name "ladies_teams_position_probabilities*.json" -type f 2>/dev/null | head -1)
    mixed_team=$(find "$json_dir" -name "mixed_teams_position_probabilities*.json" -type f 2>/dev/null | head -1)
    # Ski jumping style naming (men_team.json, mixed_team.json)
    if [[ -z "$men_team" ]]; then
        men_team=$(find "$json_dir" -name "men_team.json" -type f 2>/dev/null | head -1)
    fi
    if [[ -z "$ladies_team" ]]; then
        ladies_team=$(find "$json_dir" -name "ladies_team.json" -type f 2>/dev/null | head -1)
    fi
    if [[ -z "$mixed_team" ]]; then
        mixed_team=$(find "$json_dir" -name "mixed_team.json" -type f 2>/dev/null | head -1)
    fi

    if [[ -n "$men_team" ]] || [[ -n "$ladies_team" ]] || [[ -n "$mixed_team" ]]; then
        post_content+="
### Team
"
        if [[ -n "$men_team" ]]; then
            filename=$(basename "$men_team" .json)
            post_content+="
#### Men

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
        if [[ -n "$ladies_team" ]]; then
            filename=$(basename "$ladies_team" .json)
            post_content+="
#### Ladies

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
        if [[ -n "$mixed_team" ]]; then
            filename=$(basename "$mixed_team" .json)
            post_content+="
#### Mixed

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
    fi

    # Add Nation section
    post_content+="
## Nation
"

    # Add Summary first if it exists
    summary_file=$(find "$json_dir" -name "nations_individual_Summary.json" -type f 2>/dev/null | head -1)
    if [[ -n "$summary_file" ]]; then
        filename=$(basename "$summary_file" .json)
        post_content+="
### Summary

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
    fi

    # Function to add relay/TS data for a nation
    add_nation_relay_ts() {
        local nation="$1"
        local gender="$2"  # "Men" or "Ladies"
        local content=""

        # Check for relay data
        local relay_podium=$(find "$json_dir" -name "nations_relay_podium_${nation}_${gender}.json" -type f 2>/dev/null | head -1)
        local relay_win=$(find "$json_dir" -name "nations_relay_win_${nation}_${gender}.json" -type f 2>/dev/null | head -1)

        if [[ -n "$relay_podium" ]] || [[ -n "$relay_win" ]]; then
            content+="
##### Relay
"
            if [[ -n "$relay_podium" ]]; then
                local fn=$(basename "$relay_podium" .json)
                content+="
###### Podium Optimized

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$fn\" >}}
"
            fi
            if [[ -n "$relay_win" ]]; then
                local fn=$(basename "$relay_win" .json)
                content+="
###### Win Optimized

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$fn\" >}}
"
            fi
        fi

        # Check for team sprint data
        local ts_podium=$(find "$json_dir" -name "nations_ts_podium_${nation}_${gender}.json" -type f 2>/dev/null | head -1)
        local ts_win=$(find "$json_dir" -name "nations_ts_win_${nation}_${gender}.json" -type f 2>/dev/null | head -1)

        if [[ -n "$ts_podium" ]] || [[ -n "$ts_win" ]]; then
            content+="
##### Team Sprint
"
            if [[ -n "$ts_podium" ]]; then
                local fn=$(basename "$ts_podium" .json)
                content+="
###### Podium Optimized

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$fn\" >}}
"
            fi
            if [[ -n "$ts_win" ]]; then
                local fn=$(basename "$ts_win" .json)
                content+="
###### Win Optimized

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$fn\" >}}
"
            fi
        fi

        echo "$content"
    }

    # Add Men's nations (excluding Other and Summary)
    men_nations=$(find "$json_dir" -name "nations_individual_*_Men.json" -type f 2>/dev/null | grep -v "Other_Men" | sort)
    if [[ -n "$men_nations" ]]; then
        post_content+="
### Men
"
        while IFS= read -r json_file; do
            filename=$(basename "$json_file" .json)
            # Extract nation name (e.g., nations_individual_Norway_Men -> Norway)
            nation=$(echo "$filename" | sed 's/nations_individual_//' | sed 's/_Men$//')
            post_content+="
#### $nation

##### Individual

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
            # Add relay and team sprint for this nation
            post_content+="$(add_nation_relay_ts "$nation" "Men")"
        done <<< "$men_nations"

        # Add Other Men if exists
        other_men=$(find "$json_dir" -name "nations_individual_Other_Men.json" -type f 2>/dev/null | head -1)
        if [[ -n "$other_men" ]]; then
            filename=$(basename "$other_men" .json)
            post_content+="
#### Other

##### Individual

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
    fi

    # Add Ladies' nations (excluding Other and Summary)
    ladies_nations=$(find "$json_dir" -name "nations_individual_*_Ladies.json" -type f 2>/dev/null | grep -v "Other_Ladies" | sort)
    if [[ -n "$ladies_nations" ]]; then
        post_content+="
### Ladies
"
        while IFS= read -r json_file; do
            filename=$(basename "$json_file" .json)
            # Extract nation name (e.g., nations_individual_Norway_Ladies -> Norway)
            nation=$(echo "$filename" | sed 's/nations_individual_//' | sed 's/_Ladies$//')
            post_content+="
#### $nation

##### Individual

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
            # Add relay and team sprint for this nation
            post_content+="$(add_nation_relay_ts "$nation" "Ladies")"
        done <<< "$ladies_nations"

        # Add Other Ladies if exists
        other_ladies=$(find "$json_dir" -name "nations_individual_Other_Ladies.json" -type f 2>/dev/null | head -1)
        if [[ -n "$other_ladies" ]]; then
            filename=$(basename "$other_ladies" .json)
            post_content+="
#### Other

##### Individual

{{< $sport/datatable2 \"$sport/drafts/champs-predictions/$CURRENT_YEAR/$filename\" >}}
"
        fi
    fi

    # Write the post file
    echo "$post_content" > "$post_file"
    log_message "Generated: $sport.md"
done

log_message ""
log_message "======================================="
log_message "Pipeline Complete"
log_message "======================================="
log_message ""
log_message "Generated posts are in: $post_dir"
log_message ""
log_message "Post structure (auto-generated from JSON files):"
log_message "  ## Calendar - Individual races, Relay, Team Sprint"
log_message "  ## Nation"
log_message "    ### Summary"
log_message "    ### Men / Ladies"
log_message "      #### {Nation}"
log_message "        ##### Individual"
log_message "        ##### Relay (Podium + Win Optimized)"
log_message "        ##### Team Sprint (Podium + Win Optimized)"
log_message ""
log_message "To regenerate posts after updating R scripts:"
log_message "  1. Run R champs-predictions.R for the sport"
log_message "  2. Re-run: ./champs_script.sh $CURRENT_YEAR"
