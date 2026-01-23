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

# Phase 3: Create blog post directory structure
log_message ""
log_message "=== Phase 3: Blog Post Structure ==="

post_dir="$CONTENT_DIR/champs-predictions/$CURRENT_YEAR"
mkdir -p "$post_dir"
log_message "Created post directory: $post_dir"

# Create one placeholder post per sport (with Calendar and Nation sections)
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

    post_file="$post_dir/$sport.md"

    if [[ ! -f "$post_file" ]]; then
        cat > "$post_file" << EOF
---
title: "$CURRENT_YEAR Winter Olympics - $sport_display Predictions"
date: $(date -Iseconds)
draft: true
tags: ["predictions", "olympics", "$CURRENT_YEAR", "$sport"]
---

# $CURRENT_YEAR Winter Olympics - $sport_display Predictions

## Calendar

[Position probability tables for each event]

## Nation

[Nation breakdown tables]

EOF
        log_message "Created placeholder: $sport.md"
    else
        log_message "Skipped $sport.md (already exists)"
    fi
done

log_message ""
log_message "======================================="
log_message "Pipeline Complete"
log_message "======================================="
log_message ""
log_message "Next steps:"
log_message "1. Run R champs-predictions.R scripts for each sport"
log_message "2. Re-run this script to convert new Excel outputs"
log_message "3. Edit blog posts in: $post_dir"
log_message ""
log_message "Datatable shortcode format:"
log_message '  {{< {sport}/datatable "{sport}/drafts/champs-predictions/{year}/{filename}" >}}'
log_message ""
log_message "Example:"
log_message '  {{< cross-country/datatable "cross-country/drafts/champs-predictions/2026/men_position_probabilities_Skiathlon" >}}'
