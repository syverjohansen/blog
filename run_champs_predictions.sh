#!/bin/bash

# Activate virtual environment
source ~/blog/venv/bin/activate

echo "=== Championships Prediction Pipeline (All Sports) ==="
echo "Running from: $(pwd)"
echo "Date: $(date)"

# Define sports - maps content folder name to elo/python folder name
declare -A SPORT_FOLDERS
SPORT_FOLDERS[alpine]="alpine"
SPORT_FOLDERS[biathlon]="biathlon"
SPORT_FOLDERS[cross-country]="ski"
SPORT_FOLDERS[nordic-combined]="nordic-combined"
SPORT_FOLDERS[skijump]="skijump"

# Step 0: Update Elo predictions for all sports
echo ""
echo "======================================="
echo "Step 0: Updating Elo predictions for all sports"
echo "======================================="

for elo_folder in alpine biathlon ski nordic-combined skijump; do
    echo ""
    echo "--- Processing $elo_folder ---"
    cd ~/ski/elo/python/$elo_folder/polars

    if [ -f "elo_predict_script.sh" ]; then
        echo "Running elo_predict_script.sh..."
        ./elo_predict_script.sh
    fi

    if [ -f "chrono_predict.py" ]; then
        echo "Running chrono_predict.py..."
        python3 chrono_predict.py
    fi

    echo "✓ $elo_folder Elo predictions updated"
done

echo ""
echo "======================================="
echo "Elo predictions updated for all sports"
echo "======================================="

# Track success/failure
declare -a SUCCEEDED
declare -a FAILED

for sport in alpine biathlon cross-country nordic-combined skijump; do
    elo_folder="${SPORT_FOLDERS[$sport]}"

    echo ""
    echo "======================================="
    echo "Processing: $sport"
    echo "======================================="

    # Step 1: Generate Championships startlists
    echo ""
    echo "Step 1: Generating Championships startlists for $sport..."
    cd ~/ski/elo/python/$elo_folder/polars

    if [ -f "startlist-scrape-champs.py" ]; then
        python startlist-scrape-champs.py

        # Check if startlists were generated successfully
        if [ ! -f "excel365/startlist_champs_men.csv" ] && [ ! -f "excel365/startlist_champs_ladies.csv" ]; then
            echo "WARNING: Championships startlists were not generated for $sport"
            FAILED+=("$sport (startlist)")
            continue
        fi
        echo "✓ Championships startlists generated for $sport"
    else
        echo "WARNING: startlist-scrape-champs.py not found for $sport"
        FAILED+=("$sport (no scraper)")
        continue
    fi

    # Step 2: Run R script for predictions
    echo ""
    echo "Step 2: Running Championships predictions R script for $sport..."
    cd ~/blog/daehl-e/content/post/$sport/drafts

    if [ -f "champs-predictions.R" ]; then
        Rscript champs-predictions.R
        echo "✓ Championships predictions completed for $sport"
        SUCCEEDED+=("$sport")
    else
        echo "WARNING: champs-predictions.R not found for $sport"
        FAILED+=("$sport (no R script)")
    fi
done

echo ""
echo "======================================="
echo "=== Championships Prediction Pipeline Complete ==="
echo "======================================="
echo ""
echo "Succeeded: ${SUCCEEDED[*]:-none}"
echo "Failed: ${FAILED[*]:-none}"
echo ""
echo "To convert Excel outputs to JSON and generate blog posts, run:"
echo "  ./champs_script.sh [YEAR]"
