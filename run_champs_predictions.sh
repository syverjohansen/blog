#!/bin/bash

# Activate virtual environment
source ~/blog/venv/bin/activate

echo "=== Championships Prediction Pipeline (All Sports) ==="
echo "Running from: $(pwd)"
echo "Date: $(date)"

# Function to map content folder name to elo/python folder name
get_elo_folder() {
    case "$1" in
        alpine) echo "alpine" ;;
        biathlon) echo "biathlon" ;;
        cross-country) echo "ski" ;;
        nordic-combined) echo "nordic-combined" ;;
        skijump) echo "skijump" ;;
        *) echo "" ;;
    esac
}

# Track success/failure
declare -a SUCCEEDED
declare -a FAILED

# Process each sport completely before moving to the next
for sport in alpine biathlon cross-country nordic-combined skijump; do
    elo_folder=$(get_elo_folder "$sport")

    echo ""
    echo "======================================="
    echo "Processing: $sport (elo folder: $elo_folder)"
    echo "======================================="

    # Step 1: Update Elo predictions
    echo ""
    echo ">>> Step 1: Updating Elo predictions for $sport <<<"
    cd ~/ski/elo/python/$elo_folder/polars
    echo "Working directory: $(pwd)"

    if [ -f "elo_predict_script.sh" ]; then
        echo "Running elo_predict_script.sh..."
        ./elo_predict_script.sh
        echo "✓ elo_predict_script.sh complete for $sport"
    else
        echo "Note: elo_predict_script.sh not found for $sport"
    fi

    # Step 2: Run chrono_predict.py
    echo ""
    echo ">>> Step 2: Running chrono_predict.py for $sport <<<"
    cd ~/ski/elo/python/$elo_folder/polars

    if [ -f "chrono_predict.py" ]; then
        echo "Running chrono_predict.py..."
        python3 chrono_predict.py
        echo "✓ chrono_predict.py complete for $sport"
    else
        echo "Note: chrono_predict.py not found for $sport"
    fi

    # Step 3: Generate Championships startlists
    echo ""
    echo ">>> Step 3: Generating Championships startlists for $sport <<<"
    cd ~/ski/elo/python/$elo_folder/polars
    echo "Working directory: $(pwd)"

    if [ -f "startlist-scrape-champs.py" ]; then
        echo "Running startlist-scrape-champs.py..."
        python startlist-scrape-champs.py
        echo "startlist-scrape-champs.py exit code: $?"

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

    # Step 4: Run R script for predictions
    echo ""
    echo ">>> Step 4: Running Championships predictions R script for $sport <<<"
    cd ~/blog/daehl-e/content/post/$sport/drafts
    echo "Working directory: $(pwd)"

    if [ -f "champs-predictions.R" ]; then
        echo "Running champs-predictions.R..."
        Rscript champs-predictions.R
        echo "✓ Championships predictions completed for $sport"
        SUCCEEDED+=("$sport")
    else
        echo "WARNING: champs-predictions.R not found for $sport"
        FAILED+=("$sport (no R script)")
    fi

    echo ""
    echo "======================================="
    echo "Completed all steps for: $sport"
    echo "======================================="
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
