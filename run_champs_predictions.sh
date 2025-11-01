#!/bin/bash

# Activate virtual environment
source ~/blog/venv/bin/activate

echo "=== Alpine Championships Prediction Pipeline ==="
echo "Running from: $(pwd)"
echo "Date: $(date)"

# Step 1: Generate Championships startlists
echo ""
echo "Step 1: Generating Championships startlists..."
cd ~/ski/elo/python/alpine/polars
python startlist-scrape-champs.py

# Check if startlists were generated successfully
if [ ! -f "excel365/startlist_champs_men.csv" ] || [ ! -f "excel365/startlist_champs_ladies.csv" ]; then
    echo "ERROR: Championships startlists were not generated successfully"
    exit 1
fi

echo "✓ Championships startlists generated successfully"

# Step 2: Run R script for predictions
echo ""
echo "Step 2: Running Championships predictions R script..."
cd ~/blog/daehl-e/content/post/alpine/drafts
Rscript champs-predictions.R

echo ""
echo "=== Alpine Championships Prediction Pipeline Complete ==="