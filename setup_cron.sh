#!/bin/bash

# Setup script for Nordic Numbers Blog automation via cron
# This script helps set up the crontab entry for master_automation.sh

BLOG_DIR="$HOME/blog/daehl-e"
MASTER_SCRIPT="$BLOG_DIR/master_automation.sh"

echo "======================================="
echo "Nordic Numbers Blog Automation Setup"
echo "======================================="

# Check if master script exists
if [[ ! -f "$MASTER_SCRIPT" ]]; then
    echo "Error: Master automation script not found at: $MASTER_SCRIPT"
    exit 1
fi

# Make the master script executable
echo "Making master automation script executable..."
chmod +x "$MASTER_SCRIPT"

if [[ $? -eq 0 ]]; then
    echo "✓ Script permissions set successfully"
else
    echo "✗ Failed to set script permissions"
    exit 1
fi

# Display current crontab
echo ""
echo "Current crontab entries:"
echo "------------------------"
crontab -l 2>/dev/null || echo "(No existing crontab entries)"

echo ""
echo "======================================="
echo "Cron Setup Options"
echo "======================================="

# The cron entry we want to add
CRON_ENTRY="0 0 * * * $MASTER_SCRIPT"

echo "This script will add the following cron entry:"
echo "  $CRON_ENTRY"
echo ""
echo "This means the automation will run:"
echo "  - Every day at midnight UTC (00:00)"
echo "  - Throughout the year (the script determines if it's racing season)"
echo ""

read -p "Do you want to add this cron entry? (y/N): " -n 1 -r
echo ""

if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Adding cron entry..."
    
    # Create a temporary file with current crontab + new entry
    TEMP_CRON=$(mktemp)
    
    # Get existing crontab (if any)
    crontab -l 2>/dev/null > "$TEMP_CRON"
    
    # Check if our entry already exists
    if grep -F "$MASTER_SCRIPT" "$TEMP_CRON" >/dev/null 2>&1; then
        echo "⚠️  An entry for this script already exists in crontab:"
        grep -F "$MASTER_SCRIPT" "$TEMP_CRON"
        echo ""
        read -p "Do you want to replace it? (y/N): " -n 1 -r
        echo ""
        
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            # Remove existing entry and add new one
            grep -v -F "$MASTER_SCRIPT" "$TEMP_CRON" > "$TEMP_CRON.new"
            echo "$CRON_ENTRY" >> "$TEMP_CRON.new"
            mv "$TEMP_CRON.new" "$TEMP_CRON"
            
            # Install the new crontab
            if crontab "$TEMP_CRON"; then
                echo "✓ Crontab entry replaced successfully"
            else
                echo "✗ Failed to update crontab"
                rm -f "$TEMP_CRON"
                exit 1
            fi
        else
            echo "Keeping existing crontab entry unchanged"
        fi
    else
        # Add new entry
        echo "$CRON_ENTRY" >> "$TEMP_CRON"
        
        # Install the new crontab
        if crontab "$TEMP_CRON"; then
            echo "✓ Crontab entry added successfully"
        else
            echo "✗ Failed to add crontab entry"
            rm -f "$TEMP_CRON"
            exit 1
        fi
    fi
    
    # Clean up
    rm -f "$TEMP_CRON"
    
else
    echo "Cron setup cancelled. You can manually add this cron entry later:"
    echo "  $CRON_ENTRY"
    echo ""
    echo "To add it manually:"
    echo "  1. Run: crontab -e"
    echo "  2. Add the above line"
    echo "  3. Save and exit"
fi

echo ""
echo "======================================="
echo "Final crontab entries:"
echo "======================================="
crontab -l 2>/dev/null || echo "(No crontab entries)"

echo ""
echo "======================================="
echo "Setup Information"
echo "======================================="
echo "Master script location: $MASTER_SCRIPT"
echo "Logs will be written to: $BLOG_DIR/logs/"
echo ""
echo "The automation script will:"
echo "  ✓ Automatically detect racing season based on race dates"
echo "  ✓ Run predict_script.sh when races are scheduled for today"
echo "  ✓ Run score_scrape.sh daily during season (processes yesterday's races)"
echo "  ✓ Run recap_script.sh every Monday during season"
echo "  ✓ Log all activities with timestamps"
echo "  ✓ Handle errors gracefully"
echo ""
echo "To check if automation is working:"
echo "  - Check logs in: $BLOG_DIR/logs/"
echo "  - Monitor cron execution: tail -f /var/log/cron (on some systems)"
echo "  - Test manually: $MASTER_SCRIPT"
echo ""
echo "Setup complete!"