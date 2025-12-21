#!/bin/bash
# Setup script to install the daily cleanup cron job

SCRIPT_PATH="$HOME/daily_cleanup.sh"
CRON_TIME="0 18 * * *"  # 1800 UTC (6 PM UTC)

echo "Setting up daily cleanup cron job..."

# Copy script to home directory
if [ -f "daily_cleanup.sh" ]; then
    cp daily_cleanup.sh "$SCRIPT_PATH"
    chmod +x "$SCRIPT_PATH"
    echo "✓ Script copied to $SCRIPT_PATH"
else
    echo "Error: daily_cleanup.sh not found in current directory"
    exit 1
fi

# Check if cron job already exists
if crontab -l 2>/dev/null | grep -q "daily_cleanup.sh"; then
    echo "⚠ Cron job already exists. Removing old entry..."
    crontab -l 2>/dev/null | grep -v "daily_cleanup.sh" | crontab -
fi

# Add new cron job
(crontab -l 2>/dev/null; echo "$CRON_TIME $SCRIPT_PATH") | crontab -

echo "✓ Cron job installed successfully!"
echo ""
echo "Cron job will run daily at 1800 UTC (6 PM UTC)"
echo ""
echo "To view your cron jobs: crontab -l"
echo "To edit cron jobs: crontab -e"
echo "To remove this cron job: crontab -e (then delete the line)"
echo ""
echo "Logs will be written to: ~/blog/daehl-e/logs/daily_cleanup.log"
