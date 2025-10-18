# Blog Automation Scripts Documentation

This document describes the automation scripts used to manage the Nordic Numbers blog, including data processing, predictions, and content generation for winter sports.

## Core Scripts Overview

### elo_update.sh
Updates Elo ratings and rankings for all winter sports by running data scraping, standings collection, Elo calculations, chronological data processing, and rank updates for both regular and relay skiing events.

### master_automation.sh
The main orchestration script that runs daily at midnight UTC and coordinates all blog automation. It determines which scripts to run based on season dates and day of week, checking for current races and managing the execution of prediction, scoring, and recap scripts during racing season.

### predict_script.sh
A comprehensive script that processes multiple winter sports (alpine, biathlon, nordic-combined, ski, skijump) to generate race predictions and weekly picks by running scraping scripts, processing Excel files to JSON, and creating Hugo blog posts with prediction tables.

### score_scrape.sh
Updates ski statistics by checking for races from yesterday, running Elo processing scripts and rank scraping for sports that had races, then updating all Hugo site files with current data.

### recap_script.sh
Generates weekly recaps for winter sports by running standings scraping, checking for races in the last 7 days, executing R scripts for analysis, and creating Hugo posts with Elo changes, standings predictions, and magic numbers.

## Specialized Processing Scripts

### picks.sh
Checks for skiing races scheduled for today by reading race data from weekends.csv and races.csv files, then runs appropriate scraping scripts (startlist-scrape-weekend.py or startlist-scrape-races.py) if races are found.

### races_picks_processor.sh
Processes cross-country race picks by finding Excel files in today's date folder, converting them to JSON format, and creating a markdown blog post with embedded data tables for race predictions.

### weekly_picks_processor.sh
Processes weekly skiing picks by finding Excel files in today's date folder, converting them to JSON format, and creating a markdown blog post with embedded data tables for weekly predictions.

### weekly-recap.sh
Creates weekly recaps for cross-country skiing by checking for recent races, running R analysis scripts, converting Excel outputs to JSON, and generating Hugo posts with sections for race results and standings predictions.

## Utility Scripts

### setup_cron.sh
An interactive setup script that configures cron automation for the blog by making the master automation script executable and adding it to the user's crontab to run daily at midnight UTC.

### tables_update.sh
A simple script that runs a series of Python scripts in sequence (current_ids.py, all_ids.py, all_time_elo.py, races.py, ranks.py, elo_tables.py, skier_info.py, skier_tables.py) to update cross-country skiing data tables.

## Automation Workflow

The scripts work together in an automated workflow:

1. **Daily Execution**: `master_automation.sh` runs every day at midnight UTC
2. **Season Detection**: Checks if current date falls within any winter sport season
3. **Race Day Logic**: If races are scheduled for today, runs `predict_script.sh`
4. **Daily Updates**: Always runs `score_scrape.sh` to process yesterday's results
5. **Weekly Recaps**: On Mondays, runs `recap_script.sh` for weekly analysis
6. **Data Processing**: Various processor scripts handle Excel-to-JSON conversion and blog post generation

This automation system ensures the blog stays current with race predictions, results, and analysis throughout the winter sports season without manual intervention.