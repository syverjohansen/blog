# Winter Sports Analytics - How to Run

A comprehensive guide to running all scripts, automations, tests, and tools in this project.

## Table of Contents

- [Quick Start](#quick-start)
- [Environment Setup](#environment-setup)
- [Daily Automation](#daily-automation)
- [Manual Prediction Pipeline](#manual-prediction-pipeline)
- [Test Mode](#test-mode)
- [Parameter Optimization](#parameter-optimization)
- [Championships](#championships)
- [Individual Scripts](#individual-scripts)
- [Troubleshooting](#troubleshooting)

---

## Quick Start

```bash
# Run the full daily automation (what cron runs at midnight UTC)
./master_automation.sh

# Run predictions for today's races only
./predict_script.sh

# Run score updates for yesterday's races only
./score_scrape.sh

# Run the test harness to validate the pipeline
cd ~/ski/elo/python
python test_harness.py

# Optimize simulation parameters (warning: takes hours)
Rscript ~/blog/daehl-e/content/post/optimization/param-optimizer.R
```

---

## Environment Setup

### Prerequisites

- R with packages: `dplyr`, `tidyr`, `openxlsx`, `mgcv`, `leaps`, `logger`, `lubridate`, `purrr`
- Python 3 with packages: `polars`, `requests`, `beautifulsoup4`
- Git configured for both repos

### Configuration File

The `.env` file controls test vs production mode:

```bash
# Location: ~/ski/elo/.env

# Production mode (real races, real data)
TEST_MODE=false

# Test mode (uses test_races.csv, limited data)
TEST_MODE=true
```

### Directory Structure

```
~/blog/daehl-e/                    # Hugo blog + R scripts
├── master_automation.sh           # Main entry point
├── predict_script.sh              # Prediction pipeline
├── score_scrape.sh                # Score update pipeline
├── content/post/{sport}/drafts/   # R simulation scripts
├── content/post/shared/           # Shared R utilities
├── content/post/optimization/     # Parameter optimization
├── data/                          # JSON output for Hugo
└── logs/                          # Automation logs

~/ski/elo/python/                  # ELO system + Python scripts
├── {sport}/polars/                # Sport-specific scripts
│   ├── excel365/                  # CSV data files
│   │   ├── races.csv              # Race schedule
│   │   ├── weekends.csv           # Weekend schedule
│   │   └── test_races.csv         # Test mode races
│   ├── elo.py                     # ELO calculations
│   ├── startlist-scrape-*.py      # Startlist scrapers
│   └── ...
├── test_harness.py                # Test framework
└── pipeline_config.py             # Pipeline configuration
```

---

## Daily Automation

### Master Automation (Cron Job)

The main automation runs daily at midnight UTC via cron.

```bash
# Run manually
./master_automation.sh

# What it does:
# 1. git pull both repos
# 2. Check race schedule for changes
# 3. Run score_scrape.sh (every day in season)
# 4. Run predict_script.sh (days with races)
# 5. Run recap_script.sh (Mondays only)
# 6. Run season_script.sh (May 1st only)
# 7. git commit and push
```

### Setup Cron Job

```bash
# Install the cron job
./setup_cron.sh

# Or manually add to crontab:
crontab -e
# Add: 0 0 * * * /Users/syverjohansen/blog/daehl-e/master_automation.sh
```

### View Logs

```bash
# Recent automation logs
ls -la ~/blog/daehl-e/logs/

# View latest log
tail -100 ~/blog/daehl-e/logs/master-automation-$(date +%Y%m%d).log

# Watch log in real-time
tail -f ~/blog/daehl-e/logs/master-automation-*.log
```

---

## Manual Prediction Pipeline

### Run Predictions for Today

```bash
# Full prediction pipeline
./predict_script.sh

# What it does:
# 1. Scrape startlists for today's races
# 2. Run ELO predictions
# 3. Run chrono predictions
# 4. Run R simulation scripts
# 5. Convert Excel to JSON
# 6. Generate Hugo posts
```

### Run Score Updates

```bash
# Update ELO ratings from yesterday's results
./score_scrape.sh
```

### Run Individual Sport Predictions

```bash
# Cross-Country race picks
Rscript ~/blog/daehl-e/content/post/cross-country/drafts/race-picks-simulation.R

# Alpine race picks
Rscript ~/blog/daehl-e/content/post/alpine/drafts/race-picks-simulation.R

# Biathlon race picks
Rscript ~/blog/daehl-e/content/post/biathlon/drafts/race-picks-simulation.R

# Nordic Combined race picks
Rscript ~/blog/daehl-e/content/post/nordic-combined/drafts/race-picks-simulation.R

# Ski Jumping race picks
Rscript ~/blog/daehl-e/content/post/skijump/drafts/race-picks-simulation.R
```

### Weekly Picks (Fantasy)

```bash
# Cross-Country weekly fantasy picks
Rscript ~/blog/daehl-e/content/post/cross-country/drafts/weekly-picks-simulation.R

# Alpine weekly picks
Rscript ~/blog/daehl-e/content/post/alpine/drafts/weekly-picks2.R
```

### Tour de Ski

```bash
# TdS stage picks
Rscript ~/blog/daehl-e/content/post/cross-country/drafts/tds-picks-simulation.R

# TdS final climb
Rscript ~/blog/daehl-e/content/post/cross-country/drafts/final_climb-simulation.R
```

---

## Test Mode

### Enable Test Mode

```bash
# Edit the .env file
echo "TEST_MODE=true" > ~/ski/elo/.env

# Or temporarily override in your session
export TEST_MODE=true
```

### Run Test Harness

```bash
cd ~/ski/elo/python

# Test all sports
python test_harness.py

# Test specific sport
python test_harness.py alpine
python test_harness.py biathlon
python test_harness.py cross-country
python test_harness.py nordic-combined
python test_harness.py skijump

# Test only scraping phase
python test_harness.py --scrape-only

# Test only simulation phase
python test_harness.py --simulate-only

# Verbose output
python test_harness.py -v
python test_harness.py alpine --verbose
```

### Test Data Files

Test mode uses curated race files instead of the full schedule:

```
~/ski/elo/python/{sport}/polars/excel365/test_races.csv
~/ski/elo/python/{sport}/polars/excel365/test_weekends.csv
```

### Return to Production Mode

```bash
echo "TEST_MODE=false" > ~/ski/elo/.env
```

---

## Parameter Optimization

The parameter optimizer tunes simulation parameters (DECAY_LAMBDA, SD_SCALE_FACTOR, etc.) per sport and race type.

### Quick Start

```r
# In R console
source("~/blog/daehl-e/content/post/optimization/param-optimizer.R")

# Optimize one sport (faster, ~3 hours)
results <- optimize_sport("cross-country", gender = "men", verbose = TRUE)

# Optimize all sports and auto-generate sport_params.R (~30 hours)
run_full_optimization()
```

### From Command Line

**IMPORTANT: Prevent Laptop Sleep**

On macOS, laptop sleep will kill background processes. Use `caffeinate -s` to keep the system awake:

```bash
# Run optimizer for one sport, one gender (~3 hours)
caffeinate -s Rscript -e 'source("~/blog/daehl-e/content/post/optimization/param-optimizer.R"); optimize_sport("cross-country", gender = "men")'

# Run full optimization for one sport, both genders (~6 hours)
caffeinate -s Rscript -e 'source("~/blog/daehl-e/content/post/optimization/param-optimizer.R"); run_full_optimization(sports = c("cross-country"))'

# Run in background (recommended for long runs)
caffeinate -s nohup Rscript -e 'source("~/blog/daehl-e/content/post/optimization/param-optimizer.R"); run_full_optimization(sports = c("cross-country"))' > ~/optimization_xc.log 2>&1 &

# Check progress
tail -f ~/blog/daehl-e/content/post/optimization/logs/optimization_cross-country*.log

# Run full optimization (all 5 sports, both genders, ~60 hours)
caffeinate -s nohup Rscript -e 'source("~/blog/daehl-e/content/post/optimization/param-optimizer.R"); run_full_optimization()' > ~/optimization_full.log 2>&1 &

# Run all sports in the foreground with explicit sport list
caffeinate -s Rscript -e 'source("~/blog/daehl-e/content/post/optimization/param-optimizer.R"); run_full_optimization(sports = c("cross-country", "alpine", "biathlon", "nordic-combined", "skijump"))'

# Run only the sports with the newer team/mixed-event optimizer changes
caffeinate -s Rscript -e 'source("~/blog/daehl-e/content/post/optimization/param-optimizer.R"); run_full_optimization(sports = c("biathlon", "nordic-combined", "skijump"))'

# Run only men or only ladies
caffeinate -s Rscript -e 'source("~/blog/daehl-e/content/post/optimization/param-optimizer.R"); run_full_optimization(sports = c("cross-country"), genders = c("men"))'
caffeinate -s Rscript -e 'source("~/blog/daehl-e/content/post/optimization/param-optimizer.R"); run_full_optimization(sports = c("cross-country"), genders = c("ladies"))'
```

### How Results Get Applied

The optimization workflow has 3 steps:

```
1. optimize_sport()           → Finds best parameters
                              → Saves results to .rds file

2. generate_sport_params_file() → Reads optimization results
                                → Writes sport_params.R

3. Simulation scripts         → Source sport_params.R
                              → Call get_sport_params()
                              → Use optimized values
```

**Automatic:** `run_full_optimization()` does steps 1-2 for all sports.

**Manual:** If you run `optimize_sport()` individually:
```r
# After optimizing each sport separately:
results_xc <- optimize_sport("cross-country")
results_bi <- optimize_sport("biathlon")
# ... etc

# Combine and generate sport_params.R:
all_results <- list(
  "cross-country" = results_xc,
  "biathlon" = results_bi
)
generate_sport_params_file(all_results)
```

### Managing Results

```r
source("~/blog/daehl-e/content/post/optimization/param-optimizer.R")

# List all saved optimization results
list_optimization_results()

# Load previous results (latest by default)
results <- load_optimization_results("cross-country", "men")

# Load specific timestamp
results <- load_optimization_results("cross-country", "men", "20260309_143022")

# Regenerate sport_params.R from saved results
regenerate_sport_params()
```

### Configuration

The optimizer uses three phases with different simulation counts:

| Phase | Simulations | Combinations | Purpose |
|-------|-------------|--------------|---------|
| Coarse | 200 | ~1000 | Initial exploration |
| Fine | 500 | ~50 | Refinement |
| Final | 2000 | ~5 | Validation |

### Output Files

**Results (intermediate):**
```
~/blog/daehl-e/content/post/optimization/results/optimization_{sport}_{gender}_{timestamp}.rds
```

**Final parameters (used by simulation scripts):**
```
~/blog/daehl-e/content/post/shared/sport_params.R
```

### Quick Evaluation (No Optimization)

```r
# Just evaluate current parameters without optimizing
source("~/blog/daehl-e/content/post/optimization/backtest-engine.R")
metrics <- quick_evaluate_defaults("cross-country", gender = "men", n_races = 50)
```

### Compare Optimized vs Default

```r
source("~/blog/daehl-e/content/post/optimization/param-optimizer.R")

# Load optimized params
results <- load_optimization_results("cross-country", "men")
optimized <- results$default$best_params

# Compare performance
comparison <- compare_to_default("cross-country", "men", optimized, n_races = 50)
```

### Viewing Optimization Logs

The optimizer creates detailed logs for every run:

```bash
# Log directory
ls ~/blog/daehl-e/content/post/optimization/logs/

# View latest full optimization log
tail -100 ~/blog/daehl-e/content/post/optimization/logs/full_optimization_*.log

# View specific sport optimization log
tail -100 ~/blog/daehl-e/content/post/optimization/logs/optimization_cross-country_men_*.log

# Watch log in real-time during optimization
tail -f ~/blog/daehl-e/content/post/optimization/logs/*.log
```

**Log Contents Include:**
- Phase timing (data loading, grid search, random search, validation)
- Data quality summaries (chrono records, race counts, date ranges)
- Parameter grid configuration and search space
- Progress updates every 10 evaluations
- New best results highlighted with `*** NEW BEST ***`
- Top 10 results table after each phase
- Per-threshold Brier score breakdown
- Comparison to default parameters
- Final summary with timing breakdown

---

## Championships

### Pre-Championship Predictions

```bash
# Generate championship predictions
./run_champs_predictions.sh

# What it does:
# 1. Update ELO predictions
# 2. Run chrono predictions
# 3. Scrape championship startlists
# 4. Run champs-predictions-simulation.R for each sport
```

### Post-Championship Processing

```bash
# Process results after championship
./champs_script.sh 2026
```

### Run Individual Championship Scripts

```bash
# Cross-Country championships
Rscript ~/blog/daehl-e/content/post/cross-country/drafts/champs-predictions-simulation.R

# Alpine championships
Rscript ~/blog/daehl-e/content/post/alpine/drafts/champs-predictions-simulation.R

# Biathlon championships
Rscript ~/blog/daehl-e/content/post/biathlon/drafts/champs-predictions-simulation.R

# Nordic Combined championships
Rscript ~/blog/daehl-e/content/post/nordic-combined/drafts/champs-predictions-simulation.R

# Ski Jumping championships
Rscript ~/blog/daehl-e/content/post/skijump/drafts/champs-predictions-simulation.R
```

---

## Individual Scripts

### Python Scrapers

```bash
cd ~/ski/elo/python/{sport}/polars

# Scrape race startlists
python startlist-scrape-races.py

# Scrape weekend startlists
python startlist-scrape-weekend.py

# Scrape championship startlists
python startlist-scrape-champs.py

# Check for race schedule changes
python race_scrape.py

# Check for weekend schedule changes
python weekend_scrape.py

# Update all data
python all_update_scrape.py
```

### ELO Scripts

```bash
cd ~/ski/elo/python/{sport}/polars

# Run ELO calculations
./elo_script.sh

# Run ELO predictions
./elo_predict_script.sh

# Run dynamic ELO
./elo_dynamic_script.sh

# Run chrono analysis
python chrono.py

# Run chrono predictions
python chrono_predict.py
```

### Utility Scripts

```bash
# Quick ELO update
./elo_update.sh

# Quick picks generation
./picks.sh

# Update Hugo tables
./tables_update.sh

# Deploy blog
./deploy.sh

# Clean up large files
./daily_cleanup.sh
```

---

## Troubleshooting

### Common Issues

**1. "No races found" errors**

Check if you're in test mode vs production mode:
```bash
cat ~/ski/elo/.env
```

Check race schedules:
```bash
head ~/ski/elo/python/ski/polars/excel365/races.csv
```

**2. R package errors**

Install missing packages:
```r
install.packages(c("dplyr", "tidyr", "openxlsx", "mgcv", "leaps", "logger", "lubridate", "purrr"))
```

**3. Python import errors**

Activate virtual environment:
```bash
source ~/ski/elo/venv/bin/activate
```

**4. Git push failures**

Check authentication:
```bash
git remote -v
git config --list | grep user
```

**5. Simulation script errors**

Check log files:
```bash
# R simulation logs
cat ~/ski/elo/python/ski/polars/excel365/race-picks-simulation/race_picks_simulation.log
```

### View Detailed Logs

```bash
# Master automation logs
ls ~/blog/daehl-e/logs/

# R simulation logs (if enhanced logging enabled)
ls ~/ski/elo/python/{sport}/polars/excel365/race-picks-simulation/

# Python scraper output
# (Check terminal output or redirect to file)
```

### Reset to Clean State

```bash
# Pull latest from both repos
cd ~/blog/daehl-e && git pull
cd ~/ski/elo && git pull

# Clear any cached data (careful!)
rm -rf ~/ski/elo/python/{sport}/polars/excel365/startlist_*.csv
```

---

## Reference: Script Summary

| Script | Purpose | Frequency |
|--------|---------|-----------|
| `master_automation.sh` | Daily orchestrator | Daily (cron) |
| `predict_script.sh` | Race predictions | Race days |
| `score_scrape.sh` | Update ELO ratings | Daily in season |
| `recap_script.sh` | Weekly analysis | Mondays |
| `season_script.sh` | Annual processing | May 1st |
| `run_champs_predictions.sh` | Championship predictions | Before championships |
| `test_harness.py` | Validate pipeline | Manual testing |
| `param-optimizer.R` | Tune parameters | Periodic |

---

## Reference: Sport Directory Mapping

| Sport | Blog Directory | ELO Directory |
|-------|---------------|---------------|
| Cross-Country | `cross-country/` | `ski/` |
| Alpine | `alpine/` | `alpine/` |
| Biathlon | `biathlon/` | `biathlon/` |
| Nordic Combined | `nordic-combined/` | `nordic-combined/` |
| Ski Jumping | `skijump/` | `skijump/` |

---

Last updated: 2026-03-09
