#!/bin/bash
# Daily cleanup script for Git repositories and log files
# Runs git gc and removes old log files

set -e

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

LOG_DIR="$HOME/blog/daehl-e/logs"
LOG_FILE="$LOG_DIR/daily_cleanup.log"

# Create logs directory if it doesn't exist
mkdir -p "$LOG_DIR"

log() {
    local DATE=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$DATE] $1" | tee -a "$LOG_FILE"
}

log "${GREEN}Starting daily cleanup...${NC}"

# Function to run git gc on a repository
cleanup_git_repo() {
    local repo_path=$1
    local repo_name=$2
    
    if [ ! -d "$repo_path" ]; then
        log "${RED}Warning: Repository $repo_name not found at $repo_path${NC}"
        return 1
    fi
    
    log "${YELLOW}Cleaning Git repository: $repo_name${NC}"
    cd "$repo_path" || return 1
    
    # Get size before cleanup
    SIZE_BEFORE=$(du -sh .git 2>/dev/null | cut -f1)
    
    # Run git gc
    if git gc --prune=now 2>&1 | tee -a "$LOG_FILE"; then
        SIZE_AFTER=$(du -sh .git 2>/dev/null | cut -f1)
        log "${GREEN}Git cleanup completed for $repo_name (before: $SIZE_BEFORE, after: $SIZE_AFTER)${NC}"
    else
        log "${RED}Git cleanup failed for $repo_name${NC}"
        return 1
    fi
}

# Function to remove old log files
cleanup_logs() {
    local dir_path=$1
    local dir_name=$2
    
    if [ ! -d "$dir_path" ]; then
        log "${RED}Warning: Directory $dir_name not found at $dir_path${NC}"
        return 1
    fi
    
    log "${YELLOW}Cleaning old log files in: $dir_name${NC}"
    
    # Count and remove log files older than 7 days
    LOG_COUNT=$(find "$dir_path" -name "*.log" -type f -mtime +7 | wc -l)
    
    if [ "$LOG_COUNT" -gt 0 ]; then
        find "$dir_path" -name "*.log" -type f -mtime +7 -delete
        log "${GREEN}Removed $LOG_COUNT old log file(s) from $dir_name${NC}"
    else
        log "${GREEN}No old log files to remove from $dir_name${NC}"
    fi
}

# Cleanup Git repositories
cleanup_git_repo "$HOME/blog/daehl-e" "blog/daehl-e"
cleanup_git_repo "$HOME/ski" "ski"

# Cleanup old log files
cleanup_logs "$HOME/blog/daehl-e" "blog/daehl-e"
cleanup_logs "$HOME/ski" "ski"

# Check disk usage
log "${YELLOW}Current disk usage:${NC}"
df -h / | tail -1 | tee -a "$LOG_FILE"

log "${GREEN}Daily cleanup completed!${NC}"