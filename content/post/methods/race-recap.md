---
title: "Weekly Recap Methodology"
date: 2020-01-01T01:00:00+00:00
draft: false
tags: ["methodology", "skiing", "weekly-recap"]
---

## Overview

The weekly recap system provides three key analytical components for tracking World Cup season progress:

1. **Elo Change Tracking** - Monitors week-over-week changes in athlete Elo ratings
2. **Season Simulation** - Monte Carlo simulation of remaining season outcomes
3. **Magic Numbers** - Mathematical elimination thresholds for the overall standings

This methodology applies to all winter sports covered: Alpine, Biathlon, Cross-Country, Nordic Combined, and Ski Jumping.

---

## Elo Change Tracking

### Purpose

Track how athlete Elo ratings change week-to-week to identify trending performers and athletes experiencing rating declines.

### Calculation

For each athlete who competed in the past week:

```
Elo_Change = Current_Elo - Previous_Week_Elo
```

Where:
- `Current_Elo` is the athlete's Elo rating after their most recent race
- `Previous_Week_Elo` is the athlete's Elo rating from 7+ days ago

### Data Processing

1. Load chronological race data for each gender
2. Filter to athletes who competed in the current week
3. For each athlete, compare their current Elo to their Elo from before the past week
4. Only include athletes with non-zero Elo changes
5. Sort by Elo change (descending) to highlight top gainers and losers

### Output

The weekly Elo change report includes:
- **Skier**: Athlete name
- **Nation**: Country code
- **Current Elo**: Most recent overall Elo rating
- **Previous Elo**: Elo rating from 7+ days ago
- **Change**: Difference between current and previous Elo

---

## Season Simulation

### Purpose

Estimate the probability distribution of final season standings by simulating the remaining races thousands of times using Monte Carlo methods.

### Methodology

#### Remaining Races Calculation

The system dynamically calculates remaining races from the official race calendar by:

1. Reading the races CSV file containing the full season schedule
2. Filtering for races with `Date >= today` (using UTC time)
3. Excluding championship races (`Championship != 1`)
4. Excluding team events (relays, team sprints)
5. Categorizing races by type and point value

**Cross-Country Point Values:**
- World Cup races: 100 points maximum
- Stage races: 50 points maximum
- Tour de Ski stages: 300 points maximum

**Other Sports:**
- Alpine: 100 points maximum per race
- Biathlon: 90 points maximum per race (60 base + 30 bonus)
- Nordic Combined: 100 points maximum per race
- Ski Jumping: 100 points maximum per race

#### Historical Performance Modeling

For each athlete, the simulation uses their historical race results to model expected performance. The system:

1. Retrieves the athlete's points history for each race type (distance/sprint categories)
2. If the athlete has discipline-specific history, uses weighted recent performances
3. If not, falls back to overall race type history

#### Single Race Simulation

Each race is simulated using:

```
Simulated_Points = Mean_Historical_Points + N(0, σ)
```

Where:
- `Mean_Historical_Points` is the weighted average of recent performances
- `N(0, σ)` is normally distributed noise with `σ = max(5, Mean_Points × 0.15)`
- Results are bounded to `[0, Max_Points]` for the race type

The 15% coefficient of variation captures the inherent uncertainty in race outcomes while the minimum standard deviation of 5 points ensures variability even for low-scoring athletes.

#### Monte Carlo Simulation

The full simulation process:

1. Initialize each athlete's total with their current season points
2. For each of `n` simulations (default: 100-500):
   - For each remaining race:
     - Simulate points for all athletes
     - Add simulated points to running totals
   - Record final standings
3. Calculate win probability as `(Simulations_Won / Total_Simulations)`

#### Output Metrics

The simulation produces:
- **Current Points**: Actual points in current standings
- **Mean Final Points**: Average total points across all simulations
- **Mean Points Gained**: Expected points from remaining races
- **Win Probability**: Percentage of simulations where athlete finishes first

---

## Magic Numbers

### Purpose

Calculate the "magic number" - the minimum points the current leader needs to mathematically clinch the overall title over each competitor.

### Mathematical Definition

For each athlete still mathematically alive in the standings:

```
Magic_Number = max(0, Athlete_Points + Points_Remaining + 1 - Leader_Points)
```

Where:
- `Athlete_Points` is the competitor's current season points
- `Points_Remaining` is the maximum points available in remaining races
- `Leader_Points` is the current leader's season points
- The `+1` ensures the leader must have strictly more points

### Interpretation

The magic number answers: "If this athlete wins every remaining race, how many points does the leader need to clinch the title?"

**Example:**
- Leader has 1,500 points
- Athlete B has 1,200 points
- 600 points remaining in season
- Magic Number = 1,200 + 600 + 1 - 1,500 = **301**

This means if the leader gains 301 more points than Athlete B in remaining races, Athlete B is mathematically eliminated.

### Mathematical Elimination

An athlete is **mathematically eliminated** when:

```
Max_Possible_Points = Current_Points + Points_Remaining
Mathematical_Chance = (Max_Possible_Points > Leader_Points)
```

If `Mathematical_Chance = FALSE`, the athlete cannot win even by winning all remaining races.

### Points Remaining Calculation

**Cross-Country:**
```
Total_Remaining = (WC_Races × 100) + (Stage_Races × 50) + (TdS_Stages × 300)
```

**Alpine:**
```
Total_Remaining = Total_Races × 100
```

**Biathlon:**
```
Total_Remaining = Total_Races × 90
```

**Nordic Combined / Ski Jumping:**
```
Total_Remaining = Total_Races × 100
```

### Output

The magic numbers report includes only athletes with a mathematical chance, showing:
- **Skier**: Athlete name
- **Rank**: Current position in standings
- **Points**: Current season points
- **Magic #**: Points leader needs to clinch over this athlete

---

## Technical Notes

### Date Handling

All date comparisons use UTC time to ensure consistency across time zones. The system:
- Parses race dates in `MM/DD/YYYY` format
- Compares against `Sys.time()` converted to UTC date
- Uses the same date boundary for all calculations

### Race Type Categorization

Each sport has specific race type mappings that determine:
1. Which races count toward the overall standings
2. The maximum points available per race
3. How historical performances are categorized for simulation

**Cross-Country Techniques:**
- "C" = Classic
- "F" = Freestyle
- "P" = Skiathlon (pursuit format)
- "" = Unspecified (typically mass start or skiathlon)

### Simulation Stability

The Monte Carlo simulation uses:
- Minimum 100 iterations for quick estimates
- 500+ iterations for stable probability estimates
- Seeded random number generation for reproducibility when needed

### Edge Cases

The system handles:
- Athletes with no historical data (assigned 0 expected points)
- Athletes not in current standings (assigned 0 current points)
- Empty remaining race schedules (returns current standings as final)
- Tied points (uses minimum rank assignment)
