---
title: "Elo Calculations Methodology"
date: 2026-02-01T01:00:00+00:00
draft: false
tags: ["methodology", "elo", "ratings", "skiing"]
---

## Overview

Elo ratings are calculated for all winter sports using a multi-player extension of the standard Elo formula. The system tracks athlete performance over time, updating ratings after each race based on actual vs expected results.

## Core Algorithm

### Base Parameters

- **Base Elo**: 1300 for all new athletes
- **K-factor**: Dynamic, calculated per season
- **Season Discount**: 0.85 regression toward base

### Expected Score Calculation

For a race with n athletes, the expected score for athlete i against all (n-1) opponents is:

```
E_i = Σ (1 / (1 + 10^((R_j - R_i) / 400)))
```

where R_i and R_j are the Elo ratings of athletes i and j. This is computed pairwise against all other competitors.

### Actual Score Calculation

Actual score S is based on finishing position:
- Win against opponent: 1 point
- Draw (same place): 0.5 points
- Loss: 0 points

For athlete finishing in place P with n competitors: S = (athletes finishing behind) + 0.5 × (athletes tied)

### Rating Update

```
New Elo = Old Elo + K × (S - E)
```

### Dynamic K-Factor

K varies by season based on participation levels. Seasons with fewer participants have higher K values (ratings change more), while seasons with more participants have lower K values (ratings are more stable).

K is calculated as the ratio of maximum historical participation to current season participation, bounded between 1 and 5.

### Season Discount

At season end, all ratings regress toward the base:

```
End Elo = Pre-race Elo × 0.85 + 1300 × 0.15
```

This prevents rating inflation and accounts for offseason changes in athlete ability.

## Sport-Specific K Adjustments

Team and combined events receive reduced K-factors to account for shared responsibility:

| Sport | Event Type | K Adjustment |
|-------|------------|--------------|
| Alpine | Combined | K × 0.8 |
| Biathlon | Relay, Mixed Relay | K / 4 |
| Biathlon | Single Mixed Relay | K / 2 |
| Cross-Country | Relay | K / 4 |
| Cross-Country | Team Sprint | K / 2 |
| Nordic Combined | Team | K / 4 |
| Nordic Combined | Team Sprint | K / 2 |
| Ski Jumping | Team Events | K / 4 |

## Discipline-Specific Elos

Each sport calculates multiple discipline-specific Elo ratings:

### Alpine
Overall, Downhill, Super G, Giant Slalom, Slalom, Combined, Tech (Slalom + Giant Slalom), Speed (Downhill + Super G)

### Biathlon
Overall, Sprint, Pursuit, Individual, Mass Start

### Cross-Country
Overall, Distance, Distance Classic, Distance Freestyle, Sprint, Sprint Classic, Sprint Freestyle, Classic, Freestyle

### Nordic Combined
Overall, Individual, Individual Compact, Sprint, Mass Start

### Ski Jumping
Overall, Small Hill, Medium Hill, Normal Hill, Large Hill, Flying

## Pelo vs Elo

- **Pelo** (Previous Elo): Rating before the race
- **Elo**: Rating after the race

The website displays both values for each race, allowing you to see how an athlete's rating changed based on their performance.

## Data Sources

Elo ratings are calculated from World Cup, World Championship, and Olympic results:
- **Alpine, Cross-Country, Nordic Combined, Ski Jumping**: FIS (International Ski and Snowboard Federation)
- **Biathlon**: IBU (International Biathlon Union)
