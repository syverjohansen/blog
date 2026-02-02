---
title: "Elo Calculations Methodology"
date: 2020-01-01T01:00:00+00:00
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
E_i = Sum of (1 / (1 + 10^((R_j - R_i) / 400)))
```

where R_i and R_j are the Elo ratings of athletes i and j. This is computed pairwise against all other competitors.

### Actual Score Calculation

Actual score S is based on finishing position:
- Win against opponent: 1 point
- Draw (same place): 0.5 points
- Loss: 0 points

For athlete finishing in place P with n competitors: S = (athletes finishing behind) + 0.5 x (athletes tied)

### Rating Update

```
New Elo = Old Elo + K x (S - E)
```

### Example Race

Consider a 4-person race with K=3:

{{< datatable "methods/elo/example_race" >}}

**How it works:**
- Athlete A (1500 Elo) wins. Expected to beat ~2.1 opponents on average, actually beat 3. Gains 2.7 points.
- Athlete B (1450 Elo) finishes 2nd. Expected ~1.8, got 2. Small gain of 0.6.
- Athlete C (1400 Elo) finishes 3rd. Expected ~1.5, got 1. Loses 1.5 points.
- Athlete D (1350 Elo) finishes last. Expected ~0.6, got 0. Loses 1.8 points.

### Dynamic K-Factor

K varies by season based on participation levels. Seasons with fewer participants have higher K values (ratings change more), while seasons with more participants have lower K values (ratings are more stable).

K is calculated as the ratio of maximum historical participation to current season participation, bounded between 1 and 5.

### Season Discount

At season end, all ratings regress toward the base:

```
End Elo = Elo x 0.85 + 1300 x 0.15
```

This prevents rating inflation and accounts for offseason changes in athlete ability.

**Example:**

{{< datatable "methods/elo/season_discount_example" >}}

Notice how top performers lose more (1600 -> 1555, loss of 45), while struggling athletes actually gain (1200 -> 1245, gain of 45). This regression to the mean helps account for form changes over the offseason.

## Sport-Specific K Adjustments

Team and combined events receive reduced K-factors to account for shared responsibility:

{{< datatable "methods/elo/k_adjustments" >}}

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

## Data Sources

Elo ratings are calculated from World Cup, World Championship, and Olympic results:
- **Alpine, Cross-Country, Nordic Combined, Ski Jumping**: FIS (International Ski and Snowboard Federation)
- **Biathlon**: IBU (International Biathlon Union)
