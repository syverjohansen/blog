---
title: "All-Time Rankings Methodology"
date: 2026-02-01T02:00:00+00:00
draft: false
tags: ["methodology", "rankings", "skiing"]
---

## Overview

All-time rankings are calculated using a weighted points system that awards points for podium finishes (top 3) across major competitions. Points are accumulated over an athlete's entire career, with different weights for different event types.

## Points System

Points are only awarded for **top 3 finishes**. No points are given for 4th place or below.

### Base Point Values

| Place | Olympics | World Championships | World Cup | Standings |
|-------|----------|---------------------|-----------|-----------|
| 1st | 80 | 40 | 8 | 80 |
| 2nd | 40 | 20 | 4 | 40 |
| 3rd | 20 | 10 | 4 | 20 |

### Race Type Modifiers

Points are reduced for team/relay events where results are shared among team members:

| Race Type | Points Multiplier |
|-----------|-------------------|
| Individual | 1.0× (full points) |
| Team Sprint / Single Mixed Relay | 0.5× |
| Relay / Team | 0.25× |

### Sport-Specific Events

#### Cross-Country
Tour de Ski final standings receive enhanced points:
- 1st: 20, 2nd: 10, 3rd: 5

#### Ski Jumping
Additional prestige events:
- **4 Hills Tournament**: 1st: 20, 2nd: 10, 3rd: 5
- **Ski Flying World Championships**: 1st: 20, 2nd: 10, 3rd: 5

## Complete Points Tables

### Alpine (Individual Only)

| Event | 1st | 2nd | 3rd |
|-------|-----|-----|-----|
| Olympics | 80 | 40 | 20 |
| World Championships | 40 | 20 | 10 |
| World Cup | 8 | 4 | 4 |
| Standings | 80 | 40 | 20 |

### Biathlon

| Event | Individual | Single Mixed Relay | Relay/Mixed Relay |
|-------|------------|-------------------|-------------------|
| Olympics | 80/40/20 | 40/20/10 | 20/10/5 |
| World Championships | 40/20/10 | 20/10/5 | 10/5/2.5 |
| World Cup | 8/4/4 | 4/2/2 | 2/1/1 |

### Cross-Country

| Event | Individual | Team Sprint | Relay |
|-------|------------|-------------|-------|
| Olympics | 80/40/20 | 40/20/10 | 20/10/5 |
| World Championships | 40/20/10 | 20/10/5 | 10/5/2.5 |
| Tour de Ski | 20/10/5 | 20/10/5 | 20/10/5 |
| World Cup | 8/4/4 | 4/2/2 | 2/1/1 |

### Nordic Combined

| Event | Individual | Team Sprint | Team |
|-------|------------|-------------|------|
| Olympics | 80/40/20 | 40/20/10 | 20/10/5 |
| World Championships | 40/20/10 | 20/10/5 | 10/5/2.5 |
| World Cup | 8/4/4 | 4/2/2 | 2/1/1 |

### Ski Jumping

| Event | Individual | Team |
|-------|------------|------|
| Olympics | 80/40/20 | 20/10/5 |
| World Championships | 40/20/10 | 10/5/2.5 |
| 4 Hills Tournament | 20/10/5 | 10/5/2.5 |
| Ski Flying WC | 20/10/5 | 10/5/2.5 |
| World Cup | 8/4/4 | 2/1/1 |

## Current Season Handling

During an active season, current season standings are excluded from the rankings. This prevents incomplete season data from affecting career totals. Once the season ends, standings are included.

## Output Columns

| Column | Description |
|--------|-------------|
| Skier | Athlete name |
| Nation | Country code |
| Olympics | Points from Olympic Games |
| WSC | Points from World Championships |
| WC | Points from World Cup events |
| Table | Points from overall standings |
| Total | Sum of all points |
| From | First season with points |
| To | Most recent season with points |
| Age | Current age |

### Sport-Specific Columns

- **Cross-Country**: Adds `Tour` column for Tour de Ski points
- **Ski Jumping**: Adds `4 Hills` and `Ski Flying WC` columns
