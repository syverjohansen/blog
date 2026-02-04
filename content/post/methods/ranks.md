---
title: "All-Time Rankings Methodology"
date: 2020-01-01T01:00:00+00:00
draft: false
tags: ["methodology", "rankings", "skiing"]
---

## Overview

All-time rankings are calculated using a weighted points system that awards points for podium finishes (top 3) across major competitions. Points are accumulated over an athlete's entire career, with different weights for different event types.

## Points System

Points are only awarded for **top 3 finishes**. No points are given for 4th place or below.

### Base Point Values

{{< simple-table "methods/ranks/base_points" >}}

### Race Type Modifiers

Points are reduced for team/relay events where results are shared among team members:

{{< simple-table "methods/ranks/race_modifiers" >}}

### Sport-Specific Events

#### Cross-Country
Tour de Ski final standings receive enhanced points: 1st: 20, 2nd: 10, 3rd: 5

#### Ski Jumping
Additional prestige events:
- **4 Hills Tournament**: 1st: 20, 2nd: 10, 3rd: 5
- **Ski Flying World Championships**: 1st: 20, 2nd: 10, 3rd: 5

## Complete Points Tables

### Alpine (Individual Only)

{{< simple-table "methods/ranks/alpine_points" >}}

### Biathlon

{{< simple-table "methods/ranks/biathlon_points" >}}

### Cross-Country

{{< simple-table "methods/ranks/crosscountry_points" >}}

### Nordic Combined

{{< simple-table "methods/ranks/nordic_combined_points" >}}

### Ski Jumping

{{< simple-table "methods/ranks/skijump_points" >}}

## Current Season Handling

During an active season, current season standings are excluded from the rankings. This prevents incomplete season data from affecting career totals. Once the season ends, standings are included.

## Output Columns

{{< simple-table "methods/ranks/output_columns" >}}

### Sport-Specific Columns

- **Cross-Country**: Adds `Tour` column for Tour de Ski points
- **Ski Jumping**: Adds `4 Hills` and `Ski Flying WC` columns
