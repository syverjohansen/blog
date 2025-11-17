import os
#!/usr/bin/env python3
"""
Sporcle Quiz Data Generator

This script generates CSV files for various Sporcle quiz types across winter sports:
- All-Time 100: Top 100 athletes by career points
- Current Year 50: Top 50 current ELO leaders  
- King/Queen: Athletes who held #1 ELO with date ranges
- Days on Top: Total days each athlete spent at #1 ELO
- Top 10 Seasonal: Top 10 by season for each discipline

Usage:
    python sporcle.py [sport] [gender] [quiz_type]
    
    sport: alpine, biathlon, cross-country, nordic-combined, skijump
    gender: M, L (Men, Ladies)
    quiz_type: all_time_100, current_year_50, king_queen, days_on_top, top10_seasonal
"""

import pandas as pd
import numpy as np
import os
import sys
import datetime
from pathlib import Path
import argparse

# Sport-specific discipline mappings
SPORT_DISCIPLINES = {
    'alpine': {
        'columns': ['Elo', 'Downhill_Elo', 'Super G_Elo', 'Giant Slalom_Elo', 'Slalom_Elo', 'Combined_Elo', 'Tech_Elo', 'Speed_Elo'],
        'names': ['all', 'downhill', 'superg', 'gs', 'slalom', 'combined', 'tech', 'speed']
    },
    'cross-country': {
        'columns': ['Elo', 'Distance_Elo', 'Distance_C_Elo', 'Distance_F_Elo', 'Sprint_Elo', 'Sprint_F_Elo', 'Sprint_C_Elo', 'Classic_Elo', 'Freestyle_Elo'],
        'names': ['all', 'distance', 'distance_classic', 'distance_freestyle', 'sprint', 'sprint_freestyle', 'sprint_classic', 'classic', 'freestyle']
    },
    'biathlon': {
        'columns': ['Elo', 'Sprint_Elo', 'Individual_Elo', 'Pursuit_Elo', 'Mass Start_Elo', 'MassStart_Elo', 'Relay_Elo'],
        'names': ['all', 'sprint', 'individual', 'pursuit', 'mass_start', 'massstart', 'relay']
    },
    'nordic-combined': {
        'columns': ['Elo', 'Gundersen_Elo', 'Team_Elo', 'Sprint_Elo'],
        'names': ['all', 'gundersen', 'team', 'sprint']
    },
    'skijump': {
        'columns': ['Elo', 'Normal Hill_Elo', 'Large Hill_Elo', 'Flying Hill_Elo', 'Team_Elo'],
        'names': ['all', 'normal_hill', 'large_hill', 'flying_hill', 'team']
    }
}

def normalize_special_characters_single(text):
    """Convert special characters to single letter equivalents"""
    if pd.isna(text):
        return text
    
    # Character mapping for single letters (ø->o, å->a, etc.)
    char_map = {
        # Norwegian/Danish
        'æ': 'a', 'Æ': 'A',
        'ø': 'o', 'Ø': 'O', 
        'å': 'a', 'Å': 'A',
        
        # Swedish/Finnish
        'ä': 'a', 'Ä': 'A',
        'ö': 'o', 'Ö': 'O',
        
        # German/Austrian
        'ü': 'u', 'Ü': 'U',
        'ß': 'ss',
        
        # French
        'é': 'e', 'É': 'E',
        'è': 'e', 'È': 'E',
        'ê': 'e', 'Ê': 'E',
        'ë': 'e', 'Ë': 'E',
        'à': 'a', 'À': 'A',
        'â': 'a', 'Â': 'A',
        'ç': 'c', 'Ç': 'C',
        'î': 'i', 'Î': 'I',
        'ï': 'i', 'Ï': 'I',
        'ô': 'o', 'Ô': 'O',
        'û': 'u', 'Û': 'U',
        'ù': 'u', 'Ù': 'U',
        
        # Italian/Spanish
        'á': 'a', 'Á': 'A',
        'í': 'i', 'Í': 'I',
        'ó': 'o', 'Ó': 'O',
        'ú': 'u', 'Ú': 'U',
        'ñ': 'n', 'Ñ': 'N',
        
        # Czech/Slovak
        'č': 'c', 'Č': 'C',
        'ď': 'd', 'Ď': 'D',
        'ň': 'n', 'Ň': 'N',
        'ř': 'r', 'Ř': 'R',
        'š': 's', 'Š': 'S',
        'ť': 't', 'Ť': 'T',
        'ž': 'z', 'Ž': 'Z',
        'ý': 'y', 'Ý': 'Y',
        
        # Polish
        'ą': 'a', 'Ą': 'A',
        'ć': 'c', 'Ć': 'C',
        'ę': 'e', 'Ę': 'E',
        'ł': 'l', 'Ł': 'L',
        'ń': 'n', 'Ń': 'N',
        'ś': 's', 'Ś': 'S',
        'ź': 'z', 'Ź': 'Z',
        'ż': 'z', 'Ż': 'Z',
        
        # Russian (basic Cyrillic)
        'а': 'a', 'А': 'A',
        'б': 'b', 'Б': 'B',
        'в': 'v', 'В': 'V',
        'г': 'g', 'Г': 'G',
        'д': 'd', 'Д': 'D',
        'е': 'e', 'Е': 'E',
        'ё': 'e', 'Ё': 'E',
        'ж': 'z', 'Ж': 'Z',
        'з': 'z', 'З': 'Z',
        'и': 'i', 'И': 'I',
        'й': 'y', 'Й': 'Y',
        'к': 'k', 'К': 'K',
        'л': 'l', 'Л': 'L',
        'м': 'm', 'М': 'M',
        'н': 'n', 'Н': 'N',
        'о': 'o', 'О': 'O',
        'п': 'p', 'П': 'P',
        'р': 'r', 'Р': 'R',
        'с': 's', 'С': 'S',
        'т': 't', 'Т': 'T',
        'у': 'u', 'У': 'U',
        'ф': 'f', 'Ф': 'F',
        'х': 'h', 'Х': 'H',
        'ц': 'c', 'Ц': 'C',
        'ч': 'c', 'Ч': 'C',
        'ш': 's', 'Ш': 'S',
        'щ': 's', 'Щ': 'S',
        'ъ': '', 'Ъ': '',
        'ы': 'y', 'Ы': 'Y',
        'ь': '', 'Ь': '',
        'э': 'e', 'Э': 'E',
        'ю': 'u', 'Ю': 'U',
        'я': 'a', 'Я': 'A'
    }
    
    # Apply character replacements
    result = str(text)
    for old_char, new_char in char_map.items():
        result = result.replace(old_char, new_char)
    
    return result

def normalize_special_characters(text):
    """Convert special characters to English equivalents for Sporcle compatibility"""
    if pd.isna(text):
        return text
    
    # Character mapping for Nordic/European characters
    char_map = {
        # Norwegian/Danish
        'æ': 'ae', 'Æ': 'Ae',
        'ø': 'oe', 'Ø': 'Oe', 
        'å': 'aa', 'Å': 'Aa',
        
        # Swedish/Finnish
        'ä': 'ae', 'Ä': 'Ae',
        'ö': 'oe', 'Ö': 'Oe',
        
        # German/Austrian
        'ü': 'ue', 'Ü': 'Ue',
        'ß': 'ss',
        
        # French
        'é': 'e', 'É': 'E',
        'è': 'e', 'È': 'E',
        'ê': 'e', 'Ê': 'E',
        'ë': 'e', 'Ë': 'E',
        'à': 'a', 'À': 'A',
        'â': 'a', 'Â': 'A',
        'ç': 'c', 'Ç': 'C',
        'î': 'i', 'Î': 'I',
        'ï': 'i', 'Ï': 'I',
        'ô': 'o', 'Ô': 'O',
        'û': 'u', 'Û': 'U',
        'ù': 'u', 'Ù': 'U',
        
        # Italian/Spanish
        'á': 'a', 'Á': 'A',
        'í': 'i', 'Í': 'I',
        'ó': 'o', 'Ó': 'O',
        'ú': 'u', 'Ú': 'U',
        'ñ': 'n', 'Ñ': 'N',
        
        # Czech/Slovak
        'č': 'c', 'Č': 'C',
        'ď': 'd', 'Ď': 'D',
        'ň': 'n', 'Ň': 'N',
        'ř': 'r', 'Ř': 'R',
        'š': 's', 'Š': 'S',
        'ť': 't', 'Ť': 'T',
        'ž': 'z', 'Ž': 'Z',
        'ý': 'y', 'Ý': 'Y',
        
        # Polish
        'ą': 'a', 'Ą': 'A',
        'ć': 'c', 'Ć': 'C',
        'ę': 'e', 'Ę': 'E',
        'ł': 'l', 'Ł': 'L',
        'ń': 'n', 'Ń': 'N',
        'ś': 's', 'Ś': 'S',
        'ź': 'z', 'Ź': 'Z',
        'ż': 'z', 'Ż': 'Z',
        
        # Russian (basic Cyrillic)
        'а': 'a', 'А': 'A',
        'б': 'b', 'Б': 'B',
        'в': 'v', 'В': 'V',
        'г': 'g', 'Г': 'G',
        'д': 'd', 'Д': 'D',
        'е': 'e', 'Е': 'E',
        'ё': 'e', 'Ё': 'E',
        'ж': 'zh', 'Ж': 'Zh',
        'з': 'z', 'З': 'Z',
        'и': 'i', 'И': 'I',
        'й': 'y', 'Й': 'Y',
        'к': 'k', 'К': 'K',
        'л': 'l', 'Л': 'L',
        'м': 'm', 'М': 'M',
        'н': 'n', 'Н': 'N',
        'о': 'o', 'О': 'O',
        'п': 'p', 'П': 'P',
        'р': 'r', 'Р': 'R',
        'с': 's', 'С': 'S',
        'т': 't', 'Т': 'T',
        'у': 'u', 'У': 'U',
        'ф': 'f', 'Ф': 'F',
        'х': 'h', 'Х': 'H',
        'ц': 'ts', 'Ц': 'Ts',
        'ч': 'ch', 'Ч': 'Ch',
        'ш': 'sh', 'Ш': 'Sh',
        'щ': 'sch', 'Щ': 'Sch',
        'ъ': '', 'Ъ': '',
        'ы': 'y', 'Ы': 'Y',
        'ь': '', 'Ь': '',
        'э': 'e', 'Э': 'E',
        'ю': 'yu', 'Ю': 'Yu',
        'я': 'ya', 'Я': 'Ya'
    }
    
    # Apply character replacements
    result = str(text)
    for old_char, new_char in char_map.items():
        result = result.replace(old_char, new_char)
    
    return result

def format_skier_multiple_answers(original_name):
    """Format skier name with multiple answer options for Sporcle"""
    if pd.isna(original_name):
        return original_name
    
    # Extract last name (assume it's the last word)
    name_parts = str(original_name).split()
    if len(name_parts) == 0:
        return original_name
    
    last_name = name_parts[-1]
    
    # Create the three versions
    version1 = original_name  # Original with special characters
    version2 = normalize_special_characters(last_name)  # Last name with ø->oe, å->aa
    version3 = normalize_special_characters_single(last_name)  # Last name with ø->o, å->a
    
    # Remove duplicates while preserving order
    versions = [version1, version2, version3]
    unique_versions = []
    for v in versions:
        if v not in unique_versions:
            unique_versions.append(v)
    
    # Join with forward slashes
    return '/'.join(unique_versions)

def create_output_directory(sport):
    """Create output directory structure if it doesn't exist"""
    output_dir = Path(fos.path.expanduser("~/blog/daehl-e/static/python/sporcle/{sport}/excel365"))
    output_dir.mkdir(parents=True, exist_ok=True)
    return output_dir

def generate_all_time_100(sport, gender):
    """Generate All-Time 100 CSV from ranks.csv"""
    print(f"Generating All-Time 100 for {sport} {gender}...")
    
    # Load ranks data
    ranks_file = fos.path.expanduser("~/blog/daehl-e/static/python/{sport}/excel365/{gender}/ranks.csv")
    
    try:
        df = pd.read_csv(ranks_file)
        
        # Take top 100 by Total points
        top_100 = df.head(100)
        
        # Create Active Seasons column (From-To or From-Present)
        def format_active_seasons(row):
            from_year = row['From']
            to_year = row['To']
            
            # Check if 'To' year is current year or recent (indicating still active)
            current_year = 2025
            if to_year >= current_year - 1:  # If within last year, consider present
                return f"({from_year}-Present)"
            else:
                return f"({from_year}-{to_year})"
        
        top_100_clean = top_100[['Skier', 'Nation']].copy()
        top_100_clean.insert(0, 'Rank', range(1, len(top_100_clean) + 1))
        
        # Merge Nation and Active Seasons into one column
        active_seasons = top_100.apply(format_active_seasons, axis=1)
        top_100_clean['Nation'] = top_100_clean['Nation'] + ' ' + active_seasons
        
        # Format skier names with multiple answer options and normalize Nation
        top_100_clean['Skier'] = top_100_clean['Skier'].apply(format_skier_multiple_answers)
        top_100_clean['Nation'] = top_100_clean['Nation'].apply(normalize_special_characters)
        
        # Create output directory
        output_dir = create_output_directory(sport)
        
        # Generate filename
        gender_name = "ladies" if gender == "L" else "men"
        filename = f"alltime100_{gender_name}_{sport}.csv"
        
        # Save CSV with UTF-8 BOM encoding for proper Excel display
        output_path = output_dir / filename
        top_100_clean.to_csv(output_path, index=False, encoding='utf-8-sig')
        print(f"  Saved: {output_path}")
        
    except FileNotFoundError:
        print(f"  Warning: {ranks_file} not found, skipping...")
    except Exception as e:
        print(f"  Error: {e}")

def generate_current_year_50(sport, gender):
    """Generate Current Year 50 CSV from latest ELO rankings"""
    print(f"Generating Current Year 50 for {sport} {gender}...")
    
    # Map sport names for ski directory structure
    ski_sport_map = {
        'cross-country': 'ski',
        'alpine': 'alpine',
        'biathlon': 'biathlon', 
        'nordic-combined': 'nordic-combined',
        'skijump': 'skijump'
    }
    ski_sport = ski_sport_map.get(sport, sport)
    
    # Load current ELO data
    elo_file = fos.path.expanduser("~/ski/elo/python/{ski_sport}/polars/excel365/{gender}.csv")
    
    try:
        df = pd.read_csv(elo_file)
        
        # Get the highest (most recent) season
        if 'Season' in df.columns:
            latest_season = df['Season'].max()
            # Filter for latest season and Race==0 (season start rankings)
            season_data = df[(df['Season'] == latest_season) & (df['Race'] == 0)]
            # Sort by Elo descending and take top 50
            top_50 = season_data.sort_values('Elo', ascending=False).head(50)
        else:
            # Fallback if no Season column
            top_50 = df.sort_values('Elo', ascending=False).head(50)
        
        # Create rank column and select only required columns: Rank, Skier, Nation
        top_50_clean = top_50[['Skier', 'Nation']].copy()
        top_50_clean.insert(0, 'Rank', range(1, len(top_50_clean) + 1))
        
        # Format skier names with multiple answer options and normalize Nation
        top_50_clean['Skier'] = top_50_clean['Skier'].apply(format_skier_multiple_answers)
        top_50_clean['Nation'] = top_50_clean['Nation'].apply(normalize_special_characters)
        
        # Create output directory
        output_dir = create_output_directory(sport)
        
        # Generate filename
        gender_name = "ladies" if gender == "L" else "men"
        filename = f"current50_{gender_name}_{sport}.csv"
        
        # Save CSV with UTF-8 BOM encoding for proper Excel display
        output_path = output_dir / filename
        top_50_clean.to_csv(output_path, index=False, encoding='utf-8-sig')
        print(f"  Saved: {output_path}")
        
    except FileNotFoundError:
        print(f"  Warning: {elo_file} not found, skipping...")
    except Exception as e:
        print(f"  Error: {e}")

def king_queen(df, col):
    """
    Find periods when athletes held the #1 ELO ranking
    This is the provided logic from the user
    """
    df = pd.read_pickle(df) if isinstance(df, str) else df
    name = []
    ids = []
    start_date = []
    end_date = []
    top_elo = 0
    top_id = 0
    nation = []
    df = df.sort_values(['Date', 'Race', col], ascending=[True, True, False])
    seasons = df['Season'].unique()
    
    for a in range(len(seasons)):
        seasondf = df.loc[df['Season']==seasons[a]]
        id_list = list(seasondf['ID'].unique())
        
        if(len(ids)==0):
            top_id = 0
        elif(top_id in id_list):
            pass
        else:
            top_id = 0

        races = seasondf['Race'].unique()
        for b in range(len(races)):
            racedf = seasondf.loc[seasondf['Race']==races[b]]
            max_elo = max(racedf[col])
            
            if(top_id!=0):
                top_id_df = seasondf.loc[seasondf['ID']==top_id]
                top_id_df = top_id_df.loc[top_id_df['Race']<=races[b]]
                
                try:
                    top_elo = top_id_df[col].iloc[-1]
                except:
                    if(len(top_id_df)==0):
                        top_id_df = seasondf.loc[seasondf['ID']==top_id]
                        top_elo = top_id_df['Pelo'].iloc[0]
            else:
                print(seasons[a])
                top_elo = 0
                
            if(max_elo>top_elo):
                row = racedf.loc[racedf[col]==max_elo]
                ski_id = row['ID'].iloc[0]
                
                if(len(ids)==0):
                    ids.append(ski_id)
                    name.append(row['Skier'].iloc[0])
                    nation.append(row['Nation'].iloc[0])
                    start_date.append(row['Date'].iloc[0])
                    if(len(start_date)>1):
                        end_date.append(row['Date'].iloc[0])

                elif(ski_id!=ids[-1]):
                    ids.append(ski_id)
                    name.append(row['Skier'].iloc[0])
                    nation.append(row['Nation'].iloc[0])
                    start_date.append(row['Date'].iloc[0])
                    if(len(start_date)>1):
                        end_date.append(row['Date'].iloc[0])
                    
                top_id = ids[-1]
        top_id = ids[-1]

    str_start_date = start_date
    str_end_date = end_date

    for a in range(len(str_start_date)):        
        str_start_date[a] = str(str_start_date[a])[0:10]
    for a in range(len(str_end_date)):
        str_end_date[a] = str(str_end_date[a])[0:10]
    str_end_date.append("Present")
    
    dates = []
    for a in range(len(str_start_date)):
        date = "("+str_start_date[a]+" - "+str_end_date[a]+")"
        dates.append(date)

    df_result = pd.DataFrame()
    df_result['start_date'] = str_start_date
    df_result['end_date'] = str_end_date
    df_result['name'] = name
    df_result['id'] = ids
    df_result['nation'] = nation
    df_result['dates'] = dates

    for a in range(len(start_date)):
        date_parts = start_date[a].split("-")
        year = int(date_parts[0])
        month = int(date_parts[1])
        day = int(date_parts[2])
        start_date[a] = datetime.date(year, month, day)
        
    end_date = end_date[0:(len(end_date)-1)]
    for a in range(len(end_date)):
        date_parts = end_date[a].split("-")
        year = int(date_parts[0])
        month = int(date_parts[1])
        day = int(date_parts[2])
        end_date[a] = datetime.date(year, month, day)
    end_date.append(datetime.date.today())
    
    df_result['start_date'] = start_date
    df_result['end_date'] = end_date
    df_result['days'] = df_result['end_date'] - df_result['start_date']

    return df_result

def generate_king_queen(sport, gender):
    """Generate King/Queen ELO CSV showing periods of #1 ranking"""
    print(f"Generating King/Queen for {sport} {gender}...")
    
    # Map sport names for ski directory structure
    ski_sport_map = {
        'cross-country': 'ski',
        'alpine': 'alpine',
        'biathlon': 'biathlon', 
        'nordic-combined': 'nordic-combined',
        'skijump': 'skijump'
    }
    ski_sport = ski_sport_map.get(sport, sport)
    
    # Load chrono data
    gender_name = "ladies" if gender == "L" else "men"
    chrono_file = fos.path.expanduser("~/ski/elo/python/{ski_sport}/polars/excel365/{gender_name}_chrono.csv")
    
    try:
        df = pd.read_csv(chrono_file)
        
        # Use the king_queen function with overall Elo
        result_df = king_queen(df, 'Elo')
        
        # Select only required columns: Dates, Skier, Nation
        king_clean = result_df[['dates', 'name', 'nation']].copy()
        king_clean.columns = ['Dates', 'Skier', 'Nation']
        
        # Format skier names with multiple answer options and normalize Nation
        king_clean['Skier'] = king_clean['Skier'].apply(format_skier_multiple_answers)
        king_clean['Nation'] = king_clean['Nation'].apply(normalize_special_characters)
        
        # Create output directory
        output_dir = create_output_directory(sport)
        
        # Generate filename
        gender_name = "ladies" if gender == "L" else "men"
        filename = f"king_{gender_name}_{sport}.csv"
        
        # Save CSV with UTF-8 BOM encoding for proper Excel display
        output_path = output_dir / filename
        king_clean.to_csv(output_path, index=False, encoding='utf-8-sig')
        print(f"  Saved: {output_path}")
        
    except FileNotFoundError:
        print(f"  Warning: {chrono_file} not found, skipping...")
    except Exception as e:
        print(f"  Error: {e}")

def generate_days_on_top(sport, gender):
    """Generate Days on Top CSV showing total days each athlete spent at #1"""
    print(f"Generating Days on Top for {sport} {gender}...")
    
    # Map sport names for ski directory structure
    ski_sport_map = {
        'cross-country': 'ski',
        'alpine': 'alpine',
        'biathlon': 'biathlon', 
        'nordic-combined': 'nordic-combined',
        'skijump': 'skijump'
    }
    ski_sport = ski_sport_map.get(sport, sport)
    
    # Load chrono data
    gender_name = "ladies" if gender == "L" else "men"
    chrono_file = fos.path.expanduser("~/ski/elo/python/{ski_sport}/polars/excel365/{gender_name}_chrono.csv")
    
    try:
        df = pd.read_csv(chrono_file)
        
        # Get king/queen periods
        periods_df = king_queen(df, 'Elo')
        
        # Aggregate total days per athlete and get most recent end date and nation
        # Group only by name to handle athletes who represented multiple countries
        days_summary = periods_df.groupby(['name']).agg({
            'days': 'sum',
            'end_date': 'max',  # Most recent date they were on top
            'nation': 'last'    # Use the most recent nation they represented
        }).reset_index()
        
        # Sort by total days descending
        days_summary = days_summary.sort_values('days', ascending=False)
        
        # Select only required columns and rename: # of Days, Skier, Nation
        days_clean = days_summary[['days', 'name', 'nation', 'end_date']].copy()
        days_clean.columns = ['# of Days', 'Skier', 'Nation', 'End_Date']
        
        # Convert days from timedelta to just the number of days
        # Handle both timedelta and numeric types
        if days_clean['# of Days'].dtype == 'timedelta64[ns]':
            days_clean['# of Days'] = days_clean['# of Days'].dt.days
        else:
            # If it's already numeric (from the king_queen function), convert to int
            days_clean['# of Days'] = days_clean['# of Days'].astype(str).str.extract(r'(\d+)').astype(int)
        
        # Format the end date and add to Nation column
        # Handle if end_date is "Present" or a date
        def format_end_date(date_val):
            if pd.isna(date_val) or str(date_val).strip() == 'Present':
                return 'Present'
            elif hasattr(date_val, 'strftime'):
                return date_val.strftime('%Y-%m-%d')
            else:
                # Try to convert string date to datetime then format
                try:
                    return pd.to_datetime(str(date_val)).strftime('%Y-%m-%d')
                except:
                    return str(date_val)
        
        days_clean['End_Date_Formatted'] = days_clean['End_Date'].apply(format_end_date)
        days_clean['Nation'] = days_clean['Nation'] + ' (' + days_clean['End_Date_Formatted'] + ')'
        
        # Drop the temporary date columns
        days_clean = days_clean[['# of Days', 'Skier', 'Nation']]
        
        # Format skier names with multiple answer options and normalize Nation
        days_clean['Skier'] = days_clean['Skier'].apply(format_skier_multiple_answers)
        days_clean['Nation'] = days_clean['Nation'].apply(normalize_special_characters)
        
        # Create output directory
        output_dir = create_output_directory(sport)
        
        # Generate filename
        filename = f"days_{gender_name}_{sport}.csv"
        
        # Save CSV with UTF-8 BOM encoding for proper Excel display
        output_path = output_dir / filename
        days_clean.to_csv(output_path, index=False, encoding='utf-8-sig')
        print(f"  Saved: {output_path}")
        
    except FileNotFoundError:
        print(f"  Warning: {chrono_file} not found, skipping...")
    except Exception as e:
        print(f"  Error: {e}")

def generate_seasonal_wins(sport, gender):
    """Generate seasonal wins CSV showing wins per season per athlete"""
    print(f"Generating Seasonal Wins for {sport} {gender}...")
    
    # Map sport names for ski directory structure
    ski_sport_map = {
        'cross-country': 'ski',
        'alpine': 'alpine',
        'biathlon': 'biathlon', 
        'nordic-combined': 'nordic-combined',
        'skijump': 'skijump'
    }
    ski_sport = ski_sport_map.get(sport, sport)
    
    # Load chrono data  
    gender_name = "ladies" if gender == "L" else "men"
    chrono_file = fos.path.expanduser("~/ski/elo/python/{ski_sport}/polars/excel365/{gender_name}_chrono.csv")
    
    try:
        df = pd.read_csv(chrono_file)
        
        # Filter out Race==0 and only keep Place==1 (winners)
        wins_data = df[(df['Race'] != 0) & (df['Place'] == 1)].copy()
        
        if wins_data.empty:
            print(f"  No wins data found for {sport} {gender}")
            return
            
        # Group by Season and Skier, count wins
        seasonal_wins = wins_data.groupby(['Season', 'Skier', 'Nation']).size().reset_index(name='Wins')
        
        # Create Nation column with win count: "Nation (# wins)"
        seasonal_wins['Nation'] = seasonal_wins['Nation'] + ' (' + seasonal_wins['Wins'].astype(str) + ')'
        
        # Select final columns: Season, Skier, Nation
        wins_clean = seasonal_wins[['Season', 'Skier', 'Nation']].copy()
        
        # Format skier names with multiple answer options and normalize Nation
        wins_clean['Skier'] = wins_clean['Skier'].apply(format_skier_multiple_answers)
        wins_clean['Nation'] = wins_clean['Nation'].apply(normalize_special_characters)
        
        # Create output directory
        output_dir = create_output_directory(sport)
        
        # Define time periods
        time_periods = [
            {'name': 'pre1976', 'start': 0, 'end': 1975, 'label': 'Pre-1976'},
            {'name': '1976to2000', 'start': 1976, 'end': 2000, 'label': '1976-2000'}, 
            {'name': '2001present', 'start': 2001, 'end': 9999, 'label': '2001-Present'}
        ]
        
        for period in time_periods:
            # Filter data for this time period
            period_data = wins_clean[
                (wins_clean['Season'] >= period['start']) & 
                (wins_clean['Season'] <= period['end'])
            ].copy()
            
            if period_data.empty:
                print(f"  No wins data found for {period['label']}")
                continue
                
            # Sort by Season ascending, then by wins descending (extract wins from Nation column)
            period_data['Wins_for_sort'] = period_data['Nation'].str.extract(r'\((\d+)\)').astype(int)
            period_data = period_data.sort_values(['Season', 'Wins_for_sort'], ascending=[True, False])
            period_data = period_data.drop('Wins_for_sort', axis=1)
            
            # Generate filename
            filename = f"seasonalwins_{gender_name}_{sport}_{period['name']}.csv"
            
            # Save CSV with UTF-8 BOM encoding for proper Excel display
            output_path = output_dir / filename
            period_data.to_csv(output_path, index=False, encoding='utf-8-sig')
            print(f"  Saved: {output_path} - {period['label']} ({len(period_data)} rows)")
        
    except FileNotFoundError:
        print(f"  Warning: {chrono_file} not found, skipping...")
    except Exception as e:
        print(f"  Error: {e}")

def generate_career_wins(sport, gender):
    """Generate career wins CSV showing total wins per athlete"""
    print(f"Generating Career Wins for {sport} {gender}...")
    
    # Map sport names for ski directory structure
    ski_sport_map = {
        'cross-country': 'ski',
        'alpine': 'alpine',
        'biathlon': 'biathlon', 
        'nordic-combined': 'nordic-combined',
        'skijump': 'skijump'
    }
    ski_sport = ski_sport_map.get(sport, sport)
    
    # Load chrono data  
    gender_name = "ladies" if gender == "L" else "men"
    chrono_file = fos.path.expanduser("~/ski/elo/python/{ski_sport}/polars/excel365/{gender_name}_chrono.csv")
    
    try:
        df = pd.read_csv(chrono_file)
        
        # Filter for wins only (Place==1) and exclude Race==0
        wins_data = df[(df['Race'] != 0) & (df['Place'] == 1)].copy()
        
        if wins_data.empty:
            print(f"  No wins data found for {sport} {gender}")
            return
            
        # Group by Skier only to handle athletes who represented multiple countries
        career_wins = wins_data.groupby(['Skier']).agg({
            'Date': 'max',  # Last win date
            'Season': 'count',  # Count of wins (using any column for counting)
            'Nation': 'last'  # Use the most recent nation they represented
        }).reset_index()
        career_wins.columns = ['Skier', 'Last_Win_Date', 'Wins', 'Nation']
        
        # Extract last name for sorting and normalize special characters
        career_wins['Last_Name'] = career_wins['Skier'].str.split().str[-1].apply(normalize_special_characters)
        
        # Sort by wins descending, then by normalized last name ascending (A-Z)
        career_wins = career_wins.sort_values(['Wins', 'Last_Name'], ascending=[False, True])
        
        # Drop the temporary Last_Name column
        career_wins = career_wins.drop('Last_Name', axis=1)
        
        # Format the date as YYYY-MM-DD and create Nation column with date
        career_wins['Last_Win_Date'] = pd.to_datetime(career_wins['Last_Win_Date']).dt.strftime('%Y-%m-%d')
        career_wins['Nation'] = career_wins['Nation'] + ' (' + career_wins['Last_Win_Date'] + ')'
        
        # Select final columns: # Wins, Skier, Nation (with date)
        wins_clean = career_wins[['Wins', 'Skier', 'Nation']].copy()
        wins_clean.columns = ['# Wins', 'Skier', 'Nation']
        
        # Format skier names with multiple answer options and normalize Nation
        wins_clean['Skier'] = wins_clean['Skier'].apply(format_skier_multiple_answers)
        wins_clean['Nation'] = wins_clean['Nation'].apply(normalize_special_characters)
        
        # Create output directory
        output_dir = create_output_directory(sport)
        
        # Generate filename
        filename = f"careerwins_{gender_name}_{sport}.csv"
        
        # Save CSV with UTF-8 BOM encoding for proper Excel display
        output_path = output_dir / filename
        wins_clean.to_csv(output_path, index=False, encoding='utf-8-sig')
        print(f"  Saved: {output_path} ({len(wins_clean)} rows)")
        
    except FileNotFoundError:
        print(f"  Warning: {chrono_file} not found, skipping...")
    except Exception as e:
        print(f"  Error: {e}")

def generate_top10_seasonal(sport, gender, discipline=None):
    """Generate Top 10 seasonal rankings for each discipline"""
    print(f"Generating Top 10 Seasonal for {sport} {gender}...")
    
    # Map sport names for ski directory structure
    ski_sport_map = {
        'cross-country': 'ski',
        'alpine': 'alpine',
        'biathlon': 'biathlon', 
        'nordic-combined': 'nordic-combined',
        'skijump': 'skijump'
    }
    ski_sport = ski_sport_map.get(sport, sport)
    
    # Load chrono data  
    gender_name = "ladies" if gender == "L" else "men"
    chrono_file = fos.path.expanduser("~/ski/elo/python/{ski_sport}/polars/excel365/{gender_name}_chrono.csv")
    
    try:
        df = pd.read_csv(chrono_file)
        
        # Get sport disciplines
        if sport not in SPORT_DISCIPLINES:
            print(f"  Warning: No discipline mapping for {sport}")
            return
            
        disciplines = SPORT_DISCIPLINES[sport]
        
        # Create output directory
        output_dir = create_output_directory(sport)
        
        # Process each discipline
        for elo_col, discipline_name in zip(disciplines['columns'], disciplines['names']):
            if elo_col not in df.columns:
                print(f"  Warning: Column {elo_col} not found, skipping {discipline_name}")
                continue
                
            print(f"  Processing {discipline_name}...")
            
            # Filter for Race == 0 (season start rankings)
            season_starts = df[df['Race'] == 0].copy()
            
            if season_starts.empty:
                print(f"    No Race==0 data found for {discipline_name}")
                continue
            
            # Group by season and get top 10 for each
            seasons = sorted(season_starts['Season'].unique())
            
            # Define time periods - special handling for different discipline types
            if (sport == 'cross-country' and ('sprint' in discipline_name or 'freestyle' in discipline_name)):
                # Cross-country sprint/freestyle: single file from start
                time_periods = [
                    {'name': 'all', 'start': 0, 'end': 9999, 'label': 'All'}
                ]
            elif (sport == 'biathlon' and ('mass' in discipline_name or 'pursuit' in discipline_name)):
                # Biathlon mass start/pursuit: single file from start (late 90s)
                time_periods = [
                    {'name': 'all', 'start': 0, 'end': 9999, 'label': 'All'}
                ]
            elif (sport == 'biathlon' and 'sprint' in discipline_name):
                # Biathlon sprint: two periods (pre-2001 and 2001-present)
                time_periods = [
                    {'name': 'pre2001', 'start': 0, 'end': 2000, 'label': 'Pre-2001'},
                    {'name': '2001present', 'start': 2001, 'end': 9999, 'label': '2001-Present'}
                ]
            else:
                # All other disciplines: standard three periods
                time_periods = [
                    {'name': 'pre1976', 'start': 0, 'end': 1975, 'label': 'Pre-1976'},
                    {'name': '1976to2000', 'start': 1976, 'end': 2000, 'label': '1976-2000'}, 
                    {'name': '2001present', 'start': 2001, 'end': 9999, 'label': '2001-Present'}
                ]
            
            for period in time_periods:
                period_seasons = [s for s in seasons if period['start'] <= s <= period['end']]
                
                if not period_seasons:
                    print(f"    No seasons found for {period['label']}")
                    continue
                    
                period_data = []
                
                for season in period_seasons:
                    season_data = season_starts[season_starts['Season'] == season].copy()
                    
                    # Remove rows with null ELO values
                    season_data = season_data.dropna(subset=[elo_col])
                    
                    if season_data.empty:
                        continue
                        
                    # Sort by ELO descending and take top 10
                    top_10 = season_data.nlargest(10, elo_col)
                    
                    # Only keep required columns: Season, Skier, Nation
                    top_10_clean = top_10[['Season', 'Skier', 'Nation']].copy()
                    
                    # Format skier names with multiple answer options and normalize Nation
                    top_10_clean['Skier'] = top_10_clean['Skier'].apply(format_skier_multiple_answers)
                    top_10_clean['Nation'] = top_10_clean['Nation'].apply(normalize_special_characters)
                    
                    period_data.append(top_10_clean)
                
                if period_data:
                    # Combine seasons in this period
                    period_df = pd.concat(period_data, ignore_index=True)
                    
                    # Generate filename for this time period
                    single_file_disciplines = (
                        (sport == 'cross-country' and ('sprint' in discipline_name or 'freestyle' in discipline_name)) or
                        (sport == 'biathlon' and ('mass' in discipline_name or 'pursuit' in discipline_name))
                    )
                    
                    if single_file_disciplines:
                        # For single-file disciplines, no time period in filename
                        filename = f"top10_{gender_name}_{discipline_name}.csv"
                    else:
                        # For other disciplines, include time period
                        filename = f"top10_{gender_name}_{discipline_name}_{period['name']}.csv"
                    
                    # Save CSV with UTF-8 BOM encoding for proper Excel display
                    output_path = output_dir / filename
                    period_df.to_csv(output_path, index=False, encoding='utf-8-sig')
                    print(f"    Saved: {output_path} - {period['label']} ({len(period_df)} rows)")
                else:
                    print(f"    No data found for {discipline_name} {period['label']}")
        
    except FileNotFoundError:
        print(f"  Warning: {chrono_file} not found, skipping...")
    except Exception as e:
        print(f"  Error: {e}")

def main():
    """Main function to generate all Sporcle quiz data"""
    parser = argparse.ArgumentParser(description='Generate Sporcle quiz CSV files')
    parser.add_argument('--sport', choices=['alpine', 'biathlon', 'cross-country', 'nordic-combined', 'skijump'], 
                       help='Sport to process (default: all)')
    parser.add_argument('--gender', choices=['M', 'L'], 
                       help='Gender to process (default: both)')
    parser.add_argument('--quiz_type', choices=['all_time_100', 'current_year_50', 'king_queen', 'days_on_top', 'top10_seasonal', 'seasonal_wins', 'career_wins'], 
                       help='Quiz type to generate (default: all)')
    
    args = parser.parse_args()
    
    # Define sports and genders to process
    sports = [args.sport] if args.sport else ['alpine', 'biathlon', 'cross-country', 'nordic-combined', 'skijump']
    genders = [args.gender] if args.gender else ['M', 'L']
    
    print("Starting Sporcle CSV generation...\n")
    
    for sport in sports:
        print(f"Processing {sport.upper()}...")
        
        for gender in genders:
            print(f"\n  {sport} {'Men' if gender == 'M' else 'Ladies'}:")
            
            # Generate quiz types based on arguments
            if not args.quiz_type or args.quiz_type == 'all_time_100':
                generate_all_time_100(sport, gender)
                
            if not args.quiz_type or args.quiz_type == 'current_year_50':
                generate_current_year_50(sport, gender)
                
            if not args.quiz_type or args.quiz_type == 'king_queen':
                generate_king_queen(sport, gender)
                
            if not args.quiz_type or args.quiz_type == 'days_on_top':
                generate_days_on_top(sport, gender)
                
            if not args.quiz_type or args.quiz_type == 'top10_seasonal':
                generate_top10_seasonal(sport, gender)
                
            if not args.quiz_type or args.quiz_type == 'seasonal_wins':
                generate_seasonal_wins(sport, gender)
                
            if not args.quiz_type or args.quiz_type == 'career_wins':
                generate_career_wins(sport, gender)
        
        print(f"\nCompleted {sport}")
    
    print("\nSporcle CSV generation complete!")

if __name__ == "__main__":
    main()