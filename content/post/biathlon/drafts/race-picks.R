# Biathlon Race Predictions: Methodology with Race Probabilities
library(dplyr)
library(tidyr)
library(openxlsx)
library(arrow)
library(mgcv)
library(leaps)
library(logger)
library(purrr)
library(lubridate) # For better date handling
library(slider)    # For sliding window operations

# Define points systems
regular_points <- c(90,75,65,55,50,45,41,37,34,31,30,29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
mass_start_points <- c(90,75,65,55,50,45,41,37,34,31,30,29,28,27,26,25,24,23,22,21,20,18,16,14,12,10,8,6,4,2)

# Function to replace NAs with first quartile value
replace_na_with_quartile <- function(x) {
  if(all(is.na(x))) return(rep(0, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  ifelse(is.na(x), q1, x)
}

# Set up logging
log_dir <- "~/ski/elo/python/biathlon/polars/excel365/race-predictions"
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

log_threshold(DEBUG)
log_appender(appender_file(file.path(log_dir, "race_picks_processing.log")))
log_info("Starting biathlon race predictions process")

# Read in the race schedule from races.csv with proper date parsing
# Fix for race-picks.R to only process today's races

# First, read in all races
races <- read.csv("~/ski/elo/python/biathlon/polars/excel365/races.csv", 
                  stringsAsFactors = FALSE)

# Get today's date in the right format
today_date <- format(as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d"), "%m/%d/%Y")
log_info(paste("Today's date:", today_date))

# Filter to only include races for today
today_races <- races %>%
  dplyr::filter(Date == today_date)

# If no races found for today, try with 2-digit year format
if(nrow(today_races) == 0) {
  today_date_short <- format(as.Date(format(Sys.time(), tz = "UTC"), "%Y-%m-%d"), "%m/%d/%y")
  log_info(paste("Trying with short year format:", today_date_short))
  today_races <- races %>%
    dplyr::filter(Date == today_date_short)
}

# Log the results
if(nrow(today_races) > 0) {
  log_info(paste("Found", nrow(today_races), "races for today"))
  # Use today's races for processing
  races <- today_races
  # Parse the date for further processing
  races <- races %>%
    mutate(Date = as.Date(Date, format="%m/%d/%Y"))
  race_date <- unique(races$Date)[1]
} else {
  log_info("No races found for today. Exiting.")
  quit(save = "no", status = 0)  # Exit R without saving workspace
}

# Create race dataframes for men and ladies
men_races <- races %>%
  dplyr::filter(Sex == "M", 
                !RaceType %in% c("Relay", "Mixed Relay", "Single Mixed Relay")) %>%
  dplyr::select(RaceType, Period, Elevation) %>%
  rename(racetype = RaceType, period = Period, elevation = Elevation)

ladies_races <- races %>%
  dplyr::filter(Sex == "L", 
                !RaceType %in% c("Relay", "Mixed Relay", "Single Mixed Relay")) %>%
  dplyr::select(RaceType, Period, Elevation) %>%
  rename(racetype = RaceType, period = Period, elevation = Elevation)

mixed_races <- races %>%
  dplyr::filter(Sex == "Mixed") %>%
  dplyr::select(RaceType, Period, Elevation) %>%
  rename(racetype = RaceType, period = Period, elevation = Elevation)

# Create race dataframes for relays
# Create separate relays dataframes by gender
men_relays <- races %>%
  dplyr::filter(RaceType %in% c("Relay"), Sex == "M") %>%
  dplyr::select(RaceType, Period, Elevation) %>%
  rename(racetype = RaceType, period = Period, elevation = Elevation)

ladies_relays <- races %>%
  dplyr::filter(RaceType %in% c("Relay"), Sex == "L") %>%
  dplyr::select(RaceType, Period, Elevation) %>%
  rename(racetype = RaceType, period = Period, elevation = Elevation)

# Create a combined relays dataframe for backward compatibility
relays <- races %>%
  dplyr::filter(RaceType %in% c("Relay")) %>%
  dplyr::select(RaceType, Period, Elevation) %>%
  rename(racetype = RaceType, period = Period, elevation = Elevation)

log_info(paste("Found", nrow(men_relays), "men's relay races,", 
               nrow(ladies_relays), "ladies's relay races"))

mixed_relays <- races %>%
  dplyr::filter(RaceType %in% c("Mixed Relay")) %>%
  dplyr::select(RaceType, Period, Elevation) %>%
  rename(racetype = RaceType, period = Period, elevation = Elevation)

single_mixed_relays <- races %>%
  dplyr::filter(RaceType %in% c("Single Mixed Relay")) %>%
  dplyr::select(RaceType, Period, Elevation) %>%
  rename(racetype = RaceType, period = Period, elevation = Elevation)

log_info(paste("Found", nrow(men_races), "men's races,", nrow(ladies_races), "ladies's races, and", 
               nrow(relays) + nrow(mixed_relays) + nrow(single_mixed_relays), "relay races"))

# Calculate Race Probabilities
calculate_race_probabilities <- function() {
  log_info("Calculating race participation probabilities")
  
  # Initialize empty data frames
  men_startlist <- data.frame()
  ladies_startlist <- data.frame()
  
  men_chrono <- data.frame()
  ladies_chrono <- data.frame()
  
  # Only load data for race types that are happening
  if(nrow(men_races) > 0) {
    log_info("Reading men's chronological data and startlist")
    men_chrono <- read.csv("~/ski/elo/python/biathlon/polars/relay/excel365/men_chrono_elevation.csv", 
                           stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
    
    men_startlist <- read.csv("~/ski/elo/python/biathlon/polars/excel365/startlist_races_men.csv", 
                              stringsAsFactors = FALSE)
    
    men_startlist$Sex = "M"
  }
  
  if(nrow(ladies_races) > 0) {
    log_info("Reading ladies's chronological data and startlist")
    ladies_chrono <- read.csv("~/ski/elo/python/biathlon/polars/relay/excel365/ladies_chrono_elevation.csv", 
                              stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date))
    
    ladies_startlist <- read.csv("~/ski/elo/python/biathlon/polars/excel365/startlist_races_ladies.csv", 
                                 stringsAsFactors = FALSE)
    ladies_startlist$Sex = "L"
  }
  
  # Initialize relay variables
  men_relay_startlist <- data.frame()
  ladies_relay_startlist <- data.frame()
  mixed_relay_startlist <- data.frame()
  single_mixed_relay_startlist <- data.frame()
  
  # For relay events
  # Regular relays
  if(nrow(relays) > 0) {
    log_info("Reading relay chronological data and startlist")
    
    # Load men's relay startlist if it exists
    men_relay_file <- "~/ski/elo/python/biathlon/polars/relay/excel365/startlist_relay_races_teams_men.csv"
    if(file.exists(men_relay_file)) {
      men_relay_startlist <- read.csv(men_relay_file, stringsAsFactors = FALSE)
      
      log_info(paste("Loaded men's relay startlist with", nrow(men_relay_startlist), "teams"))
    } else {
      log_warn("Men's relay startlist file not found")
    }
    
    # Load ladies' relay startlist if it exists
    ladies_relay_file <- "~/ski/elo/python/biathlon/polars/relay/excel365/startlist_relay_races_teams_ladies.csv"
    if(file.exists(ladies_relay_file)) {
      ladies_relay_startlist <- read.csv(ladies_relay_file, stringsAsFactors = FALSE)
      log_info(paste("Loaded ladies' relay startlist with", nrow(ladies_relay_startlist), "teams"))
    } else {
      log_warn("Ladies' relay startlist file not found")
    }
  }
  
  # Mixed relay
  if(nrow(mixed_relays) > 0) {
    log_info("Reading mixed relay startlist")
    mixed_relay_file <- "~/ski/elo/python/biathlon/polars/relay/excel365/startlist_mixed_relay_races_teams.csv"
    if(file.exists(mixed_relay_file)) {
      mixed_relay_startlist <- read.csv(mixed_relay_file, stringsAsFactors = FALSE)
      log_info(paste("Loaded mixed relay startlist with", nrow(mixed_relay_startlist), "teams"))
    } else {
      log_warn("Mixed relay startlist file not found")
    }
  }
  
  # Single mixed relay
  if(nrow(single_mixed_relays) > 0) {
    log_info("Reading single mixed relay startlist")
    single_mixed_relay_file <- "~/ski/elo/python/biathlon/polars/relay/excel365/startlist_single_mixed_relay_races_teams.csv"
    if(file.exists(single_mixed_relay_file)) {
      single_mixed_relay_startlist <- read.csv(single_mixed_relay_file, stringsAsFactors = FALSE)
      log_info(paste("Loaded single mixed relay startlist with", nrow(single_mixed_relay_startlist), "teams"))
    } else {
      log_warn("Single mixed relay startlist file not found")
    }
  }
  
  # Function to get race probability for a skier/team using exponential decay
  get_race_probability <- function(chronos, participant, racetype, is_relay = FALSE) {
    log_debug(paste("Calculating exponential decay probability for participant:", participant))
    
    # For relay, participant is a Nation
    id_col <- if(is_relay) "Nation" else "Skier"
    
    # Get participant's race history for this race type
    participant_races <- chronos %>%
      dplyr::filter(
        get(id_col) == participant,
        RaceType == racetype
      ) %>%
      arrange(Date, Season, Race)
    
    if(nrow(participant_races) == 0) {
      log_debug(paste("No race history found for participant:", participant, "in race type:", racetype))
      return(0)
    }
    
    # Calculate exponential decay probability
    total_races <- nrow(participant_races)
    
    if(total_races > 0) {
      # Create exponential decay weights (α = 0.1)
      race_weights <- exp(-0.1 * ((total_races-1):0))
      
      # Create participation vector (1 if skier participated, 0 if not)
      # Since they're all in the chronos data, they all participated
      participation <- rep(1, total_races)
      
      # Calculate weighted probability
      weighted_participation <- sum(participation * race_weights)
      total_weight <- sum(race_weights)
      prob <- weighted_participation / total_weight
      
      log_debug(paste("Exponential decay probability for", participant, ":", round(prob, 3), 
                      "based on", total_races, "races"))
      
      return(prob)
    }
    
    return(0)
  }
  
  # Process men's and ladies's race probabilities
  process_individual_probabilities <- function(startlist, chronos, races, gender) {
    # Check if we have biathlon startlist (at least one skier has In_Startlist=True)
    has_biathlonworld_startlist <- FALSE
    if("In_Startlist" %in% names(startlist)) {
      has_biathlonworld_startlist <- any(startlist$In_Startlist, na.rm = TRUE)
    }
    
    # Handle Race1_Prob based on biathlon startlist existence
    if(has_biathlonworld_startlist) {
      log_info("Biathlon startlist exists (at least one In_Startlist=True), keeping existing Race1_Prob values")
      # Keep existing Race1_Prob if it exists, otherwise set based on In_Startlist
      if(!"Race1_Prob" %in% names(startlist)) {
        startlist$Race1_Prob <- ifelse(startlist$In_Startlist, 1, 0)
      }
    } else {
      log_info("No biathlon startlist (all In_Startlist=False), will calculate Race1_Prob")
      # Race1_Prob will be calculated like other races below
    }
    
    # Process each race
    for(i in 1:nrow(races)) {
      race_prob_col <- paste0("Race", i, "_Prob")
      
      # Skip Race1_Prob if we have a biathlon startlist and it's already set
      if(race_prob_col == "Race1_Prob" && has_biathlonworld_startlist && race_prob_col %in% names(startlist)) {
        log_info("Using existing Race1_Prob from biathlon startlist, skipping calculation")
        next
      }
      
      # Create the column if it doesn't exist
      if(!(race_prob_col %in% names(startlist))) {
        startlist[[race_prob_col]] <- NA_real_
      }
      
      # Process probabilities for each skier
      for(j in 1:nrow(startlist)) {
        skier <- startlist$Skier[j]
        startlist[j, race_prob_col] <- 
          get_race_probability(chronos, skier, races$racetype[i], is_relay = FALSE)
      }
    }
    
    return(startlist)
  }
  
  # Process relay race probabilities - set all to 1 for simplicity
  process_relay_probabilities <- function(startlist, races) {
    log_info("Processing relay race probabilities - setting all to 1")
    log_info(paste("Input races dataframe has", nrow(races), "rows"))
    
    # We should treat this as a single race type regardless of how many rows are in races
    # So we'll just set Race1_Prob for all teams
    race_prob_col <- "Race1_Prob"
    
    # Create the column if it doesn't exist
    if(!(race_prob_col %in% names(startlist))) {
      startlist[[race_prob_col]] <- 1  # Set probability to 1 for all teams
      log_info(paste("Set", race_prob_col, "to 1 for all", nrow(startlist), "teams"))
    } else {
      log_info(paste(race_prob_col, "already exists in startlist"))
    }
    
    return(startlist)
  }
  
  # Process each gender and relay
  if(nrow(men_races) > 0 && nrow(men_startlist) > 0) {
    log_info("Processing men's race probabilities")
    men_startlist_with_probs <- process_individual_probabilities(men_startlist, men_chrono, men_races, "M")
  } else {
    men_startlist_with_probs <- men_startlist
  }
  
  if(nrow(ladies_races) > 0 && nrow(ladies_startlist) > 0) {
    log_info("Processing ladies' race probabilities")
    ladies_startlist_with_probs <- process_individual_probabilities(ladies_startlist, ladies_chrono, ladies_races, "L")
  } else {
    ladies_startlist_with_probs <- ladies_startlist
  }
  
  # Process regular relays if we have any
  if(nrow(relays) > 0) {
    if(nrow(men_relay_startlist) > 0) {
      log_info("Processing men's relay race probabilities")
      
      men_relay_startlist_with_probs <- process_relay_probabilities(men_relay_startlist, relays)
    } else {
      men_relay_startlist_with_probs <- men_relay_startlist
    }
    
    if(nrow(ladies_relay_startlist) > 0) {
      log_info("Processing ladies' relay race probabilities")
      ladies_relay_startlist_with_probs <- process_relay_probabilities(ladies_relay_startlist, relays)
    } else {
      ladies_relay_startlist_with_probs <- ladies_relay_startlist
    }
  } else {
    men_relay_startlist_with_probs <- men_relay_startlist
    ladies_relay_startlist_with_probs <- ladies_relay_startlist
  }
  
  # Process mixed relays if we have any
  if(nrow(mixed_relays) > 0 && nrow(mixed_relay_startlist) > 0) {
    log_info("Processing mixed relay race probabilities")
    mixed_relay_startlist_with_probs <- process_relay_probabilities(mixed_relay_startlist, mixed_relays)
  } else {
    mixed_relay_startlist_with_probs <- mixed_relay_startlist
  }
  
  # Process single mixed relays if we have any
  if(nrow(single_mixed_relays) > 0 && nrow(single_mixed_relay_startlist) > 0) {
    log_info("Processing single mixed relay race probabilities")
    single_mixed_relay_startlist_with_probs <- process_relay_probabilities(single_mixed_relay_startlist, single_mixed_relays)
  } else {
    single_mixed_relay_startlist_with_probs <- single_mixed_relay_startlist
  }
  
  # Save results
  log_info("Saving race probability results")
  
  if(nrow(men_races) > 0 && nrow(men_startlist_with_probs) > 0) {
    write.csv(men_startlist_with_probs, 
              "~/ski/elo/python/biathlon/polars/excel365/startlist_races_men.csv", 
              row.names = FALSE)
  }
  
  if(nrow(ladies_races) > 0 && nrow(ladies_startlist_with_probs) > 0) {
    write.csv(ladies_startlist_with_probs, 
              "~/ski/elo/python/biathlon/polars/excel365/startlist_races_ladies.csv", 
              row.names = FALSE)
  }
  
  if(nrow(relays) > 0) {
    if(nrow(men_relay_startlist_with_probs) > 0) {
      write.csv(men_relay_startlist_with_probs,
                "~/ski/elo/python/biathlon/polars/relay/excel365/startlist_relay_races_teams_men.csv",
                row.names = FALSE)
    }
    
    if(nrow(ladies_relay_startlist_with_probs) > 0) {
      write.csv(ladies_relay_startlist_with_probs,
                "~/ski/elo/python/biathlon/polars/relay/excel365/startlist_relay_races_teams_ladies.csv",
                row.names = FALSE)
    }
  }
  
  if(nrow(mixed_relays) > 0 && nrow(mixed_relay_startlist_with_probs) > 0) {
    write.csv(mixed_relay_startlist_with_probs,
              "~/ski/elo/python/biathlon/polars/relay/excel365/startlist_mixed_relay_races_teams.csv",
              row.names = FALSE)
  }
  
  if(nrow(single_mixed_relays) > 0 && nrow(single_mixed_relay_startlist_with_probs) > 0) {
    write.csv(single_mixed_relay_startlist_with_probs,
              "~/ski/elo/python/biathlon/polars/relay/excel365/startlist_single_mixed_relay_races_teams.csv",
              row.names = FALSE)
  }
  
  log_info("Race probability calculation complete")
  # Return all processed startlists
  return(list(
    men = men_startlist_with_probs,
    ladies = ladies_startlist_with_probs,
    men_relay = men_relay_startlist_with_probs,
    ladies_relay = ladies_relay_startlist_with_probs,
    mixed_relay = mixed_relay_startlist_with_probs,
    single_mixed_relay = single_mixed_relay_startlist_with_probs
  ))
}

# Create processed relay chrono files
# Function to create relay chronos with error handling for missing Points column
create_relay_chronos <- function() {
  log_info("Creating team-based relay chronological files")
  
  # Only process if we have relay races
  if(nrow(relays) + nrow(mixed_relays) + nrow(single_mixed_relays) == 0) {
    log_info("No relay races scheduled - skipping relay chrono creation")
    return(FALSE)
  }
  
  # Load men's and ladies's chronological data
  men_chrono <- read.csv("~/ski/elo/python/biathlon/polars/relay/excel365/men_chrono_elevation.csv", 
                         stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date))
  
  ladies_chrono <- read.csv("~/ski/elo/python/biathlon/polars/relay/excel365/ladies_chrono_elevation.csv", 
                            stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date))
  
  # Check if Points column exists, if not create it
  if(!"Points" %in% names(men_chrono)) {
    log_info("Creating Points column in men's chrono based on Place")
    men_chrono$Points <- mapply(function(place, RaceType) {
      get_points(place, RaceType)
    }, men_chrono$Place, men_chrono$RaceType)
  }
  
  if(!"Points" %in% names(ladies_chrono)) {
    log_info("Creating Points column in ladies' chrono based on Place")
    ladies_chrono$Points <- mapply(function(place, RaceType) {
      get_points(place, RaceType)
    }, ladies_chrono$Place, ladies_chrono$RaceType)
  }
  
  # Process men's relay chrono
  if(nrow(relays) > 0) {
    log_info("Creating men's relay chrono")
    men_relay_chrono <- men_chrono %>%
      dplyr::filter(RaceType == "Relay") %>%
      # Group by race to get team results
      group_by(Date, City, Country, Nation, RaceType, Season, Race, Event, Elevation) %>%
      summarize(
        # Team rank is the first Place value within each group
        Place = first(Place),
        Points = first(Points),
        # Calculate average Elo/Pelo values for the team
        Avg_Elo = mean(Elo, na.rm = TRUE),
        Avg_Individual_Elo = mean(Individual_Elo, na.rm = TRUE),
        Avg_Sprint_Elo = mean(Sprint_Elo, na.rm = TRUE),
        Avg_Pursuit_Elo = mean(Pursuit_Elo, na.rm = TRUE),
        Avg_MassStart_Elo = mean(MassStart_Elo, na.rm = TRUE),
        Avg_Pelo = mean(Pelo, na.rm = TRUE),
        Avg_Individual_Pelo = mean(Individual_Pelo, na.rm = TRUE),
        Avg_Sprint_Pelo = mean(Sprint_Pelo, na.rm = TRUE),
        Avg_Pursuit_Pelo = mean(Pursuit_Pelo, na.rm = TRUE),
        Avg_MassStart_Pelo = mean(MassStart_Pelo, na.rm = TRUE),
        # Additional metadata
        MassStart = first(MassStart),
        Sex = "M",
        .groups = "drop"
      )
    
    # Save men's relay chrono
    write.csv(men_relay_chrono,
              "~/ski/elo/python/biathlon/polars/relay/excel365/men_relay_chrono.csv",
              row.names = FALSE)
    log_info(paste("Created men's relay chrono with", nrow(men_relay_chrono), "team entries"))
  }
  
  # Process ladies' relay chrono
  if(nrow(relays) > 0) {
    log_info("Creating ladies' relay chrono")
    ladies_relay_chrono <- ladies_chrono %>%
      dplyr::filter(RaceType == "Relay") %>%
      # Group by race to get team results
      group_by(Date, City, Country, Nation, RaceType, Season, Race, Event, Elevation) %>%
      summarize(
        # Team rank is the first Place value within each group
        Place = first(Place),
        Points = first(Points),
        # Calculate average Elo/Pelo values for the team
        Avg_Elo = mean(Elo, na.rm = TRUE),
        Avg_Individual_Elo = mean(Individual_Elo, na.rm = TRUE),
        Avg_Sprint_Elo = mean(Sprint_Elo, na.rm = TRUE),
        Avg_Pursuit_Elo = mean(Pursuit_Elo, na.rm = TRUE),
        Avg_MassStart_Elo = mean(MassStart_Elo, na.rm = TRUE),
        
        Avg_Pelo = mean(Pelo, na.rm = TRUE),
        Avg_Individual_Pelo = mean(Individual_Pelo, na.rm = TRUE),
        Avg_Sprint_Pelo = mean(Sprint_Pelo, na.rm = TRUE),
        Avg_Pursuit_Pelo = mean(Pursuit_Pelo, na.rm = TRUE),
        Avg_MassStart_Pelo = mean(MassStart_Pelo, na.rm = TRUE),
        # Additional metadata
        MassStart = first(MassStart),
        Sex = "L",
        .groups = "drop"
      )
    
    # Save ladies' relay chrono
    write.csv(ladies_relay_chrono,
              "~/ski/elo/python/biathlon/polars/relay/excel365/ladies_relay_chrono.csv",
              row.names = FALSE)
    log_info(paste("Created ladies' relay chrono with", nrow(ladies_relay_chrono), "team entries"))
  }
  # Process mixed relay chrono
  if(nrow(mixed_relays) > 0) {
    log_info("Creating mixed relay chrono")
    
    # Combine men's and ladies's chronos for mixed events
    combined_chrono <- bind_rows(men_chrono, ladies_chrono)
    
    # Check if there are any Mixed Relay records
    mixed_relay_records <- combined_chrono %>% dplyr::filter(RaceType == "Mixed Relay")
    
    if(nrow(mixed_relay_records) > 0) {
      mixed_relay_chrono <- mixed_relay_records %>%
        # Group by race to get team results
        group_by(Date, City, Country, Nation, RaceType, Season, Race, Event, Elevation) %>%
        summarize(
          # Team rank is the first Place value within each group
          Place = first(Place),
          Points = first(Points),
          # Calculate average Elo/Pelo values for the team
          Avg_Elo = mean(Elo, na.rm = TRUE),
          Avg_Individual_Elo = mean(Individual_Elo, na.rm = TRUE),
          Avg_Sprint_Elo = mean(Sprint_Elo, na.rm = TRUE),
          Avg_Pursuit_Elo = mean(Pursuit_Elo, na.rm = TRUE),
          Avg_MassStart_Elo = mean(MassStart_Elo, na.rm = TRUE),
          Avg_Pelo = mean(Pelo, na.rm = TRUE),
          Avg_Individual_Pelo = mean(Individual_Pelo, na.rm = TRUE),
          Avg_Sprint_Pelo = mean(Sprint_Pelo, na.rm = TRUE),
          Avg_Pursuit_Pelo = mean(Pursuit_Pelo, na.rm = TRUE),
          Avg_MassStart_Pelo = mean(MassStart_Pelo, na.rm = TRUE),
          # Additional metadata
          MassStart = first(MassStart),
          Sex = "Mixed",
          .groups = "drop"
        )
      
      # Save mixed relay chrono
      write.csv(mixed_relay_chrono,
                "~/ski/elo/python/biathlon/polars/relay/excel365/mixed_relay_chrono.csv",
                row.names = FALSE)
      log_info(paste("Created mixed relay chrono with", nrow(mixed_relay_chrono), "team entries"))
    } else {
      log_warn("No Mixed Relay records found in chronological data")
      
      # Create an empty chrono file with the correct structure
      mixed_relay_chrono <- data.frame(
        Date = as.Date(character()),
        City = character(),
        Country = character(),
        Nation = character(),
        RaceType = character(),
        Season = integer(),
        Race = integer(),
        Event = character(),
        Elevation = integer(),
        Place = integer(),
        Points = numeric(),
        Avg_Elo = numeric(),
        Avg_Individual_Elo = numeric(),
        Avg_Sprint_Elo = numeric(),
        Avg_Pursuit_Elo = numeric(),
        Avg_MassStart_Elo = numeric(),
        Avg_Pelo = numeric(),
        Avg_Individual_Pelo = numeric(),
        Avg_Sprint_Pelo = numeric(),
        Avg_Pursuit_Pelo = numeric(),
        Avg_MassStart_Pelo = numeric(),
        MassStart = integer(),
        Sex = character(),
        stringsAsFactors = FALSE
      )
      
      write.csv(mixed_relay_chrono,
                "~/ski/elo/python/biathlon/polars/relay/excel365/mixed_relay_chrono.csv",
                row.names = FALSE)
      log_info("Created empty mixed relay chrono file with correct structure")
    }
  }
  
  # Process single mixed relay chrono
  if(nrow(single_mixed_relays) > 0) {
    log_info("Creating single mixed relay chrono")
    
    # Ensure we have combined chronos
    if(!exists("combined_chrono")) {
      combined_chrono <- bind_rows(men_chrono, ladies_chrono)
    }
    
    # Check if there are any Single Mixed Relay records
    single_mixed_relay_records <- combined_chrono %>% dplyr::filter(RaceType == "Single Mixed Relay")
    
    if(nrow(single_mixed_relay_records) > 0) {
      single_mixed_relay_chrono <- single_mixed_relay_records %>%
        # Group by race to get team results
        group_by(Date, City, Country, Nation, RaceType, Season, Race, Event, Elevation) %>%
        summarize(
          # Team rank is the first Place value within each group
          Place = first(Place),
          Points = first(Points),
          # Calculate average Elo/Pelo values for the team
          Avg_Elo = mean(Elo, na.rm = TRUE),
          Avg_Individual_Elo = mean(Individual_Elo, na.rm = TRUE),
          Avg_Sprint_Elo = mean(Sprint_Elo, na.rm = TRUE),
          Avg_Pursuit_Elo = mean(Pursuit_Elo, na.rm = TRUE),
          Avg_MassStart_Elo = mean(MassStart_Elo, na.rm = TRUE),
          Avg_Pelo = mean(Pelo, na.rm = TRUE),
          Avg_Individual_Pelo = mean(Individual_Pelo, na.rm = TRUE),
          Avg_Sprint_Pelo = mean(Sprint_Pelo, na.rm = TRUE),
          Avg_Pursuit_Pelo = mean(Pursuit_Pelo, na.rm = TRUE),
          Avg_MassStart_Pelo = mean(MassStart_Pelo, na.rm = TRUE),
          # Additional metadata
          MassStart = first(MassStart),
          Sex = "Mixed",
          .groups = "drop"
        )
      
      # Save single mixed relay chrono
      write.csv(single_mixed_relay_chrono,
                "~/ski/elo/python/biathlon/polars/relay/excel365/single_mixed_relay_chrono.csv",
                row.names = FALSE)
      log_info(paste("Created single mixed relay chrono with", nrow(single_mixed_relay_chrono), "team entries"))
    } else {
      log_warn("No Single Mixed Relay records found in chronological data")
      
      # Create an empty chrono file with the correct structure
      single_mixed_relay_chrono <- data.frame(
        Date = as.Date(character()),
        City = character(),
        Country = character(),
        Nation = character(),
        RaceType = character(),
        Season = integer(),
        Race = integer(),
        Event = character(),
        Elevation = integer(),
        Place = integer(),
        Points = numeric(),
        Avg_Elo = numeric(),
        Avg_Individual_Elo = numeric(),
        Avg_Sprint_Elo = numeric(),
        Avg_Pursuit_Elo = numeric(),
        Avg_MassStart_Elo = numeric(),
        Avg_Pelo = numeric(),
        Avg_Individual_Pelo = numeric(),
        Avg_Sprint_Pelo = numeric(),
        Avg_Pursuit_Pelo = numeric(),
        Avg_MassStart_Pelo = numeric(),
        MassStart = integer(),
        Sex = character(),
        stringsAsFactors = FALSE
      )
      
      write.csv(single_mixed_relay_chrono,
                "~/ski/elo/python/biathlon/polars/relay/excel365/single_mixed_relay_chrono.csv",
                row.names = FALSE)
      log_info("Created empty single mixed relay chrono file with correct structure")
    }
  }
  
  return(TRUE)
}

# Combine predictions from multiple races
combine_predictions <- function(race_dfs, startlist, is_relay = FALSE) {
  log_info("Combining race predictions")
  
  # Different ID column based on type
  id_col <- if(is_relay) "Nation" else "ID"
  participant_col <- if(is_relay) "Nation" else "Skier"
  
  # Start with first race
  log_info("Starting with first race data")
  final_predictions <- race_dfs[[1]] %>%
    rename(
      Race1_Base = Base_Prediction,
      Race1_Period = period_adjustment,
      Race1_Elevation = elevation_adjustment,
      Race1_Points = Final_Prediction,
      Race1_Safe = Safe_Prediction,
      Race1_Upside = Upside_Prediction,
      Race1_Volatility = prediction_volatility,
      Race1_Ratio = volatility_ratio,
      Race1_Confidence = confidence_factor,
      Race1_Probability = Race1_Prob
    )
  
  # Add Sex column if needed
  if(!is_relay) {
    final_predictions <- final_predictions %>%
      left_join(
        startlist %>% dplyr::select(Skier, ID, Sex),
        by = "Skier"
      )
  }
  
  # Add remaining races dynamically
  if(length(race_dfs) > 1) {
    for(i in 2:length(race_dfs)) {
      log_info(paste("Adding race", i, "to combined predictions"))
      
      # Get probability column name
      race_prob_col <- paste0("Race", i, "_Prob")
      
      # Check if Race{i}_Prob exists in this race_df
      if(!race_prob_col %in% names(race_dfs[[i]])) {
        log_warn(paste("Race probability column", race_prob_col, "not found in race_dfs[[", i, "]]"))
        
        # Try to fix by getting from startlist
        if(race_prob_col %in% names(startlist)) {
          log_info("Copying probability from original startlist")
          race_dfs[[i]][[race_prob_col]] <- startlist[match(race_dfs[[i]][[participant_col]], startlist[[participant_col]]), race_prob_col]
        } else {
          log_warn("Cannot find probability in startlist either, using default value")
          race_dfs[[i]][[race_prob_col]] <- 0
        }
      }
      
      # Handle Race{i}_Probability column properly
      final_predictions <- final_predictions %>%
        left_join(
          race_dfs[[i]] %>%
            rename(
              !!paste0("Race", i, "_Base") := Base_Prediction,
              !!paste0("Race", i, "_Period") := period_adjustment,
              !!paste0("Race", i, "_Elevation") := elevation_adjustment,
              !!paste0("Race", i, "_Points") := Final_Prediction,
              !!paste0("Race", i, "_Safe") := Safe_Prediction,
              !!paste0("Race", i, "_Upside") := Upside_Prediction,
              !!paste0("Race", i, "_Volatility") := prediction_volatility,
              !!paste0("Race", i, "_Ratio") := volatility_ratio,
              !!paste0("Race", i, "_Confidence") := confidence_factor,
              !!paste0("Race", i, "_Probability") := !!sym(paste0("Race", i, "_Prob"))
            ) %>%
            dplyr::select(!!participant_col, 
                          !!paste0("Race", i, "_Base"),
                          !!paste0("Race", i, "_Period"),
                          !!paste0("Race", i, "_Elevation"),
                          !!paste0("Race", i, "_Points"),
                          !!paste0("Race", i, "_Safe"),
                          !!paste0("Race", i, "_Upside"),
                          !!paste0("Race", i, "_Volatility"),
                          !!paste0("Race", i, "_Ratio"),
                          !!paste0("Race", i, "_Confidence"),
                          !!paste0("Race", i, "_Probability")),
          by = participant_col
        )
    }
  }
  
  # Check if probability columns exist in final predictions
  prob_cols <- paste0("Race", 1:length(race_dfs), "_Probability")
  for(col in prob_cols) {
    if(!col %in% names(final_predictions)) {
      log_warn(paste("Probability column", col, "missing from final predictions!"))
    } else {
      log_info(paste("Probability column", col, "exists in final predictions"))
      log_info(paste("  Mean:", mean(final_predictions[[col]], na.rm = TRUE)))
      log_info(paste("  Sum:", sum(final_predictions[[col]], na.rm = TRUE)))
    }
  }
  
  # Weighted sum expression that accounts for Race{i}_Probability
  weighted_sum_expr <- function(prefix, n_races) {
    terms <- sapply(1:n_races, function(i) {
      paste0("Race", i, "_", prefix, " * Race", i, "_Probability")
    })
    parse(text = paste(terms, collapse = " + "))
  }
  
  avg_expr <- function(prefix, n_races) {
    syms <- paste0("Race", 1:n_races, "_", prefix)
    parse(text = paste0("(", paste(syms, collapse = " + "), ")/", n_races))
  }
  
  # Calculate totals dynamically based on number of races, using probability-weighted sums
  log_info("Calculating totals with probability weighting")
  final_predictions <- final_predictions %>%
    mutate(
      # Use probability-weighted sums for point calculations
      Total_Points = eval(weighted_sum_expr("Points", length(race_dfs))),
      Total_Period = eval(weighted_sum_expr("Period", length(race_dfs))),
      Total_Elevation = eval(weighted_sum_expr("Elevation", length(race_dfs))),
      Total_Safe = eval(weighted_sum_expr("Safe", length(race_dfs))),
      Total_Upside = eval(weighted_sum_expr("Upside", length(race_dfs))),
      Avg_Volatility = eval(avg_expr("Volatility", length(race_dfs))),
      Avg_Confidence = eval(avg_expr("Confidence", length(race_dfs)))
    ) %>%
    arrange(desc(Total_Points))
  
  # Select columns dynamically based on number of races
  select_cols <- c(participant_col)
  
  # For individual races, include ID, Nation, Sex
  if(!is_relay) {
    select_cols <- c(select_cols, "ID", "Nation", "Sex")
  }
  
  for(i in 1:length(race_dfs)) {
    select_cols <- c(select_cols,
                     paste0("Race", i, "_Base"),
                     paste0("Race", i, "_Period"),
                     paste0("Race", i, "_Elevation"),
                     paste0("Race", i, "_Points"),
                     paste0("Race", i, "_Safe"),
                     paste0("Race", i, "_Upside"),
                     paste0("Race", i, "_Volatility"),
                     paste0("Race", i, "_Ratio"),
                     paste0("Race", i, "_Confidence"),
                     paste0("Race", i, "_Probability"))
  }
  select_cols <- c(select_cols,
                   "Total_Points", "Total_Safe", "Total_Upside",
                   "Total_Period", "Total_Elevation",
                   "Avg_Volatility", "Avg_Confidence")
  
  log_info("Returning final predictions")
  final_predictions %>%
    dplyr::select(all_of(select_cols))
}

# Function to prepare startlist data with ELO information
prepare_startlist_data <- function(startlist, race_df, pelo_col, is_relay = FALSE) {
  # Print some debug info
  
  log_info(paste("Preparing startlist data for", pelo_col))
  
  # Dynamically get race probability columns - important to preserve these!
  race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
  log_info(paste("Race probability columns found:", paste(race_prob_cols, collapse=", ")))
  
  # Different ID column based on type
  id_col <- if(is_relay) "Nation" else "ID"
  participant_col <- if(is_relay) "Nation" else "Skier"
  
  # For relays, handle differently since startlist already has team average Elos
  if(is_relay) {
    # Get all columns that might be needed for the model
    elo_cols <- c("Avg_Sprint_Elo", "Avg_Individual_Elo", "Avg_MassStart_Elo", "Avg_Pursuit_Elo", "Avg_Elo")
    pelo_cols <- c("Avg_Sprint_Pelo", "Avg_Individual_Pelo", "Avg_MassStart_Pelo", "Avg_Pursuit_Pelo", "Avg_Pelo")
    
    # Select needed columns from startlist
    result_cols <- c("Nation", elo_cols, race_prob_cols)
    result_df <- startlist %>%
      dplyr::select(any_of(result_cols))
    
    # Get recent points from historical data if available
    if("Points" %in% names(race_df)) {
      recent_points <- race_df %>%
        dplyr::filter(Nation %in% result_df$Nation) %>%
        group_by(Nation) %>%
        arrange(Season, Race) %>%
        slice_tail(n = 5) %>%
        summarise(
          Prev_Points_Weighted = if(n() > 0) 
            weighted.mean(Points, w = seq_len(n()), na.rm = TRUE) 
          else 0
        )
      
      # Add previous points if they exist
      if(nrow(recent_points) > 0) {
        result_df <- result_df %>%
          left_join(recent_points, by = "Nation") %>%
          mutate(Prev_Points_Weighted = replace_na(Prev_Points_Weighted, 0))
      } else {
        result_df$Prev_Points_Weighted <- 0
      }
    } else {
      # If no points in historical data
      result_df$Prev_Points_Weighted <- 0
    }
    
    # Set default values for missing Elo columns
    for(col in elo_cols) {
      if(!col %in% names(result_df)) {
        log_info(paste("Adding missing Elo column:", col))
        result_df[[col]] <- 0
      }
    }
  } else {
    # For individual races, use the original approach
    base_df <- startlist %>%
      dplyr::select(Skier, ID, Nation, Sex, all_of(race_prob_cols))
    
    # For individual races
    elo_cols <- c("Sprint_Elo", "Individual_Elo", "MassStart_Elo", "Pursuit_Elo", "Elo")
    pelo_cols <- c("Sprint_Pelo", "Individual_Pelo", "MassStart_Pelo", "Pursuit_Pelo", "Pelo")
    
    # Get most recent Elo values
    most_recent_elos <- race_df %>%
      dplyr::filter(Skier %in% base_df$Skier) %>%
      group_by(Skier) %>%
      arrange(Date, Season, Race) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      dplyr::select(Skier, any_of(elo_cols))
    
    # Debug: Check elo columns
    log_info(paste("Available elo columns:", paste(names(most_recent_elos), collapse=", ")))
    
    # Get recent points for specific race type
    recent_points <- race_df %>%
      dplyr::filter(Skier %in% base_df$Skier) %>%
      group_by(Skier) %>%
      arrange(Season, Race) %>%
      slice_tail(n = 5) %>%
      summarise(
        Prev_Points_Weighted = if(n() > 0) 
          weighted.mean(Points, w = seq_len(n()), na.rm = TRUE) 
        else 0
      )
    
    # Combine all data
    result_df <- base_df %>%
      left_join(most_recent_elos, by = "Skier") %>%
      left_join(recent_points, by = "Skier")
  }
  
  # For both relay and individual: create Pelo percentage columns
  # Ensure we have all the required Pelo_Pct columns for model prediction
  for(i in seq_along(pelo_cols)) {
    pelo_pct_col <- paste0(pelo_cols[i], "_Pct")
    
    # First check if we have the corresponding Elo column to map from
    elo_col <- elo_cols[i]
    if(elo_col %in% names(result_df)) {
      # If we have race_df with this column, get max values for normalization
      if(elo_col %in% names(race_df)) {
        max_val <- max(race_df[[elo_col]], na.rm = TRUE)
        if(!is.na(max_val) && max_val > 0) {
          log_info(paste("Calculating", pelo_pct_col, "from", elo_col))
          result_df[[pelo_pct_col]] <- result_df[[elo_col]] / max_val
        } else {
          log_info(paste("Using default value for", pelo_pct_col, "(max value issue)"))
          result_df[[pelo_pct_col]] <- replace_na_with_quartile(rep(NA, nrow(result_df)))
        }
      } else {
        # If not available in race_df, normalize within the current dataset
        max_val <- max(result_df[[elo_col]], na.rm = TRUE)
        if(!is.na(max_val) && max_val > 0) {
          log_info(paste("Calculating", pelo_pct_col, "from", elo_col, "(internal max)"))
          result_df[[pelo_pct_col]] <- result_df[[elo_col]] / max_val
        } else {
          log_info(paste("Using default value for", pelo_pct_col, "(internal max issue)"))
          result_df[[pelo_pct_col]] <- replace_na_with_quartile(rep(NA, nrow(result_df)))
        }
      }
    } else if(!pelo_pct_col %in% names(result_df)) {
      # If we don't have the Elo column and the PCT doesn't exist yet
      log_info(paste("Creating missing Pelo Pct column:", pelo_pct_col))
      result_df[[pelo_pct_col]] <- 0.5  # Default to 0.5 (middle value)
    }
  }
  
  # Replace NAs with first quartile
  result_df <- result_df %>%
    mutate(
      across(
        ends_with("_Pct"),
        ~replace_na_with_quartile(.x)
      ),
      Prev_Points_Weighted = replace_na(Prev_Points_Weighted, 0)
    )
  
  # Check race probability columns in result
  for(col in race_prob_cols) {
    if(!is.null(result_df[[col]])) {
      log_info(paste("Race probability column", col, "summary:"))
      log_info(paste("  Mean:", mean(result_df[[col]], na.rm = TRUE)))
      log_info(paste("  Sum:", sum(result_df[[col]], na.rm = TRUE)))
      log_info(paste("  Max:", max(result_df[[col]], na.rm = TRUE)))
    } else {
      log_warn(paste("Race probability column", col, "is missing from result_df!"))
    }
  }
  
  # Ensure result_df has all the columns needed by the model
  log_info(paste("Final columns in result_df:", paste(names(result_df), collapse=", ")))
  
  return(result_df)
}

# Function to get points based on place
get_points <- function(place, RaceType) {
  points_list <- if(RaceType == "MassStart") mass_start_points else regular_points
  
  if (place >= 1 && place <= length(points_list)) {
    return(points_list[place])
  }
  return(0)
}

# Normalization function for position probabilities
normalize_position_probabilities <- function(predictions, race_prob_col, position_thresholds) {
  # Make a copy to avoid modifying the original data frame
  normalized <- predictions
  
  # Log initial sums before any modifications
  log_info("Position probability sums BEFORE normalization:")
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    if(prob_col %in% names(normalized)) {
      initial_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
      log_info(sprintf("  %s: %.2f%% (target: %d%%)", 
                       prob_col, initial_sum, 100 * threshold))
    }
  }
  
  # For each threshold, adjust and normalize probabilities
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    
    # First, adjust by race participation probability
    if(race_prob_col %in% names(normalized)) {
      # Log sum before race probability adjustment
      sum_before_race_adj <- sum(normalized[[prob_col]], na.rm = TRUE)
      
      # Apply race probability adjustment
      normalized[[prob_col]] <- normalized[[prob_col]] * normalized[[race_prob_col]]
      
      # Log sum after race probability adjustment
      sum_after_race_adj <- sum(normalized[[prob_col]], na.rm = TRUE)
      log_info(sprintf("  %s after race prob adjustment: %.2f%% (scaling by race participation)", 
                       prob_col, sum_after_race_adj))
    }
    
    # Only normalize among athletes who actually have a chance to participate (race_prob > 0)
    if(race_prob_col %in% names(normalized)) {
      participating_mask <- normalized[[race_prob_col]] > 0
      participating_indices <- which(participating_mask)
    } else {
      # Fallback if no race probability column
      participating_mask <- rep(TRUE, nrow(normalized))
      participating_indices <- seq_len(nrow(normalized))
    }
    
    # Calculate the current sum only among participating athletes
    current_sum <- sum(normalized[[prob_col]][participating_mask], na.rm = TRUE)
    
    # Target sum should be 100 * threshold (e.g., 100% for top 1, 300% for top 3)
    target_sum <- 100 * threshold
    
    # Normalize only if current sum is not zero and we have participating athletes
    if(current_sum > 0 && length(participating_indices) > 0) {
      # Apply scaling factor to adjust the probabilities (only for participating athletes)
      scaling_factor <- target_sum / current_sum
      normalized[[prob_col]][participating_indices] <- normalized[[prob_col]][participating_indices] * scaling_factor
      
      # Cap individual probabilities at 100% (only among participating athletes)
      over_hundred <- intersect(which(normalized[[prob_col]] > 100), participating_indices)
      if(length(over_hundred) > 0) {
        log_info(sprintf("  Capping %d participants with >100%% probability for %s", 
                         length(over_hundred), prob_col))
        
        # Calculate excess probability that needs to be redistributed
        excess <- sum(normalized[[prob_col]][over_hundred] - 100)
        
        # Cap values at 100%
        normalized[[prob_col]][over_hundred] <- 100
        
        # Redistribute the excess to other participating athletes under 100%
        under_hundred <- intersect(which(normalized[[prob_col]] < 100), participating_indices)
        if(length(under_hundred) > 0 && excess > 0) {
          # Get current sum of under-100 probabilities (only participating athletes)
          under_sum <- sum(normalized[[prob_col]][under_hundred])
          
          # Calculate scaling factor for redistribution
          if(under_sum > 0) {
            redistrib_factor <- (under_sum + excess) / under_sum
            normalized[[prob_col]][under_hundred] <- normalized[[prob_col]][under_hundred] * redistrib_factor
            
            # Recursively cap again if needed (unlikely but possible)
            over_hundred_after <- intersect(which(normalized[[prob_col]] > 100), participating_indices)
            if(length(over_hundred_after) > 0) {
              log_info("  Recursive capping needed after redistribution")
              normalized[[prob_col]][over_hundred_after] <- 100
            }
          }
        }
      }
      
      log_info(sprintf("  %s normalization: applied scaling factor of %.4f", 
                       prob_col, scaling_factor))
    } else {
      # If sum is zero, distribute evenly among PARTICIPATING athletes only
      # This is a fallback that should rarely be needed
      log_warn(paste("Zero sum for", prob_col, "- distributing evenly among participating athletes"))
      if(length(participating_indices) > 0) {
        # Set all non-participants to 0 and distribute among participants
        normalized[[prob_col]] <- 0
        normalized[[prob_col]][participating_indices] <- target_sum / length(participating_indices)
      } else {
        # If no participants, set all to 0
        normalized[[prob_col]] <- 0
      }
    }
    
    # Final check to ensure we're close to the target sum
    final_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
    if(abs(final_sum - target_sum) > 1) {  # Allow for small rounding differences
      log_warn(sprintf("  %s sum after capping: %.2f%% (target: %.2f%%)", 
                       prob_col, final_sum, target_sum))
    }
  }
  
  # APPLY MONOTONIC CONSTRAINTS: Ensure Win <= Podium <= Top5 <= Top10 <= Top30
  log_info("Applying monotonic constraints...")

  # Get available probability columns in ascending order
  prob_cols <- paste0("prob_top", sort(position_thresholds))
  prob_cols <- prob_cols[prob_cols %in% names(normalized)]

  # For each athlete, ensure probabilities are monotonically non-decreasing
  # But only apply to athletes with race participation > 0
  for(i in 1:nrow(normalized)) {
    # Skip monotonic constraints for athletes with 0% race participation
    if(race_prob_col %in% names(normalized) && normalized[[race_prob_col]][i] == 0) {
      next  # Skip this athlete - they should stay at 0% for all positions
    }
    
    probs <- numeric(length(prob_cols))
    for(j in 1:length(prob_cols)) {
      probs[j] <- normalized[[prob_cols[j]]][i]
    }
    
    # Apply monotonic adjustment: each probability should be >= previous one
    for(j in 2:length(probs)) {
      if(probs[j] < probs[j-1]) {
        probs[j] <- probs[j-1]  # Set to previous value
      }
    }
    
    # Update the normalized dataframe
    for(j in 1:length(prob_cols)) {
      normalized[[prob_cols[j]]][i] <- probs[j]
    }
  }

  # RE-NORMALIZE after monotonic adjustment to maintain target sums
  # Only normalize among participating athletes (race_prob > 0)
  log_info("Re-normalizing after monotonic constraints...")
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    
    if(prob_col %in% names(normalized)) {
      # Only consider participating athletes for re-normalization
      if(race_prob_col %in% names(normalized)) {
        participating_mask <- normalized[[race_prob_col]] > 0
        participating_indices <- which(participating_mask)
      } else {
        participating_mask <- rep(TRUE, nrow(normalized))
        participating_indices <- seq_len(nrow(normalized))
      }
      
      current_sum <- sum(normalized[[prob_col]][participating_mask], na.rm = TRUE)
      target_sum <- 100 * threshold
      
      if(current_sum > 0 && length(participating_indices) > 0) {
        scaling_factor <- target_sum / current_sum
        normalized[[prob_col]][participating_indices] <- normalized[[prob_col]][participating_indices] * scaling_factor
        
        # Cap at 100% again (only for participating athletes)
        over_hundred <- intersect(which(normalized[[prob_col]] > 100), participating_indices)
        if(length(over_hundred) > 0) {
          normalized[[prob_col]][over_hundred] <- 100
        }
      }
    }
  }
  
  # Log final sums after all adjustments
  log_info("Position probability sums AFTER normalization and monotonic constraints:")
  for(threshold in position_thresholds) {
    prob_col <- paste0("prob_top", threshold)
    if(prob_col %in% names(normalized)) {
      final_sum <- sum(normalized[[prob_col]], na.rm = TRUE)
      log_info(sprintf("  %s: %.2f%% (target: %d%%)", 
                       prob_col, final_sum, 100 * threshold))
    }
  }
  
  # Return normalized probabilities
  return(normalized)
}

# Helper function to format position probability results
format_position_results <- function(position_results, race_date, gender, is_relay = FALSE, relay_type = NULL) {
  # Create a more reader-friendly version
  participant_col <- if(is_relay) "Nation" else "Skier"
  
  # Use standard R operations for adding columns
  formatted_results <- position_results
  
  # Add participation and probability columns
  formatted_results$Participation <- NA_real_
  formatted_results$Win <- NA_real_
  formatted_results$Podium <- NA_real_
  formatted_results$Top5 <- NA_real_
  formatted_results$Top10 <- NA_real_
  formatted_results$Top30 <- NA_real_
  
  # Process row by row instead of using rowwise()
  for(i in 1:nrow(formatted_results)) {
    race_num <- formatted_results$Race[i]
    race_prob_col <- paste0("Race", race_num, "_Prob")
    
    if(race_prob_col %in% names(position_results)) {
      formatted_results$Participation[i] <- position_results[[race_prob_col]][i]
    }
    
    formatted_results$Win[i] <- position_results$prob_top1[i]
    formatted_results$Podium[i] <- position_results$prob_top3[i]
    formatted_results$Top5[i] <- position_results$prob_top5[i]
    formatted_results$Top10[i] <- position_results$prob_top10[i]
    formatted_results$Top30[i] <- position_results$prob_top30[i]
  }
  
  # Select columns without sym()
  select_cols <- c(participant_col)
  if(!is_relay) {
    select_cols <- c(select_cols, "ID", "Nation", "Sex")
  }
  select_cols <- c(select_cols, "Race", "Participation", "Win", "Podium", "Top5", "Top10", "Top30")
  
  formatted_results <- formatted_results[, select_cols]
  
  # Sort results by Race and Win
  formatted_results <- formatted_results[order(formatted_results$Race, -formatted_results$Win),]
  
  race_folder <- format(race_date, "%Y%m%d")
  dir_path <- paste0(
    "~/blog/daehl-e/content/post/biathlon/drafts/race-picks/", 
    race_folder
  )
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Split by race
  races <- unique(formatted_results$Race)
  race_dfs <- list()
  
  for(race_num in races) {
    race_df <- formatted_results[formatted_results$Race == race_num, ]
    race_df <- race_df[, colnames(race_df) != "Race"]  # Remove Race column
    
    if(is_relay) {
      # Relay sheet naming
      relay_prefix <- if(is.null(relay_type)) "Relay" else relay_type
      sheet_name <- paste0(relay_prefix, " Race ", race_num)
    } else {
      # Individual race sheet naming
      sheet_name <- paste0(
        ifelse(gender == "men", "Men", "Ladies"),
        " Race ", race_num
      )
    }
    
    race_dfs[[sheet_name]] <- race_df
  }
  
  # Save to Excel
  if(is_relay) {
    relay_type_short <- if(is.null(relay_type)) "relay" else gsub(" ", "_", tolower(relay_type))
    output_file <- file.path(
      dir_path,
      paste0(ifelse(is.null(gender), "", paste0(gender, "_")), 
             relay_type_short, "_position_probabilities.xlsx")
    )
  } else {
    output_file <- file.path(
      dir_path,
      paste0(ifelse(gender == "men", "men", "ladies"), "_position_probabilities.xlsx")
    )
  }
  
  write.xlsx(race_dfs, output_file)
  
  log_info(paste("Formatted position probabilities saved to", output_file))
  
  return(race_dfs)
}

# Function to create top contenders summary for biathlon 
create_top_contenders_summary <- function(results_list, race_date) {
  short_display_name <- case_when(
    display_name == "Men" ~ "M",
    display_name == "Ladies" ~ "L", 
    display_name == "Men Relay" ~ "MR",
    display_name == "Ladies Relay" ~ "LR",
    display_name == "Mixed Relay" ~ "MXR", 
    display_name == "Single Mixed Relay" ~ "SMR",
    TRUE ~ substr(display_name, 1, 3)  # Fallback
  )
  race_folder <- format(race_date, "%Y%m%d")
  dir_path <- paste0(
    "~/blog/daehl-e/content/post/biathlon/drafts/race-picks/", 
    race_folder
  )
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  top_contenders <- list()
  
  # Process each result set
  for(result_name in names(results_list)) {
    # Skip if this result doesn't have position predictions
    if(!("position_predictions" %in% names(results_list[[result_name]]))) {
      next
    }
    
    position_data <- results_list[[result_name]]$position_predictions
    
    # Determine if this is a relay result
    is_relay <- grepl("relay", result_name)
    participant_col <- if(is_relay) "Nation" else "Skier"
    
    # Get races
    races <- unique(position_data$Race)
    
    # Format the display name
    display_name <- result_name
    if(result_name == "men") display_name <- "Men"
    else if(result_name == "ladies") display_name <- "Ladies"
    else if(result_name == "men_relay") display_name <- "Men Relay"
    else if(result_name == "ladies_relay") display_name <- "Ladies Relay"
    else if(result_name == "mixed_relay") display_name <- "Mixed Relay"
    else if(result_name == "single_mixed_relay") display_name <- "Single Mixed Relay"
    
    for(race_num in races) {
      race_df <- position_data[position_data$Race == race_num, ]
      
      # Top 5 for win probability
      win_order <- order(-race_df$prob_top1)
      win_contenders <- race_df[win_order[1:min(5, length(win_order))], ]
      
      if(!is_relay) {
        win_contenders <- win_contenders[, c(participant_col, "Nation", "prob_top1")]
      } else {
        win_contenders <- win_contenders[, c(participant_col, "prob_top1")]
      }
      
      names(win_contenders)[names(win_contenders) == "prob_top1"] <- "Win%"
      
      sheet_name <- paste0(display_name, " Race ", race_num, " - Win")
      top_contenders[[sheet_name]] <- win_contenders
      
      # Top 5 for podium probability
      podium_order <- order(-race_df$prob_top3)
      podium_contenders <- race_df[podium_order[1:min(5, length(podium_order))], ]
      
      if(!is_relay) {
        podium_contenders <- podium_contenders[, c(participant_col, "Nation", "prob_top3")]
      } else {
        podium_contenders <- podium_contenders[, c(participant_col, "prob_top3")]
      }
      
      names(podium_contenders)[names(podium_contenders) == "prob_top3"] <- "Podium%"
      
      sheet_name <- paste0(display_name, " Race ", race_num, " - Podium")
      top_contenders[[sheet_name]] <- podium_contenders
      
      # Top 5 for Top-5 probability
      top5_order <- order(-race_df$prob_top5)
      top5_contenders <- race_df[top5_order[1:min(5, length(top5_order))], ]
      
      if(!is_relay) {
        top5_contenders <- top5_contenders[, c(participant_col, "Nation", "prob_top5")]
      } else {
        top5_contenders <- top5_contenders[, c(participant_col, "prob_top5")]
      }
      
      names(top5_contenders)[names(top5_contenders) == "prob_top5"] <- "Top5%"
      
      sheet_name <- paste0(display_name, " Race ", race_num, " - Top5")
      top_contenders[[sheet_name]] <- top5_contenders
    }
  }
  
  # Save to Excel
  output_file <- file.path(dir_path, "biathlon_top_contenders.xlsx")
  write.xlsx(top_contenders, output_file)
  
  log_info(paste("Biathlon top contenders summary saved to", output_file))
  
  return(top_contenders)
}

# Preprocessing function for historical race data
preprocess_data <- function(df, is_relay = FALSE) {
  # Load races data to determine points systems for historical races
  races_data <- read.csv("~/ski/elo/python/biathlon/polars/excel365/races.csv", 
                         stringsAsFactors = FALSE) %>%
    mutate(Date = as.Date(Date, format="%Y-%m-%d"))
  
  participant_col <- if(is_relay) "Nation" else "Skier"
  id_col <- if(is_relay) "Nation" else "ID"
  
  # For relay data, we want to keep the team structure
  if(is_relay) {
    log_info("Processing relay data - using team-based metrics")
    
    # Define the relay-specific columns we expect/need (without relay-specific PELOs)
    relay_cols <- c("Avg_Sprint_Elo", "Avg_Individual_Elo", "Avg_MassStart_Elo", "Avg_Pursuit_Elo", "Avg_Elo",
                    "Avg_Sprint_Pelo", "Avg_Individual_Pelo", "Avg_MassStart_Pelo", "Avg_Pursuit_Pelo", "Avg_Pelo")
    # Check which columns exist and create missing ones
    for(col in relay_cols) {
      if(!col %in% names(df)) {
        log_info(paste("Creating missing relay column:", col))
        # If we're missing a column but have the corresponding base column without Avg_
        base_col <- gsub("^Avg_", "", col)
        if(base_col %in% names(df)) {
          log_info(paste("Using base column", base_col, "to create", col))
          df[[col]] <- df[[base_col]]
        } else {
          df[[col]] <- 0
        }
      }
    }
  }
  
  # First calculate points using data with appropriate points system
  df_with_points <- df %>%
    # Add points based on the race type if they don't already exist
    mutate(
      Points = if("Points" %in% names(df)) {
        Points
      } else {
        mapply(function(place, RaceType) {
          get_points(place, RaceType)
        }, Place, RaceType)
      }
    ) %>%
    # Sort
    arrange(Season, Race, Place)
  
  # Calculate weighted previous points separately for each race type
  df_with_points <- df_with_points %>%
    # Group by ID and race type
    group_by(!!sym(id_col), RaceType) %>%
    arrange(Season, Race) %>%
    mutate(Prev_Points_Weighted = sapply(1:n(), function(j) {
      if (j == 1) return(0)
      start_index <- max(1, j - 5)
      num_races <- j - start_index
      weights <- seq(1, num_races)
      weighted.mean(Points[start_index:(j-1)], w = weights, na.rm = TRUE)
    })) %>%
    ungroup()
  
  # Check if Pelo columns exist, if not create them
  if(is_relay) {
    pelo_cols <- c("Avg_Sprint_Pelo", "Avg_Individual_Pelo", "Avg_MassStart_Pelo", "Avg_Pursuit_Pelo", "Avg_Pelo")
  } else {
    pelo_cols <- c("Sprint_Pelo", "Individual_Pelo", "MassStart_Pelo", "Pursuit_Pelo", "Pelo")
  }
  
  # Make sure these columns exist (create if missing)
  for (col in pelo_cols) {
    if (!col %in% names(df_with_points)) {
      log_info(paste("Creating missing column:", col))
      df_with_points[[col]] <- 0
    }
  }
  
  # Now apply other preprocessing steps and dplyr::filter for recent data
  processed_df <- df_with_points %>%
    # Add period (biathlon has 4 periods per season)
    group_by(Season) %>%
    mutate(
      Num_Races = max(Race),
      Period = case_when(
        Num_Races <= 5 ~ 1,
        Num_Races <= 10 ~ 2,
        Num_Races <= 15 ~ 3,
        TRUE ~ 4
      )
    ) %>%
    ungroup() %>%
    # Add elevation flag (0 if below 1300m, 1 if above)
    mutate(
      Elevation_Flag = ifelse(Elevation >= 1300, 1, 0)
    ) %>%
    # dplyr::filter relevant races and add cumulative points
    dplyr::filter(
      Season >= max(Season-10)
    ) %>%
    group_by(!!sym(id_col), Season) %>%
    mutate(Cumulative_Points = cumsum(Points)) %>%
    ungroup() %>%
    # Handle NAs and calculate percentages
    group_by(Season, Race) %>%
    mutate(
      across(
        all_of(pelo_cols),
        ~replace_na_with_quartile(.x)
      )
    ) %>%
    # Calculate percentages for each Pelo column
    mutate(
      across(
        all_of(pelo_cols),
        ~{
          max_val <- max(.x, na.rm = TRUE)
          if (max_val == 0) return(rep(0, length(.x)))
          .x / max_val
        },
        .names = "{.col}_Pct"
      )
    ) %>%
    ungroup()
  
  # Ensure all required Pelo_Pct columns exist
  pct_cols <- paste0(pelo_cols, "_Pct")
  for (col in pct_cols) {
    if (!col %in% names(processed_df)) {
      log_info(paste("Creating missing percentage column:", col))
      processed_df[[col]] <- 0
    }
  }
  
  log_info(paste("Preprocessing complete - final dimensions:", nrow(processed_df), "x", ncol(processed_df)))
  log_info(paste("Final columns:", paste(names(processed_df)[1:min(10, length(names(processed_df)))], collapse=", "), "..."))
  
  return(processed_df)
}

create_post_predictions <- function(final_predictions, n_races, gender=NULL, is_relay = FALSE) {
  # Select columns dynamically based on number of races
  participant_col <- if(is_relay) "Nation" else "Skier"
  
  select_cols <- c(participant_col)
  
  if(!is_relay) {
    select_cols <- c(select_cols, "ID", "Nation")
    if("Sex" %in% names(final_predictions)) {
      select_cols <- c(select_cols, "Sex")  # Include Sex if it exists
    }
  }
  
  for(i in 1:n_races) {
    select_cols <- c(select_cols,
                     paste0("Race", i, "_Points"),
                     paste0("Race", i, "_Probability"))
  }
  select_cols <- c(select_cols, "Total_Points")
  
  post_predictions <- final_predictions %>%
    dplyr::select(all_of(select_cols)) %>%
    # Add Sex if not present and not a relay
    mutate(
      Sex = if(!is_relay && "Sex" %in% names(.)) {
        Sex  # Use existing Sex column
      } else if(!is_relay) {
        ifelse(gender == "men", "M", "L")  # Fallback
      } else {
        NULL  # No Sex for relays
      }
    )
  
  # Make sure Sex is in the right position for individuals (after Nation)
  if(!is_relay) {
    post_predictions <- post_predictions %>%
      dplyr::select(Skier, ID, Nation, Sex, everything())
  }
  
  # Arrange by total points
  post_predictions <- post_predictions %>%
    arrange(desc(Total_Points))
  
  # Replace underscores with spaces in column names
  post_predictions <- post_predictions %>%
    rename_with(~ gsub("_", " ", .x), .cols = contains("_"))
  
  return(post_predictions)
}

# Predict races function with support for relay races
# Modified predict_races function with position probability
predict_races <- function(gender, is_relay = FALSE, relay_type = NULL, startlist_override = NULL) {
  # Add logging for the startlist_override
  if(!is.null(startlist_override)) {
    log_info(paste("Using provided startlist_override with", nrow(startlist_override), "entries"))
  }
  
  # Determine the race type and paths based on gender and is_relay
  if(is_relay) {
    participant_col <- "Nation"
    
    # Define races variable based on relay_type FIRST
    if(relay_type == "Relay") {
      if(gender == "men") {
        races <- men_relays
        log_info(paste("Using men's relay races:", nrow(races), "races found"))
      } else if(gender == "ladies") {
        races <- ladies_relays
        log_info(paste("Using ladies' relay races:", nrow(races), "races found"))
      } else {
        races <- relays
        log_info(paste("Using all relay races:", nrow(races), "races found"))
      }
      
      log_info(paste("Using relay races:", nrow(races), "races found"))
      
      # Set chrono_path based on gender, regardless of startlist override
      if(gender == "men") {
        chrono_path <- "~/ski/elo/python/biathlon/polars/relay/excel365/men_relay_chrono.csv"
        log_info("Using men's relay chronological data")
      } else {
        chrono_path <- "~/ski/elo/python/biathlon/polars/relay/excel365/ladies_relay_chrono.csv"
        log_info("Using ladies' relay chronological data")
      }
    } else if(relay_type == "Mixed Relay") {
      races <- mixed_relays
      chrono_path <- "~/ski/elo/python/biathlon/polars/relay/excel365/mixed_relay_chrono.csv"
      log_info(paste("Using mixed relay races:", nrow(races), "races found"))
    } else if(relay_type == "Single Mixed Relay") {
      races <- single_mixed_relays
      chrono_path <- "~/ski/elo/python/biathlon/polars/relay/excel365/single_mixed_relay_chrono.csv"
      log_info(paste("Using single mixed relay races:", nrow(races), "races found"))
    } else {
      races <- relays
      # Default chrono path
      if(gender == "men") {
        chrono_path <- "~/ski/elo/python/biathlon/polars/relay/excel365/men_relay_chrono.csv"
      } else {
        chrono_path <- "~/ski/elo/python/biathlon/polars/relay/excel365/ladies_relay_chrono.csv"
      }
      log_info(paste("Using default relay races:", nrow(races), "races found"))
    }
    
    # Now handle startlist selection - separate from chrono_path selection
    if(!is.null(startlist_override)) {
      startlist <- startlist_override
    } else if(relay_type == "Relay") {
      if(gender == "men") {
        startlist <- men_relay_startlist
        log_info("Processing men's relay data")
      } else {
        startlist <- ladies_relay_startlist
        log_info("Processing ladies' relay data")
      }
    } else if(relay_type == "Mixed Relay") {
      startlist <- mixed_relay_startlist
      log_info("Processing mixed relay data")
    } else if(relay_type == "Single Mixed Relay") {
      startlist <- single_mixed_relay_startlist
      log_info("Processing single mixed relay data")
    } else {
      # Default relay handling if relay_type not specified
      log_warn("No relay type specified, using default settings")
      if(gender == "men") {
        startlist <- men_relay_startlist
      } else {
        startlist <- ladies_relay_startlist
      }
    }
  } else {
    # Individual race handling
    # Set races variable first
    if(gender == "men") {
      races <- men_races
      chrono_path <- "~/ski/elo/python/biathlon/polars/relay/excel365/men_chrono_elevation.csv"
      log_info(paste("Using men's races:", nrow(races), "races found"))
    } else {
      races <- ladies_races
      chrono_path <- "~/ski/elo/python/biathlon/polars/relay/excel365/ladies_chrono_elevation.csv"
      log_info(paste("Using ladies' races:", nrow(races), "races found"))
    }
    
    # Then handle startlist - separate from chrono_path
    if(!is.null(startlist_override)) {
      startlist <- startlist_override
    } else if(gender == "men") {
      startlist <- men_startlist
    } else {
      startlist <- ladies_startlist
    }
    participant_col <- "Skier"
  }
  
  # Log the size of the startlist to track if it's being modified
  log_info(paste("Selected startlist has", nrow(startlist), "entries"))
  log_info(paste("Using chronological data from:", chrono_path))
  
  # Check if races variable is defined and non-empty
  if(!exists("races") || nrow(races) == 0) {
    log_error("No races found for this race type")
    return(NULL)
  }
  
  # Load chronological data
  log_info(paste("Loading chronological data from", chrono_path))
  
  # Handle the fact that some chrono files might not exist yet
  # Create a fallback mechanism for relay chronos
  if(is_relay) {
    tryCatch({
      # Try to read the specified chrono file
      df <- read.csv(chrono_path, stringsAsFactors = FALSE) %>%
        mutate(Date = as.Date(Date)) %>%
        preprocess_data(is_relay = TRUE)
      
      log_info(paste("Successfully read relay chrono data from", chrono_path, 
                     "with", nrow(df), "rows"))
    }, error = function(e) {
      log_warn(paste("Error reading relay chrono file:", e$message))
      log_info("Creating a synthetic relay chronology from the startlist")
      
      # If we can't read the chrono file, create a minimal chronology from the startlist
      # This allows the prediction code to run even if historical team data is missing
      
      # First check if we have a valid startlist
      if(is.null(startlist) || nrow(startlist) == 0) {
        log_error("No startlist available for creating synthetic relay chronology")
        stop("Cannot proceed without chronology or startlist data")
      }
      
      # Create a minimal chronology from the startlist - using <- not <<-
      df <- startlist %>%
        dplyr::select(Nation, 
                      # Include any average Elo columns that exist
                      matches("^Avg_.+_Elo$")) %>%
        # Add minimum required columns for the prediction pipeline
        mutate(
          Date = Sys.Date() - 30,  # Use a recent date
          Season = as.integer(format(Sys.Date(), "%Y")),  # Current year as integer
          Race = 1,
          Place = row_number(),  # Rank teams by their order in the startlist
          City = "Unknown",
          Country = "Unknown",  # Add Country column which is often needed
          Event = "World Cup",
          Elevation = 1000,  # Default value
          RaceType = relay_type,  # String value is fine here
          Points = 0,
          MassStart = 0,  # Add MassStart flag commonly needed
          Elevation_Flag = ifelse(Elevation >= 1300, 1, 0)  # Add Elevation_Flag
        ) %>%
        preprocess_data(is_relay = TRUE)
      
      log_info(paste("Created synthetic relay chronology with", nrow(df), "teams"))
      return(df)  # Return the created dataframe from the error handler
    })
  } else {
    # Standard individual race handling
    df <- read.csv(chrono_path, stringsAsFactors = FALSE) %>%
      mutate(Date = as.Date(Date)) %>%
      preprocess_data(is_relay = is_relay)
  }
  
  # For debugging
  log_info(paste("Chronological data dimensions:", nrow(df), "x", ncol(df)))
  log_info(paste("First few columns:", paste(names(df)[1:min(10, length(names(df)))], collapse=", ")))
  
  # Initialize results list
  race_predictions <- list()
  race_dfs <- list()
  
  # Initialize position predictions list
  position_predictions <- list()
  
  # Define position thresholds
  position_thresholds <- c(1, 3, 5, 10, 30)  # Win, Podium, Top 5, Top 10, Top 30
  
  # Debug: Show race probability columns in startlist
  race_prob_cols <- grep("^Race\\d+_Prob$", names(startlist), value = TRUE)
  log_info(paste("Race probability columns in startlist:", paste(race_prob_cols, collapse=", ")))
  
  # Process each race
  for(i in 1:nrow(races)) {
    log_info(sprintf("Processing %s race %d: %s", 
                     if(is_relay) "relay" else gender, 
                     i, races$racetype[i]))
    
    # Get race probability column name for this race
    race_prob_col <- paste0("Race", i, "_Prob")
    
    race_info <- races %>%
      dplyr::slice(i)
    
    # Use the appropriate points system
    points_system <- if(races$racetype[i] == "MassStart") mass_start_points else regular_points
    
    log_info(paste("Using World Cup points for this race"))
    
    # Debug: Check if this race probability column exists in startlist
    if(race_prob_col %in% names(startlist)) {
      log_info(paste("Race probability column", race_prob_col, "exists in startlist"))
      # Show some stats
      log_info(paste("  Mean:", mean(startlist[[race_prob_col]], na.rm = TRUE)))
      log_info(paste("  Sum:", sum(startlist[[race_prob_col]], na.rm = TRUE)))
    } else {
      log_warn(paste("Race probability column", race_prob_col, "NOT FOUND in startlist!"))
    }
    
    # Filter base dataset for race type
    race_df <- df %>%
      dplyr::filter(RaceType == races$racetype[i])
    
    # Get relevant Pelo column based on race type
    if(is_relay) {
      # For relay races, use general average PELO
      pelo_col <- "Avg_Pelo_Pct"
    } else {
      if(races$racetype[i] == "Sprint") {
        pelo_col <- "Sprint_Pelo_Pct"
      } else if(races$racetype[i] == "Individual") {
        pelo_col <- "Individual_Pelo_Pct"
      } else if(races$racetype[i] == "MassStart") {
        pelo_col <- "MassStart_Pelo_Pct"
      } else if(races$racetype[i] == "Pursuit") {
        pelo_col <- "Pursuit_Pelo_Pct"
      } else {
        pelo_col <- "Pelo_Pct"
      }
    }
    
    # Filter for top performers and add previous points
    race_df_75 <- race_df %>%
      dplyr::filter(get(pelo_col) > 0.75) %>%
      group_by(!!sym(participant_col)) %>%
      arrange(Season, Race) %>%
      ungroup()
    
    # Feature selection and model fitting for points prediction
    response_variable <- "Points"
    
    # Define explanatory variables based on race type
    if(is_relay) {
      explanatory_vars <- c("Avg_Sprint_Pelo_Pct", "Avg_Individual_Pelo_Pct", 
                            "Avg_MassStart_Pelo_Pct", "Avg_Pursuit_Pelo_Pct", 
                            "Avg_Pelo_Pct")
    } else {
      explanatory_vars <- c("Prev_Points_Weighted", 
                            "Sprint_Pelo_Pct", "Individual_Pelo_Pct", 
                            "MassStart_Pelo_Pct", "Pursuit_Pelo_Pct", 
                            "Pelo_Pct")
    }
    
    # Apply quartile imputation to the selected explanatory variables
    for(var in explanatory_vars) {
      if(var %in% names(race_df_75)) {
        race_df_75[[var]] <- replace_na_with_quartile(race_df_75[[var]])
      }
    }
    
    # Create and fit model for points
    formula <- as.formula(paste(response_variable, "~", paste(explanatory_vars, collapse = " + ")))
    tryCatch({
      exhaustive_selection <- regsubsets(formula, data = race_df_75, nbest = 1, method = "exhaustive")
      summary_exhaustive <- summary(exhaustive_selection)
      best_bic_vars <- names(coef(exhaustive_selection, which.min(summary_exhaustive$bic)))
      smooth_terms <- paste("s(", best_bic_vars[-1], ")", collapse=" + ")
      gam_formula <- as.formula(paste("Points ~", smooth_terms))
      
      model <- gam(gam_formula, data = race_df_75)
    }, error = function(e) {
      log_warn(paste("Error in model selection:", e$message))
      # Fallback to a simpler model
      fallback_formula <- as.formula(paste("Points ~ s(", pelo_col, ")"))
      model <<- gam(fallback_formula, data = race_df_75)
    })
    
    # Calculate adjustments for historical data step by step
    race_df_75 <- race_df_75 %>%
      arrange(Date) %>%
      group_by(!!sym(participant_col)) %>%
      mutate(
        row_id = row_number()
      ) %>%
      ungroup() %>%
      # Step 1: Initial predictions
      mutate(
        Initial_Prediction = predict(model, newdata = .)
      ) %>%
      group_by(!!sym(participant_col)) %>%
      mutate(
        Prediction_Diff = Points - Initial_Prediction
      ) %>%
      # Step 2: Calculate Period adjustments (disabled for relay teams since team compositions change)
      mutate(
        # period_p = purrr::map_dbl(row_id, function(r) {
        #   if(r <= 1) return(1)
        #   prior_period_curr <- Prediction_Diff[Period == Period[r] & row_id < r]
        #   prior_period_other <- Prediction_Diff[Period != Period[r] & row_id < r]
        #   if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
        #   tryCatch({
        #     t.test(prior_period_curr, prior_period_other)$p.value
        #   }, error = function(e) 1)
        # }),
        # period_correction = ifelse(period_p < 0.05,
        #                            mean(Prediction_Diff[Period == Period], na.rm = TRUE),
        #                            0),
        # Period adjustments disabled for relay teams - team compositions change between races
        period_p = if(is_relay) 1 else purrr::map_dbl(row_id, function(r) {
          if(r <= 1) return(1)
          prior_period_curr <- Prediction_Diff[Period == Period[r] & row_id < r]
          prior_period_other <- Prediction_Diff[Period != Period[r] & row_id < r]
          if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
          tryCatch({
            t.test(prior_period_curr, prior_period_other)$p.value
          }, error = function(e) 1)
        }),
        period_correction = if(is_relay) 0 else ifelse(period_p < 0.05,
                                   mean(Prediction_Diff[Period == Period], na.rm = TRUE),
                                   0),
        
        # Step 3: Calculate Elevation adjustments (disabled for relay teams since team compositions change)
        # elevation_p = purrr::map_dbl(row_id, function(r) {
        #   if(r <= 1) return(1)
        #   prior_elev_curr <- Prediction_Diff[Elevation_Flag == Elevation_Flag[r] & row_id < r]
        #   prior_elev_other <- Prediction_Diff[Elevation_Flag != Elevation_Flag[r] & row_id < r]
        #   if(length(prior_elev_curr) < 3 || length(prior_elev_other) < 3) return(1)
        #   tryCatch({
        #     t.test(prior_elev_curr, prior_elev_other)$p.value
        #   }, error = function(e) 1)
        # }),
        # elevation_correction = ifelse(elevation_p < 0.05,
        #                               mean(Prediction_Diff[Elevation_Flag == Elevation_Flag], na.rm = TRUE),
        #                               0),
        # Elevation adjustments disabled for relay teams - team compositions change between races
        elevation_p = if(is_relay) 1 else purrr::map_dbl(row_id, function(r) {
          if(r <= 1) return(1)
          prior_elev_curr <- Prediction_Diff[Elevation_Flag == Elevation_Flag[r] & row_id < r]
          prior_elev_other <- Prediction_Diff[Elevation_Flag != Elevation_Flag[r] & row_id < r]
          if(length(prior_elev_curr) < 3 || length(prior_elev_other) < 3) return(1)
          tryCatch({
            t.test(prior_elev_curr, prior_elev_other)$p.value
          }, error = function(e) 1)
        }),
        elevation_correction = if(is_relay) 0 else ifelse(elevation_p < 0.05,
                                      mean(Prediction_Diff[Elevation_Flag == Elevation_Flag], na.rm = TRUE),
                                      0),
        
        # Combine adjustments
        Adjusted_Prediction = Initial_Prediction + period_correction + elevation_correction
      ) %>%
      ungroup()
    
    # NEW CODE: Create position probability models using the same variables as points model
    position_models <- list()
    position_adjustments <- list()  # To store adjustments for each threshold
    
    # Feature selection - use the same explanatory variables as the points model
    position_feature_vars <- explanatory_vars
    
    # Create models for each position threshold
    for(threshold in position_thresholds) {
      log_info(paste("Creating model for top", threshold, "positions"))
      
      # Create binary outcome variable for position threshold
      race_df$position_achieved <- race_df$Place <= threshold
      
      # Create formula for regsubsets using the same explanatory variables as the points model
      pos_formula <- as.formula(paste("position_achieved ~", paste(position_feature_vars, collapse = " + ")))
      
      # Use regsubsets to select best features for this position threshold
      tryCatch({
        pos_selection <- regsubsets(pos_formula, data = race_df, nbest = 1, method = "exhaustive")
        pos_summary <- summary(pos_selection)
        pos_best_bic_vars <- names(coef(pos_selection, which.min(pos_summary$bic)))
        
        # Create smooth terms for GAM using best BIC variables (remove intercept)
        pos_smooth_terms <- paste("s(", pos_best_bic_vars[-1], ")", collapse=" + ")
        pos_gam_formula <- as.formula(paste("position_achieved ~", pos_smooth_terms))
        
        # Fit the position model with binomial family
        position_model <- gam(pos_gam_formula,
                              data = race_df,
                              family = binomial,
                              method = "REML")
        
        # Calculate Brier score for model evaluation
        predicted_probs <- predict(position_model, newdata = race_df, type = "response")
        brier_score <- mean((race_df$position_achieved - predicted_probs)^2, na.rm = TRUE)
        log_info(paste("Brier score for threshold", threshold, ":", round(brier_score, 4)))
        
        # Log selected variables
        log_info(paste("Variables selected for", threshold, "position model:", 
                       paste(pos_best_bic_vars[-1], collapse=", ")))
        
        # Store the model
        position_models[[paste0("threshold_", threshold)]] <- position_model
        
        # Calculate adjustments for period for this threshold
        position_df <- race_df %>%
          arrange(Date) %>%
          group_by(!!sym(participant_col)) %>%
          mutate(
            row_id = row_number()
          ) %>%
          ungroup()
        
        # Add predictions separately (outside of mutate)
        position_df$initial_prob <- predict(position_model, newdata = position_df, type = "response")
        
        # Continue with period adjustment calculations
        position_df <- position_df %>%
          group_by(!!sym(participant_col)) %>%
          mutate(
            prob_diff = as.numeric(position_achieved) - initial_prob,
            
            # Calculate period adjustments (disabled for relay teams since team compositions change)
            period_p = if(is_relay) 1 else purrr::map_dbl(row_id, function(r) {
              if(r <= 1) return(1)
              prior_period_curr <- prob_diff[Period == Period[r] & row_id < r]
              prior_period_other <- prob_diff[Period != Period[r] & row_id < r]
              if(length(prior_period_curr) < 3 || length(prior_period_other) < 3) return(1)
              tryCatch({
                t.test(prior_period_curr, prior_period_other)$p.value
              }, error = function(e) 1)
            }),
            period_correction = if(is_relay) 0 else ifelse(period_p < 0.05,
                                       mean(prob_diff[Period == Period], na.rm = TRUE),
                                       0),
            period_adjusted = pmin(pmax(initial_prob + period_correction, 0), 1)
          ) %>%
          ungroup()
        
        # Get final adjustments for each participant
        skier_pos_adjustments <- position_df %>%
          group_by(!!sym(participant_col)) %>%
          summarise(
            period_effect = last(period_correction)
          )
        
        # Store adjustments for this threshold
        position_adjustments[[paste0("threshold_", threshold)]] <- skier_pos_adjustments
        
      }, error = function(e) {
        log_warn(paste("Error in position model for threshold", threshold, ":", e$message))
        
        # Create a simpler fallback model with just the pelo column
        fallback_vars <- c("Prev_Points_Weighted", pelo_col)
        fallback_vars <- fallback_vars[fallback_vars %in% names(race_df)]
        
        if(length(fallback_vars) > 0) {
          fallback_terms <- paste("s(", fallback_vars, ")", collapse=" + ")
          fallback_formula <- as.formula(paste("position_achieved ~", fallback_terms))
          
          position_models[[paste0("threshold_", threshold)]] <- gam(
            fallback_formula,
            data = race_df,
            family = binomial,
            method = "REML"
          )
          
          # Create empty adjustments object since we can't calculate them for the fallback model
          empty_adjustments <- data.frame(x = unique(race_df[[participant_col]]))
          names(empty_adjustments)[1] <- participant_col
          empty_adjustments$period_effect <- 0
          position_adjustments[[paste0("threshold_", threshold)]] <- empty_adjustments
          
          log_info(paste("Created fallback model for threshold", threshold, 
                         "using variables:", paste(fallback_vars, collapse=", ")))
        } else {
          # Last resort fallback - just use the pelo column
          fallback_formula <- as.formula(paste("position_achieved ~ s(", pelo_col, ")"))
          position_models[[paste0("threshold_", threshold)]] <- gam(
            fallback_formula,
            data = race_df,
            family = binomial,
            method = "REML"
          )
          
          # Create empty adjustments object
          empty_adjustments <- data.frame(x = unique(race_df[[participant_col]]))
          names(empty_adjustments)[1] <- participant_col
          empty_adjustments$period_effect <- 0
          position_adjustments[[paste0("threshold_", threshold)]] <- empty_adjustments
          
          log_info(paste("Created last-resort fallback model for threshold", threshold, 
                         "using only", pelo_col))
        }
      })
    }
    
    # Calculate volatility metrics using recent races
    race_df_75 <- race_df_75 %>%
      group_by(!!sym(participant_col)) %>%
      arrange(Date) %>%  # Ensure chronological order
      mutate(
        # Create rolling window calculations for last 10 races
        recent_prediction_volatility = slider::slide_dbl(
          Points - Initial_Prediction,
          sd,
          .before = 9,  # Look at current race plus 9 previous
          .complete = FALSE  # Allow partial windows
        ),
        
        recent_consistency_score = slider::slide_dbl(
          abs(Points - Initial_Prediction),
          mean,
          .before = 9,
          .complete = FALSE
        ),
        
        recent_upside_potential = slider::slide_dbl(
          Points - Initial_Prediction,
          ~quantile(.x, 0.9, na.rm = TRUE),
          .before = 9,
          .complete = FALSE
        ),
        
        recent_downside_risk = slider::slide_dbl(
          Points - Initial_Prediction,
          ~quantile(.x, 0.1, na.rm = TRUE),
          .before = 9,
          .complete = FALSE
        )
      ) %>%
      mutate(
        recent_volatility_ratio = recent_upside_potential / abs(recent_downside_risk)
      ) %>%
      ungroup()
    
    # Get final adjustments for each participant
    skier_adjustments <- race_df_75 %>%
      group_by(!!sym(participant_col)) %>%
      summarise(
        period_effect = last(period_correction),
        elevation_effect = last(elevation_correction),
        
        # Recent volatility metrics
        prediction_volatility = last(recent_prediction_volatility),
        consistency_score = last(recent_consistency_score),
        upside_potential = last(recent_upside_potential),
        downside_risk = last(recent_downside_risk),
        volatility_ratio = last(recent_volatility_ratio),
        
        # Add number of recent races for confidence
        n_recent_races = sum(!is.na(tail(Points, 10)))
      )
    
    # Prepare startlist data
    startlist_prepared <- prepare_startlist_data(startlist, race_df, pelo_col, is_relay = is_relay)
    
    # Ensure race probability column exists
    if(!(race_prob_col %in% names(startlist_prepared))) {
      log_warn(paste("Race probability column missing:", race_prob_col))
      if(race_prob_col %in% names(startlist)) {
        # Copy from original startlist if available
        log_info("Copying from original startlist")
        startlist_prepared[[race_prob_col]] <- startlist[match(startlist_prepared[[participant_col]], startlist[[participant_col]]), race_prob_col]
      } else {
        # Default to 1 for first race, 0 for others if not available
        log_info("Setting default probabilities")
        startlist_prepared[[race_prob_col]] <- if(i == 1) 1 else 0
      }
    }
    
    # NEW CODE: Make position probability predictions with adjustments
    position_preds <- data.frame(startlist_prepared[[participant_col]])
    names(position_preds)[1] <- participant_col
    
    # Add ID, Nation, Sex for individual races
    if(!is_relay) {
      position_preds$ID <- startlist_prepared$ID
      position_preds$Nation <- startlist_prepared$Nation
      position_preds$Sex <- startlist_prepared$Sex
    }
    
    # Add race number
    position_preds$Race <- i
    
    # Add race probability column for later normalization
    if(race_prob_col %in% names(startlist_prepared)) {
      position_preds[[race_prob_col]] <- startlist_prepared[[race_prob_col]]
    }
    
    # Make predictions for each threshold
    for(threshold in position_thresholds) {
      model_name <- paste0("threshold_", threshold)
      adj_name <- paste0("threshold_", threshold)
      prob_col <- paste0("prob_top", threshold)
      
      if(model_name %in% names(position_models)) {
        tryCatch({
          # Get the model
          pos_model <- position_models[[model_name]]
          
          # Check what variables the model actually needs
          model_vars <- names(pos_model$var.summary)
          log_info(paste("Model for threshold", threshold, "requires variables:", paste(model_vars, collapse=", ")))
          
          # Create a clean subset of prediction data with only required variables
          prediction_subset <- startlist_prepared
          
          # Explicitly check for each variable
          for(var in model_vars) {
            if(!(var %in% names(prediction_subset))) {
              log_warn(paste("Missing required variable:", var, "- adding with default values"))
              # Add missing variable with appropriate default value
              prediction_subset[[var]] <- 0
            } else {
              # Ensure the variable has the right type
              model_var_type <- class(pos_model$var.summary[[var]])
              data_var_type <- class(prediction_subset[[var]])
              
              if(!identical(model_var_type, data_var_type)) {
                log_warn(paste("Variable type mismatch for", var, ":", 
                               "model expects", model_var_type, "but got", data_var_type))
                # Convert to correct type
                if(model_var_type == "numeric") {
                  prediction_subset[[var]] <- as.numeric(prediction_subset[[var]])
                } else if(model_var_type == "factor") {
                  prediction_subset[[var]] <- as.factor(prediction_subset[[var]])
                }
              }
              
              # Handle NAs
              if(any(is.na(prediction_subset[[var]]))) {
                log_info(paste("Replacing NAs in", var))
                if(is.numeric(prediction_subset[[var]])) {
                  prediction_subset[[var]] <- replace_na_with_quartile(prediction_subset[[var]])
                } else {
                  # For non-numeric, use most common value
                  most_common <- names(sort(table(prediction_subset[[var]], useNA = "no"), decreasing = TRUE))[1]
                  prediction_subset[[var]][is.na(prediction_subset[[var]])] <- most_common
                }
              }
            }
          }
          
          # Make predictions with explicit try-catch
          base_predictions <- tryCatch({
            # Debug output
            log_info(paste("Attempting prediction for threshold", threshold, "with", nrow(prediction_subset), "rows"))
            
            # Explicit call to mgcv::predict.gam to avoid method dispatch issues
            mgcv::predict.gam(pos_model, newdata = prediction_subset, type = "response")
          }, error = function(e) {
            log_warn(paste("Prediction call failed:", e$message))
            
            # Try alternative prediction approach with one row at a time
            log_info("Trying row-by-row prediction as fallback")
            result <- numeric(nrow(prediction_subset))
            
            for(j in 1:nrow(prediction_subset)) {
              single_row <- prediction_subset[j,, drop = FALSE]
              result[j] <- tryCatch({
                mgcv::predict.gam(pos_model, newdata = single_row, type = "response")
              }, error = function(e2) {
                log_warn(paste("Failed on row", j, ":", e2$message))
                threshold/100  # Default value based on threshold
              })
            }
            return(result)
          })
          
          # Store predictions
          position_preds[[paste0(prob_col, "_base")]] <- base_predictions
          
          # Apply adjustments if available - COMMENTED OUT: Remove double-dipping
          # if(adj_name %in% names(position_adjustments)) {
          #   # Get adjustments
          #   pos_adj <- position_adjustments[[adj_name]]
          #   
          #   # Join with predictions
          #   position_preds <- position_preds %>%
          #     left_join(pos_adj, by = participant_col) %>%
          #     mutate(
          #       # Replace NAs with zeros
          #       period_effect = replace_na(period_effect, 0),
          #       
          #       # Apply adjustments
          #       period_adjustment = period_effect,
          #       
          #       # Calculate adjusted probabilities
          #       adjusted_prob = get(paste0(prob_col, "_base")) + period_adjustment,
          #       
          #       # Ensure probabilities are between 0 and 1
          #       adjusted_prob = pmin(pmax(adjusted_prob, 0), 1)
          #     )
          #   
          #   # Use adjusted probability as final
          #   position_preds[[prob_col]] <- position_preds$adjusted_prob
          #   
          #   # Clean up temporary columns
          #   position_preds <- position_preds %>%
          #     dplyr::select(-period_effect, -period_adjustment, -adjusted_prob)
          # } else {
            # Use base prediction without adjustments
            position_preds[[prob_col]] <- position_preds[[paste0(prob_col, "_base")]]
          # }
          
          # Clean up base prediction column
          position_preds <- position_preds %>%
            dplyr::select(-paste0(prob_col, "_base"))
          
          # Convert to percentage and round
          position_preds[[prob_col]] <- round(position_preds[[prob_col]] * 100, 1)
          
          log_info(paste("Made predictions with adjustments for position threshold", threshold))
          
        }, error = function(e) {
          log_warn(paste("Complete failure for threshold", threshold, ":", e$message))
          # Set a reasonable default based on threshold (1% for top1, 3% for top3, etc.)
          position_preds[[prob_col]] <- rep(threshold, nrow(position_preds))
        })
      } else {
        log_warn(paste("No model found for threshold", threshold))
        position_preds[[prob_col]] <- NA
      }
    }
    
    # Normalize position probabilities to ensure they sum to the correct totals
    position_preds <- normalize_position_probabilities(position_preds, race_prob_col, position_thresholds)
    
    # Add verification logging for each threshold
    log_info(sprintf("Race %d position probability sums after normalization:", i))
    for(threshold in position_thresholds) {
      prob_col <- paste0("prob_top", threshold)
      if(prob_col %in% names(position_preds)) {
        sum_val <- sum(position_preds[[prob_col]], na.rm = TRUE)
        log_info(sprintf("  %s: %.2f%% (should be %d%%)", 
                         prob_col, sum_val, 100 * threshold))
      }
    }
    
    # Store position predictions for this race
    position_predictions[[i]] <- position_preds
    
    # Check if probabilities are getting lost
    log_info(paste("Race", i, "probability check:"))
    prob_summary <- startlist_prepared %>%
      group_by(if(!is_relay) Nation else NULL) %>%
      summarise(
        mean_prob = mean(get(race_prob_col), na.rm = TRUE),
        sum_prob = sum(get(race_prob_col), na.rm = TRUE),
        n = n()
      ) %>%
      arrange(desc(sum_prob))
    
    # Prepare startlist points predictions (original functionality)
    race_dfs[[i]] <- startlist_prepared %>%
      mutate(
        Base_Prediction = predict(model, newdata = .),
      ) %>%
      left_join(skier_adjustments, by = participant_col) %>%
      mutate(
        # Regular adjustments
        period_effect = replace_na(period_effect, 0),
        elevation_effect = replace_na(elevation_effect, 0),
        
        # Volatility metrics
        prediction_volatility = replace_na(prediction_volatility, 0),
        consistency_score = replace_na(consistency_score, 0),
        upside_potential = replace_na(upside_potential, 0),
        downside_risk = replace_na(downside_risk, 0),
        volatility_ratio = replace_na(volatility_ratio, 1),
        n_recent_races = replace_na(n_recent_races, 0),
        
        # Using existing adjustment approach (disabled for relay teams since team compositions change)
        period_adjustment = if(is_relay) 0 else period_effect,
        elevation_adjustment = if(is_relay) 0 else elevation_effect,
        
        # Base prediction and adjustments (period and elevation adjustments disabled for relay teams)
        Predicted_Points = Base_Prediction + period_adjustment + elevation_adjustment,
        Predicted_Points = pmax(pmin(Predicted_Points, 100), 0),
        
        # Apply race probability to predictions
        Race_Prob = get(race_prob_col),
        Final_Prediction = Predicted_Points * Race_Prob,
        
        # Different scoring scenarios - adjusted by race probability
        confidence_factor = pmin(n_recent_races / 10, 1),
        scaled_upside_potential = upside_potential * (Predicted_Points/100),
        scaled_downside_potential = downside_risk * (Predicted_Points/100),
        
        # Safe prediction (downside)
        Safe_Prediction = pmax(
          (Predicted_Points - (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
          0
        ),
        
        # Upside prediction
        Upside_Prediction = pmin(
          (Predicted_Points + (prediction_volatility * 1.5 * confidence_factor)) * Race_Prob, 
          100 * Race_Prob  # Cap at 100 * probability
        )
      ) %>%
      dplyr::select(all_of(participant_col), 
                    if(!is_relay) "Nation" else NULL,
                    Base_Prediction, period_adjustment, elevation_adjustment,
                    prediction_volatility, volatility_ratio, confidence_factor,
                    Final_Prediction, Safe_Prediction, Upside_Prediction,
                    race_prob_col)
    
    # Extra check to ensure race probability column exists and is properly named
    if(!race_prob_col %in% names(race_dfs[[i]])) {
      log_warn(paste("Race probability column", race_prob_col, "not in final race_dfs!"))
      # Try to fix
      if("Race_Prob" %in% names(race_dfs[[i]])) {
        log_info("Renaming Race_Prob column to correct race probability column name")
        race_dfs[[i]][[race_prob_col]] <- race_dfs[[i]][["Race_Prob"]]
      }
    }
  }
  
  # Get number of races from races dataframe
  n_races <- nrow(races)
  
  # Combine all race predictions (points)
  final_predictions <- combine_predictions(race_dfs, startlist, is_relay = is_relay)
  
  log_info(paste("Final predictions calculated for", if(is_relay) "relay" else gender))
  
  # Create post predictions for blog (points)
  post_predictions <- create_post_predictions(final_predictions, n_races, gender, is_relay = is_relay)
  
  # Combine all position predictions into one dataframe
  all_position_predictions <- bind_rows(position_predictions)
  
  # Format position results
  formatted_position_results <- format_position_results(all_position_predictions, race_date, gender, is_relay, relay_type)
  
  # Create folder path based on race date
  race_folder <- format(race_date, "%Y%m%d")
  dir_path <- paste0("~/blog/daehl-e/content/post/biathlon/drafts/race-picks/", race_folder)
  
  # Create directory if it doesn't exist
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Save points predictions to Excel
  if(is_relay) {
    relay_type_short <- gsub(" ", "_", tolower(relay_type))
    if(is.null(gender) || gender == "") {
      file_name <- paste0(relay_type_short, ".xlsx")
    } else {
      file_name <- paste0(gender, "_", relay_type_short, ".xlsx")
    }
  } else {
    file_name <- paste0(gender, ".xlsx")
  }
  
  file_path <- file.path(dir_path, file_name)
  write.xlsx(post_predictions, file = file_path)
  
  log_info(paste("Saved predictions to", file_path))
  
  return(list(
    full_predictions = final_predictions,
    post_predictions = post_predictions,
    position_predictions = all_position_predictions,
    formatted_position_results = formatted_position_results
  ))
}

# Main workflow function
run_integrated_predictions_workflow <- function() {
  log_info("Running integrated predictions workflow (points and position probabilities)")
  
  # First calculate race probabilities
  log_info("Calculating race probabilities")
  prob_results <- calculate_race_probabilities()
  
  log_info("Updating startlist variables with calculated probabilities")
  # Individual events startlists
  men_startlist <- prob_results$men
  ladies_startlist <- prob_results$ladies
  
  # Relay startlists
  men_relay_startlist <- prob_results$men_relay
  ladies_relay_startlist <- prob_results$ladies_relay
  mixed_relay_startlist <- prob_results$mixed_relay
  single_mixed_relay_startlist <- prob_results$single_mixed_relay
  
  # Log the sizes of the startlists
  log_info(paste("Men's startlist has", nrow(men_startlist), "entries"))
  log_info(paste("Ladies' startlist has", nrow(ladies_startlist), "entries"))
  log_info(paste("Men's relay startlist has", nrow(men_relay_startlist), "entries"))
  log_info(paste("Ladies' relay startlist has", nrow(ladies_relay_startlist), "entries"))
  log_info(paste("Mixed relay startlist has", nrow(mixed_relay_startlist), "entries"))
  log_info(paste("Single mixed relay startlist has", nrow(single_mixed_relay_startlist), "entries"))
  
  # Create relay chronos if we have relay races
  if(nrow(relays) + nrow(mixed_relays) + nrow(single_mixed_relays) > 0) {
    log_info("Creating relay chronos")
    create_relay_chronos()
  }
  
  # Results containers
  all_results <- list()
  
  men_races <- men_races %>%
    dplyr::filter(!racetype %in% c("Relay", "Mixed Relay", "Single Mixed Relay"))
  log_info(paste("After filtering out relays:", nrow(men_races), "men's individual races remain"))
  
  ladies_races <- ladies_races %>%
    dplyr::filter(!racetype %in% c("Relay", "Mixed Relay", "Single Mixed Relay"))
  log_info(paste("After filtering out relays:", nrow(ladies_races), "ladies' individual races remain"))
  
  # Run for men if races exist
  if(nrow(men_races) > 0 && nrow(men_startlist) > 0) {
    log_info("Processing men's predictions")
    # Make a copy of the startlist to avoid global state issues
    men_startlist_copy <- men_startlist
    all_results$men <- predict_races("men", is_relay = FALSE, startlist_override = men_startlist_copy)
  } else {
    log_info("No men's races scheduled or no startlist available")
  }
  
  # Run for ladies if races exist
  if(nrow(ladies_races) > 0 && nrow(ladies_startlist) > 0) {
    log_info("Processing ladies' predictions")
    # Make a copy of the startlist to avoid global state issues
    ladies_startlist_copy <- ladies_startlist
    all_results$ladies <- predict_races("ladies", is_relay = FALSE, startlist_override = ladies_startlist_copy)
  } else {
    log_info("No ladies' races scheduled or no startlist available")
  }
  
  # Run for relay events (with tracking to avoid duplicate processing)
  processed_relays <- list()
  
  # Men's relay
  if(nrow(relays) > 0 && nrow(men_relay_startlist) > 0 && !("men_relay" %in% names(processed_relays))) {
    log_info("Processing men's relay predictions")
    # Make a copy of the startlist to avoid global state issues
    men_relay_startlist_copy <- men_relay_startlist
    all_results$men_relay <- predict_races("men", is_relay = TRUE, relay_type = "Relay", 
                                           startlist_override = men_relay_startlist_copy)
    processed_relays$men_relay <- TRUE
  } else if(nrow(relays) > 0) {
    log_info("No men's relay startlist available or already processed")
  }
  
  # Ladies' relay
  if(nrow(relays) > 0 && nrow(ladies_relay_startlist) > 0 && !("ladies_relay" %in% names(processed_relays))) {
    log_info("Processing ladies' relay predictions")
    # Make a copy of the startlist to avoid global state issues
    ladies_relay_startlist_copy <- ladies_relay_startlist
    all_results$ladies_relay <- predict_races("ladies", is_relay = TRUE, relay_type = "Relay", 
                                              startlist_override = ladies_relay_startlist_copy)
    processed_relays$ladies_relay <- TRUE
  } else if(nrow(relays) > 0) {
    log_info("No ladies' relay startlist available or already processed")
  }
  
  # Mixed relay
  if(nrow(mixed_relays) > 0 && nrow(mixed_relay_startlist) > 0) {
    log_info("Processing mixed relay predictions")
    # Make a copy of the startlist to avoid global state issues
    mixed_relay_startlist_copy <- mixed_relay_startlist
    all_results$mixed_relay <- predict_races(NULL, is_relay = TRUE, relay_type = "Mixed Relay", 
                                             startlist_override = mixed_relay_startlist_copy)
  } else if(nrow(mixed_relays) > 0) {
    log_info("No mixed relay startlist available")
  }
  
  # Single mixed relay
  if(nrow(single_mixed_relays) > 0 && nrow(single_mixed_relay_startlist) > 0) {
    log_info("Processing single mixed relay predictions")
    # Make a copy of the startlist to avoid global state issues
    single_mixed_relay_startlist_copy <- single_mixed_relay_startlist
    all_results$single_mixed_relay <- predict_races(NULL, is_relay = TRUE, relay_type = "Single Mixed Relay", 
                                                    startlist_override = single_mixed_relay_startlist_copy)
  } else if(nrow(single_mixed_relays) > 0) {
    log_info("No single mixed relay startlist available")
  }
  
  # Create top contenders summary
  #top_contenders <- create_top_contenders_summary(all_results, race_date)
  #all_results$top_contenders <- top_contenders
  
  log_info("Prediction workflow with position probabilities complete")
  return(all_results)
}

# Run the integrated predictions workflow
run_integrated_predictions_workflow()
        