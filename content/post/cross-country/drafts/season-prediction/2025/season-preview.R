# Load necessary libraries
library(dplyr)
library(purrr)
# library(readr)  # For reading CSV files (included in tidyverse)
library(tidyr)
# library(arrow)  # No longer needed for CSV files
library(AER)
library(ggplot2)

# Step One: Read in the necessary info
M_chrono <- read_csv('/Users/syverjohansen/ski/elo/python/ski/polars/excel365/men_chrono.csv')

# Step Two: Create a column called WC Points that maps place to world cup points from a list
wc_points <- c(100,95,90,85,80,75,72,69,66,63,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1)
stage_points <- c(50, 47, 44, 41, 38, 35, 32, 30, 28, 26, 24, 22, 20, 18, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
tds_points <- c(300, 285, 270, 255, 240, 216, 207, 198, 189, 180, 174, 168, 162, 156, 150, 144, 138, 132, 126, 120, 114, 108, 102, 96, 90, 84, 78, 72, 66, 60, 57, 54, 51, 48, 45, 42, 39, 36, 33, 30, 27, 24, 21, 18, 15, 12, 9, 6, 3)

# Function to safely fetch points based on Place
get_points <- function(place, points_list) {
  if (place >= 1 && place <= length(points_list)) {
    return(points_list[place])
  }
  return(0)
}

# Apply points logic based on Event and Distance
df <- M_chrono %>%
  mutate(Points = case_when(
    Event == "Tour de Ski" & Distance == "Stage" ~ map_int(Place, ~ get_points(.x, tds_points)),
    Event == "Tour de Ski" ~ map_int(Place, ~ get_points(.x, stage_points)),
    TRUE ~ map_int(Place, ~ get_points(.x, wc_points))
  ))

# Sort the df by Date, Race, and Place
df <- df %>%
  arrange(Date, Race, Place)

# Step Three: Filter for the last five years
df <- df %>%
  filter(Season > 2018, Event %in% c("Offseason", "World Cup", "Nordic Opening", "Tour de Ski")) %>%
  group_by(ID, Season) %>%
  mutate(Cumulative_Points = cumsum(Points)) %>%
  ungroup()


# Step One: Filter for first place
first_place <- df %>%
  filter(Place == 1)

# Step Two: Group by Season and calculate total points for first place
max_points_per_season <- first_place %>%
  group_by(Season) %>%
  summarise(Max_Points = sum(Points), .groups = 'drop')

# Step Three: Join the max points back to the original data frame
df <- df %>%
  left_join(max_points_per_season, by = "Season")

# Step Four: Calculate percentage of maximum points
df <- df %>%
  mutate(Pct_of_Max_Points = Cumulative_Points / Max_Points)


# Step Four: Set up the explanatory variables
# Filter for the offseason
df <- df %>%
  filter(Event == "Offseason") %>%
  arrange(ID, Season) %>%
  group_by(ID) %>%
  mutate(Prev_Pelo = lag(Pelo)) %>%
  ungroup() %>%
  filter(!is.na(Prev_Pelo))


# Fit the Tobit model
tobit_model <- tobit(Pct_of_Max_Points ~ Prev_Pelo, data = df, left = .1)

# Summarize the model
summary(tobit_model)


# Create a data frame with predicted values
df$Predicted <- predict(tobit_model)

# Plot the results
ggplot(df, aes(x = Prev_Pelo, y = Pct_of_Max_Points)) +
  geom_point(alpha = 0.5) +  # Scatter plot of observed data
  geom_line(aes(y = Predicted), color = "blue", linewidth = 1) +  # Line for predicted values
  labs(title = "Tobit Model: Predicted vs. Observed",
       x = "Previous ELO Score (Prev_Pelo)",
       y = "Percentage of Max Points (Pct_of_Max_Points)") +
  theme_minimal()








