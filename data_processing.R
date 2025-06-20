# Load required libraries
library(dplyr)
library(ggplot2)
library(nycflights13)
library(readr)

# Step 1: Inspect the flights dataset
glimpse(flights)

# Step 2: Filter for relevant columns and handle missing values
flights_clean <- flights %>%
  select(origin, month, day, dep_delay, arr_delay) %>%  # Select relevant columns
  filter(origin %in% c("JFK", "LGA", "EWR")) %>%  # Ensure only the 3 airports
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%  # Remove canceled flights (missing delays)
  mutate(
    avg_delay = (dep_delay + arr_delay) / 2  # Average of departure and arrival delays
  )

# Step 3: Assign seasons based on month
flights_clean <- flights_clean %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    ),
    season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))  # Set factor order
  )


# Step 4: Check for remaining missing values
colSums(is.na(flights_clean))

# Step 5: Verify the data
summary(flights_clean$avg_delay)
table(flights_clean$origin, flights_clean$season)

# Step 6: Save the cleaned data as an .rds file
write_rds(flights_clean, "flights_clean.rds")
# It’s compact, preserves R-specific data types (e.g., tibbles), and is fast to read/write. It’s ideal if you’re only working in R.

