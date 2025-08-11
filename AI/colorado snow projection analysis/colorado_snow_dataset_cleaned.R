## ---------------------------------
## Step 0: Load Libraries and Data
## ---------------------------------

# Load the following libraries
library(dplyr)
library(tidyverse)
library(lubridate)

# Read in the dataset
data <- readr::read_csv("4094467.csv")

## ---------------------------------
## Step 1: Initial Data Cleaning
## ---------------------------------

# We need to define our first snow season
# Clean up the raw data
weather_daily <- data %>%
  # Select only the columns we need and give them clean names
  select(DATE, PRCP, SNOW, TMAX, TMIN) %>%
  rename(
    date = DATE,
    prcp = PRCP,
    snow = SNOW,
    tmax = TMAX,
    tmin = TMIN
  ) %>%
  # Convert the date column from text to a proper R date object
  mutate(date = ymd(date)) %>%
  # Make sure our numeric columns are numeric
  mutate(across(prcp:tmin, as.numeric))

# Verify our cleaned up data
glimpse(weather_daily)

## ------------------------------------------------------------------
## Step 2: Create the Target Variable (Day of First Measurable Snow)
## ------------------------------------------------------------------

min_snow_threshold <- 0.1 # Defining "measurable snow" as 0.1 inches

# This is our wrangling step
first_snow_dates <- weather_daily %>%
  # 1. Define a "snow season" that runs from Aug 1 to Jul 31
  #    This prevents a late spring snow from being counted as the "first"
  mutate(season = ifelse(month(date) >= 8, year(date), year(date) - 1)) %>%
  
  # 2. Filter for only days with measurable snow
  filter(snow >= min_snow_threshold) %>%
  
  # 3. For each season, find the EARLIEST date with snow
  group_by(season) %>%
  summarise(first_snow_date = min(date)) %>%
  
  # 4. Calculate our target variable: the day of the year
  mutate(day_of_first_snow = yday(first_snow_date))

# View the results: one row per season with the first snow date
print(first_snow_dates)

## ------------------------------------------------------------------
## Step 3: Create the Predictor Variables (Features)
## ------------------------------------------------------------------

# We go back to our clean daily data to create features for each season
feature_data <- weather_daily %>%
  mutate(season = ifelse(month(date) >= 8, year(date), year(date) - 1)) %>%
  group_by(season) %>%
  
  # Create summary metrics for the months LEADING UP to the first snow
  summarise(
    # Use na.rm = TRUE to handle any missing data points
    avg_aug_tmax = mean(tmax[month(date) == 8], na.rm = TRUE),
    avg_sept_tmax = mean(tmax[month(date) == 9], na.rm = TRUE),
    avg_sept_tmin = mean(tmin[month(date) == 9], na.rm = TRUE),
    total_sept_prcp = sum(prcp[month(date) == 9], na.rm = TRUE)
  )

# View the features
print(feature_data)

## -------------------------------------------------------------------
## Step 4: Combine into a Final Model-Ready Data Frame
## -------------------------------------------------------------------

# Join our features and our target variable by the "season"
model_ready_data <- inner_join(feature_data, first_snow_dates, by = "season") %>%
  # Add the previous year's first snow day as a feature
  mutate(prev_year_first_snow = lag(day_of_first_snow)) %>%
  
  # Remove any rows with missing data (e.g., the very first year with no 'lag')
  na.omit() %>%
  
  # We don't need the actual date column for modeling anymore
  select(-first_snow_date)

# This is the final data frame we will use to train our AI model
# Each row is a year, with features and the target variable.
glimpse(model_ready_data)

# Save our model_ready_data to .rds file type
saveRDS(model_ready_data, file = "model_ready_data.rds")