# Load the following libraries
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(readr)
library(sf)
library(ggplot2)
library(rvest)
library(data.table)
library(stringr)
library(forcats)
library(maps)
library(tidyverse)
library(patchwork)

# Load the API key to access the NOAA weather patterns
noaa_api_key <- "erSecNRvClQKLfbNSkYeFyUKCToOkVWu"

# Define the base API URL and Headers
base_url <- "https://www.ncei.noaa.gov/cdo-web/api/v2/"
headers <- add_headers(token = noaa_api_key)

# Find Weather Stations using 'extent' (Bounding Box)
stations_url <- paste0(base_url, "stations")

# Define a bounding box for Denver area:
# south_lat, west_lon, north_lat, east_lon
# This covers a reasonably large area around Denver to catch multiple stations.
denver_bbox <- "39.0,-106.0,40.5,-104.0"

query_params_stations <- list(
  datasetid = "GHCND",
  extent = "denver_bbox", # Use 'extent' instead of 'locationid'
  limit = 1000          # Max limit per request
)

response_stations <- GET(stations_url, query = query_params_stations, headers)

# Endpoint for datasets
datasets_url <- paste0(base_url, "datasets")

# Make the GET reques
response_datasets <- GET(datasets_url, headers)

# Check the status code (200 means success)
status_code(response_datasets)

# Parse the JSON content
datasets_content <- fromJSON(content(response_datasets, "text", encoding = "UTF-8"))

# The actual data is usually in a 'results' element
datasets_df <- as_tibble(datasets_content$results)
print(datasets_df)

# This analysis will be investigating weather patterns in Denver, CO
# Establish lat & long coordinates for Denver, CO
lat <- 39.7392
lon <- -104.9903

# You need to define a bounding box or use location categories.
# For simplicity, let's look for stations within a state (Colorado FIPS code: 08)
# First, list location categories to find the FIPS code for states
locationcategories_url <- paste0(base_url, "locationcategories")
response_loccats <- GET(locationcategories_url, headers)
loccats_content <- fromJSON(content(response_loccats, "text", encoding = "UTF-8"))
loccats_df <- as_tibble(loccats_content$results)
print(loccats_df) # Look for 'ST' (States) and then use 'locations' endpoint

# Directly search for stations in Colorado (FIPS:08) for the GHCND dataset.
# The 'locationid' for a state is 'FIPS:XX' where XX is the FIPS code
# For Colorado, FIPS code is 08
co_fips <- "FIPS:08"

# Endpoint for stations
stations_url <- paste0(base_url, "stations")

# Define query parameters for stations
query_params_stations <- list(
  datasetid = "GHCND",
  locationid = co_fips,
  # Dates here limit stations to those active during this period
  startdate = "2020-01-01",
  enddate = format(Sys.Date(), "%Y-%m-%d"), # NOAA API v2 data generally ends around Sept 2022.
  limit = 1000 # Max limit per request
)

# Make the GET request
response_stations <- GET(stations_url, query = query_params_stations, headers)

# Check status and parse
status_code(response_stations)
stations_content <- fromJSON(content(response_stations, "text", encoding = "UTF-8"))
stations_df <- as_tibble(stations_content$results)

# Filter for stations around Denver (roughly) and check date coverage
# Let's filter by approximate lat/lon bounds for Denver
denver_stations_filtered <- stations_df %>%
  filter(
    latitude >= 39.5 & latitude <= 40.0,
    longitude >= -105.5 & longitude <= -104.5,
    grepl("DENVER", name, ignore.case = TRUE), # Filter by name if possible
    ymd(maxdate) >= ymd("2020-01-01")
  ) %>%
  arrange(desc(maxdate)) # Sort by most recent data

print("--- Filtered Denver Stations (most recent data available via API v2) ---")
print(head(denver_stations_filtered))
print(paste("Number of filtered stations:", nrow(denver_stations_filtered)))

selected_station_id <- NA
if (nrow(denver_stations_filtered) > 0) {
  selected_station_id <- denver_stations_filtered$id[1]
  message(paste("Selected station ID for historical data (up to ~Sept 2022):", selected_station_id))
} else {
  message("No suitable stations found around Denver with data extending to ~Sept 2022 via NOAA CDO API v2.")
  message("Consider adjusting the 'maxdate' filter or using a different data source for current weather patterns.")
  # Fallback to a known station if you absolutely want to try to pull some older data
  selected_station_id <- "GHCND:USW00094038" # DENVER STAPLETON INTL AP (active until ~2022)
  message(paste("Defaulting to known station ID:", selected_station_id))
}

# Now, proceed with fetching data types and actual dat ausing selected_stations_id
# Remember, this data will likely not be newer than Sept 2022.
if (!is.na(selected_station_id)) {
  # Endpoint for data types
  datatypes_url <- paste0(base_url, "datatypes")
  query_params_datatypes <- list (
    datasetid = "GHCND",
    stationid = selected_station_id,
    limit = 1000
  )
  # Make sure your stat_date and end_date are within the station's maxdate
  state_date_str <- "2022-01-01"
  end_date_str <- "2022-03-31" # Can't go past ~Sept 2022
} else {
  message("Cannot proceed to fetch weather data as no station was selected.")
}

# Function to download and read a single year's details file
download_and_read_storm_events <- function(year) {
  # Find the exact filename by scraping the directory
  message(paste("Finding file for year", year, "using robust text search."))
  
  tryCatch({
    # Use GET to download the raw HTML text of the directory page
    response <- GET(storm_events_base_url)
    stop_for_status(response) # This will throw an error if the page doesn't load
    page_text <- content(response, "text")
    
    # Use regex to find all filenames directly from the page text
    # This pattern is very specific to the storm data file format
    all_filenames <- str_extract_all(page_text, "StormEvents_details-ftp_v1\\.0_d[0-9]{4}_c[0-9]{8}\\.csv\\.gz")[[1]]
    
    
    if (length(all_filenames) == 0) {
      warning("Could not find any data file links on the directory page using text search.")
      return(NULL)
    }
    
    # Use a pattern to find the details file for the specified year
    # We now search within the clean 'all_filenames' list
    pattern <- paste0("StormEvents_details-ftp_v1.0_d", year)
    found_files <- grep(pattern, all_filenames, value = TRUE)
    
    # Check if any files were found
    if (length(found_files) == 0) {
      warning(paste("No file found for year", year, "on the server matching the pattern."))
      return(NULL)
    }
    
    # Get the first match from the found files
    file_name <- found_files[1]
    
    # --- Step 2: Construct the URL and download the file ---
    full_url <- paste0(storm_events_base_url, file_name)
    temp_file <- tempfile(fileext = ".csv.gz")
    
    message(paste("File found:", file_name))
    message(paste("Attempting download from:", full_url))
    
    # Use your original tryCatch block for the download and read process
    tryCatch({
      download.file(full_url, temp_file, mode = "wb", quiet = TRUE)
      data <- read_csv(temp_file, col_types = cols(.default = "c"), show_col_types = FALSE)
      message(paste("Successfully downloaded and read data for", year))
      return(data) # Return data if successful
      
    }, error = function(e) {
      error_msg <- conditionMessage(e)
      warning(paste("An error occurred while reading the file for year", year, ":", error_msg))
      return(NULL) # Return NULL on error
      
    }, finally = {
      if (file.exists(temp_file)) {
        file.remove(temp_file) # Clean up temp file
      }
    })
    
  }, error = function(e) {
    # This will catch errors during the web scraping step
    warning(paste("Could not read the NOAA directory page:", conditionMessage(e)))
    return(NULL)
  })
}

# --- How to Use the Function ---

# Define the base URL once, outside the function.
storm_events_base_url <- "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"

# Download data for a single year
storm_data_2024 <- download_and_read_storm_events(2024)

# Download data for multiple years and combine them
years_to_download <- c(2020, 2022, 2024)
list_of_storm_data <- lapply(years_to_download, download_and_read_storm_events)

# Combine all the data frames in the list into one large data frame
# (This will only include the years that were successfully downloaded)
all_storm_data <- dplyr::bind_rows(list_of_storm_data)

# View the first few rows of the combined data
if (!is.null(all_storm_data) && nrow(all_storm_data) > 0) {
  print("Successfully combined all storm data:")
  head(all_storm_data)
}

#############################################################
# DIAGNOSIS


# Base URL for Storm Events data
storm_events_base_url <- "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"

# Define column types explicitly for read_csv to ensure consistency across years.
# This is crucial for avoiding `bind_rows` errors caused by type mismatches
# across different years' files (e.g., TOR_OTHER_CZ_FIPS being character in one, double in another).
# It also helps to prevent some 'parsing issues' warnings.
storm_events_col_types <- cols(
  .default = col_character(), # Default to character for safety
  BEGIN_LAT = col_double(),
  BEGIN_LON = col_double(),
  END_LAT = col_double(),
  END_LON = col_double(),
  INJURIES_DIRECT = col_double(),
  DEATHS_DIRECT = col_double(),
  MAGNITUDE = col_double(),
  TOR_LENGTH = col_double(),
  TOR_WIDTH = col_double()
)

# Setup our Download Function
download_and_read_storm_events <- function(year, col_spec) {
  message(paste("Processing year:", year))
  
  tryCatch({
    page_text <- read_html(storm_events_base_url) %>% html_text()
    all_filenames <- str_extract_all(page_text, "StormEvents_details-ftp_v1\\.0_d[0-9]{4}_c[0-9]{8}\\.csv\\.gz")[[1]]
    pattern <- paste0("StormEvents_details-ftp_v1.0_d", year)
    file_name <- grep(pattern, all_filenames, value = TRUE)[1]
    
    if (is.na(file_name)) {
      warning(paste("-> No file found for year", year))
      return(NULL)
    }
    
    full_url <- paste0(storm_events_base_url, file_name)
    temp_file <- tempfile(fileext = ".csv.gz")
    
    message(paste("  -> File found:", file_name))
    download.file(full_url, temp_file, mode = "wb", quiet = TRUE)
    
    data <- read_csv(temp_file, col_types = col_spec, show_col_types = FALSE)
    message(paste("  -> Successfully downloaded and read data for", year))
    
    file.remove(temp_file)
    return(data)
    
  }, error = function(e) {
    warning(paste("-> An error occurred for year", year, ":", conditionMessage(e)))
    return(NULL)
  })
}

# Download the Data
years_to_analyze <- 2015:2025
all_storm_data <- lapply(years_to_analyze, function(y) {
  download_and_read_storm_events(y, col_spec = storm_events_col_types)
})

storm_events_raw <- bind_rows(all_storm_data)

if (nrow(storm_events_raw) == 0) {
  stop("No storm event data was downloaded. Please check the year range and server status.")
}

# Data Cleaning and Filtering
tornado_data_colorado <- storm_events_raw %>%
  filter(
    EVENT_TYPE == "Tornado",
    STATE == "COLORADO", # Filter for the entire state
    !is.na(BEGIN_LAT) & !is.na(BEGIN_LON)
  ) %>%
  mutate(
    # Construct datetime from individual components for robustness
    datetime_str = paste0(BEGIN_YEARMONTH, sprintf("%02d", as.numeric(BEGIN_DAY)), sprintf("%04d", as.numeric(BEGIN_TIME))),
    event_datetime = ymd_hm(datetime_str, tz = "UTC", quiet = TRUE),
    
    year = year(event_datetime),
    month = lubridate::month(event_datetime, label = TRUE, abbr = FALSE),
    day_of_week = lubridate::wday(event_datetime, label = TRUE, abbr = FALSE),
    hour = hour(event_datetime),
    tornado_intensity = as.numeric(MAGNITUDE)
  ) %>%
  filter(!is.na(event_datetime)) %>%
  select(
    EVENT_ID, event_datetime, year, month, day_of_week, hour,
    tornado_intensity, BEGIN_LAT, BEGIN_LON,
    BEGIN_LOCATION, CZ_NAME, STATE,
    INJURIES_DIRECT, DEATHS_DIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS,
    TOR_LENGTH, TOR_WIDTH, EPISODE_ID, EPISODE_NARRATIVE, EVENT_NARRATIVE
  ) %>%
  arrange(desc(event_datetime))

# Visualization
if (nrow(tornado_data_colorado) > 0) {
  
  # Yearly Plot
  tornado_yearly_count <- tornado_data_colorado %>%
    group_by(year) %>%
    summarise(count = n(), .groups = 'drop')
  
  ggplot(tornado_yearly_count, aes(x = year, y = count, group = 1)) +
    geom_line(color = "steelblue") +
    geom_point(color = "darkblue") +
    labs(
      title = "Yearly Tornado Occurrences in Colorado",
      x = "Year",
      y = "Number of Tornadoes",
      caption = "Data Source: NOAA Storm Events Database"
    ) +
    theme_minimal()
  
} else {
  message("No tornado events found to plot.")
}

### TORNADO PATTERN ANALYSIS 
# Data acquisition and loading
tornado_data <- read_csv("storm_data_search_results.csv")

# Inspect the data
print(head(tornado_data))
print(str(tornado_data))
print(summary(tornado_data))

# Data cleaning and preperation
# Rename columns for easier use
tornado_data_clean <- tornado_data %>%
  rename(
    date = BEGIN_DATE,
    time = BEGIN_TIME,
    f_scale = TOR_F_SCALE,
    fatalities = DEATHS_DIRECT,
    injuries = INJURIES_DIRECT,
    property_damage = DAMAGE_PROPERTY_NUM,
    county = CZ_NAME_STR,
    latitude = BEGIN_LAT,
    longitude = BEGIN_LON,
    end_latitude = END_LAT,
    end_longitude = END_LON
  )

# Combine date and time to create a datetime column
# Ensure time is zero-padded to four digits (e.g., 17 -> 0017)
# Assuming 'time' is an integer, so we need to format it to a string with leading zeros if less than 4 digits
tornado_data_clean <- tornado_data_clean %>%
  mutate(
    time_str = sprintf("%04d", time), # Format time as 4-digit string with leading zeros
    date_time_start = mdy_hm(paste(date, time_str), quiet = TRUE) # Parse MM/DD/YYYY
  )

# Extract year and months
tornado_data_clean <- tornado_data_clean %>%
  mutate(
    year = lubridate::year(date_time_start),
    month = lubridate::month(date_time_start, label = TRUE, abbr = FALSE) # Full month name
  )

# Clean F/EF Scale: convert 'EFU' to NA and extract numeric part
# If TOR_F_SCALE is "EFU" it gets converted to NA. Otherwise, extract the digit after "EF"
tornado_data_clean <- tornado_data_clean %>%
  mutate(
    f_scale_numeric = as.numeric(gsub("EF", "", f_scale)) # Remove "EF" and convert to numeric
  ) %>%
  # Fill NA values in f_scale_number with MAGNITUDE if NAGNITUDE is numeric
  # NWS data sometimes uses MAGNITUDE for F/EF scale if TOR_F_SCALE is missing/EFU
  mutate(
    MAGNITUDE_numeric = as.numeric(MAGNITUDE), # Convert MAGNITUDE to numeric
    f_scale_numeric = ifelse(is.na(f_scale_numeric), MAGNITUDE_numeric, f_scale_numeric)
  )

# Filter for Colorado (STATE_ABBR should be 'CO')
tornado_data_clean <- tornado_data_clean %>%
  filter(STATE_ABBR == "CO")

# Remove rows with NA in critical columns for analysis
tornado_data_clean <- tornado_data_clean %>%
  filter(!is.na(year) & !is.na(f_scale_numeric))

# Tornado Analysis

# Annual Tornado Trends
annual_tornadoes <- tornado_data_clean %>%
  group_by(year) %>%
  summarise(count = n())

# Plot annual trends
p1 <- ggplot(annual_tornadoes, aes(x = year, y = count)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Annual Tornado Occurrences in Colorado",
    x = "Year",
    y = "Number of Tornadoes"
  ) +
  theme_minimal()
print(p1)
ggsave("annual_tornado_trends.png", width = 10, height = 6) # Save the plot

# Monthly Tornado Distribution (Seasonality)
monthly_tornadoes <- tornado_data_clean %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  # Ensure months are in order
  mutate(month = fct_relevel(month, month.name))

# Plot monthly distribution
p2 <- ggplot(monthly_tornadoes, aes(x = month, y = count, fill = month)) +
  geom_bar(stat = "identity") + 
  labs(
    title = "Monthly Tornado Occurrences in Colorado",
    x = "Month",
    y = "Number of Tornadoes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()
print(p2)
ggsave("monthly_tornado_distribution.png", plot = p2, width = 12, height = 7) # Save the plot

# Tornado Intensity Distribution (F/EF Scale)
f_scale_distribution <- tornado_data_clean %>%
  group_by(f_scale_numeric) %>%
  summarise(count = n()) %>%
  arrange(f_scale_numeric)

# Plot F/EF Scale distribution
p3 <- ggplot(f_scale_distribution, aes(x = factor(f_scale_numeric), y = count, fill = factor(f_scale_numeric))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Tornado Intensity Distribution in Colorado (F/EF Scale)",
    x = "F/EF Scale Rating",
    y = "Number of Tornadoes",
    fill = "Rating"
  ) +
  theme_minimal() +
  scale_fill_viridis_d()
print(p3) # Display the plot
ggsave("tornado_intensity_distribution.png", plot = p3, width = 8, height = 6)

# Tornadoes by County
# Remove 'CO' suffix from county names for cleaner labels
tornado_data_clean <- tornado_data_clean %>%
  mutate(county_clean = gsub(" CO\\.", "", county))

top_counties <- tornado_data_clean %>%
  group_by(county_clean) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10) # Get top 10 counties

# Plot top counties
p4 <- ggplot(top_counties, aes( x = reorder(county_clean, count), y = count, fill = county_clean)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 Colorado Counties by Tornado Occurrences",
    x = "County",
    y = "Number of Tornadoes"
  ) +
  coord_flip() + # Fill coordinate for better readability of county names
  theme_minimal() + 
  theme(legend.position = "none") +
  scale_fill_viridis_d()
print(p4) # Display the plot
ggsave("top_counties_tornadoes.png", plot = p4, width = 10, height = 7)

# Impact analysis (injuries, fatalities, property damage)
impact_by_f_scale <- tornado_data_clean %>%
  group_by(f_scale_numeric) %>%
  summarise(
    total_injuries = sum(injuries, na.rm = TRUE),
    total_fatalities = sum(fatalities, na.rm = TRUE),
    total_property_damage_billions = sum(property_damage, na.rm = TRUE) / 1e9 # Convert to billions
  ) %>%
  arrange(f_scale_numeric)

print(impact_by_f_scale)

# Plot fatalities by tornado intensity
p5 <- ggplot(impact_by_f_scale, aes(x = factor(f_scale_numeric), y = total_fatalities, fill = factor(f_scale_numeric))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Fatalities by Tornado Intensity (F/EF Scale) in Colorado",
    x = "F/EF Scale Rating",
    y = "Total Fatalities",
    fill = "Rating"
  ) +
  theme_minimal() +
  scale_fill_viridis_d()
print(p5)
ggsave("fatalities_by_f_scale.png", plot = p5, width = 8, height = 6)

# Plot property damage by Tornado Intensity
p6 <- ggplot(impact_by_f_scale, aes(x = factor(f_scale_numeric), y = total_property_damage_billions, fill = factor(f_scale_numeric))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Property Damage by Tornado Intenisty (F/EF Scale) in Colorado",
    x = "F/EF Scale Rating",
    y = "Total Property Damage (Billions USD)",
    fill = "Rating"
  ) +
  theme_minimal() +
  scale_fill_viridis_d()
print(p6)
ggsave("property_damage_by_f_scale.png", plot = p6, width = 8, height = 6)

# Spatial Analysis: Tornado Touchdown Locations and Paths
# Using base map of Colorado, we may need 'maps' package or 'sf' for more detailed maps
library(maps)
colorado_map <- map_data("county", region = "colorado")

p7 <- ggplot() +
  geom_polygon(data = colorado_map, aes(x = long, y = lat, group = group), fill = "white", color = "gray") +
  geom_point(data = tornado_data_clean, aes(x = longitude, y = latitude, color = f_scale_numeric), alpha = 0.6) +
  scale_color_viridis_c(option = "magma", direction = -1, name = "F/EF Scale") +
  scale_size_continuous(range = c(1, 8), name = "F/EF Scale") + # Adjust size range for better visualization
  labs(
    title = "Colorado Tornado Touchdown Locations by Intensity",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_void() +
  coord_fixed(1.3)
print(p7)
ggsave("tornado_touchdown_locations.png", plot = p7, width = 10, height = 8)

# For tornado paths, filter for valid start and end coordinates
tornado_paths_data <- tornado_data_clean %>%
  filter(!is.na(latitude) & !is.na(longitude) & !is.na(end_latitude) & !is.na(end_longitude))

if(nrow(tornado_paths_data) > 0) {
  p8 <- ggplot() +
    geom_polygon(data = colorado_map, aes(x = long, y = lat, group = group), fill = "white", color = "gray") +
    geom_segment(data = tornado_paths_data, aes(x = longitude, y = latitude, xend = end_longitude, yend = end_latitude, color = f_scale_numeric),
                 arrow = arrow(length = unit(0.02, "npc")), linewidth = 0.5, alpha = 0.7) +
    scale_color_viridis_c(option = "magma", direction = -1, name = "F/EF Scale") +
    labs(
      title = "Colorado Tornado Paths by Intensity",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_void() +
    coord_fixed(1.3)
  print(p8)
  ggsave("tornado_paths_approximation.png", plot = p8, width = 10, height = 8)
} else {
  print("No complete tornado path data (missing start/end coordinates) to plot paths.")
}

# Combine plots with patchwork
# Layout for the analytical plots
analytical_layout <- (p1+ p2) / (p3 + p4) / (p5 + p6)

# Add a main title and subtitle to the combined analyical plot
final_analytical_plot <- analytical_layout +
  plot_annotation(
    title = 'Comprehensive Analysis of Colorado Tornadoes',
    subtitle = 'Trends, Seasonality, Intensity, and Impact',
    caption = 'Source: NOAA Storm Events Database (storm_data_search_results.csv)',
    theme = theme(plot.title = element_text(size = 10), plot.subtitle = element_text(size = 14))
  )

# Print the combined analytical plot
print(final_analytical_plot)

# Save the combined analytical plot
ggsave("combined_tornado_analysis.png", plot = final_analytical_plot, width = 16, heigh = 12, dpi = 300)

# Layout for the spatial maps
spatial_layout <- p7 + p8

# Add a min title to the combined plot
final_spatial_plot <- spatial_layout +
  plot_annotation(
    title = 'Spatial Distribution of Colorado Tornadoes',
    caption = 'Source: NOAA Storm Events Database (storm_data_search_results.csv)',
    theme = theme(plot.title = element_text(size = 18))
  )

# Print the combined map plot
print(final_spatial_plot)

# Save the combined map plot
ggsave("combined_tornado_maps.png", plot = final_spatial_plot, width = 16, height = 8, dpi = 300)












