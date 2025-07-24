# Load the following libraries
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(readr)
library(sf)
library(ggplot2)

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
  extent = denver_bbox, # Use 'extent' instead of 'locationid'
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
co_fips <- "FIPS:80"

# Endpoint for stations
stations_url <- paste0(base_url, "stations")

# Define query parameters for stations
query_params_stations <- list(
  datasetid = "GHCND",
  locationid = co_fips,
  # Dates here limit stations to those active during this period
  startdate = "2020-01-01",
  enddate = "2022-12-31", # NOAA API v2 data generally ends around Sept 2022.
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

# For this analysis, we're specifically looking for Tornado weather patterns in Denver County
storm_events_base_url <- "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"

# Function to download and read a single year's details file
download_and_read_storm_events <- function(year) {
  # Common recent creation dates to try, from newest to oldest.
  # Manually inspect https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/
  # to find the latest common creation dates for different year ranges.
  # As of July 2025, c20250520 is very prevalent for many years.
  # Some recent years (like 2020, 2021) might have older creation dates.
  common_creation_dates <- c(
    "20250520", # Latest major update for many years
    "20240620", # Common for some recent years (e.g., 2020)
    "20240101", # Another potential recent update date
    "20230620", # Example of another possible date
    "20220620"  # Example for even older files
  )
  
  for (cdate in common_creation_dates) {
    file_name <- paste0("StormEvents_details-ftp_v1.0_d", year, "_c", cdate, ".csv.gz")
    full_url <- paste0(storm_events_base_url, file_name)
    temp_file <- tempfile(fileext = ".csv.gz")
    
    message(paste("Checking URL for year", year, "with creation date", cdate, ":", full_url))
    
    # Use HEAD request to check if the file exists without downloading it
    response_head <- HEAD(full_url)
    
    if (http_status(response_head)$category == "Success") {
      message(paste("File found for year", year, "with creation date", cdate, ". Attempting download."))
      
      # Now that we know the file exists, try to download and read it
      tryCatch({
        download.file(full_url, temp_file, mode = "wb", quiet = TRUE)
        data <- read_csv(temp_file, show_col_types = FALSE)
        message(paste("Successfully downloaded and read data for", year, "with creation date", cdate))
        return(data) # Return data and exit function if successful
      }, error = function(e) {
        # Explicitly convert error object to character string for the warning message
        error_msg <- conditionMessage(e) # This is usually more reliable than e$message
        warning(paste("Error reading file for year", year, "with creation date", cdate, ":", error_msg))
        # Do NOT return NULL here; allow the loop to try the next common_creation_date
      }, finally = {
        if (file.exists(temp_file)) {
          file.remove(temp_file) # Clean up temp file
        }
      })
    } else {
      # File not found with this creation date, print message and continue to next in loop
      message(paste("File not found (HTTP", status_code(response_head), ") for year", year, "with creation date", cdate, ". Trying next common date."))
    }
  }
  
  # If loop finishes and no file was successfully downloaded
  warning(paste("Could not download or read data for year", year, "after trying all common creation dates."))
  return(NULL) # Indicate failure for this specific year
}

# Define years to download
years_to_analyze <- 2000:2023 # Covers a good historical range

# Download and combine data for all years
all_storm_data <- list()
for (year in years_to_analyze) {
  df <- download_and_read_storm_events(year)
  if (!is.null(df)) {
    all_storm_data[[as.character(year)]] <- df
  }
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
  # Core identifiers and types
  STATE_FIPS = col_character(),
  CZ_FIPS = col_character(),
  CZ_NAME = col_character(),
  BEGIN_LOCATION = col_character(),
  EVENT_TYPE = col_character(),
  EPISODE_ID = col_character(), # Often large numbers, safer as character
  EVENT_ID = col_character(),   # Unique identifier, character
  
  # Date and Time components (read as character for flexible parsing later)
  BEGIN_DATE_TIME = col_character(),
  END_DATE_TIME = col_character(),
  BEGIN_TIME = col_character(),
  END_TIME = col_character(),
  
  # Geographic Coordinates (should be numeric/double)
  BEGIN_LAT = col_double(),
  BEGIN_LON = col_double(),
  END_LAT = col_double(),
  END_LON = col_double(),
  
  # Tornado Specifics (magnitude is numeric, length/width double)
  MAGNITUDE = col_double(), # EF-scale, can be decimal
  TOR_LENGTH = col_double(),
  TOR_WIDTH = col_double(),
  
  # Impact Data (read as double, handle NA/empty strings)
  INJURIES_DIRECT = col_double(),
  DEATHS_DIRECT = col_double(),
  
  # Damage Data (read as character first, requires custom parsing for K/M/B)
  DAMAGE_PROPERTY = col_character(),
  DAMAGE_CROPS = col_character(),
  
  # FIPS for other affected zones (explicitly set to character to resolve the error)
  TOR_OTHER_CZ_FIPS = col_character(),
  TOR_OTHER_CZ_NAME = col_character(),
  TOR_OTHER_CZ_STATE = col_character(),
  
  # Set a default for any other columns not explicitly listed.
  # This makes the type inference safer, but explicit is always better for known columns.
  .default = col_character()
)


# Function to download and read a single year's details file
download_and_read_storm_events <- function(year) {
  # Common recent creation dates to try, from newest to oldest.
  # These are based on observed filenames on the NCEI server.
  # As of July 7, 2025, 20250520 is the most recent common.
  common_creation_dates <- c(
    "20250520", # Latest major update for many years (as of 2025-07)
    "20240620", # Common for some recent years (e.g., 2020, 2021)
    "20240101", # Another potential common update date
    "20230620", # Older common update date
    "20220620"  # Even older common update date
  )
  
  for (cdate in common_creation_dates) {
    file_name <- paste0("StormEvents_details-ftp_v1.0_d", year, "_c", cdate, ".csv.gz")
    full_url <- paste0(storm_events_base_url, file_name)
    temp_file <- tempfile(fileext = ".csv.gz")
    
    message(paste0("Checking URL for year ", year, " with creation date ", cdate, ": ", full_url))
    
    # --- Step 1: Robust HEAD request ---
    head_success <- FALSE
    tryCatch({
      response_head <- HEAD(full_url)
      if (http_status(response_head)$category == "Success") {
        head_success <- TRUE
        message(paste("File found (HEAD success) for", year, "with creation date", cdate))
      } else {
        message(paste("File not found or HEAD failed (HTTP", status_code(response_head), ") for", year, "with creation date", cdate, ". Trying next common date."))
      }
    }, error = function(e) {
      message(paste0("HEAD request error for ", full_url, ": ", as.character(e)))
    }, warning = function(w) {
      message(paste0("HEAD request warning for ", full_url, ": ", as.character(w)))
      if (grepl("cannot open URL", conditionMessage(w)) || grepl("404 Not Found", conditionMessage(w))) {
        # Explicitly a 404, don't proceed with download
      } else {
        response_head_after_warn <- HEAD(full_url)
        if (http_status(response_head_after_warn)$category == "Success") {
          head_success <- TRUE
          message("Proceeding despite HEAD warning as file was ultimately found.")
        }
      }
    })
    
    
    if (head_success) {
      # --- Step 2: Attempt download using download.file ---
      download_successful <- FALSE
      tryCatch({
        download_result <- download.file(full_url, temp_file, mode = "wb", quiet = TRUE)
        if (download_result == 0) {
          download_successful <- TRUE
          message(paste("Download completed for", year, "with creation date", cdate))
        } else {
          message(paste("download.file returned non-zero status (failure) for", year, "with creation date", cdate, ". Trying next common date."))
        }
      }, error = function(e) {
        message(paste0("Error during download.file for ", full_url, ": ", as.character(e)))
      }, warning = function(w) {
        message(paste0("Warning during download.file for ", full_url, ": ", as.character(w)))
        if (grepl("cannot open URL", conditionMessage(w)) || grepl("404 Not Found", conditionMessage(w))) {
          # Download failed due to 404
          message(paste("Download failed due to 404 warning from download.file for", year, "with creation date", cdate, ". Trying next common date."))
        } else {
          if (file.exists(temp_file) && file.info(temp_file)$size > 0) {
            download_successful <- TRUE
            message(paste("Download considered successful despite warning for", year, "with creation date", cdate))
          } else {
            message(paste("Download yielded no valid file after warning for", year, "with creation date", cdate, ". Trying next common date."))
          }
        }
      })
      
      if (download_successful && file.exists(temp_file) && file.info(temp_file)$size > 0) {
        # --- Step 3: Attempt to read CSV with explicit column types ---
        tryCatch({
          data <- read_csv(temp_file, show_col_types = FALSE, col_types = storm_events_col_types)
          message(paste("Successfully read data for", year, "with creation date", cdate))
          return(data)
        }, error = function(e) {
          warning(paste("Critical error reading CSV file for year", year, "with creation date", cdate, ". Error object:", as.character(e)))
        }, finally = {
          if (file.exists(temp_file)) {
            file.remove(temp_file)
          }
        })
      } else {
        message(paste("Download did not yield a valid file for year", year, "with creation date", cdate, ". Moving to next common date."))
        if (file.exists(temp_file)) file.remove(temp_file)
      }
    } else {
      # HEAD request failed, loop to next creation date.
    }
  }
  
  warning(paste("Could not download or read data for year", year, "after trying all common creation dates."))
  return(NULL)
}

# Define the range of years to download.
years_to_analyze <- 2000:lubridate::year(Sys.Date())

all_storm_data <- list()
for (year in years_to_analyze) {
  df <- download_and_read_storm_events(year)
  if (!is.null(df)) {
    all_storm_data[[as.character(year)]] <- df
  }
}

storm_events_raw <- bind_rows(all_storm_data)

if (nrow(storm_events_raw) == 0) {
  stop("No storm event data downloaded successfully for any year. Please check your internet connection, the NCEI server, or the 'common_creation_dates' list.")
}
print(paste("Total storm events records:", nrow(storm_events_raw)))
print(colnames(storm_events_raw))
print(head(storm_events_raw))

message("\n--- Diagnosing read_csv parsing problems ---")
problems_df <- problems(storm_events_raw)
if (nrow(problems_df) > 0) {
  print(problems_df)
} else {
  message("No parsing problems found!")
}
message("--- End Parsing Problems Diagnosis ---")


# --- Data Cleaning and Filtering for Tornadoes in Denver Area ---
denver_lat <- 39.7392
denver_lon <- -104.9903

min_lat <- denver_lat - 0.5
max_lat <- denver_lat + 0.5
min_lon <- denver_lon - 0.5
max_lon <- denver_lon + 0.5

tornado_data_denver <- storm_events_raw %>%
  filter(
    EVENT_TYPE == "Tornado",
    !is.na(BEGIN_LAT) & !is.na(BEGIN_LON) &
      BEGIN_LAT >= min_lat & BEGIN_LAT <= max_lat &
      BEGIN_LON >= min_lon & BEGIN_LON <= max_lon
  ) %>%
  mutate(
    parsed_time_str = sprintf("%04s", as.character(BEGIN_TIME)),
    event_datetime = lubridate::parse_date_time(paste(BEGIN_DATE_TIME, parsed_time_str),
                                                orders = c("mdy HM", "ymd HM"))
  ) %>%
  filter(!is.na(event_datetime)) %>%
  mutate(
    year = year(event_datetime),
    month = month(event_datetime, label = TRUE, abbr = TRUE),
    day_of_week = wday(event_datetime, label = TRUE, abbr = TRUE),
    hour = hour(event_datetime),
    tornado_intensity = as.numeric(MAGNITUDE)
  ) %>%
  select(
    EVENT_ID, event_datetime, year, month, day_of_week, hour,
    tornado_intensity, BEGIN_LAT, BEGIN_LON,
    BEGIN_LOCATION, CZ_NAME, STATE, # <--- **FIXED HERE: Changed STATE_ABBR to STATE**
    INJURIES_DIRECT, DEATHS_DIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS,
    TOR_LENGTH, TOR_WIDTH, EPISODE_ID, EPISODE_NARRATIVE, EVENT_NARRATIVE
  ) %>%
  arrange(event_datetime)

print(paste("Number of tornado events in Denver area:", nrow(tornado_data_denver)))
print(head(tornado_data_denver))
print(summary(tornado_data_denver$tornado_intensity))

# --- Explore Tornado Patterns ---
if (nrow(tornado_data_denver) > 0) {
  
  tornado_yearly_count <- tornado_data_denver %>%
    group_by(year) %>%
    summarise(count = n()) %>%
    ungroup()
  print(ggplot(tornado_yearly_count, aes(x = year, y = count)) +
          geom_line(color = "steelblue") +
          geom_point(color = "darkblue") +
          labs(title = "Yearly Tornado Occurrences in Denver Area",
               x = "Year", y = "Number of Tornadoes") +
          theme_minimal() +
          scale_x_continuous(breaks = seq(min(tornado_yearly_count$year), max(tornado_yearly_count$year), by = 2)))
  
  tornado_monthly_count <- tornado_data_denver %>%
    group_by(month) %>%
    summarise(count = n()) %>%
    ungroup()
  print(ggplot(tornado_monthly_count, aes(x = month, y = count)) +
          geom_bar(stat = "identity", fill = "coral") +
          labs(title = "Monthly Tornado Occurrences in Denver Area",
               x = "Month", y = "Number of Tornadoes") +
          theme_minimal())
  
  tornado_hourly_count <- tornado_data_denver %>%
    group_by(hour) %>%
    summarise(count = n()) %>%
    ungroup()
  print(ggplot(tornado_hourly_count, aes(x = hour, y = count)) +
          geom_bar(stat = "identity", fill = "lightgreen") +
          labs(title = "Hourly Tornado Occurrences in Denver Area",
               x = "Hour of Day (MDT)", y = "Number of Tornadoes") +
          theme_minimal() +
          scale_x_continuous(breaks = 0:23))
  
  print(ggplot(tornado_data_denver, aes(x = factor(tornado_intensity))) +
          geom_bar(fill = "purple") +
          labs(title = "Tornado Intensity Distribution (EF-Scale) in Denver Area",
               x = "Tornado Intensity (EF-Scale)", y = "Number of Tornadoes") +
          theme_minimal())
  
  print(ggplot(tornado_data_denver, aes(x = BEGIN_LON, y = BEGIN_LAT, color = tornado_intensity)) +
          geom_point(alpha = 0.7, size = 3) +
          geom_point(aes(x = denver_lon, y = denver_lat), color = "red", shape = 8, size = 5, inherit.aes = FALSE) +
          annotate("text", x = denver_lon, y = denver_lat + 0.05, label = "Denver Center", color = "red", vjust = 0, size = 4) +
          scale_color_viridis_c(option = "plasma", direction = -1, name = "EF-Scale") +
          labs(title = "Geographic Distribution of Tornadoes in Denver Area",
               subtitle = "Red star marks approximate Denver City Center",
               x = "Longitude", y = "Latitude") +
          coord_fixed() +
          theme_minimal())
  
} else {
  message("No tornado events found within the specified Denver area and date range for plotting.")
}

















