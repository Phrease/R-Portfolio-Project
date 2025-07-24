# Load the required packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(forcats)

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
    year = year(date_time_start),
    month = month(date_time_start, label = TRUE, abbr = FALSE) # Full month name
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