# Forecasting Denver's First Snowfall

### Project Objective
This project uses R and the `tidymodels` framework to build a machine learning model that forecasts the date of the first measurable snowfall in Denver, Colorado, based on historical weather data.

### Key Findings
- A Random Forest model was trained on over 70 years of climate data from NOAA.
- The model's final predictions were, on average, off by **~19 days** (Mean Absolute Error of 18.6).
- The model's features (such as temperature and precipitation in preceding months) could only explain about **9%** of the variation in the first snow date (R-squared of 0.09).
- **Conclusion:** The timing of the first snow is incredibly difficult to predict from long-range climate signals, highlighting the chaotic nature of specific weather events.

### How to Run This Project
1. Clone or download this repository.
2. The R scripts require the following packages: `tidyverse`, `tidymodels`.
3. The data can be downloaded from the [NOAA Climate Data Online portal](https://www.ncdc.noaa.gov/cdo-web/). Use station ID `USW00003017` for Denver International Airport and `USW00023062` for Denver Central Park.

---
  *This analysis was completed on August 10, 2025.*