# R-Portfolio-Project

Colorado County Economic Analysis
An exploratory data analysis of economic indicators across various counties in Colorado, focusing on rent burden and median income.

View the Final Portfolio
For the best viewing experience, you can see the final, rendered portfolio report here:
View the Portfolio

Project Overview
This project aims to identify economic trends and disparities among Colorado's counties. By analyzing publicly available data on employment, income, and population, I sought to answer key questions about the region's economic landscape. The goal was to use data visualization to tell a clear story about which counties are thriving and what factors might be contributing to their success.

Key questions explored:

Which counties have seen the most significant economic growth over the past five years?

Is there a correlation between population density and median income in Colorado?

Data Source
The dataset was sourced from the U.S. Census Bureau's American Community Survey (ACS) and the Colorado Department of Labor and Employment. The data covers the years 2018-2023 and was joined to create a comprehensive view of each county.

Tools and Libraries
Language: R

Libraries: tidycensus, tidyverse, sf, ggplot2, viridis, scales

Tools: RStudio, Git, GitHub

How to Run This Project
To reproduce this analysis on your own machine, please follow these steps:

Clone this repository:

git clone https://github.com/Phrease/R-Portfolio-Project.git

Ensure you have R and RStudio installed.

Open the co_rent_analysis.R file to view the full data cleaning and analysis process.

To view the final report, either open the index.html file in a web browser or knit the portfolio.Rmd file in RStudio.

**IMPORTANT**

This project uses the `tidycensus` R package to analyze census data. To run the scripts, you will need to acquire and install your own free Census Bureau API key.

## 🔑 Setup Instructions

1.  **Get a Key**: Request a free API key from the Census Bureau's website: [https://api.census.gov/data/key_signup.html](https://api.census.gov/data/key_signup.html)

2.  **Install the Key**: Once you have your key, run the following command **in your R console** (do not add this line to the script). This will save the key for all future R sessions on your computer.

    ```R
    # Run this command only once in your console
    census_api_key("PASTE_YOUR_KEY_HERE", install = TRUE)
    ```

3.  **Restart R**: After installing the key, restart your R session.

4.  **Run the Script**: The R script should now run successfully without any modifications.

Key Findings
A few of the key insights from the analysis include:

The analysis revealed a significant positive correlation between population growth and rising median income, particularly in the Front Range counties like Denver, Douglas, and Boulder.

A data visualization of median household income highlighted key areas of economic strength and potential areas for development in the Northern and Eastern Plains.