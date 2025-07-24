# To perform a data analysis on the astrodatR package, we need go into the respository and download it from there as this dataset has been deprecated
# Install removes library to access the dataset
library(remotes)

# Install astrodatR from the CRAN archive
# The version 0.1 was the last one available before archiving
install_version("astrodatR", version = "0.1", repos = "http://cran.us.r-project.org")

# Now we can load the astrodatR library
library(astrodatR)

# Access the asteroid_dens dataset
data("asteroid_dens")

# Rename our columns for easier interpretation
asteroid_dens <- asteroid_dens %>%
  rename(
    name = Asteroid,
    density = Dens,
    unc = Err
  )

# For this analysis, we'll be looking at the Weighted Mean and Robust Estimation
# Load the following libraries
library(ggplot2)
library(dplyr)

# Explore the basic statistics
cat("--- Basic Statistics ---\n")
mean_density <- mean(asteroid_dens$density)
median_density <- median(asteroid_dens$density)
sd_density <- sd(asteroid_dens$density)

cat("Simple Mean Density:", round(mean_density, 3), "g/cm^3\n")
cat("Median Density:", round(median_density, 3), "g/cm^3\n")
cat("Standard Deviation of Densities:", round(sd_density, 3), "g/cm^3\n")

# Calculate the weighted mean density
# Weights are inversely proportional to the variance (uncertainty squared)
# w_i = 1 / sigma_i^2
# X_w = sum(w_i * x_i) / sum(w_i)
asteroid_dens <- asteroid_dens %>%
  mutate(variance = unc^2,          # Calculate variance from uncertainty
         weight = 1 / variance)     # Calculate weight

weighted_mean_density <- sum(asteroid_dens$density * asteroid_dens$weight) / sum(asteroid_dens$ weight)
cat("Weighted Mean Density (accounting for uncertainty):", round(weighted_mean_density, 3), "g/cm^r\n")

# Calculate the standard error of the weighted mean
se_weighted_mean <- sqrt(1 / sum(asteroid_dens$weight))
cat("Standard Error of the Weighted Mean:", round (se_weighted_mean, 3), "g/cm^r\n")

cat("\n--- Precision Analysis ---\n")
# Sort by uncertainty to see which measurements are more/least precise
asteroid_dens_sorted_unc <- asteroid_dens %>%
  arrange(unc)

cat("Top 5 Most Precisely Measure Asteroids:\n")
print(head(asteroid_dens_sorted_unc, 5) %>% select(name, density, unc))

cat("Top 5 Least Precisely Measure Asteroids:\n")
print(tail(asteroid_dens_sorted_unc, 5) %>% select(name, density, unc))

# --- Visualizing the Weighted Mean and Distribution ---

# Re-create the density plot with error bars, and add the weighted mean
asteroid_dens_ordered <- asteroid_dens %>%
  arrange(density) %>%
  mutate(asteroid_name_factor = factor(name, levels = name))

ggplot(asteroid_dens_ordered, aes(x = asteroid_name_factor, y = density)) +
  geom_pointrange(aes(ymin = density - unc, ymax = density + unc), color = "darkblue") +
  geom_hline(yintercept = weighted_mean_density, linetype = "dashed", color = "red", size = 0.8) +
  annotate("text", x = 1, y = weighted_mean_density + 0.2, label = paste("Weighted Mean:", round(weighted_mean_density)), color = "red", hjust = 0) +
  labs(title = "Asteroid Densities with Measurement Uncertainties and Weighted Mean",
       x = "Asteroid",
       y = "Density (g/cm^3)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Histogram of the weighted mean
ggplot(asteroid_dens, aes(x = density)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = weighted_mean_density, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Distribution of Asteroid Densities with Weighted Mean",
       x = "Density (g/cm^3)",
       y = "Frequency") +
  theme_minimal() +
  annotate("text", x = weighted_mean_density + 0.5, y = max(hist(asteroid_dens$density, breaks=seq(min(asteroid_dens$density), max(asteroid_dens$density)+0.2, by=0.2), plot=FALSE)$counts) * 0.9,
           label = paste("Weighted Mean:", round(weighted_mean_density, 2)), color = "red", hjust = 0)

# Investigate potential outliers (Qualitative, given small N)
asteroid_dens_deviations <- asteroid_dens %>%
  mutate(deviation_from_wmean = abs(density - weighted_mean_density),
         deviation_in_unc = deviation_from_wmean / unc) %>%
  arrange(desc(deviation_in_unc))

cat("\n--- Asteroids with Largest Deviations (in terms of their uncertainty) ---\n")
print(head(asteroid_dens_deviations, 5) %>%
        mutate(w_mean = weighted_mean_density) %>%
        select(name, density, unc, w_mean, deviation_in_unc))


