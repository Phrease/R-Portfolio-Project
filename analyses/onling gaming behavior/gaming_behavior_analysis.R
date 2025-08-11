# Install and load the necessary packages
library(dplyr)
library(ggplot2)

# Load the datasets from my working directory
gaming_data <- read.csv("online_gaming_behavior_dataset.csv")

# Let's take a look at the data structure
head(gaming_data)
str(gaming_data)

# Group the game genre and summarize key metrics
genre_summary <- gaming_data %>%
  group_by(GameGenre) %>%
  summarise(
    AvgPlayerLevel = mean(PlayerLevel),
    AvgAchievement = mean(AchievementsUnlocked),
    PlayerCount = n() # Count how many players are in each genre
  ) %>%
  arrange(desc(AvgPlayerLevel)) # Sort by the highest average level

# Print the summary table
print(genre_summary)

# Set EngagementLevel to a specific, logical order
gaming_data_ordered <- gaming_data %>%
  mutate(
    EngagementLevel = factor(EngagementLevel, levels = c("Low", "Medium", "High"))
  )

# Create a visualization
ggplot(gaming_data_ordered, aes(x = GameGenre, fill = EngagementLevel)) +
  geom_bar(position = "fill") +
  facet_wrap(~ ifelse(InGamePurchases == 1, "Made In-Game Purchase", "No In-Game Purchase")) +
  scale_fill_brewer(palette = "YlOrRd") + # A color-blind friendly pallete from yellow to red
  labs(
    title = "Player Engagement by Genre and Purchase Behavior",
    subtitle = "Engagement levels are now ordered logically",
    x = "Game Genre",
    y = "Proportion of Players"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# We need to calculate the count and percetnage for each purchase group
purchase_summary <- gaming_data %>%
  group_by(InGamePurchases) %>%
  summarise(
    PlayerCount = n()
  ) %>%
  mutate(
    Percentage = PlayerCount / sum(PlayerCount) * 100
  )

print(purchase_summary)

ggplot(gaming_data_ordered, aes(x = GameGenre, fill = EngagementLevel)) +
  geom_bar() + # Using the default stacked position
  facet_wrap(~ ifelse(InGamePurchases == 1, "Made In-Game Purchases", "No In-Game Purchases")) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(
    title = "Player Engagement and Count by Genre",
    subtitle = "Bar height shows the total number of players in each genre",
    x = "Game Genre",
    y = "Number of Players" # The y-axis is now a count, not a proportion
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# We need to investigate how we define medium engaged players
# Group by engagement level and calculate the average for key metrics
engagement_profile <- gaming_data %>%
  group_by(EngagementLevel) %>%
  summarise(
    Avg_PlayerLevel = mean(PlayerLevel),
    Avg_PlayTime_Hours = mean(PlayTimeHours),
    Avg_Sessions_Per_Week = mean(SessionsPerWeek),
    Avg_Achievements = mean(AchievementsUnlocked)
  ) %>%
  # Reorder the results logically
  mutate(EngagementLevel = factor(EngagementLevel, levels = c("Low", "Medium", "High"))) %>%
  arrange(EngagementLevel)

print(engagement_profile)

ggplot(gaming_data, aes(x = EngagementLevel, y = PlayTimeHours, fill = EngagementLevel)) +
  geom_boxplot() +
  scale_x_discrete(limits = c("Low", "Medium", "High")) + # Order the x-axis
  labs(
    title = "Distribution of Play Time by Engagement Level",
    x = "Engagement Level",
    y = "Play Time (Hours)"
  ) +
  theme_minimal() +
  guides(fill = "none") # Hide the legend as it's redundant