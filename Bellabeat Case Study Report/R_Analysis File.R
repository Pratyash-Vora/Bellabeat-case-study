###############################################################################
# BELLABEAT CASE STUDY — DATA ANALYSIS SCRIPT
# Author: Pratyash
# Purpose: Clean, merge, analyze, and visualize Fitbit user data for insights
###############################################################################

########################
# 1. Load Required Packages
########################

# tidyverse → for data manipulation and plotting
# lubridate → for working with dates
# ggplot2 → visualization (included in tidyverse)
# dplyr → core data transformation functions

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)


########################
# 2. Import CSV Data
########################

# IMPORTANT:
# Use the exact filenames located in your project folder.
# These files were exported from your Google Sheets after cleaning.

daily_activity <- read_csv("dailyActivy_merged.csv")
daily_calories <- read_csv("dailyCalories_merged.csv")
daily_intensities <- read_csv("dailyIntensities_merged.csv")
daily_steps <- read_csv("dailySteps_merged.csv")

hourly_steps <- read_csv("hourlySteps_merged.csv")
hourly_calories <- read_csv("hourlyCalories_merged.csv")
hourly_intensities <- read_csv("hourlyIntensities_merged.csv")

sleep_day <- read_csv("sleepDay_merged.csv")


########################
# 3. Clean Data
########################

# Convert text dates (MM/DD/YYYY) into proper Date format
# This is necessary for any time-based analysis or merging.

daily_activity <- daily_activity %>%
  mutate(ActivityDate = mdy(ActivityDate))

# Sleep dataset contains timestamps — extract only the date part
# then convert to proper Date format and remove duplicates
sleep_day <- sleep_day %>%
  mutate(SleepDay = str_sub(SleepDay, 1, 10)) %>% 
  mutate(SleepDay = mdy(SleepDay)) %>%
  distinct()


########################
# 4. Merge Activity + Sleep Data
########################

# We join using Id (user identifier) and Date
# Left join keeps all activity data and adds sleep data where available.

merged <- daily_activity %>%
  left_join(sleep_day, by = c("Id" = "Id", "ActivityDate" = "SleepDay"))


########################
# 5. Summary Statistics
########################

# Daily activity stats: movement + calories
daily_activity %>%
  summarise(
    rows = n(),
    mean_steps = mean(TotalSteps, na.rm = TRUE),
    median_steps = median(TotalSteps, na.rm = TRUE),
    max_steps = max(TotalSteps, na.rm = TRUE),
    min_steps = min(TotalSteps, na.rm = TRUE),
    mean_calories = mean(Calories, na.rm = TRUE)
  )

# Sleep stats: number of logs + average sleep time
sleep_day %>%
  summarise(
    rows = n(),
    mean_sleep_min = mean(TotalMinutesAsleep, na.rm = TRUE),
    median_sleep_min = median(TotalMinutesAsleep, na.rm = TRUE)
  )


########################
# 6. Correlation: Steps vs Calories
########################

# Shows how strongly steps influence calories burned.
cor(merged$TotalSteps, merged$Calories, use = "complete.obs")


########################
# 7. Weekday Activity Trends
########################

# Adds weekday column → averages steps per weekday → identifies most/least active days.
weekday_steps <- merged %>%
  mutate(weekday = wday(ActivityDate, label = TRUE, abbr = TRUE)) %>%
  group_by(weekday) %>%
  summarise(
    avg_steps = mean(TotalSteps, na.rm = TRUE),
    count = n()
  )

weekday_steps


########################
# 8. Visualizations
########################

# A. Steps vs Calories (scatter + trendline)
ggplot(merged, aes(TotalSteps, Calories)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relationship Between Steps and Calories",
    x = "Total Steps",
    y = "Calories Burned"
  )


# B. Average Daily Steps Over Time
daily_trend <- merged %>%
  group_by(ActivityDate) %>%
  summarise(avg_steps = mean(TotalSteps, na.rm = TRUE))

ggplot(daily_trend, aes(ActivityDate, avg_steps)) +
  geom_line(color = "forestgreen", size = 1) +
  geom_point(color = "darkgreen") +
  labs(
    title = "Average Daily Steps Over Time",
    x = "Date",
    y = "Average Steps"
  ) +
  theme_minimal()


# C. Sleep Duration vs Steps
sleep_activity <- merged %>%
  filter(!is.na(TotalMinutesAsleep)) %>%
  mutate(hours_asleep = TotalMinutesAsleep / 60)

ggplot(sleep_activity, aes(hours_asleep, TotalSteps)) +
  geom_point(alpha = 0.4, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Relationship Between Sleep Duration and Steps",
    x = "Hours Asleep",
    y = "Total Steps"
  ) +
  theme_minimal()


########################
# END OF SCRIPT
########################

