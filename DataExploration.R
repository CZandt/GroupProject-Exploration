##Test file

library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
data <- read_csv('https://www.dropbox.com/scl/fi/steivcw21cotncwx8tg46/20_train.csv?rlkey=a8x29lfr17pl0dbcsanx7b2qj&dl=1')

# Exploratory Data Analysis
# -----------------------------------------------------------------------------------------------------------

# Part A (Dependent Variable Characteristics)

# Summary statistics
summary_stats <- data %>% 
  summarise(mean = mean(shares, na.rm = TRUE),
            standar_dev = sd(shares, na.rm = TRUE),
            min = min(shares, na.rm = TRUE),
            q1 = quantile(shares, probs = 0.25, na.rm = TRUE),
            median = median(shares, na.rm = TRUE),
            q3 = quantile(shares, probs = 0.75, na.rm = TRUE),
            max = max(shares, na.rm = TRUE)
            ) %>% glimpse()

# Missingness Analysis
missingness <- data %>% 
  count(is.na(shares))

# Checking data type
data_types <- data %>% 
  count(is.numeric(shares))

# Distribution Analysis
  # Box Plot w/ Outliers
plotOutliers <- data %>% 
  ggplot(aes(x = shares)) +
  geom_boxplot(width = 0.1, fill = "skyblue", color = "black") +
  labs(title = 'Shares Box Plot (With Outliers)') +
  theme_bw()

print(plotOutliers)

  #Box Plot without Outliers 
plotNoOutliers <- data %>% 
  filter(shares >= quantile(shares, probs = .1 ) & shares <= quantile(shares, probs = .9)) %>% 
  ggplot(aes(x = shares)) +
  geom_boxplot(width = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Shares (Outliers Removed)", x = 'Number of Shares') +
  theme_bw()

print(plotNoOutliers)

  # Box Plot and Violin without Outliers
plotCurveNoOutliers <- data %>% 
  filter(shares >= quantile(shares, probs = .1 ) & shares <= quantile(shares, probs = .9)) %>% 
ggplot(aes(x = "", y = shares)) +
  geom_violin(fill = "skyblue", color = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = "lightgray", color = "black", outlier.shape = NA) +
  coord_flip() +
  labs(title = "Distribution of Shares (Outliers Removed)", x = NULL, y = "Number of Shares") +
  theme_bw()

print(plotCurveNoOutliers)

# Part B (High-Level Summary of Variables)

# Analyze Missing Data
missing_by_column <- tibble(Column = names(data), Missing_Values = sapply(data, function(x) sum(is.na(x))))

# Create a histogram for each column to view distributions
df_long <- data %>%  
  select(-url) %>% 
  pivot_longer(cols = everything(), names_to = "Column", values_to = "Value")

histograms <- ggplot(df_long, aes(x = Value)) +
  geom_histogram() +
  facet_wrap(~Column, scales = "free")

print(histograms)

# Part C (Analysis of Independent Variables)