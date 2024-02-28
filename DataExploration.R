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
data %>% 
  count(is.numeric(shares))

# Distribution Analysis
  # Box Plot w/ Outliers
data %>% 
  ggplot(aes(x = shares)) +
  geom_boxplot(width = 0.1, fill = "skyblue", color = "black") +
  labs(title = 'Shares Box Plot (With Outliers)') +
  theme_bw()

  #Box Plot without Outliers 
data %>% 
  filter(shares >= quantile(shares, probs = .1 ) & shares <= quantile(shares, probs = .9)) %>% 
  ggplot(aes(x = shares)) +
  geom_boxplot(width = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Shares (Outliers Removed)", x = 'Number of Shares') +
  theme_bw()
  # Box Plot and Violin without Outliers
data %>% 
  filter(shares >= quantile(shares, probs = .1 ) & shares <= quantile(shares, probs = .9)) %>% 
ggplot(aes(x = "", y = shares)) +
  geom_violin(fill = "skyblue", color = "blue", alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = "lightgray", color = "black", outlier.shape = NA) +
  coord_flip() +
  labs(title = "Distribution of Shares (Outliers Removed)", x = NULL, y = "Number of Shares") +
  theme_bw()

# Part B (High-Level Summary of Variables)

# Part C (Analysis of Independent Variables)