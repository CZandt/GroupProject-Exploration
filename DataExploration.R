##Test file

library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
data <- read_csv('https://www.dropbox.com/scl/fi/steivcw21cotncwx8tg46/20_train.csv?rlkey=a8x29lfr17pl0dbcsanx7b2qj&dl=1')


data

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
correlation_results <- sapply(data[, c("rate_positive_words", "rate_negative_words")], function(x) cor(x, data$self_reference_avg_sharess))


ggplot(data, aes(x = rate_positive_words, y = self_reference_avg_sharess)) +
  geom_point() +
  ggtitle("Scatterplot of rate_positive_words vs Self_reference_avg_sharess")

ggplot(data, aes(x = num_hrefs, y = shares)) +
  geom_point() +
  ggtitle("Scatterplot of num_hrefs vs shares")

ggplot(data, aes(x = num_imgs, y = shares)) +
  geom_point() +
  ggtitle("Scatterplot of num_imgs vs shares")

ggplot(data, aes(x = LDA_03, y = shares)) +
  geom_point() +
  ggtitle("Scatterplot of LDA_03 vs shares")

mean_kw_avg_avg <- mean(data$kw_avg_avg)

median_kw_avg_avg <- median(data$kw_avg_avg)

sd_kw_avg_avg <- sd(data$kw_avg_avg)

cat("Mean:", mean_kw_avg_avg, "\n")
cat("Median:", median_kw_avg_avg, "\n")
cat("Standard Deviation:", sd_kw_avg_avg, "\n")



correlation_results <- cor(data[c("n_tokens_title", "n_tokens_content", "n_unique_tokens", "n_non_stop_words", "n_non_stop_unique_tokens", "self_reference_avg_sharess")])

correlation_results


mean_LDA_03 <- mean(data$LDA_03)
median_LDA_03 <- median(data$LDA_03)
sd_LDA_03 <- sd(data$LDA_03)


cat("LDA_03 - Mean:", mean_LDA_03, "\n")
cat("LDA_03 - Median:", median_LDA_03, "\n")
cat("LDA_03 - Standard Deviation:", sd_LDA_03, "\n")


mean_self_ref_min_shares <- mean(data$self_reference_min_shares)
median_self_ref_min_shares <- median(data$self_reference_min_shares)
sd_self_ref_min_shares <- sd(data$self_reference_min_shares)


cat("Self Reference Min Shares - Mean:", mean_self_ref_min_shares, "\n")
cat("Self Reference Min Shares - Median:", median_self_ref_min_shares, "\n")
cat("Self Reference Min Shares - Standard Deviation:", sd_self_ref_min_shares, "\n")


mean_hrefs <- mean(data$num_hrefs)
median_hrefs <- median(data$num_hrefs)
sd_hrefs <- sd(data$num_hrefs)

mean_imgs <- mean(data$num_imgs)
median_imgs <- median(data$num_imgs)
sd_imgs <- sd(data$num_imgs)

cat("Self Reference Avg Shares - Mean:", mean_imgs, "\n")
cat("Self Reference Avg Shares - Median:", median_imgs, "\n")
cat("Self Reference Avg Shares - Standard Deviation:", sd_imgs, "\n")


mean_kw_max_avg <- mean(data$kw_max_avg)
median_kw_max_avg <- median(data$kw_max_avg)
sd_kw_max_avg <- sd(data$kw_max_avg)

cat("KW Max Avg - Mean:", mean_kw_max_avg, "\n")
cat("KW Max Avg - Median:", median_kw_max_avg, "\n")
cat("KW Max Avg - Standard Deviation:", sd_kw_max_avg, "\n")

correlation <- cor(data$num_imgs, data$kw_avg_avg)


correlation

print(correlation_results)

relevant_columns <- c("timedelta", "n_tokens_title", "n_tokens_content", 
                      "n_unique_tokens", "n_non_stop_words", "n_non_stop_unique_tokens",
                      "num_hrefs", "num_self_hrefs", "num_imgs", "num_videos", 
                      "average_token_length", "num_keywords", "data_channel_is_lifestyle", 
                      "data_channel_is_entertainment", "data_channel_is_bus", 
                      "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world", 
                      "kw_min_min", "kw_max_min", "kw_avg_min", "kw_min_max", 
                      "kw_max_max", "kw_avg_max", "kw_min_avg", "kw_max_avg", 
                      "kw_avg_avg", "self_reference_min_shares", "self_reference_max_shares", 
                      "self_reference_avg_sharess", "weekday_is_monday", "weekday_is_tuesday", 
                      "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", 
                      "weekday_is_saturday", "weekday_is_sunday", "is_weekend", 
                      "LDA_00", "LDA_01", "LDA_02", "LDA_03", "LDA_04", 
                      "global_subjectivity", "global_sentiment_polarity", 
                      "global_rate_positive_words", "global_rate_negative_words", 
                      "rate_positive_words", "rate_negative_words", 
                      "avg_positive_polarity", "min_positive_polarity", 
                      "max_positive_polarity", "avg_negative_polarity", 
                      "min_negative_polarity", "max_negative_polarity", 
                      "title_subjectivity", "title_sentiment_polarity", 
                      "abs_title_subjectivity", "abs_title_sentiment_polarity", 
                      "shares")


relevant_data <- data[, relevant_columns]

correlation_matrix <- cor(relevant_data)


print(correlation_matrix)

correlation_results <- cor(data[c("data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world", "kw_min_min", "kw_max_min", "kw_avg_min", "kw_min_max", "kw_max_max", "kw_avg_max", "kw_min_avg", "kw_max_avg", "kw_avg_avg", "weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday", "weekday_is_sunday", "is_weekend", "LDA_00", "LDA_01", "LDA_02", "LDA_03", "LDA_04", "global_subjectivity", "global_sentiment_polarity", "global_rate_positive_words", "global_rate_negative_words", "rate_positive_words", "rate_negative_words", "avg_positive_polarity", "min_positive_polarity", "max_positive_polarity", "avg_negative_polarity", "min_negative_polarity", "max_negative_polarity", "title_subjectivity", "title_sentiment_polarity", "abs_title_subjectivity", "abs_title_sentiment_polarity", "shares","self_reference_avg_sharess")])

correlation_with_target <- correlation_matrix["shares", ]


correlation_with_target <- correlation_with_target[-which(names(correlation_with_target) == "shares")]


sorted_correlation <- sort(correlation_with_target, decreasing = TRUE)


print(head(sorted_correlation, 15))


url_summary <- data %>%
  group_by(url) %>%
  summarise(mean_share_rate = mean(self_reference_avg_sharess),
            median_share_rate = median(self_reference_avg_sharess),
            num_articles = n())



ggplot(data, aes(x = kw_avg_avg, y = shares)) +
  geom_point() +
  labs(x = "Average Number of Keywords", y = "Shares") +
  ggtitle("Scatter Plot of Shares vs. Average Number of Keywords")

missing_kw_avg_avg <- sum(is.na(data$kw_avg_avg))
cat("Missing values in kw_avg_avg:", missing_kw_avg_avg, "\n")


missing_LDA_03 <- sum(is.na(data$LDA_03))
cat("Missing values in LDA_03:", missing_LDA_03, "\n")


missing_self_ref_min_shares <- sum(is.na(data$self_reference_min_shares))
cat("Missing values in self_reference_min_shares:", missing_self_ref_min_shares, "\n")


missing_num_hrefs <- sum(is.na(data$num_hrefs))
cat("Missing values in num_hrefs:", missing_num_hrefs, "\n")


missing_num_imgs <- sum(is.na(data$num_imgs))
cat("Missing values in num_imgs:", missing_num_imgs, "\n")

Q1 <- quantile(data$self_reference_min_shares, 0.25)
Q3 <- quantile(data$self_reference_min_shares, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

data_filtered <- data[data$self_reference_min_shares >= lower_bound & data$self_reference_min_shares <= upper_bound, ]


plot(data_filtered$self_reference_min_shares, data_filtered$shares,
     xlab = "self_reference_min_shares",
     ylab = "shares",
     main = "Scatterplot of shares vs self_reference_min_shares (without outliers)",
     col = "blue")

