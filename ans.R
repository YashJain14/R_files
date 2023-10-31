# Load required libraries
library(dplyr)
library(caret)
library(rpart)
library(corrplot)
library(data.table)
library(ggplot2)  
library(gridExtra)
library(rpart)
library(rpart.plot)

#Load Data
setwd("/Users/yash/Downloads/AY23 BC2406 CBA")
data <- fread("marun_sample2.csv")

# Check for missing values
missing_values <- data %>%
  summarise_all(~sum(is.na(.)))

# Print the results
cat("Missing Values:\n")
print(missing_values)

# Handle missing values (impute with mean for numeric columns)
data <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) 

# Count unique values in each column
unique_counts <- sapply(data, function(x) length(unique(x)))

# Create a data frame with column names and unique value counts
unique_counts_df <- data.frame(
  ColumnName = names(unique_counts),
  UniqueCount = unique_counts
)

# Display the result
print(unique_counts_df)


# Formation appears as a categorical column
data$Formation <- as.factor(data$Formation)

# Replace Missing Values
categorical_columns <- c("Formation")
continuous_columns <- setdiff(names(data), categorical_columns)


# Outlier Detection and Handling
data_cleaned <- data
predictor_vars <- setdiff(continuous_columns, "MUDLOSSU")
outliers_list <- list()

for (var in predictor_vars) {
  q1 <- quantile(data[[var]], 0.25)
  q3 <- quantile(data[[var]], 0.75)
  iqr <- q3 - q1
  lower_limit <- q1 - 1.5 * iqr
  upper_limit <- q3 + 1.5 * iqr
  outliers <- data[[var]][data[[var]] < lower_limit | data[[var]] > upper_limit]
  outliers_list[[var]] <- outliers
  data_cleaned <- data_cleaned[!(data_cleaned[[var]] < lower_limit | data_cleaned[[var]] > upper_limit), ]
}




