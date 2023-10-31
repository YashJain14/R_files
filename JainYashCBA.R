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

#EDA
continuous_data <- data[, ..continuous_columns]
correlation_matrix <- cor(continuous_data)
print(correlation_matrix)
corrplot(correlation_matrix, method = "color")

# Print the top 5 correlations
correlation_df <- as.data.frame(as.table(correlation_matrix))
correlation_df <- correlation_df[correlation_df$Var1 != correlation_df$Var2, ]
correlation_df$Abs_Freq <- abs(correlation_df$Freq)
top_correlations <- head(correlation_df[order(-correlation_df$Abs_Freq), ], 10)
print(top_correlations)

# Categorical variable Plot
table(data$Formation)
ggplot(data, aes(x = Formation)) +
  geom_bar() +
  labs(title = "Frequency of Formation Categories", x = "Formation", y = "Frequency")


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






# Data Splitting
set.seed(200)
# Samples data in a 70/30 split
sample_index <- sample(1:nrow(data_cleaned), 0.7 * nrow(data_cleaned))
train_data <- data_cleaned[sample_index, ]
test_data <- data_cleaned[-sample_index, ]

# Linear Regression Model
linear_model <- lm(MUDLOSSU ~ ., data = train_data)
summary(linear_model)

# Backtracking to Get Ideal Model
m.be <- step(linear_model)

# Summary of Best Model
best_model <- lm(MUDLOSSU ~ Northing + Easting + `Depth (ft)` + Formation + 
                   `Fracture pressure` + METERAGE + DRLTIME + `Pump flow rate` + 
                   MFVIS + RETSOLID + FAN600 + FAN300 + RPM, data = train_data)


# Check VIF values
vif_values <- car::vif(best_model)
print(vif_values)


summary(best_model)

# Create diagnostic plots for the final linear model
par(mfrow = c(2, 2))
plot(best_model)
par(mfrow = c(1, 1))


# Model Evaluation
train_predictions_linear <- predict(best_model, newdata = train_data)
test_predictions_linear <- predict(best_model, newdata = test_data)

# Calculate RMSE 
train_rmse_linear <- sqrt(mean((train_predictions_linear - train_data$MUDLOSSU)^2))
test_rmse_linear <- sqrt(mean((test_predictions_linear - test_data$MUDLOSSU)^2))

# Create Results Table
results_table <- data.frame(
  Model = "Linear Regression",
  Train_RMSE = train_rmse_linear,
  Test_RMSE = test_rmse_linear
)

print(results_table)



##CART

set.seed(200)
cart1 <- rpart(MUDLOSSU ~ ., data = train_data, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))

# Print the complexity parameter table
printcp(cart1)

# Plot the complexity parameter table
plotcp(cart1)

# Determine the optimal complexity parameter (cp) based on cross-validation error
CVerror_cap <- cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xerror"] +
  cart1$cptable[which.min(cart1$cptable[, "xerror"]), "xstd"]

i <- 1
j <- 4

while (cart1$cptable[i, j] > CVerror_cap) {
  i <- i + 1
}

cp.opt <- ifelse(i > 1, sqrt(cart1$cptable[i, 1] * cart1$cptable[i - 1, 1]), 1)

# Prune the tree with the optimal CP value
cart2 <- prune(cart1, cp = cp.opt)
print(cart2)
printcp(cart2, digits = 3)

rpart.plot(cart2, nn = TRUE, main = "Optimal Tree in Your Dataset")


summary(cart2)

# Make predictions on both training and testing data using the pruned model
train_predictions_pruned_cart <- predict(cart2, newdata = train_data)
test_predictions_pruned_cart <- predict(cart2, newdata = test_data)

# Calculate RMSE for the pruned model on the training data
train_rmse_pruned_cart <- sqrt(mean((train_predictions_pruned_cart - train_data$MUDLOSSU)^2))

# Calculate RMSE for the pruned model on the testing data
test_rmse_pruned_cart <- sqrt(mean((test_predictions_pruned_cart - test_data$MUDLOSSU)^2))

# Create a table to compare RMSE values for the pruned CART model
pruned_cart_results_table <- data.frame(
  Model = "Pruned CART Regression",
  Train_RMSE = train_rmse_pruned_cart,
  Test_RMSE = test_rmse_pruned_cart
)

# Print the results table for the pruned CART model
print(pruned_cart_results_table)





