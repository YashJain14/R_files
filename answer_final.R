# Load required libraries
library(dplyr)
library(caret)
library(rpart)

# Load your dataset (replace 'your_dataset.csv' with your actual dataset file)
data <- read.csv(file.choose())

# Check for missing values
missing_values <- data %>%
  summarise_all(~sum(is.na(.)))

# Print the results
print("Missing Values:")
print(missing_values)

# Handle missing values (for simplicity, impute with mean for numeric columns)
data_cleaned <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Check for missing values again
missing_values <- data_cleaned %>%
  summarise_all(~sum(is.na(.)))

# Print the results after handling missing values
print("\nMissing Values after Imputation:")
print(missing_values)

# Check for zeros in numeric columns (excluding categorical columns)
numeric_columns <- setdiff(names(data), c("Formation"))
zeros_as_placeholders <- data_cleaned %>%
  summarise_all(~sum(. == 0))

print("\nZeros as Placeholders:")
print(zeros_as_placeholders)

# Encode categorical variable (Formation) as a factor
data_cleaned$Formation <- as.factor(data_cleaned$Formation)

# Split the data into a training set (70%) and a testing set (30%)
set.seed(200)  # Set a seed for reproducibility
train_index <- createDataPartition(data_cleaned$MUDLOSSU, p = 0.7, list = FALSE)
train_data <- data_cleaned[train_index, ]
test_data <- data_cleaned[-train_index, ]

# Train Linear Regression model
lm_model <- lm(MUDLOSSU ~ ., data = train_data)

# Train CART model
cart_model <- rpart(MUDLOSSU ~ ., data = train_data, control = rpart.control(cp = 0.01))

# Make predictions on both training and testing data for both models
train_lm_predictions <- predict(lm_model, newdata = train_data)
test_lm_predictions <- predict(lm_model, newdata = test_data)

train_cart_predictions <- predict(cart_model, newdata = train_data)
test_cart_predictions <- predict(cart_model, newdata = test_data)

# Calculate RMSE for Linear Regression on both training and testing data
train_lm_rmse <- sqrt(mean((train_lm_predictions - train_data$MUDLOSSU)^2))
test_lm_rmse <- sqrt(mean((test_lm_predictions - test_data$MUDLOSSU)^2))

# Calculate RMSE for CART on both training and testing data
train_cart_rmse <- sqrt(mean((train_cart_predictions - train_data$MUDLOSSU)^2))
test_cart_rmse <- sqrt(mean((test_cart_predictions - test_data$MUDLOSSU)^2))

# Create a table to display the results
results <- data.frame(
  Model = c("Linear Regression", "CART"),
  Complexity = c(0, 0),  # Initialize complexity placeholders
  Trainset_RMSE = c(train_lm_rmse, train_cart_rmse),
  Testset_RMSE = c(test_lm_rmse, test_cart_rmse)
)

# Number of predictor variables in Linear Regression
num_predictors_lm <- length(coef(lm_model)) - 1  # Subtract 1 for the intercept term

# Number of terminal nodes in CART
num_terminal_nodes_cart <- sum(cart_model$splits$var == "") + 1  # Add 1 for the root node

# Update the results data frame with complexities
results$Complexity[results$Model == "Linear Regression"] <- num_predictors_lm
results$Complexity[results$Model == "CART"] <- num_terminal_nodes_cart

# Print the results table with complexities
print("Results Table with Complexities:")
print(results)


# Count the number of numeric predictors
num_numeric_predictors <- sum(sapply(train_data, is.numeric))

# Count the number of categorical predictors (assuming they were one-hot encoded)
num_categorical_predictors <- ncol(train_data) - num_numeric_predictors

cat("Complexity in Linear Regression Model:\n")
cat("Number of Numeric Predictors:", num_numeric_predictors, "\n")
cat("Number of Categorical Predictors:", num_categorical_predictors, "\n")

