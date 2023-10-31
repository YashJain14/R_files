# Load required libraries
library(dplyr)
library(caret)
library(rpart)

# Load the dataset
data <- read.csv(file.choose())

# Check for missing values
missing_values <- data %>%
  summarise_all(~sum(is.na(.)))



# Print the results
cat("Missing Values:\n")
print(missing_values)


# Handle missing values (impute with mean for numeric columns)
data_cleaned <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) 


# Check for zeros in numeric columns (excluding categorical columns)
numeric_columns <- setdiff(names(data_cleaned), c("Formation"))
zeros_as_placeholders <- data_cleaned %>%
  summarise_all(~sum(. == 0))

cat("\nZeros as Placeholders:\n")
print(zeros_as_placeholders)

# Encode categorical variables (one-hot encoding)
data_cleaned <- data_cleaned %>%
  select(-Formation) %>%
  model.matrix(~ ., data = .) %>%
  as.data.frame()

# Verify that the data is now clean
cat("\nCleaned Data Summary:\n")
summary(data_cleaned)

# Split the data into a training set (70%) and a testing set (30%)
set.seed(200)  # Set a seed for reproducibility
train_index <- createDataPartition(data_cleaned$MUDLOSSU, p = 0.7, list = FALSE)
train_data <- data_cleaned[train_index, ]
test_data <- data_cleaned[-train_index, ]

# Build a linear regression model
lm_model <- lm(MUDLOSSU ~ ., data = train_data)

# Build a CART model
cart_model <- rpart(MUDLOSSU ~ ., data = train_data, control = rpart.control(cp = 0.01))

# Calculate the complexity for Linear Regression (number of X variables by type)
lm_complexity <- length(coef(lm_model))

# Calculate the complexity for CART (number of terminal nodes)
cart_complexity <- sum(cart_model$frame$var == "<leaf>")

# Print the complexities
cat("\nComplexity for Linear Regression (Number of X Variables by Type):", lm_complexity, "\n")
cat("Complexity for CART (Number of Terminal Nodes):", cart_complexity, "\n")
