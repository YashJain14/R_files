# Load the necessary libraries
library(caret)
library(dplyr)

# Read the data from a file (replace 'data.csv' with the actual file path)
data <- read.csv(file.choose())

# Handle missing values (if necessary)
data <- na.omit(data)

# Split the data into a 70-30 train-test set
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(data$MUDLOSSU, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Perform Linear Regression
model <- lm(MUDLOSSU ~ ., data = trainData)

# Predict on the test set
predictions <- predict(model, newdata = testData)

# Calculate RMSE for both train and test sets
trainRMSE <- sqrt(mean((trainData$MUDLOSSU - predict(model, newdata = trainData))^2))
testRMSE <- sqrt(mean((testData$MUDLOSSU - predictions)^2))

# Display results in a table
results <- data.frame(
  Model = "Linear Regression",
  Complexity = length(coefficients(model)), # Number of coefficients
  Trainset_RMSE = trainRMSE,
  Testset_RMSE = testRMSE
)

print(results)


#Model Complexity Trainset_RMSE Testset_RMSE
#1 Linear Regression         20      124.3101     120.5492
