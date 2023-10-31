# Load the necessary libraries
library(caret)
library(rpart)

# Read the data from a file (replace 'data.csv' with the actual file path)
data <- read.csv(file.choose())

# Handle missing values (if necessary)
data <- na.omit(data)

# Split the data into a 70-30 train-test set
set.seed(200) # For reproducibility
trainIndex <- createDataPartition(data$MUDLOSSU, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Perform CART analysis
model <- rpart(MUDLOSSU ~ ., data = trainData, method = "anova")

# Calculate the complexity of the CART model (number of terminal nodes)
complexity <- nrow(model$splits)

# Predict on the test set
predictions <- predict(model, newdata = testData)

# Calculate RMSE for both train and test sets
trainRMSE <- sqrt(mean((trainData$MUDLOSSU - predict(model, newdata = trainData))^2))
testRMSE <- sqrt(mean((testData$MUDLOSSU - predictions)^2))

# Display results in a table
results <- data.frame(
  Model = "CART",
  Complexity = complexity,
  Trainset_RMSE = trainRMSE,
  Testset_RMSE = testRMSE
)

print(results)


#Model Complexity Trainset_RMSE Testset_RMSE
#2  CART        105      103.7854     109.4273
