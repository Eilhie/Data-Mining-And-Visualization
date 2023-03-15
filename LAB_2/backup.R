# Encode the categorical variables
churn_data$gender <- ifelse(churn_data$gender == "Male", 1, 0)
churn_data$Partner <- ifelse(churn_data$Partner == "Yes", 1, 0)
churn_data$Dependents <- ifelse(churn_data$Dependents == "Yes", 1, 0)
churn_data$PhoneService <- ifelse(churn_data$PhoneService == "Yes", 1, 0)
churn_data$MultipleLines <- ifelse(churn_data$MultipleLines == "Yes", 1, 0)
churn_data$InternetService <- ifelse(churn_data$InternetService == "Fiber optic", 1, ifelse(churn_data$InternetService == "DSL", 2, 0))
churn_data$OnlineSecurity <- ifelse(churn_data$OnlineSecurity == "Yes", 1, 0)
churn_data$OnlineBackup <- ifelse(churn_data$OnlineBackup == "Yes", 1, 0)
churn_data$DeviceProtection <- ifelse(churn_data$DeviceProtection == "Yes", 1, 0)
churn_data$TechSupport <- ifelse(churn_data$TechSupport == "Yes", 1, 0)
churn_data$StreamingTV <- ifelse(churn_data$StreamingTV == "Yes", 1, 0)
churn_data$StreamingMovies <- ifelse(churn_data$StreamingMovies == "Yes", 1, 0)
churn_data$Contract <- ifelse(churn_data$Contract == "Month-to-month", 1, ifelse(churn_data$Contract == "One year", 2, 3))
churn_data$PaperlessBilling <- ifelse(churn_data$PaperlessBilling == "Yes", 1, 0)
churn_data$PaymentMethod <- ifelse(churn_data$PaymentMethod == "Electronic check", 1, ifelse(churn_data$PaymentMethod == "Mailed check", 2, ifelse(churn_data$PaymentMethod == "Bank transfer (automatic)", 3, 4)))
churn_data$Churn <- ifelse(churn_data$Churn == "Yes", 1, 0)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(nrow(churn_data), 0.7 * nrow(churn_data))
train_data <- churn_data[train_index, ]
test_data <- churn_data[-train_index, ]

# Fit a decision tree model
library(rpart)
tree_model <- rpart(Churn ~ ., data = train_data, method = "class")

# Make predictions on the testing data
predictions <- predict(tree_model, newdata = test_data, type = "class")

# Calculate accuracy on the testing data
accuracy <- sum(predictions == test_data$Churn) / nrow(test_data)
print(paste0("Accuracy: ", round(accuracy, 4)))