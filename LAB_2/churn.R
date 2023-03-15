churn_data <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Print the first 5 rows of the dataset
head(churn_data)

# Get summary statistics for the numerical columns
summary(churn_data)

# View type of the dataset
str(churn_data)

# Look for missing value
sumMisVal = function(x){
  temp <- sum(is.na(x))
  return(temp)
}
sapply(X = churn_data,FUN = sumMisVal)

# Dropping unneeded 
churn_data <- subset(churn_data,select = -c(customerID))
churn_data <- subset(churn_data,select = -c(totalCharges))


# Create a histogram of customer tenure
hist(churn_data$tenure, main = "Customer Tenure", xlab = "Tenure (months)")

# Create a bar chart of contract types
barplot(table(churn_data$Contract), main = "Contract Types", xlab = "Contract", ylab = "Count")

# Create a boxplot of monthly charges by churn status
boxplot(churn_data$MonthlyCharges ~ churn_data$Churn, main = "Monthly Charges by Churn Status", xlab = "Churn Status", ylab = "Monthly Charges")

