# Course CS 513 A
# Amruta Kulkarni
# Midterm Question 3a


"Question 3a.
Company XYZ is targeting professionals between the ages of 20 to 70 years old with an asset size of 50K to 150K.  To estimate the missing income fields, the company is using k-nearest neighbors. 
What would be the value of income for customer x in the table below if:
K = 1 and method = un-weighted vote is used
K = 2 and method = un-weighted vote is used
K = 3 and method = distance weighted vote is used?"


# Clear the environment
rm(list = ls())

# Create a data frame from given data with Id, Age, AssetSize and Income columns
my_data <- data.frame(ID = c(0,1,2,3), Age = c(30,25,33,35), 
                      AssetSize =c(60,50,60,80), Income = c(NA, 100, 90, 150))

# Function to normalize the data using min max normalization
mmnorm <- function(x,minx, maxx){
      z <- (x - minx)/(maxx - minx)
      return(z)
}  

# Normalize the required predictor variables Age and AssetSize
age <- mmnorm(my_data$Age, 20, 70)
assetsize <- mmnorm(my_data$AssetSize, 50, 150)
income <- my_data$Income

# Create a new data set new_data with normalized predictor variables
new_data <- data.frame(age, assetsize, income)

# Divide the data set into test data and training data
# Training data will have 3 complete rows and test data will have the one row where Income value is missing
# Also split the predictor variables 'Age, AssetSize' from the response variable'Income'

training <-new_data[2:4,1:2]
test <- new_data[1,1:2]
outcome <- new_data[2:4,3]
testy <- new_data[1,3]
train <- new_data[2:4,]
testing <- new_data[1,]

# Load the libraries for un-weighted and weighted knn
library(class)
library(kknn)

# Perform the un-weighted knn on the training data with k = 1
knnout1 <- knn(training, test, outcome, k = 1)
# Predict the value of missing Income in test data using the knn classification done above
result1 <- cbind(testy, as.character(knnout1))
result1[,2]
# Result: For un-weighted knn method and k = 1, Income of customer x is 90k

# Perform the un-weighted knn on the training data with k = 2
knnout2 <- knn(training, test, outcome, k = 2)
# Predict the value of missing Income in test data using the knn classification done above
result2 <- cbind(testy, as.character(knnout2))
result2[,2]
# Result: Since we can get the income of any of the two nearest neighbours randomly as the result, 
# we can consider the average of both incomes i.e.95k

# Perform weighted knn on the training data with k = 3
knnoutw3 <- kknn(formula = income~., train, testing, k = 3)
# Predict the value of missing Income in test data using weighted distance vote i.e. kknn
resultw3 <- cbind(testy, knnoutw3$fitted.values)
resultw3
# For weighted knn method and k = 3, Income of customer x is 94.44k
