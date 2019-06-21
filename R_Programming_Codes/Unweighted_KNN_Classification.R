# Knowledge Discovery & Data Mining
# FIrst Name: Amruta
# Last Name: Kulkarni
# Purpose: Unweighted K Nearest Neighbour classification

# TO clean the memory
rm(list = ls())

# Load the IRIS dataset into memory
data(iris)

# Create a test data set by extracting every fifth row of the data, starting with the first row.
# Create idx dataset for every fifth row in the iris data set
idx <-seq(1, nrow(iris), by = 5)

# Create the test data having every fifth row from the iris data set
testdata <- iris[idx,]

# Create a training data set by excluding the testdata from iris dataset
trainingdata <- iris[-idx,]

# Use the training dataset to classify the test data using knn with k=1, k=2, k=5, k=30
# First let's split the variables in training data set into predictor and response variables
trainingx <- trainingdata[,-5]
trainingy <- trainingdata[,5]

# Now let's split the variables in test data set into predictor and response variables
testx <- testdata[,-5]
testy <- testdata[,5]

# Now let's create the classification  model using training data set and various values of k
# For k = 1
library(class)
predict1 <- knn(trainingx, testx, trainingy, k = 1)
table(Predict = predict1, Test = testy)

# TO verify the predicted classification and actual classification
result <- cbind(testy, as.character(predict1))

# Result: For k = 1, one iris was classified as virginica but it was in fact versicolor. 

# For k = 2
predict2 <- knn(trainingx, testx, trainingy, k = 2)
table(Predict = predict2, Test = testy)
"Result: For k = 2, two iris were misclassified, one was predicted virginica 
but was versicolor and one was classified versicolor but was virginica"

# For k = 5
predict5 <- knn(trainingx, testx, trainingy, k = 5)
table(Predict = predict5, Test = testy)
# Result: For k = 5, one iris was classified as virginica but it was in fact versicolor.

# For k = 10
predict10 <- knn(trainingx, testx, trainingy, k = 10)
table(Predict = predict10, Test = testy)
# Result: For k = 10, all the iris are classified in correct groups. Hence best value for k = 10


