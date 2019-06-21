# Course CS 513 A 
# Final Project: Predicting online News Popularity
# Code for KNN 

rm(list = ls())

# Load the source file for 'norm' function ans set working directory to the folder where data is stored
source('C:/Stevens/KDDM/Project/Function.R')
setwd("C:/Stevens/KDDM/Project/")

# Use data.table library and 'fread' function to read the data file faster
library(data.table)
data <- fread("OnlineNewsPopularity.csv")
head(data)
ncol(data)

# Remove the first two columns from the original data set as they are non predictive
news_data <- subset(data, select = c(3:61))
news_data <- news_data[-31038,]
head(news_data)
ncol(news_data)

# Normalize all the columns of the news data set
norm_data <- as.data.frame(lapply(news_data, norm))
head(norm_data)
ncol(norm_data)
nrow(norm_data)

# Split the normalized data set into training (70%) and testing (30%)
idx <- sample(nrow(norm_data), as.integer(.70 * nrow(norm_data)))
training <- norm_data[idx,]
testing <- norm_data[-idx,]

# Split the training data set into predictor and outcome variables
train <- training[,-59]
train_outcome <- training[,59]
# Make the train_outcome variable categorical  
train_outcome <- ifelse(train_outcome>0.001658961, "popular","not-popular")

# Split the training data set into predictor and outcome variables
test <- testing[,-59]
test_outcome <- testing[,59]
# Make the test_outcome variable categorical 
test_outcome <- ifelse(test_outcome>0.001658961, "popular","not-popular")
length(test_outcome)

# Find the optimal value of K using library 'caret'
# Use 'library doParallel' to run the code parallely and get the results faster
library(doParallel)
registerDoParallel()
library('caret')
library(e1071)

model <- train(train, train_outcome,
               method='knn',
               tuneGrid=expand.grid(.k=1:30),
               metric='Accuracy',
               trControl=trainControl(
                 method= 'cv'))
plot(model)

# Perform knn on the normalized news data set
library(class)

classknn <- knn(train, test, train_outcome, k = 26)
# Compare the result of knn and test outcome
result <- cbind(test_outcome, as.character(classknn))

#ncol(result)
# Check the accuracy of the model for given value of k
correct <- result[,1]==result[,2]
accuracy <- sum(correct)/length(test_outcome)
accuracy
plot(classknn)
# Cross check the predicted values with the actual values in test data
table(Predict = classknn, Test = test_outcome)





