# Course CS 513 A 
# Final Project: Predicting online News Popularity
# Code for Neural Networks 

rm(list = ls())

# Load the source file for 'norm' function ans set working directory to the folder where data is stored
source('C:/Stevens/KDDM/Project/Function.R')
setwd("C:/Stevens/KDDM/Project/")

# Use data.table library and 'fread' function to read the data file faster
library(data.table)
data <- fread("OnlineNewsPopularity.csv")
#head(data)
ncol(data)

# Remove the first two columns from the original data set as they are non predictive
news_data <- subset(data, select = c(3:61))
news_data <- news_data[-31038,]
#head(news_data)
#ncol(news_data)

# Normalize all the columns of the news data set
norm_data <- as.data.frame(lapply(news_data, norm))
#head(norm_data)
#ncol(norm_data)
#nrow(norm_data)

# Split the normalized data set into training (70%) and testing (30%)
idx <- sample(nrow(norm_data), as.integer(.70 * nrow(norm_data)))
training <- norm_data[idx,]
testing <- norm_data[-idx,]


# Convert output to numeric since ann works only with quantitative data and not factors
training$shares <- ifelse(training$shares > 0.001658961, 1,0) 
testing$shares <- ifelse(testing$shares > 0.001658961, 1,0)
head(training$shares)
head(testing$shares)

library(doParallel)
registerDoParallel()
library(neuralnet)
n <- names(training)
f <- as.formula(paste("shares ~", paste(n[!n %in% "shares"], collapse = " + ")))

ann <- neuralnet(f, training, hidden = 3, threshold = 0.01)
plot(ann)


nn <- ifelse(ann$net.result[[1]]>0.5,1,2)
nn
misclass <- mean(training$shares != nn)
misclass


# Test the resulting output on test data set
tmp_testing <- testing[,-59]
#head(tmp_testing)
# Compute function is used to compute the output based on input variables in the test data and trained neural n/w  
result <- compute(ann, tmp_testing)

# Check the resultd produced by neural network
popular_result <- data.frame(actual = testing$shares, prediction = result$net.result)

# Round the result to compare the popularity
popular_result <- round(popular_result)
correct <- popular_result[,1] == popular_result[,2]
sum(correct) / nrow(testing)

