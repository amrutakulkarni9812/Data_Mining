# Predicting online News Popularity

rm(list = ls())

# Load the source file for 'norm' function ans set working directory to the folder where data is stored
source('C:/Stevens/KDDM/Project/Function.R')
setwd("C:/Stevens/KDDM/Project/")

# Use data.table library and 'fread' function to read the data file faster
library(data.table)
data <- fread("OnlineNewsPopularity.csv")
#head(data)
#ncol(data)

# Remove the first two columns from the original data set as they are non predictive
n<- subset(data, select = c(3:61))
#n <- n[-31038,]

# Use summary function on all columns to check extreme values 
summaries <- apply(n, 2, FUN = summary)

# Use boxplot to visualize the outliers in the columns
par(mfrow = c(1, 2))
boxplot(n$n_unique_tokens, xlab = ("Rate of Unique words"))
boxplot(n$n_non_stop_words, xlab = ("Rate of Non-Stop words"))
boxplot(n$n_tokens_content, xlab = ("No of words in content"))