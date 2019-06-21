# Predicting online News Popularity

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
#head(news_data)
ncol(news_data)

# Normalize all the columns of the news data set
norm_data <- as.data.frame(lapply(news_data, norm))
head(norm_data)
#ncol(norm_data)
#nrow(norm_data)

reg <- lm(shares~., data = norm_data)
summary(reg)


# To edit the columns 
news_data$data_channel<- ifelse(news_data$data_channel_is_lifestyle ==1, "1",
                         ifelse(news_data$data_channel_is_entertainment==1, "2",
                         ifelse(news_data$data_channel_is_bus==1, "3",
                         ifelse(news_data$data_channel_is_socmed==1, "4", 
                         ifelse(news_data$data_channel_is_tech==1, "5",
                         ifelse(news_data$data_channel_is_world==1, "6",
                                                                  NA  ))))))
news <- subset(news_data, select = c(1:11, 18:59))
head(news)