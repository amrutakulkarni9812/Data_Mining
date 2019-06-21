# Predicting online News Popularity
# Set working directory to the folder where data is stored
setwd("C:/Stevens/KDDM/Project/")

# Use data.table library and 'fread' function to read the data file faster
library(data.table)
data <- fread("OnlineNewsPopularity.csv")
head(data)
ncol(data)

# Remove the first two columns from the original data set as they are non predictive
news_data <- subset(data, select = c(3:61))
head(news_data)
ncol(news_data)

# Separate the binary variables before normalizing
news_binary <- subset(news_data, select = c(12:17, 30:37))
head(news_binary)

news <- subset(news_data, select = c(1:11, 18:29, 38:59))
ncol(news)
head(news)

# Normalize all the columns of the news data set
norm <- function(x){
  z <- (x - min(x))/(max(x) - min(x))
  return(z)
}

norm_data <- as.data.frame(lapply(news, norm))
head(norm_data)
ncol(norm_data)
nrow(norm_data)

# Get the final data
final_data <- cbind(norm_data, news_binary)
head(final_data)
ncol(final_data)
nrow(final_data)
