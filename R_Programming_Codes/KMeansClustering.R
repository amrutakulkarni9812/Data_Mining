# Course : Knowledge Discovery and Data Mining 
# First name: Amruta
# Last Name: Kulkarni
# Purpose: K Means Clustering

# Clear all objects from environment

rm(list = ls())

# Read the help.csv file from web

helpdata <- read.csv("http://www.math.smith.edu/sasr/datasets/help.csv")
View(helpdata)

# Function for normalization

norm <- function(x)
  {
  z <- (x - min(x))/(max(x) - min(x))
  return(z)
  }

# Get only the required columns i.e. age, substance ane gender

helpdata_age <- helpdata[,"age"]
helpdata_age <- norm(helpdata_age)
helpdata_substance <- as.factor(helpdata[,'substance'])
helpdata_gender <- as.factor(helpdata[,"female"])

# Create a new dataset with the normalized and required columns

norm_helpdata <- cbind(helpdata_age,helpdata_substance,helpdata_gender)
View(norm_helpdata)

# Use kmeans and plotting them

clust <- kmeans(norm_helpdata,centers = 3)
clust
clust$cluster
clust$centers
plot(norm_helpdata, col = clust$cluster)
points(clust$centers,col = 1:3, pch = 17, cex = 1.5)


