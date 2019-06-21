# Course: CS 513 A
# Amruta Kulkarni
# Midterm Question 7

"Question: Cluster the twenty seven (27) food items in the accompanying table using R and 
Hierarchical (method=complete-linkage).What are the members at each clustering level?"

#Clear the environment
rm(list = ls())

# Save the food.csv file in Junk folder and read it in R environment
setwd("C:/Junk")
food_data <- read.csv("fooditems.csv", header = T, sep = ',')
View(food_data)

# Function for normalization
mmnorm <- function(x){
  z <- ( x - min(x) )/( max(x) - min(x) )
  return(z)
  
}

#Normalize the data, Remove the ID column since it is not needed for clustering the data.
normal_food <- cbind(
  Energy = mmnorm(food_data$Energy),
  Protein = mmnorm(food_data$Protein),
  Fat = mmnorm(food_data$Fat),
  Calcium = mmnorm(food_data$Calcium),
  Iron = mmnorm(food_data$Iron)
  
)

# Calculate the eucledean distance between all the data points in food_data
dfood <- dist(normal_food, method = 'euclidean', diag = F)
dfood

# Perform the clustering analysis using average method
clust <- hclust(dfood, method = 'complete', members = NULL)
plot(clust)

cutree(clust, k = 1:27)