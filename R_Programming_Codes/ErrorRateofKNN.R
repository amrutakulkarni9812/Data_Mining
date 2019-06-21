# Course CS 513 A
# Amruta Kulkarni
# Midterm Exam Question 6

"Question: Use the Churn dataset and columns International Plan, Voice Plan, Day Minutes, Evening Minutes 
and Night Minutes and develop   knn and kknn  models; and measure the error rates 
for k=1, k=3 by selecting every fifth record, starting from record 1 as the test data
and the remaining records as training"

rm(list = ls())

# Read the churn data into R environment
churn <- read.csv("C:/Junk/churn_workbook.csv")
head(churn)

# Function for normalizing the data using min max normalization method:

mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

# Normalize the required 5 predictor variables and create a new data set using normalized variables
normalchurn <- cbind( IntPlan = as.factor(churn[,5]) 
                      ,VoiPlan = as.factor(churn[,6])
                     ,DayMin=mmnorm(churn[,8],min(churn[,8]),max(churn[,8] ))
                     ,EveMin=mmnorm(churn[,11],min(churn[,11]),max(churn[,11] ))
                     ,NigMin=mmnorm(churn[,14],min(churn[,14]), max(churn[,14]))
                     ,churner = as.factor(churn[,21]) )

# View few elements of the normalized churn data
head(normalchurn)
nrow(normalchurn)

# Split the normalchurn data into test data (every fifth row) and training data (remaining)
idx <- seq(1, nrow(normalchurn), 5)
test <- data.frame(normalchurn[idx,])
nrow(test)
training <- data.frame(normalchurn[-idx,])
nrow(training)
head(training)

# Separate the predictor and response variables of training and test data set
trainingx <- training[,1:5]
testx <- test[,1:5]
trainingy <- training[,6]
testy <- test[,6]
head(training)

# Build a knn model for k = 1
library(class)
knnout1 <- knn(trainingx, testx, trainingy, k = 1)
result1 <- cbind(testy, as.character(knnout1)) 

predict <- table(Predict = knnout1, Test = testy)
error <- result1[,1]!=result1[,2]
rate <- sum(error)/length(error)
rate
# Hence error rate for k = 1 is 15.74%

# Build a knn model for k = 3
library(class)
knnout3 <- knn(trainingx, testx, trainingy, k = 3)
result3 <- cbind(testy, as.character(knnout3)) 

predict <- table(Predict = knnout3, Test = testy)
error <- result3[,1]!=result3[,2]
rate <- sum(error)/length(error)
rate
#Error rate for k = 3 is 11.99%, therefore k = 3 gives more accurate classification

# Build a kknn model for k = 1
library(kknn)
kknnout1 <- kknn(formula = churner~., training, test, k = 1)
resultk1 <- cbind(testy, kknnout1$fitted.values) 

#predict <- table(Predict = kknnout1, Test = testy)
error <- resultk1[,1]!=resultk1[,2]
rate <- sum(error)/length(error)
rate
# Error Rate is 15.89%

# Build a kknn model for k = 3
library(kknn)
kknnout3 <- kknn(formula = churner~., training, test, k = 3)
resultk3 <- cbind(testy, kknnout3$fitted.values) 

#predict <- table(Predict = kknnout1, Test = testy)
error <- resultk3[,1]!=resultk3[,2]
rate <- sum(error)/length(error)
rate
# Error Rate is 28.03%,
"Hence k = 1 should be selected as error rate is less for it. i.e. with k = 1, 
less churners are misclassified"