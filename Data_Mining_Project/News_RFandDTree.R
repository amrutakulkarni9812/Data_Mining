newsData<-read.csv(file='/Users/somayah/Desktop/Data Mining/OnlineNewsPopularity.csv')
View(newsData)
newsData2<-newsData
# replace Shares humber by labels " popular" and "not popular"
newsData2[,"shares"]<-ifelse(newsData[,"shares"]>1400,"Popular","notpopular")
View(newsData2)
# extract the  usable variabels
newsData2<-newsData2[3:61]
# remove the outlier
newdData2<-newsData2[-31038,]
# Load the required library

library(C50)
library(randomForest)
#--------------------------------------------------------------
# Split the data into 70% training and 30% testing 
idx <- sample(nrow(newsData2), as.integer(.70 * nrow(newsData2)))
test<-newsData2[-idx,]
training<-newsData2[idx,]
trainset<-training

# covert shares variabels into factor in order to be used in RF
trainset$shares<-as.factor(trainset$shares)
head(trainset)
# sqrt(nrow(trainset))  Gives the best value for K 
#------------------------------------- RF---------------------------------------------
tree.data<-randomForest(shares~., data=trainset, keep.forest=TRUE,importance=TRUE, proximity=TRUE )
summary(tree.data)
#------------------- Using the resulted RF to predict the testing data----------
resultRF<- predict(tree.data, test, type="class")
#--------------- Getting the accuarcy------------
rFTable<-table(Predict=resultRF, Test=test[,59])
accuracy=(sum(diag(rFTable))/nrow(test))*100
accuracy 
#------Getting the important varaibels and order them ------------------
imVar<-round(importance(tree.data),2)
varImpPlot(tree.data, class="Popular", main="Class= Popular Importance")
varImpPlot(tree.data, class="notpopular",main="Class= Not Popular Importance" )
import<-data.frame( importance= imVar[,1:4])
import<-import[order(-import[,4]),]
View(import)
View(imVar)
write.csv(import, file="RFImportance3.csv")
varImp(tree.data)
#------------------------------
# ------tuning RF mtry parameter----
rf<-tuneRF(trainset[,-59], trainset$shares,ntreeTry=200 ,stepFactor = 1.5)
tree.data$err.rate
tree.data$mtry
tree.data$ntree
varImpPlot(tree.data, main="Importance of Variables")
plot(tree.data, log="y")
plot(margin(tree.data))
varImpPlot(tree.data)

getTree(tree.data, 1)
print(tree.data)
varImp(tree.data)
#-------------C5.0------------------
idx <- sample(nrow(newsData2), as.integer(.70 * nrow(newsData2)))
test<-newsData2[-idx,]
training<-newsData2[idx,]
treeModel<-C5.0 (x= training[,-59], y=as.factor(training$shares), rules=TRUE)
#treeModel
summary(treeModel)
result<- predict(treeModel, test, type="class")
rTable<-table(Predict=result, Test=test[,59])
accuracy=(sum(diag(rTable))/nrow(test))*100
accuracy

#----------------------C5.0 on the reduced dataset -------------
reduced.data<-read.csv("NewsReducedData.csv")
View(reduced.data)
reduced.data<-reduced.data[,-1]
idx <- sample(nrow(reduced.data), as.integer(.70 * nrow(reduced.data)))
test2<-reduced.data[-idx,]
training2<-reduced.data[idx,]
#treeModel<- C5.0(shares ~ ., data=training, rules=TRUE)
treeModel2<-C5.0 (x= training2[,-42], y=as.factor(training2$shares), rules=TRUE)
#treeModel
summary(treeModel2)
result2<- predict(treeModel2, test2, type="class")
rTable2<-table(Predict=result2, Test=test2[,42])
accuracy=(sum(diag(rTable2))/nrow(test2))*100
accuracy

