setwd("~/百度云同步盘/513/project")
library(randomForest)
library(miscTools)
library(caret) # Accuracy
library(ROCR) #ROC curve
library(pROC)
library(e1071)
library(C50) #C5.0
library(rpart) #R part
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(grid)
library(corrplot)
library(plotly)
library(neuralnet)
# Load data
news=read.csv("OnlineNewsPopularity.csv")
summary(news)
head(news)
# Drop the line having unreasonable value
news=news[!news$n_unique_tokens==701,]
# Dataset for regression
# Exclude url and timedelta
newsreg <- subset( news, select = -c(url, timedelta ) )
names(newsreg)

# Normalize the data
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
                                return(z)}
for(i in ncol(newsreg)-1){ 
  newsreg[,i]<-mmnorm(newsreg[,i],min(newsreg[,i]),max(newsreg[,i]))
}

# generate z-scores using the scale() function
#for(i in ncol(newsreg)-1){ 
#  newsreg[,i]<-scale(newsreg[,i], center = TRUE, scale = TRUE)
#}
summary(newsreg)

# Dataset for classification
newscla <-newsreg
newscla$shares <- as.factor(ifelse(newscla$shares > 1400,1,0))

# Split data--------------------------------------------------
#set random situation
set.seed(100)
# Select traning data and prediction data
ind<-sample(2,nrow(newscla),replace=TRUE,prob=c(0.7,0.3))

# Color palatte------------------------------------------------
color.lr<-'#efab69'
color.knn<-'#ef696a'
color.cart<-'#ab69ef'
color.c50<-'#69adef'
color.rf<-'#adef69'
#color.lr<-rgba(239,171,105, 0.8)
#color.knn<-rgba(239,105,106, 0.8)
#color.cart<-rgba(171,105,239, 0.8)
#color.c50<-rgba(105,173,239, 0.8)
#color.rf<-rgba(173,239,105, 0.8)
####display.brewer.all()
####display.brewer.pal(5,"Set1")

# Correlation matrix---------------
newsreg.cor<-cor(newsreg[sapply(newsreg, is.numeric)])
## the following code from http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
newsreg.p<- cor.mtest(newsreg[sapply(newsreg, is.numeric)])
####head(p.mat[, 1:5])
oldpar<-par(family='Georgia')
par(pty="m",ps=11,mar=c(0,0,0,0),oma=c(1,1,1,1),family='Georgia')
corrplot(newsreg.cor, method="circle",type="upper", # shape
         col=brewer.pal(n=8, name="Spectral"), # color
         tl.col="steelblue4", tl.srt=60, # label color and size
         p.mat = newsreg.p, sig.level = 0.01, insig = "blank"# eliminate those with p-value larger than 0.01
         )

#----------------------------------------------------
# KNN
newscla.knn <- knn3(shares ~.,newscla[ind==1,])
newscla.knn.pred <- predict( newscla.knn,newscla[ind==2,],type="class")
newscla.knn.prob <- predict( newscla.knn,newscla[ind==2,],type="prob")
# Confusion matrix
confusionMatrix(newscla.knn.pred, newscla[ind==2,]$shares)
# ROC Curve
newscla.knn.roc <- roc(newscla[ind==2,]$shares,newscla.knn.prob[,2])
plot(newscla.knn.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col=color.knn, print.thres=TRUE)

#--------------------------------------------------------------
# CART
# method主要有 "anova", "poisson", "class"  "exp"。通常作生存分析选exp,因变量是因子变量选class,作poisson回归选poisson,其他情况通常选择anova
newscla.cart<-rpart(shares ~.,newscla[ind==1,],method='class')
# Plot tree
####plot(newscla.cart)
####text(newscla.cart)
prp(newscla.cart)
fancyRpartPlot(newscla.cart)

"Summary of CART"
summary(newscla.cart)
#predict
newscla.cart.pred<-predict( newscla.cart,newscla[ind==2,] ,type="class")
newscla.cart.prob<-predict( newscla.cart,newscla[ind==2,] ,type="prob")
# Confusion matrix
confusionMatrix(newscla.cart.pred, newscla[ind==2,]$shares)
# ROC Curve
newscla.cart.roc <- roc(newscla[ind==2,]$shares,newscla.cart.prob[,2])
plot(newscla.cart.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col=color.cart, print.thres=TRUE)

#----------------------------------------------------
# C5.0 classification
newscla.c50<-C5.0(shares ~.,newscla[ind==1,],trials=5)
"Summary of C5.0"
summary(newscla.c50)
#predict
newscla.c50.pred<-predict( newscla.c50,newscla[ind==2,],type="class" )
newscla.c50.prob<-predict( newscla.c50,newscla[ind==2,],type="prob" )
# Confusion matrix
confusionMatrix(newscla.c50.pred, newscla[ind==2,]$shares)


# ROC Curve
newscla.c50.roc <- roc(newscla[ind==2,]$shares,newscla.c50.prob[,2])
plot(newscla.c50.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col=color.c50, print.thres=TRUE)

# Precision/Recall graph
newscla.c50.pred.pred <- prediction(newscla.c50.prob[,2],newscla[ind==2,]$shares)
newscla.c50.pred.perf <- performance(newscla.c50.pred.pred,"prec","rec")
plot(newscla.c50.pred.perf, avg= "threshold", colorize=T, lwd= 3,
     main= "... Precision/Recall graphs ...")
plot(newscla.c50.pred.perf, lty=3, col="grey78", add=T)

#-------------------------------------------------
# Random forest classification
# Build model
####newscla.rf<-randomForest(shares ~.,newscla[ind==1,],ntree=100,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE)
newscla.rf<-randomForest(shares ~.,newscla[ind==1,],ntree=50,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE)
"Summary of Ramdom Forest"
summary(newscla.rf)
# Plot # of trees vs error
####plot(newscla.rf)
# Feature importance
newscla.rf.imp <- importance(newscla.rf)
newscla.rf.impvar <- newscla.rf.imp[order(newscla.rf.imp[, 3], decreasing=TRUE),]
# MeanDecreaseAccuracy and MeanDecreaseGini
# MeanDecreaseAccuracy衡量把一个变量的取值变为随机数，随机森林预测准确性的降低程度。该值越大表示该变量的重要性越大
# MeanDecreaseGini通过基尼（Gini）指数计算每个变量对分类树每个节点上观测值的异质性的影响，从而比较变量的重要性。该值越大表示该变量的重要性越大
newscla.rf.impvar
# Plot feature importance
varImpPlot(newscla.rf)
# Partial dependence
#### partialPlot(newscla.rf, newscla[ind==1,], kw_avg_avg, "0", main='' , xlab='kw_avg_avg', ylab="Variable effect")
#show the model
####print(newscla.rf)
#predict
newscla.rf.pred<-predict( newscla.rf,newscla[ind==2,], type="class")
newscla.rf.prob<-predict( newscla.rf,newscla[ind==2,], type="prob")
#show the prediction result compare to original
####table(observed=newscla[ind==2,"shares"],predicted=newscla.rf.pred )
# Confusion matrix
confusionMatrix(newscla.rf.pred, newscla[ind==2,]$shares)
# ROC Curve
newscla.rf.roc <- roc(newscla[ind==2,]$shares,newscla.rf.prob[,2])
plot(newscla.rf.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col=color.rf, print.thres=TRUE)
# Another way to plot ROC
####newscla.rf.prob.preds <- newscla.rf.prob[,2]
ROCCurve<-par(pty = "s",mfrow=c(1,1))
plot(performance(prediction(newscla.lr.prob[,2],newscla[ind==2,]$shares),'tpr','fpr'),
     col=color.lr, lwd=3
)
text(0.8,0.8,"Logistic Regression",col=color.lr)
#text(locator(1), labels = c("Logistic Regression"),col=color.lr, lwd=3)
plot(performance(prediction(newscla.knn.prob[,2],newscla[ind==2,]$shares),'tpr','fpr'),
     col=color.knn, lwd=3, add=TRUE
)
text(0.55,0.6,"KNN",col=color.knn)
#text(locator(1), labels = c("KNN"),col=color.knn, lwd=3)
plot(performance(prediction(newscla.cart.prob[,2],newscla[ind==2,]$shares),'tpr','fpr'),
     col=color.cart, lwd=3, add=TRUE
)
text(0.3,0.4,"CART",col=color.cart)
#text(locator(1), labels = c("CART"),col=color.cart, lwd=3)
plot(performance(prediction(newscla.c50.prob[,2],newscla[ind==2,]$shares),'tpr','fpr'),
     col=color.c50, lwd=3, add=TRUE
)
text(0.15,0.5,"C5.0",col=color.c50)
#text(locator(1), labels = c("C5.0"),col=color.c50, lwd=3)
plot(performance(prediction(newscla.rf.prob[,2],newscla[ind==2,]$shares),'tpr','fpr'),
     col=color.rf, lwd=3, add=TRUE
)
text(0.3,0.7,"Random Forest",col=color.rf)
#text(locator(1), labels = c("Random Forest"),col=color.rf, lwd=3)


# Clean everything-------------------------------
palette("default")
detach("package:RColorBrewer", unload=TRUE)
rm(list=ls())
