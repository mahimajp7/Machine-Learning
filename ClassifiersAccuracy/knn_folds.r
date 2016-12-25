install.packages("class")
library(class)

nrFolds <- 10
data1[,35]<-as.numeric(data1[,35])

# generate array containing fold-number for each sample (row)
folds <- rep_len(1:nrFolds, nrow(data1))

# actual cross validation
for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  data.train <- data1[-fold,]
  data.test <- data1[fold,]
  
  Class<-data.train[,35]
  Class1<-data.test[,35]
  # train and test model with data.train and data.test
  
  pred<-knn(data.train, data.test, Class, k = 9, l = 0, prob = FALSE, use.all = TRUE)
  data<-data.frame('predict'=pred, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(data.test)
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  method<-"KNN" 
  accuracy<-avg
  
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

# draw ROC curve
install.packages("ROCR")
library(ROCR)
pred <- prediction(Class1,pred)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, main="ROC for kNN")
abline(0,1)
