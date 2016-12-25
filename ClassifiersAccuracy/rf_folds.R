install.packages("randomForest")
library(randomForest)


#data1<-read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data',header = FALSE)
ClassCol<-as.integer(35) #class column index
data1[,ClassCol]<-as.numeric(data1[,ClassCol])

nrFolds <- 10
data1[,35]<-as.numeric(data1[,35])

# generate array containing fold-number for each sample (row)
folds <- rep_len(1:nrFolds, nrow(data1))


for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  data.train <- data1[-fold,]
  data.test <- data1[fold,]
  
  Class<-data.train[,35]
  Class1<-data.test[,35]
  ClassName<- names(data.train[ClassCol])
  f <- as.formula(paste(ClassName," ~."))
  Class1<-data.test[,ClassCol]
  # which one is the class attribute
  # now create all the classifiers and output accuracy values:
  model <- randomForest(formula=f,data=data.train,ntree=100,mtry=10, 
                        keep.forest=TRUE, importance=TRUE,test=Class1) #function
  pred<-predict(model,data.test,type="class")
  pred<-round(pred)
  data<-data.frame('predict'=pred, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(data.test)
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  avg  #example of how to output
  method<-"Random Forest"
  
  accuracy<-avg
  cat("mtry=20")
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
}


install.packages("ROCR")
library(ROCR)
pred <- prediction(Class1,pred)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, main="ROC for Random Forest")
abline(0,1)
