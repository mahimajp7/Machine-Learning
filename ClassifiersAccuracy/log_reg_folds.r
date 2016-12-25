install.packages("stats")
library(stats)

nrFolds <- 10
ClassCol<-as.integer(35) #class column index
data1[,ClassCol]<-as.numeric(data1[,ClassCol])

for(i in 1:nrow(data1))
{
  if(data1[i,ClassCol]==1)
  {
    data1[i,ClassCol]<-0
  }
  else
  {  
    data1[i,ClassCol]<-1
  }
}

# generate array containing fold-number for each sample (row)
folds <- rep_len(1:nrFolds, nrow(data1))


# actual cross validation
for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  data.train <- data1[-fold,]
  ClassName<- names(data.train[ClassCol])
  f <- as.formula(paste(ClassName," ~."))
  data.test <- data1[fold,]
  Class1<-data.test[,ClassCol]
  
  # train and test your model with data.train and data.test
  
  model <- glm(f,family=binomial,data.train) #function
  pred<- predict.glm(model,data.test,type="response")
  pred<-round(pred,digits=0)
  data<-data.frame('predict'=pred, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(data.test)
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  method<-"Logistic Regression"
  accuracy<-avg
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
}

install.packages("ROCR")
library(ROCR)
pred <- prediction(Class1,pred)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T, main="ROC for Logistic Regression")
abline(0,1)
