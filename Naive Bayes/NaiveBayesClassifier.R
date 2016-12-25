
mynaivebayes <- function(trainPath, testPath)
{
  
#install.packages('e1071', dependencies = TRUE)
#install.packages('MASS', dependencies = TRUE)
#install.packages('rpart', dependencies = TRUE)
#install.packages('mlbench', dependencies = TRUE)
#install.packages('klaR', dependencies = TRUE)
#install.packages('tm', dependencies = TRUE )
#install.packages('NLP', dependencies = TRUE)
#install.packages('caret', dependencies = TRUE)
#install.packages('lattice', dpendencies = TRUE)
  #install.packages('COUNT', dpendencies = TRUE)
  #install.packages('plyr', dpendencies = TRUE)
#('ggplot2', dependencies = TRUE)
library(class)
library(e1071)
library(MASS)
library(mlbench)
library(klaR)
library(NLP)
library(tm)
library(rpart)
library(caret)
library(plyr)
#library(COUNT)

args <- commandArgs(trailingOnly=TRUE)

#testPath <- "C:\\Users\\lganesha\\Documents\\College\\ML\\Assignment3-me\\testcsv.csv"
#testPath <- args[2]
testDataTokens <-read.csv(testPath,header= TRUE,row.names=NULL)

#trainPath <- "C:\\Users\\lganesha\\Documents\\College\\ML\\Assignment3-me\\traincsv.csv"
#trainPath <- args[1]
dataText<-read.csv(trainPath, header= TRUE,row.names=NULL)
#inputfile <- args[1]
#dataset<-read.csv(inputfile,header = FALSE)
dataText$class<-as.factor(dataText$class)
testDataTokens$class<-as.factor(testDataTokens$class)
#train_count<-floor(0.9*nrow(dataset))
#train_arr<-sample(nrow(dataset),size=train_count)
#train_data<-dataset[train_arr,]
#test_data<-dataset[-train_arr,]
#train data

#partition train data
train_count<-floor(0.9*nrow(dataText))
train_arr<-sample(nrow(dataText),size=train_count)
dataText_data<-dataText[train_arr,]
dataText_test<-dataText[-train_arr,]

#create model
model <- naiveBayes(class ~ ., data = dataText_data)



NumberOfInstances<- nrow(dataText)
class0<- sum(dataText$class==0)
class1<-sum(dataText$class==1)
cat('P(Class=0)',class0/NumberOfInstances, sep='=')

for(col in colnames(dataText[,-7])){
  frequency<-count(dataText, col)
  classone=frequency[1,1]
  classonefreq=frequency[1,2]
  pone=round(classonefreq/class0, digits = 3)
  cat(paste(' P(',col,'=',classone,'|',0,') =',pone))
  
}
cat('\n')
cat(' P(Class=1)',class1/NumberOfInstances, sep='=')

for(col in colnames(dataText[,-7])){
  classtwo=frequency[2,1]
  classtwofreq=frequency[2,2]
  ptwo=round(classtwofreq/class1, digits = 3)
  cat(paste('P(',col,'=',classtwo,'|',1,') = ',ptwo))
}




#predict test part of train data
x_test <- dataText_test[,1:(ncol(dataText_test)-1)]
y_test <- dataText_test[,ncol(dataText_test)]
model_out<-predict(model,newdata=x_test)

#is.recursive('class')
cm <- confusionMatrix(model_out, y_test)
overall.accuracy <- cm$overall['Accuracy']
a2 <- overall.accuracy
#accuracy<-sum(diag(table))/sum(table)
#accuracyPercent<-accuracy*100
cat('\n')
cat(paste("The accuracy is:(10% of training instances) ",a2,"%",sep=""))



#predict test data
x1_test <- testDataTokens[,1:(ncol(testDataTokens)-1)]
x1_test
y1_test <- testDataTokens[,ncol(testDataTokens)]
model_out1<-predict(model,newdata=x1_test)

cm <- confusionMatrix(model_out1, y1_test)
overall.accuracy <- cm$overall['Accuracy']
a1 <- overall.accuracy
#accuracy<-sum(diag(table))/sum(table)
#accuracyPercent<-accuracy*100
cat('\n')
cat(paste("The accuracy is-all given instances on testcsv.csv, trained on 90% training instances: ",a1,"%",sep=""))

}
