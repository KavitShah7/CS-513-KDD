#First Name:Kavit
#Last Name: Shah
#CWID: 10452991
#Assignment 7 - CS 513

#Clear the previous variables
rm(list=ls())
library(randomForest)

#Change the Directory and import the dataset
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
Cancer_data <- read.csv("breast-cancer-wisconsin.data.csv",na.strings = '?')
View(Cancer_data)


table(Cancer_data$Class)


#To factor the data set
Cancer_data$Class <- factor(Cancer_data$Class, levels = c(2,4),labels = c("Benign", "Malignant"))

# To split the data set into test and testing 
idx<-sort(sample(nrow(Cancer_data),as.integer(.70*nrow(Cancer_data))))
training<-Cancer_data[idx,]
test<-Cancer_data[-idx,]

#randomForest
randomForest_class<-C5.0(Class~.,data=training)
summary(randomForest_class)
plot(randomForest_class)
randomForest_predict<-predict(randomForest_class,test,type="class")
table(actual=test[,11],randomForest=randomForest_predict)

#Error rate
Cart_wrong<-sum(test[,11]!=randomForest_predict)
randomForest_error_rate<-Cart_wrong/length(test[,11])
randomForest_error_rate

