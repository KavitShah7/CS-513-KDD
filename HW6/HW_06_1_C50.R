#  First Name       : Kavit
#  Last Name        : Shah 
#  CWID             : 10452991
#  Purpose          : Homework 6 - C50

rm(list=ls())

setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
Cancer_data<-read.csv("breast-cancer-wisconsin.data.csv",na.strings = "0")
View(cancer)

#Installing C50 packages
install.packages("C50")
library(C50)

#Tranforming all the columns into Factor Dataset
Cancer_data<-transform(Cancer_data,F1=as.factor(F1),F2=as.factor(F2),F3=as.factor(F3),F4=as.factor(F4),F5=as.factor(F5),F6=as.factor(F6),F7=as.factor(F7),F8=as.factor(F8),F9=as.factor(F9))

Cancer_data$Class <- factor(Cancer_data$Class,levels=c(2,4),labels=c("Benign","Malignant"))

#Dividing into training and testing data
index<-sort(sample(nrow(Cancer_data),round(.25*nrow(Cancer_data))))
training<-Cancer_data[-index,]
test<-Cancer_data[index,]

#C50
C50_class<-C5.0(Class~.,data=training)
summary(C50_class)
plot(C50_class)
C50_predict<-predict(C50_class,test,type="class")
table(actual=test[,11],C50=C50_predict)

#Error rate
Cart_wrong<-sum(test[,11]!=C50_predict)
C50_error_rate<-Cart_wrong/length(test[,11])
C50_error_rate
# 0.03428571

