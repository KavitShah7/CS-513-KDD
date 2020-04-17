#Course: CS 513 Knowledge Discovery and Data Mining
#First name: Kavit
#Last name: Shah
#Purpose: HW03 KNN Algorithm
#CWID: 10452991
#Email: kshah116@stevens.edu
#Date: 03/08/2020


# clearing previous data and windows
rm(list=ls())


#Import dataset
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
cancer<-read.csv("breast-cancer-wisconsin.data.csv")
View(cancer)

sapply(cancer_clean, class)

#Cleaning the Dataset
cancer[cancer=="?"]<-NA
cancer_clean<-na.omit(cancer)

#View Cleaned Cancer dataset
View(cancer_clean)

#Change the datatype of the categories
cancer_clean<-transform(cancer_clean,F1=as.factor(F1),F2=as.factor(F2),F3=as.factor(F3),F4=as.factor(F4),F5=as.factor(F5),F6=as.factor(F6),F7=as.factor(F7),F8=as.factor(F8),F9=as.factor(F9),Class=as.factor(Class))


#Dividing the data
idx<-sample(nrow(cancer_clean),as.integer(.70*nrow(cancer_clean)))
training<-cancer_clean[idx,]
test<-cancer_clean[-idx,]

#Using KNN Algorithm
install.packages("kknn")
library(kknn)
library(class)


#for k = 3
predict_k3 <- kknn(formula = Class~. , training, test, k =3, kernel = "rectangular")
fit <- fitted(predict_k3)
table(Actual=test$Class, Fitted=fit)


#for k = 5
predict_k5 <- kknn(formula = Class~. , training, test, k =5, kernel = "rectangular")
fit <- fitted(predict_k5)
table(Actual=test$Class, Fitted=fit)


#for k = 10
predict_k10 <- kknn(formula = Class~. , training, test, k =10, kernel = "rectangular")
fit <- fitted(predict_k10)
table(Actual=test$Class,Fitted=fit)

summary(cancer_clean)
