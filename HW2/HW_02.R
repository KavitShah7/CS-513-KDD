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


#1. Summarizing each column

summary(cancer)



#2. Identifying missing values

cancer[cancer=="?"]<-NA
is.na(cancer)


#3. Replacing the missing values wiht the mean of the column

sapply(cancer, class)
cancer<-transform(cancer,F6=as.integer(F6))
cancer$F6[is.na(cancer$F6)]<-mean(cancer$F6, na.rm=TRUE)
View(cancer)



#4. Displaying the frequency table of "Class" vs F6
library(plyr)
y<-data.frame(class=cancer$Class, F6=cancer$F6)
x<-count(y)
x



#5. Displaying the scatter plot of F1 to F6, one pair at a time

pairs(cancer[2:7],main = "Breast Cancer Wisconsin Data -- 2 Classes",
      pch = 21,bg =c("red","blue")[factor(cancer$Class)])
plot(cancer[2:7])




#6. Show histogram boxplot for columns F7 to F9

hist(cancer$F7, main="Histogram",xlab="column F7")
hist(cancer$F8, main="Histogram",xlab="Column F8")
hist(cancer$F9, main="Histogram",xlab="Column F9")
boxplot(cancer[8:10])

