# Name : Kavit Shah           #
# CWID : 10452991             #
# Course : CS513- B           #
# Purpose : HW_Midterm_Exam   #
# Email:kshah116@stevens.edu  #

#Question 7
#Load the CANVAS fictional "COVID19_v3.CSV" dataset into R/Python. Remove the missing values. Use unweighted knn(k=5) to  predict infection rate (infected) for a random sample (30%) of the data (test dataset).

# clearing previous data and windows
rm(list=ls())


#Import dataset
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
covid19<-read.csv("COVID19_v3.csv")
View(covid19)

#Cleaning the dataset
clean_covid19<-na.omit(covid19)

#Dividing the data
idx<-sample(nrow(clean_covid19),as.integer(.70*nrow(clean_covid19)))
training<-clean_covid19[idx,]
test<-clean_covid19[-idx,]

#Using KNN Algorithm
install.packages("kknn")
library(kknn)
library(class)


#for k = 5
predict_k5 <- kknn(formula = Infected~. , training, test, k =5, kernel = "rectangular")
fit <- fitted(predict_k5)
table(Actual=test$Infected, Fitted=fit)

#Accuracy of the model
cf_matrix<-table(Actual=test$Infected, Fitted=fit)
accuracy<-sum(diag(cf_matrix)/nrow(test))*100
accuracy

summary(clean_covid19)