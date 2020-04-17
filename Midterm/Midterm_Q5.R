# # # # # # # # # # # # # # # #
# Name : Kavit Shah           #
# CWID : 10452991             #
# Course : CS513- B           #
# Purpose : HW_Midterm_Exam   #
# Email:kshah116@stevens.edu  #
# # # # # # # # # # # # # # # #

# Question 5
library(e1071)
rm(list= ls())
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
covid19 = read.csv('COVID19_v3.csv')

is.na(covid19)
clean_covid19<-na.omit(covid19)

clean_covid19$Age<-cut(clean_covid19$Age , c(0,35,50,60))
clean_covid19$Age<- factor(clean_covid19$Age, levels = c("(0,35]","(35,50]","(50,60]") , labels = c("Less 35","35 to 50","More than 50"))

clean_covid19$MonthAtHospital<-cut(clean_covid19$MonthAtHospital, c(0,6,32))
clean_covid19$MonthAtHospital<- factor(clean_covid19$MonthAtHospital, levels = c("(0,6]","(6,32]") , labels = c("Less than 6 Months","6 months or more"))

index<-sort(sample(nrow(clean_covid19),round(0.30*nrow(clean_covid19))))
training<-clean_covid19[-index,]
testing<-clean_covid19[index,]

# Train the model with Naive Bayes Algorithm 
nbayes_all<-naiveBayes(Infected~.,data = training)

# Predict using the testing data
category_all<-predict(nbayes_all,testing)

table(NBAYES_ALL = category_all,Class = testing$Infected)

# Calculate Error Rate
NB_wrong<-sum(category_all!=testing$Infected)
NB_error_rate<-NB_wrong/length(category_all)

accuracy<- 1 - NB_error_rate
accuracy

