
# Name : Kavit Shah           #
# CWID : 10452991             #
# Course : CS513- B           #
# Purpose : HW_Midterm_Exam   #
# Email:kshah116@stevens.edu  #

# Question 6
#Load the CANVAS "COVID19_v3.CSV" dataset into R/Python. Remove the missing values. Discretize the "MonthAtHospital" into "less than 6 months" and "6 or more months".  Also discretize the age into "less than 35", "35 to 50" and "51 and over".  Construct a CART model to classify infection ("infected') based on the other variables. Measure the accuracy of the model.Do not use the original MonthAtHospital variable as a predictor.
rm(list= ls())

# Import all libraries after installing packages
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

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

#Train the model on the rpart library
cart_class<-rpart(Infected~.,data = training)

# Plot the trained class
rpart.plot(cart_class)

# Predict using the trained model based on class.
cart_predict2<-predict(cart_class,testing,type = 'class')

# Print the confusion matrix
table(Actual =testing[,7],CART = cart_predict2)

# Predict using the trained model based on probabilities.
cart_predict<-predict(cart_class,testing)
#cart_predict
str(cart_predict)
cart_predict_cat<-ifelse(cart_predict[,1]<=0.5,'Yes','No')
table(Actual=testing[,7],CART=cart_predict_cat)

#Calculate the error rate
cart_wrong<-sum(testing[,7]!=cart_predict_cat)
cart_error_rate<-cart_wrong/ length(testing[,7])

cart_predict2<-predict(cart_class,testing, type="class")
cart_wrong2<-sum(testing[,7]!=cart_predict2)
cart_error_rate2<-cart_wrong2/length(testing[,7])

#Plotting the decision tree
library(rpart.plot)
prp(cart_class)

# Fancier plot of Decison Tree
fancyRpartPlot(cart_class)

