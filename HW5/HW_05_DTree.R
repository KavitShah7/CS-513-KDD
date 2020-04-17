#  First Name       : Kavit
#  Last Name        : Shah  
#  CWID             : 10452991
#  Purpose          : Homework 5 - Decision Tree

# clearing previous data and windows
rm(list=ls())


#Import dataset
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
Cancer<-read.csv("breast-cancer-wisconsin.data.csv")
View(Cancer)
Cancer$Class<-factor(Cancer$Class, level=c(2,4),labels=c("Benign ","Malignant"))

install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

?set.seed

#sorting the dataset and training and testing them
set.seed(111)
index<-sort(sample(nrow(Cancer),round(.25*nrow(Cancer))))
training<-Cancer[-index,]
test<-Cancer[index,]

?rpart()
dev.off()

# using the CART methodology 
CART_class<-rpart(Class~.,data=training)
rpart.plot(CART_class)
CART_predict<-predict(CART_class,test, type="class") 
table(Actual=test[,11],CART=CART_predict2)
CART_predict2<-predict(CART_class,test) 


CART_predict<-predict(CART_class,test)
str(CART_predict)
CART_predict_cat<-ifelse(CART_predict[,1]<=.5,"Malignant","Benign")


table(Actual=test[,11],CART=CART_predict_cat)
##Cart
##Actual      Benign Malignant
##Benign       104         6
##Malignant      3        62

CART_wrong<-sum(test[,11]!=CART_predict_cat)
###### 9

### Finding the error rate in the model
CART_error_rate<-CART_wrong/length(test[,11])
CART_error_rate
CART_predict2<-predict(CART_class,test, type="class")
CART_wrong2<-sum(test[,11]!=CART_predict2)
CART_error_rate2<-CART_wrong2/length(test[,11])
CART_error_rate2 

library(rpart.plot)
prp(CART_class)


# much fancier graph
fancyRpartPlot(CART_class)


