#  First Name       : Kavit
#  Last Name        : Shah  
#  CWID             : 10452991
#  Purpose          : Homework 4 - Naive Bayes


# Clear the directories and the global environment
rm(list=ls())

#Import Directory
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
cancer<-read.csv("breast-cancer-wisconsin.data.csv")
View(cancer)

#installing the packages
install.packages("e1071", dependencies= TRUE)
install.packages(class)
library(e1071)
library(class)

#remove missing 
cancer[cancer=="?"]<-NA
cancer_missing <- na.omit(cancer)
is.na(cancer_missing)

cancer_missing$Class<- factor(cancer_missing$Class , levels = c("2","4") , labels = c("Benign","Malignant"))
is.factor(cancer_missing$Class)

str(cancer_missing)
cancer_missing <- cancer_missing[2:11]
View(cancer_missing)


#split data for testing and training
idx<-sort(sample(nrow(cancer_missing),as.integer(.70*nrow(cancer_missing))))

training<-cancer_missing[idx,]
test <-cancer_missing[-idx,]


#Implementing NaiveBayes
model_naive<- naiveBayes(Class ~ ., data = training)

predict_naive <- predict(model_naive, test)
table(model_naive=predict_naive,class=test$Class)

prop.table(table(model_naive=predict_naive,class=test$Class))

#Error rate
wrong_prediction<-sum(predict_naive!=test$Class)

#Calculate accuracy
wrong_prediction_rate<-wrong_prediction/length(predict_naive)

#Output of Naive Bayes Classifier
print(paste("Total wrong Predictions:" , wrong_prediction))
print(paste("Error Rate:" , wrong_prediction_rate))
print(paste("Accuracy:" , 100-(wrong_prediction_rate*100)))



### Output
### Table
###          class
# model_naive Benign Malignant
# Benign       119         3
# Malignant      9        74


###          class
# model_naive     Benign  Malignant
# Benign    0.58048780 0.01463415
# Malignant 0.04390244 0.36097561



### Output
# print(paste("Total wrong Predictions:" , wrong_prediction))
#[1] "Total wrong Predictions: 12"
#> print(paste("Error Rate:" , wrong_prediction_rate))
#[1] "Error Rate: 0.0585365853658537"
#> print(paste("Accuracy:" , 100-(wrong_prediction_rate*100)))
#[1] "Accuracy: 94.1463414634146"
