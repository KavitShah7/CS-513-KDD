# First Name : Kavit          #
# Last Name : Shah            #
# CWID : 10452991             #
# Course : CS513- B           #
# Purpose : Final_Exam        #
# Email:kshah116@stevens.edu  #
# # # # # # # # # # # # # # # #

library('randomForest')
#Question_2
#Clear all previous variables
rm(list = ls())

#Set working directory 
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD\\Final_Exam")

#Import Admission_cat dataset
data<-read.csv(file = 'Admission_cat.csv')

set.seed(123)
#splitting the dataset into train and test
train_data = floor(0.7*nrow(data))
train_ind = sample(seq_len(nrow(data)),size = train_data)
train =data[train_ind,]
test=data[-train_ind,]

#applying random forest using the ADMIT column
randomForest_class<-randomForest(ADMIT~.,data = train)
summary(randomForest_class)
#plotting random forest graph
plot(randomForest_class)

randomForest_predict<-predict( randomForest_class ,test , type="class" )
randomForest_predict

random_predict_cart <- ifelse(randomForest_predict<0.5,"0","1")
#creating confusion matrix
conf_mat<-table(actual=test[,2],Random_Forest = random_predict_cart )
conf_mat

#finding out accuracy
accuracy<-sum(diag(conf_mat)/nrow(test)) * 100
accuracy

