# First Name : Kavit          #
# Last Name : Shah            #
# CWID : 10452991             #
# Course : CS513- B           #
# Purpose : Final_Exam        #
# Email:kshah116@stevens.edu  #
# # # # # # # # # # # # # # # #

#Question_1

#Clear all previous variables
rm(list = ls())

#Set working directory 
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD\\Final_Exam")

#Import Admission_cat
dataset<-read.csv(file = 'Admission.csv')
# Delete the first and Fifth Column 

dataset<-dataset[-5]
dataset<-dataset[-1]
#Normalised the Columns
dataset$GRE_normalised<-scale(dataset$GRE)
dataset$GPA_normalised<-scale(dataset$GPA)
dataset<-dataset[-3]
dataset<-dataset[-2]
# Convert all columns to Factor
dataset<-transform(dataset, ADMIT = as.factor(ADMIT),GRE_normalised = as.factor(GRE_normalised),GPA_normalised = as.factor(GPA_normalised))
dataset$ADMIT<- factor(dataset$ADMIT , levels = c("0","1") , labels = c("1","2"))

# Omit all NA Values
dataset<-na.omit(dataset)
# Implement Kmeans Algortihm with 2 centers
kmeans_2<- kmeans(dataset[-1],2,nstart = 10)
kmeans_2$cluster
# Plot table against ADMIT column
conf_mat<-table(Kmeans = kmeans_2$cluster,Admit = dataset[,1])
conf_mat

# Calculate Distance
dataset_dist<-dist(dataset)
# Implement HClust algorithm
hclust_results<-hclust(dataset_dist)
# PLot the Cluster Dendogram
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
# Plot table 
conf_mat<-table(hclust_2,dataset[,1])
conf_mat

