#First Name:Kavit
#Last Name: Shah
#CWID: 10452991
#Assignment 8 - CS 513


#Clearing the previous data and values
rm(list=ls())


#Set the directory and import the dataset
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
cancer <- read.csv("wisc_bc_ContinuousVar.csv",na.strings = '?')
View(cancer)
summary(cancer)
table(cancer$diagnosis)

#To factor the data set
cancer<-na.omit(cancer)
cancer<-cancer[-1]
cancer_dist<-dist(cancer[,-1])

#h clust
hclust_results<-hclust(cancer_dist)
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
table(hclust_2,cancer[,1])

#### QUESTION 2 ####

#Clearing the values of the previous data
rm(list=ls())

#Changing the directory and importing the dataset
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
cancer <- read.csv("wisc_bc_ContinuousVar.csv",na.strings = '?')
View(cancer)
summary(cancer)
table(cancer$diagnosis)

#To factor the data set
cancer<-na.omit(cancer)
cancer<-cancer[-1]

#kmeans
kmeans_2<- kmeans(cancer[,-1],2,nstart = 10)
kmeans_2$cluster
table(kmeans_2$cluster,cancer[,1])

