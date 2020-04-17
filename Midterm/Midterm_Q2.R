
# Name : Kavit Shah           #
# CWID : 10452991             #
# Course : CS513- B           #
# Purpose : HW_Midterm_Exam   #
# Email:kshah116@stevens.edu  #



#Purpose: Question 2 of midterm


#Cleaning the datatset
rm(list=ls())


#Import dataset
setwd("C:\\Kavit\\Sem 2\\CS_513_KDD")
covid19<-read.csv("COVID19_v3.csv")
View(covid19)

#I.	Summarizing each column (e.g. min, max, mean)
summary(covid19)

#II.	Identifying missing values
is.na(covid19)

#III.	Displaying the frequency table of "Infected" vs. "MaritalStatus" 
ftable(covid19$Infected,covid19$MaritalStatus)

#IV.	Displaying the scatter plot of "Age", "MaritalStatus" and "MonthAtHospital", one pair at a time
plot(covid19$Age, covid19$MaritalStatus)
plot(covid19$Age, covid19$MonthAtHospital)
plot(covid19$MaritalStatus,covid19$MonthAtHospital)
#V.	Show box plots for columns:  "Age", "MaritalStatus" and "MonthAtHospital"
boxplot(covid19$Age, covid19$MaritalStatus)
boxplot(covid19$Age, covid19$MonthAtHospital)
boxplot(covid19$MaritalStatus,covid19$MonthAtHospital )


