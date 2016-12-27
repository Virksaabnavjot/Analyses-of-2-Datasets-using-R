#Author: Navjot Singh Virk
#Student Number: x13112406

#Getting the current working directory
getwd()

#NOTE: Please modify the working directory when working in your computer and the files
#are saved at different location then whats set into setwd

#Setting current directory to where datasets are saved
setwd("~/documents/love/analyses-of-2-datasets-using-r/datasets/")

#Reading Dataset for math student and storing in an object called mathData
mathData <- read.csv("student-mat.csv",sep=";",header=TRUE)

#Printing the data
mathData

#Display the structure of R object, summary and view data
str(mathData)
summary(mathData)
View(mathData)

#prints the number of rows and colums in dataset
dim(mathData)
#Printing the first 2 rows just to see the data
head(mathData, 2)

#Printing the number of attributes in the dataset (i.e no. of columns)
length(mathData)

#TESTING - Playing around with data
#The below subset the data and shows school,sex and age
mathData[1:3]
#The line below subsets and print the data in row 1 and column 3, which is 18
mathData[1,3]
#The line below prints the number of girls and boys i.e (male,female)
summary(mathData$sex) 
#Another way -The line below prints the number of students in each school
#(GP - 349 and MS - 46)
summary(mathData[ ,1])  # or summary(mathData$school)

#End of TESTING











######################################################################################
#Reading Dataset for portuguese subjects student and storing in an object called porData
porData <- read.csv("student-por.csv",sep=";",header=TRUE)

#Printing the data
porData




