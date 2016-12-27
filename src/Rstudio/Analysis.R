install.packages(c("e1071", "C50", "ggplot2", "hexbin","descr", "caret", "e1071"))
library(e1071)
library(hexbin)
library(ggplot2)
library(caret)
library(descr)
library(C50)
library(zoo)

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

#Reading Dataset for portuguese subjects student and storing in an object called porData
porData <- read.csv("student-por.csv",sep=";",header=TRUE)

#set names of an object (here mathData and porData)
names(mathData)
names(porData)

#Printing the data
# mathData
# porData

#Start to clean data .. removing this and that columns do this later

#Display the structure of R object, summary and view data
str(mathData)
summary(mathData)
View(mathData)

#Display the average age of students in each dataset
median(mathData$age)
median(porData$age)

#Display the number of rows and colums in dataset
dim(mathData)
#Display the first 2 rows just to see the data
head(mathData, 2)

#Display the number of attributes in the dataset (i.e no. of columns)
length(mathData)

#TESTING - Playing around with math students data
#The below subset the data and shows school,sex and age
mathData[1:3]
#The line below subsets and display the data in row 1 and column 3, which is 18
mathData[1,3]
#The line below displays the number of girls and boys i.e (male,female)
summary(mathData$sex) 
#Another way -The line below displays the number of students in each school
#(GP - 349 and MS - 46)
summary(mathData[ ,1])  # or summary(mathData$school)

#End of TESTING


#Drawing a plot to see which school has the most students in maths class (GP or MS)
plot(x=mathData$school, y=mathData$school)
#Drawing a plot to see which school has the most students in portuguese class 
plot(x=porData$school, y=porData$school)


#Drawing a plot to see male and female students in both subject and their age range
#and the average age i.e 17 for females and 16 for male students in portugues class
#In Maths class 
plot(x=mathData$sex, y=mathData$age)
#In portugues class and  
plot(x=porData$sex, y=porData$age)

######################################################################################


#Subset data where daily drinking exceeds from medium - 3 to 
#very high alcohol consumption - 5
#In maths class
high.drinking.math <- subset(mathData, Dalc > 3)
high.drinking.math
#In portuguese class
high.drinking.por <- subset(porData, Dalc > 3)
high.drinking.por
dim(high.drinking.math)
dim(high.drinking.por)

#References
#https://www.youtube.com/watch?v=qK1ElUMkhq0



