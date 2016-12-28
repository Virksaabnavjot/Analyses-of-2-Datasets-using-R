install.packages(c("e1071", "C50", "ggplot2", "hexbin","descr", "caret", "e1071", "plotly"))
library(e1071)
library(waffle)
library(hexbin)
library(ggplot2)
library(plotly)
library(caret)
library(descr)
library(C50)
library(zoo)
library(plyr)

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

#################################################
#TESTING - Playing around with math students data
#################################################

#The below subset the data and shows school,sex and age
mathData[1:3]
#The line below subsets and display the data in row 1 and column 3, which is 18
mathData[1,3]
#The line below displays the number of girls and boys i.e (male,female)
summary(mathData$sex) 
#Another way -The line below displays the number of students in each school
#(GP - 349 and MS - 46)
summary(mathData[ ,1])  # or summary(mathData$school)

#################################################
#End of TESTING
#################################################


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

#################################################
#Helpful Plots
################################################

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


#################################################
#Maths Class Plots
#################################################


mathData$Dalc <- as.factor(mathData$Dalc)      
mathData$Dalc <- mapvalues(mathData$Dalc, 
                           from = 1:5, 
                           to = c("Very Low", "Low", "Medium", "High", "Very High"))

mathData$Walc <- as.factor(mathData$Walc)      
mathData$Walc <- mapvalues(mathData$Walc, 
                           from = 1:5, 
                           to = c("Very Low", "Low", "Medium", "High", "Very High"))


# Weekly alcohol consumption and guardian's
plotWG <- ggplot(mathData, aes(guardian,Walc, fill = guardian)) +
  geom_boxplot()+
  ggtitle("Weekend alcohol consumption as per guardian's")

ggplotly(plotWG)

plotWS <- ggplot(mathData, aes(sex, Walc, fill = sex))+
  geom_boxplot()+
  ggtitle("Weekend Alcohol consumption as per Gender")

ggplotly(plotWS)


### Weekend Alchol Consumption based on age of student in maths class
plotWA <- ggplot(mathData, aes(age, Walc, fill = Walc))+
  geom_boxplot(aes(fill=factor(age)))+
  ggtitle("Weekend Alcohol consumption as per age. \nin Maths Class")

ggplotly(plotWA)

####Working bit above
barplot(table(mathData$Dalc), ylab='Number of Students', xlab='Weekend Alcohol',
        main ='Workday Alcohol Consumption (Maths Class) ',
        col=rainbow(7))

ggplot(mathData, aes(x=Dalc, y=absences, fill=Dalc, color = Dalc))+
  geom_jitter(alpha=0.7)+
  theme_bw()+
  ggtitle("School Absences distribution per Workday alcohol consumption")+
  xlab("Workday Alcohol consumption")+
  ylab("Number of school absences")

ggplot(mathData, aes(x=Walc, y=absences, fill=Walc, color = Walc))+
  geom_jitter(alpha=0.7)+
  theme_bw()+
  ggtitle("School Absences distribution per Weekend alcohol consumption")+
  xlab("Weekend Alcohol consumption")+
  ylab("No. of absences")


ggplot(mathData, aes(x=Walc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Weekend alcohol consumption")+
  ylab("School")+
  ggtitle("Weekend alcohol consumption per school and sex")



#References
#Loading Data and Working With Data Frames: https://www.youtube.com/watch?v=qK1ElUMkhq0
#UCI Kaggel: https://www.kaggle.com/uciml/student-alcohol-consumption/
#Add horizontal lines to whiskers using ggplot2: https://plot.ly/ggplot2/box-plots/
#Using plyr::mapvalues with dplyr: http://stackoverflow.com/questions/28013652/using-plyrmapvalues-with-dplyr
#Error: stat_count() must not be used with a y aesthetic: https://github.com/tidyverse/ggplot2/issues/1415
#Add color to boxplot - “Continuous value supplied to discrete scale” error: http://stackoverflow.com/questions/10805643/add-color-to-boxplot-continuous-value-supplied-to-discrete-scale-error
#Student Alcohol Consumption: https://rpubs.com/Ndee/student-alcohol
#Kaggle: https://www.kaggle.com/marcdeveaux/d/uciml/student-alcohol-consumption/student-alcohol-consumption/code




