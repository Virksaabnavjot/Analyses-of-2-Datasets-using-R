install.packages(c("e1071", "C50", "ggplot2", "hexbin","descr", "caret", "e1071", "plotly"))
library(e1071)
library(hexbin)
library(ggplot2)
library(waffle)
library(plotly)
library(caret)
library(descr)
library(C50)
library(plyr)
library(gridExtra)

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
#View(mathData)

#Display the average age of students in each dataset
median(mathData$age)
median(porData$age)

#Display the number of rows and colums in datasets
dim(mathData)
dim(porData)
#Display the first 2 rows just to see the data
head(mathData, 2)

#Display the number of attributes in the dataset (i.e no. of columns)
length(mathData)
length(porData)

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
# Plots
#################################################

#USING workday and weekend alcohol cnsumption as factors
#and mapping to a range form very low to very high consumption level
mathData$Dalc <- as.factor(mathData$Dalc)      
mathData$Dalc <- mapvalues(mathData$Dalc, 
                           from = 1:5, 
                           to = c("Very Low", "Low", "Medium", "High", "Very High"))

mathData$Walc <- as.factor(mathData$Walc)      
mathData$Walc <- mapvalues(mathData$Walc, 
                           from = 1:5, 
                           to = c("Very Low", "Low", "Medium", "High", "Very High"))


porData$Dalc <- as.factor(porData$Dalc)      
porData$Dalc <- mapvalues(porData$Dalc, 
                           from = 1:5, 
                           to = c("Very Low", "Low", "Medium", "High", "Very High"))

porData$Walc <- as.factor(porData$Walc)      
porData$Walc <- mapvalues(porData$Walc, 
                           from = 1:5, 
                           to = c("Very Low", "Low", "Medium", "High", "Very High"))

#################################################
# Plots # Weekly alcohol consumption as per guardian
#################################################

#Maths Class
plotWGMath <- ggplot(mathData, aes(guardian,Walc, fill = guardian)) +
  geom_boxplot()+
  ggtitle("Weekend alcohol consumption as per guardian's")

ggplotly(plotWGMath)  #Result - Draw plot

# In Portuguese language class
plotWGPor <- ggplot(porData, aes(guardian,Walc, fill = guardian)) +
  geom_boxplot()+
  ggtitle("Weekend alcohol consumption as per guardian's")

ggplotly(plotWGPor) #Result - Draw plot

#Comparision b/w classes
subplot(plotWGMath, plotWGPor, nrows = 2)  #Result - Draw plot


#################################################
# Plot # Weekend Alcohol consumption as per Gender
#################################################

plotWS <- ggplot(mathData, aes(sex, Walc, fill = sex))+
  geom_boxplot()+
  ggtitle("Weekend Alcohol consumption as per Gender")

ggplotly(plotWS)

#################################################
# Plot # Weekend Alchol Consumption based on age of student
#################################################

#In Maths Class
plotWAMath <- ggplot(mathData, aes(age, Walc, fill = Walc))+
  geom_boxplot(aes(fill=factor(age)))+
  ggtitle("Weekend Alcohol consumption as per age")

ggplotly(plotWAMath)

#In Portuguese language class
plotWAPor <- ggplot(porData, aes(age, Walc, fill = Walc))+
  geom_boxplot(aes(fill=factor(age)))+
  ggtitle("Weekend Alcohol consumption as per age")

ggplotly(plotWAPor)

#################################################
# Barplot # Weekday Consumption and no. of students
#################################################

#Maths Class
barplot(table(mathData$Dalc), ylab='Number of Students', xlab='Weekend Alcohol',
        main ='Workday Alcohol Consumption (Maths Class) ',
        col=rainbow(7))

barplot(table(porData$Dalc), ylab='Number of Students', xlab='Weekend Alcohol',
        main ='Workday Alcohol Consumption (Por Class) ',
        col=rainbow(7))

#################################################
# School Absences distribution per Workday alcohol consumption
# Maths Class
#################################################

m1 <- ggplot(mathData, aes(x=Dalc, y=absences, fill=Dalc, color = Dalc))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff33ff", "#00ffff", "#468499", "#ff7f50", "#0000cc"))+
  theme_bw()+
  ggtitle("School Absences distribution per Workday alcohol consumption")+
  xlab("Workday Alcohol consumption (Maths Class)")+
  ylab("Number of school absences")

m2 <- ggplot(mathData, aes(x=Walc, y=absences, fill=Walc, color = Walc))+
  geom_jitter(alpha=0.7)+
  theme_bw()+
  scale_colour_manual(values=c("#ff33ff", "#00ffff", "#468499", "#ff7f50", "#0000cc"))+
  ggtitle("School Absences distribution per Weekend alcohol consumption")+
  xlab("Weekend Alcohol consumption (Maths Class)")+
  ylab("No. of absences")

grid.arrange(m1,m2) #Plots Results comparision

#################################################
# School Absences distribution per Workday alcohol consumption
# Portuguese Subject Class
#################################################

p1 <- ggplot(porData, aes(x=Dalc, y=absences, fill=Dalc, color = Dalc))+
  geom_jitter(alpha=0.7)+
  theme_bw()+
  ggtitle("School Absences distribution per Workday alcohol consumption")+
  xlab("Workday Alcohol consumption (Portuguese Class)")+
  ylab("Number of school absences")

p2 <- ggplot(porData, aes(x=Walc, y=absences, fill=Walc, color = Walc))+
  geom_jitter(alpha=0.7)+
  theme_bw()+
  ggtitle("School Absences distribution per Weekend alcohol consumption")+
  xlab("Weekend Alcohol consumption (Portuguese Class)")+
  ylab("No. of absences")

grid.arrange(p1,p2) #Plots Results comparision

#################################################
# Alcohol consumption per school and sex
#################################################

# Weekend Comsumption (Maths Student)
w1 <- ggplot(mathData, aes(x=Walc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff33ff", "#00cc00"))+
  theme_bw()+
  xlab("Weekend alcohol consumption (Maths Class)")+
  ylab("School")+
  ggtitle("Weekend alcohol consumption per school and sex")

# Workday Comsumption (Maths Student)
w2 <- ggplot(mathData, aes(x=Dalc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff33ff", "#00cc00"))+
  theme_bw()+
  xlab("Workday alcohol consumption (Maths Class)")+
  ylab("School")+
  ggtitle("Workday alcohol consumption per school and sex")

grid.arrange(w1,w2)

# Weekend Comsumption (Portuguese Student)
w3 <- ggplot(porData, aes(x=Walc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff33ff", "#0000ff"))+
  theme_bw()+
  xlab("Weekend alcohol consumption (Portuguese Class)")+
  ylab("School")+
  ggtitle("Weekend alcohol consumption per school and sex")

# Workday Comsumption (Portuguese Student)
w4 <- ggplot(porData, aes(x=Dalc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff33ff", "#0000ff"))+
  theme_bw()+
  xlab("Workday alcohol consumption (Portuguese Class)")+
  ylab("School")+
  ggtitle("Workday alcohol consumption per school and sex")

grid.arrange(w3,w4)


#################################################
# Workday alcohol consumption and grades
#################################################

#Maths Students 
g1 <- ggplot(mathData, aes(x=Dalc, y=G1, fill=Dalc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("First Grade")

g2 <- ggplot(mathData, aes(x=Dalc, y=G2, fill=Dalc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Second Grade")

g3 <- ggplot(mathData, aes(x=Dalc, y=G3, fill=Dalc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Final Grade")

grid.arrange(g1,g2,g3,ncol=3)

#Portuguese Students
h1 <- ggplot(porData, aes(x=Dalc, y=G1, fill=Dalc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("First Grade")

h2 <- ggplot(porData, aes(x=Dalc, y=G2, fill=Dalc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Second Grade")

h3 <- ggplot(porData, aes(x=Dalc, y=G3, fill=Dalc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Final Grade")

grid.arrange(h1,h2,h3,ncol=3)

#################################################
# Histogram to see weekly Study time of students
#################################################

#Maths class
hist(mathData$studytime, main = "Study Time (Maths Class)",
     xlab = "Weekly Studytime \nnumeric:1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours",
     xlim=c(0,10))

#Portuguese class
hist(porData$studytime, main = "Study Time (Portuguese Class)",
     xlab = "Weekly Studytime \nnumeric:1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10 hours",
     xlim=c(0,10))

#################################################
# Plot to see grades and weekly Study time of students
#################################################

mathData$studytime <- as.factor(mathData$studytime)      
mathData$studytime <- mapvalues(mathData$studytime, 
                       from = 1:4, 
                       to = c(" < 2 ", "2 - 5", "5 - 10", " > 10"))

porData$studytime <- as.factor(porData$studytime)      
porData$studytime <- mapvalues(porData$studytime, 
                                from = 1:4, 
                                to = c(" < 2 ", "2 - 5", "5 - 10", " > 10"))


#Maths Students
s1 <- ggplot(mathData, aes(x=studytime, y=G1, fill=studytime))+
  geom_boxplot(aes(fill=factor(studytime)))+
  
  theme_bw()+
  theme(legend.position="none")+
  xlab("Weekly Studytime (in hours)")+
  ylab("Grade")+
  ggtitle("First Grade")

s2 <- ggplot(mathData, aes(x=studytime, y=G2, fill=studytime))+
  geom_boxplot(aes(fill=factor(studytime)))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Weekly Studytime (in hours)")+
  ylab("Grade")+
  ggtitle("Second Grade")

s3 <- ggplot(mathData, aes(x=studytime, y=G3, fill=studytime))+
  geom_boxplot(aes(fill=factor(studytime)))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Weekly Studytime (in hours)")+
  ylab("Grade")+
  ggtitle("Final Grade")

grid.arrange(s1,s2,s3,ncol=3)

#Portuguese Students
t1 <- ggplot(mathData, aes(x=studytime, y=G1, fill=studytime))+
  geom_boxplot(aes(fill=factor(studytime)))+
  
  theme_bw()+
  theme(legend.position="none")+
  xlab("Weekly Studytime (in hours)")+
  ylab("Grade")+
  ggtitle("First Grade")

t2 <- ggplot(mathData, aes(x=studytime, y=G2, fill=studytime))+
  geom_boxplot(aes(fill=factor(studytime)))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Weekly Studytime (in hours)")+
  ylab("Grade")+
  ggtitle("Second Grade")

t3 <- ggplot(mathData, aes(x=studytime, y=G3, fill=studytime))+
  geom_boxplot(aes(fill=factor(studytime)))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Weekly Studytime (in hours)")+
  ylab("Grade")+
  ggtitle("Final Grade")

grid.arrange(t1,t2,t3,ncol=3)


#################################################
# References
#################################################

#Loading Data and Working With Data Frames: https://www.youtube.com/watch?v=qK1ElUMkhq0
#UCI Kaggel: https://www.kaggle.com/uciml/student-alcohol-consumption/
#Add horizontal lines to whiskers using ggplot2: https://plot.ly/ggplot2/box-plots/
#Using plyr::mapvalues with dplyr: http://stackoverflow.com/questions/28013652/using-plyrmapvalues-with-dplyr
#Error: stat_count() must not be used with a y aesthetic: https://github.com/tidyverse/ggplot2/issues/1415
#Add color to boxplot - “Continuous value supplied to discrete scale” error: http://stackoverflow.com/questions/10805643/add-color-to-boxplot-continuous-value-supplied-to-discrete-scale-error
#Student Alcohol Consumption: https://rpubs.com/Ndee/student-alcohol
#Kaggle: https://www.kaggle.com/marcdeveaux/d/uciml/student-alcohol-consumption/student-alcohol-consumption/code
#Arranging multiple grobs on a page: https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
#Error: could not find function "grid.arrange" :http://stackoverflow.com/questions/32826957/errors-using-multi-plot-ggplot2-and-grid-arrange-gridextra
#Html Color Picker: http://www.w3schools.com/colors/colors_picker.asp


