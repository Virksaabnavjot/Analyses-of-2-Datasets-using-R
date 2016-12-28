#################################################
# Joining both datasets together
#################################################
mathData <- read.csv("student-mat.csv",sep=";",header=TRUE)
porData <- read.csv("student-por.csv",sep=";",header=TRUE)

#Saving all the data in combinedData dataframe
combinedData <- rbind(mathData,porData)

View(combinedData)