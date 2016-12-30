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
#The purpose of this file is to analyse the data we outputted through our reducer

#Getting the current working directory
getwd()

#NOTE: Please modify the working directory when working in your computer and the files
#are saved at different location then whats set into setwd

#Setting current directory to where datasets are saved
setwd("~/documents/love/analyses-of-2-datasets-using-r/src/atom")

#Reading Dataset for math student and storing in an object called mathData
g3Data <- read.csv("me.csv",sep=";",header=TRUE)

View(g3Data)
ggplot(g3Data, aes(x=Dalc, y=G3, fill=Dalc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Alcohol consumption")+
  ylab("Grade")+
  ggtitle("Final Grade(Por Class)")


