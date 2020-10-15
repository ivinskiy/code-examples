# Project 2 in SF2935 HT19
# Part A
#Only need to install following packages once
#install.packages("ISLR")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("gridExtra")
#install.packages("caTools")
#install.packages("caret")
#install.packages("class")
#install.packages("topicmodels")
# Get packages
library(ISLR)
library(MASS)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(caTools)
library(lattice)
library(class)
library(topicmodels)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

# Get dataset (from package ISLR)
auto= Auto

#Find cutoff point between high/low mpg
med = median(auto$mpg)

# Calculates the binary high/low mpg - variable
auto$mpg_bin = floor(auto$mpg/med)
auto$mpg_bin[auto$mpg_bin>=2] = 1

#Create groups of selected variables
auto$displacement_group = cut(auto$displacement, breaks=c(0, 91, 105, 121, 151, 225, 267, 350, 455), labels=c("91","104","121","146","225","262","350","350+"))
auto$horsepower_group = cut(auto$horsepower, breaks=c(0, 67, 75, 87, 93, 105, 125, 150, 230), labels = c("67","75","87","93","105","125","150", "150+"))
auto$weight_group = cut(auto$weight, breaks=c(0, 2045, 2223, 2515, 2800, 3193, 3613, 4154, 5140),labels=c("2045","2223","2515","2800","3193","3613","4154","4154+"))
auto$acceleration_group = cut(auto$acceleration, breaks=c(0,13, 14.5, 15.5, 16.5, 18.1, 24.8),labels=c("13","15","16","17","18","18+"))

#Removes mpg - column
auto = auto[-c(1)]
auto_norm = as.data.frame(lapply(auto[,c(3,4,5,6,7,9)],normalize))

#Split data into training and testing data sets
set.seed(5)
sample_size <- floor(0.5 * nrow(auto))
train_index <- sample(seq_len(nrow(auto_norm)), size = sample_size)

train <- auto_norm[train_index, ]
test <- auto_norm[-train_index, ]

#----------------------Classify with SVM------------
#install.packages('e1071') 
library(e1071) 
classifier = svm(formula = mpg_bin ~ ., 
                 data = train,
                 type = 'C-classification', 
                 kernel = 'linear')

# Predicting the Test set results 
y_pred = predict(classifier, newdata = test[-6]) 


