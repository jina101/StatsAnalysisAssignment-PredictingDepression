#load the caret, tidyverse and mlbench packages
library(mlbench)
library(caret)
library(dplyr)
library(corrplot)
library(tidyverse)

#set the seed
set.seed(729)

# Read in the full dataset
busara.data = read.csv("Full_Data.csv", header=TRUE)
str(busara.data)

#Remove the columns which have an excess amount of Nas (greater than 25%):
#The likes of med_u5_deaths have been removed which contain over 85% Nas
b.less_Nas <- busara.data[ , colSums(is.na(busara.data)) < (nrow(busara.data)*0.4)]
str(b.less_Nas)

#Remove the columns where all values are the same (i.e. variance is 0)
b.less <- Filter(var, b.less_Nas[,c(-1, -2, -3, -53, - 54)])
str(b.less)

#Remove columns with excess 0s (more than 80%)
b.less0 <- b.less[ , colSums((b.less==0)) < (nrow(b.less)*0.8)]
str(b.less0)

#Remove columns with excess 1s (mor than 80%)
b.less1 <- b.less0[ , colSums((b.less0==1)) < (nrow(b.less0)*0.8)]
str(b.less1)

#calculate the correlation matrix
corr_matrix <- cor(b.less1)

#print summary
#print(corr_matrix)

#plot correlation matrix
#corrplot(corr_matrix, method="number")

#which variables which are highly correlated:
#highly_corr <- findCorrelation(corr_matrix, cutoff=0.75)

#print which ones are highly correlated variables
#print(highly_corr)

#Remove the highly correlated ones:
#b.lesscorr <- b.less1[-highly_corr]
#str(b.lesscorr)

#corrplot(cor(b.lesscorr), method="number")
#pairs(b.less1)

#write.csv(b.lesscorr, "DataToAnalyse.csv")
