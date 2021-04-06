#load the caret and mlbench packages
library(mlbench)
library(caret)
library(dplyr)

#set the seed
set.seed(729)

# Read in the full dataset
busara.data = read.csv("Full_Data.csv", header=TRUE)
str(busara.data)

#Remove the columns which have an excess amount of Nas (greater than 25%):
b.less_Nas <- busara.data[ , colSums(is.na(busara.data)) < (nrow(busara.data)*0.25)]
str(b.less_Nas)

#Remove the columns where all values are the same (i.e. variance is 0)
b.less <- Filter(var, b.less_Nas[,c(-1, -2, -3, -53, - 54)])
str(b.less)

#Check how much of the Mpesa columns are 0


#calculate the correlation matrix
corr_matrix <- cor(b.less_Nas[,c(-1, -2, -3, -53, - 54)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)