#load the caret, tidyverse and mlbench packages
library(mlbench)
library(caret)
library(dplyr)
library(corrplot)
library(tidyverse)

#set the seed
set.seed(729)

# Read in the full dataset with all 75 predictor variables and 1143 objects
busara.data = read.csv("Full_Data.csv", header=TRUE)
str(busara.data)

#How many are respondents specific to one gender
sum(busara.data[,4]==1)

#How many are depressed
sum(busara.data[,74]==1)



####################################################################################################
#                             DATA CLEANSING: Remove Excess Missing Values                              
####################################################################################################

#Remove the columns which have an excess amount of Nas (greater than 25%):
#The likes of med_u5_deaths have been removed which contain over 85% Nas
#and only the columns with less than 25% values being Nas are kept
b.less_Nas <- busara.data[ , colSums(is.na(busara.data)) < (nrow(busara.data)*0.25)]
str(b.less_Nas)

####################################################################################################
#                             DATA CLEANSING : Remove Columns Where All Data
#                                   Is the Same or Almost the Same
####################################################################################################

#Remove the columns where all values are the same (i.e. variance is 0)
# This is because if all the values are the same, then it won't add 
# much to our prediction model 
identifiers <- b.less_Nas[,c(1,2,4,53)]
str(identifiers)
b.less <- Filter(var, b.less_Nas[,c(-1, -2, -3, -53, -54)])
str(b.less)

#Remove columns with excess 0s (more than 80%), the same principal 
# as above applies 
b.less0 <- b.less[ , colSums((b.less==0)) < (nrow(b.less)*0.8)]
str(b.less0)

#Remove columns with excess 1s (mor than 80%) the same principal 
# as above applies
b.less1 <- b.less0[ , colSums((b.less0==1)) < (nrow(b.less0)*0.8)]
str(b.less1)

exploratorydataset <- cbind(b.less1, identifiers)
str(exploratorydataset)

#Find columns with null values and fill them in with the average/median 
# of the non null values in that column

colnames(exploratorydataset)[colSums(is.na(exploratorydataset)) > 0]
# Luckily removing columns with excess Nas has actually left us
# with data that no longer has any missing values

####################################################################################################
#                         DATA CLEANSING: Check for and Remove Duplicates                               
####################################################################################################

#Is there someone who's taken a survey twice?
exploratorydataset[duplicated(exploratorydataset$surveyid)]

#There are no duplicates, note more than one person can be 
# from the same village so it is inappropriate to check for duplicates there

library(dplyr)

#Additionally some values in the age column are not whole numbers and those values
# after the decimal place will be removed
exploratorydataset <- exploratorydataset %>% mutate_at(vars(age), funs(signif(., 2)))

#Finally we need to round monetary values in the following columns to decimal places
exploratorydataset <- as.data.frame(lapply(exploratorydataset, function(x) if(is.numeric(x)) round(x, 2) else x))
exploratorydataset

#exploratorydataset <- exploratorydataset %>% mutate_at(vars(starts_with("cons_nondurable")), funs(round(., 2)))
#vars(cons_nondurable, asset_livestock,asset_durable,asset_phone,asset_savings,asset_land_owned_total,cons_allfood,cons_ownfood,cons_ed)

# This dataset will now be brought forward to the exploratory analysis
# and further modified
write.csv(exploratorydataset, "DataToAnalyse.csv")

# I have decided to keep female respondents and remove the date and day of week
# variables as these surveys were once off


#calculate the correlation matrix
#corr_matrix <- cor(b.less1)

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

