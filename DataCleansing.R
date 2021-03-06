#load the caret, tidyverse and mlbench packages
library(mlbench)
library(caret)
library(dplyr)
library(corrplot)
library(tidyverse)


#set the seed
set.seed(729)

# Read in the full dataset with all 75 variables and 1143 objects
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
#The likes of med_u5_deaths have been removed which contain over 75% Nas
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


####################################################################################################
#                         DATA CLEANSING: Rounding and Grouping                              
####################################################################################################

library(dplyr)

#Additionally some values in the age column are not whole numbers and those values
# after the decimal place will be removed
exploratorydataset <- exploratorydataset %>% mutate_at(vars(age), funs(signif(., 2)))

#We need to round monetary values in the following columns to decimal places
exploratorydataset <- as.data.frame(lapply(exploratorydataset, function(x) if(is.numeric(x)) round(x, 2) else x))
exploratorydataset

#I would like to group the children and number of children in a household and hh size
#3 levels: 0-2 is 1 [Below average], 3-5 [Average] is 2, 6+ is 3 [Above Average]
summary(exploratorydataset$children)
unique(exploratorydataset$children)

#3 levels: 0-2 is 1 [Below average], 2-4 [Average] is 2, 5+ is 3 [Above Average]
summary(exploratorydataset$hh_children)
unique(exploratorydataset$hh_children)

#3 levels: 0-4 is 1 [Below average], 5-7 [Average] is 2, 8+ is 3 [Above Average]
summary(exploratorydataset$hhsize)
unique(exploratorydataset$hhsize)


#Function to group the data and add new factor values as above:
function.group <- function(dataframe, column, upperbnd1, upperbnd2) 
{
  for(i in 1:nrow(dataframe))
  {
    if(dataframe[i,column] <= upperbnd1)
    {
      dataframe[i,column] = 1
    }
    else if(dataframe[i,column] > upperbnd1 && dataframe[i,column] <= upperbnd2)
    {
      dataframe[i,column] = 2
    }
    else
    {
      dataframe[i,column] = 3
    }
  }
  return(dataframe[,column])
}

#Change children
exploratorydataset$children <- function.group(exploratorydataset, 3, 2, 5)

#Change hh_children
exploratorydataset$hh_children <- function.group(exploratorydataset, 6, 2, 4)

#Change hhsize
exploratorydataset$hhsize <- function.group(exploratorydataset, 4, 4, 7)

str(exploratorydataset)

# This dataset will now be brought forward to the exploratory analysis
# and further modified
write.csv(exploratorydataset, "DataToAnalyse.csv")

# I have decided to keep female respondents and remove the date and day of week
# variables as these surveys, to my understanding were once off an not recurring

