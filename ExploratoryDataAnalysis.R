#load in some useful packages
#load the caret, tidyverse and mlbench packages
library(mlbench)
library(caret)
library(dplyr)
library(corrplot)
library(tidyverse)


# Read in the dataset for exploratory analysis with 34 variables, 31 of which are 
# for predicting whether someone has depression or not
depression.data = read.csv("DataToAnalyse.csv", header=TRUE)
str(depression.data)

#Convert children, hhsize, hh_children, ent_ownfarm, ent_nonagbusiness to factor variables
# as they represent categorical above, below and average levels
depression.factor <- depression.data
depression.factor$children = as.factor(depression.factor$children)
depression.factor$hh_children = as.factor(depression.factor$hh_children)
depression.factor$hhsize = as.factor(depression.factor$hhsize)

#depression.factor$femaleres = as.factor(depression.factor$femaleres)
#depression.factor$married = as.factor(depression.factor$married)
#depression.factor$ent_ownfarm = as.factor(depression.factor$ent_ownfarm)
#depression.factor$ent_nonagbusiness = as.factor(depression.factor$ent_nonagbusiness)
#depression.factor$labor_primary = as.factor(depression.factor$labor_primary)
#depression.factor$depressed = as.factor(depression.factor$depressed)

str(depression.factor)
