# Load the Necessary Packages
library(cluster)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Rtsne) 
library(funModeling)
library(dlookr)
library(factoextra)
library(NbClust)

#set seed
set.seed(7920)

#Load in the data
og.dataset = read.csv("FinalDataset.csv", header=TRUE)
str(og.dataset)
summary(og.dataset)

#################################################################
#             DATA PREPARATION AND TRANSFORMATION
#################################################################

# 1). Remove non predictor variables: SurveyId, Village and Depressed
dep.dataset = og.dataset[, -c(1,2,24)]
str(dep.dataset)

# 2). Convert the relevant variables to Type Factor
dep.dataset <- cbind(dep.dataset[, -c(2,3,5,16,17,21)],lapply(dep.dataset[, c(2,3,5,16,17,21)], as.factor))
str(dep.dataset)

# 3). Much of the data is severely skewed and needs to be transformed
plot_num(dep.dataset)
skewed_variables <- find_skewness(dep.dataset)
str(dep.dataset[, c(skewed_variables)])

#Skewed variables: 
#asset_livestock, asset_durable, asset_phone, asset_savings
#asset_land_owned_total, cons_allfood, cons_ownfood, cons_ed, cons_social, cons_other
#ent_farmexpenses, ent_animalstockres, fs_adskipm_often 

#use the log transform on these variables, especially because they relate to income
#I want to use hierarchical clustering with Gower which is sensitive to 
# non normality. Additionally since many of the variables are 0, a simple
# log transform will return infinity values so log+1 is required
dep.dataset.log <- cbind(dep.dataset[,-c(skewed_variables)],lapply(dep.dataset[, c(skewed_variables)], transform, method="log+1"))
plot_num(dep.dataset.log)
#Slight improvement but still a lot of 0 values exist, particularlty for asset_savings 
#ent_animalstockrevenue

str(dep.dataset.log)

#need to convert the values back to int/num
dep.dataset.log <- cbind(dep.dataset.log[, c(1:8)],lapply(dep.dataset[, -c(1:8)], as.integer))
str(dep.dataset.log)


#################################################################
#             Hierarchical Clustering: Gower
#################################################################
#As the data is mixed with continuous, discrete and binary variables
#I will use the daisy function in package cluster to calculate the Gower distance
#The Gower algorithm automatically standardises data so no further
#standardisation is needed

#Calculate distance matrix
gower.matrix <- daisy(dep.dataset.log, metric = "gower")

#Use a variety of linkage methods: I want compact clusters
hc.w = hclust(gower.matrix, method="ward.D")
hc.w2 = hclust(gower.matrix, method="ward.D2")
hc.c = hclust(gower.matrix, method="complete")

plot(hc.w)
plot(hc.w2)
plot(hc.c)



#########################################################
# Selecting the Optimal Algorithm and no. of Clusters
#########################################################




