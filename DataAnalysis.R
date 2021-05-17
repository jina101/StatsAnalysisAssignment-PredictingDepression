# Code for Full Analysis: Clustering and Classification

#Set seed
set.seed(7290)

# Load necessary packages
library(cluster)
library(Rtsne) 
library(ggplot2) 
library(dplyr)

# Read in Data
dataset.fin = read.csv("FinalDataset.csv", header=TRUE)
str(dataset.fin)
summary(dataset.fin)

#Remove certain variables: village id, survey id, depressed etc which are not
#predictor variables

depression.data = dataset.fin[, -c(1,2,26)]
str(depression.data)

#set binary variables as factors as well as other factor variables
depression.data$married = as.factor(depression.data$married)
depression.data$hh_children = as.factor(depression.data$hh_children)
depression.data$ent_nonagbusiness =as.factor(depression.data$ent_nonagbusiness)
depression.data$ent_ownfarm = as.factor(depression.data$ent_ownfarm) 
depression.data$labor_primary = as.factor(depression.data$labor_primary)

str(depression.data)

#######################################################################
#             Clustering 1: Hierarchical Agglomerative                        
#######################################################################
#As the data is mixed with continuous, discrete and binary variables
#I will use the daisy function in cluster to calculate the Gower distance

#Calculate distance matrix
gower.matrix <- daisy(depression.data, metric = "gower")

#Use a variety of linkage methods: I want compact clusters
hc.w = hclust(gower.matrix, method="ward.D")
hc.c = hclust(gower.matrix, method="complete")

plot(hc.w)
plot(hc.c)

#Using the ward distance for now, we say there look as if there are five clusters
#Look like clean clusters, far from each other(joined at higher distances so probably
#more distinct). Use some measures like Dunn Index etc to test

#To Do: Analyse more methods to determine the optimal number of clusters
# Using PAM (not K-means) etc etc

#Let's analyse these five clusters and see if there is any insight
clusters <- cutree(hc.w, 5)
clusters


#Looks like everyone is married in this cluster and has few children
h1<- which(clusters == 1)
summary(dataset.fin[h1,])
str(dataset.fin[h1,])
#The amount who are depressed is 42 vs 241 not depressed (17%)
table(dataset.fin[h1,26])

h2<- which(clusters == 2)
summary(dataset.fin[h2,])
str(dataset.fin[h2,])
#The amount who are depressed is 23 vs 133 not depressed (17%)
table(dataset.fin[h2,26])

h3<- which(clusters == 3)
summary(dataset.fin[h3,])
str(dataset.fin[h3,])
#The amount who are depressed is 34 vs 183 not  (18%)
table(dataset.fin[h3,26])

h4<- which(clusters == 4)
summary(dataset.fin[h4,]) 
str(dataset.fin[h4,])
#The amount who are depressed is 48 vs 237 not depressed (20%)
table(dataset.fin[h4,26])
  
  #This one seems to have more married people (younger)
  #More children
  #Need to properly analyse the differences between each

h5<- which(clusters == 5)
summary(dataset.fin[h5,])
str(dataset.fin[h5,])
#The amount who are depressed is 46 vs 157 not depressed (30%)
table(dataset.fin[h5,26])

#Conclusion, higher rates of depression in the fifth cluster
#Their charateristics include:
      #They are older on average than the other clusters (mean 45 vs 30-33)
      #Check if more people are married than other clusters
      #Analyse the differences between each variables for the other clusters


#######################################################################
#                   Clustering 2: PAM                          
#######################################################################





#######################################################################
#               Prediction 1: Logistic Regression                          
#######################################################################


#######################################################################
#               Prediction 2: Some other Method                          
#######################################################################

