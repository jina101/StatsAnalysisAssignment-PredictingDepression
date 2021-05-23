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
library(clValid)

#set seed
set.seed(7920)

#Load in the data
og.dataset = read.csv("FinalDataset.csv", header=TRUE)
str(og.dataset)
summary(og.dataset)

#how many are depressed
table(og.dataset$depressed)

#Only 193 are depressed
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

###########################################################################################
#This section in this box did not work, the transform changed the variable type from int to 
#non numeric. However, changing it back to int removed the transformation performed
#Box-Cox transform instead of log transform did not give overly different results
#due to the sheer number of 0s in this dataset for all numeric variables
#use the log transform on these variables, especially because they relate to income
#I want to use hierarchical clustering with Gower which is sensitive to 
# non normality. Additionally since many of the variables are 0, a simple
# log transform will return infinity values so log+1 is required
#dep.dataset.log <- cbind(dep.dataset[,-c(skewed_variables)],lapply(dep.dataset[, c(skewed_variables)], log(x)))
#need to convert the values back to int/num
#dep.dataset.log <- cbind(dep.dataset.log[, c(1:8)],lapply(dep.dataset[, -c(1:8)], as.integer))

#############################################################################################

dep.dataset.log <- cbind(dep.dataset[,-c(skewed_variables)],log((dep.dataset[, c(skewed_variables)]) + 1))
plot_num(dep.dataset.log)
str(dep.dataset.log)
#Slight improvement but still a lot of 0 values exist, particularly for asset_savings 
#ent_animalstockrevenue

# I'm going to create datasets with just the people who are depressed and those who are not and see how
# their asset_savings and ent_animalstockrevenue etcis distributed, if there's no difference
# I will remove these variables because the clustering algorithm may be sensitive to the excessive
# skewness
#Depressed
depressed.farmers <- dep.dataset.log[og.dataset$depressed==1,]
plot_num(depressed.farmers)
str(depressed.farmers)

#Not depressed
not_depressed.farmers <- dep.dataset.log[og.dataset$depressed==0,]
plot_num(not_depressed.farmers)
str(not_depressed.farmers)

#Very interestingly, most of the numeric variables follow the same distribution among the
#depressed groups and non depressed groups - it could suggest these economic variables may not be
# the strongest indicators of depression afterall. But this can be further examined. The depressed group
# seem to have a little less education, are a little bit older, have less livestock, less durable assets etc.
# But only ever so slightly so. I will remove asset_savings, since most people have 0, depressed or not, 
# I will also remove, ent_animalstockrev and asset_phone. There are far too many 0s in both depressed
# And non depressed groups, although non depressed people proportionately speaking, are more likely 
# to have phones - but this is very slight and the majority don't have phones either

#Remove asset_savings, asset_phone and ent_animalstockrevenue
dep.dataset.log <- dep.dataset.log[,-c(11,12,20)]
str(dep.dataset.log)


#################################################################
#           A-i  Hierarchical Clustering: Gower
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
hc.a = hclust(gower.matrix, method="average")

plot(hc.w)
plot(hc.w2)
plot(hc.c)
plot(hc.a)


#########################################################
# A-ii Selecting the Optimal Method + no. of Clusters
#########################################################

# Will use the Dunn Index and Silhoutte Method Criteria
# By visual inspection of the dendograms, there appear to be anywhere between 
# 2-4, possibly 5 clusters which seem reasonably distinct from one another. So we will 
# test the internal validity of these number of clusters

library(fpc)
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum amount of clusters by 5
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result

#Optimal solution
stats.df.aggl <- cstats.table(gower.matrix, hc.w, 5)
stats.df.aggl

stats.df.aggl2 <- cstats.table(gower.matrix, hc.w2, 5)
stats.df.aggl2

stats.df.aggl3 <- cstats.table(gower.matrix, hc.c, 5)
stats.df.aggl3

stats.df.aggl4 <- cstats.table(gower.matrix, hc.a, 5)
stats.df.aggl4

#Table to compare solution 1, 2 and 4 since they are very similar
# with their dunn, silhouette and within ss values
clust1_label <- cutree(hc.w, k=3)
clust2_label <- cutree(hc.w2, k=3)
clust4_label <- cutree(hc.a, k=3)

#The first two solutiona have 100% agreement and aren't any different
table(clust1_label, clust2_label)

#The average linkage and ward linkage also have 100% agreement
table(clust1_label, clust4_label)

#By considering the silhouette, Dunn and within ss together
# 3 clusters seem to be the optimal choice and the best values for
# Dunn, silhouette and Within cluster ss come from wards and average,
# so we can choose either and we will go with wards from here

#################################################################
#           B-i   Clustering: PAM
#################################################################

silhouette.w <- c()
for(i in 2:20){
  
  data.pam <- pam(gower.matrix,
                 diss = TRUE,
                 k = i)
  
  silhouette.w[i] <- data.pam$silinfo$avg.width
  
}


#plot the different silhouette widths
plot(1:20, silhouette.w,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:20, silhouette.w)

#Again it looks like 3 is the optimal number of clusters

###############################################################
#         B-i: Compare to Ward's Cluster Solution
###############################################################

pam.3 <- pam(gower.matrix,
                diss = TRUE,
                k = 3)

table(clust1_label, pam.3$clustering)

#Compare using Adjusted Rand Index
library("flexclust")
randIndex(clust1_label, pam.3$clustering)

#The solutions are pretty similar, the adjusted Rand Index value is
#pretty high (0.931) and so 3 clusters are optimal and since solution
# 1 has a marginally higher silhoutte width than pam, I will choose 
# Hclust, gower dist and ward's linkage to analyse

#############################################################################################
#                 C - Model Based Clustering
#############################################################################################
library(mclust)
fitM <- Mclust(dep.dataset.log)

#It suggests the model EEv with 3 clusters

fitM
plot(fitM)


############################################################################################
#             D - Analyse Hierarchical Clustering Solution
############################################################################################

#I've chosen Hclustering to be the best in this case and will analyse that cluster to see what 
# insights I can get about depression and different variables

h1<- which(clust1_label == 1)
summary(dep.dataset.log[h1,])
str(og.dataset[h1,])
#The amount who are depressed is 65 vs 269 not depressed (24%)
table(og.dataset[h1,24])

h2<- which(clust1_label == 2)
summary(dep.dataset.log[h2,])
str(og.dataset[h2,])
#The amount who are depressed is 52 vs 314 not depressed (16.6%%)
table(og.dataset[h2,24])

h3<- which(clust1_label == 3)
summary(dep.dataset.log[h3,])
str(og.dataset[h3,])
#The amount who are depressed is 42 vs 241 not depressed (20.7%)
table(og.dataset[h3,24])

#Cluster 1 and 3 have higher levels of depression than the overal average of 16.8%
# Of the total sample population

#This is an analysis of their differences:
plot_num(dep.dataset.log[h1,])
plot_num(dep.dataset.log[h2,])
plot_num(dep.dataset.log[h3,])
###############################################################################################



###### GLMs ###########
#add the depressed column to the dataset to be used in the analysis
set.seed(729)
depression.dataset <- cbind(dep.dataset.log, og.dataset$depressed)
colnames(depression.dataset)[19] <- "depressed"

#Split the data into train, test and validation sets (60%, 20%, 20%) respectively
data <- sort(sample(nrow(depression.dataset), nrow(depression.dataset)*.6))
train<-depression.dataset[data,]

#Split remaining 40% of data into test and validation
remaining_data<-depression.dataset[-data,]

#use sampling without replacement
data <- sort(sample(nrow(remaining_data), nrow(remaining_data)*.5))
test<-remaining_data[data,]
validate<- remaining_data[-data,]

####################################################################################
#                   GLM 1: FEATURE SELECTION with Transformed Data
####################################################################################

# Part 1:
# I'm going to run logistic regression on the training dataset and see
# which variables are statistically significant
train.glm <- glm(depressed ~., data=train, family = binomial)
plot(train.glm)
summary(train.glm)
BIC(train.glm)

#Only asset_durable is statistically significant. Education, cons_ownfood
#cons_ed and ent_farmexpenses are not statistically significant but have 
#p values lower than 0.1. Perhaps these variables are more important when 
#it comes to predictions. The AIC is 638.84. BIC is 733.9547


#Part 2:
#Regsubsets to find the important variables: BIC
library(leaps)
plot(regsubsets(depressed ~., data = train, method = "exhaustive", nbest = 1))
#For the lowest BIc (6.3), only the intercept and edu is included, however this would
# make the model too simple in my opinion (I believe economic factors relating to current
# personal financial and home situations have to have some sort of effect on depression)
# the variables included in the highest bic are similar to the statistically
# significant ones above and the ones in the backward stepwise selection process below

#Part 3:
#Forward: AIC requires more variables
step.f <- (step(lm(depressed ~ ., data = train), trace = F, direction = "forward"))
summary(step.f)
AIC(step.f) #AIC is 618.5599
BIC(step.f) #BIC is 718.2071

#Backward
step.b <- (step(lm(depressed ~ ., data = train), trace = F, direction = "backward"))
summary(step.b)
AIC(step.b) #AIC is 597.542
BIC(step.b) #BIC is 633.7782

# Only education, asset_durable, cons_allfood, cons_ownfood, cons_ed are statistically
# significant. ent_farmexpenses was also included. 

# I will take three models (logistic regression) and compare them. The first one is with variables selected 
# bystep.b (it has similar statistically significant variables as output by the logistic regression
# model on all variables). The second is from reg_subsets with only the intercept and edcucation. The final will
# will include all variables

############################################################################################################
#RUN Logistic Regression on datasets

lreg.train1 <- glm(depressed ~ edu + asset_durable + cons_allfood + cons_ownfood + cons_ed + ent_farmexpenses, data = train, family = binomial)

lreg.train2 <- glm(depressed ~ edu, data = train, family = binomial)

lreg.train3 <- glm(depressed ~., data = train, family = binomial)

#Now use the above model to make some predictions
lreg.predict1 = predict(lreg.train1, type="response")

lreg.predict2 = predict(lreg.train2, type="response")

lreg.predict3 = predict(lreg.train3, type="response")

#Now evaluate performance using ROC curves and AUC
library(ROCR)
# ROC and Performance function

ROCRpred1 = prediction(lreg.predict1, train$depressed)
ROCRperf1 = performance(ROCRpred1, "tpr", "fpr")

ROCRpred2 = prediction(lreg.predict2, train$depressed)
ROCRperf2 = performance(ROCRpred2, "tpr", "fpr")

ROCRpred3 = prediction(lreg.predict3, train$depressed)
ROCRperf3 = performance(ROCRpred3, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf1)
plot(ROCRperf1, colorize=TRUE)
plot(ROCRperf1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

plot(ROCRperf2)
plot(ROCRperf2, colorize=TRUE)
plot(ROCRperf2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

plot(ROCRperf3)
plot(ROCRperf3, colorize=TRUE)
plot(ROCRperf3, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#The first model and last model seem to have a higher AUCs over the second which seems too simplistic and
# very close to simply random classification in terms of its ROC curve shape
# Either, way, there won't be high levels of accuracy in any of these models for correctly identifying depression

#Logically, I believe increasing the amount of true positives is the best approach 
#It would be more detrimental for someone with depression to go undiagnosed and untreated
# However, I don't want there to be too many false positives. I will try 0.2, 0.15 and 0.1
# and test their accuracy

# I will test each threshold and choose the best one
new.lreg1.predict <- ifelse(lreg.predict1 > 0.2,1,0)
cont.mat <- table(train$depressed, new.lreg1.predict)
accuracy <- (sum(diag(cont.mat))/sum(cont.mat))
accuracy

new.lreg1.predict2 <- ifelse(lreg.predict1 > 0.15,1,0)
cont.mat2 <- table(train$depressed, new.lreg1.predict2)
accuracy2 <- (sum(diag(cont.mat2))/sum(cont.mat2))
accuracy2

new.lreg1.predict3 <- ifelse(lreg.predict1 > 0.1,1,0)
cont.mat3 <- table(train$depressed, new.lreg1.predict3)
accuracy3 <- (sum(diag(cont.mat3))/sum(cont.mat3))
accuracy3


new.lreg1.predict4 <- ifelse(lreg.predict1 > 0.5,1,0)
cont.mat4 <- table(train$depressed, new.lreg1.predict4)
accuracy4 <- (sum(diag(cont.mat4))/sum(cont.mat4))
accuracy4


#The highest accuracy is for a threshold of 0.5 and it decreases as 
# the threshold lowers. Now to check if lowering the threshold actually
# increases the number of true positives:
cont.mat #lots of incorrect predictions for depression: 115 fp, 52 true positive
cont.mat2 #lots of incorrect predictions for depression: 310 fp vs 86 true positive
cont.mat3 #lots of incorrect predictions for depression: 496 fp vs 107 true positive
cont.mat4 #lots of incorrect predictions for depression:  3 fp vs 1 true positive


#Now with model 3
new.lreg3.predict <- ifelse(lreg.predict3 > 0.2,1,0)
cont3.mat <- table(train$depressed, new.lreg3.predict)
accuracy.m3 <- (sum(diag(cont3.mat))/sum(cont3.mat))
accuracy.m3

new.lreg3.predict2 <- ifelse(lreg.predict3 > 0.15,1,0) #294 fn vs 90 tp, performs the best
cont3.mat2 <- table(train$depressed, new.lreg3.predict2)
accuracy.m32 <- (sum(diag(cont3.mat2))/sum(cont3.mat2))
accuracy.m32

new.lreg3.predict3 <- ifelse(lreg.predict3 > 0.1,1,0)
cont3.mat3 <- table(train$depressed, new.lreg3.predict3)
accuracy.m33 <- (sum(diag(cont3.mat3))/sum(cont3.mat3))
accuracy.m33

new.lreg3.predict4 <- ifelse(lreg.predict3 > 0.5,1,0)
cont3.mat4 <- table(train$depressed, new.lreg3.predict4)
accuracy.m34 <- (sum(diag(cont3.mat4))/sum(cont3.mat4))
accuracy.m34


# Conclusion: this is not a very good model and in order to get higer levels of
# true positives, we need to accept more false positives but to an extent that seems
# quite unreasonable. The has better accuracy as thresholds of 0.5 and higher
# but this is only good for saying that people do not have depression. Otherwise,
# it typically misclassifies actually depressed people. And that is not the aim of
# this model. We want to be able to accurately diagnose depressed people
# The model with all parameters performs marginally better

# Going to test the model on test data
lreg.predict1.test <- predict(lreg.train1, test, type="response")
cont.mat.test <- table(test$depressed, lreg.predict1.test>0.5)
accuracy.test <- (sum(diag(cont.mat.test))/sum(cont.mat.test)) 
accuracy.test

lreg.predict1.test2 <- predict(lreg.train1, test, type="response")
cont.mat.test2 <- table(test$depressed, lreg.predict1.test2>0.15)
accuracy.test2 <- (sum(diag(cont.mat.test2))/sum(cont.mat.test2)) 
accuracy.test2

#Performs poorly on the test dataset as well


########################################################################################################

# Lasso, Ridge and Elastic Net: After this I'm going to alter the dataset again