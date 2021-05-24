#########################################################################################
################### Sec.1 FINAL CODE FOR THE REPORT AND ANALYSIS ########################

#majority of the code developed with help from previous labs, lectures and rhelp files

#This section includes the code that I want to refer back to when doing the
#report. It starts with a little bit of data preparation, Cluster Analysis, PCA
# and GLMs. The data is not modified too  much beyond a log transform to normalise,
# and removing some more variables with excess 0s. 

# In other scripts, further modifcations to the data were made and tested to see if there would be any
# sort of improvement. This included grouping age into different levels, 
# combining all asset, income and expedeniture variables into one each and developing
# a training and test set which included a higher percentage of people with 
# depression to see if a more accurate model that could correctly classify 
# depression could be made. Unfortunately, none of these attempts yielded
# an improved model but they are evidence to support that these algorithms and 
# linear models are not overly effective for this dataset. The code is included after
# this section to highlight the process and my thinking.

# In conclusion, whilst clear clusters are formed and the GLMs have an accuracy of
# 80% overall - these don't provide any type of insight into who has depression and 
# who doesn't. No one variable has an obvious nor strong impact on depresion being a 
# positve outcome, hence the ability to correctly classify non depressed patients, but
# not depressed ones.


# Load some packages. Further packages are loaded within the code
# in the necessary section
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
summary(og.dataset) #print summary statistics

#check how how many are depressed
table(og.dataset$depressed) #Only 193 are depressed in the sample (approx 17%)

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

#log transform
dep.dataset.log <- cbind(dep.dataset[,-c(skewed_variables)],log((dep.dataset[, c(skewed_variables)]) + 1))
plot_num(dep.dataset.log)
str(dep.dataset.log)
#Slight improvement but still a lot of 0 values exist, particularly for asset_savings 
#ent_animalstockrevenue

# I'm going to create datasets with just the people who are depressed and those who are not and see how
# their asset_savings and ent_animalstockrevenue etc is distributed, if there's no difference
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
# seem to have a little less education, are a little bit older (but only very slightly), have less livestock, less durable assets etc.
# However, those differences are quite slight. I will remove asset_savings, since most people have 0, whether they are depressed or not, 
# I will also remove, ent_animalstockrev and asset_phone. There are far too many 0s in both depressed
# And non depressed groups, although non depressed people proportionately speaking, are more likely 
# to have phones - but this is very slight and the majority who are non depressed still don't have a phone.
# I don't want to remove too many variables but it is looking like there won't be much insight as there
# is very marginal difference.

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
#Create a fuction to easily take the data related to clusters and output is as a table
#instead of having to do each one individually everytime
#Function code based on: Anastasia Reusova Apr 2018-Hierarchical Clustering on Categorical Data in R, TowardsDataScience
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
# I am capping the maximum amount of clusters by 5, even though it looks like there may be 3 on the dendograms
# just to ensure 3 is the best choice

stats.df.aggl <- cstats.table(gower.matrix, hc.w, 5) #chosen solution to analyse
stats.df.aggl

stats.df.aggl2 <- cstats.table(gower.matrix, hc.w2, 5)
stats.df.aggl2

stats.df.aggl3 <- cstats.table(gower.matrix, hc.c, 5)
stats.df.aggl3

stats.df.aggl4 <- cstats.table(gower.matrix, hc.a, 5)
stats.df.aggl4

#three clusters seems to be the best choice/a good choice in all scenarios

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
############################################################################
#create a vector to store average silhouette widths - I prefer using it to 
# select clusters

silhouette.w <- c()
for(i in 2:20){
  
  data.pam <- pam(gower.matrix, diss = TRUE, k = i)
  
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

#pam with 3 clusters
pam.3 <- pam(gower.matrix, diss = TRUE, k = 3)

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
library(mclust) #load the library
fitM <- Mclust(dep.dataset.log)

#It suggests the model EEv with 3 clusters is best using BIC
fitM
plot(fitM)

#I will use this result as further
# evidence for a three cluster solution

############################################################################################
#             D - Analyse Hierarchical Clustering Solution
############################################################################################

#I've chosen Hclustering to be the best in this case and will analyse that cluster to see what 
# insights I can get about depression and different variables

#cluster 1
h1<- which(clust1_label == 1)
summary(dep.dataset.log[h1,])
str(og.dataset[h1,])
#The amount who are depressed is 65 vs 269 not depressed (19.46%)
table(og.dataset[h1,24])

#cluster 2
h2<- which(clust1_label == 2)
summary(dep.dataset.log[h2,])
str(og.dataset[h2,])
#The amount who are depressed is 52 vs 314 not depressed (14.21%)
table(og.dataset[h2,24])

#cluster 3
h3<- which(clust1_label == 3)
summary(dep.dataset.log[h3,])
str(og.dataset[h3,])
#The amount who are depressed is 42 vs 241 not depressed (17.15%)
table(og.dataset[h3,24])

#Cluster 1 and 3 have higher levels of depression than the overal average of 16.8%
# Of the total sample population. Cluster one seems to consist of people
# with less income/assets and more children at home. Cluster three seem a little
# more educated than 2. They spend a little more on education but have higher depression 
# levels than 2. This will be further explored in the report

#analyse numerical variables visually:
plot_num(dep.dataset.log[h1,])
plot_num(dep.dataset.log[h2,])
plot_num(dep.dataset.log[h3,])
###############################################################################################


######################################################################################################################
#                     PRINCIPAL COMPONENT ANALYSIS
######################################################################################################################

# PCA: I have an idea as to some of the trends variables have when in groups with
# high levels vs low levels of depression. I want to see if doing PCA will
# bring out any further insights. Clustering could be done on the PCA data, however
# this practice is usually for efficiency purposes with extremely large
# datasets and since this dataset isn't too big, it is not necessary. I still want to 
# see if there is any improvement, as distance measures are not great when it 
# comes to data with high dimensionality. So I will run pam clustering on pca and 
# compare it to hclust from above to see how it perfoms and also with pam without pca

#Code modified from r help and also: Statistical tools for high-throughput data analysis site (FAMD page)
#load the necessary packages for pca with mixed variables
library(FactoMineR)
library(factoextra)

str(dep.dataset.log)

# I need to scale the continuous variables
new.data.log.scale <- lapply(dep.dataset.log[,c(9:17)], scale)
str(new.data.log.scale)
#convert to numeric
new.data.log.scale <- lapply(new.data.log.scale, as.numeric)
str(new.data.log.scale)

#combine with other variables to make a full dataset that is scaled
scaled.data <- cbind(new.data.log.scale, dep.dataset.log[, -c(9:17)])
str(scaled.data)

#now to conduct pca analysis (will allow for 9 dimensions)
data.pca <- FAMD(scaled.data, ncp=9, graph=FALSE) #graph set to true will create a pca plot 
data.pca

#get the eigen values to use for analysis
eig.val <- get_eigenvalue(data.pca)
head(eig.val)

#create a scree plot to see how much variance each dim accounts for
fviz_screeplot(data.pca)
?fviz_screeplot()

#get the variables
var <- get_famd_var(data.pca)
var

# Coordinates of variables
head(var$coord)
# Cos2: quality of representation on the factore map
head(var$cos2)
# Contributions to the  dimensions
head(var$contrib)

#coordinates of indiviuals for clustering
ind <- data.pca$ind
head(ind$coord)


# Plot of variables
fviz_famd_var(data.pca, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(data.pca, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(data.pca, "var", axes = 2)


# Analysis of Quantitative Variables
quanti.var <- get_famd_var(data.pca, "quanti.var")
quanti.var

fviz_famd_var(data.pca, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

fviz_famd_var(data.pca, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)

# A little insight on how the variables are related to eachother, but nothing very new
# PC1 has a positive contribution from certanin variables (must be the component that
# determines people who are better off than others - there is no opposing signs. Additionally
# the rest of the components are also all positively contributed to by the variables.)

#I want to try clustering on PCA data on the new individual coordinates
#plot the results from the three or four clusters
pca.pam <- pam(ind$coord, 4)
plot(ind$coord[,1:2],col=factor(pca.pam$cluster))

pca.pam2 <- pam(ind$coord, 3)
plot(ind$coord[,1:2],col=factor(pca.pam2$cluster))

#get cluster information
pca.pam$clusinfo
pca.pam2$clusinfo

#compare to pam without pca
table(pam.3$clustering, pca.pam$clustering) # very similar results
randIndex(pam.3$clustering, pca.pam$clustering) #very high adjusted rand index

table(pam.3$clustering, pca.pam2$clustering)
randIndex(pam.3$clustering, pca.pam2$clustering) #very high adjusted rand index

#compare to hierarchical clustering
table(clust1_label, pca.pam$clustering)
randIndex(clust1_label, pca.pam$clustering)

table(clust1_label, pca.pam2$clustering)
randIndex(clust1_label, pca.pam2$clustering)

#In both cases, even with pca, the results are very, very similar. PAM without PCA is
# very similar to hclust with gower and ward, and Pam with PCA is very similar to PAM without. 
# there is not much improvement or extra insight, but it helps confirm the idea that there are 3 clusters.
# further analysis shows that one cluster is full of people with no assets or who had 0 as 
# their answers for those monetary factors. However, through further analysis, conducted 
# later on in this script using glms, simple models with only education etc (with minimal 0s)
# does not make a good model for classifying depression and is akin to random classification.

######################################################################################################################

## GENERAL LINEAR MODELS

#add the depressed column to the dataset to be used in the analysis
set.seed(729)
depression.dataset <- cbind(dep.dataset.log, og.dataset$depressed)
colnames(depression.dataset)[19] <- "depressed"

#Split the data into train, test and validation sets (60%, 20%, 20%) respectively
data <- sort(sample(nrow(depression.dataset), nrow(depression.dataset)*.6))
train<-depression.dataset[data,]

table(train$depressed) #17% depressed as in the overall sample, good representation in the training set

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
# significant. ent_farmexpenses was also included with backward selection

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

# Plot ROC curves
plot(ROCRperf1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

plot(ROCRperf2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

plot(ROCRperf3, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#The first model and last model seem to have a higher AUCs over the second which seems too simplistic and
# very close to simply random classification in terms of its ROC curve shape
# Either, way, there won't be high levels of accuracy in any of these models for correctly identifying depression

#Choosing threshold
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
# quite unreasonable. There is better accuracy at thresholds of 0.5 and higher
# but this is only good for saying that people do not have depression. Otherwise,
# it typically misclassifies actually depressed people. And that is not the aim of
# this model. We want to be able to accurately diagnose depressed people
# The model with all parameters included performs marginally better

# Going to test the model on test data
lreg.predict1.test <- predict(lreg.train1, test, type="response")
cont.mat.test <- table(test$depressed, lreg.predict1.test>0.5)
accuracy.test <- (sum(diag(cont.mat.test))/sum(cont.mat.test)) 
accuracy.test

lreg.predict1.test2 <- predict(lreg.train1, test, type="response")
cont.mat.test2 <- table(test$depressed, lreg.predict1.test2>0.15)
accuracy.test2 <- (sum(diag(cont.mat.test2))/sum(cont.mat.test2)) 
accuracy.test2

#Performs poorly on the test dataset as well. The first one did not 
# correctly find positives for depression. The second one does but with
# far lower accuracy


########################################################################################################
# GLMS: REGULARISATION WITH LASSO< RIDGE AND ELASTIC NET
# code modified from lab 7 and related notes

library(glmnet)
# Lasso, Ridge and Elastic Net: After this I'm going to alter the dataset again

#create a predictor matrix and also an outcome variable for each of the above sets
train$depressed <- as.factor(train$depressed)
str(train)
#Rename levels
levels(train$depressed) <- c("no", "yes")


train.X <- data.matrix(within(train, rm(depressed)))
val.X <- data.matrix(within(validate, rm(depressed)))
test.X <- data.matrix(within(test, rm(depressed)))
train.y <- train$depressed
val.y <- validate$depressed
test.y <- test$depressed

# RIDGE AND LASSO
#Tune the lamda for both the ridge and lasso methods using cross-validation
cvridge <- cv.glmnet(train.X, train.y, family="binomial", alpha=0, nlambda=20, type.measure="auc")
cvlasso <- cv.glmnet(train.X, train.y, family="binomial", alpha=1, nlambda=20, type.measure="auc")

#create the final models using the best lamda from above
ridgemod <- glmnet(train.X, train.y, family="binomial", alpha = 0, lambda = cvridge$lambda.1se)
lassomod <- glmnet(train.X, train.y, family="binomial", alpha = 1, lambda = cvlasso$lambda.1se)

#see the details of the models
ridgemod
lassomod

##############################################################################################################

#ELASTIC NET
#This section pre-standardises the predictor matrix
train.stdX <-scale(train.X)

library(caret) # load the caret package

# Set the control for training
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              search = "random",
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              verboseIter = TRUE)

# Start training the model and look for the best lamda and alpha
elastic_grid <- train(train.stdX, train.y,
                      method = "glmnet",
                      tuneLength = 25,
                      trControl = train_control,
                      metric= "ROC",
                      family = "binomial",
                      standardize = FALSE)

#use the final and best lamda and alpha for the model
elasticmod <- glmnet(train.X, train.y, family="binomial", alpha = elastic_grid$bestTune$alpha, lambda = elastic_grid$bestTune$lambda)
elasticmod

#Can view the final betas for each of the models:
Intercepts <- cbind(ridgemod$a0,lassomod$a0,elasticmod$a0)
Coefs <- cbind(ridgemod$beta,lassomod$beta, elasticmod$beta)
Betas <-rbind(Intercepts, Coefs)
rownames(Betas)[1] = "(Intercept)"
colnames(Betas) = c("Ridge", "Lasso", "Elastic Net")
Betas

#####################################################################################################
 # start Properly Building the models and using it to predict
#####################################################################################################
#use the models for predicting
fit.ridge <- predict(ridgemod, val.X, type="response")

#Use the accuracy method to visualise
cutoffs <- seq(min(fit.ridge),max(fit.ridge),(max(fit.ridge)-min(fit.ridge))/100)
accuracy <- NULL

for (i in seq(along = cutoffs)){
  prediction <- ifelse(fit.ridge >= cutoffs[i], 1, 0) #Predicting for cut-off
  accuracy <- c(accuracy,length(which(val.y ==prediction))/length(prediction)*100)
}

plot(cutoffs, accuracy, pch =19,type='l',col= "steelblue",
     main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")


cutoffs <- seq(min(fit.ridge),max(fit.ridge),(max(fit.ridge)-min(fit.ridge))/100)
accuracy <- NULL

for (i in seq(along = cutoffs)){
  prediction <- ifelse(fit.ridge >= cutoffs[i], 1, 0) #Predicting for cut-off
  accuracy <- c(accuracy,length(which(val.y ==prediction))/length(prediction)*100)
}

plot(cutoffs, accuracy, pch =19,type='l',col= "steelblue",
     main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")

#######################################################################################################

library(pROC) #load pROC library

#fit lasso and elastic models
fit.lasso <- predict(lassomod, val.X, type="response")
fit.elastic <- predict(elasticmod, val.X, type="response")

#get the best thresholds
thresh.r <- coords(roc(val.y, as.vector(fit.ridge)), "best", best.method="youden", transpose=TRUE, ret="threshold")
thresh.l <- coords(roc(val.y, as.vector(fit.lasso)), "best", best.method="youden", transpose=TRUE, ret="threshold")
thresh.e <- coords(roc(val.y, as.vector(fit.elastic)), "best", best.method="youden", transpose=TRUE, ret="threshold")

#use the test data for predicting
final.r <- predict(ridgemod, test.X, type="response")
final.l <- predict(lassomod, test.X, type="response")
final.e <- predict(elasticmod, test.X, type="response")


#use a confusion matrix to analyse how well each model has performed
class.ridge <- as.factor(ifelse(final.r <= thresh.r, 0, 1))
table(class.ridge, test.y)

class.lasso <- as.factor(ifelse(final.l <= thresh.l, 0, 1))
table(class.lasso, test.y)

class.elastic <- as.factor(ifelse(final.e <= thresh.e, 0, 1))
table(class.elastic, test.y)

#None of these models perform well at all. There are
# far more false negatives than there are true positives
# elastic net and ridge seem to at least do better than lasso
# as such there is not much point going ahead with these methods

#Use AIC to check models:
tLL <- lassomod$nulldev - deviance(lassomod)
k <- lassomod$df
n <- lassomod$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICc

tL <- ridgemod$nulldev - deviance(ridgemod)
kl <- ridgemod$df
nl <- ridgemod$nobs
AICcr <- -tL+2*kl+2*kl*(kl+1)/(nl-kl-1)
AICcr

eTLL <- elasticmod$nulldev - deviance(elasticmod)
kle <- elasticmod$df
nle <- elasticmod$nobs
AICce <- -eTLL+2*kle+2*kle*(kle+1)/(nle-kle-1)
AICce

#According to AIC ridge performs best (it has the higher accuracy rate)
# but like the others, it still performs very poorly in correctly 
# classifying people with depression




########################################################################################################
## Section: Code Snippets from Other Attempts


# Altering the dataset to have less variables and hope by combining
# income, expenditure variables etc, there may be less skewness and 
# variables overall

set.seed(7290) # set the seed

#read in the new dataset
og.data = read.csv("FinalDataset.csv", header=TRUE)
str(og.data)

#add assets together
og.data$total_assetsUSD <- og.data$asset_livestock + og.data$asset_durable + og.data$asset_phone + og.data$asset_phone + og.data$asset_savings
str(og.data) #note land is in acres, not USD

#food consumption - own food consumption will be assumed to be the expenditure on food
og.data$food_exp <- og.data$cons_allfood - og.data$cons_ownfood

#add expenditure
og.data$total_expenditureUSD <- og.data$cons_ed + og.data$cons_social + og.data$cons_other + og.data$food_exp
str(og.data)

#Remove the old variables and non predictor variables
new.data <- og.data[, -c(1,2,8,9,10,11,13,14,15,16,17,24,26)]
str(new.data)

#group age into a range
AgeRange <- c()
rows <- nrow(new.data)

for(i in 1:rows)
{
  age=new.data[i,1]
  if(age>=29 && age<40)
  {
    AgeRange[i]=1
  }
  else if(age>=40 && age<50)
  {
    AgeRange[i]=2
  }
  else if(age>=50 && age<60)
  {
    AgeRange[i]=3
  }
  else if(age>=60 && age<70)
  {
    AgeRange[i]=4
  }
  else
  {
    AgeRange[i]=5
  }
  
}

new.data <- cbind(new.data, AgeRange)
new.data <- new.data[, -c(1,9)]
str(new.data)
describe(new.data)


#Gonna add income to the datatset from old, full dataset: Revenue - Cost
dep.dat <- read.csv("Full_Data.csv", header=TRUE)
#replace Nas with 0
dep.dat[is.na(dep.dat)] <- 0

#Sum revenue:
dep.dat$totalrevenue <- dep.dat$ent_nonag_revenue + dep.dat$ent_farmrevenue + dep.dat$ent_animalstockrev
#Less costs
dep.dat$incomeUSD <- dep.dat$totalrevenue - dep.dat$ent_total_cost

new.data <- cbind(new.data, dep.dat$incomeUSD)
str(new.data)

#education level
unique(new.data$edu)
summary(new.data$edu)
hist(new.data$edu) #most people have 8 years of education, so primary/secondary

#convert factor variables to type factor
new.data <- cbind(new.data[, -c(1,2,4,6,7,10,13)],lapply(new.data[, c(1,2,4,6,7,10,13)], as.factor))
str(new.data)


#some income variables are going to be negative, set those to 0
for(i in 1:nrow(new.data))
{
  income <- new.data[i,7]
  if(income<0)
  {
    new.data[i,7] = 0
  }
}

str(new.data)

plot_num(new.data)
skewed_features <- find_skewness(new.data)
str(new.data[, c(skewed_features)])

#log transform to remove some of the skewness
new.data.log <- cbind(new.data[,-c(skewed_features)],log((new.data[, c(skewed_features)]) + 1))
plot_num(new.data.log)
str(new.data.log)

#remove animal stock rev
new.data.log <- new.data.log[,-10]



# The hierarchical clustering, pam and glm code is very similar to the previous section. 
# There were no noticeable improvements



# code snippet to have more depressed respondents in training set
#Split the data into train and test I want a higher proportion of depressed people in the train set
#after splitting the dataset into dat.depressed (everyone who was depressed) and
# dat.not_depressed, I tried allocated the data objects in such a way that 
# more depressed people (than 17% were in the sample)

#gonna put 90% of depressed data into train
dt = sort(sample(nrow(dat.depressed), nrow(dat.depressed)*.9))
train<-dat.depressed[dt,]
test<-dat.depressed[-dt,]

#gonna put 60% of the non depressed data into train
dt2 = sort(sample(nrow(dat.not_depressed), nrow(dat.not_depressed)*.6))
train.bind<-dat.not_depressed[dt2,]
test.bind<-dat.not_depressed[-dt2,]

train <- rbind(train, train.bind)
test <- rbind(test, test.bind)

str(train)
str(test)

#shuffle the dataset
set.seed(42)
rows <- sample(nrow(train))
train <- train[rows, ]

table(train$depressed)
table(test$depressed)

# again the glm code was the same, and it did not improve results that much
# I also ran the analysis on the full original dataset with 75 variables - the results
# were no different