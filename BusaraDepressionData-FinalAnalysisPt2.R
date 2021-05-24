# The previous models did not work on the dataset.
# While prediction accuracy was relatively high,
# this was only due to the large number of non depressed
# people correctly classified. A large part of the sample
# is not depressed anyway (only 17% suffer from depression)
# so that the models were not well trained. In this attempt, I have
# altered the dataset to combine economic variables into one
# I have put them into levels/factors and finally, I make sure that 
# the training set has a higher % of depressed people (slightly)
# more than the sample, to see if it will perform better that way
# by being exposed to more patients with depression

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

set.seed(7290) # set the seed

#read in the new dataset
og.data = read.csv("FinalDataset.csv", header=TRUE)
str(og.data)
##########################################################################################################
# EXPLORATORY ANALYSIS AND DATA PROCESSING

#I'm going to combine all the assets into one variable: total assets
# I'm going to combine all expenditure into one

#assets
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
new.data <- new.data[, -1]
str(new.data)
describe(new.data)

#education level
unique(new.data$edu)
summary(new.data$edu)
hist(new.data$edu) #most people have 8 years of education, so primary/secondary

#convert factor variables to type factor
new.data <- cbind(new.data[, -c(1,2,4,6,7,11,13)],lapply(new.data[, c(1,2,4,6,7,11,13)], as.factor))
str(new.data)

#look for skewed data
plot_num(new.data)
skewed_features <- find_skewness(new.data)
str(new.data[, c(skewed_features)])

#log transform to remove some of the skewness
new.data.log <- cbind(new.data[,-c(skewed_features)],log((new.data[, c(skewed_features)]) + 1))
plot_num(new.data.log)
str(new.data.log)

#there is an improvement, however ent_animal stock rev just has too many 0 values. 
# examine if there is a difference between these variables between depressed and non
# depressed users before removing

#Depressed
depressed.ppl <- new.data.log[og.data$depressed==1,]
plot_num(depressed.ppl)
str(depressed.ppl)

#Not depressed
not_depressed.ppl <- new.data.log[og.data$depressed==0,]
plot_num(not_depressed.ppl)
str(not_depressed.ppl)

#There appears to be very little difference between the two, especially for
# ent_animalstockrevenue

new.data.log <- new.data.log[,-11]

##########################################################################################################
# CLUSTERING: Hierarchical

#As last time, I will use, daisy and the gower dissimilarity measure

gower.mat <- daisy(new.data.log, metric = "gower")

#Use a variety of linkage methods: I want compact clusters
hclust.w = hclust(gower.mat, method="ward.D")
hclust.w2 = hclust(gower.mat, method="ward.D2")
hclust.c = hclust(gower.mat, method="complete")
hclust.a = hclust(gower.mat, method="average")

plot(hclust.w)
plot(hclust.w2)
plot(hclust.c)
plot(hclust.a)

#Now to determine the optimal type
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

stats.aggl <- cstats.table(gower.mat, hclust.w, 5)
stats.aggl

#Optimal solution with highest sil width, dunn and also lower within.ss than solution a
stats.aggl2 <- cstats.table(gower.mat, hclust.w2, 5)
stats.aggl2

stats.aggl3 <- cstats.table(gower.mat, hclust.c, 5)
stats.aggl3

stats.aggl4 <- cstats.table(gower.mat, hclust.a, 5)
stats.aggl4

##########################################################################################################
#Clustering: PAM

sil.w <- c()
for(i in 2:20){
  
  dat.pam <- pam(gower.mat,
                  diss = TRUE,
                  k = i)
  
  sil.w[i] <- dat.pam$silinfo$avg.width
  
}


#plot the different silhouette widths
plot(1:20, sil.w,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:20, sil.w)

#Here three is the optimal number of clusters (sil.w is 0.356)

#Compare PAM and HCLUST:

clust.w2 <- cutree(hclust.w2, k=4)


pam.3.clust <- pam(gower.mat,
             diss = TRUE,
             k = 3)

table(clust.w2, pam.3.clust$clustering)

#Compare using Adjusted Rand Index
library("flexclust")
randIndex(clust.w2, pam.3.clust$clustering)
#The results are very similar, since the silhouette and Dunn Index is higher for
#ward, we'll stick with that

###########################################################################################################

#Analyse the results from the optimal solution (4 clusters)

h1.w2<- which(clust.w2 == 1)
summary(new.data.log[h1.w2,])
str(og.data[h1.w2,])
#The amount who are depressed is 65 vs 272 not depressed (19.28%)
table(og.data[h1.w2,24])

h2.w2<- which(clust.w2 == 2)
summary(new.data.log[h2.w2,])
str(og.data[h2.w2,])
#The amount who are depressed is 52 vs 311 not depressed (14.33%)
table(og.data[h2.w2,24])

h3.w2<- which(clust.w2 == 3)
summary(new.data.log[h3.w2,])
str(og.data[h3.w2,])
#The amount who are depressed is 50 vs 266 not depressed (15.82%)
table(og.data[h3.w2,24])

h4.w2<- which(clust.w2 == 4)
summary(new.data.log[h4.w2,])
str(og.data[h4.w2,])
#The amount who are depressed is 26 vs 101 not depressed (20.47%)
table(og.data[h4.w2,24])

#Analyse to see if there's any differences among them
plot_num(new.data.log[h1.w2,])
plot_num(new.data.log[h2.w2,])
plot_num(new.data.log[h3.w2,])
plot_num(new.data.log[h4.w2,])
###### GLMs ###########
#add the depressed column to the dataset to be used in the analysis
set.seed(729)
depression.data <- cbind(new.data.log, og.data$depressed)
colnames(depression.data)[14] <- "depressed"

#Split the data into train, test and validation sets (60%, 20%, 20%) respectively
dat.data <- sort(sample(nrow(depression.data), nrow(depression.data)*.6))
dat.train<-depression.data[dat.data,]

table(dat.train$depressed) #17% depressed as in the overall sample, good representation in the training set

#Split remaining 40% of data into test and validation
dat.remaining_data<-depression.data[-dat.data,]

#use sampling without replacement
dat.data <- sort(sample(nrow(dat.remaining_data), nrow(dat.remaining_data)*.5))
test<-dat.remaining_data[dat.data,]
validate<- dat.remaining_data[-dat.data,]

#################################################################################
# Logistic Regression

train.fit <- glm(depressed ~., data=dat.train, family = binomial)
plot(train.fit)
summary(train.fit)
BIC(train.fit)

#Only education, total assets are statistically significant, current hh_children3 is almost
# statistically significant (p=0.06), i.e the more children at home currently

#Regsubsets to find the important variables: BIC
library(leaps)
plot(regsubsets(depressed ~., data = dat.train, method = "exhaustive", nbest = 1))

#Part 3:
#Forward: AIC requires more variables
step.f2 <- (step(lm(depressed ~ ., data = dat.train), trace = F, direction = "forward"))
summary(step.f2)
AIC(step.f2) #AIC is 619.5509
BIC(step.f2) #BIC is 710.1393

#Backward
step.b2 <- (step(lm(depressed ~ ., data = dat.train), trace = F, direction = "backward"))
summary(step.b2)
AIC(step.b2) #AIC is 603.77
BIC(step.b2) #BIC is 635.4792

#Will use backward and all variables
log.train1 <- glm(depressed ~ edu + hh_children + ent_farmexpenses + total_assetsUSD, data = dat.train, family = binomial)

log.train2 <- glm(depressed ~ ., data = dat.train, family = binomial)


#Now use the above model to make some predictions
log.predict1 = predict(log.train1, type="response")

log.predict2 = predict(log.train2, type="response")

library(ROCR)
# ROC and Performance function

ROCRpred.1 = prediction(log.predict1, dat.train$depressed)
ROCRperf.1 = performance(ROCRpred.1, "tpr", "fpr")

ROCRpred.2 = prediction(log.predict2, dat.train$depressed)
ROCRperf.2 = performance(ROCRpred.2, "tpr", "fpr")

plot(ROCRperf.1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

plot(ROCRperf.2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Same issue as before. No improvement


