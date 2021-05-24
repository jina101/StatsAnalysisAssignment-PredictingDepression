# load data
library(ROCR)
library(Amelia) #make sure there are no missing data
library(mlbench)

dep.dat <- read.csv("Full_Data.csv", header=TRUE)

#replace Nas with 0
dep.dat[is.na(dep.dat)] <- 0

#check using a missing map to make sure there is no missing data
missmap(dep.dat, col=c("blue", "red"), legend=FALSE)

str(dep.dat)
dep.dat <- dep.dat[,-c(1,2,3)]


#Run a GLM on it

#Part 3:
#Forward: AIC requires more variables
step.f.2 <- (step(lm(depressed ~ ., data = dep.dat), trace = F, direction = "forward"))
summary(step.f.2)
AIC(step.f.2) #AIC is 619.5509
BIC(step.f.2) #BIC is 710.1393

#Backward
step.b.2 <- (step(lm(depressed ~ ., data = dep.dat), trace = F, direction = "backward"))
summary(step.b.2)
AIC(step.b.2) #AIC is 603.77
BIC(step.b.2) #BIC is 635.4792


log.pred = predict(step.b.2, type="response")
ROCRp1 = prediction(log.pred, dep.dat$depressed)
ROCRp1 = performance(ROCRp1, "tpr", "fpr")
plot(ROCRp1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


#
