#Read in data

set.seed(7290) # set the seed

#read in the new dataset
og.data = read.csv("FinalDataset.csv", header=TRUE)
str(og.data)

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


#Gonna add income to the datatset: Revenue - Cost
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


##############################################################################################
# GLM
#add depressed column to data
depression.da <- cbind(new.data.log, og.data$depressed)
str(depression.da)
colnames(depression.da)[14] <- "depressed"

train.fit1 <- (step(lm(depressed ~ ., data = depression.da), trace = F, direction = "backward"))
summary(train.fit1)




#Now use the above model to make some predictions
predict1 = predict(train.fit1, type="response")

library(ROCR)
# ROC and Performance function

ROCRpre1 = prediction(predict1, depression.da$depressed)
ROCRperf.1 = performance(ROCRpre1, "tpr", "fpr")


plot(ROCRperf.1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# Try splitting the data into training and set sets so that the training set has a higher proportion
# of depressed users


#split the data into depressed and non depressed:
dat.depressed <- depression.da[og.data$depressed==1,]

dat.not_depressed <- depression.da[og.data$depressed==0,]

str(dat.not_depressed)  
  
#Gonna do an 65/35 split, but I want a higher proportion of depressed people in the train set

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

#Now run a glm model and see how the ROC performance is

train.fit2 <- (glm(depressed ~ ., data = train), trace = F, direction = "forward"))
summary(train.fit2)


#Now use the above model to make some predictions
predict2 = predict(train.fit2, type="response")

# ROC and Performance function

ROCRp = prediction(predict2, train$depressed)
ROCRpe = performance(ROCRp, "tpr", "fpr")


plot(ROCRpe, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
