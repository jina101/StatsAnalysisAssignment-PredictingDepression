#load in some useful packages
library(mlbench)
library(caret)
library(dplyr)
library(corrplot)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(funModeling) 
library(Hmisc)
library(cluster)



# Read in the dataset for exploratory analysis with 34 variables, 31 of which are 
# for predicting whether someone has depression or not
depression.data = read.csv("DataToAnalyse.csv", header=TRUE)
str(depression.data)

##################################################################################################
#Convert children, hhsize, hh_children, ent_ownfarm, ent_nonagbusiness to factor variables
# as they represent categorical above, below and average levels
depression.factor <- depression.data
depression.factor$children = as.factor(depression.factor$children)
depression.factor$hh_children = as.factor(depression.factor$hh_children)
depression.factor$hhsize = as.factor(depression.factor$hhsize)

str(depression.factor)


##################################################################################################
#                                       Variation in Dataset
##################################################################################################

#########################################
#DEMOGRAPHICS
#########################################
#Depression

#Use ggplot to visualise the distribution of some variables 
ggplot(data = depression.data) +
  geom_bar(mapping = aes(x = depressed), width=0.3,fill = c("#FF6666", "#1b98e0"),space=c(0.1,0.2))+
  ggtitle("Busara Data Proportion Depressed = 16.9%")+
  xlab("Depressed: 0 = No, 1 = Yes") +
  ylab("Frequency")+
  scale_x_continuous(breaks=c(0,1))+
  theme_bw()

#view in a table
table(depression.data$depressed)

#193 depressed (16.89% are depressed)
#Is this consistent across all groups?
#Is there any group that sees a higher 
# level of depression? If so, why? --> Clustering

#GENDER:
dev.off()
##ggplot view as barchart
ggplot(data = depression.factor) +
  geom_bar(mapping = aes(x = femaleres))

#the majority of this dataset is one gender. I still want to know
#if relatively speaking, rates of depression are higher among one gender
#over the other:

depression.gender <- depression.factor[,c(3,34)]
table(depression.gender)

#Gender 1: 95 total, 17.89% are depressed
#Gender 2: 1048 total, 16.79% are depressed

# Both genders have a similar level of depression which suggests
# gender is not significant: I won't use it in my analysis

#MARRRIAGE
ggplot(data = depression.factor) +
  geom_bar(mapping = aes(x = married), width=0.3,fill = c("#FF6666", "#1b98e0"),space=c(0.1,0.2))+
  ggtitle("Busara Data Proportion Married = 77.3%")+
  xlab("Married: 0 = No, 1 = Yes") +
  ylab("Frequency")+
  scale_x_continuous(breaks=c(0,1))+
  theme_bw()

depression.married <- depression.factor[,c(5,34)]
table(depression.married)

#Married 0: 260 total, 22.31% are depressed
#Gender 2: 883 total, 15.28% are depressed

#Keep in analysis: one group has a higher rate of depression

#CHILDREN. HH_ChILDREN, HHSIZE
depression.factor %>% 
  count(children)

depression.factor %>% 
  count(hh_children)

depression.factor %>% 
  count(hhsize)

depression.hhsize <- depression.factor[,c(7,34)]
table(depression.hhsize)

#1: 16.69% depressed
#2: 16.83% depressed
#3: 17.82% depressed

depression.hh_children <- depression.factor[,c(9,34)]
table(depression.hh_children)

#1: 16.71% depressed
#2: 15.56% depressed
#3: 20.56% depressed

depression.children <- depression.factor[,c(6,34)]
table(depression.children)

#1: 16.40% depressed
#2: 16.66% depressed
#3: 20.58% depressed


#Most families have between 0-5 children, but less than 2 are currently 
# living in the household. The majority of the households are are sized
# between 0-7. As expected, those with more children living at home than average 
# seem to have a higher level of depression. Those who have had more children
# also have higher levels of depression. Since children seem to have more of an effect
# I will omit household size from my analysis. I will leave both the children
# datasets as I would like to see if children moving out reduces depression rates,
# if not, then is it because children move out at a young age and not as adults?


#AGE
#dev.off()
ggplot(data = depression.factor) +
  geom_bar(mapping = aes(x = age), fill="#FF6666")+
  ggtitle("Busara Data Age of Respondents")+
  xlab("Age in Years") +
  ylab("Frequency")+
  scale_x_continuous(breaks=c(0,1))+
  theme_bw()

#Most people surveyed are between 20 and 50, the majority are 20-30 years of age
#It will be interesting to see how age impacts depression especially as this variable
# interacts with others

#############################
#Economics
#############################
#Gather all the economic data into one dataframe
depression.economic <- depression.factor[,c(10:34)]
str(depression.economic)

#Are any of them highly correlated and thus can we eliminate some of them?
economic.correlation <- cor(depression.economic[,-25])
corrplot(economic.correlation, method = "circle")

#Get the highly correlated economic values
highlycorr <- findCorrelation(
  economic.correlation,
  cutoff = 0.75,
  names = FALSE,
) #When name is set to true, it returns highly correlated variables' names

#Highly Correlated
#cons_nondurable and cons_allfood [presumably non durable exp is for food]
#durable_investment and asset_livestock
#non_durable investment with non ag_flow_cost and also ent_total_cost
#ent_nonag_flowcost with ent_animalstockrev

#Remove the highly correlated variables
depression.economic <- depression.economic[, -highlycorr]
str(depression.economic)

### Plot these numeric values
plot_num(depression.economic)

#It looks like despite removing columns that were over 75% 0s,
#non ag revenue and ent_total cost still have a substantial
# amount of 0 values. I feel as if the other
#economic values will suffice to help me understand if those
# who are richer, spend slighly more on education etc (education in Kenya is free 
#for primary and secondary education) and thus I'll remove these two variables too

depression.economic <- depression.economic[, -c(13,17)]
str(depression.economic)

##########################################
# Combine Both sets of data for analysis:
##########################################

#Demographic Data without femaleres and hhsize
depression.demographic <- depression.factor[,c(1,2,4,5,6,8,9)]

depression.combined <- cbind(depression.demographic, depression.economic)
str(depression.combined)

# The dataset is now down to 21 variables, economic and demographic.
# The other 3 are depressed, survey id and village id
# Now for some summary statistics for each of the variables we'll use for predictions
summary(depression.combined[,-c(1,2)])
describe(depression.combined[,-c(1,2)])

#convert depressed to factor for pairs function
depression.combined$depressed = as.factor(depression.combined$depressed)

#Use pairs function to create different scatterplots to examine any relationships between variables
pairs(depression.combined[,4:7],col=depression.combined$depressed)
pairs(depression.combined[,8:11],col=depression.combined$depressed)
pairs(depression.combined[,12:15],col=depression.combined$depressed)
pairs(depression.combined[,16:21],col=depression.combined$depressed)

#Visualise one example
depression.combined %>%
  ggplot(aes(fs_adskipm_often,cons_ownfood, color=depressed)) +
  geom_point(alpha=0.5, size=2) +
  labs(y="Non Durable Investment", x="Adults Often Skipping Meals", subtitle="Busara Depression Dataset")

#save final dataset tp CSV
write.csv(depression.combined,"FinalDataset.csv")