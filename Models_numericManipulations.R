########################################################################
# Dataset: Job Change of Data Scientists                               #
# Subject: HR Analytics                                                # 
# *Extra*                                                              #
# Models Part  -   Numeric   manipulation                              #
# Version: 4.2                                                         #
# Author:  Alon Krasnitsky & Rotem Levi                                #
# Date: 20.2.2021                                                      #
########################################################################

#############
# Packages  #
#############
library(randomForest) # Package for random Forest
library(tree) # Package for decision trees
library(ggplot2) # Graphics
library(randomForest) # Random Forest
library(caTools) # Random Split Train&Test set
library(class) # Impotent
library(gmodels) # KNN & CV Models


###############
# Import Data #
###############
# import data and # replace blank with na values
df = read.csv("D:/R-Project/aug_train.csv" , na = c("", "NA", "N/A"))
str(df)

#####################
# Data manipulation #
#####################

# fix company size mistake
df$company_size[df$company_size =="10/49"] <- "10-49"

# experience
df$experience = factor(df$experience, levels = c("<1","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20",">20"))
levels(df$experience)

# last_new_job
df$last_new_job[df$last_new_job =="never"] <- "0"
df$last_new_job = factor(df$last_new_job, levels = c("0","1","2","3","4",">4"))
levels(df$last_new_job)

# relevent_experience
df$relevent_experience =  as.factor(df$relevent_experience)

# education_level
df$education_level = factor(df$education_level, levels = c("Primary School","High School", "Graduate", "Masters", "Phd"))
levels(df$education_level)

## company_type ##
df$company_size = factor(df$company_size, levels = c("<10","10-49","50-99","100-500", "500-999", "1000-4999","5000-9999","10000+"))
levels(df$company_size)

## gender ##
df$gender = factor(df$gender)
## enrolled_university ##
df$enrolled_university = factor(df$enrolled_university)
## relevent_experience ##
df$relevent_experience = factor(df$relevent_experience)
## major_discipline ##
df$major_discipline = factor(df$major_discipline)
## company_type ##
df$company_type = factor(df$company_type)
## target ##
df$target = factor(df$target)
## city ##
df$city = factor(df$city)

##################
#     Models     #
##################
##  model function  ##

confusion_Matrixs = function(confusion) {
  
  #This function evaluates data presicion,sensitivity ansd F-Score "
  
  #Parameters :confusionMatrix
  #Output:     NoneType Info about model
  
  TP = confusion[4]
  TN = confusion[1]
  FP = confusion[2]
  FN = confusion[3]
  
  accuracy = round((TP+TN)/(TP+TN+FP+FN),4)
  sensitivity = round(TP/(TP+FN),4)
  specificity = round(TN/(TN+FP),4)
  F1Score = round((2*TP)/(2*TP+FP+FN),4)
  PPV = TP/(TP+FP)
  NPV = TN/(TN+FN)
  
  print(confusion)
  print(c("accuracy:", round(accuracy,4)))
  print(c("sensitivity:", round(sensitivity,4)))
  print(c("specificity:", round(specificity,4)))
  print(c("F1Score:", round(F1Score,4)))
  print(c("PPV:", round(PPV,4)))
  print(c("NPV:", round(NPV,4)))
  return(accuracy)
}

## pre - modeling ##
# data without NA's & problematic colums (gender, major, company size, company type)
newdata = df[-c(4,8,10,11)]
newdata = na.omit(newdata)
nrow(newdata)
newdata = newdata[,-1]

#number of NA in each colum
sapply(df, function(x) sum(is.na(x)))

### convert categorical to numeric
for (i in 1:7) {
  if(i==2){
    i = i+1
  }else{
    newdata[,i] = as.integer(newdata[,i])
  }
}
summary(newdata)

#newdata_numeric = newdata[,c(2,6,7,8,9)]
#newdata_categor = newdata[,c(1,3,4,5,9)]


## Splitting the dataset into the Training set and Test set  ##
set.seed(123)
split = sample.split(newdata$target, SplitRatio = 0.8)
training_set = subset(newdata, split == TRUE)
test_set = subset(newdata, split == FALSE)

## Logistic Regration ##

# after check the city not good to predict the 0 -> so we take him down
logistic = glm(target ~ ., data = training_set, family = binomial)

# Deatails about Logistic Regression
summary(logistic)
OR =exp(logistic$coefficients) # Odds Ratio
#OR

# Midul
test.predictions = predict(logistic, newdata = test_set , type = "response")

predictions = rep("0",length(test_set$target))
# After some checks we choos cutoff 0.4
predictions[test.predictions>0.4] = "1"


confusion_logistic = table(predictions,test_set$target)  # Testing confusion matrix
confusion_logistic
accuracy_LR = confusion_Matrixs(confusion_logistic)


## END - Logistic Regration ##
#######################################################################
## Srart - Desition Tree ##

# Fitting Decision Tree Classification to the Training set

tree = tree(target ~. , data = training_set )

summary(tree)

plot(tree)
text(tree)

# Predicting the Test set results
test.predictions = predict(tree, newdata = test_set, type = 'class')

# Making the Confusion Matrix
confusion_tree = table(test.predictions,test_set$target)
confusion_tree
accuracy_Tree = confusion_Matrixs(confusion_tree)

## END - Desition Tree ##
######################################################################
## START - Random Forest##
# we choose mtry 1 after some tests
Random_forest = randomForest(target ~ .-city , data = training_set,ntree=300, mtry = 1 ,type = 'class')
test.predictions = predict(Random_forest, newdata = test_set)

summary(Random_forest)

varImpPlot(Random_forest)
importance(Random_forest)

# Making the Confusion Matrix
confusion_RF = table(test_set$target, test.predictions)
confusion_RF
accuracy_RF = confusion_Matrixs(confusion_RF)

## END - Random Forest##
#############################################################
## KNN ##

# Min-Max : X-min / range
normalize = function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

# nirmul function on numeric colums
# ignore categorical colums -> not good for KNN
newdata.N = sapply(newdata[,c(2,6,7,8)], normalize) # applying normalize on each numerical variable
newdata.N = as.data.frame(newdata.N) # Making a data frame from the matrix

summary(newdata.N)

## Splitting the dataset into the Training set and Test set  ##

train.x = subset(newdata.N, split == TRUE)
test.x = subset(newdata.N, split == FALSE)
train.y = subset(newdata$target, split == TRUE)
test.y = subset(newdata$target, split == FALSE)

# Cross Validation to choose the value of K.
# Using K-Fold Cross Validation we will choose the value of K for KNN
accuracy.cv = rep(0,23) # 24=length(seq(from = 3, to = 51, by = 2))
count = 1
for (i in seq(from = 3, to = 51, by = 2))
{
  knnCV = knn.cv(train = train.x, cl = train.y, k = i)
  TBL = table(predicted = knnCV, actual = train.y)
  accuracy.cv[count] = (TBL[4]+TBL[1]) / length(train.y)
  count = count+1
}

sequence = seq(from = 3, to = 51, by = 2)
plot(accuracy.cv~sequence, type = "b", xlab = "K", ylab  = "CV Accuracy")
points(sequence[which.max(accuracy.cv)],max(accuracy.cv),col="red", pch = 16)

max(accuracy.cv) # maximal accuracy
maxi = which.max(accuracy.cv)  # position of maximal accuracy
sequence[maxi]  # K in the position that matches the maximal accuracy

#we choose k=45

# Predicting the Test set results
test.predictions = knn(train = train.x, test = test.x, cl = train.y, k = 45)

# Making the Confusion Matrix
confusion_KNN = table(test.predictions,test.y)
confusion_KNN
accuracy_KNN = confusion_Matrixs(confusion_KNN)
