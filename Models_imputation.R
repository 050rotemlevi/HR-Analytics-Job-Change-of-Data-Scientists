########################################################################
# Dataset: Job Change of Data Scientists                               #
# Subject: HR Analytics                                                # 
# *Extra*                                                              #
# Models Part  -   Numeric and Imputation  manipulation                #
# Version: 5.3                                                         #
# Author:  Alon Krasnitsky & Rotem Levi                                #
# Date: 22.2.2021                                                      #
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
library(missForest) # imputation 


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

newdata = df

### convert categorical to numeric
for (i in 2:13) {
  if(i==3){
    i = i+1
  }else{
    newdata[,i] = as.numeric(newdata[,i])
  }
}
summary(newdata)
str(newdata)

#number of NA in each colum
sapply(df, function(x) sum(is.na(x)))

##############################
# training data imputation   #
##############################


set.seed(123)

#remove ID
newdata = newdata[,-1]

df_IM = missForest(xmis = newdata, maxiter = 2, ntree = 20)


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

newdata = df_IM$ximp

# NA`s values`
sapply(newdata, function(x) sum(is.na(x)))

## Splitting the dataset into the Training set and Test set  ##
set.seed(123)
split = sample.split(newdata$target, SplitRatio = 0.8)
training_set = subset(newdata, split == TRUE)
test_set = subset(newdata, split == FALSE)

## START - Random Forest##

# choose mtry
mtry_Mat = tuneRF(newdata[,-13], newdata[,13],
                  stepFactor = 0.5,
                  plot = TRUE,
                  ntreeTry = 200,
                  trace = TRUE,
                  improve = 0.05)

Random_forest = randomForest(target ~ ., data = training_set,ntree=300, mtry = 3 ,type = 'class')
test.predictions = predict(Random_forest, newdata = test_set)

summary(Random_forest)

varImpPlot(Random_forest)
importance(Random_forest)

# Making the Confusion Matrix
confusion_RF = table(test_set$target, test.predictions)
confusion_RF
accuracy_RF = confusion_Matrixs(confusion_RF)
