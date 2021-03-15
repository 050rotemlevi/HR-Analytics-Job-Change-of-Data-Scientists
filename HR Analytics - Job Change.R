########################################################################
# Dataset: Job Change of Data Scientists                               #
# Subject: HR Analytics                                                # 
# EDA Part                                                             #
# Version: 2.1                                                         #
# Author:  Alon Krasnitsky & Rotem Levi                                #
# Date: 21.2.2021                                                      #
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
library(dplyr) # functions
library(pastecs) # functions



###############
# Import Data #
###############
# import data and # replace blank with na values
df = read.csv("D:/R-Project/aug_train.csv" , na = c("", "NA", "N/A"))
str(df)

#####################
# Data manipulation #
#####################

## change values & types ##

# fix company size mistake
df$company_size[df$company_size =="10/49"] <- "10-49"

#fix experiense
df$experience[df$experience =="<1"] <- "0"
df$experience[df$experience ==">20"] <- "21"
df$experience = as.numeric(df$experience) 

#fix last_new_job
df$last_new_job[df$last_new_job ==">4"] <- "5"
df$last_new_job[df$last_new_job =="never"] <- "0"
df$last_new_job = as.numeric(df$last_new_job) 

#######factors########

## city ##
df$city = factor(df$city)
# str(df$city)

## gender ##
df$gender = factor(df$gender)
levels(df$gender)= c("Female", "Male","other")
#str(df$gender)

## relevent_experience ##
df$relevent_experience = factor(df$relevent_experience)
levels(df$relevent_experience)= c("Has relevant experience", "No relevant experience")
#str(df$relevent_experience)

## enrolled_university ##
df$enrolled_university = factor(df$enrolled_university)
levels(df$enrolled_university)= c("Full time", "No Enrollment", "Part time")
#str(df$enrolled_university)

## education_level ##
df$education_level = factor(df$education_level)
#str(df$education_level)

## major_discipline ##
df$major_discipline = factor(df$major_discipline)
#str(df$major_discipline)

## company_size ##
df$company_size = factor(df$company_size)
#str(df$company_size)

## company_type ##
df$company_type = factor(df$company_type)
#str(df$company_type)

## target ##
df$target = factor(df$target)
str(df$target)

##################
# Data research  #
##################

## data dietails ##
summary(df)
str(df)

#number of NA in each colum
sapply(df, function(x) sum(is.na(x)))

##################
#    EDA         #
##################

## city ##
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
counts = table(df$city)
barplot(counts, main="City", cex.names=0.6)
#ggplot(data = df) + geom_bar(mapping = aes(x = city), fill = "blue", col = "black")

# Frequency Table
ft = data.frame(table(df$city))
colnames(ft) = c("city", "Freq")
ft$Rel.Freq = round((ft$Freq / sum(ft$Freq))*100,2)
ft = arrange(ft, desc(Freq))
ft

summary(df$city)

## city development index ##
ggplot(data = df) + geom_histogram(mapping = aes(x = city_development_index), fill = "blue", col = "black", bins=20  )

summary(df$city_development_index)

## gender ##
mytable = table(df$gender)
lbls = paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,main="Gender")

## Frequency Table ##
ft = data.frame(table(df$gender))
colnames(ft) = c("Gender", "Freq")
ft$Rel.Freq = round((ft$Freq / sum(ft$Freq))*100,2)
ft = arrange(ft, desc(Freq))
ft

summary(df$gender)

## relrvent experience ##
#ggplot(data = df) + geom_bar(mapping = aes(x = relevent_experience), fill = "blue", col = "black")
mytable = table(df$relevent_experience)
lbls = paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,main="relevent experience")

## Frequency Table ##
ft = data.frame(table(df$relevent_experience))
colnames(ft) = c("relevent experience", "Freq")
ft$Rel.Freq = round((ft$Freq / sum(ft$Freq))*100,2)
ft = arrange(ft, desc(Freq))
ft

summary(df$relevent_experience)

## enrolled university ##
#ggplot(data = df) + geom_bar(mapping = aes(x = enrolled_university ), fill = "blue", col = "black")
mytable <- table(df$enrolled_university)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, main="enrolled university")

## Frequency Table ##
ft = data.frame(table(df$enrolled_university))
colnames(ft) = c("enrolled university", "Freq")
ft$Rel.Freq = round((ft$Freq / sum(ft$Freq))*100,2)
ft = arrange(ft, desc(Freq))
ft

summary(df$enrolled_university)

## education level
ggplot(data = df) + geom_bar(mapping = aes(x = education_level ), fill = "blue", col = "black")

# Frequency Table
ft = data.frame(table(df$education_level))
colnames(ft) = c("education level", "Freq")
ft$Rel.Freq = round((ft$Freq / sum(ft$Freq))*100,2)
ft = arrange(ft, desc(Freq))
ft

summary(df$education_level)

## major discipline
ggplot(data = df) + geom_bar(mapping = aes(x = major_discipline ), fill = "blue", col = "black", width = 0.6)

# Frequency Table
ft = data.frame(table(df$major_discipline))
colnames(ft) = c("major discipline", "Freq")
ft$Rel.Freq = round((ft$Freq / sum(ft$Freq))*100,2)
ft = arrange(ft, desc(Freq))
ft

summary(df$major_discipline)

## experience
ggplot(data = df) + geom_bar(mapping = aes(x = experience ), fill = "blue", col = "black", width = 0.6)
summary(df$experience)

#ggplot(data = df)+geom_density(mapping = aes(x = experience), fill = "blue")
#hist(df$experience)

summary(df$experience)

## company size ##
ggplot(data = df) + geom_bar(mapping = aes(x = company_size ), fill = "blue", col = "black", width = 0.6)

summary(df$company_size)

# Frequency Table
ft = data.frame(table(df$company_size))
colnames(ft) = c("company size", "Freq")
ft$Rel.Freq = round((ft$Freq / sum(ft$Freq))*100,2)
ft = arrange(ft, desc(Freq))
ft

## company type ##
ggplot(data = df) + geom_bar(mapping = aes(x = company_type ), fill = "blue", col = "black", width = 0.6)

# Frequency Table
ft = data.frame(table(df$company_type))
colnames(ft) = c("company type", "Freq")
ft$Rel.Freq = round((ft$Freq / sum(ft$Freq))*100,2)
ft = arrange(ft, desc(Freq))
ft

summary(df$company_type)

## last new job ##
ggplot(data = df) + geom_bar(mapping = aes(x = last_new_job ), fill = "blue", col = "black", width = 0.6)

# Frequency Table
ft = data.frame(table(df$last_new_job))
colnames(ft) = c("last new job", "Freq")
ft$Rel.Freq = round((ft$Freq / sum(ft$Freq))*100,2)
ft = arrange(ft, desc(Freq))
ft

summary(df$last_new_job)

## training hours ##
ggplot(data = df) + geom_bar(mapping = aes(x = training_hours ), fill = "blue", col = "black", width = 0.6)

summary(df$training_hours)

## TARGET ##
mytable <- table(df$target)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,main="TARGET")

# Frequency Table
ft = data.frame(table(df$target))
colnames(ft) = c("target", "Freq")
ft$Rel.Freq = round((ft$Freq / sum(ft$Freq))*100,2)
ft = arrange(ft, desc(Freq))
ft

##################
# Correletions   #
##################

## city -> target - correlation ##
ggplot(data = df)+
  geom_bar(mapping = aes(x = city,y = after_stat(100*count/sum(count))))+
  facet_grid(~target)

CrossTable(df$city,df$target)
chisq.test(df$city, df$target)


## city development index -> target - correlation / T-test ##
ggplot(data = df)+
  geom_density(mapping = aes(x = city_development_index, y = after_stat(scaled), color = target))

t.test(df$city_development_index ~ df$target, mu = 0, alternative = "two.sided", var.equal = T)

## Gender -> target - correlation ##
ggplot(data = df)+
  geom_bar(mapping = aes(x = gender,y = after_stat(100*count/sum(count))))+
  facet_grid(~target)

CrossTable(df$gender,df$target)
chisq.test(df$gender, df$target)

## relevent experience -> target - correlation ##
ggplot(data = df)+
  geom_bar(mapping = aes(x = relevent_experience,y = after_stat(100*count/sum(count))))+
  facet_grid(~target)

CrossTable(df$relevent_experience,df$target)
chisq.test(df$relevent_experience, df$target)

## enrolled university -> target - correlation ##
ggplot(data = df)+
  geom_bar(mapping = aes(x = enrolled_university,y = after_stat(100*count/sum(count))))+
  facet_grid(~target)

CrossTable(df$enrolled_university,df$target)
chisq.test(df$enrolled_university, df$target)

## education level -> target - correlation ##
ggplot(data = df)+
  geom_bar(mapping = aes(x = education_level,y = after_stat(100*count/sum(count))))+
  facet_grid(~target)

CrossTable(df$education_level,df$target)
chisq.test(df$education_level, df$target)

## major discipline -> target - correlation ##
ggplot(data = df)+
  geom_bar(mapping = aes(x = major_discipline,y = after_stat(100*count/sum(count))))+
  facet_grid(~target)

CrossTable(df$major_discipline,df$target)
chisq.test(df$major_discipline, df$target)

## experience -> target - correlation - T-test  ##
ggplot(data = df)+
  geom_density(mapping = aes(x = experience, y = after_stat(scaled), color = target))

t.test(df$experience ~ df$target, mu = 0, alternative = "two.sided", var.equal = T)

## company size -> target - correlation ##
ggplot(data = df)+
  geom_bar(mapping = aes(x = company_size,y = after_stat(100*count/sum(count))))+
  facet_grid(~target)

CrossTable(df$company_size,df$target)
chisq.test(df$company_size, df$target)

## company type -> target - correlation ##
ggplot(data = df)+
  geom_bar(mapping = aes(x = company_type,y = after_stat(100*count/sum(count))))+
  facet_grid(~target)

CrossTable(df$company_type,df$target)
chisq.test(df$company_type, df$target)

## last new job -> target - correlation - T-test ##
ggplot(data = df)+
  geom_density(mapping = aes(x = last_new_job, y = after_stat(scaled), color = target))

t.test(df$last_new_job ~ df$target, mu = 0, alternative = "two.sided", var.equal = T)

## training hours -> target - correlation - T-test ##
ggplot(data = df)+
  geom_density(mapping = aes(x = training_hours, y = after_stat(scaled), color = target))

t.test(df$training_hours ~ df$target, mu = 0, alternative = "two.sided", var.equal = T)



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

# newdata_numeric = newdata[,c(2,6,7,8,9)] #
# newdata_categor = newdata[,c(1,3,4,5,9)] #


## Splitting the dataset into the Training set and Test set  ##
set.seed(123)
split = sample.split(newdata$target, SplitRatio = 0.8)
training_set = subset(newdata, split == TRUE)
test_set = subset(newdata, split == FALSE)

## Logistic Regration ##

# after check the city not good to predict the 0 -> so we take him down
logistic = glm(target ~ .-city, data = training_set, family = binomial)

# Deatails about Logistic Regression
summary(logistic)
OR =exp(logistic$coefficients) # Odds Ratio
OR

# Midul
test.predictions = predict(logistic, newdata = test_set , type = "response")

predictions = rep("0",length(test_set$target))
# After some checks we chose cutoff 0.4
predictions[test.predictions>0.4] = "1"


confusion_logistic = table(predictions,test_set$target)  # Testing confusion matrix
confusion_logistic
accuracy_LR = confusion_Matrixs(confusion_logistic)

## END - Logistic Regration ##
#######################################################################
## Srart - Desition Tree ##

# Fitting Decision Tree Classification to the Training set
# without city - not good column
tree = tree(target ~.-city , data = training_set )

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
# ignore categorical columns -> not good for KNN
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
accuracy.cv = rep(0,23) #10=length(seq(from = 3, to = 51, by = 2))
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

# we choose k=37

# Predicting the Test set results
test.predictions = knn(train = train.x, test = test.x, cl = train.y, k = 37)

# Making the Confusion Matrix
confusion_KNN = table(test.predictions,test.y)
confusion_KNN
accuracy_KNN = confusion_Matrixs(confusion_KNN)

