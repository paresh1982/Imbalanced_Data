#set working directory
setwd("~/GitHub/Imbalanced Data")
getwd()

#load packages & data
install.packages("data.table")
library(data.table)
train <- fread("train.csv",na.strings = c(""," ","?","NA",NA))
test <- fread("test.csv",na.strings = c(""," ","?","NA",NA))

#look at data
dim(train) 
str (train)

dim(test) 
str (test)

#check first few rows of train & test
train[1:5]
test [1:5]

#check target variables
unique(train$income_level)
unique(test$income_level)

#encode target variables
train[,income_level := ifelse(income_level == "-50000",0,1)]
test[,income_level := ifelse(income_level == "-50000",0,1)]

#checking proportions
round(prop.table(table(train$income_level))*100)

# We see that the majority class has a proportion of 94%.
#As seen in str() above, the columns in the both data set aren't as per column 
#classes given on data set page.

#set column classes

factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
numcols <- setdiff(1:40,factcols)

train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

#Now, let's separate categorical variables & numerical variables.

#subset categorical variables
cat_train <- train[,factcols, with=FALSE]
cat_test <- test[,factcols,with=FALSE]

#subset numerical variables
num_train <- train[,numcols,with=FALSE]
num_test <- test[,numcols,with=FALSE]

rm(train,test) #to save memory

#Let's begin with numerical data now

#load libraries
install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)

#write a plot function
tr <- function(a){
  ggplot(data = num_train, aes(x= a, y=..density..)) + 
    geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
  ggplotly()
}

#we've created a histogram overlapped with density curve.

#variable age
tr(num_train$age)

#variable capital_losses
tr(num_train$capital_losses)

# this is highly skewed

#add target variable
num_train[,income_level := cat_train$income_level]

#create a scatter plot
ggplot(data=num_train,aes(x = age, y=wage_per_hour)) + 
  geom_point(aes(colour=income_level)) + 
  scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))

#As we can see, most of the people having income_level 1, seem to fall in the age of 25-65 
#earning wage of $1000 to $4000 per hour.
#also age < 20 would have income_level 0

#dodged bar chart
all_bar <- function(i){
  ggplot(cat_train,aes(x=i,fill=income_level)) + 
    geom_bar(position = "dodge",  color="black") + 
    scale_fill_brewer(palette = "Pastel1") + 
    theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}

#variable class_of_worker
all_bar(cat_train$class_of_worker)

#no specific information is provided about Not in universe category


#variable education
all_bar(cat_train$education)

#all children have income_level 0. 
#we can say that Bachelors degree holders have the largest proportion of people have income_level 1.

# proportion table
prop.table(table(cat_train$marital_status,cat_train$income_level),1)

prop.table(table(cat_train$class_of_worker,cat_train$income_level),1)


#check missing values in numerical data
table(is.na(num_train))
table(is.na(num_test))

#We see that numeric variables has no missing values

install.packages("caret")
library(caret)

# Correlation among numerical variables
# set threshold as 0.7

# convert from factor to numeric
num_train$income_level <- as.numeric(num_train$income_level)

ax <- findCorrelation(x = cor(num_train), cutoff = 0.7)

num_train <- num_train[,-ax,with=FALSE]
num_test[,weeks_worked_in_year := NULL]

# The variable weeks_worked_in_year gets removed, we also remove the variable from the test data


#Now, let's check for missing values in categorical data.

#check missing values per columns
# for train data
mvtr <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100

# for test data
mvte <- sapply(cat_test, function(x){sum(is.na(x)/length(x))}*100)

# view mvtr and mvte
mvtr
mvte

#We find that some of the variables have ~50% missing values

#select columns with missing value less than 5%
cat_train <- subset(cat_train, select = mvtr < 5 )
cat_test <- subset(cat_test, select = mvte < 5)

#set NA as Unavailable - train data

#convert to characters
cat_train <- cat_train[,names(cat_train) := lapply(.SD, as.character),.SDcols = names(cat_train)]
for (i in seq_along(cat_train)) set(cat_train, i=which(is.na(cat_train[[i]])), j=i, value="Unavailable")

#convert back to factors
cat_train <- cat_train[, names(cat_train) := lapply(.SD,factor), .SDcols = names(cat_train)]

#set NA as Unavailable - test data
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, as.character), .SDcols = names(cat_test)]
for (i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value="Unavailable")

#convert back to factors
cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, factor), .SDcols = names(cat_test)]


#combine factor levels with less than 5% values
#train
for(i in names(cat_train)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p))
  levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}

#test
for(i in names(cat_test)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_test[[i]])) < p))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}


#check columns with unequal levels
install.packages("mlr")
library(mlr)
summarizeColumns(cat_train)[,"nlevs"]
summarizeColumns(cat_test)[,"nlevs"]

#The parameter "nlevs" returns the unique number of level from the given set of variables.


num_train[,.N,age][order(age)]

num_train[,.N,wage_per_hour][order(-N)]


#bin age variable 0-30 31-60 61 - 90
num_train[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,
                     labels = c("young","adult","old"))]
num_train[,age := factor(age)]

num_test[,age:= cut(x = age,breaks = c(0,30,60,90),include.lowest = TRUE,
                    labels = c("young","adult","old"))]
num_test[,age := factor(age)]


#Bin numeric variables with Zero and MoreThanZero
# for train data
num_train[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]

num_train[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]

num_train[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]

num_train[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]

# for test data
num_test[,wage_per_hour := ifelse(wage_per_hour == 0,"Zero","MoreThanZero")][,wage_per_hour := as.factor(wage_per_hour)]

num_test[,capital_gains := ifelse(capital_gains == 0,"Zero","MoreThanZero")][,capital_gains := as.factor(capital_gains)]

num_test[,capital_losses := ifelse(capital_losses == 0,"Zero","MoreThanZero")][,capital_losses := as.factor(capital_losses)]

num_test[,dividend_from_Stocks := ifelse(dividend_from_Stocks == 0,"Zero","MoreThanZero")][,dividend_from_Stocks := as.factor(dividend_from_Stocks)]


target <- num_train$income_level
target_1 <- ifelse(target == "1",0,1)

# remove dependent variable from num_train
num_train[,income_level := NULL]


#combine data and make test & train files
d_train <- cbind(num_train,cat_train)
d_test <- cbind(num_test,cat_test)

dtrain <- as.data.frame(sapply(d_train, as.numeric))
dtest <- as.data.frame(sapply(d_test, as.numeric))

#remove unwanted files
rm(num_train,num_test,cat_train,cat_test) #save memory

tar_test <- dtest$income_level
tar_test_1 <- ifelse(tar_test == "1",0,1)

dtrain$income_level <- NULL
dtest$income_level <- NULL

library(funModeling)
library(Matrix)
library(xgboost)

trainSparse <- sparse.model.matrix(~.,data=dtrain)
testSparse <- sparse.model.matrix(~.,data=dtest)

common <- intersect(colnames(trainSparse),colnames(testSparse))
trainSparse <- trainSparse[,common]
testSparse <- testSparse[,common]

dtrain <- xgb.DMatrix(data = trainSparse, label = target_1)
dtest <- xgb.DMatrix(data = testSparse)


max.depth = 6.0000	
min_child_weight = 1.0000	
subsample = 0.7742	
lambda = 0.2568	
alpha = 0.9799	
gamma = 5.0000	
colsample = 0.6696	

optimal_round <- 2100
model <- xgb.train(params = list(booster = "gbtree", 
                                 eta = 0.01,
                                 max_depth = max.depth,
                                 min_child_weight = min_child_weight,
                                 subsample = subsample, 
                                 colsample_bytree = colsample,
                                 objective = "binary:logistic",
                                 eval_metric = "auc"),
                   data = dtrain, 
                   nround = optimal_round,
                   maximize = TRUE,
                   lambda = lambda,
                   gamma = gamma,
                   alpha = alpha,
                   nthread = 10,
                   verbose = TRUE,
                   tree_method = 'auto'
)

pred <- predict(model,dtest)

# Threshold as 0.5
pred_1 <- ifelse(pred > 0.5, 1, 0)

#make confusion matrix
pred_CM <- confusionMatrix(tar_test_1, pred_1)

#Accuracy : 0.9475
#Sensitivity : 0.9574
#Specificity : 0.6489

# Print 
pred_CM$byClass

# Precision
(precision <- pred_CM$byClass["Pos Pred Value"])
#0.9880204

# Recall
(recall <- pred_CM$byClass["Sensitivity"])
#0.9573983

f_measure <- 2*((precision*recall)/(precision+recall))

f_measure
#0.9724684







