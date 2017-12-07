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

#remove unwanted files
rm(num_train,num_test,cat_train,cat_test) #save memory

#using one hot encoding
tr_labels <- d_train$income_level
ts_labels <- d_test$income_level

new_tr <- model.matrix(~.+0,data = d_train[,-c("income_level"),with=F])
new_ts <- model.matrix(~.+0,data = d_test[,-c("income_level"),with=F])

#convert factor to numeric
tr_labels <- as.numeric(tr_labels)-1
ts_labels <- as.numeric(ts_labels)-1

library(xgboost)

dtrain <- xgb.DMatrix(data = new_tr,label = tr_labels) 
dtest <- xgb.DMatrix(data = new_ts,label= ts_labels)

params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=6, 
               min_child_weight=1, subsample=1, 
               colsample_bytree=1)

xgbcv <- xgb.cv( params = params, 
                 data = dtrain, nrounds = 100, 
                 nfold = 5, showsd = T, 
                 stratified = T, print.every.n = 10,
                 early.stop.round = 20, maximize = F)


xgb1 <- xgb.train (params = params, 
                   data = dtrain, nrounds = 100, 
                   watchlist = list(val=dtest,train=dtrain), 
                   print.every.n = 10, 
                   early.stop.round = 10, 
                   maximize = F , eval_metric = "error")

xgbpred <- predict (xgb1,dtest)
xgbpred_bi <- ifelse (xgbpred > 0.5,1,0)

library(caret)
pred_CM <- confusionMatrix(xgbpred_bi, ts_labels)

mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 


# Print 
pred_CM$byClass

#Accuracy : 0.9485
#Sensitivity : 0.9877
#Specificity : 0.3555


# Precision
(precision <- pred_CM$byClass["Precision"])
#0.9586458

# Recall
(recall <- pred_CM$byClass["Sensitivity"])
#0.9876892

(f_measure <- 2*((precision*recall)/(precision+recall)))
#0.9729508




