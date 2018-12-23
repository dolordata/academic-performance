rm(list=ls())

library(rpart)
library(caret)
library(randomForest)
library(adabag)
library(plyr)
library(dplyr)

# set working dictionary and load dataset
setwd("~/Desktop/CSC 529/case study 1")
API=read.csv("xAPI-Edu-Data.csv",sep=",",header=TRUE)

# data cleaning
# detect missing value
summary(API)
sum(is.na(API))

# exploratory analysis
# linear regression to briefly check the correlation and significance
fit <- glm(Class~., data=API, family = binomial(logit))
summary(fit)

# ggplot on some of the features vs class
# ggplot gender and class
class.gender <- data.frame(API$gender, API$Class)
ggplot(class.gender, aes(x = reorder(API.gender,API.gender, function(x) - length(x)), ..count..)) + 
  geom_bar(aes(fill = API.Class), position = "dodge") +
  labs(y = "gender", x = "count",title = 'class by gender')

# ggplot topic and class
class.topic <- data.frame(API$Topic, API$Class)
ggplot(class.topic, aes(x = reorder(API.Topic,API.Topic, function(x) - length(x)), ..count..)) + 
  geom_bar(aes(fill = API.Class), position = "dodge") +
  labs(y = "topic", x = "count",title = 'class by topic')

# ggplot studentabsencedays and class
class.absence <- data.frame(API$StudentAbsenceDays, API$Class)
ggplot(class.absence, aes(x = reorder(API.StudentAbsenceDays,API.StudentAbsenceDays, function(x) - length(x)), ..count..)) + 
  geom_bar(aes(fill = API.Class), position = "dodge") +
  labs(y = "Student absence days", x = "count", title = 'class by student absence days')

# ggplot nationality and class
class.NationalITy <- data.frame(API$NationalITy, API$Class)
ggplot(class.NationalITy, aes(x = reorder(API.NationalITy,API.NationalITy, function(x) - length(x)), ..count..)) + 
  geom_bar(aes(fill = API.Class), position = "dodge") +
  labs(y = "nationality", x = "count", title = 'class by nationality')


# machine learning analysis
#create a training and testing set with replacement
set.seed(1234)
ind <- sample(2, nrow(API), replace=TRUE, prob=c(0.8, 0.2))
train <- API[ind==1,]
test <- API[ind==2,]

#create simple decision tree as basic learner
tree.a <- rpart(Class ~ . , data=train)
plot(tree.a)
text(tree.a)
tree.a
summary(tree.a)

#use caret to evalueate
pred.a <- predict(tree.a, test, type = "class")
t.tree.a <- table(pred.a,test$Class)
confusionMatrix(t.tree.a)

#use “randomForest” to build 3 decision trees
set.seed(1234)
rf.3 <- randomForest(Class ~ ., data=train, ntree=3)
pred.rf.3 <- predict(rf.3, test, type = "class")
t.rf.3 <- table(pred.rf.3,test$Class)
confusionMatrix(t.rf.3)

#use “randomForest” to build 10 decision trees
set.seed(1234)
rf.10 <- randomForest(Class ~ ., data=train, ntree=10)
pred.rf.10 <- predict(rf.10, test, type = "class")
t.rf.10 <- table(pred.rf.10,test$Class)
confusionMatrix(t.rf.10)

#use “randomForest” to build 15 decision trees
set.seed(1234)
rf.15 <- randomForest(Class ~ ., data=train, ntree=15)
summary(rf.15)
pred.rf.15 <- predict(rf.15, test, type = "class")
t.rf.15 <- table(pred.rf.15,test$Class)
confusionMatrix(t.rf.15)

#use “randomForest” to build 20 decision trees
set.seed(1234)
rf.20 <- randomForest(Class ~ ., data=train, ntree=20)
pred.rf.20 <- predict(rf.20, test, type = "class")
t.rf.20 <- table(pred.rf.20,test$Class)
confusionMatrix(t.rf.20)

#use “randomForest” to build 25 decision trees
set.seed(1234)
rf.25 <- randomForest(Class ~ ., data=train, ntree=25)
pred.rf.25 <- predict(rf.25, test, type = "class")
t.rf.25 <- table(pred.rf.25,test$Class)
confusionMatrix(t.rf.25)

#use “randomForest” to build 30 decision trees
set.seed(1234)
rf.30 <- randomForest(Class ~ ., data=train, ntree=30)
pred.rf.30 <- predict(rf.30, test, type = "class")
t.rf.30 <- table(pred.rf.30,test$Class)
confusionMatrix(t.rf.30)

# adaboost
# use adaboost with decision trees as base learner
set.seed(1234)
adab1<-boosting(Class~., data=train, boos=TRUE, mfinal=10, control = rpart.control(maxdepth = 1))
pred.ada1 <- predict.boosting(adab1, test, type="class")
pred.ada1$confusion
pred.ada1$error

# try out different number of iteration and depth combiantion
set.seed(1234)
adab2<-boosting(Class~., data=train, boos=TRUE, mfinal=10, control = rpart.control(maxdepth = 5))
pred.ada2 <- predict.boosting(adab2, test, type="class")
pred.ada2$confusion
pred.ada2$error

set.seed(1234)
adab3<-boosting(Class~., data=train, boos=TRUE, mfinal=20, control = rpart.control(maxdepth = 5))
sort(adab3$importance)
pred.ada3 <- predict.boosting(adab3, test, type="class")
pred.ada3$confusion
pred.ada3$error

set.seed(1234)
adab4<-boosting(Class~., data=train, boos=TRUE, mfinal=30, control = rpart.control(maxdepth = 5))
pred.ada4 <- predict.boosting(adab4, test, type="class")
pred.ada4$confusion
pred.ada4$error

set.seed(1234)
adab5<-boosting(Class~., data=train, boos=TRUE, mfinal=10, control = rpart.control(maxdepth = 10))
pred.ada5 <- predict.boosting(adab5, test, type="class")
pred.ada5$confusion
pred.ada5$error
