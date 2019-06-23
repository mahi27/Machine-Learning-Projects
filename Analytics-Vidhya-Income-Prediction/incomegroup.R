train <- read.csv("train.csv",na.strings = "")
test <- read.csv("test.csv",na.strings = "")

str(train)
train_con <- subset(train,select = c(ID, Age, Hours.Per.Week))
train_cat <- subset(train,select = -c(ID, Age, Hours.Per.Week))
summary(train_con)
summary(train_cat)
#installpackage
library("pastecs")
options(scipen = 100)
options(digits = 2)
stat.desc(train_con)
apply(train, 2, function(x){length(unique(x))})
#Printing the count of race category
table(train_cat$Race)
#printing percentage of race category
as.matrix(prop.table(table(train_cat$Race)))
#count of top 20 countries
head(sort(table(train_cat$Native.Country),decreasing = TRUE),20)
#Percentage of top 20 countries
head(round(sort(prop.table(table(train_cat$Native.Country)),decreasing = TRUE),6),20)
#installpackages
#comparing both cat variables
library("gmodels")
CrossTable(train$Sex,train$Income.Group)
#using ggplot library
library("ggplot2")
ggplot(train, aes(Sex, fill = Income.Group)) + geom_bar() + labs(title = "Stacked Bar Chart", x = "Sex", y = "Count") + theme_bw()
#comparing both cont variables
ggplot(train, aes(Age, fill = Hours.Per.Week)) + geom_bar() + labs(title = "Stacked Bar Chart", x = "Age", y = "Hours") + theme_bw()
#comparing cat-con variables
ggplot(train, aes(Sex, Hours.Per.Week))+geom_boxplot()+labs(title = "Boxplot")

#checking missing values
table(is.na(train))
colSums(is.na(train))
colSums(is.na(test))
library(mlr)
#impute with mode method
imputed_data <- impute(train, classes = list(factor = imputeMode()))
#updating train data with imputed values
train <- imputed_data$data
#checking missing values again
colSums(is.na(train))
#imputing test data
imputed_test_data <- impute(train, classes = list(factor = imputeMode()))
test <- imputed_test_data$data
#check test for na values again
colSums(is.na(test))
#checking for outliers
library(ggplot2)
ggplot(train, aes(ID,Age)) + geom_jitter()
ggplot(train, aes(ID,Hours.Per.Week)) + geom_jitter()
sapply(train, class)
as.matrix(prop.table(table(train$Workclass)))
#using recode from car package
library("car")
train$Workclass <- recode(train$Workclass,"c('State-gov','Self-emp-inc','Federal-gov','Without-pay','Never-worked')  = 'Others'")
test$Workclass <- recode(test$Workclass, "c('State-gov','Self-emp-inc','Federal-gov','Without-pay','Never-worked') = 'Others'")
as.matrix(prop.table(table(train$Workclass)))
#predictive modelling
table(train$Income.Group)
train$Income.Group <- ifelse(train$Income.Group == "<=50K",0,1)
table(train$Income.Group)
train <- subset(train,select = -c(ID))
library(rpart)
set.seed(333)
train.tree <- rpart(Income.Group ~ .,data = train, method = "class", control = rpart.control(minsplit = 20,minbucket = 100,maxdepth = 10, xval = 5) )
summary(train.tree)
library(rpart.plot)
rpart.plot(train.tree)
prediction_train <- predict(train.tree, newdata = train, type = "class")
prediction_test <- predict(train.tree, newdata = test, type = "class")
library(caret)
confusionMatrix(prediction_train, train$Income.Group)
solution_frame <- data.frame(ID = test$ID, Income.Group = prediction_test)
write.csv(solution_frame, file = "final_solution.csv")
