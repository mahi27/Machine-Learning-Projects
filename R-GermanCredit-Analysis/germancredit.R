credit_data <- read.csv("credit.csv", sep = ",", header = TRUE)
str(credit_data)
table(credit_data$default)
summary(credit_data)

sum(is.na(credit_data))

barplot(table(credit_data$checking_balance))
barplot(table(credit_data$credit_history))
barplot(table(credit_data$purpose))
boxplot(credit_data$amount)
barplot(table(credit_data$savings_balance))
barplot(table(credit_data$installment_plan))
barplot(table(credit_data$installment_rate))
barplot(table(credit_data$personal_status))
barplot(table(credit_data$other_debtors))
boxplot(credit_data$months_loan_duration)
mosaicplot(credit_data$default ~ credit_data$other_debtors, col = T)
mosaicplot(credit_data$default ~ credit_data$other_debtors, col = T)


library(gmodels)
CrossTable(credit_data$default, credit_data$checking_balance,digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

set.seed(123)
train <- sample(1000,800)
credit_train <- credit_data[train,]
credit_test <- credit_data[-train,]
credit_train$default <- as.factor(credit_train$default)
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_pred <- predict(credit_model, credit_test)

CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
boost_pred <- predict(credit_boost10, credit_test)

CrossTable(credit_test$default, boost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

matrix_dims <- list(c(1,2), c(1,2))
names(matrix_dims) <- c("predicted","actual")

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,dimnames = matrix_dims)

model_cost <- C5.0(credit_train[-17],credit_train$default,costs = error_cost, trials = 10)
credit_cost_pred <- predict(model_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
