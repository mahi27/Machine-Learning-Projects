wine_data <- read.csv("whitewines.csv", sep = ",", header = TRUE)
str(wine_data)
sum(is.na(wine_data))
hist(wine_data$quality)
ggplot(wine_data,aes(fixed.acidity,quality))+geom_point()
library(psych)
pairs.panels(wine_data[c("fixed.acidity","volatile.acidity","citric.acid")])
cor(wine_data[c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar",     
                "chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density",             
                "pH","sulphates","alcohol","quality")])
wine_train <- wine_data[1:3750,]
wine_test <- wine_data[3751:4898,]
table(wine_train$quality)
table(wine_test$quality)
library(rpart)
wine_model <- rpart(quality ~ ., data = wine_train)
summary(wine_model)
library(rpart.plot)
rpart.plot(wine_model, digits = 3)
rpart.plot(wine_model, digits = 3, fallen.leaves = TRUE, type = 3, extra = 101)
wine_pred <- predict(wine_model, wine_test)
summary(wine_pred)
summary(wine_test$quality)
cor(wine_pred, wine_test$quality)
#M5Algorithm

library(RWeka)
model2 <- M5P(quality ~ ., data = wine_train)
pred <- predict(model2, wine_test)
summary(pred)
model2
cor(pred, wine_test$quality)

