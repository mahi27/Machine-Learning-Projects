data(iris)
str(iris)
head(iris)
summary(iris)
table(iris$Species)
#eda
library(plotly)
plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, color = ~Species, type = 'scatter', mode = 'markers')
plot_ly(data = iris, x = ~Sepal.Width, y = ~Petal.Width, color = ~Species, type = 'scatter', mode = 'markers')
plot_ly(y = iris$Sepal.Length, type = "box") %>%
  add_trace(y = iris$Sepal.Width) %>% add_trace(y = iris$Petal.Width ) %>% add_trace(y = iris$Petal.Length)

#normalizing the dataset
norm <- function(x){
return ((x - min(x))/(max(x)-min(x)))
}

normdata <- as.data.frame(lapply(iris[1:4],norm))
summary(normdata)

#prepare training and test data
set.seed(1234)
data = sample(2, nrow(iris), replace = TRUE,prob = c(0.67,0.33))
iris.training <- iris[data==1, 1:4]
iris.test <- iris[data==2, 1:4]
iris.trainingLabels <- iris[data==1, 5]
iris.testLabels <- iris[data == 2,5]
#knn prediction
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainingLabels, k = 2)
#accuracy
library(gmodels)
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)
