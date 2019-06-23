raw_data <- read.table("wisc_bc_data.csv", sep = ",", header = TRUE)

data <- raw_data[,-1]
summary(data)
data$diagnosis<- factor(data$diagnosis, levels = c("B", "M"),
                        labels = c("Benign", "Malignant"))

prop.table(table(data$diagnosis))

normalise <- function(x){
  return ((x - min(x))/(max(x)-min(x)))
}

n_data <- as.data.frame(lapply(data[,2:31], normalise))

wbcd_train <- n_data[1:469, ]
wbcd_test <- n_data[470:569, ]

training_labels <- data[1:469,1]
test_labels <- data[470:569,1]

library(class)

model <- knn(train = wbcd_train, test = wbcd_test, cl = training_labels, k = 21)

library(gmodels)

CrossTable(y = model,x = test_labels)
