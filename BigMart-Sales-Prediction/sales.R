raw_data <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")
str(raw_data)
summary(raw_data)

levels(raw_data$Item_Fat_Content)
raw_data$Item_Fat_Content <- gsub("LF", "LowFat",raw_data$Item_Fat_Content)
raw_data$Item_Fat_Content <- gsub("low fat", "LowFat",raw_data$Item_Fat_Content)
raw_data$Item_Fat_Content <- gsub("Low Fat", "LowFat",raw_data$Item_Fat_Content)
raw_data$Item_Fat_Content <- gsub("reg", "Regular",raw_data$Item_Fat_Content)
raw_data$Item_Fat_Content <- as.factor(raw_data$Item_Fat_Content)
table(raw_data$Item_Fat_Content)

sum(is.na(raw_data$Item_Type))
str(raw_data$Item_Type)

sum(is.na(raw_data$Item_Weight))
mean_Weight <- mean(raw_data$Item_Weight, na.rm = TRUE)
raw_data$Item_Weight[is.na(raw_data$Item_Weight)] <- mean_Weight

Data_1 <- subset(raw_data, Item_Visibility != 0)
visibility_model <- lm(Item_Visibility ~ Item_Weight + Item_Fat_Content +Item_Type + Item_MRP + 
                         Outlet_Establishment_Year + Outlet_Size + Outlet_Location_Type + Item_Outlet_Sales,
                         data = Data_1)
raw_data$Item_Visibility[raw_data$Item_Visibility == 0] <- predict(visibility_model,newdata = raw_data[raw_data$Item_Visibility == 0,])
raw_data[raw_data$Item_Visibility == 0,]

library(caTools)
set.seed(100)
raw_data$Outlet_Size <- as.character(raw_data$Outlet_Size)
Storetypes <- subset(raw_data, Outlet_Size != "")
spl <- sample.split(Storetypes$Outlet_Size, SplitRatio = 0.8)
Train <- subset(Storetypes, spl == TRUE)
Test <- subset(Storetypes, spl == FALSE)
###Using Random Forest for classification
library(randomForest)
Train$Outlet_Size <- as.factor(Train$Outlet_Size)
Test$Outlet_Size <- as.factor(Test$Outlet_Size)
###Creating the model
SizeForest <- randomForest(Outlet_Size ~.-Item_Outlet_Sales -Item_Identifier,
                           data =  Train,nodesize = 25, ntree = 100)  
###Predicting on the test set
PredictForest <- predict(SizeForest, newdata = Test)
#Confusion matrix for accuracy
table(Test$Outlet_Size, PredictForest)


###Classifying the missing values in the dataset
raw_data$Outlet_Size <- predict(SizeForest, newdata =raw_data)

