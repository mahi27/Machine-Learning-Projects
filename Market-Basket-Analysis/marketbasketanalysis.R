groceries_data <- read.csv("groceries.csv", sep = ",", header = FALSE)
str(groceries_data)
head(groceries_data)
#the transactional data has been stored as data frame of 4 columns. Transaction with more than
#four items are stored in different rows. Install arules to create a transactional matrix
install.packages("arules")
library(arules)
groceries <- read.transactions("groceries.csv", sep =",")
summary(groceries)
#View first five transactions
inspect(groceries[1:5])
#View frquencies of fourth and sixth column items
itemFrequency(groceries[,4:6])
itemFrequencyPlot(groceries, support = 0.12)
itemFrequencyPlot(groceries, topN = 10)
#Transaction vs items
image(groceries[1:15])
image(groceries[1:200])
#Create rule by choosing appropriate support and confidence
apriori(groceries)
groceryrules <- apriori(groceries, parameter = list(
                support = 0.006, confidence = 0.25, minlen = 2))
summary(groceryrules)
#View the rules
inspect(groceryrules[4:10])
#sort grocery rules by ift
inspect(sort(groceryrules, by = "lift")[6:10])
#subsetting association rules
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)
#saving association rules
write(groceryrules, file = "C:/Users/mahitha.tammineedi/Desktop/groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)
groceryrules_df <- as(groceryrules,"data.frame")
