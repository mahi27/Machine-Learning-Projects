#rule learning
mushroom_data <- read.csv("mushrooms.csv", sep = ",", header = TRUE)
str(mushroom_data)
mushroom_data$veil_type <- NULL
table(mushroom_data$type)
#zeroR would predict all edible mushrooms
library(RWeka)
mush_1R <- OneR(type ~ ., data = mushroom_data)

summary(mush_1R)

mush_jrip <- JRip(type ~ ., data = mushroom_data)
summary(mush_jrip)

mush_jrip
