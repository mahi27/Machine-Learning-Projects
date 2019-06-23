insurance_data <- read.csv("insurance.csv", sep = ",", header = TRUE)
str(insurance_data)
sum(is.na(insurance_data))
boxplot(insurance_data$age)
boxplot(insurance_data$bmi)
summary(insurance_data$bmi)
library(ggplot2)
ggplot(insurance_data, aes(age,charges)) + geom_bar(stat = "identity")
ggplot(insurance_data, aes(bmi,charges)) + geom_bar(stat = "identity")
table(insurance_data$region)
ggplot(insurance_data, aes(children,charges)) + geom_bar(stat = "identity")
cor(insurance_data[c("age","bmi", "children","charges")])
pairs(insurance_data[c("age", "bmi", "children", "charges")])
library(psych)
pairs.panels(insurance_data[c("age", "bmi", "children", "charges")])

insurance_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = insurance_data)
#improved regression
insurance_data$age2 <- insurance_data$age^2
insurance_data$bmi30 <- ifelse(insurance_data$bmi >= 30, 1, 0)
insurance_model2 <- lm(charges ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance_data)
