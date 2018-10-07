library(arules)
library(haven)

rest = read.transactions("C:\\Users\\thebi\\OneDrive\\Documents\\Data Mining\\restaurantData.csv", sep=",")
summary(rest)

inspect(rest[1:10])
itemFrequency(rest)

rules = apriori(rest)
summary(rules)

inspect(rules[1:3])
