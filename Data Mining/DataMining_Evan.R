install.packages('arules')
library(arules)

restaurant <- read.transactions('C:\\Users\\escha\\OneDrive\\Documents\\MSA\\Fall Classes\\Data Mining\\Data\\res_complete.csv',sep=',')
summary(restaurant)

inspect(restaurant[17])
itemFrequency(restaurant)

rules <- apriori(restaurant)
summary(rules)
inspect(sort(rules, by = "lift"))

newrules <- apriori(restaurant, parameter = list(support=0.05, confidence=0.5, minlen=2, maxlen=2))
summary(newrules)
inspect(sort(newrules, by = "lift"))

meats <- subset(newrules,items %in% c("Duck","Filet Mignon","Pork Tenderloin","Roast Chicken"))
inspect(sort(meats, by = "lift"))

### Meat -> Wine pairings ###
#Filet Mignon -> Blackstone Merlot
#Roast Chicken -> Duckhorn Chardonnay
#Duck -> Duckhorn Chardonnay
#Pork Tenderloin -> Cantina Pinot Bianco

popmeal <- apriori(restaurant, parameter = list(support=0.05, confidence=0.5, minlen=3, maxlen=3))
summary(popmeal)
inspect(sort(popmeal, by = "support"))

### Most Popular Meal ###
# Cantina Pinot Bianco, Roasted Root Veg, Pork Tenderloin