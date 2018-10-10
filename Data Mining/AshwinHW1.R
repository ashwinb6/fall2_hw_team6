library(arules)
library(haven)

rest = read.transactions("C:\\Users\\thebi\\OneDrive\\Documents\\Data Mining\\restaurantData.csv", 'single', skip = 1, sep = ",", cols = c(3, 1))
summary(rest)

inspect(rest[1:10])
itemFrequency(rest)

rules1 = apriori(rest)
summary(rules1)
rules = apriori(rest, parameter = list(support = .05, confidence = .25, minlen = 2, maxlen=2))
summary(rules)
rules

inspect(sort(rules, by = "lift")[1:8])

meats = subset(rules, items %in% c("Filet Mignon","Pork Tenderloin","Roast Chicken", "Duck"))
inspect(sort(meats, by="lift"))

# ~~~~~Meat           =>  Wine Pairings~~~~~~~
# {Filet Mignon}      => {Blackstone Merlot}
# {Roast Chicken}     => {Duckhorn Chardonnay}
# {Duck}              => {Duckhorn Chardonnay}
# {Pork Tenderloin}   => {Cantina Pinot Bianco}

popmeal <- apriori(rest, parameter = list(support=0.05, confidence=0.25, minlen=3, maxlen=3))
summary(popmeal)
inspect(sort(popmeal, by = "count"))

# lhs                       rhs                       support confidence      lift            count
# [1]  {Cantina Pinot Bianco,Roasted Root Veg}     => {Pork Tenderloin}      0.13342746       1134
# [5]  {Pork Tenderloin,Steamed Seasonal Veg}      => {Cantina Pinot Bianco} 0.12389693       1053
# [6]  {Baked Beans,Cantina Pinot Bianco}          => {Pork Tenderloin}      0.06883163       585