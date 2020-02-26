install.packages('arules') #representing,analysing and manipulating transaction data
library(arules)
install.packages("arulesViz") #extension to arules
library(arulesViz)
Movies = read.csv(file.choose())
data()
data(Movies)
summary(Movies)
View(Movies)
Movies$Sixth.Sense = as.factor(Movies$Sixth.Sense)
Movies$Gladiator = as.factor(Movies$Gladiator)
Movies$Patriot = as.factor(Movies$Patriot)
Movies$LOTR1 = as.factor(Movies$LOTR1)
Movies$LOTR2 = as.factor(Movies$LOTR2)
Movies$LOTR = as.factor(Movies$LOTR)
Movies$Harry.Potter1 = as.factor(Movies$Harry.Potter1)
Movies$Harry.Potter2 = as.factor(Movies$Harry.Potter2)
Movies$Braveheart = as.factor(Movies$Braveheart)
Movies$Green.Mile = as.factor(Movies$Green.Mile)
str(Movies)
View(Movies)
#Association rule algorithim 
rules = apriori(Movies,parameter = list(supp = 0.8,conf = 0.7))
rules
plot(rules)
inspect(head(sort(rules,by="lift")))
head(quality(rules))
plot(rules,method="grouped")
rules = apriori(Movies)

#inspect the set of  rules associated
arules::inspect(rules)
rules.sorted = sort(rules,by = "lift")
#rules <- rules[!is.redundant(rules)]
arules::inspect(rules.sorted)
# rules with rhs containing Patriot only
rules = apriori(Movies,parameter = list(minlen = 2,supp = 0.6,conf = 0.7)
                ,appearance = list(rhs = "Patriot=1"))
rules
arules::inspect(rules)
?apriori                
table(Movies$Patriot)

#since the redundant is not working use below code to remove the repeatition.
rules.sorted = sort(rules, by="lift")
subset.matrix = is.subset(rules.sorted, rules.sorted,sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] = NA
redundant = colSums(subset.matrix, na.rm=T) >= 1

rules.pruned = rules.sorted[!redundant]
inspect(rules.pruned)

head(quality(rules.pruned))
