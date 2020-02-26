install.packages('arules') #representing,analysing and manipulating transaction data
library(arules)
install.packages("arulesViz") #extension to arules
library(arulesViz)
Groceries = read.csv("C:/Users/tussh/Documents/Association Rules/groceries.csv",sep = ";")
data()
data(Groceries)
summary(Groceries)
View(Groceries)
itemFrequencyPlot(Groceries,topN=20,type="absolute")
rules <- apriori(Groceries,parameter = list(support= 0.002,confidence = 0.6))
rules
plot(rules)
inspect(head(sort(rules,by="lift")))
head(quality(rules))
plot(rules,method="grouped")
rules = apriori(Groceries)


arules::inspect(rules)
rules.sorted = sort(rules,by = "lift")
#rules <- rules[!is.redundant(rules)]
arules::inspect(rules.sorted)
# rules with rhs containing Patriot only
rules = apriori(Groceries,parameter = list(minlen = 1,supp = 0.02,conf = 0.4))
rules
arules::inspect(rules)
?apriori                


#since the redundant is not working use below code to remove the repeatition.
rules.sorted = sort(rules, by="lift")
subset.matrix = is.subset(rules.sorted, rules.sorted,sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] = NA
redundant = colSums(subset.matrix, na.rm=T) >= 1

rules.pruned = rules.sorted[!redundant]
inspect(rules.pruned)

head(quality(rules.pruned))
