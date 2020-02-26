install.packages('arules')
library(arules)
install.packages("arulesViz") #extension to arules
library(arulesViz)

Book = read.csv(file.choose())
Book$ChildBks = as.factor(Book$ChildBks)
Book$YouthBks = as.factor(Book$YouthBks)
Book$CookBks = as.factor(Book$CookBks)
Book$DoItYBks = as.factor(Book$DoItYBks)
Book$ArtBks = as.factor(Book$ArtBks)
Book$RefBks = as.factor(Book$RefBks)
Book$GeogBks = as.factor(Book$GeogBks)
Book$ItalAtlas = as.factor(Book$ItalAtlas)
Book$ItalCook = as.factor(Book$ItalCook)
Book$ItalArt = as.factor(Book$ItalArt)
Book$Florence = as.factor(Book$Florence)
str(Book)
View(Book)

rules = apriori(Book,parameter = list(supp = 0.7,conf = 0.7))
rules
plot(rules)
inspect(head(sort(rules,by="lift")))
head(quality(rules))
plot(rules,method="grouped")

rules = apriori(Book)
arules::inspect(rules)
rules.sorted = sort(rules,by = "lift")
#rules <- rules[!is.redundant(rules)]
arules::inspect(rules.sorted)
# rules with rhs containing Survived only
rules = apriori(Book,parameter = list(minlen = 2,supp = 0.4,conf = 0.5)
                ,appearance = list(rhs = "ChildBks=0",lhs = "CookBks=1"))
arules::inspect(rules)
?apriori                
table(Book$ChildBks)

#since the redundant is not working use below code to remove the repeatition.
rules.sorted = sort(rules, by="lift")
subset.matrix = is.subset(rules.sorted, rules.sorted,sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag=T)] = 1
redundant = colSums(subset.matrix, na.rm=T) >= NA

rules.pruned = rules.sorted[!redundant]
inspect(rules.pruned)
