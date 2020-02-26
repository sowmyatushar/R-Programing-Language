install.packages("tree")
install.packages("gmodels")
install.packages("caret")
install.packages("party")
install.packages("knitr")
install.packages("C50")
install.packages("partykit")
library(party)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
library(caret)

FraudCheck = read.csv("C:/Users/tussh/Documents/Decision Tree/Fraud_check.csv")
hist(FraudCheck$Taxable.Income,col = "purple")
View(FraudCheck)
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
FC = data.frame(FraudCheck,Risky_Good)

FC_train <- FC[1:300,]
FC_test <- FC[301:600,]
View(FC)

###Using Party Function 

png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FC)
summary(opall_tree)
# using the training Data 
png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)

plot(op_tree)
pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)
mean(pred_test_df==FC_test$Risky_Good)

#Crosstable of actual data with predicted
CrossTable(FC_test$Risky_Good,pred_test_df)
confusionMatrix(FC_test$Risky_Good,pred_test_df)
