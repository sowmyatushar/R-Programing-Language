install.packages("randomForest")
install.packages("MASS")
install.packages("caret")
library(randomForest)
library(MASS)
library(caret)

set.seed(125)

FraudCheck <- read.csv("C:/Users/tussh/Documents/Random Forest/Fraud_checkRF.csv")
hist(FraudCheck$Taxable.Income, col = "chocolate")


Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
# if Taxable Income is less than or equal to 30000 then Risky else Good.
FCtemp= data.frame(FraudCheck,Risky_Good)
FC = FCtemp[,c(1:7)]

str(FC)
table(FC$Risky_Good)
View(FC)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]
set.seed(213)
rf <- randomForest(Risky_Good~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting

attributes(rf)

# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)
head(train$Risky_Good)
confusionMatrix(pred1, train$Risky_Good)

# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good)
plot(rf)

rf1 <- randomForest(Risky_Good~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1

pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$Risky_Good) 

pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$Risky_Good)


hist(treesize(rf1), main = "No of Nodes for the trees", col = "red")

