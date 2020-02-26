install.packages("randomForest")
install.packages("caret")
install.packages("Mass")
library(randomForest)
library(caret)
library(MASS)
CompanyData <- read.csv("C:/Users/tussh/Documents/Random Forest/Company_DataRF.csv")
set.seed(200)
View(CompanyData)
hist(CompanyData$Sales, main = "Sales of Companydata",xlim = c(0,20), col = c("maroon","grey", "pink","violet"))

highsales = ifelse(CompanyData$Sales<9, "No", "Yes")  # if greater than 8 then high sales else Low
CD = data.frame(CompanyData[2:11], highsales)
str(CD)
table(CD$highsales)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test  <- CD[ind==2,]
set.seed(213)
rf <- randomForest(highsales~., data=train)

attributes(rf)

# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)
head(train$highsales)
confusionMatrix(pred1, train$highsales)

# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsales)

# Error Rate in Random Forest Model :
plot(rf)

# Tune Random Forest Model mtry 
#tune <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
              # trace = TRUE, improve = 0.05)

rf1 <- randomForest(highsales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1

pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$highsales)


# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$highsales)

#no of nodes of trees

hist(treesize(rf1), main = "No of Nodes for the trees", col = "sky blue")


# Extract single tree from the forest :
getTree(rf, 1, labelVar = TRUE)
