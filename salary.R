Salary = read.csv("C:/Users/tussh/Documents/SVM/SalaryData.csv")
View(Salary)

Salary_train <- Salary[1:35221,]
Salary_test  <- Salary[35222:45000,]
library(kernlab)
Salary_classifier <- ksvm(Salary~ ., data = Salary_train,
                          kernel = "vanilladot")
Salary_classifier

# predictions on testing dataset
Salary_predictions <- predict(Salary_classifier, Salary_test)

head(Salary_predictions)

table(Salary_predictions, Salary_test$Salary)


agreement <- Salary_predictions == Salary_test$Salary
table(agreement)
prop.table(table(agreement))


## Improving model performance ---- kernel = polydot
Salary_classifier_rbf <- ksvm(Salary ~ ., data = Salary_train, kernel = "polydot")
Salary_predictions_rbf <- predict(Salary_classifier_rbf, Salary_test)

agreement_rbf <- Salary_predictions_rbf == Salary_test$Salary
table(agreement_rbf)
prop.table(table(agreement_rbf))

## Improving model performance ---- kernel = rdfdot
Salary_classifier_rbf <- ksvm(Salary ~ ., data = Salary_train, kernel = "rdfdot")
Salary_predictions_rbf <- predict(Salary_classifier_rbf, Salary_test)

agreement_rbf <- Salary_predictions_rbf == Salary_test$Salary
table(agreement_rbf)
prop.table(table(agreement_rbf))

