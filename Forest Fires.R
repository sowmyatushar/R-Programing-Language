forestfires = read.csv("C:/Users/tussh/Documents/SVM/forestfires.csv")
View(forestfires)

forest_train <- forestfires[1:350,]
forest_test  <- forestfires[351:517,]
library(kernlab)
forest_classifier <- ksvm(size_category~ ., data = forest_train,
                          kernel = "vanilladot")
forest_classifier

# predictions on testing dataset
forest_predictions <- predict(forest_classifier, forest_test)

head(forest_predictions)

table(forest_predictions, forest_test$size_category)


agreement <- forest_predictions == forest_test$size_category
table(agreement)
prop.table(table(agreement))


## Improving model performance ---- kernel = polydot
forest_classifier_rbf <- ksvm(size_category ~ ., data = forest_train, kernel = "polydot")
forest_predictions_rbf <- predict(forest_classifier_rbf, forest_test)

agreement_rbf <- forest_predictions_rbf == forest_test$size_category
table(agreement_rbf)
prop.table(table(agreement_rbf))


## Improving model performance ---- kernel = rbfdot
forest_classifier_rbf <- ksvm(size_category ~ ., data = forest_train, kernel = "rbfdot")
forest_predictions_rbf <- predict(forest_classifier_rbf, forest_test)

agreement_rbf <- forest_predictions_rbf == forest_test$size_category
table(agreement_rbf)
prop.table(table(agreement_rbf))
