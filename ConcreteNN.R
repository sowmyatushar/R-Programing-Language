library(neuralnet)  # regression
library(nnet) # classification 
library(NeuralNetTools)
concrete <- read.csv(file.choose())
str(concrete)
View(concrete)
# Exploratory data Analysis :

hist(concrete$cement, prob = T, breaks = 30)
lines(density(concrete$cement))
summary(concrete$cement)
hist(concrete$slag, prob = T, breaks = 30)
lines(density(concrete$slag))
summary(concrete$slag)
summary(concrete)


# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
summary(concrete_norm$strength) # Normalized form of strength
# Data Partition 
set.seed(123)
ind <- sample(2, nrow(concrete_norm), replace = TRUE, prob = c(0.7,0.3))
concrete_train <- concrete_norm[ind==1,]
concrete_test  <- concrete_norm[ind==2,]


# Creating a neural network model on training data


concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
str(concrete_model)
plot(concrete_model, rep = "best")
summary(concrete_model)

# Evaluating model performance

set.seed(12323)
model_results <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_results$net.result

# Predicted strength Vs Actual Strength of test data.
cor(predicted_strength,concrete_test$strength)


# Improve the model performance :
set.seed(12345)
concrete_model2 <- neuralnet(strength~cement+slag+ash+water+superplastic+
                               coarseagg+fineagg+age,data= concrete_train,
                             hidden = 5)
plot(concrete_model2, rep = "best")
