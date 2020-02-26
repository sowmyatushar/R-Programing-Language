calories = read.csv("C:/Users/tussh/Downloads/calories_consumed.csv")
View(calories)

#wc.at <- read.csv("E:/Classes/Trainer Tools/Final/03 Linear Regression/Data Sets/wc-at.csv",header=T)
dataset = read.csv(calories_consumed.csv)
getwd()
View(calories)
library(lattice)
#Scatter plot
#plot(wc.at$Waist,wc.at$AT,main="Scatter Plot", col="Dodgerblue4", 
# col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="Waist Ciscumference", 
# ylab="Adipose Tissue area", pch=20) 
# plot(x,y)

attach(calories)

cor(Calories.Consumed,Weight.gained..grams.)
plot(Calories.Consumed,Weight.gained..grams.)
#cor(wc.at$Waist , wc.at$AT)



reg <- lm(Weight.gained..grams.~ Calories.Consumed, data=calories) # Y ~ X
summary(reg)

reg$coefficients
reg$residuals


pred <- predict(reg,interval="confidence")

pred <- as.data.frame(pred)



sqrt(sum(reg$residuals^2)/nrow(calories)) ## RMSE 


pred
View(pred)
??predict

cor(pred$fit, calories$Weight.gained..grams.)
plot(Weight.gained..grams.,Calories.Consumed)
cor(Weight.gained..grams.,sqrt(Calories.Consumed))
# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Weight.gained..grams.~sqrt(Calories.Consumed), data=calories)
summary(reg_sqrt)
cor(sqrt(Calories.Consumed),Weight.gained..grams.)
plot(sqrt(Calories.Consumed),Weight.gained..grams.)



sqrt(sum(reg_sqrt$residuals^2)/nrow(wc.at)) ## RMSE 



confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")

cor(Weight.gained..grams.,log(Calories.Consumed))
plot(Weight.gained..grams.,log(Calories.Consumed))

reg_log<-lm(Weight.gained..grams.~log(Calories.Consumed), data=calories)
summary(reg_log)

sqrt(sum(reg_log$residuals^2)/nrow(calories)) ## RMSE 


confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

cor(log(Weight.gained..grams.),Calories.Consumed*Calories.Consumed)

reg1 <-lm(log(Weight.gained..grams.)~Calories.Consumed+ I(Calories.Consumed*Calories.Consumed), data=calories)
summary(reg1)
pred3 = as.data.frame(predict(reg1, interval = "prediction"))
sqrt((sum(reg1$residuals^2))/nrow(calories))

y = exp(pred3$fit)
error  = y - calories$Weight.gained..grams.
sqrt(sum(error^2)/nrow(calories)) ## RMSE 


confint(reg1,level=0.95)
predict(reg1,interval="predict")
pred<-predict(reg1,interval="predict")

pred<-as.data.frame(pred)
View(pred)



reg_sqrt1<-lm(sqrt(Weight.gained..grams.)~Calories.Consumed, data=calories)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
predict(reg_sqrt1,interval="predict")

reg_log1<-lm(log(Weight.gained..grams.)~Calories.Consumed, data=calories)
summary(reg_log1)
confint(reg_log1,level=0.95)
predict(reg_log1,interval="predict")
