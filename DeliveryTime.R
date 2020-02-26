Delivery = read.csv("C:/Users/tussh/Documents/Simple Linear Regression/delivery_time.csv")
View(Delivery)

library(lattice)
dotplot(Delivery$Sorting.Time, main="Dot Plot of Sorting time")
dotplot(Delivery$Delivery.Time, main="Dot Plot of Delivery time")
boxplot(Delivery$Sorting.Time,col="dodgerblue4")
boxplot(Delivery$Delivery.Time,col="red",horizontal = T)


hist(Delivery$Sorting.Time)
hist(Delivery$Delivery.Time)

qqnorm(Delivery$Delivery.Time)
qqline(Delivery$Delivery.Time)

#scatter plot
plot(Delivery$Sorting.Time,Delivery$Delivery.Time)

attach(Delivery)

cor(Sorting.Time,Delivery.Time)
#plot(Waist,AT)
#cor(wc.at$Waist , wc.at$AT)

#linear regression model
reg <- lm(Delivery.Time~Sorting.Time, data=Delivery) # Y ~ X
summary(reg)

reg$coefficients
reg$residuals

-215.98 + 3.45*(74.75)
41.90-25.75

pred<- predict(reg,interval="confidence")

pred<- as.data.frame(pred)



sqrt(sum(reg$residuals^2)/nrow(wc.at)) ## RMSE 


pred
View(pred)
??predict

cor(pred$fit, Delivery$Delivery.Time)
plot(Delivery.Time,Sorting.Time)
cor(Delivery.Time,sqrt(Sorting.Time))
# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Delivery.Time~sqrt(Sorting.Time), data=Delivery)
summary(reg_sqrt)
cor(sqrt(Sorting.Time),Delivery.Time)
plot(sqrt(Sorting.Time),Delivery.Time)



sqrt(sum(reg_sqrt$residuals^2)/nrow(Delivery)) ## RMSE 



confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")

cor(Delivery.Time,log(Sorting.Time))
plot(Delivery.Time,log(Sorting.Time))

reg_log<-lm(Delivery.Time~log(Sorting.Time), data=Delivery)
summary(reg_log)

sqrt(sum(reg_log$residuals^2)/nrow(wc.at)) ## RMSE 


confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

cor(log(Delivery.Time),Sorting.Time*Sorting.Time)

reg1<-lm(log(Delivery.Time)~Sorting.Time + I(Sorting.Time*Sorting.Time), data=Delivery)
summary(reg1)
pred3 = as.data.frame(predict(reg1, interval = "prediction"))
sqrt((sum(reg1$residuals^2))/nrow(Delivery))

y = exp(pred3$fit)
error  = y - Delivery$Delivery.Time
sqrt(sum(error^2)/nrow(Delivery.Time)) ## RMSE 


confint(reg1,level=0.95)
predict(reg1,interval="predict")
pred<-predict(reg1,interval="predict")

pred<-as.data.frame(pred)
View(pred)



#linear model regression with Sqrt
reg_sqrt1<-lm(sqrt(Delivery.Time)~Sorting.Time, data=Delivery)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
predict(reg_sqrt1,interval="predict")


#linear model regression with log
reg_log1<-lm(log(Delivery.Time)~Sorting.Time, data=Delivery)
summary(reg_log1)
confint(reg_log1,level=0.95)
predict(reg_log1,interval="predict")
