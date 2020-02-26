Salary = read.csv("C:/Users/tussh/Downloads/emp_data.csv")
View(Salary)

library(lattice)
dotplot(Salary$Salary_hike, main="Dot Plot of Sorting time")
dotplot(Salary$Churn_out_rate, main="Dot Plot of Delivery time")
boxplot(Salary$Salary_hike,col="dodgerblue4")
boxplot(Salary$Churn_out_rate,col="red",horizontal = T)


hist(Salary$Salary_hike)
hist(Salary$Churn_out_rate)

qqnorm(Salary$Salary_hike)
qqline(Salary$Churn_out_rate)

#scatter plot
plot(Salary$Salary_hike,Salary$Churn_out_rate)

attach(Salary)

cor(Salary$Salary_hike,Salary$Churn_out_rate)
#plot(Waist,AT)
#cor(wc.at$Waist , wc.at$AT)

#linear regression model
reg <- lm(Churn_out_rate~Salary_hike, data=Salary) # Y ~ X
summary(reg)

reg$coefficients
reg$residuals

-215.98 + 3.45*(74.75)
41.90-25.75

pred<- predict(reg,interval="confidence")

pred<- as.data.frame(pred)



sqrt(sum(reg$residuals^2)/nrow(Salary)) ## RMSE 


pred
View(pred)
??predict

cor(pred$fit, Salary$Churn_out_rate)
plot(Salary_hike,Churn_out_rate)
cor(Churn_out_rate,sqrt(Salary_hike))
# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Churn_out_rate~sqrt(Salary_hike), data=Salary)
summary(reg_sqrt)
cor(sqrt(Salary_hike),Churn_out_rate)
plot(sqrt(Salary_hike),Churn_out_rate)



sqrt(sum(reg_sqrt$residuals^2)/nrow(Salary)) ## RMSE 



confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")

cor(Churn_out_rate,log(Salary_hike))
plot(Churn_out_rate,log(Salary_hike))

reg_log<-lm(Churn_out_rate~log(Salary_hike), data=Salary)
summary(reg_log)

sqrt(sum(reg_log$residuals^2)/nrow(Salary)) ## RMSE 


confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

cor(log(Churn_out_rate),Salary_hike*Salary_hike)

reg1<-lm(log(Churn_out_rate)~Salary_hike+ I(Salary_hike*Salary_hike), data=Salary)
summary(reg1)
pred3 = as.data.frame(predict(reg1, interval = "prediction"))
sqrt((sum(reg1$residuals^2))/nrow(Salary))

y = exp(pred3$fit)
error  = y - Salary$Churn_out_rate
sqrt(sum(error^2)/nrow(Churn_out_rate)) ## RMSE 


confint(reg1,level=0.95)
predict(reg1,interval="predict")
pred<-predict(reg1,interval="predict")

pred<-as.data.frame(pred)
View(pred)



#linear model regression with Sqrt
reg_sqrt1<-lm(sqrt(Churn_out_rate)~Salary_hike, data=Salary)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
predict(reg_sqrt1,interval="predict")


#linear model regression with log
reg_log1<-lm(log(Churn_out_rate)~Salary_hike, data=Salary)
summary(reg_log1)
confint(reg_log1,level=0.95)
predict(reg_log1,interval="predict")
