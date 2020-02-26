Salary = read.csv("C:/Users/tussh/Documents/Simple Linear Regression/Salary_Data.csv")
View(Salary)

library(lattice)
dotplot(Salary$Salary, main="Dot Plot of Sorting time")
dotplot(Salary$YearsExperience, main="Dot Plot of Delivery time")
boxplot(Salary$Salary,col="dodgerblue4")
boxplot(Salary$YearsExperience,col="red",horizontal = T)


hist(Salary$Salary)
hist(Salary$YearsExperience)

qqnorm(Salary$Salary)
qqline(Salary$YearsExperience)

#scatter plot
plot(Salary$Salary,Salary$YearsExperience)

attach(Salary)

cor(Salary$Salary,Salary$YearsExperience)


#linear regression model
reg <- lm(Salary~YearsExperience, data=Salary) # Y ~ X
summary(reg)

reg$coefficients
reg$residuals

-215.98 + 3.45*(74.75)
41.90-25.75

pred<- predict(reg,interval="confidence")

pred<- as.data.frame(pred)


#rootmean square error
sqrt(sum(reg$residuals^2)/nrow(Salary)) ## RMSE 


pred
View(pred)
??predict

cor(pred$fit, Salary$YearsExperience)
plot(Salary,YearsExperience)
cor(Salary,sqrt(YearsExperience))
# transform the variables to check whether the predicted values are better
reg_sqrt <- lm(Salary~sqrt(YearsExperience), data=Salary)
summary(reg_sqrt)
cor(sqrt(Salary),YearsExperience)
plot(sqrt(Salary),YearsExperience)



sqrt(sum(reg_sqrt$residuals^2)/nrow(Salary)) ## RMSE 



confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval="predict")

cor(Salary,log(YearsExperience))
plot(Salary,log(YearsExperience))

reg_log<-lm(Salary~log(YearsExperience), data=Salary)
summary(reg_log)

sqrt(sum(reg_log$residuals^2)/nrow(Salary)) ## RMSE 


confint(reg_log,level=0.95)
predict(reg_log,interval="predict")

cor(log(Salary),YearsExperience*YearsExperience)

reg1<-lm(log(Salary)~YearsExperience+ I(YearsExperience*YearsExperience), data=Salary)
summary(reg1)
pred3 = as.data.frame(predict(reg1, interval = "prediction"))
sqrt((sum(reg1$residuals^2))/nrow(Salary))

y = exp(pred3$fit)
error  = y - Salary$YearsExperience
sqrt(sum(error^2)/nrow(Salary)) ## RMSE 


confint(reg1,level=0.95)
predict(reg1,interval="predict")
pred<-predict(reg1,interval="predict")

pred<-as.data.frame(pred)
View(pred)



#linear model regression with Sqrt
reg_sqrt1<-lm(sqrt(Salary)~YearsExperience, data=Salary)
summary(reg_sqrt1)
confint(reg_sqrt1,level=0.95)
predict(reg_sqrt1,interval="predict")


#linear model regression with log
reg_log1<-lm(log(Churn_out_rate)~Salary_hike, data=Salary)
summary(reg_log1)
confint(reg_log1,level=0.95)
predict(reg_log1,interval="predict")