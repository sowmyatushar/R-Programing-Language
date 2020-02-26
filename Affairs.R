Affair = read.csv("C:/Users/tussh/Downloads/affairs.csv")
#install.packages('AER')
#data(Affair,package="AER")
summary(Affair)
View(Affair)
str(Affair) #structure of dataset
summary(Affair)
attach(Affair)

Affair$affairs =as.factor(Affair$affairs)
summary(Affair$affairs)
Affair$gender = factor(Affair$gender,levels = c('female', 'male'),labels = c(0, 1))
Affair$children = factor(Affair$children,levels = c('no', 'yes'),labels = c(0, 1))
View(Affair)
#claimants = na.omit(claimants) # claimants dataset with NA will be omitted.


#creating dummy variable
#install.packages("dummies")
#library(dummies)
#Affair$gender <- dummy.data.frame(Affair$gender,sep = "_")
#dummies = dummy(Affair$gender, sep = "_")
#dummies1 = dummy(Affair$children, sep = "_")
# Logistic Regression
#glm =>generalised linear model 

logit = glm(affairs ~ factor(gender)  + age + yearsmarried 
          + factor(children) + factor(religiousness) + factor(education) + factor(occupation) + factor(rating),family= "binomial",data=Affair)
?glm
summary(logit)
?logit


# Odds Ratio


# Confusion Matrix Table

prob=predict(logit,type=c("response"),Affair)
prob

confusion<-table(prob>0.6,Affair$affairs)
confusion


# Model Accuracy

Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy

