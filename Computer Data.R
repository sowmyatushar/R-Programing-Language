getwd()#gives working directory

Computer<- read.csv ("C:/Users/tussh/Downloads/Computer.csv")
View(Computer)

attach(Computer)

qqnorm(screen)
qqline(ram)

summary(Computer) # Explore the data

plot(hd,price) # Plot relation ships between each X with Y
plot(ram,price)
plot(price,screen)

## Or make a combined plot
pairs(Computer)   # Scatter plot for all pairs of variables
cor(trend,price)
cor(Computer) # correlation matrix
?cor
# The Linear Model of interest
model.Computer <- lm(price~ ., data = Computer)
#model.car <- lm(MPG~., data = Cars)# lm(Y ~ X)
summary(model.Computer)

model.carV<-lm(price~hd)
summary(model.carV)
model.cars<-lm(price~speed)
summary(model.cars)
model.carW <- lm(price~screen)
summary(model.carW)
model.carr<-lm(price~ram)
summary(model.carr)
model.cara<-lm(price~ads)
summary(model.cara)
model.cart<-lm(price~trend)
summary(model.cart)
model.carVW <- lm(price ~ram+hd)
summary(model.carVW)
?exp
#######                    Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Cars, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

###                                   Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
?cor
cor(Computer)
cor2pcor(cor(Cars))


# Diagnostic Plots
install.packages(car)
library(car)
plot(model.car)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
View(Computer)

qqPlot(model.car, id.n=5) # QQ plots of studentized residuals, helps identify outliers

# Deletion Diagnostics for identifying influential variable
influence.measures(model.car)
influenceIndexPlot(model.car, id.n=3)#id.n=3 is name first 3 variable # Index Plots of the influence measures
influencePlot(model.car, id.n=3) # A user friendly representation of the above

## Regression after deleting the 77th observation
model.car1<-lm(price~ ., data=Computer[-1441,-1701])
summary(model.car1)


### Variance Inflation Factors
vif(model.car)  # VIF is > 10 => collinearity
VIFWT<-lm(WT~VOL+HP+SP)
VIFVOL<-lm(VOL~WT+HP+SP)
VIFHP<-lm(HP~VOL+WT+SP)
VIFSP<-lm(SP~VOL+HP+WT)
summary(VIFWT)
summary(VIFVOL)
summary(VIFHP)
summary(VIFSP)
#### Added Variable Plots ######
avPlots(model.car, id.n=2, id.cex=0.8, col="red")

library("MASS")
stepAIC(model.car) # backward

plot(model.car)

model.final <- lm(MPG~VOL+HP+SP, data=Cars)
summary(model.final)


model.final1 <- lm(price~ ., data=Computer[-1441,-1701])
summary(model.final1)

avPlots(model.final1, id.n=2, id.cex=0.8, col="red")
?avPlots
vif(model.final1)
?vif
# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.