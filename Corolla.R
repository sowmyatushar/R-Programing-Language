getwd()#gives working directory

Corolla<- read.csv ("C:/Users/tussh/Documents/Multi Linear Regression/ToyotaCorolla.csv")
View(Corolla)

attach(Corolla)

qqnorm(Quarterly_Tax)
qqline(HP)

summary(Corolla) # Explore the data

plot(HP,Price) # Plot relation ships between each X with Y
plot(cc,Doors)
## Or make a combined plot
pairs(Corolla)   # Scatter plot for all pairs of variables
cor(HP,Price)
cor(Corolla) # correlation matrix

# The Linear Model of interest
model.Corolla <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
#model.car <- lm(MPG~., data = Cars)# lm(Y ~ X)
summary(model.Corolla)

model.carV<-lm(Price~cc)
summary(model.carV)

model.carW <- lm(Price~Doors)
summary(model.carW)

model.carVW <- lm(Price~cc+Doors)
summary(model.carVW)

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
cor(Corolla)
cor2pcor(cor(Corolla))


# Diagnostic Plots
install.packages(car)
library(car)
plot(model.Corolla)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
qqPlot(model.Corolla) # QQ plots of studentized residuals, helps identify outliers

# Deletion Diagnostics for identifying influential variable
influence.measures(Corolla)
influenceIndexPlot(model.Corolla)#id.n=3 is name first 3 variable # Index Plots of the influence measures
influencePlot(model.Corolla) # A user friendly representation of the above

## Regression after deleting the 81th observation
model.Corolla1<-lm(Price~ ., data = Corolla[-81,-222,-961])
#model.Corolla1<-lm(Price~ ., data = Corolla[-222,])
#model.Corolla1<-lm(Price~ ., data = Corolla[-,])
summary(model.Corolla1)
#Variance < 10 so what next??
#R Squared gives importance of input to get output. Adjusted R significance for my model wheather input variable is actally giveing pricted output  is explaining the 
#Backward, forward,stepwise treatment if vif is > 10 => Feature engineering.
### Variance Inflation Factors
vif(model.Corolla)  # VIF is > 10 => collinearity problem
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
stepAIC(model.Corolla) # backward

plot(model.Corolla)


model.final <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax,Weight, data=Corolla[-81,-222,-961])
summary(model.final)


#model.final1 <- lm(Price~ ., data=Corolla)            ])
#summary(model.final1)
avPlots(model.final, id.n=0, id.cex=0.8, col="red")
?avPlots()

vif(model.final)

# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.