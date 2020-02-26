setwd("E:\\Excelr Data\\R Codes\\Hyothesis Testing")

##### Normality Test##################
library(readxl)

######## Promotion.xlsx data ###################

Promotion<-read_excel("C:/Users/tussh/Downloads/Promotion.xlsx")    # Promotion.xlsx
attach(Promotion)
View(Promotion)
colnames(Promotion)<-c("Credit","Promotion.Type","InterestRateWaiver","StandardPromotion")
# Changing column names
View(Promotion)
attach(Promotion)

#############Normality test###############

shapiro.test(InterestRateWaiver)
# p-value = 0.2246 >0.05 so p high null fly => It follows normal distribution

shapiro.test(StandardPromotion)
# p-value = 0.1916 >0.05 so p high null fly => It follows normal distribution

#############Variance test###############

var.test(InterestRateWaiver,StandardPromotion)#variance test
# p-value = 0.653 > 0.05 so p high null fly => Equal variances

############2 sample T Test ##################

t.test(InterestRateWaiver,StandardPromotion,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.02523 < 0.05 accept alternate Hypothesis 
# unequal means

?t.test
t.test(InterestRateWaiver,StandardPromotion,alternative = "greater",var.equal = T)

# alternative = "greater means true difference is greater than 0
# Null Hypothesis -> (InterestRateWaiver-StandardPromotion) < 0
# Alternative Hypothesis -> (StandardPromotion - InterestRateWaiver) > 0
# p-value = 0.01211 < 0.05 => p low null go => accept alternate hypothesis
# InterestRateWaiver better promotion than StandardPromotion


###################Proportional T Test(JohnyTalkers data)##########

Johnytalkers<-read_excel("C:/Users/tussh/Downloads/JohnyTalkers.xlsx")   # JohnyTalkers.xlsx
View(Johnytalkers) 
attach(Johnytalkers)
table1 <- table(Drinks,Person)
table1
?prop.test
prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# two. sided -> means checking for equal proportions of Adults and children under purchased
# p-value = 6.261e-05 < 0.05 accept alternate hypothesis i.e.
# Unequal proportions 
?prop.test
prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "less")
# Ha -> Proportions of Children <  Proportions of Adults
# Ho -> Proportions of Children > Proportions of Adults
# p-value = 0.999 >0.05 accept null hypothesis 
# so proportion of Children > proportion of children 
# Do not launch the ice cream shop


library(readxl)
BuyerRatio<-read.csv("C:/Users/tussh/Downloads/BuyerRatio.csv") # Bahaman.xlsx
View(BuyerRatio)
Stacked_Data <- stack(BuyerRatio)
View(Stacked_Data)
attach(Stacked_Data)
table(East,West)
table(North,South)
t2 <- prop.table(table(East))
t1 <- table(West)
?chisq
chisq.test(table(West,East))
#########Chi Square(Bahaman Research)#################

library(readxl)
Bahaman<-read_excel("C:/Users/tussh/Downloads/Bahaman.xlsx") # Bahaman.xlsx
View(Bahaman)
attach(Bahaman)
table(Country,Defective)

t2 <- prop.table(table(Defective))
t1 <- table(Country)
chisq
chisq.test(table(Country,Defective))
# p-value = 0.6315 > 0.05  => Accept null hypothesis
# => All countries have equal proportions 

#############Anova (Contract_Renewal Data )##########

CRD<-read_excel(file.choose())   # ContractRenewal_Data(unstacked).xlsx
View(CRD)
Stacked_Data <- stack(CRD)
View(Stacked_Data)
attach(Stacked_Data)

shapiro.test(CRD$`Supplier A`)
shapiro.test(CRD$`Supplier B`)
shapiro.test(CRD$`Supplier C`)
summary(CRD)
# Data is normally distributed
library(car)
leveneTest(values~ ind, data = Stacked_Data)
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)
# p-value = 0.104 > 0.05 accept null hypothesis 
# All Proportions all equal 
BuyerRatio<-read.csv("C:/Users/tussh/Downloads/BuyerRatio.csv") 
View(BuyerRatio)
Stacked_Data <- stack(BuyerRatio)
View(Stacked_Data)
attach(Stacked_Data)

shapiro.test(Stacked_Data$`East`)
shapiro.test(Stacked_Data$`ind`)



LabTAT<-read.csv("C:/Users/tussh/Downloads/LabTAT.csv") 
View(LabTAT)
Stacked_Data <- stack(LabTAT)
View(Stacked_Data)
attach(Stacked_Data)
shapiro.test(LabTAT$`Laboratory.1`)
shapiro.test(LabTAT$`Laboratory.2`)
shapiro.test(LabTAT$`Laboratory.3`)
shapiro.test(LabTAT$`Laboratory.4`)
summary(CRD)

library(car)
leveneTest(values~ ind, data = Stacked_Data)
Anova_results <- aov(values~ind,data = Stacked_Data)
summary(Anova_results)


# Customer order form 
# Unstacked data 

cof<-read_excel(file.choose()) # customer_order(unstacked).xlsx
View(cof) # countries are in their own columns; so we need to stack the data 
str(cof)
stacked_cof<-stack(cof)
attach(stacked_cof)
View(stacked_cof)
table(stacked_cof$ind,stacked_cof$values)
chisq.test(table(stacked_cof$ind,stacked_cof$values))


################# Mood's Median Test #################
install.packages("RVAideMemoire")
library(RVAideMemoire)
height <- read_excel(file.choose())
height
table(height$Treatment)

attach(height)
mood.medtest(Growth ~ Treatment,data = height,exact = FALSE)

############### Kruskal Wallis #################
kruskal.test(Growth,Treatment)



Faltoons<-read.csv("C:/Users/tussh/Downloads/Faltoons.csv")   
View(Faltoons) 
attach(Faltoons)
table1 <- table(Weekdays,Weekend)
table1
?prop.test
prop.test(x=c(66,47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
