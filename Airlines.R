library(readxl)
Airlines <- read_excel("C:/Users/tussh/Documents/Hirachical Clustering/EastWestAirlines.xlsx", sheet = 2)
summary(Airlines)
View(Airlines)

#normalize data.EDA exploratory data analysis.
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normalized_data<-as.data.frame(lapply(Airlines[,2:11],normalize))
summary(normalized_data)
cor(normalized_data)
?scale

normalized_data <- scale(Airlines[,2:11]) #excluding the ID column before normalizing
summary(normalized_data)

d <- dist(normalized_data, method = "euclidean")# distance matrix
fit <- hclust(d, method="complete") # linkage = avg or centroid or complete or single
?hclust
?dist
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=5) # cut tree into 5 clusters

?cutree
rect.hclust(fit, k=5, border="blue")
?rect.hclust

milegeoffers<-as.matrix(groups) #group to which cluster they belong

final <- data.frame(Airlines, milegeoffers)
final <- final[c("milegeoffers","Balance","Qual_miles","cc1_miles","cc2_miles","cc3_miles","Bonus_miles","Bonus_trans","Flight_miles_12mo","Flight_trans_12","Days_since_enroll","Awards")]#Shift membershipt to the first column
View(final)
#final1 <- final[,c(ncol(final),1:(ncol(final)-1))] #shifting the final1 to 1st column

aggregate(Airlines[,2:11], by=list(final$milegeoffers), FUN=mean) #to find mean of all the variables.

#stability of the model & balance is used as one of variable
require(caTools)
set.seed(105) 
?set.seed
sample = sample.split(Airlines$Balance, SplitRatio = .70)
Airlines1= subset(Airlines, sample == TRUE)
test  = subset(Airlines, sample == FALSE)
View(Airlines1)


normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normalized_data<-as.data.frame(lapply(Airlines1[,2:11],normalize))
summary(normalized_data)
cor(normalized_data)
?scale

normalized_data <- scale(Airlines1[,2:11]) #excluding the university name columnbefore normalizing
summary(normalized_data)

d <- dist(normalized_data, method = "euclidean")# distance matrix
fit <- hclust(d, method="average") # linkage = avg or centroid or complete or single
?hclust
?dist
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=5) # cut tree into 5 clusters

?cutree
rect.hclust(fit, k=5, border="red")
?rect.hclust

milegeoffers<-as.matrix(groups) #group to which cluster they belong

final <- data.frame(Airlines1, milegeoffers)
final <- final[c("milegeoffers","Balance","Qual_miles","cc1_miles","cc2_miles","cc3_miles","Bonus_miles","Bonus_trans","Flight_miles_12mo","Flight_trans_12","Days_since_enroll","Awards")]#Shift membershipt to the first column
View(final)
#final1 <- final[,c(ncol(final),1:(ncol(final)-1))] #shifting the final1 to 1st column

aggregate(Airlines1[,2:11], by=list(final$milegeoffers), FUN=mean) #to find mean of all the variables.







?write.xlsx

write.csv(final, file="final1.csv")#after samplesplit & clustering write the final file write.csv

getwd()



########################## K means clustering################

install.packages("plyr")#For data manupulation 
library(plyr)

x <-  runif(50) # generating 50 random numbers or rnorm for normal distribution.
x

y <-  runif(50) # generating 50 random numbers 
y

data <- cbind(x,y) 
data

plot(data)

plot(data, type="n")
text(data, rownames(data))

km <- kmeans(data,4)
#km <- kmeans(data,49) #kmeans clustering
str(km)

install.packages("animation")
library(animation)

km <- kmeans.ani(data, 4)
km$centers


#input <- read.xlsx("~/Desktop/Datasets_BA 2/Universities_Clustering.xlsx",1)
#mydata <- input[1:25,c(1,3:8)]
Airlines= read_excel("C:/Users/tussh/Documents/Hirachical Clustering/EastWestAirlines.xlsx", sheet = 2)
#input = read.csv(file.choose())

normalized_data <- scale(Airlines[,2:7])
fit <- kmeans(normalized_data, 4) # 4 cluster solution
str(fit)
final2<- data.frame(Airlines, fit$cluster) # append cluster membership
final2

final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]#or mention the index number or variable names

aggregate(Airlines[,2:7], by=list(fit$cluster), FUN=mean) #find the mean of each variables.

#elbow curve & k ~ sqrt(n/2) to decide the k value

#wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
twss = c()
for (i in 1:8) twss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:8, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

km$withinss
###########Clara methos #############


# k clustering alternative for large dataset - Clustering Large Applications (Clara)
install.packages("cluster")
library(cluster)
xds <- rbind(cbind(rnorm(5000, 0, 8), rnorm(5000, 0, 8)), cbind(rnorm(5000, 50, 8), rnorm(5000, 50, 8)))
xcl <- clara(xds, 2, sample = 100)
clusplot(xcl)


################Pam######################

#Partitioning around medoids or pam
xpm <- pam(xds, 2)
clusplot(xpm)


