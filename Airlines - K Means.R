######################### K means clustering################

install.packages("plyr")#For data manupulation 
library(plyr)
#rnuif produce uniform random number
x <-  runif(50) # generating 50 random numbers or rnorm for normal distribution.
x

y <-  runif(50) # generating 50 random numbers 
y
#cbind columnn bind function is used to combine vectors, matrices and/or data frames by columns.
data <- cbind(x,y) 
data

plot(data) #scatter plot

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

#wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))
# Determine number of clusters by scree-plot 
twss = c() #totalwithinsumofsquare.
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


