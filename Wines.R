data = read.csv("C:/Users/tussh/Documents/PCA/wine.csv")

View(data)

help(princomp) # to understand the api for princomp
attach(data)
cor(data)
pcaObj<-princomp(data,cor = TRUE,scores = TRUE)
?princomp
## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

#pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
mydata<-cbind(data,pcaObj$scores[,1:3])
View(mydata)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,15:17]

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram

groups<-cutree(fit1,5) # Cutting the dendrogram for 5 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,mydata) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(1,15:17)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the wine data on membership_1

write.csv(final1,file="wine.csv",row.names = F,col.names = F)
getwd()

######################### K means clustering################

install.packages("plyr")#For data manupulation 
library(plyr)

x <-  runif(50) # generating 50 random numbers or rnorm for normal distribution.
x

y <-  runif(50) # generating 50 random numbers 
y

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



#mydata <- input[1:25,c(1,3:8)]
data= read.csv("C:/Users/tussh/Documents/PCA/wine.csv")
#input = read.csv(file.choose())

normalized_data <- scale(data[,1:14])
fit <- kmeans(normalized_data, 3) # 4 cluster solution
str(fit)
final2<- data.frame(data, fit$cluster) # append cluster membership
final2

final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]#or mention the index number or variable names

aggregate(data[,1:14], by=list(fit$cluster), FUN=mean) #find the mean of each variables.

#elbow curve & k ~ sqrt(n/2) to decide the k value

#wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))
# Determine number of clusters by scree-plot 
twss = c() #totalwithinsumofsquare.
for (i in 1:14) twss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:14, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
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


