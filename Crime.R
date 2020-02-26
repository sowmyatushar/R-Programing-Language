Crime = read.csv("C:/Users/tussh/Documents/Hirachical Clustering/Crime_data.csv")
summary(Crime)
View(Crime)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normalized_data<-as.data.frame(lapply(Crime[,2:5],normalize))
summary(normalized_data)
cor(normalized_data)
?scale

normalized_data <- scale(Crime[,2:5]) #excluding the university name columnbefore normalizing
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

criminals<-as.matrix(groups) #group to which cluster they belong

final <- data.frame(Crime, criminals)
final <- final[c("criminals","Murder","Assault","UrbanPop","Rape")]#Shift membershipt to the first column
View(final)
#final1 <- final[,c(ncol(final),1:(ncol(final)-1))] #shifting the final1 to 1st column

aggregate(Crime[,2:5], by=list(final$criminals), FUN=mean) #to find mean of all the variables.


require(caTools)
set.seed(101) 
?set.seed
sample = sample.split(Crime$Murder, SplitRatio = .70)
Crime1= subset(Crime, sample == TRUE)
test  = subset(Crime, sample == FALSE)



normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normalized_data<-as.data.frame(lapply(Crime1[,2:5],normalize))
summary(normalized_data)
cor(normalized_data)
?scale

normalized_data <- scale(Crime1[,2:5]) #excluding the university name columnbefore normalizing
summary(normalized_data)

d <- dist(normalized_data, method = "euclidean")# distance matrix
fit <- hclust(d, method="complete") # linkage = avg or centroid or complete or single
?hclust
?dist
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=5) # cut tree into 5 clusters

?cutree
rect.hclust(fit, k=5, border="red")
?rect.hclust

criminals<-as.matrix(groups) #group to which cluster they belong

final <- data.frame(Crime1, criminals)
final <- final[c("criminals","Murder","Assault","UrbanPop","Rape")]#Shift membershipt to the first column
View(final)
#final1 <- final[,c(ncol(final),1:(ncol(final)-1))] #shifting the final1 to 1st column

aggregate(Crime1[,2:5], by=list(final$criminals), FUN=mean) #to find mean of all the variables.







?write.xlsx

write.csv(final, file="final1.csv")#after samplesplit & clustering write the final file write.csv

getwd()


