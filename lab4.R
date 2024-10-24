#installing cluster package
install.packages("cluster")

#importing iris file
iris <- read.csv("C:/Users/veron/OneDrive/Desktop/CIS 389/week4/iris.csv")

#displaying first 6 elements of iris dataframe
summary(iris)

#loading cluster library
library(cluster)

#creating new dataframe out of petallength and petalwidth
kmeans_variables = data.frame(iris$petallength,
                              iris$petalwidth)

#converting vectors of characters to factors
pClass <- as.factor(iris$class)

#displaying data
plot(kmeans_variables, col=pClass)

#applying k-means
KMC = kmeans(kmeans_variables, centers=3, iter.max=50,
             nstart=20)

#display output for above clusterings
KMC

#show data point assigned with which cluster
KMC$cluster

#give details of cluster centroids of each cluster
KMC$centers

#give number of data points inside each cluster
KMC$size

#shows which cluster got which cluster
table(KMC$cluster, iris$class)

#plotting the k-mean
clusplot(iris, KMC$cluster, color=TRUE, shade=TRUE,
         lines=0)

#more simple plot use
plot(kmeans_variables, col=KMC$clutser)



