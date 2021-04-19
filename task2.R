############### task 2 ################
#Prediction using Unsupervised ML
#k-means clustering 

# loading all the libraries required 
library(cluster)
library(ggplot2)

iris_data = iris
nrow(iris_data) #number of rows in iris_data
ncol(iris_data) #number of columns in iris_data
View(iris_data)

#visualizations of the data 

plot(iris_data$Sepal.Length,iris_data$Sepal.Width,col=iris_data$Species)
plot(iris_data$Petal.Length,iris_data$Petal.Width,col=iris_data$Species)


# scaling the variables 

iris_data$Sepal.Length <- scale(iris_data$Sepal.Length)
iris_data$Sepal.Width <- scale(iris_data$Sepal.Width)
iris_data$Petal.Length = scale(iris_data$Petal.Length)
iris_data$Petal.Width = scale(iris_data$Petal.Width)

# iris data after scaling the variables 

View(iris_data)


#plot for finding optimal k 

r_sq<- rnorm(20)

for (number in 1:20){clus <- kmeans(iris_data[,-5], centers = number, nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss
}

par(mfrow=c(1,1)) # ONLY ONE PLOT 

# plot for optimal number of clusters :

plot(1:20,r_sq,xlab = "number of clusters ",ylab="Total within clusters sum of squares",main="optimal number of clusters",type="b", pch=19)

print(paste(" the optimal k from the above plot is",3))

# the optimal number of clusters is 3

result = kmeans(iris_data[,-5], centers = 3,iter.max = 50,nstart=50)
result
result$cluster

summary(factor(result$cluster))

table(result$cluster,iris_data$Species)

# visualization of clusters


clusplot(iris_data, result$cluster, color=T, shade=T, labels=0, lines=0)


par(mfrow=c(1,1)) 

plot(iris_data$Sepal.Length,iris_data$Sepal.Width,col=iris_data$Species,main = "before")
plot(iris_data$Sepal.Length,iris_data$Sepal.Width,col=result$cluster,main="after")

par(mfrow=c(1,1))
plot(iris_data$Petal.Length,iris_data$Petal.Width,col=iris_data$Species,main = "before")
plot(iris_data$Petal.Length,iris_data$Petal.Width,col=result$cluster,main="after")


