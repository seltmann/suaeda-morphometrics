library(datasets)
head(iris)
library(ggplot2)



ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster

table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$cluster)) + geom_point()

