library(datasets)
head(iris)
library(ggplot2)


ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

#now try changing for sepal.length and sepal.width

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = species)) + geom_point()

#UPGMA hiearchical clustering

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster
