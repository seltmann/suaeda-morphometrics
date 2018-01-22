library(datasets)
head(iris)
library(ggplot2)


ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

#now try changing for sepal.length and sepal.width



