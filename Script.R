#load R packages
library(vegan)
library(ape)
library(ggplot2)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#set working directory
setwd("~/Documents/suaeda-morphometrics")

#read data into program
dat <- read.csv("leaf_shape.csv",stringsAsFactors=TRUE)

help("read.csv")

#explore data
head(dat)
summary(dat)

#plot
ggplot(dat, aes(location,length, color = location)) + geom_point()
ggplot(dat, aes(location,area, color = location)) + geom_point()

# listwise deletion of missing 
mydata <- na.omit(dat)
head(mydata)

data2 <- mydata[,-2]
#data2 <- mydata[,1]
head(data2)
rownames(data2) <- data2[,1]
princ <- prcomp(data2)

mydata <- scale(mydata)


# Determine number of clusters
wss <- (nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

sCluster$cluster <- as.factor(sCluster$cluster)
ggplot(dat, aes(location, length, color = sCluster$cluster)) + geom_point()

#write.csv(test, "../test.txt")
#end here