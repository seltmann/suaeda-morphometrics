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
ggplot(dat, aes(length,"1", color = location)) + geom_point()
ggplot(dat, aes(location,length, color = location)) + geom_point()
ggplot(dat, aes(location, area, color = location)) + geom_point()


# listwise deletion of missing 
mydata <- na.omit(dat)
head(mydata)
df <- scale(mydata[3:9])
head(df)
wssplot(df) 

# not working k-means
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data[8])-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       #ylab="Within groups sum of squares")}

#write.csv(test, "../test.txt")
#end here