#load R packages
library(vegan)
library(ape)
library(ggplot2)
library(grDevices)


#set working directory
setwd("~/Documents/suaeda-morphometrics")
setwd("~/CCBER/Suaeda/suaeda-morphometrics")

#read data into program
dat <- read.csv("leaf_shape.csv",stringsAsFactors=TRUE, header=TRUE)

help("read.csv")

#explore data
head(dat)
summary(dat)

###############simple plot based on one variable and location##################
ggplot(dat, aes(length,"1", color = location)) + geom_point()
ggplot(dat, aes(location,length, color = location)) + geom_point()
ggplot(dat, aes(location, area, color = location)) + geom_point()

###########clustering########################
#clustering ### the good test data
set.seed(32297)
d <- data.frame(x=runif(100),y=runif(100))

#clustering ### the real data
x <- dat$area
y <- dat$length
d <- data.frame(x,y)
head(d)

#####k-means#####################################
clus <- kmeans(d, centers=4)
d$cluster <- clus$cluster

help(kmeans)

#plot
h <- do.call(rbind,
             lapply(unique(clus$cluster), 
                    function(c) { f <- subset(d,cluster==c); f[chull(f),]}))

ggplot() +
  geom_text(data=d,aes(label=cluster, x=x, y=y,
                       color=cluster), size=3) +
  geom_polygon(data=h,aes(x=x, y=y, group=cluster, fill=as.factor(cluster)),
               alpha=0.4,linetype=0) +
  theme(legend.position = "none")

############end k-means##########################
##############hierachical clustering###############
vars.to.use <- colnames(dat)[3:8]
pmatrix <- scale(dat[,vars.to.use])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")
plot(pfit, labels=dat$location, horiz=TRUE, cex=0.4)

help(hclust)



#write.csv(test, "../test.txt")
#end here