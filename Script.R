#load R packages
library(vegan)
library(ape)
library(ggplot2)
library(grDevices)
library(cluster)
require(graphics)


#set working directory
setwd("~/Documents/suaeda-morphometrics")
#setwd("~/CCBER/Suaeda/suaeda-morphometrics")

#read data into program
dat <- read.csv("combined.csv",stringsAsFactors=TRUE, header=TRUE)

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
x <- dat$flower_diameter
y <- dat$flower_width
z <- dat$location
d <- data.frame(x,y,z)
dclus <- d[1:2]
head(dclus)

#####k-means#####################################
clus <- kmeans(dclus, centers=8)
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
##############start poster analysis here##########

vars.to.use <- colnames(dat)[3:7]
pmatrix <- scale(dat[,vars.to.use])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

#standard
d <- dist(pmatrix, method="euclidean")
#"euclidean", "maximum", "manhattan", "#canberra", "binary" or "minkowski"

#also did it with "average" and works as expected. Average is UPGMA
pfit <- hclust(d, method="average")
plot(pfit, labels=dat$location, cex=.8, hang = .1, xlab="", ylab="", main="UPGMA Flowers and leaf",axes = FALSE)
rect.hclust(pfit, k = 3, border = "red")
help(hclust)

#########calculated cophenetic correlation
cophenetic(pfit)

#principal coordinate analysis 
pcoa <- pcoa(d)
head(pcoa)
biplot(pcoa)
help(pcoa)

#matches citation of Rodrigues et. al., 2013. Gower is good for binary data, which you have none.
e<- daisy(pmatrix,metric="gower")
efit <- hclust(e, method="average")
plot(efit, labels=dat$location, cex=.9, hang = .1, xlab="", ylab="", main="UPGMA Flowers and leaf",axes = FALSE)
box()

help(plot)

par(mar=c(4.2, 4.2, 1.2, 1.2))

#cleans graphical memory
plot.new()

help(hclust)
help(daisy)



#write.csv(test, "../test.txt")
#end here