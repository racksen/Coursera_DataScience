# Hierarchical Clustering Example
set.seed(1234)
par(mar=c(5,5,5,5))
x <- rnorm(12,mean= rep(1:3, each=4),sd=0.2)
y <- rnorm(12,mean= rep(c(1,2,1), each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05, y+0.05, labels = as.character(1:12))

# Hierarchical Clustering - dist
df <- data.frame(X=x, Y=y)
distXY= dist(df) #Euclidean Distance. the point 5 and 6 are very close

# Hierarchical Clustering - hclust
hClustering <- hclust(distXY)
plot(hClustering) # draw cluster dendrogram

# Draw prettier Clustering using the coursera custom function
myplclust(hClustering, lab= rep(1:3, each=4),lab.col = rep(1:3, each=4))

#'Merging Points 
#' - merging points will create new point. Usually its an average of x and y coordinates
#' of two points,
#' - two ways, 1.) avg of two farthest points 2.) avg of center of gravity.

# Heat Map
set.seed(143)
dataMatrix <- as.matrix(df)[sample(1:12),]
heatmap(dataMatrix)
 




