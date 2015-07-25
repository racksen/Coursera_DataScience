#' PCA (Principal Componenet Analysis) and 
#' SVD (Single Value Decomposition) 
#' Both are techniques used in EDA phase as well as in Formal Modeling Phase
#' Let see how these techniques are used in EDA Phase 

#1. Random Matrix Data with noise
set.seed(12345)
par(mfrow= c(1,1), rep(0.5,4))
dataMatrix <- matrix(rnorm(400), nrow=40)
image(1:10,1:40, t(dataMatrix)[, nrow(dataMatrix):1])

#2. Cluster analysis with heat map
heatmap(dataMatrix)

#3. Adding some pattern
# we are arbitrarily introduced pattern in data: we flip a coin and if the it is heads, we replace the row with [0, 0, 0, 0, 0, 3, 3, 3, 3, 3]
set.seed(678910)
for(i in 1:40){
  # flip a coin
  coinFlip <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  if(coinFlip){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3),each=5)
  }
}
image(1:10,1:40, t(dataMatrix)[, nrow(dataMatrix):1]) #plotting the image for the added pattern
heatmap(dataMatrix) #clustered data


#4. visualizing the data patterns at the rows and column wise
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),40:1, xlab = "Row Mean", ylab = "Row", pch=19)
plot(colMeans(dataMatrixOrdered),ylab = "Column Mean", xlab = "Column", pch=19)

#' In real time there will be Multivariate variables
#' objective any modeling is to findout a set of variables which explains the variance of all other datapoints in the dataset.
#' So there are 2 goals => 
#' 1.) Data Compression - SVD technique = U D Vt
#' 2.) Statistical Goal - PCA technique = Right singular values of scalled matrix (-Mean, /Sigma) 

#5. SVD components 
# finding u and v matrix
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[,1],40:1, xlab = "Row", ylab = "First Left Singular Vector", pch=19)
plot(svd1$v[,1],xlab = "Column", ylab = "First Right Singular Vector", pch=19)
# variance explained
par(mfrow=c(1,2))
plot(svd1$d,xlab = "Column", ylab = "Singular Value", pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab = "Column", ylab = "Prop. Variance Explained", pch=19)

#6. Relationship between PCA and SVD
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered,scale = TRUE)
plot(pca1$rotation[,1], svd1$v[,1],  xlab = "PCA", ylab = "SVD", pch=19)
abline(c(0,1))

#7. Variance Explained with simple datasets
constantMatrix <- dataMatrixOrdered *0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1), each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[, nrow(dataMatrixOrdered):1])
plot(svd1$d, xlab = "Column", ylab = "Singular Value", pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab = "Column", ylab = "Prop. Variance Explained", pch=19)


#8
#9
#10
#11
#12 Missing Values Handling
dataMatrix2 <- dataMatrixOrdered
# randomly insert some missing data
dataMatrix2[sample(1:100,size=40, replace=FALSE)]<-NA
svd1 <- svd(scale(dataMatrix2))

#13 using impute lib to exclude NA
library(impute)
dataMatrix2 <- dataMatrixOrdered
# randomly insert some missing data
dataMatrix2[sample(1:100,size=40, replace=FALSE)]<-NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch=19)

#14 Face example
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/face.rda",destfile="./04-Exploratory Data Analysis/week 3/data/face.rda")
load("./04-Exploratory Data Analysis/week 3/data/face.rda")
image(t(faceData)[,nrow(faceData):1])

#15 Face example - Variance explained
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2),xlab = "Singular Vector", ylab = "Variance Explained", pch=19)

#16 Face example - Create approximation
approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10])
par(mfrow= c(1,4))
image(t(approx1)[,nrow(approx1):1], main = "(a)")
image(t(approx5)[,nrow(approx5):1], main = "(b)")
image(t(approx10)[,nrow(approx10):1], main = "(c)")
image(t(faceData)[,nrow(faceData):1], main = "(d)")


#17 Working with Color in R
pal <- colorRamp(c("red","blue")) # between 0 and 1
pal(0)
pal(1)
pal(0.5)
pal(seq(0,1,len=10))

pal1 <- colorRampPalette(c("red","yellow")) # takes any integer
pal1(2)
pal1(10)

#17 RColorBrewer Package
library(RColorBrewer)
cols <- brewer.pal(3, "BuGn") 
cols
pal2 <- colorRampPalette(cols)
par(mfrow= c(1,1))
image(volcano,col= pal2(20))

#18 SmoothScatter function
x<- rnorm(1000)
y<- rnorm(1000)
par(mfrow= c(1,2))
plot(x,y)
smoothScatter(x,y)

#19 Color Transparency
par(mfrow= c(1,2))
plot(x,y, pch=19)
plot(x,y, pch=19, col=rgb(0,0,0,0.2))














