#####Principal Component Analysis

#Lets start with a very simple example in which we have two predictors that we'd like to condense to one predictor.  We start with this case because it's easy to plot things in 2 dimensions.

x1 = rnorm(30,10,5)
x2 = x1*.5 + rnorm(30,5,sqrt(7))
y = 2*x1 + 1*x2 + rnorm(30,0,3)

model1 = lm(y~x1+x2)
model2 = lm(y~x1)
model3 = lm(y~x2)
summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared

#Notice that the model using x1 and x2 performs better than either of the models using just x1 or x2.  If we want to do dimension reduction by dropping weaker predictors (x2) we'd settle on model 2.  

x1.0 = x1 - mean(x1)
x2.0 = x2 - mean(x2)
X = data.frame(x1 = x1.0,x2 = x2.0)
C = cov(X)
C

###Find the eigen values and vectors or the covariance matrix

eigen.values = eigen(C)$values
eigen.vectors = eigen(C)$vectors
eigen.values
eigen.vectors

###Verifying they are eigen vectors and values

C %*% eigen.vectors
cbind(eigen.values[1]*eigen.vectors[,1],eigen.values[2]*eigen.vectors[,2])

###Notice that our eigen.vectors are unit eigen vectors (i.e.) if I square all the values in the eigen vector and sum them they sum to 1.

###Notice what happens when we plot the eigen vectors on top of the original data.  
###To plot a two dimensional vector we plot a line whose intercept is 0 and whose slope is (rise/run) eigen.vector[2]/eigen.vector[1]
plot(x1.0,x2.0,xlab = "x1 standardized",ylab="x2 standardized",main = "")
abline(0,eigen.vectors[2,1]/eigen.vectors[1,1],col = 2,lwd=2,lty=2)
abline(0,eigen.vectors[2,2]/eigen.vectors[1,2],col = 2,lwd=2,lty=2)
lines(c(-9.3,-8.3),c(3,3),col=2,lwd=2,lty=2)
text(-6,3,"Eigen Vectors")

###Obviously one of the eigen vectors is characterizing the relationship between the two predictors.  It turns out this will always be the one with the highest eigen value.  The magnitude of the eigen values indicate how well the eigenvector is mapping to the data.  

k = (1:2)[eigen.values==max(eigen.values)]
plot(x1.0,x2.0)
abline(0,eigen.vectors[2,k]/eigen.vectors[1,k],col=2,lwd=2,lty=2)
lines(c(-9.3,-8.3),c(3,3),col=2,lwd=2,lty=2)
text(-6,3,"Eigen Vector")
text(-6,2,"w Max Eigen Value")

#We'd call the two eigen-vectors principal components.  Obviously the larger the eigen value, the more important the principal component is and if we ignore principal components with smaller eigen values this performs the dimension reduction with little loss of information. 

#A feature vector is a matrix that combines the eigen-vectors we're choosing to keep as principal components.  For our example we most likely only need one eigenvector

feature.vector = matrix(eigen.vectors[,k],ncol = length(k))


#our new observed predictor will be feature.vector[1]*x1 + feature.vector[2]*x2

new.pred =t(t(feature.vector) %*% t(as.matrix(X)))
model4 = lm(y~new.pred)


model1 = lm(y~x1+x2)
model2 = lm(y~x1)
model3 = lm(y~x2)
summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared
summary(model4)$r.squared

#We often articulate the efficiency of principal components based on how much of the total variability: var(x1) + var(x2) + ... + var(xk)

#is present in the principal components var(PC1) + var(PC2) + ...
#As soon as principal components aren't contributing significant variance we stop.

#Note the var(PC1) + ... + var(PCk) = var(x1) + var(x2) + ... + var(xk)

pct.var = var(new.pred)/sum(diag(C))
pct.var


#Obviously this seems like a lot of work, there are some nice built in packages for PCA in R.  

pca = prcomp(X)
pca
names(pca)
plot(pca,type="l")

#Now for a more sophisticated example. iris is a built in dataset to R
#We're tryign to predict Species based on four predictors, but we'd like to reduce the dimensionality of our predictor space. We're going to log transform our predictors (the motivation for why is not important).
data(iris)
head(iris)

log.ir = log(iris[,1:4])
ir.species = iris[,5]
ir.pca = prcomp(log.ir,center=TRUE,scale. = TRUE)
names(ir.pca)
ir.pca$x
ir.pca$rotation
plot(ir.pca,type="l",main = "Variance of Principal Components")

#A really fun plot for looking at how all of our predictors map onto the first two principal components. There is a biplot package in R but the gg plot version is so much nicer. It's not on CRAN so I've uploaded the source code into our course code files.
#Download packages plyr, scales, grid, ggplot2
source("ggbiplot.R")
g = ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
groups = ir.species, ellipse = TRUE,
circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
legend.position = 'top')
g


