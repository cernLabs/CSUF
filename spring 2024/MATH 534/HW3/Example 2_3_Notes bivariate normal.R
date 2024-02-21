sqrtm <- function (A) {
  # Obtain matrix square root of a matrix A
  a = eigen(A)
  sqm = a$vectors %*% diag(sqrt(a$values)) %*% t(a$vectors)
  sqm = (sqm+t(sqm))/2
}

# Generate data

gen <- function(n,p,mu,sig,seed = 534){
  #---- Generate data from a p-variate normal with mean mu and covariance sigma
  # mu should be a p by 1 vector
  # sigma should be a positive definite p by p matrix
  # Seed can be optionally set for the random number generator
  set.seed(seed)
  # generate data from normal mu sigma
  z = matrix(rnorm(n*p),n,p)
  datan = z %*% sqrtm(sig) + matrix(mu,n,p, byrow = TRUE)
  datan
}


likemvn <- function (x,mu,sig=NULL,siginv = NULL, gcomp = FALSE) {
  # computes the likelihood and the gradient for multivariate normal
  # if gcomp=FALSE, then the gradient is not computed
  # x is the n by p data matrix
  # mu is the mean
  # sig is the covariance
  # gcomp if TRUE, the gradient with respect to mu will be output
  if(is.null(sig) && is.null(siginv)) stop("At least on of sig or its inverse must be input")
  a = dim(x)
  n = a[1]
  p = a[2]
  if(is.null(siginv)){
    siginv = solve(sig)
  }
  C= matrix(0,p,p); # initializing sum of (xi-mu)(xi-mu)^T
  sxm = matrix(0,p,1) # initializing sum of xi-mu
  gradm = sxm; # initializing this sum is used for the gradient w.r.t. mu
  for (i in 1:n){
    xm = x[i,] - mu
    sxm = sxm + xm
    C = C + xm %*% t(xm)
  }
  if(gcomp==TRUE){
    gradm = siginv %*% sxm
  }
  # --- Note that trace(siginv %*% C) = sum(siginv*C)
  if(is.null(siginv)){
    log_det_sig <- log(det(sig))
  } else {
    #-- in this case siginv is input so we use the fact that det(sig)=1/det(siginv)
    log_det_sig <-log(1/det(siginv))
  }
  l = -(n*p*log(2*pi)+n*log_det_sig + sum(siginv * C ))/2
  list(l = l, gradm = if(gcomp) gradm)
}

# --- vectorized likelihood function, as a function of mu, for use in the outer function
# Our aim is to plot the log-likelihood as a function of mu
loglike = function(mu1,mu2, datan = NULL, siginv = NULL) {
  # log-likelihood of bivariate normal is computed at vectors of mu1 and mu2
  # sigma , and the data are assumed as global variables 
  n1 = length(mu1)
  l = numeric(length(n1))
  for (i in 1:n1){
    l[i]= likemvn(x = datan, mu = c(mu1[i],mu2[i]), siginv = siginv)$l
  }
  l
}


# Steepest ascent and Newton iterations

optmvn <- function (mu, datan, siginv , maxit = 20, method = "SA") {
  #mu is a vector of initial values for mu
  #datan is the data matrix
  #siginv, is assumed known and is inverse of sig
  # method is one of "Newton" or "SA" (standing fro steepest ascent)
  # # maxit is the maximum number of iterations
  n <- nrow(datan)
  path = t(mu)
  for (it in 1:maxit){
    a <- likemvn(x = datan, mu = mu, siginv = siginv, gcomp = TRUE) 
    if(method == "Newton") dir <-  sig %*% a$grad/n # Newton's Method
    if(method == "SA") dir <-  a$grad  # Steepest Ascent
    mu1 = mu + dir # Newton's Method
    atmp = likemvn(x = datan, mu = mu1, siginv = siginv, gcomp = FALSE)
    halve = 0;
    print(c(it, halve,a$l))
    print(c(it, halve,atmp$l))
    while (atmp$l < a$l & halve <= 20){
      halve = halve+1
      mu1 = mu + dir/2^halve  # Steepest Ascent
      atmp = likemvn(x = datan, mu = mu1, siginv = siginv, gcomp = FALSE)
      print(c(it, halve,atmp$l))
    }
    if (halve >= 20) print('Step-halving failed after 20 halvings')
    mu = mu1
    path = rbind(path,t(mu1))
    print(sprintf('it = %2.0f   mu1 = %12.12f    mu2 = %12.12f  l = %12.12f',
                  it,mu1[1],mu1[2],atmp$l))
    print ('-----------------------------------------')
  }
  path
}

#--- generating data and graphing
# Generate data inside f, (mu1 and mu2 can be entered as vectors, 
# if evaluation at several values of mu1 and mu2 is to be done)
n <- 200
p <- 2
sig <<- matrix(c(1,.7,.7,1),2,2) # known sigma Note <<- makes it global
siginv <- solve(sig)
mu <- matrix(c(0,5),2,1) # generated data from  mu(0,5) 
datan = gen(n,p,mu,sig,seed = 111)
#--
#--- begin graphing th log-likelihood
# Graph the log-likelihood function as a function of mu1 and mu2, 
# We assume that sigma is known
mu1 = seq(-1,1,len=50)
mu2 = seq(4,6,len=50)
l = outer(mu1,mu2,FUN = loglike, datan = datan, siginv = siginv)
options(rgl.useNULL = TRUE) # Added this since rgl does not work with the new version of Xquartz
library (rgl)
bg3d("white")
material3d(col="blue")
open3d()
persp3d(mu1, mu2, l, aspect=c(1,1,1),col = "blue")
contour(mu1,mu2,l,nlevels=100,xlab='mu1',ylab='mu2')
abline(v=mean(datan[,1]),lty=2,col='brown')
abline(h=mean(datan[,2]),lty=2,col='brown')
points(mean(datan[,1]), mean(datan[,2]),type = "p", col = "red", pch = 19, lwd=3)

mu = matrix(c(-0.9,4),2,1)
#mu = matrix(c(0.7,4),2,1)
#mu = matrix(c(-.5,4),2,1)
path = optmvn(mu, datan, siginv , maxit = 20, method = "SA")

for (i in 1: (nrow(path)-1)){
  segments(path[i,1],path[i,2],path[i+1,1],path[i+1,2],col='red', lwd = 2)
}
path = optmvn(mu, datan, siginv, maxit = 10, method = "Newton")
for (i in 1: (nrow(path) -1)){
  segments(path[i,1],path[i,2],path[i+1,1],path[i+1,2],col='forestgreen', lwd = 2)
}

#--- using different data
sig <<- matrix(c(1,.9,.9,1),2,2) # known sigma Note <<- makes it global
siginv <- solve(sig)
mu <- matrix(c(0,5),2,1) # generated data from this mu
datan = gen(n,p,mu,sig,seed= 534)
l = outer(mu1,mu2,FUN = loglike, datan = datan, siginv = siginv)
options(rgl.useNULL = TRUE) # Added this since rgl does not work with the new version of Xquartz
library (rgl)
bg3d("white")
material3d(col="blue")
open3d()
persp3d(mu1, mu2, l, aspect=c(1,1,1),col = "blue")
contour(mu1,mu2,l,nlevels=100,xlab='mu1',ylab='mu2')
abline(v=mean(datan[,1]),lty=2,col='brown')
abline(h=mean(datan[,2]),lty=2,col='brown')
points(mean(datan[,1]), mean(datan[,2]),type = "p", col = "red", pch = 19, lwd=3)
# Begin steepest ascent
mu = matrix(c(-.5,4),2,1)
#mu = matrix(c(0.7,4),2,1)
path = optmvn(mu, datan, siginv , maxit = 100, method = "SA")
for (i in 1: (nrow(path)-1)){
  segments(path[i,1],path[i,2],path[i+1,1],path[i+1,2],col='red', lwd = 2)
}

#begin Newton
mu = matrix(c(-.5,4),2,1)
#mu = matrix(c(0.7,4),2,1)
path = optmvn(mu, datan, siginv, maxit = 10, method = "Newton")
for (i in 1: (nrow(path) -1)){
  segments(path[i,1],path[i,2],path[i+1,1],path[i+1,2],col='forestgreen', lwd = 2)
}

