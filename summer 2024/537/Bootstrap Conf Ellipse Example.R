#Bootstrapping confidence region.

#Step 1:  Sample n obs from original data with replacement
#need to keep rows together.

n = dim(data)[1]
index = sample(1:n,n,replace=T)
BS.data = data[index,]

#Step 2:  Compute BS.xbar, I use the apply function.
#Sure a bunch of you are using tidyverse ect.  Doesn't matter.

BS.xbar = apply(BS.data,2,mean)

#Step 3:  Repeat steps 1 and 2, 10,000 times.

BS.xbars = matrix(0,10000,2)
for(i in 1:10000){
	index = sample(1:n,n,replace=T)
	BS.data = data[index,]
	BS.xbars[i,] = apply(BS.data,2,mean)
}

#Step 4:  Add Mahalanoobis distance to each row in 
#BS.xbars.  This should be the distance between
#original x.bar and each BS xbar.  For covariance
#matrix we should be using S/n, where S is covariance
#matrix for our sample data

xbar = apply(data,2,mean)
S.xbar = cov(data)/n
MD = rep(0,10000)
for(i in 1:10000){
	MD[i] = t(xbar-BS.xbars[i,])%*%solve(S.xbar)%*%(xbar - BS.xbars[i,])
}

#Step 5:  Compute the cutoff for the smallest 95% of Mahalanoobi distances
#only keep those bootstrap values.  Plot them.  Note, I'm going to run my
#code from last class to make my ellipse.

final.BS.xbars = BS.xbars[MD <= sort(MD)[9500],]

#Step 6, plotting stuff (most of it from previous code file)

attach(data)
plot(x,y,xlim=c(min(x),max(x)),ylim=c(min(y),max(y)))
p=2

x.bar = mean(x)
y.bar = mean(y)

#set up vectors and matrices for x.bar.vec, mu0.vec and sigma.xbar.mat

x.bar.vec = matrix(c(x.bar,y.bar),ncol=1)
mu0.vec = matrix(c(1.5,1.5),ncol=1)
S.x.bar = matrix(var(data),ncol=2)/n
T2 = t(x.bar.vec-mu0.vec)%*%solve(S.x.bar)%*%(x.bar.vec-mu0.vec)
pval = 1-pchisq(T2,2)


eig = eigen(S.x.bar)
lam1 = eig$'values'[1]
lam2 = eig$'values'[2]
v1 = eig$vectors[,1]
v2 = eig$vectors[,2]
v1 = abs(v1)

#dist = (p*(n-1)/(n-p))*qf(.95,p,n-p)
dist = qchisq(.95,p)

a1 = v1*sqrt(lam1)*sqrt(dist)
a2 = v2*sqrt(lam2)*sqrt(dist)


lines(c(x.bar,x.bar+a1[1]),c(y.bar,y.bar+a1[2]),lwd=1.5,col=2)
lines(c(x.bar,x.bar+a2[1]),c(y.bar,y.bar+a2[2]),lwd=1.5,col=2)

#Hmm...maybe we need to zoom in on our plot
plot(x,y,xlim=c(1,3),ylim=c(0,2))
points(x.bar,y.bar,pch=19,cex=1.8,col=2)

lines(c(x.bar,x.bar+a1[1]),c(y.bar,y.bar+a1[2]),lwd=3,col=2)
lines(c(x.bar,x.bar+a2[1]),c(y.bar,y.bar+a2[2]),lwd=3,col=2)

#####Add the bootstrap points!

points(final.BS.xbars[,1],final.BS.xbars[,2],cex=.3,col=4)

#####Add our theoretical conf ellipse

library(plotrix)

d1 = sqrt(sum(a1*a1))
d2 = sqrt(sum(a2*a2))
theta = acos(v1[1])
draw.ellipse(x=x.bar.vec[1],y=x.bar.vec[2],a=d1,b=d2,angle=theta,deg=F)

