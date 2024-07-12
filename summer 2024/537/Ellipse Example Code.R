###Confidence Region and Hyp Test for class Example

data = read.csv("example.csv",h=T)


attach(data)
plot(x,y,xlim=c(min(x),max(x)),ylim=c(min(y),max(y)))

n = length(x)
p=2

x.bar = mean(x)
y.bar = mean(y)

#set up vectors and matrices for x.bar.vec, mu0.vec and sigma.xbar.mat

x.bar.vec = matrix(c(x.bar,y.bar),ncol=1)
mu0.vec = matrix(c(1.5,1.5),ncol=1)
S.x.bar = matrix(var(data),ncol=2)/n
T2 = t(x.bar.vec-mu0.vec)%*%solve(S.x.bar)%*%(x.bar.vec-mu0.vec)
pval = 1-pchisq(T2,2)


#Plotting Challenge

points(x.bar,y.bar,pch=19,cex=1.8,col=2)

eig = eigen(S.x.bar)

lam1 = eig$'values'[1]
lam2 = eig$'values'[2]

v1 = eig$vectors[,1]
v2 = eig$vectors[,2]
#I don't like that R put my first eigen vector in the third quadrant, so I'm moving it
#to the first quadrant with the abs().
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


####Really fun little package if you want for drawing ellipses

library(plotrix)

#####Find theta to rotate ellipse.

d1 = sqrt(sum(a1*a1))
d2 = sqrt(sum(a2*a2))
theta = acos(v1[1])



draw.ellipse(x=x.bar.vec[1],y=x.bar.vec[2],a=d1,b=d2,angle=theta,deg=F)
lines(x.bar+new.x,y.bar+new.y,col=2,lwd=.5,lty=2)
