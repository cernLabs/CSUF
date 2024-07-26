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
#lines(x.bar+new.x,y.bar+new.y,col=2,lwd=.5,lty=2)






###  Confidence Intervals "Boxes" 

#Marginal Confidence Intervals for mu_j:
#Get ingredients
tstar <- qt(p = 1-0.05/2, df = n-1)
S.x <- S.x.bar[1,1]
S.y <- S.x.bar[2,2]

#Plot center of data
plot(x,y,xlim=c(1,3),ylim=c(0,2), main = "Marginal Confidence Intervals for mu_x and mu_y")
draw.ellipse(x=x.bar.vec[1],y=x.bar.vec[2],a=d1,b=d2,angle=theta,deg=F)

margin_left <- x.bar - tstar * sqrt(S.x)
margin_right <- x.bar + tstar * sqrt(S.x)
margin_bottom <- y.bar - tstar * sqrt(S.y)
margin_top <- y.bar + tstar * sqrt(S.y)

abline(v = margin_left, col='purple2', lwd=2)
abline(v = margin_right, col='purple2', lwd=2)
abline(h = margin_bottom, col='purple2', lwd=2)
abline(h = margin_top, col='purple2', lwd=2)



#Simultaneous Confidence Intervals for mu_j
Fscore <- qf(p=1-0.05, 2, 200-2)
big_square_root <- sqrt((200-1)*2/(200-2) * Fscore)

#Add simultaneous conf intervals to plot

simul_left <- x.bar - (big_square_root * sqrt(S.x))
simul_right <- x.bar + (big_square_root * sqrt(S.x))
simul_bottom <- y.bar - (big_square_root * sqrt(S.y))
simul_top <- y.bar + (big_square_root * sqrt(S.y))

abline(v = simul_left, col='red', lwd=2)
abline(v = simul_right, col='red', lwd=2)
abline(h = simul_bottom, col='red', lwd=2)
abline(h = simul_top, col='red', lwd=2)


#Joint Confidence Intervals for mu_j:
#Note I'm using the conservatinve 1 - (alpha/p)/2 convention here.

#Get ingredients
tstar <- qt(p = 1-(0.05/2)/2, df = n-1)
S.x <- S.x.bar[1,1]
S.y <- S.x.bar[2,2]

#Add joint confidence intervals to the plot

joint_left <- x.bar - tstar * sqrt(S.x)
joint_right <- x.bar + tstar * sqrt(S.x)
joint_bottom <- y.bar - tstar * sqrt(S.y)
joint_top <- y.bar + tstar * sqrt(S.y)

abline(v = joint_left, col='purple2', lwd=2)
abline(v = joint_right, col='purple2', lwd=2)
abline(h = joint_bottom, col='purple2', lwd=2)
abline(h = joint_top, col='purple2', lwd=2)


#Joint Confidence Intervals for mu_j part 2:
#Note I'm using the conservatinve 1-(1-sqrt(1-alpha))/2 convention here.

#Get ingredients
tstar <- qt(p = 1-(1-sqrt(.95))/2, df = n-1)
S.x <- S.x.bar[1,1]
S.y <- S.x.bar[2,2]

#Add joint confidence intervals to the plot

joint_left <- x.bar - tstar * sqrt(S.x)
joint_right <- x.bar + tstar * sqrt(S.x)
joint_bottom <- y.bar - tstar * sqrt(S.y)
joint_top <- y.bar + tstar * sqrt(S.y)

abline(v = joint_left, col='purple2', lwd=2)
abline(v = joint_right, col='purple2', lwd=2)
abline(h = joint_bottom, col='purple2', lwd=2)
abline(h = joint_top, col='purple2', lwd=2)


