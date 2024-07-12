#installing mvtnorm package

#install.packages('mvtnorm')
library(mvtnorm)


#Part 1, choose my mean vector mu and covariance matrix Sig
#note, I'm going to define B, then use (B)(Bt) to define Sig.

mu = matrix(c(2,4),nrow=2)
B = matrix(c(1,2,3,4),nrow=2)
Sig = B%*%t(B)

#Part 2, going to simulate 10,000 observations from norm(mu,Sig).
#Going to do it two separate ways, once with mvtnorm package, once 
#using the definition.

x.vec1 = rmvnorm(10000,mu,Sig)

#or......

z = matrix(rnorm(20000,0,1),ncol=2)
x.vec2 = z%*%B +matrix(rep(mu,10000),byrow=T,ncol=2)

par(mfrow=c(2,1))
plot(x.vec1[,1],x.vec1[,2])
plot(x.vec2[,1],x.vec2[,2])

#Part 3, compute quadratic forms for all vectors in x.vec1
#not going to keep messing with x.vec2

QF = rep(0,10000)
for(i in 1:10000){
	QF[i] = t(x.vec1[i,] - mu)%*%solve(Sig)%*%(x.vec1[i,] - mu)
}

#Part 4, Find 95%tile for simulated quadratic forms, compare to chi.sq with 2 degrees of freedom

sort(QF)[9500]
qchisq(.95,2)

#Bonus plot for funsies.
dev.off()
hist(QF,freq=F)
x.seq = 0:200/10
y.seq = dchisq(x.seq,2)
lines(x.seq,y.seq,col=2,lwd=2)

