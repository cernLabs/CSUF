###Step 1, create a population of vectors (I'm going to make my vector length 3)
###just for funsies.  Maybe 50,000 observations in my population.  

x1 = runif(50000,0,10)
x2 = .5*x1 + runif(50000,-3,3)
x3 = .25*x1 + .25*x2 + runif(50000,-3,3)

data = matrix(c(x1,x2,x3),ncol=3)
dim(data)


###Step 2, take 10,000 samples, all of size 200, each sample, compute
###the sample mean vector and store it in a matrix called mean.vec

mean.vec = matrix(0,10000,3)
for(i in 1:10000){
	index = sample(1:50000,200,replace=F)
	samp.data = data[index,]
	sample.mean = apply(samp.data,2,mean)
	mean.vec[i,] = sample.mean
}

###Step 3 feels tricky.  hard to show that I'm trivariate normal.
###Going to steal from what we did yesterday.
###Step 3a.  Compute quadratic form for each mean vector (note, should be using Sigma/n not Sigma)
###Step 3b.  Going to plot a histogram of mean vectors and overlay a chi-sq-3 
###distribution on top of it.

Sigma = cov(data)
Sigma.x.bar = Sigma/200
mu = apply(data,2,mean)

QF = rep(0,10000)
for(i in 1:10000){
	QF[i] = (mean.vec[i,] - mu)%*%solve(Sigma.x.bar)%*%(mean.vec[i,] - mu)
}

hist(QF,breaks=20,freq=F)
x.seq = (0:200)/10
y.seq = dchisq(x.seq,3)
lines(x.seq,y.seq,col=2,lwd=2)
