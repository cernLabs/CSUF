###First lets look at Kernel Density Estimation techniques.  

###suppose x1....x100 are exponential random variables with mean = 5 (lambda = .2).
###We want to estimate this density using a guassian kernel and the 100 observations.

x = rexp(100,.2)
x1 = seq(0,max(x)+2,by=.2)
y1 = .2*exp(-.2*x1)
plot(x,rep(c(0,.3),50),pch="",xlab="x",ylab="f(x)")
hist(x,freq=F,breaks=12)
lines(x1,y1,col=2)
points(x,rep(0,100),pch="x",cex=.5)

kx = sort(x)[80]
#I know it seems like Im pulling a crazy formula for h here, we'll talk about it soon.  The forumla is Silvermans rule of thumb.
#This formula assumes the density we're estimating is normal (which is kind of dumb, if we knew it was normal we could just parametrically model a normal density).
#Anyway, people use Silvermans rule of thumb for densities that aren't normal.
#Silvermans rule of thumb picks the h that theoretically minimizes AMISE (Asymptotic Mean Integrated Squared Error) for KDE of a normal density.

h = 1.06*sd(x)*length(x)^(-.2)
h.sil = h
x1 = seq(kx-2*h,kx+2*h,by=h/50)

#If you're wondering where the 5 comes from in the next equation, I added that just so
#The guassian curve is more visible, you'll notice later when I compute
#The actual density estimate it's been removed.

y1 = 5*(1/(length(x)*h*sqrt(2*pi))*exp(-.5*((kx-x1)/h)^2))
lines(x1,y1)

for(i in 1:10){
	kx = x[i]
	x1 = seq(kx-2*h,kx+2*h,by=h/50)
	y1 = 5*(1/(length(x)*h*sqrt(2*pi))*exp(-.5*((kx-x1)/h)^2))
	lines(x1,y1)
}

for(i in 11:100){
	kx = x[i]
	x1 = seq(kx-2*h,kx+2*h,by=h/50)
	y1 = 5*(1/(length(x)*h*sqrt(2*pi))*exp(-.5*((kx-x1)/h)^2))
	lines(x1,y1)
}


# Next we actually compute the density estimates for 1000
# equally spaced points and plot them with a line
###NOTE:  x1 here represents the x's at which we want to make a prediction.
#x1 could be a single point, could be a vector of new points we want to forecast or could be our original x's if we want to compute our fitted values and residuals (which could be helpful in a bootstrap setting)

x1 = seq(0,max(x)+3,by=(max(x)+3)/999)
y1 = rep(0,1000)
for(i in 1:1000){
	y1[i] = sum((1/(length(x)*h*sqrt(2*pi))*exp(-.5*((x1[i]-x)/h)^2)))
}
lines(x1,y1,col=3)


##Hmm...not quite ideal on the edge points yet, what if we came up with a weight
##for each estimated point based on how much of it's gaussian normal is
###actually in our spatial support.

z1 = rep(0,1000)
w1 = rep(0,1000)
for(i in 1:1000){
	z1[i] = (0-x1[i])/h
	w1[i] = 1/(1-pnorm(z1[i])) 
}

lines(x1,y1*w1,col=4)

#Notice that our bandwidth selection was done using silvermans rule of thumb.
#Even though our underlying distribution was exponential, people still 
#use Silverman's rule of thumb anyway.  What if we wanted to select h
#using cross validation.
#For cross validation we need to choose some measure of goodness of fit
#Let's stick with AMISE.  In class we saw that minimizing AMISE can be condensed to minimizing a riemann approximation of the intergral of f.hat^2(x) over it's domain plus the CV expectation of f^(x).  We'll do leave one out cross validation for the second half.

#In the following code for P1, x1 represents the midpoints of our riemann sum.  y1 represents f^(x1).  output is the riemann sum.

P1 = function(h){
	x1 = seq(min(x),max(x)+3,by=(max(x)+3-min(x))/999)
	y1 = rep(0,1000)
	z1 = rep(0,1000)
	w1 = rep(0,1000)
	for(i in 1:1000){
		z1[i] = (0-x1[i])/h
		w1[i] = 1/(1-pnorm(z1[i]))
		y1[i] = w1[i]*sum((1/(length(x)*h*sqrt(2*pi))*exp(-.5*((x1[i]-x)/h)^2)))
	}	
	output = sum((max(x)+3 - min(x))/999*y1^2)
	output
}

#In the following code you'll notice I kernel density estimate each x in our original data using all the other data points x[-i].  Then I simply compute 1/2*E(f.hat(x))

P2 = function(h){
	leave.out.estimate = rep(0,length(x))
	z2 = rep(0,length(x))
	w2 = rep(0,length(x))
	for(i in 1:length(x)){
			z2[i] = (0-x[i])/h
			w2[i] = 1/(1-pnorm(z2[i]))
			leave.out.estimate[i] = w2[i]*sum(1/(h*(length(x)-1)*sqrt(2*pi))*exp(-.5*((x[i]-x[-i])/h)^2))
	}
output = -2*sum(leave.out.estimate)/length(x)
output
}

CV = function(h){P1(h) + P2(h)}

###Can use optim to find the minimizing argument.

optim(4,CV)

h = optim(4,CV)$par


#Let's compare to h from before with Silvermans.

h
h.sil

#In general there are several other measures of goodness of fit.  Log likelihood for example.  We could select h to maximize goodness of fit for a variety of choices.

#Now lets look at Kernel Regression.
#See next code file.

x = seq(10,100,by=10) + runif(10,0,5)
y = -.05*(x-52.5)^2 + 3*x - 100 + rnorm(10,0,20)
plot(x,y,xlim=c(min(x)-10,max(x)+10),ylim=c(min(y)-30,max(y)+30))
lines(c(min(x)-15,max(x)+15),c(0,0),lty=2)

#Assume our kernel is guassian with h = 10
h=10
for(i in 1:1){
	x1 = seq(x[i]-40,x[i]+40,by=1)
	y1 = (1/(sqrt(2*pi)))*exp(-.5*((x[i]-x1)/h)^2)
	lines(x1,y1*y[i]) 
}

for(i in 2:10){
	x1 = seq(x[i]-40,x[i]+40,by=1)
	y1 = (1/(sqrt(2*pi)))*exp(-.5*((x[i]-x1)/h)^2)
	lines(x1,y1*y[i]) 
}

x1 = seq(min(x),max(x),by=(max(x)-min(x))/99)
y1 = rep(0,100)
for(i in 1:100){
	y1[i] = sum((1/(sqrt(2*pi)))*exp(-.5*((x1[i]-x)/h)^2)*y)/sum((1/(sqrt(2*pi)))*exp(-.5*((x1[i]-x)/h)^2))
}

lines(x1,y1,col=2)
###Or...could just use build in function ksmooth...
###Important to note...kmooth employs boundary correction
###Which is something we will talk about but the ouput will be slightly different
###To begin with.

k.smooth1 = ksmooth(x,y,kernel="normal",bandwidth=10)
lines(k.smooth1$x,k.smooth1$y,col=3)


#What if h = 2

plot(x,y,xlim=c(min(x)-10,max(x)+10),ylim=c(min(y)-30,max(y)+30))
lines(c(min(x)-15,max(x)+15),c(0,0),lty=2)


h=2
for(i in 1:10){
	x1 = seq(x[i]-40,x[i]+40,by=1)
	y1 = (1/(sqrt(2*pi)))*exp(-.5*((x[i]-x1)/h)^2)
	lines(x1,y1*y[i]) 
}


x1 = seq(min(x),max(x),by=(max(x)-min(x))/99)
y1 = rep(0,100)
for(i in 1:100){
	y1[i] = sum((1/(sqrt(2*pi)))*exp(-.5*((x1[i]-x)/h)^2)*y)/sum((1/(sqrt(2*pi)))*exp(-.5*((x1[i]-x)/h)^2))
}

lines(x1,y1,col=2)

k.smooth2 = ksmooth(x,y,kernel="normal",bandwidth=2)
lines(k.smooth2$x,k.smooth2$y,col=3)


#What if h = 50

plot(x,y,xlim=c(min(x)-10,max(x)+10),ylim=c(min(y)-30,max(y)+30))
lines(c(min(x)-15,max(x)+15),c(0,0),lty=2)


h=50
for(i in 1:10){
	x1 = seq(x[i]-40,x[i]+40,by=1)
	y1 = (1/(sqrt(2*pi)))*exp(-.5*((x[i]-x1)/h)^2)
	lines(x1,y1*y[i]) 
}

x1 = seq(min(x),max(x),by=(max(x)-min(x))/99)
y1 = rep(0,100)
for(i in 1:100){
	y1[i] = sum((1/(sqrt(2*pi)))*exp(-.5*((x1[i]-x)/h)^2)*y)/sum((1/(sqrt(2*pi)))*exp(-.5*((x1[i]-x)/h)^2))
}

lines(x1,y1,col=2)

k.smooth3 = ksmooth(x,y,kernel="normal",bandwidth=50)
lines(k.smooth3$x,k.smooth3$y,col=3)


