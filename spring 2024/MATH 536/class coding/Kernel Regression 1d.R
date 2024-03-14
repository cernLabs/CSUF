#Lets look at Kernel Regression.  Let us assume for the time
#being that we will have one predictor x and one response y.
#Multivariate kernel smoothing is a different monster all together
#So we will limit ourself to the univariate analysis

#For a pre-determined value of h, we are going
#To estimate y = f^(x,h) using kernel smoothing.
#Recall that f need not have functional form

x = seq(10,100,by=10) + runif(10,0,5)
y = -.05*(x-52.5)^2 + 3*x - 100 + rnorm(10,0,20)
plot(x,y,xlim=c(min(x)-10,max(x)+10),ylim=c(min(y)-30,max(y)+30))
lines(c(min(x)-15,max(x)+15),c(0,0),lty=2)

#As mentioned in class the choice of kernel
#is far less important than the bandwidth selection
#Before we get into bandwidth selection, let us
#Assume a Gaussian (normal) kernel with h = 10
#The following code is used to illustrate how Kernel
#Regression works

#see cv section below
h=6.95

#Coding note.  x1 are user defined values we want to input to the model
#For now, x1 goes from smallest to largest x in small increments for plotting
#y1 is what you get out of the model when you plug in x1

x1 = seq(min(x),max(x),by=(max(x)-min(x))/99)
y1 = rep(0,100)
for(i in 1:100){
	y1[i] = sum(exp(-.5*((x1[i]-x)/h)^2)*y)/sum(exp(-.5*((x1[i]-x)/h)^2))
}

lines(x1,y1,col=2)

#What if I wanted my original residuals?  now, x1 = x.  

x1 = x
y1 = rep(0,length(x1))
for(i in 1:length(x1)){
	y1[i] = sum(exp(-.5*((x1[i]-x)/h)^2)*y)/sum(exp(-.5*((x1[i]-x)/h)^2))
}
resid = y - y1

#What if I want to predict y when x = 65

x1 = 65
y1 = rep(0,length(x1))
for(i in 1:length(x1)){
	y1[i] = sum(exp(-.5*((x1[i]-x)/h)^2)*y)/sum(exp(-.5*((x1[i]-x)/h)^2))
}
y1

#How do I pick h?

#Cross validation, leave one out.

data = data.frame(x,y)
n = length(x)

CV = function(h){
	SSR = 0
	for(i in 1:n){
		y1 = sum(exp(-.5*((x[i]-x[-i])/h)^2)*y[-i])/sum(exp(-.5*((x[i]-x[-i])/h)^2))
		resid = y[i] - y1
		SSR = SSR + resid^2
	}
	SSR
}

h=optim(1,CV)$par








