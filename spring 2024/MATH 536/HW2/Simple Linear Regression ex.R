#airquality is a built in data set for R
#airquality has some issues with missing data
#Since we don't want to mess with missing data
#We can just omit incomplete records


atmosphere = na.omit(airquality)
attach(atmosphere)

#Check out the variables for atmosphere
#Now we want to create a simple linear regression model
#for atmosphere where we predict Ozone using Temp.
#I'm going to manually do this problem once, then
#highlight the built in functions for R that
#generate and analyze linear models

xbar = mean(Temp)
sx = sd(Temp)
ybar = mean(Ozone)
sy = sd(Ozone)
r = cor(Temp,Ozone)
n = length(Ozone)

b = r*sy/sx
a = ybar - b*xbar

yhat = Temp*b + a
errors = Ozone - yhat
s.errors = sqrt(sum(errors^2)/(n-2))
SXX = var(Temp)*(n-1)

se.b = s.errors/sqrt(SXX)
se.a = s.errors*sqrt(1/n + xbar^2/SXX)

b
a
r^2
se.b
se.a
s.errors

CI.yhat = function(x.star,alpha){
	left = b*x.star + a + (qt(alpha,n-2)*s.errors*sqrt(1/n + (xbar-x.star)^2/SXX))
	right = b*x.star + a - (qt(alpha,n-2)*s.errors*sqrt(1/n + (xbar-x.star)^2/SXX)) 	
	output = c(left,right)
	output
}

PI.y = function(x.star,alpha){
	left = b*x.star + a + (qt(alpha,n-2)*s.errors*sqrt(1 + 1/n + (xbar-x.star)^2/SXX))
	right = b*x.star + a - (qt(alpha,n-2)*s.errors*sqrt(1 + 1/n + (xbar-x.star)^2/SXX)) 	
	output = c(left,right)
	output	
}

CI.yhat(75,.025)
PI.y(75,.025)

#Wow that was a lot of work, it certainly would
#be painful if we had to do all this work everytime.
#Before we move onto how R packages simple linear regression
#lets take a moment to highlight our results.
#Our linear model was yhat = 2.439*Temp - 147.6461
#The standard error of the slope was .2393, the
#standard error of the intercept was 18.7553.
#The correlation coefficient squared (r^2 ) was .4879
#The standard deviation of the errors was 23.92.
#Now lets see if we can't get the same level of analysis
#using the lm function.

model1 = lm(Ozone~Temp)

model1
summary(model1)
newdata = data.frame(Temp=75)
predict(model1,newdata,interval="predict")
predict(model1,newdata,interval="confidence")

#Recall that we had four basic assumptions, we can
#easily check those using a lm object and some plots

plot(model1)

#Later on we're going to talk about ANOVA models, we'll come back!

anova(model1)

