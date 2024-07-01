
##################################
# shampoo sales
##################################

shampoo.data <- read.csv(file="shampoo.csv", header=F) # reading the data file into R
shampoo <-  ts(shampoo.data$V1, start = 1,  frequency = 1) #put the data into a time series format
plot(shampoo.data$V1, main="Shampoo sales", ylab="Sales", xlab="Month")


##################################
# finite moving average filter
##################################

# this function performs moving average (MA) filtering
 ma <- function(data, q){ #data: dataset in time series format and q: the order of the MA filter  
 	n <- length(data)
 	out <- NULL
 	out$ma <- rep(0, length=n-2*q)
 	out$time <- time(data)[(q+1):(n-q)]
 	for(i in 1:length(out$ma)){
 		lis<- i + (-q:q)
  		out$ma[i] <- sum(data[q+lis])/(2*q+1)	
 	}
	out$ma <- c(rep(NA,q),out$ma,rep(NA,q))
	out$time <- c(rep(NA,q),out$time,rep(NA,q))
 	out
 }
##################################
plot(shampoo.data$V1, main="Shampoo sales", ylab="Sales", xlab="Month")
junk <- ma(shampoo, 2) # change q to see its efffect
lines(junk$time, junk$ma, col=1)

##################################
# Exponential smoothing
##################################
 
# this function performs exponential smoothing (ES) 
 es <- function(data, alpha){ #data: dataset in time series format and alpha: the parameter of the smoother
	n <- length(data)
 	out <- NULL
 	out$es <- rep(0, length=n)
 	out$time <- time(data)[1:n]
 	out$es[1] <- data[1]
 	for(i in 2:n){
 		out$es[i] <- (1-alpha)*out$es[i-1] + alpha*data[i]
 	}	
 	out
 }
################################## 

shampoo.data <- read.csv(file="shampoo.csv", header=F)

plot(shampoo.data$V1, main="Shampoo sales", ylab="Sales", xlab="Month")
junk <- es(shampoo, 0.3) #change the parameter alpha to see its effect
lines(junk$time, junk$es, col=1)

############################
# polynomial regrssion
############################
# Here we fit linear and quadratic models to shampoo data

tim <- 1:36 #making the explanatory variable for time.
shampoo.data <- read.csv(file="shampoo.csv", header=F)
model <- lm(shampoo.data$V1~tim) #linear model
plot(shampoo.data$V1, main="Shampoo sales", ylab="Sales", xlab="Month")
abline(model,col="blue")

tim2 <- tim^2
model <- lm(shampoo~tim+tim2) #quadratic model
summary(model)
lines( tim,202.8789-5.8801*tim+0.4854*tim2, col="red")


################
# differencing
################

#the function diff(x,k) performs differencing operator k times in the the dataset x

plot(diff(shampoo.data$V1), main="First difference of shampoo data", xlab="time", ylab="First difference") #first difference: X(t)-X(t-1)

acf(diff(shampoo.data$V1),main="acf of 1 time differenced data")

plot(diff(shampoo.data$V1,2), main="Second difference of shampoo data", xlab="time", ylab="Second difference") #second difference: X(t)-2X(t-1)+X(t-2)

acf(diff(shampoo.data$V1,2),main="acf of 2 times differenced data")


#######################
# Denmark birth data
#######################

junk <- read.table(file="denmark.txt", header=T) #reading Danish birth data into R
birth <- ts(data = junk$birth, start = 1900,  frequency = 12) #making the data into timeseries format
plot(birth, main="Monthly Births in Denmark")
 
plot(decompose(birth,type='multiplicative')) #estimating s(s), m(t) and the residual in X(t)=m(t)*s(t)*noise(t) 
plot(decompose(birth,type='additive')) #estimating s(s), m(t) and the residual in X(t)=m(t)+s(t)+noise(t) 

plot(birth, main="Monthly Births in Denmark")
lines(decompose(birth)$trend,col='red') #estimating the trend 
  
plot(decompose(birth)$season[1:12]) #seasonal effect (this is only for on period, i.e. 12 month) 
plot(decompose(birth)$season) #seasonal effect (all 93 years together)

plot(decompose(birth,type='additive')$random) #plotting residuals, i.e. x(t)-m.hat(t)-s.hat(t)

resid <- as.vector(na.omit(decompose(birth)$random)) 
acf(resid,main="ACF random component") # acf plot for the residuals (notice the seasonality that still remained in the residuals)

kpss.test(resid, null = "Trend") #library(tseries)
?kpss.test

#performing difference sign test
n <- length(resid)
S <- sum(diff(resid)>0) # counting the number of r(i)>r(i-1)
mu <- (n-1)/2
sigma2 <- (n+1)/2
Z <- abs((S-mu)/sqrt(sigma2))
1-pnorm(Z) #p-value
# Notice that for birth data we fail to reject the null hypothesis H0:data is random, because of the strong
# seasonal component. See the slides.
