


# sample ACF for iid noise [N(0,1)]
pdf('SampleACF1.pdf')
X <- rnorm(100) # generating (independently) 100 realizations of N(0,1)
par(mfrow=c(2,1)) #dividing the page into 2 rows and one column
plot(X,type='l',main='iid noise') #plotting the data
acf(X,main='sample ACF for iid noise') # plotting the acf
dev.off()

# sample ACF for MA(1) process
pdf('SampleACF-MA1.pdf')
X <- arima.sim(list(order = c(0,0,1), ma = 0.85), n = 200) # simulating data from an MA(1) process
par(mfrow=c(2,1)) #dividing the page into 2 rows and one column
plot(X,type='l',main='simulated data from MA(1)') #plotting the data
acf(X,main='sample ACF for MA(1)') # plotting the acf
dev.off()

# sample ACF for MA(1) process
#pdf('SampleACF-AR1-NON.pdf')
X <- arima.sim(list(order = c(1,0,0), ar = .7), n = 200) # simulating data from an AR(1) process
par(mfrow=c(2,1)) #dividing the page into 2 rows and one column
plot(X,type='l',main='simulated data from AR(1)') #plotting the data
acf(X,main='sample ACF for AR(1)') # plotting the acf
#dev.off()


# sample ACF for data with trend
#pdf('SampleACF1-1.pdf')
a <- seq(1,100,length=200)
X <- 22-15*a+0.3*a^2+rnorm(200,500,50)
par(mfrow=c(2,1)) #dividing the page into 2 rows and one column
ts.plot(X)
acf(X)
#dev.off()

# sample ACF for data with trend and seasonal component
#pdf('SampleACF1-2.pdf')
a <- seq(1,10,length=200)
X <- 22-15*a+a^2+5*sin(20*a)+rnorm(200,20,2)
par(mfrow=c(2,1)) #dividing the page into 2 rows and one column
ts.plot(X)
acf(X)
#dev.off()

# sample ACF for US Accidental Deaths data
#pdf('SampleACF2.pdf')
data(USAccDeaths) #calling the data
par(mfrow=c(2,1)) #dividing the page into 2 rows and one column
plot(USAccDeaths,type='l',main='# of Accidental deaths in US') #plotting the data
acf(USAccDeaths,main='sample ACF for US accidental deaths data',lag.max=24) # plotting the acf
#dev.off()


# a simulation example with substantial deterministic periodicity
#pdf('SampleACF3.pdf')
X <- sin(1:200) #generating purely periodic data
par(mfrow=c(2,1))
plot(X,type='l')
acf(X)
#dev.off()


# Introducing noise to the previous case
#pdf('SampleACF4-4.pdf')
X <- sin(1:200)+rnorm(200,0,2) #0.5 is the standard deviation of the noise term. Change and see the effect on sample ACF
par(mfrow=c(2,1))
plot(X,type='l')
title(main=expression(sigma==2))
acf(X)
#dev.off()


# a simulation example with substantial deterministic periodicity
#pdf('SampleACF5.pdf')
X <- 1:500+100*sin(1:500) #generating data with trend and periodic component
par(mfrow=c(2,1))
plot(X,type='l')
acf(X)
#dev.off()

