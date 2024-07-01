

data(AirPassengers) # Calling the built-in R data
head(AirPassengers)

plot(AirPassengers) # plotting the time-series data. Notice that the data is already in the form of a time series. To check this type AirPassengers and hit enter.

plot(log(AirPassengers))  # Plotting the log-transform of data




tim <- time(AirPassengers) #creates vector of time that TS was collected
cycle(AirPassengers) #gives the positions in the cycle of each observation
month <- as.factor(cycle(AirPassengers))
reg1 <- lm(log(AirPassengers)~tim+month)

plot(log(AirPassengers))
points(tim,predict.lm(reg1),type='l',col='red')


plot(AirPassengers)
points(tim,exp(predict.lm(reg1)),type='l',col='red')


reg1 <- lm(log(AirPassengers)~tim+month)
summary(reg1)

#par(mfrow=c(1,1)) # Dividing the plotting page into 4 panels
plot(reg1$fitted, reg1$residuals) # plot of fitted values vs residuals 
qqnorm(reg1$residuals) #qq-plot of residuals
qqline(reg1$residuals) # plotting the line, along which the dots in qq-plot should lie 
plot(reg1$residuals) # plotting the residuals vs time
abline(h=0,lty=2) # plotting a horizontal line at 0
acf(reg1$residuals) #sample acf plot of residuals


tim2 <- tim^2
reg2 <- lm(log(AirPassengers)~tim+tim2+month)
summary(reg2)

pdf('AirTimeMonthLOG2.pdf')
plot(log(AirPassengers))
points(tim,predict.lm(reg2),type='l',col='red')
dev.off()

pdf('AirDiagnose2.pdf')
par(mfrow=c(2,2)) # Dividing the plotting page into 4 panels
plot(reg2$fitted, reg2$residuals) # plot of fitted values vs residuals 
qqnorm(reg2$residuals) #qq-plot of residuals
qqline(reg2$residuals) # plotting the line, along which the dots in qq-plot should lie 
plot(reg2$residuals) # plotting the residuals vs time
abline(h=0,lty=2) # plotting a horizontal line at 0
acf(reg2$residuals) #sample acf plot of residuals
dev.off()



#Prediction in AirPassengers data

data(AirPssengers) # Calling the data

tim <- time(AirPassengers) #introducing time (t) 
cycle(AirPassengers) # Getting values for season
month <- as.factor(cycle(AirPassengers)) # Intoroducing season for regression (11 binary variables)
reg1 <- lm(log(AirPassengers)~tim+month) # Fitting the regression model
tim.new <- seq(1961,1963,length=25)[1:24] # Intoducing new time for forecatsting 2 years 1961 and 1962 (notice that it is 1:24 because 1963 should not be included)
month.new <- factor(rep(1:12,2)) # Introducing the season value for forecasting

new <- data.frame(tim=tim.new,month=month.new) # Putting the values for forecasting into a dataframe
a <- predict.lm(reg1,new,interval='prediction') # Computing the prediction as well as prediction interval
a

plot(AirPassengers,xlim=c(1949,1963),ylim=c(0,900)) #plotting the data

abline(v=1961,col='blue',lty=2) # adding a vertical line at the point where prediction starts
lines(exp(a[,1])~tim.new,type='l',col='red')# plotting the predict
lines(exp(a[,2])~tim.new,col='green') # plotting lower limit of the prediction interval
lines(exp(a[,3])~tim.new,col='green') # plotting upper limit of the  prediction interval





