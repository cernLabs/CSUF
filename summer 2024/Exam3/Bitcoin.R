library('ggplot2')
library('forecast')
library('tseries')
library('rio')
library('dplyr')
library('tidyverse')
library('tidyquant') #geom_ma
library('seismicRoll')
library(lubridate)



setwd("~/Dropbox (CSU Fullerton)/My Math 531T Time Series/Summer2024/Bitcoin")

Bitcoin.data <- read.csv(file="BTCfull.csv", header=T) # reading the data file into R
View(Bitcoin.data)

#
# Read in data, format, and clean
#
#
Bitcoin <- data.frame(Date = Bitcoin.data$Date, Close = (Bitcoin.data$Close))
head(Bitcoin)
class(Bitcoin$Date)
class(Bitcoin$Close)
# we need Date to be a date class and Close to be numeric
Bitcoin$Date <- as.Date(Bitcoin$Date, format = "%m/%d/%Y" )
Bitcoin$Close <- as.numeric(Bitcoin$Close) #NAs produced
which(is.na(Bitcoin$Close) == TRUE) #no days missing 
  
## Create year column to divide into train/test sets
Bitcoin$year <- year(Bitcoin$Date) #lubridate library
## Use 2020, 2021, and 2022 as training
## 2023 as test set
Bitcoin_test <- Bitcoin[which(Bitcoin$year == "2023"),]
Bitcoin_train <- Bitcoin[which(Bitcoin$year == "2020" | Bitcoin$year == "2021" | Bitcoin$year == "2022"),]
#
# Plot the data look for trends/seasonalities
#
#ggplot2
theme_set(theme_gray())

p <- ggplot(data = Bitcoin_train, aes(Date, Close)) + 
  geom_line(color = "#00AFBB") +
  scale_x_date(date_labels = "%Y" ) +
  xlab("Year") +
  ylab("Closing Cost")
p 

# smoother
p+ stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",method = "gam")

###acf plot of original data, wlthough, there is obviously a trend
acf(Bitcoin_train$Close, lag.max = 100)


p <- ggplot(data = Bitcoin_test, aes(Date, Close)) + 
  geom_line(color = "#00AFBB") +
  scale_x_date(date_labels = "%Y" ) +
  xlab("Year") +
  ylab("Closing Cost")

#+ geom_point(aes(x = date, y = close))
p 

# smoother
p+ stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",method = "gam")


#### Difference the data 
x1 = diff(rev(Bitcoin_train$Close), lag = 1, differences = 1) #reverse order so left to right is in order of date timeline
acf(x1, lag.max = 60) 
pacf(x1, lag.max = 60) 
plot(x1, type="l") # try the log

x1 = diff(rev(log(Bitcoin_train$Close)), lag = 1, differences = 1)
acf(x1, lag.max = 60) 
pacf(x1, lag.max = 60) 
plot(x1, type="l") # try the log


Bitcoin_train[which(x1 ==max(x1)),] #large decline in price


#Checking out the classical Decomposition Data
count = ts((Bitcoin_train$Close), frequency = 365.25)
decomp1 = decompose(log(count), type = "additive")
plot(decomp1)
autoplot(decomp1)
acf(as.vector(na.omit(decomp1$random)), lag = 100)
pacf(as.vector(na.omit(decomp1$random)), lag = 100)



{
#One recommendation to aid with the outlier is to smooth the data by month (approximately)
Bitcoin2 <- Bitcoin2 %>% mutate(mean30 = seismicRoll::roll_mean(Bitcoin2$Close, n = 30, align = "center"))

p <- ggplot(data = Bitcoin2, aes(Date, Close)) + 
  geom_line(color = "#00AFBB") +
  scale_x_date(date_labels = "%Y" ) +
  xlab("Year") +
  ylab("Closing Cost") +
  geom_line(aes(x = Date, y=mean30),color="red1")   
p

#### Difference the data 
x2 = na.omit(diff(log(Bitcoin2$mean30), lag = 1, differences = 1))
acf(x2, lag.max = 60) #woah still non -stationary
pacf(x2, lag.max = 60) #spike at 32 is VERY odd, but let's get this stationary first
plot(x2, type="l")
}

#### Difference the data again 
x2 = na.omit(diff(rev(log(Bitcoin_train$Close)), lag = 1, differences = 2))
acf(x2, lag.max = 60) #the differences are correlated with the 29 lag 
pacf(x2, lag.max = 60) #correlated at 29 and 58 - we shall return to this tomorrow 
plot(x2, type="l")




#Tests for Stationarity
adf.test(as.vector(na.omit(decomp1$random)))
kpss.test(as.vector(na.omit(decomp1$random)))

adf.test(as.vector(na.omit(x1)))
kpss.test(as.vector(na.omit(x1)))

adf.test(as.vector(na.omit(x2)))
kpss.test(as.vector(na.omit(x2)))



#Model
fit0<-auto.arima(x2, trace = TRUE)
fit0$aicc
plot(fit0)

fit1<-Arima(x2, order = c(5,0,0))
fit1$aicc
autoplot(fit1)

##On original daily close
fit11<-Arima(rev(log(Bitcoin_train$Close)), order = c(5,2,0))
fit11$aicc
autoplot(fit11)

fit22<-Arima(rev(log(Bitcoin_train$Close)), order = c(0,2,1))
fit22$aicc
autoplot(fit22)

fit33<-Arima(rev(log(Bitcoin_train$Close)), order = c(2,2,1))
fit33$aicc
autoplot(fit33)


# Evaluate residuals
checkresiduals(fit11)
acf(residuals(fit11))
plot(residuals(fit11))

qqnorm(residuals(fit11))
qqline(residuals(fit11)) #yikes that outlier
shapiro.test(residuals(fit11)) #residuals are heavier-tailed than normal

plot(exp(fit11$x), type ="l")
lines(exp(fit11$fitted),col="blue")


checkresiduals(fit22)
acf(residuals(fit22))
plot(residuals(fit22))
plot(exp(fit22$x), type ="l")
lines(exp(fit22$fitted),col="blue")



checkresiduals(fit33)
qqline(residuals(fit33)) #yikes that outlier
shapiro.test(residuals(fit33)) #residuals are heavier-tailed than normal

plot(exp(fit33$x), type ="l")
lines(exp(fit33$fitted),col="blue")







#Forcasting

autoplot(forecast::forecast(fit11, h=20))

fcast1 <- forecast::forecast(fit11, h=20)


fcast1$fitted = exp(fcast1$fitted)
fcast1$lower = exp(fcast1$lower)
fcast1$upper = exp(fcast1$upper)
fcast1$mean = exp(fcast1$mean)
fcast1$x = exp(fcast1$x)
fcast1$series = fcast1$series
fcast1$residuals = exp(fcast1$residuals)
fcast1$mean


test= rev(Bitcoin_test$Close)[1:20]

test

sse1 = sum((fcast1$mean[1:20] - test)^2) #SSE

plot(seq(1,20),fcast1$mean[1:20],type="l", col = "blue",ylim=c(10000,25000))
points(seq(1,20), test,type="l", col="red")
points(seq(1,20), fcast1$lower[1:20],type="l", col="black")
points(seq(1,20), fcast1$upper[1:20], type="l",col="black")



p <- autoplot(fcast1) + ggtitle("") + 
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5))
p

fcast2 <- forecast::forecast(fit22, h=20)


fcast2$fitted = exp(fcast2$fitted)
fcast2$lower = exp(fcast2$lower)
fcast2$upper = exp(fcast2$upper)
fcast2$mean = exp(fcast2$mean)
fcast2$x = exp(fcast2$x)
fcast2$series = fcast2$series
fcast2$residuals = exp(fcast2$residuals)
fcast2$mean


sse2 = sum((fcast2$mean[1:20] - test)^2) #SSE

plot(seq(1,20),fcast2$mean[1:20],type="l", col = "blue",ylim=c(10000,25000))
points(seq(1,20), test,type="l", col="red")
points(seq(1,20), fcast2$lower[1:20],type="l", col="black")
points(seq(1,20), fcast2$upper[1:20], type="l",col="black")



fcast3 <- forecast::forecast(fit33, h=20)


fcast3$fitted = exp(fcast3$fitted)
fcast3$lower = exp(fcast3$lower)
fcast3$upper = exp(fcast3$upper)
fcast3$mean = exp(fcast3$mean)
fcast3$x = exp(fcast3$x)
fcast3$series = fcast3$series
fcast3$residuals = exp(fcast3$residuals)
fcast3$mean


sse3 = sum((fcast3$mean[1:20] - test)^2) #SSE

plot(seq(1,20),fcast3$mean[1:20],type="l", col = "blue",ylim=c(10000,25000))
points(seq(1,20), test,type="l", col="red")
points(seq(1,20), fcast3$lower[1:20],type="l", col="black")
points(seq(1,20), fcast3$upper[1:20], type="l",col="black")


sse1
sse2
sse3



