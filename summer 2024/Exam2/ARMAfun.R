ar1 <-arima.sim(list(order = c(1,0,0),ar = 0.9), n= 100)
plot(ar1, type="l")
acf(ar1)

ar2 <-arima.sim(list(order = c(2,0,0),ar = c(0.4,-0.4)), n= 100)
plot(ar2, type="l")
acf(ar2)


library(forecast)
x = numeric()
x[1] = 0
for(j in 2:101){
  x[j] = 1.5*x[(j-1)] + rnorm(1)
}
plot(x, type= "l")
acf(x)
#arima will give an error if estimates imply nonstationarity under causal model
auto.arima(x, stationary = TRUE, allowmean = TRUE) #try stationary true 
arima(x, order = c(1,0,0), include.mean = FALSE) #Exam 2


#ARMA simulations and estimations

arma1 <-arima.sim(list(order = c(2,0,1),ar = c(0.2, -.4), ma= 0.5), n= 100)
plot(arma1)
auto.arima(arma1, stationary = TRUE, allowmean = FALSE, allowdrift = FALSE)
arima(arma1, order = c(2,0,1), include.mean = FALSE)

# Xt = 0.8X(t-1) + 0.2X(t-2) + Wt  -->AR(2)

m <- 1
ar2 <- arima.sim(model = list(order= c(2,0,0),ar = c(0.8, 0.1)), n = 500) + m
plot(ar2, type="l")                 
?arima
ar2.mod <- arima(ar2, order= c(2,0,0))
acf(ar2.mod$residuals)


m <- 1
ar2ma1 <- arima.sim(model = list(order= c(2,0,1), ar = c(0.8, 0.1), ma = 0.3), n = 500) + m
plot(ar2ma1, type="l")                 
?arima
ar2ma1.mod <- arima(ar2ma1, order= c(2,0,1))
ar2ma1.mod
acf(ar2ma1.mod$residuals)

