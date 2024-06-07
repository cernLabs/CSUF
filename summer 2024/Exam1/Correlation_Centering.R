#### Centering and collinearity 
## 

library(astsa)
ts.plot(cmort, tempr, part, col =1:3)

d = data.frame(cmort=cmort,
               tempr=tempr,
               part=part)

#library("PerformanceAnalytics")
chart.Correlation(d, histogram = T, pch= 19)



tempc =  ts(tempr - mean(tempr), start = 1970, end = c(1979,40), frequency = 52)
tempc2 = ts(tempc^2, start = 1970, end = c(1979,40), frequency = 52)
tempc3 = ts(tempc^3, start = 1970, end = c(1979,40), frequency = 52)
tempr2 = ts(tempr^2, start = 1970, end = c(1979,40), frequency = 52)
tempr3 = ts(tempr^3, start = 1970, end = c(1979,40), frequency = 52)
trend = time(cmort)

d1 = data.frame(tempc=tempc,
                tempc2=tempc2,
                tempc3 =tempc3,
                trend=trend)

chart.Correlation(d1, histogram = T, pch= 19)

d2 = data.frame(tempr=d$tempr,
                tempr2=tempr2,
                tempr3 = tempr3,
                trend=trend)

chart.Correlation(d2, histogram = T, pch= 19)

reg1 = lm(cmort ~ trend + tempc + tempc2 + tempc3 + part, na.action = NULL)
#library("car") #variance inflation
vif(reg1)
summary(reg1)


par(mfrow=c(2,2)) # Dividing the plotting page into 4 panels
plot(reg1$fitted, reg1$residuals) # plot of fitted values vs residuals 
qqnorm(reg1$residuals) #qq-plot of residuals
qqline(reg1$residuals) # plotting the line, along which the dots in qq-plot should lie 
plot(reg1$residuals) # plotting the residuals vs time
abline(h=0,lty=2) # plotting a horizontal line at 0
acf(reg1$residuals) #sample acf plot of residuals

reg2 = lm(cmort ~ trend + tempr + tempr2 + tempr3 + part, na.action = NULL)
vif(reg2)
summary(reg2)
par(mfrow=c(2,2)) # Dividing the plotting page into 4 panels
plot(reg2$fitted, reg2$residuals) # plot of fitted values vs residuals 
qqnorm(reg2$residuals) #qq-plot of residuals
qqline(reg2$residuals) # plotting the line, along which the dots in qq-plot should lie 
plot(reg2$residuals) # plotting the residuals vs time
abline(h=0,lty=2) # plotting a horizontal line at 0
acf(reg2$residuals) #sample acf plot of residuals



#Centering does not always work: https://pubmed.ncbi.nlm.nih.gov/31488914/