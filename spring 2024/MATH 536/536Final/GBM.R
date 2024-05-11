###Gradient Boosting Trees in R

#Lets attack that pesky airquality problem again using gradient boosting trees

library(gbm)
data = na.omit(airquality)
gbm.model = gbm(Ozone~Solar.R+Wind+Temp,data=data,n.trees=2000,distribution="gaussian",shrinkage=.01,cv.folds=5)

#model diagnostics

names(gbm.model)

#Train vs. test error

par(mfrow=c(2,1))


gbm.model$cv.error
plot(1:2000,gbm.model$cv.error,type="l")

gbm.model$train.error
plot(1:2000,gbm.model$train.error,type="l")

#Model will always continue to overfit!!! CV is our friend.  Probably want to use approximately 1000 trees.

gbm.model = gbm(Ozone~Solar.R+Wind+Temp,data=data,n.trees=1000,distribution="gaussian",shrinkage=.01,cv.folds=5)

fit = gbm.model$cv.fitted
errors = data$Ozone - fit

#Create residual vs. fit plot.

plot(fit,errors)
loess.line = loess(errors~fit)
lines(loess.line$x[order(loess.line$x)],loess.line$fitted[order(loess.line$x)],col=2)
lines(c(0,200),c(0,0),lty=2,col="grey")

#Compute r^2 manually

r2 = (sum((data$Ozone - mean(data$Ozone))^2) - sum(errors^2))/sum((data$Ozone - mean(data$Ozone))^2)

#Compare with a linear model

model.lin = lm(Ozone~Temp+Wind+Solar.R,data=data)
summary(model.lin)
plot(model.lin)
