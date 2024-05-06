
#Let's create some data, for now just two predictors

x1 = rnorm(2000,0,10)
x2 = rnorm(2000,0,10)

#Now let's let a logistic regression model be appropriate.  That is to say
#that the log odds are linearly correlated with x1,x2

log.odds = .05*x1 + .05*x2
prob = exp(log.odds)/(1 + exp(log.odds))
hist(prob)

#Now sample y's based on their respective probabilities

y = rep(0,2000)
for(i in 1:2000){y[i] = sample(c(1,0),1,prob=c(prob[i],1-prob[i]))}

log.odds = .05*x1 + .05*x2
prob = exp(log.odds)/(1 + exp(log.odds))
hist(prob)

#Now sample y's based on their respective probabilities

y = rep(0,2000)
for(i in 1:2000){y[i] = sample(c(1,0),1,prob=c(prob[i],1-prob[i]))}

#Now we fit a logistic model 

model = glm(y~x1+x2,family='binomial')

######################################################
###Side quest to discuss interpretations of slopes####
######################################################

#Interpreting slopes of a log reg model
#On average, a one unit increase in x1 results in a 
#.048056 increase in the log odds.

exp(.048056)

#Interpreting slopes of a log reg model
#On average, a one unit increase in x1 results in a 
#4.92% increase in odds.

exp(.048056-1.96*.004946)
exp(.048056+1.96*.004946)

#Interpreting slopes of a log reg model
#We're 95% confidenent that
#on average, Holding all else constant
#a one unit increase in x1 results in a 
#3.91 to 5.95% increase in odds.

#Slope for gender is 0.9 with s.e. = 0.2
#F=1, M=0

exp(0.9+1.96*.2)
exp(0.9-1.96*.2)

#if response is prob you get harrassed in the workplace

#We are 95% confident that on average and
#holding all else constant
#Being a female as opposed to being a male is
#a 66% to 264% increase in odds of being harrassed.

#fitted values (probabilities)

###############################################
#####End of side quest to understand slopes####
###############################################

model$fit


#Note we're in the unique position to compare estimated probabilities to true probabilities

plot(model$fit,prob)

#Now we want to talk about residual diagnostics.  There are several different ways we could think of a residual.  Two that we've discussed are Deviance and Pearson residuals.

dev = (2*y - 1)*sqrt(-2*(y*log(model$fit)+(1-y)*log(1-model$fit)))

pearson = (y - model$fit)/(model$fit*(1-model$fit))

###Note to get deviance you could use residuals(model), to get pearson you could use model$res

plot(model$res,pearson)
plot(residuals(model),dev)

###Now we're interested in understanding how these 
#residuals help clue us in to model assumptions.  
#Let's plot pearson and dev against our estimated log odds.

est.ln.odds = log(model$fit/(1-model$fit))
plot(est.ln.odds,pearson)
plot(est.ln.odds,dev)

#For a specific phat, or estimated log odds, there are only two possible residuals (y = 0, or y = 1).  The idea is that one of those errors will be a smaller error.  As long as
#the larger errors are occurring in relative probabilisitic frequency we have the right model.

loess1 = loess(pearson~est.ln.odds)
plot(est.ln.odds,pearson)
lines(est.ln.odds[order(est.ln.odds)],loess1$fit[order(est.ln.odds)])

loess2 = loess(dev~est.ln.odds)
plot(est.ln.odds,dev)
lines(est.ln.odds[order(est.ln.odds)],loess2$fit[order(est.ln.odds)])

#Pearson residuals are probably better for determining model fit, deviance can be used to determine if there is undue impact of an observation to the model.

#Could also consider plotting each predictor against pearson residuals instead of 
#est.log.odds (which is a linear combination of all predictors)

loess3 = loess(pearson~x1)
loess4 = loess(pearson~x2)
par(mfrow=c(2,1))
plot(x1,pearson)
lines(x1[order(x1)],loess3$fit[order(x1)])

plot(x2,pearson)
lines(x2[order(x2)],loess4$fit[order(x2)])

#The car package has a really nice and easy way of doing all this work.

library(car)
residualPlots(model)

#Lets pretend we have the wrong model now.


log.odds = .05*x1 + .008*x2^2
prob = exp(log.odds)/(1 + exp(log.odds))
hist(prob)


y = rep(0,2000)
for(i in 1:2000){y[i] = sample(c(1,0),1,prob=c(prob[i],1-prob[i]))}


model = glm(y~x1+x2,family='binomial')

plot(model$fit,prob)

residualPlots(model)

model1 = glm(y~x1 + I(x2^2),family='binomial')
residualPlots(model1)
plot(model1$fit,prob)




