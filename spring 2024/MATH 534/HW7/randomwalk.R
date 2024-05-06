randomwalk<- function (nsim, sig) {
  x=numeric(nsim)
  x[1] = 25 # This is the initial value, grossly away from the target
  nu = 4
  ft = dt(x[1],nu)
  accept = 0
  for (i in 2:nsim){
    xs = rnorm(1,x[i-1],sig)
    fs = dt(xs,nu)
    r = fs/ft
    move <- "stay"
    if(r >= 1){
      move <- "leave"
    } else {
      move <- sample(x = c("stay", "leave"), size = 1, prob = c(1-r, r))
    }
    if (move == "leave"){
      x[i] = xs
      ft = fs
      accept = accept+1
    }else{
      x[i] = x[i-1]
    }
  }
  ratio = accept/nsim
  list(x = x, ratio = ratio)
}

set.seed(110)
nsim=10000
X = matrix(NA,nsim,5) # each column is a MC corresponding to a value of sigma
cnt=0
# Note that variance of target is 2
for (sig in c(.05,.5, 1.4, 2, 20)){
  temp = randomwalk(nsim, sig)
  cnt = cnt+1
  X[,cnt] = temp$x
}


#--------- diagnostic using coda
library(coda)
mcmc.draws = mcmc(X) # This turns our draws into an mcmc object
# you can do burn-in or drop draws with the start and end argument
summary(mcmc.draws) # Get summary statistics for each draw
traceplot(mcmc.draws)
1-rejectionRate(mcmc.draws)
par(mfrow=c(1,1))
par(ask = FALSE)
plot(X[,1],type='l') # low rejection rate
plot(X[,2],type='l') # not enough mixing
plot(X[,3],type='l') # good mixing
plot(X[,4],type='l') # good mixing
plot(X[,5],type='l') # high rejection rate


# May be used as a tool to determine burn in
temp=apply(X,2,cumsum)
means = temp/matrix(1:nsim,nsim,5)
par(ask=TRUE)
for( i in 1:5){
  plot(1:nsim,means[,i],type='l')
}

#Let's burn 2000 and look again
mcmc.draws = mcmc(X[2000:10000,]) # This turns our draws into an mcmc object
# you can do burn-in or drop draws with the start and end argument
summary(mcmc.draws) # Get summary statistics for each draw
1-rejectionRate(mcmc.draws)
par(mfrow=c(1,1))
par(ask = FALSE)
plot(X[2000:10000,1],type='l') # low rejection rate
plot(X[2000:10000,2],type='l') # not enough mixing
plot(X[2000:10000,3],type='l') # good mixing
plot(X[2000:10000,4],type='l') # good mixing
plot(X[2000:10000,5],type='l') # high rejection rate


#---------- Plot of auto correlation 
# (as lags increase, auto correlations should drop)
# 
par(ask=TRUE)
for( i in 1:4){
  acf(ts(X[,i]))
}
par(ask=FALSE)
# ---- Geweke Diagnostic
#compares two non-overlapping parts
#(usually the first 0.1 and last 0.5 proportions) 
# of the Markov chain to see if the two parts of 
# the chain are from the same distribution (null hypothesis).
# The test statistic is a standard Z-score with 
# the standard errors adjusted for autocorrelation.

geweke.diag(mcmc.draws)
geweke.plot(mcmc.draws)

hist(X[2000:10000,4],type='l', breaks = 50, freq = FALSE)
curve(dt(x,4), -10,10, add = TRUE, col = "red")
