#Example from Carlin and Louis text, Page 145
set.seed(1100)
# number of power plant failures
y <- c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22)
# thousands of hours for 10 different systems
t <- c(94.3, 15.72, 62.88, 125.76, 5.24, 31.44, 1.048, 1.048, 2.096, 10.48)
n=10000
gam = 0.1
del = 1
alpha=1.8
nsim = n
np = length(y) # in this example it is the number of powerplants, which is 10
teta_Gibbs = matrix(NA,nsim,np) # A matrix with each row representing theta draws
beta_Gibbs = matrix(NA,nsim,1) # A vector containing beta draws
# Draw a starting value for beta
i = 1
beta_Gibbs[i] = rgamma(1,gam,del)
repeat {
  teta_Gibbs[i,] = rgamma(np,y+alpha, t+ beta_Gibbs[i])
  if (i == nsim) break
  beta_Gibbs[i+1] = rgamma(1,np*alpha+gam,del+sum( teta_Gibbs[i,]))
  i = i+1
}
burn = 1000
mean(beta_Gibbs[burn:nsim])
sd(beta_Gibbs[burn:nsim])
colMeans(teta_Gibbs[burn:nsim,])
apply(teta_Gibbs[burn:nsim,],2,sd)
hist(teta_Gibbs[,3])
hist(beta_Gibbs)

plot(beta_Gibbs[9000:n],type='l')
qqplot(beta_Gibbs[9000:n],beta_Gibbs[8000:9000])
%--------- diagnostic using coda
library(coda)
gibbs.draws = cbind(teta_Gibbs,beta_Gibbs)
mcmc.draws = mcmc(gibbs.draws) # This turns our draws into an mcmc object
# you can do burn-in or drop draws with the start and end argument
summary(mcmc.draws) # Get summary statistics for each draw
plot(mcmc.draws) # shows iteratin vs chain and density
# check convergence of running means
# May be used as a tool to determine burn in
temp=apply(gibbs.draws,2,cumsum)
means = temp/matrix(1:n,n,11)
par(ask=TRUE)
for( i in 1:11){
  plot(1:n,means[,i],type='l')
}

#---------- Plot of auto correlation 
# (as lags increase, auto correlations should drop)
# 
par(ask=TRUE)
for( i in 1:11){
  acf(ts(gibbs.draws[,i]))
}
# ---- Geweke Diagnostic
#compares two non-overlapping parts
#(usually the first 0.1 and last 0.5 proportions) 
# of the Markov chain to see if the two parts of 
# the chain are from the same distribution (null hypothesis).
# The test statistic is a standard Z-score with 
# the standard errors adjusted for autocorrelation.

geweke.diag(mcmc.draws, frac1=.1,frac2=.5)
geweke.plot(mcmc.draws)

#--- other diagnostics
autocorr(as.mcmc(gibbs.draws))
cumuplot(as.mcmc(gibbs.draws), probs=c(0.025,0.5,0.975))
densplot(as.mcmc(gibbs.draws))
