J =  4 # number of batches
X = c(.86,.3,.05,.73)*(-1)
n = rep(5,4)
Y = c(0,1,3,5)
library(boot)
library(mvtnorm)

Prior <- function(alpha,beta){
  1
}

likelihood <- function(Y,n,X,alpha,beta){
 theta = inv.logit(alpha + beta*X)
 prod(dbinom(Y,n,theta))
}

rProposal <- function(n,alpha,beta,s,S){
  rmvnorm(n, mean = c(alpha,beta), sigma = s*S)
}


accept  = 0
s = 1 # tuning parameter 
S = diag(2)
##
temp <- Y/n #MLE's of thetas
summary(lm(logit(temp ~ X)))



alpha.post <- beta.post <- numeric()
alpha.post[1] = 0.1
beta.post[1] = 2.9


B = 1000

for(b in 1:B){
  # joint Metropolis - Hastings
  ab.star = rProposal(1, alpha.post(b-1), beta.post(b-1),s,S)

  r = (Likelihood(y, n, x, ab.star[1], ab.star[2]))/(Likelihood(y, n, x, alpha.post[(b-1)], beta.post[(b-1)])*Prior(alpha.post[(b-1)], beta.post[(b-1)]))
  
  # Accept/Reject
  if(runif(1) < min(1, r)){
    alpha.post[b] = ab.star[1]
    beta.post[b] = ab.star[2]
    accept = accept + 1
  }else{
    alpha.post[b] = ab.star[(b-1)]
    beta.post[b] = ab.star[(b-1)]
  }
}