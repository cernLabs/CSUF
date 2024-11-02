library(boot)
library(mvtnorm)

# Bioassay
J = 4
n = c(5,5,5,5)
y = c(0, 1, 3, 5)
x = c(-0.86, -0.30, -0.05, 0.73)

# Y_i|theta_i ~ Bin(n_i, theta_i); logit(theta_i) = alpha + beta*x_i
# p(alpha, beta) = 1

Prior = function(alpha, beta){
  1
}

Likelihood = function(Y, N, X, alpha, beta){
  th = inv.logit(alpha + beta*X)
  prod(dbinom(Y, N, th))
}

# because of symmetry no
# Proposal = function(alpha, beta, S, s){
#   dmvnorm(1, mean = c(alpha, beta), sigma = s*S)
# }

rProposal = function(n, alpha, beta, s, S){
  rmvnorm(1, mean = c(alpha, beta), sigma = s*S)
}

### initialize
accept = 0
s = 1 # tuning param. 
S = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)

# S = matrix(c(1, 2.3, 2.3, 18), nrow = 2, ncol = 2)
###

temp = y/n # "MLE's of thetas"
temp = c(0.01, 0.2, 0.6, 0.9) # fixing the boundries
summary(lm(logit(temp) ~ x)) # est. are reasonable est. for alpha and beta

alpha.post = beta.post = numeric()
alpha.post[1] = 0.1
beta.post[1] = 2.9

B = 1000

for(b in 2:(B+1)){
  
  #Joint MH step: alpha, beta|data
  ab.star = rProposal(1, alpha.post[(b-1)], beta.post[(b-1)], s, S)
  
  r = (Likelihood(y, n, x, ab.star[1], ab.star[2])) /
    (Likelihood(y, n, x, alpha.post[(b-1)], beta.post[(b-1)]) *
       Prior(alpha.post[(b-1)], beta.post[(b-1)]))
  
  # Accept/Reject
  if(runif(1) < min(1, r)){
    alpha.post[b] = ab.star[1]
    beta.post[b] = ab.star[2]
    accept = accept + 1
  }else{
    alpha.post[b] = alpha.post[(b-1)]
    beta.post[b] = beta.post[(b-1)]
  }
  
}

plot(alpha.post, type = "l")
plot(beta.post, type = "l")
burn = 500
acf(alpha.post[-c(1:burn)])
acf(beta.post[-c(1:burn)])

var(alpha.post[-c(1:burn)])
cov(alpha.post[-c(1:burn)], beta.post[-c(1:burn)])
var(beta.post[-c(1:burn)])













