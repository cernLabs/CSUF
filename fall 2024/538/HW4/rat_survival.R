
dat <- read.csv("/Users/valeriepoynor/CSU Fullerton Dropbox/Valerie Poynor/My Math 538/Math 538 Fall 2024/Exam 1/rat.csv")
event <- rep(1, dim(dat)[1])
View(dat)

### The Frequentist log-linear Weibull Regression
library(survival)
m2=survreg(Surv(dat$V1,event)~dat$group,dist="weibull")
m2

### What you did on the exam
?dweibull #stats
mean(dat$V1)

y = dat$V1
x = dat$group


Prior <- function(beta0, beta1){
  1
}

Likelihood <- function(Y, X, beta0, beta1){
  sig <- exp(beta0 + beta1*X) #library(boot)
  prod(dweibull(Y, 4, sig))
}

#Proposal <- function(alpha, beta, S, s){

#}

rProposal <- function(n, beta0, beta1, s, S){
  rmvnorm(n, mean= c(beta0, beta1),  sigma = s*S) #library(mvtnorm)
}

## initialize 
accept = 0
s = 1  # tuning parameter
S = matrix(c(0.00063, -0.00064 , -0.00064, 0.00125), nrow = 2, ncol = 2)
###
beta0.post <- beta1.post <- numeric()
beta0.post[1] <- 0.678
beta1.post[1] <- 0.404
B = 10000

for(b in 2:(B+1)){
  
  #Joint Metropolis - Hastings step alpha, beta | data
  ab.star = rProposal(1, beta0.post[(b-1)], beta1.post[(b-1)], s, S )
  
  r = ((Likelihood(y, x, ab.star[1], ab.star[2])
        *Prior(ab.star[1], ab.star[2])
        #*Proposal()
  )
  /(Likelihood(y, x, beta0.post[(b-1)], beta1.post[(b-1)])
    *Prior(beta0.post[(b-1)], beta1.post[(b-1)])
    #*Proposal()
  )
  )
  if(runif(1) < min(1, r)){
    beta0.post[(b)] = ab.star[1]
    beta1.post[(b)] = ab.star[2]
    accept = accept + 1
  }else{ 
    beta0.post[(b)] = beta0.post[(b-1)]
    beta1.post[(b)] = beta1.post[(b-1)]
  }
  
  
  # Prints every 100th iteration
  if(b %% 1000==0) {
    cat(paste0("iteration: ", b, "\n"))
  } }

accept/B #0.5692

plot(beta0.post, type="l")
plot(beta1.post, type="l")
quantile(beta1.post, probs = c(0.025, 0.5, 0.975))
quantile(beta0.post, probs = c(0.025, 0.5, 0.975))

burn = 1
acf(beta0.post[-c(1:burn)])
acf(beta1.post[-c(1:burn)])

var(beta0.post[-c(1:burn)])
cov(beta0.post[-c(1:burn)],beta1.post[-c(1:burn)] )
var(beta1.post[-c(1:burn)])


     
########################
#### Using Rstan #######
########################

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

rat_dat <- list(
  N = dim(dat)[1],
  y = c(dat$V1),
  x = c(dat$group)
)


fit1 <- stan(
  file = "weibull_regression.stan", # Stan program
  data = rat_dat,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 20000,           # total number of iterations per chain
  cores = 2,              # number of cores 
  refresh = 1000,         # show progress every 'refresh' iterations
  thin = 10               # number of thinning
)


stan_trace(fit1, pars = c("alpha","beta0", "beta1"), inc_warmup = TRUE, nrow = 2)


stan_ac(fit1, pars = c("beta0", "beta1"), inc_warmup = FALSE, nrow = 2, separate_chains =TRUE, lags = 25)


pairs(fit1, pars = c("alpha","beta0", "beta1"), las = 1)     

print(fit1, pars=c("alpha","beta0", "beta1"), probs=c(.025,.5,.975))

fit_ss <- extract(fit1) 
names(fit_ss)

biv_kde <- MASS::kde2d(fit_ss$beta0,fit_ss$beta1)
plot(fit_ss$beta0,fit_ss$beta1, pch = "." , xlab = "beta0", ylab = "beta1")
contour(biv_kde, col = 2, add=T)

# Hazard 

