# The histogram for the prior
mids=seq(.005,.105,.01)
freq = c(1.6,4.9,15.6,39.5,100,50.1,14.9,6.9,4.3,3.6,2.3)
barplot(freq,space=0,names.arg=as.character(mids),xlab='p',
        ylab='frequency',main = 'The pior distribution')
# Sampling from the prior distribution histogram
set.seed(112)
m = 5000 # Number of samples taken from the prior
prob = freq/sum(freq);  # compute probabilities corresponding to intervals
p_prior = sample(mids,m,replace=TRUE,prob=prob)
p_prior = p_prior +runif(m,-.005,.005)

# Computing weights
w = (p_prior^4*(1-p_prior)^40)
w = w/sum(w)

# Pick values from the posterior
p_post = sample(p_prior,size=m,replace=TRUE,prob=w)

#--- Going to export the p_prior and p_post to be able 
#to see the graph on Rguroo
Compare <- data.frame(prior = p_prior, posterior = p_post)
write.csv(Compare, file = "/Users/Mori/Desktop/Compare.csv")


# Inference from the posterior
# Compute The probability of 0 to 7 home runs, when N=40
# at bat. - We use p_post and the fact that Y|P is binomial
py = numeric(8)
for( y in 1:7){
  py[y]  = mean(dbinom(y,size = 40, p_post))
}
barplot(py)

