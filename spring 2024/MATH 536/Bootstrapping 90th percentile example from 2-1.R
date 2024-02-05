###Bootstrap example with 90th percentiles from class

###################Part 1, perform a hypothesis test for whether or not
#the population 90th percentile could be greater than 12 using
#the amb_waittime.csv dataset.

#Step 1, set directories, read in data, compute sample size and sample
#90th percentile.  Maybe pop hist?

setwd("C:/Users/knichols/Desktop")
data = read.csv("amb_waittime.csv",h=T)
samp.90 = quantile(data$Waittime,.90)
n = length(data$Waittime)
hist(data$Waittime,breaks=20)


#Step 2, create a new pseudo population where the 90th
#percentile is actually 12 (i.e. the null hypothesis is true)
#Could do this via the bootstrap, or by parametrically picking
#a distribution for the pseudo population.  I'm choosing
#to go the bootstrapping route.

new.pop = data$Waittime - samp.90 + 12
quantile(new.pop,.90)

#Step 3, repeatedly resample n observations (like 10,000ish times) from our new
#pseudo population with replacement.  Store the results in a vector.

BS.90th = rep(0,10000)
for(i in 1:10000){
	BS.90th[i] = quantile(sample(new.pop,n,replace=T),.90)
}

#Step 4, Optional histogram.

hist(BS.90th,breaks=20)


#Step 5, empirically compute the pvalue (what % of our BS 90th percentiles
#were bigger than our actual 90th percentile from part 1?)

pval=length(BS.90th[BS.90th >= samp.90])/10000
pval

#Step 6, Chipotle...Mmm...white rice. black beans. chicken (not Al Pastor).
#All three salsas (trust me).  Corn.  Lettuce.  Sour cream.  No guac (I'm on 
#a professor's salary...CSUF doesn't pay like UCLA).  Cheese.  Perfection.

###################Part 2, create a bootstrap confidence interval for the true 90th percentile.

#Step 1, read in data.


setwd("C:/Users/knichols/Desktop")
data = read.csv("amb_waittime.csv",h=T)
samp.90 = quantile(data$Waittime,.90)
n = length(data$Waittime)
hist(data$Waittime,breaks=20)
new.pop = data$Waittime

#Step 2, create a new pseudo population without recentering anything.  Draw
#10,000 samples, each time recording the 90th percentile.

BS.90th = rep(0,10000)
for(i in 1:10000){
	BS.90th[i] = quantile(sample(new.pop,n,replace=T),.90)
}
hist(BS.90th,breaks=20)


#Step 3, find the cutoff for the 2.5th percentile and the 97.5th percentile of
#your bootstreap sample results.

a = quantile(BS.90th,.025)
b = quantile(BS.90th,.975)
c(a,b)


################Part 3.  Shoot.  I think that the 90th percentile might
#be biased.  I need to check.  Theory is for suckers.  Bootstrap to the rescue.

#Step 1:  Create a pseudo pop via bootstrap with some jittering

pseudo.pop = c()
orig.samp = data$Waittime
n = length(orig.samp)
sig = .24

for(i in 1:n){
	pseudo.pop = c(pseudo.pop,rep(orig.samp[i],1000) + rnorm(1000,0,sig))
}

par(mfrow=c(2,1))
hist(orig.samp,breaks=20)
hist(pseudo.pop,breaks=20)

#Step 2:  compute the 90th percentile for our pseudo pop

pop.90th = quantile(pseudo.pop,.90)

#Step 3:  Compute 10,000 90th percentiles for samples of size 300 from
#our pseudo pop.  Compute their average (est expected value)

BS.90th = rep(0,10000)
for(i in 1:10000){
	BS.90th[i] = quantile(sample(pseudo.pop,n),.90)
}

exp.samp.90th = mean(BS.90th)


#Step 4:  Est bia.  Step 2 res - step 3 res.  (Probably run it a few times.)
#If we keep getting similar results, our bootstrap size is good.
#If results rae highly varied, need more bootstrapping power.

est.bias = pop.90th - exp.samp.90th
est.bias

#My sample based estimate is gonna be on average about 0.18 lower than the actual
#90th percentile.  

#notes:  histograms are our friends.    


###############################Part 4 
#Lets repeat part 1, this time adjusting for bias.
#Anytime I take a sample and compute a sample 90th percentile (orig or bs)
#I'm going to add my est of bias to that estimate. 

#Step 1, set directories, read in data, compute sample size and sample
#90th percentile (adjust for bias (0.18).  Maybe pop hist?
est.bias = 0.18

setwd("C:/Users/knichols/Desktop")
data = read.csv("amb_waittime.csv",h=T)
samp.90 = quantile(data$Waittime,.90) + est.bias
n = length(data$Waittime)
hist(data$Waittime,breaks=20)


#Step 2, create a new pseudo population where the 90th
#percentile is actually 12 (i.e. the null hypothesis is true)
#Could do this via the bootstrap, or by parametrically picking
#a distribution for the pseudo population.  I'm choosing
#to go the bootstrapping route.

new.pop = data$Waittime - samp.90 + est.bias + 12
quantile(new.pop,.90)

#Step 3, repeatedly resample n observations (like 10,000ish times) from our new
#pseudo population with replacement.  Compute the 90th percentile (adj for bias)
#Store the results in a vector.

BS.90th = rep(0,10000)
for(i in 1:10000){
	BS.90th[i] = quantile(sample(new.pop,n,replace=T),.90) + est.bias
}

#Step 4, Optional histogram.

hist(BS.90th,breaks=20)


#Step 5, empirically compute the pvalue (what % of our BS 90th percentiles
#were bigger than our actual 90th percentile from part 1?)

pval=length(BS.90th[BS.90th >= samp.90])/10000
pval
    