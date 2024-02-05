#Problem 2:  Single sample test of a mean.  Theoretical way first.

data = read.csv("amb_waittime.csv",h=T)
length(data$Waittime)
mean(data$Waittime)
sd(data$Waittime)

z = (mean(data$Waittime) - 10)/(sd(data$Waittime)/sqrt(length(data$Waittime)))
pval1 = 1-pnorm(z)
pval2 = 1-pt(z,299)


#Or built in functions work just fine.
t.test(data$Waittime,alternative="greater",mu=10)

#Final answer, pvalue = 0.295

#Now...simulation via the bootstrap!

#Step 1:  Take a random sample from our pseudo/bootstrap population (if the mean of the population is equal to 10).  
#note:  for each value in population, subtract xbar, then add 10.  

BS.pop = data$Waittime - mean(data$Waittime) + 10

#Step 2:  Take a sample of size 300 from our adjusted population, record the sample mean.

single.samp = sample(BS.pop,300,replace=T)
single.xbar = mean(single.samp)


#Step 3:  Repeat step 2, 10,000 times, record all 10,000 sample means

BS.xbars = rep(0,10000)
for(i in 1:10000){
	single.samp = sample(BS.pop,300,replace=T)
	BS.xbars[i] = mean(single.samp)
}


#Step 4: Histograms are our friends (should look roughly normal).  

hist(BS.xbars,breaks=20)
lines(c(10,10),c(0,10000),lwd=2,lty=2,col=2)
lines(c(10.34,10.34),c(0,10000),lwd=2,lty=2,col=4)

#Step 5:  Empirically compute what percentage of our simulated sample means under the Ho are greater than
#our original xbar of 10.32.  

pval = length(BS.xbars[BS.xbars>mean(data$Waittime)])/10000
pval

