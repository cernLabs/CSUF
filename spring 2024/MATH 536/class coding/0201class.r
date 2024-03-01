# getting the waittimes
wait <- read.csv("amb_waittime.csv")
# recentering ?
bs.pop <- wait$Waittime -mean(wait$Waittime)

bs.sample <- sample(bs.pop,300,replace=TRUE)

xbar <- mean(bs.sample)

bs.xbars <- rep(0,10000)

for (i in 1:10000){
  sample.samp = sample(bs.pop,300, replace =TRUE)
  bs.xbars[i] = mean(sample.samp)
  
}

hist(bs.xbars,breaks=40)



#lines(c(10,10),c(0,10000),lwd=2,col=2)
#lines(c(10.34,10.34),c(0,10000),lwd=2,col=2)
pval = length(bs.xbars[bs.xbars>mean(wait$Waittime)])/10000
pval

