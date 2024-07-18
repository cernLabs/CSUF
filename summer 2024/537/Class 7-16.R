library(mvtnorm)
data = read.csv("example.csv", header = T)
n = length(data[,1])
head(data) 
BS.xbars = matrix(0,10000,2)
for (i in 1:10000){
  index = sample(1:n,n,replace = T)
  BS.data = data[index,]
  BS.xbars[i,] = apply(BS.data,2,mean)
   }

xbar = apply(data,2,mean)

S.xbar = cov(data)/n
MD = rep(0,10000)

for(i in 1:10000){
  MD[i] = t(xbar-BS.xbars[i,])%*%solve(S.xbar)%*%(xbar - BS.xbars[i,])
}
quantile(MD, .95)
final.BS.xbars = BS.xbars[MD <= sort(MD)[9500],]
