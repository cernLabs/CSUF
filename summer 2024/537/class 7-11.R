n = 9999
vec1 <- rexp(n,rate=runif(1))
vec2 <- rexp(n,rate=runif(1))
vec3 <- rexp(n,rate=runif(1))
V <- as.matrix(cbind(vec1,vec2,vec3))
# bootstrap
N = 10000
X = matrix(0,N,3)
for(i in 1:10000){
  BS.i <- sample(1:n,10000, replace =T)
  X[i,1] = mean(vec1[BS.i])
  X[i,2] = mean(vec2[BS.i])
  X[i,3] = mean(vec3[BS.i])
}
dim(X)
# show him it's MVTnorm
par(mfrow = c(2,2))
for(i in 1:N){hist(X[,i],breaks = 100, freq = F)}

S = cov(V)
S.xbar = S/N
mu = apply(V,2,mean)


QF = rep(0,10000)
for(i in 1:10000){
  QF[i] = (X[i,] - mu)%*%solve(S.xbar)%*%(X[i,] - mu)
}

hist(QF,breaks = 20, freq = F)
x.seq = (0:N)/10
y.seq = dchisq(x.seq,3)
lines(x.seq,y.seq,col = 2, lwd = 2)
