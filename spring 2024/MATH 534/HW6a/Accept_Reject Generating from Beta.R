set.seed(111)
n = 10000
U = runif(n)
V = runif(n, 0,2.6697)
f = dbeta(U, shape1 = 2.7, shape2 = 6.3)
X = U[which(V <= f)] # These are random values from Beta(2.7, 6.3)

#--- testing the values
qqplot(x = X, y = rbeta(length(X), shape1 = 2.7, shape2 = 6.3))
