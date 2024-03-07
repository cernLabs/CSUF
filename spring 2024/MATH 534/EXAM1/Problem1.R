like <- function(y, X, beta, grad = FALSE) {
  # X is an n x p data frame consisting of predictors. If there is an intercept,
  #    the first column of X is 1
  # y is an n x 1 vector of responses
  # beta is a p x 1 vector of coefficients
  g = NULL
  ell = NULL
  tmp <- dim(X)
  n <- tmp[1]
  p <- tmp[2]
  v = matrix(0, p, 1)
  ebx <- 0
  ebxx <- matrix(0, p, 1)
  for (i in 1:n) {
    v = v + t(X[i,]*y[i])
    e_xbeta <- exp(as.numeric(as.matrix(X[i,]) %*% beta))
    ebx <- ebx + e_xbeta  
  }
  ell <- -ebx + t(v) %*% beta
  list(ell = ell, g = g)
}
