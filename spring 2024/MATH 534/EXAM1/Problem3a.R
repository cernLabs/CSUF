
Hess <- function(y, X, beta) {
  # X is an n x p matrix of predictors
  # y is an n x 1 vector of responses
  # beta is a p x 1 vector of coefficients
  
  tmp <- dim(X)
  n <- tmp[1]
  p <- tmp[2]
  H <- matrix(0, p, p)
  for( i in 1:n){
    e_xbeta <- exp(as.matrix(X[i,]) %*% beta)
    for (k in 1:p) {
      for(m in 1:p) {
        H[k,m] <- H[k,m] - e_xbeta * X[i,k] * X[i,m]
      }
    }
  }
  return(H)
}

#create Newton's method for maximization
newton <- function(y, X, beta, maxit = 100, tol_err = 1e-6, tol_grad = 1e-6) {
  # X is an n x p matrix of predictors
  # y is an n x 1 vector of responses
  # beta is a p x 1 vector of coefficients
  # maxit is the maximum number of iterations
  # tol is the convergence tolerance
  
  # initialize the parameters
  tmp <- dim(X)
  n <- tmp[1]
  p <- tmp[2]
  beta_old <- beta
  beta_new <- beta
  # iterate until convergence
  for (iter in 1:maxit) {
    # compute the gradient and Hessian
    a <- like(y, X, beta_new, grad = TRUE)
    g <- a$g
    H <- Hess(y, X, beta_new)
    # update the parameters
    beta_old <- beta_new
    beta_new <- beta_old - solve(H) %*% g
    # Add your code for checking convergence here
    # Keep the print statement below for your final version
    print(c(iter, beta_new, a$ell, norm(a$g, "2"), mre))
  }
  # return the estimated parameters
  return(beta_new)
}
