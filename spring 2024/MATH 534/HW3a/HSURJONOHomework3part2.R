sigma <- matrix(c(1,0.7,0.7,0.7,1,0.7,0.7,0.7,1), nrow = 3, ncol = 3)
mu <- c(-1,1,2)
sigma_inv <- solve(sigma)
#function to square root a matrix "A"
sqrtm <- function(A){
  a <- eigen(A)
  sqm <- a$vectors %*% diag(sqrt(a$values)) %*% t(a$vectors)
  sqm <- (sqm+t(sqm))/2
}

#function for generating data
gen <- function(n,p,mu,sigma,seed){
  #generate data from a p-variate normal with mean mu and covaraince sigma
  #set seed to 2024
  set.seed(seed)
  #generate data from normal
  z <- matrix(rnorm(n*p),n,p)
  datan <- z %*% sqrtm(sigma) + matrix(mu,n,p,byrow = TRUE)
  return(datan)
}

datan <- gen(200, 3, mu, sigma,2024)
print(datan[1:3,])


#part b

#function for gradient
gradient <- function(x, mu, sigma){
  p <- dim(x)[2]
  n <- dim(x)[1]
  siginv <- solve(sigma)
  sum_xi <- matrix(0, p, 1) # initialize the sum of xi matrix
  grad_mu <- sum_xi # initialize the gradient in term of mu
  C_mu <- matrix(0, p, p) # initialize the matrix
  for(i in 1:n){
    xi <- as.numeric(x[i,] - mu) # compute each xi - mu
    sum_xi <- sum_xi + xi # sum of xi - mu
    # now to find C(mu) = sum(xi-mu)(xi-mu)^(T)
    C_mu <- C_mu + xi %*% t(xi)
  }
  gradm <- siginv %*% sum_xi
  A <- (n * siginv) - siginv %*% C_mu %*% siginv
  grads <- matrix(0, nrow = dim(A)[1], ncol = dim(A)[2])
  for(i in 1:dim(sigma)[1]){
    grads[i,i] <- -(1/2) * A[i,i]
  }

  for(i in 1:dim(sigma)[1]-1){
    for (j in (i+1):dim(sigma)[2]){
        grads[i,j] <- -1 * A[i,j]
        grads[j,i] <- -1 * A[j,i]
      }
  }
  list(gradm = gradm, grads = grads)
}

#Make the gradient for mu and gradient for sigma into one vector
mu_sig_vec <- function(mu, sigma) {
  p <- length(mu)
  theta <- matrix(0, nrow = p + p*(p+1)/2, ncol = 1)
  theta[1:p] <- mu
  k <- p + 1
  for (i in 1:p) {
    for (j in 1:i) {
      theta[k] <- sigma[i, j]
      k <- k + 1
    }
  }
  return(theta)
}


#make theta into mu and sigma
teta_to_mu_sigma <- function(p, theta) {
  mu <- theta[1:p]
  sigma <- matrix(0, nrow = p, ncol = p)
  k <- p + 1
  for (i in 1:p) {
    for (j in 1:i) {
      sigma[i, j] <- theta[k]
      sigma[j, i] <- theta[k]  # symmetric
      k <- k + 1
    }
  }
  list(mu = mu, sigma = sigma)
}


#test gradient function to see if correct
#mu_hat <- colMeans(datan)
#sigma_hat <- (199*cov(datan))/200
#result <- gradient(datan, mu_hat, sigma_hat)
#result



#function for computing likelihood function and gradiant for multivariate normal
# x is my data matrix
# mu is the mean
# sigma is covariance matrix, sigma_inv is the inverse of sigma matrix
likemvn <- function(x, mu, sigma){
  n <- nrow(x)  # Number of rows
  p <- ncol(x)  # Number of columns
  #compute (xi -mu)(xi-mu)^(T)
  C_mu <- matrix(0, nrow= p, ncol= p)  # Initialize the matrix
  sum_xi <- matrix(0, nrow = p, ncol= 1)  # Initial sum of xi matrix
  for (i in 1:n) {
    xi <- x[i,] - mu
    sum_xi <- sum_xi + xi
    print(sum_xi)
    C_mu <- C_mu + xi %*% t(xi) 
  }
  l <- -(n * p * log(2*pi) + n * log(det(sigma)) + sum(solve(sigma) %*% C_mu)) / 2 
  return(l)
}


# Steepest Ascent optimization
optmvn <- function(mu, datan, sigma, maxit, tolerr, tolgrad) {
  header = paste0("Iteration", "      halving", "     log-likelihood",      "||Gradient||")
  print(header)
  for (it in 1:maxit) {
    a <- likemvn(datan, mu, sigma)
    gradient_matrix <- gradient(datan, mu, sigma)
    gradm <- gradient_matrix$gradm
    grads <- gradient_matrix$grads
    dir_n <- c(gradm, grads)
    norm_grad <- norm(dir_n, type ='2')
    theta <- mu_sig_vec(mu, sigma)
    theta_new <- theta + dir_n
    mu1 <- theta_new[1:p]
    sigma1<- theta_new[(p+1):nrow(theta_new),]
    atmp <- likemvn(datan, mu1, sigma1)
    halve <- 0
    a <- dim(x)
    p <- a[2]
    while (atmp[1] < a[1] & halve <= 20) {
      halve <- halve + 1
      theta_new <- theta + (dir_n / 2^halve)
      mu1 <- theta_new[1:p]
      sigma1 <- theta_new[(p+1):nrow(theta_new),]
      atmp <- likemvn(datan, mu1, sigma1)
      print(c(it, halve, atmp[1] ,norm_grad))
    }
    if (halve >= 21) {
      print('Step-halving failed after 20 halvings')
      break
    }
    
    print(sprintf('it = %2.0f halve=%2.0f  mu1 = %12.12f   atmp = %12.12f   norm_grad = %12.12f',
                  it, halve, atmp[1], norm_grad))
    print('-------------------------------------------------')
    
    # Stopping condition
      max_rel_err <- max(abs((theta_new - theta) / pmax(1, abs(mu))))
      abs_grad <- abs(max(theta_new))
      if (max_rel_err < tolerr & abs_grad < tolgrad) {
        break
      }
      #update variables to next iteration
      mu <- mu1
      sigma <- sigma1
    }
  return(list(l = atmp[1], mu = mu, sigma = sigma, iteration = it))
  }
  
  
test <- optmvn(datan, mu, sigma,500, 10e-6, 10e-5)
test

