#generate the data
sigma <- matrix(c(1,0.7,0.7,0.7,1,0.7,0.7,0.7,1), nrow = 3, ncol = 3)
mu <- matrix(c(-1, 1, 2), ncol = 1)
mu_inital <- matrix(c(-1.5,1.5, 2.3), ncol = 1)
sigma_inital <- matrix(c(1,0.5,0.5,0.5,1,0.5,0.5,0.5,1), nrow = 3, ncol = 3)
n <- 200
p <- 3
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
  datan
}

#getting the data
datan <- gen(200, 3, mu, sigma,2025)

#gradient function
gradient <- function(x, mu, sigma){
  p <- dim(sigma)[1]
  n <- dim(x)[1]
  siginv <- solve(sigma)
  sxm <- matrix(0, p, 1) # initialize the sum of xm matrix
  grad_mu <- sxm # initialize the sum for the gradient in term of mu
  C <- matrix(0, p, p) # initialize the matrix
  for(i in 1:n){
    xm <- x[i,] - mu # compute each xi - mu
    sxm <- sxm + xm # sum of xi - mu
    # now to find C = sum(xi-mu)(xi-mu)^(T)
    C <- C + xm %*% t(xm)
  }
  gradm <- siginv %*% sxm
  A <- (n * siginv) - siginv %*% C %*% siginv
  grads <- matrix(0, nrow = dim(A)[1], ncol = dim(A)[2])
  for(i in 1:dim(sigma)[1]){
    grads[i,i] <- -(1/2) * A[i,i]
  }
  
  for(i in 1:dim(sigma)[1]-1){
    for (j in (i+1):dim(sigma)[2]){
      grads[i,j] <- -1 * A[i,j]
      grads[j,i] <- grads[i,j]
    }
  }
  grad_norm <- norm(mu_sig_vec(gradm,grads), type = '2')
  list(gradm = gradm, grads = grads, grad_norm = grad_norm)
}

#function to create the hessian

hessian <- function(x, mu, sigma) {
  n <- dim(x)[1]
  p <- dim(x)[2]
  siginv <- solve(sigma)
  mu_hess <- -n * siginv # second derivative of dmu,dmu
  
  # initialize matrix for hessian of dsig,dsig
  sig_hess <- matrix(0, nrow = p * (p + 1) / 2, ncol = p * (p + 1) / 2) 
  
  # initialize C matrix, calculate C(mu)
  C <- matrix(0, nrow = p, ncol = p)
  sxm <- matrix(0, p, 1)  # initialize sxm
  for(i in 1:n) {
    xm <- x[i,] - mu  # compute each xi - mu
    sxm <- sxm + xm  # sum of xi - mu
    # now to find C = sum(xi-mu)(xi-mu)^(T)
    C <- C + xm %*% t(xm)
  }
  Z <- (-1/2)*((-n*diag(p)+2* siginv %*% C) %*% siginv)
  
  # calculating the hessian matrix
  c_count <- 0
  r_count <- 0
  for(i in 1:p) {
    for(j in 1:i) {
      r_count <- r_count + 1
      c_count <- 0
      for(k in 1:p) {
        for(l in 1:k) {
          c_count <- c_count + 1
          if(i == j && k == l) {
            sig_hess[r_count, c_count] <- Z[k,i] * siginv[i,k]
          } else if(i != j && k != l) {
            sig_hess[r_count, c_count] <- Z[k,i] * siginv[j,l] + Z[l,j] * siginv[i,k] + Z[k,j] * siginv[i,l] + Z[l,i] * siginv[j,k]
          } else if(i != j && k == l) {
            sig_hess[r_count, c_count] <- Z[k,i] * siginv[j,k] + Z[k,j] * siginv[i,k]
          } else if(i == j && k != l) {
            sig_hess[r_count, c_count] <- Z[l,i] * siginv[i,k] + Z[k,i] * siginv[i,l]
          }
        }
      }
    }
  }
  
  # dmu,dsigma
  mu_sig_hess <- matrix(0, nrow = p, ncol = p * (p + 1) / 2)
  sxm2 <- matrix(0,p,1)
  for(i in 1:n){
    xm = x[i,] - mu
    sxm2 = sxm2 + xm
  }
  D <- -siginv %*% sxm2
  r_count <- 0
  c_count <- 0
  for(i in 1:p){
    r_count = r_count +1
    c_count = 0
  for(k in 1:p) {
    for(l in 1:k) {
      c_count <- c_count + 1
      if(k == l) {
        mu_sig_hess[r_count, c_count] <- D[l,] * siginv[i,k]
      } else if(k != l) {
        mu_sig_hess[r_count, c_count] <- D[l,] * siginv[k,i] + D[k,] * siginv[l,i]
      }
    }
  }
  }
  hessian <- cbind(rbind(mu_hess, t(mu_sig_hess)), rbind(mu_sig_hess, sig_hess))
  hessian <- 0.5 * (hessian + t(hessian))
  return(hessian)
}

test_hess <- hessian(datan, mu_inital, sigma_inital)

#mu and sigma to teta
mu_sig_vec <- function(mu, sigma) {
  p <- length(mu)
  theta <- matrix(0, nrow = p + p*(p+1)/2, ncol = 1)
  theta[1:p] <- mu
  for (i in 1:p) {
    for (j in 1:i) {
      p = p+1
      theta[p] <- sigma[i, j]
    }
  }
  theta
}

# teta to mu and sigma
teta_to_mu_sigma <- function(p, theta) {
  mu <- theta[1:p]
  sigma <- matrix(0, nrow = p, ncol = p)
  for (i in 1:p) {
    for (j in 1:i) {
      p <- p+1
      sigma[i, j] <- theta[p]
      sigma[j, i] <- sigma[i,j]  # symmetric
    }
  }
  list(mu = mu, sigma = sigma)
}

#creating the multivariate normal distribution liklihood function 
likemvn <- function(x, mu, sigma){
  a = dim(x)
  n = a[1]
  p = a[2]
  sig_inv <- solve(sigma)
  C = matrix(0,p,p); # initializing sum of (xi-mu)(xi-mu)^T (pxp matrix)
  sxm = matrix(0,p,1) # initializing sum of xi-mu (px1 vector)
  for (i in 1:n){
    xm = x[i,] - mu
    sxm = sxm + xm
    C = C + xm %*% t(xm)
  }
  log_det_sig <- log(det(sigma))
  l = -(n*p*log(2*pi)+n*log_det_sig + sum(sig_inv * C ))/2
  l
}

#Newton Method Function
newton <- function(x, mu, sigma, maxit, tolerr, tolgrad){
  header = paste0("Iteration", "      halving", "     log-likelihood","      ||Gradient||")
  print(header)
  
  for (it in 1:maxit) {
    theta <- mu_sig_vec(mu, sigma) #first theta
    l_initial <- likemvn(x, mu, sigma) #first step
    
    #calculate gradient
    grad_mu <- gradient(x, mu, sigma)$gradm
    grad_sig <- gradient(x,mu, sigma)$grads
    norm_grad <- gradient(x,mu,sigma)$grad_norm
    
    #calculate hessian
    hess <- hessian(x,mu, sigma)
    hess_inv <- solve(hess)
    
    dir_n <- -1*(hess_inv%*% mu_sig_vec(grad_mu, grad_sig)) #direction 
    #print out iteration
    print(sprintf('%2.0f                 --          %3.4f               %.1e', 
                    it,  l_initial, norm_grad))
      

    
    #calculate new theta, mu, sigma
    theta_n <- theta + dir_n
    mu1 <- teta_to_mu_sigma(length(mu), theta_n)$mu
    sigma1 <- teta_to_mu_sigma(length(mu), theta_n)$sigma
    
    #feasible conditions
    pos_definite <- all(eigen(sigma1)$values > 0)
    
    #calculate new norm grad
    norm_grad_new <- gradient(x, mu1, sigma1)$grad_norm
    
    #if pos_definite is negative, shows as NA
    if(pos_definite){l_new <- likemvn(x,mu1,sigma1)}
    else{l_new <- -Inf}
    
    halve <- 0
    print(sprintf('%2.0f                 %2.0f          %3.4f               %.1e', 
                    it,  halve, l_new, norm_grad_new))
    
    #newton loop
    while((pos_definite == FALSE & halve < 20) || l_new < l_initial){
      halve = halve + 1
      theta_n <- theta + dir_n/(2^halve) 
      
      #new mu and sigma for our iteration
      mu1 <- teta_to_mu_sigma(length(mu), theta_n)$mu
      sigma1 <- teta_to_mu_sigma(length(mu), theta_n)$sigma
      pos_definite <- all(eigen(sigma1)$values >0)
      
      #if not positive definite , show as NA
      if(pos_definite){l_new <- likemvn(x,mu1, sigma1)}
      
      else{l_new <- -Inf}
      
      #get the new norm grad
      norm_grad_new <- gradient(x, mu1, sigma1)$grad_norm
      
      #print results
        print(sprintf('%2.0f                 %2.0f          %3.4f               %.1e', 
                      it,  halve, l_new, norm_grad_new))
    }
      print("-----------------------------------------------------------------")
      print(header) 

    theta <- theta_n
    
    #stopping conditions
    mod_rel_error = max(abs(theta - theta_n)/abs(pmax(1,abs(theta))))
    if (mod_rel_error < tolerr & norm_grad_new < tolgrad) {
      break}
    mu <- mu1
    sigma <- sigma1
  }
  return(list("estimator of mu"=mu, "estimator of sigma" = sigma, "iteration" = it))
    
  }

newton(datan, mu_inital, sigma_inital,500, 1e-7, 1e-7)

fisher <- function(x, mu, sigma){
  p <- dim(sig)[1]
  n <- dim(x)[1]
  sig_inv <- solve(sigma)
  
  #dmu/dmu
  dmu_dmu_matrix <- matrix(0,nrow = p, ncol = p)
  for(i in 1:){
    for(j in 1:i){
      dmu_dmu_matrix[i,j] <- -n*sig_inv[i,j]
    }
      
  }
  
  
}


