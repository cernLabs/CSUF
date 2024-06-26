# inputs

x <- as.numeric(dat[,2])
n <- length(x)
omega <- 1
lambda <- 100 

err_tol <- 0.1
sig_A_init <- 5
sig_S_init <- .5


detect_activity <- function(x, omega, lambda, err_tol, sig_A_init,sig_S_init){
  #  compute b.tilde using eq.19
  b <- rep(0,n)
  for(i in 1:n){
    b[i] <- dnorm(x[i],sd = sig_S_init) / (dnorm(x[i],sd = sig_A_init) + dnorm(x[i],sd = sig_S_init) )
  }
  # iteration count
  m = 0
  repeat{
    m <- m + 1
    sig_A <- sum(b^2* x^2)/sum(b^2)
    sig_S <- sum((1-b)^2 +  x^2)/sum((1 - b)^2)
    bm = rep(0,n)
    
    # eq 12
    bm[1] = (2*dnorm(x[1], sd = sig_S) - 2*lambda*b[2] + omega) / (2*(dnorm(x[i],sd = sig_A) + dnorm(x[i],sd = sig_S) - 2*lambda + 2*omega))
    
    # eq 13
    for(i in 2 : (n-1)){
      bm[i] <- (2*dnorm(x[i],sd = sig_S) - 2*lambda*(b[i+1] - b[i-1])+ omega) / (2*(dnorm(x[i],sd = sig_A)+ dnorm(x[i],sd = sig_S)) - 4*lambda + 2*omega)
      }
    
    # eq 14
    bm[n] <- (2*dnorm(x[n],sd = sig_S) -2*lambda*b[n-1] + omega)/(2*(dnorm(x[n],sd = sig_A)+ dnorm(x[n],sd = sig_S)) - 2*lambda + 2*omega)
    
    # update b using eq 20
    for(i in 1:n){
      if(!is.logical(bm[i])){b[i] = 0} # BANDAID
      else if(bm[i] < 0){b[i] = 0}
      else if(bm[i] >= 0 && bm[i] <= 1){b[i] = bm[i]}
      else{b[i] = 1}
    }

    if( (norm((b - bm), type  = '2') < err_tol ) || m > 1000){
      break
    }
  }
  
  # binarize b
  bin = rep(0,n)
  bin = as.numeric(b < 0.5)
  b = bin
  
  return(list("onoff" = b, "sd_active" = sig_A, "sd_silent" = sig_S))
}

testing <- detect_activity(x,omega,lambda,err_tol,sig_A_init,sig_S_init)
