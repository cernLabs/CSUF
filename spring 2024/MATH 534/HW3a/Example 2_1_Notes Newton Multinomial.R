# Example 2.1 of the notes: Multinomial example
# -- Newton's Method

# This function computes the gradient of the log-likelihood
grad <- function (x, teta) {
  #x=(x1,x2,x3,x4), and teta is the parameter
  dl = x[1]/(2+teta)-(x[2]+x[3])/(1-teta)+x[4]/teta
}

# This function computes the Hessian of the log-likelihood

Hess <- function (x, teta) {
  # This function computes the Hessian of the log-likelihood
  ddl = -(x[1]/(2+teta)^2+(x[2]+x[3])/(1-teta)^2+x[4]/teta^2)
}

# This function computes the log-likelihood 
# function, up to a constant (not needed for Newton's method)

log_likelihood <- function (x, teta) {
  l = x[1]*log(2+teta)+(x[2]+x[3])*log(1-teta)+x[4]*log(teta)  
}

# plot of the likelihood finction
teta=seq(0.01,.99,0.01)
x = c(1997,907,904,32)
l = log_likelihood(x,teta)
plot(teta,l, type = "l")
# --- zoom in around 0
teta=seq(0.0001,.1,.0001)
l = log_likelihood(x,teta)
plot(teta,l,type='l')

# plot of the gradient 
teta=seq(-0.5,.5,0.01)
x = c(1997,907,904,32)
dl = grad(x,teta)
plot(teta,dl,type='l')
abline(h=0,col='red')

# Newton's Method for the multinomial

Newton <- function (maxit, x, teta) {
  for (it in 1:maxit){
    dl = grad(x,teta)
    ddl = Hess(x,teta)
    tnew = teta - dl/ddl
    print(c(it, teta))
    teta = tnew
  }
}


# Run Newton, using various starting values
maxit = 20
x = c(1997,907,904,32)
teta=0.5; Newton(maxit, x, teta) # converges to the negative solution

teta=0.01; Newton(maxit, x, teta) # converges to the positive solution 


# Showing quadratic rate of convergence, as well as outputing formatting

Newton1 <- function (maxit, x, teta) {
  tstar = -1657/7680 + sqrt(3728689)/7680
  digits = -log10(abs(teta-tstar)/abs(tstar))
  header = paste0("iteration", "      teta","         Ratio", "      Digits")
  print(header)
  print(sprintf('%2.0f         %12.12f      --        %2.1f',0,teta,digits))
  for (it in 1:maxit){
    dl = grad(x,teta)
    ddl = Hess(x,teta)
    tnew = teta - dl/ddl
    ratio = abs(tnew-tstar)/(teta-tstar)^2
    digits = -log10(abs(tnew-tstar)/abs(tstar))
    print(sprintf('%2.0f         %12.12f    %4.2f       %2.1f',it,tnew, ratio,digits))
    teta = tnew # Update and return
  }
}
teta=0.01;  maxit =10;  x = c(1997,907,904,32); Newton1(maxit, x, teta) 

