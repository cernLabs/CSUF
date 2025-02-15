
##  PART A

# define the algebraic function we will be using in our secant method
f.of.theta <- function(theta){
  7988/(2+theta) - 7244/(1-theta) + 128/theta
}

# define a function that performs one iterations of the secant method
secant_method <- function(x0,x1){
  x2 = x1 - f.of.theta(x1)*(x1-x0)/(f.of.theta(x1) - f.of.theta(x0))
  return(x2)
}

# now will will do what Mori asked us to do and make the actual algorithm
algorithm1 <- function(theta.0 =.02, theta.1 =.01, maxit = 20, tolerr = 1e-6, tolgrad = 1e-9){
  
  #let set some intial values
  
  #we will make the first error
  error = abs(theta.1- theta.0) / max(1,abs(theta.1)) 
  # we will initialize the iteration value at zero
  iteration = 0
  
  #print the header
  
  #let's start the looping part
  while(error >= tolerr && iteration <= maxit ){
    
    # employ the secant method
    theta.2 <- secant_method(theta.0,theta.1)
    
    # resetting
    theta.0 <- theta.1
    theta.1 <- theta.2
    
    # recalculate the error with the new thetas
    error = abs(theta.1- theta.0) / max(1,abs(theta.1)) 
    
    # add iteration
    iteration = iteration + 1
    
    # print the outputs
    print(sprintf("Iteration: %d, Theta: %0.12f, MRE: %2.1e",iteration,theta.1,error))
  }
  
}

# this tell the computer to run the algorithm
algorithm1()

