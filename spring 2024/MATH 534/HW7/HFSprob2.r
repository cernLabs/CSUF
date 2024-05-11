# problem 2

data <- read.table("breastcancer.dat", header =TRUE)

gibs2 <- function(n, theta){
  
  #initialize my gibs sample matrix
  gibs_sample <- matrix(0, nrow = n, ncol = 2)
  a <- 3
  b <- 1
  c <- 60
  d <- 120
  
  #treatment and reacurrence
  t_c <-sum(data$treatment == 1 & data$censored == 0)
  
  #contorl and not censored
  c_c <- sum(data$treatment == 0 & data$censored == 0) 
  
  
  #recurtime when group has treatment
  t_r <- sum(data$recurtime[data$treatment == 1])
  
  #recurtime when group is in control
  c_r <- sum(data$recurtime[data$treatment == 0]) 
  
  for(i in 1:n){
    current_theta <- theta
    
    #calculate the mean to generate for new y, from the conditional of y|x=x
    tau_alpha <- b + t_c + 1
    tau_beta <- current_theta *(t_r + d)
    
    #my conditional for x and y are normals
    new_tau <- rgamma(1, shape = tau_alpha, rate =  tau_beta)
    
    #calculate the mean to generate for new y, from the conditional of y|x=x
    theta_alpha <- a + c_c + t_c +1
    theta_beta <- c_r + new_tau*t_r + c+d*new_tau
    
    #calculate my new y
    new_theta <- rgamma(1,shape = theta_alpha , rate = theta_beta)
    
    #input my new x,y into a vector
    z <- c(new_theta, new_tau)
    
    #input the vector into my row of my gibs_sample matrix
    gibs_sample[i,] <- z
    
    #replace my new_x as my x
    theta <- new_theta
  }
  gibs_sample
}

prob_2 <- gibs2(5000,1)

hist(prob_2[,1], main = "theta")

hist(prob_2[,2], main = "Tau")

plot(prob_2[,1], prob_2[,2])