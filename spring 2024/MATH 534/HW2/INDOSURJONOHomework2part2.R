#homework 1

#part a)
library(tidyverse)
library(dplyr)
x <- c(1997,907,904,32)

#function for calculating the gradient of the log-likelihood
grad <- function(x, teta) {
  dl <- x[1]/(2+teta) - (x[2] + x[3])/(1-teta) + x[4]/teta
  return(dl)
}

output <- data.frame()
#secant function
secant <- function(maxit, x ,teta_n, teta_n1, parta,partb) {
  stop <- 0
  it <- 0
  while (it < maxit & stop == 0) {
    it <- it + 1 
    #calculate the gradients of teta_n and teta_n1
    d_teta_n <- grad(x,teta_n)
    d_teta_n1 <- grad(x, teta_n1)
    
    #secant method
    teta_n2 <- teta_n - (d_teta_n*((teta_n - teta_n1)/(d_teta_n - d_teta_n1)))
    
    #stopping conditions
    totalgrad = 1e-9
    tolerr = 1e-6
    mod_rel_err <- abs(teta_n2 - teta_n1) / max(1,abs(teta_n2))
    abs_grad <- abs(grad(x, teta_n2))

  if(parta == TRUE){
    #store into dataframe
    row <- as.numeric(c(it, teta_n1,mod_rel_err, abs_grad))
    output <-rbind(output, row)
    
    #format dataframe
    output <- data.frame(output) %>% set_names("iteration", "theta", 
                                               "mod. rel. err", "val of grad") %>%
      mutate(
        theta = sprintf("%12.12f", theta),
        `mod. rel. err` = sprintf("%3.1e", `mod. rel. err`),
        `val of grad` = sprintf("%3.1e", `val of grad`)
      )
  }
  
  if(partb == TRUE){
    teta_star <- (-1657 + sqrt(3728689)) / 7680
    converg_ratio <- abs(teta_n1 - teta_star) / (abs(teta_n - teta_star)^((1+sqrt(5))/2))
    sig_digit <- -log10(abs(teta_n1 - teta_star) / (abs(teta_star)))
    
    #input the rows to my dataframe

    row <- as.numeric(c(it, teta_n1, converg_ratio, sig_digit))
    output <- rbind(output, row)
    
    #format dataframe
    output <- data.frame(output) %>% set_names("iteration", "theta", 
                                               "convergence ratio",  "significant digit") %>%
      mutate(
        theta = sprintf("%12.12f", theta),
        `convergence ratio` = sprintf("%3.3e", `convergence ratio`),
        `sigificant digit` = sprintf("%3.1e", `significant digit`)
      )
    
  }
    #stopping condition
    if(any(mod_rel_err < tolerr & abs_grad < totalgrad))
      stop <- 1
    # put teta_1 to teta_n and teta_2 to teta_1
    teta_n <- teta_n1
    teta_n1 <- teta_n2
  }
  return(output)
}

secant(20,x, .02, .01, parta = TRUE,partb = FALSE)

#part b)

secant(20, x,.02, .01, parta = FALSE, partb =TRUE)
