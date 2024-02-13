# Example 2.2 of the notes: Multinomial example
# -- Fisher-scoring algorithm

# This function computes the gradient of the log-likelihood
grad <- function (x, teta) {
  #x=(x1,x2,x3,x4), and teta os the parameter (teta)
  dl = x[1]/(2+teta)-(x[2]+x[3])/(1-teta)+x[4]/teta
}

# This function computes the Fisher information of the log-likelihood

Fish <- function (x, teta) {
  n =  sum(x)
  I = n*(1/(4*(teta+2))+1/(2*(1-teta))+1/(2*teta))
}  

# Fisher-scoring, Linear ratio of convergence; added convergence criteria

Fisher.score <- function (maxit, x, teta, tolerr, tolgrad) {
  tstar = -1657/7680+sqrt(3728689)/7680
  digits = -log10(abs(teta-tstar)/tstar)
  it=0
  header = c("iteration      teta         Ratio      Digits    Rel Error    Gradient")
  print(header, quote = FALSE)
  print(sprintf('%2.0f         %12.12f      --        %2.1f      --           --',0,teta,digits), quote = FALSE)
  stop=0
  while (it<maxit & stop==0){
    it = it+1
    dl = grad(x,teta)
    I = Fish(x,teta)
    tnew = teta + dl/I
    ratio = abs((tnew-tstar)/(teta-tstar))
    digits = -log10(abs(tnew-tstar)/tstar)
    relerr = abs(teta-tnew)/max(1,abs(tnew))
    print(sprintf('%2.0f         %12.12f    %4.4f       %2.1f    %3.2e    %3.2e',it,tnew, ratio,digits, relerr, abs(dl)), quote = FALSE)
    if (relerr<tolerr && abs(dl)< tolgrad) stop=1
    teta = tnew # Update and return
  }
}


teta=0.01;  maxit =30;  x = c(1997,907,904,32); Fisher.score(maxit, x, teta,1e-6, 1e-9) 
#teta=0.5;  maxit =30;  x = c(1997,907,904,32); Fisher.score(maxit, x, teta,1e-6, 1e-9)  

