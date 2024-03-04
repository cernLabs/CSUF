
model <- function(alpha,lam,t){
  E = exp(-lam*t)
  f = alpha*E
  grad = cbind(E,-alpha*t*E)
  attr(f,'gradient') <- grad
  f
}

data = read.table('Example_2_4_data.txt', header=TRUE)
attach(data)

plot(Time, Concentration)
Result = nls(Concentration ~ model(alpha,lam,Time),
             start=list(alpha=100,lam=0.05),trace = TRUE,
             nls.control(maxiter = 50, tol = 1e-5, minFactor = 1/1024,
                         printEval=TRUE))
summary(Result)

t = seq(0,50,.1)
lines(t,211.9203*exp(-0.2357*t),col='red')