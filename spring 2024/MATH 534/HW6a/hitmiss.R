hitmiss <- function (n, a, b) {
  s = rnorm(n)
  tetah = sum(s>a & s<b)/n # this is the hit-miss approximation to the integral
  tetah_se = sqrt((tetah-tetah^2)/n) #This is the standard error of the approximation
  exact=pnorm(b)-pnorm(a) #This is the exact value
  list(exact=exact, tetah=tetah,tetah_se=tetah_se)}

# Check standard error and normality
n = 100
a=0; b=2
I = hitmiss(n,a,b)
output <- data.frame(Exact = I$exact, M_C_approx = I$tetah, S.E. = I$tetah_se)
print(output)# Checking convergence

# Check standard error and normality
rep=200
Ivec = numeric(rep)
for (i in 1:200){
  I = hitmiss(n,a,b)
  Ivec[i]= I$tetah
}
qqnorm(Ivec)
sd(Ivec)

# true variance /standard error of HM estimator
pab = pnorm(b)-pnorm(a)
v = (pab-pab^2)/n
se_hm = sqrt(v)
print(se_hm)
