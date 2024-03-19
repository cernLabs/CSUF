# Generate data from three normal with means mu1, mu2, mu3 and common variance 1
# with proportions p1, p2 and p3 and see if the EM algorithm can recover it
n=200
p1=0.1; p2=0.3; p3=0.6
mu1 = 0; mu2=2; mu3=5
z= rnorm(n)
y = matrix(0,n,1)
n1 = floor(n*p1) 
n2 = floor(n*p2) 
n3 = floor(n*p3) 
y[1:n1]=rnorm(n1,mu1) 
y[(n1+1):(n1+n2)]=rnorm(n2,mu2)
y[(n1+n2+1):n]=rnorm(n3,mu3)
hist(y,breaks=20,freq=FALSE)
lines(density(y),col='red')

# start with proportions 1/3, 1/3, 1/3
p = c(1/3,1/3,1/3)
for(it in 1:20){
  f1 = dnorm(y,mean=mu1,sd=1)
  f2 = dnorm(y,mean=mu2,sd=1)
  f3 = dnorm(y,mean=mu3,sd=1)
  N1 = f1*p[1]
  N2 = f2*p[2]
  N3 = f3*p[3]
  D = N1+N2+N3
  Post1 = N1/D # Posterior probability of belonging to group 1
  Post2 = N2/D # Posterior probability of belonging to group 2
  Post3 = N3/D # Posterior probability of belonging to group 3
  Post = cbind(Post1,Post2,Post3)
  p=apply(Post,2,sum)/n
  print(c(it, p))
}
class = numeric(n)
for(i in 1:n){
  class[i] = which(Post[i,] == max(Post[i,]))
}
plot(1:n1,class[1:n1],col='red',ylim=c(0,4),xlim=c(0,n))
points((n1+1):(n1+n2),class[(n1+1):(n1+n2)],col='blue')
points((n1+n2+1):n,class[(n1+n2+1):n],col='green')
abline(v=n1,col = "red")
abline(v=n1+n2,col = "blue")

correct_classification <- (sum(class[1:n1] == 1) + sum(class[(n1+1):(n1+n2)] == 2) + sum(class[(n1+n2+1):n] == 3))/n
print(correct_classification)

False_classification <- (sum(class[1:n1] != 1) + sum(class[(n1+1):(n1+n2)] != 2) + sum(class[(n1+n2+1):n] != 3))/n
print(False_classification)





