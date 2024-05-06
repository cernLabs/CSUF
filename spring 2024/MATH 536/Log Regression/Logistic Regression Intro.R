#Setwd and read in data

#setwd("C:/Users/knichols/Desktop")
data = read.csv("Log Reg ex.csv",h=T)

#Set up our X and Y matrices the way they are supposed to be.

X = matrix(c(rep(1,length(data$x1)),data$x1,data$x2),ncol=3)
Y = matrix(data$y,ncol=1)

#After we initialize a Beta hat vector, we want to update.
#I'm going to represent all of the B^ values in a matrix (I'll iterate 100 time).
#So...first column of B.hat will be my initial values for B.hat
#Second value will be updates after one iteration of Newton Raphson.

B.hat = matrix(0,nrow=3,ncol=100)

#Initial Values, I'm just going to pick B.hat0 = 1, B.hat1 = .2, B.hat2 = .2.

B.hat[,1] = c(0,0,0)

#Now we want to loop to update our coefficients.  
#Each iteration I will need to compute p.hat for the current B.hat column
#Then I'll need dL(B^) and dL2(B^) (first and second derivatives of the log likilihood
#Then I can use the Newton Raphson equation to update.

for(i in 1:99){

	#p.hat = e(XB)/(1 + e(XB))
	p.hat = exp(X%*%B.hat[,i])/(1+exp(X%*%B.hat[,i]))
	#DLL1 is first derivative evaluated at B^, t(X)Y - t(X)p.hat
	DLL1 = t(X)%*%Y - t(X)%*%p.hat
	#DLL2 is second derivative evaluated at B^, -t(X)p.hat(1-p.hat)X
	#Note diag(c(p.hat*(1-p.hat))) creates an nxn matrix whose diag elements
	#Are p.hat_j*(1-p.hat_j)
	DLL2 = -t(X)%*%diag(c(p.hat*(1-p.hat)))%*%X
	#Now we update B.hat[,i+1] with Newton Raphson
	B.hat[,i+1] = B.hat[,i] - solve(DLL2)%*%DLL1
  }


###Built in R function for fitting a logistic regression model

model = glm(y~x1+x2,family="binomial")
log.odds = predict.glm(model,newdata=data.frame(x1=3,x2=6))
exp(log.odds)/(1+exp(log.odds))


