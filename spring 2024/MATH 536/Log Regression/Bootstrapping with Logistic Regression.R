###Bootstrapping in classification
###An example using logistic regression

data = read.csv("Log Reg ex.csv",h=T)

#Step six starts here.

BS.b1 = rep(0,1000)
BS.pstar = rep(0,1000)

#Step 1, create orig model

orig.model = glm(y~x1+x2,family="binomial",data=data)

for(j in 1:1000){


	#Step 2, resample the x's with replacement

	n = length(data$x1)
	row.index= sample(1:n,n,replace=T)
	BS.x = data.frame(x1 = data$x1[row.index],x2 = data$x2[row.index])

	#Step 3, Plug BS.x into orig model to get BS.phat

	BS.phat = predict(orig.model,newdata=BS.x,type="response")

	#Step 4, Sample from (1,0) to get BS.y

	BS.y = rep(0,n)
	for(i in 1:n){
		BS.y[i] = sample(c(1,0),1,prob=c(BS.phat[i],1-BS.phat[i]))
	}
	BS.data = data.frame(y = BS.y, x1 = BS.x$x1,x2 = BS.x$x2)

	#Step 5, New model using BS.x and BS.y.  Compute BS.par

	newmodel = glm(y~x1+x2,data=BS.data,family='binomial')
	BS.b1[j] = coef(newmodel)[2]
	BS.pstar[j] = predict(newmodel,newdata=data.frame(x1=2,x2=2),type="response")

	#Step 6, repeat.
}

mu = mean(BS.b1)
s = sd(BS.b1)
hist(BS.b1,breaks=50)
x1 = seq(min(BS.b1),max(BS.b1),by=.0001)
y1 = dnorm(x1,mu,s)
lines(x1,y1,col=3,lwd=2,lty=2)
qqnorm(BS.b1)
lowbound = sort(BS.pstar)[25]
highbound = sort(BS.pstar)[975]


