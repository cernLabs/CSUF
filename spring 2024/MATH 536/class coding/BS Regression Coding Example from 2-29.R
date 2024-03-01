Bootstrapping protocol for regression

#Take your hw2 dataset.  remove point 257 (data = data[-257,])
#Let x = spend
#Let y = revenue
#Using bootstrapping, create a confidence interval for B1.  Compare to result in HW2
#Using bootstrapping, create a conditional prediction interval for y|x=500000, compare to HW2

#Read in data, change working directories, ect.

setwd("C:/Users/knichols/Downloads")
data = read.csv("hw2.csv",h=T)
data = data[-257,]

#Step 1 Fit Original Model, store orig res

model = lm(Revenue~Spend,data=data)
orig.res = model$res
n = length(model$res)


#Step 2 Generate BS.x

BS.x = sample(data$Spend,n,replace=T)

#Step 3 Generate BS.yhat using original model (plugging in BS.x to orig model)

BS.yhat = predict(model,newdata=data.frame(Spend=BS.x))

#Step 4 Generate BS.y by adding n random residuals (samp w rep) to BS.yhat)

BS.y = BS.yhat + sample(orig.res,n,replace=T)


#Step 5 Generate Model using BS.x, BS.y.  
#Store desired output (Bhat1, Bhat0, y.hat.star)

BS.model = lm(BS.y~BS.x)
BS.B1 = BS.model$coef[2]
BS.yhat.500k = predict(BS.model,newdata=data.frame(BS.x=500000))

#Step 6 Repeat 10,000 times 

BS.B1 = rep(0,10000)
BS.yhat.500k = rep(0,10000)
BS.y.500k = rep(0,10000)

for(i in 1:10000){
	BS.x = sample(data$Spend,n,replace=T)
	BS.yhat = predict(model,newdata=data.frame(Spend=BS.x))
	BS.y = BS.yhat + sample(orig.res,n,replace=T)
	BS.model = lm(BS.y~BS.x)
	BS.B1[i] = BS.model$coef[2]
	BS.yhat.500k[i] = predict(BS.model,newdata=data.frame(BS.x=500000))
	BS.y.500k[i] = BS.yhat.500k[i] + sample(orig.res,1)
}

#Step 7 Empirical distribution, conf interval, pval.

#distributions

hist(BS.B1,breaks=20)
hist(BS.yhat.500k,breaks=20)
hist(BS.y.500k,breaks=20)

#conf intervals

quantile(BS.B1,c(.025,.975))
quantile(BS.y.500k,c(.025,.975))

#pvalue for testing whether B1 = 0

BS.B1.recentered = BS.B1 - mean(BS.B1)
orig.B1 = model$coef[2]
pval = length(BS.B1.recentered[abs(BS.B1.recentered) > abs(orig.B1)])/10000
pval


#Step 8  Chipotle.

####In class assignment.  
