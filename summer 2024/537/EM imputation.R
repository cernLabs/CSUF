
data = read.csv("imp ex.csv",h=T)

#Step 1, imputation initialization.

#For each row of data I want to identify any missing values
#then fill them with the current column average. (LAZY AND WRONG, but who cares for init)

n = dim(data)[1]
mu.vec = c(mean(na.omit(data$x1)),mean(na.omit(data$x2)),mean(na.omit(data$x3)))
sig.vec = diag(1,3)

#Dont care about sig.vec for the initialization step.
#don't want to overwrite my original data, so we duplicate it.

data1 = data

for(j in 1:n){
	data1[j,][is.na(data1[j,])] = mu.vec[is.na(data1[j,])]
}

data
data1

#Step 2, maximization initilization.
#Again, doesn't really matter which values we pick as long as their reasonable
#our Sig.vec will have too little variance because we've replaced 
#some values with average values, will converge back to what it should later.

#Note I could just take the mean and covariance of data1
#I'm choosing to break up the steps to make replication of step 2 easier.

#mu.tilda and sig.tilda are MLE estimates with the current imputed data

data.sum = c(0,0,0)
for(j in 1:n){
	data.sum = data.sum + data1[j,]
}
mu.tilda = data.sum/n  


x2.sum = matrix(0,ncol=3,nrow=3)
for(j in 1:n){
	x2.sum = x2.sum + t(as.matrix(data1[j,] - mu.tilda))%*%(as.matrix(data1[j,] - mu.tilda))
}
sig.tilda = x2.sum/n


mu.tilda
sig.tilda

#Step 3, Estimation not initilization.  I'm going to impute four things at each
#Step (when necessary).  
#1. missing values in data (will overwrite data1)
#2. cross products between missing values.
#3. cross products between missing values and known values
#4. The matrix x%*%t(x) 

#Going to store part 1 just in the data1 data frame
#Going to store part 4 in a list.  Note 2. and 3. don't need to be
#stored as they only contribute to 4.

xtx = list()
data1 = data

#3.1

#Note I'm replacing missing values with mu.tilda now.

for(j in 1:n){
	if(sum(is.na(data[j,])) > 0){
		data1[j,1] = mu.tilda[1] + sig.tilda[1,2:3]%*%solve(sig.tilda[2:3,2:3])%*%t(as.matrix(data[j,2:3] - mu.tilda[2:3]))
	}
}

#3.4


for(j in 1:n){
	if(sum(is.na(data[j,])) > 0){
		x1x1.imp = sig.tilda[1,1] - sig.tilda[1,2:3]%*%solve(sig.tilda[2:3,2:3])%*%sig.tilda[2:3,1] + data1[j,1]^2
		x1x2.imp = data1[j,1]*as.matrix(data[j,2:3])
		x2x2 = t(as.matrix(data[j,2:3]))%*%as.matrix(data[j,2:3])
		temp = matrix(0,3,3)
		temp[1,1] = as.matrix(x1x1.imp)
		temp[1,2:3] = x1x2.imp
		temp[2:3,1] = x1x2.imp
		temp[2:3,2:3] = x2x2
		xtx[[j]] = temp
	} else{
		xtx[[j]] = t(as.matrix(data[j,]))%*%as.matrix(data[j,])
	}	
}

#Step 4, Update mu.tilda and sig.tilda

data.sum = c(0,0,0)
for(j in 1:n){
	data.sum = data.sum + data1[j,]
}
mu.tilda = data.sum/n

x2.sum = matrix(0,ncol=3,nrow=3)
for(j in 1:n){
	x2.sum = x2.sum+xtx[[j]]
}
sig.tilda =  x2.sum/n - t(as.matrix(mu.tilda))%*%(as.matrix(mu.tilda)) 


mu.tilda
sig.tilda

#Just copy and paste steps 2 and 3 a few times for visualization.

data1
mu.tilda
sig.tilda


