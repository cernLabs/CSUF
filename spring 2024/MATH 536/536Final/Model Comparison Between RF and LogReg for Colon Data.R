###Set working directory, read in data, fix race variable

setwd("C:/Users/knichols/Downloads")
data = read.csv("colon2017.csv",h=T)
unique(data$Race)
data$Race[data$Race %in% c("white","White ","W")] = "White"
data$Anastamotic.Leak = as.factor(data$Anastamotic.Leak)

###Load in packages needed

library(rpart)
library(adabag)
library(randomForest)
library(ROCR)
library(gbm)

###Set up basic model.a (log reg) and model.b (random forest)

model.a = glm(Anastamotic.Leak~Gender+
							BMI+
							Age+
							Race+
							Tobacco+
							DM+
							CAD.PAD+
							Cancer+
							Albumin..g.dL.+
							Operative.Length,
							data=data,family='binomial')

model.b = randomForest(Anastamotic.Leak~Gender+
							BMI+
							Age+
							Race+
							Tobacco+
							DM+
							CAD.PAD+
							Cancer+
							Albumin..g.dL.+
							Operative.Length,
							data=data,
							ntree=500,
							mtry=3,
							control=rpart.control(minsplit=10,cp=.02))

#note, just picked whatever for tuning parameters for now.  Will refine later.

###Next I want to see if I can estimate probabilities for the observed values in our data,
#compute a log likelihood for each model and then compare them.

y = as.numeric(data$Anastamotic.Leak) - 1
p.a = predict.glm(model.a,newdata=data,type="response")
p.b = predict(model.b,newdata=data,type="prob")[,2]
#Probably don't want rounded probabilities to exactly 0 or 1
p.b[p.b==0] = 0.00001
p.b[p.b==1] = .9999

LL.a = sum(y*log(p.a) + (1-y)*log(1-p.a))
LL.b = sum(y*log(p.b) + (1-y)*log(1-p.b))
LL.a
LL.b

###model b wins by a mile, but I'm a little worried about overfitting.  Time to cross validate)
###Going to 10 fold cross validate

n = dim(data)[1]
n

###with 179 data points, going to have 17 data points in each testing set
###Obviously that means there are 9 data points that never get partitioned
###as testing data.  But we can pick those up as their own testing group at the end if we want to.

CV.ind.mat = matrix(sample(1:n,170,replace=F),nrow=10)

#Run same code as before, but each time, we're going to run our models using only train data
#and testing them using only testing data.

CVLL.a = 0
CVLL.b = 0

for(k in 1:10){
	train.data = data[(1:n)[-CV.ind.mat[k,]],]
	test.data = data[CV.ind.mat[k,],]
	
	model.a = glm(Anastamotic.Leak~Gender+
							BMI+
							Age+
							Race+
							Tobacco+
							DM+
							CAD.PAD+
							Cancer+
							Albumin..g.dL.+
							Operative.Length,
							data=train.data,family='binomial')

	model.b = randomForest(Anastamotic.Leak~Gender+
							BMI+
							Age+
							Race+
							Tobacco+
							DM+
							CAD.PAD+
							Cancer+
							Albumin..g.dL.+
							Operative.Length,
							data=train.data,
							ntree=500,
							mtry=4,
							control=rpart.control(minsplit=3,cp=.01))


	y = (as.numeric(data$Anastamotic.Leak) - 1)[CV.ind.mat[k,]]
	p.a = predict.glm(model.a,newdata=test.data,type="response")
	p.b = predict(model.b,newdata=test.data,type="prob")[,2]
	#Probably don't want rounded probabilities to exactly 0 or 1
	p.b[p.b==0] = 0.00001
	p.b[p.b==1] = .9999

	LL.a1 = sum(y*log(p.a) + (1-y)*log(1-p.a))
	LL.b1 = sum(y*log(p.b) + (1-y)*log(1-p.b))
	CVLL.a = CVLL.a + LL.a1
	CVLL.b = CVLL.b + LL.b1



}

#for funsies I'm going to recover the last 9 observations that were left out and have them be their own test group

	train.data = data[(1:n)[CV.ind.mat],]
	test.data = data[(1:n)[-CV.ind.mat],]
	
	model.a = glm(Anastamotic.Leak~Gender+
							BMI+
							Age+
							Race+
							Tobacco+
							DM+
							CAD.PAD+
							Cancer+
							Albumin..g.dL.+
							Operative.Length,
							data=train.data,family='binomial')

	model.b = randomForest(Anastamotic.Leak~Gender+
							BMI+
							Age+
							Race+
							Tobacco+
							DM+
							CAD.PAD+
							Cancer+
							Albumin..g.dL.+
							Operative.Length,
							data=train.data,
							ntree=500,
							mtry=4,
							control=rpart.control(minsplit=3,cp=.01))


	y = (as.numeric(data$Anastamotic.Leak) - 1)[(1:n)[-CV.ind.mat]]
	p.a = predict.glm(model.a,newdata=test.data,type="response")
	p.b = predict(model.b,newdata=test.data,type="prob")[,2]
	#Probably don't want rounded probabilities to exactly 0 or 1
	p.b[p.b==0] = 0.00001
	p.b[p.b==1] = .9999

	LL.a1 = sum(y*log(p.a) + (1-y)*log(1-p.a))
	LL.b1 = sum(y*log(p.b) + (1-y)*log(1-p.b))
	CVLL.a = CVLL.a + LL.a1
	CVLL.b = CVLL.b + LL.b1

CVLL.a
CVLL.b

#What about some residual diagnostic plots?
#Obviously need to tune my random forest model still.

