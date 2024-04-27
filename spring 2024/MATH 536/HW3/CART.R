############Classification and Regression Trees
#####Overview of Classification Trees
#There are a lot of packages and libraries that deal with tree nested logic.
#By far the most reputable for basic trees is rpart
#As we get to forest techniques adabag and randomForest are helpful libraries as well
#The ROCR library is helpful for ROC curves and is necessary for some of the other libraries to run
#gbm stands for generalized boosting models, it's helpful for boosting in a regression setting.

library(rpart)
library(adabag)
library(randomForest)
library(ROCR)
library(gbm)

#The following code is just designed to provide us with some data for plotting to contextualize the conversation.  It is not necessary for fitting CART models.
#After creating three predictors x1,x2 and x3 and a response y (categorical) we save the data.frame as example.

x1 = rnorm(100,15,2)
x2 = rnorm(100,25,5)
x3 = sample(c(0,1),100,replace=T)

y = x1/90+x2/150+x3*.166666
y1 = round(y)
y1[sample(1:100,20,replace=F)]=1
y=y1
y = as.character(y)
y[y=="0"]="fail"
y[y=="1"]="success"

example = data.frame(y=as.factor(y),x1=x1,x2=x2,x3=as.factor(x3))
example$col = rep(2,length(example$y))
example$col[example$y=="success"]=3

par(mfrow=c(2,1))

#Just some fancy plots to show you whats going on with the data...similar to the example with sorting our class by gender.

plot(example$x1,example$x2,pch="",main="x3==0",xlab="x1",ylab="x2")
text(example$x1[example$x3=="0"],example$x2[example$x3=="0"],example$y[example$x3=="0"],col=example$col[example$x3=="0"],cex=.5)


plot(example$x1,example$x2,pch="",main="x3==1",,xlab="x1",ylab="x2")
text(example$x1[example$x3=="1"],example$x2[example$x3=="1"],example$y[example$x3=="1"],col=example$col[example$x3=="1"],cex=.5)

#####End of overview of CART

#####Classification Trees

#The syntax for creating a classification tree is as follows:
#rpart is a modeling function, much like lm().  It takes additional arguments method and contral.
#method takes on four arguments, two of which are relevant for us.
#method = "class" means we want a classification tree (i.e. categorical response).
#method = "anova" means we want a regression tree (i.e. continuous response).
#control must be set to the output of a function called rpart.control.
#rpart.control is where we control parameters minsplit and cp.
#Check it out for our example data.

model.tree1 = rpart(y~x1+x2+x3,method="class",control=rpart.control(minsplit=1,cp=.0000001),data=example)
names(model.tree1)

#We can plot our tree using the following code
dev.off()
plot(model.tree1)
text(model.tree1,cex=.65)

#If we want to see how our tree did with prediction we need to ask it to make predictions for the original data.  predict takes three arguments for an rpart() model.
#1.  The model
#2.  Data with just the predictors over which you want to make a prediction
#3.  The type of prediction we want to make (class is for classification).

prediction.tree1 = as.character(predict(model.tree1,example[,2:4],type="class"))
prediction.tree1

#table is a fun function for creating contingency tables.

table(prediction.tree1,example$y)

#Wow...perfect classification...hmmm.
#What if we make the same model but this time evaluate using cross validation.  We'll do 10 fold cross validation.

n = length(y)
train.n = floor(n/9)
test.n = n - train.n
test.index.matrix = matrix(sample(1:n,n,replace=F),nrow=10)

#test.index.matrix has partitioned the indeces of our original data into 10 partitions, each rpresented by a row in the matrix.
overall.prediction = c()
overall.y = c()
for(i in 1:10){
	train.example = example[c(test.index.matrix[-i,]),]
	test.example = example[c(test.index.matrix[i,]),]
	model.tree = rpart(y~x1+x2+x3,method="class",control=rpart.control(minsplit=5,cp=.001),data=train.example)
	prediction.tree = as.character(predict(model.tree,test.example[,2:4],type="class"))
	print(table(prediction.tree,test.example$y))
    overall.prediction = c(overall.prediction,prediction.tree)
    overall.y = c(overall.y,test.example$y)
}
table(overall.prediction,overall.y)

#A very different result, obviously we've overfit the data.

model.tree = rpart(y~x1+x2+x3,method="class",control=rpart.control(minsplit=10,cp=.05),data=example)
dev.off()
plot(model.tree)
text(model.tree,cex=.65)
prediction.tree = as.character(predict(model.tree,example[,2:4],type="class"))
table(prediction.tree,example$y)

#####End of classification trees

#####Regression trees

#Lets look at the built in airquality dataset for R.
data = na.omit(airquality)
model.tree = rpart(Ozone~Temp+Wind+Solar.R,method="anova",control=rpart.control(minsplit=10,cp=.02),data=data)
dev.off()
plot(model.tree)
text(model.tree,cex=.65)

#Again compute residuals by predicting using the original data and comparing to actual responses.
prediction.tree = (predict(model.tree,data[,2:4],type="matrix"))

#Manually compute r^2 based on residuals.
SSR = sum((data$Ozone - prediction.tree)^2)
SST = var(data$Ozone)*(length(data$Ozone)-1)
r2 = (SST - SSR)/SST
r2

#compare to a simple linear model.
model = lm(Ozone~Temp+Wind+Solar.R,data=data)
summary(model)

#The tree seems to do much better but we're still probably over-fitting.
#Cross Validation is our friend
#####End of Regression Trees
