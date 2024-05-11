
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
example = data.frame(y=as.factor(y),x1,x2,x3)

#####Random Forests and Adaptive Boosting
#randomForest() works a lot like rpart, but with a few extra parameters.
#ntree is the number of trees in the forest
#mtry is the number of predictors randomly selected to consider at each split, usually this is about 1/3 your total predictors or the square root of your total predictors rounded up. i.e. if I have 10 predictors each time I'd sample about 4.
#control is the same as before. It needs to be an rpart.control object.
model.rf = randomForest(y~x1+x2+x3,data=example,ntree=100,mtry=2,control=rpart.control(minsplit=10,cp=.05))
predict(model.rf,newdata=example)
predict(model.rf,newdata=example,type='prob')
model.rf$votes
model.rf$err.rate
plot(model.rf)


prediction.rf = model.rf$predicted
table(prediction.rf,example$y)

#adaptive boosting syntax
#mfinal is the total number of trees to consider.

model.ab = boosting(y~x1+x2+x3,data=example,mfinal=100,control=rpart.control(minsplit=10,cp=.02))
model.ab$votes
prediction.ab = model.ab$class
table(prediction.ab,example$y)


###Random Forests

model.rf = randomForest(Ozone~Temp+Wind+Solar.R,data=data,ntree=100,mtry=2,control=rpart.control(minsplit=10,cp=.02))
model.rf$err.rate
model.rf$votes
model.rf$rsq
prediction.rf = model.rf$predicted

#Note without cross validation our forest will continue to do better and better...
#####End of Random Forests and Adaptive Boosting