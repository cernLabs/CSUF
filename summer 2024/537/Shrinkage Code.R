# ridge regression

# Suppose we have a model that is y = B1x1 + B2x2 + e where e~N(0,1)
# Two variables are measured: x1,x2  
# Ridge regression function, ridge.lm(), is on MASS package

install.packages("MASS")
library(MASS)

# Generating the data
set.seed(24601)

N <- 200      

#First let's assume x1 and x2 are independent
#Note the mean of x1 and x2 are both 0, the mean of y is 0.
#This is important, if this wasn't the case we first have to normalize.

x1 <- runif(n=N)
x2 <- runif(n=N)
e <- rnorm(n=N)
y <- x1 + x2 + e
y = (y - mean(y))/sd(y)

z1 = (x1 - mean(x1))/sd(x1)
z2 = (x2 - mean(x2))/sd(x2)

# Ordinary least squares model for x1 and x2
ols <- lm(y~ z1 + z2)
summary(ols)
par(mfrow=c(2,2))
plot(ols)
ols$coef


# Ridge regression model for x1 and x2
ridge <- lm.ridge(y ~ z1+z2, lambda = 10,model=T)
summary(ridge)
ridge$coef

ridge <- lm.ridge(y ~ z1+z2, lambda = 0:10,model=T)
summary(ridge)
ridge$coef



#Now let's do the same exercise but let's let x1 and x2 be dependent
set.seed(24)
x1 <- runif(n=N)
set.seed(8)
x2 <- x1 + runif(n=N)
set.seed(2)
e <- rnorm(n=N)
y <- x1 + x2 + e

z1 = (x1 - mean(x1))/sd(x1)
z2 = (x2 - mean(x2))/sd(x2)


ols <- lm(y~ z1 + z2)
summary(ols)
par(mfrow=c(2,2))
plot(ols)
ols$coef


# Ridge regression model for x1 and x2
ridge <- lm.ridge(y ~ z1+z2, lambda = 10,model=T)
summary(ridge)
ridge$coef

ridge <- lm.ridge(y ~ z1+z2, lambda = 0:10,model=T)
summary(ridge)
ridge$coef

#Even if we crank up lambda none of our coefficients go to zero?
#Why didn't any of our coefficients go to zero?

library(tidyverse)
library(caret)
library(glmnet)

# Load the data
data("Boston", package = "MASS")

# Split the data into training and test set
index = sample(1:(dim(Boston)[1]),round(dim(Boston)[1]*.8))
train.data  <- Boston[index, ]
test.data <- Boston[-index, ]

#This is a little annoying but the syntax for glmnet is a bit different.
#You need to feed it your predictors and responses are arguments
#Not using the traditional y~x notation for models in r.

# Predictor variables
x <- model.matrix(medv~., train.data)[,-1]

# Outcome variable
y <- train.data$medv

#Find CV best lambda for Ridge Regression
#alpha is the blending parameter for lambda.  if alpha = 0, all of lambda is L2 norm (ridge)
#If alpha = 1 then all of lamba is L1 norm (Lasso).  If alpha is anything between then
#you get a portion of lambda for L1 and a portion for L2.

cv <- cv.glmnet(x, y, alpha = 0)
cv$lambda.min

model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
coef(model)
summary(model)

model1 = lm(medv~.,data=train.data)

SSR.cv.lm = sum((predict(model1,newdata=test.data) - test.data$medv)^2)
SSR.cv.ridge = sum((predict(model,newx=model.matrix(medv~., test.data)[,-1]) - test.data$medv)^2)



