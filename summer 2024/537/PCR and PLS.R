require(pls)
set.seed(1000)
data = log(iris[,1:4])
###Let's assume we want to predict log Sepal.Length as a function of
###Sepal.Width,Petal.Length and Petal.Width.

z_SL = (data$Sepal.Length - mean(data$Sepal.Length))/(sd(data$Sepal.Length))
z_SW = (data$Sepal.Width - mean(data$Sepal.Width))/(sd(data$Sepal.Width))
z_PL = (data$Petal.Length - mean(data$Petal.Length))/(sd(data$Petal.Length))
z_PW = (data$Petal.Width - mean(data$Petal.Width))/(sd(data$Petal.Width))


pcr_model = pcr(z_SL~z_SW+z_PL+z_PW)
summary(pcr_model)
names(pcr_model)
pcr_model$coef

###Compare to a linear model

linear_model = lm(z_SL~z_SW+z_PL+z_PW)
summary(linear_model)

###pcr_has built in functions for cross validation.

pcr_model = pcr(z_SL~z_SW+z_PL+z_PW,validation="CV")
summary(pcr_model)

###What about partial least squares...its in the same package.

partial_model = plsr(z_SL~z_SW+z_PL+z_PW,validation="CV")
summary(partial_model)
partial_model$coef

###Which model performed better, a PCR model with two components or....a partial linear regression model with two components?



