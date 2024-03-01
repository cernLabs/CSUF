# BOOTSTRAPPING PROTOCAL FOR REGRESSION

## step 1: Fit original model, store original residuals
df <- read.csv('spend_revenue.csv')
df = df[-257,]

fit <- lm(df$Spend ~ df$Revenue)
orig_res <- fit$residuals
n <- length(orig_res)

## step 2: generte BS.x

BS.x <- sample(df[,2],n,replace = T)

## step 3: generate BS.yhat using original model (pluggin in BS.x to original model)

BS.yhat <- predict(fit,newdata=data.frame(Spend=BS.x))


## step 4: generate BS.y by addingn random residuals (samp w rep) to BS.yhat

BS.y <- BS.yhat + sample(orig_res,n,replace = T)

## step 5: generate model using BS.x, BS.y; store desired output (Bhat0, Bhat1, y.hat.stat)

BS_fit <- lm(BS.y~BS.x)
BS.b1 <- BS_fit$coef[2]
BS.yhat.500k <- predict(BS_fit, newdata = data.frame(BS.x = 500000))
## step 6:  repeat 10k times
BS.b1 <- rep(0,10000)
BS.yhat.500k <- rep(0,10000)
BS.y.500k <- rep(0,10000)

for (i in 1:10000){
  
}

## step 7: Emperical distribution, conf. interval, pval.

## step 8: food


