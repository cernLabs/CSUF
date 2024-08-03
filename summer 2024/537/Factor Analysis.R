#####Factor Analysis
#Lets read in the CHSI1.csv dataset.

data = read.csv("CHSI1.csv")

#In this dataset the first 4 columns are response information, the next five columns are information on device counts and the remaining columns are customer package levels.

#We're first going to do some exploratory factor analysis.  First step is to isolate the predictors.

X = data[,-c(1,2,3,4)]
fact.model = factanal(X,factors=2,rotation="varimax")
fact.model
names(fact.model)
fact.model$loadings
loadings = fact.model$loadings[,1:2]
plot(loadings,type="n")
text(loadings,labels=names(X),cex=.7)
lines(c(-2,2),c(0,0),lty=2,lwd=.7)
lines(c(0,0),c(-2,2),lty=2,lwd=.7)

###It might seem shady that we simply decided there were two factors in an exploratory analysis (though this is perfectly normal in a confirmatory analysis)

###We might explore how many factors seem relevent using a scree plot like we did in PCA.

library(nFactors)

ev <- eigen(cor(X)) # get eigenvalues
ap <- parallel(subject=nrow(X),var=ncol(X),
rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

###Confirmatory analysis is a bit more difficult, as we first have to specify a model.  Syntax can be quite annoyting and to be honest programs like SAS and SPSS are better built for this.  But if this is of interest I suggest you start with the sem package (structural equation model)
