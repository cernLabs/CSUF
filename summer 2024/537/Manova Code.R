#Manova in R.

data=read.csv("manova_ex.csv",h=T)
data = data[,-1]
model = manova(cbind(x1,x2,x3,x4)~group,data=data)
summary(model)

#Function for Wilks Lambda

Wlam = function(df){
	unique.groups = unique(df$group)
	g = length(unique.groups)
	ni = rep(0,g)
	Si = list()
	SSw = matrix(0,ncol(df)-1,ncol(df)-1)
	for(i in 1:g){
		ni[i] = length(df$x1[df$group==unique.groups[i]])
		Si[[i]] = var(df[df$group==unique.groups[i],-1])
		SSw = SSw+(Si[[i]]*(ni[i]-1))
	}
	n = sum(ni)
	S = var(df[,-1])
	SSt = S*(n-1)
	lambda = det(SSw)/det(SSt)
	lambda
}

lambda.obs = Wlam(data)
chisq = -(320-1 - (16/2))*log(lambda.obs)
1-pchisq(chisq,4*3)

#Hmm didn't quite match.  
#You can ask manova() for the Wilks lambda 
#test instead of Pillai's trac (which we didn't cover)


summary.manova(model,test="Wilks")
lambda.obs = summary.manova(model,test="Wilks")$stats[3]

#Notice our pvalues are a little off even though our test stats are the same.
#I more or less just lazily used a chi-sq distribution.  Not a huge deal.
#One thing I hate about some of this stuff is the distributions we should
#be using isn't always clear.  Why not BS 2000 obs of Wilk's Lambda unde the 
#Null hypothesis and then emprically see how often we replicate our observed
#Wilk's Lambda?


