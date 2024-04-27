
###Back to the volcano

volc = reshape2::melt(volcano)
head(volc)
attach(volc)
model.tree = rpart(value~Var1+Var2,method="anova",control=rpart.control(minsplit=5,cp=.0002),data=volc)
prediction.tree = (predict(model.tree,volc[,1:2],type="matrix"))
dev.off()
plot(model.tree)
text(model.tree,cex=.65)


#static plot

scatter3D(x=volc$Var1,y=volc$Var2,z=prediction.tree,ticktype="detailed",pch=2,xlab="longitude",
		ylab="latitude",zlab="depth, km", main = "")

#interactive plot
plotrgl(lighting=T,smooth=T,cex=2)
title3d(main="Sample 3D Plot", line=4, cex=2)



