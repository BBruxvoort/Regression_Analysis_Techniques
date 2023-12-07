#read in data from working directory
murder<-read.csv(file="MURDERRATE.csv", header=TRUE, sep=",")
murder

#make scatterplot
plot(murder$weapons, murder$murder.rate, pch=20)

#run basic SLR
murderSLR<-lm(murder.rate~weapons, data=murder)
summary(murderSLR)

#add regression line to scatterplot
abline(murderSLR)

#confidence intervals for coefficients
confint(murderSLR, level=0.95)

#Create ANOVA table
anova(murderSLR)

#Basic residual plots
plot(murderSLR, which=1, pch=20)  #residual vs fits
par(mfrow=c(1,2))                 
hist(resid(murderSLR))            #histograam of residuals
plot(murderSLR, which=2, pch=20)  #QQplot
par(mfrow=c(1,1))
library(car)
qqPlot(murderSLR, pch=20)         #another QQplot
par(mfrow=c(1,1))
plot(resid(murderSLR), pch=20)
