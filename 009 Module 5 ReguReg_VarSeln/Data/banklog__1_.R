#
# Predicting bankruptcy in the telecommunications industry
#
bankruptcy = read.table("bankruptcy.dat",sep="\t",header=T,row.names=NULL)
attach(bankruptcy)

#
# boxplots
par(mfrow=c(2,3))
boxplot(split(WC.TA,Bankrupt),style.bxp="old",xlab="Bankrupt",ylab="WC.TA",main="Boxplot of WC/TA")
boxplot(split(RE.TA,Bankrupt),style.bxp="old",xlab="Bankrupt",ylab="RE.TA",main="Boxplot of RE/TA")
boxplot(split(EBIT.TA,Bankrupt),style.bxp="old",xlab="Bankrupt",ylab="EBIT.TA",main="Boxplot of EBIT/TA")
boxplot(split(S.TA,Bankrupt),style.bxp="old",xlab="Bankrupt",ylab="S.TA",main="Boxplot of S/TA")
boxplot(split(BVE.BVL,Bankrupt),style.bxp="old",xlab="Bankrupt",ylab="BVE.BVL",main="Boxplot of BVE/BVL")

cor(bankruptcy[,-c(1,7)])

bank1 = glm(Bankrupt ~ WC.TA + RE.TA + EBIT.TA + S.TA + BVE.BVL, family=binomial)
summary(bank1)

## test for overall regression
gstat = bank1$null.deviance - deviance(bank1)
cbind(gstat, 1-pchisq(gstat,length(coef(bank1))-1))

#
# model selection
#
library(bestglm)
input.Xy <- as.data.frame(cbind(WC.TA, RE.TA, EBIT.TA, S.TA, BVE.BVL,Bankrupt))
bestBIC <- bestglm(input.Xy, IC="BIC",family=binomial)

bank2 = glm(Bankrupt ~ RE.TA + EBIT.TA + BVE.BVL, family=binomial, maxit=500, x=T)
summary(bank2)

## Compare the full model vs reduced model:
gstat = deviance(bank2) - deviance(bank1)
cbind(gstat, 1-pchisq(gstat,length(coef(bank1))-length(coef(bank2))))

## Compare exponential coefficients
exp(coef(bank1)[-1])
exp(coef(bank2)[-1])
#
#
# omit one outlier observation
#
bankrupt2 = bankruptcy[-1,]
attach(bankrupt2)
#
# The results aren't exactly the same when there is separation, but the symptoms of the problem
# are similar
#
bank3 = glm(Bankrupt ~ WC.TA + RE.TA + EBIT.TA + S.TA + BVE.BVL, family=binomial, data=bankrupt2, maxit=500)
summary(bank3)

## best subset regression
input.Xy <- as.data.frame(cbind(WC.TA, RE.TA, EBIT.TA, S.TA, BVE.BVL,Bankrupt))
bestBIC <- bestglm(input.Xy, IC="BIC",family=binomial)
bestBIC

## Stepwise Regression
bank3.select=step(bank3,direction = "backward")
summary(bank3.select)

#
# best subset regression
#
bank4 = glm(Bankrupt ~ RE.TA + EBIT.TA + BVE.BVL, family=binomial, maxit=500, x=T)
summary(bank4)

exp(coef(bank2)[-1])
exp(coef(bank4)[-1])

## Apply LASSO/Elastic Net
library(glmnet)
## alpha=1 for lasso, alpha=0 for ridge, other alpha for elastic net
## standardize=TRUE is the default

X = cbind(WC.TA, RE.TA, EBIT.TA, S.TA, BVE.BVL)

## LASSO
## 10-fold CV to find the optimal lambda 
bank5.cv = cv.glmnet(X, Bankrupt, family=c("binomial"), alpha=1, type="class", nfolds=10)

## Fit lasso model with 100 values for lambda
bank5 = glmnet(X, Bankrupt, family=c("binomial"), alpha=1, nlambda=100)

## Extract coefficients at optimal lambda
coef(bank5, s=bank5.cv$lambda.min)

## Plot coefficient paths
plot(bank5,xvar="lambda",label=TRUE,lwd=2)
abline(v=log(bank5.cv$lambda.min),col='black',lty = 2,lwd=2)

## Elastic Net
## 10-fold CV to find the optimal lambda 
bank6.cv = cv.glmnet(X, Bankrupt, family=c("binomial"), alpha=0.5, type="class", nfolds=10)

## Fit elastic net model with 100 values for lambda
bank6 = glmnet(X, Bankrupt, family=c("binomial"), alpha=0.5, nlambda=100)

## Extract coefficients at optimal lambda
coef(bank6, s=bank6.cv$lambda.min)

## Plot coefficient paths
plot(bank6, xvar="lambda", lwd=2)
abline(v=log(bank6.cv$lambda.min), col='black', lty = 2, lwd=2)
