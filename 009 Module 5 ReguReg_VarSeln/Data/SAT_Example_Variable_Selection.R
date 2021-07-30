################################################
### Multiple Linear regression #################
#### SAT DATA Example ##########################
datasat = read.table("CASE1201.ASC.txt", header = TRUE)
attach(datasat)

## Fit full model
regression.line = lm(sat ~ log(takers) + rank + income + years + public + expend)
summary(regression.line)

## Compare to the model with confounding variables only
regression.red = lm(sat ~ log(takers) + rank)
anova(regression.red, regression.line)

## Obtain Mallow's Cp, AIC, BIC criterion values
library(CombMSC)
n = nrow(datasat)



# START OF VERSIONS (SELECT ONLY ONE)

# version 1
## full model
c(Cp(regression.line, S2=24.86^2), 
  AIC(regression.line, k=2), AIC(regression.line,k=log(n)))

## reduced model
c(Cp(regression.red, S2=summary(regression.line)$sigma^2), 
  AIC(regression.red, k=2), AIC(regression.red,k=log(n)))

# version 2
## full model
c(Cp(regression.line, S2=summary(regression.line)$sigma^2), 
  AIC(regression.line, k=2), AIC(regression.line,k=log(n)))

## reduced model
c(Cp(regression.red, S2=summary(regression.line)$sigma^2), 
  AIC(regression.red, k=2), AIC(regression.red,k=log(n)))

# version 3
## full model
c(Cp(regression.line, S2=24.86^2), 
  AIC(regression.line, k=2), AIC(regression.line,k=log(n)))

## reduced model
c(Cp(regression.red, S2=24.86^2), 
  AIC(regression.red, k=2), AIC(regression.red,k=log(n)))

# END OF VERSIONS



# NOT IN VIDEO BUT IN ORIGINAL CODE FILE
library(boot)
## CV: 10-fold and leave one out 
gregression.line = glm(sat ~log(takers) + rank + income + years + public + expend)
c(cv.glm(datasat, gregression.line, K=10)$delta[1], cv.glm(datasat, gregression.line, K=n)$delta[1])
gregression.red = glm(sat ~ log(takers)  + rank)
c(cv.glm(datasat, gregression.red, K=10)$delta[1], cv.glm(datasat, gregression.red, K=n)$delta[1])
# END OF NOT IN VIDEO


### Search over all  (2^6=64 models total)
library(leaps)
out = leaps(datasat[,-c(1,2)], sat, method = "Cp")
cbind(as.matrix(out$which),out$Cp)
best.model = which(out$Cp==min(out$Cp))
cbind(as.matrix(out$which),out$Cp)[best.model,]

### Apply Stepwise Regression

## Forward Stepwise Regression
step(lm(sat~log(takers)+rank), scope=list(lower=sat~log(takers)+rank, 
     upper=sat~log(takers)+rank+expend+years+income+public), direction="forward")

# NOT IN VIDEO BUT IN ORIGINAL CODE FILE
par(mfrow = c(1,1))
AIC = c(346.7, 331.66, 323.9)
ommitted = c(325.85, 327.8)
plot(1:3, AIC, xlim=c(1,5), type="l", xaxt="n", xlab="", main="Forward Stepwise AIC Plot")
points(1:3, AIC, pch=19)
points(4:5, ommitted)
axis(1, at = 1:5, labels = c("log(takers)+rank", "expend", "years", "income", "public"))
abline(h=323.9, lty=2)
# END OF NOT IN VIDEO

## Backward Stepwise Regression
full = lm(sat ~ log(takers) + rank + expend + years + income + public)
minimum = lm(sat ~ log(takers) + rank)
step(full, scope=list(lower=minimum, upper=full), direction="backward")

# NOT IN VIDEO OR OUT OF ORDER
# Both
step(minimum, scope = list(lower=minimum, upper = full), direction = "both")
step(full, scope = list(lower=minimum, upper = full), direction = "both")
# END OF NOT IN VIDEO

##### Ridge Regression ######################

library(MASS)
## Scale the predicting variables and the response variable
ltakers = log(takers)
predictors = cbind(ltakers, rank, income, years, public, expend)
predictors = scale(predictors)
sat.scaled = scale(sat)

## Apply ridge regression for a range of penalty constants
lambda = seq(0, 10, by=0.25)
out = lm.ridge(sat.scaled~predictors, lambda=lambda)
round(out$GCV, 4)
which(out$GCV == min(out$GCV))
round(out$coef[,10], 4)

## Plot lambdas against coefficients
par(mfrow = c(1,1))
# Plot first value
plot(lambda, out$coef[1,], type = "l", col=1, lwd=3,
     xlab = "Lambda", ylab = "Coefficients",
     main = "Plot of Regression Coefficients vs. Lambda Penalty Ridge Regression", 
     ylim = c(min(out$coef), max(out$coef)))
# Now the other values
for(i in 2:6)
    points(lambda, out$coef[i,], type = "l", col=i, lwd=3)
# Now horizontal and vertical dotted lines
abline(h = 0, lty = 2, lwd = 3)
abline(v = 2.25, lty = 2, lwd=3)



##### LASSO Regression ######################

library(lars)
object = lars(x=predictors, y=sat.scaled)
object
round(object$Cp, 2)

plot(object)
plot.lars(object, xvar="df", plottype="Cp")

## Apply LASSO/Elastic Net
library(glmnet)

## LASSO
## alpha=1 for lasso
Xpred = cbind(ltakers, rank, income, years, public, expend)

## Find the optimal lambda using 10-fold CV 
satmodel.cv = cv.glmnet(Xpred, sat, alpha=1, nfolds=10)

## Fit lasso model with 100 values for lambda
satmodel = glmnet(Xpred, sat, alpha=1, nlambda=100)

## Extract coefficients at optimal lambda
coef(satmodel, s=satmodel.cv$lambda.min)

## Plot coefficient paths
plot(satmodel, xvar="lambda", lwd=2)
abline(v=log(satmodel.cv$lambda.min), col='black', lty=2, lwd=2)

## Elastic Net
## alpha=0.5 (or other values different from 0 or 1) for elastic net
Xpred = cbind(ltakers, rank, income, years, public, expend)

## Find the optimal lambda using 10-fold CV 
satmodel.cv = cv.glmnet(Xpred,sat, alpha=0.5, nfolds=10)

## Fit elastic net model with 100 values for lambda
satmodel = glmnet(Xpred, sat, alpha=0.5, nlambda = 100)

## Extract coefficients at optimal lambda
coef(satmodel, s=satmodel.cv$lambda.min)

## Plot coefficient paths
plot(satmodel, xvar="lambda", lwd=2)
abline(v=log(satmodel.cv$lambda.min), col='black', lty=2, lwd=2)
