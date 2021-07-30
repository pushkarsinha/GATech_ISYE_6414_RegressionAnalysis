meddcor = read.table("meddcor.txt", sep="", header = FALSE)
n = dim(meddcor)[1] # number of observations
colnames(meddcor) = c("sales", "advertising", "bonuses", "marketshare", "largestcomp", "region")
meddcor$region = as.factor(meddcor$region)
model = lm(sales ~ ., data = meddcor)
summary(model)

## Estimate variance
s2 = summary(model)$sigma^2


## Design Matrix
X = model.matrix(model)

## First office data for variance and standard deviation formula
xstar = X[1,]

## prediction variance and standard deviation
predvar <- s2*(xstar%*%solve(t(X)%*%X)%*%xstar)
sqrt(predvar)

## First office data for confidence and prediction intervals
newdata = meddcor[1,-1]

## Confidence Interval
predict(model, newdata, interval="confidence")

## Prediction Interval
predict(model, newdata, interval="prediction")

## Change the competitor's sales for prediction of future observation
newobs = newdata
newobs[4] = 303
xstar.newobs = xstar
xstar.newobs[5] = 303

predvar.newobs = s2*(1+xstar.newobs%*%solve(t(X)%*%X)%*%xstar.newobs)
sqrt(predvar.newobs)

## Prediction Interval
predict(model, newobs, interval="prediction")

#### Evaluating model assumptions
## Scatter plot matrix of sales and quantitative predicting variables 
plot(meddcor[,1:5])
## (Standardized) Residuals versus individual predicting variables 
library(MASS)
resids = stdres(model)
par(mfrow =c(2,2))
plot(meddcor[,2],resids,xlab="Adv Expenditure",ylab="Residuals")
abline(0,0,col="red")
plot(meddcor[,3],resids,xlab="Bonuses",ylab="Residuals")
abline(0,0,col="red")
plot(meddcor[,4],resids,xlab="Market Share",ylab="Residuals")
abline(0,0,col="red")
plot(meddcor[,5],resids,xlab="Competitor's Sales",ylab="Residuals")
abline(0,0,col="red")

library(car)
fits = model$fitted
cook = cooks.distance(model)
par(mfrow =c(2,2))
plot(fits, resids, xlab="Fitted Values",ylab="Residuals")
abline(0,0,col="red")
qqPlot(resids, ylab="Residuals", main = "")
hist(resids, xlab="Residuals", main = "",nclass=10,col="orange")
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")


