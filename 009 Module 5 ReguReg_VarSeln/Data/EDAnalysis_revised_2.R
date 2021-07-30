dataAdult = read.csv("DataADULT.csv", header=TRUE)
attach(dataAdult)

###############################################################################
################ EXPLORATORY ANALYSIS #########################################
###############################################################################

## Rescale outcome/response variable
EDCost.pmpm = EDCost/PMPM

## Rescale utilization
dataAdult$PO = PO/PMPM
dataAdult$HO = HO/PMPM

detach(dataAdult)
attach(dataAdult)

## Histogram of the response variable
par(mfrow=c(2,1))
hist(EDCost.pmpm, breaks=300, xlab="Emergency Department Cost", main="")
hist(log(EDCost.pmpm), breaks=300, xlab="Log-Emergency Department Cost", main="")

log.EDCost.pmpm = log(EDCost.pmpm)

## Response variable vs categorical prediciting variables
par(mfrow=c(2,1))
boxplot(log.EDCost.pmpm ~ State, main = "Variation of log of ED costs by state")
boxplot(log.EDCost.pmpm ~ Urbanicity, main = "Variation of log of ED costs by urbanicity")


## Scatterplot matrix plots
library(car)
par(mfrow=c(1,1))

## Response vs Utilization
scatterplotMatrix(~ log(EDCost.pmpm) + HO + PO, data=dataAdult, smooth=FALSE)

## Response vs Population Characteristics
scatterplotMatrix(~ log(EDCost.pmpm) + WhitePop + BlackPop + OtherPop + HealthyPop + 
                      ChronicPop + ComplexPop, data=dataAdult,smooth=FALSE)

## Response vs Socioeconomic and Environmental Characteristics
scatterplotMatrix(~ log(EDCost.pmpm) + Unemployment + Income + Poverty + Education + 
                      Accessibility + Availability + ProvDensity, data=dataAdult, smooth=FALSE)

## Response vs County Health Rankings
scatterplotMatrix(~ log(EDCost.pmpm) + RankingsPCP + RankingsFood + RankingsHousing + 
                      RankingsExercise + RankingsSocial, data=dataAdult, smooth=FALSE)

## Correlation matrix plot
library(corrplot)

corr = cor(cbind(log(EDCost.pmpm), dataAdult[,-c(1, 2, 3, 18)]))
corrplot(corr)

################################################################################
################# FULL MODEL: Fitted Model & Residual Analysis #################
################################################################################

## Exclude GEOID, scaling factor PMPM, and confounding factors EDCost and ED
## Exclude OtherPop & ComplexPop because of linear dependence
dataAdult.red = dataAdult[, -c(1, 3, 4, 5, 10, 13)] 

fullmodel = lm(log(EDCost.pmpm) ~ ., data=dataAdult.red)
summary(fullmodel)

### Residual Analysis

## Residuals versus individual predicting variables 
full.resid = rstandard(fullmodel)
cook = cooks.distance(fullmodel)

par(mfrow=c(2,2))

## Check outliers
influencePlot(fullmodel)
plot(cook, type="h", lwd=3, col="red", ylab="Cook's Distance")

## Check Normality
qqPlot(full.resid, ylab="Residuals", main = "")
qqline(full.resid, col="red", lwd=2)
hist(full.resid, xlab="Residuals", main = "", nclass=30, col="orange")

## Check Constant Variance & Uncorrelated Errors
full.fitted = fitted(fullmodel)
par(mfrow=c(1,1))
plot(full.fitted, full.resid, xlab="Fitted Values", ylab="Residuals")

## Check Linearity
crPlots(fullmodel, ylab="")

############################################################################
######################### VARIABLE SELECTION ###############################
############################################################################

## 2^23 =  8388608 models -- not feasible to compare all models

## Apply Lasso /Elastic Net

# THERE ARE TWO VERSIONS, SELECT ONE

# Version 1
library(glmnet)
predictors = as.matrix(dataAdult[, -c(1, 2, 3, 4, 5, 10, 13, 18)])

# Use semicolon ; to separate two statements on one line in R
# Leave out one indicator (dummy) variable for each group
# AL     = rep(0, length(State));                 AL[as.numeric(factor(State))==1] = 1
AR       = rep(0, length(State));                 AR[as.numeric(factor(State))==2] = 1
LA       = rep(0, length(State));                 LA[as.numeric(factor(State))==3] = 1
NC       = rep(0, length(State));                 NC[as.numeric(factor(State))==4] = 1
# rural  = rep(0, length(Urbanicity));    rural[as.numeric(factor(Urbanicity))==1] = 1
suburban = rep(0, length(Urbanicity)); suburban[as.numeric(factor(Urbanicity))==2] = 1
urban    = rep(0, length(Urbanicity));    urban[as.numeric(factor(Urbanicity))==3] = 1

predictors = cbind(predictors, AR, LA, NC, suburban, urban)

## 10-fold CV to find the optimal lambda 
lassomodel.cv = cv.glmnet(predictors, log(EDCost.pmpm), alpha=1, nfolds=10)

## Fit lasso model with 100 values for lambda
lassomodel = glmnet(predictors, log(EDCost.pmpm), alpha=1, nlambda=100)

## Plot coefficient paths
plot(lassomodel, xvar="lambda", label=TRUE, lwd=2)
abline(v=log(lassomodel.cv$lambda.min), col='black', lty=2, lwd=2)

coef(lassomodel, lassomodel.cv$lambda.min)
# END VERSION 1


# Version 2
library(glmnet)
predictors = as.matrix(dataAdult[, -c(1, 2, 3, 4, 5, 10, 13, 18)])

# Set up indicator (dummy) variables for State and Urbanicity
# Leave out one indicator (dummy) variable for each group

#AL= rep(0, length(State))
AR = rep(0, length(State))
LA = rep(0, length(State))
NC = rep(0, length(State))
#AL[as.numeric(factor(State))==1] = 1
AR[as.numeric(factor(State))==2] = 1
LA[as.numeric(factor(State))==3] = 1
NC[as.numeric(factor(State))==4] = 1

#rural   = rep(0, length(Urbanicity))
suburban = rep(0, length(Urbanicity))
urban    = rep(0, length(Urbanicity))
#  rural[as.numeric(factor(Urbanicity))==1] = 1
suburban[as.numeric(factor(Urbanicity))==2] = 1
urban[as.numeric(factor(Urbanicity))==3] = 1

predictors = cbind(predictors, AR, LA, NC, suburban, urban)

## LASSO Regression
## 10-fold CV to find the optimal lambda 
lassomodel.cv = cv.glmnet(predictors, log(EDCost.pmpm), alpha=1, nfolds=10)

## Fit lasso model with 100 values for lambda
lassomodel = glmnet(predictors, log(EDCost.pmpm), alpha=1, nlambda=100)

## Plot coefficient paths
plot(lassomodel, xvar="lambda", label=TRUE, lwd=2)
abline(v=log(lassomodel.cv$lambda.min), col='black', lty=2, lwd=2)

## Extract coefficients at optimal lambda
coef(lassomodel, lassomodel.cv$lambda.min)
# END VERSION 2


## Elastic Net Regression
## 10-fold CV to find the optimal lambda 
enetmodel.cv = cv.glmnet(predictors, log(EDCost.pmpm), alpha=0.5, nfolds=10)

## Fit lasso model with 100 values for lambda
enetmodel = glmnet(predictors, log(EDCost.pmpm), alpha=0.5, nlambda=100)

## Plot coefficient paths
plot(enetmodel, xvar="lambda", label=TRUE, lwd=2)
abline(v=log(enetmodel.cv$lambda.min), col='black', lty=2, lwd=2)

## Extract coefficients at optimal lambda
coef(enetmodel, s=enetmodel.cv$lambda.min)


# NOT IN VIDEO OR OUT OF ORDER
library(lars)
object = lars(x = predictors, y = log(EDCost.pmpm))
plot(object)
object$Cp

plot.lars(object, xvar="df", plottype="Cp")
# END NOT IN VIDEO


### Apply Stepwise Regression
full = lm(log(EDCost.pmpm) ~ HealthyPop + ChronicPop + State + Urbanicity + HO + PO +
              BlackPop + WhitePop + Unemployment + Income + Poverty+ Education +
              Accessibility + Availability + ProvDensity +
              RankingsPCP + RankingsFood + RankingsExercise + RankingsSocial,data=dataAdult)  

minimum = lm(log(EDCost.pmpm) ~ HealthyPop + ChronicPop, data=dataAdult)

# Forward Stepwise Regression
forward.model = step(minimum, scope=list(lower=minimum, upper=full), direction="forward")
summary(forward.model)

# Backward Stepwise Regression
backward.model = step(full, scope=list(lower=minimum, upper=full), direction = "backward")
summary(backward.model)

# Forward-Backward Stepwise Regression
both.min.model = step(minimum, scope=list(lower=minimum, upper=full), direction = "both")
summary(both.min.model)

## Compare full model to selected model
reg.step = lm(log(EDCost.pmpm) ~ HealthyPop + ChronicPop + State +  Urbanicity + HO + 
                PO + BlackPop + WhitePop + Education +  Accessibility + Availability + 
                ProvDensity + RankingsPCP + RankingsFood,data=dataAdult)

anova(reg.step, full)

################################################################
############# Residual Analysis for Selected Model #############
################################################################

red.resid = rstandard(reg.step)
red.cook = cooks.distance(reg.step)
par(mfrow=c(2,2))

## Check outliers
influencePlot(reg.step)

plot(red.cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")

## Check normality
qqPlot(red.resid, ylab="Residuals", main = "")
qqline(red.resid, col="red", lwd=2)

hist(red.resid, xlab="Residuals", main = "", nclass=30, col="orange")

## Check Linearity
crPlots(reg.step,ylab="")

###########################################################################
##################### Remove Outlier ######################################
###########################################################################

dataAdult.no.out = dataAdult[-909,]
EDCost.pmpm.no.out = EDCost.pmpm[-909]

full.no.out = lm(log(EDCost.pmpm.no.out) ~ HealthyPop + ChronicPop + State + Urbanicity +
                     HO + PO + BlackPop + WhitePop + Unemployment + Income + Poverty +
                     Education + Accessibility + Availability + ProvDensity + RankingsPCP +
                     RankingsFood + RankingsExercise + RankingsSocial, data=dataAdult.no.out)

minimum.no.out = lm(log(EDCost.pmpm.no.out) ~ HealthyPop+ChronicPop, data=dataAdult.no.out)

# Backward
backward.model.no.out = step(full.no.out, scope=list(lower=minimum.no.out,
                             upper=full.no.out), direction="backward")
summary(backward.model)
summary(backward.model.no.out)

reg.step.no.out = lm(log(EDCost.pmpm.no.out) ~ HealthyPop + ChronicPop + State + HO +
                         Education + ProvDensity + RankingsPCP + Accessibility +
                         Availability + PO + Urbanicity + BlackPop + WhitePop +
                         RankingsFood, data=dataAdult.no.out)

# NOT IN VIDEO
summary(reg.step.no.out) # gives same results as summary(backward.model.no.out) above
summary(reg.step) # gives same results as summary(backward.model) above
# END NOT IN VIDEO


############################################################################
######################### PREDICTION: Interventions ########################
############################################################################

newdata=dataAdult.no.out
index = which(newdata$Availability >= 0.5)

# Improve Availability to at most 0.5 congestion experienced by all communities
newdata$Availability[index] = 0.5

# Predict by changing Availability with all other predictors fixed
EDCost.predict = predict(reg.step.no.out, newdata, interval="prediction")[,1]

# Compare predicted to fitted for those communities with intervention
EDCost.diff.fitted = exp(fitted(reg.step.no.out)) - exp(EDCost.predict)
hist(EDCost.diff.fitted[index], xlab="Difference in Expected versus Predicted ED Cost", 
     main="Predicted vs. Fitted with Intervention", col="orange")

# Compare predicted to observed for those communities with intervention
EDCost.diff.observed = EDCost.pmpm[-909] - exp(EDCost.predict)
summary(EDCost.diff.observed[index])
hist(EDCost.diff.observed[index], xlab="Difference in Observed versus Predicted ED Cost", 
     main="Predicted vs. Observed with Intervention", col="red")

##############################################################################
############################### FINDINGS #####################################
##############################################################################

boxplot(ED/PMPM~State, xlab="ED Utilization")
