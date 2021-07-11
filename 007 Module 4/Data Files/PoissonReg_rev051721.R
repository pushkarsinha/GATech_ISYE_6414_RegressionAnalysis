## First Example: Student Awards data
awardsdata = read.csv("students_awards.csv",header=T)
awardsdata = within(awardsdata, 
 {
  prog = factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id = factor(id)
})
summary(awardsdata)

#conditional histograms
library(ggplot2)
ggplot(awardsdata, aes(num_awards, fill = prog)) + geom_histogram(binwidth=.5, position="dodge")

# Fit a Stnadard Regression Model
m0 = lm(num_awards ~ prog + math, data=awardsdata)
## Peform Residual Analysis
res = m0$res
par(mfrow = c(2,2))
plot(awardsdata$math, res, xlab = "Math Exam Score", ylab = "Residuals", pch = 19)
abline(h = 0)
plot(fitted(m0), res, xlab = "Fitted Values", ylab = "Residuals", pch = 19)
abline(h = 0)
hist(res, xlab="Residuals", main= "Histogram of Residuals")
qqnorm(res)
qqline(res)

#poisson model 
m1 = glm(num_awards ~ prog + math, family="poisson", data=awardsdata)
summary(m1)

# residual deviance test
with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

## Residual Analysis
res = resid(m1,type="deviance")
par(mfrow=c(2,2))
plot(awardsdata$math,res,ylab="Std residuals",xlab="Math Exam")
abline(0,0,col="blue",lwd=2)
boxplot(res~prog,ylab = "Std residuals")
qqnorm(res, ylab="Std residuals")
qqline(res,col="blue",lwd=2)
hist(res,10,xlab="Std residuals", main="")



#poisson model with nonlinear modeling of math
library(mgcv)
m2 = gam(num_awards ~ prog + s(math), family="poisson", data=awardsdata)

# residual deviance test
with(m2, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


#########################################################################
################## Second example: Insurance Data #######################
#########################################################################
library(MASS)
summary(Insurance)

Ins.dat = within(Insurance, 
                 {
                   Age = factor(Age, ordered=F)
                   Group = factor(Group, ordered=F)
                   District = factor(District, ordered=F)
                 })

#par(mfrow=c(2,2))
boxplot(Claims/Holders~District, xlab = "District", ylab = "Rate of claims per policyholder",data=Ins.dat)
boxplot(Claims/Holders~Group, xlab = "Group", ylab = "Rate of claims per policyholder",data=Ins.dat)
boxplot(Claims/Holders~Age, xlab = "Age", ylab = "Rate of claims per policyholder",data=Ins.dat)


# poisson regression
m.ins = glm(Claims ~ District + Group + Age + offset(log(Holders)), data = Ins.dat, family = poisson)
summary(m.ins)


# test for overall regression
1-pchisq((236.26-51.42),(63-54))
# test for subsets of regression coefficients
## Is District significantly explanatory? 
library(aod)
wald.test(b=coef(m.ins), Sigma=vcov(m.ins), Terms=2:4)

#residual deviance test
with(m.ins, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail=FALSE))) 


## Residual Analysis
res = resid(m.ins,type="deviance")
par(mfrow=c(2,2))
boxplot(res~District,ylab = "Std residuals",data = Ins.dat)
boxplot(res~Age,ylab = "Std residuals",data = Ins.dat)
qqnorm(res, ylab="Std residuals")
qqline(res,col="blue",lwd=2)
hist(res,10,xlab="Std residuals", main="")

