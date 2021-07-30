#
# Purchasing power parity - is it true?
#
# Data files that are comma-delimited (csv) files are easy to read into R using the read.csv() command#
# sep="\t“ specifies how the data values are separated; & header=T is for specification of the column names#
ppp = read.table("ppp.dat",sep="\t", header=T, row.names=NULL)
## Check to make sure you read the data in R correctly 
ppp[1:2,]
# Country Inflation.difference Exchange.rate.change Developed
# 1 Australia              -1.2351              -3.1870         1
# 2   Austria               1.5508               1.4781         1
## How many countries? 
dim(ppp)
# [1] 40  4

##  Brazil is an outlier and it was not included in the data set initially; I am adding it back as follows
addp = data.frame("Brazil",-76,-73,0)
names(addp)= names(ppp)
## Save the data variables to be recognized by R as separate variables
ppp = data.frame(rbind(ppp,addp))
#attach(ppp)

## Relabel the ‘Developed’ column to differentiate between Developed and Developing countries
ppp$Developed[ppp$Developed==1] = "Developed"
ppp$Developed[ppp$Developed==0] = "Developing"

#### Exploratory Data Analysis

#  Evaluate the Linear Relationship: Perform a scatter plot of the two variables
plot(ppp$Inflation.difference, ppp$Exchange.rate.change,  main="Scatterplot of Exchange rate change vs Inflation difference",
     xlab="Inflation difference",ylab="Exchange rate change")
boxplot(ppp$Exchange.rate.change~as.factor(ppp$Developed), main="Boxplot of Exchange rate change by Developed vs 
Developing Countries",xlab="Country Class",ylab="Exchange rate change")

#### Perform a linear regression analysis

pppa = lm(Exchange.rate.change ~ Inflation.difference, data = ppp)  ## regression model
summary(pppa)
#
# Load John Fox's car package (see http://socserv.socsci.mcmaster.ca/jfox/Books/Companion/)
# Note that certain functions in this or other packages might be different in different releases of the 
# package; see the package documentation if issues arise
#
# Packages must be downloaded and installed before they can be used. They can be installed from within R 
# by clicking on Package -> Install package(s) and following the instructions. A package only needs 
# to be downloaded and installed once; after that, it will always be available, and you can load it into 
# your current R session using the library() command. 
#you will have to intall if not done already
#install.packages("car")
library(car)
#
# Partial F-test for slope coefficient equaling 1
#
linearHypothesis(pppa,c(0,1),rhs=1)
## Alternatively, you can compute the t-value and p-value as follows:
tvalue = (0.9618-1)/0.01781
tvalue

pvalue = 2*(1-pt(abs(tvalue),39))
pvalue


#
# Function for fitted line plot
#
regplot.confbands.fun <- function(x,y,confidencelevel=.95,CImean=T,PI=T,CIregline=F,legend=F){
    #### Modified from a function written by Sandra McBride, Duke University
    #### For a simple linear regression line, this function
    #### will plot the line, CI for mean response, prediction intervals, 
    #### and (optionally) a simulataneous CI for the regression line.
    xx <- x[order(x)]
    yy <- y[order(x)]
    lm1 <- lm(yy~xx)    
    plot(xx,yy,ylim=c(min(yy),(max(yy)+.2*max(yy))),main="Fitted Line Plot",ylab="Exchange rate change",
         xlab="Inflation difference")
    abline(lm1$coefficients)
    #### calculation of components of intervals ####
    n <- length(yy)
    sx2 <- (var(xx))
    shat <- summary(lm1)$sigma
    s2hat <- shat^2
    SEmuhat <- shat*sqrt(1/n+ ((xx-mean(xx))^2)/((n-1)*sx2))
    SEpred <- sqrt(s2hat+SEmuhat^2)
    t.quantile <- qt(confidencelevel,lm1$df.residual)
    ####
    if (CImean==T){
        mean.up <- lm1$fitted+t.quantile*SEmuhat
        mean.down <- lm1$fitted-t.quantile*SEmuhat
        lines(xx,mean.up,lty=2)
        lines(xx,mean.down,lty=2)
    }
    if (PI==T){
        PI.up <- lm1$fitted+t.quantile*SEpred
        PI.down <- lm1$fitted-t.quantile*SEpred
        lines(xx,PI.up,lty=3)
        lines(xx,PI.down,lty=3)
    }
    if (CIregline==T){
        HW <- sqrt(2*qf(confidencelevel,n-lm1$df.residual,lm1$df.residual))*SEmuhat 
        CIreg.up <- lm1$fitted+HW
        CIreg.down <- lm1$fitted-HW
        lines(xx,CIreg.up,lty=4)
        lines(xx,CIreg.down,lty=4)
    }   
      if (legend==T){
        choices <- c(CImean,PI,CIregline)
        line.type <- c(2,3,4)
             names.line <- c("CI for mean resp.","Prediction Int.","CI for reg. line")
        legend(max(xx)-.2*max(xx),max(yy)+.2*max(yy),legend=names.line[choices],lty=line.type[choices])
    }
}
regplot.confbands.fun(ppp$Inflation.difference,ppp$Exchange.rate.change) 
#
# Confidence and prediction intervals for new observation
#
newppp = data.frame(Inflation.difference = c(-0.68))
#
predict(pppa,newppp,interval=c("confidence"))
predict(pppa,newppp,interval=c("prediction"))
#
# 4 in 1 residual plot. 
#
par(mfrow=c(2,2))
plot(ppp$Inflation.difference, residuals(pppa),xlab="Inflation Difference",ylab="Residuals",main="Versus Predictor")
abline(h=0,lty=2)
plot(fitted(pppa),residuals(pppa),xlab="Fitted values",ylab="Residuals",main="Versus Fits")
abline(h=0,lty=2)
qqnorm(residuals(pppa))
abline(0,1,lty=1,col="red")
hist(residuals(pppa),main="Histogram of residuals",xlab="Residuals")
#



##### Repeat Analysis: Omit Brazil #########

newppp = ppp[ppp$Country!="Brazil",]
#attach(newppp)
par(mfrow=c(1,1))
plot(newppp$Inflation.difference,newppp$Exchange.rate.change, main="Scatterplot of Exchange rate change 
     vs Inflation difference", xlab="Inflation difference",ylab="Exchange rate change")
pppn = lm(newppp$Exchange.rate.change ~ newppp$Inflation.difference)
summary(pppn)

linearHypothesis(pppn,c(0,1),rhs=1)

# 4 in 1 residual plot. 
#
par(mfrow=c(2,2))
plot(newppp$Inflation.difference, residuals(pppn),xlab="Inflation Difference",ylab="Residuals",main="Versus Predictor")
abline(h=0,lty=2)
plot(fitted(pppn),residuals(pppn),xlab="Fitted values",ylab="Residuals",main="Versus Fits")
abline(h=0,lty=2)
qqnorm(residuals(pppn))
abline(0,1,lty=1,col="red")
hist(residuals(pppn),main="Histogram of residuals",xlab="Residuals")
#

######################### Split data ###################################################

#attach(newppp)
developed = newppp[newppp$Developed=="Developed",] #20 developed countries
developing = newppp[newppp$Developed=="Developing",] #20 developing countries

######################### Developed Country ############################################

#attach(developed)
pppb = lm(Exchange.rate.change ~ Inflation.difference, data=developed)
summary(pppb)

linearHypothesis(pppb,c(0,1),rhs=1)

par(mfrow=c(2,2))
plot(developed$Inflation.difference, residuals(pppb),xlab="Inflation Difference",ylab="Residuals",main="Versus Predictor")
abline(h=0,lty=2)
plot(fitted(pppb),residuals(pppb),xlab="Fitted values",ylab="Residuals",main="Versus Fits")
abline(h=0,lty=2)
qqnorm(residuals(pppb))
abline(0,1,lty=1,col="red")
hist(residuals(pppb),main="Histogram of residuals",xlab="Residuals")
#

######################### Developing Country ############################################

#attach(developing)
plot(developing$Inflation.difference,developing$Exchange.rate.change,
     main="Scatterplot of Exchange rate change 
     vs Inflation difference",
     xlab="Inflation difference",ylab="Exchange rate change")
pppc = lm(Exchange.rate.change ~ Inflation.difference, data=developing)
summary(pppc)
anova(pppc)
linearHypothesis(pppc,c(0,1),rhs=1)

par(mfrow=c(2,2))
plot(developing$Inflation.difference, residuals(pppc),xlab="Inflation Difference",ylab="Residuals",main="Versus Predictor")
abline(h=0,lty=2)
plot(fitted(pppc),residuals(pppc),xlab="Fitted values",ylab="Residuals",main="Versus Fits")
abline(h=0,lty=2)
qqnorm(residuals(pppc))
abline(0,1,lty=1,col="red")
hist(residuals(pppc),main="Histogram of residuals",xlab="Residuals")
#


######################## Omit Indonesia

newdeveloping = developing[developing$Country!="Indonesia",]
#attach(newdeveloping)
plot(newdeveloping$Inflation.difference,newdeveloping$Exchange.rate.change,
     main="Scatterplot of Exchange rate change 
     vs Inflation difference",
     xlab="Inflation difference",ylab="Exchange rate change")
pppd = lm(Exchange.rate.change ~ Inflation.difference, data= newdeveloping)
summary(pppd)
anova(pppd)
linearHypothesis(pppd,c(0,1),rhs=1)

par(mfrow=c(2,2))
plot(newdeveloping$Inflation.difference, residuals(pppd),xlab="Inflation Difference",ylab="Residuals",main="Versus Predictor")
abline(h=0,lty=2)
plot(fitted(pppd),residuals(pppd),xlab="Fitted values",ylab="Residuals",main="Versus Fits")
abline(h=0,lty=2)
qqnorm(residuals(pppd))
abline(0,1,lty=1,col="red")
hist(residuals(pppd),main="Histogram of residuals",xlab="Residuals")
#

