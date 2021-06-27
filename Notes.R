################################################################################################
# Create a data frame 
################################################################################################
# Single Data Point
newcpu = data.frame(chmax=128)

## Multiple Data Points
df <- data.frame(name = c("Jon", "Bill", "Maria"),
                 age = c(23, 41, 32)
)

## Number of rows
n=nrow(data)

# Subset
data2 = data2[data2$vendor %in% c("honeywell", "hp", "nas"), ]
data2$vendor = factor(data2$vendor)

# Get the element at row 1, column 3
data[1,3]
data[1,"size"]

# Get rows 1 and 2, and all columns
data[1:2, ]  
data[c(1,2), ]

#Get rows 1 and 2, and only column 2
data[1:2, 2]
data[c(1,2), 2]

#Get rows 1 and 2, and only the columns named "sex" and "size"
data[1:2, c("sex","size")]

data[data$subject < 3, ]

#Drop the first element
v[-1]

# Remove outlier
fish2<-fish[-30,]


################################################################################################
## Linearity/Mean Zero, Independence and Constant Variance - X vs Y
################################################################################################
ggplot(data=data, aes(x=chmax, y=performance)) +
  geom_point(alpha=I(0.2),color='blue') +
  xlab('Maximum Channels') +
  ylab('CPU Performance') +
  ggtitle('Maximum Channels vs CPU Performance') +
  geom_smooth(method= "lm",color='gray', se=FALSE)

## scatterplots multiple Features
par(mfrow=c(2,3))
for (i in c(3:7)){
  col_name = names(fish[i])
  plot(fish[,i], fish$Weight, xlab= col_name, ylab = "Weight")
}

################################################################################################
# Linearity - scatterplots of the standardized residuals of model2 versus each quantitative predictor (X vs residuals)
################################################################################################
resids =rstandard(model2)
par(mfrow=c(2,3))
for (i in c(3:7)){
  col_name = names(fish2[i])
  plot(fish2[,i], resids, xlab= col_name, ylab = "S. Residuals")
  abline(h=0, col="red")
  lines(lowess(fish2[,i], resids), col='blue')
}

################################################################################################
# Independence (Uncorrelated errors) and Constant Variance (fitted vs residuals)
################################################################################################
ggplot(data=data, aes(x=model1$fitted.values, y=model1$residuals)) +
  geom_point(alpha=I(0.4),color='darkorange') +
  xlab('Fitted Values') +
  ylab('Residuals') +
  ggtitle('Residual Plot') +
  geom_hline(yintercept=0)

# Plot of std. residuals versus fitted values
plot(model2$fitted.values, resids, xlab="Fitted Values", ylab=" S. Residuals")
lines(lowess(model2$fitted.values, resids), col='blue')
abline(h=0, col="red")

################################################################################################
##Normality - Histogram and q-q plot of teh residuals
################################################################################################
hp = qplot(model1$residuals,
           geom="histogram",
           bins=11,
           main = "Histogram of Residuals",
           xlab = "Residuals",
           ylab = "Count",
           fill=I("blue"),
           alpha=I(0.2))
qqp = ggplot(data, aes(sample=model1$residuals)) +
stat_qq(alpha=I(0.2),color='darkorange') +
  stat_qq_line() +
  ggtitle("Q-Q Plot of Residuals")
ggarrange(hp, qqp, ncol=2, nrow=1)

## QQ plot
qqPlot(model1$residuals)

resids =rstandard(model2)
qqPlot(resids)

## Histogram Plots for normality
resids =rstandard(model2)
hist(resids, col="orange", nclass=15)


################################################################################################
## Model Assumptions - One Variable 
################################################################################################
# Get standardized residuals
resids2 =rstandard(model3)
# Residual plots
par(mfrow=c(2,2))
plot(fish2$Total.Length, resids2 , main="Lineartiy/Mean Zero",
     xlab="Total.Length", ylab="Residuals")
abline(h=0, col="red")
lines(lowess(fish2$Total.Length, resids2), col='blue')
plot(model3$fitted.values, resids2 , main="Constant Variance/Correlation",
     xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")
lines(lowess(model3$fitted.values, resids2), col='blue')
hist(resids2 , col="orange", nclass=15)
qqPlot(resids2)


################################################################################################
## Box Cox Transformation 
################################################################################################
bc = boxCox(model1)
# Find the optimal lambda
opt.lambda = bc$x[which.max(bc$y)]
# Round it to the nearest 0.5
cat("Optimal lambda:", round(opt.lambda/0.5)*0.5, end="\n")

## Square Root
model4<-lm(Weightˆ(1/2)~ Species + Total.Length, data=fish2)

## log- if transforming both X and Y
model2 = lm(log(performance) ~ log(chmax + 1), data=data)

################################################################################################
## Box Plot 
################################################################################################
library(ggplot2)
ggplot(fish, aes(x=Species, y=Weight, color=Species), 
       xlab = "Species", 
       ylab = "Weight") + 
  geom_boxplot() + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.5))+
  ggtitle("Weight with Respect to Species")

## Box Plot Residuals
boxplot(model[['residuals']],main='Boxplot: Residuals',ylab='residual value')

################################################################################################
## Correlation
################################################################################################
cor(data$performance, data$chmax)
library(GGally)
ggpairs(fish[ ,!(colnames(fish) == "Species")])


################################################################################################
## Modeling  - SLR - Log transform
################################################################################################
model2 = lm(log(performance) ~ log(chmax + 1), data=data)

sp2 = ggplot(data=data, aes(x=log(chmax+1), y=log(performance))) +
  geom_point(alpha=I(0.2),color='blue') +
  xlab('Maximum Channels') +
  ylab('Performance') +
  ggtitle('Max Channels vs Performance') +
  geom_line(data=data, aes(x=log(chmax+1), y=model2$fitted.values))
rp2 = ggplot(data=data, aes(x=model2$fitted.values, y=model2$residuals)) +
  geom_point(alpha=I(0.4),color='darkorange') +
  xlab('Fitted Values') +
  ylab('Residuals') +
  ggtitle('Residual Plot') +
  geom_hline(yintercept=0)
hp2 = qplot(model2$residuals,
            geom="histogram",
            bins=11,
            main = "Histogram of Residuals",
            xlab = "Residuals",
            ylab = "Count",
            fill=I("blue"),
            alpha=I(0.2))
qqp2 = ggplot(data, aes(sample=model2$residuals)) +
  stat_qq(alpha=I(0.2),color='darkorange') +
  stat_qq_line() +
  ggtitle("Q-Q Plot of Residuals")
ggarrange(sp2, rp2, hp2, qqp2, ncol=2, nrow=2)



################################################################################################
## Modeling  - Anova
################################################################################################
model3 = aov(performance ~ vendor, data2)
summary(model3)

model = aov (speed ~ layout) 
model.tables (model, type = "means")

TukeyHSD(model3, "vendor", conf.level=0.95)


################################################################################################
# Conduct Partial F-test
################################################################################################
anova(model3, model2)


################################################################################################
## Modeling  - MLR
################################################################################################
# fit full model
model1<-lm(Weight~., data=fish)
# Display summary
summary(model1)

################################################################################################
## Prediction - SLR/MLR
################################################################################################
predict(model1, newcpu, interval="prediction", level=0.95)
# Predict with Log Transform
exp(predict(model2, newcpu, interval="prediction", level=0.95))
# Predict with Square Root transformation
pred4<-predict(model4, fishtest) ^ 2
# Calculate MSPE
mse.model4 = mean((pred4 - fishtest$Weight ) ^ 2)

# Create new data point
new.point<-data.frame(Species="Perch", Total.Length=32)
# Calulate prediction interval
predict(model4, new.point, interval="prediction", level=0.9)ˆ2

################################################################################################
## Estimation - SLR/MLR
################################################################################################
predict(model1, newcpu, interval="prediction", level=0.95)
exp(predict(model2, newcpu, interval="prediction", level=0.95))

################################################################################################
## Cooks Distance - SLR/MLR
################################################################################################
# Calculating Cook's distances
library(car)
cook = cooks.distance(model1)
# Plotting Cook's distances
plot(cook,type="h",lwd=3,col="red", ylab= "Cook's Distance")
abline(1,0,col="blue")


################################################################################################
## VIF
################################################################################################
# VIF Threshold
cat("VIF Threshold:", max(10, 1/(1-summary(model2)$r.squared)), "\n")
# Calculate VIF
vif(model2)



################################################################################################
# Prediction Accuracy
################################################################################################
## Save Predictions to compare with observed data
pred1 <-predict(model1, test, interval = 'prediction')test.pred1 <-pred1[,1]
test.lwr1 <-pred1[,2]
test.upr1 <-pred1[,3]
# Mean Squared Prediction Error (MSPE)
mean((test.pred1-test$cnt)^2)
[1] 10304.95
# Mean Absolute Prediction Error (MAE)
mean(abs(test.pred1-test$cnt))
[1] 74.52024
# Mean Absolute Percentage Error (MAPE)
mean(abs(test.pred1-test$cnt)/test$cnt)
[1] 2.724609
# Precision Measure (PM)
sum((test.pred1-test$cnt)^2)/sum((test$cnt-mean(test$cnt))^2)
[1] 0.3101164
# CI Measure (CIM)
sum(test$cnt<test.lwr1)+sum(test$cnt>test.upr1)/nrow(test)
[1] 0.06904488



################################################################################################
# Model Summary
################################################################################################
summary(model)

names(summary(model))

sigsq = summary(model1)$sigma ** 2

## Confint
confint(model1)['chmax',]

#Residual Standard error (Like Standard Deviation)
k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals**2)
n=length(model$residuals)
sqrt(SSE/(n-(1+k))) #Residual Standard Error

#Multiple R-Squared (Coefficient of Determination)
SSyy=sum((y-mean(y))**2)
SSE=sum(model$residuals**2)
(SSyy-SSE)/SSyy
#Alternatively
1-SSE/SSyy

#Adjusted R-Squared
n=length(y)
k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals**2)
SSyy=sum((y-mean(y))**2)
1-(SSE/SSyy)*(n-1)/(n-(k+1))

#F-Statistic
#Ho: All coefficients are zero
#Ha: At least one coefficient is nonzero
#Compare test statistic to F Distribution table
n&lt;-length(y)
SSE&lt;-sum(model$residuals**2)
SSyy&lt;-sum((y-mean(y))**2)
k&lt;-length(model$coefficients)-1
((SSyy-SSE)/k) / (SSE/(n-(k+1)))

#### T-Value
tval = summary(model1)$coefficients['chmax','t value']
df = model1$df.residual
###upper tail, since this we are testing if Beta1 is positive
pval = pt(tval, df, lower=F) #upper tail
pval
###upper tail, since this we are testing if Beta1 is negative
pval = pt(tval, df, lower=T) #upper tail
pval


data(mtcars)
fit <- lm(mpg ~ wt, mtcars)
names(summary(fit))
summary(fit)
confint(fit)
summary(fit)$coefficients[,'t value']
summary(fit)$coefficients[,'Std. Error']
summary(fit)$coefficients[,'Estimate']
summary(fit)$coefficients[,'Pr(>|t|)']


