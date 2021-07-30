## Read data with 'read.table' R command for reading ASCII files
cancer_data = read.table("CancerStudy.txt", header=T)
## Response Variable
survival = cancer_data$Survival

## Explore the shape of the distribution of the response variable
hist(survival, xlab="", ylab="Number of Survival Days", main="", nclass=15)

## Transform due to skewness of the distribution
hist(log(survival), xlab="", ylab="Number of Survival Days", main="", nclass=15)

## Need to specify Response & Categorical Variables
survival = log(survival)
cancertype = cancer_data$Organ
## Convert into categorical variable in R
cancertype = as.factor(cancertype)
boxplot(survival~cancertype, xlab = "Cancer Type", ylab = "Log(Number of Survival Days)")

## ANOVA in R: Is the between-variability significantly larger than within-variability
model = aov(survival ~ cancertype)
summary(model)

## Obtain estimated means
model.tables(model, type="means")

## Which means are statistically significantly different? Pairwise Comparison
TukeyHSD(model)

par(mfrow=c(2,2))
qqnorm(residuals(model)) 
qqline(residuals(model))
hist(residuals(model), main="Histogram of residuals", xlab="Residuals")
plot(residuals(model), xlab="Order", ylab="Residuals")
abline(0, 0, lty=1, col="red")
plot(fitted(model), residuals(model), xlab="Fitted values", ylab="Residuals")
abline(0, 0, lty=1, col="red")