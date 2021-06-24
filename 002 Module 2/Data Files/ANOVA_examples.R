
## Analysis of Suicide Data
suicide_data = read.csv("suicide_data.csv", header = TRUE)

## Side by Side Boxplot
library(ggplot2)
ggplot(suicide_data, aes(x=region, y=suicidesper100k)) + geom_boxplot() + theme(axis.text.x  = element_text(angle=90, vjust=0.5))+ggtitle("Suicides per 100K Pop with Respect to World Region - ANOVA")

# apply ANOVA
model = aov(suicidesper100k ~ region, data=suicide_data)

## More extensive output
summary(model)
model.tables(model, type="means")

## Pairwise comparison
TukeyHSD(aov(suicidesper100k ~ region, data=suicide_data))

## Discard groups with 1 observation
region = suicide_data$region
groups.1 = c(which(region=="NORTHERN AMERICA"),which(region=="WESTERN ASIA"))
suicide_data = suicide_data[-groups.1,]
model.1 = aov(suicidesper100k ~ region, data=suicide_data)

## Diagnostic plots
plot(model.1)
resid.model.1=residuals(model.1)
################################################################################

## Analysis of Keyboard Data

# read in times without header
speed.data = read.table("keyboard_data.txt")
# convert wide format table to long format without the 0s indicating missing data
speeds = data.frame(speed=speed.data[(speed.data[,1]!=0),1], layout="1")
speeds = rbind(speeds, data.frame(speed=speed.data[(speed.data[,2]!=0),2], layout="2"))
speeds = rbind(speeds, data.frame(speed=speed.data[(speed.data[,3]!=0),3], layout="3"))
attach(speeds)

# visualize
boxplot(speed ~ layout, xlab="Keyboard layout", ylab = "Times taken to perform a task")

## apply ANOVA
keyboard.aov = aov(speed ~ layout)

## Diagnostic plots
plot(keyboard.aov)

## More extensive output
summary(aov(speed ~ layout))
# or, equivalently
# summary(keyboard.aov)

model.tables(keyboard.aov, type="means")

TukeyHSD(aov(speed ~ layout))
# or, equivalently
# TukeyHSD(keyboard.aov, conf.level = .95)
