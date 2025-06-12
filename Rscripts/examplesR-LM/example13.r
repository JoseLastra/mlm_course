# Two-way ANOVA

setwd("M:/LM/data")  # Change to whatever path you use yourself
limedata <- read.table("lime.prn", header=T)    # This reads in a raw ASCII datafile; column names are given on first line of datafile

limedata$lime <- as.factor(limedata$lime)
limedata$rate <- as.factor(limedata$rate)

levels(limedata$lime)
levels(limedata$rate)

lime.anova <- lm(pH ~ lime + rate + lime*rate, data=limedata)

anova(lime.anova)

(SST <- sum((limedata$pH-mean(limedata$pH))^2 ))

coef(summary(lime.anova))