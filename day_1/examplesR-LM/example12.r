# Example 11: oneway ANOVA

# setwd("M:/LM/data")  # Change to whatever path you use yourself
limedata <- read.table("lime.prn", header=T)    # This reads in a raw ASCII datafile; column names are given on first line of datafile

# select first three groups
limeAL <- limedata[(limedata$lime=="AL") & (limedata$rate<4),]

limeAL$rate <- as.factor(limeAL$rate)
levels(limeAL$rate) <- c("no","low","high")

library(doBy)
# some descriptive statistics per group
summaryBy(pH ~ rate, data=limeAL, FUN=c(mean,sd, length))  

# fit one-way ANOVA model to pH
limeAL.oneway <- lm(pH ~ rate, data=limeAL)

anova(limeAL.oneway)    # gives ANOVA table
(SST <- sum((limeAL$pH-mean(limeAL$pH))^2))

summary(limeAL.oneway)  # gives other summary information for lm object

coef(summary(limeAL.oneway))  # only parameter estimates, se and t-tests
