# Two-way ANOVA

setwd("M:/LM/data")  # Change to whatever path you use yourself
limedata <- read.table("lime.prn", header=T)    # This reads in a raw ASCII datafile; column names are given on first line of datafile

limedata$lime <- as.factor(limedata$lime)
limedata$rate <- as.factor(limedata$rate)

lime.anova <- lm(pH ~ lime + rate + lime:rate, data=limedata)

# Estimated (marginal) means (same as ordinary means here):
library(emmeans)
emmeans(lime.anova, ~ lime:rate)
emmip(lime.anova, lime ~ rate, 
      main="Interaction plot: plot of mean pH", cex=1.5)
# interaction plot
