# Balanced two-way ANOVA

# a: read cattle.dat
setwd("M:/LM/data")  # Change to whatever path you use yourself
cb <- read.table("cattle-balanced.dat", col.names=c("feed","bull","wgtgain"), colClasses=c("factor","factor","numeric"))   # This reads in a raw ASCII datafile; column names are specified
cb

# b: ordinary means per feed-bull combination
tapply(cb$wgtgain, list(cb$feed, cb$bull), mean)

# c: two-way anova with interaction
cb.lm <- lm(wgtgain ~ feed + bull + feed:bull, data=cb)
summary(cb.lm)
anova(cb.lm)

# type II and type III SS in package car
library(car)

Anova(cb.lm, type="II")

cb.lm.type3 <- lm(wgtgain ~ feed + bull + feed:bull, contrasts=list(feed=contr.sum, bull=contr.sum), data=cb)
# Note: for type III SS sum-to-zero contrasts are needed!
Anova(cb.lm.type3, type="III") 


# d: pairwise comparisons between bulls and feeds, using Tukey method
library(emmeans)
(emm.b <- emmeans(cb.lm, pairwise ~ bull, adjust="tukey"))
(emm.f <- emmeans(cb.lm, pairwise ~ feed, adjust="tukey"))

# e: profile plot
emmip(cb.lm, bull ~ feed)

# f: fitting additive model
cba.lm <- lm(wgtgain ~ feed + bull, data=cb)
summary(cba.lm)
anova(cba.lm)

# profile plot for additive model
emmip(cba.lm, bull ~ feed)