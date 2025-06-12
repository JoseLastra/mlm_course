# Exercise 3: t-tests and mixed model

# From previous exercise 1: t-tests

# paired samples t-test for animals 1-6
animal <- 1:6
feedA <- c(31.85, 27.31, 31.09, 31.77, 34.62, 31.28)
feedB <- c(32.03, 26.84, 29.99, 31.47, 34.24, 31.02)
(paired <- data.frame(animal=animal, feedA=feedA, feedB=feedB))
t.test(paired$feedA, paired$feedB, mu=0, paired = TRUE)

# independent samples t-test for animals 7-12 
animal <- 7:12
feed <- c(rep("A",3), rep("B",3))
y <- c(36.00, 32.39, 34.35, 27.14, 29.14, 31.49)
(indep <- data.frame(animal=animal, feed=feed, y=y))
# Notice the "long" format, instead of the "wide" format in paired analysis
t.test(y ~ feed, data=indep, var.equal=T)

# join the two using mixed model
# An alternative way of specifying data is needed: "long" format as in independent samples part
animal <- c(rep(1:6, each=2), 7:12)
feed <- as.factor(c(rep(c("A","B"), 6), rep("A",3), rep("B",3)))
y <- c(as.vector(t(cbind(feedA,feedB))), y)
(all <- data.frame(animal=animal, feed=feed, y=y))

# Now mixed model approaches:

# use package lme4

library(lme4)
library(lmerTest)  # for p-values in summary output

(mm1.1 <- lmer(y ~ feed + (1 | animal), data=all))


summary(mm1.1)          # summary() of mixed model object

#some other functions (methods) for the mer object
anova(mm1.1)            # Notice that no P-value is given, but F-value is same as obtained with lme
VarCorr(mm1.1)          # Notice that square root of variance components (standard deviations) are given
print(VarCorr(mm1.1), comp=c("Variance"))  # Now variances are given
fixef(mm1.1)            # estimates of fixed effects (ic = mean feedA; feedB = difference feedB-feedA)
ranef(mm1.1)   # BLUPs of random effects for animals
intervals(mm1.1)  # confidence intervals for fixed effects

# Other often used package for mixed models:
library(nlme)

(mm1.2 <- lme(fixed= y ~ feed, random = ~ 1 | animal, data=all))
summary(mm1.2)          # summary of mixed model object

# some other functions for the lme object
anova(mm1.2)            # hypothesis test (F-test) for fixed effect(s)
VarCorr(mm1.2)          # variance components   
fixef(mm1.2)            # estimates of fixed effects (ic = mean feedA; feedB = difference feedB-feedA)
random.effects(mm1.2)   # BLUPs of random effects for animals
intervals(mm1.2)  # confidence intervals for fixed effects

# Yet another way: look at the problem as linear model with correlated errors (so, bypassing the Zu part, and directly target the R matrix)
# The function gls() of the nlme package may be used.
# Use a compound symmetric correlation structure

(mm1.3 <- gls(y ~ feed, correlation =  corCompSymm(form = ~ 1 | animal)) )
summary(mm1.3)
# parameter rho is the intraclass correlation = var(u) / (var(u)+var(e))
anova(mm1.3)

# Notice the different P-value for feedB in output by gls(), compared to output by lme(); this relates to used error df=16 in gls(), 5 in lme().