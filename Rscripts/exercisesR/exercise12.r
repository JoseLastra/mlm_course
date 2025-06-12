# Exercise 12: Repeated measurement for Sitka spruce data

# Read data
#setwd(" ") # change to proper working directory, if needed
Sitka <- read.csv("exercise12.csv")
Sitka$tree <- factor(Sitka$tree)
Sitka$treat <- factor(Sitka$treat)
head(Sitka)

library(nlme)
Sitka.grp <- groupedData(size ~ time | tree, outer= ~treat, data=Sitka)
# nice plot of size vs time
plot(Sitka.grp, outer= ~treat, key=FALSE)

# 1: Compound symmetry: constant correlation among observations from same tree
(lme.tree1 <- gls(size ~ treat + time + treat:time, correlation =  corCompSymm(form = ~ 1 | tree), data=Sitka) ) 
summary(lme.tree1)
logLik(lme.tree1)
AIC(lme.tree1)

# Different specification of same model
(lme.tree1.1 <- lme(size ~ treat + time + treat:time, random= ~ 1  | tree, data=Sitka) ) 
summary(lme.tree1.1)
logLik(lme.tree1.1)
AIC(lme.tree1.1)

# 2: AR(1): autocorrelation
(lme.tree2 <- gls(size ~ treat + time + treat:time, correlation = corAR1(form = ~ 1 | tree), data=Sitka) )
summary(lme.tree2)
logLik(lme.tree2)
AIC(lme.tree2)

#3b: unstructured, heterogeneous variance
(lme.tree3a <- gls(size ~ treat + time + treat:time, 
                   correlation =  corSymm(form = ~ 1 | tree), 
                   weights=varIdent(form= ~ 1 | time), data=Sitka) ) 
summary(lme.tree3a)
logLik(lme.tree3a)
AIC(lme.tree3a)

#3b: unstructured, homogeneous variance
(lme.tree3b <- lme(size ~ treat + time + treat:time, 
                   random = list(tree = pdSymm(~ 1)), data=Sitka) ) 
summary(lme.tree3b)
logLik(lme.tree3b)
AIC(lme.tree3b)

# 4: random coefficient model)
lme.tree4 <- lme(size ~ treat + time + treat:time, random= ~ 1 + time | tree, data=Sitka.grp)
summary(lme.tree4)
logLik(lme.tree4)
AIC(lme.tree4)

# Compare F-values of fixed effects
anova(lme.tree1)
anova(lme.tree2)
anova(lme.tree3a)
anova(lme.tree3b)
anova(lme.tree4)

# Extract variance covariance matrices
getVarCov(lme.tree1)
getVarCov(lme.tree2)
getVarCov(lme.tree3a)
getVarCov(lme.tree3b)

getVarCov(lme.tree4, type="marginal")

getVarCov(lme.tree4, type="random.effects") # This is the covariance matrix of intercepts - slopes

# To get correlation matrix do e.g.

covmat3a <- getVarCov(lme.tree3a)
std3a <- sqrt(diag(covmat3a))
(corrmat3a <- diag(1/std3a) %*% covmat3a %*% diag(1/std3a))

# Do the same for the other 4 covariance matrices...


# We forgot something.....there could be quadratic effect of time

