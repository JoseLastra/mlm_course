# Unbalanced two-way ANOVA

# a: read cattle.dat
setwd("M:/LM/data")  # Change to whatever path you use yourself
cattle <- read.table("cattle.dat", col.names=c("feed","bull","wgtgain"), colClasses=c("factor","factor","numeric"))   # This reads in a raw ASCII datafile; column names are specified
cattle

# b
cattle.lm <- lm(wgtgain ~ feed + bull + feed:bull, data=cattle)
summary(cattle.lm)
anova(cattle.lm)

library(car)

Anova(cattle.lm, type="II")

cattle.lm.type3 <- lm(wgtgain ~ feed + bull + feed:bull, data=cattle, contrasts=list(feed=contr.sum, bull=contr.sum))
# Again, for type III SS sum-to-zero contrasts are needed!
Anova(cattle.lm.type3, type="III")


# c Profile plot
emmip(cattle.lm, bull ~ feed)

# d Means and estimated marginal means:

# Ordinary means per bull:
tapply(cattle$wgtgain, cattle$bull, mean) # ordinary means in groups
# alternative:  
library(doBy)
summaryBy(wgtgain ~ bull, data=cattle, FUN=c(mean))

# Estimated marginal means per bull:
emmeans(cattle.lm, ~ bull)

# Or use glht function:
library(multcomp)
# emm for bull 1 = 0.5*(mu11+mu12) 
# Translate into parameters used by R: 0.5*(ic + ic+f2) = ic + 0.5*f2,
# because f1,b1,fb11,fb12,fb13 are all zero

K  <- rbind("(mu11+mu21)/2" = c(1, 0.5, 0,0, 0,0))
lp <- glht(cattle.lm, linfct=K)
summary(lp)

# e Compare bull 1 with average of bull2 and 3  within feed 1
# Translate into parameters used by R:
# m + f1 + b1 + fb11  - 0.5(m+f1+b2+fb12 + m+f1+b3+fb13)
# =-0.5*b2 - 0.5*b3, because f1,b1,fb11,fb12,fb13 are all zero
K2  <- rbind("f1b1vsb23" = c(0, 0, -0.5, -0.5, 0, 0))
lp2 <- glht(cattle.lm, linfct=K2) 
summary(lp2)

# f Slice food 1, two contrasts needed
K1 <- c(0, 0, -1, 0, 0, 0)
K2 <- c(0, 0, 0, -1, 0, 0)
K <- rbind(K1,K2)
rownames(K) <- c("f1:b1-b2", "f1:b1-b3")

slicef1 <- glht(cattle.lm, linfct=K) 
summary(slicef1)

# g Type III SS according to SAS

# We use the function linearHypothesis from car package.
# Type III SS for food is based on H0: (mu11+mu12+mu13)/3 - (mu21+mu22+mu23)/3 = 0
# Translate into R parameters: (ic + ic+b2 + ic+b3)/3 - (ic+f2 + ic+f2+b2+fb22 + ic+f2+b3+fb23)/3 = 0
# Work out contrast: (-f2 - f2-fb22 - f2-fb23)/3 = 0, because all ic, b2, b3 parameters cancel out
# Equivalent with 3*f2 + fb22+fb23 = 0
names(coef(cattle.lm))
linearHypothesis(cattle.lm, "3*feed2 + 1*feed2:bull2 + 1*feed2:bull3")

# Type III SS for bull and food:bull equivalently.

