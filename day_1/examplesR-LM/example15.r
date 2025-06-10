
peanuts <- read.csv("peanuts.csv")
head(peanuts, nrow=3)

peanuts$fert <- factor(peanuts$fert)
levels(peanuts$fert) <- c("C","F","S")

# one-way ANOVA
aov.peanuts <- lm(yield ~ fert, data=peanuts)
anova(aov.peanuts)

# ANCOVA
anc.peanuts <-  lm(yield ~ height + fert, data=peanuts)
anova(anc.peanuts) # sequential SS, height first!
coef(anc.peanuts)

library(car)
Anova(anc.peanuts, type="II") # type II SS, order unimportant now

coef(summary(anc.peanuts))

confint(anc.peanuts)

# Ordinary means:
library(doBy)
summaryBy(yield+height ~ fert, data=peanuts, FUN=mean)

# Adjusted means:
library(emmeans)
emmeans(anc.peanuts, ~ fert)

# Pairwise comparisons among adjusted means / comparison among treatments (corrected for covariate)
emmeans(anc.peanuts, pairwise ~ fert, adjust="tukey")