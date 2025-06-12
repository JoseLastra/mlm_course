# Example subsampling

method <- factor(rep(c("a","b"), each=6))
batch <- factor(rep(1:6, each=2))
pesticide <- c(120,110,120,100,140,130,71,71,70,76,63,68)

(subsampling <- data.frame(method, batch, pesticide))

library(emmeans)

# ordinary linear model: ignore batch completely

lm.sub1 <- lm(pesticide ~ method, data=subsampling)
anova(lm.sub1)  # wrong F-test for method!
coef(summary(lm.sub1))  # wrong t-test

emmeans(lm.sub1, ~ method)
emmeans(lm.sub1, pairwise ~ method)


# ordinary linear model: include batch (nested within method) into model

lm.sub2 <- lm(pesticide ~ method + batch %in% method, data=subsampling)
anova(lm.sub2)  # wrong F-test for method!
coef(summary(lm.sub2))  # wrong t-test

emmeans(lm.sub2, ~ method)  # doesn't work
emmeans(lm.sub2, pairwise ~ method)  # doesn't work

emmeans(lm.sub2, ~ batch:method)  # this is ok


# package nlme, function lme()
library(nlme, lsmeans)
mm.sub1 <- lme(fixed = pesticide ~ method, random = ~ 1 | batch, data=subsampling)
summary(mm.sub1)
anova(mm.sub1)  # proper F-test
emmeans(mm.sub1, pairwise ~ method)  

# package lme4, function lmer()
library(lme4)
library(pbkrtest)
library(lmerTest)

mm.sub2 <- lmer(pesticide ~ method + (1 | batch ), data=subsampling)
summary(mm.sub2)
anova(mm.sub2, ddf="Kenward-Roger")

emmeans(mm.sub2,  pairwise ~ method) # gives proper means and standard errors, and in combination with pbkrtest package also proper df.


aov.sub <- aov(pesticide ~ method + Error(batch), data=subsampling)
summary(aov.sub)

