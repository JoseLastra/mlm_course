# Exercise 6: RCBD

# Example from Kuehl (Ex8.1, p 264)

# a Read data into R 

block <- factor(rep(1:4, each=6))
trt   <- factor(rep(1:6, times=4))
nitrogen <- c(34.98, 40.89, 42.07, 37.18, 37.99, 34.89, 41.22, 46.69, 49.42, 45.85, 41.99, 50.15,
              36.94, 46.65, 52.68, 40.23, 37.61, 44.57, 39.97, 41.90, 42.91, 39.20, 40.45, 43.29)
                
(rcbd <- data.frame(block, trt, nitrogen))

# a Blocks fixed
library(emmeans)

b.fixed <- lm(nitrogen ~ block + trt, data=rcbd)
# Use anova to test the fixed effects model
anova(b.fixed) 
summary(b.fixed)
# Use emmeans function from emmeans package to get post-hoc pairwise comparisons
emmeans(b.fixed, pairwise ~ trt, adjust="none")


# b Blocks random

# package nlme, function lme

library(nlme)
# Fit a mixed model with block as random effect
b.random1 <- lme(nitrogen ~ trt, random= ~ 1 | block, data=rcbd)
# Use anova to test the mixed effects model
anova(b.random1)

summary(b.random1)
# Use emmeans function from emmeans package to get post-hoc pairwise comparisons
emmeans(b.random1, pairwise ~ trt, adjust="none")


#lmer
library(lme4)
# Fit a mixed model with block as random effect
b.random2 <- lmer(nitrogen ~ trt + (1 | block), data=rcbd)
summary(b.random2)
# Use anova to test the mixed effects model
anova(b.random2)
# Notice: no P-value for F-test
# This is because the lmer function does not provide P-values for F-tests by default.
# To get P-value load lmerTest and pbkrtest
library(lmerTest) 
library(pbkrtest) 
# These packages provide methods for calculating P-values for F-tests in mixed models
# Refit the mixed model once these packages are loaded
b.random2 <- lmer(nitrogen ~ trt + (1 | block), data=rcbd)
anova(b.random2, ddf="Kenward-Roger") # Use Kenward-Roger method for degrees of freedom 

# Use emmeans function from emmeans package to get post-hoc pairwise comparisons
emmeans(b.random2, pairwise ~ trt, adjust="none")

# Unload packages lmerTest and pbkrtest again
detach("package:lmerTest", unload=TRUE)
detach("package:pbkrtest", unload=TRUE)

#aov
b.random3 <- aov(nitrogen ~ trt + Error(block), data=rcbd)
summary(b.random3)

emmeans(b.random3, pairwise ~ trt, adjust="none")
