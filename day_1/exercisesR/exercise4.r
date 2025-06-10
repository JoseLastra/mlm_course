# Exercise 4: fixed and random effects model; shrinkage

# a Introduce data

line <- factor(rep(1:3, each=10))
plant <- factor(rep(1:10,3))
height <- c(88.0, 88.0, 94.8, 90.0, 93.0, 89.0, 86.0, 92.9, 89.0, 93.0,
            85.9, 88.6, 90.0, 87.1, 85.6, 86.0, 91.0, 89.6, 93.0, 87.5,
            94.2, 91.5, 92.0, 96.5, 95.6, 93.8, 92.5, 93.2, 96.2, 92.5)

(barley <- data.frame(line, plant, height))

# b Fixed effects model using lm()

lm3 <- lm(height ~ line, data=barley)
anova(lm3)
summary(lm3)
library(emmeans)  # for estimated marginal means
emmeans(lm3, ~ line)  # estimated means mu + alpha_i

tapply(barley$height, barley$line, mean)  # just ordinary means, i.e. not model-based; same here

# c Random effects model using lme()

library(nlme)
(mm3 <- lme(fixed= height ~ 1, random = ~ 1 | line, data=barley))
VarCorr(mm3) # variance components
ranef(mm3)   # BLUPs of random effects
fixef(mm3)   # fixed effects, here intercept only
(pred.mm <- unlist(coef(mm3)))    # predictions of means, based on BLUPs

# Notice that these predictions are somewhat shrunken 
# towards the overall mean compared to the means from fixed effects model

# In powerpoint it is said that shrunken means are obtained as weighted average of 
# group mean and overall mean, with weights w = k v_a / (k v_a + v_e) and 1-w.
# Check:

(va <- as.numeric(VarCorr(mm3)[1,1]))  # variance component of lines
(ve <- as.numeric(VarCorr(mm3)[2,1]))  # error variance
k <- 10                    # 10 plants per line
(w <- k*va/(k*va + ve))

av <- mean(barley$height)
gr.av <- tapply(barley$height, barley$line, mean)

w.av <- (1-w)*av + w*gr.av

cbind(av, gr.av, w.av, pred.mm)  # check that w.av and pred.mm are identical

# d Variance component estimates using mean squares from lm()
# EMS.L = 10*v.a + v.e    (10 observations per line)
# EMS.E = v.e.
lm2 <- lm(height ~ line, data=barley)
(anova.table <- anova(lm2))
MS.L <- anova.table[1,3]   # Mean square of line
MS.E <- anova.table[2,3]   # Mean square of error
(vc.L <- (MS.L-MS.E)/10)   # Compare with variance component estimate from lme()
